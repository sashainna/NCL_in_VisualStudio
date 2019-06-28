/*********************************************************************
**    NAME         :  lipvmot.c
**       CONTAINS:
**				ul_ipv_pltmot
**          ul_ipv_cutters
**          ul_ipv_cutter
**          ul_ipv_lathe_cutter
**          ul_ipv_reset_tool
**          ul_ipv_set_tool_colors
**          ul_ipv_reset_tool_colors
**          ul_ipv_view_taxis
**
**    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvmot.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       05/19/15 , 14:01:09
*********************************************************************/

#include "usysdef.h"
#include "stdio.h"
#include "lcom.h"
#include "nclmplay.h"
#include "lipv.h"
#include "lipvmach.h"
#include "lipvmplay.h"
#include "lipvstack.h"
#include "bsym.h"
#include "mdrel.h"
#include "m2dattr.h"
#include "mfort.h"
#include "mgeom.h"
#include "mdclass.h"
#include "msol.h"
#include "nccs.h"
#include "nclfc.h"
#include "nclfile.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "uhep.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "driver.h"
#include "mdcpln.h"

extern int moving_part;

void ul_ipv_set_tool_colors();
void ul_ipv_reset_tool_props();
void ul_ipv_view_taxis();

static void S_mill_profile();
static void S_lathe_profile();
static void S_init_stock();
static void S_store_mattr();
static int S_symbol_prim(),S_symbol_surface(),S_symbol_solid();

static int Slast_act[LW_MAX_SPINDLE]={0,0,0,0,0,0,0,0,0,0};
static UU_REAL Stoler=0.;
static UU_REAL Smaxang=0.;
static UU_REAL Sparms[LW_MAX_SPINDLE][3][4];
static int Stranslucency[LW_MAX_SPINDLE][3]={0,0,0, 0,0,0, 0,0,0, 0,0,0,
	0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0, 0,0,0};
static UU_LOGICAL Sedge[LW_MAX_SPINDLE][3]={-1,-1,-1, -1,-1,-1, -1,-1,-1,
	-1,-1,-1, -1,-1,-1, -1,-1,-1, -1,-1,-1, -1,-1,-1, -1,-1,-1, -1,-1,-1};
static int Sedgcol[LW_MAX_SPINDLE][3]={-1,-1,-1, -1,-1,-1, -1,-1,-1, -1,-1,-1,
	-1,-1,-1, -1,-1,-1, -1,-1,-1, -1,-1,-1, -1,-1,-1, -1,-1,-1};
static int Stool_colors[LW_MAX_SPINDLE][7],Sshank_colors[LW_MAX_SPINDLE][6];
static int Sholder_colors[LW_MAX_SPINDLE][5];
static UM_vector Sfwd_vec={1,0,0};

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_pltmot(mblock,mdata,mattr,flush,fromt,cirt,
**                                    cirrec)
**       Creates the actual cutting sequence for NCLIPV.
**    PARAMETERS
**       INPUT  :
**          mblock   Motion (position) block structure.
**          mdata    Motion data structure.
**          mattr    Motion attribute structure.
**          flush    UU_TRUE if display should be updated.
**          infromt  UU_TRUE if this move is a FROM.
**          cirt     0 = Linear, 1 = Horizontal circle, 3 = Vertical circle.
**          cirrec   Circular record when 'cirt' is true.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_pltmot(mblock,mdata,mattr,flush,infromt,cirt,cirrec)
UN_motion_block *mblock;
UN_mot_data *mdata;
UN_mot_attr *mattr;
UU_LOGICAL flush,infromt;
int cirt;
UU_REAL cirrec[];
{
	int i,j,nval,tlno;
	UU_LOGICAL ithrd,fromt;
	UU_REAL fed,fed2,rnum,rdata[20],fdata[LW_MAX_AXES],axis[LW_MAX_AXES];
	LtNat32 cutn,ncutn;
	LtData stuff;
	LtDouble angle,zdelt;
	LtDoublePoint tool_pos;
	LtDoubleVector tool_vec;
	UN_cutter_list *ipt;
	LW_mot_stack_ctype motype;
	LW_mach_model_struc *mpt;

	UM_vector vc1,vc2,vtmp;
	UU_REAL um_mag();
/*
.....Initialize routine
*/
	motype = LW_CUT_NONE;
	fromt = infromt;
/*
.....Reset any clash color changes
*/
	ul_ipv_reset_clash_colors();
/*
.....No tool defined
.....Don't simulate motion
*/
	if (LW_act_tool[0] == -1)
	{
		if (LW_mach_simul) fromt = UU_TRUE;
		else return;
	}
/*
.....Store variables in LW format
*/
	if (LW_mach_mode == LW_VISICUT && LW_mach_type == LW_LATHE)
	{
		tool_pos[0] = mblock->ept[1];
		tool_pos[1] = mblock->ept[2];
		tool_pos[2] = mblock->ept[0];
		for (i=0;i<3;i++) tool_vec[i] = mblock->ept[i+3];
	}
	else
	{
		for (i=0;i<3;i++)
		{
			tool_pos[i] = mblock->ept[i];
			tool_vec[i] = mblock->ept[i+3];
		}
	}
/*
.....Get cut number
*/
	LiSessionGetProperty(LW_session[LW_mach_mode],LI_SESS_PROP_MW_CUT_NUMBER,
		&stuff);
	cutn = LiDataGetNat32(&stuff);
/*
.....Store Machining Attributes
*/
	S_store_mattr(mdata,mattr,cutn);
/*
.....Calculate the move time
*/
	ithrd = UU_FALSE;
	if (LW_tool[0] > 0 && !fromt)
	{
		ipt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
		for (j=0;j<LW_spindle_num;j++)
		{
			fed = 0.;
			if (LW_mot_data != UU_NULL)
			{
				if (LW_mot_data->fr_mode == 0)
				{
					fed = ipt[LW_act_tool[j]].rapid;
					if (fed == 0.) fed = 400.;
				}
				else
				{
					if (LW_mot_data->fr_mode == 2)
						fed = LW_mot_data->fr_val * LW_mot_attr->sp_val;
					else
						fed = LW_mot_data->fr_val;
				}
			}
			if (fed != 0.)
			{
/*				UM_len_exttoint(fed,fed);*/
				if (mblock->time == 0.)
				{
					if (cirt != 0)
						mblock->time = ((2.*UM_PI*cirrec[6]) *
							(fabs(cirrec[7])/360.)) / fed;
					else
						mblock->time = um_dcccc(tool_pos,LW_tool_pos) / fed;
				}
				ipt[LW_act_tool[j]].mchtim = ipt[LW_act_tool[j]].mchtim +
					mblock->time;
			}
		}
/*
........Determine if this is a threading move
*/
		if (LW_mach_mode == LW_VISICUT && LW_mach_type == LW_LATHE)
		{
			if (LW_mot_attr->sp_val != 0. && LW_mot_data != UU_NULL &&
				LW_mot_data->fr_mode != 0)
			{
				zdelt = tool_pos[2] - LW_tool_pos[2];
				fed2 = fed / LW_mot_attr->sp_val;
				if (fed2 > ipt[LW_act_tool[0]].cutter[0]*3 && fabs(zdelt) > UM_FUZZ)
					ithrd = UU_TRUE;
			}
		}
	}
/*
.....VisiCut
*/
	if (LW_mach_mode == LW_VISICUT)
	{
/*
........Update Monitor Panel
*/
		if (LW_monitor) ul_ipv_update_monitor(mblock,mdata,mattr,UU_FALSE);
/*
........Moving part
*/
/*		if (LW_assemb != UU_NULL)
			ul_ipv_move_part(tool_pos,tool_vec,fromt);*/
/*
........Machine Simulation
*/
		if (LW_mach_simul)
		{
/*
...........Lathe threading
*/
			if (LW_mach_type == LW_LATHE && ithrd)
			{
				angle = zdelt / fed2 * 360.;
				if (LW_mot_attr != UU_NULL && LW_mot_attr->sp_mode == 0)
					angle = -angle;
				mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
				LiViLatheSetOrientation(LW_lathe,0.);
				LiViLatheRotate(LW_lathe,angle);
			}
/*
...........Store in Undo Stack
*/
			nval = 10;
			if (mblock->type >= 100) nval = 18;
			ul_ipv_get_axis(fdata);
			if (ul_ipv_push_circle(fdata,cutn,Stool_colors[0]))
				motype = LW_CUT_CIREND;
			else
			{
				motype = LW_CUT_5AXIS;
				if (fromt) motype = LW_CUT_POSITION;
				else if (LW_mach_type == LW_LATHE)
				{
					motype = LW_CUT_LATHE;
					if (ithrd)
					{
						nval = 11; axis[10] = angle;
						motype = LW_CUT_THREAD;
					}
				}
			}
			ul_ipv_calc_axis(mblock->axis,mblock->type,axis);
			for (i=0;i<nval;i++) rdata[i] = axis[i];
/*
...........Move axes to perform cut
*/
			ul_ipv_move_assemblies(axis,flush,fromt);
		}
/*
........Standard simulation
*/
		else
		{
			if (fromt)
			{
				motype = LW_CUT_POSITION;
				um_vctovc(tool_pos,rdata); nval = 3;
				LiViToolSetPosition(LW_tool[0],tool_pos);
				if (LW_mach_type != LW_LATHE)
				{
					um_vctovc(tool_vec,&rdata[3]); nval = 6;
					LiViToolSetAxis(LW_tool[0],tool_vec);
				}
			}
			else if (cirt != 0)
			{
				if (LW_mach_type != LW_LATHE)
				{
					if (cirt == 1)
					{
						motype = LW_CUT_ARC;
						um_vctovc(cirrec,rdata); rdata[3] = cirrec[7]; nval = 4;
						LiViToolDoArcCut(LW_tool[0],cirrec,cirrec[7]);
					}
					else
					{
						motype = LW_CUT_VERTICAL_ARC;
						um_vctovc(&cirrec[3],rdata); um_vctovc(cirrec,&rdata[3]);
						rdata[6] = cirrec[7]; nval = 7;
						LiViToolDoVerticalArcCut(LW_tool[0],&cirrec[3],cirrec,
							cirrec[7]);
					}
				}
				else if (LW_mach_type == LW_LATHE)
				{
					rnum = cirrec[0];
					cirrec[0] = cirrec[1];
					cirrec[1] = cirrec[2];
					cirrec[2] = rnum;
					motype = LW_CUT_LATHE_ARC;
					um_vctovc(cirrec,rdata); rdata[3] = cirrec[7]; nval = 4;
					LiViTurningToolDoSmoothArcCut(LW_tool[0],cirrec,cirrec[7]);
				}
				if (flush) ul_ipv_flush();
			}
			else
			{
				if (LW_mach_type != LW_LATHE)
				{
					if (ipt[LW_act_tool[0]].type == NCL_CUTTER_BLADE)
					{
						um_unitvc(tool_vec,vc1);
						um_unitvc(mblock->vfwd,vc2);
						if (um_mag(vc2) == 0. || um_vcparall(vc2,tool_vec))
						{
							um_vctovc(Sfwd_vec,vc2);
							if (um_vcparall(vc2,tool_vec)) um_perpvc(tool_vec,vc2);
						}
						um_cross(vc1,vc2,vtmp);
						um_unitvc(vtmp,vtmp);
						um_cross(vtmp,vc1,vc2);
						um_vctovc(vc2,Sfwd_vec);
						motype = LW_CUT_BLADE;
						um_vctovc(tool_pos,rdata); um_vctovc(vc2,&rdata[3]);
						um_vctovc(vc1,&rdata[6]); nval = 9;
						LiViToolDefineComplexCut(LW_tool[0],tool_pos,vc2,vc1);
					}
					else
					{
						motype = LW_CUT_5AXIS;
						um_vctovc(tool_pos,rdata); um_vctovc(tool_vec,&rdata[3]);
						nval = 6;
						LiViToolDefine5AxisLinearCut(LW_tool[0],tool_pos,tool_vec);
					}
					LiViPerformCut(LW_session[LW_mach_mode]);
				}
				else
				{
					if (ithrd)
					{
						angle = zdelt / fed2 * 360.;
						if (LW_mot_attr != UU_NULL && LW_mot_attr->sp_mode == 0)
							angle = -angle;
						motype = LW_CUT_THREAD;
						um_vctovc(tool_pos,rdata); rdata[3] = angle; nval = 4;
						LiViLatheSetOrientation(LW_lathe,0.);
						LiViTurningToolDoLinearCut(LW_tool[0],tool_pos,angle);
					}
					else
					{
						motype = LW_CUT_LATHE;
						um_vctovc(tool_pos,rdata); nval = 3;
						LiViTurningToolDoSmoothLinearCut(LW_tool[0],tool_pos);
					}
				}
			}
/*
.....Moving part is active
.....Adjust view to tool
*/
			if (moving_part == 1 && flush)
				ul_ipv_view_taxis(tool_pos,tool_vec);
/*
.....Flush the output graphics
*/
			else
				if (flush) ul_ipv_flush();
/*
.....Store tool from position
*/
			for (i=0;i<6;i++) fdata[i] = LW_tool_pos[i];
		}
/*
.....Check for clash
.....Moved here because the RED collision solids
.....would revert to their original color after
.....pressing the VIEW button
.....Bobby - 04/16/15
*/
	if (LW_clash_flag) ul_ipv_clash_form(LW_clash_record);
/*
.....Push the motion onto the UNDO stack
*/
		if (motype != LW_CUT_NONE)
			ul_ipv_mot_stack_push(fdata,rdata,nval,cutn,Stool_colors[0],
				motype);
/*
.....Store cut numbers for multiple active tools
*/
		if (LW_spindle_num > 0)
		{
/*
........Get cut number
*/
			LiSessionGetProperty(LW_session[LW_mach_mode],
				LI_SESS_PROP_MW_CUT_NUMBER,&stuff);
			ncutn = LiDataGetNat32(&stuff);
/*
........Store the attributes and cut numbers
........Will need to determine tool that made the cut
........After MachineWorks implements this feature
........For right now, just store separate attributes
........for each cut number, so save/restore session
........can restore correct face colors
*/
			for (j=cutn+1;j<ncutn;j++)
			{
				S_store_mattr(mdata,mattr,j);
			}
		}
			
	}
/*
.....RapidCut
*/
	else
	{
		if (!um_cceqcc(tool_vec,&LW_tool_pos[3]))
		{
			ul_ipv_write_log("RAPIDCUT- Multi-axis moves not allowed in RapidCut.");
			LW_errors++;
		}
		if (fromt) LiRvToolSetPosition(LW_tool[0],tool_pos);
		else LiRvToolDoLinearCut(LW_tool[0],tool_pos);
	}
/*
.....Save the current tool position
*/
	for (i=0;i<3;i++)
	{
		LW_last_tool_pos[i] = LW_tool_pos[i];
		LW_tool_pos[i] = tool_pos[i];
		LW_last_tool_pos[i+3] = LW_tool_pos[i+3];
		LW_tool_pos[i+3] = tool_vec[i];
	}
/*
.....Check for clash
*/
/*	if (LW_clash_flag) ul_ipv_clash_form(LW_clash_record);*/
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_cutters(pos,cutr,mdata,mattr)
**			Calculates a MachineWorks (IPV) cutter for each active spindle
**       (wrapper routine for 'ul_ipv_cutter').
**    PARAMETERS
**       INPUT  :
**          (see ul_ipv_cutter defined after this routine)
**       OUTPUT :
**          none.
**    RETURNS      :
**				UU_TRUE if a new cutter was selected.  UU_FALSE otherwise,
**				even if cutter was calculated due to modified attributes.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ul_ipv_cutters(pos,cutr,mdata,mattr,scan)
UU_REAL pos[6],cutr[];
UN_mot_data *mdata;
UN_mot_attr *mattr;
int scan;
{
	int i,irap,tool_color,cut_color,clash_color,clash_type;
	UU_LOGICAL status,istat;
	UU_REAL fed;
	UN_cutter_list *cpt;
/*
.....Loop through spindles to define cutter
*/
	status = UU_FALSE;
	for (i=0;i<LW_spindle_nload;i++)
	{
		istat = ul_ipv_cutter(pos,cutr,mdata,mattr,scan,LW_spindle_load[i]);
		if (istat) status = UU_TRUE;
	}
/*
.....Check for RAPID or fast feedrate
*/
	if (scan != 4)
	{
		irap = 0;
		cpt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
		if (mdata != UU_NULL)
		{
			if (mdata->fr_mode == 0)
				irap = 1;
			else if (mdata->fr_mode == 1)
				fed = mdata->fr_val;
			else
				fed = mdata->fr_val * mattr->sp_val;
		}
		else
			fed = 0.;
		if (cpt[LW_act_tool[0]].rapid != 0. &&
			fed >= cpt[LW_act_tool[0]].rapid) irap = 1;
/*
.....Rapid move
*/
		if (irap == 1 && LW_clash_material.rapid != 0)
		{
			tool_color = LW_clash_material.rapid;
			cut_color = LW_clash_material.rapid;
			clash_color = LW_clash_material.rapid;
			if (LW_clash_stop[0] || LW_clash_log[0])
				clash_type = LI_MW_INTERACT_CLASH_CUT;
			else
				clash_type = LI_MW_INTERACT_CUT;
		}
/*
.....Cutting move
*/
		else
		{
			tool_color = LW_tool_material;
			if (tool_color == -1) tool_color = LW_cut_material;
			cut_color = LW_cut_material;
			clash_color = LW_clash_material.fixture;
			clash_type = LI_MW_INTERACT_CUT;
		}
/*
.....Set the tool attributes
*/
		for (i=0;i<LW_spindle_num;i++)
		{
			if (LW_tool[i] != 0)
			{
				ul_ipv_set_tool_colors(tool_color,cut_color,clash_color,clash_type,
					i);
			}
		}
/*
.....Make cutter important solid
*/
		if (status) ul_ipv_display_obstruct(UU_FALSE);
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_cutter(pos,cutr,mdata,mattr,scan,ispin)
**			Calculates a MachineWorks (IPV) cutter for display.
**    PARAMETERS
**       INPUT  :
**				pos     = Current tool position and axis.
**				cutr    = Cutter parameters.
**          mdata   = Motion parameters for current move.
**          mattr   = Motion attributes for current move.
**          scan    = -1 = Stepping through NCLIOPV motion,
**                    4 = Scanning clfile to place tools in turret,
**                    otherwise load tool.
**          ispin   = Spindle number to load cutter into.
**       OUTPUT :
**          none.
**    RETURNS      :
**				UU_TRUE if a new cutter was selected.  UU_FALSE otherwise,
**				even if cutter was calculated due to modified attributes.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ul_ipv_cutter(pos,cutr,mdata,mattr,scan,ispin)
UU_REAL pos[6],cutr[];
UN_mot_data *mdata;
UN_mot_attr *mattr;
int scan,ispin;
{
	int i,j,n,icol,stat,npts,ntry;
	UU_LOGICAL iret;
	static UU_LOGICAL ifl = UU_TRUE;
	char sbuf[80],buf[132],cbuf[132];
	UU_REAL tol_inc,ofs[3];
	UM_coord *pts;
	LtDouble junk;
	LtDoublePoint tool_pos,origpt;
	LtDoubleVector tool_vec,xaxis,yaxis,zaxis,vec;
	LtData stuff;
	LtPrim prim,tool,prims[LW_MAX_HOLDER],holder[LW_MAX_HOLDER];
	LtSessionPrim tsav;
	UN_cutter_list *cpt;
	LtProfile symprof;
	LtData transp;

	UN_motseg_cutter cutseg;
/*
.....Initialize routine
*/
	ncl_cutter_get(&cutseg,UN_MOTSEG_ACTIVE);
	cpt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
	ofs[0] = ofs[1] = ofs[2] = 0.;
/*
.....See if Cutter has changed
*/
	iret = UU_FALSE;
	if (LW_act_tool[ispin] == -1 || LW_act_tool[ispin] >= LW_ntool) goto done;
	if ((Slast_act[ispin] != LW_act_tool[ispin] ||
		LW_tool[ispin] == (LtSessionPrim)0) && LW_act_tool[ispin] != -1)
	{
		ifl = UU_TRUE;
		iret = UU_TRUE;
		if (scan != -1)
		{
			ul_ipv_write_log(" ");
			if (LW_spindle_num > 1)
			{
				sprintf(buf,"SPINDLE - %d",LW_spindle_ix[ispin]);
				ul_ipv_write_log(buf);
			}
			ul_ipv_format_tool(cbuf,LW_act_tool[ispin]);
			sprintf(buf,"TOOL    - %s",cbuf);
			ul_ipv_write_log(buf);
		}
	}
	if (!ifl)
	{
		if (LW_toler != Stoler) ifl = UU_TRUE;
		if (LW_maxang != Smaxang) ifl = UU_TRUE;
		for (i=0;i<3;i++)
		{
			if (LW_translucency[i] != Stranslucency[ispin][i]) ifl = UU_TRUE;
			if (cpt[LW_act_tool[ispin]].edge[i] != Sedge[ispin][i]) ifl = UU_TRUE;
			if (cpt[LW_act_tool[ispin]].edge_color[i] != Sedgcol[ispin][i])
				ifl = UU_TRUE;
			for (j=0;j<4;j++)
				if (cpt[LW_act_tool[ispin]].parms[i][j] != Sparms[ispin][i][j])
					ifl = UU_TRUE;
		}
	}
	else if (LW_act_tool[ispin] == -1) ifl = UU_FALSE;
/*
.....Save current settings
*/
	if (ifl)
	{
		Stoler = LW_toler;
		Smaxang = LW_maxang;
		for (i=0;i<3;i++)
		{
			Stranslucency[ispin][i] = LW_translucency[i];
			Sedge[ispin][i] = cpt[LW_act_tool[ispin]].edge[i];
			Sedgcol[ispin][i] = cpt[LW_act_tool[ispin]].edge_color[i];
			for (j=0;j<4;j++)
				Sparms[ispin][i][j] = cpt[LW_act_tool[ispin]].parms[i][j];
		}
	}
/*
.....Lathe simulation active
.....just rotate turret
*/
	if (LW_mach_simul && LW_is_lathe)
	{
 		if (scan != 4)
		{
			if (ul_ipv_turret_same(&cpt[LW_act_tool[ispin]],UU_TRUE))
			{
				if (ifl)
				{
					ul_ipv_mot_stack_cutter(cutr,mdata,mattr,ofs[2],ispin);
					ul_ipv_position_turret(cpt[LW_act_tool[ispin]].tlno,UU_FALSE);
				}
				ifl = UU_FALSE;
			}
			else
				ifl = UU_TRUE;
		}
		else
		{
			if (ul_ipv_turret_used(cpt[LW_act_tool[ispin]].tlno)) ifl = UU_FALSE;
		}
	}
/*
.....Calculate lathe cutter
*/
	if (ifl && cpt[LW_act_tool[ispin]].type == NCL_CUTTER_LATHE)
	{
		for (i=0;i<3;i++) cutdef[0].cutfl[i] = cpt[LW_act_tool[ispin]].ctype[i];
		stat = ul_ipv_lathe_cutter(pos,cutr,&cpt[LW_act_tool[ispin]],&cutseg,
			scan,ispin);
		if (stat != UU_SUCCESS) goto done;
	}
/*
.....Recalculate cutter
*/
	else if (ifl)
	{
		ntry = 0;
retry:;
		tsav = LW_tool[ispin];
		if (LW_tool[ispin] != (LtSessionPrim)0)
		{
			if (LW_mach_mode == LW_VISICUT && scan != 4)
				ul_ipv_deselect_tool();
		}
		for (i=0;i<7;i++) cutdef[0].cutr[i] = cutr[i];
/*
.....Set cutter tolerance
*/
		if (LW_mach_mode == LW_VISICUT)
		{
			junk = LW_maxang;
			LiDataSetDouble(&stuff,junk);
			LiSessionSetProperty(LW_session[LW_mach_mode],
				LI_SESS_PROP_FX_MAX_INTERP_ANG,&stuff);
			junk = LW_toler;
			LiDataSetDouble(&stuff,junk);
			LiSessionSetProperty(LW_session[LW_mach_mode],
				LI_SESS_PROP_VI_FACET_TOLERANCE,&stuff);
			LiSessionSetProperty(LW_session[LW_mach_mode],
				LI_SESS_PROP_VI_PATH_TOLERANCE,&stuff);
			LiSessionSetProperty(LW_session[LW_mach_mode],
				LI_SESS_PROP_VI_PATH_FIT_TOL,&stuff);
		}
/*
.....Tessellated cutter
*/
		if (cpt[LW_act_tool[ispin]].ctype[0] == 2)
		{
			ofs[0] = ofs[1] = 0.;
			n = S_symbol_prim(&cpt[LW_act_tool[ispin]].symbol[0],ofs,&prim,1,1);
			if (prim == 0) LW_tool[ispin] = 0;
			else
			{
				tool = LiPrimitiveCreatePolygonTool(prim,LI_MW_SIMPLIFY_NOTHING,
					0.,0.);
				LiPrimitiveDestroy(prim);
			}
		}
/*
.....Blade cutter
*/
		else if (cpt[LW_act_tool[ispin]].type == NCL_CUTTER_BLADE)
		{
			pts = (UM_coord *)UU_LIST_ARRAY(&cutseg.cutsym->geo->curve);
			npts = UU_LIST_LENGTH(&cutseg.cutsym->geo->curve);
			S_mill_profile(&symprof,pts,npts,&ofs[2],NCL_CUTTER_BLADE);
			if (symprof == 0) LW_tool[ispin] = 0;
			else
			{
				LiPointInitialise(origpt,0.,cutdef[0].cutr[5]/2.,0.);
				LiVectorInitialise(xaxis,1.,0.,0.);
				LiVectorInitialise(yaxis,0.,0.,cutdef[0].cutr[5]);
				LiVectorInitialise(zaxis,0.,0.,1.);
				prim = LiPrimitiveCreateExtrudedSolid(symprof,yaxis,xaxis,zaxis,
					origpt,LW_toler,LI_MW_SOLID_TYPE_GENERAL);
				tool = LiPrimitiveCreatePolygonTool(prim,
					LI_MW_SIMPLIFY_FIT_ARCS,0.,0.);
				LiProfileDestroy(symprof);
				LiPrimitiveDestroy(prim);
				cutdef[0].cutr[2] = ofs[2];
			}
		}
/*
.....Determine if flat, bull, or ball cutter
.....Use MachineWorks to draw cutter if so
.....Use angle (cutr[3]) & bar flag (cutr[8])
.....to determine cutter shape
*/
		else if (cpt[LW_act_tool[ispin]].ctype[0] <= 1 &&
			cutr[3]+cutr[8] == 0. && cutr[2] > cutr[1])
		{
			if (cutr[2] > ofs[2]) ofs[2] = cutr[2];
/*
........Flat end tool
*/
			if (cutr[1] == 0.)
				tool = LiPrimitiveCreateFlatEndTool(cutr[2],cutr[0]/2.);
/*
........Ball end tool
*/
			else if (cutr[1] == cutr[0]/2.)
				tool = LiPrimitiveCreateBallEndTool(cutr[2],cutr[0]/2.);
/*
........Bull nose tool
*/
			else
				tool = LiPrimitiveCreateBullNoseTool(cutr[2],cutr[0]/2.,cutr[1]);
		}
/*
.....Cutter profile
*/
		else
		{
			if (cutseg.cutsym != UU_NULL)
			{
				pts = (UM_coord *)UU_LIST_ARRAY(&cutseg.cutsym->geo->curve);
				npts = UU_LIST_LENGTH(&cutseg.cutsym->geo->curve);
				S_mill_profile(&symprof,pts,npts,&ofs[2],NCL_CUTTER_MILL);
				tool = LiPrimitiveCreateTool(symprof,LI_MW_SIMPLIFY_FIT_ARCS,0.,0.);
				LiProfileDestroy(symprof);
				cutdef[0].cutr[2] = ofs[2];
			}
		}
/*
.....Error creating tool
*/
		if (tool == 0)
		{
			sprintf(sbuf,"CUTTER  - Failed to create tool %d",
				LW_act_tool[ispin]+1);
			ul_ipv_write_log(sbuf);
			goto done;
		}
/*
.....Create the tool solid
*/
		LW_tool[ispin] = LiSessionAddPrim(LW_session[LW_mach_mode],tool);
		Stool_colors[ispin][0] = LW_tool_material;
		if (Stool_colors[ispin][0] == -1)
			Stool_colors[ispin][0] = LW_cut_material;
		Stool_colors[ispin][1] = LW_cut_material;
		Stool_colors[ispin][2] = LW_clash_material.fixture;
		Stool_colors[ispin][3] = LI_MW_INTERACT_CUT;
		Stool_colors[ispin][4] = LW_translucency[0];
		Stool_colors[ispin][5] = cpt[LW_act_tool[ispin]].edge[0];
		Stool_colors[ispin][6] = cpt[LW_act_tool[ispin]].edge_color[0];
/*
.....Set the tool position
*/
		tool_pos[0] = pos[0] ; tool_pos[1] = pos[1] ; tool_pos[2] = pos[2];
		tool_vec[0] = pos[3] ; tool_vec[1] = pos[4] ; tool_vec[2] = pos[5];
/*
.....Tool Shank
*/
		if (cpt[LW_act_tool[ispin]].ctype[1] > 0)
		{
/*
.........Generate the shank profile
*/
			pts = (UM_coord *)UU_LIST_ARRAY(&cutseg.shank->geo->curve);
			npts = UU_LIST_LENGTH(&cutseg.shank->geo->curve);
			if (cpt[LW_act_tool[ispin]].ctype[1] == 1)
				ofs[2] = ofs[2] + cpt[LW_act_tool[ispin]].parms[1][3];
			else
				ofs[2] = ofs[2] + cpt[LW_act_tool[ispin]].parms[1][0];
/*
........Tessellated shank
*/
			if (cpt[LW_act_tool[ispin]].ctype[1] == 2)
			{
				ofs[0] = ofs[1] = 0.;
				n = S_symbol_prim(&cpt[LW_act_tool[ispin]].symbol[1],ofs,&prim,1,1);
				if (prim == 0) LW_shank[ispin] = 0;
				else
				{
					tool = LiPrimitiveCreatePolygonHolder(prim,
						LI_MW_SIMPLIFY_NOTHING,0.,0.);
					LiPrimitiveDestroy(prim);
				}
			}
/*
........Create shank solid
*/
			else
			{
				S_mill_profile(&symprof,pts,npts,&ofs[2],NCL_CUTTER_MILL);
				if (symprof != (LtProfile)0)
				{
					tool = LiPrimitiveCreateToolHolder(symprof,
						LI_MW_SIMPLIFY_FULL,0.,0.);
					LiProfileDestroy(symprof);
				}
				else
					tool = 0;
			}
			if (tool == 0)
			{
				sprintf(sbuf,"CUTTER  - Failed to create tool shank %d\n",
					LW_act_tool[ispin]+1);
				ul_ipv_write_log(sbuf);
			}
			else
			{
				LW_shank[ispin] = LiSessionAddPrim(LW_session[LW_mach_mode],tool);
				Sshank_colors[ispin][0] = cpt[LW_act_tool[ispin]].color[1];
				if (Sshank_colors[ispin][0] == -1)
				{
					Sshank_colors[ispin][0] = LW_default_tool.shank_color;
					if (Sshank_colors[ispin][0] == -1)
						Sshank_colors[ispin][0] = Stool_colors[ispin][0];
				}
				if (cpt[LW_act_tool[ispin]].shank_clash == 1)
					Sshank_colors[ispin][1] = LW_clash_material.holder;
				else
					Sshank_colors[ispin][1] = LW_cut_material;
				Sshank_colors[ispin][2] = cpt[LW_act_tool[ispin]].shank_clash;
				Sshank_colors[ispin][3] = LW_translucency[1];
				Sshank_colors[ispin][4] = cpt[LW_act_tool[ispin]].edge[1];
				Sshank_colors[ispin][5] = cpt[LW_act_tool[ispin]].edge_color[1];
			}
		}
/*
.....Tool Holder
*/
		if (cpt[LW_act_tool[ispin]].ctype[2] > 0)
		{
			pts = (UM_coord *)UU_LIST_ARRAY(&cutseg.holder->geo->curve);
			npts = UU_LIST_LENGTH(&cutseg.holder->geo->curve);
			if (cpt[LW_act_tool[ispin]].ctype[2] == 1)
				ofs[2] = ofs[2] + cpt[LW_act_tool[ispin]].parms[2][3];
			else
				ofs[2] = ofs[2] + cpt[LW_act_tool[ispin]].parms[2][0];
/*
........Tessellated holder
*/
			if (cpt[LW_act_tool[ispin]].ctype[2] == 2)
			{
				ofs[0] = ofs[1] = 0.;
				LW_num_holder[ispin] =
					S_symbol_prim(&cpt[LW_act_tool[ispin]].symbol[2],ofs,prims,
						LW_MAX_HOLDER,1);
				for (i=0;i<LW_num_holder[ispin];i++)
				{
					holder[i] =
						LiPrimitiveCreatePolygonHolder(prims[i],
							LI_MW_SIMPLIFY_NOTHING,0.,0.);
					LiPrimitiveDestroy(prims[i]);
				}
			}
/*
........Create holder solid
*/
			else
			{
				S_mill_profile(&symprof,pts,npts,&ofs[2],NCL_CUTTER_MILL);
				if (symprof != (LtProfile)0)
				{
					holder[0] = LiPrimitiveCreateToolHolder(symprof,
						LI_MW_SIMPLIFY_FULL,0.,0.);
					LiProfileDestroy(symprof);
					LW_num_holder[ispin] = 1;
				}
				else
					holder[0] = 0;
			}
			for (i=0;i<LW_num_holder[ispin];i++)
			{
				if (holder[i] == 0)
				{
					sprintf(sbuf,"CUTTER  - Failed to create tool holder %d.%d\n",
						LW_act_tool[ispin]+1,i);
					ul_ipv_write_log(sbuf);
				}
				else
				{
					LW_holder[ispin][i] = LiSessionAddPrim(LW_session[LW_mach_mode],
						holder[i]);
				}
			}
			Sholder_colors[ispin][0] = cpt[LW_act_tool[ispin]].color[2];
			if (Sholder_colors[ispin][0] == -1)
			{
				Sholder_colors[ispin][0] = LW_default_tool.hold_color;
				if (Sholder_colors[ispin][0] == -1)
					Sholder_colors[ispin][0] = Stool_colors[ispin][0];
			}
			Sholder_colors[ispin][1] = LW_clash_material.holder;
			Sholder_colors[ispin][2] = LW_translucency[2];
			Sholder_colors[ispin][3] = cpt[LW_act_tool[ispin]].edge[2];
			Sholder_colors[ispin][4] = cpt[LW_act_tool[ispin]].edge_color[2];
		}
/*
.....Set tool properties
*/
		ul_ipv_reset_tool_props();
/*
........Visicut
*/
		if (LW_mach_mode == LW_VISICUT)
		{
/*
........Tool
*/
			stat = LiViToolSelect(LW_tool[ispin],TRUE);
/*
........Shank
*/
			if (stat == 0 && LW_shank[ispin] != 0)
			{
				stat = LiViToolAttachHolder(LW_tool[ispin],LW_shank[ispin],0.,TRUE);
				if (stat != 0)
				{
					LW_shank[ispin] = 0;
					sprintf(sbuf,"CUTTER  - Failed to attach shank %d",
						LW_act_tool[ispin]+1);
					ul_ipv_write_log(sbuf);
				}
			}
/*
........Holder
*/
			if (stat == 0 && LW_num_holder[ispin] != 0)
			{
				for (i=0;i<LW_num_holder[ispin];i++)
				{
					if (LW_holder[ispin][i] != 0)
					{
						stat = LiViToolAttachHolder(LW_tool[ispin],
							LW_holder[ispin][i],0.,TRUE);
						if (stat != 0)
						{
							LW_holder[ispin][i] = 0;
							sprintf(sbuf,"CUTTER  - Failed to attach holder %d.%d",
								LW_act_tool[ispin]+1,i);
							ul_ipv_write_log(sbuf);
						}
					}
				}
			}
/*
........Could not create cutter
........Try altering tolerance
*/
			if (stat != 0)
			{
				if (ntry == 8)
				{
					sprintf(sbuf,"CUTTER  - Failed to create tool %d",
						LW_act_tool[ispin]+1);
					ul_ipv_write_log(sbuf);
					goto done;
				}
				else if (ntry == 0)
				{
					sprintf(sbuf,
						"CUTTER  - Could not create tool %d.  Altering Cut Tolerance.",
						LW_act_tool[ispin]+1);
						ul_ipv_write_log(sbuf);
					tol_inc = LW_toler / 4.;
					LW_toler = LW_toler / 10.;
				}
				else
				{
					LW_toler = LW_toler + tol_inc;
				}
				ntry++;
				goto retry;
			}
/*
.....Set clash types
*/
			ul_ipv_tool_clash();
/*
.....Attach tool to turret
*/
			if (cpt[LW_act_tool[ispin]].tlen > 0.)
				ofs[2] = cpt[LW_act_tool[ispin]].tlen +
					cpt[LW_act_tool[ispin]].tlofs;
			if (LW_mach_simul)
			{
				if (LW_is_lathe)
				{
					vec[0] = 1.; vec[1] = 0.; vec[2] = 0.;
					ul_ipv_lathe_tool_assembly(tool_pos,
						cpt[LW_act_tool[ispin]].clrec,cpt[LW_act_tool[ispin]].tlno,
						vec,ofs[2],NCL_CUTTER_MILL);
					if (scan != 4)
						ul_ipv_position_turret(cpt[LW_act_tool[ispin]].tlno,UU_FALSE);
				}
/*
.....Attach tool to head
*/
				else
					ul_ipv_tool_assembly(tool_pos,tool_vec,ofs[2],ispin);
			}
/*
.....Position cutter
*/
			else
			{
				LiViToolSetPosition(LW_tool[0],tool_pos);
				if (!LW_is_lathe) LiViToolSetAxis(LW_tool[0],tool_vec);
			}
		}
/*
.....Rapidcut
*/
		else
		{
			stat = LiRvToolSelect(LW_tool[ispin],TRUE);
/*
........Could not create tool
........Try Convex tool
*/
			if (stat != 0)
			{
				LiDataSetBoolean(&stuff,TRUE);
				LiSessionSetProperty(LW_session[LW_mach_mode],
					LI_CONTROL_RV_CONVEXIFY_TOOLS,&stuff);
				stat = LiRvToolSelect(LW_tool[ispin],TRUE);
				LiDataSetBoolean(&stuff,FALSE);
				LiSessionSetProperty(LW_session[LW_mach_mode],
					LI_CONTROL_RV_CONVEXIFY_TOOLS,&stuff);
				if (stat == 0)
				{
					sprintf(sbuf,"CUTTER  - Convex tool created for tool %d",
						LW_act_tool[ispin]+1);
					ul_ipv_write_log(sbuf);
				}
				else
				{
					sprintf(sbuf,"CUTTER  - Failed to create tool %d",
						LW_act_tool[ispin]+1);
					ul_ipv_write_log(sbuf);
					goto done;
				}
			}
/*
........Attach holder
*/
			LiRvToolAttachHolder(LW_tool[ispin],LW_holder[ispin][0],0.,TRUE);
			if (stat == 0)
				LiSessionPrimSetInteractType(LW_holder[ispin][0],
					LI_MW_SOLID_TYPE_STOCK,LI_MW_INTERACT_CLASH_CUT);
			else
			{
				LW_holder[ispin][0] = 0;
				sprintf(sbuf,"CUTTER  - Failed to attach holder %d",
					LW_act_tool[ispin]+1);
				ul_ipv_write_log(sbuf);
			}
/*
........Set tool position
*/
			LiRvToolSetPosition(LW_tool[ispin],tool_pos);
		}
	}
/*
.....Store calculated cutter in motion stack
*/
	if (scan != 4)
	{
		if (scan != -1 && scan != 4 && ifl)
			ul_ipv_mot_stack_cutter(cutr,mdata,mattr,ofs[2],ispin);
	}
/*
.....End of routine
*/
done:;
	Slast_act[ispin] = LW_act_tool[ispin];
	ifl = UU_FALSE;
	return(iret);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_lathe_cutter(pos,cutr,cpt,cutseg,scan,ispin)
**			Calculates a MachineWorks (IPV) lathe cutter for display.
**    PARAMETERS
**       INPUT  :
**				pos     = Current tool position and axis.
**				cutr    = Cutter parameters.
**          cpt     = Active tool.
**          cutseg  = Active cutter structure.
**          scan    = 4 = Scanning clfile to place tools in turret,
**                    otherwise load tool.
**          ispin   = Spindle number to load cutter into.
**       OUTPUT :
**          none.
**    RETURNS      :
**				UU_TRUE if a new cutter was selected.  UU_FALSE otherwise,
**				even if cutter was calculated due to modified attributes.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_lathe_cutter(pos,cutr,cpt,cutseg,scan,ispin)
UU_REAL pos[6],cutr[];
UN_cutter_list *cpt;
UN_motseg_cutter *cutseg;
int scan,ispin;
{
	int i,icol,tcol,stat,status,npts,n,ix0,ix1;
	UU_LOGICAL irot;
	char sbuf[80];
	UM_coord *pts,tofs,ptx;
	LtPrim prim,tool,prims[LW_MAX_HOLDER],holder[LW_MAX_HOLDER];
	LtProfile profile;
	LtDouble ofs[3],junk,ymin,ymax,zofs,zhgt;
	LtDoublePoint tool_pos;
	LtDoubleVector vec;
	LtData stuff;
	LtData transp;
	static LtDoubleVector x_axis={0.,1.,0.};
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
	profile = 0;
/*
.....Unload active tool
*/
	if (LW_tool[ispin] != (LtSessionPrim)0)
	{
		vec[0] = 0.; vec[1] = 1.; vec[2] = 0.;
		if (scan != 4) ul_ipv_deselect_tool();
	}
/*
.....Set cutter tolerance
*/
	junk = LW_toler;
	LiDataSetDouble(&stuff,junk);
	LiSessionSetProperty(LW_session[LW_mach_mode],
		LI_SESS_PROP_VI_FACET_TOLERANCE,&stuff);
	LiSessionSetProperty(LW_session[LW_mach_mode],
		LI_SESS_PROP_VI_PATH_TOLERANCE,&stuff);
	LiSessionSetProperty(LW_session[LW_mach_mode],
		LI_SESS_PROP_VI_PATH_FIT_TOL,&stuff);
/*
.....Determine the cutter height
*/
	if (cpt->ctype[0] <=0)
	{
		ymax = 0.;
		ymin = -cutr[2];
	}
	else
	{
		ymin = -(cpt->parms[0][2]+cpt->parms[0][3]);
		ymax = cpt->parms[0][2];
		if (ymin == ymax) ymin = ymax - cutr[2];
	}
/*
.....Tessellated cutter
*/
	if (cpt->ctype[0] == 2)
	{
		ofs[0] = ofs[1] = 0.;
		ofs[2] = ymax;
		n = S_symbol_prim(&cpt->symbol[0],ofs,&prim,1,2);
		if (prim == 0) LW_tool[ispin] = 0;
		else
		{
			tool = LiPrimitiveCreateTurningTool(UU_NULL,prim,ymin,ymax,
				LI_MW_SIMPLIFY_NOTHING,.001,.001);
			LiPrimitiveDestroy(prim);
		}
	}
/*
.....No cutter defined
*/
	else if (cutseg->cutsym == UU_NULL)
			LW_tool[ispin] = 0;
/*
.....Create standard cutter
*/
	else
	{
		pts = (UM_coord *)UU_LIST_ARRAY(&cutseg->cutsym->geo->curve);
		npts = UU_LIST_LENGTH(&cutseg->cutsym->geo->curve);
		ofs[0] = ofs[1] = 0.;
		S_lathe_profile(&profile,pts,npts,&ofs);
		tool = LiPrimitiveCreateTurningTool(profile,UU_NULL,ymin,ymax,
			LI_MW_SIMPLIFY_STANDARD_TOOL,.001,.001);
		LiProfileDestroy(profile);
	}
/*
.....Error creating tool
*/
	if (tool == 0)
	{
		sprintf(sbuf,"CUTTER  - Failed to create tool %d",LW_act_tool[0]+1);
		ul_ipv_write_log(sbuf);
		goto done;
	}
/*
.....Create the tool solid
*/
	LW_tool[ispin] = LiSessionAddPrim(LW_session[LW_mach_mode],tool);
	Stool_colors[ispin][0] = LW_tool_material;
	if (Stool_colors[ispin][0] == -1) Stool_colors[ispin][0] = LW_cut_material;
	Stool_colors[ispin][1] = LW_cut_material;
	Stool_colors[ispin][2] = LW_clash_material.fixture;
	Stool_colors[ispin][3] = LI_MW_INTERACT_CUT;
	Stool_colors[ispin][4] = LW_translucency[0];
	Stool_colors[ispin][5] = cpt->edge[0];
	Stool_colors[ispin][6] = cpt->edge_color[0];
/*
	tcol = LW_tool_material;
	if (tcol == -1) tcol = LW_cut_material;
	LiDataSetGenericPtr(&stuff,LW_material[LW_tool_material]);
	LiSessionPrimSetVisualProperty(LW_tool[ispin],LI_MW_VIS_PROP_MATERIAL,
		&stuff);
	LiDataSetGenericPtr(&stuff,LW_material[LW_cut_material]);
	LiSessionPrimSetProperty(LW_tool[ispin],LI_SPRIM_PROP_CUT_MATERIAL,&stuff);
	LiDataSetGenericPtr(&stuff,LW_material[LW_clash_material.fixture]);
	LiSessionPrimSetProperty(LW_tool[ispin],LI_SPRIM_PROP_CLASH_MATERIAL,&stuff);
	LiDataSetNat32(&transp,100-LW_translucency[0]);
	LiSessionPrimSetVisualProperty(LW_tool[ispin],LI_MW_VIS_PROP_TRANSPARENCY,
		&transp);
*/
/*
.....Set the tool position
*/
	tool_pos[0] = pos[1] ; tool_pos[1] = pos[2] ; tool_pos[2] = pos[0];
	vec[0] = 1.; vec[1] = 0.; vec[2] = 0.;
/*
.....Tool Shank
*/
	if (cpt->ctype[1] > 0)
	{
/*
........Determine if shank is rotated
*/
		irot = UU_FALSE;
		vec[0] = cutseg->shank->geo->axis[1];
		vec[1] = cutseg->shank->geo->axis[2];
		vec[2] = cutseg->shank->geo->axis[0];
		if (vec[2] > vec[0]+vec[1]) irot = UU_TRUE;
/*
.........Generate the shank profile
*/
		pts = (UM_coord *)UU_LIST_ARRAY(&cutseg->shank->geo->curve);
		npts = UU_LIST_LENGTH(&cutseg->shank->geo->curve);
		if (cpt->ctype[1] == 1)
		{
			if (irot) ofs[0] = ofs[0] + cpt->parms[1][3];
			else ofs[1] = ofs[1] + cpt->parms[1][3];
			ymax = ymin;
			ymin = ymax - cpt->parms[1][2];
		}
		else
		{
			ofs[0] = ofs[0] + cpt->parms[1][0];
			ofs[1] = ofs[1] + cpt->parms[1][1];
			ymax = ymin - cpt->parms[1][2];
			ymin = ymax - cpt->parms[1][3];
		}
/*
........Tessellated shank
*/
		if (cpt->ctype[1] == 2)
		{
			tofs[2] = ofs[0];
			tofs[0] = ofs[1];
			tofs[1] = ymax;
			n = S_symbol_prim(&cpt->symbol[1],tofs,&prim,1,2);
			if (prim == 0) LW_shank[ispin] = 0;
			else
			{
				tool = LiPrimitiveCreateTurningHolder(UU_NULL,prim,ymin,ymax,
						LI_MW_SIMPLIFY_NOTHING,.001,.001);
				LiPrimitiveDestroy(prim);
			}
		}
/*
........Revolved shank
*/
		else if (cpt->ctype[1] == 5 && npts > 0)
		{
			if (ymin == ymax) ymin = ymax - cutr[2];
			ix0 = 0; ix1 = 1;
			if (irot)
			{
				ix0 = 1; ix1 = 0;
			}
			zhgt = ofs[ix1];
			S_mill_profile(&profile,pts,npts,&zhgt,NCL_CUTTER_MILL);
			ptx[0] = pts[0][1]; ptx[1] = pts[0][2]; ptx[2] = pts[0][0];
			if (irot) ptx[0] += ofs[1];
			else ptx[2] += ofs[0];
			ofs[ix1] = zhgt;
			
			prim = LiPrimitiveCreateRotatedSolid(profile,ptx,vec,.001,
				LI_MW_SOLID_TYPE_GENERAL);
			tool = LiPrimitiveCreateTurningHolder(UU_NULL,prim,ymin,ymax,
				LI_MW_SIMPLIFY_NOTHING,.001,.001);
			LiProfileDestroy(profile);
			LiPrimitiveDestroy(prim);
			ymin = ymax;
		}
/*
........Standard shank
*/
		else if (npts > 0)
		{
			if (ymin == ymax) ymin = ymax - cutr[2];
			S_lathe_profile(&profile,pts,npts,&ofs);
			tool = LiPrimitiveCreateTurningHolder(profile,UU_NULL,ymin,ymax,
				LI_MW_SIMPLIFY_STANDARD_TOOL,.001,.001);
			LiProfileDestroy(profile);
			ymin = ymax;
		}
/*
........Create shank solid
*/
		if (tool == 0)
		{
			sprintf(sbuf,"CUTTER  - Failed to create tool shank %d\n",
				LW_act_tool[0]+1);
			ul_ipv_write_log(sbuf);
		}
		else
		{
			LW_shank[ispin] = LiSessionAddPrim(LW_session[LW_mach_mode],tool);
			Sshank_colors[ispin][0] = cpt->color[1];
			if (Sshank_colors[ispin][0] == -1)
			{
				Sshank_colors[ispin][0] = LW_default_tool.shank_color;
				if (Sshank_colors[ispin][0] == -1)
					Sshank_colors[ispin][0] = Stool_colors[ispin][0];
			}
			if (cpt->shank_clash == 1)
				Sshank_colors[ispin][1] = LW_clash_material.holder;
			else
				Sshank_colors[ispin][1] = LW_cut_material;
			Sshank_colors[ispin][2] = cpt->shank_clash;
			Sshank_colors[ispin][3] = LW_translucency[1];
			Sshank_colors[ispin][4] = cpt->edge[1];
			Sshank_colors[ispin][5] = cpt->edge_color[1];
/*
			icol = cpt->color[1];
			if (icol == -1)
			{
				icol = LW_default_tool.shank_color;
				if (icol == -1) icol = tcol;
			}
			LiDataSetGenericPtr(&stuff,LW_material[icol]);
			LiSessionPrimSetVisualProperty(LW_tool[ispin],LI_MW_VIS_PROP_MATERIAL,
				&stuff);
			if (cpt->shank_clash == 1)
			{
				LiDataSetGenericPtr(&stuff,
					LW_material[LW_clash_material.holder]);
				LiSessionPrimSetProperty(LW_shank[ispin],LI_SPRIM_PROP_CUT_MATERIAL,
					&stuff);
				LiSessionPrimSetProperty(LW_shank[ispin],LI_SPRIM_PROP_CLASH_MATERIAL,
					&stuff);
			}
			else
			{
				LiDataSetGenericPtr(&stuff,LW_material[LW_cut_material]);
				LiSessionPrimSetProperty(LW_shank[ispin],LI_SPRIM_PROP_CUT_MATERIAL,
					&stuff);
				LiDataSetGenericPtr(&stuff,
					LW_material[LW_clash_material.fixture]);
				LiSessionPrimSetProperty(LW_shank[ispin],
					LI_SPRIM_PROP_CLASH_MATERIAL,&stuff);
			}

			LiDataSetNat32(&transp,100-LW_translucency[1]);
			LiSessionPrimSetVisualProperty(LW_shank[ispin],
				LI_MW_VIS_PROP_TRANSPARENCY,&transp);
*/
		}
	}
/*
.....Tool Holder
*/
	if (cpt->ctype[2] > 0)
	{
/*
........Determine if holder is rotated
*/
		irot = UU_FALSE;
		vec[0] = cutseg->holder->geo->axis[1];
		vec[1] = cutseg->holder->geo->axis[2];
		vec[2] = cutseg->holder->geo->axis[0];
		if (vec[2] > vec[0]+vec[1]) irot = UU_TRUE;
/*
.........Generate the holder profile
*/
		pts = (UM_coord *)UU_LIST_ARRAY(&cutseg->holder->geo->curve);
		npts = UU_LIST_LENGTH(&cutseg->holder->geo->curve);
		if (cpt->ctype[2] == 1)
		{
			if (irot) ofs[0] = ofs[0] + cpt->parms[2][3];
			else ofs[1] = ofs[1] + cpt->parms[2][3];
			ymax = ymin;
			ymin = ymax - cpt->parms[2][2];
		}
		else
		{
			ofs[0] = ofs[0] + cpt->parms[2][0];
			ofs[1] = ofs[1] + cpt->parms[2][1];
			ymax = ymin - cpt->parms[2][2];
			ymin = ymax - cpt->parms[2][3];
		}
		if (ymin == ymax) ymin = ymax - cutr[2];
/*
........Tessellated holder
*/
		if (cpt->ctype[2] == 2)
		{
			tofs[2] = ofs[0];
			tofs[0] = ofs[1];
			tofs[1] = ymax;
			LW_num_holder[ispin] = S_symbol_prim(&cpt->symbol[2],tofs,prims,
				LW_MAX_HOLDER,2);
			for (i=0;i<LW_num_holder[ispin];i++)
			{
				holder[i] =
					LiPrimitiveCreateTurningHolder(UU_NULL,prims[i],ymin,ymax,
						LI_MW_SIMPLIFY_NOTHING,.001,.001);
				LiPrimitiveDestroy(prims[i]);
			}
		}
/*
........Revolved holder
*/
		else if (cpt->ctype[2] == 5 && npts > 0)
		{
			ix0 = 0; ix1 = 1;
			if (irot)
			{
				ix0 = 1; ix1 = 0;
			}
			zhgt = ofs[ix1];
			S_mill_profile(&profile,pts,npts,&zhgt,NCL_CUTTER_MILL);
			ptx[0] = pts[0][1]; ptx[1] = pts[0][2]; ptx[2] = pts[0][0];
			if (irot) ptx[0] += ofs[1];
			else ptx[2] += ofs[0];
			ofs[ix1] = zhgt;

			prim = LiPrimitiveCreateRotatedSolid(profile,ptx,vec,.001,
				LI_MW_SOLID_TYPE_GENERAL);
			holder[0] = LiPrimitiveCreateTurningHolder(UU_NULL,prim,ymin,ymax,
				LI_MW_SIMPLIFY_NOTHING,.001,.001);
			LiProfileDestroy(profile);
			LiPrimitiveDestroy(prim);
			LW_num_holder[ispin] = 1;
		}
/*
........Standard holder
*/
		else if (npts > 0)
		{
			S_lathe_profile(&profile,pts,npts,&ofs);
			holder[0] = LiPrimitiveCreateTurningHolder(profile,UU_NULL,ymin,ymax,
				LI_MW_SIMPLIFY_STANDARD_TOOL,.001,.001);
			LiProfileDestroy(profile);
			LW_num_holder[ispin] = 1;
		}
/*
........No points in holder definition
*/
		else
			holder[0] = 0;
/*
........Create holder solid
*/
		for (i=0;i<LW_num_holder[ispin];i++)
		{
			if (holder[i] == 0)
			{
				sprintf(sbuf,"CUTTER  - Failed to create tool holder %d.%d\n",
					LW_act_tool[0]+1,i);
				ul_ipv_write_log(sbuf);
			}
			else
			{
				LW_holder[ispin][i] = LiSessionAddPrim(LW_session[LW_mach_mode],
					holder[i]);
				Sholder_colors[ispin][0] = cpt->color[2];
				if (Sholder_colors[ispin][0] == -1)
				{
					Sholder_colors[ispin][0] = LW_default_tool.hold_color;
					if (Sholder_colors[ispin][0] == -1)
						Sholder_colors[ispin][0] = Stool_colors[ispin][0];
				}
				Sholder_colors[ispin][1] = LW_clash_material.holder;
				Sholder_colors[ispin][2] = LW_translucency[2];
				Sholder_colors[ispin][3] = cpt->edge[2];
				Sholder_colors[ispin][4] = cpt->edge_color[2];
/*
				icol = cpt->color[2];
				if (icol == -1)
				{
					icol = LW_default_tool.hold_color;
					if (icol == -1) icol = tcol;
				}
				LiDataSetGenericPtr(&stuff,LW_material[icol]);
				LiSessionPrimSetVisualProperty(LW_holder[ispin][i],
					LI_MW_VIS_PROP_MATERIAL,&stuff);
				LiDataSetGenericPtr(&stuff,
					LW_material[LW_clash_material.holder]);
				LiSessionPrimSetProperty(LW_holder[ispin][i],
					LI_SPRIM_PROP_CUT_MATERIAL,&stuff);
				LiSessionPrimSetProperty(LW_holder[ispin][i],
					LI_SPRIM_PROP_CLASH_MATERIAL,&stuff);
				LiDataSetNat32(&transp,100-LW_translucency[2]);
				LiSessionPrimSetVisualProperty(LW_holder[ispin][i],
					LI_MW_VIS_PROP_TRANSPARENCY,&transp);
*/
			}
		}
	}
/*
.....Set tool properties
*/
	ul_ipv_reset_tool_props();
/*
........Select Tool
*/
	if (LW_mach_simul)
		stat = LiViToolSelect(LW_tool[ispin],TRUE);
	else
		stat = LiViTurningToolSelect(LW_tool[ispin],LW_lathe,x_axis,TRUE);
/*
........Shank
*/
	if (stat == 0 && LW_shank[ispin] != 0)
	{
		stat = LiViToolAttachHolder(LW_tool[ispin],LW_shank[ispin],0.,TRUE);
		if (stat != 0)
		{
			sprintf(sbuf,"CUTTER  - Failed to attach shank %d",
				LW_act_tool[0]+1);
			ul_ipv_write_log(sbuf);
		}
	}
/*
........Holder
*/
	if (stat == 0 && LW_num_holder[ispin] != 0)
	{
		for (i=0;i<LW_num_holder[ispin];i++)
		{
			stat = LiViToolAttachHolder(LW_tool[ispin],LW_holder[ispin][i],0.,
				TRUE);
			if (stat != 0)
			{
				sprintf(sbuf,"CUTTER  - Failed to attach holder %d",
					LW_act_tool[0]+1);
				ul_ipv_write_log(sbuf);
			}
		}
	}
/*
........Could not create cutter
........Try altering tolerance
*/
	if (stat != 0)
	{
		sprintf(sbuf,"CUTTER  - Failed to create tool %d",LW_act_tool[0]+1);
		ul_ipv_write_log(sbuf);
		status = UU_FAILURE;
		goto done;
	}
/*
........Set clash types
*/
	ul_ipv_tool_clash();
/*
........Attach tool to turret
*/
	if (LW_mach_simul)
	{
		pts = (UM_coord *)UU_LIST_ARRAY(&cutseg->holder->geo->curve);
		ul_ipv_lathe_tool_assembly(tool_pos,cpt->clrec,cpt->tlno,vec,ofs[1],
			NCL_CUTTER_LATHE);
		if (scan != 4) ul_ipv_position_turret(cpt->tlno,UU_FALSE);
	}
/*
........Set tool position
*/
	if (!LW_mach_simul) LiViToolSetPosition(LW_tool[ispin],tool_pos);
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_reset_tool
**			Resets the last active tool.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      :  none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_reset_tool()
{
	int i;
	for (i=0;i<LW_MAX_SPINDLE;i++) Slast_act[i] = -1;
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_set_tool_colors(tool_color,cut_color,
**                        clash_color,clash_type,ispin)
**			Defines the cut colors for this move.
**    PARAMETERS   
**       INPUT  :
**          tool_color   = Color to set tool.
**          cut_color    = Color for actual cut.
**          clash_color  = Color for clashes.
**          clash_type   = Type of clash interaction for stock.
**          ispin        = Active spindle number.
**       OUTPUT : none
**    RETURNS      :  none
**    SIDE EFFECTS :
**          Stores the saved cut colors in the static array.
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_set_tool_colors(tool_color,cut_color,clash_color,clash_type,
	ispin)
int tool_color,cut_color,clash_color,clash_type,ispin;
{
	LtData stuff;
	if (tool_color != Stool_colors[ispin][0])
	{
		LiDataSetGenericPtr(&stuff,LW_material[tool_color]);
		LiSessionPrimSetVisualProperty(LW_tool[ispin],LI_MW_VIS_PROP_MATERIAL,
			&stuff);
	}
	if (cut_color != Stool_colors[ispin][1])
	{
		LiDataSetGenericPtr(&stuff,LW_material[cut_color]);
		LiSessionPrimSetProperty(LW_tool[ispin],LI_SPRIM_PROP_CUT_MATERIAL,
			&stuff);
	}
	if (clash_color != Stool_colors[ispin][2])
	{
		LiDataSetGenericPtr(&stuff,LW_material[clash_color]);
		LiSessionPrimSetProperty(LW_tool[ispin],LI_SPRIM_PROP_CLASH_MATERIAL,
			&stuff);
	}
	if (clash_type != Stool_colors[ispin][3])
		LiSessionPrimSetInteractType(LW_tool[ispin],LI_MW_SOLID_TYPE_STOCK,
			clash_type);
	Stool_colors[ispin][0] = tool_color;
	Stool_colors[ispin][1] = cut_color;
	Stool_colors[ispin][2] = clash_color;
	Stool_colors[ispin][3] = clash_type;
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_reset_tool_props()
**			Resets the tool properties to the last specified properties.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      :  none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_reset_tool_props()
{
	int i,j;
	LtDouble junk;
	LtData stuff;
/*
.....Set tool properties
*/
	for (j=0;j<LW_spindle_num;j++)
	{
		if (LW_tool[j] != 0)
		{
			LiDataSetGenericPtr(&stuff,LW_material[Stool_colors[j][0]]);
			LiSessionPrimSetVisualProperty(LW_tool[j],LI_MW_VIS_PROP_MATERIAL,
				&stuff);

			LiDataSetGenericPtr(&stuff,LW_material[Stool_colors[j][1]]);
			LiSessionPrimSetProperty(LW_tool[j],LI_SPRIM_PROP_CUT_MATERIAL,&stuff);

			LiDataSetGenericPtr(&stuff,LW_material[Stool_colors[j][2]]);
			LiSessionPrimSetProperty(LW_tool[j],LI_SPRIM_PROP_CLASH_MATERIAL,
				&stuff);

			LiSessionPrimSetInteractType(LW_tool[j],LI_MW_SOLID_TYPE_STOCK,
				Stool_colors[j][3]);

			LiDataSetNat32(&stuff,100-Stool_colors[j][4]);
			LiSessionPrimSetVisualProperty(LW_tool[j],LI_MW_VIS_PROP_TRANSPARENCY,
				&stuff);

			ul_ipv_render_edges(LW_tool[j],Stool_colors[j][5],Stool_colors[j][6],
				Stool_colors[j][0], UU_FALSE);
		}
/*
.....Set shank properties
*/
		if (LW_shank[j] != 0)
		{
			LiDataSetGenericPtr(&stuff,LW_material[Sshank_colors[j][0]]);
			LiSessionPrimSetVisualProperty(LW_shank[j],LI_MW_VIS_PROP_MATERIAL,
				&stuff);

			LiDataSetGenericPtr(&stuff,LW_material[Sshank_colors[j][1]]);
			LiSessionPrimSetProperty(LW_shank[j],LI_SPRIM_PROP_CUT_MATERIAL,
				&stuff);
			LiSessionPrimSetProperty(LW_shank[j],LI_SPRIM_PROP_CLASH_MATERIAL,
				&stuff);
			if (Sshank_colors[j][2] != 1)
			{
				LiDataSetGenericPtr(&stuff,LW_material[Stool_colors[j][2]]);
				LiSessionPrimSetProperty(LW_shank[j],
					LI_SPRIM_PROP_CLASH_MATERIAL,&stuff);
			}

			LiDataSetNat32(&stuff,100-Sshank_colors[j][3]);
			LiSessionPrimSetVisualProperty(LW_shank[j],
				LI_MW_VIS_PROP_TRANSPARENCY,&stuff);

			ul_ipv_render_edges(LW_shank[j],Sshank_colors[j][4],
				Sshank_colors[j][5],Sshank_colors[j][0],UU_FALSE);

			if (Sshank_colors[j][2] == 1)
			{
				junk = -(LW_toler / 2.);
				LiDataSetDouble(&stuff,junk);
				LiSessionPrimSetProperty(LW_shank[j],LI_SPRIM_PROP_VI_SHANK_EXPAND,
					&stuff);
			}
		}
/*
.....Set holder properties
*/
		for (i=0;i<LW_num_holder[j];i++)
		{
			if (LW_holder[j][i] != 0)
			{
				LiDataSetGenericPtr(&stuff,LW_material[Sholder_colors[j][0]]);
				LiSessionPrimSetVisualProperty(LW_holder[j][i],
					LI_MW_VIS_PROP_MATERIAL,&stuff);

				LiDataSetGenericPtr(&stuff,LW_material[Sholder_colors[j][1]]);
				LiSessionPrimSetProperty(LW_holder[j][i],LI_SPRIM_PROP_CUT_MATERIAL,
					&stuff);
				LiSessionPrimSetProperty(LW_holder[j][i],
					LI_SPRIM_PROP_CLASH_MATERIAL,&stuff);

				LiDataSetNat32(&stuff,100-Sholder_colors[j][2]);
				LiSessionPrimSetVisualProperty(LW_holder[j][i],
					LI_MW_VIS_PROP_TRANSPARENCY,&stuff);

					ul_ipv_render_edges(LW_holder[j][i],Sholder_colors[j][3],
					Sholder_colors[j][4],Sholder_colors[j][0],UU_FALSE);
			}
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_view_taxis(tool_pos,tool_vec)
**			Changes the view to reflect the tool axis.  Used for moving
**       part playback.
**    PARAMETERS   
**       INPUT  :
**          tool_pos     = Current tool position.
**          tool_vec     = Current tool axis.
**       OUTPUT : none
**    RETURNS      :  none
**    SIDE EFFECTS :
**          Stores the saved cut colors in the static array.
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_view_taxis(tool_pos,tool_vec)
UM_coord tool_pos;
UM_vector tool_vec;
{
	int vp;
	UM_int2 i2v0=0;
	UM_coord ptx;
	UM_vector vup,vpn,vco;
	UU_REAL um_mag(),ang;
	UM_transf mtf;
/*
.....Adjust view to tool
*/
	vp = LW_vport.xform;
	um_cross(UN_playtax[vp-1],tool_vec,vco);
	ncl_mcstowcs(1,vco,vco);
	ang = um_mag(vco);
	if (ang > UM_FUZZ)
	{
		ang = um_angle(UN_playtax[vp-1],tool_vec);
		um_rottf(vco,ang,mtf);
		um_vctmtf(UN_playvpn[vp-1],mtf,vpn);
		um_vctmtf(UN_playvup[vp-1],mtf,vup);
	}
	else
	{
		um_vctovc(UN_playvpn[vp-1],vpn);
		um_vctovc(UN_playvup[vp-1],vup);
	}
	um_vctovc(tool_pos,ptx);
	mcswcs(&i2v0,ptx);
	gsvref3(vp,ptx);
	gsvpn3(vp,vpn);
	gsvup3(vp,vup);
	ug_sntran(vp);
	ul_ipv_view_same(vp);
}

/*********************************************************************
**    I_FUNCTION     : S_mill_profile(symprof,pts,npts,zhgt,type)
**			Creates a profile used for creating mills, shanks, and holders.
**    PARAMETERS
**       INPUT  :
**				pts     = Array of points to use in profile.
**          npts    = Number of points in 'pts'.
**          zhgt    = Current height of tool.
**          type    = NCL_CUTTER_MILL or NCL_CUTTER_BLADE.
**       OUTPUT :
**				symprof = MachineWorks profile for use in creating tool.
**                    UU_NULL if profile could not be created.
**          zhgt    = Updated height of tool.
**    RETURNS      :  none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_mill_profile(symprof,pts,npts,zhgt,type)
LtProfile *symprof;
UM_coord *pts;
int npts;
UU_REAL *zhgt;
NCL_cutter_type type;
{
	int i,ist,inc;
	UU_REAL zadd;
	UM_coord pt;
	LtBoolean flag;
/*
.....Create the symbol profile
*/
	*symprof = (LtProfile)0;
	zadd = *zhgt;
	if (npts != 0)
	{
		if (type == NCL_CUTTER_MILL)
		{
			ist = 0;
			inc = 2;
		}
		else
		{
			ist = 1;
			inc = 1;
		}
		pt[0] = pts[ist][0]; pt[1] = pts[ist][inc] + zadd; pt[2] = 0;
		if (pt[0] < 0. && type == NCL_CUTTER_MILL) pt[0] = pt[0] * -1.;
		if (pt[1] > *zhgt) *zhgt = pt[1];
		*symprof = LiProfileCreate(pt);
		for (i=ist+1;i<npts;i++)
		{
			if (!um_cceqcc(&pts[i],&pts[i-1]))
			{
				pt[0] = pts[i][0]; pt[1] = pts[i][inc] + zadd;
				if (pt[0] < 0. && type == NCL_CUTTER_MILL) pt[0] = pt[0] * -1.;
				if (pt[1] > *zhgt) *zhgt = pt[1];
				if (i != npts-1) flag = S_smooth(pts,i,npts);
				LiProfileAddLine(*symprof,pt,flag);
			}
		}
	}
}

/*********************************************************************
**    I_FUNCTION     : S_lathe_profile(symprof,pts,npts,ofs)
**			Creates a profile used for creating lathe bits, shanks, and holders.
**    PARAMETERS
**       INPUT  :
**				pts     = Array of points to use in profile.
**          npts    = Number of points in 'pts'.
**          ofs     = XY-offset to apply to tool profile.
**       OUTPUT :
**				symprof = MachineWorks profile for use in creating tool.
**                    UU_NULL if profile could not be created.
**          ofs     = Updated XY-offset.
**    RETURNS      :  none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_lathe_profile(symprof,pts,npts,ofs)
LtProfile *symprof;
UM_coord *pts;
int npts;
UU_REAL ofs[];
{
	int i;
	UU_REAL xy[2];
	UM_coord pt;
/*
.....Create the symbol profile
*/
	*symprof = (LtProfile)0;
	xy[0] = ofs[0]; xy[1] = ofs[1];
	if (npts != 0)
	{
		pt[0] = pts[1][0] + xy[0]; pt[1] = pts[1][1] + xy[1]; pt[2] = 0;
		*symprof = LiProfileCreate(pt);
		for (i=2;i<npts;i++)
		{
			if (!um_cceqcc(&pts[i],&pts[i-1]))
			{
				pt[0] = pts[i][0] + xy[0]; pt[1] = pts[i][1] + xy[1];
				LiProfileAddLine(*symprof,pt,FALSE);
			}
		}
		ofs[0] = ofs[0] + pts[0][0]; ofs[1] = ofs[1] + pts[0][1];
	}
}

/*********************************************************************
**    I_FUNCTION     : S_symbol_prim(symbol,ofs,prims,maxp,ctype)
**			Tessellates all surfaces within a symbol and creates a solid
**			primitive from the triangles.  If the symbol contains Visual
**       Solids, then all Visual Solids in the symbol are merged to
**       create a single solid primitive.
**    PARAMETERS
**       INPUT  :
**				symbol  = Symbol name to create solid from.
**          ofs     = Current tool offsets.
**          maxp    = Maximum number of primitives to create.
**          ctype   = 1 = Mill cutter, 2 = Lathe cutter.
**       OUTPUT :
**          ofs     = Updated tool offsets.
**          prims   = ID(s) of solid primitives created.  An ID of 0
**                    means the primitive was not created.
**    RETURNS      :  Number of primitives created.
**    SIDE EFFECTS :
**			The symbol surfaces must define a closed solid.
**    WARNINGS     : none
*********************************************************************/
static int S_symbol_prim(symbol,ofs,prims,maxp,ctype)
char *symbol;
UU_REAL *ofs;
LtPrim *prims;
int maxp,ctype;
{
	int nprim;
	UU_LOGICAL found;
	struct UB_symbol_rec sym_rec;
	struct UC_entitydatabag ent;
/*
.....Get the requested symbol
*/
	prims[0]= 0;
	nprim = 0;
	ncl_parse_label(symbol,sym_rec.label,&sym_rec.subscr);
	if (ub_get_symmaster_by_name(&sym_rec,&found,1,1) != UU_SUCCESS) goto done;
/*
.....Determine geometry type of symbol
*/
	ent.key = sym_rec.geom[0];
	if (uc_retrieve_data(&ent,sizeof(struct UC_entitydatabag)) != UU_SUCCESS)
		goto done;
	if (ent.rel_num == UM_SOLID_REL)
		nprim = S_symbol_solid(&sym_rec,ofs,prims,maxp,ctype);
	else
		nprim = S_symbol_surface(&sym_rec,prims);
/*
.....End of routine
*/
done:;
	return(nprim);
}
	
/*********************************************************************
**    I_FUNCTION     : S_symbol_surface(sym_rec,prim)
**			Tessellates all surfaces within a symbol and creates a solid
**			primitive from the triangles.
**    PARAMETERS
**       INPUT  :
**				sym_rec  = Symbol to create solid from.
**       OUTPUT :
**          prim   = ID of solid primitives created.  An ID of 0
**                    means the primitive was not created.
**    RETURNS      :  Number of primitives created.
**    SIDE EFFECTS :
**			The symbol surfaces must define a closed solid.
**    WARNINGS     : none
*********************************************************************/
static int S_symbol_surface(sym_rec,prim)
struct UB_symbol_rec *sym_rec;
LtPrim *prim;
{
	int i,j,k,nprim,ntr,status,usav,vsav;
	UU_LOGICAL found;
	UU_REAL tsav,bsav;
	UM_coord trp[3],*tpt;
	UM_tess_settype ttyp;
	UM_tessellation tess;
	UM_transf tfmat;
	UM_trian *tri;
	UU_LIST triangles,tlist;
	struct UC_entitydatabag ent;
/*	char sbuf[80];*/
	LtSession sess;
	LtSessionPrim sprim;
/*
.....Prepare for surface tessellation
*/
	ncl_get_tess_parms(&ttyp,&tsav,&usav,&vsav);
	bsav = ncl_get_boundary_toler();
/*
.....Create the solid
*/
	nprim = 0;
	*prim = LiPrimitiveCreateTriangleSolid(LI_MW_SOLID_TYPE_GENERAL);
	if (*prim == 0) goto done;
/*
.....Initialize triangle list
.....Create the tessellation
*/
	uu_list_init(&tlist,sizeof(UM_coord),500,100);
	uu_list_init(&triangles,sizeof(UM_trian),100,50);
	um_init_tess(&tess);
	ncl_set_tess_parms(UM_TESS_TOLER,LW_toler,0,0);
	ncl_set_boundary_toler(LW_toler);
/*
.....Loop through the surfaces
*/
	for (i=0;i<sym_rec->no_geom;i++)
	{
/*
........Retrieve symbol subentity
*/
		ent.key = sym_rec->geom[i];
		if (uc_retrieve_data(&ent,sizeof(struct UC_entitydatabag)) != UU_SUCCESS)
			goto done;
/*
........Only process surfaces
*/
		if (uc_super_class(ent.rel_num) != UC_SURFACE_CLASS) continue;
		status = uc_retrieve_transf(ent.key,tfmat);
		if (status != UU_SUCCESS) goto done;
/*
........Reset the tessellation and triangles lists
*/
		um_clean_tess (&tess);
		UU_LIST_EMPTY (&triangles);
		status = uc_srf_tessellate(&ent,&tess);
		if (status != UU_SUCCESS) goto done;
		status = ncl_get_tess_triangles(&tess,&triangles,2,0);
		if (status != UU_SUCCESS) goto done;
/*
........Match points within tolerance
*/
		ntr = UU_LIST_LENGTH(&triangles);
		tri= (UM_trian *)UU_LIST_ARRAY(&triangles);
		tpt = (UM_coord *)UU_LIST_ARRAY(&tlist);
		for (j=0;j<ntr;j++)
		{
			for (k=0;k<UU_LIST_LENGTH(&tlist);k++)
			{
				if (um_dcccc(tri[j].p1,tpt[k]) <= LW_toler)
					um_vctovc(tpt[k],tri[j].p1);
				if (um_dcccc(tri[j].p2,tpt[k]) <= LW_toler)
					um_vctovc(tpt[k],tri[j].p2);
				if (um_dcccc(tri[j].p3,tpt[k]) <= LW_toler)
					um_vctovc(tpt[k],tri[j].p3);
			}
			uu_list_push(&tlist,tri[j].p1);
			uu_list_push(&tlist,tri[j].p2);
			uu_list_push(&tlist,tri[j].p3);
		}
/*
........Free tessellation
*/
	}
	um_free_tess(&tess);
	uu_list_free(&triangles);
/*
........Add each triangle to the solid
*/
	tpt= (UM_coord *)UU_LIST_ARRAY(&tlist);
	for (j=0;j<UU_LIST_LENGTH(&tlist);j=j+3)
	{
		um_vctovc(tpt[j],&trp[0]);
		um_vctovc(tpt[j+1],&trp[1]);
		um_vctovc(tpt[j+2],&trp[2]);
		LiTriangleSolidAddFace(*prim,trp);
/*
{
	char sbuf[80];
	sprintf(sbuf,"ln/%lf,%lf,%lf,%lf,%lf,%lf",trp[0][0],trp[0][1],trp[0][2],
	trp[1][0],trp[1][1],trp[1][2]);
	ul_ipv_write_log(sbuf);
	sprintf(sbuf,"ln/%lf,%lf,%lf,%lf,%lf,%lf",trp[1][0],trp[1][1],trp[1][2],
	trp[2][0],trp[2][1],trp[2][2]);
	ul_ipv_write_log(sbuf);
	sprintf(sbuf,"ln/%lf,%lf,%lf,%lf,%lf,%lf",trp[2][0],trp[2][1],trp[2][2],
	trp[0][0],trp[0][1],trp[0][2]);
	ul_ipv_write_log(sbuf);
}
*/
	}
	uu_list_free(&tlist);
/*
.....Reset tessellation settings
*/
	ncl_set_tess_parms(ttyp,tsav,usav,vsav);
	ncl_set_boundary_toler(bsav);
/*
.....Finish the solid
*/
	status = LiTriangleSolidEnd(*prim,FALSE,.001);
	if (status == LI_STATUS_OK)
	{
		sess = LiSessionCreate(LI_MW_SESSION_SOLID_OPS);
		sprim = LiSessionAddPrim(sess,*prim);
		LiSOSolidMakeConsistent(sprim);
		LiSOSolidStore(sprim);
		LiSessionDestroy(sess);
		nprim = 1;
	}
/*
.....Could not create the solid
*/
	else
	{
		LiPrimitiveDestroy(*prim);
		*prim = 0;
	}
/*
.....End of routine
*/
done:;
	return(nprim);
}

/*********************************************************************
**    I_FUNCTION     : S_symbol_solid(symbol,ofs,prims,maxp,ctype)
**       All Visual Solids in the symbol are merged to create a single
**       solid primitive.
**    PARAMETERS
**       INPUT  :
**				sym_rec  = Symbol to create solid from.
**          ofs      = Current tool offsets.
**          maxp     = Maximum number of primitives to create.
**          ctype    = 1 = Mill cutter, 2 = Lathe cutter.
**       OUTPUT :
**          ofs      = Updated tool offsets.
**          prims    = ID(s) of solid primitives created.  An ID of 0
**                     means the primitive was not created.
**    RETURNS      :  Number of primitives created.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_symbol_solid(sym_rec,ofs,prims,maxp,ctype)
struct UB_symbol_rec *sym_rec;
UU_REAL *ofs;
LtPrim *prims;
int maxp,ctype;
{
	int i,nprim,status,nkeys,nsol,nsrf,inc,rel;
	UU_LOGICAL compfl;
	UU_REAL xadd,yadd,zadd,*parms;
	UM_transf tf;
	LW_stock_struc stock;
	LtMatrix mx1;
	LtTransform xform;
	struct UC_entitydatabag ent;
	struct UM_solid_rec *solid,csol;
/*
.....Initialize stock structure
*/
	S_init_stock(&stock);
	xadd = 0.;
	yadd = 0.;
	zadd = 0.;
	nprim = 0;
/*
.....Loop through the solids
*/
	for (i=0;i<sym_rec->no_geom;i++)
	{
/*
........Retrieve symbol subentity
*/
		ent.key = sym_rec->geom[i];
		if (uc_retrieve_data(&ent,sizeof(struct UC_entitydatabag)) != UU_SUCCESS)
			continue;
/*
........Only process solids
*/
		if (ent.rel_num != UM_SOLID_REL) continue;
		solid = (struct UM_solid_rec *)&ent;
/*
........Composite solid
*/
		compfl = UU_FALSE;
		nkeys = 1;
		if (solid->type == UM_COMPOS_SOLID)
		{
			ncl_solid_count_components(solid,&nsol,&nsrf);
			if (nsol == 0) continue;
			compfl = UU_TRUE;
			csol = *solid;
			nkeys = csol.no_netkey;
		}
/*
........Loop through solids
*/
		for (inc=0;inc<nkeys;inc++)
		{
			if (compfl)
			{
				solid->key = csol.netkey[inc];
				ur_retrieve_data_relnum(solid->key,&rel);
				if (rel != UM_SOLID_REL) continue;
				status = ncl_retrieve_data(solid,sizeof(struct UM_solid_rec));
				if (status != UU_SUCCESS) goto failed;
			}
/*
........Get solid parameters
*/
			parms = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*solid->no_sdata);
			if (parms == UU_NULL) continue;
			status = ur_retrieve_data_varlist(solid->key,1,parms,1,
				solid->no_sdata);
			if (status != UU_SUCCESS) continue;
			solid->sdata = parms;
/*
........Attach solid to tool
*/
			if (ofs[0] != 0. || ofs[1] != 0. || ofs[2] != 0.)
			{
				um_identtf(tf);
				if (ctype == 2)
				{
					tf[0][0] = 0.; tf[0][2] = 1.;
					tf[1][1] = 0.; tf[1][0] = 1.;
					tf[2][2] = 0.; tf[2][1] = 1.;
				}
				tf[3][0] = ofs[0];
				tf[3][1] = ofs[1];
				tf[3][2] = ofs[2];
				ncl_xform_solid(solid->type,&solid->sdata,tf,UU_FALSE);
			}
/*
........Convert the solid to an NCLIPV stock structure
*/
			ncl_solid_to_stock(solid,&stock);
			uu_free(parms);
/*
........Create the primitive
*/
			status = ul_ipv_create_stock(&stock,3,UU_FALSE);
			if (status != UU_SUCCESS) continue;
			LiSessionRemovePrim(stock.stock);
/*
........STL solids must be xformed
........using the NCLIPV method
*/
			if (stock.type == LW_STOCK_FILE && stock.mxflag != 0)
			{
				ncl_43mx_to_44mx(stock.matrix,mx1);
				if (stock.units == 1)
				{
					stock.xform = LiTransformDefine(mx1);
					xform = LiTransformCopy(LW_xform_mminch);
					LiTransformConcatenate(xform,stock.xform);
					LiTransformDestroy(stock.xform);
				}
				else
					xform = LiTransformDefine(mx1);
				LiPrimitiveSetSolidTransform(stock.prim,xform);
				LiTransformDestroy(xform);
			}
			uu_free(stock.data);
/*
........Merge the primitive with existing primitives
*/
			prims[nprim] = stock.prim;
			nprim++;
/*
.....Update the tool offsets
*/
			if (solid->box[3] > xadd) xadd = solid->box[3];
			if (solid->box[4] > yadd) yadd = solid->box[4];
			if (solid->box[5] > zadd) zadd = solid->box[5];
		}
	}
	if (nprim == 0) goto failed;
	ofs[0] = ofs[0] + xadd;
	ofs[1] = ofs[1] + yadd;
	ofs[2] = ofs[2] + zadd;
	goto done;
/*
.....Failed to create stock
*/
failed:;
	prims[0] = 0;
	nprim = 0;
/*
.....End of routine
*/
done:;
	return(nprim);
}

/*********************************************************************
**    I_FUNCTION     : S_smooth(pts,ist,npts)
**			Determines if a generated cutter profile segment is smooth.
**    PARAMETERS   
**       INPUT  : 
**				pts     = Array of profile points.
**				ist     = Current point in 'pts'.
**				npts    = Number of points in 'pts'.
**       OUTPUT :  
**				none
**    RETURNS      :  TRUE if profile is smooth at this location,
**		                FALSE otherwise.
**    SIDE EFFECTS :
**			The symbol surfaces must define a closed solid.
**    WARNINGS     : none
*********************************************************************/
LtBoolean S_smooth(pts,ist,npts)
UM_coord *pts;
int ist,npts;
{
	int inc;
	UU_REAL ang,um_angle();
	UM_vector vc1,vc2;
/*
.....Determine points to check
*/
	inc = ist;
	if (ist == 0) inc = 1;
	if (ist+1 == npts) return(FALSE);
/*
.....Determine if smooth based on 10 degree angle
*/
	else
	{
		um_vcmnvc(pts[inc],pts[inc-1],vc1);
		um_vcmnvc(pts[inc+1],pts[inc],vc2);
		ang = um_angle(vc1,vc2);
		if (ang < .175) return(TRUE);
		else return(FALSE);
	}
}

/*********************************************************************
**    I_FUNCTION     : S_init_stock(sd)
**			Initializes a standard stock structure for cutter display.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  
**				sd     = Stock structure to initialize.
**    RETURNS      :  none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_init_stock(sd)
LW_stock_struc *sd;
{
	sd->id = 1;
	sd->color = LW_default_tool.hold_color;
	sd->translucency = LW_translucency[0];
	sd->visible = UU_TRUE;
	sd->active = UU_TRUE;
	sd->toler = LW_toler;
	sd->edge = UU_FALSE;
	sd->edge_color = -1;
	sd->mxflag = UU_FALSE;
	sd->mxchg = UU_FALSE;
	sd->axes = UU_FALSE;
	sd->axes_color = -1;
	sd->stock = 0;
	sd->mxname[0] = '/0';
	sd->axis_seg = -1;
}

/*********************************************************************
**    I_FUNCTION     : S_store_mattr(mdata,mattr,cutn)
**			Stores the active motion data and motion attributes in the lists.
**    PARAMETERS   
**       INPUT  :
**				mdata   = Motion data for current move.
**				mattr   = Motion attributes for current move.
**				cutn    = Cut number for current move.
**       OUTPUT :  
**				sd     = Stock structure to initialize.
**    RETURNS      :  none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_store_mattr(mdata,mattr,cutn)
UN_mot_data *mdata;
UN_mot_attr *mattr;
int cutn;
{
	if (LW_mot_data != UU_NULL)
	{
/*
........Check attributes first
*/
		if (LW_mot_attr != UU_NULL)
		{
			if (LW_mot_nattr == 0 ||
				(mattr->tlno != LW_mot_attr->tlno ||
				mattr->loadtl != LW_mot_attr->loadtl ||
				mattr->sp_mode != LW_mot_attr->sp_mode ||
				mattr->sp_val != LW_mot_attr->sp_val ||
				mattr->coolnt != LW_mot_attr->coolnt ||
				mattr->cc_mode != LW_mot_attr->cc_mode ||
				mattr->cc_dir != LW_mot_attr->cc_dir))
			{
				LW_mot_nattr++;
				LW_mot_attr = (UN_mot_attr *)uu_lsinsrt(LW_mot_attr,
					sizeof(UN_mot_attr));
				uu_move_byte(mattr,LW_mot_attr,sizeof(UN_mot_attr));
			}
		}
/*
........Now check motion data
*/
		if (LW_mot_ndata == 0 ||
			(mdata->isn != LW_mot_data->isn ||
			mdata->seqno != LW_mot_data->seqno ||
			mdata->fr_mode != LW_mot_data->fr_mode ||
			mdata->fr_val != LW_mot_data->fr_val ||
			LW_spindle_num != 0 ||
			mdata->isnptr != LW_mot_data->isnptr))
		{
			if (LW_mot_ndata != 0) LW_mot_data->cut[1] = cutn - 1;
			LW_mot_ndata++;
			LW_mot_data = (UN_mot_data *)uu_lsinsrt(LW_mot_data,
				sizeof(UN_mot_data));
			uu_move_byte(mdata,LW_mot_data,sizeof(UN_mot_data));
			LW_mot_data->cut[0] = cutn;
			LW_mot_data->cut[1] = cutn;
			LW_mot_data->mattr = LW_mot_attr;
		}
		else
		{
			LW_mot_data->cut[1] = cutn;
			LW_mot_data->clrec[1] = mdata->clrec[1];
		}
	}
}
