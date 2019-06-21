/*********************************************************************
**	FILENAME: lipvsess.c
**	CONTAINS:	ul_ipv_start_session
**             ul_ipv_end_session()
**             ul_ipv_archive_session()
**             ul_ipv_load_session()
**             ul_ipv_save_session()
**             ul_ipv_save_sess()
**             ul_ipv_restore_session()
**             ul_ipv_reset_session()
**             ul_ipv_delete_sessions()
**             ul_ipv_save_faces()
**             ul_ipv_restore_faces()
**             ul_ipv_color_faces()
**             ul_ipv_session_active()
**     MODULE NAME AND RELEASE LEVEL 
**       lipvsess.c , 25.6
**    DATE AND TIME OF LAST MODIFICATION
**       12/02/15 , 09:49:05
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "lcom.h"
#include "mfort.h"
#include "nclver.h"
#include "nclfc.h"
#include <ctype.h>
#include <math.h>
#include "gtbl.h"
#include "gobas.h"
#include "view.h"
#include "mdattr.h"
#include "mpocket.h"
#include "ginqatt.h"
#include "zsysdep.h"
#include "mcrv.h"
#include "uhep.h"
#include "dselmask.h"
#include "mdpick.h"
#include "modef.h"
#include "ulist.h"
#include "m2dattr.h"
#include "mdcpln.h"
#include "mcrv.h"
#include "nccs.h"
#include "mdrel.h"
#include "xenv1.h"
#include "xfsys0.h"
#include "xfsys1.h"

#include "lipv.h"
#include "lipvmach.h"
#include "lipvmplay.h"

void ul_ipv_delete_sessions();
void ul_ipv_save_faces();
void ul_ipv_restore_faces();
void ul_ipv_save_sess();
extern LW_stock_struc *ul_ipv_delist_stock(j,stock);

/*********************************************************************
**   E_FUNCTION:int ul_ipv_start_session(create_stocks,set_mach)
**      This function starts an NCLIPV Machinining Session.
**   PARAMETERS
**       INPUT  :
**          create_stocks = UU_TRUE if stocks need to be created.
**          set_mach      = UU_TRUE if mach type should be set
**                          (typically set to UU_FALSE when importing
**                           a session).
**       OUTPUT : none.
**   RETURNS: UU_FAILURE if could not create session.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_ipv_start_session(create_stocks,set_mach)
UU_LOGICAL create_stocks,set_mach;
{
	int i,j,ifl;
	UU_REAL box[6];
	LW_mach_model_struc *mpt;
	LW_mach_solid_struc *spt;
	LW_stock_struc *sd,*sdtmp;
	LW_mach_toolpin_struc *tptr;
	LtData stuff;

	if (LW_session[LW_mach_mode] == 0)
	{
/*
.....Set the machine type
*/
		if (set_mach)
		{
			if (!LW_mach_simul && LW_mach_type_flag == 0)
				LW_mach_type = LW_mach_type_main = ul_ipv_mach_type();
			else
			{
				if (LW_ntool == 0) ul_ipv_load_tools(&LW_tool_list,&LW_ntool);
				if (!LW_mach_simul)
					LW_mach_type = LW_mach_type_main = LW_mach_type_flag - 1;
			}
			if (LW_mach_type_main == LW_MILLTURN) LW_mach_type = LW_LATHE;
			if (LW_mach_type == LW_LATHE) LW_is_lathe = UU_TRUE;
			else LW_is_lathe = UU_FALSE;
		}
/*
.....Initialize the log file
*/
		if (LW_reset_log) ul_ipv_reset_log();
/*
.....Create Machining Sessions
*/
		LW_session[LW_mach_mode] = LiSessionCreate(
			(LW_mach_mode == LW_VISICUT) ? LI_MW_SESSION_VISICUT :
			LI_MW_SESSION_RAPIDCUT);
/*
.....Failed to create session
.....Delete any invalid STL models and
.....Try again
*/
		if (LW_session[LW_mach_mode] == 0 && LW_mach_mode == LW_VISICUT)
		{
			LW_session[LW_mach_mode] = LiSessionCreate(LI_MW_SESSION_VISICUT);
			for (j=0;j<2;j++)
			{
				LW_stock_data[j] = LW_stock_first[j];
				for (i=0;i<LW_nstock[j];i++)
				{
					ifl = 0;
					do
					{
						ul_ipv_get_next_stock(LW_stock_data[j],&sdtmp,&ifl,UU_FALSE);
						if (ifl == 2) break;
						if (sdtmp->active)
							sdtmp->stock = LiSessionAddPrim(LW_session[LW_mach_mode],
								sdtmp->prim);
					} while (ifl != -1);
					LW_stock_data[j] = (LW_stock_struc *)uu_lsnext(LW_stock_data[j]);
				}
			}
		}
/*
.....Could not create machining session
*/
		if (LW_session[LW_mach_mode] == 0)
		{
			ud_wrerr("Failed to create NCLIPV Session.");
			return(UU_FAILURE);
		}
/*
....Define any stocks and fixtures
*/
		if (create_stocks)
		{
			for (j=0;j<2;j++)
			{
				if (LW_nstock[j] != 0)
				{
					LW_stock_data[j] = LW_stock_first[j];
					for (i=0;i<LW_nstock[j];i++)
					{
						ul_ipv_create_stocks(LW_stock_data[j],j,0);
						LW_stock_data[j] =
							(LW_stock_struc *)uu_lsnext(LW_stock_data[j]);
					}
				}
			}
		}
/*
.....Create the Machine Solids
*/
		ul_ipv_define_assembly();
/*
.....Define the viewport
*/
		LW_view = LiViewCreate();
		LW_viewport = LiMWViewportCreate(LW_session[LW_mach_mode],LW_view);
/*		LW_so_viewport = LiSOViewportCreate(LW_view);*/
		ul_ipv_set_lights(LW_lights);
		ul_ipv_delete_sessions();
/*
.....Initialize variables
*/
		LW_print_num = 1;
/*
.....Set the obstruction style
*/
		ul_ipv_display_obstruct(UU_TRUE);
/*
.....Set stocks and fixtures attributes
*/
		for (j=0;j<2;j++)
		{
			LW_stock_data[j] = LW_stock_first[j];
			for (i=0;i<LW_nstock[j];i++)
			{
				ifl = 0;
				do
				{
					ul_ipv_get_next_stock(LW_stock_data[j],&sdtmp,&ifl,UU_FALSE);
					if (ifl == -2) break;
					ul_ipv_set_stk_attr(sdtmp);
					if (sdtmp->mxflag)
						ul_ipv_modify_stock(sdtmp,UU_FALSE);
				} while (ifl != -1);
				LW_stock_data[j] = (LW_stock_struc *)uu_lsnext(LW_stock_data[j]);
			}
		}
/*		if (LW_mach_mode == LW_RAPIDCUT) LiMachiningSessionSet(LW_session[1]);*/
		for (i=0;i<LW_MAX_SPINDLE;i++) LW_act_tool[i] = -1;
		LW_cutcolor_index = -1;
/*
.....Create the assemblies
*/
		ul_ipv_create_assembly();
/*
.....Set machine type
*/
		if (LW_mach_mode == LW_VISICUT && LW_is_lathe)
		{
			if (!LW_mach_simul)
			{
				LW_lathe = LiViLatheCreate(LW_session[LW_mach_mode]);
				for (j=0;j<2;j++)
				{
					sd = LW_stock_first[j];
					for (i=0;i<LW_nstock[j];i++)
					{
						ifl = 0;
						do
						{
							ul_ipv_get_next_stock(sd,&sdtmp,&ifl,
								UU_FALSE);
							if (ifl == -2) break;
							LiViLatheAddSolid(LW_lathe,sdtmp->stock);
						} while (ifl != -1);
					}
				}
				ul_ipv_reset_lathe();
				ul_ipv_lathe_start();
				LiViAllowCutType(LW_session[LW_mach_mode],LI_MW_TURN_SMOOTH_3AXIS,
					TRUE);
			}
			else
			{
				mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
				ul_ipv_reset_lathe();
				ul_ipv_lathe_start();
				tptr = (LW_mach_toolpin_struc *)UU_LIST_ARRAY(&LW_mach_toolpin);
				LiViAssemblySetParent(LW_lathe,
					mpt[mpt[tptr[LW_mach_tpin_ix].axis].parent].assembly);
				LiViAllowCutType(LW_session[LW_mach_mode],LI_MW_TURN_SMOOTH_3AXIS,
					TRUE);
				ul_ipv_mount_lathe_tools();
			}
		}
/*
.....Initialize motion attribute lists
*/
		LW_mot_data = (UN_mot_data *)uu_lsnew();
		LW_mot_data_first = LW_mot_data;
		LW_mot_ndata = 0;
		LW_mot_attr = (UN_mot_attr *)uu_lsnew();
		LW_mot_attr_first = LW_mot_attr;
		LW_mot_nattr = 0;
/*
.....Initialize motion stack
*/
		ul_ipv_mot_stack_init();
/*
.....Predefine stocks by
.....Processing up to 1st motion
*/
		ul_ipv_prescan_stocks();
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**   E_FUNCTION:int ul_ipv_end_session()
**      This function ends an NCLIPV Machinining Session.
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_end_session()
{
	int i,j,ifl;
	LW_stock_struc *stock;
/*
.....Get rid of the lathe
*/
	if (LW_lathe != 0)
	{
		ul_ipv_set_mach_type(LW_LATHE);
		ul_ipv_lathe_stop();
		LiViLatheDestroy(LW_lathe);
		LW_lathe = 0;
	}
/*
.....Delete the viewing axis
*/
	um_delv_axis_ipv();
/*
.....Delete the machine
*/
/*	if (LW_active)*/
	{
		ul_ipv_free_turret();
		ul_ipv_destroy_assembly();
/*
.....Delete the tool
*/
		ul_ipv_deselect_tool();
/*
.....Delete the motion stack
*/
		ul_ipv_mot_stack_delete(UU_TRUE);
/*
.....Delete the solids
*/
		for (j=0;j<2;j++)
		{
			LW_stock_data[j] = LW_stock_first[j];
			for (i=0;i<LW_nstock[j];i++)
			{
				ifl = 0;
				do
				{
					ul_ipv_get_next_stock(LW_stock_data[j],&stock,&ifl,UU_FALSE);
					if (ifl == 2) break;
					if (stock->stock != 0)
						LiPrimitiveDestroy(stock->prim);
					stock->stock = 0;
				} while (ifl != -1);
/*
........Delete axis segment
*/
            if (LW_stock_data[j]->axis_seg != -1)
               ul_delaxis_ipv(LW_stock_data[j]);
				LW_stock_data[j] = (LW_stock_struc *)uu_lsnext(LW_stock_data[j]);
			}
		}
/*
.....Terminate the machinining sessions
*/
		ul_ipv_delete_sessions();
		LiMWViewportDestroy(LW_viewport);
		LW_viewport = 0;
		LiViewDestroy(LW_view);
		LW_view = 0;
/*		LiSOViewportDestroy(LW_so_viewport);*/
		if (LW_session[0] != 0) LiSessionDestroy(LW_session[0]);
		if (LW_session[1] != 0) LiSessionDestroy(LW_session[1]);
		LW_session[0] = (LtSession)0;
		LW_session[1] = (LtSession)0;
/*
.....Get rid of the imported session tool list
*/
		if (LW_ntool_sess != 0) uu_list_free(&LW_tool_sess);
		LW_ntool_sess = 0;
		LW_tool_sess_act = 0;
/*
.....Delete the motion attribute lists
*/
		if (LW_mot_data_first != UU_NULL) uu_lsdel(LW_mot_data_first);
		if (LW_mot_attr_first != UU_NULL) uu_lsdel(LW_mot_attr_first);
		LW_mot_data_first = (UN_mot_data *)UU_NULL;
		LW_mot_attr_first = (UN_mot_attr *)UU_NULL;
		LW_mot_ndata = 0;
		LW_mot_nattr = 0;
		LW_mach_tpin_ix = 0;
		LW_active = UU_FALSE;
/*
.....Clear the clash stack
*/
		ul_ipv_reset_clash_stack();
	}
}

/*********************************************************************
**   E_FUNCTION:int ul_ipv_archive_session(fname)
**      This function saves an NCLIPV Machinining Session to disk.
**   PARAMETERS
**       INPUT  : 
**          fname  = Name of file to save, or blank if the file should
**                   be prompted for.
**       OUTPUT : none.
**   RETURNS:
**          UU_SUCCESS on success, UU_FAILURE otherwise.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_ipv_archive_session(fname)
char *fname;
{
	int i,j,k,stat,inum,dinc,retstat,ifl,inc;
	int *m,nstk[2];
	char *p,*strrchr();
	char descrip[80],tbuf[80];
	UX_pathname fullname,ftemp,ext,basename,fextb;
	char sbuf[UX_MAX_PATH_LEN*2];
	LW_stock_struc *sd,*stock;
	UN_mot_attr *ad;
	UN_mot_data *dd;
	UN_cutter_list *ipt;
	FILE *fd;
	LW_mach_model_struc *mpt;
	LW_mach_solid_struc *spt;
	LW_mach_toolpin_struc *tptr;
/*
.....Make sure a session is active
*/
	m = UU_NULL;
	retstat = UU_SUCCESS;
	if (LW_session[0] == 0 || LW_mach_mode != LW_VISICUT)
	{
		if (fname[0] == '\0')
			ud_wrerr("You may only save active Visicut sessions.");
		goto done;
	}
/*
.....Prompt the user for the filename to save to
.....Files should have the following exensions
.....'.ipv' = Session attribute file.
.....'.ws1' = Stock file (1st stock, .ws2 = 2nd, etc.).
.....'.wf1' = Fixture file.
*/
	if (fname[0] == '\0')
	{
		sprintf(sbuf,"Save Machining Session");
		strcpy(ext,"*.ipv");
		strcpy(descrip,"NCLIPV Session Files (*.ipv)");
		fullname[0] = '\0';
		ud_get_filename(sbuf,sbuf,ext,fullname,&inum,descrip,0,UU_FALSE);
		if (inum == 0) goto done;
	}
	else
		strcpy(fullname,fname);

	strcpy(basename,fullname);
	p = strrchr(basename,'.');
	if (p != UU_NULL) *p = '\0';
	else strcat(fullname,".ipv");
/*
.....Move Undo Stack to end
*/
	ul_ipv_mot_stack_step(2,"");
/*
.....Save stocks and fixtures
*/
	strcpy(fextb,".ws");
	for (j=0;j<2;j++)
	{
		sd = LW_stock_first[j];
		inc = 1;
		for (i=0;i<LW_nstock[j];i++)
		{
			ifl = -1;
			do
			{
				ul_ipv_get_next_stock(sd,&stock,&ifl,UU_TRUE);
				if (ifl == -2) break;
				sprintf(ext,"%s%d",fextb,inc);
				strcpy(ftemp,basename);
				strcat(ftemp,ext);
				fd = LiFileOpen(ftemp,LI_FILE_WRITE);
				if (fd != 0)
				{
					LiMWUViSolidSaveToFile(fd,TRUE,
						LI_NU_SOLID_SAVE_NORMALS|LI_NU_SOLID_SAVE_CUT_NUMBERS,
							stock->stock);
					LiFileClose(fd);
					inc++;
				}
				else
				{
					strcpy(fullname,ftemp);
					goto filerr;
				}
			} while (ifl != -1);
			sd = (LW_stock_struc *)uu_lsnext(sd);
		}
		strcpy(fextb,".wf");
	}
/*
.....Open attribute file
*/
	stat = ux_fopen0(fullname,"w",&fd);
	if (fd == UU_NULL || stat != UU_SUCCESS) goto filerr;
/*
.....Write out header
*/
	ul_date_and_time(tbuf);
	ul_ipv_count_stocks(nstk,UU_TRUE);
	sprintf(sbuf,"NCLIPV %g %s %d %d %d %d\n",NCL_version,tbuf,nstk[0],
		nstk[1],LW_mot_nattr,LW_mot_ndata);
	stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
	if (stat != UU_SUCCESS) goto wrterr;
/*
.....Write out pp & cl filenames
*/
	strcpy(sbuf,UL_program); strcat(sbuf,"."); strcat(sbuf,UL_program_suffix);
	strcat(sbuf,"\n");
	stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
	if (stat != UU_SUCCESS) goto wrterr;
	ncl_get_playfile(sbuf);
	strcat(sbuf,"\n");
	stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
	if (stat != UU_SUCCESS) goto wrterr;
/*
.....Write out tool position
*/
	sprintf(sbuf,"%lf %lf %lf %lf %lf %lf\n",LW_tool_pos[0],LW_tool_pos[1],
		LW_tool_pos[2],LW_tool_pos[3],LW_tool_pos[4],LW_tool_pos[5]);
	stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
	if (stat != UU_SUCCESS) goto wrterr;
	sprintf(sbuf,"%lf %lf %lf %lf %lf %lf\n",LW_last_tool_pos[0],
		LW_last_tool_pos[1],LW_last_tool_pos[2],LW_last_tool_pos[3],
		LW_last_tool_pos[4],LW_last_tool_pos[5]);
	stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
	if (stat != UU_SUCCESS) goto wrterr;
/*
.....Write out the tool list
*/
	sprintf(sbuf,"%d %d %d %d %d %d %d %d %d %d %d\n",LW_ntool,LW_act_tool[0],
		LW_act_tool[1],LW_act_tool[2],LW_act_tool[3],LW_act_tool[4],
		LW_act_tool[5],LW_act_tool[6],LW_act_tool[7],LW_act_tool[8],
		LW_act_tool[9]);
	stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
	if (stat != UU_SUCCESS) goto wrterr;

	sprintf(sbuf,"%d %d %d %d %d %d %d %d %d %d %d\n",LW_spindle_num,
		LW_spindle_ix[0],LW_spindle_ix[1],LW_spindle_ix[2],LW_spindle_ix[3],
		LW_spindle_ix[4],LW_spindle_ix[5],LW_spindle_ix[6],LW_spindle_ix[7],
		LW_spindle_ix[8],LW_spindle_ix[9]);
	stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
	if (stat != UU_SUCCESS) goto wrterr;

	ipt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
	for (i=0;i<LW_ntool;i++)
	{
		sprintf(sbuf,"%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
			ipt[i].type,ipt[i].isn,ipt[i].clrec,ipt[i].color[0],ipt[i].color[1],
			ipt[i].color[2],ipt[i].cut_color,ipt[i].trans[0],ipt[i].trans[1],
			ipt[i].trans[2],ipt[i].ncparm,ipt[i].shank_clash,ipt[i].tlno,
			ipt[i].ctype[0],ipt[i].ctype[1],ipt[i].ctype[2],ipt[i].used);
		stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
		if (stat != UU_SUCCESS) goto wrterr;

		sprintf(sbuf,"%d %s %s\n",ipt[i].symkey[0],ipt[i].symlib,
			ipt[i].symbol[0]);
		stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
		if (stat != UU_SUCCESS) goto wrterr;

		sprintf(sbuf,"%s\n",ipt[i].symbol[1]);
		stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
		if (stat != UU_SUCCESS) goto wrterr;

		sprintf(sbuf,"%s\n",ipt[i].symbol[2]);
		stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
		if (stat != UU_SUCCESS) goto wrterr;

		sprintf(sbuf,"%d %d %d %d %d %d\n",ipt[i].edge[0],ipt[i].edge[1],
			ipt[i].edge[2],ipt[i].edge_color[0],ipt[i].edge_color[1],
			ipt[i].edge_color[2]);
		stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
		if (stat != UU_SUCCESS) goto wrterr;

		for (j=ipt[i].ncparm;j<7;j++) ipt[i].cutter[j] = 0.;
		sprintf(sbuf,"%lf %lf %lf %lf %lf %lf %lf\n",ipt[i].cutter[0],
			ipt[i].cutter[1],ipt[i].cutter[2],ipt[i].cutter[3],ipt[i].cutter[4],
			ipt[i].cutter[5],ipt[i].cutter[6]);
		stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
		if (stat != UU_SUCCESS) goto wrterr;

		sprintf(sbuf,"%lf %lf %lf %lf %lf %lf\n",
			ipt[i].parms[0][0],ipt[i].parms[0][1],ipt[i].parms[0][2],
			ipt[i].parms[0][3],ipt[i].parms[1][0],ipt[i].parms[1][1]);
		stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
		if (stat != UU_SUCCESS) goto wrterr;

		sprintf(sbuf,"%lf %lf %lf %lf %lf %lf\n",
			ipt[i].parms[1][2],ipt[i].parms[1][3],ipt[i].parms[2][0],
			ipt[i].parms[2][1],ipt[i].parms[2][2],ipt[i].parms[2][3]);
		stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
		if (stat != UU_SUCCESS) goto wrterr;

		sprintf(sbuf,"%lf %lf %lf %lf %lf %lf\n",
			ipt[i].toler,ipt[i].maxang,ipt[i].rapid,ipt[i].mchtim,ipt[i].tlen,
			ipt[i].tlofs);
		stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
		if (stat != UU_SUCCESS) goto wrterr;
	}
/*
.....Write out Machine Simulation parameters
*/
	sprintf(sbuf,"%d %d %d %d %d %d\n",LW_mach_type,LW_mach_simul,LW_mach_nmodel,
		LW_mach_nsolid,LW_mach_type_main,LW_is_lathe);
	stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
	if (stat != UU_SUCCESS) goto wrterr;
	if (LW_mach_simul)
	{
/*
........Machine name
*/
		sprintf(sbuf,"%s\n",LW_mach_dir);
		stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
		if (stat != UU_SUCCESS) goto wrterr;
		sprintf(sbuf,"%s\n",LW_mach_name);
		stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
		if (stat != UU_SUCCESS) goto wrterr;
/*
........Tooling pin
*/
		tptr = (LW_mach_toolpin_struc *)UU_LIST_ARRAY(&LW_mach_toolpin);
		sprintf(sbuf,"%d %d\n",LW_mach_num_tpin,LW_mach_tpin_ix);
		stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
		if (stat != UU_SUCCESS) goto wrterr;
		for (i=0;i<LW_mach_num_tpin;i++)
		{
			sprintf(sbuf,"%s %d\n",tptr[i].label,tptr[i].axis);
			stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
			if (stat != UU_SUCCESS) goto wrterr;
			for (j=0;j<3;j++)
			{
				sprintf(sbuf,"%lf %lf %lf %lf %lf %lf\n",
					tptr[i].mpin[j][0],tptr[i].mpin[j][1],tptr[i].mpin[j][2],
					tptr[i].ppin[j][0],tptr[i].ppin[j][1],tptr[i].ppin[j][2]);
				stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
				if (stat != UU_SUCCESS) goto wrterr;
			}
			for (j=0;j<3;j++)
			{
				sprintf(sbuf,"%lf %lf %lf %lf\n",tptr[i].matrix[0][j],
					tptr[i].matrix[1][j],tptr[i].matrix[2][j],
					tptr[i].matrix[3][j]);
				stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
				if (stat != UU_SUCCESS) goto wrterr;
			}
			for (j=0;j<3;j++)
			{
				sprintf(sbuf,"%lf %lf %lf %lf\n",tptr[i].invmx[0][j],
					tptr[i].invmx[1][j],tptr[i].invmx[2][j],
					tptr[i].invmx[3][j]);
				stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
				if (stat != UU_SUCCESS) goto wrterr;
			}
		}
/*
........Model structures
*/
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		for (i=0;i<LW_mach_nmodel;i++)
		{
			sprintf(sbuf,"%lf %lf %d %d %d %d %d\n",mpt[i].position,mpt[i].offset,
				mpt[i].color,mpt[i].visible,mpt[i].translucency,
				mpt[i].edge,mpt[i].edge_color);
			stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
			if (stat != UU_SUCCESS) goto wrterr;
		}
/*
........Solid structures
*/
		spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
		for (i=0;i<LW_mach_nsolid;i++)
		{
			sprintf(sbuf,"%d %d %d %d %d\n",spt[i].stock.color,
				spt[i].stock.visible,spt[i].stock.translucency,spt[i].stock.edge,
				spt[i].stock.edge_color);
			stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
			if (stat != UU_SUCCESS) goto wrterr;
		}
	}
/*
.....Write out stock commands
*/
	stat = ul_ipv_save_stock_cmds(fd,UU_FALSE,UU_TRUE,UU_NULL,UU_NULL);
	if (stat != UU_SUCCESS) goto wrterr;
/*
.....Save stock attributes
*/
	for (j=0;j<2;j++)
	{
		sd = LW_stock_first[j];
		for (i=0;i<LW_nstock[j];i++)
		{
			ifl = -1;
			do
			{
				ul_ipv_get_next_stock(sd,&stock,&ifl,UU_TRUE);
				if (ifl == -2) break;
				sprintf(sbuf,"%d %d %d %d %f %d %d %d %d %d\n",stock->color,
					stock->translucency,stock->visible,stock->active,stock->toler,
					stock->id,stock->edge,stock->edge_color,stock->important,
					stock->tpin);
				stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
				if (stat != UU_SUCCESS) goto wrterr;
				sprintf(sbuf,"%d %d %s\n",stock->mxflag,stock->invflag,sd->mxname);
				stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
				if (stat != UU_SUCCESS) goto wrterr;
				if (stock->mxflag)
				{
					for (k=0;k<3;k++)
					{
						sprintf(sbuf,"%lf %lf %lf %lf\n",stock->matrix[0][k],
							stock->matrix[1][k],stock->matrix[2][k],
							stock->matrix[3][k]);
						stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
						if (stat != UU_SUCCESS) goto wrterr;
					}
				}
				if (stock->invflag)
				{
					for (k=0;k<3;k++)
					{
						sprintf(sbuf,"%lf %lf %lf %lf\n",stock->invmx[0][k],
							sd->invmx[1][k],sd->invmx[2][k],stock->invmx[3][k]);
						stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
						if (stat != UU_SUCCESS) goto wrterr;
					}
				}
			} while (ifl != -1);
			sd = (LW_stock_struc *)uu_lsnext(sd);
		}
	}
/*
.....Write out motion attribute records
*/
	ad = LW_mot_attr_first;
	for (i=0;i<LW_mot_nattr;i++)
	{
		ad = (UN_mot_attr *)uu_lsnext(ad);
		sprintf(sbuf,"%d %d %lf %d %lf %d %d %d\n",ad->tlno,ad->loadtl,ad->tlen,
			ad->sp_mode,ad->sp_val,ad->coolnt,ad->cc_mode,ad->cc_dir);
		stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
		if (stat != UU_SUCCESS) goto wrterr;
	}
/*
.....Write out motion data records
*/
	dd = LW_mot_data_first;
	for (i=0;i<LW_mot_ndata;i++)
	{
		dd = (UN_mot_data *)uu_lsnext(dd);
		sprintf(sbuf,"%d %d %d %d %d %d %d %d %lf %d\n",dd->isn,dd->seqno,
			dd->isnptr,dd->cut[0],dd->cut[1],dd->clrec[0],dd->clrec[1],dd->fr_mode,
			dd->fr_val,dd->mattr);
		stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
		if (stat != UU_SUCCESS) goto wrterr;
	}
/*
.....Save face colors
.....by looping through each stock and fixture
*/
	m = (int *)uu_malloc(sizeof(int)*(LW_mot_ndata+25));
	if (m == UU_NULL) goto nomem;
	for (i=0;i<LW_mot_ndata+25;i++) m[i] = 0;
	dd = LW_mot_data_first;
	dd = (UN_mot_data *)uu_lsnext(dd);
	dinc = 0;
/*
........Loop through stocks and fixtures
*/
	if (LW_mot_ndata > 0)
	{
		for (j=0;j<2;j++)
		{
			sd = LW_stock_first[j];
			for (i=0;i<LW_nstock[j];i++)
			{
				ifl = -1;
				do
				{
					ul_ipv_get_next_stock(sd,&stock,&ifl,UU_TRUE);
					if (ifl == -2) break;
					ul_ipv_save_faces(stock->stock,m,&dd,&dinc);
/*
...........Write out face colors
*/
					for (k=0;k<LW_mot_ndata;k=k+25)
					{
						sprintf(sbuf,"%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d\n",
							m[k],m[k+1],m[k+2],m[k+3],m[k+4],m[k+5],m[k+6],m[k+7],
							m[k+8],m[k+9],m[k+10],m[k+11],m[k+12],m[k+13],m[k+14],
							m[k+15],m[k+16],m[k+17],m[k+18],m[k+19],m[k+20],m[k+21],
							m[k+22],m[k+23],m[k+24]);
						stat = ux_fwrite0(sbuf,strlen(sbuf),1,fd,&inum);
						if (stat != UU_SUCCESS) goto wrterr;
					}
				} while (ifl != -1);
				sd = (LW_stock_struc *)uu_lsnext(sd);
			}
		}
	}
	ux_fclose0(fd);
	goto done;
/*
.....Error opening file
*/
filerr:;
	retstat = UU_FAILURE;
	if (fname[0] == '\0')
	{
		sprintf(sbuf,"Failed to open file %s.",fullname);
		ud_wrerr(sbuf);
	}
	goto done;
/*
.....Error writing to file
*/
wrterr:;
	retstat = UU_FAILURE;
	if (fname[0] == '\0')
	{
		sprintf(sbuf,"Error writing to %s.",fullname);
		ud_wrerr(sbuf);
	}
	LiFileClose(fd);
	goto done;
/*
.....Could not allocate memory
*/
nomem:;
	retstat = UU_FAILURE;
	if (fname[0] == '\0')
		ud_wrerr("Could not allocate memory for session.");
	LiFileClose(fd);
	goto done;
/*
.....End of routine
*/
done:;
	if (m != UU_NULL) uu_free(m);
	return(retstat);
}

/*********************************************************************
**   E_FUNCTION:int ul_ipv_load_session(fname,lwin)
**      This function loads an NCLIPV Machining Session from disk.
**   PARAMETERS
**       INPUT  : none
**          fname  = Name of file to load, or blank if the file should
**                   be prompted for.
**
**          lwin   = UU_TRUE - Open the NCLIPV Window after loading
**                   session.  UU_FALSE = Don't open Window.
**       OUTPUT :
**          fname  = Full name of file loaded.
**   RETURNS:
**          UU_FAILURE if an error occurred loading session.
**          UU_SUCCESS on a successful load.
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
int ul_ipv_load_session(fname,lwin)
char *fname;
UU_LOGICAL lwin;
{
#define MINVER 10.050
	int i,j,k,inum,nc,nstk[2],stat,dinc,ndata,nattr,retstat,type,npts,inc,ix;
	int act_tool[LW_MAX_SPINDLE],nsolid,nmodel,status,cfl,itp,lastmattr,curmattr;
	int *m,iclw[6],ncid,ifl;
	UM_int2 idx,ival;
	UU_LOGICAL fnd,isav,werr,modfl;
	char *p,*strrchr();
	char descrip[80],sbuf[132],ldate[20],ltime[20],lprog[20],fbuf[80];
	UU_REAL version,rclw[12],cnv,pos[6],*axpos;
	UM_coord *pts;
	UX_pathname fullname,ftemp,ext,basename,fextb;
	UN_mot_data *dd;
	UN_mot_attr *ad;
	UN_cutter_list ipt,*tpt,*tpt1;
	LW_stock_struc *sd,*stk[2],*stock,*sptr;
	LtNat32 maxcutn;
	LtData stuff;
	FILE *fd,*fd1;
	LW_mach_model_struc *mpt;
	LW_mach_solid_struc *spt;
	LW_mach_toolpin_struc tpin;
	double length;
/*
.....Only load Visicut sessions
*/
	werr = (fname[0] == '\0');
	retstat = UU_SUCCESS;
	m = UU_NULL;
	if (LW_mach_mode != LW_VISICUT)
	{
		retstat = UU_FAILURE;
		if (werr) ud_wrerr("Only Visicut sessions can be restored.");
		goto done;
	}
/*
.....Prompt the user for the filename to load
.....Files should have the following exensions
.....'.ipv' = Session attribute file.
.....'.ws1' = Stock file (1st stock, .ws2 = 2nd, etc.).
.....'.wf1' = Fixture file.
*/
	if (fname[0] == '\0')
	{
		sprintf(sbuf,"Restore Machining Session");
		strcpy(ext,"*.ipv");
		strcpy(descrip,"NCLIPV Session Files (*.ipv)");
		fullname[0] = '\0';
		ud_get_filename(sbuf,sbuf,ext,fullname,&inum,descrip, 1,UU_FALSE);
		if (inum == 0) goto done;
		strcpy(fname,fullname);
	}
	else
		strcpy(fullname,fname);

	strcpy(basename,fullname);
	p = strrchr(basename,'.');
	if (p != UU_NULL) *p = '\0';
	else strcat(fullname,".ipv");
/*
.....Open Attribute file
*/
	fd1 = LiFileOpen(fullname,LI_FILE_READ);
	if (fd1 == UU_NULL)
	{
		retstat = UU_FAILURE;
		if (werr)
		{
			ul_short_filename(fullname,fbuf,40);
			sprintf(sbuf,"Failed to open file '%s'.",fbuf);
			ud_wrerr(sbuf);
		}
		goto done;
	}
/*
.....Get header lines
*/
	stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
	if (stat != UU_SUCCESS) goto readerr;
	sscanf(sbuf,"%s %lf %s %s %d %d %d %d",lprog,&version,ldate,ltime,
		&nstk[0],&nstk[1],&nattr,&ndata);
	if (version < MINVER) goto vererr;
	stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
	if (stat != UU_SUCCESS) goto readerr;
	stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
	if (stat != UU_SUCCESS) goto readerr;
/*
.....End the current session
.....And get rid of stocks & fixtures
*/
	isav = LW_active;
	ul_ipv_end_session();
	ul_ipv_delete_stocks();
/*
.....Read in tool position
*/
	stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
	if (stat != UU_SUCCESS) goto readerr;
	sscanf(sbuf,"%lf %lf %lf %lf %lf %lf",&LW_tool_pos[0],&LW_tool_pos[1],
		&LW_tool_pos[2],&LW_tool_pos[3],&LW_tool_pos[4],&LW_tool_pos[5]);

	stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
	if (stat != UU_SUCCESS) goto readerr;
	sscanf(sbuf,"%lf %lf %lf %lf %lf %lf",&LW_last_tool_pos[0],
		&LW_last_tool_pos[1],&LW_last_tool_pos[2],&LW_last_tool_pos[3],
		&LW_last_tool_pos[4],&LW_last_tool_pos[5]);
/*
.....Read in the tool list
*/
	stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
	if (stat != UU_SUCCESS) goto readerr;
	if (version < 10.050)
	{
		sscanf(sbuf,"%d %d\n",&LW_ntool_sess,&act_tool[0]);
		LW_spindle_num = 1;
		LW_spindle_ix[0] = 0;
	}
	else
	{
		sscanf(sbuf,"%d %d %d %d %d %d %d %d %d %d %d\n",&LW_ntool_sess,
			&act_tool[0],&act_tool[1],&act_tool[2],&act_tool[3],&act_tool[4],
			&act_tool[5],&act_tool[6],&act_tool[7],&act_tool[8],&act_tool[9]);

		stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
		if (stat != UU_SUCCESS) goto readerr;
		sscanf(sbuf,"%d %d %d %d %d %d %d %d %d %d %d\n",&LW_spindle_num,
			&LW_spindle_ix[0],&LW_spindle_ix[1],&LW_spindle_ix[2],
			&LW_spindle_ix[3],&LW_spindle_ix[4],&LW_spindle_ix[5],
			&LW_spindle_ix[6],&LW_spindle_ix[7],&LW_spindle_ix[8],
			&LW_spindle_ix[9]);
	}

	if (LW_ntool_sess > 0)
	{
		uu_list_init(&LW_tool_sess,sizeof(UN_cutter_list),LW_ntool_sess,2);
		for (i=0;i<LW_ntool_sess;i++)
		{
			stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
			if (stat != UU_SUCCESS) goto readerr;
			if (version < 9.357)
			{
				sscanf(sbuf,"%d %d %d %d %d %d %d %d %d",&ipt.isn,&ipt.clrec,
					&ipt.ctype[2],&ipt.color[0],&ipt.cut_color,&ipt.color[2],
					&ipt.trans[0],&ipt.ncparm,&ipt.used);
				ipt.type = NCL_CUTTER_MILL;
				ipt.color[1] = LW_default_tool.shank_color;
				ipt.shank_clash = LW_default_tool.shank_clash;
				ipt.tlno = 0;
				ipt.trans[1] = ipt.trans[2] = ipt.trans[0];
			}
			else if (version < 9.550)
			{
				sscanf(sbuf,"%d %d %d %d %d %d %d %d %d %d %d %d %d %d",&ipt.type,
					&ipt.isn,&ipt.clrec,&ipt.ctype[2],&ipt.color[0],&ipt.cut_color,
					&ipt.color[1],&ipt.color[2],&ipt.trans[0],&ipt.ncparm,
					&ipt.shank_clash,&ipt.tlno,&ipt.used,&ipt.ctype[1]);
				ipt.trans[1] = ipt.trans[2] = ipt.trans[0];
			}
			else
			{
				sscanf(sbuf,"%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
					&ipt.type,&ipt.isn,&ipt.clrec,&ipt.color[0],&ipt.color[1],
					&ipt.color[2],&ipt.cut_color,&ipt.trans[0],&ipt.trans[1],
					&ipt.trans[2],&ipt.ncparm,&ipt.shank_clash,&ipt.tlno,
					&ipt.ctype[0],&ipt.ctype[1],&ipt.ctype[2],&ipt.used);
			}

			stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
			if (stat != UU_SUCCESS) goto readerr;
			ipt.symbol[0][0] = ipt.symbol[1][0] = ipt.symbol[2][0] = '\0';
			sscanf(sbuf,"%d %s %s",&ipt.symkey[0],ipt.symlib,ipt.symbol[0]);
			ipt.symkey[0] = ipt.symkey[1] = ipt.symkey[2] = 0;

			if (version < 9.550)
			{
				ipt.ctype[0] = 1;
				if (ipt.ctype[2] == 0)
				{
					if (ipt.symbol[0][0] != '\0') ipt.ctype[0] = 2;
				}
				else
				{
					ipt.ctype[2] = 2;
					strcpy(ipt.symbol[2],ipt.symbol[0]);
					ipt.symbol[0][0] = '\0';
				}
				ipt.symbol[1][0] = '\0';
				ipt.edge[0] = ipt.edge[1] = ipt.edge[2] = 0;
				ipt.edge_color[0] = ipt.edge_color[1] = ipt.edge_color[2] = 0;
			}
			else
			{
				stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
				if (stat != UU_SUCCESS) goto readerr;
				sscanf(sbuf,"%s",ipt.symbol[1]);

				stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
				if (stat != UU_SUCCESS) goto readerr;
				sscanf(sbuf,"%s",ipt.symbol[2]);

				stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
				if (stat != UU_SUCCESS) goto readerr;
				sscanf(sbuf,"%d %d %d %d %d %d",&ipt.edge[0],&ipt.edge[1],
					&ipt.edge[2],&ipt.edge_color[0],&ipt.edge_color[1],
					&ipt.edge_color[2]);
			}

			stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
			if (stat != UU_SUCCESS) goto readerr;
			sscanf(sbuf,"%lf %lf %lf %lf %lf %lf %lf",&ipt.cutter[0],
				&ipt.cutter[1],&ipt.cutter[2],&ipt.cutter[3],
				&ipt.cutter[4],&ipt.cutter[5],&ipt.cutter[6]);

			stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
			if (stat != UU_SUCCESS) goto readerr;
			if (version < 9.357)
			{
				sscanf(sbuf,"%lf %lf %lf %lf",&ipt.parms[2][0],&ipt.toler,
					&ipt.maxang,&ipt.rapid);
				ipt.parms[0][0] = ipt.parms[0][1] = ipt.parms[0][2] = 0.;
				ipt.parms[0][3] = 0.;
				ipt.parms[1][0] = ipt.parms[1][1] = ipt.parms[1][2] = 0.;
				ipt.parms[1][3] = 0.;
				ipt.parms[2][1] = ipt.parms[2][2] = ipt.parms[2][3] = 0.;
				ipt.mchtim = 0.;
			}
			else if (version < 9.550)
			{
				sscanf(sbuf,"%lf %lf %lf %lf %lf %lf %lf\n",&ipt.parms[2][0],
					&ipt.parms[2][1],&ipt.parms[2][2],&ipt.parms[2][3],
					&ipt.toler,&ipt.maxang,&ipt.rapid);

				stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
				if (stat != UU_SUCCESS) goto readerr;
				sscanf(sbuf,"%lf %lf %lf %lf %lf %lf %lf\n",&ipt.mchtim,
					&ipt.parms[1][0],&ipt.parms[1][1],&ipt.parms[1][2],
					&ipt.parms[1][3],&ipt.tlen,&ipt.tlofs);

				ipt.parms[0][0] = ipt.parms[0][1] = ipt.parms[0][2] = 0.;
				ipt.parms[0][3] = 0.;
			}
			else
			{
				sscanf(sbuf,"%lf %lf %lf %lf %lf %lf",
					&ipt.parms[0][0],&ipt.parms[0][1],&ipt.parms[0][2],
					&ipt.parms[0][3],&ipt.parms[1][0],&ipt.parms[1][1]);

				stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
				if (stat != UU_SUCCESS) goto readerr;
				sscanf(sbuf,"%lf %lf %lf %lf %lf %lf",
					&ipt.parms[1][2],&ipt.parms[1][3],&ipt.parms[2][0],
					&ipt.parms[2][1],&ipt.parms[2][2],&ipt.parms[2][3]);

				stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
				if (stat != UU_SUCCESS) goto readerr;
				sscanf(sbuf,"%lf %lf %lf %lf %lf %lf\n",
					&ipt.toler,&ipt.maxang,&ipt.rapid,&ipt.mchtim,&ipt.tlen,
					&ipt.tlofs);
			}
			uu_list_push(&LW_tool_sess,&ipt);
/*
........Make sure active tool
........is stored in main tool list
*/
			fnd = UU_FALSE;
			for (j=0;j<LW_spindle_num;j++) if (i == act_tool[j]) fnd = UU_TRUE;
			if (fnd)
			{
				if (LW_ntool == 0)
				{
					uu_list_init(&LW_tool_list,sizeof(UN_cutter_list),50,50);
					uu_list_push(&LW_tool_list,&ipt);
					LW_tool_sess_act = i;
					itp = 0;
					LW_ntool = 1;
				}
				else
				{
					tpt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
					fnd = UU_FALSE;
					for (j=0;j<LW_ntool;j++)
					{
						if (ipt.isn == tpt[i].isn && ipt.clrec == tpt[i].clrec)
						{
							fnd = ncl_cutter_same(ipt.cutter,ipt.ncparm,
								tpt[i].cutter,tpt[i].ncparm);
							if (fnd)
							{
								LW_tool_sess_act = j;
								itp = j;
							}
							break;
						}
					}
					if (!fnd)
					{
						uu_list_push(&LW_tool_list,&ipt);
						LW_tool_sess_act = LW_ntool;
						itp = LW_ntool;
						LW_ntool++;
					}
				}
			}
		}
	}
/*
.....Machine Simulation parameters
*/
	if (version < 9.357)
	{
		LW_mach_type = LW_MILL;
		LW_mach_simul = UU_FALSE;
		LW_mach_type_main = LW_mach_type;
		LW_is_lathe = UU_FALSE;
	}
	else if (version < 9.754)
	{
		stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
		if (stat != UU_SUCCESS) goto readerr;
		sscanf(sbuf,"%d %d %d %d",&LW_mach_type,&LW_mach_simul,&nmodel,
			&nsolid);
		LW_mach_type_main = LW_mach_type;
		LW_is_lathe = LW_mach_type == LW_LATHE;
	}
	else
	{
		stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
		if (stat != UU_SUCCESS) goto readerr;
		sscanf(sbuf,"%d %d %d %d %d %d",&LW_mach_type,&LW_mach_simul,&nmodel,
			&nsolid,&LW_mach_type_main,&LW_is_lathe);
	}
	if (LW_mach_simul)
	{
/*
........Machine name
*/
		stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
		if (stat != UU_SUCCESS) goto readerr;
		sscanf(sbuf,"%s",LW_mach_dir);
		stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
		if (stat != UU_SUCCESS) goto readerr;
		sscanf(sbuf,"%s",LW_mach_name);
/*
........Load machine
*/
		status = ul_ipv_load_mach(LW_mach_dir,LW_mach_name,fullname,&LW_mach_data,
			&LW_mach_model,&LW_mach_solid,&LW_mach_nmodel,&LW_mach_nsolid,
			LW_spindle,&LW_mach_toolpin,&LW_mach_num_tpin);
		if (status != UU_SUCCESS || LW_mach_nmodel == 0) LW_mach_simul = UU_FALSE;
/*
........Tooling pin
*/
		stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
		if (stat != UU_SUCCESS) goto readerr;
		sscanf(sbuf,"%d %d",&LW_mach_num_tpin,&LW_mach_tpin_ix);
		UU_LIST_EMPTY(&LW_mach_toolpin);
		for (i=0;i<LW_mach_num_tpin;i++)
		{
			stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
			if (stat != UU_SUCCESS) goto readerr;
			sscanf(sbuf,"%s %d",tpin.label,&tpin.axis);
			for (j=0;j<3;j++)
			{
				stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
				if (stat != UU_SUCCESS) goto readerr;
				sscanf(sbuf,"%lf %lf %lf %lf %lf %lf",
					&tpin.mpin[j][0],&tpin.mpin[j][1],&tpin.mpin[j][2],
					&tpin.ppin[j][0],&tpin.ppin[j][1],&tpin.ppin[j][2]);
			}
			for (j=0;j<3;j++)
			{
				stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
				if (stat != UU_SUCCESS) goto readerr;
				sscanf(sbuf,"%lf %lf %lf %lf\n",&tpin.matrix[0][j],
					&tpin.matrix[1][j],&tpin.matrix[2][j],
					&tpin.matrix[3][j]);
			}
			for (j=0;j<3;j++)
			{
				stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
				if (stat != UU_SUCCESS) goto readerr;
				sscanf(sbuf,"%lf %lf %lf %lf\n",&tpin.invmx[0][j],
					&tpin.invmx[1][j],&tpin.invmx[2][j],
					&tpin.invmx[3][j]);
			}
			uu_list_push(&LW_mach_toolpin,&tpin);
		}
/*
........Model structures
*/
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		axpos = UU_NULL;
		if (nmodel == LW_mach_nmodel)
			axpos = (UU_REAL *)uu_malloc(sizeof(UU_REAL)*nmodel);
		for (i=0;i<nmodel;i++)
		{
			stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
			if (stat != UU_SUCCESS) goto readerr;
			if (nmodel == LW_mach_nmodel)
			{
				sscanf(sbuf,"%lf %lf %d %d %d %d %d",&axpos[i],&mpt[i].offset,
					&mpt[i].color,&mpt[i].visible,&mpt[i].translucency,
					&mpt[i].edge,&mpt[i].edge_color);
			}
		}
/*
........Solid structures
*/
		spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
		for (i=0;i<nsolid;i++)
		{
			stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
			if (stat != UU_SUCCESS) goto readerr;
			if (nsolid == LW_mach_nsolid)
			{
				sscanf(sbuf,"%d %d %d %d %d",&spt[i].stock.color,
					&spt[i].stock.visible,&spt[i].stock.translucency,
					&spt[i].stock.edge,&spt[i].stock.edge_color);
			}
		}
	}
/*
.....Restore stock and fixture files
*/
	strcpy(fextb,".ws");
	for (j=0;j<2;j++)
	{
		inum = 1;
		for (i=0;i<nstk[j];i++)
		{
			sprintf(ext,"%s%d",fextb,i+1);
			strcpy(ftemp,basename);
			strcat(ftemp,ext);
			fd = LiFileOpen(ftemp,LI_FILE_READ);
			if (fd != UU_NULL)
			{
				LiFileClose(fd);
				nc = strlen(ftemp);
				ul_ipv_add_stock(j,LW_STOCK_SESSION,UU_NULL,&inum,ftemp,nc,0);
			}
			else
			{
				ul_short_filename(ftemp,fbuf,40);
				sprintf(sbuf,"Failed to open file '%s'.",fbuf);
				ud_wrerr(sbuf);
				break;
			}
		}
		strcpy(fextb,".wf");
	}
/*
.....Load the stock definitions
*/
	stk[0] = UU_NULL ; stk[1] = UU_NULL;
	if (nstk[0] > 0)
	{
		stk[0] = (LW_stock_struc *)uu_malloc(sizeof(LW_stock_struc)*nstk[0]);
		if (stk[0] == UU_NULL) goto memerr;
	}
	if (nstk[1] > 0)
	{
		stk[1] = (LW_stock_struc *)uu_malloc(sizeof(LW_stock_struc)*nstk[1]);
		if (stk[1] == UU_NULL) goto memerr;
	}
	stat = ul_ipv_load_stock_cmds(fd1,UU_FALSE,&inum,stk,nstk,UU_TRUE);
	if (stat != UU_SUCCESS) goto readerr;
/*
.....Load the stock attributes
*/
	for (j=0;j<2;j++)
	{
		sd = LW_stock_first[j];
		for (i=0;i<nstk[j];i++)
		{
			stk[j][i].id = 0;
			stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
			if (stat != UU_SUCCESS) goto readerr;
			if (version > 9.650)
			{
				sscanf(sbuf,"%d %d %d %d %lf %d %d %d %d %d",&(stk[j][i].color),
					&(stk[j][i].translucency),&(stk[j][i].visible),
					&(stk[j][i].active),&(stk[j][i].toler),&(stk[j][i].id),
					&(stk[j][i].edge),&(stk[j][i].edge_color),
					&(stk[j][i].important),&(stk[j][i].tpin));
			}
			else
			{
				sscanf(sbuf,"%d %d %d %d %lf %d %d %d",&(stk[j][i].color),
					&(stk[j][i].translucency),&(stk[j][i].visible),
					&(stk[j][i].active),&(stk[j][i].toler),&(stk[j][i].id),
					&(stk[j][i].edge),&(stk[j][i].edge_color));
				stk[j][i].important = 1;
			}
			if (stk[j][i].id == 0) stk[j][i].id = LW_stock_idn[j]++;
			if (version > 9.550)
			{
				stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
				if (stat != UU_SUCCESS) goto readerr;
				sscanf(sbuf,"%d %d %s",&(stk[j][i].mxflag),&(stk[j][i].invflag),
					stk[j][i].mxname);
				if (stk[j][i].mxflag)
				{
					for (k=0;k<3;k++)
					{
						stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
						if (stat != UU_SUCCESS) goto readerr;
						sscanf(sbuf,"%lf %lf %lf %lf",&(stk[j][i].matrix[0][k]),
							&(stk[j][i].matrix[1][k]),&(stk[j][i].matrix[2][k]),
							&(stk[j][i].matrix[3][k]));
					}
					stk[j][i].mxchg = UU_TRUE;
				}
				if (stk[j][i].invflag)
				{
					for (k=0;k<3;k++)
					{
						stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
						if (stat != UU_SUCCESS) goto readerr;
						sscanf(sbuf,"%lf %lf %lf %lf",&(stk[j][i].invmx[0][k]),
							&(stk[j][i].invmx[1][k]),&(stk[j][i].invmx[2][k]),
							&(stk[j][i].invmx[3][k]));
					}
				}
			}
			sd->tpin = stk[j][i].tpin;
			sd = (LW_stock_struc *)uu_lsnext(sd);
		}
	}
/*
.....Start new session
*/
	ul_ipv_start_session(UU_TRUE,UU_FALSE);
	if (!isav && lwin)
	{
		if (LW_monitor) ul_ipv_monitor_form();
		ul_ipv_open_window();
	}
	if (lwin || isav) LW_active = UU_TRUE;
/*
.....Update the stock definitions
*/
	inc = 0;
	for (j=0;j<2;j++)
	{
		sd = LW_stock_first[j];
		for (i=0;i<nstk[j];i++)
		{
			sd->type = stk[j][i].type;
			sd->id = stk[j][i].id;
			sd->color = stk[j][i].color;
			sd->translucency = stk[j][i].translucency;
			sd->visible = stk[j][i].visible;
			sd->edge = stk[j][i].edge;
			sd->edge_color = stk[j][i].edge_color;
			sd->active = stk[j][i].active;
			sd->toler = stk[j][i].toler;
			sd->bin = stk[j][i].bin;
			sd->important = stk[j][i].important;
			sd->tpin = stk[j][i].tpin;
			uu_free(sd->data);
			sd->data = stk[j][i].data;
			sd->mxflag = stk[j][i].mxflag;
			sd->invflag = stk[j][i].invflag;
			if (sd->mxflag)
			{
				sd->mxchg = UU_TRUE;
				um_tftotf(stk[j][i].matrix,sd->matrix);
				if (sd->invflag) um_tftotf(stk[j][i].invmx,sd->invmx);
			}
/*
			if (LW_mach_simul)
			{
				sd->mxflag = UU_FALSE;
				sd->placed = UU_FALSE;
				ul_ipv_place_stock(sd,UU_FALSE,&modfl);
			}
			else
*/
				ul_ipv_modify_stock(sd,UU_FALSE);
/*
........Composite stock
........Store sub-stocks in composite structure
*/
			if (sd->type == LW_STOCK_COMPOS || inc != 0)
			{
				if (inc == 0)
				{
					ncid = sd->bin;
					sd->data = (UU_REAL *)uu_malloc(sizeof(LW_stock_struc)*ncid);
					sptr = (LW_stock_struc *)sd->data;
					stock = (LW_stock_struc *)uu_lsnext(sd);
				}
				if (inc < ncid)
				{
					sd = &sptr[inc];
					*sd = *stock;
					stock = ul_ipv_delist_stock(j,stock);
					inc++;
				}
				else
				{
/*					sd = (LW_stock_struc *)uu_lsnext(&stock);*/
					sd = stock;
					inc = 0;
				}
			}
/*
........Point to next stock
*/
			else
				sd = (LW_stock_struc *)uu_lsnext(sd);
		}
	}
	if (stk[0] != UU_NULL) uu_free(stk[0]);
	if (stk[1] != UU_NULL) uu_free(stk[1]);
	ul_ipv_delete_sessions();
/*
.....Read in motion attribute records
*/
	LW_mot_nattr = nattr;
	for (i=0;i<LW_mot_nattr;i++)
	{
		LW_mot_attr = (UN_mot_attr *)uu_lsinsrt((char *)LW_mot_attr,
			sizeof(UN_mot_attr));
		if (LW_mot_attr == UU_NULL) goto memerr;
		stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
		if (stat != UU_SUCCESS) goto readerr;
		sscanf(sbuf,"%d %d %lf %d %lf %d %d %d",&LW_mot_attr->tlno,
			&LW_mot_attr->loadtl,&LW_mot_attr->tlen,
			&LW_mot_attr->sp_mode,&LW_mot_attr->sp_val,&LW_mot_attr->coolnt,
			&LW_mot_attr->cc_mode,&LW_mot_attr->cc_dir);
		LW_mot_attr->tlno = LW_mot_attr->tlno * -1;
	}
/*
.....Read in motion data records
*/
	LW_mot_ndata = ndata;
	ad = LW_mot_attr_first;
	lastmattr = 0;
	maxcutn = 0;
	for (i=0;i<LW_mot_ndata;i++)
	{
		LW_mot_data = (UN_mot_data *)uu_lsinsrt((char *)LW_mot_data,
			sizeof(UN_mot_data));
		if (LW_mot_data == UU_NULL) goto memerr;
		stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
		if (stat != UU_SUCCESS) goto readerr;
		if (version < 9.357)
		{
			LW_mot_data->seqno = 0;
			LW_mot_data->isnptr = -1;
			sscanf(sbuf,"%d %d %d %d %d %d %lf %d",&LW_mot_data->isn,
				&LW_mot_data->cut[0],&LW_mot_data->cut[1],&LW_mot_data->clrec[0],
				&LW_mot_data->clrec[1],&LW_mot_data->fr_mode,&LW_mot_data->fr_val,
				&LW_mot_data->mattr);
		}
		else if (version < 9.750)
		{
			LW_mot_data->isnptr = -1;
			sscanf(sbuf,"%d %d %d %d %d %d %d %lf %d",&LW_mot_data->isn,
				&LW_mot_data->seqno,
				&LW_mot_data->cut[0],&LW_mot_data->cut[1],&LW_mot_data->clrec[0],
				&LW_mot_data->clrec[1],&LW_mot_data->fr_mode,&LW_mot_data->fr_val,
				&LW_mot_data->mattr);
		}
		else
		{
			sscanf(sbuf,"%d %d %d %d %d %d %d %d %lf %d",&LW_mot_data->isn,
				&LW_mot_data->seqno,&LW_mot_data->isnptr,
				&LW_mot_data->cut[0],&LW_mot_data->cut[1],&LW_mot_data->clrec[0],
				&LW_mot_data->clrec[1],&LW_mot_data->fr_mode,&LW_mot_data->fr_val,
				&curmattr);
		}
		if (curmattr != lastmattr)
		{
			lastmattr = curmattr;
			ad = (UN_mot_attr *)uu_lsnext(ad);
		}
		LW_mot_data->mattr = ad;
		if (LW_mot_data->cut[1] > maxcutn) maxcutn = LW_mot_data->cut[1];
	}
/*
.....Set the next cut number
*/
	maxcutn++;
	LiDataSetNat32(&stuff,maxcutn);
	LiSessionSetProperty(LW_session[LW_mach_mode],
		LI_SESS_PROP_MW_CUT_NUMBER,&stuff);
/*
.....Restore face colors
.....by looping through each stock and fixture
*/
	if (LW_mot_ndata > 0)
	{
		m = (int *)uu_malloc(sizeof(int)*(LW_mot_ndata*25));
		if (m == UU_NULL) goto memerr;
		dd = LW_mot_data_first;
		dd = (UN_mot_data *)uu_lsnext(dd);
		dinc = 0;
		for (j=0;j<2;j++)
		{
			sd = LW_stock_first[j];
			for (i=0;i<LW_nstock[j];i++)
			{
				ifl = -1;
				do
				{
					ul_ipv_get_next_stock(sd,&stock,&ifl,UU_TRUE);
					if (ifl == -2) break;
/*
...........Read face colors
*/
					for (k=0;k<LW_mot_ndata;k=k+25)
					{
						stat = ul_fread(fd1,sbuf,sizeof(sbuf),&nc);
						if (stat != UU_SUCCESS) goto readerr;
						sscanf(sbuf,"%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
							&m[k+0],&m[k+1],&m[k+2],&m[k+3],&m[k+4],&m[k+5],&m[k+6],
							&m[k+7],&m[k+8],&m[k+9],&m[k+10],&m[k+11],&m[k+12],
							&m[k+13],&m[k+14],&m[k+15],&m[k+16],&m[k+17],&m[k+18],
							&m[k+19],&m[k+20],&m[k+21],&m[k+22],&m[k+23],&m[k+24]);
					}
/*
...........Set face colors
*/
					ul_ipv_restore_faces(stock->stock,m,&dd,&dinc);
				} while (ifl != -1);
				sd = (LW_stock_struc *)uu_lsnext(sd);
			}
		}
	}
/*
.....Position cutter
*/
	if (LW_ntool > 0)
	{
		
		tpt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_sess);
		for (inc=0;inc<LW_spindle_num;inc++)
		{
/*
........Load tool symbols
*/
			for (i=0;i<3;i++)
			{
				if (tpt[act_tool[inc]].ctype[i] == 2 ||
					tpt[act_tool[inc]].ctype[i] == 3)
				{
					cfl = 0;
					stat = ncl_load_cutter_symbol(tpt[act_tool[inc]].symlib,
						tpt[act_tool[inc]].symbol[i],&tpt[act_tool[inc]].symkey[i],
						cfl,&type);
					if (stat == UU_SUCCESS)
					{
						tpt1 = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
						strcat(tpt1[itp].symbol[i],"_ipv");
						strcpy(tpt[act_tool[inc]].symbol[i],tpt1[itp].symbol[i]);
						tpt1[itp].symkey[i] = tpt[act_tool[inc]].symkey[i];
						if (type != 0) tpt1[itp].ctype[i] = 3;
						else tpt1[itp].ctype[i] = 2;
					}
				}
				else if (tpt[act_tool[inc]].ctype[i] == 4)
				{
					ncl_load_cutprof(tpt[act_tool[inc]].symbol[i],&pts,&npts, &length);
				}
			}
		}
/*
........Position cutter
*/
		if (LW_mach_type == LW_LATHE && !LW_mach_simul)
		{
			pos[0] = LW_tool_pos[2];
			pos[1] = LW_tool_pos[0];
			pos[2] = LW_tool_pos[1];
		}
		else
		{
			for (i=0;i<6;i++) pos[i] = LW_tool_pos[i];
		}
/*
........Process cutter for IPV
*/
		idx = 264;
		getifl(&idx,&ival);
		if (ival == 0) cnv = 1.;
		else cnv = 25.4;
		ul_ipv_set_colors();
		LW_spindle_nload = 1;
		for (inc=0;inc<LW_spindle_num;inc++)
		{
			LW_spindle_load[0] = inc;
			iclw[0] = tpt[act_tool[inc]].isn;
			iclw[4] = tpt[act_tool[inc]].ncparm;
			iclw[5] = tpt[act_tool[inc]].clrec;
			ncl_process_ipvcutter(0,iclw,rclw,tpt[act_tool[inc]].ctype,
				&LW_tool_list,tpt[act_tool[inc]].symbol[0],
				tpt[act_tool[inc]].symbol[1],tpt[act_tool[inc]].symbol[2],
				tpt[act_tool[inc]].symkey);
			ul_ipv_cutter(pos,cutdef[0].cutr,LW_mot_data,LW_mot_attr,0,inc);
		}
	}
/*
.....Position machine axes
*/
	if (LW_mach_simul && LW_mach_nmodel == nmodel && axpos != UU_NULL)
	{
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		for (i=0;i<LW_mach_nmodel;i++) mpt[i].position = axpos[i];
		ul_ipv_place_axes();
		uu_free(axpos);
	}
	goto done;
/*
.....Could not allocate memory
*/
memerr:;
	retstat = UU_FAILURE;
	if (werr) ud_wrerr("Could not allocate memory for session.");
	LiFileClose(fd);
	goto done;
/*
.....Error writing to file
*/
readerr:;
	retstat = UU_FAILURE;
	if (werr)
	{
		ul_short_filename(fullname,fbuf,40);
		sprintf(sbuf,"Error reading from '%s'.",fbuf);
		ud_wrerr(sbuf);
	}
	LiFileClose(fd);
	goto done;
/*
....Could not read older version
*/
vererr:;
	retstat = UU_FAILURE;
	if (werr)
	{
		sprintf(sbuf,"Session files must be Version %2.3f and above.",MINVER);
		ud_wrerr(sbuf);
	}
	goto done;
	
/*
.....End of routine
*/
done:;
	if (m != UU_NULL) uu_free(m);
	return(retstat);
}

/*********************************************************************
**   E_FUNCTION:int ul_ipv_save_session()
**      This function saves an internal copy of the current NCLIPV
**      Machinining Session.
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_save_session()
{
/*
.....Make sure a session is active
*/
	if (LW_session[0] == 0 || LW_mach_mode != LW_VISICUT)
		ud_wrerr("You may only save active Visicut sessions.");
/*
.....Save the current session
*/
	else
		ul_ipv_save_sess(&LW_session_save,LW_tool_zhgt);
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_save_sess(session,zhgt)
**       Saves an internal copy of the active NCLIPV session.
**    PARAMETERS
**       INPUT  :
**          zhgt      = Current tool height.
**       OUTPUT :
**          session   = Created session.
**    RETURNS      : none
**    SIDE EFFECTS : none
**       If the output session is already defined, then it is deleted.
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_save_sess(session,zhgt)
LtSavedMWEnvironment *session;
UU_REAL *zhgt;
{
	int i,inc;
	UN_cutter_list *cpt;
	LtDoublePoint tool_pos;
	LtDoubleVector tool_vec;
	LtPrim tprim[LW_MAX_SPINDLE],sprim[LW_MAX_SPINDLE];
	LtPrim hprim[LW_MAX_SPINDLE][LW_MAX_HOLDER];
	LtData stuff;
	static LtDoubleVector x_axis={0.,1.,0.};
/*
.....Save the current tool position
*/
	if (LW_tool[0] != (LtSessionPrim)0 && (!LW_mach_simul || !LW_is_lathe))
	{
		for (inc=0;inc<LW_spindle_num;inc++)
		{
			cpt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
			LiViToolGetPosition(LW_tool[inc],tool_pos);
			if (LW_mach_type == LW_MILL || LW_mach_type == LW_STRINGER)
				LiViToolGetAxis(LW_tool[inc],tool_vec);
/*
.....Don't save the tool with the session
*/
			LiSessionPrimGetProperty(LW_tool[inc],LI_SPRIM_PROP_PRIM,&stuff);
			tprim[inc] = (LtPrim)LiDataGetGenericPtr(&stuff);
			if (cpt[LW_act_tool[inc]].type == NCL_CUTTER_LATHE)
				LiViTurningToolSelect(LW_tool[inc],LW_lathe,NULL,FALSE);
			else
				LiViToolSelect(LW_tool[inc],FALSE);
			LiSessionRemovePrim(LW_tool[inc]);
			if (LW_shank[inc] != 0)
			{
				LiSessionPrimGetProperty(LW_shank[inc],LI_SPRIM_PROP_PRIM,&stuff);
				sprim[inc] = (LtPrim)LiDataGetGenericPtr(&stuff);
				LiSessionRemovePrim(LW_shank[inc]);
			}
			for (i=0;i<LW_num_holder[inc];i++)
			{
				LiSessionPrimGetProperty(LW_holder[inc][i],LI_SPRIM_PROP_PRIM,
					&stuff);
				hprim[inc][i] = (LtPrim)LiDataGetGenericPtr(&stuff);
				LiSessionRemovePrim(LW_holder[inc][i]);
			}
		}
	}
/*
.....Don't save the viewing axis
*/
	um_delv_axis_ipv();
/*
.....Delete an existing session
*/
	if (*session != 0) LiMWEnvironmentDestroy(*session);
/*
.....Save the actual session
*/
	*session = LiMWEnvironmentSave(LW_session[LW_mach_mode]);
/*
.....Restore the tool
*/
	if (LW_tool[0] != (LtSessionPrim)0 && (!LW_mach_simul || !LW_is_lathe))
	{
		for (inc=0;inc<LW_spindle_num;inc++)
		{
			LW_tool[inc] = LiSessionAddPrim(LW_session[LW_mach_mode],tprim[inc]);
			if (cpt[LW_act_tool[inc]].type == NCL_CUTTER_LATHE)
				LiViTurningToolSelect(LW_tool[inc],LW_lathe,x_axis,TRUE);
			else
				LiViToolSelect(LW_tool[inc],TRUE);
			if (LW_shank[inc] != (LtSessionPrim)0)
			{
				LW_shank[inc] = LiSessionAddPrim(LW_session[LW_mach_mode],
					sprim[inc]);
				LiViToolAttachHolder(LW_tool[inc],LW_shank[inc],0.,TRUE);
			}
			for (i=0;i<LW_num_holder[inc];i++)
			{
				LW_holder[inc][i] =
					LiSessionAddPrim(LW_session[LW_mach_mode],hprim[inc][i]);
				LiViToolAttachHolder(LW_tool[inc],LW_holder[inc][i],0.,TRUE);
			}
		}
/*
.....Set tool clashes
*/
		ul_ipv_tool_clash();
/*
.....Restore cut colors
*/
		ul_ipv_reset_tool_props();
/*
.....Attach to machine
*/
		if (LW_mach_simul)
		{
			for (inc=0;inc<LW_spindle_num;inc++)
				ul_ipv_tool_assembly(tool_pos,tool_vec,zhgt[inc],inc);
		}
/*
.....Restore the tool position
*/
		else
		{
			LiViToolSetPosition(LW_tool[0],tool_pos);
			if (LW_mach_type == LW_MILL || LW_mach_type == LW_STRINGER)
				LiViToolSetAxis(LW_tool[0],tool_vec);
		}
	}
/*
.....Restore the viewing axis
*/
	ul_ipv_view_segs();
}

/*********************************************************************
**   E_FUNCTION:int ul_ipv_restore_session()
**      This function restores the previously saved NCLIPV Machinining
**      Session.
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_restore_session()
{
	int i;
/*
.....Only restore Visicut sessions
*/

	if (LW_mach_mode != LW_VISICUT)
	{
		ud_wrerr("Only Visicut sessions can be restored.");
		goto done;
	}
/*
.....Restore the session
*/
	if (LW_session_save != 0)
	{
/*
........Get rid of active tool
*/
		ul_ipv_deselect_tool();
/*
.....Delete the viewing axis
*/
		if (LW_active) um_delv_axis_ipv();
/*
........Display new session
*/
		LiMWEnvironmentRestore(LW_session_save);
		if (LW_active)
		{
			ul_ipv_flush();
			um_reset_pocket_graphics(UM_IPV_WINDOW);
/*
.....Restore the viewing axis
*/
			ul_ipv_view_segs();
		}
	}
/*
.....No session to restore
*/
	else
	{
		ud_wrerr("There is no saved session to restore.");
	}
done:;
}

/*********************************************************************
**   E_FUNCTION:int ul_ipv_reset_session(iface)
**      This function resests the NCLIPV Machinining Session.
**   PARAMETERS
**       INPUT  :
**          iface   = UU_TRUE = Call is from the interface.
**                    UU_FALSE = Call is internally generated.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_reset_session(iface)
UU_LOGICAL iface;
{
	int status;
/*
.....Restart the current machinining sessions
*/
	if (LW_active)
	{
		ul_ipv_enable_stl_form(UU_FALSE);
		ul_ipv_set_defered();
		ul_ipv_end_session();
		if (iface && LW_delete_stocks) ul_ipv_delete_stocks();
		LW_active = UU_TRUE;
		status = ul_ipv_start_session(UU_TRUE,UU_TRUE);
		if (status == UU_SUCCESS)
		{
			ul_ipv_view_active();
			uz_repaint(1);
			ul_ipv_flush();
			um_reset_pocket_graphics(UM_IPV_WINDOW);
			ul_ipv_set_immediate(UU_TRUE);
		}
		else
			LW_active = UU_FALSE;
		ul_ipv_enable_stl_form(UU_TRUE);
	}
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**   E_FUNCTION:int ul_ipv_delete_sessions()
**      This function deletes any saved NCLIPV Machining sessions.
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_delete_sessions()
{
/*
.....Delete the saved session
*/
	if (LW_session_save != 0) LiMWEnvironmentDestroy(LW_session_save);
	if (LW_env != 0) LiMWEnvironmentDestroy(LW_env);
	ul_ipv_mot_stack_del_sess();
	LW_session_save = 0;
	LW_env = 0;
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_save_faces(stock,fc,md,fcinc)
**      This function saves the face colors based on the cut numbers.
**   PARAMETERS
**       INPUT  : stock     = Stock to save face colors for.
**                fc        = Allocated integer array of size
**                            LW_mot_ndata+25.  This array should be set
**                            zeroed out prior to the first call and
**                            then left alone for the subsequent calls
**                            (multiple stocks/fixtures).
**                md        = Pointer to beginning of motion data list
**                            (LW_mot_data_first+1) at first call and
**                            left alone for subsequent calls.
**                fcinc     = Initialized to zero at first call and
**                            left alone for subsequent calls.
**       OUTPUT : fc        = Updated face color array.
**                md        = Updated motion array pointer.
**                fcinc     = Updated face color pointer.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_save_faces(stock,fc,md,fcinc)
LtSessionPrim stock;
int *fc;
UN_mot_data **md;
int *fcinc;
{
	int i,k,inc,cut;
	LtBody sbody;
	LtFace sface;
	LtMaterial mat;
	UN_mot_data *dd;
/*
.....Get initial face
*/
	sbody = LiViSolidGetBody(stock);
	if (sbody == 0) goto done;
	sface = LiBrBodyGetFirstFace(sbody);
	if (sface == 0) goto done;
	dd = *md;
	inc = 0;
/*
......Get all faces for current stock
*/
	while (sface != 0)
	{
		mat = LiViFaceGetMaterial(sface);
		cut = LiViFaceGetCutNumber(sface);
		for (k=0;k<UM_POCKET_COLORS;k++)
		{
			if (mat == LW_material[k]) break;
		}
/*
........Store color of face
........in cut number
*/
		if (cut >= dd->cut[0] && cut <= dd->cut[1])
		{
			fc[*fcinc] = k;
		}
		else if (cut != 0)
		{
			if (cut < dd->cut[0])
			{
				dd = LW_mot_data_first;
				dd = (UN_mot_data *)uu_lsnext(dd);
				*fcinc = 0;
			}
			for (i=*fcinc;i<LW_mot_ndata;i++)
			{
				if (cut >= dd->cut[0] && cut <= dd->cut[1])
				{
					fc[i] = k;
					break;
				}
				dd = (UN_mot_data *)uu_lsnext(dd);
				if (dd == UU_NULL)
				{
					i = *fcinc;
					dd = *md;
					break;
				}
			}
			*fcinc = i;
		}
		sface = LiBrFaceGetNextFace(sface);
	}
	*md = dd;
done:;
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_restore_faces(stock,fc,md,fcinc)
**      This function restores the face colors previously stored
**      based on the cut numbers.
**   PARAMETERS
**       INPUT  : stock     = Stock to restore face colors to.
**                fc        = Allocated integer array of size
**                            LW_mot_ndata+25.
**                md        = Pointer to beginning of motion data list
**                            (LW_mot_data_first+1) at first call and
**                            left alone for subsequent calls.
**                fcinc     = Initialized to zero at first call and
**                            left alone for subsequent calls.
**       OUTPUT : fc        = Updated face color array.
**                md        = Updated motion array pointer.
**                fcinc     = Updated face color pointer.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_restore_faces(stock,fc,md,fcinc)
LtSessionPrim stock;
int *fc;
UN_mot_data **md;
int *fcinc;
{
	int i,cut;
	LtBody sbody;
	LtFace sface;
	UN_mot_data *dd;
	LtAttributeClass class;
/*
.....Get first face
*/
	sbody = LiViSolidGetBody(stock);
	if (sbody == 0) goto done;
	sface = LiBrBodyGetFirstFace(sbody);
	if (sface == 0) goto done;
	class = LiViGetPredefinedAttributeClass(LI_VI_ATTRIB_FACE_MATERIAL);
	dd = *md;
/*
...........Get all faces for current stock
*/
	while (sface != 0)
	{
		cut = LiViFaceGetCutNumber(sface);
/*
...........Find color of face based on cut number
*/
		if (cut >= dd->cut[0] && cut <= dd->cut[1])
		{
			LiViEntitySetAttribute(sface,LI_ENTITY_TYPE_FACE,class,
				LW_material[fc[*fcinc]]);
		}
		else if (cut != 0)
		{
			if (cut < dd->cut[0])
			{
				dd = LW_mot_data_first;
				dd = (UN_mot_data *)uu_lsnext(dd);
				*fcinc = 0;
			}
			for (i=*fcinc;i<LW_mot_ndata;i++)
			{
				if (cut >= dd->cut[0] && cut <= dd->cut[1])
				{
					LiViEntitySetAttribute(sface,LI_ENTITY_TYPE_FACE,class,
						LW_material[fc[i]]);
					break;
				}
				dd = (UN_mot_data *)uu_lsnext(dd);
				if (dd == UU_NULL)
				{
					i = *fcinc;
					dd = *md;
					break;
				}
			}
			*fcinc = i;
		}
		sface = LiBrFaceGetNextFace(sface);
	}
	*md = dd;
done:;
}

/*********************************************************************
**   E_FUNCTION: ul_ipv_color_faces(stock)
**      This function sets the color of all faces on a solid to its
**      default color.  This includes cut faces.
**   PARAMETERS
**       INPUT  : sd     = Stock to set face colors for.
**       OUTPUT : none
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
void ul_ipv_color_faces(sd)
LW_stock_struc *sd;
{
	LtBody sbody;
	LtFace sface;
	LtAttributeClass class;
/*
.....Get first face
*/
	sbody = LiViSolidGetBody(sd->stock);
	if (sbody == 0) goto done;
	sface = LiBrBodyGetFirstFace(sbody);
	if (sface == 0) goto done;
	class = LiViGetPredefinedAttributeClass(LI_VI_ATTRIB_FACE_MATERIAL);
/*
.....Set face color of all faces
*/
	while (sface != 0)
	{
		LiViEntitySetAttribute(sface,LI_ENTITY_TYPE_FACE,class,
			LW_material[sd->color]);
		sface = LiBrFaceGetNextFace(sface);
	}
/*
.....End of routine
*/
done:;
}

/*********************************************************************
**   E_FUNCTION:int ul_ipv_session_active()
**      This function determines if a Visicut NCLIPV session is active.
**   PARAMETERS
**       INPUT  : none
**       OUTPUT : none.
**   RETURNS:
**          UU_TRUE if an NCLIPV Visicut session is active.
**   SIDE EFFECTS: Rapidcut sessions cannot be saved.
**   WARNINGS: none
*********************************************************************/
UU_LOGICAL ul_ipv_session_active(act)
UU_LOGICAL *act;
{
/*
.....Make sure a session is active
*/
	*act = LW_active;
	if (LW_session[0] == 0 || LW_mach_mode != LW_VISICUT)
		return(UU_FALSE);
	else
		return(UU_TRUE);
}
