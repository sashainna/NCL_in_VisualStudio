
/*********************************************************************
**  FILENAME: lipvassem.c
**  CONTAINS:
**              ul_ipv_define_assembly()
**              ul_ipv_assembly_solid()
**              ul_ipv_create_assembly()
**              ul_ipv_place_assembly()
**              ul_ipv_detach_assembly()
**              ul_ipv_destroy_assembly()
**              ul_ipv_part_assembly()
**              ul_ipv_part_destroy()
**              ul_ipv_move_assemblies()
**              ul_ipv_move_part()
**              ul_ipv_position_axis()
**              ul_ipv_get_axis()
**              ul_ipv_calc_axis()
**              ul_ipv_position_turret()
**              ul_ipv_free_turret()
**              ul_ipv_turret_same()
**              ul_ipv_turret_used()
**              ul_ipv_push_axis()
**              ul_ipv_push_circle()
**              ul_ipv_clear_axis()
**              ul_ipv_tool_assembly()
**              ul_ipv_lathe_tool_assembly()
**              ul_ipv_tool_disassembly()
**              ul_ipv_lathe_stop()
**              ul_ipv_lathe_start()
**
**    COPYRIGHT 2004 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvassem.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:11
*********************************************************************/
#include <stdio.h>
#include <math.h>
#include "usysdef.h"
#include "xenv1.h"
#include "nclfc.h"
#include "lcom.h"
#include "lipv.h"
#include "lipvmach.h"
#include "lipvstack.h"
#include "mfort.h"
#include "modef.h"
#include "msol.h"
#include "nclmplay.h"

static int Snstack=0;
static UU_LIST Sasm_list[10];
static UU_LOGICAL Slathe_started=UU_FALSE;

void ul_ipv_position_axis();
void ul_ipv_part_destroy();
void ul_ipv_push_axis();
void ul_ipv_clear_axis();
void ul_ipv_move_assemblies();

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_define_assembly()
**		Creates the machine solids.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_define_assembly()
{
	int i,j,status;
	char sbuf[80];
/*	UU_REAL rval[6];*/
/*	LW_stock_struc stock;*/
	LW_mach_model_struc *mpt;
	LW_mach_solid_struc *spt;
	LtData stuff;
/*
.....Make sure simulation file matches
.....Machine model
*/
	ul_ipv_match_mach(LW_mach_simul,&LW_mach_model,LW_mach_nmodel);
	if (LW_mach_naxes == 0) LW_mach_simul = UU_FALSE;
/*
.....Determine if machine should be defined
*/
	if (LW_mach_mode != LW_VISICUT || !LW_mach_simul)
	{
		LiDataSetBoolean(&stuff,FALSE);
		LiSessionSetProperty(LW_session[LW_mach_mode],LI_SESS_PROP_KI_SYNC_MOVES,
			&stuff);
/*		LiControlSet(LI_CONTROL_KI_SYNC_MOVES,&stuff);*/
		return(UU_SUCCESS);
	}
	LiDataSetBoolean(&stuff,TRUE);
	LiSessionSetProperty(LW_session[LW_mach_mode],LI_SESS_PROP_KI_SYNC_MOVES,
		&stuff);
/*	LiControlSet(LI_CONTROL_KI_SYNC_MOVES,&stuff);*/
/*	LiControlSet(LI_CONTROL_MW_USE_OPENGL_DISP_LISTS,&stuff);*/
/*
.....Define the machine solids
*/
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
	spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
	ul_ipv_enable_stl_form(UU_FALSE);
	for (i=0;i<LW_mach_nmodel;i++)
	{
		for (j=mpt[i].beg_solid;j<=mpt[i].end_solid;j++)
		{
			if (spt[j].stock.color == -1) spt[j].stock.color = mpt[i].color;
			if (spt[j].stock.translucency == -1)
				spt[j].stock.translucency = mpt[i].translucency;
			if (spt[j].stock.edge == -1) spt[j].stock.edge = mpt[i].edge;
			if ((spt[j].stock.edge_color == -1)||(spt[j].stock.edge_color == 64))
				spt[j].stock.edge_color = mpt[i].edge_color;
			if ((spt[j].stock.edge_color == -1)||(spt[j].stock.edge_color == 64))
				spt[j].stock.edge_color = spt[j].stock.color;
			if (spt[j].stock.visible == -1) spt[j].stock.visible = mpt[i].visible;
			status = ul_ipv_assembly_solid(&spt[j]);
			if (status != UU_SUCCESS)
			{
				sprintf(sbuf,"Failed to define machine solid %s.",spt[j].name);
				ud_wrerr(sbuf);
				spt[j].stock.stock = UU_NULL;
			}
		}
		if (status != UU_SUCCESS) break;
	}
	ul_ipv_enable_stl_form(UU_TRUE);
/*
.....End of routine
*/
	if (status != UU_SUCCESS) LW_mach_simul = UU_FALSE;
	return(status);
}

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_assembly_solid(solid)
**		Creates NCLIPV solid structures from the input Machine solids.
**	 PARAMETERS	
**		 INPUT  : 
**         solid    = Machine solid to process.
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_assembly_solid(solid)
LW_mach_solid_struc *solid;
{
	int status,type;
	LW_stock_struc *sd;
/*
.....Initialize routine
*/
	status = UU_SUCCESS;
/*
.....If this solid is already defined
.....Then do nothing
*/
	if (solid->stock.id != 0) goto done;
/*
.....Transform the solid
*/
	if (solid->stock.mxflag && solid->stock.type != LW_STOCK_BOX &&
		solid->stock.type != LW_STOCK_FILE)
	{
		switch (solid->stock.type)
		{
		case LW_STOCK_REVOLVE:
			type = UM_REVOLVED_SOLID;
			break;
		case LW_STOCK_SWEEP:
			type = UM_EXTRUDED_SOLID;
			break;
		case LW_STOCK_CONE:
			type = UM_CONE_SOLID;
			break;
		case LW_STOCK_CYLINDER:
			type = UM_CYLINDER_SOLID;
			break;
		case LW_STOCK_SPHERE:
			type = UM_SPHERE_SOLID;
			break;
		case LW_STOCK_TORUS:
			type = UM_TORUS_SOLID;
			break;
		}
		ncl_xform_solid(type,&solid->stock.data,solid->stock.matrix,UU_FALSE);
		solid->stock.mxflag = UU_FALSE;
	}
/*
.....Create the machine solid
*/
	sd = &(solid->stock);
	sd->id = 1;
	status = ul_ipv_create_stock(sd,2,UU_FALSE);
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_create_assembly()
**		Creates the machine assemblies.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_create_assembly()
{
	int i,j;
	UU_LOGICAL lmod;
	LW_mach_model_struc *mpt;
	LW_mach_solid_struc *spt;
	LW_mach_toolpin_struc *tpt;
	LtTransform idxform;
/*
.....Determine if machine should be defined
*/
	if (LW_mach_mode != LW_VISICUT || !LW_mach_simul)
		return(UU_SUCCESS);
/*
.....Create identity transform
*/
	idxform = LiTransformCreate();
/*
.....Set machine solid attributes
*/
	spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
	for (i=0;i<LW_mach_nsolid;i++) ul_ipv_set_stk_attr(&(spt[i].stock));
/*
.....Set the clash logic
*/
	ul_ipv_define_clash(UU_NULL);
/*
.....Define the machine assemblies
*/
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
	spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
	tpt = (LW_mach_toolpin_struc *)UU_LIST_ARRAY(&LW_mach_toolpin);
	if (LW_is_lathe) LW_lathe = LiViLatheCreate(LW_session[LW_mach_mode]);
	for (i=0;i<LW_mach_nmodel;i++)
	{
		mpt[i].assembly = LiViAssemblyCreate(LW_session[LW_mach_mode]);
		if (!LW_is_lathe || i != tpt[LW_mach_tpin_ix].axis)
		{
			for (j=mpt[i].beg_solid;j<=mpt[i].end_solid;j++)
				LiViAssemblyAddSolid(mpt[i].assembly,spt[j].stock.stock);
/*
........Join assemblies
*/
			if (mpt[i].parent != -1)
			{
				LiViAssemblySetParent(mpt[i].assembly,mpt[mpt[i].parent].assembly);
				LiViSolidSetProgrammingCoordSys(spt[mpt[i].beg_solid].stock.stock,
					LI_VI_PROG_COORD_SYS_SOLID,
					spt[mpt[mpt[i].parent].beg_solid].stock.stock,
					idxform,UU_FALSE);
			}
		}
/*
........Join Lathe assemblies
*/
		else
		{
			for (j=mpt[i].beg_solid;j<=mpt[i].end_solid;j++)
				LiViLatheAddSolid(LW_lathe,spt[j].stock.stock);
		}
/*
........Set Machine position
*/
		mpt[i].last_pos = 0.;
		mpt[i].position = mpt[i].home;
		mpt[i].offset = mpt[i].baseofs;
	}
/*
.....Place the stocks on the machine
*/
	LW_mach_defined = UU_TRUE;
	ul_ipv_place_stock(UU_NULL,UU_FALSE,&lmod);
/*
.....Position the axes
*/
	ul_ipv_place_axes();
	return(UU_SUCCESS);
/*
.....End of routine
*/
done:;
	return(UU_SUCCESS);
}

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_place_assembly(sd)
**		Places a stock or fixture onto the machine assembly.
**	 PARAMETERS	
**		 INPUT  :
**        sd    = Stock or fixture to place on assembly.
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_place_assembly(sd)
LW_stock_struc *sd;
{
	int ix,i;
	UU_REAL pos[LW_MAX_AXES],savpos[LW_MAX_AXES];
	LW_mach_model_struc *mpt;
	LW_mach_solid_struc *spt;
	LW_mach_toolpin_struc *tpt;
	LtSolidType type;
	LtData stuff;
/*
.....Initialize routine
*/
	if (LW_mach_simul && LW_session[0] != 0 && LW_mach_defined &&
		!(sd->placed))
	{
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		tpt = (LW_mach_toolpin_struc *)UU_LIST_ARRAY(&LW_mach_toolpin);
		ix = tpt[sd->tpin].axis;
/*
.....Save current axes positions
*/
		for (i=0;i<LW_mach_max_axes;i++) 
		{
			if (LW_mach_axes[i] != -1) savpos[i] = mpt[LW_mach_axes[i]].position;
			else savpos[i] = 0.;
			pos[i] = 0.;
		}
/*
.....Position all axes at 0
*/
		ul_ipv_move_assemblies(pos,UU_FALSE,UU_TRUE);
/*
.....Attach stock to axis
*/
		LiSessionPrimGetProperty(sd->stock,LI_SPRIM_PROP_SOLID_TYPE,&stuff);
		type = LiDataGetEnum(&stuff);
		if (type == LI_MW_SOLID_TYPE_FIXTURE)
			LiViSolidSetEnabled(sd->stock,UU_TRUE);
		if (LW_is_lathe)
			LiViLatheAddSolid(LW_lathe,sd->stock);
		else
			LiViAssemblyAddSolid(mpt[ix].assembly,sd->stock);
/*
.....Reset axis position
*/
		ul_ipv_move_assemblies(savpos,UU_FALSE,UU_TRUE);
/*
.....Set clash detection
*/
		ul_ipv_define_clash(sd);
		sd->placed = UU_TRUE;
	}
	return;
}

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_detach_assembly(sd)
**		Removes a stock or fixture from the machine assembly.
**	 PARAMETERS	
**		 INPUT  :
**        sd    = Stock or fixture to place on assembly.
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_detach_assembly(sd)
LW_stock_struc *sd;
{
	int ix;
	LW_mach_model_struc *mpt;
	LW_mach_solid_struc *spt;
	LW_mach_toolpin_struc *tpt;
	LtDoublePoint newpos,pos;
	LtProgCoordSys csys;
	LtTransform tfx;
	LtBoolean rel;
	LtGenericPtr prim;
	LtSolidType type;
	LtData stuff;
/*
.....Initialize routine
*/
	if (LW_mach_simul && LW_session[0] != 0 /*LW_mach_defined &&*/)
	{
/*
.....Set assembly to 0,0,0
*/
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
		tpt = (LW_mach_toolpin_struc *)UU_LIST_ARRAY(&LW_mach_toolpin);
		ix = tpt[sd->tpin].axis;
		if (spt[mpt[ix].beg_solid].stock.id != 0 &&
			spt[mpt[ix].beg_solid].stock.stock != 0)
		{
			newpos[0] = newpos[1] = newpos[2] = 0.;
			LiViSolidGetProgrammingCoordSys(
				spt[mpt[ix].beg_solid].stock.stock,
				&csys,&prim,&tfx,&rel);
			LiViSolidSetProgrammingCoordSys(
				spt[mpt[ix].beg_solid].stock.stock,
				LI_VI_PROG_COORD_SYS_WORLD,prim,tfx,rel);
			LiViSolidGetPosition(spt[mpt[ix].beg_solid].stock.stock,pos);
			LiViSolidSetPosition(spt[mpt[ix].beg_solid].stock.stock,
				newpos);
		}
/*
.....Detach stock from axis
*/
		LiViAssemblyRemoveSolid(sd->stock);
		sd->placed = UU_FALSE;
/*
.....Reset assembly position
*/
		if (spt[mpt[ix].beg_solid].stock.id != 0 &&
			spt[mpt[ix].beg_solid].stock.stock != 0)
		{
			LiViSolidSetPosition(spt[mpt[ix].beg_solid].stock.stock,
				pos);
			LiViSolidSetProgrammingCoordSys(
				spt[mpt[ix].beg_solid].stock.stock,
				csys,prim,tfx,rel);
		}
	}
	return;
}

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_destroy_assembly()
**		Deletes the machine assemblies.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_destroy_assembly()
{
	UU_LOGICAL lmod;
	LW_mach_model_struc *mpt;
	LW_mach_solid_struc *spt;
	LtData stuff;
	int i;
/*
.....Remove stocks and fixtures from machine
*/
	LW_mach_defined = UU_FALSE;
	ul_ipv_place_stock(UU_NULL,UU_TRUE,&lmod);
/*
.....Destroy the assemblies
*/
	if (LW_session[0] != 0)
	{
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		for (i=0;i<LW_mach_nmodel;i++)
		{
			if (mpt[i].assembly != 0) LiViAssemblyDestroy(mpt[i].assembly);
			mpt[i].assembly = /*mpt[i].solid =*/ 0;
		}
/*
.....Delete the machine solids
*/
		spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
		LiDataSetBoolean(&stuff,FALSE);
		for (i=0;i<LW_mach_nsolid;i++)
		{
			if (spt[i].stock.id != 0 && spt[i].stock.stock != 0)
			{
				LiMWViewportSetSessPrimProperty(LW_viewport,spt[i].stock.stock,
					LI_VPSP_PROP_MW_VISIBLE,&stuff);
				LiViSolidSetEnabled(spt[i].stock.stock,FALSE);
				LiSessionRemovePrim(spt[i].stock.stock);
				LiPrimitiveDestroy(spt[i].stock.prim);
				spt[i].stock.id = 0;
				spt[i].stock.stock = 0;
				spt[i].stock.prim = 0;
			}
		}
	}
/*
.....Delete dummy stock
*/
	if (LW_mach_stock.stock != 0)
	{
		LiSessionRemovePrim(LW_mach_stock.stock);
		LiPrimitiveDestroy(LW_mach_stock.prim);
		LW_mach_stock.stock = 0;
		LW_mach_stock.prim = 0;
	}
/*
.....Disable kinematics
*/
	LiDataSetBoolean(&stuff,FALSE);
	LiSessionSetProperty(LW_session[LW_mach_mode],LI_SESS_PROP_KI_SYNC_MOVES,
		&stuff);
/*	LiControlSet(LI_CONTROL_KI_SYNC_MOVES,&stuff);*/
/*
.....End of routine
*/
	return(UU_SUCCESS);
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_part_assembly()
**		Creates an assembly out of fixtures and stocks for Moving Part
**    Simulation.
**	 PARAMETERS	
**		 INPUT  : 
**        none
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_part_assembly()
{
	int ist,i,ifl;
	LW_stock_struc *sd,*sdtmp;
/*
.....Delete pre-existing assembly
*/
	if (LW_assemb != 0) ul_ipv_part_destroy();
	LW_assemb = 0;
/*
.....Create assembly
*/
	LW_assemb = LiViAssemblyCreate(LW_session[LW_mach_mode]);
	LW_assemb_pos[0] = LW_assemb_pos[1] = LW_assemb_pos[2] = 0.;
/*
.....Append fixture solids
*/
	if (LW_nstock[1] != 0)
	{
		sd = LW_stock_first[1];
		LW_part_stock = sd->stock;
		for (i=0;i<LW_nstock[1];i++)
		{
			ifl = 0;
			do
			{
				ul_ipv_get_next_stock(sd,&sdtmp,&ifl,UU_FALSE);
				if (ifl == -2) break;
				LiViAssemblyAddSolid(LW_assemb,sdtmp->stock);
			} while (ifl != -1);
			sd = (LW_stock_struc *)uu_lsnext(sd);
		}
	}
/*
.....Append stock solids
*/
	if (LW_nstock[0] != 0)
	{
		sd = LW_stock_first[0];
		for (i=0;i<LW_nstock[0];i++)
		{
			ifl = 0;
			do
			{
				ul_ipv_get_next_stock(sd,&sdtmp,&ifl,UU_FALSE);
				if (ifl == -2) break;
				LiViAssemblyAddSolid(LW_assemb,sdtmp->stock);
			} while (ifl != -1);
			sd = (LW_stock_struc *)uu_lsnext(sd);
		}
	}
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_part_destroy()
**		Destroys the part assembly used for Moving Part Simulation.
**	 PARAMETERS	
**		 INPUT  : 
**        none
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_part_destroy()
{
/*
.....Destroy the part assembly
*/
	if (LW_assemb != 0) LiViAssemblyDestroy(LW_assemb);
	LW_assemb = 0;
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_move_assemblies(pos,flush,fromt)
**		Moves all assemblies to the requested axis positions.
**	 PARAMETERS	
**		 INPUT  : 
**        pos      = Array of axis positions for all 10 axes.
**        fromt    = UU_TRUE  = Position axis.
**                   UU_FALSE = Move axis and perform a cut.
**        flush    = UU_TRUE  = Update graphics window after moving assemblies.
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_move_assemblies(pos,flush,fromt)
UU_REAL pos[];
UU_LOGICAL flush,fromt;
{
	int i;
	LW_mach_model_struc *mpt;
/*
.....Loop through all axes
*/
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
	for (i=0;i<LW_mach_max_axes;i++)
	{
		if (LW_mach_axes[i] != -1)
			ul_ipv_position_axis(&mpt[LW_mach_axes[i]],i,pos[i],fromt);
	}
/*
.....Perform the cut
*/
	if (!fromt) LiViPerformCut(LW_session[LW_mach_mode]);
/*
.....Flush output buffer
*/
	if (LW_active && flush)
	{
		ul_ipv_flush();
		um_reset_pocket_graphics(UM_IPV_WINDOW);
	}
/*
.....Clear axes stack
*/
	ul_ipv_clear_axis();
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_move_part(tool_pos,tool_vec,fromt)
**		This routine is used when NCLIPV simulation is using the Moving
**    Part feature.  It satisfies the tool end position by moving the
**    stocks and fixtures, and solves the tool axis by moving the tool.
**	 PARAMETERS	
**		 INPUT  : 
**        tool_pos = Tool end point position.
**        tool_vec = Tool vector.
**        fromt    = UU_TRUE  = Position axis.
**                   UU_FALSE = Move axis and perform a cut.
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_move_part(tool_pos,tool_vec,fromt)
LtDoublePoint tool_pos;
LtDoubleVector tool_vec;
UU_LOGICAL fromt;
{
	LtDoublePoint delt,trans;
	LtDoubleVector vc1;
	LtDouble ang;
	UU_REAL um_angle2p();
/*
.....Calculate delta movement
*/
	delt[0] = LW_tool_pos[0] - tool_pos[0];
	delt[1] = LW_tool_pos[1] - tool_pos[1];
	delt[2] = LW_tool_pos[2] - tool_pos[2];
	LW_assemb_pos[0] = LW_assemb_pos[0] + delt[0];
	LW_assemb_pos[1] = LW_assemb_pos[1] + delt[1];
	LW_assemb_pos[2] = LW_assemb_pos[2] + delt[2];
	trans[0] = tool_pos[0] + LW_assemb_pos[0];
	trans[1] = tool_pos[1] + LW_assemb_pos[1];
	trans[2] = tool_pos[2] + LW_assemb_pos[2];
/*
.....FROM
.....Position the part only
*/
	if (fromt)
	{
		LiViSolidSetPosition(LW_part_stock,LW_assemb_pos);
		LiViToolSetPosition(LW_tool[0],trans);
		LiViToolSetAxis(LW_tool[0],tool_vec);
	}
/*
.....GOTO
.....Perform cutting motion
*/
	else
	{
		LiViSolidTranslate(LW_part_stock,delt);
		um_cross(&LW_tool_pos[3],tool_vec,vc1);
		ang = um_angle2p(&LW_tool_pos[3],tool_vec,vc1) * UM_RADIAN;
		LiViSolidRotate(LW_tool[0],trans,vc1,ang);
/*		LiViToolDefine5AxisLinearCut(LW_tool[0],trans,tool_vec);*/
		LiViPerformCut(LW_session[LW_mach_mode]);
	}
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_get_axis(axis)
**		Returns the current machine axis positions.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT :
**        axis  = Returns the curren machine axis positions.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_get_axis(axis)
UU_REAL *axis;
{
	int i;
	LW_mach_model_struc *mpt;

/*
.....Get the axis positions
*/
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
	for (i=0;i<LW_mach_max_axes;i++)
	{
		if (LW_mach_axes[i] == -1)
			axis[i] = 0.;
		else
			axis[i] = mpt[LW_mach_axes[i]].position;
	}
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_calc_axis(inaxis,type,outaxis)
**		Determines the machine axes positions based on the input record
**    type (Primary/Secondary Head) and the current position of each
**    axis.
**	 PARAMETERS	
**		 INPUT  :
**        inaxis  = The input axes as follows.
**                     Primary head = 10 standard axes (6 linear, 4 rotary).
**                     Secondary head = XYZ ABC UVW IJK Q.
**        type    = <100 = Primary head, 100+ = Secondary head.
**		 OUTPUT :
**        outaxis = Returns the calculated machine axis positions.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_calc_axis(inaxis,type,outaxis)
UU_REAL *inaxis,*outaxis;
int type;
{
	int i;
	LW_mach_model_struc *mpt;
/*
.....Stringer machine
*/
	if (LW_mach_type == LW_STRINGER)
	{
		ul_ipv_get_axis(outaxis);
		if (type < 100)
		{
			outaxis[0] = inaxis[0];
			outaxis[1] = inaxis[2];
			outaxis[2] = inaxis[4];
			outaxis[3] = inaxis[6];
			outaxis[4] = inaxis[7];
		}
		else
		{
			outaxis[0] = inaxis[0];
			outaxis[1] = inaxis[1];
			for (i=2;i<13;i++)
				outaxis[i+5] = inaxis[i];
		}
	}
/*
.....Get the axis positions
*/
	else
	{
		if (LW_mach_max_axes > 10) ul_ipv_get_axis(outaxis);
		for (i=0;i<10;i++) outaxis[i] = inaxis[i];
	}
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_position_axis(axis,indx,pos,fromt)
**		Positions a machine axis (as defined in the post-processor) at the
**    requested position.
**	 PARAMETERS	
**		 INPUT  : 
**        axis  = Which model axis to position.
**        indx  = Index into 10 axes to position.
**        pos   = New position of axis.
**        fromt = UU_TRUE  = Position axis.
**                UU_FALSE = Move axis and perform a cut.
**		 OUTPUT : none.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_position_axis(axis,indx,pos,fromt)
LW_mach_model_struc *axis;
int indx;
UU_REAL pos;
UU_LOGICAL fromt;
{
	int ispt,i,j,npts;
	UU_LOGICAL idid;
	UU_REAL delt,offpos,*pts;
	LW_mach_solid_struc *spt;
	LtDoubleVector ps1,*vcs;
	UU_LIST list;
/**/
	static UM_vector xaxis={1,0,0},zaxis={0,0,1};
	UM_vector vc1,vc2;
	UM_transf tf;
/**/
/*
.....Initialize routine
*/
	spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
	ispt = axis->beg_solid;
	offpos = (pos + axis->offset) * axis->scale;
	if (axis->reverse == 1) offpos = -offpos;
/*
.....Make sure that IPV is active
*/
	if (LW_session[0] != 0)
	{
		if (fabs(offpos-axis->last_pos) != 0. || Snstack != 0)
		{
/*
........Circular stack is active
........Determine if this axis moves
*/
			if (Snstack != 0)
			{
				idid = UU_FALSE;
				pts = (UU_REAL *)UU_LIST_ARRAY(&Sasm_list[indx]);
				npts = UU_LIST_LENGTH(&Sasm_list[indx]);
			 	offpos = (pts[0] + axis->offset) * axis->scale;
				if (fabs(offpos-axis->last_pos) > UM_DFUZZ) idid = UU_TRUE;
				if (!idid)
				{
					for (i=1;i<npts;i++)
					{
						if (fabs(pts[i]-pts[i-1]) > UM_DFUZZ)
						{
							idid = UU_TRUE;
							break;
						}
					}
				}
			}
/*
........Linear axes
*/
			if (axis->type == LW_MACH_LINEAR)
			{
/*
...........Position this axis at this location
*/
				if (fromt)
				{
					ps1[0] = axis->axis[0] + offpos*axis->axis[3];
					ps1[1] = axis->axis[1] + offpos*axis->axis[4];
					ps1[2] = axis->axis[2] + offpos*axis->axis[5];
					LiViSolidSetPosition(spt[ispt].stock.stock,ps1);
				}
/*
...........Move this axis to this location
*/
				else if (Snstack == 0)
				{
					delt = offpos - axis->last_pos;
					ps1[0] = delt * axis->axis[3];
					ps1[1] = delt * axis->axis[4];
					ps1[2] = delt * axis->axis[5];
					LiViSolidTranslate(spt[ispt].stock.stock,ps1);
				}
/*
...........Process circular stack
*/
				else
				{
					if (idid)
					{
						uu_list_init(&list,sizeof(LtDoubleVector),npts,10);
						for (j=0;j<npts;j++)
						{
							offpos = (pts[j] + axis->offset) * axis->scale;
							if (axis->reverse == 1) offpos = -offpos;
							delt = offpos - axis->last_pos;
							ps1[0] = delt * axis->axis[3];
							ps1[1] = delt * axis->axis[4];
							ps1[2] = delt * axis->axis[5];
							uu_list_push(&list,ps1);
							axis->last_pos = offpos;
						}
						vcs = (LtDoubleVector *)UU_LIST_ARRAY(&list);
						LiViSolidMultipleTranslate(spt[ispt].stock.stock,npts,vcs);
						uu_list_free(&list);
					}
					else
					{
						pos = pts[npts-1];
				 		offpos = (pos + axis->offset) * axis->scale;
					}
				}
			}
/*
........Rotary axes
*/
			else
			{
/*
...........Position this axis at this location
*/
				if (fromt)
				{
					um_rottf(&axis->axis[3],offpos/UM_RADIAN,tf);
					um_vctmtf(xaxis,tf,vc1);
					um_vctmtf(zaxis,tf,vc2);
					LiViSolidSetAxes(spt[ispt].stock.stock,vc1,vc2);

					um_cctmtf(&axis->axis[0],tf,ps1);
					ps1[0] = axis->axis[0] - ps1[0];
					ps1[1] = axis->axis[1] - ps1[1];
					ps1[2] = axis->axis[2] - ps1[2];
					LiViSolidSetPosition(spt[ispt].stock.stock,ps1);
				}
/*
...........Move this axis to this location
*/
				else if (Snstack == 0)
				{
					LiViSolidRotate(spt[ispt].stock.stock,&axis->axis[0],
						&axis->axis[3],offpos-axis->last_pos);
				}
/*
...........Process circular stack
*/
				else
				{
					if (idid)
					{
						uu_list_init(&list,sizeof(LtDouble),npts,10);
						for (j=0;j<npts;j++)
						{
							offpos = (pts[j] + axis->offset) * axis->scale;
							if (axis->reverse == 1) offpos = -offpos;
							ps1[0] = offpos - axis->last_pos;
							uu_list_push(&list,&ps1[0]);
							axis->last_pos = offpos;
						}
						pos = pts[npts-1];
						pts = (UU_REAL *)UU_LIST_ARRAY(&list);
						LiViSolidMultipleRotate(spt[ispt].stock.stock,npts,
							&axis->axis[0],&axis->axis[3],pts);
						uu_list_free(&list);
					}
					else
					{
						pos = pts[npts-1];
				 		offpos = (pos + axis->offset) * axis->scale;
					}
				}
			}
			axis->last_pos = offpos;
		}
	}
/*
.....Save position
*/
	axis->position = pos;
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_position_turret(toolno,fromt)
**		Positions a lathe turret to the requested tool position.
**	 PARAMETERS	
**		 INPUT  : 
**        toolno  = Tool to position turret at.
**        fromt   = UU_TRUE  = Position turret to load tool.
**                  UU_FALSE = Rotate turret to correct tool.
**		 OUTPUT : none.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_position_turret(toolno,fromt)
int toolno;
UU_LOGICAL fromt;
{
	int ispt,ipt,tlno,i;
	UU_REAL offpos,ang,ainc;
	LW_mach_model_struc *mpt;
	LW_mach_solid_struc *spt;
	LtDoubleVector ps1;
	static UM_vector xaxis={1,0,0},zaxis={0,0,1};
	UM_vector vc1,vc2;
	UM_transf tf;
/*
.....Initialize routine
*/
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
	ipt = LW_spindle[LW_spindle_ix[0]];
	spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
	ispt = mpt[ipt].beg_solid;
	tlno = toolno - 1;
	if (tlno >= mpt[ipt].turret->iparm[0]) tlno = 0;
/*
.....Make sure that IPV is active
*/
	if (LW_session[0] != 0)
	{
/*
.....Calculate turret rotation
.....based on tool position
*/
		ainc = 360. / mpt[ipt].turret->iparm[0];
		ang = tlno * ainc;
		offpos = (ang + mpt[ipt].offset) * mpt[ipt].scale;
		if (mpt[ipt].reverse == 1) offpos = -offpos;
		if (fabs(offpos-mpt[ipt].last_pos) != 0.)
		{
/*
.....Position this axis at this location
*/
			if (fromt || !fromt)
			{
				um_rottf(&mpt[ipt].turret->parms[3],offpos/UM_RADIAN,tf);
				um_vctmtf(xaxis,tf,vc1);
				um_vctmtf(zaxis,tf,vc2);
				LiViSolidSetAxes(spt[ispt].stock.stock,vc1,vc2);

				um_cctmtf(&mpt[ipt].turret->parms[0],tf,ps1);
				ps1[0] = mpt[ipt].turret->parms[0] - ps1[0];
				ps1[1] = mpt[ipt].turret->parms[1] - ps1[1];
				ps1[2] = mpt[ipt].turret->parms[2] - ps1[2];
				LiViSolidSetPosition(spt[ispt].stock.stock,ps1);
			}
/*
.....Move this axis to this location
*/
			else
			{
				LiViSolidRotate(spt[ispt].stock.stock,&mpt[ipt].turret->parms[0],
					&mpt[ipt].turret->parms[3],offpos-mpt[ipt].last_pos);
			}
			mpt[ipt].last_pos = offpos;
		}
/*
.....Set as the active tool
*/
		if (!fromt)
		{
			tlno = S_calc_tlno(toolno,mpt[ipt].turret);
			LW_tool[0] = mpt[ipt].turret->tool_prim[tlno];
			LW_shank[0] = mpt[ipt].turret->shank_prim[tlno];
			LW_num_holder[0] = mpt[ipt].turret->nholder[tlno];
			for (i=0;i<LW_num_holder[0];i++)
				LW_holder[0][i] = mpt[ipt].turret->holder_prim[tlno][i];
		}
	}
/*
.....Save position
*/
	mpt[ipt].position = ang;
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_free_turret()
**		Deletes any tools loaded in a lathe turret.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_free_turret()
{
	int ipt,i,j;
	LW_mach_model_struc *mpt;
	LtPrim prim;
	LtData stuff;
/*
.....Initialize routine
*/
	if (!LW_mach_simul || !LW_is_lathe) goto done;
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
	ipt = LW_spindle[LW_spindle_ix[0]];
	if (mpt[ipt].turret == UU_NULL) goto done;
/*
.....Delete all tools loaded in turret
*/
	for (i=0;i<mpt[ipt].turret->iparm[0];i++)
	{
		if (mpt[ipt].turret->tool_prim[i] != 0)
		{
			LiSessionPrimGetProperty(mpt[ipt].turret->tool_prim[i],
				LI_SPRIM_PROP_PRIM,&stuff);
			prim = (LtPrim)LiDataGetGenericPtr(&stuff);
			LiViToolSelect(mpt[ipt].turret->tool_prim[i],FALSE);
			LiSessionRemovePrim(mpt[ipt].turret->tool_prim[i]);
			mpt[ipt].turret->tool_prim[i] = 0;
			LiPrimitiveDestroy(prim);
		}
		if (mpt[ipt].turret->shank_prim[i] != 0)
		{
			LiSessionPrimGetProperty(mpt[ipt].turret->shank_prim[i],
				LI_SPRIM_PROP_PRIM,&stuff);
			prim = (LtPrim)LiDataGetGenericPtr(&stuff);
			LiSessionRemovePrim(mpt[ipt].turret->shank_prim[i]);
			mpt[ipt].turret->shank_prim[i] = 0;
			LiPrimitiveDestroy(prim);
		}
		if (mpt[ipt].turret->nholder[i] != 0)
		{
			for (j=0;j<mpt[ipt].turret->nholder[i];j++)
			{
				if (mpt[ipt].turret->holder_prim[i][j] != 0)
				{
					LiSessionPrimGetProperty(mpt[ipt].turret->holder_prim[i][j],
						LI_SPRIM_PROP_PRIM,&stuff);
					prim = (LtPrim)LiDataGetGenericPtr(&stuff);
					LiSessionRemovePrim(mpt[ipt].turret->holder_prim[i][j]);
					LiPrimitiveDestroy(prim);
				}
				LW_holder[0][i] = mpt[ipt].turret->holder_prim[i][j] = 0;
			}
			mpt[ipt].turret->nholder[i] = 0;
		}
	}
	LW_tool[0] = 0;
	LW_shank[0] = 0;
	LW_num_holder[0] = 0;
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_turret_same(cpt,flag)
**		Determines if the active tool is already in the turret.
**	 PARAMETERS	
**		 INPUT  : 
**        cpt   = Active tool.
**        flag  = UU_TRUE = If tool is not the same as the one already
**                loaded in the turret then mark the active tool as the
**                one currently in the turret so it can be deselected.
**		 OUTPUT : none
**	 RETURNS:
**        UU_TRUE if the tool is the same as loaded in the turret.
**        UU_FALSE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
UU_LOGICAL ul_ipv_turret_same(cpt,flag)
UN_cutter_list *cpt;
UU_LOGICAL flag;
{
	int ipt,i,tlno;
	LW_mach_model_struc *mpt;
/*
.....Initialize routine
*/
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
	ipt = LW_spindle[LW_spindle_ix[0]];
/*
.....See if tool matches the one already loaded in the turret
*/
	tlno = S_calc_tlno(cpt->tlno,mpt[ipt].turret);
	if (cpt->clrec == mpt[ipt].turret->toolno[tlno]) return(UU_TRUE);
/*
.....Tools are different
.....Setup the tool for deselection
*/
	if (flag)
	{
		if (mpt[ipt].turret->tool_prim[tlno] != 0)
		{
			LW_tool[0] = mpt[ipt].turret->tool_prim[tlno];
			mpt[ipt].turret->tool_prim[tlno] = 0;
		}
		if (mpt[ipt].turret->shank_prim[tlno] != 0)
		{
			LW_shank[0] = mpt[ipt].turret->shank_prim[tlno];
			mpt[ipt].turret->shank_prim[tlno] = 0;
		}
		if (mpt[ipt].turret->nholder[tlno] != 0)
		{
			for (i=0;i<mpt[ipt].turret->nholder[tlno];i++)
			{
				LW_holder[0][i] = mpt[ipt].turret->holder_prim[tlno][i];
				mpt[ipt].turret->holder_prim[tlno][i] = 0;
			}
		}
	}
/*
.....End of routine
*/
	return(UU_FALSE);
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_turret_used(toolno)
**		Checks to see if a turret position already has a tool in it.
**	 PARAMETERS	
**		 INPUT  : 
**        toolno  = Turret position to check.
**		 OUTPUT : none.
**	 RETURNS:
**        UU_TRUE if the turret position is used.
**        UU_FALSE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
UU_LOGICAL ul_ipv_turret_used(toolno)
int toolno;
{
	int ipt,tlno;
	LW_mach_model_struc *mpt;
/*
.....Initialize routine
*/
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
	ipt = LW_spindle[LW_spindle_ix[0]];
/*
.....See if turret position is used
*/
	tlno = S_calc_tlno(toolno,mpt[ipt].turret);
	if (mpt[ipt].turret->tool_prim[tlno] != 0) return(UU_TRUE);
/*
.....End of routine
*/
	return(UU_FALSE);
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_push_axis(axis)
**		Pushes the machine axes onto a stack to be used for cuts that
**    require multiple positions output in a single call, such as
**    circular interpolation.
**	 PARAMETERS	
**		 INPUT  : 
**        axis  = Machine axis positions.
**		 OUTPUT : none.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_push_axis(axis)
UU_REAL axis[];
{
	int i;
/*
.....Initialize stack;
*/
	if (Snstack == 0)
	{
		for (i=0;i<10;i++)
			uu_list_init(&Sasm_list[i],sizeof(UU_REAL),50,50);
	}
/*
.....Push axis onto stack
*/
	for (i=0;i<10;i++)
		uu_list_push(&Sasm_list[i],&axis[i]);
	Snstack++;
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_push_circle(from,cutn,colors)
**		Pushes the circular interpolation stack onto the Undo stack.
**	 PARAMETERS	
**		 INPUT  : 
**        from   = Current machine axis positions.
**        cutn   = Current cut number.
**        colors = Cut color attributes.
**		 OUTPUT : none.
**	 RETURNS: UU_TRUE if the circular stack exists and was pushed onto
**           the Undo stack.  UU_FALSE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
UU_LOGICAL ul_ipv_push_circle(from,cutn,colors)
UU_REAL from[];
int cutn,colors[];
{
	int i,j;
	UU_REAL *pts[LW_MAX_AXES],fdata[LW_MAX_AXES],rdata[10];
/*
.....Stack does not exist
*/
	if (Snstack == 0) return(UU_FALSE);
/*
.....Push circular stack onto Undo stack
*/
	else
	{
		for (i=0;i<10;i++)
		{
			fdata[i] = from[i];
			pts[i] = (UU_REAL *)UU_LIST_ARRAY(&Sasm_list[i]);
		}
		for (i=0;i<Snstack;i++)
		{
			for (j=0;j<10;j++) rdata[j] = pts[j][i];
			ul_ipv_mot_stack_push(fdata,rdata,10,cutn,colors,LW_CUT_CIRCLE);
			for (j=0;j<10;j++) fdata[j] = rdata[j];
		}
	}
	return(UU_TRUE);
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_clear_axis(axis)
**		Clears (resets) the machine axes stack.
**	 PARAMETERS	
**		 INPUT  : none.
**		 OUTPUT : none.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_clear_axis()
{
	int i;
/*
.....Loop through stack and output moves
*/
	if (Snstack != 0)
	{
		for (i=0;i<10;i++)
			uu_list_free(&Sasm_list[i]);
		Snstack = 0;
	}
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_tool_assembly(tool_pos,tool_vec,zhgt)
**		Attaches the current tool, shank, and holder to the machine head.
**	 PARAMETERS	
**		 INPUT  :
**        tool_pos   = Current tool position.
**        tool_vec   = Current tool axis vector.
**        zhgt       = Current tool Z-height.
**        ispin      = Spindle to attach the tool to.
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_tool_assembly(tool_pos,tool_vec,zhgt,ispin)
LtDoublePoint tool_pos;
LtDoubleVector tool_vec;
UU_REAL zhgt;
int ispin;
{
	int status,ipt;
	UM_coord tpt;
	UM_vector zvc,sx,sy,sz,yaxis;
	UM_transf tfmat;
	LtDoublePoint pta,pos;
	LtDoubleVector xaxis,zaxis;
	LtProgCoordSys csys;
	LtGenericPtr prim;
	LtTransform tf;
	LtBoolean rel;
	LW_mach_model_struc *mpt;
	LW_mach_solid_struc *spt;
/*
....Initialize routine
*/
	status = UU_SUCCESS;
	LW_tool_zhgt[ispin] = zhgt;
	if (LW_mach_simul)
	{
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
		ipt = LW_spindle[LW_spindle_ix[ispin]];
/*
.....Attach the tool to the machine spindle
*/
		if (LW_tool[ispin] != 0 && ipt != -1)
		{
/*
........Get the spindle position
*/
			LiViSolidGetProgrammingCoordSys(spt[mpt[ipt].beg_solid].stock.stock,
				&csys,&prim,&tf,&rel);
			LiViSolidSetProgrammingCoordSys(spt[mpt[ipt].beg_solid].stock.stock,
				LI_VI_PROG_COORD_SYS_WORLD,prim,tf,rel);
			LiViSolidGetPosition(spt[mpt[ipt].beg_solid].stock.stock,pos);
			LiViSolidGetAxes(spt[mpt[ipt].beg_solid].stock.stock,xaxis,zaxis);
			LiViSolidSetProgrammingCoordSys(spt[mpt[ipt].beg_solid].stock.stock,
				csys,prim,tf,rel);
/*
........Calculate the xform matrix
........needed to place the tool
........due to the spindle rotation
*/
			sx[0] = 1.; sx[1] = 0.; sx[2] = 0.;
			sy[0] = 0.; sy[1] = 1.; sy[2] = 0.;
			sz[0] = 0.; sz[1] = 0.; sz[2] = 1.;
			um_cross(zaxis,xaxis,yaxis);
			um_chgcstf(pos,sx,sy,sz,pos,xaxis,yaxis,zaxis,tfmat);
/*
........Adjust tool stop point
........position and orientation
*/
			um_cctmtf(mpt[ipt].axis,tfmat,tpt);
			um_vctmtf(&mpt[ipt].axis[3],tfmat,zvc);
/*
.....Attach the tool to the spindle
*/
			pta[0] = tpt[0] + pos[0] - zhgt*zvc[0];
			pta[1] = tpt[1] + pos[1] - zhgt*zvc[1];
			pta[2] = tpt[2] + pos[2] - zhgt*zvc[2];
			LiViToolSetAxis(LW_tool[ispin],zvc);
			LiViToolSetPosition(LW_tool[ispin],pta);
			LiViAssemblyAddSolid(mpt[ipt].assembly,LW_tool[ispin]);
		}
	}
/*
.....Machine simulation is not active
.....So position tool
*/
	else
	{
		LiViToolSetPosition(LW_tool[ispin],tool_pos);
		LiViToolSetAxis(LW_tool[ispin],tool_vec);
	}
	return(status);
}

/*********************************************************************
**	 E_FUNCTION: ul_ipv_lathe_tool_assembly(tool_pos,clrec,toolno,taxis,zhgt,
**                                         ctype)
**		Attaches the current lathe tool, shank, and holder to the machine
**    turret.
**	 PARAMETERS	
**		 INPUT  :
**        tool_pos   = Current tool position.
**        clrec      = Cl record number of cutter command.
**        tlno       = Turret position to load tool.
**        taxis      = Axis of defined tool (0,1,0) or (1,0,0)
**        zhgt       = Tool length offset.  Used to align tool with turret.
**        ctype      = NCL_CUTTER_LATHE or NCL_CUTTER_MILL.
**		 OUTPUT : none.
**	 RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
int ul_ipv_lathe_tool_assembly(tool_pos,clrec,toolno,taxis,zhgt,ctype)
LtDoublePoint tool_pos;
int clrec,toolno;
UU_REAL taxis[],zhgt;
NCL_cutter_type ctype;
{
	int status,ipt,tlno,i,j,inc;
	UU_REAL ofs;
	UM_transf tfmat;
	LtDoublePoint pta,pos,tpt;
	LtDoubleVector xaxis,zaxis,yaxis,sx,sy,sz,xvc;
	LtProgCoordSys csys;
	LtGenericPtr prim;
	LtTransform tf;
	LtBoolean rel;
	LW_mach_model_struc *mpt;
	LW_mach_solid_struc *spt;
/*
....Initialize routine
*/
	status = UU_SUCCESS;
	if (LW_mach_simul)
	{
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
		ipt = LW_spindle[LW_spindle_ix[0]];
		ofs = zhgt;
		if (mpt[ipt].turret->iparm[1] == 0) ofs = 0.;
/*
.....Determine correct tool spindle
*/
		tlno = S_calc_tlno(toolno,mpt[ipt].turret);
/*
.....Fill in axisline data for each turret side
*/
		if (mpt[ipt].turret->naxis < mpt[ipt].turret->iparm[0])
		{
			inc = 0;
			for (i=mpt[ipt].turret->naxis;i<mpt[ipt].turret->iparm[0];i++)
			{
				for (j=0;j<6;j++)
					mpt[ipt].turret->axis[i][j] = mpt[ipt].turret->axis[inc][j];
				inc++;
			}
			mpt[ipt].turret->naxis = mpt[ipt].turret->iparm[0];
		}
		um_vctovc(&mpt[ipt].turret->axis[tlno][3],xvc);
/*
.....Attach the tool to the machine spindle
*/
		if (LW_tool[0] != 0 && ipt != -1)
		{
/*
........Position turret 1st face
*/
			ul_ipv_position_turret(1,UU_TRUE);
/*
........Get the spindle position
*/
			LiViSolidGetProgrammingCoordSys(spt[mpt[ipt].beg_solid].stock.stock,
				&csys,&prim,&tf,&rel);
			LiViSolidSetProgrammingCoordSys(spt[mpt[ipt].beg_solid].stock.stock,
				LI_VI_PROG_COORD_SYS_WORLD,prim,tf,rel);
			LiViSolidGetPosition(spt[mpt[ipt].beg_solid].stock.stock,pos);
			LiViSolidGetAxes(spt[mpt[ipt].beg_solid].stock.stock,xaxis,zaxis);
			LiViSolidSetProgrammingCoordSys(spt[mpt[ipt].beg_solid].stock.stock,
				csys,prim,tf,rel);
/*
........Position turret to correct face
*/
			ul_ipv_position_turret(toolno,UU_TRUE);
/*
........Calculate the xform matrix
........needed to place the tool
........due to the spindle rotation
*/
			sx[0] = 1.; sx[1] = 0.; sx[2] = 0.;
			sy[0] = 0.; sy[1] = 1.; sy[2] = 0.;
			sz[0] = 0.; sz[1] = 0.; sz[2] = 1.;
			um_cross(zaxis,xaxis,yaxis);
			um_chgcstf(pos,sx,sy,sz,pos,xaxis,yaxis,zaxis,tfmat);
/*
.....Adjust tool stop point
.....position and orientation
*/
			um_cctmtf(mpt[ipt].turret->axis[tlno],tfmat,tpt);
			um_cctmtf(xvc,tfmat,xvc);
/*
.....Rotate mill style tool
.....to axis line
*/
			if (ctype == NCL_CUTTER_MILL)
				LiViToolSetAxis(LW_tool[0],xvc);
/*
.....Calculate shank axis
.....based on axis line
*/
			else if (!um_vcparall(xvc,taxis))
			{
				um_cross(taxis,sx,yaxis);
				um_cross(taxis,yaxis,zaxis);
				LiViSolidSetAxes(LW_tool[0],taxis,zaxis);
			}
/*
.....Attach the tool to the spindle
*/
			pta[0] = tpt[0] + pos[0] - ofs*taxis[0];
			pta[1] = tpt[1] + pos[1] - ofs*taxis[1];
			pta[2] = tpt[2] + pos[2] - ofs*taxis[2];
			LiViToolSetPosition(LW_tool[0],pta);
			LiViAssemblyAddSolid(mpt[ipt].assembly,LW_tool[0]);
/*
.....Save the tool primitives with the turret
*/
			mpt[ipt].turret->toolno[tlno] = clrec;
			mpt[ipt].turret->tool_prim[tlno] = LW_tool[0];
			mpt[ipt].turret->shank_prim[tlno] = LW_shank[0];
			mpt[ipt].turret->nholder[tlno] = LW_num_holder[0];
			for (i=0;i<LW_num_holder[0];i++)
				mpt[ipt].turret->holder_prim[tlno][i] = LW_holder[0][i];
		}
	}
/*
.....Machine simulation is not active
.....So position tool
*/
	else
	{
		LiViToolSetPosition(LW_tool[0],tool_pos);
	}
	return(status);
}

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_tool_disassembly(ispin)
**		Removes the current tool, shank, and holder from the machine head.
**	 PARAMETERS	
**		 INPUT  :
**        ispin    = Spindle to remove tool from.
**		 OUTPUT : none.
**	 RETURNS: none.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_tool_disassembly(ispin)
int ispin;
{
/*
.....Remove the tool from the machine head
*/
	if (LW_mach_simul)
	{
		if (LW_tool[ispin] != 0) LiViAssemblyRemoveSolid(LW_tool[ispin]);
/*
//		if (LW_shank != UU_NULL) LiViAssemblyRemoveSolid(LW_shank);
//		if (LW_holder != UU_NULL) LiViAssemblyRemoveSolid(LW_holder);
*/
	}
}

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_lathe_stop()
**		Stops a spinning lathe.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS:
**        UU_TRUE if the lathe was spinning when this call was made.
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
UU_LOGICAL ul_ipv_lathe_stop()
{
	UU_LOGICAL iret;
/*
.....Determine if lathe is spinning
*/
	iret = Slathe_started;
	Slathe_started = UU_FALSE;
/*
.....Stop the lathe
*/
	if (LW_lathe != 0) LiViLatheStop(LW_lathe);
	return(iret);
}

/*********************************************************************
**	 E_FUNCTION:int ul_ipv_lathe_start()
**		Starts the lathe spinning.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
void ul_ipv_lathe_start()
{
/*
.....Start the lathe
*/
	if (LW_lathe != 0)
	{
		LiViLatheStart(LW_lathe);
		Slathe_started = UU_TRUE;
	}
}

/*********************************************************************
**	 I_FUNCTION: S_calc_tlno(toolno,npos)
**		Calculates the internal turret position from the programmed tool
**    number.  This can be different than the actual turret face that
**    the tool is loaded into, because when only a single mill spindle
**    is used on the turret, then all milling tools will be loaded into
**    this spindle, while the turret is still positioned at the first
**    turret location.
**	 PARAMETERS	
**		 INPUT  :
**        toolno     = Programmed turret position.
**        npos       = Number of available turret positions.
**		 OUTPUT : none
**	 RETURNS:
**     Internal turret position to use for this tool
**	 SIDE EFFECTS: none
**	 WARNINGS: none
*********************************************************************/
static int S_calc_tlno(toolno,turret)
int toolno;
LW_mach_turret_struc *turret;
{
	int tlno;
/*
.....Calculate internal turret position
*/
	tlno = toolno - 1;
	if (tlno >= turret->iparm[0])
	{
		tlno = 0;
		if (LW_mach_type == LW_MILL) tlno = turret->naxis - 1;
	}
	return(tlno);
}
