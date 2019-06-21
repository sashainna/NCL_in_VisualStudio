/*********************************************************************
**    NAME         :  lipvsup.c
**       CONTAINS:
**          ul_ipv_find_mdata
**          ul_ipv_mach_type
**          ul_ipv_set_mach_type
**          ul_ipv_reset_lathe
**          ul_ipv_load_tools
**          ul_ipv_append_tools
**          ul_ipv_merge_tools
**          ul_ipv_mount_lathe_tools
**          ul_ipv_format_tool
**          ul_ipv_init_list
**          ul_ipv_put_list
**          ul_ipv_render_edges
**          ul_ipv_color_set
**          ul_ipv_prescan_stocks
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvsup.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:17
*********************************************************************/

#include "usysdef.h"
#include "stdio.h"
#include "lcom.h"
#include "lipv.h"
#include "lipvmach.h"
#include "mdcpln.h"
#include "udforms.h"

void ul_ipv_reset_lathe();
void ul_ipv_load_tools();
void ul_ipv_color_set();

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_find_mdata(entity,cutn,mdata,mattr,flag)
**       Searches for the motion attribute stored with a machined face.
**    PARAMETERS
**       INPUT  :
**          entity   Entity (face) to obtain motion attributes for (UU_FALSE).
**          cutn     Number of cut which machined this entity (UU_TRUE).
**          flag     UU_TRUE = Cut number is provided, use this instead of
**                   the entity type.  This is the automatic setting for
**                   RapidCut.
**       OUTPUT :
**          cutn     Number of cut which machined this entity (Visicut).
**          mdata    Motion data structure for this cut.
**          mattr    Motion attribute structure for this cut.
**    RETURNS      :
**          UU_TRUE if motion attributes were found.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ul_ipv_find_mdata(entity,cutn,mattr,mdata,flag)
LtEntity entity;
UN_mot_attr **mattr;
UN_mot_data **mdata;
LtNat32 *cutn;
UU_LOGICAL flag;
{
	UU_LOGICAL found;
	int i,maxcut;
	UN_mot_data *md;
/*
.....Obtain the motion attributes
.....based on the cut number of this face
*/
	if (LW_mach_mode == LW_VISICUT && !flag)
		*cutn = LiViFaceGetCutNumber(entity);
	md = LW_mot_data_first;
	found = UU_FALSE;
	for (i=0;i<LW_mot_ndata;i++)
	{
		md = (UN_mot_data *)uu_lsnext(md);
		maxcut = md->cut[1];
		if (*cutn >= md->cut[0] && *cutn <= md->cut[1])
		{
			found = UU_TRUE;
			*mdata = md;
			*mattr = md->mattr;
			break;
		}
	}
/*
.....If cut is not found
.....maybe it is the currently active cut
*/
	if (!found && *cutn > maxcut)
	{
		found = UU_TRUE;
		*mdata = LW_mot_data;
		*mattr = LW_mot_attr;
	}
/*
.....End of routine
*/
	return(found);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_mach_type()
**       Determines the machine type based on the types of tools used
**       in the program.
**    PARAMETERS
**       INPUT  :   none
**       OUTPUT :   none
**    RETURNS      : Returns the machine type; LW_MILL or LW_LATHE.
**    SIDE EFFECTS : Loads the global tool array if not already loaded.
**                   in it.
**    WARNINGS     : none
*********************************************************************/
int ul_ipv_mach_type()
{
	int mach,i;
	UU_LOGICAL imill,ilathe;
	UN_cutter_list *cpt;
/*
.....Load the tool array
*/
	if (LW_ntool == 0) ul_ipv_load_tools(&LW_tool_list,&LW_ntool);
/*
.....Loop through the tools to determine machine type
*/
	imill = UU_FALSE;
	ilathe = UU_FALSE;
	cpt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
	for (i=0;i<LW_ntool;i++)
	{
		if (cpt[i].type == NCL_CUTTER_LATHE) ilathe = UU_TRUE;
		else imill = UU_TRUE;
	}
	mach = LW_MILL;
	if (ilathe)
	{
		mach = LW_LATHE;
		if (imill) mach = LW_MILLTURN;
	}
/*
.....End of routine
*/
	return(mach);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_set_mach_type(mtype)
**       Sets the machine type to either a Mill or Lathe when the actual
**       machine is a Mill/Turn.
**    PARAMETERS
**       INPUT  :   mtype  = 0 = LW_MILL or 1 = LW_LATHE.
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : Starts or stops the lathe spindle depending on the
**                   machine type.
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_set_mach_type(mtype)
int mtype;
{
	LW_mach_model_struc *mpt;
	static UU_REAL Sdia_scale=0.;
/*
.....Set the correct machine type
*/
	if (LW_mach_type_main == LW_MILLTURN && mtype != LW_mach_type &&
		 LW_lathe != 0)
	{
		if (LW_mach_simul)
		{
			mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
			if (Sdia_scale == 0.) Sdia_scale = mpt[LW_mach_axes[0]].scale;
		}
		if (mtype == 0)
		{
			LW_mach_type = LW_MILL;
			ul_ipv_lathe_stop();
			ul_ipv_reset_lathe();
			if (LW_mach_simul)
				mpt[LW_mach_axes[0]].scale = 1.;
		}
		else if (mtype == 1)
		{
			LW_mach_type = LW_LATHE;
			ul_ipv_reset_lathe();
			ul_ipv_lathe_start();
			if (LW_mach_simul)
				mpt[LW_mach_axes[0]].scale = Sdia_scale;
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_reset_lathe()
**       Resets the lathe spindle to its 0 position.
**    PARAMETERS
**       INPUT  :   none
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_reset_lathe()
{
	UU_LOGICAL iflag;
	LW_mach_model_struc *mpt;
	LW_mach_toolpin_struc *tpt;
	static LtDoublePoint orig={0.,0.,0.};
	static LtDoubleVector x_axis={1.,0.,0.};
	static LtDoubleVector y_axis={0.,1.,0.};
	static LtDoubleVector z_axis={0.,0.,1.};
/*
.....Initialize routine
*/
	if (LW_lathe == 0) return;
/*
.....Stop the lathe
*/
	iflag = ul_ipv_lathe_stop();
/*
.....Machine simulation is active
*/
	if (LW_mach_simul)
	{
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		tpt = (LW_mach_toolpin_struc *)UU_LIST_ARRAY(&LW_mach_toolpin);
		LiViLatheSetAxes(LW_lathe,mpt[tpt[LW_mach_tpin_ix].axis].axis,x_axis,
			z_axis);
		LiViLatheSetOrientation(LW_lathe,0.);
		if (iflag) ul_ipv_lathe_start();
	}
/*
.....Verification is active
*/
	else
	{
		iflag = ul_ipv_lathe_stop();
		LiViLatheSetAxes(LW_lathe,orig,y_axis,x_axis);
		LiViLatheSetOrientation(LW_lathe,0.);
	}
/*
.....Start the lathe
*/
	if (iflag) ul_ipv_lathe_start();
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_load_tools(clist,ntl)
**       Loads the tools used in the active clfile and stores them
**			into a list.
**    PARAMETERS
**       INPUT  :   ntl   = Number of tools currently in list.
**       OUTPUT :   clist = List array to receive tool list.
**                  ntl   = Number of tools in 'clist'.
**    RETURNS      : none
**    SIDE EFFECTS : Frees the current list if there are any tools
**                   in it.
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_load_tools(clist,ntl)
UU_LIST *clist;
int *ntl;
{
	int modals[20],i,icurpt;
	UN_clstruc *iclpt[4];
	UN_cutter_list *cpt;
/*
.....Initialize list
*/
	if (*ntl != 0)
		uu_list_free(clist);
/*
.....Playback is active
.....just use global list
*/
	if (UN_playback_active)
	{
		uu_list_init(clist,sizeof(UN_cutter_list),50,50);
		cpt = (UN_cutter_list *) UU_LIST_ARRAY(&LW_tool_list);
		for (i=0;i<LW_ntool;i++)
		{
			uu_list_push(clist,&cpt[i]);
		}
		*ntl = LW_ntool;
	}
/*
.....Scan clfile for defined tools
*/
	else
	{
		ncl_play_initscan(modals,iclpt,&icurpt,UU_FALSE);
		ncl_motion_playback(modals,3,UU_NULL,clist,ntl);
		ncl_play_resetscan(iclpt,icurpt);
	}
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_append_tools(clist,ntl)
**       Appends the tools used in the active clfile with an already
**       defined list of tools.  The provided list will keep all its
**       defined tools with the addition of any additional tools found
**       in the clfile.
**    PARAMETERS
**       INPUT  :   clist = Current tool list.
**                  ntl   = Number of tools currently in list.
**       OUTPUT :   clist = Updated (merged) tool list.
**                  ntl   = Number of tools in 'clist'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_append_tools(clist,ntl)
UU_LIST *clist;
int *ntl;
{
	int i,j,myntl,numtl;
	UN_cutter_list *mpt,*cpt;
	UU_LIST mylist;
/*
.....Current list is empty
.....Just load this list
*/
	if (*ntl == 0) ul_ipv_load_tools(clist,ntl);
/*
.....Get a list of tools in clfile
*/
	else
	{
		myntl = 0;
		ul_ipv_load_tools(&mylist,&myntl);
/*
.....Merge the lists
*/
		if (myntl > *ntl || *ntl == 1)
		{
			mpt = (UN_cutter_list *)UU_LIST_ARRAY(&mylist);
			cpt = (UN_cutter_list *)UU_LIST_ARRAY(clist);
			numtl = *ntl;
			for (i=0;i<myntl;i++)
			{
				for (j=0;j<*ntl;j++)
				{
					if (mpt[i].isn != cpt[j].isn || mpt[i].clrec != cpt[j].clrec)
					{
						uu_list_push(clist,&mpt[i]);
						numtl++;
						break;
					}
				}
			}
			*ntl = numtl;
		}
/*
.....Free the local list
*/
		uu_list_free(&mylist);
	}
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_merge_tools()
**       Merges the active tool list with the user provided tool list.
**       This routine is used primarily for merging any user changes
**       to the tool list (Edit Tool List form) to the tool list newly
**       created from the clfile.
**    PARAMETERS
**       INPUT  :
**          clist     Tool list generated from clfile.
**          ntools    Number of tools in 'clist'.
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_merge_tools(clist,ntools)
UU_LIST *clist;
int ntools;
{
	int i,inc,j;
	UN_cutter_list *ipt,*cpt;
/*
.....Initialize routine
*/
	if (LW_ntool == 0) return;
	inc = 0;
/*
.....Merge active list into provided list
*/
	cpt = (UN_cutter_list *) UU_LIST_ARRAY(clist);
	ipt = (UN_cutter_list *) UU_LIST_ARRAY(&LW_tool_list);
	for (i=0;i<ntools;i++)
	{
/*
........Clrecs are in numeric order
*/
		while (ipt[inc].clrec < cpt[i].clrec && inc < LW_ntool) inc++;
		if (inc >= LW_ntool) break;
/*
........Found a match
........Transfer values which can be modified by the form
*/
		if (cpt[i].isn == ipt[inc].isn && cpt[i].clrec == ipt[inc].clrec)
		{
			for (j=0;j<6;j++) cpt[i].cutter[j] = ipt[inc].cutter[j];
			cpt[i].ncparm = ipt[inc].ncparm;
			cpt[i].cut_color = ipt[inc].cut_color;
			cpt[i].toler = ipt[inc].toler;
			cpt[i].maxang = ipt[inc].maxang;
			cpt[i].rapid = ipt[inc].rapid;
			strcpy(cpt[i].symlib,ipt[inc].symlib);
			for (j=0;j<3;j++)
			{
				cpt[i].ctype[j] = ipt[inc].ctype[j];
				cpt[i].color[j] = ipt[inc].color[j];
				cpt[i].trans[j] = ipt[inc].trans[j];
				cpt[i].edge[j] = ipt[inc].edge[j];
				cpt[i].edge_color[j] = ipt[inc].edge_color[j];
				strcpy(cpt[i].symbol[j],ipt[inc].symbol[j]);
			}
			cpt[i].shank_clash = ipt[inc].shank_clash;
			for (j=0;j<4;j++)
			{
				cpt[i].parms[0][j] = ipt[inc].parms[0][j];
				cpt[i].parms[1][j] = ipt[inc].parms[1][j];
				cpt[i].parms[2][j] = ipt[inc].parms[2][j];
			}
			cpt[i].used = ipt[inc].used;
			inc++;
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_mount_lathe_tools()
**       Mounts all defined lathe tools onto the lathe turret.
**    PARAMETERS
**       INPUT  :   none
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : Loads the global tool array if not already loaded.
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_mount_lathe_tools()
{
	int tool,ipt,modals[20],icurpt,msav;
	UU_LOGICAL isav;
	UU_REAL rval;
	UN_clstruc *iclpt[4];
	LW_mach_model_struc *mpt;
/*
.....Initialize routine
*/
	isav = LW_active;
	msav = LW_mach_type;
	LW_active = UU_TRUE;
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
	ipt = LW_spindle[LW_spindle_ix[0]];
	rval = mpt[ipt].turret->iparm[0];
	tool = (rval / (360./mpt[ipt].position)) + 1;
/*
.....Load the tool array
*/
	if (LW_ntool == 0) ul_ipv_load_tools(&LW_tool_list,&LW_ntool);
/*
.....Mount defined tools onto turret
*/
	ncl_play_initscan(modals,iclpt,&icurpt,UU_FALSE);
	ncl_motion_playback(modals,4,UU_NULL,&LW_tool_list,&LW_ntool);
	ncl_play_resetscan(iclpt,icurpt);
/*
.....Reset turret position
*/
	ul_ipv_position_turret(tool,UU_TRUE);
	LW_active = isav;
	LW_mach_type = msav;
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_format_tool(sbuf,tool)
**       Formats the requested tool for output to a form list.
**    PARAMETERS
**       INPUT  :
**          tool     Tool number to format.
**       OUTPUT :
**          sbuf     Output buffer to hold formatted tool.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_format_tool(sbuf,tool)
char *sbuf;
int tool;
{
	int tlno,ntool,i,inum;
	UU_REAL rnum;
	char lnum[80];
	UN_cutter_list *cpt;
/*
.....Initialize routine
*/
	tlno = tool;
/*
.....Machining session tool list
*/
	if (tlno < 0)
	{
		tlno = tlno * -1;
		ntool = LW_ntool_sess;
		cpt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_sess);
	}
/*
.....Active tool list
*/
	else
	{
		ntool = LW_ntool;
		cpt = (UN_cutter_list *)UU_LIST_ARRAY(&LW_tool_list);
	}
/*
.....Format tool
*/
	if (tlno < ntool)
	{
		inum = tlno + 1;
		sprintf(sbuf,"%04d CUTTER/",inum);
		if (cpt[tlno].type == NCL_CUTTER_LATHE) strcat(sbuf,"LATHE,");
		else if (cpt[tlno].type == NCL_CUTTER_BLADE) strcat(sbuf,"BLADE,");
/*
........Cutter parameters
*/
		if (cpt[tlno].ncparm > 0)
		{
			UM_len_inttoext(cpt[tlno].cutter[0],rnum);
			sprintf(lnum,"%g",rnum);
			strcat(sbuf,lnum);
			for (i=1;i<cpt[tlno].ncparm;i++)
			{
				if (i<=2 || (cpt[tlno].type == NCL_CUTTER_MILL && i == 4) ||
						(cpt[tlno].type == NCL_CUTTER_LATHE && i == 4 &&
						cpt[tlno].cutter[3] == 0))
				{
					UM_len_inttoext(cpt[tlno].cutter[i],rnum);
				}
/*
				else if (cpt[tlno].type == NCL_CUTTER_BLADE && i == 3)
					rnum = asin(cpt[tlno].cutter[i]) * (180./UM_PI);
*/
				else
					rnum = cpt[tlno].cutter[i];
				sprintf(lnum,", %g",rnum);
				strcat(sbuf,lnum);
			}
/*
........Cutter symbol
*/
			if (cpt[tlno].symbol[0][0] != '\0')
			{
				strcat(sbuf,",");
				strcat(sbuf,cpt[tlno].symbol[0]);
			}
		}
		else
			strcat(sbuf,cpt[tlno].symbol[0]);
	}
/*
.....Tool not defined
*/
	else
		strcpy(sbuf,"Tool Not defined");
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_init_list(list,size)
**       Initializes a form list.
**    PARAMETERS
**       INPUT  :
**          size     Number of items to initialize list for.
**       OUTPUT :
**          list     Initialized list.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_init_list(list,size)
UD_LIST *list;
int size;
{
	ud_free_flist(list);
	list->item = (char **)uu_malloc(size*sizeof(char *));
	list->num_item = 0;
	list->answer = (char *)uu_malloc(sizeof(char)*82);
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_put_list(list,sbuf)
**       Stores an entry into a form list field.
**    PARAMETERS
**       INPUT  :
**          list     List to store the entry into.
**          sbuf     Text of entry.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_put_list(list,sbuf)
UD_LIST *list;
char *sbuf;
{
	list->item[list->num_item] = (char *)uu_malloc(81*sizeof(char));
	if (strlen(sbuf) > 80)
	{
		strncpy(list->item[list->num_item],sbuf,80);
		list->item[list->num_item][80] = '\0';
	}
	else
		strcpy(list->item[list->num_item],sbuf);
	list->num_item++;
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_render_edges(prim,edge,edgcol,intern)
**       Sets the render edges properties for a solid primitive.
**    PARAMETERS
**       INPUT  :
**          prim    = Primitive to set properties for.
**          edge    = UU_TRUE = render edges.
**          edgcol  = Color of rendered edges.
**          defcol  = Default color of rendered edges when 'edgcol' = 0.
**          intern  = UU_TRUE = render internal edges.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_render_edges(prim,edge,edgcol,defcol,intern)
LtSessionPrim prim;
UU_LOGICAL edge,intern;
int edgcol;
{
	int inc;
	LtData dat;
	LtColour col;
/*
........Display edges of stock
*/
	if (edge)
	{
		LiDataSetBoolean(&dat,TRUE);
		LiSessionPrimSetVisualProperty(prim,LI_MW_VIS_PROP_RENDER_EDGES,&dat);
		LiDataSetBoolean(&dat,intern);
		LiSessionPrimSetVisualProperty(prim,LI_MW_VIS_PROP_REN_INTERN_EDGES,&dat);
		if (edgcol == -1) inc = defcol;
		else inc = edgcol;
		ul_ipv_color_set(col,inc);
		LiDataSetColour(&dat,col);
		LiSessionPrimSetVisualProperty(prim,LI_MW_VIS_PROP_EDGE_COLOUR,&dat);
	}
/*
.....Don't display edges
*/
	else
	{
		LiDataSetBoolean(&dat,FALSE);
		LiSessionPrimSetVisualProperty(prim,LI_MW_VIS_PROP_RENDER_EDGES,&dat);
	}
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_color_set(colour,indx)
**       Defines the MachineWorks colour values for the requested color
**       index.
**    PARAMETERS
**       INPUT  :
**          indx    = NCL Color value.
**       OUTPUT :
**          colour  = MachineWorks colour variable.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_color_set(colour,indx)
LtColour colour;
int indx;
{
	LiColourInitialise(colour,UM_pkcolors[indx].red/255,
		UM_pkcolors[indx].green/255,UM_pkcolors[indx].blue/255);
}

/*********************************************************************
**    E_FUNCTION     :  ul_ipv_prescan_stocks()
**       Pre-processes the input clfile for the reason of creating any
**       stocks defined prior to the first motion.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_prescan_stocks()
{
	int modals[20],icurpt;
	UN_clstruc *iclpt[4];
/*
.....Predefine stocks by
.....Processing up to 1st motion
*/
	if (LW_active)
	{
		ncl_play_initscan(modals,iclpt,&icurpt,UU_FALSE);
		modals[2] = 3;
		ncl_motion_playback(modals,0,UU_NULL,&LW_tool_list,&LW_ntool);
/*		ncl_play_resetscan(iclpt,icurpt);*/
	}
}
