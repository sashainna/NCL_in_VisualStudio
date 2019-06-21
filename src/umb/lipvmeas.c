/*********************************************************************
**    NAME         :  lipvmeas.c
**       CONTAINS:
**				ul_ipv_measure
**				ul_ipv_measure_close
**    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvmeas.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:14
*********************************************************************/

#include "usysdef.h"
#include "stdio.h"
#include "lcom.h"
#include "lipv.h"
#include "mcrv.h"
#include "mfort.h"
#include "mdcpln.h"
#include "nclfc.h"
#include "nclmplay.h"
#include "nclfile.h"
#include "nclcmd.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "uhep.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "driver.h"

#define N_ATTRIB 20
#define GEOM_TOG 2
#define FMES 0
#define FSEL 1
#define FTOL 2
#define FSGL 3
#define FSTA 4
#define FSRC 5
#define FCAL 6
#define FSHW 7
#define FRES 8
#define FEDT 9

static void S_attrib();
static void S_point();
static void S_thick();
static void S_volume();
static void S_geometry();
static void S_fill_lists();
static void S_load_source();
static void S_restore_faces();
static int S_distance();
static int S_feature();
static UU_LOGICAL S_2dcalc();

static int Stype=0,Sisn,Slast_isn,Ssrc=1,Ssgl=0,Sreset=1;
static int Sfrm=0,Snoldm=0;
static UU_LOGICAL Sselect = UU_FALSE;
static char Subuf[10];
static UU_REAL Stoler;
static UD_LIST Sstat_list,Ssrc_list,Scall_list;
static LtMaterial Soldm[2];
static LtEntity Sentity[2];
static LtPrim Spoly[2]={0,0};
static LtSessionPrim Sesspoly[2]={0,0};
static LtSessionPrim Sprim[2];
static LtEntityType Setype[2];
static LtBoolean Sedge[2];
static LtColour Secolor[2];

/*********************************************************************
**    S_FUNCTION     :  OnType(filedno, val, stat)
**       Method called when the Measurement Type is changed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnType(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (Stype == GEOM_TOG)
		ud_setfrm_traverse_mask(Sfrm,FTOL,1);
	else
		ud_setfrm_traverse_mask(Sfrm,FTOL,0);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnToler(filedno, val, stat)
**       Method called when the tolerance field is changed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnToler(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UM_len_exttoint(Stoler,LW_geom_toler);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnShow(filedno, val, stat)
**       Method called when the Measurement Type is changed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnShow(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Clear source window if disabled
*/
	if (!Ssrc)
	{
		S_fill_lists(UU_NULL);
	}
/*
.....Set traverse masks
*/
	ud_setfrm_traverse_mask(Sfrm,FSRC,Ssrc);
	ud_setfrm_traverse_mask(Sfrm,FCAL,Ssrc);
	ud_setfrm_traverse_mask(Sfrm,FEDT,Ssrc);
	ud_setfrm_traverse_mask(Sfrm,FRES,Ssrc);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnSelect(filedno, val, stat)
**       Method called when the Select button is pushed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnSelect(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char prompt[80];
	int cursor,button,event,status;
	int *numary;
	UU_LOGICAL split;
	LtPoint orig;
	LtVector norm;
	LtPickedEntityList entlist;
	LtPrim *solptr;
	LtNat32 numsol;
	LW_rv_picked_entity rvent;
/*
.....Initialize routine
*/
	if (Sselect) goto done;
	Sselect = UU_TRUE;
	um_linear_units_str(UM_cpln.length_unit,Subuf);
	cursor = 0;
/*
.....Unhighlight the solids
*/
	S_restore_faces();
/*
.....Take down form
.....if Single Pick Mode
*/
	if (Ssgl) ud_dspfrm_invis(Sfrm);
/*
.....Let user select solid/face to analyze
*/
	strcpy(prompt,"Select face to analyze");
	for(;;)
	{
/*
.....Initialize the status list
*/
		ul_ipv_init_list(&Sstat_list,N_ATTRIB);
		status = UU_SUCCESS;
/*
.....For volume calculations
.....Split solids first
*/
		split = UU_FALSE;
		if (LW_mach_mode == LW_VISICUT && (Stype == 2 || Stype == 6))
		{
			split = UU_TRUE;
			ul_ipv_split_solids(&solptr,&numsol,&numary,UU_TRUE);
		}
/*
.....Get user selection
*/
		if (LW_mach_mode == LW_RAPIDCUT && Stype == 6)
			rvent.picked = UU_TRUE;
		else
		{
			event = ul_ipv_pick_entity("Select entity to analyze",
				LI_ENTITY_TYPE_FACE,&entlist,&rvent,orig,norm,&button);
/*
.....Unhighlight previously selected surface
*/
			S_restore_faces();
/*
.....Exit on middle or right mouse button
*/
			if (event != 3 || button != 1 || Sfrm == 0)
			{
				if (Ssgl) ud_dspfrm_vis(Sfrm);
				break;
			}
		}
/*
.....No entities selected
*/
		if (!rvent.picked)
		{
			ul_ipv_init_list(&Sstat_list,1);
			ul_ipv_put_list(&Sstat_list,"No entities selected.");
		}
/*
.....Determine type of analyzation to perform
*/
		else
		{
			switch (Stype)
			{
/*
.....Motion attributes
*/
			case 0:
				S_attrib(entlist,&rvent,orig,norm);
				break;
/*
.....Point
*/
			case 1:
				S_point(entlist,&rvent,orig,norm);
				break;
/*
.....Geometry
*/
			case 2:
				S_geometry(entlist,&rvent,orig,norm);
				break;
/*
.....Thickness
*/
			case 3:
				S_thick(entlist,&rvent,orig,norm,0);
				break;
/*
.....Gap
*/
			case 4:
				S_thick(entlist,&rvent,orig,norm,1);
				break;
/*
.....Distance
*/
			case 5:
				status = S_distance(entlist,&rvent,orig,norm);
				break;
/*
.....Volume
*/
			case 6:
				S_volume(entlist,&rvent,orig,norm);
				break;
/*
.....Unknown type
*/
			default:
				break;
			}
		}
/*
.....UnSplit solids
*/
		if (split)
		{
			if (numsol > 1)
				ul_ipv_unsplit_solids(solptr,numary,UU_TRUE);
			split = UU_FALSE;
		}
/*
.....Get rid of the picking list
*/
		if (LW_mach_mode == LW_VISICUT)
			LiViPickedEntityListDestroy(entlist);
/*
.....Redisplay the form
*/
		if (Ssgl)
		{
			ud_dspfrm_vis(Sfrm);
		}
/*
.....User rejected secondary picking prompt
*/
		if (status == UU_FAILURE)
		{
			S_restore_faces();
			break;
		}
/*
.....Update the form list
*/
		if (Sfrm != 0)
			ud_dispfrm_update_answer(Sfrm,FSTA,(int *)&Sstat_list);
/*
.....Single Pick Mode
.....Return after first analyzation
*/
		if (Ssgl) break;
/*
.....Volume calculate in RapidCut
.....Does not require picking
*/
		if (LW_mach_mode == LW_RAPIDCUT && Stype == 6) break;
	}
/*
.....UnSplit solids
*/
	if (split)
	{
		if (numsol > 1)
			ul_ipv_unsplit_solids(solptr,numary,UU_TRUE);
		split = UU_FALSE;
	}
	Sselect = UU_FALSE;
/*
.....End of routine
*/
done:;
/*
.....If the form is already closed/deleted then
.....Return fieldno = -1 to not redisplay form
*/
	if (Sfrm==0)
	{
		*fieldno = -1;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnSource(filedno, val, stat)
**       Method called at when  tool in the tool listbox
**			is selected
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnSource(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char temp[81], *tok, *strtok();

	if (*fieldno != FSRC) return UD_FLDOK;
/*
.....val.frmstr contains the selected string
....."isn COMMAND"
*/
	if (val->frmstr!=NULL)
	{
		strcpy(Ssrc_list.answer, val->frmstr);
		strcpy(temp, val->frmstr);
		tok = strtok(temp, " ");
		if (tok!=NULL) Sisn = atoi(tok);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnStack(fieldno, val, stat)
**       Updates the Source list when a line is selected in the
**       Call Stack list.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnStack(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int isn;
	char *tok,*strtok();
	char temp[81];
/*
.....Get ISN of call stack line
.....and display appropriate source lines
*/
	if (val->frmstr != UU_NULL)
	{
		strcpy(Scall_list.answer,val->frmstr);
		strcpy(temp,val->frmstr);
		tok = strtok(temp,":");
		if (tok != UU_NULL)
		{
			isn = atoi(tok);
			ncl_motisn_source_list(isn,&Ssrc_list);
			ud_dispfrm_update_answer(Sfrm,FSRC,(int *)&Ssrc_list);
		}
	}
	return(UD_FLDOK);
}
		
/*********************************************************************
**    S_FUNCTION     :  OnEdit(filedno, val, stat)
**       Method called when the "Edit" button is pushed
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnEdit(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int markval;
	if (Sisn > 0)
	{
		UD_MARK(markval,UU_TRUE);
		if (markval != 0) goto done;
		setnln(&Sisn);
		ncl_cmd_mode();
done:;
		if (Sreset) setnln(&Slast_isn);
		UD_UNMARK(markval);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnClose()
**       Method called at when form is closed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnClose()
{
/*
.....Free the lists
*/
	ud_free_flist(&Sstat_list);
	ud_free_flist(&Ssrc_list);
	ud_free_flist(&Scall_list);
	Sfrm = 0;
/*
.....Unhighlight the solids
*/
	S_restore_faces();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     : S_restore_faces()
**       Resets the highlighted faces/solids to their original color.
**    PARAMETERS
**       INPUT  :   none
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_restore_faces()
{
	int j;
	LtMaterial oldm;
/*
.....Unhighlight previously selected surface
*/
	if (LW_mach_mode == LW_VISICUT)
	{
		for (j=0;j<Snoldm;j++)
		{
			if (Setype[j] == LI_ENTITY_TYPE_SOLID)
				ul_ipv_highlight_entity(&oldm,Sprim[j],LI_ENTITY_TYPE_SOLID,
					Soldm[j],&Sedge[j],Secolor[j]);
			else if (Setype[j] == LI_ENTITY_TYPE_FACE ||
					Setype[j] == LI_ENTITY_TYPE_PATCH)
				ul_ipv_highlight_entity(&oldm,Sentity[j],Setype[j],
					Soldm[j],&Sedge[j],Secolor[j]);
		}
		Snoldm = 0;
/*
.....Delete any generated polycurves
*/
		for (j=0;j<2;j++)
		{
			if (Spoly[j] != 0)
			{
				LiSessionRemovePrim(Sesspoly[j]);
				LiPrimitiveDestroy(Spoly[j]);
				Spoly[j] = 0;
				Sesspoly[j] = 0;
			}
		}
	}
}

/*********************************************************************
**    S_FUNCTION     :  S_fill_lists(mdata)
**       Propogates the Source and Call lists with the data associated
**       with the current selection.
**    PARAMETERS
**       INPUT  :
**          mdata   = Motion attribute bundle.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_fill_lists(mdata)
UN_mot_data *mdata;
{
	int indx,nent,nline,type,nc,isn,i,icmt,iline;
	UM_int2 itype;
	UU_REAL dval;
	char sbuf[NCL_MAX_COMLINE],buf[NCL_MAX_COMLINE],tbuf[NCL_MAX_COMLINE];
	char *p,*p1,*p2,**cptr,*strstr(),*strchr(),lnum[40];
/*
.....Get Call list
*/
	if (mdata == UU_NULL)
	{
		ul_ipv_init_list(&Scall_list,1);
		ul_ipv_put_list(&Scall_list,"");
		ul_ipv_init_list(&Ssrc_list,1);
		ul_ipv_put_list(&Ssrc_list,"");
	}
	else
	{
		ncl_motisn_call_list(mdata->isnptr,UU_NULL,&Scall_list,&isn,2);
		ncl_motisn_source_list(isn,&Ssrc_list);
	}
/*
.....Update the lists
*/
	ud_dispfrm_update_answer(Sfrm,FCAL,(int *)&Scall_list);
	ud_dispfrm_update_answer(Sfrm,FSRC,(int *)&Ssrc_list);
}

/*********************************************************************
**    S_FUNCTION     :  S_attrib(entlist,orig,norm)
**       Displays the motion attributes for the selected face.
**    PARAMETERS
**       INPUT  :
**          entlist  List of entities picked (VisiCut).
**          rvent    Picked entity (RapidCut).
**          orig     World coordinate of user selection.
**          norm     Normal vector of selection.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_attrib(entlist,rvent,orig,norm)
LtPickedEntityList entlist;
LtPoint orig;
LtVector norm;
LW_rv_picked_entity *rvent;
{
	int status;
	UU_LOGICAL found;
	char sbuf[82];
	UU_REAL rval;
	UN_mot_attr *mattr;
	UN_mot_data *mdata;
	LtNat32 cutn;
	LtDouble dis1;
	LtPickedEntity entpick;
	LtEntityType type;
/*	LtGenericPtr cutm;*/
	static char *colcd[]={"OFF","FLOOD","MIST","AIR"};
	static char *cdir[]={"OFF","LEFT","RIGHT"};
	static char *cmod[]={"XYPLAN","YZPLAN","ZXPLAN"};
	static char *sdir[]={"CLW","CCLW"};
/*
.....Calculate user selection
........Visicut
*/
	if (LW_mach_mode == LW_VISICUT)
	{
		entpick = LiViPickedEntityListGetFirst(entlist);
		if (entpick == 0) goto failed;
		status = LiViPickedEntityExpand(entpick,&Sentity[0],&type,&dis1);
		if (status != 0) goto failed;
/*
........Code only used if attribute stored with cut
*/
/*		mdata = (UN_mot_data *)LiViEntityGetAttribute(Sentity[0],LW_attrib);
		if (mdata == UU_NULL) goto no_attr;*/
/*
........Highlight selection
*/
		Setype[0] = LI_ENTITY_TYPE_FACE;
		ul_ipv_highlight_entity(&Soldm[0],Sentity[0],LI_ENTITY_TYPE_FACE,
			LW_material[LW_highlight_color],&Sedge[0],Secolor[0]);
		Snoldm = 1;
	}
/*
.....RapidCut
*/
	else
	{
		cutn = rvent->cutn;
	}
/*
.....Obtain the motion attributes
.....based on the cut number of this face
*/
	found = ul_ipv_find_mdata(Sentity[0],&cutn,&mattr,&mdata,UU_FALSE);
	if (!found) goto no_attr;
/*
.....Display the attributes
........ISN
*/
	sprintf(sbuf,"ISN/%d",mdata->isn);
	ul_ipv_put_list(&Sstat_list,sbuf);
/*
........SEQNO
*/
	if (mdata->seqno != 0)
	{
		sprintf(sbuf,"SEQNO/%d",mdata->seqno);
		ul_ipv_put_list(&Sstat_list,sbuf);
	}
/*
........CLREC
*/
	sprintf(sbuf,"CLREC/%d,%d",mdata->clrec[0],mdata->clrec[1]);
	ul_ipv_put_list(&Sstat_list,sbuf);
/*
........CUT number
*/
	sprintf(sbuf,"CUT/%d",cutn);
	ul_ipv_put_list(&Sstat_list,sbuf);
/*
........CUTTER
*/
	ul_ipv_format_tool(sbuf,mattr->tlno);
	ul_ipv_put_list(&Sstat_list,sbuf);
/*
........Loadtl
*/
	if (mattr->loadtl != 0)
	{
		UM_len_inttoext(mattr->tlen,rval);
		sprintf(sbuf,"LOADTL/%d,LENGTH,%f",mattr->loadtl,rval);
		ul_ipv_put_list(&Sstat_list,sbuf);
	}
/*
........Feedrate
*/
	if (mdata->fr_mode == 0)
	{
		ul_ipv_put_list(&Sstat_list,"RAPID");
	}
	else
	{
		if (mdata->fr_val != 0.)
		{
			UM_len_inttoext(mdata->fr_val,rval);
			if (mdata->fr_mode == 2)
				sprintf(sbuf,"FEDRAT/FPR,%f",rval);
			else
				sprintf(sbuf,"FEDRAT/FPM,%f",rval);
			ul_ipv_put_list(&Sstat_list,sbuf);
		}
	}
/*
........Spindle
*/
	if (mattr->sp_val != 0.)
	{
		sprintf(sbuf,"SPINDL/%f,%s",mattr->sp_val,sdir[mattr->sp_mode]);
		ul_ipv_put_list(&Sstat_list,sbuf);
	}
/*
........Coolnt
*/
	strcpy(sbuf,"COOLNT/");
	strcat(sbuf,colcd[mattr->coolnt]);
	ul_ipv_put_list(&Sstat_list,sbuf);
/*
........Cutcom
*/
	if (mattr->cc_dir == 0)
		sprintf(sbuf,"CUTCOM/%s",cdir[mattr->cc_dir]);
	else
		sprintf(sbuf,"CUTCOM/%s,%s",cdir[mattr->cc_dir],cmod[mattr->cc_mode]);
	ul_ipv_put_list(&Sstat_list,sbuf);
/*
.....Load the source commands
.....which generated this cut
*/
	S_fill_lists(mdata);
/*
.....End of attributes
*/
	goto done;
/*
.....Did not select anything
*/
failed:;
	ul_ipv_put_list(&Sstat_list,"No entities selected.");
	S_fill_lists(UU_NULL);
	goto done;
/*
.....No attribute found for cut number
*/
no_attr:;
	ul_ipv_put_list(&Sstat_list,"Attributes are not stored with this entity.");
	S_fill_lists(UU_NULL);
	goto done;
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    S_FUNCTION     :  S_point(entlist,rvent,orig,norm)
**       Displays the point and normal of the selected face.
**    PARAMETERS
**       INPUT  :
**          entlist  List of entities picked (Visicut).
**          rvent    Picked entity (RapidCut).
**          orig     World coordinate of user selection.
**          norm     Normal vector of selection.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_point(entlist,rvent,orig,norm)
LtPickedEntityList entlist;
LW_rv_picked_entity *rvent;
LtPoint orig;
LtVector norm;
{
	int status;
	char sbuf[82];
	LtDouble dis1;
	LtPickedEntity entpick;
	LtEntityType type;
	LtDouble plane[4];
	UU_REAL pt[3];
	UN_mot_attr *mattr;
	UN_mot_data *mdata;
	LtNat32 cutn;
	UU_LOGICAL found;
/*
.....Calculate user selection
*/
	if (LW_mach_mode == LW_VISICUT)
	{
		entpick = LiViPickedEntityListGetFirst(entlist);
		if (entpick == 0) goto failed;
		status = LiViPickedEntityExpand(entpick,&Sentity[0],&type,&dis1);
		if (status != 0) goto failed;
/*
.....Highlight selection
*/
		Setype[0] = LI_ENTITY_TYPE_FACE;
		ul_ipv_highlight_entity(&Soldm[0],Sentity[0],LI_ENTITY_TYPE_FACE,
			LW_material[LW_highlight_color],&Sedge[0],Secolor[0]);
		Snoldm = 1;
		LiBrFaceGetPlaneEquation(Sentity[0],plane);
	}
/*
.....Get RapidCut parameters
*/
	else
	{
		dis1 = rvent->dis;
		um_vctovc(rvent->norm,plane);
		cutn = rvent->cutn;
	}
/*
.....Display the face attributes
*/
	pt[0] = orig[0] + norm[0]*dis1;
	pt[1] = orig[1] + norm[1]*dis1;
	pt[2] = orig[2] + norm[2]*dis1;
	UM_cc_inttoext(pt,pt);
	sprintf(sbuf,"Point  = %f, %f, %f",pt[0],pt[1],pt[2]);
	ul_ipv_put_list(&Sstat_list,sbuf);
	sprintf(sbuf,"Normal = %f, %f, %f",plane[0],plane[1],plane[2]);
	ul_ipv_put_list(&Sstat_list,sbuf);
/*
.....Show the source command for this face
*/
	if (Ssrc)
	{
		found = ul_ipv_find_mdata(Sentity[0],&cutn,&mattr,&mdata,UU_FALSE);
		if (!found) goto no_attr;
		S_fill_lists(mdata);
	}
	goto done;
/*
.....Did not select anything
*/
failed:;
	ul_ipv_put_list(&Sstat_list,"No entities selected.");
	if (Ssrc)
		S_fill_lists(UU_NULL);
	goto done;
/*
.....No attribute found for cut number
*/
no_attr:;
	ul_ipv_init_list(&Ssrc_list,1);
	ul_ipv_put_list(&Ssrc_list,"Attributes are not stored with this entity.");
	ud_dispfrm_update_answer(Sfrm,FSRC,(int *)&Ssrc_list);
	goto done;
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    S_FUNCTION     :  S_geometry(entlist,rvent,orig,norm,dir)
**       Determines if the selected face is part of a larger radius
**       (sphere, cylinder, etc.) or is simply a plane.  The results
**       are displayed.
**    PARAMETERS
**       INPUT  :
**          entlist  List of entities picked (Visicut).
**          rvent    Picked entity (RapidCut).
**          orig     World coordinate of user selection.
**          norm     Normal vector of selection.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_geometry(entlist,rvent,orig,norm)
LtPickedEntityList entlist;
LW_rv_picked_entity *rvent;
LtPoint orig;
LtVector norm;
{
	int status,i,npts,ist,icvst,icven,inc,nt,nbpts,ien;
	char sbuf[82];
	UU_REAL dis,um_dsupt(),lowdis,cir[8],pltemp[2][3],x,y,r;
	UM_coord cen,ptx;
	UU_LOGICAL found,radflg;
	UU_LIST slist,blist;
	UN_mot_attr *mattr;
	UN_mot_data *mdata;
	LtNat32 cutn;
	LtDoublePoint pt,*cvpt,*bpt,spos;
	LtDouble dis1,plane[4];
	LtPickedEntity entpick;
	LtEntityType type;
	LtVector axis[2];
	LtBody sbody;
	LtBodyQuery qbody;
	LtFace sface;
	LtLoop sloop;
	LtLoopEdge sedge,sed1;
	LtVertex svert;
	LtPrim prim;
	LtData stuff;
/*
.....Initialize routine
*/
	Sprim[1] = 0;
	npts = 0;
	nbpts = 0;
/*
.....RapidCut
.....Just show the plane
*/
	if (LW_mach_mode == LW_RAPIDCUT)
	{
		pt[0] = orig[0] + norm[0]*rvent->dis;
		pt[1] = orig[1] + norm[1]*rvent->dis;
		pt[2] = orig[2] + norm[2]*rvent->dis;
		dis1 = um_dot(rvent->norm,pt);
		UM_len_inttoext(dis1,r);
		sprintf(sbuf,"Plane = %f,%f,%f,%f",rvent->norm[0],rvent->norm[1],
			rvent->norm[2],dis1);
		ul_ipv_put_list(&Sstat_list,sbuf);
		cutn = rvent->cutn;
		goto srclab;
	}
/*
.....Calculate user selection
*/
	entpick = LiViPickedEntityListGetFirst(entlist);
	if (entpick == 0) goto failed;
	status = LiViPickedEntityExpand(entpick,&Sentity[0],&type,&dis1);
	if (status != 0) goto failed;
/*
.....Process Visicut Solid Feature
.....Cone, Cylinder, etc.
*/
	status = S_feature(entpick);
	if (status == UU_SUCCESS) goto srclab;
/*
.....Get the face plane
*/
	LiBrFaceGetPlaneEquation(Sentity[0],plane);
	um_vctmsc(plane,-1.,plane);
	pt[0] = orig[0] + norm[0]*dis1;
	pt[1] = orig[1] + norm[1]*dis1;
	pt[2] = orig[2] + norm[2]*dis1;
/*
.....Get face boundary
*/
	Sprim[0] = LiViEntityGetSessionPrim(Sentity[0],LI_ENTITY_TYPE_FACE);
	if (Sprim[0] == 0) goto failed;
	uu_list_init(&blist,sizeof(LtDoublePoint),200,200);
	sbody = LiViSolidGetBody(Sprim[0]);
	qbody = LiBrBodyQueryCreate(sbody);
	sloop = LiBrFaceGetFirstLoop(qbody,Sentity[0]);
	if (sloop == 0) goto no_analyze;
	for (;;)
	{
		sedge = LiBrLoopGetFirstLoopEdge(qbody,sloop);
		if (sedge == 0) goto no_analyze;
		sed1 = sedge;
		for (;;)
		{
			if (nbpts == 0)
			{
				svert = LiBrLoopEdgeGetStartVertex(qbody,sedge);
				LiBrVertexGetPosition(svert,spos);
				uu_list_push(&blist,spos);
				nbpts++;
			}
			svert = LiBrLoopEdgeGetFinishVertex(qbody,sedge);
			LiBrVertexGetPosition(svert,spos);
			uu_list_push(&blist,spos);
			nbpts++;
			sedge = LiBrLoopEdgeGetNextLoopEdge(qbody,sedge);
			if (sedge == 0 || sedge == sed1) break;
		}
		sloop = LiBrLoopGetNextLoop(qbody,sloop);
		if (sloop == 0) break;
	}
	LiBrBodyQueryDestroy(qbody);
/*
.....Calculate the slicing planes
*/
	bpt = (LtDoublePoint *)UU_LIST_ARRAY(&blist);
	um_vctovc(pt,pltemp[0]); um_vctovc(plane,pltemp[1]);
	status = um_calc_rect(pltemp,bpt,nbpts,cen,axis[0],axis[1],&x,&y);
	if (status == UU_FAILURE) goto no_analyze;
/*
.....Highlight selections
*/
	Setype[0] = LI_ENTITY_TYPE_FACE;
	ul_ipv_highlight_entity(&Soldm[0],Sentity[0],LI_ENTITY_TYPE_FACE,
		LW_material[LW_highlight_color],&Sedge[0],Secolor[0]);
	Snoldm = 1;
/*
.....Display the face attributes
*/
	UM_len_inttoext(plane[3],r);
	sprintf(sbuf,"Plane = %f,%f,%f,%f",plane[0],plane[1],plane[2],r);
	ul_ipv_put_list(&Sstat_list,sbuf);
/*
.....Get solid slice
.....For forward and tlaxis vectors
*/
	for (nt=0;nt<2;nt++)
	{
		Sprim[1] = LiViSolidCreateSlice(Sprim[0],pt,axis[nt]);
		if (Sprim[1] == 0) goto no_analyze;
/*
.....Get the points from the slice
*/
		sbody = LiViSolidGetBody(Sprim[1]);
		qbody = LiBrBodyQueryCreate(sbody);
		if (sbody == 0) goto no_analyze;
		sface = LiBrBodyGetFirstFace(sbody);
		if (sface == 0) goto no_analyze;
		uu_list_init(&slist,sizeof(LtDoublePoint),200,200);
		npts = 0;
		lowdis = 1000000.;
		for (;;)
		{
			sloop = LiBrFaceGetFirstLoop(qbody,sface);
			if (sloop == 0) goto no_analyze;
			for (;;)
			{
				sedge = LiBrLoopGetFirstLoopEdge(qbody,sloop);
				if (sedge == 0) goto no_analyze;
				sed1 = sedge;
				ist = npts;
				for (;;)
				{
					svert = LiBrLoopEdgeGetStartVertex(qbody,sedge);
					LiBrVertexGetPosition(svert,spos);
					dis = um_dsupt(plane,spos);
					if (npts == 0)
					{
						uu_list_push(&slist,spos);
						npts++;
					}
					svert = LiBrLoopEdgeGetFinishVertex(qbody,sedge);
					LiBrVertexGetPosition(svert,spos);
					dis = dis + um_dsupt(plane,spos);
					if (dis < lowdis)
					{
						icvst = ist;
						lowdis = dis;
					}
					uu_list_push(&slist,spos);
					npts++;
					sedge = LiBrLoopEdgeGetNextLoopEdge(qbody,sedge);
					if (sedge == 0 || sedge == sed1) break;
				}
				if (icvst == ist) icven = npts;
				sloop = LiBrLoopGetNextLoop(qbody,sloop);
				if (sloop == 0) break;
			}
			sface = LiBrFaceGetNextFace(sface);
			if (sface == 0) break;
		}
		LiBrBodyQueryDestroy(qbody);
/*
.....Find the closest point to selected point
.....from splice
*/
		lowdis = 10000.;
		cvpt = (LtDoublePoint *)UU_LIST_ARRAY(&slist);
		for (i=0;i<npts;i++)
		{
			dis = um_dcccc(cvpt[i],pt);
			if (dis < lowdis)
			{
				inc = i;
				lowdis = dis;
			}
		}
/*
.....Check for a radius at selected point
*/
/*for (i=0;i<nbpts;i++) printf("Bpt = %g,%g,%g\n",bpt[i][0],bpt[i][1],bpt[i][2]);
printf("Xaxis = %g,%g,%g\n",axis[0][0],axis[0][1],axis[0][2]);
printf("Yaxis = %g,%g,%g\n",axis[1][0],axis[1][1],axis[1][2]);*/
		radflg = S_2dcalc(cvpt,icvst,icven,inc,bpt,nbpts,pt,cir,&ist,&ien);
/*
.....Display the cylinder if applicable
*/
		if (radflg)
		{
			UM_len_inttoext(cir[6],r);
			sprintf(sbuf,"Radius = %f %s",r,Subuf);
			ul_ipv_put_list(&Sstat_list,sbuf);
			UM_len_inttoext(cir[7],r);
			sprintf(sbuf,"Arc Length = %f %s",r,Subuf);
			ul_ipv_put_list(&Sstat_list,sbuf);
			UM_cc_inttoext(cir,ptx);
			sprintf(sbuf,"Center = %f,%f,%f",ptx[0],ptx[1],ptx[2]);
			ul_ipv_put_list(&Sstat_list,sbuf);
			sprintf(sbuf,"Normal = %f,%f,%f",cir[3],cir[4],cir[5]);
			ul_ipv_put_list(&Sstat_list,sbuf);
/*
........Show curve on model
*/
			ien = ien - ist + 1;
			ul_ipv_polycurve(&Spoly[nt],&Sesspoly[nt],&cvpt[ist],ien);
		}
/*
.....Display the point list
*/
/*		for (i=icvst;i<icven;i++)
		{
			UM_cc_inttoext(cvpt[i],pt);
			sprintf(sbuf,"Point = %f,%f,%f",pt[0],pt[1],pt[2]);
			ul_ipv_put_list(&Sstat_list,sbuf);
		}*/
/*
.....Free memory
*/
/*		LiSessionPrimGetProperty(Sprim[1],LI_SPRIM_PROP_PRIM,&stuff);
		prim = (LtPrim)LiDataGetGenericPtr(&stuff);*/
		LiSessionRemovePrim(Sprim[1]);
/*		LiPrimitiveDestroy(prim);*/
		uu_list_free(&slist);
		Sprim[1] = 0;
		npts = 0;
	}
/*
.....Show the source command for this face
*/
srclab:;
	if (Ssrc)
	{
		found = ul_ipv_find_mdata(Sentity[0],&cutn,&mattr,&mdata,UU_FALSE);
		if (!found) goto no_attr;
		S_fill_lists(mdata);
	}
	goto done;
/*
.....Did not select anything
*/
failed:;
	ul_ipv_put_list(&Sstat_list,"No entities selected.");
	if (Ssrc)
		S_fill_lists(UU_NULL);
	goto done;
/*
.....Could not analyze solid
*/
no_analyze:;
	sprintf(sbuf,"Could not analyze solid.");
	ul_ipv_put_list(&Sstat_list,sbuf);
	if (Ssrc)
		S_fill_lists(UU_NULL);
	goto done;
/*
.....No attribute found for cut number
*/
no_attr:;
	ul_ipv_init_list(&Ssrc_list,1);
	ul_ipv_put_list(&Ssrc_list,"Attributes are not stored with this entity.");
	ud_dispfrm_update_answer(Sfrm,FSRC,(int *)&Ssrc_list);
	goto done;
/*
.....End of routine
*/
done:;
	if (Sprim[1] != 0)
	{
/*		LiSessionPrimGetProperty(Sprim[1],LI_SPRIM_PROP_PRIM,&stuff);
		prim = (LtPrim)LiDataGetGenericPtr(&stuff);*/
		LiSessionRemovePrim(Sprim[1]);
/*		LiPrimitiveDestroy(prim);*/
		Sprim[1] = 0;
	}
	if (npts > 0) uu_list_free(&slist);
	if (nbpts > 0) uu_list_free(&blist);
	return;
}

/*********************************************************************
**    S_FUNCTION     :  S_feature(entpick)
**       Determines if the selected face is part of a Visicut feature
**       (surface sphere, cylinder, etc.) or is simply a plane.  The results
**       are displayed if it is or else further analysis can be done
**       by the calling routine.
**    PARAMETERS
**       INPUT  :
**          entpick  Entity picked.
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS if entity picked was a valid feature
**                   surface in Visicut, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_feature(entpick)
LtPickedEntity entpick;
{
	int status;
	char sbuf[82];
	UU_REAL r;
	UM_coord ptx;
	LtPickedSurface vsurf;
	LtStatus vstat;
/*
.....Initialize routine
*/
	status = UU_FAILURE;
/*
.....Process Visicut Solid Feature
*/
	vstat = LiViPickedEntityGetSurface(entpick,&vsurf);
	if (vstat != LI_STATUS_OK) goto done;
/*
.....Print out surface data
*/
	switch (vsurf.type)
	{
	case LI_MW_SURFACE_CONE:
		ul_ipv_put_list(&Sstat_list,"Primitive: Cone");
		UM_cc_inttoext(vsurf.surface.cone.p,ptx);
		sprintf(sbuf,"Center = %f,%f,%f",ptx[0],ptx[1],ptx[2]);
		ul_ipv_put_list(&Sstat_list,sbuf);
		sprintf(sbuf,"Normal = %f,%f,%f",vsurf.surface.cone.v[0],
			vsurf.surface.cone.v[1],vsurf.surface.cone.v[2]);
		r = atan(vsurf.surface.cone.k) * UM_RADIAN;
		sprintf(sbuf,"Angle = %f deg",r);
		ul_ipv_put_list(&Sstat_list,sbuf);
		break;
/*
........Cylinder
*/
	case LI_MW_SURFACE_CYLINDER:
		ul_ipv_put_list(&Sstat_list,"Primitive: Cylinder");
		UM_len_inttoext(vsurf.surface.cylinder.r,r);
		sprintf(sbuf,"Radius = %f %s",r,Subuf);
		ul_ipv_put_list(&Sstat_list,sbuf);
		UM_cc_inttoext(vsurf.surface.cylinder.p,ptx);
		sprintf(sbuf,"Center = %f,%f,%f",ptx[0],ptx[1],ptx[2]);
		ul_ipv_put_list(&Sstat_list,sbuf);
		sprintf(sbuf,"Normal = %f,%f,%f",vsurf.surface.cylinder.v[0],
			vsurf.surface.cylinder.v[1],vsurf.surface.cylinder.v[2]);
		ul_ipv_put_list(&Sstat_list,sbuf);
		break;
/*
........Plane
*/
	case LI_MW_SURFACE_PLANE:
		ul_ipv_put_list(&Sstat_list,"Primitive: Plane");
		UM_len_inttoext(vsurf.surface.plane.d,r);
		r = r * -1;
		sprintf(sbuf,"Plane = %f,%f,%f,%f",vsurf.surface.plane.a,
			vsurf.surface.plane.b,vsurf.surface.plane.c,r);
		ul_ipv_put_list(&Sstat_list,sbuf);
		break;
/*
........Sphere
*/
	case LI_MW_SURFACE_SPHERE:
		ul_ipv_put_list(&Sstat_list,"Primitive: Sphere");
		UM_len_inttoext(vsurf.surface.sphere.r,r);
		sprintf(sbuf,"Radius = %f %s",r,Subuf);
		ul_ipv_put_list(&Sstat_list,sbuf);
		UM_cc_inttoext(vsurf.surface.sphere.p,ptx);
		sprintf(sbuf,"Center = %f,%f,%f",ptx[0],ptx[1],ptx[2]);
		ul_ipv_put_list(&Sstat_list,sbuf);
		break;
/*
........Swept Torus
*/
	case LI_MW_SURFACE_SWEPT_TORUS:
		ul_ipv_put_list(&Sstat_list,"Primitive: Swept Torus");
		UM_len_inttoext(vsurf.surface.swept_torus.r_major,r);
		sprintf(sbuf,"Major Radius = %f %s",r,Subuf);
		ul_ipv_put_list(&Sstat_list,sbuf);
		UM_len_inttoext(vsurf.surface.swept_torus.r_minor,r);
		sprintf(sbuf,"Minor Radius = %f %s",r,Subuf);
		ul_ipv_put_list(&Sstat_list,sbuf);
		UM_cc_inttoext(vsurf.surface.swept_torus.p,ptx);
		sprintf(sbuf,"Center = %f,%f,%f",ptx[0],ptx[1],ptx[2]);
		ul_ipv_put_list(&Sstat_list,sbuf);
		sprintf(sbuf,"Normal = %f,%f,%f",vsurf.surface.swept_torus.vperp[0],
			vsurf.surface.swept_torus.vperp[1],vsurf.surface.swept_torus.vperp[2]);
		ul_ipv_put_list(&Sstat_list,sbuf);
		sprintf(sbuf,"Sweep = %f,%f,%f",vsurf.surface.swept_torus.vaxis[0],
			vsurf.surface.swept_torus.vaxis[1],vsurf.surface.swept_torus.vaxis[2]);
		ul_ipv_put_list(&Sstat_list,sbuf);
		break;
/*
........Torus
*/
	case LI_MW_SURFACE_TORUS:
		ul_ipv_put_list(&Sstat_list,"Primitive: Torus");
		UM_len_inttoext(vsurf.surface.torus.r_major,r);
		sprintf(sbuf,"Major Radius = %f %s",r,Subuf);
		ul_ipv_put_list(&Sstat_list,sbuf);
		UM_len_inttoext(vsurf.surface.torus.r_minor,r);
		sprintf(sbuf,"Minor Radius = %f %s",r,Subuf);
		ul_ipv_put_list(&Sstat_list,sbuf);
		UM_cc_inttoext(vsurf.surface.torus.p,ptx);
		sprintf(sbuf,"Center = %f,%f,%f",ptx[0],ptx[1],ptx[2]);
		ul_ipv_put_list(&Sstat_list,sbuf);
		sprintf(sbuf,"Normal = %f,%f,%f",vsurf.surface.torus.v[0],
			vsurf.surface.torus.v[1],vsurf.surface.torus.v[2]);
		ul_ipv_put_list(&Sstat_list,sbuf);
		break;
/*
........Unsupported surface
*/
	default:
		goto done;
	}
/*
.....Highlight selection
*/
	Setype[0] = LI_ENTITY_TYPE_PATCH;
	ul_ipv_highlight_entity(&Soldm[0],Sentity[0],Setype[0],
		LW_material[LW_highlight_color],&Sedge[0],Secolor[0]);
	Snoldm = 1;
	status = UU_SUCCESS;
/*
.....End of routine
*/
done:;
	return(status);
}

/*********************************************************************
**    S_FUNCTION     :  S_thick(entlist,rvent,orig,norm,dir)
**       Displays a thickness of the part at the selected face.
**    PARAMETERS
**       INPUT  :
**          entlist  List of entities picked (Visicut).
**          rvent    Picked entity (RapidCut).
**          orig     World coordinate of user selection.
**          norm     Normal vector of selection.
**          dir      0 = Report thickness.  1 = Report gap.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_thick(entlist,rvent,orig,norm,dir)
LtPickedEntityList entlist;
LW_rv_picked_entity *rvent;
LtPoint orig;
LtVector norm;
int dir;
{
	int status;
	char sbuf[82];
	LtDouble dis1,dis2,thk;
	LtPickedEntity entpick;
	LtEntityType type;
	LtDouble plane[4];
	UM_coord pt1,pt2;
	UM_vector vc1;
	UN_mot_attr *mattr;
	UN_mot_data *mdata;
	LtNat32 cutn;
	UU_LOGICAL found;
	LtPickedEntityList entlist2;
	static char *prm[]={"Thickness","Gap"};
	LW_rv_picked_entity rvent1;
	LtData stuff;
/*
.....Initialize routine
*/
	thk = .005;
/*
.....Calculate user selection
*/
	if (LW_mach_mode == LW_VISICUT)
	{
		entpick = LiViPickedEntityListGetFirst(entlist);
		if (entpick == 0) goto failed;
		status = LiViPickedEntityExpand(entpick,&Sentity[0],&type,&dis1);
		if (status != 0) goto failed;
		LiDataSetEnum(&stuff,LI_MW_COORD_SYSTEM_WORLD);
		LiSessionSetProperty(LW_session[LW_mach_mode],
			LI_SESS_PROP_VI_QUERY_COORD_SYS,&stuff);
		LiBrFaceGetPlaneEquation(Sentity[0],plane);
		LiDataSetEnum(&stuff,LI_MW_COORD_SYSTEM_LOCAL);
		LiSessionSetProperty(LW_session[LW_mach_mode],
			LI_SESS_PROP_VI_QUERY_COORD_SYS,&stuff);
	}
	else
	{
		dis1 = rvent->dis;
		um_vctovc(rvent->norm,plane);
		cutn = rvent->cutn;
		if (dir == 0) thk =.1;
	}
/*
.....Prepare for thickness calculations
*/
	if (dir == 0) um_vctmsc(plane,-1.,plane);
	pt1[0] = orig[0] + norm[0]*dis1;
	pt1[1] = orig[1] + norm[1]*dis1;
	pt1[2] = orig[2] + norm[2]*dis1;
	pt1[0] = pt1[0] + plane[0]*thk;
	pt1[1] = pt1[1] + plane[1]*thk;
	pt1[2] = pt1[2] + plane[2]*thk;
/*
.....Get second entity
*/
	if (LW_mach_mode == LW_VISICUT)
	{
		entlist2 = LiViSolidPick(LW_viewport,pt1,plane,LI_MW_COORD_SYSTEM_VIEW,
			LI_ENTITY_TYPE_FACE,.001,2);
		if (entlist2 == 0) goto one_ent;
		entpick = LiViPickedEntityListGetFirst(entlist2);
		if (entpick == 0) goto one_ent;
		status = LiViPickedEntityExpand(entpick,&Sentity[1],&type,&dis2);
		if (status != 0) goto one_ent;
	}
/*
.....RapidCut
*/
	else
	{
		if (dir == 1)
		{
			ul_ipv_pick_next(pt1,plane,&rvent1);
			if (!rvent1.picked) goto one_ent;
			dis2 = rvent1.dis;
		}
/*
.....RapidCut will return a distance of 0
.....if the projection point is within the stock
.....so we have to keep moving it until it
.....reaches the other side on a Thickness calculation
*/
		else
		{
			um_vctovc(pt1,pt2);
			rvent1.dis = 0.;
			while (rvent1.dis == 0.)
			{
				ul_ipv_pick_next(pt1,plane,&rvent1);
				if (!rvent1.picked) rvent1.dis = 1.;
				if (rvent1.dis == 0.)
				{
					pt1[0] = pt1[0] + plane[0]*thk;
					pt1[1] = pt1[1] + plane[1]*thk;
					pt1[2] = pt1[2] + plane[2]*thk;
				}
			}
/*
........Made it through the part
........Now reverse the vector and find the part edge
*/
			um_vctmsc(plane,-1.,vc1);
			ul_ipv_pick_next(pt1,vc1,&rvent1);
			if (!rvent1.picked) goto one_ent;
			pt1[0] = pt1[0] + vc1[0]*rvent1.dis;
			pt1[1] = pt1[1] + vc1[1]*rvent1.dis;
			pt1[2] = pt1[2] + vc1[2]*rvent1.dis;
			dis2 = um_dcccc(pt1,pt2);
		}
	}
/*
.....Calculate distance
*/
	dis1 = dis2 + thk;
/*
.....Highlight selections
*/
	if (LW_mach_mode == LW_VISICUT)
	{
		if (Sentity[0] == Sentity[1]) goto one_ent;
		Setype[0] = LI_ENTITY_TYPE_FACE;
		ul_ipv_highlight_entity(&Soldm[0],Sentity[0],LI_ENTITY_TYPE_FACE,
			LW_material[LW_highlight_color],&Sedge[0],Secolor[0]);
		Setype[1] = LI_ENTITY_TYPE_FACE;
		ul_ipv_highlight_entity(&Soldm[1],Sentity[1],LI_ENTITY_TYPE_FACE,
			LW_material[LW_highlight_color],&Sedge[1],Secolor[1]);
		Snoldm = 2;
	}
/*
.....Display the thickness
*/
	UM_len_inttoext(dis1,dis1);
	sprintf(sbuf,"%s = %f %s",prm[dir],dis1,Subuf);
	ul_ipv_put_list(&Sstat_list,sbuf);
/*
.....Show the source command for this face
*/
	if (Ssrc)
	{
		found = ul_ipv_find_mdata(Sentity[0],&cutn,&mattr,&mdata,UU_FALSE);
		if (!found) goto no_attr;
		S_fill_lists(mdata);
	}
	goto done;
/*
.....Did not select anything
*/
failed:;
	ul_ipv_put_list(&Sstat_list,"No entities selected.");
	if (Ssrc)
		S_fill_lists(UU_NULL);
	goto done;
/*
.....Only one entity selected
*/
one_ent:;
	sprintf(sbuf,"Could not calculate %s",prm[dir]);
	ul_ipv_put_list(&Sstat_list,sbuf);
	if (Ssrc)
		S_fill_lists(UU_NULL);
	goto done;
/*
.....No attribute found for cut number
*/
no_attr:;
	ul_ipv_init_list(&Ssrc_list,1);
	ul_ipv_put_list(&Ssrc_list,"Attributes are not stored with this entity.");
	ud_dispfrm_update_answer(Sfrm,FSRC,(int *)&Ssrc_list);
	goto done;
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    S_FUNCTION     :  S_distance(entlist,rvent,orig,norm)
**       Displays the distance between two points and optionally faces
**       if the faces are parallel.
**    PARAMETERS
**       INPUT  :
**          entlist  List of entities picked (Visicut).
**          rvent    Picked entity (RapidCut).
**          orig     World coordinate of user selection.
**          norm     Normal vector of selection.
**       OUTPUT :
**          none
**    RETURNS      : UU_FAILURE if the user entered Reject Op to the
**                   picking prompt.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_distance(entlist,rvent,orig,norm)
LtPickedEntityList entlist;
LW_rv_picked_entity *rvent;
LtPoint orig;
LtVector norm;
{
	int status,event,button,retstat;
	char sbuf[82];
	LtDouble dis1,dis2,disx,disy,disz;
	LtPickedEntity entpick;
	LtEntityType type;
	LtDouble plane[4],plane2[4];
	LtMaterial oldm;
	UU_REAL pt1[3],pt2[3],co,fabs();
	UN_mot_attr *mattr;
	UN_mot_data *mdata;
	LtNat32 cutn;
	UU_LOGICAL found;
	LtPickedEntityList entlist2;
	LtPoint orig2;
	LtVector norm2;
	LW_rv_picked_entity rvent1;
/*
.....Calculate user selection
*/
	retstat = UU_SUCCESS;
	entlist2 = 0;
	if (LW_mach_mode == LW_VISICUT)
	{
		entpick = LiViPickedEntityListGetFirst(entlist);
		if (entpick == 0) goto failed;
		status = LiViPickedEntityExpand(entpick,&Sentity[0],&type,&dis1);
		if (status != 0) goto failed;
		LiBrFaceGetPlaneEquation(Sentity[0],plane);
	}
	else
	{
		dis1 = rvent->dis;
		um_vctovc(rvent->norm,plane);
		cutn = rvent->cutn;
	}
/*
.....Prepare for distance calculations
*/
	pt1[0] = orig[0] + norm[0]*dis1;
	pt1[1] = orig[1] + norm[1]*dis1;
	pt1[2] = orig[2] + norm[2]*dis1;
	if (LW_mach_mode != LW_VISICUT) plane[3] = um_dot(plane,pt1);
/*
.....Highlight selections
*/
	if (LW_mach_mode == LW_VISICUT)
	{
		Setype[0] = LI_ENTITY_TYPE_FACE;
		ul_ipv_highlight_entity(&Soldm[0],Sentity[0],LI_ENTITY_TYPE_FACE,
			LW_material[LW_highlight_color],&Sedge[0],Secolor[0]);
		Snoldm = 1;
	}
	for(;;)
	{
/*
.....Destroy 2nd entity pick list
*/
		if (entlist2 != 0) LiViPickedEntityListDestroy(entlist2);
		entlist2 = 0;
/*
.....Pick second entity
*/
		event = ul_ipv_pick_entity("Select 2nd entity for distance",
			LI_ENTITY_TYPE_FACE,&entlist2,&rvent1,orig2,norm2,&button);
/*
.....Exit on middle or right mouse button
*/
		if ((event == 3 && button == 3) || Sfrm == 0) retstat = UU_FAILURE;
		if (event != 3 || button != 1 || Sfrm == 0) break;
/*
.....No entities selected
*/
		if (!rvent1.picked)
		{
			ud_wrerr("Failed to select the 2nd entity");
			continue;
		}
/*
.....Unhighlight previous selection
*/
		if (LW_mach_mode == LW_VISICUT)
		{
			if (Snoldm == 2)
				ul_ipv_highlight_entity(&oldm,Sentity[1],LI_ENTITY_TYPE_FACE,
					Soldm[1],&Sedge[1],Secolor[1]);
/*
.....Get second entity
*/
			entpick = LiViPickedEntityListGetFirst(entlist2);
			if (entpick == 0)
			{
				ud_wrerr("Failed to select the 2nd entity");
				continue;
			}
			status = LiViPickedEntityExpand(entpick,&Sentity[1],&type,&dis2);
			if (status != 0)
			{
				ud_wrerr("Failed to select the 2nd entity");
				continue;
			}
			LiBrFaceGetPlaneEquation(Sentity[1],plane2);
		}
		else
		{
			dis2 = rvent1.dis;
			um_vctovc(rvent1.norm,plane2);
			cutn = rvent1.cutn;
		}
/*
.....Calculate distance
*/
		pt2[0] = orig2[0] + norm2[0]*dis2;
		pt2[1] = orig2[1] + norm2[1]*dis2;
		pt2[2] = orig2[2] + norm2[2]*dis2;
		if (LW_mach_mode != LW_VISICUT) plane2[3] = um_dot(plane2,pt2);
		dis1 = um_dcccc(pt1,pt2);
		disx = fabs(pt1[0]-pt2[0]);
		disy = fabs(pt1[1]-pt2[1]);
		disz = fabs(pt1[2]-pt2[2]);
/*
.....Calculate plane distance if parallel
*/
		dis2 = 0.;
		if (um_vcparall(plane,plane2))
		{
			co = plane[0]*plane2[0] + plane[1]*plane2[1] + plane[2]*plane2[2];
			if (co <= 0.) plane2[3] = -plane2[3];
			dis2 = fabs(plane[3]-plane2[3]);
		}
/*
.....Highlight selection
*/
		if (LW_mach_mode == LW_VISICUT)
		{
			Setype[1] = LI_ENTITY_TYPE_FACE;
			ul_ipv_highlight_entity(&Soldm[1],Sentity[1],LI_ENTITY_TYPE_FACE,
				LW_material[LW_highlight_color],&Sedge[1],Secolor[1]);
			Snoldm = 2;
		}
/*
.....Display the distance(s)
*/
		ul_ipv_init_list(&Sstat_list,5);
		UM_len_inttoext(dis1,dis1);
		sprintf(sbuf,"Point Distance = %f %s",dis1,Subuf);
		ul_ipv_put_list(&Sstat_list,sbuf);
		UM_len_inttoext(disx,disx);
		sprintf(sbuf,"X Distance = %f %s",disx,Subuf);
		ul_ipv_put_list(&Sstat_list,sbuf);
		UM_len_inttoext(disy,disy);
		sprintf(sbuf,"Y Distance = %f %s",disy,Subuf);
		ul_ipv_put_list(&Sstat_list,sbuf);
		UM_len_inttoext(disz,disz);
		sprintf(sbuf,"Z Distance = %f %s",disz,Subuf);
		ul_ipv_put_list(&Sstat_list,sbuf);
		if (dis2 != 0.)
		{
			UM_len_inttoext(dis2,dis2);
			sprintf(sbuf,"Planar Distance = %f %s",dis2,Subuf);
			ul_ipv_put_list(&Sstat_list,sbuf);
		}
		if (Ssgl) ud_dspfrm_vis(Sfrm);
		ud_dispfrm_update_answer(Sfrm,FSTA,(int *)&Sstat_list);
/*
.....Show the source command for this face
*/
		if (Ssrc)
		{
			found = ul_ipv_find_mdata(Sentity[1],&cutn,&mattr,&mdata,UU_FALSE);
			if (!found)
			{
				ul_ipv_init_list(&Ssrc_list,1);
				ul_ipv_put_list(&Ssrc_list,"Attributes are not stored with this entity.");
				ud_dispfrm_update_answer(Sfrm,FSRC,(int *)&Ssrc_list);
			}
			else
				S_fill_lists(mdata);
		}
		if (Ssgl) break;
	}
	goto done;
/*
.....Did not select anything
*/
failed:;
	ul_ipv_put_list(&Sstat_list,"No entities selected.");
	if (Ssrc)
		S_fill_lists(UU_NULL);
	goto done;
/*
.....Only one entity selected
*/
one_ent:;
	sprintf(sbuf,"Could not calculate distance");
	ul_ipv_put_list(&Sstat_list,sbuf);
	if (Ssrc)
		S_fill_lists(UU_NULL);
	goto done;
/*
.....End of routine
*/
done:;
	return(retstat);
}

/*********************************************************************
**    S_FUNCTION     :  S_volume(entlist,rvent,orig,norm,dir)
**       Displays the volume of a solid primitive.
**    PARAMETERS
**       INPUT  :
**          entlist  List of entities picked (Visicut).
**          rvent    Picked entity (RapidCut).
**          orig     World coordinate of user selection.
**          norm     Normal vector of selection.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_volume(entlist,rvent,orig,norm)
LtPickedEntityList entlist;
LW_rv_picked_entity *rvent;
LtPoint orig;
LtVector norm;
{
	int status;
	char sbuf[82];
	LtDouble vol,a,pow();
	LtPickedEntity entpick;
	LtEntityType type;
/*
.....Visicut
*/
	if (LW_mach_mode == LW_VISICUT)
	{
/*
.....Calculate user selection
*/
		entpick = LiViPickedEntityListGetFirst(entlist);
		if (entpick == 0) goto failed;
		status = LiViPickedEntityExpand(entpick,&Sentity[0],&type,&vol);
		if (status != 0) goto failed;
/*
.....Get solid volume
*/
		Sprim[0] = LiViEntityGetSessionPrim(Sentity[0],LI_ENTITY_TYPE_FACE);
		if (Sprim[0] == 0) goto failed;
		if (LiViSolidGetVolume(Sprim[0],&vol) != 0) goto no_vol;
/*
.....Highlight selections
*/
		Setype[0] = LI_ENTITY_TYPE_SOLID;
		ul_ipv_highlight_entity(&Soldm[0],Sprim[0],LI_ENTITY_TYPE_SOLID,
			LW_material[LW_highlight_color],&Sedge[0],Secolor[0]);
		Snoldm = 1;
	}
/*
.....Rapidcut
*/
	else
	{
		if (LiRvSolidGetVolume(LW_stock_first[0]->stock,&vol) != 0) goto no_vol;
	}
/*
.....Display the volume
*/
	a = 1.;
	UM_len_inttoext(a,a);
	if (a != 1.) vol = vol * pow(a,3);
	sprintf(sbuf,"Volume = %f cubic %s",vol,Subuf);
	ul_ipv_put_list(&Sstat_list,sbuf);
/*
.....No source to show for solids
*/
	ul_ipv_init_list(&Ssrc_list,1);
	ul_ipv_put_list(&Ssrc_list,"Attributes are not stored with this entity.");
	ud_dispfrm_update_answer(Sfrm,FSRC,(int *)&Ssrc_list);
	goto done;
/*
.....Did not select anything
*/
failed:;
	ul_ipv_put_list(&Sstat_list,"No entities selected.");
	if (Ssrc)
		S_fill_lists(UU_NULL);
	goto done;
/*
.....Could not calculate solid volume
*/
no_vol:;
	sprintf(sbuf,"Could not calculate solid volume.");
	ul_ipv_put_list(&Sstat_list,sbuf);
	if (Ssrc)
		S_fill_lists(UU_NULL);
	goto done;
/*
.....End of routine
*/
done:;
	return;
}

/*********************************************************************
**    S_FUNCTION     :  S_2dcalc(pts,ibeg,npts,inc,bpt,nbpts,cyl,kst,ken)
**       Determines if an array of points or a portion of the array
**       contains a valid circle.
**    PARAMETERS
**       INPUT  :
**          pts      Array of points.
**          ibeg     Starting point in array for analyzation.
**          npts     Ending point in array for analyzation.
**				inc      Index to closest point within 'pts' to selection
**                   point (nrpt).
**          bpt      Array of boundary points for selected face.
**          nbpts    Number of points in 'bpt'.
**          nrpt     Near point which must be included in the circle
**                   check.
**       OUTPUT :
**          cyl      Cylinder record.  cyl[7] = Arc length.
**          kst      Starting point of arc.
**          ken      Ending point of arc.
**    RETURNS      :
**          UU_TRUE if a circle is present.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_2dcalc(pts,ibeg,npts,inc,bpt,nbpts,nrpt,cyl,kst,ken)
UM_coord *pts,*bpt;
int ibeg,npts,inc,nbpts,*kst,*ken;
UM_coord nrpt;
UU_REAL cyl[];
{
	int i,j,ist,ien,imid,stat;
	UU_LOGICAL status;
	UU_REAL dis,fabs();
	UM_coord temp;
	struct UM_circle_rec cir;
/*
.....Initialize routine
*/
/*printf("PNTVEC/%f,%f,%f, 0,0,1\n",nrpt[0],nrpt[1],nrpt[2]);
for (i=ibeg;i<npts;i++) printf("POINT/%f,%f,%f\n",pts[i][0],pts[i][1],pts[i][2]);
printf("Inc = %d\n",inc);*/
	ist = ibeg;
	ien = npts - 1;
	status = UU_FALSE;
/*
.....Attempt to find circle
.....in list of points
*/
	for (;;)
	{
		for (i=ist;i<ien-1;i++)
		{
/*
........Make sure selected point is within cylinder
*/
			if (i > inc) break;
/*
........Calculate middle point
*/
			imid = i + (ien-i+1)/2;
			if (imid <= ist || imid >= ien) break;
/*
........Build the circle
*/
			stat = um_c3_test_3pt(pts[i],pts[imid],pts[ien]);
			if (stat != UU_SUCCESS) continue;
			stat = um_c3_arc3pt(pts[i],pts[imid],pts[ien],&cir);
/*printf("\nPoints = %f,%f,%f   %f,%f,%f   %f,%f,%f\n",pts[i][0],pts[i][1],
pts[i][2],pts[imid][0],pts[imid][1],pts[imid][2],pts[ien][0],pts[ien][1],
pts[ien][2]);*/
			if (stat != UU_SUCCESS || cir.radius > 10.e+8 ||
				cir.radius <= LW_geom_toler) continue;
/*printf("Circle = %g,%g,%g, %g\n",cir.center[0],cir.center[1],cir.center[2],
cir.radius);*/
/*
........Check selected point to cylinder
*/
			dis = um_dcccc(nrpt,cir.center);
			dis = fabs(dis-cir.radius);
/*printf("Nrpt Dis = %f\n",dis);*/
			if (dis > LW_geom_toler) continue;
/*
........Check face boundary points to cylinder
*/
			um_vctovc(cir.center,cyl);
			um_vctovc(cir.nvec,&cyl[3]);
			cyl[6] = cir.radius;
			cyl[7] = cir.radius * fabs(cir.dang);
			for (j=0;j<nbpts;j++)
			{
				stat = um_on_cylinder(cyl,pts[i],pts[imid],pts[ien],bpt[j],
					LW_geom_toler);
/*printf("On Cylinder = %d\n",stat);*/
				if (!stat) break;
			}
			if (j < nbpts) continue;
/*
........Check slice points to cylinder
*/
			for (j=i;j<ien;j++)
			{
				dis = um_dcccc(pts[j],cir.center);
				dis = fabs(dis-cir.radius);
/*printf("Dis = %f,%f,%f   %g\n",pts[j][0],pts[j][1],pts[j][2],dis);*/
				if (dis > LW_geom_toler) break;
				um_middlept(pts[j],pts[j+1],temp);
				dis = um_dcccc(temp,cir.center);
				dis = fabs(dis-cir.radius);
/*printf("Dis2 = %f,%f,%f   %g\n",temp[0],temp[1],temp[2],dis);*/
				if (dis > LW_geom_toler) break;
			}
/*
........Found our circle
*/
			if (j >= ien)
			{
				*kst = i;
				*ken = ien;
				status = UU_TRUE;
				goto done;
			}
		}
		ien = ien - 1;
		if (ien < imid || ist+2 > ien) break;
		if (ien < inc) break;
	}
/*
....End of routine
*/
done:
	return(status);
}
			
/*********************************************************************
**    E_FUNCTION     : ul_ipv_measure()
**       Processes the NCLIPV Measurement form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_measure()
{
/*
.....Set up form fields
*/
	static char traverse[]     = {1,1,1,1, 1,1,1, 1,1,1, 1};
	static UD_METHOD methods[] = {OnType,OnSelect,OnToler,UU_NULL,UU_NULL,
		OnSource,OnStack,OnShow,UU_NULL,OnEdit,OnClose};
	static char called[] = {6,6,6,6, 6,6,6, 6,6,6, 6};
	static char display[] = {1,1,1,1, 1,1,1, 1,1,1, 1};
	static int *ans[] = {&Stype,UU_NULL,(int *)&Stoler,&Ssgl,
		(int *)&Sstat_list,(int *)&Ssrc_list,(int *)&Scall_list,&Ssrc,&Sreset,
		UU_NULL};
/*
.....Make sure NCLIPV is active
*/
	if (!LW_active)
	{
		ud_wrerr("NCLIPV must be active for measurements to be taken.");
		goto done;
	}
	if (Sfrm != 0) goto done;
/*
.....Initialize routine
*/
	Sisn = 0;
	getnln(&Slast_isn);

	Sselect = UU_FALSE;
	Sfrm = -1;

	UM_len_inttoext(LW_geom_toler,Stoler);

	Sstat_list.num_item = 0;
	Sstat_list.item = UU_NULL;
	Sstat_list.answer = UU_NULL;

	Ssrc_list.num_item = 0;
	Ssrc_list.item = UU_NULL;
	Ssrc_list.answer = UU_NULL;

	Scall_list.num_item = 0;
	Scall_list.item = UU_NULL;
	Scall_list.answer = UU_NULL;
/*
.....Set traverse fields
*/
	traverse[FTOL] = 1;
	if (Stype != GEOM_TOG) traverse[FTOL] = 0;
	traverse[FSRC] = Ssrc;
	traverse[FCAL] = Ssrc;
	if (LW_nclipv == LW_STANDALONE)
	{
		display[FEDT] = display[FRES] = 0;
		traverse[FEDT] = traverse[FRES] = 0;
	}
	else
	{
		traverse[FEDT] = Ssrc;
		traverse[FRES] = Ssrc;
	}
/*
.....Display the Form
*/
	Sfrm = ud_form_display1("ipvmeas.frm", ans, ans, methods, called, display,
		traverse);
	if (Sfrm == -1) goto err1;
	goto done;
/*
.....Could not create the form
*/
err1:;
	ud_wrerr("Could not create measurement form.");
done:;
	return;
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_measure_close()
**       Closes the NCLIPV Measurement form if it is not already closed.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_measure_close()
{
	if (Sfrm != 0) OnClose();
}
