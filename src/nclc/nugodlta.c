/********************************************************************* 
**    NAME         :  nugodlta.c
**       CONTAINS:
**		             nclu_godlta()
**
**    COPYRIGHT 2015 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nugodlta.c , 26.3
**    DATE AND TIME OF LAST MODIFICATION
**       09/20/18 , 11:40:09
*********************************************************************/
#include <string.h>
#include "class.h"
#include "dselmask.h"
#include "nclmplay.h"
#include "mdrel.h"
#include "mgeom.h"
#include "mdpick.h"
#include "mdcpln.h"
#include "mdunits.h"
#include "mxxx.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "uhep.h"
#include "ulist.h"

extern int NCL_preview_mot;
enum
{
/*
.....Goto fields
*/
	FGDLT,FGTXT,
	FGFD1,FGFD2,FGFD3,FGFD4,
	FGTHK,FGAUT,
/*
.....Color Fields
*/
	FCOLA, FCOLB, FCOLC,
/*
.....Action item fields
*/
	FAPV, FAPY, FARS, FAPB, FAVE, FAGE, FAVW, FVIDEO
};

#define HIDE_KEY 0
#define CONTOUR_KEY 1
/*
.....Global variables
*/
extern UD_METHOD UD_initfrm_intry;
extern int NAUTIPV;
/*
.....Section definitions
*/
enum {Godlta, Colors};
static char *Smode[]={"Godlta", "Colors"};
static int Sred[3]={180,0,0}, Syellow[3]={180,148,0}, Sgreen[3]={0,180,0};
static int Sblack[3]={0,0,0};
/*
.....Main variables
*/
static UU_LOGICAL Sform_init = UU_FALSE, Sfedfl=UU_FALSE;
static UU_LOGICAL Sgo_chg = UU_FALSE;
static UN_motseg *Smptr=0,Smotatt;
static UN_mot_vpbox_struc Smvpbox[UV_NVPORTS];
static UU_LIST Skey_list;
static int Snkeys;
static UU_LOGICAL Skey_init;
static char Sav_valuestr[65];
/*
.....GODLTA variables
*/
static char Sgd[65],Tgd[65],Sdthk[65],Spthk[65],Scthk[65],Tpthk[65];
static int Sgtfmode,Tgtfmode;
static int Sgtfdav,Tgtfdav;
static int Sfdmod, Tfdmod;
static char Sgtfd[65],Tgtfd[65];
static NCLX_mot_feedrate Sgfedrat,Tgfedrat;
static int Tautopvw;
/*
.....Color variables
*/
static int Sgdcol,Tgdcol;
static int Tgeotog,Tgeocol;
static UU_LOGICAL Sgeo_redraw;
/*
.....Geometry variables
*/
static UM_sgeo Sgeo;
static UU_LOGICAL Sgeo_apply=UU_FALSE;
/*
.....Godlta callback routines
*/
static UD_FSTAT OnGdSel(),OnGdTxt(),OnGdTog(),OnCoTog();
static UD_FSTAT OnAction(),OnVideo();
/*
.....Local routines
*/
static void S_init_form(),S_save_form(),S_hilite_entity(),S_unhilite_entity();
static UD_FSTAT S_enter_form();
static void S_init_traverse(),S_form_invis(),S_form_vis(),S_init_geo();
static void S_unhilite_all();
static void S_update_answers(),S_section_changed(),S_push_geo();
static void S_add_key(),S_create_key_list();
static int S_select_geo(),S_build_command();

static int SfrmPlay=0;
static int Serrflg,Spreviewflg,Schgmade;

static int *Sanswers[] = {
	UU_NULL,(int *)&Tgd,
	&Tgtfmode,(int *)&Tgtfd,&Tgtfdav,&Tfdmod,
	(int *)Tpthk,&Tautopvw,

	&Tgdcol,&Tgeotog,&Tgeocol,

	UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL};
/*********************************************************************
**    I_FUNCTION     : S_enable_buttons()
**			Determines which fields should be enabled and which buttons
**       marked as necessary.
**			since the code in function already handle some, we just add more here
**    PARAMETERS   
**       INPUT  :
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_enable_buttons()
{
	int nc;
	UU_REAL rval;
	UU_LOGICAL ifl;
	UU_LOGICAL sec1, sec2;
	sec1 = sec2 = UU_TRUE;
	if (Tgtfmode != 0)
	{
		ud_dispfrm_set_attribs(0, FGFD2, UM_BLACK, UM_WHITE);
	}
	else 
	{
		nc = (int)strlen(Tgtfd);
		ul_strip_blanks(Tgtfd, &nc);
		if (nc<=0)
		{
			ud_dispfrm_set_attribs(0,FGFD2,UM_WHITE,UM_RED);
			sec1 = UU_FALSE;
		}
		else
		{
			rval = atof(Tgtfd);
			if (rval<=0)
				sec1 =  UU_FALSE;
			if (sec1) 
			{
				ud_dispfrm_set_attribs(0, FGFD2, UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(0, FGFD2, UM_WHITE,UM_RED);
			}
		}
	}
	if (strlen(Sgeo.label)<=0)
		sec2 = UU_FALSE;
	if (sec1&&sec2)
	{
		if (Sgo_chg==0)
		{
			S_section_changed(Smode[Godlta],Sblack,UU_TRUE,UU_TRUE);
		}
		else
		{	
			S_section_changed(Smode[Godlta],Sgreen,UU_TRUE,UU_TRUE);
		}
	}
	else
	{
		S_section_changed(Smode[Godlta],Sred,UU_TRUE,UU_TRUE);
	}
/*
.....Set Action Buttons
*/
	ifl = sec1&&sec2;
	ud_frm_enable_ok(ifl);
	ud_set_traverse_mask(FAPV,ifl);
	ud_set_traverse_mask(FAPY,ifl);
	ud_set_traverse_mask(FARS,ifl);
	ud_set_traverse_mask(FAGE,ifl);
}

/*********************************************************************
**    E_FUNCTION     : nclu_godlta()
**       Controlling routine for the Go Delta form.
**       GODLTA statements are output from this form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_godlta()
{
	int status,flag;
	UU_LOGICAL cmdreject;
	UD_METHOD save_entry;
/*
.....Set up form fields
*/
	static char traverse[] = {1,1, 1,1,1,1, 1,1,
		1,1,1,
		1,1,1,1,1,1,1,1};
	static char display[] = {1,1, 1,1,1,1, 1,1,
		1,1,1,
		1,1,1,1,1,1,1,1};
	static char called[] = {6,6, 6,6,6,6, 6,6,
		6,6,6,
		6,6,6,6,6,6,6,6};

	static UD_METHOD methods[] = {OnGdSel,OnGdTxt,
		OnGdTog,OnGdTxt,OnGdTog,OnGdTog,
		OnGdTxt,OnGdTog,

		OnCoTog,OnCoTog,OnCoTog,

		OnAction,OnAction,OnAction,OnAction,OnAction,OnAction,OnAction,OnVideo};
/*
.....Get the current feed rate and thick
*/
	NclxMotGetFeedrate(&Sgfedrat);
	Tgfedrat = Sgfedrat;
/*
.....Initialize form answers
*/
	S_init_form();
	Serrflg = 0;
	Spreviewflg = 0;
	Schgmade = UU_FALSE;
	save_entry = UD_initfrm_intry;
	SfrmPlay = 0;
	Sfedfl = UU_FALSE;
	Skey_init = UU_FALSE;
	Snkeys = 0;
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0)	goto done;
/*
.....Initialize form fields
*/
repeat:;
	NCL_preview_mot = 1;
	S_init_traverse(display,traverse);
	UD_initfrm_intry = S_enter_form;
/*
.....Get the Form input
*/
	Sgo_chg = 0;
	status = ud_form1("ngodlta.frm",Sanswers,Sanswers,methods,called,display,
		traverse);
/*
.....Erase last previewed motion
*/
	if (Smptr != UU_NULL && (!Spreviewflg || Schgmade || status == -1))
		ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
	else if (Spreviewflg)
		ncl_step_displayed(2,0,UU_NULL);
	Smptr = UU_NULL;
/*
.....Free the motion settings created during preview if a change has been
.....made and ok was pressed or if cancel was pressed
*/
	if (Spreviewflg)
	{
		if (!Schgmade && status != -1 && Serrflg == 0) flag = UU_TRUE;
		else flag = UU_FALSE;
		moinfr(&flag);
	}
	if (status == -1) goto done;
/*
.....Geometry must be selected
*/
	if (strlen(Tgd) == 0 && !Sgeo_apply)
	{
		ud_wrerr("You must select a tool position.");
		goto repeat;
	}
/*
.....Save the form settings
*/
	S_save_form();
/*
.....Output the command
*/
	status = S_build_command(UU_TRUE);
	if (status != UU_SUCCESS) goto repeat;
/*
.....End of routine
*/
done:;
	if (Sgeo_redraw)
		ncl_hide_geo(-1,-1,-1,-1,UU_TRUE,UU_NULL,0);
	if (Skey_init)
		uu_list_free(&Skey_list);
	Skey_init = UU_FALSE;
	S_unhilite_all();
	NCL_preview_mot = 0;
	UD_UNMARK(cmdreject);

	return;
}

/*********************************************************************
**    I_FUNCTION     : OnGdSel(fieldno,val,stat)
**       Method called when a Select button is pressed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnGdSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,status,prmpt,fno;
	unsigned *mask;
	UU_LOGICAL flag=0,done;
	char last_geo[65] = "";
/*
.....Select GODLTA geometry
*/
	switch (*fieldno)
	{
	case FGDLT:
		strcpy(last_geo, Sgeo.label);
		S_unhilite_entity(&Sgeo);
		S_form_invis();
		mask = UD_godlta;
		prmpt = 717;
		status = S_select_geo(&Sgeo,UU_NULL,mask,2,prmpt,Tgdcol,0,FGTXT,Tgd);
		if ((strlen(Sgeo.label) != 0) && (Tautopvw || (stricmp (last_geo, Sgeo.label)==0)))
		{
			if (Smptr != UU_NULL) ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
			Smptr = UU_NULL;
			if (Spreviewflg) moinfr(&flag);
			Spreviewflg = UU_FALSE;
			Schgmade = UU_FALSE;
			Serrflg = 0;
			if (Tautopvw && (stricmp (last_geo, Sgeo.label)!=0))
			{
				fno = FAPV;
				OnAction(&fno,val,stat);
			}
/*
........Enable the Action buttons if it is different select geom
*/
			if ((strlen(Sgeo.label) != 0)&&(stricmp (last_geo, Sgeo.label)!=0))
			{
				ud_set_traverse_mask(FAPV,1);
				ud_set_traverse_mask(FAPY,1);
				ud_set_traverse_mask(FAGE,1);
				S_section_changed(Smode[Godlta],Sgreen,UU_TRUE,UU_TRUE);
				ud_frm_enable_ok(UU_TRUE);
				ud_dispfrm_set_attribs(0,FGDLT,UM_BLACK,Tgdcol);
			}
			else
			{
				S_unhilite_entity(&Sgeo);
				Sgeo.key = 0;
				Sgeo.color = -1;
				Sgeo.label[0] = '\0';
				nclu_disp_cut();
				Tgd[0] = '\0';			
				ud_update_answer(FGTXT,(int *)Tgd);
				ud_set_traverse_mask(FAPV,0);
				ud_set_traverse_mask(FAPY,0);
				ud_set_traverse_mask(FAGE,0);
				ud_set_traverse_mask(FAPB,0);
				ud_set_traverse_mask(FAVE,0);
				S_section_changed(Smode[Godlta],Sred,UU_TRUE,UU_FALSE);
				ud_frm_enable_ok(UU_FALSE);
				ud_dispfrm_set_attribs(0,FGDLT,UM_WHITE,UM_RED);
			}
		}
		S_form_vis();
		break;
	}
/*
.....Check for change since preview
*/
	if (status == UU_SUCCESS) Schgmade = 1;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnGdTog(fieldno,val,stat)
**       Method called when a toggle field is changed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnGdTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	char cfed[16][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	UM_sgeo *geo;
	char valuestr[65];
/*
.....Process Levels toggle field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FGFD1:
		if (Sgtfmode != Tgtfmode) 
		{
			Schgmade = 1;
			Sgo_chg = 1;
		}
/*
......Tgtfmode, now only "current" and "Rapid"
*/
		if (Tgtfmode == 0)
		{
			ud_set_traverse_mask(FGFD2,UU_TRUE);
			ud_set_traverse_mask(FGFD3,UU_TRUE);
			strcpy(Tgtfd, Sav_valuestr);
			ud_update_answer(FGFD2,(int *)Tgtfd);
			ud_set_traverse_mask(FGFD4,Tgtfdav);
		}
		else
		{
/*
.....RAPID
*/
			Sfedfl = UU_FALSE;
			Tgtfdav = 0; ud_update_answer(FGFD3,&Tgtfdav);
			ud_set_traverse_mask(FGFD2,UU_FALSE);
			ud_set_traverse_mask(FGFD3,UU_FALSE);
			ud_set_traverse_mask(FGFD4,UU_FALSE);
/*
.....update text field to empty
*/
			valuestr[0] = '\0';
			strcpy(Sav_valuestr, Tgtfd);
			ud_update_answer(FGFD2,(int *)valuestr);			
		}
		break;
	case FGFD3:
		ud_set_traverse_mask(FGFD4,Tgtfdav);
		Schgmade = 1;
		Sgo_chg = 1;
		if (!(Tgtfdav))
			break;
	case FGFD4:
		Tgfedrat.base_feedrate = atof(Tgtfd);
		nclu_fedrat_form(&Tgfedrat, &Sgfedrat,cfed,UU_FALSE,Sfedfl, FGFD2);
		Sfedfl = UU_TRUE;
		break;
	}
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnGdTxt(fieldno,val,stat)
**       Method called when a text field is changed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnGdTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Point label text field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FGTXT:
		S_unhilite_entity(&Sgeo);
		ul_to_upper(Tgd);
		if (strcmp(Sgd,Tgd) != 0)
		{
			Schgmade = 1;
			Sgo_chg = 1;
			S_section_changed(Smode[Godlta],Sgreen,UU_TRUE,UU_TRUE);
			S_init_geo(&Sgeo,Tgd,Tgdcol);
/*
........Enable the Action buttons
*/
			if (strlen(Tgd) != 0)
			{
				ud_set_traverse_mask(FAPV,1);
				ud_set_traverse_mask(FAPY,1);
				ud_set_traverse_mask(FAGE,1);
				S_section_changed(Smode[Godlta],Sgreen,UU_TRUE,UU_TRUE);
				ud_frm_enable_ok(UU_TRUE);
				ud_dispfrm_set_attribs(0,FGDLT,UM_BLACK,Tgdcol);
			}
		}
		break;
	case FGTHK:
		ul_to_upper(Tpthk);
		if (strcmp(Spthk,Tpthk) != 0) 
		{
			Schgmade = 1;
			Sgo_chg = 1;
		}
		break;
	case FGFD3:
		ul_to_upper(Tgtfd);
		if (strcmp(Sgtfd,Tgtfd) != 0) 
		{
			Schgmade = 1;
			Sgo_chg = 1;
		}
		break;
	}
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnCoTog(fieldno,val,stat)
**       Method called when an Color form toggle field is changed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnCoTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int fno;
	UU_LOGICAL ifl1,ifl2;
/*
.....Process Color toggle field
*/
	switch (*fieldno)
	{
	case FCOLA:
		if (strlen(Tgd) != 0)
			ud_dispfrm_set_attribs(0,FGDLT,UM_BLACK,Tgdcol);
		S_hilite_entity(&Sgeo,Tgdcol);
		break;
	case FCOLB:
		Tgeotog = val->frmint[0];
	case FCOLC:
		if (Sgeo_redraw)
		{
			Sgeo_redraw = UU_FALSE;
			fno = FAGE;
			OnAction(&fno,val,stat);
		}
		break;
	}
	return(UD_FLDOK);
}
/*********************************************************************
**    I_FUNCTION     : OnVideo(fieldno,val,stat)
**       Method called when an Video button is pressed.
**		Video button will play default video file, but we can change the file
**		doing nothing now exepect play default file, but put here in case we need it later
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnVideo(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(UD_FLDOK);
}
/*********************************************************************
**    I_FUNCTION     : OnAction(fieldno,val,stat)
**       Method called when an Action button is pressed.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnAction(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status,i,flag= 0,n;

	switch (*fieldno)
	{
/*
.....Preview command
*/
	case FAPV:
		if (Spreviewflg) moinfr(&flag);
		Spreviewflg = UU_FALSE;
		Schgmade = UU_FALSE;
		Serrflg = 0;
/*
........Erase last previewed motion
*/
		if (Smptr != UU_NULL) ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
		Smptr = UU_NULL;
/*
........Save motion settings
*/
		moinfs();
		ncl_mark_mdisplay(&Smptr,&Smotatt,Smvpbox,UU_TRUE);
/*
........Output command
*/
		status = S_build_command(UU_FALSE);
/*
........Delete the temp cl file if no curve was selected
........since no cmd call was made
........The temp cl file needs to be deleted so that
........if another preview occurs or OK is pressed
........the pointers are at the right locations after the error from
........no curves being selected
*/
		if (status != UU_SUCCESS) moinfr(&flag);
/*
........A curve was selected so keep the temp clfile
........for now and set the preview flag
*/
		else
		{
			Spreviewflg = UU_TRUE;
			ud_set_traverse_mask(FAPB,1);
			ud_set_traverse_mask(FAVE,NAUTIPV);
		}
		break;
/*
.....APPLY
.....Geometry must be selected
*/
	case FAPY:
/*
........Output command
*/
/*		if (Smptr != UU_NULL) ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
		Smptr = UU_NULL;
		if (Spreviewflg) moinfr(&flag);
		Spreviewflg = UU_FALSE;
*/
		if (Spreviewflg)
		{
			if (!Schgmade && Serrflg == 0) flag = UU_TRUE;
			else flag = UU_FALSE;
			moinfr(&flag);
		}
		status = S_build_command(UU_TRUE);
		Spreviewflg = UU_FALSE;
		Smptr = UU_NULL;
		if (status != UU_SUCCESS)
			break;
		S_unhilite_entity(&Sgeo);
/*
.....reset mode = 0 (SAME) if the mode is the value after apply
*/
		if (Tgtfmode == 2)
		{
			Tgtfmode = 0;
			ud_update_answer(FGFD1,&Tgtfmode);
			Tgtfdav = 0; 
			ud_update_answer(FGFD3,&Tgtfdav);
			ud_set_traverse_mask(FGFD2,UU_FALSE);
			ud_set_traverse_mask(FGFD3,UU_FALSE);
			ud_set_traverse_mask(FGFD4,UU_FALSE);
		}
		Sgeo_apply = UU_TRUE;
		for (i=FAPV;i<FAVW;i++) ud_set_traverse_mask(i,0);
		i = -1;
		Tgd[0] = '\0';			
		ud_update_answer(FGTXT,(int *)Tgd);
		S_enter_form(&i, val, stat);
		break;
/*
.....Reset form
.....Unselect all geometry
.....Erase Preview motion
*/
	case FARS:
		if (Sgeo_redraw)
			ncl_hide_geo(-1,-1,-1,-1,UU_TRUE,UU_NULL,0);
		S_unhilite_all();
		if (Smptr != UU_NULL) ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
		Smptr = UU_NULL;
		if (Spreviewflg) moinfr(&flag);
		S_init_form();
		S_enter_form(fieldno,val,stat);
		for (i=FAPV;i<FAVW;i++) ud_set_traverse_mask(i,0);
		S_update_answers();
		Schgmade = Spreviewflg = UU_FALSE;
		Sfedfl = UU_FALSE;
		break;
/*
.....Playback motion
*/
	case FAPB:
		SfrmPlay = nclu_playback_preview();
/*
..... Added to force mainform displaying back if encountered error
*/
		if (SfrmPlay>0)
		{
			S_form_invis();
			uw_ntdspfrm_vis(SfrmPlay);
		}
		break;
/*
.....Verify motion
*/
	case FAVE:
		S_create_key_list(CONTOUR_KEY);
		SfrmPlay = ul_ipv_playback_preview(&Skey_list,Snkeys);
/*
..... Added to force mainform displaying back if encountered error
*/
		if (SfrmPlay>0)
		{
			S_form_invis();
			uw_ntdspfrm_vis(SfrmPlay);
		}
		break;
/*
.....Toggle geometry display
*/
	case FAGE:
		if (!Sgeo_redraw)
		{
			S_create_key_list(HIDE_KEY);
			ncl_hide_geo(Tgeotog,Tgeocol,UM_DASHSPC_LINE,20,UU_TRUE,UU_NULL,0);
			Sgeo_redraw = UU_TRUE;
		}
		else
		{
			ncl_hide_geo(-1,-1,-1,-1,UU_TRUE,UU_NULL,0);
			Sgeo_redraw = UU_FALSE;
		}
		break;
/*
.....Enter viewing mode
*/
	case FAVW:
		S_form_invis();
		uz_dyn_mouse();
		S_form_vis();
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_init_traverse(display,traverse)
**       Initializes the display and traverse fields of the Profile
**       Machining form.
**    PARAMETERS
**       INPUT  :
**          display     Field display settings.
**          traverse    Field traverse settings.
**       OUTPUT :
**          display     Updated field display settings.
**          traverse    Updated field traverse settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_init_traverse(display,traverse)
char *display,*traverse;
{
	int i;
/*
.....Initialize Fedrat fields
*/
/*
.....Now Tgtfmode only "Current" and "Rapid" now, per Ken
*/
/*	if (Tgtfmode < 2)
	{
		traverse[FGFD2] = traverse[FGFD3] = traverse[FGFD4] = 0;
	}
	else
*/
	{
		traverse[FGFD2] = traverse[FGFD3] = 1;
		traverse[FGFD4] = Tgtfdav;
	}
/*
.....Action item fields
*/
	for (i=FAPV;i<FAVW;i++) traverse[i] = 0;
}

/*********************************************************************
**    I_FUNCTION     : S_init_form()
**       Initializes the form variables.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_init_form()
{
	int i;
	NCLX_mot_thick thick;
	UU_REAL hgt;
	int chc;
/*
.....Initialize the Form settings
*/
	if (!Sform_init)
	{
		Sgdcol = NCLX_SEA_GREEN;
		Sgtfmode = 0;
		Sgtfdav = 0;
	}
	nclc_get_ctr_hgt(&chc, &hgt);
	Sgtfdav = Sgfedrat.accel_flag || Sgfedrat.slowdown_flag||(hgt!=0.0);
	if (Sgtfmode == 2)
	{
		Sgtfmode = 0;
		Sgtfdav = 0; 
	}
	ncl_sprintf(Sgtfd,&Sgfedrat.base_feedrate,1);
	Sfdmod = Sgfedrat.mode-1;
	Sgd[0] = '\0';
	NclxMotGetThick(&thick);
	ncl_sprintf(Sdthk,&thick.ds,1);
	ncl_sprintf(Spthk,&thick.ps,1);
	ncl_sprintf(Scthk,&thick.cs,1);

	Tgdcol = Sgdcol;
	Tgtfmode = Sgtfmode;
	Tgtfdav = Sgtfdav;
	Tfdmod = Sfdmod;
	strcpy(Tgd,Sgd);
	strcpy(Tgtfd,Sgtfd);
	if (Tgtfmode!=0)
/*
.....rapid
*/
	{
		Tgtfd[0] = '\0';
	}
	strcpy(Sav_valuestr, Sgtfd);
	strcpy(Tpthk,Spthk);
/*
.....Initialize geometry color
*/
	Tgeotog = UN_unused_geo_flag;
	Tgeocol = UN_unused_geo_color;
	Sgeo_redraw = UU_FALSE;
	Tautopvw = UN_mot_auto_preview;
/*
.....Mark variables as being initialized
*/
	Sform_init = UU_TRUE;
/*
.....Initialize geomtry variables
*/
	Sgeo.key = 0;
	Sgeo.color = -1;
	Sgeo.label[0] = '\0';
	S_init_geo(&Sgeo,Tgd,Tgdcol);
/*
.....Erase the motion &
.....Display the current cutter position
*/
/*
......don't erase motion now
*/
/*	nclu_erase_motion(); */
	nclu_disp_cut();
}

/*********************************************************************
**    I_FUNCTION     : S_enter_form(fieldno,val,stat)
**       Method called when the Form is first displayed.
**    PARAMETERS
**       INPUT  :
**          fieldno  -1 if routine is called from a form field.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT S_enter_form(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,ifl;
/*
.....Set the Section colors
*/
	S_section_changed(Smode[Godlta],Sred,UU_TRUE,UU_FALSE);
	ud_frm_enable_ok(UU_FALSE);
/*
.....Set the field traversals
*/
	ud_set_traverse_mask(FGFD4,Tgtfdav);
/*
.....Set the mandatory fields to red
*/
	ud_dispfrm_set_attribs(0,FGDLT,UM_WHITE,UM_RED);
	S_enable_buttons();
	return(UD_FLDOK);
}
/*********************************************************************
**    I_FUNCTION     : S_save_form()
**       Saves the form variables.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_save_form()
{
	int i;
/*
.....Save the Godlta settings
*/
	Sgtfmode = Tgtfmode;
	Sgdcol = Tgdcol;
	UN_unused_geo_flag = Tgeotog;
	UN_unused_geo_color = Tgeocol;
	UN_mot_auto_preview = Tautopvw;
	nclu_save_preview_modals();
}

/*********************************************************************
**    I_FUNCTION     : S_form_invis()
**       Takes down all forms.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_form_invis()
{
	ud_form_invis();
}

/*********************************************************************
**    I_FUNCTION     : S_form_vis()
**       Redisplays all forms.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_form_vis()
{
	ud_form_vis();
}

/*********************************************************************
**    I_FUNCTION     :  S_select_geo(sfpt,sflst,mask,multi,prmno,color,frm,
**                                   fieldno,label)
**       Routine to select geometry for the form.  The associated
**       text field will be updated with the geometry label and
**       the geometry will be highlighted.
**    PARAMETERS
**       INPUT  :
**          mask     Picking mask.
**          multi    0 = Single selection, 1 = multiple selection,
**                   2 = Single selection (calling routine handles invisibling
**                   of form).
**          prmno    Prompt number to use while in pick mode.
**          color    Color to highlight the picked entity.
**                   -1 = Don't highlight entity.
**          frm      Form number that the field resides on. 0 = Main form.
**          fieldno  Text field to update with label of selected entity.
**                   -1 = No field to updated.
**          label    Text buffer that belongs to 'fieldno'.
**       OUTPUT :
**          sfpt     Pointer to selected geometry structure for single entity.
**          sflst    Pointer to selected geometry structure list when
**                   multiple entities can be picked (multi).
**    RETURNS      : UU_SUCCESS if an entity is picked.  UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_select_geo(sfpt,sflst,mask,multi,prmno,color,frm,fieldno,label)
UM_sgeo *sfpt;
UU_LIST *sflst;
unsigned int *mask;
UU_LOGICAL multi;
int prmno,color,fieldno,frm;
char *label;
{
	int numint,iret,nc;
	unsigned int *imask;
	UU_LOGICAL init;
	struct NCL_fixed_databag e;
	UM_PLOCREC pick;
	UU_LOGICAL cmdreject,doend;
	UM_int4 sfkey;
	UM_int2 primtyp;
/*
.....Take down form
*/
	iret = UU_SUCCESS;
	if (multi != 2) S_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK (cmdreject, UU_TRUE);
	if (cmdreject != 0) goto failed;
/*
.....Set the appropriate selection mask
*/
	imask = mask;
	if (imask == UD_ncl_pl) imask = UD_ncl_allsfpl;
/*
.....Unhighlight the previous selection
*/
	if (multi != 2)
	{
		if (sfpt != UU_NULL) S_unhilite_entity(sfpt);
	}
	else
	{
		sfpt->key = 0;
		sfpt->color = -1;
		sfpt->label[0] = '\0';
	}
/*
.....Get the next geometry selection
*/
repeat:;
	do
	{
		doend = UU_TRUE;
		ud_lgeo(UU_TRUE,imask);
/*
.....Single selection
*/
		if (multi != 1)
		{
			ua_dl_pldas(UD_DASPCKLOC,UA_NCL,prmno,&pick,1,&numint,1);
			if (numint == 0) goto failed;
			e.key = um_get_pickkey(&(pick.pent),1);
/*
........Screen location picked
*/
			if (e.key == 0)
				strcpy(label,pick.ploc.label);
/*
........Check for planar surface
*/
			else
			{
				if (ncl_retrieve_data_fixed(&e) != 0) goto done;
				if (mask == UD_ncl_pl && e.rel_num != NCL_PLN_REL)
				{
					sfkey = e.key;
					ncl_get_sf_primtyp (&sfkey,&primtyp);
					if (primtyp != NCLSF_PLANE)
					{
						ud_wrerr("Entity Picked is not Planar.");
						doend = UU_FALSE;
					}
				}
			}
		}
/*
.....Multiple selection
*/
		else
		{
			ud_ldas(UD_DASSELECT,UA_NCL,prmno,NULL,1,&numint,UD_NODEFAULT);
			if (numint == 0) goto failed;
			doend = UU_TRUE;
		}
	} while (!doend);
/*
.....Single selection
*/
	if (multi != 1)
	{
/*
........Store the entity's data
*/
		sfpt->key = e.key;
		sfpt->relnum = e.rel_num;
		if (sfpt->key != 0) ncl_get_label(&e,sfpt->label);
		else strcpy(sfpt->label,label);
/*
........Highlight the selected entity
*/
		S_hilite_entity(sfpt,color);
/*
........Update the text field with the entity label
*/
		if (fieldno != -1)
		{
			strcpy(label,sfpt->label);
			if (frm == 0) ud_update_answer(fieldno,(int *)label);
			else ud_dispfrm_update_answer(frm,fieldno,(int *)label);
		}
	}
/*
.....Multiple selection
*/
	else
	{
/*
........Store the entities' data
*/
		uu_list_init(sflst,sizeof(UM_sgeo),numint,10);
		init = UU_TRUE;
		while (ud_gnxt(init,UU_NULL,&e.key,1))
		{
			init = UU_FALSE;
			if (e.key != 0)
			{
				if (ncl_retrieve_data_fixed(&e) != 0) goto done;
				sfpt->key = e.key;
				sfpt->relnum = e.rel_num;
				sfpt->color = -1;
				ncl_get_label(&e,sfpt->label);
/*
........CADD Text is not allowed
*/
				nc = strlen(sfpt->label);
				ul_strip_blanks(sfpt->label,&nc);
/*
........Highlight the selected entity
*/
				if (nc != 0)
				{
					S_hilite_entity(sfpt,color);
/*
........Push the entity onto the stack
*/
					uu_list_push(sflst,sfpt);
				}
			}
		}
/*
........No entities selected
*/
		if (UU_LIST_LENGTH(sflst) == 0)
		{
			ud_wrerr("No CAM entities selected.");
			uu_list_free(sflst);
			goto repeat;
		}
/*
........Update the text field with the entity label
*/
		else if (fieldno != -1)
		{
			sfpt = (UM_sgeo *)UU_LIST_ARRAY(sflst);
			strcpy(label,sfpt->label);
			if (UU_LIST_LENGTH(sflst) > 1) strcat(label,",...");
			if (frm == 0) ud_update_answer(fieldno,(int *)label);
			else ud_dispfrm_update_answer(frm,fieldno,(int *)label);
		}
	}
/*
.....End of routine
.....Redisplay form
*/
done:;
	ud_unlimit();
	if (multi != 2) S_form_vis();
	UD_UNMARK(cmdreject);
	return(iret);
/*
.....User did not select anything
*/
failed:
	iret = UU_FAILURE;
	goto done;
}

/*********************************************************************
**    I_FUNCTION     :  S_init_geo(sfpt,label,color)
**       Initializes a picked geometry structure using a user entered
**       label.
**    PARAMETERS
**       INPUT  :
**          sfpt     Pointer to selected geometry structure.
**          label    Label of entity to store.
**          color    Color to highlight the picked entity.
**                   -1 = Don't highlight entity.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_init_geo (sfpt,label,color)
UM_sgeo *sfpt;
char *label;
int color;
{
	int nc;
	UM_f77_str f77_str;
/*
.....Same geometry as picked before
*/
	nc = strlen(label);
	ul_strip_blanks(label,&nc);
	if (strcmp(label,sfpt->label) == 0) return;
/*
.....Unhilite the previous entity
*/
	S_unhilite_entity(sfpt);
/*
.....Store and hilite the new entity
*/
	if (nc != 0)
	{
		UM_init_f77_str(f77_str,label,nc);
		getkey(UM_addr_of_f77_str(f77_str),&sfpt->key);
		strcpy(sfpt->label,label);
		S_hilite_entity(sfpt,color);
	}
}

/*********************************************************************
**    I_FUNCTION     :  S_push_geo(glst,geo,init)
**       Pushes a picked geometry structure onto the geometry list.
**    PARAMETERS
**       INPUT  :
**          glst     List to push geometry on.
**          geo      Geometry structure to push onto list.
**          init     UU_TRUE = list is already initialized.
**       OUTPUT :
**          init     UU_TRUE = list is initialized.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_push_geo(glst,geo,init)
UU_LIST *glst;
UM_sgeo *geo;
UU_LOGICAL *init;
{
/*
.....Initialize the list
*/
	if (!(*init)) uu_list_init(glst,sizeof(UM_sgeo),10,10);
	uu_list_push(glst,geo);
	*init = UU_TRUE;
}

/*********************************************************************
**    S_FUNCTION     :  S_hilite_entity(sfpt,color)
**       Highlights the selected geometry.
**    PARAMETERS
**       INPUT  :
**          sfpt     Pointer to selected geometry structure.
**          color    Color to highlight the picked entity.
**                   -1 = Don't highlight entity.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_hilite_entity (sfpt,color)
UM_sgeo *sfpt;
int color;
{
	struct NCL_fixed_databag e;
/*
.....Save geometry color
*/
	if (color != -1)
	{
		if (sfpt->key != 0)
		{
			e.key = sfpt->key;
			if (ncl_retrieve_data_fixed(&e) != 0) return;
			if (sfpt->color == -1) ncl_get_geo_color (e.key,&sfpt->color);
/*
.....Highlight entity
*/
			if (Sgeo_redraw) ncl_add_override_geo(e.key);
			ncl_update_geo_color(e.key,color,UU_TRUE);
			uc_display(&e);
			ug_wflush();
		}
	}
}


/*********************************************************************
**    S_FUNCTION     :  S_unhilite_all()
**       Unhighlights all geometry entities used in this form.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_unhilite_all()
{
	S_unhilite_entity(&Sgeo);
}

/*********************************************************************
**    S_FUNCTION     :  S_unhilite_entity(sfpt)
**       Unhighlights the selected geometry.
**    PARAMETERS
**       INPUT  :
**          sfpt     Pointer to selected geometry structure.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_unhilite_entity(sfpt)
UM_sgeo *sfpt;
{
	struct NCL_fixed_databag e;
/*
.....Save geometry color
*/
	if (sfpt->color != -1)
	{
		if (sfpt->key != 0)
		{
			e.key = sfpt->key;
			if (ncl_retrieve_data_fixed(&e) != 0) return;
			if (sfpt->color == -1) ncl_get_geo_color (e.key,&sfpt->color);
/*
.....Highlight entity
*/
			if (Sgeo_redraw) ncl_remove_override_geo(e.key);
			ncl_update_geo_color(e.key,sfpt->color,UU_FALSE);
			uc_display(&e);
			ug_wflush();
		}
	}
/*
.....Reset geometry structure
*/
	sfpt->key = 0;
	sfpt->color = -1;
	sfpt->label[0] = '\0';
}

/*********************************************************************
**    I_FUNCTION     : S_update_answers()
**			Updates all answers in the form.
**    PARAMETERS   
**       INPUT  :
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_update_answers()
{
	int i;
	for (i=0;i<sizeof(Sanswers)/sizeof(int *);i++)
		if (Sanswers[i] != UU_NULL) ud_update_answer(i,Sanswers[i]);
}

/*********************************************************************
**    I_FUNCTION     : S_section_changed(section,color,bold,reset)
**			Updates all answers in the form.
**    PARAMETERS   
**       INPUT  :
**          section   = Label of section that changed.
**          color     = RGB values for section label.
**          bold      = UU_TRUE = Bold section label.
**          reset     = UU_TRUE = Enable Reset button.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_section_changed(section,color,bold,reset)
char *section;
int color[];
UU_LOGICAL bold;
{
/*
.....Change color of section label
*/
	ud_form_section_color(0,section,color,bold);
/*
.....Enable Reset button
*/
	if (reset) ud_set_traverse_mask(FARS,UU_TRUE);
}

/*********************************************************************
**    I_FUNCTION     : S_create_key_list(which)
**			Creates the key lists of geometry used in this form.
**    PARAMETERS   
**       INPUT  :
**          which     = HIDE_KEY = Create Override geometry list.  Used
**                      for hiding geometry.
**                      CONTOUR_KEY = Create Contour Stock (local) list.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_create_key_list(which)
int which;
{
	int i,n;
	UM_sgeo *mgeo;
/*
.....Free local key list
*/
	if (which == CONTOUR_KEY && Skey_init)
	{
		UU_LIST_EMPTY(&Skey_list);
		Snkeys = 0;
	}
/*
.....Add all geometry to the list
*/
	S_add_key(which,Sgeo.key);
}

/*********************************************************************
**    I_FUNCTION     : S_add_key(which,key)
**			Adds a key to either the Override_Geo key list or Contour Stock
**       key list.
**    PARAMETERS   
**       INPUT  :
**          which     = HIDE_KEY = Create Override geometry list.  Used
**                      for hiding geometry.
**                      CONTOUR_KEY = Create Contour Stock (local) list.
**          key       = Key to add to the list.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_add_key(which,key)
UU_KEY_ID key;
int which;
{
	int rel;
	if (key != 0)
	{
		if (which == HIDE_KEY) ncl_add_override_geo(key);
		else
		{
			if (!Skey_init)
			{
        		uu_list_init(&Skey_list,sizeof(UU_KEY_ID),50,50);
				Skey_init = UU_TRUE;
			}
			ur_retrieve_data_relnum(key,&rel);
			if (uc_super_class(rel) == UC_SURFACE_CLASS)
			{
				uu_list_push(&Skey_list,&key);
				Snkeys++;
			}
		}
	}
}

/*********************************************************************
**    I_FUNCTION     : S_build_command(flag)
**			Builds and outputs the GODLTA command.
**    PARAMETERS   
**       INPUT  : flag    = UU_TRUE = output command to source file.
**			                   UU_FALSE = Preview command only.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS :
**			Saves and restores motion parameters when previewing the command.
**			The motion will remain displayed though.
**    WARNINGS     : none
*********************************************************************/
static int S_build_command(flag)
UU_LOGICAL flag;
{
	int i,j,nc,nline,slen,listlen,np,rel,stat;
	UM_int2 idx;
	UU_LOGICAL taxfl;
	UU_KEY_ID key;
	UU_REAL rval,pt[3];
	char buf[80],*labptr;
	char cfed[16][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	NCL_cmdbuf cmdbuf;
	UM_sgeo *geo;
	UM_f77_str outcmd;
	NCLX_mot_feedrate fedrat;
	if (flag)
		NCL_preview_mot = 0;
	else
		NCL_preview_mot = 1;
/*
.....Output Thick command
*/
	ncl_init_cmdbuf(&cmdbuf);
	if (strcmp(Tpthk,Spthk) != 0)
	{
		if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
		ncl_add_token(&cmdbuf,NCL_thick,NCL_nocomma);
		ncl_add_token(&cmdbuf,Tpthk,NCL_comma);
		ncl_add_token(&cmdbuf,Sdthk,NCL_comma);
		ncl_add_token(&cmdbuf,Scthk,NCL_comma);
		ncl_add_cmdbuf(&cmdbuf);
	}
	if (Tgtfdav && Sfedfl && flag)
	{
		nclu_fedrat_set_fedrat(&fedrat,cfed);
		nclu_fedrat_command(&cmdbuf,&fedrat,cfed,UU_TRUE,flag);
	}
	else
	{
		if (Tgtfmode == 1)
		{
/*
.....Output RAPID command
*/
			if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
			ncl_add_token(&cmdbuf,NCL_rapid,NCL_comma);
		    ncl_add_cmdbuf(&cmdbuf);
	    }
	    else 
		{
/*
..... Only output FEDRAT/value command if not Preview
*/			
	        if (strcmp(Tgtfd,Sgtfd) != 0 && flag)
            {					
		        ncl_add_token(&cmdbuf,NCL_fedrat,NCL_nocomma);
			    ncl_add_token(&cmdbuf,Tgtfd,NCL_comma);
		        ncl_add_cmdbuf(&cmdbuf);
                if (flag == UU_TRUE)
                {
                    ncl_set_cmdmode(UU_TRUE);
                    ncl_call(&cmdbuf);
				}
			}
		}
	}
/*
.....GODLTA command
*/
	if (!flag) ncl_add_token(&cmdbuf,"*",NCL_nocomma);
	ncl_add_token(&cmdbuf,NCL_godlta,NCL_nocomma);
	ncl_add_token(&cmdbuf,Tgd,NCL_comma);
	ncl_add_cmdbuf(&cmdbuf);
/*
........Tagged Feed rate

   if (Tgtfmode == 2 && !Tgtfdav && strcmp(Tgtfd,Sgtfd) != 0)
      ncl_add_token(&cmdbuf,Tgtfd,NCL_comma);

.....Call the command

	ncl_set_cmdmode(UU_TRUE);
	ncl_add_cmdbuf(&cmdbuf);
*/
/*
.....Insert the command at the current line, but do not execute
.......Only need to write the command if Preview was already pressed
.......and no changes were made since the command was already executed
*/
	if (Spreviewflg && !Schgmade) ncl_call_input(&cmdbuf,UU_TRUE);
/*
.....Execute command if a change has been made since the last preview or no
.....preview was created
*/
	else Serrflg = ncl_call(&cmdbuf);
	NCL_preview_mot = 1;
	return(UU_SUCCESS);
/*
.....Failed to create command
*/
failed:;
	NCL_preview_mot = 1;
	return(UU_FAILURE);
}

