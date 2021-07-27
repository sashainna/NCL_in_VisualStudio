/********************************************************************* 
**    NAME         :  nugoto.c
**       CONTAINS:
**		             nclu_goto()
**
**    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nugoto.c , 26.4
**    DATE AND TIME OF LAST MODIFICATION
**       09/20/18 , 11:41:14
*********************************************************************/
#include <string.h>
#include "class.h"
#include "dselmask.h"
#include "nclmplay.h"
#include "mdrel.h"
#include "mgeom.h"
#include "mdcpln.h"
#include "mdunits.h"
#include "mdpick.h"
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
	FGFRM,FGTYP,FGTXT,FGSEL, FGSL1,FGSL2,FGSL3, FGTA1,FGTA2,
	FGFD1,FGFD2,FGFD3,FGFD4,
	FGSGL,FGAUT, FGP1A,FGP1B, FGP2A,FGPS1,FGPS2,FGP2B,FGP2C,FGP2D, FGP3A,FGP3B,
	FGP4A,FGPS3,FGP4B,FGP4C,FGP4D,
/*
.....Color Fields
*/
	FCOLA, FCOLB, FCOLC, FCOLD,
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
static int Sselecting = 0;
static int Scurrent_pt = 0;
/*
.....Section definitions
*/
enum {Goto, Options, Colors};
static char *Smode[]={"Goto", "Limit Pattern", "Colors"};
static int Sred[3]={180,0,0}, Syellow[3]={180,148,0}, Sgreen[3]={0,180,0};
static int Sblack[3]={0,0,0};
/*
.....Main variables
*/
static UU_LOGICAL Sform_init = UU_FALSE, Sfedfl = UU_FALSE;
static UU_LOGICAL Sgo_chg = UU_FALSE;
static UN_motseg *Smptr=0,Smotatt;
static UN_mot_vpbox_struc Smvpbox[UV_NVPORTS];
static UU_LIST Skey_list;
static int Snkeys;
static UU_LOGICAL Skey_init;
static char Sav_valuestr[65];
/*
.....GOTO variables
*/
static char Sgtpt[65],Tgtpt[65];
static int Sgtfrom,Tgtfrom,Sgtyp,Tgtyp,Sgtfmode,Tgtfmode;
static int Sgtfdav,Tgtfdav;
static int Sfdmod, Tfdmod;
static char Sgtve[65],Tgtve[65],Sgtfd[65],Tgtfd[65];
static NCLX_mot_feedrate Sgfedrat,Tgfedrat;
static NCLX_mot_tlaxis Sgtaxis,Tgtaxis;
static int Tsingle,Tautopvw;
/*
.....Pattern Option variables
*/
static int Spninv,Tpninv,Spntyp,Tpntyp,Spnthr1,Tpnthr1,Spnavd,Tpnavd;
static int Spnthr2,Tpnthr2;
static char Tpnp1a[65],Tpnp1b[65],Tpnp2a[65],Tpnp2b[65],Spnhgt[65],Tpnhgt[65];
/*
.....Color variables
*/
static int Sgtcol,Tgtcol;
static int Svecol,Tvecol;
static int Tgeotog,Tgeocol;
static UU_LOGICAL Sgeo_redraw;
/*
.....Geometry variables
*/
static UU_LIST Sglst;
static UM_sgeo Sggt,Sgve;
static UU_LOGICAL Sgeo_init;
static UU_LOGICAL Sgeo_apply = UU_FALSE;
/*
.....Goto callback routines
*/
static UD_FSTAT OnGtSel(),OnGtSelSp(),OnGtTxt(),OnGtTog(),OnCoTog();
static UD_FSTAT OnAction(), OnVideo();
/*
.....Option callback routines
*/
static UD_FSTAT OnOpSel(),OnOpSelSp(),OnOpTxt(),OnOpTog();
/*
.....Local routines
*/
static void S_init_form(),S_save_form(),S_hilite_entity(),S_unhilite_entity();
static UD_FSTAT S_enter_form();
static void S_init_traverse(),S_form_invis(),S_form_vis(),S_init_geo();
static void S_hilite_entities(),S_unhilite_entities(),S_unhilite_all();
static void S_update_answers(),S_section_changed(),S_push_geo();
static void S_add_key(),S_create_key_list();
static int S_select_geo(),S_build_command();

static int SfrmPlay=0;
static int Serrflg,Spreviewflg,Schgmade;


static int *Sanswers[] = {
	&Tgtfrom,&Tgtyp,(int *)&Tgtpt,UU_NULL, UU_NULL,UU_NULL,UU_NULL,
	UU_NULL,(int *)&Tgtve,
	&Tgtfmode,(int *)&Tgtfd,&Tgtfdav,&Tfdmod,
	&Tsingle,&Tautopvw,

	&Tpninv,&Tpntyp,
	UU_NULL,UU_NULL,UU_NULL,(int *)Tpnp1a,&Tpnthr1,(int *)Tpnp1b,
	&Tpnavd,(int *)Tpnhgt, UU_NULL,UU_NULL,(int *)Tpnp2a,&Tpnthr2,(int *)Tpnp2b,

	&Tgtcol,&Tvecol,&Tgeotog,&Tgeocol,

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
/*
.....why only Tgtyp = 0 yurong
//	if ((Tgtyp==0)&&(UU_LIST_LENGTH(&Sglst) <= 0))
*/
	if (UU_LIST_LENGTH(&Sglst) <= 0)
	{
		sec2 = UU_FALSE;
	}
	if (sec1&&sec2)
	{
		if (Sgo_chg==0)
		{
			S_section_changed(Smode[Goto],Sblack,UU_TRUE,UU_TRUE);
		}
		else
		{	
			S_section_changed(Smode[Goto],Sgreen,UU_TRUE,UU_TRUE);
		}
	}
	else
	{
		S_section_changed(Smode[Goto],Sred,UU_TRUE,UU_TRUE);
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
**    E_FUNCTION     : nclu_goto()
**       Controlling routine for the GOTO Position form.
**       GOTO statements are output from this form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_goto()
{
	int status,flag;
	UU_LOGICAL cmdreject;
	UD_METHOD save_entry;
/*
.....Set up form fields
*/
	static char traverse[] = {1,1,1,1, 0,0,0, 1,1, 1,1,1,1, 1,1,
		1,1, 1,0,0,1,1,1, 1,1, 1,0,1,1,1,
		1,1,1,1,
		1,1,1,1,1,1,1,1};
	static char display[] = {1,1,1,1, 0,0,0, 1,1, 1,1,1,1, 1,1,
		1,1, 1,0,0,1,1,1, 1,1, 1,0,1,1,1,
		1,1,1,1,
		1,1,1,1,1,1,1,1};
	static char called[] = {6,6,6,6, 6,6,6, 6,6, 6,6,6,6, 6,6,
		6,6, 6,6,6,6,6,6, 6,6, 6,6,6,6,6,
		6,6,6,6,
		6,6,6,6,6,6,6,6};

	static UD_METHOD methods[] = {OnGtTog,OnGtTog,OnGtTxt,OnGtSel,
		OnGtSelSp,OnGtSelSp,OnGtSelSp,
		OnGtSel,OnGtTxt, OnGtTog,OnGtTxt,OnGtTog,OnGtTog,
		OnGtTog,OnGtTog,

		OnOpTog,OnOpTog, OnOpSel,OnOpSelSp,OnOpSelSp,OnOpTxt,OnOpTog,OnOpTxt,
		OnOpTog,OnOpTxt, OnOpSel,OnOpSelSp,OnOpTxt,OnOpTog,OnOpTxt,

		OnCoTog,OnCoTog,OnCoTog,OnCoTog,

		OnAction,OnAction,OnAction,OnAction,OnAction,OnAction,OnAction, OnVideo};
/*
.....Get the current tool axis and feed rate
*/
	NclxMotGetTlaxis(&Sgtaxis);
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
	Skey_init = UU_FALSE;
	Snkeys = 0;
	Sfedfl = UU_FALSE;
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
	status = ud_form1("ngoto.frm",Sanswers,Sanswers,methods,called,display,
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
//put the moinfr after build command in order to easier clfile swap if form accept
	if (status == -1)
	{
		if (Spreviewflg)
		{
			if (!Schgmade && status != -1 && Serrflg == 0) flag = UU_TRUE;
			else flag = UU_FALSE;
			moinfr(&flag);
		}
		goto done;
	}
/*
.....An annotation must be selected
*/
	if ((!Sgeo_init || UU_LIST_LENGTH(&Sglst) == 0) && !Sgeo_apply)
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
//put reset routine here
	if (status != UU_SUCCESS) Serrflg = 1;
	if (Spreviewflg)
	{
		if (!Schgmade && status != -1 && Serrflg == 0) flag = UU_TRUE;
		else flag = UU_FALSE;
		moinfr(&flag);
	}
	Scurrent_pt = 0;
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
	Scurrent_pt = 0;
	Sselecting = 0;
	UD_UNMARK(cmdreject);
	
	return;
}

/*********************************************************************
**    I_FUNCTION     : OnGtSelSp(fieldno,val,stat)
**       Method called when a specific (hidden) Select button is pressed.
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
static UD_FSTAT OnGtSelSp(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int fno;
/*
.....Update the geometry type form field
*/
	Tgtyp = *fieldno - FGSL1;
	ud_update_answer(FGTYP,&Tgtyp);
/*
.....Call selection routine with proper geometry type
*/
	fno = FGSEL;
	OnGtSel(&fno,val,stat);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnGtSel(fieldno,val,stat)
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
static UD_FSTAT OnGtSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status,prmpt,fno,jmpflag,invis;
	unsigned *mask;
	UU_LOGICAL flag,done;
	invis = 0;
	Sselecting = 1;
	if ((Tautopvw)&&(*fieldno==FGSEL))		
	{
		flag = 0;
		if (Spreviewflg) moinfr(&flag);
		Spreviewflg = UU_FALSE;
/*
........Erase last previewed motion
*/
		if (Smptr != UU_NULL) 
		{
			ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
			nclu_disp_cut();
		}
		Smptr = UU_NULL;
		moinfs();
		ncl_mark_mdisplay(&Smptr,&Smotatt,Smvpbox,UU_TRUE);
	}
	UD_MARK (jmpflag, UU_TRUE);
	if (jmpflag != 0)
	{
/*
.....if reject, should remove all select list
*/
handle_reject:;
		if (*fieldno==FGSEL)
		{
/*
.....unselect all
*/
			if (Tautopvw)
			{
				flag = 0;
				moinfr(&flag);
				Spreviewflg = UU_FALSE;
/*
........Erase last previewed motion
*/
				if (Smptr != UU_NULL)
				{
					ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
					nclu_disp_cut();
				}
				Smptr = UU_NULL;
			}
			S_unhilite_entities(&Sglst);
			S_form_vis();
			invis = 0;
/*
........Disable the Action buttons
*/
//			ud_set_traverse_mask(FAPV,0);
//			ud_set_traverse_mask(FAPY,0);
//			ud_set_traverse_mask(FAGE,0);
//			S_section_changed(Smode[Goto],Sred,UU_TRUE,UU_TRUE);
//			ud_frm_enable_ok(UU_FALSE);
			ud_dispfrm_set_attribs(0,FGSEL,UM_WHITE,UM_RED);
			Tgtpt[0] = '\0';
			ud_update_answer(FGTXT,(int *)&Tgtpt);
		}
		Sselecting = 0;
		Schgmade = UU_FALSE;
		goto done;
	}
/*
.....Select a GOTO point
*/
	switch (*fieldno)
	{
	case FGSEL:
/*		flag = !Tsingle;  */
		if (Tsingle)
			flag = UU_FALSE;
		else
	        flag = UU_TRUE;
		if (Tgtfrom == 0) flag = UU_FALSE;
		S_unhilite_entities(&Sglst);
		S_form_invis();
		invis = 1;
/*
........Select a screen location
*/
		do
		{
			if (Tgtyp == 2)
				status = S_locate_geo(&Sggt,&Sglst,2,421,0,FGTXT,Tgtpt);
/*
........Pick a point/pattern
*/
			else
			{
				mask = UD_ncl_ptpv;
				prmpt = 402;
				if (Tgtyp == 1)
				{
					mask = UD_ncl_patern;
					prmpt = 450;
				}
				status = S_select_geo(&Sggt,&Sglst,mask,2,prmpt,Tgtcol,0,FGTXT,
					Tgtpt);
				if (status==-2)
					goto handle_reject;
			}
/*
........User picked a location
*/
			if (strlen(Sggt.label) != 0)
			{
				S_push_geo(&Sglst,&Sggt,&Sgeo_init);
/*
........Automatically Preview the motion
*/
				if (Tautopvw)
				{
					fno = FAPV;
					OnAction(&fno,val,stat);
					Schgmade = UU_FALSE;
				}
				done = UU_FALSE;
			}
/*
........Mark picking as finished
*/
			else
				done = UU_TRUE;
			if (!flag) done = UU_TRUE;
		} while (!done);
/*
........Enable the Action buttons
*/
		if (UU_LIST_LENGTH(&Sglst) > 0)
		{
//			ud_set_traverse_mask(FAPV,1);
//			ud_set_traverse_mask(FAPY,1);
//			ud_set_traverse_mask(FAGE,1);
//			S_section_changed(Smode[Goto],Sgreen,UU_TRUE,UU_TRUE);
//			ud_frm_enable_ok(UU_TRUE);
			ud_dispfrm_set_attribs(0,FGSEL,UM_BLACK,Tgtcol);
		}
		S_form_vis();
		invis = 0;
/*
.....Check for change since preview
*/
		if (Tautopvw)
			Schgmade = UU_FALSE;
		else if (status == UU_SUCCESS) 
			Schgmade = 1;
		break;
/*
.....Select the tool axis vector
*/
	case FGTA1:
		status = S_select_geo(&Sgve,UU_NULL,UD_ncl_vepv,0,218,Tvecol,0,
			FGTA2,Tgtve);
		break;
/*
.....Check for change since preview
*/
		if (status == UU_SUCCESS) Schgmade = 1;
	}
done:;
	Sselecting = 0;
	Scurrent_pt = 0;
    if (invis) 
		S_form_vis();
	S_enable_buttons();
	UD_UNMARK(jmpflag);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnGtTog(fieldno,val,stat)
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
static UD_FSTAT OnGtTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char cfed[16][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	char valuestr[65];
/*
.....Process Levels toggle field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FGFRM:
		if (Sgtfrom != Tgtfrom) 
		{
			Schgmade = 1;
			Sgo_chg = 1;
		}
		if (Tgtfrom == 0)
		{
/*
.....diable single select field and check the single select
*/
			Tsingle = 1;
			ud_update_answer(FGSGL, &Tsingle);
			ud_set_traverse_mask(FGSGL,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(FGSGL,UU_TRUE);
		}
		break;
	case FGTYP:
		if (Tgtyp == 1) ud_form_section_enable(0,Smode[Options],UU_TRUE);
		else ud_form_section_enable(0,Smode[Options],UU_FALSE);
		if (Tgtfrom == 0 && Tgtyp == 1)
		{
			Tgtyp = 0;
			ud_update_answer(*fieldno,&Tgtyp);
		}
		break;
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
/*
.....Current
*/
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
		if (Tgfedrat.mode==3)
			Tgfedrat.mode = 1;
		nclu_fedrat_form(&Tgfedrat, &Sgfedrat, cfed, UU_FALSE, Sfedfl, FGFD2);
		Sfedfl = UU_TRUE;
		break;
	}
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnGtTxt(fieldno,val,stat)
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
static UD_FSTAT OnGtTxt(fieldno, val, stat)
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
		if (Sgeo_init) S_unhilite_entities(&Sglst);
		ul_to_upper(Tgtpt);
		if (strcmp(Sgtpt,Tgtpt) != 0)
		{
			Schgmade = 1;
			Sgo_chg = 1;
			S_section_changed(Smode[Goto],Sgreen,UU_TRUE,UU_TRUE);
			S_init_geo(&Sggt,Tgtpt,Tgtcol);
			S_push_geo(&Sglst,&Sggt,&Sgeo_init);
/*
........Enable the Action buttons
*/
			if (UU_LIST_LENGTH(&Sglst) > 0)
			{
//				ud_set_traverse_mask(FAPV,1);
//				ud_set_traverse_mask(FAPY,1);
//				ud_set_traverse_mask(FAGE,1);
//				S_section_changed(Smode[Goto],Sgreen,UU_TRUE,UU_TRUE);
//				ud_frm_enable_ok(UU_TRUE);
				ud_dispfrm_set_attribs(0,FGSEL,UM_BLACK,Tgtcol);
			}
		}
		break;
	case FGTA2:
		ul_to_upper(Tgtve);
		if (strcmp(Sgtve,Tgtve) != 0)
		{
			Schgmade = 1;
			Sgo_chg = 1;
			S_init_geo(&Sgve,Tgtve,Tvecol);
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
**    I_FUNCTION     : OnOpSelSp(fieldno,val,stat)
**       Method called when a specific (hidden) Pattern Select button
**       is pressed.
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
static UD_FSTAT OnOpSelSp(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int fno;
/*
.....Update the geometry type form field
*/
	switch (*fieldno)
	{
/*
........Omit
*/
	case FGPS1:
		Tpntyp = 1;
		ud_update_answer(FGP1B,&Tpntyp);
		fno = FGP2A;
		break;
/*
........Retain
*/
	case FGPS2:
		Tpntyp = 2;
		ud_update_answer(FGP1B,&Tpntyp);
		fno = FGP2A;
		break;
/*
........Avoid
*/
	case FGPS3:
		Tpnavd = 1;
		ud_update_answer(FGP3A,&Tgtyp);
		fno = FGP4A;
		break;
	}
/*
.....Call selection routine with proper geometry type
*/
	OnOpSel(&fno,val,stat);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnOpSel(fieldno,val,stat)
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
static UD_FSTAT OnOpSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,inc,status,rel,thrufl,fld1,fld2,prmpt,numint,*ptr;
	UU_LOGICAL cmdreject;
	char sbuf[80],*lab1,*lab2;
	UU_KEY_ID key,subkey;
	UU_LIST ilist;
	UM_sgeo *mgeo;
	UM_PLOCREC pick;
/*
.....Initialize routine
*/
	status = UU_FAILURE;
	uu_list_init(&ilist,sizeof(int),20,20);
	sbuf[0] = '\0';
	if (*fieldno == FGP2A)
	{
		thrufl = Tpnthr1;
		lab1 = Tpnp1a; lab2 = Tpnp1b;
		fld1 = FGP2B; fld2 = FGP2D;
		prmpt = 453;
		if (Tpntyp == 2) prmpt = 452;
		ud_update_answer(FGP2B,sbuf);
		ud_update_answer(FGP2D,sbuf);
	}
	else
	{
		lab1 = Tpnp2a; lab2 = Tpnp2b;
		thrufl = Tpnthr2;
		fld1 = FGP4B; fld2 = FGP4D;
		prmpt = 455;
		ud_update_answer(FGP4B,sbuf);
		ud_update_answer(FGP4D,sbuf);
	}
/*
.....Make sure a pattern is selected
*/
	if (UU_LIST_LENGTH(&Sglst) != 1) goto failed;
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sglst);
	key = mgeo[0].key;
	ur_retrieve_data_relnum(key,&rel);
	if (rel != NCL_PATERN_REL) goto failed;
/*
.....Trap Reject Op
*/
	S_form_invis();
	UD_MARK (cmdreject, UU_TRUE);
	if (cmdreject != 0) goto done;
/*
.....Select points from the pattern
*/
	inc = 0;
	ud_limit_entity(UU_TRUE,key);
	do
	{
		ua_dl_pldas(UD_DASPCKLOC,UA_NCL,prmpt,&pick,1,&numint,2);
		if (numint == 0) break;
		subkey = um_get_pickkey(&pick.pent,2);
		uu_list_push(&ilist,&subkey);
		inc++;
		if (thrufl && inc == 2) break;
	} while (numint > 0);
/*
.....Update the proper field(s) with the
.....selected point numbers
*/
	if (inc > 0)
	{
		ptr = (int *)UU_LIST_ARRAY(&ilist);
		if (thrufl)
		{
			sprintf(lab1,"%d",ptr[0]);
			sprintf(lab2,"%d",ptr[1]);
		}
		else
		{
			for (i=0;i<UU_LIST_LENGTH(&ilist);i++)
			{
				sprintf(sbuf,"%d,",ptr[i]);
				strcat(lab1,sbuf);
			}
			lab1[strlen(lab1)-1] = '\0';
		}
	}
	else
	{
		lab1[0] = lab2[0] = '\0';
	}
	ud_update_answer(fld1,lab1);
	ud_update_answer(fld2,lab2);
	status = UU_SUCCESS;
	S_section_changed(Smode[Options],Sgreen,UU_TRUE,UU_TRUE);
	S_form_vis();
	UD_UNMARK(cmdreject);
	goto done;
/*
....Pattern must be selected
*/
failed:;
	ud_wrerr("A single pattern must be first be selected.");
	status = UU_FAILURE;
/*
.....Check for change since preview
*/
done:;
	ud_limit_entity(UU_FALSE,key);
	S_form_vis();
	if (status == UU_SUCCESS) Schgmade = 1;
	uu_list_free(&ilist);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnOpTog(fieldno,val,stat)
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
static UD_FSTAT OnOpTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,ifl;
/*
.....Process Levels toggle field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FGP1A:
		if (Spninv != Tpninv) Schgmade = 1;
		break;
	case FGP1B:
		ifl = Tpntyp != 0;
		for (i=FGP2A;i<=FGP2D;i++) ud_set_traverse_mask(i,ifl);
		if (Spntyp != Tpntyp) Schgmade = 1;
		break;
	case FGP2C:
		ud_set_traverse_mask(FGP2D,Tpnthr1);
		break;
	case FGP3A:
		ifl = Tpnavd == 1;
		for (i=FGP3B;i<=FGP4D;i++) ud_set_traverse_mask(i,ifl);
		if (Spnavd != Tpnavd) Schgmade = 1;
		break;
	case FGP4C:
		ud_set_traverse_mask(FGP4D,Tpnthr2);
		break;
	}
	S_section_changed(Smode[Options],Sgreen,UU_TRUE,UU_TRUE);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnOpTxt(fieldno,val,stat)
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
static UD_FSTAT OnOpTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
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
/*
.....Process Color toggle field
*/
	switch (*fieldno)
	{
	case FCOLA:
		if (UU_LIST_LENGTH(&Sglst) != 0)
			ud_dispfrm_set_attribs(0,FGSEL,UM_BLACK,Tgtcol);
		S_hilite_entities(&Sglst,Tgtcol);
		break;
	case FCOLB:
		ud_dispfrm_set_attribs(0,FGTA1,UM_BLACK,Tvecol);
		S_hilite_entity(&Sgtve,Tvecol);
		break;
	case FCOLC:
		Tgeotog = val->frmint[0];
	case FCOLD:
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
	int status,i,flag= 0;

	switch (*fieldno)
	{
/*
.....Preview command
*/
	case FAPV:
		if (Sselecting==0)
		{
			if (Spreviewflg) moinfr(&flag);
		}
		Spreviewflg = UU_FALSE;
		Schgmade = UU_FALSE;
		Serrflg = 0;
/*
........Erase last previewed motion
*/
		if (Sselecting==0)
		{
			if (Smptr != UU_NULL) 
			{
				ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
				nclu_disp_cut();
			}
			Smptr = UU_NULL;
/*
........Save motion settings
*/
			moinfs();
			ncl_mark_mdisplay(&Smptr,&Smotatt,Smvpbox,UU_TRUE);
		}
/*
........Output PROFIL command
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
		if ((Sselecting==0)&&(status != UU_SUCCESS)) 
			moinfr(&flag);
/*
........A curve was selected so keep the temp clfile
........for now and set the preview flag
*/
		if (status == UU_SUCCESS) 
		{
			Spreviewflg = UU_TRUE;
			ud_set_traverse_mask(FAPB,1);
			ud_set_traverse_mask(FAVE,NAUTIPV);
			if (Sselecting==0)
				Scurrent_pt = 0;
		}
		break;
/*
.....APPLY
.....A curve must be selected
*/
	case FAPY:
		if (Spreviewflg)
		{
			if (!Schgmade && Serrflg == 0) flag = UU_TRUE;
			else flag = UU_FALSE;
			moinfr(&flag);
		}
		Scurrent_pt = 0;
		status = S_build_command(UU_TRUE);
		Spreviewflg = UU_FALSE;
		Smptr = UU_NULL;
		if (status != UU_SUCCESS)
			break;
		S_unhilite_entities(&Sglst);
/*
.....reset mode = 0 (SAME) if the mode is the value after apply
*/
/*
.....removed "SAME" changed per Ken
/*
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
*/
		Sgeo_apply = UU_TRUE;
		for (i=FAPV;i<FAVW;i++) ud_set_traverse_mask(i,0);
		i = -1;
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
		if (Spreviewflg) moinfr(&flag);
		if (Smptr != UU_NULL) 
		{
			ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
			nclu_disp_cut();
		}
		Smptr = UU_NULL;
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
.....Initialize Goto fields
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
**       Initializes the Profile Machining form variables.
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
	UU_REAL hgt;
	int chc;
/*
.....Initialize the Form settings
*/
	if (!Sform_init)
	{
		Sgtcol = NCLX_SEA_GREEN;
		Svecol = NCLX_TAN;
		Sgtyp = 0;
		Sgtfrom = 1;
		Sgtfmode = 0;
		Sgtfdav = 0;
		Sfdmod = 0;
		strcpy(Spnhgt, "0.0");
	}
	nclc_get_ctr_hgt(&chc, &hgt);
	Sgtfdav = Sgfedrat.slowdown_flag||Sgfedrat.accel_flag||(hgt!=0.0);
	if (Sgtfmode == 2)
	{
		Sgtfmode = 0;
		Sgtfdav = 0; 
	}
	ncl_sprintf(Sgtve,&Sgtaxis.vector,3);
	ncl_sprintf(Sgtfd,&Sgfedrat.base_feedrate,1);
	Sfdmod = Sgfedrat.mode-1;
	Spninv = 0;
	Spntyp = 0;
	Spnthr1 = Spnthr2 = 1;
	Sgtpt[0] = '\0';

	Tgtcol = Sgtcol;
	Tgtyp = Sgtyp;
	Tgtfrom = Sgtfrom;
	Tvecol = Svecol;
	Tgtfmode = Sgtfmode;
	Tgtfdav = Sgtfdav;
	Tfdmod = Sfdmod;
	Tpninv = Spninv;
	Tpntyp = Spntyp;
	Tpnthr1 = Spnthr1;
	Tpnthr2 = Spnthr2;
	strcpy(Tgtpt,Sgtpt);
	strcpy(Tgtve,Sgtve);
	strcpy(Tgtfd,Sgtfd);
	if (Tgtfmode!=0)
/*
.....rapid
*/
	{
		Tgtfd[0] = '\0';
	}
	strcpy(Sav_valuestr, Sgtfd);
	Tpnp1a[0] = Tpnp1b[0] = Tpnp2a[0] = Tpnp2b[0] = '\0';
	strcpy(Tpnhgt,Spnhgt);
/*
.....Initialize geometry color
*/
	Tgeotog = UN_unused_geo_flag;
	Tgeocol = UN_unused_geo_color;
	Sgeo_redraw = UU_FALSE;
	Tsingle = UN_mot_single_sel;
	Tautopvw = UN_mot_auto_preview;
/*
.....Mark variables as being initialized
*/
	Sform_init = UU_TRUE;
/*
.....Initialize geomtry variables
*/
	Sgeo_init = UU_FALSE;

	Sggt.key = Sgve.key = 0;
	Sggt.color = Sgve.color = -1;
	Sggt.label[0] = Sgve.label[0] = '\0';
	S_init_geo(&Sggt,Tgtpt,Tgtcol);
	S_init_geo(&Sgve,Tgtve,Tvecol);
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
	int ifl;
/*
.....Set the Section colors
*/
	S_section_changed(Smode[Goto],Sred,UU_TRUE,UU_FALSE);
	S_section_changed(Smode[Options],Sblack,UU_FALSE,UU_FALSE);
	ud_frm_enable_ok(UU_FALSE);
	if (Sgtyp == 1) ud_form_section_enable(0,Smode[Options],UU_TRUE);
	else ud_form_section_enable(0,Smode[Options],UU_FALSE);
/*
.....Set the field traversals
*/
	ud_set_traverse_mask(FGFD4,Tgtfdav);
	ifl = Tpntyp != 0;
	ud_set_traverse_mask(FGP2A,ifl);
	ud_set_traverse_mask(FGP2B,ifl);
	ud_set_traverse_mask(FGP2C,ifl);
	ud_set_traverse_mask(FGP2D,ifl);
	ifl = Tpnavd != 0;
	ud_set_traverse_mask(FGP3B,ifl);
	ud_set_traverse_mask(FGP4A,ifl);
	ud_set_traverse_mask(FGP4B,ifl);
	ud_set_traverse_mask(FGP4C,ifl);
	ud_set_traverse_mask(FGP4D,ifl);
/*
.....Set the mandatory fields to red
*/
	ud_dispfrm_set_attribs(0,FGSEL,UM_WHITE,UM_RED);
/*
.....Set Pick buttons to correct color
*/
	ud_dispfrm_set_attribs(0,FGTA1,UM_BLACK,Tvecol);
	Scurrent_pt = 0;
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
/*
.....Save the Goto settings
*/
	Sgtfrom = Tgtfrom;
	Sgtyp = Tgtyp;
	Sgtfmode = Tgtfmode;
	Spninv = Tpninv;
	Spntyp = Tpntyp;
	Spnthr1 = Tpnthr1;
	Spnthr2 = Tpnthr2;
	strcpy(Spnhgt,Tpnhgt);
	Sgtcol = Tgtcol;
	Svecol = Tvecol;
	UN_unused_geo_flag = Tgeotog;
	UN_unused_geo_color = Tgeocol;
	UN_mot_single_sel = Tsingle;
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
	if (cmdreject != 0) goto reject;
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
		if (sflst != UU_NULL) S_unhilite_entities(sflst);
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
........Unhighlight the previous selections
*/
		S_unhilite_entities(sflst);
/*
........Store the entities' data
*/
		uu_list_init(sflst,sizeof(UM_sgeo),numint,10);
		Sgeo_init = UU_TRUE;
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
reject:
	iret = -2;
	goto done;
}

/*********************************************************************
**    I_FUNCTION     :  S_locate_geo(sfpt,sflst,multi,prmno,frm,
**                                   fieldno,label)
**       Routine to screen locate a position.  The associated text field
**       will be updated with the coordinates of the selected location.
**    PARAMETERS
**       INPUT  :
**          multi    0 = Single selection, 1 = multiple selection,
**                   2 = Single selection (calling routine handles invisibling
**                   of form).
**          prmno    Prompt number to use while in locate mode.
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
static int S_locate_geo(sfpt,sflst,multi,prmno,frm,fieldno,label)
UM_sgeo *sfpt;
UU_LIST *sflst;
UU_LOGICAL multi;
int prmno,fieldno,frm;
char *label;
{
	int numint,iret,segno;
	UU_LOGICAL dragon,iupd;
	UD_NDCLOCREC pick;
	UU_LOGICAL cmdreject,doend;
	UD_RUBBER draginfo;
	UM_int2 idx, ival;
	UU_REAL pos[2];
/*
.....Take down form
*/
	iret = UU_SUCCESS;
	if (multi != 2) S_form_invis();
/*
.....Unhighlight the previous selection
*/
	if (multi != 2)
	{
		if (sfpt != UU_NULL) S_unhilite_entity(sfpt);
		if (sflst != UU_NULL) S_unhilite_entities(sflst);
	}
	else
	{
		sfpt->key = 0;
		sfpt->color = -1;
		sfpt->label[0] = '\0';
	}
/*
.....Free selection geometry
*/
	if (multi == 1)
	{
		if (Sgeo_init) UU_LIST_EMPTY(sflst);
		else uu_list_init(sflst,sizeof(UM_sgeo),10,10);
		Sgeo_init = UU_TRUE;
	}
/*
.....Set up cutter drag symbol
*/
/*
.....draginfo is not initial, it will have error if the drag segment is not created
.....then the draginfo is random number, will cause error when turn drag off
*/
	draginfo.type = 0;
	draginfo.dev = 0;
	draginfo.markval = 0;
	ncl_drag_cutter_on(&draginfo,&segno,&dragon);
/*
.....Trap Reject Op
*/
	UD_MARK (cmdreject, UU_FALSE);
	if (cmdreject != 0) goto failed;
/*
.....Get the next geometry selection
*/
	do
	{
		doend = UU_TRUE;
/*
.....Get location selection
*/
		ud_ldas(UD_DASCART,UA_NCL,prmno,&pick,1,&numint,UD_NODEFAULT);
		if (numint == 0)
		{
			if (multi != 1 || UU_LIST_LENGTH(sflst) == 0) goto failed;
			break;
		}
/*
.....conside units here
*/
		idx = 264;
		getifl (&idx, &ival);
		if (ival == 1)
		{
			pos[0] = pick.cord[0]*25.4;
			pos[1] = pick.cord[1]*25.4;
		}
		else
		{
			pos[0] = pick.cord[0];
			pos[1] = pick.cord[1];
		}
		ncl_sprintf(label,pos,2);
/*
		ncl_sprintf(label,pick.cord,2);
*/
		strcpy(sfpt->label,label);
		iupd = UU_FALSE;
		if (multi == 1)
		{
			doend = UU_FALSE;
			uu_list_push(sflst,sfpt);
			if (UU_LIST_LENGTH(sflst) == 1) iupd = fieldno != -1;
		}
		else iupd = fieldno != -1;
		if (iupd)
		{
			if (frm == 0) ud_update_answer(fieldno,(int *)label);
			else ud_dispfrm_update_answer(frm,fieldno,(int *)label);
		}
	} while (!doend);
/*
.....End of routine
.....Redisplay form
*/
done:;
	if (dragon)
		ncl_drag_cutter_off(&draginfo,segno);
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
**    I_FUNCTION     :  S_hilite_entities(sflst,color)
**       Highlights the selected geometry entities.
**    PARAMETERS
**       INPUT  :
**          sflst    Pointer to list of selected geometry structures.
**          color    Color to highlight entities.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_hilite_entities(sflst,color)
UU_LIST *sflst;
{
	int i;
	UM_sgeo *geo;
/*
.....Loop through list
*/
	if (Sgeo_init)
	{
		geo = (UM_sgeo *)UU_LIST_ARRAY(sflst);
		for (i=0;i<UU_LIST_LENGTH(sflst);i++) S_hilite_entity(&geo[i],color);
	}
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
	S_unhilite_entities(&Sglst);
	S_unhilite_entity(&Sgve);
}

/*********************************************************************
**    S_FUNCTION     :  S_unhilite_entities(sflst)
**       Unhighlights the selected geometry entities.
**    PARAMETERS
**       INPUT  :
**          sflst    Pointer to list of selected geometry structures.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_unhilite_entities(sflst)
UU_LIST *sflst;
{
	int i;
	UM_sgeo *geo;
/*
.....Loop through list
*/
	if (Sgeo_init)
	{
		geo = (UM_sgeo *)UU_LIST_ARRAY(sflst);
		for (i=0;i<UU_LIST_LENGTH(sflst);i++) S_unhilite_entity(&geo[i]);
		uu_list_free(sflst);
		Sgeo_init = UU_FALSE;
	}
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
**       Updates all answers in the form.
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
**       Updates all answers in the form.
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
**       Creates the key lists of geometry used in this form.
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
	n = UU_LIST_LENGTH(&Sglst);
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sglst);
	for (i=0;i<n;i++) S_add_key(which,mgeo[i].key);
}

/*********************************************************************
**    I_FUNCTION     : S_add_key(which,key)
**       Adds a key to either the Override_Geo key list or Contour Stock
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
**       Builds and outputs the GOTO command.
**    PARAMETERS   
**       INPUT  : flag    = UU_TRUE = output command to source file.
**	                        UU_FALSE = preview command only.
**       OUTPUT :
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS :
**          Saves and restores motion parameters when previewing the command.
**          The motion will remain displayed though.
**    WARNINGS     : none
*********************************************************************/
static int S_build_command(flag)
UU_LOGICAL flag;
{
	int i,listlen,np;
	UU_LOGICAL taxfl;
	UU_KEY_ID key;
	char *labptr;
	NCL_cmdbuf cmdbuf;
	UM_sgeo *geo;
	char cfed[16][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	NCLX_mot_feedrate fedrat;

	if (flag)
		NCL_preview_mot = 0;
	else
		NCL_preview_mot = 1;
/*
.....Determine if the Tool Axis
.....needs to be output
*/
	taxfl = (strcmp(Sgtve,Tgtve) != 0);
	ncl_init_cmdbuf(&cmdbuf);
/*
.....Loop through selections
*/
	geo = (UM_sgeo *)UU_LIST_ARRAY(&Sglst);
	listlen = UU_LIST_LENGTH(&Sglst);
	if (listlen == 0) return (UU_FAILURE);
	np = listlen;
	if (np == 0) np = 1;

	for (i=Scurrent_pt;i<np;i++)
	{
		if (i == 0) 
        {
/*
.....Output TLAXIS command if tool axis changed
*/
            if (taxfl)
            {
                if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
                ncl_add_token(&cmdbuf,NCL_tlaxis,NCL_nocomma);
		        ncl_add_token(&cmdbuf,Tgtve,NCL_comma);
		        ncl_add_cmdbuf(&cmdbuf);
                if (flag == UU_TRUE)
                {
                    ncl_set_cmdmode(UU_TRUE);
                    ncl_call(&cmdbuf);
                    taxfl = UU_FALSE;
                    strcpy(Sgtve,Tgtve);
                }
            } 
/*
.....Output advanced feedrate command
*/
			if (Tgtfdav && Sfedfl && flag)
			{
				nclu_fedrat_set_fedrat(&fedrat,cfed);
				nclu_fedrat_command(&cmdbuf,&fedrat,cfed,UU_TRUE,flag);
			}
/*
..... Output simple FEDRAT/value command if not Preview
*/
			else
			{
                if (Tgtfmode == 1)
/*
.....Output RAPID command
*/
                {
                   if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
                    ncl_add_token(&cmdbuf,NCL_rapid,NCL_nocomma);
                    ncl_add_cmdbuf(&cmdbuf);
                    if (flag == UU_TRUE)
                    {
                        ncl_set_cmdmode(UU_TRUE);
                        ncl_call(&cmdbuf);
                    }
				}
 /*
..... Output standard FEDRAT/value command if not Preview
*/
				if (Tgtfmode == 0 && strcmp(Sgtfd,Tgtfd) != 0 && flag)
				{
 		            ncl_add_token(&cmdbuf,NCL_fedrat,NCL_nocomma);
		            ncl_add_token(&cmdbuf,Tgtfd,NCL_nocomma);
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
.....GOTO command
*/ 
        if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);      	  	   	   
		if (Tgtfrom == 0)
			ncl_add_token(&cmdbuf, NCL_from, NCL_nocomma);
		else
			ncl_add_token(&cmdbuf, NCL_goto, NCL_nocomma);
/*
.....Point
*/
		key = geo[i].key;
		labptr = geo[i].label;
		ncl_add_token(&cmdbuf,labptr,NCL_comma);

/*
.....GOTO/patern,options
*/
        if (Tgtyp == 1)
		{
			if (Tpninv == 1)
			{
				ncl_add_token(&cmdbuf,NCL_invers,NCL_comma);
				ncl_add_token(&cmdbuf,NCL_const,NCL_comma);
			}
			if (Tpntyp != 0)
			{
				if (Tpntyp == 1) ncl_add_token(&cmdbuf,NCL_omit,NCL_comma);
				else ncl_add_token(&cmdbuf,NCL_retain,NCL_comma);
				if (strlen(Tpnp1a) == 0 || (Tpnthr1 == 1 && strlen(Tpnp1b) == 0))
				{
					ud_wrerr("A valid Omit/Retain range must be specified.");
					goto failed;
				}
				ncl_add_token(&cmdbuf,Tpnp1a,NCL_comma);
				if (Tpnthr1 == 1)
				{
					ncl_add_token(&cmdbuf,NCL_thru,NCL_comma);
					ncl_add_token(&cmdbuf,Tpnp1b,NCL_comma);
				}
			}
			if (Tpnavd != 0)
			{
				ncl_add_token(&cmdbuf,NCL_avoid,NCL_comma);
				if (strlen(Tpnhgt) == 0)
				{
					ud_wrerr("A valid Avoid height must be specified.");
					goto failed;
				}
				if (strlen(Tpnp2a) == 0 || (Tpnthr2 == 1 && strlen(Tpnp2b) == 0))
				{
					ud_wrerr("A valid Avoid range must be specified.");
					goto failed;
				}
				ncl_add_token(&cmdbuf,Tpnhgt,NCL_comma);
				ncl_add_token(&cmdbuf,Tpnp2a,NCL_comma);
				if (Tpnthr2 == 1)
				{
					ncl_add_token(&cmdbuf,NCL_thru,NCL_comma);
					ncl_add_token(&cmdbuf,Tpnp2b,NCL_comma);
				}
			}
		}
/*
.....Call the command
*/
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
/*
.....Insert the command at the current line, but do not execute
.......Only need to write the command if Preview was already pressed
.......and no changes were made since the command was already executed
*/
//Never execute the fedrat command if previewed.
//Yurong Ask Ken
//we need make change here
//if 
		if (Spreviewflg && !Schgmade) ncl_call_input(&cmdbuf,UU_TRUE);
/*
.....Execute command if a change has been made since the last preview or no
.....preview was created
*/
		else Serrflg = ncl_call(&cmdbuf);
	}
	Scurrent_pt = np;
	if (flag==UU_TRUE)
	{
		strcpy(Sgtve,Tgtve);
		Scurrent_pt = 0;
	}
	NCL_preview_mot = 1;
	return(UU_SUCCESS);
/*
.....Failed to create command
*/
failed:;
	NCL_preview_mot = 1;
	return(UU_FAILURE);
}

