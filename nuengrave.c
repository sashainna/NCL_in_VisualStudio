/********************************************************************* 
**    NAME         :  nuengrave.c
**       CONTAINS:
**		             nclu_engrave()
**
**    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nuengrave.c , 25.5
**    DATE AND TIME OF LAST MODIFICATION
**       10/27/16 , 13:15:34
*********************************************************************/
#include <string.h>
#include "class.h"
#include "dselmask.h"
#include "nclmplay.h"
#include "mdrel.h"
#include "mgeom.h"
#include "mdpick.h"
#include "mxxx.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclx.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "uhep.h"
#include "ulist.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"

extern int NCL_preview_mot;
static char Sfeed_valuestr[80];
enum
{
/*
.....Annotation fields
*/
	FDCV1,FDCV2,FDCV3,FDCV4,
/*
.....Level Fields
*/
	FDDP1,FDDP2,FDDP3, FDDS1,FDDS2,
/*
.....Positioning fields
*/
	FPCL1,FPCL2,FPCL3, FPRP1,FPRP2,FPRP3, FPRT1,FPRT2,FPRT3,
/*
.....Feed Rate fields
*/
	FFTG1,FFED1,FFTG2,FFED2,FFTG3,FFED3,FFTG4,FFED4,FFEDRAT,
/*
.....Color Fields
*/
	FCOLA, FCOLB, FCOLC, FCOLD, FCOLE, FCOLF, FCOLG,
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
enum {Curve, Levels, Posit, Feeds, Colors};
static char *Smode[]={"Text", "Levels", "Positioning", "Feed Rates", "Colors"};
static int Sred[3]={180,0,0}, Syellow[3]={180,148,0}, Sgreen[3]={0,180,0};
static int Sblack[3]={0,0,0};
/*
.....Main variables
*/
static UU_LOGICAL Sform_init = UU_FALSE;
static UN_motseg *Smptr=0,Smotatt;
static UN_mot_vpbox_struc Smvpbox[UV_NVPORTS];
static UU_LIST Skey_list;
static int Snkeys;
static UU_LOGICAL Skey_init;
/*
.....Engraving variables
*/
static int Sdscol,Tdscol;
static UU_REAL Tpfthk;
static char Tpfthk_str[65],Spfthk_str[65];
static int Slpdpt,Tlpdpt;
static char Slptpl[81],Tlptpl[81];
static int Slpdcol,Tlpdcol,Slpdps,Tlpdps;
UU_REAL Tlpdpst;
static char Slpdpst_str[65],Tlpdpst_str[65];
/*
.....Positioning variables
*/
static int Spoclr,Tpoclr;
static char Spocpln[81],Tpocpln[81];
static int Spoccol,Tpoccol;
static int Sporap,Tporap;
static char Spoppln[81],Tpoppln[81];
static int Spopcol,Tpopcol;
static int Sporet,Tporet;
static char Sporpln[81],Tporpln[81];
static int Sporcol,Tporcol;
static UU_REAL Tportl;
static char Sportl_str[65],Tportl_str[65];
/*
.....Feed Rate variables
*/
static int Sfopt[4],Tfopt[4];
static char Sfeed_str[4][65],Tfeed_str[4][65];
/*
.....Color variables
*/
static int Sdscol,Tdscol;
static int Slpdcol,Tlpdcol;
static int Spoccol,Tpoccol;
static int Spopcol,Tpopcol;
static int Sporcol,Tporcol;
static int Tgeotog,Tgeocol;
static UU_LOGICAL Sgeo_redraw;
/*
.....Geometry variables
*/
static UM_sgeo Sgcv,Sgcp,Sgret,Sgtpl,Sgrap;
static UU_LOGICAL Sgeo_init;
static UU_LIST Sglst;
static UU_LOGICAL Sgeo_apply = UU_FALSE;
/*
.....Main form callback routines
*/
static UD_FSTAT OnDsSel(),OnLvTxt();
static UD_FSTAT OnLvSel(),OnLvTog();
static UD_FSTAT OnEditTxt();
static UD_FSTAT OnCoTog(),OnAction(), OnVideo();
/*
.....Positioning frame routines
*/
static UD_FSTAT OnPoTog(),OnPoSel(),OnPoTxt();
/*
.....Feed Rate frame routines
*/
static UD_FSTAT OnFdTog();
/*
.....Local routines
*/
static void S_init_form(),S_save_form(),S_hilite_entity(),S_unhilite_entity();
static UD_FSTAT S_enter_form();
static void S_init_traverse(),S_form_invis(),S_form_vis(),S_init_geo();
static void S_unhilite_entities(),S_unhilite_all();
static void S_update_answers(),S_section_changed();
static void S_add_key(),S_create_key_list();
static int S_select_geo(),S_build_command();

static int SfrmPlay=0;
static int Serrflg,Spreviewflg,Schgmade;
static char Sav_valuestr1[2][65], Sav_valuestr2[2][65], Sav_valuestr3[2][65], 
			Sav_valuestr4[2][65];
static int Ssav_fchc[6];
static UU_LOGICAL Sfd_chg = UU_FALSE;

static int *Sanswers[] = {
	UU_NULL,UU_NULL, (int *)&Tpfthk_str,(int *)&Tportl_str,

	&Tlpdpt,(int *)&Tlptpl,UU_NULL, &Tlpdps,(int *)&Tlpdpst_str,

	&Tpoclr,(int *)Tpocpln,UU_NULL, &Tporap,(int *)&Tpoppln,UU_NULL,
	&Tporet,(int *)Tporpln,UU_NULL,
		
	&Tfopt[0],(int *)&Tfeed_str[0],&Tfopt[1],(int *)&Tfeed_str[1],
	&Tfopt[2],(int *)&Tfeed_str[2],&Tfopt[3],(int *)&Tfeed_str[3],
	&Sfeed_valuestr, 

	&Tdscol,&Tlpdcol,&Tpoccol,&Tpopcol,&Tporcol,&Tgeotog,&Tgeocol,

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
	UU_LOGICAL sec2, fdsec1, fdsec2, fdsec3, fdsec4;

/*
.....Section Feed Rates parameter
*/
	fdsec1 = fdsec2 = fdsec3 = fdsec4 = UU_TRUE; 
	sec2 = UU_TRUE;
	if ((Tfopt[0]==0)||(Tfopt[0]==2))
	{
		ud_dispfrm_set_attribs(0,FFED1,UM_BLACK, UM_WHITE);
	}
	else
	{
		nc = (int)strlen(Tfeed_str[0]);
		ul_strip_blanks(Tfeed_str[0], &nc);
		if (nc<=0)
		{
			fdsec1 = UU_FALSE;
			ud_dispfrm_set_attribs(0,FFED1,UM_WHITE,UM_RED);
		}
		else
		{
			rval = atof(Tfeed_str[0]);
			if (rval<=0)
				fdsec1 =  UU_FALSE;
			if (fdsec1) 
			{
				ud_dispfrm_set_attribs(0,FFED1,UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(0,FFED1,UM_WHITE,UM_RED);
			}
		}
	}
	if ((Tfopt[1]==0)||(Tfopt[1]==2))
	{
		ud_dispfrm_set_attribs(0,FFED2,UM_BLACK, UM_WHITE);
	}
	else
	{
		nc = (int)strlen(Tfeed_str[1]);
		ul_strip_blanks(Tfeed_str[1], &nc);
		if (nc<=0)
		{
			fdsec2 = UU_FALSE;
			ud_dispfrm_set_attribs(0,FFED2,UM_WHITE,UM_RED);
		}
		else
		{
			rval = atof(Tfeed_str[1]);
			if (rval<=0)
				fdsec2 =  UU_FALSE;
			if (fdsec2) 
			{
				ud_dispfrm_set_attribs(0,FFED2,UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(0,FFED2,UM_WHITE,UM_RED);
			}
		}
	}
	if ((Tfopt[2]==0)||(Tfopt[2]==2))
	{
		ud_dispfrm_set_attribs(0,FFED3,UM_BLACK, UM_WHITE);
	}
	else
	{
		nc = (int)strlen(Tfeed_str[2]);
		ul_strip_blanks(Tfeed_str[2], &nc);
		if (nc<=0)
		{
			fdsec3 = UU_FALSE;
			ud_dispfrm_set_attribs(0,FFED3,UM_WHITE,UM_RED);
		}
		else
		{
			rval = atof(Tfeed_str[2]);
			if (rval<=0)
				fdsec3 =  UU_FALSE;
			if (fdsec3) 
			{
				ud_dispfrm_set_attribs(0,FFED3,UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(0,FFED3,UM_WHITE,UM_RED);
			}
		}
	}
	if ((Tfopt[3]==0)||(Tfopt[3]==2))
	{
		ud_dispfrm_set_attribs(0,FFED4,UM_BLACK, UM_WHITE);
	}
	else
	{
		nc = (int)strlen(Tfeed_str[3]);
		ul_strip_blanks(Tfeed_str[3], &nc);
		if (nc<=0)
		{
			fdsec4 = UU_FALSE;
			ud_dispfrm_set_attribs(0,FFED4,UM_WHITE,UM_RED);
		}
		else
		{
			rval = atof(Tfeed_str[3]);
			if (rval<=0)
				fdsec4 =  UU_FALSE;
			if (fdsec4) 
			{
				ud_dispfrm_set_attribs(0,FFED4,UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(0,FFED4,UM_WHITE,UM_RED);
			}
		}
	}
	if (fdsec1&&fdsec2&&fdsec3&&fdsec4)
	{
		if (Sfd_chg==0)
		{
			S_section_changed(Smode[Feeds],Sblack,UU_FALSE,UU_FALSE);
		}
		else
		{	
			S_section_changed(Smode[Feeds],Sgreen,UU_TRUE,UU_TRUE);
		}
	}
	else
	{
		S_section_changed(Smode[Feeds],Sred,UU_TRUE,UU_FALSE);
	}
/*
.....check text section, since it already handled
.....we don't do anything about that section but need check it 
....to see if we need enable OK button
*/
	if (UU_LIST_LENGTH(&Sglst) <= 0)
	{
		sec2 = UU_FALSE;
	}
/*
.....Set Action Buttons
*/
	ifl = fdsec1&&fdsec2&&fdsec3&&fdsec4&&sec2;
	ud_frm_enable_ok(ifl);
	ud_set_traverse_mask(FAPV,ifl);
	ud_set_traverse_mask(FAPY,ifl);
	ud_set_traverse_mask(FARS,ifl);
	ud_set_traverse_mask(FAGE,ifl);
}


/*********************************************************************
**    E_FUNCTION     : nclu_engrave()
**       Controlling routine for the Letter Engraving form.
**       A PROFIL/anote style command is output by this form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_engrave()
{
	int status,flag;
	UU_LOGICAL cmdreject;
	UD_METHOD save_entry;
/*
.....Set up form fields
*/
	static char traverse[] = {1,1,1,1,
		1,1,1, 1,1,
		1,1,1, 1,1,1, 1,1,1,
		1,1,1, 1,1,1, 1,1,1, 1,1,1,
		1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1};
	static char display[] = {1,1,1,1,
		1,1,1, 1,1,
		1,1,1, 1,1,1, 1,1,1,
		1,1,1, 1,1,1, 1,1,1, 1,1,1,
		1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1};
	static char called[] = {6,6,6,6,
		6,6,6, 6,6,
		6,6,6, 6,6,6, 6,6,6,
		6,6,6, 6,6,6, 6,6,6, 6,6,6,
		6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6};

	static UD_METHOD methods[] = {OnDsSel,OnDsSel,OnEditTxt,OnEditTxt,
		OnLvTog,OnLvTxt,OnLvSel, OnLvTog,OnEditTxt,

		OnPoTog,OnPoTxt,OnPoSel, OnPoTog,OnPoTxt,OnPoSel, OnPoTog,OnPoTxt,OnPoSel,

		OnFdTog,OnEditTxt,OnFdTog,OnEditTxt, OnFdTog,OnEditTxt,OnFdTog,OnEditTxt,UU_NULL,

		OnCoTog,OnCoTog,OnCoTog,OnCoTog,OnCoTog,OnCoTog,OnCoTog,

		OnAction,OnAction,OnAction,OnAction,OnAction,OnAction,OnAction,OnVideo};
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
	status = ud_form1("nengrave.frm",Sanswers,Sanswers,methods,called,display,
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
.....An annotation must be selected
*/
	if ((!Sgeo_init || UU_LIST_LENGTH(&Sglst) == 0) && !Sgeo_apply)
	{
		ud_wrerr("You must select an annotation to engrave.");
		goto repeat;
	}
/*
.....Save the form settings
*/
	S_save_form();
/*
.....Output the command
*/
	S_build_command(UU_TRUE);
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
**    I_FUNCTION     : OnDsSel(fieldno,val,stat)
**       Method called when a Drive Surface Select button is pressed.
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
static UD_FSTAT OnDsSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,status;
/*
.....Process Drive Surface pushbuttons
*/
	switch (*fieldno)
	{
	case FDCV1:
		status = S_select_geo(&Sgcv,&Sglst,UD_text,UU_TRUE,672,Tdscol,0,-1,
			UU_NULL);
		if (UU_LIST_LENGTH(&Sglst) > 0)
		{
			ud_set_traverse_mask(FDCV2,1);
			ud_set_traverse_mask(FAPV,1);
			ud_set_traverse_mask(FAPY,1);
			ud_set_traverse_mask(FAGE,1);
			S_section_changed(Smode[Curve],Sgreen,UU_TRUE,UU_TRUE);
			ud_frm_enable_ok(UU_TRUE);
			ud_dispfrm_set_attribs(0,FDCV1,UM_BLACK,Tdscol);
		}
		break;
	case FDCV2:
		S_unhilite_entities(&Sglst);
		ud_set_traverse_mask(FDCV2,0);
		for (i=FAPV;i<FAVW;i++) ud_set_traverse_mask(i,0);
		status = UU_SUCCESS;
		S_section_changed(Smode[Curve],Sred,UU_TRUE,UU_FALSE);
		ud_frm_enable_ok(UU_FALSE);
		ud_dispfrm_set_attribs(0,FDCV1,UM_WHITE,UM_RED);
		break;
	case FDDP3:
		status = S_select_geo(&Sgtpl,UU_NULL,UD_ncl_pl,UU_FALSE,669,Tlpdcol,0,
			FDDP2,Tlptpl);
		break;
	}
/*
.....Check for change since preview
*/
	if (status == UU_SUCCESS) Schgmade = 1;
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnLvSel(fieldno,val,stat)
**       Method called when a Level Surface Select button is pressed.
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
static UD_FSTAT OnLvSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,status;
/*
.....Process Drive Surface pushbuttons
*/
	switch (*fieldno)
	{
	case FDDP3:
		status = S_select_geo(&Sgtpl,UU_NULL,UD_ncl_pl,UU_FALSE,669,Tlpdcol,0,
			FDDP2,Tlptpl);
		break;
	}
/*
.....Check for change since preview
*/
	if (status == UU_SUCCESS) Schgmade = 1;
	S_section_changed(Smode[Levels],Sgreen,UU_TRUE,UU_TRUE);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnLvTog(fieldno,val,stat)
**       Method called when a Levels toggle field is changed.
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
static UD_FSTAT OnLvTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	UM_sgeo *geo;
/*
.....Process Levels toggle field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FDDP1:
		if (Tlpdpt == 0)
		{
			ud_set_traverse_mask(FDDP2,0);
			ud_set_traverse_mask(FDDP3,0);
			ud_set_traverse_mask(FDDS1,0);
			ud_set_traverse_mask(FDDS2,0);
		}
		else if (Tlpdpt == 1)
		{
			ud_set_traverse_mask(FDDP2,1);
			ud_set_traverse_mask(FDDP3,1);
			ud_set_traverse_mask(FDDS1,1);
			ud_set_traverse_mask(FDDS2,1);
		}
		else
		{
			ud_set_traverse_mask(FDDP2,1);
			ud_set_traverse_mask(FDDP3,0);
			ud_set_traverse_mask(FDDS1,1);
			ud_set_traverse_mask(FDDS2,1);
			if (Tlpdpt == 3 && Tlpdps != 1)
			{
				Tlpdps = 1;
				ud_update_answer(FDDS1,&Tlpdps);
			}
		}
		if (Slpdpt != Tlpdpt) Schgmade = 1;
	case FDDS1:
		if (Tlpdpt == 3 && Tlpdps != 1)
		{
			Tlpdps = 1;
			ud_update_answer(FDDS1,&Tlpdps);
		}
		if (Slpdps != Tlpdps) Schgmade = 1;
		break;
	}
	S_section_changed(Smode[Levels],Sgreen,UU_TRUE,UU_TRUE);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnLvTxt(fieldno,val,stat)
**       Method called when a Levels text field is changed.
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
static UD_FSTAT OnLvTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Process Drive Surface text field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FDDP2:
		ul_to_upper(Tlptpl);
		if (strcmp(Slptpl,Tlptpl) != 0)
		{
			Schgmade = 1;
			S_section_changed(Smode[Levels],Sgreen,UU_TRUE,UU_TRUE);
			S_init_geo(&Sgtpl,Tlptpl,Tlpdcol);
		}
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnPoSel(fieldno,val,stat)
**       Method called when a Positioning button is pressed.
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
static UD_FSTAT OnPoSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status;
/*
.....Process Positioning pushbuttons
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FPCL3:
		status = S_select_geo(&Sgcp,UU_NULL,UD_ncl_pl,UU_FALSE,472,Tpoccol,0,
			FPCL2,Tpocpln);
		break;
	case FPRP3:
		status = S_select_geo(&Sgrap,UU_NULL,UD_ncl_pl,UU_FALSE,659,Tpopcol,0,
			FPRP2,Tpoppln);
		break;
	case FPRT3:
		status = S_select_geo(&Sgret,UU_NULL,UD_ncl_pl,UU_FALSE,660,Tporcol,0,
			FPRT2,Tporpln);
		break;
	}
	if (status == UU_SUCCESS) Schgmade = 1;
	S_section_changed(Smode[Posit],Sgreen,UU_TRUE,UU_TRUE);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnPoTog(fieldno,val,stat)
**       Method called when a Positioning toggle field is changed.
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
static UD_FSTAT OnPoTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Process Positioning toggle field
.......Check for change since preview
*/
	switch (*fieldno)
	{
/*
........Clearance fields
*/
	case FPCL1:
		if (Tpoclr == 0)
		{
			ud_set_traverse_mask(FPCL2,0);
			ud_set_traverse_mask(FPCL3,0);
		}
		else if (Tpoclr == 1)
		{
			ud_set_traverse_mask(FPCL2,1);
			ud_set_traverse_mask(FPCL3,1);
		}
		else
		{
			ud_set_traverse_mask(FPCL2,1);
			ud_set_traverse_mask(FPCL3,0);
		}
		if (Spoclr != Tpoclr) Schgmade = 1;
		break;
/*
........Rapto fields
*/
	case FPRP1:
		if (Tporap == 0)
		{
			ud_set_traverse_mask(FPRP2,0);
			ud_set_traverse_mask(FPRP3,0);
		}
		else if (Tporap == 1)
		{
			ud_set_traverse_mask(FPRP2,1);
			ud_set_traverse_mask(FPRP3,1);
		}
		else
		{
			ud_set_traverse_mask(FPRP2,1);
			ud_set_traverse_mask(FPRP3,0);
		}
		if (Sporap != Tporap) Schgmade = 1;
		break;
/*
........Retract fields
*/
	case FPRT1:
		if (Tporet == 0 || Tporet == 1)
		{
			ud_set_traverse_mask(FPRT2,0);
			ud_set_traverse_mask(FPRT3,0);
		}
		else if (Tporet == 2)
		{
			ud_set_traverse_mask(FPRT2,1);
			ud_set_traverse_mask(FPRT3,1);
		}
		else
		{
			ud_set_traverse_mask(FPRT2,1);
			ud_set_traverse_mask(FPRT3,0);
		}
		if (Sporet != Tporet) Schgmade = 1;
		break;
	}
	S_section_changed(Smode[Posit],Sgreen,UU_TRUE,UU_TRUE);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnPoTxt(fieldno,val,stat)
**       Method called when a Positioning Surface text field is changed.
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
static UD_FSTAT OnPoTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char *spt,*tpt;
/*
.....Process Positioning text field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FPCL2:
		ul_to_upper(Tpocpln);
		spt = Spocpln; tpt = Tpocpln;
		S_init_geo(&Sgcp,Tpocpln,Tpoccol);
		break;
	case FPRP2:
		ul_to_upper(Tpoppln);
		spt = Spoppln; tpt = Tpoppln;
		S_init_geo(&Sgrap,Tpoppln,Tpopcol);
		break;
	case FPRT2:
		ul_to_upper(Tporpln);
		spt = Sporpln; tpt = Tporpln;
		S_init_geo(&Sgret,Tporpln,Tporcol);
		break;
	}
	if (strcmp(spt,tpt) != 0)
	{
		Schgmade = UU_TRUE;
		S_section_changed(Smode[Posit],Sgreen,UU_TRUE,UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnFdTog(fieldno,val,stat)
**       Method called when a Feed Rate toggle field is changed.
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
static UD_FSTAT OnFdTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char valuestr[65];
	int i = -1;

	switch(*fieldno)
	{
	case FFTG1:
		i = 0;
		if (Tfopt[0]==0)
		{
			ud_setfrm_traverse_mask(0,FFED1,UU_FALSE);
			valuestr[0] = '\0';
			if (Tfeed_str[0][0]!='\0')
			{
				if (Ssav_fchc[0]==1)
					strcpy(Sav_valuestr1[0], Tfeed_str[0]);
				else if (Ssav_fchc[0]==3)
					strcpy(Sav_valuestr1[1], Tfeed_str[0]);
			}
			ud_dispfrm_update_answer(0,FFED1,(int *)valuestr);
		}
		else if (Tfopt[0]==2)
		{
/*
.....rapid
*/
			ud_setfrm_traverse_mask(0,FFED1,UU_FALSE);
			if (Tfeed_str[0][0]!='\0')
			{
				if (Ssav_fchc[0]==1)
					strcpy(Sav_valuestr1[0], Tfeed_str[0]);
				else if (Ssav_fchc[0]==3)
					strcpy(Sav_valuestr1[1], Tfeed_str[0]);
			}
			valuestr[0] = '\0';
			ud_dispfrm_update_answer(0,FFED1,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(0,FFED1,UU_TRUE);
			if (Tfopt[0]==1)
			{
				if (Tfeed_str[0][0]!='\0')
				{	
					if (Ssav_fchc[0]==3)
						strcpy(Sav_valuestr1[1], Tfeed_str[0]);
				}
				strcpy(Tfeed_str[0], Sav_valuestr1[0]);
			}
			else if (Tfopt[0]==3)
			{
				if (Tfeed_str[0][0]!='\0')
				{
					if (Ssav_fchc[0]==1)
						strcpy(Sav_valuestr1[0], Tfeed_str[0]);
				}
				strcpy(Tfeed_str[0], Sav_valuestr1[1]);
			}
			ud_dispfrm_update_answer(0,FFED1,(int *)Tfeed_str[0]);
		}	
		Ssav_fchc[0] = *(val->frmint);
		break;
	case FFTG2:
		i = 1;
		if (Tfopt[1]==0)
		{
			ud_setfrm_traverse_mask(0,FFED2,UU_FALSE);
			valuestr[0] = '\0';
			if (Tfeed_str[1][0]!='\0')
			{
				if (Ssav_fchc[1]==1)
					strcpy(Sav_valuestr2[0], Tfeed_str[1]);
				else if (Ssav_fchc[1]==3)
					strcpy(Sav_valuestr2[1], Tfeed_str[1]);
			}
			ud_dispfrm_update_answer(0,FFED2,(int *)valuestr);
		}
		else if (Tfopt[1]==2)
		{
/*
.....rapid
*/
			ud_setfrm_traverse_mask(0,FFED2,UU_FALSE);
			if (Tfeed_str[1][0]!='\0')
			{
				if (Ssav_fchc[1]==1)
					strcpy(Sav_valuestr2[0], Tfeed_str[1]);
				else if (Ssav_fchc[1]==3)
					strcpy(Sav_valuestr2[1], Tfeed_str[1]);
			}
			valuestr[0] = '\0';
			ud_dispfrm_update_answer(0,FFED2,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(0,FFED2,UU_TRUE);
			if (Tfopt[1]==1)
			{
				if (Tfeed_str[1][0]!='\0')
				{
					if (Ssav_fchc[1]==3)
						strcpy(Sav_valuestr2[1], Tfeed_str[1]);
				}
				strcpy(Tfeed_str[1], Sav_valuestr2[0]);
			}
			else if (Tfopt[1]==3)
			{
				if (Tfeed_str[1][0]!='\0')
				{
					if (Ssav_fchc[1]==1)
						strcpy(Sav_valuestr2[0], Tfeed_str[1]);
				}
				strcpy(Tfeed_str[1], Sav_valuestr2[1]);
			}
			ud_dispfrm_update_answer(0,FFED2,(int *)Tfeed_str[1]);
		}	
		Ssav_fchc[1] = *(val->frmint);
		break;
	case FFTG3:
		i = 2;
		if (Tfopt[2]==0)
		{
			ud_setfrm_traverse_mask(0,FFED3,UU_FALSE);
			valuestr[0] = '\0';
			if (Tfeed_str[2][0]!='\0')
			{
				if (Ssav_fchc[2]==1)
					strcpy(Sav_valuestr3[0], Tfeed_str[2]);
				else if (Ssav_fchc[2]==3)
					strcpy(Sav_valuestr3[1], Tfeed_str[2]);
			}
			ud_dispfrm_update_answer(0,FFED3,(int *)valuestr);
		}
		else if (Tfopt[2]==2)
		{
/*
.....rapid
*/
			ud_setfrm_traverse_mask(0,FFED3,UU_FALSE);
			if (Tfeed_str[2][0]!='\0')
			{
				if (Ssav_fchc[2]==1)
					strcpy(Sav_valuestr3[0], Tfeed_str[2]);
				else if (Ssav_fchc[2]==3)
					strcpy(Sav_valuestr3[1], Tfeed_str[2]);
			}
			valuestr[0] = '\0';
			ud_dispfrm_update_answer(0,FFED3,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(0,FFED3,UU_TRUE);
			if (Tfopt[2]==1)
			{
				if (Tfeed_str[2][0]!='\0')
				{
					if (Ssav_fchc[2]==3)
						strcpy(Sav_valuestr3[1], Tfeed_str[2]);
				}
				strcpy(Tfeed_str[2], Sav_valuestr3[0]);
			}
			else if (Tfopt[2]==3)
			{
				if (Tfeed_str[2][0]!='\0')
				{
					if (Ssav_fchc[2]==1)
						strcpy(Sav_valuestr3[0], Tfeed_str[2]);
				}
				strcpy(Tfeed_str[2], Sav_valuestr3[1]);
			}
			ud_dispfrm_update_answer(0,FFED3,(int *)Tfeed_str[2]);
		}	
		Ssav_fchc[2] = *(val->frmint);
		break;
	case FFTG4:
		i = 3;
		if (Tfopt[3]==0)
		{
			ud_setfrm_traverse_mask(0,FFED4,UU_FALSE);
			valuestr[0] = '\0';
			if (Tfeed_str[3][0]!='\0')
			{
				if (Ssav_fchc[3]==1)
					strcpy(Sav_valuestr4[0], Tfeed_str[3]);
				else if (Ssav_fchc[3]==3)
					strcpy(Sav_valuestr4[1], Tfeed_str[3]);
			}
			ud_dispfrm_update_answer(0,FFED4,(int *)valuestr);
		}
		else if (Tfopt[3]==2)
		{
/*
.....rapid
*/
			ud_setfrm_traverse_mask(0,FFED4,UU_FALSE);
			if (Tfeed_str[3][0]!='\0')
			{
				if (Ssav_fchc[3]==1)
					strcpy(Sav_valuestr4[0], Tfeed_str[3]);
				else if (Ssav_fchc[3]==3)
					strcpy(Sav_valuestr4[1], Tfeed_str[3]);
			}
			valuestr[0] = '\0';
			ud_dispfrm_update_answer(0,FFED4,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(0,FFED4,UU_TRUE);
			if (Tfopt[3]==1)
			{
				if (Tfeed_str[3][0]!='\0')
				{
					if (Ssav_fchc[3]==3)
						strcpy(Sav_valuestr4[1], Tfeed_str[3]);
				}
				strcpy(Tfeed_str[3], Sav_valuestr4[0]);
			}
			else if (Tfopt[3]==3)
			{
				if (Tfeed_str[3][0]!='\0')
				{
					if (Ssav_fchc[3]==1)
						strcpy(Sav_valuestr4[0], Tfeed_str[3]);
				}
				strcpy(Tfeed_str[3], Sav_valuestr4[1]);
			}
			ud_dispfrm_update_answer(0,FFED4,(int *)Tfeed_str[3]);
		}	
		Ssav_fchc[3] = *(val->frmint);
		break;
	}
	if (Sfopt[i] != Tfopt[i]) 
		Schgmade = 1;
/*
.....Process Feed Rate toggle field
*/
/*
	if (*(val->frmint) == 0 || *(val->frmint) == 2)
		ud_set_traverse_mask((*fieldno)+1,UU_FALSE);
	else
		ud_set_traverse_mask((*fieldno)+1,UU_TRUE);
*/
	S_section_changed(Smode[Feeds],Sgreen,UU_TRUE,UU_TRUE);
	Sfd_chg = 1;
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
		if (UU_LIST_LENGTH(&Sglst) != 0)
			ud_dispfrm_set_attribs(0,FDCV1,UM_BLACK,Tdscol);
		S_hilite_entity(&Sgcv,Tdscol);
		break;
	case FCOLB:
		ud_dispfrm_set_attribs(0,FDDP3,UM_BLACK,Tlpdcol);
		S_hilite_entity(&Sgtpl,Tlpdcol);
		break;
	case FCOLC:
		ud_dispfrm_set_attribs(0,FPCL3,UM_BLACK,Tpoccol);
		S_hilite_entity(&Sgcp,Tpoccol);
		break;
	case FCOLD:
		ud_dispfrm_set_attribs(0,FPRP3,UM_BLACK,Tpopcol);
		S_hilite_entity(&Sgrap,Tpopcol);
		break;
	case FCOLE:
		ud_dispfrm_set_attribs(0,FPRT3,UM_BLACK,Tporcol);
		S_hilite_entity(&Sgret,Tporcol);
		break;
	case FCOLF:
		Tgeotog = val->frmint[0];
	case FCOLG:
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
.....A curve must be selected
*/
	case FAPY:
		if (!Sgeo_init || UU_LIST_LENGTH(&Sglst) == 0)
		{
			ud_wrerr("You must select an annotation to engrave.");
			break;
		}
/*
........Output PROFIL command
*/
		if (UU_LIST_LENGTH(&Sglst) > 0)
		{
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
			S_unhilite_entities(&Sglst);
			Sgeo_apply = UU_TRUE;
			for (i=FAPV;i<FAVW;i++) ud_set_traverse_mask(i,0);
			i = -1;
			S_enter_form(&i, val, stat);
		}
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
**    I_FUNCTION     : OnEditTxt()
**			Checks for any changes made to text fields
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnEditTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int mode;
	char *spt,*tpt;
/*
.....Check for change since preview
*/
	switch(*fieldno)
	{
	case FDCV3:
		mode = -1;
		spt = Spfthk_str; tpt = Tpfthk_str;
		break;

	case FDCV4:
		mode = -1;
		spt = Sportl_str; tpt = Tportl_str;
		break;

	case FDDS2:
		mode = Levels;
		spt = Slpdpst_str; tpt = Tlpdpst_str;
		break;

	case FFED1:
		mode = Feeds;
		spt = Sfeed_str[0]; tpt = Tfeed_str[0];
		Sfd_chg = 1;
		S_enable_buttons();
		break;

	case FFED2:
		mode = Feeds;
		spt = Sfeed_str[1]; tpt = Tfeed_str[1];
		Sfd_chg = 1;
		S_enable_buttons();
		break;

	case FFED3:
		mode = Feeds;
		spt = Sfeed_str[2]; tpt = Tfeed_str[2];
		Sfd_chg = 1;
		S_enable_buttons();
		break;

	case FFED4:
		Sfd_chg = 1;
		S_enable_buttons();
		mode = Feeds;
		spt = Sfeed_str[3]; tpt = Tfeed_str[3];
		break;
	}
	if (strcmp(spt,tpt) != 0)
	{
		Schgmade = UU_TRUE;
		if (mode != -1) S_section_changed(Smode[mode],Sgreen,UU_TRUE,UU_TRUE);
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
.....Initialize Engraving fields
*/
	if (Tlpdpt == 0)
	{
		traverse[FDDP2] = traverse[FDDP3] = 0;
		traverse[FDDS1] = traverse[FDDS2] = 0;
	}
	else if (Tlpdpt == 1)
	{
		traverse[FDDP2] = traverse[FDDP3] = 1;
		traverse[FDDS1] = traverse[FDDS2] = 1;
	}
	else
	{
		traverse[FDDP2] = 1;
		traverse[FDDP3] = 0;
		traverse[FDDS1] = traverse[FDDS2] = 1;
	}
/*
.....Positioning fields
*/
	if (Tpoclr == 0)
		traverse[FPCL2] = traverse[FPCL3] = 0;
	else if (Tpoclr == 1)
		traverse[FPCL2] = traverse[FPCL3] = 1;
	else
	{
		traverse[FPCL2] = 1;
		traverse[FPCL3] = 0;
	}
	if (Tporap == 0)
		traverse[FPRP2] = traverse[FPRP3] = 0;
	else if (Tporap == 1)
		traverse[FPRP2] = traverse[FPRP3] = 1;
	else
	{
		traverse[FPRP2] = 1;
		traverse[FPRP3] = 0;
	}
	if (Tporet == 0 || Tporet == 1)
		traverse[FPRT2] = traverse[FPRT3] = 0;
	else if (Tpoclr == 2)
		traverse[FPRT2] = traverse[FPRT3] = 1;
	else
	{
		traverse[FPRT2] = 1;
		traverse[FPRT3] = 0;
	}
/*
.....Feed rate fields
*/
	for (i=0;i<4;i++)
	{
		if (Tfopt[i] == 0 || Tfopt[i] == 2) traverse[FFTG1+i*2+1] = 0;
		else traverse[FFTG1+i*2+1] = 1;
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
	int i;
	UU_REAL feedrate;
	NCLX_mot_feedrate fedrat;

	UM_real8 fthick1,fthick2,toolaxangle, tiltanglebegin, tiltangleend, tildistbegin, tiltdistend,
		endis, enangle, enrise, enrad, enrise2,exdis, exangle, exrise, exrad, exrise2,clrdis, rptdis, rtrdis, lrtrdis, npas,
		cutcpdst, fdrt1, fdrt2, fdrt3, fdrt4, fdrt5, fdrt6 , fdrt[6];
	UM_int4 fnorm;
	prfpar (&fthick1, &fthick2, &toolaxangle, &tiltanglebegin, &tiltangleend, &tildistbegin, &tiltdistend,
		&endis, &enangle, &enrise, &enrad, &enrise2,&exdis, &exangle, &exrise, &exrad, &exrise2, &clrdis, &rptdis, &rtrdis, &lrtrdis,
		&npas, &cutcpdst, &fdrt1, &fdrt2, &fdrt3, &fdrt4, &fdrt5, &fdrt6 , &fnorm);

	fdrt[0] = fdrt1;
	fdrt[1] = fdrt2;
	fdrt[2] = fdrt3;
	fdrt[3] = fdrt4;
	fdrt[4] = fdrt5;
	fdrt[5] = fdrt6;

	NclxMotGetFeedrate(&fedrat);
	feedrate = fedrat.base_feedrate;
	ncl_val2str (feedrate, Sfeed_valuestr);
/*
.....Initialize the Engraving settings
*/
	if (!Sform_init)
	{
		Sdscol = NCLX_SEA_GREEN;
		ncl_sprintf(Spfthk_str,&fthick2,1);
		Slpdpt = 0;
		Slptpl[0] = '\0';
		Slpdcol = NCLX_TAN;

		Slpdps = npas;
		ncl_sprintf(Slpdpst_str,&npas,1);
	}
	Tdscol = Sdscol;
	strcpy(Tpfthk_str, Spfthk_str);
	Tlpdpt = Slpdpt;
	strcpy(Tlptpl,Slptpl);
	Tlpdcol = Slpdcol;

	Slpdps = npas;
	Tlpdps = Slpdps;
	ncl_sprintf(Slpdpst_str,&npas,1);
	strcpy(Tlpdpst_str, Slpdpst_str);
/*
.....Initialize the Positioning settings
*/
	if (!Sform_init)
	{
		Spoclr = clrdis;
		ncl_sprintf(Spocpln,&clrdis,1);
		Spoccol = NCLX_LT_BLUE;
		Sporap = rptdis;
		ncl_sprintf(Spoppln,&rptdis,1);
		Spopcol = NCLX_BROWN;
		Sporet = rtrdis;
		ncl_sprintf(Sporpln,&rtrdis,1);
		Sporcol = NCLX_GREY;
		ncl_sprintf(Sportl_str,&lrtrdis,1);
	}

	Spoclr = clrdis;
	ncl_sprintf(Spocpln,&clrdis,1);
	Sporap = rptdis;
	ncl_sprintf(Spoppln,&rptdis,1);
	Sporet = rtrdis;
	ncl_sprintf(Sporpln,&rtrdis,1);
	ncl_sprintf(Sportl_str,&lrtrdis,1);

	Tpoclr = Spoclr;
	strcpy(Tpocpln,Spocpln);
	Tpoccol = Spoccol;
	Tporap = Sporap;
	strcpy(Tpoppln,Spoppln);
	Tpopcol = Spopcol;
	Tporet = Sporet;
	strcpy(Tporpln,Sporpln);
	Tporcol = Sporcol;
	strcpy(Tportl_str, Sportl_str);
/*
.....Initialize Feed Rate settings
*/
	if (!Sform_init)
	{
		for (i=0;i<4;i++)
		{
			Sfopt[i] = 0.;
			ncl_sprintf(Sfeed_str[i],&fdrt[i],1);
		}
		Sfopt[1] = Sfopt[2] = 2;
	}
	for (i=0;i<4;i++)
	{
		ncl_sprintf(Sfeed_str[i],&fdrt[i],1);
		Tfopt[i] = Sfopt[i];
		Ssav_fchc[i] = Sfopt[i];
		strcpy(Tfeed_str[i], Sfeed_str[i]);
		if ((Tfopt[i]==0)||(Tfopt[i]==2))
			Tfeed_str[i][0] = '\0';
	}
	/*strcpy(Sav_valuestr1[0], Sfeed_valuestr);
	strcpy(Sav_valuestr1[1], Sfeed_valuestr);
	strcpy(Sav_valuestr2[0], Sfeed_valuestr);
	strcpy(Sav_valuestr2[1], Sfeed_valuestr);
	strcpy(Sav_valuestr3[0], Sfeed_valuestr);
	strcpy(Sav_valuestr3[1], Sfeed_valuestr);
	strcpy(Sav_valuestr4[0], Sfeed_valuestr);
	strcpy(Sav_valuestr4[1], Sfeed_valuestr);*/

	strcpy(Sav_valuestr1[0], Sfeed_str[0]);
	strcpy(Sav_valuestr1[1], Sfeed_str[0]);
	strcpy(Sav_valuestr2[0], Sfeed_str[1]);
	strcpy(Sav_valuestr2[1], Sfeed_str[1]);
	strcpy(Sav_valuestr3[0], Sfeed_str[2]);
	strcpy(Sav_valuestr3[1], Sfeed_str[2]);
	strcpy(Sav_valuestr4[0], Sfeed_str[3]);
	strcpy(Sav_valuestr4[1], Sfeed_str[3]);
/*
.....Initialize geometry color
*/
	Tgeotog = UN_unused_geo_flag;
	Tgeocol = UN_unused_geo_color;
	Sgeo_redraw = UU_FALSE;
/*
.....Mark variables as being initialized
*/
	Sform_init = UU_TRUE;
/*
.....Initialize geomtry variables
*/
	Sgeo_init = UU_FALSE;
	S_init_geo(&Sgtpl,Tlptpl,Tlpdcol);

	Sgcp.key = Sgrap.key = Sgret.key = 0;
	Sgcp.color = Sgrap.color = Sgret.color = -1;
	Sgcp.label[0] = Sgrap.label[0] = Sgret.label[0] = '\0';
	S_init_geo(&Sgcp,Tpocpln,Tpoccol);
	S_init_geo(&Sgrap,Tpoppln,Tpopcol);
	S_init_geo(&Sgret,Tporpln,Tporcol);
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
	int i;
/*
.....Set the Section colors
*/
	S_section_changed(Smode[Curve],Sred,UU_TRUE,UU_FALSE);
	ud_frm_enable_ok(UU_FALSE);
	if (*fieldno != -1)
	{
		for (i=Levels;i<=Feeds;i++)
			S_section_changed(Smode[i],Sblack,UU_FALSE,UU_FALSE);
	}
/*
.....Set the mandatory fields to red
*/
	ud_dispfrm_set_attribs(0,FDCV1,UM_WHITE,UM_RED);
/*
.....Set Pick buttons to correct color
*/
	ud_dispfrm_set_attribs(0,FDDP3,UM_BLACK,Tlpdcol);
	ud_dispfrm_set_attribs(0,FPCL3,UM_BLACK,Tpoccol);
	ud_dispfrm_set_attribs(0,FPRP3,UM_BLACK,Tpopcol);
	ud_dispfrm_set_attribs(0,FPRT3,UM_BLACK,Tporcol);
	S_enable_buttons();
	return(UD_FLDOK);
}
/*********************************************************************
**    I_FUNCTION     : S_save_form()
**       Saves the Profile Machining form variables.
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
.....Save the Engraving settings
*/
	Sdscol = Tdscol;
	strcpy(Spfthk_str, Tpfthk_str);
	Slpdpt = Tlpdpt;
	strcpy(Slptpl,Tlptpl);
	Slpdcol = Tlpdcol;
	Slpdps = Tlpdps;
	strcpy(Slpdpst_str, Tlpdpst_str);
/*
.....Save the Positioning settings
*/
	Spoclr = Tpoclr;
	strcpy(Spocpln,Tpocpln);
	Spoccol = Tpoccol;
	Sporap = Tporap;
	strcpy(Spoppln,Tpoppln);
	Spopcol = Tpopcol;
	Sporet = Tporet;
	strcpy(Sporpln,Tporpln);
	Sporcol = Tporcol;
	strcpy(Sportl_str, Tportl_str);
/*
.....Save the Feed Rate settings
*/
	for (i=0;i<4;i++)
	{
		Sfopt[i] = Tfopt[i];
		strcpy(Sfeed_str[i], Tfeed_str[i]);
	}
/*
.....Save the modal settings
*/
	UN_unused_geo_flag = Tgeotog;
	UN_unused_geo_color = Tgeocol;
	nclu_save_preview_modals();
}

/*********************************************************************
**    I_FUNCTION     : S_form_invis()
**       Takes down all Profile forms.
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
**       Redisplays all Profile forms.
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
**    S_FUNCTION     :  S_select_geo(sfpt,sflst,mask,multi,prmno,color,frm,
**                                   fieldno,label)
**       Routine to select geometry for the Profile form.  The associated
**       text field will be updated with the geometry label and
**       the geometry will be highlighted.
**    PARAMETERS
**       INPUT  :
**          sfpt     Pointer to selected geometry structure for single entity.
**          sflst    Pointer to selected geometry structure list when
**                   multiple entities can be picked (multi).
**          mask     Picking mask.
**          multi    UU_TRUE = Allow selection of multiple entities.
**          prmno    Prompt number to use while in pick mode.
**          color    Color to highlight the picked entity.
**                   -1 = Don't highlight entity.
**          frm      Form number that the field resides on. 0 = Main form.
**          fieldno  Text field to update with label of selected entity.
**                   -1 = No field to updated.
**          label    Text buffer that belongs to 'fieldno'.
**       OUTPUT :
**          sfpt     Updated geometry structure.
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
	S_form_invis();
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
		if (!multi)
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
	if (!multi)
	{
/*
........Unhighlight the previous selection
*/
		S_unhilite_entity(sfpt);
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
	}
/*
.....End of routine
.....Redisplay form
*/
done:;
	ud_unlimit();
	S_form_vis();
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
**    S_FUNCTION     :  S_init_geo(sfpt,label,color)
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
	S_unhilite_entity(&Sgcp);
	S_unhilite_entity(&Sgrap);
	S_unhilite_entity(&Sgret);
	S_unhilite_entity(&Sgtpl);
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
	n = UU_LIST_LENGTH(&Sglst);
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sglst);
	for (i=0;i<n;i++) S_add_key(which,mgeo[i].key);
	S_add_key(which,Sgcp.key);
	S_add_key(which,Sgret.key);
	S_add_key(which,Sgtpl.key);
	S_add_key(which,Sgrap.key);
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
**			Builds and outputs the PROFIL command.
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
	int i,j,nc,nline,slen,listlen;
	char buf[80];
	NCL_cmdbuf cmdbuf;
	UM_sgeo *geo;
	UM_f77_str outcmd;
	if (flag)
		NCL_preview_mot = 0;
	else
		NCL_preview_mot = 1;
/*
.....Initialize command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
/*
.....Loop through selections
*/
	geo = (UM_sgeo *)UU_LIST_ARRAY(&Sglst);;
	listlen = UU_LIST_LENGTH(&Sglst);
	if (listlen == 0) return (UU_FAILURE);
	for (i=0;i<listlen;i++)
	{
/*
.....PROFIL command
*/
		if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_profil, NCL_nocomma);
/*
.....Drive Surface parameters
*/
		ncl_add_token(&cmdbuf,geo[i].label,NCL_comma);
		ncl_get_scalar_value(Tpfthk_str, &Tpfthk);
		if (Tpfthk != 0.)
		{
			ncl_add_token(&cmdbuf,NCL_ps,NCL_comma);
			ncl_sprintf(buf,&Tpfthk,1);
			ncl_add_token(&cmdbuf,buf,NCL_comma);
		}
/*
.....STEP parameters
*/
		if (Tlpdpt != 0)
		{
			ncl_add_token(&cmdbuf,NCL_run_step,NCL_comma);
			if (Tlpdpt == 1 || Tlpdpt == 2)
			{
				nc = strlen(Tlptpl);
				ul_strip_blanks(Tlptpl,&nc);
				if (nc != 0) ncl_add_token(&cmdbuf,Tlptpl,NCL_comma);
			}
			if (Tlpdpt == 3 || Tlpdps == 0)
			{
				ncl_add_token(&cmdbuf,NCL_pass,NCL_comma);
				if (Tlpdpt == 3)
				{
					nc = strlen(Tlptpl);
					ul_strip_blanks(Tlptpl,&nc);
					ncl_add_token(&cmdbuf,Tlptpl,NCL_comma);
				}
				else
				{
					ncl_add_token(&cmdbuf,Tlpdpst_str,NCL_comma);
				}
			}
			ncl_get_scalar_value(Tlpdpst_str, &Tlpdpst);
			if (Tlpdps == 1 && Tlpdpst != 0.)
				ncl_add_token(&cmdbuf,Tlpdpst_str,NCL_comma);
		}
/*
.....Rapto parameters
*/
		nc = strlen(Tpoppln);
		if (Tporap != 0 && nc != 0)
		{
			ncl_add_token(&cmdbuf,NCL_rapto,NCL_comma);
			if (Tporap == 3) ncl_add_token(&cmdbuf,NCL_incr,NCL_comma);
			ul_strip_blanks(Tpoppln,&nc);
			ncl_add_token(&cmdbuf,Tpoppln,NCL_comma);
		}
/*
.....RETRCT parameters
*/
		ncl_get_scalar_value(Tportl_str, &Tportl);
		if (Tporet != 0 || Tportl != 0.)
		{
			nc = strlen(Tporpln);
			ncl_add_token(&cmdbuf,NCL_retrct,NCL_comma);
			if (Tporet == 0)
				ncl_add_token(&cmdbuf,NCL_off,NCL_comma);
			else if (Tporet == 1)
				ncl_add_token(&cmdbuf,NCL_on,NCL_comma);
			else
			{
				if (Tporet == 4) ncl_add_token(&cmdbuf,NCL_incr,NCL_comma);
				nc = strlen(Tporpln);
				ul_strip_blanks(Tporpln,&nc);
				ncl_add_token(&cmdbuf,Tporpln,NCL_comma);
			}
			ncl_get_scalar_value(Tportl_str, &Tportl);
			if (Tportl != 0.)
			{
				ncl_add_token(&cmdbuf,NCL_rtrcto,NCL_comma);
				ncl_add_token(&cmdbuf,Tportl_str,NCL_comma);
			}
		}
/*
.....Clearance surface parameters
*/
		nc = strlen(Tpocpln);
		if (Tpoclr != 0 && nc != 0)
		{
			ncl_add_token(&cmdbuf,NCL_clrsrf,NCL_comma);
			if (Tpoclr == 3) ncl_add_token(&cmdbuf,NCL_incr,NCL_comma);
			ul_strip_blanks(Tpocpln,&nc);
			ncl_add_token(&cmdbuf,Tpocpln,NCL_comma);
		}
/*
.....Feed rate parameters
*/
		if (Tfopt[0] != 0 || Tfopt[1] != 2 || Tfopt[2] != 2 || Tfopt[3] != 0)
		{
			ncl_add_token(&cmdbuf,NCL_feedrat,NCL_comma);
			for (j=0;j<4;j++)
			{
				switch (Tfopt[j])
				{
				case 0:
					strcpy(buf,"0.");
					break;
				case 1:
					strcpy(buf, Tfeed_str[j]);
					break;
				case 2:
					strcpy(buf,NCL_rapid);
					break;
				case 3:
					sprintf(buf, "-%s", Tfeed_str[j]);
					break;
				}
				ncl_add_token(&cmdbuf,buf,NCL_comma);
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
		if (Spreviewflg && !Schgmade) ncl_call_input(&cmdbuf,UU_TRUE);
/*
.....Execute command if a change has been made since the last preview or no
.....preview was created
*/
		else Serrflg = ncl_call(&cmdbuf);
	}
	NCL_preview_mot = 1;
	return(UU_SUCCESS);
}
