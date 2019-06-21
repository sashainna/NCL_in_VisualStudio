/*********************************************************************
**    NAME         :  nuanote.c
**       CONTAINS:
**         nclu_notes
**         nclu_notes_attrib
**    COPYRIGHT 2008 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nunotes.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:11
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclmodals.h"
#include "nclx.h"
#include "mdcpln.h"
#include "mdrel.h"
#include "mfort.h"
#include "modef.h"
#include "ncl.h"
#include "nclfc.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "xenv1.h"
#include "atext.h"

#include "class.h"
#include "mdpick.h"
/*
.....Form fields
*/
#define FLAB 0
#define FCV1 1
#define FCV2 2
#define FATT 3
#define FPT1 4
#define FPT2 5
#define FLET 6
#define FLIN 7
#define FTYP 8
#define FSF1 9
#define FSF2 10
#define FVC1 11
#define FVC2 12
#define FPA1 13
#define FPA2 14
#define FPA3 15
#define FTXT 16
#define FVIW 17
#define FPRE 18
#define FAPP 19
/*
.....Main variables
*/
#define MAXTXT 1024
static int Sorigin,Smode,Sattach;
static char Slabel[65],Scvlab[65],Sorgpt[65],Sletlab[65],Slinlab[65];
static char Ssflab[65],Svec[65],Sattpt[65],Stext[MAXTXT+1];
static int Torigin,Tmode,Tattach;
static char Tlabel[65],Tcvlab[65],Torgpt[65],Tletlab[65],Tlinlab[65];
static char Tsflab[65],Tvec[65],Tattpt[65],Ttext[MAXTXT+1];
static UU_LOGICAL Sform_init = UU_FALSE;
/*
.....Main form callback routines
*/
static UD_FSTAT OnSelect(),OnTog(),OnAction();
/*
.....Local routines
*/
static void S_init_form(),S_save_form(),S_hilite_entity(),S_unhilite_entity();
static void S_init_traverse(),S_form_invis(),S_form_vis(),S_init_geo();
static void S_delete_preview();
static int S_select_geo(),S_build_cmd(),S_verify_form(),S_pick_geo();

extern char uw_color_name[64][96];
/*********************************************************************
**    E_FUNCTION     : nclu_notes()
**          Controlling routine for the Project Onto Surface form.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_notes()
{
	UU_LOGICAL cmdreject;
/*
.....Set up form fields
*/
	static char traverse[]     = {1,1,1,1,1,1, 1,1, 1,0,0,0,0,0,0,0, 1, 1,1,1};
	static char display[]      = {1,1,1,1,1,1, 1,1, 1,1,1,0,0,1,0,0, 1, 1,1,1};
	static UD_METHOD methods[] = {
		UU_NULL,OnSelect,UU_NULL, OnTog,OnSelect,UU_NULL,
		UU_NULL,UU_NULL,
		OnTog,OnSelect,UU_NULL, OnSelect,UU_NULL, OnTog,OnSelect,UU_NULL,
		UU_NULL,
		OnAction,OnAction,OnAction};
	static char called[]       = {6,6,6,6,6,6, 6,6, 6,6,6,6,6,6,6,6, 6, 6,6,6};
	static int *ans[] = {
		(int *)&Tlabel,UU_NULL,(int *)&Tcvlab,&Torigin,UU_NULL,(int *)&Torgpt,
		(int *)&Tletlab,(int *)&Tlinlab,&Tmode,UU_NULL,(int *)&Tsflab,UU_NULL,
		(int *)&Tvec,&Tattach,UU_NULL,(int *)&Tattpt,(int *)&Ttext,UU_NULL,
		UU_NULL,UU_NULL};

	int status;
/*
.....Initialize form answers
*/
	S_init_form();
/*
.....Trap Reject Op
*/
	UD_MARK (cmdreject, UU_FALSE);
	if (cmdreject != 0) goto done;
/*
.....Initialize form fields
*/
repeat:;
	S_init_traverse(display,traverse);
/*
.....Get the Form input
*/
	status = ud_form1("nanote.frm",ans,ans,methods,called,display,traverse);
/*
.....Delete last previewed projection
*/
	S_delete_preview();
	if (status == -1) goto done;
/*
.....Verify form fields
*/
	status = S_verify_form();
	if (status != UU_SUCCESS) goto repeat;
/*
.....Save the form settings
*/
	S_save_form();
/*
.....Output the command
*/
	status = S_build_cmd(UU_TRUE);
/*
.....End of routine
*/
done:;
/*
.....Restore the entity colors
*/
	UD_UNMARK(cmdreject);
	return;
}

/*********************************************************************
**    I_FUNCTION     : OnSelect()
**			Geometry selection routine.
**    PARAMETERS   
**       INPUT  : fieldno = Form field which initiated this call.
**			         val     = Not used, form initiated through pushbutton.
**			         stat    = Not used.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT  OnSelect(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status;
/*
.....Process geometry selection buttons
*/
	switch (*fieldno)
	{
	case FCV1:
		status = S_pick_geo(Tcvlab,UD_ncl_lnci,481,FCV2);
		break;
	case FPT1:
		status = S_pick_geo(Torgpt,UD_ncl_ptpv,489,FPT2);
		break;
	case FSF1:
		status = S_pick_geo(Tsflab,UD_ncl_allsfpl,483,FSF2);
		break;
	case FVC1:
		status = S_pick_geo(Tvec,UD_ncl_vepv,218,FVC2);
		break;
	case FPA2:
		status = S_pick_geo(Tattpt,UD_ncl_ptpv,216,FPA3);
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnTog()
**			Choice field setting routine.
**    PARAMETERS   
**       INPUT  : fieldno = Form field which initiated this call.
**			         val     = Not used, form initiated through pushbutton.
**			         stat    = Not used.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT  OnTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	UU_LOGICAL dis[4];
/*
.....Initialize routine
*/
	for (i=0;i<4;i++) dis[i] = UU_FALSE;
/*
.....Process toggle fields
*/
	switch (*fieldno)
	{
/*
........Attach type
*/
	case FATT:
		Torigin = *(val->frmint);
		if (Torigin == 0)
		{
			ud_set_traverse_mask(FPT1,UU_TRUE);
			ud_set_traverse_mask(FPT2,UU_TRUE);
		}
		else
		{
			ud_set_traverse_mask(FPT1,UU_FALSE);
			ud_set_traverse_mask(FPT2,UU_FALSE);
		}
		break;
/*
........Projection type
*/
	case FTYP:
		Tmode = *(val->frmint);
		break;
/*
........Projection Attach type
*/
	case FPA1:
		Tattach = *(val->frmint);
		break;
	}
/*
.....Set display masks
*/
	switch (Tmode)
	{
	case 1:
		dis[0] = UU_TRUE;
		break;
	case 2:
		dis[0] = UU_TRUE;
		dis[1] = UU_TRUE;
		break;
	case 3:
	case 4:
	case 5:
		dis[0] = UU_TRUE;
		dis[1] = UU_TRUE;
		dis[2] = UU_TRUE;
		if (Tattach == 4) dis[3] = UU_TRUE;
		break;
	}
/*
.....Set display & traverse settings
*/
	ud_set_display_mask(UD_INPUTF,FSF1,dis[0]);
	ud_set_traverse_mask(FSF1,dis[0]);
	ud_set_display_mask(UD_INPUTF,FSF2,dis[0]);
	ud_set_traverse_mask(FSF2,dis[0]);
	ud_set_display_mask(UD_INPUTF,FVC1,dis[1]);
	ud_set_traverse_mask(FVC1,dis[1]);
	ud_set_display_mask(UD_INPUTF,FVC2,dis[1]);
	ud_set_traverse_mask(FVC2,dis[1]);
	ud_set_display_mask(UD_INPUTF,FPA1,dis[2]);
	ud_set_traverse_mask(FPA1,dis[2]);
	ud_set_display_mask(UD_INPUTF,FPA2,dis[3]);
	ud_set_traverse_mask(FPA2,dis[3]);
	ud_set_display_mask(UD_INPUTF,FPA3,dis[3]);
	ud_set_traverse_mask(FPA3,dis[3]);
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
	int status;

	switch (*fieldno)
	{
/*
.....Enter viewing mode
*/
	case FVIW:
		ud_form_invis();
		uz_dyn_mouse();
		ud_form_vis();
		break;
/*
.....Preview command
*/
	case FPRE:
		status = S_verify_form();
		if (status != UU_SUCCESS) break;
		S_delete_preview();
		if (status == -1) break;
		status = S_build_cmd(UU_FALSE);
		break;
/*
.....Apply - Output command
*/
	case FAPP:
		status = S_verify_form();
		if (status != UU_SUCCESS) break;
		S_delete_preview();
		if (status == -1) break;
		S_save_form();
		status = S_build_cmd(UU_TRUE);
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_init_form(type)
**			Initializes the Annotation form variables.
**    PARAMETERS   
**       INPUT  :
**          type   = 0 = Projecting a pattern, 1 = spline,
**                   3 = Surface spline.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS :
**			none
**    WARNINGS     : none
*********************************************************************/
static void S_init_form(type)
int type;
{
/*
.....Initialize the form variables
*/
	Slabel[0] = '\0';
	Sorgpt[0] = '\0';
	Stext[0] = '\0';

	if (!Sform_init)
	{
		Sorigin = 0;
		Scvlab[0] = '\0';
		Sletlab[0] = '\0';
		Slinlab[0] = '\0';
		Smode = 0;
		Ssflab[0] = '\0';
		Svec[0] = '\0';
		Sattach = 0;
		Sattpt[0] = '\0';
		Sform_init = UU_TRUE;
	}
/*
.....Work with temporary variables
*/
	strcpy(Tlabel,Slabel);
	strcpy(Torgpt,Sorgpt);
	strcpy(Ttext,Stext);
	Torigin = Sorigin;
	strcpy(Tcvlab,Scvlab);
	strcpy(Tletlab,Sletlab);
	strcpy(Tlinlab,Slinlab);
	Tmode = Smode;
	strcpy(Tsflab,Ssflab);
	strcpy(Tvec,Svec);
	Tattach = Sattach;
	strcpy(Tattpt,Sattpt);
}

/*********************************************************************
**    I_FUNCTION     : S_init_traverse(display,traverse)
**			Initializes the Annotation form field traversal settings.
**    PARAMETERS   
**       INPUT  :
**          none.
**       OUTPUT :  
**          display   = Form field display settings.
**          traverse  = Form field traverse settings.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS :
**			none
**    WARNINGS     : none
*********************************************************************/
static void S_init_traverse(display,traverse)
char display[],traverse[];
{
/*
.....Initialize Geometry fields
*/
	if (Sorigin == 0)
		traverse[FPT1] = traverse[FPT2] = 1;
	else
		traverse[FPT1] = traverse[FPT2] = 0;
/*
.....Initialize projection type fields
*/
	traverse[FSF1] = traverse[FSF2] = 0;
	display[FVC1] = traverse[FVC1] = 0;
	display[FVC2] = traverse[FVC2] = 0;
	display[FPA1] = traverse[FPA1] = 0;
	display[FPA2] = traverse[FPA2] = 0;
	display[FPA3] = traverse[FPA3] = 0;
	switch (Smode)
	{
/*
........Normal
*/
	case 1:
		traverse[FSF1] = traverse[FSF2] = 1;
		break;
/*
........Vector
*/
	case 2:
		traverse[FSF1] = traverse[FSF2] = 1;
		display[FVC1] = traverse[FVC1] = 1;
		display[FVC2] = traverse[FVC2] = 1;
		break;
/*
........Wrap
........Revolve
........Radial
*/
	case 3:
	case 4:
	case 5:
		traverse[FSF1] = traverse[FSF2] = 1;
		display[FVC1] = traverse[FVC1] = 1;
		display[FVC2] = traverse[FVC2] = 1;
		display[FPA1] = traverse[FPA1] = 1;
		if (Sattach == 4)
		{
			display[FPA2] = traverse[FPA2] = 1;
			display[FPA3] = traverse[FPA3] = 1;
		}
		break;
	}
}

/*********************************************************************
**    I_FUNCTION     : S_verify_form()
**			Verifies that the form data is correct.
**    PARAMETERS   
**       INPUT  :
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS :
**			none
**    WARNINGS     : none
*********************************************************************/
static int S_verify_form()
{
	UD_DDATA fdat;
	fdat.frmstr = Ttext;
	ud_get_field(FTXT,fdat);
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_save_form()
**			Saves the Projection form variables.
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT :  none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_save_form()
{
/*
.....Save the form variables
*/
	strcpy(Slabel,Tlabel);
	strcpy(Sorgpt,Torgpt);
	strcpy(Stext,Ttext);
	Sorigin = Torigin;
	strcpy(Scvlab,Tcvlab);
	strcpy(Sletlab,Tletlab);
	strcpy(Slinlab,Tlinlab);
	Smode = Tmode;
	strcpy(Ssflab,Tsflab);
	strcpy(Svec,Tvec);
	Sattach = Tattach;
	strcpy(Sattpt,Tattpt);
}

/*********************************************************************
**    I_FUNCTION     : S_delete_preview()
**			Deletes the temporary geometry created by the Preview button.
**    PARAMETERS   
**       INPUT  :
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS :
**			none
**    WARNINGS     : none
*********************************************************************/
static void S_delete_preview()
{
	int nc;
	char label[65];
	UU_KEY_ID key;
	UM_f77_str f77_str;
/*
.....See if preview geometry exists
*/
	strcpy(label,"@UZTMP");
	nc = strlen(label);
	UM_init_f77_str(f77_str,label,nc);
	getkey(UM_addr_of_f77_str(f77_str),&key);
/*
.....Delete preview geometry
*/
	if (key != 0) dlgeom(&key);
}

/*********************************************************************
**    I_FUNCTION     : S_build_cmd(flag)
**			Builds and outputs the ANOTE command.
**    PARAMETERS   
**       INPUT  : flag    = UU_TRUE = output command to source file.
**			                   UU_FALSE = Preview command only.
**       OUTPUT : none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS :
**			Creates temporary geometry that must be deleted later when
**			flag = UU_FALSE.
**    WARNINGS     : none
*********************************************************************/
static int S_build_cmd(flag)
UU_LOGICAL flag;
{
#define MAXC 40
	int i,nc,inc,stat;
	UU_REAL ptx[3];
	char buf[80],sbuf[80],tstr[MAXTXT+1],*p;
	NCL_cmdbuf cmdbuf,cmdtmp;
/*
.....Initialize command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
/*
.....ANOTE/ command
*/
	if (flag)
	{
		nc = strlen(Tlabel);
		ul_strip_blanks(Tlabel,&nc);
		if (nc != 0)
		{
			ncl_add_token(&cmdbuf,Tlabel,NCL_nocomma);
			ncl_add_token(&cmdbuf,"=",NCL_nocomma);
		}
	}
	else
		ncl_add_token(&cmdbuf, "*@UZTMP=", NCL_nocomma);
	ncl_add_token(&cmdbuf,NCL_anote,NCL_nocomma);
/*
.....Text
*/
	nc = strlen(Ttext);
	inc = 0;
	for (i=0;i<nc;i++)
	{
		if (Ttext[i] == '\n')
		{
			tstr[inc++] = '\\';
			tstr[inc++] = 'n';
		}
		else if (Ttext[i] != '\r')
			tstr[inc++] = Ttext[i];
	}
	tstr[inc] = '\0';
	ul_cut_string(tstr,inc);
/*
........Output in single command
*/
	if (inc <= MAXC)
	{
		ncl_add_token(&cmdbuf,"\"",NCL_nocomma);
		ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
		ncl_add_token(&cmdbuf,"\"",NCL_comma);
	}
/*
........Text string is too long
........Output text definitions first
*/
	else
	{
		p = tstr;
		buf[MAXC] = '\0';
		nc = inc;
		inc = 1;
		ncl_init_cmdbuf(&cmdtmp);
/*
...........Output text commands
*/
		do
		{
			if (!flag) ncl_add_token(&cmdtmp,"*",NCL_nocomma);
			if (nc > 40) strncpy(buf,p,MAXC);
			else strcpy(buf,p);
			sprintf(sbuf,"@TX%d=\"%s\"",inc++,buf);
			ncl_add_token(&cmdtmp,sbuf,NCL_nocomma);
			ncl_add_cmdbuf(&cmdtmp);
			p += MAXC;
			nc -= MAXC;
		} while (nc > 0);
		ncl_set_cmdmode(UU_TRUE);
		ncl_call(&cmdtmp);
/*
...........Create combined text variable
*/
		if (!flag) ncl_add_token(&cmdtmp,"*",NCL_nocomma);
		sprintf(buf,"@TX%d=",inc);
		ncl_add_token(&cmdtmp,buf,NCL_nocomma);
		for (i=1;i<inc;i++)
		{
			if (i > 1) ncl_add_token(&cmdtmp,"&",NCL_nocomma);
			sprintf(sbuf,"@TX%d",i);
			ncl_add_token(&cmdtmp,sbuf,NCL_nocomma);
		}
		ncl_add_cmdbuf(&cmdtmp);
		ncl_set_cmdmode(UU_TRUE);
		ncl_call(&cmdtmp);
/*
...........Add text variable to command
*/
		sprintf(buf,"@TX%d",inc);
		ncl_add_token(&cmdbuf,buf,NCL_comma);
	}
/*
.....At position
*/
	switch (Torigin)
	{
	case 0:
		nc = strlen(Torgpt);
		ul_strip_blanks(Torgpt,&nc);
		if (nc != 0)
		{
			ncl_add_token(&cmdbuf,NCL_at,NCL_comma);
			ncl_add_token(&cmdbuf,Torgpt,NCL_comma);
		}
		break;
	case 1:
		ncl_add_token(&cmdbuf,NCL_at,NCL_comma);
		ncl_add_token(&cmdbuf,NCL_letter,NCL_comma);
		break;
	case 2:
		ncl_add_token(&cmdbuf,NCL_at,NCL_comma);
		ncl_add_token(&cmdbuf,NCL_line,NCL_comma);
		break;
	}
/*
.....Next positions
*/
	nc = strlen(Tletlab);
	ul_strip_blanks(Tletlab,&nc);
	if (nc != 0)
	{
		ncl_add_token(&cmdbuf,NCL_letter,NCL_comma);
		ncl_add_token(&cmdbuf,Tletlab,NCL_comma);
	}
	nc = strlen(Tlinlab);
	ul_strip_blanks(Tlinlab,&nc);
	if (nc != 0)
	{
		ncl_add_token(&cmdbuf,NCL_line,NCL_comma);
		ncl_add_token(&cmdbuf,Tlinlab,NCL_comma);
	}
/*
.....Along a curve
*/
	nc = strlen(Tcvlab);
	ul_strip_blanks(Tcvlab,&nc);
	if (nc != 0)
	{
		ncl_add_token(&cmdbuf,NCL_curve,NCL_comma);
		ncl_add_token(&cmdbuf,Tcvlab,NCL_comma);
	}
/*
.....Projection
*/
	if (Tmode != 0)
	{
		ncl_add_token(&cmdbuf,NCL_projct,NCL_comma);
/*
........Vector
*/
		if (Tmode != 1)
		{
			stat = ul_to_reals(ptx,&nc,3,Tvec);
			if (stat == UU_SUCCESS && nc == 3)
			{
				ncl_sprintf(buf,ptx,3);
				sprintf(sbuf,"(VE/%s)",buf);
				ncl_add_token(&cmdbuf,sbuf,NCL_comma);
			}
			else
			{
				nc = strlen(Tvec);
				ul_strip_blanks(Tvec,&nc);
				if (nc != 0) ncl_add_token(&cmdbuf,Tvec,NCL_comma);
			}
		}
/*
.....Surface
*/
		ncl_add_token(&cmdbuf,Tsflab,NCL_comma);
/*
.....Projection type
*/
		if (Tmode == 3)
			ncl_add_token(&cmdbuf,NCL_wrap,NCL_comma);
		else if (Tmode == 4)
			ncl_add_token(&cmdbuf,NCL_revolv,NCL_comma);
		else if (Tmode == 5)
			ncl_add_token(&cmdbuf,NCL_radial,NCL_comma);
/*
.....Start point
*/
		if (Tmode >= 3)
		{
			if (Tattach == 1)
				ncl_add_token(&cmdbuf,NCL_middle,NCL_comma);
			else if (Tattach == 2)
				ncl_add_token(&cmdbuf,NCL_end,NCL_comma);
			else if (Tattach == 3)
				ncl_add_token(&cmdbuf,NCL_center,NCL_comma);
			else if (Tattach == 4)
			{
				ncl_add_token(&cmdbuf,NCL_at,NCL_comma);
				stat = ul_to_reals(ptx,&nc,3,Tattpt);
				if (stat == UU_SUCCESS && (nc == 2 || nc == 3))
				{
					ncl_sprintf(buf,ptx,nc);
					sprintf(sbuf,"(PT/%s)",buf);
					ncl_add_token(&cmdbuf,sbuf,NCL_comma);
				}
				else
				{
					nc = strlen(Tattpt);
					ul_strip_blanks(Tattpt,&nc);
					if (nc != 0) ncl_add_token(&cmdbuf,Tattpt,NCL_comma);
				}
			}
		}
	}
/*
.....Call the command
*/
	if (!flag) ua_save_text_pos();
	ncl_set_cmdmode(UU_TRUE);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
	if (!flag) ua_restore_text_pos();
	return(UU_SUCCESS);
}

/*********************************************************************
**    S_FUNCTION     :  S_pick_geo(label,mask,prmno,fieldno)
**       Routine to select geometry for the Project form.  The associated
**       text field will be updated with the geometry label.
**    PARAMETERS
**       INPUT  :
**          mask     Picking mask.
**          prmno    Prompt number to use while in pick mode.
**          fieldno  Text field to update with label of selected entity.
**                   -1 = No field to updated.
**       OUTPUT :
**          label    Text buffer that belongs to 'fieldno'.
**    RETURNS      : UU_SUCCESS if an entity is picked.  UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_pick_geo(label,mask,prmno,fieldno)
char *label;
unsigned int *mask;
int prmno,fieldno;
{
	int numint,iret;
	struct NCL_fixed_databag e;
	UM_PLOCREC pick;
	UU_LOGICAL cmdreject;
/*
.....Take down form
*/
	iret = UU_SUCCESS;
	ud_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK (cmdreject, UU_TRUE);
	if (cmdreject != 0) goto failed;
/*
.....Get the next geometry selection
*/
	ud_lgeo(UU_TRUE,mask);
	ua_dl_pldas(UD_DASPCKLOC,UA_NCL,prmno,&pick,1,&numint,1);
	if (numint == 0) goto failed;
	e.key = um_get_pickkey(&(pick.pent),1);
	if (e.key == 0)
		strcpy(label,pick.ploc.label);
	else
	{
		if (ncl_retrieve_data_fixed(&e) != 0) goto done;
/*
.....Store the entity's data
*/
		ncl_get_label(&e,label);
/*
.....Update the text field with the entity label
*/
	}
	if (fieldno != -1)
		ud_update_answer(fieldno,(int *)label);
/*
.....End of routine
.....Redisplay form
*/
done:;
	ud_unlimit();
	ud_form_vis();
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
**    E_FUNCTION     : nclu_notes_attrib(flag)
**          Sets/modifies the attributes for CAM annotation.
**    PARAMETERS   
**       INPUT  : 
**          flag    0 = Set default attributes, 1 = Modify attributes.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_notes_attrib(flag)
int flag;
{
#define MAXKEY 200
	int i,num1,status;
	UU_LOGICAL first;
	UU_KEY_ID keys[MAXKEY];
	NCL_cmdbuf cmdbuf;
	ATXT_FRM attr,save_attr;
	struct UA_txt_rec note;
	struct UA_txtattr_rec note_attr;
/*
.....Set the default annotation attributes
*/
	if (flag == 0)
	{
		attr.color = UA_txtattr.color;
		strcpy(attr.fontname,UA_txtattr.fontname);
		attr.prec = UA_txtattr.prec;
		attr.expn = UA_txtattr.expn;
		attr.spacing = UA_txtattr.spacing;
		attr.height = UA_txtattr.height;
		attr.tangle = UA_txtattr.tangle;
		attr.path = UA_txtattr.path;
		attr.align_hor = UA_txtattr.align_hor;
		attr.align_ver = UA_txtattr.align_ver;
		attr.txt_dens = UA_txtattr.txt_dens;
		attr.slant = UA_txtattr.slant;
		attr.sub_sup = UA_txtattr.sub_sup;
		attr.line_spacing = UA_txtattr.line_spacing;
		attr.entity_site = UA_txtattr.entity_site;
/*
........Get the form input
*/
		ua_get_txt_attr(&attr,"atxtinfo.frm");
/*
........Build the DRAFT command
*/
		ncl_init_cmdbuf(&cmdbuf);
		ncl_add_token(&cmdbuf,NCL_draft,NCL_nocomma);
		ncl_add_token(&cmdbuf,"ANOTE",NCL_comma);
		if (S_build_draft(&cmdbuf,&UA_txtattr,&attr))
		{
			ncl_add_cmdbuf(&cmdbuf);
			ncl_set_cmdmode(UU_TRUE);
			ncl_call(&cmdbuf);
		}
	}
/*
.....Modify notes' attributes
*/
	else
	{
/*
........Get the notes to modify
*/
		ud_lgeo(UU_TRUE,UD_text);
		ua_select(135,&num1);
/*
........Get list of selected keys
*/
		for (i=0;i<num1;i++)
		{
			if (ua_gnxt(&keys[i]) != UU_TRUE)
			{
				num1 = i;
				break;
			}
		}
/*
........Loop through the selected notes
*/
		first = UU_TRUE;
		for (i=0;i<num1;i++)
		{
			ncl_init_cmdbuf(&cmdbuf);
			ncl_add_token(&cmdbuf,NCL_draft,NCL_nocomma);
			ncl_add_token(&cmdbuf,"ANOTE,MODIFY=",NCL_nocomma);
			note.key = keys[i];
			if (note.key != 0)
			{
				if (ua_get_text1(&note,sizeof(struct UA_txt_rec)) == UU_SUCCESS)
				{
					note_attr.key = note.key;
					if (ur_retrieve_attr(&note_attr) == UU_SUCCESS)
					{
						ua_copy_text_attr(&save_attr,&UA_txtattr,&note,
							&note_attr);
/*
...........Use the first entity as the
...........default attributes
*/
						if (first)
						{
							first = UU_FALSE;
							attr = save_attr;
							ua_get_txt_attr(&attr,"atxtinfo.frm");
						}
/*
..........Add entity to DRAFT command
*/
						ncl_add_token(&cmdbuf,note.label,NCL_comma);
						if (S_build_draft(&cmdbuf,&save_attr,&attr))
						{
							ncl_add_cmdbuf(&cmdbuf);
							ncl_set_cmdmode(UU_TRUE);
							ncl_call(&cmdbuf);
						}
					}
				}
			}
		}
	}
/*
.....End of routine
*/
done:;
}

/*********************************************************************
**    I_FUNCTION     : S_build_draft(cmdbuf,iattr,oattr)
**			Builds the parameter list for the DRAFT/ANOTE command.
**    PARAMETERS   
**       INPUT  :
**         cmdbuf    = Active command line being built.
**         iattr     = Saved attributes prior to form input.
**         oattr     = Updated attributes after form input.
**       OUTPUT :
**         cmdbuf    = Updated command line.
**    RETURNS      : UU_TRUE if any parameters have changed.
**    SIDE EFFECTS : none.
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_build_draft(cmdbuf,iattr,oattr)
NCL_cmdbuf *cmdbuf;
ATXT_FRM *iattr,*oattr;
{
	UU_LOGICAL idid;
	UU_REAL tmp[2];
	char sbuf[40],tbuf[40];
/*
.....Build DRAFT command with
.....Changed attributes only
*/
	idid = UU_FALSE;
/*
........COLOR
*/
	if (oattr->color != iattr->color)
	{
		sprintf(sbuf,"COLOR=%s",uw_color_name[oattr->color]);
		ncl_add_token(cmdbuf,sbuf,NCL_comma);
		idid = UU_TRUE;
	}
/*
........SIZE
*/
	if (fabs(oattr->height-iattr->height) > UM_FUZZ ||
		fabs(oattr->expn-iattr->expn) > UM_FUZZ)
	{
		ncl_add_token(cmdbuf,"SIZE=",NCL_nocomma);
		UM_len_inttoext(oattr->height,tmp[0]);
		tmp[1] = oattr->expn;
		ncl_sprintf(sbuf,tmp,2);
		ncl_add_token(cmdbuf,sbuf,NCL_comma);
		idid = UU_TRUE;
	}
/*
........ATANGL
*/
	if (fabs(oattr->tangle-iattr->tangle) > UM_FUZZ)
	{
		tmp[0] = oattr->tangle * UM_RADIAN;
		ncl_add_token(cmdbuf,"ATANGL=",NCL_nocomma);
		ncl_sprintf(sbuf,&tmp[0],1);
		ncl_add_token(cmdbuf,sbuf,NCL_comma);
		idid = UU_TRUE;
	}
/*
........FONT
*/
	if (strcmp(oattr->fontname,iattr->fontname) != 0)
	{
		ncl_add_token(cmdbuf,"FONT=",NCL_nocomma);
		sprintf(sbuf,"\"%s\"",oattr->fontname);
		ncl_add_token(cmdbuf,sbuf,NCL_comma);
		idid = UU_TRUE;
	}
/*
........STYLE
*/
	if (oattr->prec != iattr->prec)
	{
		ncl_add_token(cmdbuf,"STYLE=",NCL_nocomma);
		if (oattr->prec == 2) ncl_add_token(cmdbuf,"STROKE",NCL_comma);
		else ncl_add_token(cmdbuf,"STRING",NCL_comma);
		idid = UU_TRUE;
	}
/*
........START
*/
	if (oattr->entity_site != iattr->entity_site)
	{
		sprintf(sbuf,"START=%d",oattr->entity_site);
		ncl_add_token(cmdbuf,sbuf,NCL_comma);
		idid = UU_TRUE;
	}
/*
........LETDIR
*/
	if (oattr->path != iattr->path)
	{
		ncl_add_token(cmdbuf,"LETDIR=",NCL_nocomma);
		if (oattr->path == 0) ncl_add_token(cmdbuf,"RIGHT",NCL_comma);
		else if (oattr->path == 1) ncl_add_token(cmdbuf,"LEFT",NCL_comma);
		else if (oattr->path == 2) ncl_add_token(cmdbuf,"UP",NCL_comma);
		else ncl_add_token(cmdbuf,"DOWN",NCL_comma);
		idid = UU_TRUE;
	}
/*
........ALIGN
*/
	if (oattr->align_hor != iattr->align_hor ||
		 oattr->align_ver != iattr->align_ver)
	{
		ncl_add_token(cmdbuf,"ALIGN=",NCL_nocomma);
		if (oattr->align_hor == 0) ncl_add_token(cmdbuf,"NORMAL",NCL_comma);
		else if (oattr->align_hor == 1) ncl_add_token(cmdbuf,"LEFT",NCL_comma);
		else if (oattr->align_hor == 2) ncl_add_token(cmdbuf,"CENTER",NCL_comma);
		else ncl_add_token(cmdbuf,"RIGHT",NCL_comma);
		if (oattr->align_ver == 0) ncl_add_token(cmdbuf,"NORMAL",NCL_comma);
		else if (oattr->align_ver == 1) ncl_add_token(cmdbuf,"UP",NCL_comma);
		else if (oattr->align_ver == 2) ncl_add_token(cmdbuf,"CENTER",NCL_comma);
		else if (oattr->align_ver == 3) ncl_add_token(cmdbuf,"BASE",NCL_comma);
		else ncl_add_token(cmdbuf,"DOWN",NCL_comma);
		idid = UU_TRUE;
	}
/*
........SPACE
*/
	if (fabs(oattr->spacing-iattr->spacing) > UM_FUZZ ||
		fabs(oattr->line_spacing-iattr->line_spacing) > UM_FUZZ)
	{
		ncl_add_token(cmdbuf,"SPACE=",NCL_nocomma);
		UM_len_inttoext(oattr->spacing,tmp[0]);
		UM_len_inttoext(oattr->line_spacing,tmp[1]);
		ncl_sprintf(sbuf,tmp,2);
		ncl_add_token(cmdbuf,sbuf,NCL_comma);
		idid = UU_TRUE;
	}
/*
........LINWGT
*/
	if (oattr->txt_dens != iattr->txt_dens)
	{
		ncl_add_token(cmdbuf,"LINWGT=",NCL_nocomma);
		if (oattr->txt_dens == 1) ncl_add_token(cmdbuf,"STD",NCL_comma);
		else if (oattr->txt_dens == 2) ncl_add_token(cmdbuf,"MEDIUM",NCL_comma);
		else if (oattr->txt_dens == 3) ncl_add_token(cmdbuf,"HEAVY",NCL_comma);
		else ncl_add_token(cmdbuf,"EXHVY",NCL_comma);
		idid = UU_TRUE;
	}
/*
........SMALL
*/
	if (fabs(oattr->sub_sup-iattr->sub_sup) > UM_FUZZ)
	{
		ncl_add_token(cmdbuf,"SMALL=",NCL_nocomma);
		ncl_sprintf(sbuf,&oattr->sub_sup,1);
		ncl_add_token(cmdbuf,sbuf,NCL_comma);
		idid = UU_TRUE;
	}
	return(idid);
}
