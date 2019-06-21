/*********************************************************************
**    NAME         :  nproj.c
**       CONTAINS:
**         nclu_project_sf
**    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nuproj.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:13
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
#include "nclvx.h"
#include "mdrel.h"
#include "mfort.h"
#include "ncl.h"
#include "nclfc.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "xenv1.h"

#include "class.h"
#include "mdpick.h"
/*
.....Form fields
*/
#define FCRV 0
#define FPAT 1
#define FCVT 2
#define FCCO 3
#define FSRF 4
#define FSFT 5
#define FSCO 6
#define FNPT 7
#define FTYP 8
#define FVCS 9
#define FVEC 10
#define FANS 11
#define FANE 12
#define FATT 13
#define FPTS 14
#define FAPT 15
#define FSF2 16
#define FS2T 17
#define FSFC 18
#define FVIW 19
#define FPRE 20
#define FAPP 21
/*
.....Main variables
*/
static int Scvcol,Tcvcol,Ssfcol,Tsfcol,Snrpt,Tnrpt,Smode,Tmode;
static int Sattach,Tattach,Ssf2col,Tsf2col,Stype;
static char Svec[65],Tvec[65],Sstart[65],Tstart[65],Send[65],Tend[65];
static char Sattpt[65],Tattpt[65],Tcvlab[65],Tsflab[65],Tsf2lab[65];
static char Tnplab[65];
static UU_LOGICAL Sform_init = UU_FALSE;
/*
.....Geometry variables
*/
static UM_sgeo Sgcv,Sgsf,Sgsf2;
/*
.....Main form callback routines
*/
static UD_FSTAT OnSelect(),OnText(),OnColor(),OnTog(),OnAction();
/*
.....Local routines
*/
static void S_init_form(),S_save_form(),S_hilite_entity(),S_unhilite_entity();
static void S_init_traverse(),S_form_invis(),S_form_vis(),S_init_geo();
static void S_delete_preview();
static int S_select_geo(),S_build_cmd(),S_verify_form(),S_pick_geo();

/*********************************************************************
**    E_FUNCTION     : nclu_project_sf(type)
**          Controlling routine for the Project Onto Surface form.
**    PARAMETERS   
**       INPUT  : 
**          type   = 0 = Pattern projection, 1 = Spline projection,
**                   2 = Sspline projection.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_project_sf(type)
int type;
{
	UU_LOGICAL cmdreject;
/*
.....Set up form fields
*/
	static char traverse[]     = {1,0,1,1,1,1,1,1, 1,1,0,0,0,0,0,0,0,0,0, 1,1,1};
	static char display[]      = {1,0,1,1,1,1,1,1, 1,1,0,0,0,0,0,0,0,0,0, 1,1,1};
	static UD_METHOD methods[] = {
		OnSelect,OnSelect,OnText,OnColor,OnSelect,OnText,OnColor,UU_NULL,
		OnTog,OnSelect,UU_NULL, UU_NULL,UU_NULL, OnTog,OnSelect,UU_NULL,
		OnSelect,OnText,OnColor,
		OnAction,OnAction,OnAction};
	static char called[]       = {6,6,6,6,6,6,6,6, 6,6,6,6,6,6,6,6,6,6,6, 6,6,6};
	static int *ans[] = {
		UU_NULL,UU_NULL,(int *)&Tcvlab,&Tcvcol,UU_NULL,(int *)&Tsflab,&Tsfcol,
		&Tnrpt,&Tmode,UU_NULL,(int *)&Tvec,(int *)&Tstart,(int *)&Tend,&Tattach,
		UU_NULL,(int *)&Tattpt,UU_NULL,(int *)&Tsf2lab,&Tsf2col,UU_NULL,UU_NULL,
		UU_NULL};

	int status;
/*
.....Initialize form answers
*/
	S_init_form(type);
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
	status = ud_form1("nproject.frm",ans,ans,methods,called,display,traverse);
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
	S_unhilite_entity(&Sgcv);
	S_unhilite_entity(&Sgsf);
	S_unhilite_entity(&Sgsf2);
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
	case FCRV:
		status = S_select_geo(&Sgcv,UD_ncl_offcv,221,Tcvcol,FCVT,Tcvlab,UU_FALSE,
			UU_NULL);
		break;
	case FPAT:
		status = S_select_geo(&Sgcv,UD_ncl_patern,450,Tcvcol,FCVT,Tcvlab,UU_FALSE,
			UU_NULL);
		break;
	case FSRF:
		status = S_select_geo(&Sgsf,UD_ncl_allsfpl,483,Tsfcol,FSFT,Tsflab,UU_TRUE,
			Tnplab);
		break;
	case FSF2:
		status = S_select_geo(&Sgsf2,UD_ncl_allsfpl,247,Tsf2col,FS2T,Tsf2lab,
			UU_FALSE,UU_NULL);
		break;
	case FVCS:
		status = S_pick_geo(Tvec,UD_ncl_vepv,218,FVEC);
		break;
	case FPTS:
		status = S_pick_geo(Tattpt,UD_ncl_ptpv,216,FAPT);
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnText(fieldno,val,stat)
**       Method called when a geometry text field is changed.
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
static UD_FSTAT OnText(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Process Drive Surface text field
*/
	switch (*fieldno)
	{
	case FCVT:
		ul_to_upper(Tcvlab);
		S_init_geo(&Sgcv,Tcvlab,Tcvcol);
		break;
	case FSFT:
		ul_to_upper(Tsflab);
		S_init_geo(&Sgsf,Tsflab,Tsfcol);
		break;
	case FS2T:
		ul_to_upper(Tsf2lab);
		S_init_geo(&Sgsf2,Tsf2lab,Tsf2col);
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnColor()
**			Geometry selection color setting routine.
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
static UD_FSTAT  OnColor(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Process color toggles
*/
	switch (*fieldno)
	{
	case FCCO:
		S_hilite_entity(&Sgcv,Tcvcol);
		break;
	case FSCO:
		S_hilite_entity(&Sgsf,Tsfcol);
		break;
	case FSF2:
		S_hilite_entity(&Sgsf2,Tsf2col);
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
	UU_LOGICAL dis[7];
/*
.....Initialize routine
*/
	for (i=0;i<7;i++) dis[i] = UU_FALSE;
/*
.....Process toggle fields
*/
	switch (*fieldno)
	{
/*
........Projection type
*/
	case FTYP:
		Tmode = *(val->frmint);
		break;
/*
........Attach type
*/
	case FATT:
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
		dis[1] = dis[2] = UU_TRUE;
		dis[5] = UU_TRUE;
		break;
	case 3:
	case 4:
	case 5:
		dis[0] = UU_TRUE;
		dis[3] = UU_TRUE;
		if (Tattach == 4) dis[4] = UU_TRUE;
		break;
	}
/*
.....Set display & traverse settings
*/
	ud_set_display_mask(UD_INPUTF,FVCS,dis[0]);
	ud_set_traverse_mask(FVCS,dis[0]);
	ud_set_display_mask(UD_INPUTF,FVEC,dis[0]);
	ud_set_traverse_mask(FVEC,dis[0]);
	ud_set_display_mask(UD_INPUTF,FANS,dis[1]);
	ud_set_traverse_mask(FANS,dis[1]);
	ud_set_display_mask(UD_INPUTF,FANE,dis[2]);
	ud_set_traverse_mask(FANE,dis[2]);
	ud_set_display_mask(UD_INPUTF,FATT,dis[3]);
	ud_set_traverse_mask(FATT,dis[3]);
	ud_set_display_mask(UD_INPUTF,FPTS,dis[4]);
	ud_set_traverse_mask(FPTS,dis[4]);
	ud_set_display_mask(UD_INPUTF,FAPT,dis[4]);
	ud_set_traverse_mask(FAPT,dis[4]);
	ud_set_display_mask(UD_INPUTF,FSF2,dis[5]);
	ud_set_display_mask(UD_INPUTF,FS2T,dis[5]);
	ud_set_display_mask(UD_INPUTF,FSFC,dis[5]);
	ud_set_traverse_mask(FSF2,dis[5]);
	ud_set_traverse_mask(FS2T,dis[5]);
	ud_set_traverse_mask(FSFC,dis[5]);
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
**			Initializes the Projection form variables.
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
	Stype = type;
	if (!Sform_init)
	{
		Scvcol = NCLX_SEA_GREEN;
		Ssfcol = NCLX_BROWN;
		Snrpt = 0;
		Smode = 0;
		Sattach = 0;
		Ssf2col = NCLX_LT_BLUE;
		Svec[0] = '\0';
		strcpy(Sstart,"0.");
		strcpy(Send,"0.");
		Sattpt[0] = '\0';
		Sform_init = UU_TRUE;
	}
/*
.....Initialize geometry variables
*/
	Sgcv.key = Sgsf.key = Sgsf2.key = 0;
	Sgcv.color = Sgsf.color = Sgsf2.color = -1;
	Sgcv.label[0] = Sgsf.label[0] = Sgsf2.label[0] = '\0';
	Tcvlab[0] = Tsflab[0] = Tsf2lab[0] = '\0';
/*
.....Work with temporary variables
*/
	Tcvcol = Scvcol;
	Tsfcol = Ssfcol;
	Tnrpt = Snrpt;
	Tmode = Smode;
	Tattach = Sattach;
   Tsf2col = Ssf2col;
	strcpy(Tvec,Svec);
	strcpy(Tstart,Sstart);
	strcpy(Tend,Send);
	strcpy(Tattpt,Sattpt);
	Tnplab[0] = '\0';
}

/*********************************************************************
**    I_FUNCTION     : S_init_traverse(display,traverse)
**			Initializes the Projection form field traversal settings.
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
.....Initialize Curve/Pattern fields
*/
	if (Stype != 0)
	{
		display[FPAT] = traverse[FPAT] = 0;
		display[FCRV] = traverse[FCRV] = 1;
	}
	else
	{
		display[FPAT] = traverse[FPAT] = 1;
		display[FCRV] = traverse[FCRV] = 0;
	}
/*
.....Initialize projection type fields
*/
	display[FVCS] = traverse[FVCS] = 0;
	display[FVEC] = traverse[FVEC] = 0;
	display[FANS] = traverse[FANS] = 0;
	display[FANE] = traverse[FANE] = 0;
	display[FATT] = traverse[FATT] = 0;
	display[FPTS] = traverse[FPTS] = 0;
	display[FAPT] = traverse[FAPT] = 0;
	display[FSF2] = traverse[FSF2] = 0;
	display[FS2T] = traverse[FS2T] = 0;
	display[FSFC] = traverse[FSFC] = 0;
	switch (Smode)
	{
/*
........Vector
*/
	case 1:
		display[FVCS] = traverse[FVCS] = 1;
		display[FVEC] = traverse[FVEC] = 1;
		break;
/*
........Atangle
*/
	case 2:
		display[FANS] = traverse[FANS] = 1;
		display[FANE] = traverse[FANE] = 1;
		display[FSF2] = traverse[FSF2] = 1;
		display[FS2T] = traverse[FS2T] = 1;
		display[FSFC] = traverse[FSFC] = 1;
		break;
/*
........Wrap
........Revolve
........Radial
*/
	case 3:
	case 4:
	case 5:
		display[FVCS] = traverse[FVCS] = 1;
		display[FVEC] = traverse[FVEC] = 1;
		display[FATT] = traverse[FATT] = 1;
		if (Sattach == 4)
		{
			display[FPTS] = traverse[FPTS] = 1;
			display[FAPT] = traverse[FAPT] = 1;
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
	Scvcol = Tcvcol;
	Ssfcol = Tsfcol;
	Snrpt = Tnrpt;
	Smode = Tmode;
	Sattach = Tattach;
   Ssf2col = Tsf2col;
	strcpy(Svec,Tvec);
	strcpy(Sstart,Tstart);
	strcpy(Send,Tend);
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
			ncl_update_geo_color(e.key,color,UU_TRUE);
			uc_display(&e);
		}
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
			ncl_update_geo_color(e.key,sfpt->color,UU_FALSE);
			uc_display(&e);
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
**    I_FUNCTION     : S_build_cmd(flag)
**			Builds and outputs the geo/PROJCT command.
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
	int nc,stat;
	UU_REAL ptx[3];
	char buf[80],sbuf[80];
	NCL_cmdbuf cmdbuf;
/*
.....Initialize command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
/*
.....geo/PROJCT command
*/
	if (!flag) ncl_add_token(&cmdbuf, "*@UZTMP=", NCL_nocomma);
	if (Stype == 0) ncl_add_token(&cmdbuf,NCL_pn,NCL_nocomma);
	else if (Stype == 1) ncl_add_token(&cmdbuf,NCL_spline,NCL_nocomma);
	else if (Stype == 2) ncl_add_token(&cmdbuf,NCL_ssplin,NCL_nocomma);
	ncl_add_token(&cmdbuf, NCL_projct, NCL_comma);
/*
.....Geometry
*/
	ncl_add_token(&cmdbuf,Sgcv.label,NCL_comma);
/*
.....Vector
*/
	if (Tmode != 0 && Tmode != 2)
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
.....Atangl
*/
	else if (Tmode == 2)
	{
		ncl_add_token(&cmdbuf,NCL_atangl,NCL_comma);
		ncl_add_token(&cmdbuf,Tstart,NCL_comma);
		ncl_add_token(&cmdbuf,Tend,NCL_comma);
		nc = strlen(Sgsf2.label);
		ul_strip_blanks(Sgsf2.label,&nc);
		if (nc != 0) ncl_add_token(&cmdbuf,Sgsf2.label,NCL_comma);
	}
/*
.....Surface
*/
	ncl_add_token(&cmdbuf,Sgsf.label,NCL_comma);
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
/*
.....Near point
*/
	if (Tnrpt)
		ncl_add_token(&cmdbuf,Tnplab,NCL_nocomma);
/*
.....Call the command
*/
	ncl_set_cmdmode(UU_TRUE);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
	return(UU_SUCCESS);
}

/*********************************************************************
**    S_FUNCTION     :  S_select_geo(sfpt,mask,prmno,color,fieldno,label,
**                                   nptfl,nptlab)
**       Routine to select geometry for the Project form.  The associated
**       text field will be updated with the geometry label and
**       the geometry will be highlighted.
**    PARAMETERS
**       INPUT  :
**          sfpt     Pointer to selected geometry structure.
**          mask     Picking mask.
**          prmno    Prompt number to use while in pick mode.
**          color    Color to highlight the picked entity.
**                   -1 = Don't highlight entity.
**          fieldno  Text field to update with label of selected entity.
**                   -1 = No field to updated.
**          label    Text buffer that belongs to 'fieldno'.
**          nptfl    UU_TRUE = Return near point definition.
**       OUTPUT :
**          sfpt     Updated geometry structure.
**          nptlab   Near point location formatted as (PT/x,y,z).
**    RETURNS      : UU_SUCCESS if an entity is picked.  UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_select_geo(sfpt,mask,prmno,color,fieldno,label,nptfl,nptlab)
UM_sgeo *sfpt;
unsigned int *mask;
int prmno,color,fieldno;
char *label;
UU_LOGICAL nptfl;
char *nptlab;
{
	int numint,iret,status,i;
	struct NCL_fixed_databag e;
	UM_PLOCREC pick;
	UU_LOGICAL cmdreject;
	UM_f77_str f77_str;
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
	if (nptfl)
	{
		status = ncl_add_label_nrpt(prmno,mask,e.label,nptlab,&e.rel_num);
		if (status != NCL_OKINPUT) goto failed;
		for (i=strlen(e.label);i<64;i++) e.label[i] = ' ';
		UM_init_f77_str(f77_str,e.label,nc);
		getkey(UM_addr_of_f77_str(f77_str),&e.key);
	}
	else
	{
		ua_dl_pldas(UD_DASPCKLOC,UA_NCL,prmno,&pick,1,&numint,1);
		if (numint == 0) goto failed;
		e.key = um_get_pickkey(&(pick.pent),1);
	}
	if (ncl_retrieve_data_fixed(&e) != 0) goto done;
/*
.....Unhighlight the previous selection
*/
	S_unhilite_entity(sfpt);
/*
.....Store the entity's data
*/
	sfpt->key = e.key;
	sfpt->relnum = e.rel_num;
	ncl_get_label(&e,sfpt->label);
/*
.....Highlight the selected entity
*/
	S_hilite_entity(sfpt,color);
/*
.....Update the text field with the entity label
*/
	if (fieldno != -1)
	{
		strcpy(label,sfpt->label);
		ud_update_answer(fieldno,(int *)label);
	}
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
	if (ncl_retrieve_data_fixed(&e) != 0) goto done;
/*
.....Store the entity's data
*/
	ncl_get_label(&e,label);
/*
.....Update the text field with the entity label
*/
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
		strncpy(sfpt->label,label,NCL_MAX_LABEL);
		S_hilite_entity(sfpt,color);
	}
}
