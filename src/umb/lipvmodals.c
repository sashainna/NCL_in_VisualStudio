/*********************************************************************
**   FILENAME: lipvmodals.c
**   CONTAINS:
**             ul_ipv_modal_form()
**
**     COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       lipvmodals.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:15
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "lcom.h"
#include "mdcpln.h"
#include "mdrel.h"
#include "mfort.h"
#include "nclfc.h"
#include "nccs.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "lumb.h"
#include "lipv.h"
#include "lipvmach.h"
#include "lipvmplay.h"
#include "wsgl.h"

extern int NAUTLTH;

static int Sfrm,Smon;

static UD_FSTAT OnMode();
static UD_FSTAT OnMonitor();
static UD_FSTAT OnOption();
static UD_FSTAT OnClose();
static int S_save_modfile();

#define FMOD 0
#define FTYP 1
#define FGRD 2
#define FRST 3
#define FMON 4
#define FOPT 5

/*********************************************************************
**	 E_FUNCTION : ul_ipv_modal_form()
**			This function handles the NCLIPV modals form.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: Initializes NCL502 variables.
**	 WARNINGS:
*********************************************************************/
void ul_ipv_modal_form()
{
	int status,i;
	UU_LOGICAL cmdreject;
	UU_LOGICAL iact,ichg;

	static int mode,accy,areset,mach,tdum;
	int defopt[N_IPVMON_FLD];
	static UD_METHOD methods[] = {OnMode,UU_NULL,UU_NULL,UU_NULL,OnMonitor,
		OnOption};
	static char called[]       = {6,6,6,6,6,6};
	static char traverse[]     = {1,1,1,1,1,1};
	static int *ans[] = {&mode,&mach,&accy,&areset,&Smon,&tdum};
/*
.....Debug Enter
*/
	uu_denter(UU_ITRC,(us,"ul_verify_modals()"));
/*
.....Load the input values into
.....local storage area
*/
	mode = LW_mach_mode;
	mach = LW_mach_type_flag;
/*	if (NAUTLTH != 1) mach = LW_MILL;*/
	accy = LW_rv_accy;
	areset = LW_auto_reset;
	Smon = LW_monitor;
/*
.....Save the default Monitor options
*/
	for (i=0;i<N_IPVMON_FLD;i++) defopt[i] = LW_monitor_field[i];
/*
.....Set traverse flags
*/
	if (LW_mach_mode == LW_VISICUT)
	{
		traverse[FMOD] = 1;
		traverse[FTYP] = 1;
		traverse[FGRD] = 0;
		traverse[FRST] = 1;
		traverse[FMON] = 1;
		traverse[FOPT] = Smon;
	}
	else
	{
		traverse[FMOD] = 1;
		traverse[FTYP] = 0;
		traverse[FGRD] = 1;
		traverse[FRST] = 1;
		traverse[FMON] = 0;
		traverse[FOPT] = 0;
	}
/*
.....Get the Form input
*/
	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)
	{
		status = ud_form1("ipvmod.frm", ans, ans, methods, called, UU_NULL,
			traverse);
		if (status==-1)
			goto done;
	}	
	else
		goto done;
/*
.....Determine if the Monitor Panel
.....needs to be redisplayed
*/
	iact = ul_ipv_monitor_active();
	if (iact || LW_monitor != Smon)
	{
		ichg = UU_FALSE;
		if (LW_monitor != Smon) ichg = UU_TRUE;
		for (i=0;i<N_IPVMON_FLD;i++)
			if (defopt[i] != LW_monitor_field[i]) ichg = UU_TRUE;
/*
........Redisplay the monitor form
*/
		if (ichg && LW_active)
		{
			ul_ipv_monitor_close();
			if (Smon) ul_ipv_monitor_form();
		}
	}
/*
.....Store IPV Modals
*/
	if (mode != LW_mach_mode)
	{
		LtData stuff;
		if (LW_mach_mode == LW_RAPIDCUT)
		{
			LiDataSetNat32(&stuff,accy);
			LiControlSet(LI_CONTROL_RV_RESOLUTION,&stuff);
		}
		if (LW_session[LW_mach_mode] != 0)
		{
/*
.....Destroy the current session we are moving to,
.....then recreate it using the
.....stock we extract from the session we are moving from
*/
			um_delv_axis_ipv();
/*
.....Get stock from current session
*/	
			if (LW_nstock[0] != 0)
				LW_stock_first[0]->prim =
					LiPrimitiveSolidExtract(LW_stock_first[0]->stock);

			/* destroy existing 'new' session */
			LiMWViewportDestroy(LW_viewport);
			if (LW_nstock[0] != 0)
				LiSessionRemovePrim(LW_stock_first[0]->stock);
			LiSessionDestroy( LW_session[LW_mach_mode] );
/*			if (LW_nstock[0] != 0)
				LiPrimitiveDestroy(LW_stock_first[0]->prim);*/


			LW_session[mode] = LiSessionCreate( 
				(mode == 0) ? LI_MW_SESSION_VISICUT : LI_MW_SESSION_RAPIDCUT );
			LW_view = LiViewCreate();
			LW_viewport = LiMWViewportCreate(LW_session[mode],LW_view);
			ul_ipv_set_lights(LW_lights);
			if (LW_nstock[0] != 0)
				LW_stock_first[0]->stock = LiSessionAddPrim(LW_session[mode],
					LW_stock_first[0]->prim);
			ul_ipv_deselect_tool();
		}
		LW_mach_mode = mode;
	}
	LW_mach_type_flag = mach;
	LW_rv_accy = accy;
	LW_auto_reset = areset;
	LW_monitor = Smon;
	ul_ipv_flush();
/*
.....Save the NCLIPV Modals file
*/
	S_save_modfile();
/*
.....Update the NCLIPV dislpay
.....with the new properties
*/
	um_reset_pocket_graphics(UM_IPV_WINDOW);
done:
	UD_UNMARK(cmdreject);
	uu_dexit;
	return;
}

/*********************************************************************
**    S_FUNCTION     :  OnMode(filedno, val, stat)
**       Method called Machining Mode toggle is changed.
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
static UD_FSTAT OnMode(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (*(val->frmint) == LW_VISICUT)
	{
		ud_set_traverse_mask(FTYP,UU_TRUE);
		ud_set_traverse_mask(FGRD,UU_FALSE);
	}
	else
	{
		ud_set_traverse_mask(FTYP,UU_FALSE);
		ud_set_traverse_mask(FGRD,UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnMonitor()
**       Callback routine for the Monitor Playback field.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnMonitor(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	ud_default_method(fieldno,val,stat);
/*
.....Enable/Disable Option button
.....based on Monitor Playback field
*/
	if (Smon == 1) ud_set_traverse_mask(FOPT,UU_TRUE);
	else ud_set_traverse_mask(FOPT,UU_FALSE);
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnOption()
**       Callback routine for the Options button.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnOption(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
/*
.....Set up form fields
*/
	static char traverse[N_IPVMON_FLD];
	static char display[N_IPVMON_FLD];
	static UD_METHOD methods[N_IPVMON_FLD+1];
	static char called[N_IPVMON_FLD];
	static int *ans[N_IPVMON_FLD];
/*
....Initialize routine
*/
	if (Sfrm != 0) goto done;
/*
.....Setup default answers
*/
	for (i=0;i<N_IPVMON_FLD;i++)
	{
		traverse[i] = 1;
		display[i] = 1;
		methods[i] = UU_NULL;
		called[i] = 6;
		ans[i] = &LW_monitor_field[i];
	}
	methods[N_IPVMON_FLD] = OnClose;
/*
.....Set up field traversals
*/
	if (LW_mach_desc.type == -1)
	{
		traverse[IPVMON_LINAXS] = 0;
		traverse[IPVMON_ROTAXS] = 0;
		traverse[IPVMON_HEAD2] = 0;
		traverse[IPVMON_HEAD3] = 0;
		traverse[IPVMON_HEAD4] = 0;
		LW_monitor_field[IPVMON_LINAXS] = 0;
		LW_monitor_field[IPVMON_ROTAXS] = 0;
		LW_monitor_field[IPVMON_HEAD2] = 0;
		LW_monitor_field[IPVMON_HEAD3] = 0;
		LW_monitor_field[IPVMON_HEAD4] = 0;
	}

	if (LW_mach_type != LW_STRINGER)
	{
		display[IPVMON_HEAD2] = 0;
		display[IPVMON_HEAD3] = 0;
		display[IPVMON_HEAD4] = 0;
	}
/*
.....Display the form
*/
	Sfrm = ud_form_display1("ipvmonopt.frm",ans,ans,methods,called,display,
		traverse);
	if (Sfrm == -1) goto nofrm;
	goto done;
/*
.....Could not load form
*/
nofrm:
	ud_wrerr("Could not load 'ipvmonopt.frm'.");
	goto done;
/*
.....End of routine
*/
done:
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnClose()
**       Callback routine for the NCLIPV Monitor Options close form.
**    PARAMETERS   
**       INPUT  : 
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnClose()
{
/*
.....Mark the form as closed
*/
	Sfrm = 0;
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : S_save_modfile
**       Save the NCLIPV playback properties into modals file.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : UU_FAILURE if could not save modals file,  UU_SUCCESS
**                   otherwise.
**		SIDE EFFECTS : none
**    WARNINGS     : none
**
*************************************************************************/
static int S_save_modfile()
{
	int stat,i;
	char msg[80];
	UU_REAL rval[2];
	UX_pathname fname;
	FILE *fptr;
	static char yesno[2][10] = {"*NO","*YES"};
	static char cmode[2][10] = {"*VISICUT","*RAPIDCUT"};
	static char cmach[5][10] = {"*AUTO","*MILL","*LATHE","*MILLTURN",
		"*STRINGER"};
	static char cdock[3][10] = {"*OFF","*LEFT","*RIGHT"};
	static char *csub[] = {"ISN","MODE","MACHTYP","TOOL_END","TOOL_AXIS",
		"LINEAR","ROTARY","HEAD2","HEAD3","HEAD4","LOADTL","DIAMETER","RADIUS",
		"HEIGHT","CUTCOM","FEDRAT","MOVE_TIME","SPINDL","COOLNT","TOTAL_TIME",
		"PROGRESS","DOCK"};
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	strcpy (fname, "nclipv_modals.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", UU_NULL, UU_NULL,
					fname, 3, &fptr);
	if ((stat!=UU_SUCCESS)||(fptr==UU_NULL)) goto done;
/*
.....Store playback modals
*/
	ux_fputs0("#SESSION#\n", fptr);
	sprintf(msg,"/MODE/ %s\n",cmode[LW_mach_mode]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/MACHINE/ %s\n",cmach[LW_mach_type_flag]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/RAPIDCUT_GRID/ %d\n",LW_rv_accy);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/AUTO_RESET/ %s\n",yesno[LW_auto_reset]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/MONITOR/ %s\n",yesno[LW_monitor]);
	ux_fputs0(msg, fptr);

	sprintf(msg,"/TITLE/ %s\n",LW_ipv_title);
	ux_fputs0(msg, fptr);
	rval[0] = (UU_REAL)LW_ipv_pos[0] / (UU_REAL)uw_gl.dev_xmax;
	rval[1] = (UU_REAL)LW_ipv_pos[1] / (UU_REAL)uw_gl.dev_ymax;
	sprintf(msg,"/POSITION/ %lf,%lf\n",rval[0],rval[1]);
	ux_fputs0(msg, fptr);

	rval[0] = (UU_REAL)LW_ipv_size[0] / (UU_REAL)uw_gl.dev_xmax;
	rval[1] = (UU_REAL)LW_ipv_size[1] / (UU_REAL)uw_gl.dev_ymax;
	sprintf(msg,"/SIZE/ %lf,%lf\n",rval[0],rval[1]);
	ux_fputs0(msg, fptr);
/*
.....Store monitor modals
*/
	ux_fputs0("\n#MONITOR#\n", fptr);
	for (i=0;i<N_IPVMON_FLD-1;i++)
	{
		sprintf(msg,"/%s/ %s\n",csub[i],yesno[LW_monitor_field[i]]);
		ux_fputs0(msg, fptr);
	}
	sprintf(msg,"/%s/ %s\n",csub[N_IPVMON_FLD-1],
		cdock[LW_monitor_field[N_IPVMON_FLD-1]]);
	ux_fputs0(msg, fptr);
/*
.....Close modals file
*/
	ux_fclose0 (fptr);
/*
.....End of routine
*/
done:
	return(stat);
}
