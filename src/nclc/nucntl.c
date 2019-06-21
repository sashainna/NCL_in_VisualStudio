/*********************************************************************
**    NAME         :  nucntl.c
**       CONTAINS:
**         nclu_cmd_set
**         nclu_cmd_run
**         nclu_cmd_show
**         nclu_clean_ln
**         umu_set_adispl()
**         nclu_set_autost()
**         nclu_cmd_run_from_to
**         nclu_auto_save()
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nucntl.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:04
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "dmark.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclfc.h"
#include "ncldef.h"
#include "mattr.h"
#include "modef.h"
#include "udforms.h"
#include "udfconst.h"
#include "udfdata.h"
#include "class.h"
#include "nccs.h"
#include "dselect.h"
#include "mlab.h"
#include "mdpick.h"
#include "lcom.h"

#define NUMGEO 13

static int ncvpts,nulin,nvlin,nupts,nvpts;
static UU_LOGICAL window_stat = UU_FALSE;

static int outcmdMot,changeGeo,mainsets[16],Smainsets[16],cutacc,outcmdMain;
static int Sfrm,Smotsets[5],motsets[5],changeMot,trct,Sason,Sppgon;
static UU_REAL tolr,ltol,atol,rlabls[NUMGEO],cvdisp_val[5],Stolr,Slabls[NUMGEO],
					Scvdisp[5],Smtols[2],mtols[2],ver,tracut;
static int Sdisp[NUMGEO];
static int dispOutCmdBox,reschoice,dispgeo[NUMGEO],outcmdGeo;
static char labls[NUMGEO][65],cvdisp_str[6][65],toler_str[65],angtol_str[65],
				mtols_str[2][65],mainsets_str[2][65],Smainsets_str[2][65];
static UD_FSTAT OnGeoSel(),OnMotSel(),OnSelectDClass(),OnTogAstrt(),
					OnTrcTog(),OnMxname(),OnSelDesel(),OnAutoSaveTog(),
					OnSnapSave(),OnRestore();
static char Smxname[NCL_MAX_LABEL_AND_SUBSCRIPT];
static UD_LIST Smxlist;
#define UAUT 0
#define UTIM 1
#define UTYP 2
#define USNP 3
#define URES 4
#define PAUT 5
#define PTIM 6
#define PTYP 7
#define PSVS 8
#define PSNP 9
#define PRES 10
extern UU_LOGICAL UR_ason;
extern long	UR_last_autosave;
extern int	UR_switch;
extern int	UR_time;
extern int	UR_num_chg;
extern UU_LOGICAL NCL_ason;
extern long NCL_last_autosave;
extern int NCL_switch;
extern int NCL_time;
extern int NCL_num_chg;
extern int NCL_max_saves;

/*********************************************************************
**    E_FUNCTION     : ncl_cmd_set()
**       Process SET/RESET command from CONTROL menu.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_cmd_set(parms)
char *parms;
{
   int status,istat,i;
   int choice,call_form;
   int count;
   char sbuf[120];
   NCL_cmdbuf cmdbuf;

	static int *ans[20];
	static UD_METHOD methods[] = {UU_NULL,UU_NULL, UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL, UU_NULL,UU_NULL,UU_NULL, UU_NULL,OnTrcTog,UU_NULL, UU_NULL,UU_NULL,
		UU_NULL,UU_NULL, UU_NULL,UU_NULL, UU_NULL, OnGeoSel,OnMotSel};
	static char called[] = {6,6, 6,6,6, 6,6, 6,6,6, 6,6,6, 6,6, 6,6, 6,6, 6, 6,6};
	char traverse[] = {1,1, 1,1,1, 1,1, 1,1,1, 1,1,0, 1,1, 1,1, 1,1, 1, 1,1};
	static char display[] = {1,1,1, 1,1,1,1, 1,1, 1,1,1, 1,1,1, 1,1, 1,1, 1,1, 1,
		1,1};

	dispOutCmdBox = 1;
	if (parms[0] == '\0')
	{
		call_form = 0;
		dispOutCmdBox = 0;
	}
	else if (strcmp(parms,"GEOMETRY") == 0) call_form = 1;
	else if (strcmp(parms,"MOTION") == 0) call_form = 2;
	else ud_wrerr("Invalid RUN Paramter.");

   uu_denter( UU_MTRC,(us,"ncl_cmd_set()"));
/*
.....Bring up desired form
*/
   if (call_form == 0) 
	{
		S_set_main_settings(traverse);

		for (i=0;i<6;i++) ans[i] = &mainsets[i];
		ans[6] = (int*)mainsets_str[0];
		for (i=7;i<12;i++) ans[i] = &mainsets[i-1];
		ans[12] = (int*)&Smxlist;
		ans[13] = &mainsets[11];
		ans[14] = (int*)mainsets_str[1];
		for (i=15;i<19;i++) ans[i] = &mainsets[i-3];
		ans[19] = &outcmdMain;

		status = ud_form1("setcmd.frm",ans,ans,methods,called,display,traverse);
	}
	else if (call_form == 1) 
		status = OnGeoSel();
	else if (call_form == 2) 
		status = OnMotSel();
	if (Sfrm == -1 || status == -1)
	{
		goto done;
	}
	if (changeGeo)
	{
/*
.....Store edit field answers
*/
		for (i=0;i<NUMGEO;i++) ncl_get_scalar_value(labls[i],&rlabls[i]);
		for (i=0;i<5;i++)	ncl_get_scalar_value(cvdisp_str[i+1],&cvdisp_val[i]);
		ncl_get_scalar_value(cvdisp_str[0],&tolr);
		S_mod_geo_settings();
	}
	if (changeMot)
	{
/*
.....Store edit field answers
*/
		ncl_get_scalar_value(mtols_str[0],&mtols[0]);
		ncl_get_scalar_value(mtols_str[1],&mtols[1]);
		S_mod_motion_settings();
	}
	S_mod_main_settings();
done:;
	if (call_form == 0) ud_free_flist(&Smxlist);
   uu_dexit;
   return(0);
}
/*********************************************************************
**    E_FUNCTION     : ncl_cmd_run()
**       Process RUN command from CONTROL menu.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_cmd_run(parms)
char *parms;
{
   int status, i;
   int choice;
   NCL_cmdbuf cmdbuf;
   UU_LOGICAL loop_par;

	static char *rbuf[] = {"END","MACRO","TERMAC","LOOPST","LOOPND","LINE"};

   uu_denter( UU_MTRC,(us,"ncl_cmd_run()"));

   status = NCL_OKINPUT;
   loop_par = UU_TRUE;
/*
.....Check for parameter
*/
	if (parms[0] == '\0')
		choice = 7;
	else
	{
		for (choice=0;choice<6;choice++)
			if (strcmp(parms,rbuf[choice]) == 0) break;
		if (choice == 6)
		{
			ud_wrerr("Invalid RUN Paramter.");
			uu_dexit;
			return(0);
		}
		choice += 1;
	}
   while(status == NCL_OKINPUT || status == NCL_DONE)
	{
		if (choice == 7) status = ncl_popup(NCL_CMD_RUN, &choice);
		if(status != NCL_OKINPUT) break;
		ncl_init_cmdbuf(&cmdbuf);
		switch(choice)
		{
			case 1:   /* run */
				status = ncl_add_token(&cmdbuf, NCL_cmd_run, NCL_nocomma);
				loop_par = UU_FALSE;
				break;

			case 2:   /* macro start */
				status = ncl_add_token(&cmdbuf, NCL_run, NCL_nocomma);
				status = ncl_add_token(&cmdbuf, "MACRO", NCL_nocomma);
				loop_par = UU_FALSE;
				break;

			case 3:   /* macro end */
				status = ncl_add_token(&cmdbuf, NCL_run, NCL_nocomma);
				status = ncl_add_token(&cmdbuf, "TERMAC", NCL_nocomma);
				loop_par = UU_FALSE;
				break;

			case 4:   /* loop start */
				status = ncl_add_token(&cmdbuf, NCL_run, NCL_nocomma);
				status = ncl_add_token(&cmdbuf, "LOOPST", NCL_nocomma);
				loop_par = UU_FALSE;
				break;

			case 5:   /* loop end */
				status = ncl_add_token(&cmdbuf, NCL_run, NCL_nocomma);
				status = ncl_add_token(&cmdbuf, "LOOPND", NCL_nocomma);
				loop_par = UU_FALSE;
				break;

			case 6:   /* run to line */
				status = ncl_add_token(&cmdbuf, NCL_run, NCL_nocomma);
				status = ncl_add_str(&cmdbuf, 203, NCL_nocomma);
				break;

			default:
				status = NCL_NOINPUT;
		}

		if(status == NCL_OKINPUT || status == NCL_DONE )
		{
			ud_reset_prompt();
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);

			clswin();

			if(!loop_par) status = NCL_NOINPUT;
		}
	}

   uu_dexit;
   return(0);
}

/*********************************************************************
**    E_FUNCTION     : ncl_cmd_show(form)
**       Process SHOW command from CONTROL menu.
**    PARAMETERS
**       INPUT  :
**          form                  choice
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_cmd_show(form)
   int form;
   {
   int status;
   NCL_cmdbuf cmdbuf;
   UU_LOGICAL loop, scroll_flag;
   UU_LOGICAL answer;
   int markval;

   uu_denter( UU_MTRC,(us,"ncl_cmd_edit()"));

   status = NCL_OKINPUT;
   window_stat = UU_FALSE;
   scroll_flag = UU_FALSE;
   UD_MARK(markval, UU_FALSE);
   if(markval == 0)
      {
      while(status == NCL_OKINPUT || status == NCL_DONE)
         {
         ncl_init_cmdbuf(&cmdbuf);
         status = ncl_add_token(&cmdbuf, NCL_show, NCL_nocomma);
         loop = UU_FALSE;
         switch(form)
            {
            case 1:   /* show geometry */
               status = ncl_add_label(UD_DASPCKLOC, &cmdbuf, 202,
                                       UD_ncl_show);
               loop = UU_TRUE;
               break;

            case 2:   /* show cutter */
               status = ncl_add_token(&cmdbuf, NCL_show_cutter, NCL_nocomma);
               break;

            case 3:   /* show files */
               status = ncl_add_token(&cmdbuf, NCL_show_files, NCL_nocomma);
               break;

            case 4:   /* show source * */
               status = ncl_add_token(&cmdbuf, NCL_show_source, NCL_comma);
               status = ncl_add_token(&cmdbuf, "*", NCL_nocomma);
               break;

            case 5:  /* show line num */
               status = ncl_add_token(&cmdbuf, NCL_show_source, NCL_comma);
               status = ncl_add_str(&cmdbuf, 203, NCL_nocomma);
               if(status == NCL_DONE)
                  {
                  status = NCL_NOINPUT;
                  loop = UU_FALSE;
                  }
               else
                  {
                  loop = UU_TRUE;
                  }
               break;
   
            case 6:  /* show scroll */
               status = ncl_add_token(&cmdbuf, NCL_show_source, NCL_comma);
               status = ncl_add_token(&cmdbuf, NCL_scroll, NCL_nocomma);
               scroll_flag = UU_TRUE;
               break;
   
            case 7:   /* show thick */
               status = ncl_add_token(&cmdbuf, NCL_show_thick, NCL_nocomma);
               break;

            case 8:   /* show toler */
               status = ncl_add_token(&cmdbuf, NCL_show_toler, NCL_nocomma);
               break;

            case 9:   /* show tool */
               status = ncl_add_token(&cmdbuf, NCL_show_tool, NCL_nocomma);
               break;

            case 10:   /* show modals */
               status = ncl_add_token(&cmdbuf, NCL_show_modals, NCL_nocomma);
               break;

            default:
               status = NCL_NOINPUT;
            }

         if(status == NCL_OKINPUT || status == NCL_DONE )
            {
            ncl_add_cmdbuf(&cmdbuf);
            opnwin();
            window_stat = UU_TRUE;
            ncl_call(&cmdbuf);
            if(!scroll_flag) answer = ud_hakt(10, 1);
            clswin();
            window_stat = UU_FALSE;
            }
         if(!loop) break;
         }
      }
   if(window_stat == UU_TRUE)
      {
      clswin();
      window_stat = UU_FALSE;
      }
   UD_UNMARK(markval);
   uu_dexit;
   return(0);
   }

/********************************************************************
**    E_FUNCTION     : nclu_clean_ln
**      Cleans last source line before get out of NCL501+ menu
**      subsystem. ONLY for NCL501+ mode. Paul. 04/07/92
**    PARAMETERS
**       INPUT  :
**          form                  choice
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_clean_ln()
{
	NCL_cmdbuf cmdbuf;

	ncl_init_cmdbuf(&cmdbuf);
	strcpy(cmdbuf.cur_str, "*remark");
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
	return(0);
}

/*********************************************************************
**    E_FUNCTION     : umu_set_adispl()
**      Change curve/surface display modals with a form.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
 umu_set_adispl(cmdbuf)
 NCL_cmdbuf *cmdbuf;
{
	int status;
	UM_int2 ifl,ival;
	UM_real8 rval;
	static UU_REAL toler;
	UU_REAL toler1, value;
	char toler_str[65], ncvpts_str[65],nulin_str[65],
		nvlin_str[65],nupts_str[65],nvpts_str[65];

 /* Array used to call ud_form */
	static int *form_ans[6];
	uu_denter( UU_MTRC,(us,"umu_set_adispl()"));
/*
... Put up form with correct defaults, and traverse it 
*/

	ifl   = 136;
	getifl (&ifl,&ival);
	ncvpts = ival;
	nulin = UM_srfattr.numvpaths;
	nvlin = UM_srfattr.numupaths;
	nupts = UM_srfattr.ptspervcrv;
	nvpts = UM_srfattr.ptsperucrv;
	ifl = 175; getsc(&ifl,&rval);
	toler = rval; toler1 = rval;

	sprintf (ncvpts_str,"%d",ncvpts);
	sprintf (nulin_str,"%d",nulin);
	sprintf (nvlin_str,"%d",nvlin);
	sprintf (nupts_str,"%d",nupts);
	sprintf (nvpts_str,"%d",nvpts);
	ncl_sprintf (toler_str,&toler,1);
	form_ans[0] = (int*)ncvpts_str;
	form_ans[1] = (int*)nulin_str;
	form_ans[2] = (int*)nvlin_str;
	form_ans[3] = (int*)nupts_str;
	form_ans[4] = (int*)nvpts_str;
	form_ans[5] = (int*)toler_str;
	status = ud_form( "cvsrfattr.frm", form_ans, form_ans );
	if (status==-1)
		return -1;
/*
... Build command line
*/
/*
 sprintf (cpar,"%d",ncvpts);
 status = ncl_add_token(cmdbuf, cpar, NCL_comma);
 sprintf (cpar,"%d",nupts);
 status = ncl_add_token(cmdbuf, cpar, NCL_comma);
 sprintf (cpar,"%d",nulin);
 status = ncl_add_token(cmdbuf, cpar, NCL_comma);
 sprintf (cpar,"%d",nvpts);
 status = ncl_add_token(cmdbuf, cpar, NCL_comma);
 sprintf (cpar,"%d",nvlin);
 status = ncl_add_token(cmdbuf, cpar, NCL_nocomma);
*/
	status = ncl_add_token(cmdbuf, ncvpts_str, NCL_comma);
	status = ncl_add_token(cmdbuf, nupts_str, NCL_comma);
	status = ncl_add_token(cmdbuf, nulin_str, NCL_comma);
	status = ncl_add_token(cmdbuf, nvpts_str, NCL_comma);
	status = ncl_add_token(cmdbuf, nvlin_str, NCL_nocomma);
/*
.....If display tolerance has changed
.....Then output current command and start another
.....Bobby - 5/28/96
*/
	status = ncl_get_scalar_value(toler_str, &value);
	toler = value;
	if (fabs((toler-toler1)) > UM_FUZZ)
	{
		ncl_add_cmdbuf(cmdbuf);
		status = ncl_add_token(cmdbuf, NCL_set, NCL_nocomma);
		status = ncl_add_token(cmdbuf, NCL_cmd_adispl, NCL_comma);
		status = ncl_add_token(cmdbuf, NCL_show_toler, NCL_comma);
/*	ncl_sprintf (cpar,&toler,1); 
	status = ncl_add_token(cmdbuf, cpar, NCL_nocomma);
*/
		status = ncl_add_token(cmdbuf, toler_str, NCL_nocomma);
	}

	uu_dprint(UU_MTRC,(us,
		"final values cvpts%d, upaths %d, vpaths %d, ptsperu %d, ptsperv %d ",
		ncvpts, nupts, nulin, nvpts, nvlin));

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : nclu_set_autost()
**      Set autost paramters.
**    PARAMETERS
**       INPUT  :
**          form    - 1 = set, 2 = reset.
**          cmdbuf  - started command buffer.
**       OUTPUT :
**          cmdbuf  - completed command buffer.
**    RETURNS      : UU_SUCCESS if no error, otherwise UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
 nclu_set_autost(form,cmdbuf)
 int form;
 NCL_cmdbuf *cmdbuf;
{
	int status;
	UM_int2 ix, ival;
	UM_real8 rval;
	static UU_REAL toler, angtol;
	static int lomit, ltosrc;
	char toler_str[65], angtol_str[65];

 /* Array used to call ud_form */
	static int *form_ans[4];
/*
... Put up form with correct defaults, and traverse it 
*/
	ix    = 91;
	getsc(&ix,&rval);
	toler = rval;
	ix    = 92;
	getsc(&ix,&rval);
	if (rval >  1.0) rval =  1.0;
	if (rval < -1.0) rval = -1.0;
	angtol = acos(rval) * UM_RADIAN;
	ix     = 363;
	getifl (&ix,&ival);
	lomit  = ival;
	ltosrc = 0;
		
	ncl_sprintf (toler_str,&toler,1);
	ncl_sprintf (angtol_str,&angtol,1);
	form_ans[0] = (int*)toler_str;
	form_ans[1] = (int*)angtol_str;
	form_ans[2] = (int*)&lomit;
	form_ans[3] = (int*)&ltosrc;

	status = ud_form( "nautost.frm", form_ans, form_ans );
/*
... Build command line
*/
	if (status==UU_SUCCESS)
	{
		ncl_init_cmdbuf(cmdbuf);
		if (ltosrc) status = ncl_add_token(cmdbuf, "*", NCL_nocomma);
	}
	if (status==UU_SUCCESS)
	{
		if (form == 1)
			status = ncl_add_token(cmdbuf, NCL_set, NCL_nocomma);
		else
			status = ncl_add_token(cmdbuf, NCL_reset, NCL_nocomma);
	}
	if (status==UU_SUCCESS)
	{
		status = ncl_add_token(cmdbuf, NCL_autost, NCL_comma);
	}
	if (status==UU_SUCCESS)
	{
/*		ncl_sprintf (cpar,&toler,1); */
		status = ncl_add_token(cmdbuf, toler_str, NCL_comma);
	}
	if (status==UU_SUCCESS)
	{
/*		ncl_sprintf (cpar,&angtol,1); */
		status = ncl_add_token(cmdbuf, angtol_str, NCL_comma);
	}
	if (status==UU_SUCCESS)
	{
		if (lomit)
			status = ncl_add_token(cmdbuf, NCL_omit, NCL_nocomma);
		else
			status = ncl_add_token(cmdbuf, NCL_retain, NCL_nocomma);
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : nclu_reset_call(parms)
**      Set autost paramters.
**    PARAMETERS
**       INPUT  :
**          form    - 1 = set, 2 = reset.
**          cmdbuf  - started command buffer.
**       OUTPUT :
**          cmdbuf  - completed command buffer.
**    RETURNS      : UU_SUCCESS if no error, otherwise UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_reset_call(parms)
char *parms;
{
	int status;
   NCL_cmdbuf cmdbuf;
/*
.....Format command
*/
	ncl_init_cmdbuf(&cmdbuf);
	status = ncl_add_token(&cmdbuf,NCL_reset,NCL_nocomma);
	status = ncl_add_token(&cmdbuf,NCL_cmd_call1,NCL_comma);
/*
.....Prompt for number of calls to reset
*/
	if (strcmp(parms,"PROMPT") == 0)
		status = ncl_add_str(&cmdbuf, 245, NCL_nocomma);
/*
.....Output the command
*/
	if (status == NCL_OKINPUT || status == NCL_DONE)
	{
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
}

/*********************************************************************
**    E_FUNCTION     : nclu_cmd_run_from_to(kbeg,kend)
**       run command from line kbeg to kend from current PP files
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_cmd_run_from_to(kbeg,kend)
int *kbeg,*kend;
{
	int i, nc, end, status;
	NCL_cmdbuf cmdbuf;
	char lbuf[81];
	
	for (i=*kbeg-1; i<*kend;i++)
	{
		getsrcc(lbuf, &nc, &i, &end);
		if (end==1)
			break;
/*
.....we still need execute the empty statement since it involve
.....the line value setting and writing, otherwise, will missing a empty line
.....when writing, and cause some problems
//		if (nc==0) continue;
//		if (nc>=72) nc=71;
*/
		if (UL_comment_column != 0 && nc>=UL_comment_column)
			nc = UL_comment_column-1;
		ncl_init_cmdbuf(&cmdbuf);
		lbuf[nc] = '\0';	
		status = ncl_add_token(&cmdbuf, lbuf, NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}
}

/*********************************************************************
**    I_FUNCTION     : OnMotSel(fieldno,val,stat)
**       Method called when Motion button is pressed or the form is
**       called by user in menu parameters.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnMotSel(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status;
/*
.....Set up form fields
*/
	static UD_METHOD methods[] = {OnTogAstrt,UU_NULL, UU_NULL,UU_NULL,
		UU_NULL, UU_NULL,UU_NULL, UU_NULL, UU_NULL};
	static char called[] = {6,6, 6,6, 6, 6,6, 6};
	static char called2[] = {6,6, 6,6, 6, 6,6, 6, 6};
	char traverse[] = {1,0, 0,0, 1, 1,1, 1};
	static char display[] = {1,1, 1,1, 1, 1,1, 1};
   static int *ans[8];
/*
.....Output command should only be displayed if the form was called directly
.....and not through the main settings form
*/
	if (!dispOutCmdBox)
	{
		traverse[7] = 0;
		display[7] = 0;
	}
	else
	{
		traverse[7] = 1;
		display[7] = 1;
	}
	
	S_set_motion_settings(traverse);

	ans[0] = &motsets[0];
	ans[1] = &motsets[1];
	ans[2] = (int*)mtols_str[0];
	ans[3] = (int*)mtols_str[1];
	ans[4] = &motsets[2];
	ans[5] = &motsets[3];
	ans[6] = &motsets[4];
	ans[7] = &outcmdMot;
/*
.....Get the Form input
*/
	if (dispOutCmdBox)
	{
		Sfrm = ud_form1("setmotion.frm",ans,ans,methods,called,display,traverse);
		if (Sfrm == -1)
		{
			Sfrm = 0;
			goto done;
		}
	}
	else
	   Sfrm = ud_form_display1("setmotion.frm",ans,ans,methods,called2,display,traverse);
/*
.....Set change settings flag so the settings are updated
*/
	changeMot = 1;
/*
.....End of routine
*/
done:;
   return(UD_FLDOK);
}
/*********************************************************************
**    I_FUNCTION     : OnGeoSel(fieldno,val,stat)
**       Method called when Geometry button is pressed or the form is
**       called by user in menu parameters.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnGeoSel(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	extern UD_FILTREC UD_filter;  /* attribute filter */
	int status,i,ifl,tval;
   NCL_cmdbuf cmdbuf;
/*
.....Set up form fields
*/
	static UD_METHOD methods[] = {UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL,UU_NULL, UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		UU_NULL, OnSelDesel,OnSelDesel, UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL, UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL, UU_NULL, OnSelectDClass,UU_NULL,
		UU_NULL, UU_NULL,UU_NULL, UU_NULL,UU_NULL, UU_NULL, UU_NULL};
	static char called[] = {6,6,6,6, 6,6,6,6, 6,6,6,6, 6, 6,6, 6,6,6, 6,6,6,
		6,6,6, 6,6,6, 6, 6,6, 6, 6,6, 6,6, 6};
	static char called2[] = {6,6,6,6, 6,6,6,6, 6,6,6,6, 6, 6,6, 6,6,6, 6,6,6,
		6,6,6, 6,6,6, 6, 6,6, 6, 6,6, 6,6, 6, 6};
	char traverse[] = {1,1,1,1, 1,1,1,1, 1,1,1,1, 1, 1,1, 1,1,1, 1,1,1, 1,1,1,
		1,1,1, 1, 1,1, 0, 0,0, 0,0, 1};
	static char display[] = {1,1,1,1, 1,1,1,1, 1,1,1,1, 1, 1,1, 1,1,1, 1,1,1,
		1,1,1, 1,1,1, 1, 1,1, 1, 1,1, 1,1, 1};
   static int *ans[36];
/*
.....Output command should only be displayed if the form was called directly
.....and not through the main settings form
*/
	if (!dispOutCmdBox)
	{
		traverse[35] = 0;
		display[35] = 0;
	}
	else
	{
		traverse[35] = 1;
		display[35] = 1;
	}
/*
.....Set up data
*/
	S_set_geo_settings(traverse);

	for (i=0;i<NUMGEO;i++) 
	{
		ans[i] = &dispgeo[i];
		ans[i+NUMGEO+2] = (int*)labls[i];
	}
	ans[28] = &reschoice;
	ans[29] = (int*)cvdisp_str[0];
	for (i=0;i<5;i++) ans[i+30] = (int*)cvdisp_str[i+1];
	ans[35] = &outcmdGeo;
/*
.....Get the Form input
*/
	if (dispOutCmdBox)
	{
		Sfrm = ud_form1("setgeo.frm",ans,ans,methods,called,display,traverse);
		if (Sfrm == -1)
		{
			Sfrm = 0;
			goto done;
		}
	}
	else 
		Sfrm = ud_form_display1("setgeo.frm",ans,ans,methods,called2,display,traverse);
/*
.....Set change settings flag so the settings are updated
*/
	changeGeo = 1;
/*
.....End of routine
*/
done:;
   return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     :  S_set_main_settings(traverse)
**       Sets the main form field values prior to display.
**    PARAMETERS
**       INPUT  : traverse array
**       OUTPUT : none
**    RETURNS      : UU_TRUE if attributes can be set for this entity.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_set_main_settings(traverse)
char traverse[];
{
	int ifl,lfl,i,sci;
	UU_REAL sc;
	UM_int2 tval,tflg;
/*
.....Get current settings
*/
	ifl = 392; getifl(&ifl,&mainsets[0]);  /* runcmd */
	ifl = 393; getifl(&ifl,&mainsets[1]);  /* stpcmd */
	lfl = 122; getlfl(&lfl,&mainsets[2]);  /* ignore stop */
	lfl = 121; getlfl(&lfl,&mainsets[3]);  /* ignore pause */
	lfl = 7;   getlfl(&lfl,&mainsets[4]);  /* ignore warnings */
	lfl = 126; getlfl(&lfl,&mainsets[5]);  /* case sensitive */
	sci = 169; getsc(&sci,&ver);			   /* version */
	lfl = 129; getlfl(&lfl,&mainsets[6]);  /* header */
	lfl = 1;   getlfl(&lfl,&mainsets[7]);  /* comments */
	lfl = 168; getlfl(&lfl,&mainsets[8]);  /* remarks */
	ifl = 371; getifl(&ifl,&mainsets[9]);  /* circular choice */
	ifl = 353; getifl(&ifl,&mainsets[10]); /* tracut */
	ifl = 307; getifl(&ifl,&mainsets[11]); /* cutter choice */
	ifl = 390; getifl(&ifl,&cutacc);			/* cutter accuracy */
	ifl = 308; getifl(&ifl,&mainsets[12]); /* simulation */
	lfl = 170; getlfl(&lfl,&mainsets[13]); /* pprint ipv */
	ifl = 319; getifl(&ifl,&mainsets[14]); /* output accuracy */
	ifl = 386; getifl(&ifl,&mainsets[15]); /* integer output */

	for (i=0;i<16;i++) Smainsets[i] = (mainsets[i])? 1 : 0;
	Smainsets[9] = mainsets[9];
	Smainsets[11] = mainsets[11];

	ncl_sprintf(&mainsets_str[0],&ver,1);
	sprintf(mainsets_str[1],"%d",cutacc);

	strcpy(Smainsets_str[0],mainsets_str[0]);
	strcpy(Smainsets_str[1],mainsets_str[1]);

	nclu_load_matrix_list(&Smxlist,&Smxname); /* generate matrix list */

	traverse[12] = (mainsets[10])? 1 : 0;

	if (strcmp(Smxlist.answer,"") == 0) strcpy(Smxname,"");

	return(UU_TRUE);
}
/*********************************************************************
**    S_FUNCTION     :  S_set_geo_settings()
**       Sets the geometry form field values prior to display.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : UU_TRUE if attributes can be set for this entity.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_set_geo_settings(traverse)
char traverse[];
{
	int ifl,lfl,i,tint;
	UU_REAL sc;
	UM_int2 tval,tflg;

	int rels[] = {1,2,3,82,80,98,92,81,21,83,77,91,87};
/*
.....Get displayable flags for geometric entities
*/
	lfl = 15;  getlfl(&lfl,&tflg);
	dispgeo[0] = (tflg)? 1 : 0; /* point */
	lfl = 17;  getlfl(&lfl,&tflg);
	dispgeo[1] = (tflg)? 1 : 0; /* line */
	lfl = 18;  getlfl(&lfl,&tflg);
	dispgeo[2] = (tflg)? 1 : 0; /* circle */
	lfl = 19;  getlfl(&lfl,&tflg);
	dispgeo[3] = (tflg)? 1 : 0; /* curve */
	lfl = 16;  getlfl(&lfl,&tflg);
	dispgeo[4] = (tflg)? 1 : 0; /* vector */
	lfl = 74;  getlfl(&lfl,&tflg);
	dispgeo[5] = (tflg)? 1 : 0; /* point vector*/
	lfl = 51;  getlfl(&lfl,&tflg);
	dispgeo[6] = (tflg)? 1 : 0; /* pattern */
	lfl = 89; getlfl(&lfl,&tflg);
	dispgeo[7] = (tflg)? 1 : 0; /* matrix */
	lfl = 164; getlfl(&lfl,&tflg);
	dispgeo[8] = (tflg)? 1 : 0; /* solid */
	lfl = 20;  getlfl(&lfl,&tflg);
	dispgeo[9] = (tflg)? 1 : 0; /* surface */
	lfl = 157;  getlfl(&lfl,&tflg);
	dispgeo[10] = (tflg)? 1 : 0; /* annotation */
	lfl = 39;  getlfl(&lfl,&tflg);
	dispgeo[11] = (tflg)? 1 : 0; /* plane */
	lfl = 21;  getlfl(&lfl,&tflg);
	dispgeo[12] = (tflg)? 1 : 0; /* shape */

	for (i=0;i<NUMGEO;i++) Sdisp[i] = dispgeo[i];
/*
.....Get starting indexes for geometry entities
*/
	for (i=0;i<NUMGEO;i++)
	{
		sprintf(labls[i],"%d",UM_labelmdl.next[UM_labelmdl.rel[rels[i]]]);
		Slabls[i] = UM_labelmdl.next[UM_labelmdl.rel[rels[i]]];
	}
/*
.....Get curve/surface display parameters
*/
	ifl = 136; getifl(&ifl,&tint);
	cvdisp_val[0] = tint;
	cvdisp_val[1] = UM_srfattr.numupaths;
	cvdisp_val[2] = UM_srfattr.numvpaths;
	cvdisp_val[3] = UM_srfattr.ptsperucrv;
	cvdisp_val[4] = UM_srfattr.ptspervcrv;
	ifl = 175; getsc(&ifl,&tolr);

	ncl_sprintf (cvdisp_str[0],&tolr,1);
	for (i=0;i<5;i++) 
	{
		sprintf (cvdisp_str[i+1],"%.0f",cvdisp_val[i]);
		Scvdisp[i] = cvdisp_val[i]; 
	}
	Stolr = tolr; 
/*
.....Set up traverse based on previous resolution choice
*/
	if (reschoice == 0) traverse[25] = 1;
	else
	{
		traverse[30] = 1;
		traverse[31] = 1;
		traverse[32] = 1;
		if (reschoice == 2)
		{
			traverse[33] = 1;
			traverse[34] = 1;
		}
	}

	return(UU_TRUE);
}
/*********************************************************************
**    S_FUNCTION     :  S_set_motion_settings()
**       Sets the motion form field values prior to display.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : UU_TRUE if attributes can be set for this entity.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_set_motion_settings(traverse)
char traverse[];
{
	int ifl,lfl,i,sc;
	UM_int2 tval,tflg;
	UU_REAL ltol,rval,angtol;
	UU_LOGICAL tog;
/*
.....Set up motion values
*/
	sc = 91;	getsc(&sc,&mtols[0]);
	sc = 92;	getsc(&sc,&mtols[1]);
	if (mtols[1] >  1.0) mtols[1] =  1.0;
	if (mtols[1] < -1.0) mtols[1] = -1.0;
	mtols[1] = acos(mtols[1]) * UM_RADIAN;
		
	ncl_sprintf (mtols_str[0],&mtols[0],1); /* linear tolerance */
	ncl_sprintf (mtols_str[1],&mtols[1],1); /* angular tolerance */
	lfl = 2;   getlfl(&lfl,&tflg); /* auto start */
	motsets[0] = (tflg)? 1 : 0;
	ifl = 363; getifl(&ifl,&tflg); /* omit first point */
	motsets[1] = (tflg)? 1 : 0;
	lfl = 72;  getlfl(&lfl,&tflg); /* auto uv settings */
	motsets[2] = (tflg)? 1 : 0;
	lfl = 3;   getlfl(&lfl,&tflg); /* show motion */
	motsets[3] = (tflg)? 1 : 0;
	lfl = 102; getlfl(&lfl,&tflg); /* output expanded CL files */
	motsets[4] = (tflg)? 1 : 0;

	for (i=0;i<5;i++) Smotsets[i] = motsets[i];
	Smtols[0] = mtols[0]; Smtols[1] = mtols[1];

	traverse[1] = traverse[2] = traverse[3] = motsets[0];

	return(UU_TRUE);
}
/*********************************************************************
**    S_FUNCTION     :  S_mod_motion_settings()
**       Update the settings given from the Motion Settings form.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : UU_TRUE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_mod_motion_settings()
{
	int i,status,outcmd;
   NCL_cmdbuf cmdbuf;

	static char *rbuf[] = {"AUTOUV","MOTION","EXPCL"};
	
	outcmd = 0;
	ncl_init_cmdbuf(&cmdbuf);
/*
.....Auto start setting
*/
	if (Smotsets[0] != motsets[0] || (motsets[0] && (fabs((mtols[0]-Smtols[0])) > UM_FUZZ
		|| fabs((mtols[1]-Smtols[1])) > UM_FUZZ || Smotsets[1] != motsets[1])))
	{
		if (motsets[0])
		{
			if (outcmdMot || outcmdMain) ncl_add_token(&cmdbuf, "**SET/AUTOST", NCL_comma);
			else ncl_add_token(&cmdbuf, "*SET/AUTOST", NCL_comma);
		}
		else
		{
			if (outcmdMot || outcmdMain) ncl_add_token(&cmdbuf, "**RESET/AUTOST", NCL_comma);
			else ncl_add_token(&cmdbuf, "*RESET/AUTOST", NCL_comma);
		}
		if (fabs((mtols[0]-Smtols[0])) > UM_FUZZ || fabs((mtols[1]-Smtols[1])) > UM_FUZZ
			 || Smotsets[1] != motsets[1])
		{
			ncl_add_token(&cmdbuf,mtols_str[0],NCL_comma);
			ncl_add_token(&cmdbuf,mtols_str[1],NCL_comma);
			if (motsets[1]) ncl_add_token(&cmdbuf,"OMIT",NCL_nocomma);
			else ncl_add_token(&cmdbuf,"RETAIN",NCL_nocomma);
			Smotsets[1] = motsets[1];
		}
		outcmd = 1;
		ncl_add_cmdbuf(&cmdbuf);
		Smotsets[0] = motsets[0];
	}
/*
.....Check for additional settings change
*/
	for (i=2;i<5;i++)
	{
		if (Smotsets[i] != motsets[i])
		{
			if (Smotsets[i] == 0)
			{
				if (outcmdMot || outcmdMain) ncl_add_token(&cmdbuf, "**SET/", NCL_nocomma);
				else ncl_add_token(&cmdbuf, "*SET/", NCL_nocomma);
			}
			else
			{
				if (outcmdMot || outcmdMain) ncl_add_token(&cmdbuf, "**RESET/", NCL_nocomma);
				else ncl_add_token(&cmdbuf, "*RESET/", NCL_nocomma);
			}
			ncl_add_token(&cmdbuf,rbuf[i-2],NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
			Smotsets[i] = motsets[i];
			outcmd = 1;
		}
	}
/*
.....Output *(RE)SET command(s) if cmdbuf has been filled
*/
	if (outcmd)
	{
		ncl_set_cmdmode(UU_TRUE);
		ncl_call(&cmdbuf);
	}

	return(UU_TRUE);
}
/*********************************************************************
**    S_FUNCTION     :  S_mod_geo_settings()
**       Update the settings given from the Geometry Settings form.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : UU_TRUE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_mod_geo_settings()
{
	int i,status,marked;
   NCL_cmdbuf cmdbuf,cmdbuf2;
	UU_LOGICAL changed,outcmd,outcmd2,changeCV;
	
	static char *rbuf[] = {"PT","LN","CI","CV","VE","PV","PN","MX","SOLID",
		"SF","ANOTE","PL","SH"};

	marked = 0;
	changed = changeCV = outcmd = outcmd2 = UU_FALSE;
	ncl_init_cmdbuf(&cmdbuf);
	ncl_init_cmdbuf(&cmdbuf2);
/*
.....Display settings
*/
	if (outcmdGeo || outcmdMain)
	{
		ncl_add_token(&cmdbuf, "**SET/DISPLY", NCL_comma);
		ncl_add_token(&cmdbuf2, "**RESET/DISPLY", NCL_comma);
	}
	else 
	{
		status = ncl_add_token(&cmdbuf, "*SET/DISPLY", NCL_comma);
		status = ncl_add_token(&cmdbuf2, "*RESET/DISPLY", NCL_comma);
	}
	for (i=0;i<NUMGEO;i++)
	{
		if (Sdisp[i] != dispgeo[i]) changed = UU_TRUE;
		marked += dispgeo[i];
	}
	if (changed)
	{
		if (marked == NUMGEO)
			outcmd = UU_TRUE;
		else if (marked == 0)
			outcmd2 = UU_TRUE;
		else
		{
			for (i=0;i<NUMGEO;i++)
			{
				if (Sdisp[i] != dispgeo[i])
				{
					Sdisp[i] = dispgeo[i];			
					if (dispgeo[i] == 1)
					{
						ncl_add_token(&cmdbuf, rbuf[i], NCL_comma);
						outcmd = UU_TRUE;
					}
					else
					{
						ncl_add_token(&cmdbuf2, rbuf[i], NCL_comma);
						outcmd2 = UU_TRUE;
					}
				}
			}
		}
	}
	if (outcmd) ncl_add_cmdbuf(&cmdbuf);
	else ncl_init_cmdstr(&cmdbuf);
	if (outcmd2) ncl_add_cmdbuf(&cmdbuf2);
/*
.....Label settings
*/
	for (i=0;i<NUMGEO;i++)
	{
		if (Slabls[i] != rlabls[i])
		{
			if (outcmdGeo || outcmdMain) ncl_add_token(&cmdbuf, "**SET/", NCL_nocomma);
			else ncl_add_token(&cmdbuf, "*SET/", NCL_nocomma);
			Slabls[i] = rlabls[i];			
			ncl_add_token(&cmdbuf, rbuf[i], NCL_comma);
			ncl_add_token(&cmdbuf, labls[i],NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
			outcmd = UU_TRUE;
		}
	}
/*
.....Curve/Surface settings
*/
	for (i=1;i<5;i++) 
	{
		if (Scvdisp[i] != cvdisp_val[i])
		{
			changeCV = UU_TRUE;
			break;
		}
	}
	if (changeCV)
	{
		if (outcmdGeo || outcmdMain) ncl_add_token(&cmdbuf, "**SET/ADISPL", NCL_comma);
		else status = ncl_add_token(&cmdbuf, "*SET/ADISPL", NCL_comma);
		ncl_add_token(&cmdbuf, cvdisp_str[1], NCL_comma);
/*
.....Line settings
*/
		if (reschoice == 2)
		{
			ncl_add_token(&cmdbuf, cvdisp_str[5], NCL_comma);
			ncl_add_token(&cmdbuf, cvdisp_str[3], NCL_comma);
			ncl_add_token(&cmdbuf, cvdisp_str[4], NCL_comma);
			ncl_add_token(&cmdbuf, cvdisp_str[2], NCL_nocomma);
		}
/*
.....Point settings
*/
		else
		{
			ncl_add_token(&cmdbuf, cvdisp_str[2], NCL_comma);
			ncl_add_token(&cmdbuf, cvdisp_str[3], NCL_nocomma);
		}
		ncl_add_cmdbuf(&cmdbuf);
		outcmd = UU_TRUE;
	}
/*
.....Tolerance settings
*/
	if (fabs((Stolr-tolr)) > UM_FUZZ)
	{
		if (outcmdGeo || outcmdMain) ncl_add_token(&cmdbuf, "**SET/ADISPL,TOLER", NCL_comma);
		else ncl_add_token(&cmdbuf, "*SET/ADISPL,TOLER", NCL_comma);
		ncl_add_token(&cmdbuf, cvdisp_str[0], NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		outcmd = UU_TRUE;
	}
/*
.....Output *(RE)SET command(s) if cmdbuf has been filled
*/
	if (outcmd)
	{
		ncl_set_cmdmode(UU_TRUE);
		ncl_call(&cmdbuf);
	}
	if (outcmd2)
	{
		ncl_set_cmdmode(UU_TRUE);
		ncl_call(&cmdbuf2);
	}
	return(UU_TRUE);
}
/*********************************************************************
**    S_FUNCTION     :  S_mod_main_settings()
**       Update the settings given from the General Settings form.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : UU_TRUE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_mod_main_settings()
{
	int i,status,outcmd;
   NCL_cmdbuf cmdbuf;
	
	static char *rbuf[] = {"RUNCMD","STPCMD","STOP","PAUSE","NOWARN",
							"CASE","VER","DATA","REMARK","REMARK,ACTIVE",
							"CIRCUL,PLANAR","CIRCUL,ALL","TRACUT","CUTTER,APT",
							"CUTTER,NCL","CUTTER,PPRINT","CUTTER",
							"IPV","IPVCOM","LOW","HIGH","REAL"};

	outcmd = 0;
	ncl_init_cmdbuf(&cmdbuf);
/*
.....SET and RESET will be handled by the same form so both commands will
.....built at the same time
*/
/*
.....RUNCMD,STPCMD,STOP,PAUSE,NOWARN,CASE
*/
	for (i=0;i<6;i++)
	{
		if (mainsets[i] != Smainsets[i])
		{
			if (mainsets[i])
			{
				if (outcmdMain) ncl_add_token(&cmdbuf, "**SET/", NCL_nocomma);
				else ncl_add_token(&cmdbuf, "*SET/", NCL_nocomma);
			}
			else
			{
				if (outcmdMain) ncl_add_token(&cmdbuf, "**RESET/", NCL_nocomma);
				else ncl_add_token(&cmdbuf, "*RESET/", NCL_nocomma);
			}
			ncl_add_token(&cmdbuf, rbuf[i], NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
			outcmd = 1;
			Smainsets[i] = mainsets[i];
		}
	}
/*
.....Set version
*/
	if (strcmp(Smainsets_str[0],mainsets_str[0]) != 0)
	{
		if (outcmdMain) ncl_add_token(&cmdbuf, "**SET/", NCL_nocomma);
		else ncl_add_token(&cmdbuf, "*SET/", NCL_nocomma);
		ncl_add_token(&cmdbuf, rbuf[6], NCL_comma);
		ncl_add_token(&cmdbuf, mainsets_str[0], NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		outcmd = 1;
		strcpy(Smainsets_str[0],mainsets_str[0]);
	}
/*
.....APTSRC settings
*/
	for (i=6;i<9;i++)
	{
		if (mainsets[i] != Smainsets[i])
		{
			if (mainsets[i])
			{
				if (outcmdMain) ncl_add_token(&cmdbuf, "**SET/APTSRC", NCL_comma);
				else ncl_add_token(&cmdbuf, "*SET/APTSRC", NCL_comma);
			}
			else
			{
				if (outcmdMain) ncl_add_token(&cmdbuf, "**RESET/APTSRC", NCL_comma);
				else ncl_add_token(&cmdbuf, "*RESET/APTSRC", NCL_comma);
			}
			ncl_add_token(&cmdbuf, rbuf[i+1], NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
			outcmd = 1;
			Smainsets[i] = mainsets[i];
		}
	}
/*
.....Circular settings
*/
	if (mainsets[9] != Smainsets[9])
	{
		if (outcmdMain) ncl_add_token(&cmdbuf, "**SET/APTSRC", NCL_comma);
		else ncl_add_token(&cmdbuf, "*SET/APTSRC", NCL_comma);
		if (!mainsets[9]) ncl_add_token(&cmdbuf, rbuf[10], NCL_nocomma);
		else ncl_add_token(&cmdbuf, rbuf[11], NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		outcmd = 1;
		Smainsets[9] = mainsets[9];
	}
/*
.....Tracut settings
*/
	if (mainsets[10] != Smainsets[10] ||
		(mainsets[10]  && strcmp(Smxname,Smxlist.answer) != 0 &&
		strcmp(Smxlist.answer," ") != 0))
	{
		if (mainsets[10])
		{
			if (outcmdMain) ncl_add_token(&cmdbuf, "**SET/APTSRC", NCL_comma);
			else ncl_add_token(&cmdbuf, "*SET/APTSRC", NCL_comma);
			ncl_add_token(&cmdbuf, rbuf[12], NCL_comma);
			if (strcmp(Smxname,Smxlist.answer) != 0)
				ncl_add_token(&cmdbuf, Smxlist.answer, NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
			outcmd = 1;
			strcpy(Smxname,Smxlist.answer);
		}
		else
		{
			if (outcmdMain) ncl_add_token(&cmdbuf, "**RESET/APTSRC", NCL_comma);
			else ncl_add_token(&cmdbuf, "*RESET/APTSRC", NCL_comma);
			ncl_add_token(&cmdbuf, rbuf[12], NCL_nocomma);
			ncl_add_cmdbuf(&cmdbuf);
			outcmd = 1;
			strcpy(Smxname,"");
		}
		Smainsets[10] = mainsets[10];
	}
/*
.....Cutter settings
*/
	if (mainsets[11] != Smainsets[11])
	{
		if (outcmdMain) ncl_add_token(&cmdbuf, "**SET/APTSRC", NCL_comma);
		else ncl_add_token(&cmdbuf, "*SET/APTSRC", NCL_comma);
		if (mainsets[11] == 0) ncl_add_token(&cmdbuf, rbuf[13], NCL_nocomma);
		else if (mainsets[11] == 1) ncl_add_token(&cmdbuf, rbuf[14], NCL_nocomma);
		else if (mainsets[11] == 2) ncl_add_token(&cmdbuf, rbuf[15], NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		outcmd = 1;
		Smainsets[11] = mainsets[11];
	}
/*
.....Cutter accuracy
*/
	if (strcmp(Smainsets_str[1],mainsets_str[1]) != 0) 
	{
		if (outcmdMain) ncl_add_token(&cmdbuf, "**SET/APTSRC", NCL_comma);
		else ncl_add_token(&cmdbuf, "*SET/APTSRC", NCL_comma);
		ncl_add_token(&cmdbuf, rbuf[16], NCL_comma);
		ncl_add_token(&cmdbuf, mainsets_str[1],NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		outcmd = 1;
	}
/*
.....Simulation
*/
	if (mainsets[12] != Smainsets[12])
	{
		if (mainsets[12])
		{
			if (outcmdMain) ncl_add_token(&cmdbuf, "**SET/APTSRC", NCL_comma);
			else ncl_add_token(&cmdbuf, "*SET/APTSRC", NCL_comma);
		}
		else
		{
			if (outcmdMain) ncl_add_token(&cmdbuf, "**RESET/APTSRC", NCL_comma);
			else ncl_add_token(&cmdbuf, "*RESET/APTSRC", NCL_comma);
		}
		ncl_add_token(&cmdbuf, rbuf[17], NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		outcmd = 1;
		Smainsets[12] = mainsets[12];
	}
/*
.....PPRINT IPV
*/
	if (mainsets[13] != Smainsets[13])
	{
		if (mainsets[13])
		{
			if (outcmdMain) ncl_add_token(&cmdbuf, "**SET/APTSRC", NCL_comma);
			else ncl_add_token(&cmdbuf, "*SET/APTSRC", NCL_comma);
		}
		else
		{
			if (outcmdMain) ncl_add_token(&cmdbuf, "**RESET/APTSRC", NCL_comma);
			else ncl_add_token(&cmdbuf, "*RESET/APTSRC", NCL_comma);
		}
		ncl_add_token(&cmdbuf, rbuf[18], NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		outcmd = 1;
		Smainsets[13] = mainsets[13];
	}
/*
.....Output Accuracy
*/
	if (mainsets[14] != Smainsets[14])
	{
		if (outcmdMain) ncl_add_token(&cmdbuf, "**SET/APTSRC", NCL_comma);
		else ncl_add_token(&cmdbuf, "*SET/APTSRC", NCL_comma);
		if (mainsets[14]) ncl_add_token(&cmdbuf, rbuf[20], NCL_nocomma);
		else ncl_add_token(&cmdbuf, rbuf[19], NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		outcmd = 1;
		Smainsets[14] = mainsets[14];
	}
/*
.....Integer output
*/
	if (mainsets[15] != Smainsets[15])
	{
		if (mainsets[15])
		{
			if (outcmdMain) ncl_add_token(&cmdbuf, "**SET/APTSRC", NCL_comma);
			else ncl_add_token(&cmdbuf, "*SET/APTSRC", NCL_comma);
		}
		else
		{
			if (outcmdMain) ncl_add_token(&cmdbuf, "**RESET/APTSRC", NCL_comma);
			else ncl_add_token(&cmdbuf, "*RESET/APTSRC", NCL_comma);
		}
		ncl_add_token(&cmdbuf, rbuf[21], NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
		outcmd = 1;
		Smainsets[15] = mainsets[15];
	}
/*
.....Output *(RE)SET command(s) if cmdbuf has been filled
*/
	if (outcmd)
	{
		ncl_set_cmdmode(UU_TRUE);
		ncl_call(&cmdbuf);
	}

	return(UU_TRUE);
}
/*********************************************************************
**   I_FUNCTION: OnSelectDClass(fieldno,val,stat)
**      Callback function for a list Selection from the Define Scalar form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnSelectDClass()
{
	UU_LOGICAL flag = UU_FALSE;
/*
.....Toler chosen so disable fields not related to tolerance
*/
   if (reschoice == 0) 
	{
		ud_setfrm_traverse_mask(Sfrm,29,UU_TRUE);
		ud_setfrm_traverse_mask(Sfrm,30,UU_FALSE);
		ud_setfrm_traverse_mask(Sfrm,31,UU_FALSE);
		ud_setfrm_traverse_mask(Sfrm,32,UU_FALSE);
		ud_setfrm_traverse_mask(Sfrm,33,UU_FALSE);
		ud_setfrm_traverse_mask(Sfrm,34,UU_FALSE);
	}
/*
.....Point or Line chosen
*/
	else
	{
/*
.....These fields are needed for both choices
*/
		ud_setfrm_traverse_mask(Sfrm,29,UU_FALSE);
		ud_setfrm_traverse_mask(Sfrm,30,UU_TRUE);
		ud_setfrm_traverse_mask(Sfrm,31,UU_TRUE);
		ud_setfrm_traverse_mask(Sfrm,32,UU_TRUE);
/*
.....Additional fields are available for Points
*/
		if (reschoice == 2) flag = UU_TRUE;
		ud_setfrm_traverse_mask(Sfrm,33,flag);
		ud_setfrm_traverse_mask(Sfrm,34,flag);
	}
   return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnTogAstrt(fieldno,val,stat)
**     Set traverse based on auto start checkbox. The settings fields 
**     sould not be accessible unless autostart is selected.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnTogAstrt(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL tog=UU_TRUE;
	
	if (motsets[0] == 0) tog = UU_FALSE;
   ud_setfrm_traverse_mask(Sfrm,1,tog);
	ud_setfrm_traverse_mask(Sfrm,2,tog);
	ud_setfrm_traverse_mask(Sfrm,3,tog);

   return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnSelDesel(fieldno,val,stat)
**     Set all geometry display boxes when Select All or Clear All
**     is pressed.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnSelDesel(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{	
	int i;
	UU_LOGICAL flag = UU_TRUE;

	if (*fieldno == 14) flag = UU_FALSE;
	for (i=0;i<NUMGEO;i++) ud_dispfrm_update_answer(Sfrm,i,&flag);

   return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnTrcTog(fieldno,val,stat)
**     Set matrix menu traverse based on tracut checkbox.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnTrcTog(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{	
	UU_LOGICAL tog = UU_TRUE;

	if (!mainsets[10]) tog = UU_FALSE;
   ud_set_traverse_mask(12,tog);

   return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnAutoSaveTog(fieldno,val,stat)
**     Toggle traverse flags for time/changes and type fields.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnAutoSaveTog(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{	
	UU_LOGICAL flag;
	switch(*fieldno)
	{
	case UAUT:
		if (Sason) flag = UU_TRUE;
		else flag = UU_FALSE;
		ud_set_traverse_mask(UTIM,flag);
		ud_set_traverse_mask(UTYP,flag);
		break;
	case PAUT:
		if (Sppgon) flag = UU_TRUE;
		else flag = UU_FALSE;
		ud_set_traverse_mask(PTIM,flag);
		ud_set_traverse_mask(PTYP,flag);
		ud_set_traverse_mask(PSVS,flag);
		break;
	}
   return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnSnapSave(fieldno,val,stat)
**     Call the appropriate snap saving routine.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnSnapSave(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch(*fieldno)
	{
	case USNP:
		ur_snap_save();
		break;
	case PSNP:
		ncl_snap_save();
		break;
	}
   return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnRestore(fieldno,val,stat)
**     Call the appropriate snap saving routine.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnRestore(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch(*fieldno)
	{
	case URES:
		ur_recover_part();
		break;
	case PRES:
		ncl_recover_ppg();
		break;
	}
   return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : S_save_auosavemod
**       Save the autosave settings into modals file.
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
static int S_save_autosavemod()
{
	int i,stat;
	char msg[80];
	UX_pathname fname;
	FILE *fptr;
/*
.....Initialize routine
*/
	stat = UU_SUCCESS;
/*
.....Open modals file
*/
	strcpy(fname,"ncl_autosave.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS","modals",UU_NULL,UU_NULL,fname,
		3,&fptr);
	if (stat != UU_SUCCESS || fptr == UU_NULL) goto done;
/*
.....Store unibase modals
*/	
	ux_fputs0("#AUTOSAVE#\n", fptr);
	if (UR_ason)
		ux_fputs0("/AUTO_U/ *ON\n", fptr);
	else
		ux_fputs0("/AUTO_U/ *OFF\n", fptr);
	if (UR_switch == 0)
	{
		ux_fputs0("/TYPE_U/ *TIME\n", fptr);
		sprintf(msg,"/INTERVAL_U/ %d\n",UR_time/60);
	}
	else
	{
		ux_fputs0("/TYPE_U/ *CHANGES\n", fptr);
		sprintf(msg,"/INTERVAL_U/ %d\n",UR_num_chg);
	}
	ux_fputs0(msg, fptr);
/*
.....Store part program modals
*/
	if (NCL_ason)
		ux_fputs0("/AUTO_PP/ *ON\n", fptr);
	else
		ux_fputs0("/AUTO_PP/ *OFF\n", fptr);
	if (NCL_switch == 0)
	{
		ux_fputs0("/TYPE_PP/ *TIME\n", fptr);
		sprintf(msg,"/INTERVAL_PP/ %d\n",NCL_time/60);
	}
	else
	{
		ux_fputs0("/TYPE_PP/ *CHANGES\n", fptr);
		sprintf(msg,"/INTERVAL_PP/ %d\n",NCL_num_chg);
	}
	ux_fputs0(msg, fptr);
	sprintf(msg,"/MAX_SAVE_PP/ %d\n",NCL_max_saves);
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

/*********************************************************************
**    E_FUNCTION     : nclu_auto_save()
**       Handle auto save interface
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_auto_save()
{
   int i,tval,status,uni_on,ppg_on;
	char utime[64],ptime[64],csaves[64];
	UU_REAL ttime,saves;

	static int *ans[21];
	static UD_METHOD methods[] = {OnAutoSaveTog, UU_NULL,UU_NULL,
		OnSnapSave,OnRestore, OnAutoSaveTog, UU_NULL,UU_NULL,UU_NULL,
		OnSnapSave,OnRestore};
	static char called[] = {6, 6,6, 6,6, 6, 6,6, 6, 6,6};
	char traverse[] = {1, 0,0, 1,1, 1, 0,0, 0, 1,1};
	static char display[] = {1, 1,1, 1,1, 1, 1,1, 1, 1,1};
/*
.....Save previous on flags.
*/
	Sason = uni_on = UR_ason;
	Sppgon = ppg_on = NCL_ason;
/*
.....Set up parameters.
*/
	if (UR_switch == 0) tval = UR_time/60;
	else tval = UR_num_chg;
	sprintf(utime,"%d",tval);
	if (NCL_switch == 0) tval = NCL_time/60;
	else tval = NCL_num_chg;
	sprintf(ptime,"%d",tval);
	sprintf(csaves,"%d",NCL_max_saves);

	traverse[UTIM] = traverse[UTYP] = UR_ason;
	traverse[PTIM] = traverse[PTYP] = traverse[PSVS] = NCL_ason;

	ans[UAUT] = &Sason;
	ans[UTIM] = (int *)&utime;
	ans[UTYP] = &UR_switch;
	ans[PAUT] = &Sppgon;
	ans[PTIM] = (int *)&ptime;
	ans[PTYP] = &NCL_switch;
	ans[PSVS] = (int *)&csaves;
	ans[USNP] = UU_NULL;
	ans[URES] = UU_NULL;
	ans[PSNP] = UU_NULL;
	ans[PRES] = UU_NULL;
/*
....Bring up form.
*/
	status = ud_form1("autosave.frm",ans,ans,methods,called,display,traverse);
	if (status == -1) goto done;
/*
.....Save unibase auto save settings.
*/
	ncl_get_scalar_value(utime,&ttime);
	if (UR_switch == 0) UR_time = (int)(ttime*60.);
	else UR_num_chg = (int)ttime;
/*
.....Fixed setting auto save flags - ASF 2/19/14.
*/
	if (Sason != uni_on)
	{
		if (Sason) ur_enable_auto_save();
		else ur_disable_auto_save();
	}
/*
.....Save part program auto save settings.
*/
	ncl_get_scalar_value(ptime,&ttime);
	if (NCL_switch == 0) NCL_time = (int)(ttime*60.);
	else NCL_num_chg = (int)ttime;
	ncl_get_scalar_value(csaves,&saves);
	if (saves < 0.) saves = 0.;
	NCL_max_saves = (int)saves;
	NCL_ason = Sppgon;
	if (NCL_ason && NCL_ason != ppg_on) NCL_last_autosave = time((long *) 0);
	S_save_autosavemod();
done:;
   uu_dexit;
   return(0);
}
