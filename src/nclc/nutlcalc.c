/*********************************************************************
**		NAME:  nutlcalc.c
**		CONTAINS:
**			nclu_tool_calc
**			nclu_tool_calc_form
**			nclu_tool_calc_set
**			nclu_tool_calc_command
**    COPYRIGHT 2015 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nutlcalc.c , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**       05/22/18 , 10:25:38
*********************************************************************/

#include "ncldef.h"
#include "nclfc.h"
#include "nconst.h"
#include "nccs.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"
#include "dselmask.h"
#include "mdattr.h"
#include "mdpick.h"
#include "modef.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "uhep.h"
#include "view.h"
#include "view1.h"

enum
{
	MDP_DIS,/*Max Step */
	MDP_STP,/*Output All Steps*/
	MDP_AUT,/*Auto Setting check*/
	MDP_TOG,/*Choice toggle button */ 
	MDP_LOP,/*Max Attempts*/
	MDP_MIN,/*Min Step*/
	MDP_WRN,/*Output Warning*/
	MDP_ONC,/*Next Move Only*/
	MDP_VDO,/*Video button*/

	ITR_NUM,
	ITR_ANG,
	ITR_ONL,
	ITR_VDO,/*Video button*/

	THK_PS,
	THK_DS,
	THK_CK1,
	THK_NCK,
	THK_CK2,
	THK_CK3,
	THK_CK4,
	THK_CK5,
	THK_VDO,/*Video button*/

	TOL_CHO,
	TOL_POS,
	TOL_AUT,
	TOL_OMT,
	TOL_LIN,
	TOL_ANG,

	GOG_PS,
	GOG_DS,
	GOG_CS,
	GOG_VDO /*Video button*/
};

void nclu_tool_calc_command();

static int Sfrm1=0;
static UU_LOGICAL Sactive1=UU_FALSE;
static NCLX_mot_maxdp Smaxdp;
static UU_REAL Siter[3];
static NCLX_mot_thick Sthick;
static NCLX_mot_toler Stoler;
static int Sitoler[2];
static int Sgougck[3];

static int Smtog[4] = {0,0,0,0};
static int Sttog[2],Shtog[2],Sotog[2],Sgtog[3];
static char Smvar[3][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Sav_Smvar[3][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Stvar[2][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Shvar[7][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Sovar[4][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static int Stogchc = 0;
/*
		(int *)Smvar[0], //max step
		&Smtog[1]//output all step
		&Smtog[2]//enable auto setting
		&togchc, //toggle choice
		(int *)Smvar[1],//min step
		(int *)Smvar[2],//max att
		&Smtog[3],//output warning 
		&Smtog[0],//next move only
		UU_NULL,
		(int *)Stvar[0],(int *)Stvar[1],&Sttog[0],UU_NULL,
		(int *)Shvar[0],(int *)Shvar[1],(int *)Shvar[2],&Shtog[0],(int *)Shvar[3],
			(int *)Shvar[4],(int *)Shvar[5],(int *)Shvar[6],UU_NULL,
		(int *)Sovar[0],(int *)Sovar[1],&Sotog[0],&Sotog[1],(int *)Sovar[2],
			(int *)Sovar[3],
		&Sgtog[0],&Sgtog[1],&Sgtog[2],UU_NULL
*/	
static int Sblack[3]={0,0,0}, Sgreen[3]={0,180,0};
static int Sred[3]={180,0,0};
enum {Maxdp, Iterations, Thicks, Tolerances, Gouge};
static char *Smode[]={"Maxdp", "Iterations", "Thicks", "Tolerances",
	"Gouge Check"};

static UD_FSTAT OnClose();
static void S_define_maxdp();
static void S_define_iter();
static void S_define_thick();
static void S_define_toler();
static void S_define_gougck();
static UD_FSTAT S_toggle_maxdp(), S_Edit_field();
static UD_FSTAT S_toggle_thick();
static UD_FSTAT S_toggle_toler();

#define NEQTOL(r,s) (fabs(r-s)>=.0001)

/*********************************************************************
**    E_FUNCTION     : nclu_tool_calc()
**       Controlling routine to call the MOTION CALC form routine from
**       the menu.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_tool_calc()
{
	int inum,status,gougck[3],itoler[2];
	UM_int2 idx,i2;
	UU_REAL iter[3];
	char caxis[16][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	NCL_cmdbuf cmdbuf;
	NCLX_mot_maxdp maxdp;
	NCLX_mot_thick thick;
	NCLX_mot_toler toler;
/*
.....Get active Motion Calculation parameters
*/
	NclxMotGetMaxdp(&maxdp);
	NclxMotGetNumpts(&inum);
	iter[0] = inum;
	NclxMotGetMaxang(&iter[1]);
	NclxMotGetThick(&thick);
	NclxMotGetToler(&toler);
	toler.start_cos = acos(toler.start_cos) * UM_RADIAN;
	idx = 2; getlfl(&idx,&i2); itoler[0] = i2;
	idx = 363; getifl(&idx,&i2); itoler[1] = i2;
	NclxMotGetGougck1(&gougck[0],&gougck[1],&gougck[2]);
/*
.....Display the form
*/
	status = nclu_tool_calc_form(&maxdp,iter,&thick,&toler,itoler,gougck,caxis,
		UU_TRUE,UU_FALSE);
/*
.....Output the motion calculation commands
*/
	if (status == UU_SUCCESS)
	{
		nclu_tool_calc_command(&cmdbuf,&maxdp,iter,&thick,&toler,itoler,gougck,
			caxis,UU_TRUE,UU_TRUE);
		ncl_set_cmdmode(UU_TRUE);
		ncl_call(&cmdbuf);
	}
}

/*********************************************************************
**    E_FUNCTION     : nclu_tool_calc_form(maxdp,iter,thick,toler,itoler,
**                                         gougck,caxis,modal,reentry)
**       Processes the MOTION CALC form.
**    PARAMETERS
**       INPUT  :
**          maxdp    = Active MAXDP parameters.
**          iter     = Active Iteration parameters.
**          thick    = Active THICK parameters.
**          toler    = Active Tolerance parameters.
**          itoler   = Active Tolerance flags.
**          gougck   = Active GOUGCK parameters.
**          modal    = UU_TRUE = Form is modal (standard),
**                     UU_FALSE = Form is display type subform.
**          reentry  = UU_TRUE = Form is being reentered for the 2nd
**                     time.  Only used for nonmodal form type.
**       OUTPUT :
**          maxdp    = Updated with form MAXDP settings.
**          caxis    = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_tool_calc_form(maxdp,iter,thick,toler,itoler,gougck,caxis,modal,
	reentry)
NCLX_mot_maxdp *maxdp;
UU_REAL iter[];
NCLX_mot_thick *thick;
NCLX_mot_toler *toler;
int itoler[],gougck[];
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
UU_LOGICAL modal,reentry;
{
	int status,nc,irtn,i;

	static char traverse[] =
	{
		1,1,1,1,1,1,1,1,1, 			/* MAXDP */
		1,1,1,1,						/* ITERATIONS */
		1,1,1,1,1,1,1,1,1,			/* THICK */
		1,1,1,1,1,1,				/* TOLERANCE */
		1,1,1,1							/* GOUGCK */
	};
	static char display[] =
	{
		1,1,1,1,1,1,1,1,1, 			/* MAXDP */
		1,1,1,1,						/* ITERATIONS */
		1,1,1,1,1,1,1,1,1,			/* THICK */
		1,1,1,1,1,1,				/* TOLERANCE */
		1,1,1,1							/* GOUGCK */
	};
	static UD_METHOD methods[] =
	{
		S_Edit_field, UU_NULL,S_toggle_maxdp,S_toggle_maxdp,S_Edit_field,S_Edit_field,S_toggle_maxdp,S_toggle_maxdp,UU_NULL,
		S_Edit_field,S_Edit_field,UU_NULL,UU_NULL,
		S_Edit_field,S_Edit_field,S_Edit_field,S_toggle_thick,S_Edit_field,S_Edit_field,S_Edit_field,S_Edit_field,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL, UU_NULL, OnClose
	};

	static char called[] =
	{
		6,6,6,6,6,6,6,6,6,
		6,6,6,6,
		6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,
		6,6,6,6
	};
	static int *ans[] =
	{
		(int *)Smvar[0] /*max step*/,
		&Smtog[1]/*output all step*/,
		&Smtog[2]/*enable auto setting*/,
		&Stogchc, /*toggle choice*/
		(int *)Smvar[2]/*max att*/,
		(int *)Smvar[1]/*min step*/,
		&Smtog[3]/*output warning*/, 
		&Smtog[0]/*next move only*/,
		UU_NULL,
		(int *)Stvar[0],(int *)Stvar[1],&Sttog[0],UU_NULL,
		(int *)Shvar[0],(int *)Shvar[1],(int *)Shvar[2],&Shtog[0],(int *)Shvar[3],
			(int *)Shvar[4],(int *)Shvar[5],(int *)Shvar[6],UU_NULL,
		(int *)Sovar[0],(int *)Sovar[1],&Sotog[0],&Sotog[1],(int *)Sovar[2],
			(int *)Sovar[3],
		&Sgtog[0],&Sgtog[1],&Sgtog[2],UU_NULL
	};
/*
.....Initialize routine
*/
	Smaxdp = *maxdp;
	for (i=0;i<3;i++) Siter[i] = iter[i];
	Sthick = *thick;
	Stoler = *toler;
	Sitoler[0] = itoler[0]; Sitoler[1] = itoler[1];
	for (i=0;i<3;i++) Sgougck[i] = gougck[i];
/*
.....Set up form fields
*/
	S_define_maxdp(maxdp,Smtog,Smvar,traverse,reentry);
	S_define_iter(iter,Sttog,Stvar,traverse,reentry);
	S_define_thick(thick,Shtog,Shvar,display,reentry);
	S_define_toler(toler,itoler,Sotog,Sovar,traverse,reentry);
	S_define_gougck(gougck,Sgtog,traverse,reentry);
/*
.....Get the Form input
*/
again:;
	if (modal)
	{
		Sfrm1 = 0;
		status = ud_form1("motcalc.frm",ans,ans,methods,called,display,traverse);
		if (status == -1) return(status);
	}
	else if (!Sactive1)
	{
		Sfrm1 = ud_form_display1("motcalc.frm",ans,ans,methods,called,display,
			traverse);
		if (Sfrm1 != 0) Sactive1 = UU_TRUE;
		goto done;
	}
/*
.....Store the active motion settings
*/
/*
.....parameter pass wrong	
	irtn = nclu_tool_calc_set(&maxdp,iter,&thick,&toler,itoler,gougck,caxis);
*/
	irtn = nclu_tool_calc_set(maxdp,iter,&thick,&toler,itoler,gougck,caxis);
/*
.....End of routine
*/
done:;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_tool_calc_set(maxdp,iter,thick,toler,itoler,
**                         gougck,caxis)
**       Defines the Motion assist parameters from the form values.
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          maxdp    = Active MAXDP parameters.
**          iter     = Active Iteration parameters.
**          thick    = Active Thick values.
**          toler    = Active Tolerance values.
**          itoler   = Active Tolerance settings.
**          gougck   = Active GOUGCK parameters.
**          caxis    = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_tool_calc_set(maxdp,iter,thick,toler,itoler,gougck,caxis)
NCLX_mot_maxdp *maxdp;
NCLX_mot_toler *toler;
NCLX_mot_thick *thick;
UU_REAL iter[];
int itoler[],gougck[];
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
{
	int irtn;
/*
.....Store the active motion settings
*/
	irtn = S_set_maxdp(maxdp,caxis,Smtog,Smvar);
	irtn = S_set_iter(iter,caxis,Sttog,Stvar);
	irtn = S_set_thick(caxis,Shtog,Shvar);
	irtn = S_set_toler(itoler,caxis,Sotog,Sovar);
	irtn = S_set_gougck(gougck,Sgtog);
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : nclu_tool_calc_command(cmdbuf,maxdp,iter,thick,
**                         toler,itoler,gougck,caxis,kfl,flag)
**       Builds and outputs the tool axis command.
**    PARAMETERS
**       INPUT  :
**          maxdp    = Active MAXDP parameters.
**          iter     = Active Iteration parameters.
**          thick    = Active Thick values.
**          toler    = Active Tolerance values.
**          itoler   = Active Tolerance settings.
**          gougck   = Active GOUGCK parameters.
**          kfl      = UU_TRUE = Command is generated from the interface.
**                     Check if changed from previous settings and use
**                     'caxis' variables in command output.
**          flag     = UU_TRUE = output command to source file.
**                     UU_FALSE = Preview command only.
**       OUTPUT :
**          cmdbuf   = Command buffer to containt the Motion Calc command(s).
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_tool_calc_command(cmdbuf,maxdp,iter,thick,toler,itoler,gougck,
	caxis,kfl,flag)
NCL_cmdbuf *cmdbuf;
NCLX_mot_maxdp *maxdp;
NCLX_mot_toler *toler;
NCLX_mot_thick *thick;
UU_REAL iter[];
int itoler[],gougck[];
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
UU_LOGICAL kfl,flag;
{
	UU_LOGICAL once,changed;
	int nc,inum,i,j,ist,inc,status;
	UU_REAL rval[3];
	char str[80];
	char *tptr[16],tbuf[16][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
/*
.....Initialize the command buffer
*/
	ncl_init_cmdbuf(cmdbuf);
	if (kfl)
	{
		for (i=0;i<16;i++) tptr[i] = caxis[i];
	}
	else
	{
		for (i=0;i<16;i++) tptr[i] = tbuf[i];
		ncl_sprintf(tbuf[0],&maxdp->max,1);
		ncl_sprintf(tbuf[1],&maxdp->min,1);
		sprintf(tbuf[2],"%d",maxdp->attempts);
		i = iter[0];
		sprintf(tbuf[3],"%d",i);
		ncl_sprintf(tbuf[4],&iter[1],1);
		ncl_sprintf(tbuf[5],&thick->ps,1);
		ncl_sprintf(tbuf[6],&thick->ds,1);
		ncl_sprintf(tbuf[7],&thick->cs,1);
		ncl_sprintf(tbuf[8],&thick->cs2,1);
		ncl_sprintf(tbuf[9],&thick->cs3,1);
		ncl_sprintf(tbuf[10],&thick->cs4,1);
		ncl_sprintf(tbuf[11],&thick->cs5,1);
		ncl_sprintf(tbuf[12],&toler->chordal,1);
		ncl_sprintf(tbuf[13],&toler->dsps,1);
		ncl_sprintf(tbuf[14],&toler->start_pt,1);
		rval[0] = acos(toler->start_cos) * UM_RADIAN;
		ncl_sprintf(tbuf[15],&rval[0],1);
	}
/*
.....MAXDP
*/
	changed = UU_TRUE;
	if (kfl) changed = S_maxdp_changed(maxdp,caxis);
	if (changed)
	{
		if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_maxdp,NCL_nocomma);
		ncl_add_token(cmdbuf,tptr[0],NCL_comma);
		once = UU_FALSE;
		if (maxdp->step >= 100)
		{
			once = UU_TRUE;
			maxdp->step -= 100;
		}
/*
......see if this field changed
*/
		if (maxdp->step != Smaxdp.step)
		{
			if (maxdp->step)
			{
				ncl_add_token(cmdbuf,NCL_run_step,NCL_comma);
				ncl_add_token(cmdbuf,NCL_on,NCL_comma);
			}
			else
			{
				ncl_add_token(cmdbuf,NCL_run_step,NCL_comma);
				ncl_add_token(cmdbuf,NCL_off,NCL_comma);
			}
		}
/*
......see if this field changed
*/
		if (maxdp->mauto != Smaxdp.mauto)
		{
			if (maxdp->mauto)
			{
/*
.......from uncheck to checked, always output the AUTO and value
.......Need check with Ken again
*/
				ncl_add_token(cmdbuf,NCL_auto,NCL_comma);
				if (Stogchc)
				{
//always output max value
//					if ((maxdp->attempts!=atof(Smvar[2]))||(maxdp->min!=atof(Smvar[1])))
					{
						ncl_add_token(cmdbuf,tptr[2],NCL_comma);
						if (maxdp->min!=atof(Smvar[1]))
							ncl_add_token(cmdbuf,tptr[1],NCL_comma);
					}
					if (maxdp->warn) 
						ncl_add_token(cmdbuf,NCL_warn,NCL_comma);
					else 
						ncl_add_token(cmdbuf,NCL_nowarn,NCL_comma);
				}
			}
			else
//From checked to unchecked, output "OFF"
				ncl_add_token(cmdbuf,NCL_off,NCL_comma);
		}
		else
		{
			if (maxdp->mauto)
			{
//Last time is checked and this time is checked also
//need ask Ken? Yurong
//if value changed, need output, no change, no any AUTO and value output?
//For now, always output
				ncl_add_token(cmdbuf,NCL_auto,NCL_comma);
				if (Stogchc)
				{
//always output max value
//					if ((maxdp->attempts!=atof(Smvar[2]))||(maxdp->min!=atof(Smvar[1])))
					{
						ncl_add_token(cmdbuf,tptr[2],NCL_comma);
						if (maxdp->min!=atof(Smvar[1]))
							ncl_add_token(cmdbuf,tptr[1],NCL_comma);
					}
					if (maxdp->warn) 
						ncl_add_token(cmdbuf,NCL_warn,NCL_comma);
					else 
						ncl_add_token(cmdbuf,NCL_nowarn,NCL_comma);
				}
			}
//off already, do we need output again
//			else
//				ncl_add_token(cmdbuf,NCL_off,NCL_comma);
		}
		if (once) ncl_add_token(cmdbuf,NCL_once,NCL_comma);
/*
........Output NCL command
*/
		ncl_add_cmdbuf(cmdbuf);
	}
/*
.....NUMPTS
*/
	changed = UU_TRUE;
	if (kfl) changed = S_numpts_changed(caxis[3]);
	if (changed)
	{
		if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_numpts,NCL_nocomma);
		ncl_add_token(cmdbuf,tptr[3],NCL_comma);
		if (iter[2] == 1) ncl_add_token(cmdbuf,NCL_once,NCL_comma);
/*
........Output NCL command
*/
		ncl_add_cmdbuf(cmdbuf);
	}
/*
.....MAXANG
*/
	changed = UU_TRUE;
	if (kfl) changed = S_maxang_changed(caxis[4]);
	if (changed)
	{
		if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_maxang,NCL_nocomma);
		ncl_add_token(cmdbuf,tptr[4],NCL_comma);
		if (iter[2] == 1) ncl_add_token(cmdbuf,NCL_once,NCL_comma);
/*
........Output NCL command
*/
		ncl_add_cmdbuf(cmdbuf);
	}
/*
.....THICK
*/
	changed = UU_TRUE;
	if (kfl) changed = S_thick_changed(caxis[5]);
	if (changed)
	{
		if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_thick,NCL_nocomma);
		ncl_add_token(cmdbuf,tptr[5],NCL_comma);
		inc = 0;
		if (ul_to_reals(&rval[0],&inum,1,tptr[5]) != UU_SUCCESS)
			rval[0] = -9999.9999;
		for (i=6;i<=11;i++)
		{
			if (tptr[i][0] != '\0')
			{
				status = ul_to_reals(&rval[1],&inum,1,tptr[i]);
				if (status == UU_SUCCESS && rval[1] != rval[0]) inc = i;
				rval[0] = rval[1];
			}
		}

		if (strcmp(tptr[5],tptr[6]) != 0 || strcmp(tptr[6],tptr[7]) != 0 ||
			inc != 0)
		{
			ncl_add_token(cmdbuf,tptr[6],NCL_comma);
//			if (strcmp(tptr[6],tptr[7]) != 0 || inc != 0)
			if (strcmp(tptr[6],tptr[7]) != 0 || inc != 6)
				ncl_add_token(cmdbuf,tptr[7],NCL_comma);
			if (inc != 0)
			{
				for (i=8;i<=inc;i++)
				{
					if (tptr[i][0] == '\0') strcpy(tptr[i],tptr[7]);
					ncl_add_token(cmdbuf,tptr[i],NCL_comma);
				}
			}
		}
/*
........Output NCL command
*/
		ncl_add_cmdbuf(cmdbuf);
	}
/*
.....TOLER
*/
	changed = UU_TRUE;
	if (kfl) changed = S_toler_changed(caxis[12]);
	if (changed)
	{
		if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_toler,NCL_nocomma);
		ncl_add_token(cmdbuf,tptr[12],NCL_comma);
		if (strcmp(tptr[12],tptr[13]) != 0.)
			ncl_add_token(cmdbuf,tptr[13],NCL_comma);
/*
........Output NCL command
*/
		ncl_add_cmdbuf(cmdbuf);
	}
/*
.....*SET/AUTOST
*/
	changed = UU_TRUE;
	if (kfl) changed = S_autost_changed(itoler,caxis[14]);
	if (changed)
	{
		if (flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
		if (itoler[0])
			ncl_add_token(cmdbuf,NCL_set,NCL_nocomma);
		else
			ncl_add_token(cmdbuf,NCL_reset,NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_autost,NCL_comma);
		ncl_add_token(cmdbuf,tptr[14],NCL_comma);
		ncl_add_token(cmdbuf,tptr[15],NCL_comma);
		if (itoler[1]) ncl_add_token(cmdbuf,NCL_omit,NCL_comma);
		else ncl_add_token(cmdbuf,NCL_retain,NCL_comma);
/*
........Output NCL command
*/
		ncl_add_cmdbuf(cmdbuf);
	}
/*
.....GOUGCK
*/
	changed = UU_TRUE;
	if (kfl) changed = S_gougck_changed(gougck);
	if (changed)
	{
		if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_gougck,NCL_nocomma);
		if (gougck[1] == gougck[0] && gougck[2] == gougck[0])
		{
			if (gougck[0] == 0)
				ncl_add_token(cmdbuf,NCL_off,NCL_comma);
			else
			{
				ncl_add_token(cmdbuf,NCL_on,NCL_comma);
				sprintf(str,"%d",gougck[0]);
				ncl_add_token(cmdbuf,str,NCL_comma);
			}
		}
		else
		{
			ncl_add_token(cmdbuf,NCL_ps,NCL_comma);
			sprintf(str,"%d",gougck[0]);
			ncl_add_token(cmdbuf,str,NCL_comma);
			ncl_add_token(cmdbuf,NCL_ds,NCL_comma);
			sprintf(str,"%d",gougck[1]);
			ncl_add_token(cmdbuf,str,NCL_comma);
			ncl_add_token(cmdbuf,NCL_cs,NCL_comma);
			sprintf(str,"%d",gougck[2]);
			ncl_add_token(cmdbuf,str,NCL_comma);
		}
/*
........Output NCL command
*/
		ncl_add_cmdbuf(cmdbuf);
	}
}

/*********************************************************************
**    I_FUNCTION     : S_define_maxdp(maxdp,mtog,mvar,traverse,reentry)
**       Defines the field parameters for the Maxdp fields based on the
**       active Maxdp settings.
**    PARAMETERS
**       INPUT  :
**          maxdp    = Current Maxdp settings.
**       OUTPUT :
**          mtog[0]  = MAXDP/ONCE setting.
**          mvar[1]  = Output all steps.
**          mtog[2]  = MAXDP/AUTO setting.
**          mtog[3]  = Output warnings.
**          mvar[0]  = Maxdp step.
**          mvar[1]  = Mindp step.
**          mvar[2]  = Max loops.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_maxdp(maxdp,mtog,mvar,traverse,reentry)
NCLX_mot_maxdp *maxdp;
int mtog[];
char mvar[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
char *traverse;
UU_LOGICAL reentry;
{
	UU_REAL min;
	UM_real8 tol;
/*
.....Define MAXDP settings
*/
	if ((!reentry)||(Smtog[0]))
	{
		mtog[0] = UU_FALSE;
		mtog[1] = maxdp->step;
		mtog[2] = maxdp->mauto;
		mtog[3] = maxdp->warn;
		Stogchc = 0;
		ncl_sprintf(mvar[0],&maxdp->max,1);
		ncl_sprintf(mvar[1],&maxdp->min,1);
		sprintf(mvar[2],"%d",maxdp->attempts);
	}
/*
......save value for max attempt and min step for value
......we use default as initial choice
*/
	strcpy(Sav_Smvar[1], Smvar[1]);
	strcpy(Sav_Smvar[2], Smvar[2]);
	mtog[0] = UU_FALSE;
/*
.....Set MAXDP/AUTO fields
*/
	if (maxdp->mauto)
	{
		if (Smtog[3]||Smtog[0])
		{
			traverse[MDP_TOG] = 0;
			Stogchc = 1;
		}
		else
		{
			traverse[MDP_TOG] = 1;
			Stogchc = 0;
		}
		if (Stogchc)
		{
			traverse[MDP_MIN] = traverse[MDP_LOP] = 1;
		}
		else
		{
			traverse[MDP_MIN] = traverse[MDP_LOP] = 0;
			mtog[3] = 0;
			gettol (&tol);
			min = 10 * tol;
			ncl_sprintf(mvar[1],&min,1);
			strcpy(mvar[2],"10");
		}
	}
	else
	{
		Stogchc = 0;
		traverse[MDP_TOG] = traverse[MDP_MIN] = traverse[MDP_LOP] = traverse[MDP_WRN] = 0;
		gettol (&tol);
		min = 10 * tol;
		ncl_sprintf(mvar[1],&min,1);
		strcpy(mvar[2],"10");
	}
}

/*********************************************************************
**    I_FUNCTION     : S_set_maxdp(maxdp,mtog,mvar)
**       Defines the field parameters for the Maxdp fields based on the
**       active Maxdp mode.
**    PARAMETERS
**       INPUT  :
**          mtog[0]  = MAXDP/ONCE setting.
**          mtog[1]  = Output all steps.
**          mtog[2]  = MAXDP/AUTO setting.
**          mtog[3]  = Output warnings.
**          mvar[0]  = Maxdp step.
**          mvar[1]  = Mindp step.
**          mvar[2]  = Max loops.
**       OUTPUT :
**          maxdp    = Current tool axis settings.
**          caxis    = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_maxdp(maxdp,caxis,mtog,mvar)
NCLX_mot_maxdp *maxdp;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int mtog[];
char mvar[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
{
	maxdp->step = mtog[1];
	if (mtog[0]) maxdp->step += 100;
	maxdp->mauto = mtog[2];
	maxdp->warn = mtog[3];
	strcpy(caxis[0],mvar[0]);
	strcpy(caxis[1],mvar[1]);
	strcpy(caxis[2],mvar[2]);
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_maxdp_changed(maxdp,caxis)
**       Determines if the MAXDP settings have changed.
**    PARAMETERS
**       INPUT  :
**          maxdp    = Current tool axis settings.
**          caxis    = Textual form responses.
**       OUTPUT :
**    RETURNS      : UU_TRUE if the settings have changed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_maxdp_changed(maxdp,caxis)
NCLX_mot_maxdp *maxdp;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
{
	int inum;
	UU_LOGICAL chg;
	UU_REAL rval;
/*
.....Determine if MAXDP settings have changed
*/
	chg = UU_FALSE;
	if (maxdp->step != Smaxdp.step || maxdp->mauto != Smaxdp.mauto ||
		maxdp->warn != Smaxdp.warn) chg = UU_TRUE;
	else
	{
		if (ul_to_reals(&rval,&inum,1,caxis[0]) == UU_SUCCESS)
			if (NEQTOL(rval,Smaxdp.max)) chg = UU_TRUE;
		if (maxdp->mauto)
		{
			if (ul_to_reals(&rval,&inum,1,caxis[1]) == UU_SUCCESS)
				if (NEQTOL(rval,Smaxdp.min)) chg = UU_TRUE;
			if (ul_to_number(caxis[2],&inum) == UU_SUCCESS)
				if (inum != Smaxdp.attempts) chg = UU_TRUE;
		}
	}
	return(chg);
}

/*********************************************************************
**    I_FUNCTION     : S_define_iter(iter,ttog,tvar,traverse,reentry)
**       Defines the tool interation fields.
**    PARAMETERS
**       INPUT  :
**          taxis   = Current tool axis settings.
**       OUTPUT :
**          ttog[0] = Once setting.
**          tvar[0] = NUMPTS value.
**          tvar[1] = MAXANG value.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_iter(iter,ttog,tvar,traverse,reentry)
UU_REAL iter[];
int ttog[];
char tvar[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
char *traverse;
UU_LOGICAL reentry;
{
	int inum;
/*
.....Define Iteration settings
*/
	if (!reentry)
	{
		ttog[0] = UU_FALSE;
		inum = iter[0];
		sprintf(tvar[0],"%d",inum);
		ncl_sprintf(tvar[1],&iter[1],1);
	}
}

/*********************************************************************
**    I_FUNCTION     : S_set_iter(iter,caxis,ttog,tvar)
**       Sets the tool interation variables based on the field parameters.
**    PARAMETERS
**       INPUT  :
**          ttog[0] = Once setting.
**          tvar[0] = NUMPTS value.
**          tvar[1] = MAXANG value.
**       OUTPUT :
**          iter    = Current iteration settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_iter(iter,caxis,ttog,tvar)
UU_REAL iter[];
int ttog[];
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
char tvar[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
{
	iter[2] = ttog[0];
	strcpy(caxis[3],tvar[0]);
	strcpy(caxis[4],tvar[1]);
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_numpts_changed(caxis)
**       Determines if the NUMPTS settings have changed.
**    PARAMETERS
**       INPUT  :
**          caxis    = Textual form responses.
**       OUTPUT :
**    RETURNS      : UU_TRUE if the settings have changed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_numpts_changed(caxis)
char caxis[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
{
	int inum;
	UU_LOGICAL chg;
/*
.....Determine if NUMPTS settings have changed
*/
	chg = UU_FALSE;
	if (ul_to_number(caxis,&inum) == UU_SUCCESS)
		if (inum != Siter[0]) chg = UU_TRUE;
	return(chg);
}

/*********************************************************************
**    I_FUNCTION     : S_maxang_changed(caxis)
**       Determines if the MAXANG settings have changed.
**    PARAMETERS
**       INPUT  :
**          caxis    = Textual form responses.
**       OUTPUT :
**    RETURNS      : UU_TRUE if the settings have changed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_maxang_changed(caxis)
char caxis[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
{
	int inum;
	UU_LOGICAL chg;
	UU_REAL rval;
/*
.....Determine if MAXANG settings have changed
*/
	chg = UU_FALSE;
	if (ul_to_reals(&rval,&inum,1,caxis) == UU_SUCCESS)
		if (NEQTOL(rval,Siter[1])) chg = UU_TRUE;
	return(chg);
}

/*********************************************************************
**    I_FUNCTION     : S_define_thick(thick,htog,hvar,display,reentry)
**       Defines the THICK fields based on the Thick parameters.
**    PARAMETERS
**       INPUT  :
**          thick   = Current THICK settings.
**       OUTPUT :
**          htog[0]   = Multiple check surface thicks.
**          hvar[0]   = Part surface thick.
**          hvar[1]   = Drive surface thick.
**          hvar[2:6] = Check surface thicks.
**          display   = Field display settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_thick(thick,htog,hvar,display,reentry)
NCLX_mot_thick *thick;
int htog[];
char hvar[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
char *display;
UU_LOGICAL reentry;
{
	int i;
/*
.....Define Thick settings
*/
	if (!reentry)
	{
		htog[0] = 0;
		ncl_sprintf(hvar[0],&thick->ps,1);
		ncl_sprintf(hvar[1],&thick->ds,1);
		ncl_sprintf(hvar[2],&thick->cs,1);
/*
.....default same as CS
*/
//		hvar[3][0] = hvar[4][0] = hvar[5][0] = hvar[6][0] = '\0';
		strcpy(hvar[3], hvar[2]);
		strcpy(hvar[4], hvar[2]);
		strcpy(hvar[5], hvar[2]);
		strcpy(hvar[6], hvar[2]);
		if (thick->cs2 != thick->cs)
		{
			ncl_sprintf(hvar[3],&thick->cs2,1);
			htog[0] = 1;
		}
		if (thick->cs3 != thick->cs)
		{
			ncl_sprintf(hvar[4],&thick->cs3,1);
			htog[0] = 1;
		}
		if (thick->cs4 != thick->cs)
		{
			ncl_sprintf(hvar[5],&thick->cs4,1);
			htog[0] = 1;
		}
		if (thick->cs5 != thick->cs)
		{
			ncl_sprintf(hvar[6],&thick->cs5,1);
			htog[0] = 1;
		}
	}
/*
.....Set display flags
*/
	for (i=THK_CK2;i<=THK_CK5;i++) display[i] = htog[0];
}

/*********************************************************************
**    I_FUNCTION     : S_set_thick(caxis,ttog,tvar)
**       Sets the THICK variables based on the field parameters.
**    PARAMETERS
**       INPUT  :
**          htog[0]   = Multiple check surface thicks.
**          hvar[0]   = Part surface thick.
**          hvar[1]   = Drive surface thick.
**          hvar[2:6] = Check surface thicks.
**       OUTPUT :
**          caxis    = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_thick(caxis,htog,hvar)
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int htog[];
char hvar[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
{
	int i,inum;
	UU_REAL rval;
	for (i=0;i<3;i++)
	{
		if (ul_to_reals(&rval,&inum,1,hvar[i]) == UU_SUCCESS)
			ncl_sprintf(hvar[i],&rval,1);
		strcpy(caxis[i+5],hvar[i]);
	}
	if (htog[0])
	{
		for (i=3;i<7;i++)
		{
			if (hvar[i][0] == '\0') strcpy(hvar[i],hvar[2]);
			else
			{
				if (ul_to_reals(&rval,&inum,1,hvar[i]) == UU_SUCCESS)
					ncl_sprintf(hvar[i],&rval,1);
				strcpy(caxis[i+5],hvar[i]);
			}
		}
	}
	else
		for (i=3;i<7;i++) caxis[i+5][0] = '\0';
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_thick_changed(caxis)
**       Determines if the THICK settings have changed.
**    PARAMETERS
**       INPUT  :
**          caxis    = Textual form responses.
**       OUTPUT :
**    RETURNS      : UU_TRUE if the settings have changed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_thick_changed(caxis)
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
{
	int inum;
	UU_LOGICAL chg;
	UU_REAL rval;
/*
.....Determine if THICK settings have changed
*/
	chg = UU_FALSE;
	if (ul_to_reals(&rval,&inum,1,caxis[0]) == UU_SUCCESS)
		if (NEQTOL(rval,Sthick.ps)) chg = UU_TRUE;
	if (ul_to_reals(&rval,&inum,1,caxis[1]) == UU_SUCCESS)
		if (NEQTOL(rval,Sthick.ds)) chg = UU_TRUE;
	if (ul_to_reals(&rval,&inum,1,caxis[2]) == UU_SUCCESS)
		if (NEQTOL(rval,Sthick.cs)) chg = UU_TRUE;
	if (ul_to_reals(&rval,&inum,1,caxis[3]) == UU_SUCCESS)
		if (NEQTOL(rval,Sthick.cs2)) chg = UU_TRUE;
	if (ul_to_reals(&rval,&inum,1,caxis[4]) == UU_SUCCESS)
		if (NEQTOL(rval,Sthick.cs3)) chg = UU_TRUE;
	if (ul_to_reals(&rval,&inum,1,caxis[5]) == UU_SUCCESS)
		if (NEQTOL(rval,Sthick.cs4)) chg = UU_TRUE;
	if (ul_to_reals(&rval,&inum,1,caxis[6]) == UU_SUCCESS)
		if (NEQTOL(rval,Sthick.cs5)) chg = UU_TRUE;
	return(chg);
}

/*********************************************************************
**    I_FUNCTION     : S_define_toler(toler,ovar,traverse,reentry)
**       Defines the Tolerance fields.
**    PARAMETERS
**       INPUT  :
**          toler   = Current tolerance values.
**          itoler  = Current tolerance settings.
**       OUTPUT :
**          ovar[0] = Chordal tolerance.
**          ovar[1] = Positional Tolerance.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_toler(toler,itoler,otog,ovar,traverse,reentry)
NCLX_mot_toler toler[];
int itoler[],otog[];
char ovar[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
char *traverse;
UU_LOGICAL reentry;
{
	int inum;
/*
.....Define Tolerance settings
*/
	if (!reentry)
	{
		otog[0] = itoler[0];
		otog[1] = itoler[1];
		ncl_sprintf(ovar[0],&toler->chordal,1);
		ncl_sprintf(ovar[1],&toler->dsps,1);
		ncl_sprintf(ovar[2],&toler->start_pt,1);
		ncl_sprintf(ovar[3],&toler->start_cos,1);
	}
}

/*********************************************************************
**    I_FUNCTION     : S_set_toler(itoler,caxis,otog,ovar)
**       Sets the tool interation variables based on the field parameters.
**    PARAMETERS
**       INPUT  :
**          otog[0] = Set Auto start.
**          otog[1] = Omit first move for autost.
**          ovar[0] = Chordal tolerance.
**          ovar[1] = Positional Tolerance.
**       OUTPUT :
**          itoler   = Tolerance settings.
**          caxis    = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_toler(itoler,caxis,otog,ovar)
int itoler[];
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int otog[];
char ovar[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
{
	itoler[0] = otog[0];
	itoler[1] = otog[1];
	strcpy(caxis[12],ovar[0]);
	strcpy(caxis[13],ovar[1]);
	strcpy(caxis[14],ovar[2]);
	strcpy(caxis[15],ovar[3]);
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_toler_changed(caxis)
**       Determines if the Tolerance settings have changed.
**    PARAMETERS
**       INPUT  :
**          caxis    = Textual form responses.
**       OUTPUT :
**    RETURNS      : UU_TRUE if the settings have changed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_toler_changed(caxis)
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
{
	int inum;
	UU_LOGICAL chg;
	UU_REAL rval;
/*
.....Determine if MAXANG settings have changed
*/
	chg = UU_FALSE;
	if (ul_to_reals(&rval,&inum,1,caxis[0]) == UU_SUCCESS)
		if (NEQTOL(rval,Stoler.chordal)) chg = UU_TRUE;
	if (ul_to_reals(&rval,&inum,1,caxis[1]) == UU_SUCCESS)
		if (NEQTOL(rval,Stoler.dsps)) chg = UU_TRUE;
	return(chg);
}

/*********************************************************************
**    I_FUNCTION     : S_autost_changed(itoler,caxis)
**       Determines if the Tolerance settings have changed.
**    PARAMETERS
**       INPUT  :
**          itoler   = Autost settings.
**          caxis    = Textual form responses.
**       OUTPUT :
**    RETURNS      : UU_TRUE if the settings have changed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_autost_changed(itoler,caxis)
int itoler[];
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
{
	int inum;
	UU_LOGICAL chg;
	UU_REAL rval;
/*
.....Determine if MAXANG settings have changed
*/
	chg = UU_FALSE;
	if (itoler[0] != Sitoler[0]) chg = UU_TRUE;
	if (itoler[1] != Sitoler[1]) chg = UU_TRUE;
	if (ul_to_reals(&rval,&inum,1,caxis[0]) == UU_SUCCESS)
		if (NEQTOL(rval,Stoler.start_pt)) chg = UU_TRUE;
	if (ul_to_reals(&rval,&inum,1,caxis[1]) == UU_SUCCESS)
		if (NEQTOL(rval,Stoler.start_cos)) chg = UU_TRUE;
	return(chg);
}

/*********************************************************************
**    I_FUNCTION     : S_define_gougck(gtog,traverse,reentry)
**       Defines the Gougck fields based on the active parameters.
**    PARAMETERS
**       INPUT  :
**          gougck  = Current GOUGCK settings.
**       OUTPUT :
**          gtog[0] = Part surface gougck.
**          gtog[1] = Part surface gougck.
**          gtog[2] = Part surface gougck.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_gougck(gougck,gtog,traverse,reentry)
int gougck[];
int gtog[];
char *traverse;
{
/*
.....Define Gougck settings
*/
	if (!reentry)
	{
		gtog[0] = gougck[0];
		gtog[1] = gougck[1];
		if (gtog[1] == 1) gtog[1] = 0;
		else if (gtog[1] == 2 || gtog[1] == 3) gtog[1] = 1;
		else if (gtog[1] == 4) gtog[1] = 2;
		gtog[2] = gougck[2];
		if (gtog[2] > 0) gtog[2]--;
	}
}

/*********************************************************************
**    I_FUNCTION     : S_set_gougck(gougck,gtog)
**       Sets the Gougck parameters based on the form fields.
**    PARAMETERS
**       INPUT  :
**          gtog[0] = Part surface gougck.
**          gtog[1] = Part surface gougck.
**          gtog[2] = Part surface gougck.
**       OUTPUT :
**          gougck  = Current GOUGCK settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_gougck(gougck,gtog)
int gougck[];
int gtog[];
{
/*
.....Define Gougck settings
*/
	gougck[0] = gtog[0];
	gougck[1] = gtog[1];
	if (gougck[1] == 1) gougck[1] = 2;
	else if (gougck[1] == 2) gougck[1] = 4;
	gougck[2] = gtog[2];
	if (gougck[2] > 0) gougck[2]++;
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_gougck_changed(gougck)
**       Determines if the GOUGCK settings have changed.
**    PARAMETERS
**       INPUT  :
**          gougck   = Gougck settings.
**       OUTPUT :
**    RETURNS      : UU_TRUE if the settings have changed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_gougck_changed(gougck)
int gougck[];
{
	UU_LOGICAL chg;
/*
.....Determine if GOUGCK settings have changed
*/
	chg = UU_FALSE;
	if (gougck[0] != Sgougck[0] || gougck[1] != Sgougck[1] ||
		gougck[2] != Sgougck[2]) chg = UU_TRUE;
	return(chg);
}

/*********************************************************************
**    I_FUNCTION     : S_enable_buttons()
**			Determines which fields should be enabled and which buttons
**       marked as necessary, but unfulfilled based on the Part, Drive,
**       and Check surface selections.
**    PARAMETERS   
**       INPUT  :
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_TRUE if Check Surface is satisfied.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_enable_buttons()
{
	int nc, stat;
	char label[2*65];
	UU_REAL rval;
	UU_LOGICAL ifl,MXcheck1,MXcheck2,MXcheck3, check11, check12, check21, check22, check23, check24, check25, check26, check27;
/*
......MAX step
*/
	nc = strlen(Smvar[0]);
	ul_strip_blanks(Smvar[0],&nc);
	if (nc>0)
	{
		MXcheck1 = UU_TRUE;
		rval = atof(Smvar[0]);
		if ((rval<=0)||(rval>1000))
			MXcheck1 =  UU_FALSE;
	}
	else
		MXcheck1 = UU_FALSE;
/*
......MAX Attempts:
*/
	nc = strlen(Smvar[2]);
	ul_strip_blanks(Smvar[2],&nc);
	if (nc>0)
	{
		MXcheck2 = UU_TRUE;
		rval = atof(Smvar[2]);
		if ((rval<1)||(rval>100))
			MXcheck2 =  UU_FALSE;
	}
	else
		MXcheck2 = UU_FALSE;
/*
......Min step
*/
	nc = strlen(Smvar[1]);
	ul_strip_blanks(Smvar[1],&nc);
	if (nc>0)
	{
		MXcheck3 = UU_TRUE;
		rval = atof(Smvar[1]);
		if ((rval<=0)||(rval>1000))
			MXcheck3 =  UU_FALSE;
	}
	else
		MXcheck3 = UU_FALSE;
/*
.....Iteration parameter
*/
	nc = strlen(Stvar[0]);
	ul_strip_blanks(Stvar[0],&nc);
	if (nc>0)
	{
		check11 = UU_TRUE;
		rval = atof(Stvar[0]);
		if (rval<100)
			check11 =  UU_FALSE;
	}
	else
		check11 = UU_FALSE;
	nc = strlen(Stvar[1]);
	ul_strip_blanks(Stvar[1],&nc);
	if (nc>0)
	{
		check12 = UU_TRUE;
		rval = atof(Stvar[1]);
		if (rval<=0)
			check12 =  UU_FALSE;
	}
	else
		check12 = UU_FALSE;
/*
.....Thicks parameter
*/
	nc = strlen(Shvar[0]);
	ul_strip_blanks(Shvar[0],&nc);
	if (nc>0)
	{
		check21 = UU_TRUE;
		stat = ncl_get_scalar_value(Shvar[0], &rval);
		if (stat==-1)
			check21 =  UU_FALSE;
	}
	else
		check21 = UU_FALSE;
	nc = strlen(Shvar[1]);
	ul_strip_blanks(Shvar[1],&nc);
	if (nc>0)
	{
		check22 = UU_TRUE;
		stat = ncl_get_scalar_value(Shvar[1], &rval);
		if (stat==-1)
			check22 =  UU_FALSE;
	}
	else
		check22 = UU_FALSE;
	nc = strlen(Shvar[2]);
	ul_strip_blanks(Shvar[2],&nc);
	if (nc>0)
	{
		check23 = UU_TRUE;
		stat = ncl_get_scalar_value(Shvar[2], &rval);
		if (stat==-1)
			check23 =  UU_FALSE;
	}
	else
		check23 = UU_FALSE;
	check24 = UU_TRUE;
	check25 = UU_TRUE;
	check26 = UU_TRUE;
	check27 = UU_TRUE;
	if (Shtog[0])
	{
		nc = strlen(Shvar[3]);
		ul_strip_blanks(Shvar[3],&nc);
		if (nc>0)
		{
			check24 = UU_TRUE;
			stat = ncl_get_scalar_value(Shvar[3], &rval);
			if (stat==-1)
				check24 =  UU_FALSE;
		}
		else
			check24 = UU_FALSE;
		nc = strlen(Shvar[4]);
		ul_strip_blanks(Shvar[4],&nc);
		if (nc>0)
		{
			check25 = UU_TRUE;
			stat = ncl_get_scalar_value(Shvar[4], &rval);
			if (stat==-1)
				check25 =  UU_FALSE;
		}
		else
			check25 = UU_FALSE;
		nc = strlen(Shvar[5]);
		ul_strip_blanks(Shvar[5],&nc);
		if (nc>0)
		{
			check26 = UU_TRUE;
			stat = ncl_get_scalar_value(Shvar[5], &rval);
			if (stat==-1)
				check26 =  UU_FALSE;
		}
		else
			check26 = UU_FALSE;
		nc = strlen(Shvar[6]);
		ul_strip_blanks(Shvar[6],&nc);
		if (nc>0)
		{
			check27 = UU_TRUE;
			stat = ncl_get_scalar_value(Shvar[6], &rval);
			if (stat==-1)
				check27 =  UU_FALSE;
		}
		else
			check27 = UU_FALSE;
	}
/*
.....Define button colors
*/
	if (MXcheck1)
		ud_dispfrm_set_attribs(Sfrm1,MDP_DIS,UM_BLACK,UM_WHITE);
	else 
		ud_dispfrm_set_attribs(Sfrm1,MDP_DIS,UM_WHITE,UM_RED);
	if (MXcheck2)
		ud_dispfrm_set_attribs(Sfrm1,MDP_LOP,UM_BLACK,UM_WHITE);
	else 
		ud_dispfrm_set_attribs(Sfrm1,MDP_LOP,UM_WHITE,UM_RED);
	if (MXcheck3)
		ud_dispfrm_set_attribs(Sfrm1,MDP_MIN,UM_BLACK,UM_WHITE);
	else 
		ud_dispfrm_set_attribs(Sfrm1,MDP_MIN,UM_WHITE,UM_RED);
	if (check11) 
		ud_dispfrm_set_attribs(Sfrm1,ITR_NUM,UM_BLACK,UM_WHITE);
	else 
		ud_dispfrm_set_attribs(Sfrm1,ITR_NUM,UM_WHITE,UM_RED);
	if (check12) 
		ud_dispfrm_set_attribs(Sfrm1,ITR_ANG,UM_BLACK,UM_WHITE);
	else 
		ud_dispfrm_set_attribs(Sfrm1,ITR_ANG,UM_WHITE,UM_RED);

	if (check21) 
		ud_dispfrm_set_attribs(Sfrm1,THK_PS,UM_BLACK,UM_WHITE);
	else 
		ud_dispfrm_set_attribs(Sfrm1,THK_PS,UM_WHITE,UM_RED);
	if (check22) 
		ud_dispfrm_set_attribs(Sfrm1,THK_DS,UM_BLACK,UM_WHITE);
	else 
		ud_dispfrm_set_attribs(Sfrm1,THK_DS,UM_WHITE,UM_RED);
	if (check23) 
		ud_dispfrm_set_attribs(Sfrm1,THK_CK1,UM_BLACK,UM_WHITE);
	else 
		ud_dispfrm_set_attribs(Sfrm1,THK_CK1,UM_WHITE,UM_RED);
	if (check24) 
		ud_dispfrm_set_attribs(Sfrm1,THK_CK2,UM_BLACK,UM_WHITE);
	else 
		ud_dispfrm_set_attribs(Sfrm1,THK_CK2,UM_WHITE,UM_RED);
	if (check25) 
		ud_dispfrm_set_attribs(Sfrm1,THK_CK3,UM_BLACK,UM_WHITE);
	else 
		ud_dispfrm_set_attribs(Sfrm1,THK_CK3,UM_WHITE,UM_RED);
	if (check26) 
		ud_dispfrm_set_attribs(Sfrm1,THK_CK4,UM_BLACK,UM_WHITE);
	else 
		ud_dispfrm_set_attribs(Sfrm1,THK_CK4,UM_WHITE,UM_RED);
	if (check27) 
		ud_dispfrm_set_attribs(Sfrm1,THK_CK5,UM_BLACK,UM_WHITE);
	else 
		ud_dispfrm_set_attribs(Sfrm1,THK_CK5,UM_WHITE,UM_RED);
/*
.....Set section color
*/
	if (MXcheck1 && MXcheck2 && MXcheck3)
	{
		ud_form_section_color(Sfrm1, Smode[Maxdp], Sblack, 0);
	}
	else
	{
		ud_form_section_color(Sfrm1, Smode[Maxdp], Sred, 1);
	}
	if (check11 && check12)
	{
		ud_form_section_color(Sfrm1, Smode[Iterations], Sblack, 0);
	}
	else
	{
		ud_form_section_color(Sfrm1, Smode[Iterations], Sred, 1);
	}
	if (check21 && check22&& check23&& check24&& check25&& check26&& check27)
	{
		ud_form_section_color(Sfrm1, Smode[Thicks], Sblack, 0);
	}
	else
	{
		ud_form_section_color(Sfrm1, Smode[Thicks], Sred, 1);
	}
/*
.....Set Action Buttons
*/
	ifl = check11 && check12 && check21 && check22 && check23 && check24 && check25 && check26 && check27;
	ifl = ifl&&MXcheck1&&MXcheck2&&MXcheck3;
	if (Sfrm1==0)
		ud_frm_enable_ok(ifl);
	else
		ud_frm_enable_close(Sfrm1, ifl);
}


/*********************************************************************
**    I_FUNCTION     : S_Edit_field(fieldno, val, stat)
**       Handles some edit fields need handled.
**    PARAMETERS
**       INPUT  :
**          fieldno = Form field which initiated this call.
**          val     = Current field value.
**          stat    = Not used.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT S_Edit_field(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch(*fieldno)
	{
	case MDP_DIS:
	case MDP_LOP:
	case MDP_MIN:
	case ITR_NUM:
	case ITR_ANG:
	case THK_PS:
	case THK_DS:
	case THK_CK1:
	
	case THK_CK2:
	case THK_CK3:
	case THK_CK4:
	case THK_CK5:
		S_enable_buttons();
		break;
	}
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_maxdp(fieldno, val, stat)
**       Handles the Maxdp toggle fields.
**    PARAMETERS
**       INPUT  :
**          fieldno = Form field which initiated this call.
**          val     = Current field value.
**          stat    = Not used.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT S_toggle_maxdp(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_REAL min;
	UM_real8 tol;

	ud_default_method(fieldno, val, stat);

	switch(*fieldno)
	{
	case MDP_ONC:
	case MDP_WRN: 
	case MDP_AUT: 
		if (Smtog[2])
		{
			ud_setfrm_traverse_mask(Sfrm1,MDP_WRN,Smtog[2]);
			if (Smtog[3]||Smtog[0])
			{
				ud_setfrm_traverse_mask(Sfrm1,MDP_TOG,UU_FALSE);
				Stogchc = 1;
				ud_dispfrm_update_answer(Sfrm1, MDP_TOG,&Stogchc);
			}
			else
			{
				ud_setfrm_traverse_mask(Sfrm1,MDP_TOG,UU_TRUE);
			}
			if (Stogchc)
			{
				ud_setfrm_traverse_mask(Sfrm1,MDP_MIN,Smtog[2]);
				ud_setfrm_traverse_mask(Sfrm1,MDP_LOP,Smtog[2]);
			}
			else
			{
				ud_setfrm_traverse_mask(Sfrm1,MDP_MIN,UU_FALSE);
				ud_setfrm_traverse_mask(Sfrm1,MDP_LOP,UU_FALSE);
				Smtog[3] = 0;
				ud_dispfrm_update_answer(Sfrm1, MDP_WRN,&Smtog[3]);
			}
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm1,MDP_TOG,Smtog[2]);
			ud_setfrm_traverse_mask(Sfrm1,MDP_WRN,Smtog[2]);
			ud_setfrm_traverse_mask(Sfrm1,MDP_MIN,Smtog[2]);
			ud_setfrm_traverse_mask(Sfrm1,MDP_LOP,Smtog[2]);
			Smtog[3] = 0;
			ud_dispfrm_update_answer(Sfrm1, MDP_WRN,&Smtog[3]);		
		}
		break;
	case MDP_TOG:
		if (val->frmint[0])
		{
			ud_setfrm_traverse_mask(Sfrm1,MDP_MIN,val->frmint[0]);
			ud_setfrm_traverse_mask(Sfrm1,MDP_LOP,val->frmint[0]);
			strcpy(Smvar[1], Sav_Smvar[1]);
			strcpy(Smvar[2], Sav_Smvar[2]);
			ud_dispfrm_update_answer(Sfrm1, MDP_MIN, &Smvar[1]);
			ud_dispfrm_update_answer(Sfrm1, MDP_LOP, &Smvar[2]);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm1,MDP_MIN,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,MDP_LOP,UU_FALSE);
			Smtog[3] = 0;
			ud_dispfrm_update_answer(Sfrm1, MDP_WRN,&Smtog[3]);
			strcpy(Sav_Smvar[1], Smvar[1]);
			strcpy(Sav_Smvar[2], Smvar[2]);
			gettol (&tol);
			min = 10 * tol;
			ncl_sprintf(Smvar[1],&min,1);
			strcpy(Smvar[2],"10");
			ud_dispfrm_update_answer(Sfrm1, MDP_MIN, &Smvar[1]);
			ud_dispfrm_update_answer(Sfrm1, MDP_LOP, &Smvar[2]);
		}
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_thick(fieldno, val, stat)
**       Handles the Thicks toggle fields.
**    PARAMETERS
**       INPUT  :
**          fieldno = Form field which initiated this call.
**          val     = Current field value.
**          stat    = Not used.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT S_toggle_thick(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;

	ud_default_method(fieldno, val, stat);

	switch(*fieldno)
	{
	case THK_NCK: 
		for (i=THK_CK2;i<=THK_CK5;i++)
			ud_set_display_mask(UD_INPUTF,i,val->frmint[0]);
		break;
	}
	return(UD_FLDOK);
}

/***********************************************************************
**     I_FUNCTION   :   OnClose()
**        Method called Advanced Settings form is closed.
**     PARAMETERS
**        INPUT  :
**           none
**        OUTPUT :
**           none
**        RETURNS      : none
**        SIDE EFFECTS : none
**        WARININGS    : none
************************************************************************/
static UD_FSTAT OnClose()
{
	Sactive1 = UU_FALSE;
	Sfrm1 = 0;
	return(UD_FLDOK);
}
