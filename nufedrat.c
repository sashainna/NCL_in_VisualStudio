/*********************************************************************
**		NAME:  nufedrat.c
**		CONTAINS:
**			nclu_fedrat
**			nclu_fedrat_form
**			nclu_fedrat_command
**    COPYRIGHT 2015 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nufedrat.c , 25.5
**    DATE AND TIME OF LAST  MODIFICATION
**       07/05/16 , 12:26:08
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
#include "mdcpln.h"
#include "mdunits.h"
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
	FED_MOD,
	FED_MOD2,
	FED_RAT,
	SLW_HGT_CHC,
	SLW_HGT,

	SLW_MOD,
	SLW_DIS,
	SLW_FED,
	SLW_TYP,
	SLW_ONC,

	ACC_MOD,
	ACC_DIS,
	ACC_FED,
	ACC_TYP,
	ACC_ONC,
};

extern UD_METHOD UD_initfrm_intry;
	
static int Sctr_hgt_chc = 0;
static int Sctr_hgt_set = 0;
static UU_REAL Sctr_hgt_val[2] = {0.0,0.0};
static int S_fldno = -1;

void nclu_fedrat_command();

static UU_LOGICAL Smodal = UU_TRUE; 
static int Sftog[2],Sstog[3],Satog[3],Shgt_chc;
static char Sfvar[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Ssvar[3][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Savar[2][NCL_MAX_LABEL_AND_SUBSCRIPT+1];

static int Sfrm1=0;
static UU_LOGICAL Sactive1 = UU_FALSE,Sfixed;
static NCLX_mot_feedrate Sfedrat;
static int Sadvtog;
	
static UD_FSTAT S_init_form();
static void S_define_fedrat();
static UD_FSTAT S_toggle_fedrat();
static UD_FSTAT S_toggle_slow(), S_slow_text();
static UD_FSTAT S_toggle_accel(), S_accel_text();
static UD_FSTAT S_toggle_advanced();
static UD_FSTAT OnClose();
static UD_FSTAT S_fedrat_text();

#define NEQTOL(r,s) (fabs(r-s)>=.0001)

/*********************************************************************
**    E_FUNCTION     : nclu_fedrat()
**       Controlling routine to call the Feed Rate form routine from
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
void nclu_fedrat()
{
	int status;
	UM_int2 idx,lfl, rapid;
	char caxis[16][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	NCLX_mot_feedrate fedrat, save_fedrat;
	NCL_cmdbuf cmdbuf;
/*
.....Get active Feed Rate parameters
*/
	NclxMotGetFeedrate(&fedrat);
/*
....SAME not used in Fedrat mode form, still keep as 3 though
*/
	idx = 54; getlfl(&idx,&lfl);
	if (lfl) fedrat.mode = 3;
/*
.....mode=0 rapid
.....mode=1 IPM
.....mode=2 IPR
*/
	rpchk (&rapid);
/* 
....don't do anything with mode rapid or not
	if (rapid)	fedrat.mode = 0;
*/
/*
.....Display the form
*/
	status = nclu_fedrat_form(&fedrat,caxis,UU_TRUE,UU_FALSE, -1);
/*
.....Output the feed rate command
*/
	if (status == UU_SUCCESS)
	{
		nclu_fedrat_command(&cmdbuf,&fedrat,caxis,UU_TRUE,UU_TRUE);
		ncl_set_cmdmode(UU_TRUE);
		ncl_call(&cmdbuf);
	}
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
**    E_FUNCTION     : nclu_fedrat_form(fedrat,caxis,modal,reentry, fldno)
**       Processes the Feed Rate form.
**    PARAMETERS
**       INPUT  :
**          fedrat   = Active Feed Rate parameters.
**          modal     - UU_TRUE = Form is modal (standard),
**                      UU_FALSE = Form is display type subform.
**          reentry   - UU_TRUE = Form is being reentered for the 2nd
**                      time.  Only used for nonmodal form type.
**			fldno:	- When this form display as nonmodal form type,
**						when form closed, we need update the feedrate field
**					this "fldno' is the main form feedrate field number
**       OUTPUT :
**          fedrat   = Updated with form FEDRAT settings.
**          caxis    = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_fedrat_form(fedrat,caxis,modal,reentry, fldno)
NCLX_mot_feedrate *fedrat;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
UU_LOGICAL modal,reentry;
int fldno;
{
	int status,irtn,fieldno,stat;
	UD_DDATA val;
	UD_METHOD save_entry;

	static char traverse[] =
	{
		1,1,1,1,1,					/* FEDRAT */
		1,1,1,1,1,		/* SLOWDOWN */
		1,1,1,1,1,1			/* ACCELERATE */
	};
	static char display[] =
	{
		1,1,1,1,1,					/* FEDRAT */
		1,1,1,1,1,		/* SLOWDOWN */
		1,1,1,1,1,1			/* ACCELERATE */
	};
	static UD_METHOD methods[] =
	{
		S_toggle_fedrat,S_toggle_fedrat, S_fedrat_text, S_toggle_fedrat, UU_NULL,
		S_toggle_slow,UU_NULL,S_slow_text,UU_NULL,UU_NULL,
		S_toggle_accel,UU_NULL,S_accel_text,UU_NULL,UU_NULL,OnVideo,OnClose,
	};
	static char called[] =
	{
		6,6,6,6,6,
		6,6,6,6,6,
		6,6,6,6,6,6
	};
	static int *ans[] =
	{
		&Sftog[0], &Sftog[1], (int *)Sfvar,(int *)&Shgt_chc,(int *)Ssvar[2],
		&Sstog[0],(int *)Ssvar[0],(int *)Ssvar[1],&Sstog[1],&Sstog[2],
		&Satog[0],(int *)Savar[0],(int *)Savar[1],&Satog[1],&Satog[2],UU_NULL
	};
/*
.....Initialize routine
*/
	Smodal = modal;
/*
......only first time, save original data
*/
//always save and compare
//	if ((modal)||(reentry==0))
		Sfedrat = *fedrat;
/*
.....Set up form fields
*/
	Shgt_chc = Sctr_hgt_chc;
	S_define_fedrat(fedrat,Sftog,Sfvar,Sstog,Ssvar,Satog,Savar,traverse,display,
		reentry);
/*
.....Get the Form input
*/
	if (modal)
	{
		S_fldno = -1;
		display[0] = 1;
		display[1] = 0;
		save_entry = UD_initfrm_intry;
		UD_initfrm_intry = S_init_form;
		status = ud_form1("feedrate.frm",ans,ans,methods,called,display,traverse);
		UD_initfrm_intry = save_entry;
		if (status == -1) return(status);
/*
.....Store the active tool axis parameters
*/
		irtn = nclu_fedrat_set_fedrat(fedrat,caxis);
	}
	else if (!Sactive1)
	{
		S_fldno = fldno;
		display[0] = 0;
		display[1] = 1;
		Sfrm1 = ud_form_display1("feedrate.frm",ans,ans,methods,called,display,
			traverse);
		if (Sfrm1 <= 0) return(UU_FAILURE);
		{
			Sactive1 = UU_TRUE;
			fieldno = stat = 0;
			S_init_form(&fieldno,&val,stat);
		}
/*
.....for display type form, nclu_fedrat_set_fedrat will not 
.....set until it close, using nclu_fedrat_set_fedrat(&fedrat,cfed) later
.....in calling routine
*/
	}
/*
.....End of routine
*/
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_fedrat_set_fedrat(fedrat,caxis)
**       Defines the field parameters for the Feed rate fields based on the
**       active Fedrat settings.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT :
**          fedrat   = Current Feed Rate settings.
**          caxis    = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_fedrat_set_fedrat(fedrat,caxis)
NCLX_mot_feedrate *fedrat;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
{
/*
.....Define FEDRAT settings
*/
	if (Smodal)
		fedrat->mode = Sftog[0];
	else
		fedrat->mode = Sftog[1];
	fedrat->base_feedrate = atof(Sfvar);
	strcpy(caxis[0],Sfvar);
/*
.....Define FEDRAT/AT settings
*/	
//	if (Sadvtog) not used, always 1
//	{
		fedrat->slowdown_flag = Sstog[0];
		fedrat->slowdown_type = Sstog[1];
		if (Sstog[2]) fedrat->slowdown_flag += 100;
		strcpy(caxis[1],Ssvar[0]);
		strcpy(caxis[2],Ssvar[1]);
		strcpy(caxis[3],Ssvar[2]);
		if (Sstog[0])
		{
			fedrat->slowdown_dist = atof(Ssvar[0]);
			fedrat->slowdown_feedrate = atof(Ssvar[1]);
		}
/*
.....Define FEDRAT/OUT settings
*/	
		fedrat->accel_flag = Satog[0];
		fedrat->accel_type = Satog[1];
		if (Satog[2]) fedrat->accel_flag += 100;
		strcpy(caxis[4],Savar[0]);
		strcpy(caxis[5],Savar[1]);
		if (Satog[0])
		{
			fedrat->accel_dist = atof(Savar[0]);
			fedrat->accel_feedrate = atof(Savar[1]);
		}
/*
......not output the LENGTH with AT/OUT anymore
*/
//		fedrat->height = atof(Ssvar[2]);
//	}
/*
.....Advanced settings not used
*/
//	else
//	{
//		fedrat->slowdown_flag = fedrat->accel_flag = 0;
//	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_fedrat_command(cmdbuf,fedrat,caxis,kfl,flag)
**       Builds and outputs the FEDRAT command.
**    PARAMETERS
**       INPUT  :
**          fedrat   = Active Feed Rate parameters.
**          caxis    = Textual form responses.
**          kfl      = UU_TRUE = Command is generated from the interface.
**                     Check if changed from previous settings and use
**                     'caxis' variables in command output.
**          flag     = UU_TRUE = output command to source file.
**                     UU_FALSE = Preview command only.
**       OUTPUT :
**          cmdbuf   = Command buffer to containt the FEDRAT command(s).
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_fedrat_command(cmdbuf,fedrat,caxis,kfl,flag)
NCL_cmdbuf *cmdbuf;
NCLX_mot_feedrate *fedrat;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
UU_LOGICAL kfl,flag;
{
	UU_LOGICAL changed;
	int i, k;
	char tbuf[6][NCL_MAX_LABEL_AND_SUBSCRIPT+1],*tptr[6];
	k = 0;
/*
.....FEDRAT/f
*/
	changed = UU_TRUE;
	if (kfl) 
		changed = S_fedrat_changed(fedrat,caxis);
	if (changed)
	{
/*
.....Initialize command
*/
		if (kfl)
		{
			for (i=0;i<6;i++) tptr[i] = caxis[i];
		}
		else
		{
			for (i=0;i<6;i++) tptr[i] = tbuf[i];
			ncl_sprintf(tbuf[0],&fedrat->base_feedrate,1);
			ncl_sprintf(tbuf[1],&fedrat->slowdown_dist,1);
			ncl_sprintf(tbuf[2],&fedrat->slowdown_feedrate,1);
//			ncl_sprintf(tbuf[3],&fedrat->height,1);
			ncl_sprintf(tbuf[3],&Ssvar[2],1);
			ncl_sprintf(tbuf[4],&fedrat->accel_dist,1);
			ncl_sprintf(tbuf[5],&fedrat->accel_feedrate,1);
		}
		if ((fedrat->mode != Sfedrat.mode)&&(fedrat->mode==0))
		{
/*
.....RAPID
*/
			ncl_init_cmdbuf(cmdbuf);
			if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
			ncl_add_token(cmdbuf,NCL_rapid,NCL_nocomma);					
/*
........Output NCL command
*/
			ncl_add_cmdbuf(cmdbuf);
			return;
		}
		ncl_init_cmdbuf(cmdbuf);
		if (fedrat->mode!=0) 
		{
			if ((fedrat->base_feedrate!=Sfedrat.base_feedrate)
				|| (fedrat->mode != Sfedrat.mode))
			{
				if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
				ncl_add_token(cmdbuf,NCL_fedrat,NCL_nocomma);
				if (fedrat->mode != Sfedrat.mode)
				{
					if (fedrat->mode == 1)
					{
						if (UM_cpln.length_unit ==  UM_MM)
							ncl_add_token(cmdbuf,NCL_mmpm,NCL_comma);
						else
							ncl_add_token(cmdbuf,NCL_ipm,NCL_comma);
					}
					else if (fedrat->mode == 2)
					{
						if (UM_cpln.length_unit ==  UM_MM)
							ncl_add_token(cmdbuf,NCL_mmpr,NCL_comma);
						else
							ncl_add_token(cmdbuf,NCL_ipr,NCL_comma);
					}
				}
				ncl_add_token(cmdbuf,tptr[0],NCL_nocomma);
/*
........Output NCL command
*/
				ncl_add_cmdbuf(cmdbuf);
			}
		}
/*
.....Output FEDRAT/LENGTH or FEDRAT/SCALE if type or value changed
*/
		if ((Shgt_chc==0&&atof(Ssvar[2])!=Sctr_hgt_val[0]) ||
				(Shgt_chc==1&&atof(Ssvar[2])!=Sctr_hgt_val[1]) ||
				(Shgt_chc!=Sctr_hgt_chc))
		{
			ncl_add_token(cmdbuf,NCL_fedrat,NCL_nocomma);
			if (Shgt_chc==0)
				ncl_add_token(cmdbuf,NCL_length,NCL_comma);
			else
				ncl_add_token(cmdbuf,NCL_scale,NCL_comma);
			ncl_add_token(cmdbuf,tptr[3],NCL_nocomma);
/*
........Output NCL command
*/
			ncl_add_cmdbuf(cmdbuf);
		}
/*
......if no advance value changed
*/			
		if ((fedrat->slowdown_flag == Sfedrat.slowdown_flag)
			&& (fedrat->slowdown_type == Sfedrat.slowdown_type)
			&& (fedrat->slowdown_dist == Sfedrat.slowdown_dist)
			&& (fedrat->slowdown_feedrate == Sfedrat.slowdown_feedrate)
			&& (fedrat->accel_flag == Sfedrat.accel_flag)
			&& (fedrat->accel_type == Sfedrat.accel_type)
			&& (fedrat->accel_dist == Sfedrat.accel_dist)
			&& (fedrat->accel_feedrate == Sfedrat.accel_feedrate)
			&& (fedrat->height == Sfedrat.height))
			return;	
/*
......if there is advance value changed
*/
/*
.....FEDRAT/AT
*/
		if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
		if (fedrat->mode != 0)
		{
			if (fedrat->slowdown_flag)
			{
				if ((fedrat->slowdown_dist==Sfedrat.slowdown_dist)
						&&(fedrat->slowdown_dist==0.0))
				{
/*
.....don't output advance setting
*/
					goto Fed_out;
				}
				if ((fedrat->slowdown_dist!=Sfedrat.slowdown_dist)
						&&(fedrat->slowdown_dist==0.0))
				{
/*
......Output FEDRAT/AT, 0
*/
					ncl_add_token(cmdbuf,NCL_fedrat,NCL_nocomma);
					ncl_add_token(cmdbuf,NCL_at,NCL_comma);
					ncl_add_token(cmdbuf,"0.",NCL_comma);
//					ncl_add_cmdbuf(cmdbuf);
					k = 1;
					goto Fed_out;
				}				
				ncl_add_token(cmdbuf,NCL_fedrat,NCL_nocomma);
				ncl_add_token(cmdbuf,NCL_at,NCL_comma);
				ncl_add_token(cmdbuf,tptr[1],NCL_comma);
/*
.....only output slowdown_feedrate if not 0
*/
				if (fedrat->slowdown_feedrate!=0)
				{	if (fedrat->slowdown_type == 1)
					     ncl_add_token(cmdbuf,NCL_scale,NCL_comma);
					ncl_add_token(cmdbuf,tptr[2],NCL_comma);
				}
				else
				{
/*
.....error message, feedrate=0 not allowed
*/
					;
				}

				if (fedrat->slowdown_flag > 99)
					ncl_add_token(cmdbuf,NCL_once,NCL_comma);
/*
				ncl_add_cmdbuf(cmdbuf);
*/
                k = 1;
			}
			else
			{
/*
......slowdown unchecked
*/
				if (kfl && (Sfedrat.slowdown_flag==1))
				{
					ncl_add_token(cmdbuf,NCL_fedrat,NCL_nocomma);
					ncl_add_token(cmdbuf,NCL_at,NCL_comma);
					ncl_add_token(cmdbuf,"0.",NCL_comma);
//					ncl_add_cmdbuf(cmdbuf);
					k = 1;
				}
			}
Fed_out:;
			if (fedrat->accel_flag)
			{
/*
.....FEDRAT/OUT
*/
				if ((fedrat->accel_dist==Sfedrat.accel_dist)
					&&(fedrat->accel_dist==0.0))
				{
/*
.....don't output advance setting
*/
					if (k==1)
						ncl_add_cmdbuf(cmdbuf);
					return;
				}
				if ((fedrat->accel_dist!=Sfedrat.accel_dist)
						&&(fedrat->accel_dist==0.0))
				{
/*
......Output FEDRAT/OUT, 0
*/
					if (k == 0) 
	                    ncl_add_token(cmdbuf,NCL_fedrat,NCL_nocomma);
					ncl_add_token(cmdbuf,NCL_out,NCL_comma);
					ncl_add_token(cmdbuf,"0.",NCL_comma);
					ncl_add_cmdbuf(cmdbuf);
					return;
				}			
				if (k == 0) ncl_add_token(cmdbuf,NCL_fedrat,NCL_nocomma);				
				ncl_add_token(cmdbuf,NCL_out,NCL_comma);
				ncl_add_token(cmdbuf,tptr[4],NCL_comma);
				if (fedrat->accel_type == 1)
					ncl_add_token(cmdbuf,NCL_scale,NCL_comma);
				ncl_add_token(cmdbuf,tptr[5],NCL_comma);
//				ncl_add_token(cmdbuf,tptr[0],NCL_comma);
				if (fedrat->accel_flag > 99)
					ncl_add_token(cmdbuf,NCL_once,NCL_comma);
			}
			else if (kfl && (Sfedrat.accel_flag==1))
			{
				if (k == 0)
					ncl_add_token(cmdbuf,NCL_fedrat,NCL_nocomma);
				ncl_add_token(cmdbuf,NCL_out,NCL_comma);
				ncl_add_token(cmdbuf,"0.",NCL_comma);
			}
/*
........Output NCL command
*/
//see if cmdbuf is empty string
			if (strlen(cmdbuf->cur_str)!=0)
				ncl_add_cmdbuf(cmdbuf);
//			}
			k = 0;
		}
	}
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
/*********************************************************************
**    I_FUNCTION     : S_init_form(fieldno,val,stat)
**       Defines the displayed picture based on the feed rate mode.
**    PARAMETERS
**       INPUT  :
**          fieldno = Not used.
**          val     = Not used.
**          stat    = Not used.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT S_init_form(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_REAL rval;
/*
.....Display the correct picture
*/
	if (Sadvtog)
	{
		if (Sstog[0]||Satog[0])
			ud_dispfrm_update_frmpic(Sfrm1,0,"Feedrate_Advanced.jpg");
		else
			ud_dispfrm_update_frmpic(Sfrm1,0,"Feedrate.jpg");
	}
	else
		ud_dispfrm_update_frmpic(Sfrm1,0,"Feedrate.jpg");
			
	S_enable_buttons();

	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_define_fedrat(fedrat,ftog,fvar,stog,svar,atog,
**                                     avar,traverse,display,reentry);
**       Defines the Fedrat settings based on the field parameters.
**    PARAMETERS
**       INPUT  :
**          fedrat   = Current Feed Rate settings.
**       OUTPUT :
**          ftog[0]  = FEDRAT/mode setting.
**          fvar     = Feed rate.
**          stog[0]  = FEDRAT/AT mode.
**          stog[1]  = FEDRAT/AT feed rate type.
**          stog[2]  = Once setting.
**          svar[0]  = FEDRAT/AT distance.
**          svar[1]  = FEDRAT/AT feed rate.
**          svar[2]  = FEDRAT/AT,OUT feed rate control height.
**          atog[0]  = FEDRAT/OUT mode.
**          atog[1]  = FEDRAT/OUT feed rate type.
**          atog[2]  = Once setting.
**          avar[0]  = FEDRAT/OUT distance.
**          avar[1]  = FEDRAT/OUT feed rate.
**          traverse = Field traversal settings.
**          reentry  = UU_TRUE = Form is being reentered for the 2nd
**                     time.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_fedrat(fedrat,ftog,fvar,stog,svar,atog,avar,traverse,
	display,reentry)
NCLX_mot_feedrate *fedrat;
int ftog[];
char fvar[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int stog[];
char svar[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int atog[];
char avar[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
char *traverse,*display;
UU_LOGICAL reentry;
{
	int i;
/*
.....Define FEDRAT settings
*/
	if (!reentry)
	{
		if (Smodal)
		{
			if (fedrat->mode == 3)
/*
.....SAME, not used, default to IPM
*/
			{
				Sfedrat.mode = fedrat->mode = ftog[0] = 1;
			}
			else
			{
				Sfedrat.mode = fedrat->mode;
				ftog[0] = fedrat->mode;
			}
		}
		else
		{
			Sfedrat.mode = fedrat->mode;
			if (fedrat->mode == 3)
			{
/*
.....SAME
*/
				Sfedrat.mode = fedrat->mode = ftog[1] = 1;
			}
			else
			{
				Sfedrat.mode = ftog[1] = fedrat->mode;
			}
		}
		ncl_sprintf(fvar,&fedrat->base_feedrate,1);
/*
.....Define FEDRAT/AT settings
*/	
		if (fedrat->slowdown_flag)
			stog[0] = 1;
		if (fedrat->slowdown_flag==2)
		{
/*
....ONCE is on
....we should uncheck the accelerate checkbox
*/
			stog[0] = 0;
		}
		stog[1] = fedrat->slowdown_type;
		stog[2] = UU_FALSE;
		ncl_sprintf(svar[0],&fedrat->slowdown_dist,1);
		ncl_sprintf(svar[1],&fedrat->slowdown_feedrate,1);
/*
.....the "FEDRAT/LENGTH(SCALE)", val output the control height is different that original "FEDRAT/AT(OUT), LENGTH, val"
.....We will never out put "LENGTH" with "AT/OUT", per KEN
*/
//		ncl_sprintf(svar[2],&fedrat->height,1);
		if (Shgt_chc==0)
			ncl_sprintf(svar[2], &Sctr_hgt_val[0], 1);
		else
			ncl_sprintf(svar[2], &Sctr_hgt_val[1], 1);
/*
.....Define FEDRAT/OUT settings
*/	
		if (fedrat->accel_flag)
			atog[0] = 1;
		if (fedrat->accel_flag==2)
		{
/*
....ONCE is on
....we should uncheck the accelerate checkbox
*/
			atog[0] = 0;
		}
		atog[1] = fedrat->accel_type;
		atog[2] = UU_FALSE;
		ncl_sprintf(avar[0],&fedrat->accel_dist,1);
		ncl_sprintf(avar[1],&fedrat->accel_feedrate,1);
	}
	traverse[SLW_HGT] = 1;
	traverse[SLW_HGT_CHC] = 1;
/*
.....Set FEDRAT/AT fields
*/
	traverse[SLW_DIS] = traverse[SLW_FED] = stog[0];
	traverse[SLW_TYP] = traverse[SLW_ONC] = stog[0];
/*
.....Set FEDRAT/OUT fields
*/
	traverse[ACC_DIS] = traverse[ACC_FED] = atog[0];
	traverse[ACC_TYP] = traverse[ACC_ONC] = atog[0];
/*
....Set Advanced fields
*/
	Sadvtog = stog[0] || atog[0] || Smodal;
/*	if (Smodal&&(Sftog[0]==2)||(Smodal==0)&&(Sftog[1]==0))
	{
		traverse[FED_ADV] = 1;
	}
*/
/*
.....Same and FPR allow advance
.....Yurong per Ken
*/
/*
	else if ((Sftog[0]==0)||(Sftog[0]==0))
	{
		traverse[FED_ADV] = 1;
	}
	else
	{
		traverse[FED_ADV] = 0;
		Sadvtog = 0;
	}
*/
/*
.....init to disable if rapid
*/
	if (Smodal&&(Sftog[0]!=0)||(Smodal==0)&&(Sftog[1]!=0))
	{
		traverse[FED_RAT] = 1;
		traverse[SLW_HGT_CHC] = 1;
		traverse[SLW_HGT] = 1;
	}
	else
	{
		Sadvtog = 0;
		traverse[FED_RAT] = 0;
		traverse[SLW_HGT_CHC] = 0;
		traverse[SLW_HGT] = 0;
	}
	for (i=SLW_MOD;i<=ACC_ONC;i++) // display[i] = Sadvtog;
		traverse[i] = Sadvtog;
	traverse[SLW_MOD] = 1;
	traverse[ACC_MOD] = 1;
}

/*********************************************************************
**    I_FUNCTION     : S_fedrat_changed(fedrat,caxis)
**       Determines if the Feed Rate settings have changed.
**    PARAMETERS
**       INPUT  :
**          fedrat   = Active feed rate settings.
**          caxis    = Textual form responses.
**       OUTPUT :
**    RETURNS      : UU_TRUE if the settings have changed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_fedrat_changed(fedrat,caxis)
NCLX_mot_feedrate *fedrat;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
{
	int inum;
	UU_LOGICAL chg;
	UU_REAL rval;
/*
.....Determine if FEDRAT settings have changed
*/
	chg = UU_FALSE;
	if (fedrat->mode != Sfedrat.mode ||
		fedrat->slowdown_flag != Sfedrat.slowdown_flag ||
		fedrat->slowdown_type != Sfedrat.slowdown_type ||
		fedrat->accel_flag != Sfedrat.accel_flag ||
		fedrat->accel_type != Sfedrat.accel_type ||
		(Shgt_chc==0&&atof(Ssvar[2])!=Sctr_hgt_val[0]) ||
		(Shgt_chc==1&&atof(Ssvar[2])!=Sctr_hgt_val[1]) ||
		(Shgt_chc!=Sctr_hgt_chc))
		chg = UU_TRUE;
	else
	{
		if (ul_to_reals(&rval,&inum,1,caxis[0]) == UU_SUCCESS)
			if (NEQTOL(rval,Sfedrat.base_feedrate)) chg = UU_TRUE;
		if (Sadvtog)
		{
			if (fedrat->slowdown_flag)
			{
				if (ul_to_reals(&rval,&inum,1,caxis[1]) == UU_SUCCESS)
					if (NEQTOL(rval,Sfedrat.slowdown_dist)) chg = UU_TRUE;
				if (ul_to_reals(&rval,&inum,1,caxis[2]) == UU_SUCCESS)
					if (NEQTOL(rval,Sfedrat.slowdown_feedrate)) chg = UU_TRUE;
				if (ul_to_reals(&rval,&inum,1,caxis[3]) == UU_SUCCESS)
					if (NEQTOL(rval,Sfedrat.height)) chg = UU_TRUE;
			}
			if (fedrat->accel_flag)
			{
				if (ul_to_reals(&rval,&inum,1,caxis[4]) == UU_SUCCESS)
					if (NEQTOL(rval,Sfedrat.accel_dist)) chg = UU_TRUE;
				if (ul_to_reals(&rval,&inum,1,caxis[5]) == UU_SUCCESS)
					if (NEQTOL(rval,Sfedrat.accel_feedrate)) chg = UU_TRUE;
				if (ul_to_reals(&rval,&inum,1,caxis[3]) == UU_SUCCESS)
					if (NEQTOL(rval,Sfedrat.height)) chg = UU_TRUE;
			}
		}
	}
	return(chg);
}
/*********************************************************************
**    I_FUNCTION     : S_fedrat_text(fieldno, val, stat)
**       Handles the Feed Rate text fields.
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
static UD_FSTAT S_fedrat_text(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int fno;
	UU_REAL rval;
	char erms[132];

	switch (*fieldno)
	{
	case FED_RAT:
		S_enable_buttons();
		if (Smodal)
			return(UD_FLDOK);
/*
......updated main form
......this feedrate form called by many form,
......nclc_get_fedrate_fldnum only used by contour form
.....changed the way we pass the field number
*/
//		fno = nclc_get_fedrate_fldnum();
		fno = S_fldno;
		if (fno>=0)
		{
			ud_update_answer(fno, &Sfvar);
/*
.....Update the form
*/
			ud_update_form(0);
		}
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_slow_text(fieldno, val, stat)
**       Handles the Feed Rate slowdown text fields.
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
static UD_FSTAT S_slow_text(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int fno;
	UU_REAL rval;
	char erms[132];

	switch (*fieldno)
	{
	case SLW_FED:
		S_enable_buttons();
	}
	return(UD_FLDOK);
}


/*********************************************************************
**    I_FUNCTION     : S_accel_text(fieldno, val, stat)
**       Handles the Feed Rate slowdown text fields.
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
static UD_FSTAT S_accel_text(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_fedrat(fieldno, val, stat)
**       Handles the Feed Rate toggle fields.
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
static UD_FSTAT S_toggle_fedrat(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int ival,fno;
	UD_DDATA tval;
	UD_DDATA data;
	char str[80];
	UU_REAL rval;

	ud_default_method(fieldno, val, stat);

	switch(*fieldno)
	{
	case FED_MOD: 
	case FED_MOD2: 
		if (val->frmint[0] == 0)
		{
/*
.....RAPID
*/
			ud_setfrm_traverse_mask(Sfrm1,FED_RAT,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,SLW_HGT_CHC,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,SLW_HGT,UU_FALSE);
			ival = 0;
			ud_dispfrm_update_frmpic(Sfrm1,0,"Feedrate.jpg");
			tval.frmint = &ival;
			S_toggle_advanced(&tval,stat);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm1,FED_RAT,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,SLW_HGT_CHC,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,SLW_HGT,UU_TRUE);
			ival = 1;
			tval.frmint = &ival;
			S_toggle_advanced(&tval,stat);
		}
		S_enable_buttons();
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_slow(fieldno, val, stat)
**       Handles the Slowdown toggle fields.
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
static UD_FSTAT S_toggle_slow(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UD_DDATA data;
	char str[80];
	UU_REAL rval;

	ud_default_method(fieldno, val, stat);
	data.frmstr = str;

	switch(*fieldno)
	{
	case SLW_MOD: 
		ud_setfrm_traverse_mask(Sfrm1,SLW_DIS,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,SLW_FED,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,SLW_TYP,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,SLW_ONC,val->frmint[0]);
		if (Sstog[0]||Satog[0])
			ud_dispfrm_update_frmpic(Sfrm1,0,"Feedrate_Advanced.jpg");
		else
			ud_dispfrm_update_frmpic(Sfrm1,0,"Feedrate.jpg");
//		ud_getfrm_field(Sfrm1,ACC_MOD,data,UU_FALSE);
//		ud_setfrm_traverse_mask(Sfrm1,SLW_HGT,val->frmint[0]||data.frmint[0]);
		S_enable_buttons();
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_accel(fieldno, val, stat)
**       Handles the Accelerate toggle fields.
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
static UD_FSTAT S_toggle_accel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UD_DDATA data;
	char str[80];
	UU_REAL rval;

	ud_default_method(fieldno, val, stat);
	data.frmstr = str;

	switch(*fieldno)
	{
	case ACC_MOD: 
		ud_setfrm_traverse_mask(Sfrm1,ACC_DIS,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,ACC_FED,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,ACC_TYP,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,ACC_ONC,val->frmint[0]);
		if (Sstog[0]||Satog[0])
			ud_dispfrm_update_frmpic(Sfrm1,0,"Feedrate_Advanced.jpg");
		else
			ud_dispfrm_update_frmpic(Sfrm1,0,"Feedrate.jpg");
//		ud_getfrm_field(Sfrm1,SLW_MOD,data,UU_FALSE);
//		ud_setfrm_traverse_mask(Sfrm1,SLW_HGT,val->frmint[0]||data.frmint[0]);
		S_enable_buttons();
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_advanced(val, stat)
**       Handles the Advanced toggle field.
**    PARAMETERS
**       INPUT  :
**          val     = Current field value.
**          stat    = Not used.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT S_toggle_advanced(val, stat)
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	for (i=SLW_MOD;i<=ACC_ONC;i++)
		ud_setfrm_traverse_mask(Sfrm1,i,val->frmint[0]);
/*
.....Display the correct picture
*/
	if (val->frmint[0])
	{
		if (Sstog[0]||Satog[0])
			ud_dispfrm_update_frmpic(Sfrm1,0,"Feedrate_Advanced.jpg");
		else
			ud_dispfrm_update_frmpic(Sfrm1,0,"Feedrate.jpg");
	}
	else
		ud_dispfrm_update_frmpic(Sfrm1,0,"Feedrate.jpg");
	return(UD_FLDOK);
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
	UU_REAL rval;
	UU_LOGICAL chk1,chk2, chk3;
	UU_LOGICAL ifl;

	chk2 = UU_TRUE;
	chk3 = UU_TRUE;
	chk1 = UU_TRUE;

	if (Smodal&&(Sftog[0]==0)||(Smodal==0)&&(Sftog[1]==0))
	{
		ud_dispfrm_set_attribs(0,FED_RAT,UM_BLACK,UM_WHITE);
	}
	else
	{
		rval = atof(Sfvar);
		if (!((strlen(Sfvar)==0)||((rval>0.001)&&(rval<=99999.999))))
		{
/*
.....use red color and dis-active the OK button
*/
			ud_dispfrm_set_attribs(0,FED_RAT,UM_WHITE,UM_RED);
			chk1 = UU_FALSE;
		}
		else
		{
			ud_dispfrm_set_attribs(0,FED_RAT,UM_BLACK,UM_WHITE);
		}
/*
......default uncheck, but if checked, need enable/disable the related item
*/
		if (Sstog[0]==1)
		{
			ud_setfrm_traverse_mask(Sfrm1,SLW_DIS,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,SLW_FED,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,SLW_TYP,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,SLW_ONC,UU_TRUE);
//			ud_setfrm_traverse_mask(Sfrm1,SLW_HGT,UU_TRUE);
			rval = atof(Ssvar[1]);
			if (!((strlen(Ssvar[1])==0)||((rval>0.0)&&(rval<=99999.999))))
			{
				ud_dispfrm_set_attribs(0,SLW_FED,UM_WHITE,UM_RED);
				chk2 = UU_FALSE;
			}
			else
				ud_dispfrm_set_attribs(0,SLW_FED,UM_BLACK,UM_WHITE);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm1,SLW_DIS,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,SLW_FED,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,SLW_TYP,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,SLW_ONC,UU_FALSE);
//			ud_setfrm_traverse_mask(Sfrm1,SLW_HGT,Satog[0]);
			ud_dispfrm_set_attribs(0,SLW_FED,UM_BLACK,UM_WHITE);
		}
		if (Satog[0]==1)
		{
			ud_setfrm_traverse_mask(Sfrm1,ACC_DIS,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,ACC_FED,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,ACC_TYP,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,ACC_ONC,UU_TRUE);
//			ud_setfrm_traverse_mask(Sfrm1,SLW_HGT,UU_TRUE);
			rval = atof(Savar[1]);
			if (!((strlen(Savar[1])==0)||((rval>0.0)&&(rval<=99999.999))))
			{
				ud_dispfrm_set_attribs(0,ACC_FED,UM_WHITE,UM_RED);
				chk3 = UU_FALSE;
			}
			else
				ud_dispfrm_set_attribs(0,ACC_FED,UM_BLACK,UM_WHITE);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm1,ACC_DIS,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,ACC_FED,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,ACC_TYP,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,ACC_ONC,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,SLW_HGT,Sstog);
			ud_dispfrm_set_attribs(0,ACC_FED,UM_BLACK,UM_WHITE);
		}
	}
//	ifl = chk1 && chk2 && chk3;
	ifl = chk1;
	ud_frm_enable_ok(ifl);
}

void nclf_set_ctr_hgt(chc, rnum)
UM_real8 *rnum;
UM_int2 *chc;
{
	Sctr_hgt_chc = (int)(*chc);
	Sctr_hgt_val[Sctr_hgt_chc] = *rnum;
	Sctr_hgt_set = 1;
}

void nclc_get_ctr_hgt(chc, rnum)
UU_REAL *rnum;
int *chc;
{
	if (Sctr_hgt_set)
	{
		*chc = Sctr_hgt_chc;
		*rnum = Sctr_hgt_val[Sctr_hgt_chc];
	}
	else
	{
		*chc = 0;
		*rnum = 0.0;
	}
}