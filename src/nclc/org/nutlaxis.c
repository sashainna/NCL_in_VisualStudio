/*********************************************************************
**		NAME:  nutlaxis.c
**		CONTAINS:
**			nclu_tlaxis
**			nclu_tlaxis_form
**			nclu_tlaxis_set_tlaxis
**			nclu_tlaxis_command
**    COPYRIGHT 2015 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nutlaxis.c , 25.6
**    DATE AND TIME OF LAST  MODIFICATION
**       11/23/15 , 09:14:21
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
	MODE_FIXED,
	FXD_MODE,
	FXD_VEC,
	FXD_NORM,

	MODE_TANTO,
	TAN_HGT,
	TAN_PARL,
	TAN_SURF,
	TAN_4AX,
	TAN_VEC,
	TAN_MNT,

	MODE_FAN,
	FAN_HGT,
	FAN_CEN,
	FAN_SMO,
	FAN_DEG,
	FAN_RAT,

	MODE_COMBIN,
	COM_HGT,
	COM_PARL,
	COM_LVD,
	COM_APD,
	COM_SURF,
	COM_CEN,
	COM_SMO,
	COM_DEG,
	COM_RAT,

	MODE_NORMAL,
	NOR_SURF,
	NOR_4AX,
	NOR_VEC,
	NOR_MNT,

	MODE_ATANGL,
	ATA_ANG,
	ATA_CLD,
	ATA_SURF,
	ATA_CNT,
	ATA_4AX,
	ATA_VEC,
	ATA_MNT,

	MODE_POINT,
	PNT_GEO,

	MODE_CURVE,
	CRV_GEO,
	CRV_DIS,

	MODE_INTERP,
	INT_GEO,
	INT_SMO,
	INT_DEG,
	INT_RAT,

	MODE_TILT,
	TLT_FWD,
	TLT_RGT,

	MODE_GUIDE,
	GDE_GEO,
	GDE_CTN,
	GDE_DIS,
	GDE_CON,

	MODE_GOUGE,

	MODE_LOCK,
	LCK_TYP,
	LCK_LIN,
	LCK_DIS,
	LCK_TRN,
	LCK_FAN,
	LCK_MOV,

	MODE_MODIFY,
	MOD_UP,
	MOD_RGT,
	MOD_LFT,
	MOD_FWD,
	MOD_ANG,

	MODE_ADV
};

extern UD_METHOD UD_initfrm_intry;

static int Sfrm1=0;
static UU_LOGICAL Sactive1 = UU_FALSE,Sfixed;
static NCLX_mot_tlaxis Staxis;
static int Sadvtog=0;
enum {Fixed, Tanto, Fan, Combin, Normal, Atangl, Point, Curve, Interp};
	
#define NOPT 5

static int Sblack[3]={0,0,0}, Sgreen[3]={0,180,0};
static int Syellow[3]={180,180,0}, Sorange[3]={180,116,0};
static char *Smode[]={"Fixed", "Tanto DS", "Fan", "Combine", "Normal PS",
	"Atangle PS", "Thru Point", "Thru Curve", "Interpolate"};
static char *Soption[5]={"Tilt Angles", "Guide Curve", "Gouge Check",
	"Lock Tlaxis","Modify"};

static int Sftog[3],Sdtog[4],Sntog[3],Sctog[4],Smtog[3],Satog[4],Sptog[2],Svtog[2];
static int Srtog[2],Sttog[2],Sgtog[3],Sktog[2],Sltog[5],Sytog[2];
static char Sfvec[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Sdvec[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Sdhgt[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Sdsurf[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Snhgt[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Sndeg[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Snrat[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Schgt[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Sclvd[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Scapd[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Scsurf[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Scdeg[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Scrat[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Smsurf[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Smvec[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Saang[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Sadis[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Sasurf[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Savec[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Spgeo[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Svgeo[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Svdis[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Srgeo[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Srdeg[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Srrat[NCL_MAX_LABEL_AND_SUBSCRIPT+1];

static char Stfwd[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Strgt[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Sggeo[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Sgdis[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Slds1[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Slds2[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Symod[5][NCL_MAX_LABEL_AND_SUBSCRIPT+1];

void nclu_tlaxis_command();

static UD_FSTAT S_init_form();
static void S_define_advanced();
static void S_define_fixed();
static void S_define_tanto();
static void S_define_fan();
static void S_define_combin();
static void S_define_normal();
static void S_define_atangl();
static void S_define_point();
static void S_define_curve();
static void S_define_interp();
static void S_define_tilt();
static void S_define_guide();
static void S_define_gougck();
static void S_define_lock();
static void S_define_modify();
static UD_FSTAT S_toggle_mode();
static UD_FSTAT S_toggle_option();
static UD_FSTAT S_toggle_fixed();
static UD_FSTAT S_toggle_tanto();
static UD_FSTAT S_toggle_fan();
static UD_FSTAT S_toggle_combin();
static UD_FSTAT S_toggle_normal();
static UD_FSTAT S_toggle_atangl();
static UD_FSTAT S_toggle_interp();
static UD_FSTAT S_toggle_guide();
static UD_FSTAT S_toggle_lock();
static UD_FSTAT S_toggle_advanced();
static UD_FSTAT OnClose();
static void S_toggle_page();
static void S_modify_pages();

/*********************************************************************
**    E_FUNCTION     : nclu_tlaxis()
**       Controlling routine to call the TOOL AXIS form routine from
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
void nclu_tlaxis()
{
	int status;
	char caxis[6][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	char maxis[12][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	NCL_cmdbuf cmdbuf;
	NCLX_mdl_struct secps;
	NCLX_mdl_data guide;
	NCLX_mdl_curve curve;
	NCLX_mot_tlaxis taxis;
/*
.....Get active Tool Axis parameters
*/
	taxis.curve = &curve;
	taxis.modify.secps = &secps;
	taxis.modify.guide = &guide;
	NclxMotGetTlaxis(&taxis);
/*
.....Display the form
*/
	status = nclu_tlaxis_form(&taxis,caxis,maxis,UU_FALSE,UU_TRUE,UU_FALSE);
/*
.....Output the tool axis command
*/
	if (status == UU_SUCCESS)
	{
		nclu_tlaxis_command(&cmdbuf,&taxis,caxis,maxis,UU_TRUE,UU_TRUE);
		ncl_set_cmdmode(UU_TRUE);
		ncl_call(&cmdbuf);
	}
}

/*********************************************************************
**    E_FUNCTION     : nclu_tlaxis_form(taxis,caxis,maxis,fixed,modal,reentry)
**       Processes the TOOL AXIS form.
**    PARAMETERS
**       INPUT  :
**          taxis    = Active tool axis parameters.
**          fixed     - UU_TRUE = Allow only FIXED tool axis mode.
**          modal     - UU_TRUE = Form is modal (standard),
**                      UU_FALSE = Form is display type subform.
**          reentry   - UU_TRUE = Form is being reentered for the 2nd
**                      time.  Only used for nonmodal form type.
**       OUTPUT :
**          taxis    = Updated with form tool axis settings.
**          caxis    = Textual form responses.
**          maxis    = Textual form responses for optional parameters.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_tlaxis_form(taxis,caxis,maxis,fixed,modal,reentry)
NCLX_mot_tlaxis *taxis;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
char maxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
UU_LOGICAL fixed,modal,reentry;
{
	int status,nc,irtn,fieldno,stat;
	UD_DDATA val;
	UD_METHOD save_entry;

	static char traverse[] =
	{
		1,1,1, 1, 					/* FIXED */
		1,1,1, 1,1,1,1,			/* TANTO,DS */
		1,1, 1,1,1,1,				/* FAN */
		1,1,1,1,1, 1,1,1,1,1,	/* COMBIN */
		1, 1,1,1,1,					/* NORMAL,PS */
		1,1,1, 1,1,1,1,1,			/* ATANGL,PS */
		1,1,		 					/* THRU,PT */
		1,1, 1,	 					/* THRU,CV */
		1,1, 1,1,1,					/* INTERP */
		1,1,1,						/* TILT */
		1,1,1,1,1,					/* GUIDE CV */
		1,								/* GOUGCK */
		1,1,1,1,1,1,1, 			/* LOCK */
		1,1,1,1,1,1,				/* MODIFY */

		1								/* Advanced */
	};
	static char display[] =
	{
		1,1,1, 0, 					/* FIXED */
		1,1,1, 0,0,0,0,			/* TANTO,DS */
		1,1, 0,0,0,0, 				/* FAN */
		1,1,1,1,1, 0,0,0,0,0,	/* COMBIN */
		1, 0,0,0,0,					/* NORMAL,PS */
		1,1,1, 0,0,0,0,0,			/* ATANGL,PS */
		1,1,		 					/* THRU,PT */
		1,1, 0,	 					/* THRU,CV */
		1,1, 0,0,0,					/* INTERP */
		1,1,1,						/* TILT */
		1,1,1,1,1,					/* GUIDE CV */
		1,								/* GOUGCK */
		1,1,1,1,1,1,1,				/* LOCK */
		1,1,1,1,1,1,				/* MODIFY */
		1								/* Advanced */
	};
	static UD_METHOD methods[] =
	{
		S_toggle_mode,S_toggle_fixed,UU_NULL, UU_NULL,
		S_toggle_mode,UU_NULL,S_toggle_tanto, UU_NULL,S_toggle_tanto,UU_NULL,
			UU_NULL,
		S_toggle_mode,UU_NULL, UU_NULL,S_toggle_fan,UU_NULL,UU_NULL,
		S_toggle_mode,UU_NULL,S_toggle_combin,UU_NULL,UU_NULL, UU_NULL,UU_NULL,
			S_toggle_combin,UU_NULL,UU_NULL,
		S_toggle_mode, UU_NULL,S_toggle_normal,UU_NULL,UU_NULL,
		S_toggle_mode,UU_NULL,UU_NULL, UU_NULL,UU_NULL,S_toggle_atangl,UU_NULL,
			UU_NULL,
		S_toggle_mode,UU_NULL,
		S_toggle_mode,UU_NULL, UU_NULL,
		S_toggle_mode,UU_NULL, S_toggle_interp,UU_NULL,UU_NULL,
		S_toggle_option,UU_NULL,UU_NULL,
		S_toggle_option,UU_NULL,S_toggle_guide,UU_NULL,UU_NULL,
		S_toggle_option,
		S_toggle_option,S_toggle_lock,UU_NULL,UU_NULL,S_toggle_lock,UU_NULL,
			UU_NULL,
		S_toggle_option,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		S_toggle_advanced,OnClose
	};

	static char called[] =
	{
		6,6,6, 6,
		6,6,6, 6,6,6,6,
		6,6, 6,6,6,6,
		6,6,6,6,6, 6,6,6,6,6,
		6, 6,6,6,6,
		6,6,6, 6,6,6,6,6,
		6,6,
		6,6, 6,
		6,6, 6,6,6,
		6,6,6,
		6,6,6,6,6,
		6,
		6,6,6,6,6,6,6,
		6,6,6,6,6,6,
		6
	};
	static int *ans[] =
	{
		&Sftog[0],&Sftog[1],(int *)Sfvec, &Sftog[2],
		&Sdtog[0],(int *)Sdhgt,&Sdtog[1], (int *)Sdsurf,&Sdtog[2],(int *)Sdvec,
			&Sdtog[3],
		&Sntog[0],(int *)Snhgt, &Sntog[1],&Sntog[2],(int *)Sndeg,(int *)Snrat,
		&Sctog[0],(int *)Schgt,&Sctog[1],(int *)Sclvd,(int *)Scapd, (int *)Scsurf,
			&Sctog[2],&Sctog[3],(int *)Scdeg,(int *)Scrat,
		&Smtog[0],(int *)Smsurf,&Smtog[1],(int *)Smvec,&Smtog[2],
		&Satog[0],(int *)Saang,(int *)Sadis,(int *)Sasurf,&Satog[1],&Satog[2],
			(int *)Savec,&Satog[3],
		&Sptog[0],(int *)Spgeo,
		&Svtog[0],(int *)Svgeo,(int *)Svdis,
		&Srtog[0],(int *)Srgeo,&Srtog[1],(int *)Srdeg,(int *)Srrat,
		&Sttog[0],(int *)Stfwd,(int *)Strgt,
		&Sgtog[0],(int *)Sggeo,&Sgtog[1],(int *)Sgdis,&Sgtog[2],
		&Sktog[0],
		&Sltog[0],&Sltog[1],&Sltog[2],(int *)Slds1,&Sltog[3],&Sltog[4],
			(int *)Slds2,
		&Sytog[0],(int *)Symod[0],(int *)Symod[1],(int *)Symod[2],(int *)Symod[3],
			(int *)Symod[4],
		&Sadvtog
	};
/*
.....Initialize routine
*/
	Staxis = *taxis;
	Sfixed = fixed;
/*
.....Set up form fields
*/
	S_define_advanced(taxis,reentry);
	S_define_fixed(taxis,Sftog,Sfvec,display,traverse,reentry);
	S_define_tanto(taxis,Sdtog,Sdhgt,Sdsurf,Sdvec,display,traverse,reentry);
	S_define_fan(taxis,Sntog,Snhgt,Sndeg,Snrat,display,traverse,reentry);
	S_define_combin(taxis,Sctog,Schgt,Sclvd,Scapd,Scsurf,Scdeg,Scrat,
		display,traverse,reentry);
	S_define_normal(taxis,Smtog,Smsurf,Smvec,display,traverse,reentry);
	S_define_atangl(taxis,Satog,Saang,Sadis,Sasurf,Savec,display,traverse,
		reentry);
	S_define_point(taxis,Sptog,Spgeo,display,traverse,reentry);
	S_define_curve(taxis,Svtog,Svgeo,Svdis,display,traverse,reentry);
	S_define_interp(taxis,Srtog,Srgeo,Srdeg,Srrat,display,traverse,reentry);
	S_define_tilt(taxis,Sttog,Stfwd,Strgt,display,traverse,reentry);
	S_define_guide(taxis,Sgtog,Sggeo,Sgdis,display,traverse,reentry);
	S_define_gougck(taxis,Sktog,display,traverse,reentry);
	S_define_lock(taxis,Sltog,Slds1,Slds2,display,traverse,reentry);
	S_define_modify(taxis,Sytog,Symod,display,traverse,reentry);
/*
.....Define form entry routine
*/
	save_entry = UD_initfrm_intry;
/*
.....Get the Form input
*/
again:;
	if (modal)
	{
		Sfrm1 = 0;
		UD_initfrm_intry = S_init_form;
		status = ud_form1("ntlaxis.frm",ans,ans,methods,called,display,traverse);
		UD_initfrm_intry = save_entry;
		if (status == -1) return(status);
	}
	else if (!Sactive1)
	{
		Sfrm1 = ud_form_display1("ntlaxis.frm",ans,ans,methods,called,display,
			traverse);
		if (Sfrm1 != 0)
		{
			Sactive1 = UU_TRUE;
			fieldno = stat = 0;
			S_init_form(&fieldno,&val,stat);
		}
		goto done;
	}
/*
.....Store the active tool axis parameters
*/
	irtn = nclu_tlaxis_set_tlaxis(taxis,caxis,maxis);
	if (irtn != UU_SUCCESS) goto again;
/*
.....End of routine
*/
done:;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : nclu_tlaxis_set_tlaxis(taxis,caxis,maxis)
**       Builds and outputs the tool axis command.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT :
**          taxis    = Active tool axis parameters.
**          caxis    = Textual form responses.
**          maxis    = Textual form responses for optional parameters.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_tlaxis_set_tlaxis(taxis,caxis,maxis)
NCLX_mot_tlaxis *taxis;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
char maxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
{
	int irtn;
/*
.....Store active tool axis settings
*/
	if (Sftog[0] == 1)
		irtn = S_set_fixed(taxis,caxis,Sftog,Sfvec);
	else if (Sdtog[0] == 1)
		irtn = S_set_tanto(taxis,caxis,Sdtog,Sdhgt,Sdsurf,Sdvec);
	else if (Sntog[0] == 1)
		irtn = S_set_fan(taxis,caxis,Sntog,Snhgt,Sndeg,Snrat);
	else if (Sctog[0] == 1)
		irtn = S_set_combin(taxis,caxis,Sctog,Schgt,Sclvd,Scapd,Scsurf,Scdeg,
			Scrat);
	else if (Smtog[0] == 1)
		irtn = S_set_normal(taxis,caxis,Smtog,Smsurf,Smvec);
	else if (Satog[0] == 1)
		irtn = S_set_atangl(taxis,caxis,Satog,Saang,Sadis,Sasurf,Savec);
	else if (Sptog[0] == 1)
		irtn = S_set_point(taxis,caxis,Sptog,Spgeo);
	else if (Svtog[0] == 1)
		irtn = S_set_curve(taxis,caxis,Svtog,Svgeo,Svdis);
	else if (Srtog[0] == 1)
		irtn = S_set_interp(taxis,caxis,Srtog,Srgeo,Srdeg,Srrat);
/*
.....Store active tool axis options
*/
	irtn = irtn + S_set_tilt(taxis,maxis,Sttog,Stfwd,Strgt);
	irtn = irtn + S_set_guide(taxis,maxis,Sgtog,Sggeo,Sgdis);
	irtn = irtn + S_set_gougck(taxis,maxis,Sktog);
	irtn = irtn + S_set_lock(taxis,maxis,Sltog,Slds1,Slds2);
	irtn = irtn + S_set_modify(taxis,maxis,Sytog,Symod);
/*
.....End of routine
*/
	return(irtn);
}

/*********************************************************************
**    E_FUNCTION     : nclu_tlaxis_command(cmdbuf,taxis,caxis,maxis,kfl,flag)
**       Builds and outputs the tool axis command.
**    PARAMETERS
**       INPUT  :
**          taxis    = Active tool axis parameters.
**          caxis    = Textual form responses.
**          maxis    = Textual form responses for optional parameters.
**          kfl      = UU_TRUE = Command is generated from the interface.
**                     Check if changed from previous settings and use
**                     'caxis' variables in command output.
**          flag     = UU_TRUE = output command to source file.
**                     UU_FALSE = Preview command only.
**       OUTPUT :
**          cmdbuf   = Command buffer to contain the TLAXIS command.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_tlaxis_command(cmdbuf,taxis,caxis,maxis,kfl,flag)
NCL_cmdbuf *cmdbuf;
NCLX_mot_tlaxis *taxis;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
char maxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
UU_LOGICAL kfl,flag;
{
	int nc,inum,i,j,ist,ideg;
	UU_REAL rval[3],rate;
	char str[80];
	char *cptr[6],cbuf[6][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	char *mptr[12],mbuf[12][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
/*
.....Initialize command
*/
	ncl_init_cmdbuf(cmdbuf);
	if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
	ncl_add_token(cmdbuf,NCL_tlaxis,NCL_nocomma);
	if (kfl)
	{
		for (i=0;i<6;i++) cptr[i] = caxis[i];
		for (i=0;i<11;i++) mptr[i] = maxis[i];
	}
	else
	{
		for (i=0;i<6;i++) cptr[i] = cbuf[i];
		for (i=0;i<10;i++) mptr[i] = mbuf[i];
		switch (taxis->mode)
		{
		case NCLX_MOT_TLAXIS_FIXED:
			ncl_sprintf(cbuf[0],taxis->vector,3);
			break;
		case NCLX_MOT_TLAXIS_TANTO:
			ncl_sprintf(cbuf[0],&taxis->height,1);
			cbuf[1][0] = '\0';
			if (taxis->modify.secps_flag)
				ncl_get_label_with_key(taxis->modify.secps->key,cbuf[1]);
			ncl_sprintf(cbuf[2],taxis->perpto,3);
			break;
		case NCLX_MOT_TLAXIS_FAN:
			ncl_sprintf(cbuf[0],&taxis->height,1);
			NclxMotGetFanInterp(&ideg,&rate);
			sprintf(cbuf[1],"%d",ideg);
			ncl_sprintf(cbuf[2],&rate,1);
			break;
		case NCLX_MOT_TLAXIS_COMBIN:
			ncl_sprintf(cbuf[0],&taxis->height,1);
			ncl_sprintf(cbuf[1],&taxis->cmb_depart,1);
			ncl_sprintf(cbuf[2],&taxis->cmb_approach,1);
			cbuf[3][0] = '\0';
			if (taxis->modify.secps_flag)
				ncl_get_label_with_key(taxis->modify.secps->key,cbuf[3]);
			NclxMotGetFanInterp(&ideg,&rate);
			sprintf(cbuf[4],"%d",ideg);
			ncl_sprintf(cbuf[5],&rate,1);
			break;
		case NCLX_MOT_TLAXIS_NORMAL:
			cbuf[0][0] = '\0';
			if (taxis->modify.secps_flag)
				ncl_get_label_with_key(taxis->modify.secps->key,cbuf[0]);
			ncl_sprintf(cbuf[1],taxis->perpto,3);
			break;
		case NCLX_MOT_TLAXIS_ATANGL:
			ncl_sprintf(cbuf[0],&taxis->angle,1);
			ncl_sprintf(cbuf[1],&taxis->heel,1);
			cbuf[2][0] = '\0';
			if (taxis->modify.secps_flag)
				ncl_get_label_with_key(taxis->modify.secps->key,cbuf[2]);
			ncl_sprintf(cbuf[3],taxis->perpto,3);
			break;
		case NCLX_MOT_TLAXIS_POINT:
			ncl_sprintf(cbuf[0],taxis->point,3);
			break;
		case NCLX_MOT_TLAXIS_CURVE:
			cbuf[0][0] = '\0';
			if (taxis->curve != UU_NULL &&
					taxis->curve->header.relnum != NCLX_MDL_UNDEF)
				ncl_get_label_with_key(taxis->curve->header.key,cbuf[0]);
			ncl_sprintf(cbuf[1],&taxis->curve_dist,1);
			break;
		case NCLX_MOT_TLAXIS_INTERP:
			ncl_sprintf(cbuf[0],taxis->vector,3);
			NclxMotGetFanInterp(&ideg,&rate);
			sprintf(cbuf[1],"%d",ideg);
			ncl_sprintf(cbuf[2],&rate,1);
			break;
		}

		ncl_sprintf(mbuf[0],&taxis->modify.fwd_angle,1);
		ncl_sprintf(mbuf[1],&taxis->modify.right_angle,1);
		mbuf[2][0] = '\0';
		if (taxis->modify.guide_flag == 1)
			ncl_get_label_with_key(taxis->modify.guide->data.header.key,mbuf[2]);
		ncl_sprintf(mbuf[3],&taxis->modify.guide_offset,1);
		ncl_sprintf(mbuf[4],&taxis->modify.lock_transition,1);
		ncl_sprintf(mbuf[5],&taxis->modify.lock_radius,1);

		ncl_sprintf(mbuf[6],&taxis->adjust.up_offset,1);
		ncl_sprintf(mbuf[7],&taxis->adjust.fwd_offset,1);
		ncl_sprintf(mbuf[8],&taxis->adjust.right_offset,1);
		ncl_sprintf(mbuf[9],&taxis->adjust.fwd_tilt,1);
		ncl_sprintf(mbuf[10],&taxis->adjust.right_tilt,1);
	}
/*
.....TLAXIS/SAME
*/
	switch (taxis->mode)
	{
	case NCLX_MOT_TLAXIS_SAME:
		ncl_add_token(cmdbuf,NCL_same,NCL_comma);
		if (taxis->normal) ncl_add_token(cmdbuf,NCL_normal,NCL_comma);
		break;
/*
.....TLAXIS/vector
*/
	case NCLX_MOT_TLAXIS_FIXED:
		nc = strlen(cptr[0]);
		ul_strip_blanks(cptr[0],&nc);
		ncl_add_token(cmdbuf,cptr[0],NCL_comma);
		if (taxis->normal) ncl_add_token(cmdbuf,NCL_normal,NCL_comma);
		break;
/*
.....TLAXIS/TANTO,DS
*/
	case NCLX_MOT_TLAXIS_TANTO:
		ncl_add_token(cmdbuf,NCL_tanto,NCL_comma);
		ncl_add_token(cmdbuf,NCL_ds,NCL_comma);
		nc = strlen(cptr[0]);
		ul_strip_blanks(cptr[0],&nc);
		ncl_add_token(cmdbuf,cptr[0],NCL_comma);
		if (taxis->parelm == 1)
			ncl_add_token(cmdbuf,NCL_parelm,NCL_comma);
		else
		{
			nc = strlen(cptr[1]);
			ul_strip_blanks(cptr[1],&nc);
			if (nc != 0)
			{
				ncl_add_token(cmdbuf,NCL_ps,NCL_comma);
				ncl_add_token(cmdbuf,cptr[1],NCL_comma);
			}
			if (taxis->perpto_flag != 0)
			{
				ncl_add_token(cmdbuf,NCL_perpto,NCL_comma);
				if (taxis->perpto_flag == 2)
					ncl_add_token(cmdbuf,NCL_last,NCL_comma);
				nc = strlen(cptr[2]);
				ul_strip_blanks(cptr[2],&nc);
				if (ul_to_reals(rval,&inum,3,cptr[2]) == UU_SUCCESS)
				{
					sprintf(str,"(VECTOR/%s)",cptr[2]);
					strcpy(cptr[2],str);
				}
				ncl_add_token(cmdbuf,cptr[2],NCL_comma);
			}
		}
		break;
/*
.....TLAXIS/FAN
*/
	case NCLX_MOT_TLAXIS_FAN:
		ncl_add_token(cmdbuf,NCL_tanto,NCL_comma);
		ncl_add_token(cmdbuf,NCL_ds,NCL_comma);
		nc = strlen(cptr[0]);
		ul_strip_blanks(cptr[0],&nc);
		ncl_add_token(cmdbuf,cptr[0],NCL_comma);
		ncl_add_token(cmdbuf,NCL_fan,NCL_comma);
		if (taxis->center != 0)
		{
			ncl_add_token(cmdbuf,NCL_center,NCL_comma);
			if (taxis->center == 1)
				ncl_add_token(cmdbuf,NCL_on,NCL_comma);
			else
				ncl_add_token(cmdbuf,NCL_auto,NCL_comma);
		}
		if (taxis->normal == 1)
		{
			ncl_add_token(cmdbuf,NCL_smooth,NCL_comma);
			nc = strlen(cptr[1]);
			ul_strip_blanks(cptr[1],&nc);
			ncl_add_token(cmdbuf,cptr[1],NCL_comma);
			nc = strlen(cptr[2]);
			ul_strip_blanks(cptr[2],&nc);
			ncl_add_token(cmdbuf,cptr[2],NCL_comma);
		}
		break;
/*
.....TLAXIS/COMBIN
*/
	case NCLX_MOT_TLAXIS_COMBIN:
		ncl_add_token(cmdbuf,NCL_combin,NCL_comma);
		nc = strlen(cptr[0]);
		ul_strip_blanks(cptr[0],&nc);
		ncl_add_token(cmdbuf,cptr[0],NCL_comma);
		if (taxis->parelm == 1)
			ncl_add_token(cmdbuf,NCL_parelm,NCL_comma);
		if (taxis->parelm != 1)
		{
			nc = strlen(cptr[3]);
			ul_strip_blanks(cptr[3],&nc);
			if (nc != 0)
			{
				ncl_add_token(cmdbuf,NCL_ps,NCL_comma);
				ncl_add_token(cmdbuf,cptr[3],NCL_comma);
			}
		}
		if (taxis->center != 0)
		{
			ncl_add_token(cmdbuf,NCL_center,NCL_comma);
			if (taxis->center == 1)
				ncl_add_token(cmdbuf,NCL_on,NCL_comma);
			else
				ncl_add_token(cmdbuf,NCL_auto,NCL_comma);
		}
		nc = strlen(cptr[1]);
		ul_strip_blanks(cptr[1],&nc);
		ncl_add_token(cmdbuf,cptr[1],NCL_comma);
		nc = strlen(cptr[2]);
		ul_strip_blanks(cptr[2],&nc);
		ncl_add_token(cmdbuf,cptr[2],NCL_comma);
		if (taxis->normal == 1)
		{
			ncl_add_token(cmdbuf,NCL_smooth,NCL_comma);
			nc = strlen(cptr[4]);
			ul_strip_blanks(cptr[4],&nc);
			ncl_add_token(cmdbuf,cptr[4],NCL_comma);
			nc = strlen(cptr[5]);
			ul_strip_blanks(cptr[5],&nc);
			ncl_add_token(cmdbuf,cptr[5],NCL_comma);
		}
		break;
/*
.....TLAXIS/NORMAL,PS
*/
	case NCLX_MOT_TLAXIS_NORMAL:
		ncl_add_token(cmdbuf,NCL_normal,NCL_comma);
		ncl_add_token(cmdbuf,NCL_ps,NCL_comma);
		nc = strlen(cptr[0]);
		ul_strip_blanks(cptr[0],&nc);
		if (nc != 0)
			ncl_add_token(cmdbuf,cptr[0],NCL_comma);
		if (taxis->perpto_flag != 0)
		{
			ncl_add_token(cmdbuf,NCL_perpto,NCL_comma);
			if (taxis->perpto_flag == 2)
				ncl_add_token(cmdbuf,NCL_last,NCL_comma);
			nc = strlen(cptr[1]);
			ul_strip_blanks(cptr[1],&nc);
			if (ul_to_reals(rval,&inum,3,cptr[1]) == UU_SUCCESS)
			{
				sprintf(str,"(VECTOR/%s)",cptr[1]);
				strcpy(cptr[1],str);
			}
			ncl_add_token(cmdbuf,cptr[1],NCL_comma);
		}
		break;
/*
.....TLAXIS/ATANGL
*/
	case NCLX_MOT_TLAXIS_ATANGL:
		ncl_add_token(cmdbuf,NCL_atangl,NCL_comma);
		nc = strlen(cptr[0]);
		ul_strip_blanks(cptr[0],&nc);
		ncl_add_token(cmdbuf,cptr[0],NCL_comma);
		ncl_add_token(cmdbuf,NCL_ps,NCL_comma);
		nc = strlen(cptr[2]);
		ul_strip_blanks(cptr[2],&nc);
		if (nc != 0)
			ncl_add_token(cmdbuf,cptr[2],NCL_comma);
		nc = strlen(cptr[1]);
		ul_strip_blanks(cptr[1],&nc);
		if (nc != 0)
		{
			ncl_add_token(cmdbuf,NCL_cldist,NCL_comma);
			ncl_add_token(cmdbuf,cptr[1],NCL_comma);
		}
		if (taxis->contact != 0)
			ncl_add_token(cmdbuf,NCL_contct,NCL_comma);
		if (taxis->perpto_flag != 0)
		{
			ncl_add_token(cmdbuf,NCL_perpto,NCL_comma);
			if (taxis->perpto_flag == 2)
				ncl_add_token(cmdbuf,NCL_last,NCL_comma);
			nc = strlen(cptr[3]);
			ul_strip_blanks(cptr[3],&nc);
			if (ul_to_reals(rval,&inum,3,cptr[3]) == UU_SUCCESS)
			{
				sprintf(str,"(VECTOR/%s)",cptr[3]);
				strcpy(cptr[3],str);
			}
			ncl_add_token(cmdbuf,cptr[3],NCL_comma);
		}
		break;
/*
.....TLAXIS/THRU,point
*/
	case NCLX_MOT_TLAXIS_POINT:
		ncl_add_token(cmdbuf,NCL_thru,NCL_comma);
		nc = strlen(cptr[0]);
		ul_strip_blanks(cptr[0],&nc);
		ncl_add_token(cmdbuf,cptr[0],NCL_comma);
		break;
/*
.....TLAXIS/THRU,curve
*/
	case NCLX_MOT_TLAXIS_CURVE:
		ncl_add_token(cmdbuf,NCL_thru,NCL_comma);
		nc = strlen(cptr[0]);
		ul_strip_blanks(cptr[0],&nc);
		ncl_add_token(cmdbuf,cptr[0],NCL_comma);
		nc = strlen(cptr[1]);
		ul_strip_blanks(cptr[1],&nc);
		if (nc != 0)
			ncl_add_token(cmdbuf,cptr[1],NCL_comma);
		break;
/*
.....TLAXIS/INTERP
*/
	case NCLX_MOT_TLAXIS_INTERP:
		ncl_add_token(cmdbuf,NCL_interp,NCL_comma);
		nc = strlen(cptr[0]);
		ul_strip_blanks(cptr[0],&nc);
		ncl_add_token(cmdbuf,cptr[0],NCL_comma);
		if (taxis->normal == 1)
		{
			ncl_add_token(cmdbuf,NCL_smooth,NCL_comma);
			nc = strlen(cptr[1]);
			ul_strip_blanks(cptr[1],&nc);
			ncl_add_token(cmdbuf,cptr[1],NCL_comma);
			nc = strlen(cptr[2]);
			ul_strip_blanks(cptr[2],&nc);
			ncl_add_token(cmdbuf,cptr[2],NCL_comma);
		}
		break;
	}
/*
.....TLAXIS/FWD,RIGHT
*/
	if (taxis->modify.angle_flag == 1)
	{
		if (ul_to_reals(rval,&inum,1,mptr[0]) != UU_SUCCESS)
			rval[0] = 1.;
		if (ul_to_reals(rval,&inum,1,mptr[1]) != UU_SUCCESS)
			rval[1] = 1.;
		if (rval[0] != 0.)
		{
			ncl_add_token(cmdbuf,NCL_fwd,NCL_comma);
			ncl_add_token(cmdbuf,mptr[0],NCL_comma);
		}
		if (rval[1] != 0.)
		{
			ncl_add_token(cmdbuf,NCL_right,NCL_comma);
			ncl_add_token(cmdbuf,mptr[1],NCL_comma);
		}
	}
/*
.....TLAXIS/GUIDE,cv
*/
	if (taxis->modify.guide_flag == 1)
	{
		ncl_add_token(cmdbuf,NCL_guide,NCL_comma);
		ncl_add_token(cmdbuf,mptr[2],NCL_comma);
		if (taxis->modify.guide_cond == 1)
			ncl_add_token(cmdbuf,NCL_tlon,NCL_comma);
		else
		{
			if (taxis->modify.guide_contact == 1)
				ncl_add_token(cmdbuf,NCL_contct,NCL_comma);
			else
				ncl_add_token(cmdbuf,NCL_offset,NCL_comma);
			if (taxis->modify.guide_cond == 0)
				ncl_add_token(cmdbuf,NCL_tllft,NCL_comma);
			else if (taxis->modify.guide_cond == 2)
				ncl_add_token(cmdbuf,NCL_tlrgt,NCL_comma);
			if (ul_to_reals(rval,&inum,1,mptr[3]) != UU_SUCCESS)
				rval[0] = 1.;
			if (rval[0] != 0.) ncl_add_token(cmdbuf,mptr[3],NCL_comma);
		}
	}
/*
.....TLAXIS/GOUGCK
*/
	if (taxis->modify.gouge == 1)
	{
		ncl_add_token(cmdbuf,NCL_gougck1,NCL_comma);
		ncl_add_token(cmdbuf,NCL_on,NCL_comma);
	}
/*
.....TLAXIS/LOCK
*/
	if (taxis->modify.lock_mode != NCLX_LOCK_OFF)
	{
		ncl_add_token(cmdbuf,NCL_lock,NCL_comma);
		if (taxis->modify.lock_mode == NCLX_LOCK_END)
			ncl_add_token(cmdbuf,NCL_end,NCL_comma);
		else
		{
			ncl_add_token(cmdbuf,mptr[4],NCL_comma);
			if (taxis->modify.lock_radius == 0)
				ncl_add_token(cmdbuf,NCL_linear,NCL_comma);
			else
				ncl_add_token(cmdbuf,NCL_radius,NCL_comma);
			
			if (ul_to_reals(rval,&inum,1,mptr[5]) != UU_SUCCESS)
				rval[0] = 1.;
			if (rval[0] != 0.)
			{
				ncl_add_token(cmdbuf,mptr[5],NCL_comma);
				if (taxis->modify.lock_transition == 0)
					ncl_add_token(cmdbuf,NCL_interp,NCL_comma);
				else if (taxis->modify.lock_transition == 1)
					ncl_add_token(cmdbuf,NCL_fan,NCL_comma);
			}
			if (taxis->modify.lock_mode == NCLX_LOCK_OMIT)
				ncl_add_token(cmdbuf,NCL_omit,NCL_comma);
		}
	}
/*
.....TLAXIS/MODIFY
*/
	if (taxis->adjust_flag == 1)
	{
		ncl_add_token(cmdbuf,NCL_modify,NCL_comma);
		ncl_add_token(cmdbuf,mptr[6],NCL_comma);
		ist = 7;
		for (i=7;i<=10;i++)
		{
			rval[0] = 0.;
			if (ul_to_reals(rval,&inum,1,mptr[i]) != UU_SUCCESS || rval[0] != 0.)
			{
				for (j=ist;j<=i;j++) ncl_add_token(cmdbuf,mptr[j],NCL_comma);
				ist = i + 1;
			}
		}
	}
/*
.....Output NCL command
*/
	ncl_add_cmdbuf(cmdbuf);
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
**       Defines the Advanced checkbox value based on the tool axis mode.
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
	int i,fno,istat,inc,i1;
	UD_DDATA data;
/*
.....TLAXIS/fixed
*/
	switch (Staxis.mode)
	{
	case NCLX_MOT_TLAXIS_FIXED:
	case NCLX_MOT_TLAXIS_SAME:
		inc = Fixed;
		fno = MODE_FIXED;
		if (Sfixed)
			for (i=Tanto;i<=Interp;i++)
				ud_form_section_enable(Sfrm1,Smode[i],UU_FALSE);
		break;
/*
.....TLAXIS/TANTO,DS
*/
	case NCLX_MOT_TLAXIS_TANTO:
		inc = Tanto;
		fno = MODE_TANTO;
		break;
/*
.....TLAXIS/FAN
*/
	case NCLX_MOT_TLAXIS_FAN:
		inc = Fan;
		fno = MODE_FAN;
		break;
/*
.....TLAXIS/COMBIN
*/
	case NCLX_MOT_TLAXIS_COMBIN:
		inc = Combin;
		fno = MODE_COMBIN;
		break;
/*
.....TLAXIS/NORMAL,PS
*/
	case NCLX_MOT_TLAXIS_NORMAL:
		inc = Normal;
		fno = MODE_NORMAL;
		break;
/*
.....TLAXIS/ATANGL
*/
	case NCLX_MOT_TLAXIS_ATANGL:
		inc = Atangl;
		fno = MODE_ATANGL;
		break;
/*
.....TLAXIS/THRU,point
*/
	case NCLX_MOT_TLAXIS_POINT:
		inc = Point;
		fno = MODE_POINT;
		break;
/*
.....TLAXIS/THRU,curve
*/
	case NCLX_MOT_TLAXIS_CURVE:
		inc = Curve;
		fno = MODE_CURVE;
		break;
/*
.....TLAXIS/INTERP
*/
	case NCLX_MOT_TLAXIS_INTERP:
		inc = Interp;
		fno = MODE_INTERP;
		break;
	}
/*
.....Set active tool axis page
*/
	i1 = 1;
	data.frmint = &i1;
	istat = 0;
	ud_form_section_active(Sfrm1, Smode[inc]);
	S_toggle_mode(&fno,&data,&istat);
/*
.....Set active tool axis options
*/
	fno = MODE_TILT; i1 = Staxis.modify.angle_flag;
	S_toggle_option(&fno,&data,&istat);
	fno = MODE_GUIDE; i1 = Staxis.modify.guide_flag;
	S_toggle_option(&fno,&data,&istat);
	fno = MODE_GOUGE; i1 = Staxis.modify.gouge;
	S_toggle_option(&fno,&data,&istat);
	if (Staxis.modify.lock_mode == NCLX_LOCK_OFF) i1 = 0;
	else if (Staxis.modify.lock_mode == NCLX_LOCK_OMIT) i1 = 1;
	else if (Staxis.modify.lock_mode == NCLX_LOCK_ON) i1 = 2;
	else if (Staxis.modify.lock_mode == NCLX_LOCK_END) i1 = 3;
	fno = MODE_LOCK;
	i1 = 0; if (Staxis.modify.lock_mode != 0) i1 = 1;
	S_toggle_option(&fno,&data,&istat);
	fno = MODE_MODIFY; i1 = Staxis.adjust_flag;
	S_toggle_option(&fno,&data,&istat);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_define_advanced(taxis,reentry)
**       Defines the Advanced checkbox value based on the tool axis mode.
**    PARAMETERS
**       INPUT  :
**          taxis    = Current tool axis settings.
**          reentry  = UU_TRUE - Don't initialize fillet settings.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_advanced(taxis,reentry)
NCLX_mot_tlaxis *taxis;
UU_LOGICAL reentry;
{
	int ideg;
	UU_REAL rate;
/*
.....Initialize Advanced checkbox
*/
	if (reentry) return;
	Sadvtog = 0;
/*
.....TLAXIS/fixed
*/
	switch (taxis->mode)
	{
	case NCLX_MOT_TLAXIS_FIXED:
	case NCLX_MOT_TLAXIS_SAME:
		if (taxis->normal) Sadvtog = 1;
		break;
/*
.....TLAXIS/TANTO,DS
*/
	case NCLX_MOT_TLAXIS_TANTO:
		if (taxis->modify.secps_flag || taxis->perpto_flag != 0) Sadvtog = 1;
		break;
/*
.....TLAXIS/FAN
*/
	case NCLX_MOT_TLAXIS_FAN:
		if (taxis->center) Sadvtog = 1;
		NclxMotGetFanInterp(&ideg,&rate);
		if (ideg > 1 && rate != 0.) Sadvtog = 1;
		break;
/*
.....TLAXIS/COMBIN
*/
	case NCLX_MOT_TLAXIS_COMBIN:
		if (taxis->modify.secps_flag || taxis->center != 0) Sadvtog = 1;
		NclxMotGetFanInterp(&ideg,&rate);
		if (ideg > 1 && rate != 0.) Sadvtog = 1;
		break;
/*
.....TLAXIS/NORMAL,PS
*/
	case NCLX_MOT_TLAXIS_NORMAL:
		if (taxis->modify.secps_flag || taxis->perpto_flag != 0) Sadvtog = 1;
		break;
/*
.....TLAXIS/ATANGL
*/
	case NCLX_MOT_TLAXIS_ATANGL:
		if (taxis->modify.secps_flag || taxis->perpto_flag != 0 ||
			taxis->contact != 0) Sadvtog = 1;
		break;
/*
.....TLAXIS/THRU,curve
*/
	case NCLX_MOT_TLAXIS_CURVE:
		if (taxis->curve_dist != 0) Sadvtog = 1;
		break;
/*
.....TLAXIS/INTERP
*/
	case NCLX_MOT_TLAXIS_INTERP:
		NclxMotGetFanInterp(&ideg,&rate);
		if (ideg > 1 && rate != 0.) Sadvtog = 1;
		break;
	}
}

/*********************************************************************
**    I_FUNCTION     : S_define_fixed(taxis,ftog,fvec,display,traverse,reentry)
**       Defines the field parameters for the Fixed Tool Axis fields
**       based on the active tool axis mode.
**    PARAMETERS
**       INPUT  :
**          taxis    = Current tool axis settings.
**          reentry  = UU_TRUE - Don't initialize fillet settings.
**       OUTPUT :
**          ftog[0]  = TLAXIS/SAME is in effect.
**          ftog[1]  = Same, Fixed, or View.
**          ftog[2]  = Normal to PS.
**          fvec     = Tool axis vector.
**          display  = Field display settings.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_fixed(taxis,ftog,fvec,display,traverse,reentry)
NCLX_mot_tlaxis *taxis;
int ftog[];
char *fvec;
char *display,*traverse;
UU_LOGICAL reentry;
{
	UU_LOGICAL flag;
/*
.....Define FIXED settings
*/
	if (!reentry)
	{
		ftog[1] = taxis->mode == NCLX_MOT_TLAXIS_SAME ? 0 : 1;
		ftog[2] = taxis->normal;
		sprintf(fvec,"%8.5f,%8.5f,%8.5f",taxis->vector[0],taxis->vector[1],
			taxis->vector[2]);
	}
/*
.....Disable FIXED fields
*/
	flag = taxis->mode == NCLX_MOT_TLAXIS_SAME ||
		taxis->mode == NCLX_MOT_TLAXIS_FIXED;
	if (reentry) flag = ftog[0];
	if (!flag)
	{
		ftog[0] = 0;
		traverse[FXD_MODE] = traverse[FXD_VEC] = traverse[FXD_NORM] = 0;
	}
/*
.....Enable FIXED fields
*/
	else
	{
		ftog[0] = 1;
		traverse[FXD_MODE] = traverse[FXD_NORM] = 1;
		if (ftog[1] == 0 || ftog[1] == 2)
			traverse[FXD_VEC] = 0;
		else
			traverse[FXD_VEC] = 1;
	}
/*
.....Set Advanced fields
*/
	display[FXD_NORM] = Sadvtog;
}

/*********************************************************************
**    I_FUNCTION     : S_set_fixed(taxis,ftog,fvec,display,traverse)
**       Defines the field parameters for the Fixed Tool Axis fields
**       based on the active tool axis mode.
**    PARAMETERS
**       INPUT  :
**          ftog[0]  = TLAXIS/SAME is in effect.
**          ftog[1]  = Same, Fixed, or View.
**          ftog[2]  = Normal to PS.
**          fvec     = Tool axis vector.
**       OUTPUT :
**          taxis    = Current tool axis settings.
**          caxis    = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_fixed(taxis,caxis,ftog,fvec)
NCLX_mot_tlaxis *taxis;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int ftog[];
char *fvec;
{
	if (ftog[1] == 0) taxis->mode = NCLX_MOT_TLAXIS_SAME;
	else taxis->mode = NCLX_MOT_TLAXIS_FIXED;
	strcpy(caxis[0],fvec);
	if (Sadvtog) taxis->normal = ftog[2];
	else taxis->normal = 0;
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_define_tanto(taxis,dtog,dhgt,dsurf,dvec,display,
**                                    traverse,reentry)
**       Sets the tool axis variables based on the field parameters for
**       the Tanto DS Tool Axis fields
**    PARAMETERS
**       INPUT  :
**          taxis   = Current tool axis settings.
**          reentry  = UU_TRUE - Don't initialize fillet settings.
**       OUTPUT :
**          dtog[0] = TLAXIS/TANTO,DS is in effect.
**          dtog[1] = Parallel to Elements
**          dtog[2] = 4-axis Control
**          dtog[3] = Maintain perpto vector after modifiers
**          dhgt    = Contact height along cutter.
**          dsurf   = Controlling surface.
**          dvec    = Perpto vector.
**          display  = Field display settings.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_tanto(taxis,dtog,dhgt,dsurf,dvec,display,traverse,
	reentry)
NCLX_mot_tlaxis *taxis;
int dtog[];
char *dhgt,*dsurf,*dvec;
char *display,*traverse;
UU_LOGICAL reentry;
{
	UU_LOGICAL flag;
/*
.....Define TANTO settings
*/
	if (!reentry)
	{
		dtog[1] = taxis->parelm;
		dtog[2] = taxis->perpto_flag != 0;
		dtog[3] = taxis->perpto_flag == 2;
		ncl_sprintf(dhgt,&taxis->height,1);
		if (taxis->modify.secps_flag)
			ncl_get_label_with_key(taxis->modify.secps->key,dsurf);
		else
			dsurf[0] = '\0';
		if (dtog[2] != 0)
			sprintf(dvec,"%8.5f,%8.5f,%8.5f",taxis->perpto[0],taxis->perpto[1],
				taxis->perpto[2]);
		else
			dvec[0] = '\0';
	}
/*
.....Disable TANTO fields
*/
	flag = taxis->mode == NCLX_MOT_TLAXIS_TANTO;
	if (reentry) flag = dtog[0];
	if (!flag)
	{
		dtog[0] = 0;
		traverse[TAN_HGT] = traverse[TAN_PARL] = traverse[TAN_SURF] = 0;
		traverse[TAN_4AX] = traverse[TAN_VEC] = traverse[TAN_MNT] = 0;
	}
/*
.....Enable TANTO fields
*/
	else
	{
		dtog[0] = 1;
		traverse[TAN_HGT] = traverse[TAN_PARL] = traverse[TAN_SURF] = 1;
		if (dtog[1] == 1)
		{
			traverse[TAN_4AX] = 0;
			traverse[TAN_VEC] = traverse[TAN_MNT] = 0;
		}
		else
		{
			traverse[TAN_4AX] = 1;
			traverse[TAN_VEC] = traverse[TAN_MNT] = dtog[2];
		}
	}
/*
.....Set Advanced fields
*/
	display[TAN_SURF] = Sadvtog;
	display[TAN_4AX] = Sadvtog;
	display[TAN_VEC] = Sadvtog;
	display[TAN_MNT] = Sadvtog;
}

/*********************************************************************
**    I_FUNCTION     : S_set_tanto(taxis,dtog,dhgt,dsurf,dvec)
**       Sets the tool axis parameters based on the field parameters for
**       the Tanto DS Tool Axis fields
**    PARAMETERS
**       INPUT  :
**          dtog[1] = Parallel to Elements
**          dtog[2] = 4-axis Control
**          dtog[3] = Maintain perpto vector after modifiers
**          dhgt    = Contact height along cutter.
**          dsurf   = Controlling surface.
**          dvec    = Perpto vector.
**       OUTPUT :
**          taxis   = Current tool axis settings.
**          caxis   = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_tanto(taxis,caxis,dtog,dhgt,dsurf,dvec)
NCLX_mot_tlaxis *taxis;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int dtog[];
char *dhgt,*dsurf,*dvec;
{
/*
.....Define TANTO settings
*/
	taxis->mode = NCLX_MOT_TLAXIS_TANTO;
	taxis->parelm = dtog[1];
	strcpy(caxis[0],dhgt);
/*
.....Define Advanced settings
*/
	taxis->perpto_flag = 0;
	caxis[1][0] = '\0';
	caxis[2][0] = '\0';
	if (Sadvtog)
	{
		taxis->perpto_flag = dtog[2];
		if (dtog[2] == 1 && dtog[3] == 1) taxis->perpto_flag = 2;
		if (taxis->parelm == 0)
		{
			strcpy(caxis[1],dsurf);
			if (dtog[2] != 0) strcpy(caxis[2],dvec);
		}
	}
/*
.....End of routine
*/
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_define_fan(taxis,ntog,nhgt,display,traverse,reentry)
**       Sets the tool axis variables based on the field parameters for
**       the Fan Tool Axis fields
**    PARAMETERS
**       INPUT  :
**          taxis   = Current tool axis settings.
**          reentry  = UU_TRUE - Don't initialize fillet settings.
**       OUTPUT :
**          ntog[0] = TLAXIS/FAN is in effect.
**          ntog[1] = Enable center control.
**          nhgt    = Contact height along cutter.
**          ndeg    = Degree of polynomial curve for smooth control.
**          nrat    = Rate of change for smooth control.
**          display  = Field display settings.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_fan(taxis,ntog,nhgt,ndeg,nrat,display,traverse,reentry)
NCLX_mot_tlaxis *taxis;
int ntog[];
char *nhgt,*ndeg,*nrat;
char *display,*traverse;
UU_LOGICAL reentry;
{
	int ideg;
	UU_LOGICAL flag;
	UU_REAL rate;
/*
.....Define FAN settings
*/
	if (!reentry)
	{
		ntog[1] = taxis->center;
		ncl_sprintf(nhgt,&taxis->height,1);
		NclxMotGetFanInterp(&ideg,&rate);
		if (ideg > 1 && rate != 0.) ntog[2] = 1;
		sprintf(ndeg,"%d",ideg);
		ncl_sprintf(nrat,&rate,1);
	}
/*
.....Disable FAN fields
*/
	flag = taxis->mode == NCLX_MOT_TLAXIS_FAN;
	if (reentry) flag = ntog[0];
	if (!flag)
	{
		ntog[0] = 0;
		traverse[FAN_HGT] = traverse[FAN_CEN] = 0;
		traverse[FAN_SMO] = traverse[FAN_DEG] = traverse[FAN_RAT] = 0;
	}
/*
.....Enable FAN fields
*/
	else
	{
		ntog[0] = 1;
		traverse[FAN_HGT] = traverse[FAN_CEN] = 1;
		traverse[FAN_SMO] = 1;
		traverse[FAN_DEG] = traverse[FAN_RAT] = ntog[2];
	}
/*
.....Set Advanced fields
*/
	display[FAN_CEN] = Sadvtog;
	display[FAN_SMO] = Sadvtog;
	display[FAN_DEG] = Sadvtog;
	display[FAN_RAT] = Sadvtog;
}

/*********************************************************************
**    I_FUNCTION     : S_set_fan(taxis,ntog,nhgt)
**       Sets the tool axis parameters based on the field parameters for
**       the Fan Tool Axis fields
**    PARAMETERS
**       INPUT  :
**          ntog[1] = Tool center control.
**          nhgt    = Contact height along cutter.
**          ndeg    = Degree of polynomial curve for smooth control.
**          nrat    = Rate of change for smooth control.
**       OUTPUT :
**          taxis   = Current tool axis settings.
**          caxis   = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_fan(taxis,caxis,ntog,nhgt,ndeg,nrat)
NCLX_mot_tlaxis *taxis;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int ntog[];
char *nhgt,*ndeg,*nrat;
{
/*
.....Define FAN settings
*/
	taxis->mode = NCLX_MOT_TLAXIS_FAN;
	strcpy(caxis[0],nhgt);
/*
.....Define Advanced settings
*/
	taxis->center = 0;
	caxis[3][0] = '\0';
	caxis[4][0] = '\0';
	caxis[5][0] = '\0';
	if (Sadvtog)
	{
		taxis->center = ntog[1];
		taxis->normal = ntog[2];
		if (taxis->normal == 1)
		{
			strcpy(caxis[1],ndeg);
			strcpy(caxis[2],nrat);
		}
	}
/*
.....End of routine
*/
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_define_combin(taxis,ctog,chgt,clvd,capd,csurf,
**                                     cdeg,crat,display,traverse,reentry)
**       Sets the tool axis variables based on the field parameters for
**       the Combine Tool Axis fields
**    PARAMETERS
**       INPUT  :
**          taxis   = Current tool axis settings.
**          reentry  = UU_TRUE - Don't initialize fillet settings.
**       OUTPUT :
**          ctog[0] = TLAXIS/TANTO,DS is in effect.
**          ctog[1] = Parallel to Elements
**          ctog[2] = Center of tool control
**          ctog[3] = Specify smooth fanning parameters
**          chgt    = Contact height along cutter.
**          clvd    = Departure distance.
**          capd    = Approach distance.
**          csurf   = Controlling surface.
**          cdeg    = Degree of polynomial curve for smooth control.
**          crat    = Rate of change for smooth control.
**          display  = Field display settings.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_combin(taxis,ctog,chgt,clvd,capd,csurf,cdeg,crat,
	display,traverse,reentry)
NCLX_mot_tlaxis *taxis;
int ctog[];
char *chgt,*clvd,*capd,*csurf,*cdeg,*crat;
char *display,*traverse;
UU_LOGICAL reentry;
{
	int ideg;
	UU_LOGICAL flag;
	UU_REAL rate;
/*
.....Define COMBIN settings
*/
	if (!reentry)
	{
		ctog[1] = taxis->parelm;
		ctog[2] = taxis->center;
		ctog[3] = 0;
		ncl_sprintf(chgt,&taxis->height,1);
		ncl_sprintf(clvd,&taxis->cmb_depart,1);
		ncl_sprintf(capd,&taxis->cmb_approach,1);
		if (taxis->modify.secps_flag)
			ncl_get_label_with_key(taxis->modify.secps->key,csurf);
		else
			csurf[0] = '\0';
		NclxMotGetFanInterp(&ideg,&rate);
		if (ideg > 1 && rate != 0.) ctog[3] = 1;
		sprintf(cdeg,"%d",ideg);
		ncl_sprintf(crat,&rate,1);
	}
/*
.....Disable COMBIN fields
*/
	flag = taxis->mode == NCLX_MOT_TLAXIS_COMBIN;
	if (reentry) flag = ctog[0];
	if (!flag)
	{
		ctog[0] = 0;
		traverse[COM_HGT] = traverse[COM_PARL] = traverse[COM_LVD] = 0;
		traverse[COM_APD] = traverse[COM_SURF] = traverse[COM_CEN] = 0;
		traverse[COM_SMO] = traverse[COM_DEG] = traverse[COM_RAT] = 0;
	}
/*
.....Enable COMBIN fields
*/
	else
	{
		ctog[0] = 1;
		traverse[COM_HGT] = traverse[COM_PARL] = traverse[COM_LVD] = 1;
		traverse[COM_APD] = traverse[COM_SURF] = traverse[COM_CEN] = 1;
		traverse[COM_SMO] = 1;
		traverse[COM_DEG] = traverse[COM_RAT] = ctog[3];
	}
/*
.....Set Advanced fields
*/
	display[COM_SURF] = Sadvtog;
	display[COM_CEN] = Sadvtog;
	display[COM_SMO] = Sadvtog;
	display[COM_DEG] = Sadvtog;
	display[COM_RAT] = Sadvtog;
}

/*********************************************************************
**    I_FUNCTION     : S_set_combin(taxis,caxis,ctog,chgt,clvd,capd,csurf,
**                                     cdeg,crat)
**       Sets the tool axis parameters based on the field parameters for
**       the Combin Tool Axis fields
**    PARAMETERS
**       INPUT  :
**          ctog[0] = TLAXIS/TANTO,DS is in effect.
**          ctog[1] = Parallel to Elements
**          ctog[2] = Center of tool control
**          ctog[3] = Specify smooth fanning parameters
**          chgt    = Contact height along cutter.
**          clvd    = Departure distance.
**          capd    = Approach distance.
**          csurf   = Controlling surface.
**          cdeg    = Degree of polynomial curve for smooth control.
**          crat    = Rate of change for smooth control.
**       OUTPUT :
**          taxis   = Current tool axis settings.
**          caxis   = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_combin(taxis,caxis,ctog,chgt,clvd,capd,csurf,cdeg,crat)
NCLX_mot_tlaxis *taxis;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int ctog[];
char *chgt,*clvd,*capd,*csurf,*cdeg,*crat;
{
/*
.....Define COMBIN settings
*/
	taxis->mode = NCLX_MOT_TLAXIS_COMBIN;
	taxis->parelm = ctog[1];
	strcpy(caxis[0],chgt);
	strcpy(caxis[1],clvd);
	strcpy(caxis[2],capd);
/*
.....Define Advanced settings
*/
	taxis->center = taxis->normal = 0;
	caxis[3][0] = '\0';
	caxis[4][0] = '\0';
	caxis[5][0] = '\0';
	if (Sadvtog)
	{
		strcpy(caxis[3],csurf);
		taxis->center = ctog[2];
		taxis->normal = ctog[3];
		if (taxis->normal == 1)
		{
			strcpy(caxis[4],cdeg);
			strcpy(caxis[5],crat);
		}
	}
/*
.....End of routine
*/
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_define_normal(taxis,mtog,msurf,mvec,display,
**                                    traverse,reentry)
**       Sets the tool axis variables based on the field parameters for
**       the Normal PS Tool Axis fields
**    PARAMETERS
**       INPUT  :
**          taxis   = Current tool axis settings.
**          reentry  = UU_TRUE - Don't initialize fillet settings.
**       OUTPUT :
**          mtog[0] = TLAXIS/NORMAL,PS is in effect.
**          mtog[1] = 4-axis Control
**          mtog[2] = Maintain perpto vector after modifiers
**          msurf   = Controlling surface.
**          mvec    = Perpto vector.
**          display  = Field display settings.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_normal(taxis,mtog,msurf,mvec,display,traverse,reentry)
NCLX_mot_tlaxis *taxis;
int mtog[];
char *msurf,*mvec;
char *display,*traverse;
UU_LOGICAL reentry;
{
	UU_LOGICAL flag;
/*
.....Define NORMAL settings
*/
	if (!reentry)
	{
		mtog[1] = taxis->perpto_flag != 0;
		mtog[2] = taxis->perpto_flag == 2;
		if (taxis->modify.secps_flag)
			ncl_get_label_with_key(taxis->modify.secps->key,msurf);
		else
			msurf[0] = '\0';
		if (mtog[1] != 0)
			sprintf(mvec,"%8.5f,%8.5f,%8.5f",taxis->perpto[0],taxis->perpto[1],
				taxis->perpto[2]);
		else
			mvec[0] = '\0';
	}
/*
.....Disable NORMAL fields
*/
	flag = taxis->mode == NCLX_MOT_TLAXIS_NORMAL;
	if (reentry) flag = mtog[0];
	if (!flag)
	{
		mtog[0] = 0;
		traverse[NOR_SURF] = traverse[NOR_4AX] = traverse[NOR_VEC] = 0;
		traverse[NOR_MNT] = 0;
	}
/*
.....Enable TANTO fields
*/
	else
	{
		mtog[0] = 1;
		traverse[NOR_SURF] = traverse[NOR_4AX] = 1;
		traverse[NOR_VEC] = traverse[NOR_MNT] = mtog[1];
	}
/*
.....Set Advanced fields
*/
	display[NOR_SURF] = Sadvtog;
	display[NOR_4AX] = Sadvtog;
	display[NOR_VEC] = Sadvtog;
	display[NOR_MNT] = Sadvtog;
}

/*********************************************************************
**    I_FUNCTION     : S_set_normal(taxis,mtog,msurf,mvec)
**       Sets the tool axis parameters based on the field parameters for
**       the Normal PS Tool Axis fields
**    PARAMETERS
**       INPUT  :
**          mtog[1] = 4-axis Control
**          mtog[2] = Maintain perpto vector after modifiers
**          msurf   = Controlling surface.
**          mvec    = Perpto vector.
**       OUTPUT :
**          taxis   = Current tool axis settings.
**          caxis   = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_normal(taxis,caxis,mtog,msurf,mvec)
NCLX_mot_tlaxis *taxis;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int mtog[];
char *msurf,*mvec;
{
/*
.....Define NORMAL settings
*/
	taxis->mode = NCLX_MOT_TLAXIS_NORMAL;
/*
.....Define Advanced settings
*/
	taxis->perpto_flag = 0;
	caxis[0][0] = '\0';
	caxis[1][0] = '\0';
	if (Sadvtog)
	{
		taxis->perpto_flag = mtog[1];
		if (mtog[1] == 1 && mtog[2] == 1) taxis->perpto_flag = 2;
		strcpy(caxis[0],msurf);
		if (taxis->perpto_flag != 0) strcpy(caxis[1],mvec);
	}
/*
.....End of routine
*/
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_define_atangl(taxis,atog,aang,adis,asurf,avec,
**                                     display,traverse,reentry)
**       Sets the tool axis variables based on the field parameters for
**       the Atangle PS Tool Axis fields
**    PARAMETERS
**       INPUT  :
**          taxis   = Current tool axis settings.
**          reentry  = UU_TRUE - Don't initialize fillet settings.
**       OUTPUT :
**          atog[0] = TLAXIS/ATANGL,PS is in effect.
**          atog[1] = Contact mode
**          atog[2] = 4-axis Control
**          atog[3] = Maintain perpto vector after modifiers
**          aang    = Angle to PS.
**          adis    = Heel clearance distance.
**          nsurf   = Controlling surface.
**          nvec    = Perpto vector.
**          display  = Field display settings.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_atangl(taxis,atog,aang,adis,asurf,avec,display,traverse,
	reentry)
NCLX_mot_tlaxis *taxis;
int atog[];
char *aang,*adis,*asurf,*avec;
char *display,*traverse;
UU_LOGICAL reentry;
{
	UU_LOGICAL flag;
/*
.....Define ATANGL settings
*/
	if (!reentry)
	{
		atog[1] = taxis->contact;
		atog[2] = taxis->perpto_flag != 0;
		atog[3] = taxis->perpto_flag == 2;
		ncl_sprintf(aang,&taxis->angle,1);
		if (taxis->heel == 0.) adis[0] = '\0';
		else ncl_sprintf(adis,&taxis->heel,1);
		if (taxis->modify.secps_flag)
			ncl_get_label_with_key(taxis->modify.secps->key,asurf);
		else
			asurf[0] = '\0';
		if (atog[2] != 0)
			sprintf(avec,"%8.5f,%8.5f,%8.5f",taxis->perpto[0],taxis->perpto[1],
				taxis->perpto[2]);
		else
			avec[0] = '\0';
	}
/*
.....Disable ATANGL fields
*/
	flag = taxis->mode == NCLX_MOT_TLAXIS_ATANGL;
	if (reentry) flag = atog[0];
	if (!flag)
	{
		atog[0] = 0;
		traverse[ATA_ANG] = traverse[ATA_CLD] = traverse[ATA_SURF] = 0;
		traverse[ATA_CNT] = traverse[ATA_4AX] = traverse[ATA_VEC] = 0;
		traverse[ATA_MNT] = 0;
	}
/*
.....Enable ATANGL fields
*/
	else
	{
		atog[0] = 1;
		atog[1] = taxis->contact;
		traverse[ATA_ANG] = traverse[ATA_CLD] = traverse[ATA_SURF] = 1;
		traverse[ATA_CNT] = traverse[ATA_4AX] = 1;
		traverse[ATA_VEC] = traverse[ATA_MNT] = atog[2];
	}
/*
.....Set Advanced fields
*/
	display[ATA_SURF] = Sadvtog;
	display[ATA_CNT] = Sadvtog;
	display[ATA_4AX] = Sadvtog;
	display[ATA_VEC] = Sadvtog;
	display[ATA_MNT] = Sadvtog;
}

/*********************************************************************
**    I_FUNCTION     : S_set_atangl(taxis,atog,aang,adis,asurf,avec)
**       Sets the tool axis parameters based on the field parameters for
**       the Atangle PS Tool Axis fields
**    PARAMETERS
**       INPUT  :
**          atog[1] = Contact mode.
**          atog[2] = 4-axis Control
**          atog[3] = Maintain perpto vector after modifiers
**          aang    = Angle to PS.
**          adis    = Heel clearance distance.
**          nsurf   = Controlling surface.
**          nvec    = Perpto vector.
**       OUTPUT :
**          taxis   = Current tool axis settings.
**          caxis   = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_atangl(taxis,caxis,atog,aang,adis,asurf,avec)
NCLX_mot_tlaxis *taxis;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int atog[];
char *aang,*adis,*asurf,*avec;
{
/*
.....Define ATANGL settings
*/
	taxis->mode = NCLX_MOT_TLAXIS_ATANGL;
	strcpy(caxis[0],aang);
	strcpy(caxis[1],adis);
/*
.....Define Advanced settings
*/
	taxis->contact = 0;
	taxis->perpto_flag = 0;
	caxis[2][0] = '\0';
	caxis[3][0] = '\0';
	if (Sadvtog)
	{
		taxis->contact = atog[1];
		taxis->perpto_flag = atog[2];
		if (atog[2] == 1 && atog[3] == 1) taxis->perpto_flag = 2;
		strcpy(caxis[2],asurf);
		if (atog[2] != 0) strcpy(caxis[3],avec);
	}
/*
.....End of routine
*/
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_define_point(taxis,ptog,pgeo,display,traverse)
**       Defines the field parameters for the Thru Point Tool Axis fields
**       based on the active tool axis mode.
**    PARAMETERS
**       INPUT  :
**          taxis    = Current tool axis settings.
**          reentry  = UU_TRUE - Don't initialize fillet settings.
**       OUTPUT :
**          ptog[0]  = TLAXIS/THRU,point is in effect.
**          pgeo     = Point for tool axis control.
**          display  = Field display settings.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_point(taxis,ptog,pgeo,display,traverse,reentry)
NCLX_mot_tlaxis *taxis;
int ptog[];
char *pgeo;
char *display,*traverse;
UU_LOGICAL reentry;
{
	UU_LOGICAL flag;
/*
.....Define POINT settings
*/
	if (!reentry)
	{
		sprintf(pgeo,"%8.5f,%8.5f,%8.5f",taxis->point[0],taxis->point[1],
			taxis->point[2]);
	}
/*
.....Disable POINT fields
*/
	flag = taxis->mode == NCLX_MOT_TLAXIS_POINT;
	if (reentry) flag = ptog[0];
	if (!flag)
	{
		ptog[0] = 0;
		traverse[PNT_GEO] = 0;
	}
/*
.....Enable FIXED fields
*/
	else
	{
		ptog[0] = 1;
		traverse[PNT_GEO] = 1;
	}
}

/*********************************************************************
**    I_FUNCTION     : S_set_point(taxis,ptog,pgeo)
**       Defines the field parameters for the Thru Point Tool Axis fields
**       based on the active tool axis mode.
**    PARAMETERS
**       INPUT  :
**          ptog[0]  = TLAXIS/THRU,point is in effect.
**          pgeo     = Point for tool axis control.
**       OUTPUT :
**          taxis    = Current tool axis settings.
**          caxis    = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_point(taxis,caxis,ptog,pgeo)
NCLX_mot_tlaxis *taxis;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int ptog[];
char *pgeo;
{
	taxis->mode = NCLX_MOT_TLAXIS_POINT;
	strcpy(caxis[0],pgeo);
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_define_curve(taxis,vtog,vgeo,vdis,display,traverse)
**       Defines the field parameters for the Thru Curve Tool Axis fields
**       based on the active tool axis mode.
**    PARAMETERS
**       INPUT  :
**          taxis    = Current tool axis settings.
**          reentry  = UU_TRUE - Don't initialize fillet settings.
**       OUTPUT :
**          vtog[0]  = TLAXIS/THRU,curve is in effect.
**          vgeo     = Curve for tool axis control.
**          vdis     = Approximate distance of curve.
**          display  = Field display settings.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_curve(taxis,vtog,vgeo,vdis,display,traverse,reentry)
NCLX_mot_tlaxis *taxis;
int vtog[];
char *vgeo,*vdis;
char *display,*traverse;
UU_LOGICAL reentry;
{
	UU_LOGICAL flag;
/*
.....Define CURVE settings
*/
	if (!reentry)
	{
		if (taxis->curve != UU_NULL &&
			taxis->curve->header.relnum != NCLX_MDL_UNDEF)
				ncl_get_label_with_key(taxis->curve->header.key,vgeo);
		else
			vgeo[0] = '\0';
		if (taxis->curve_dist == 0.)
			vdis[0] = '\0';
		else
			ncl_sprintf(vdis,&taxis->curve_dist,1);
	}
/*
.....Disable CURVE fields
*/
	flag = taxis->mode == NCLX_MOT_TLAXIS_CURVE;
	if (reentry) flag = vtog[0];
	if (!flag)
	{
		vtog[0] = 0;
		traverse[CRV_GEO] = traverse[CRV_DIS] = 0;
	}
/*
.....Enable CURVE fields
*/
	else
	{
		vtog[0] = 1;
		traverse[CRV_GEO] = traverse[CRV_DIS] = 1;
	}
/*
.....Set Advanced fields
*/
	display[CRV_DIS] = Sadvtog;
}

/*********************************************************************
**    I_FUNCTION     : S_set_curve(taxis,vtog,vgeo,vdis)
**       Defines the field parameters for the Thru Curve Tool Axis fields
**       based on the active tool axis mode.
**    PARAMETERS
**       INPUT  :
**          vtog[0]  = TLAXIS/THRU,curve is in effect.
**          vgeo     = Curve for tool axis control.
**          vdis     = Approximate distance of curve.
**       OUTPUT :
**          taxis    = Current tool axis settings.
**          caxis    = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_curve(taxis,caxis,vtog,vgeo,vdis)
NCLX_mot_tlaxis *taxis;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int vtog[];
char *vgeo,*vdis;
{
	int inum;
	UU_REAL rval;
	taxis->mode = NCLX_MOT_TLAXIS_CURVE;
	strcpy(caxis[0],vgeo);
	caxis[1][0] = '\0';
	if (Sadvtog)
	{
		if (ul_to_reals(&rval,&inum,1,vdis) != UU_SUCCESS || rval != 0.)
			strcpy(caxis[1],vdis);
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_define_interp(taxis,rtog,rgeo,cdeg,crat,
**                                     display,traverse)
**       Sets the tool axis variables based on the field parameters for
**       the Interp Tool Axis fields
**    PARAMETERS
**       INPUT  :
**          taxis    = Current tool axis settings.
**          reentry  = UU_TRUE - Don't initialize fillet settings.
**       OUTPUT :
**          rtog[0]  = TLAXIS/INTERP is in effect.
**          rtog[1]  = Specify smooth fanning parameters
**          rgeo     = Interpolation vector.
**          rdeg     = Degree of polynomial curve for smooth control.
**          rrat     = Rate of change for smooth control.
**          display  = Field display settings.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_interp(taxis,rtog,rgeo,rdeg,rrat,display,traverse,reentry)
NCLX_mot_tlaxis *taxis;
int rtog[];
char *rgeo,*rdeg,*rrat;
char *display,*traverse;
UU_LOGICAL reentry;
{
	int ideg;
	UU_LOGICAL flag;
	UU_REAL rate;
/*
.....Define INTERP settings
*/
	if (!reentry)
	{
		sprintf(rgeo,"%8.5f,%8.5f,%8.5f",taxis->vector[0],taxis->vector[1],
			taxis->vector[2]);
		NclxMotGetFanInterp(&ideg,&rate);
		rtog[1] = 0.;
		if (ideg > 1 && rate != 0.) rtog[1] = 1;
		sprintf(rdeg,"%d",ideg);
		ncl_sprintf(rrat,&rate,1);
	}
/*
.....Disable INTERP fields
*/
	flag = taxis->mode == NCLX_MOT_TLAXIS_INTERP;
	if (reentry) flag = rtog[0];
	if (!flag)
	{
		rtog[0] = 0;
		traverse[INT_GEO] = traverse[INT_SMO] = traverse[INT_DEG] = 0;
		traverse[INT_RAT] = 0;
	}
/*
.....Enable INTERP fields
*/
	else
	{
		rtog[0] = 1;
		traverse[INT_GEO] = traverse[INT_SMO] = 1;
		traverse[INT_DEG] = traverse[INT_RAT] = rtog[1];
	}
/*
.....Set Advanced fields
*/
	display[INT_SMO] = Sadvtog;
	display[INT_DEG] = Sadvtog;
	display[INT_RAT] = Sadvtog;
}

/*********************************************************************
**    I_FUNCTION     : S_set_interp(taxis,caxis,rtog,rgeo,cdeg,crat)
**       Sets the tool axis parameters based on the field parameters for
**       the Interp Tool Axis fields
**    PARAMETERS
**       INPUT  :
**          rtog[0] = TLAXIS/INTERP is in effect.
**          rtog[1] = Specify smooth fanning parameters
**          rgeo    = Interpolation vector.
**          rdeg    = Degree of polynomial curve for smooth control.
**          rrat    = Rate of change for smooth control.
**       OUTPUT :
**          taxis   = Current tool axis settings.
**          caxis   = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_interp(taxis,caxis,rtog,rgeo,rdeg,rrat)
NCLX_mot_tlaxis *taxis;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int rtog[];
char *rgeo,*rdeg,*rrat;
{
/*
.....Define INTERP settings
*/
	taxis->mode = NCLX_MOT_TLAXIS_INTERP;
	strcpy(caxis[0],rgeo);
/*
.....Define Advanced settings
*/
	taxis->normal = 0;
	caxis[1][0] = '\0';
	caxis[2][0] = '\0';
	if (Sadvtog)
	{
		taxis->normal = rtog[1];
		if (taxis->normal == 1)
		{
			strcpy(caxis[1],rdeg);
			strcpy(caxis[2],rrat);
		}
	}
/*
.....End of routine
*/
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_define_tilt(taxis,ttog,tfwd,trgt,display,traverse,
**                       reentry)
**       Defines the field parameters for the Tilt Angle Tool Axis fields
**       based on the active tool axis mode.
**    PARAMETERS
**       INPUT  :
**          taxis    = Current tool axis settings.
**          reentry  = UU_TRUE - Don't initialize fillet settings.
**       OUTPUT :
**          ttog[0]  = TLAXIS/FWD,RIGHT is in effect.
**          tfwd     = Forward tilt angle.
**          trgt     = Right tilt angle.
**          display  = Field display settings.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_tilt(taxis,ttog,tfwd,trgt,display,traverse,reentry)
NCLX_mot_tlaxis *taxis;
int ttog[];
char *tfwd,*trgt;
char *display,*traverse;
UU_LOGICAL reentry;
{
/*
.....Define TILT settings
*/
	if (!reentry)
	{
		ttog[0] = taxis->modify.angle_flag;
		ncl_sprintf(tfwd,&taxis->modify.fwd_angle,1);
		ncl_sprintf(trgt,&taxis->modify.right_angle,1);
	}
	traverse[TLT_FWD] = traverse[TLT_RGT] = ttog[0];
}

/*********************************************************************
**    I_FUNCTION     : S_set_tilt(taxis,caxis,ttog,tfwd,trgt)
**       Sets the tool axis parameters based on the field parameters for
**       the Tilt Angles Tool Axis fields
**    PARAMETERS
**       INPUT  :
**          ttog[0]  = TLAXIS/FWD,RIGHT is in effect.
**          tfwd     = Forward tilt angle.
**          trgt     = Right tilt angle.
**       OUTPUT :
**          taxis    = Current tool axis settings.
**          maxis    = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_tilt(taxis,maxis,ttog,tfwd,trgt)
NCLX_mot_tlaxis *taxis;
char maxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int ttog[];
char *tfwd,*trgt;
{
/*
.....Define TILT settings
*/
	taxis->modify.angle_flag = ttog[0];
	strcpy(maxis[0],tfwd);
	strcpy(maxis[1],trgt);
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_define_guide(taxis,gtog,ggeo,gdis,display,traverse,
                          reentry)
**       Defines the field parameters for the Guide Curve Angle Tool Axis
**       fields based on the active tool axis mode.
**    PARAMETERS
**       INPUT  :
**          taxis    = Current tool axis settings.
**          reentry  = UU_TRUE - Don't initialize fillet settings.
**       OUTPUT :
**          gtog[0]  = TLAXIS/GUIDE,cv is in effect.
**          gtog[1]  = Guide curve contact condition.
**          gtog[2]  = Guide curve contact mode.
**          ggeo     = Guide curve.
**          gdis     = Offset distance.
**          display  = Field display settings.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_guide(taxis,gtog,ggeo,gdis,display,traverse,reentry)
NCLX_mot_tlaxis *taxis;
int gtog[];
char *ggeo,*gdis;
char *display,*traverse;
UU_LOGICAL reentry;
{
/*
.....Define GUIDE settings
*/
	if (!reentry)
	{
		gtog[0] = taxis->modify.guide_flag;
		gtog[1] = taxis->modify.guide_cond + 1;
		gtog[2] = taxis->modify.guide_contact;
		if (gtog[0] == 1)
			ncl_get_label_with_key(taxis->modify.guide->data.header.key,ggeo);
		else
			ggeo[0] = '\0';
		ncl_sprintf(gdis,&taxis->modify.guide_offset,1);
	}
	traverse[GDE_GEO] = traverse[GDE_CTN] = gtog[0];
	traverse[GDE_DIS] = traverse[GDE_CON] = gtog[0];
	if (gtog[1] == 1) traverse[GDE_DIS] = traverse[GDE_CON] = 0;
}

/*********************************************************************
**    I_FUNCTION     : S_set_guide(taxis,caxis,gtog,ggeo,gdis)
**       Sets the tool axis parameters based on the field parameters for
**       the Tilt Angles Tool Axis fields
**    PARAMETERS
**       INPUT  :
**          gtog[0]  = TLAXIS/GUIDE,cv is in effect.
**          gtog[1]  = Guide curve contact condition.
**          gtog[2]  = Guide curve contact mode.
**          ggeo     = Guide curve.
**          gdis     = Offset distance.
**       OUTPUT :
**          taxis    = Current tool axis settings.
**          maxis    = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_guide(taxis,maxis,gtog,ggeo,gdis)
NCLX_mot_tlaxis *taxis;
char maxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int gtog[];
char *ggeo,*gdis;
{
	int nc,status;
	status = UU_SUCCESS;
/*
.....Define GUIDE settings
*/
	taxis->modify.guide_flag = gtog[0];
	taxis->modify.guide_cond = gtog[1];
	taxis->modify.guide_contact = gtog[2];
	strcpy(maxis[2],ggeo);
	strcpy(maxis[3],gdis);
	if (gtog[0] == 1)
	{
		nc = strlen(ggeo);
		ul_strip_blanks(ggeo,&nc);
		if (nc == 0)
		{
			ud_wrerr("You must specify a curve.");
			status = UU_FAILURE;
		}
	}
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : S_define_gougck(taxis,ktog,display,traverse,reentry)
**       Defines the field parameters for the GOUGCK Angle Tool Axis fields
**       based on the active tool axis mode.
**    PARAMETERS
**       INPUT  :
**          taxis    = Current tool axis settings.
**          reentry  = UU_TRUE - Don't initialize fillet settings.
**       OUTPUT :
**          ktog[0]  = TLAXIS/GOUGCK is in effect.
**          display  = Field display settings.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_gougck(taxis,ktog,display,traverse,reentry)
NCLX_mot_tlaxis *taxis;
int ktog[];
char *display,*traverse;
UU_LOGICAL reentry;
{
/*
.....Define TILT settings
*/
	if (!reentry) ktog[0] = taxis->modify.gouge;
}

/*********************************************************************
**    I_FUNCTION     : S_set_gougck(taxis,ktog)
**       Sets the tool axis parameters based on the field parameters for
**       the Tilt Angles Tool Axis fields
**    PARAMETERS
**       INPUT  :
**          ktog[0]  = TLAXIS/GOUGCK is in effect.
**       OUTPUT :
**          taxis    = Current tool axis settings.
**          caxis    = Ignored.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_gougck(taxis,caxis,ktog)
NCLX_mot_tlaxis *taxis;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int ktog[];
{
/*
.....Define GOUGCK settings
*/
	taxis->modify.gouge = ktog[0];
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_define_lock(taxis,ltog,lds1,lds2,display,traverse,
**                       reentry)
**       Defines the field parameters for the Lock Tool Axis fields
**       based on the active tool axis mode.
**    PARAMETERS
**       INPUT  :
**          taxis    = Current tool axis settings.
**          reentry  = UU_TRUE - Don't initialize fillet settings.
**       OUTPUT :
**          ltog[0]  = TLAXIS/LOCK is in effect.
**          ltog[1]  = TLAXIS/LOCK type.
**          ltog[2]  = Lock distance units.
**          ltog[3]  = Enable Transition moves.
**          ltog[4]  = Transition move type.
**          lds1     = Lock distance.
**          lds2     = Transition distance.
**          display  = Field display settings.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_lock(taxis,ltog,lds1,lds2,display,traverse,reentry)
NCLX_mot_tlaxis *taxis;
int ltog[];
char *lds1,*lds2;
char *display,*traverse;
UU_LOGICAL reentry;
{
	int i;
/*
.....Define LOCK settings
*/
	if (!reentry)
	{
		if (taxis->modify.lock_mode == NCLX_LOCK_OFF)
		{
			ltog[0] = 0;
			ltog[1] = 0;
		}
		else
		{
			ltog[0] = 1;
			if (taxis->modify.lock_mode == NCLX_LOCK_OMIT) ltog[1] = 0;
			else if (taxis->modify.lock_mode == NCLX_LOCK_ON) ltog[1] = 1;
			else if (taxis->modify.lock_mode == NCLX_LOCK_END) ltog[1] = 2;
		}
		ltog[2] = taxis->modify.lock_radius;
		ltog[4] = 0;
		if (taxis->modify.lock_interp_dist == 0.) ltog[3] = 0;
		else
		{
			ltog[3] = 1;
			ltog[4] = taxis->modify.lock_transition;
		}
		ncl_sprintf(lds1,&taxis->modify.lock_dist,1);
		ncl_sprintf(lds2,&taxis->modify.lock_interp_dist,1);
	}
/*
.....Set traverse masks
*/
	if (ltog[1] == 0 || ltog[1] == 2)
	{
		for (i=LCK_LIN;i<=LCK_MOV;i++) traverse[i] = 0;
	}
	else
	{
		for (i=LCK_LIN;i<=LCK_TRN;i++) traverse[i] = 1;
		traverse[LCK_FAN] = traverse[LCK_MOV] = ltog[3];
	}
}

/*********************************************************************
**    I_FUNCTION     : S_set_lock(taxis,ltog,lds1,lds2)
**       Sets the field parameters for the Lock Tool Axis fields
**       based on the active tool axis mode.
**    PARAMETERS
**       INPUT  :
**          ltog[0]  = TLAXIS/LOCK is in effect.
**          ltog[1]  = TLAXIS/LOCK type.
**          ltog[2]  = Lock distance units.
**          ltog[3]  = Enable Transition moves.
**          ltog[4]  = Transition move type.
**          lds1     = Lock distance.
**          lds2     = Transition distance.
**       OUTPUT :
**          taxis    = Current tool axis settings.
**          caxis    = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_lock(taxis,caxis,ltog,lds1,lds2)
NCLX_mot_tlaxis *taxis;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int ltog[];
char *lds1,*lds2;
{
	int nc;
/*
.....Define LOCK settings
*/
	if (ltog[0] == 0) taxis->modify.lock_mode = NCLX_LOCK_OFF;
	else
	{
		if (ltog[1] == 0) taxis->modify.lock_mode = NCLX_LOCK_OMIT;
		if (ltog[1] == 1) taxis->modify.lock_mode = NCLX_LOCK_ON;
		if (ltog[1] == 2) taxis->modify.lock_mode = NCLX_LOCK_END;
	}
	taxis->modify.lock_radius = ltog[2];
	taxis->modify.lock_transition = 0;
	if (ltog[3] == 1) taxis->modify.lock_transition = ltog[4];
	nc = strlen(lds1);
	ul_strip_blanks(lds1,&nc);
	if (nc == 0) strcpy(caxis[4],"0.");
	else strcpy(caxis[4],lds1);
	nc = strlen(lds2);
	ul_strip_blanks(lds2,&nc);
	if (nc == 0) strcpy(caxis[5],"0.");
	else strcpy(caxis[5],lds2);
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_define_modify(taxis,ytog,ymod,display,traverse,reentry)
**       Defines the field parameters for the Modify Tool Axis fields
**       based on the active tool axis mode.
**    PARAMETERS
**       INPUT  :
**          taxis    = Current tool axis settings.
**          reentry  = UU_TRUE - Don't initialize fillet settings.
**       OUTPUT :
**          ytog[0]  = TLAXIS/MODIFY is in effect.
**          ymod     = Modify parameters.
**          display  = Field display settings.
**          traverse = Field traversal settings.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_modify(taxis,ytog,ymod,display,traverse,reentry)
NCLX_mot_tlaxis *taxis;
int ytog[];
char ymod[5][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
char *display,*traverse;
UU_LOGICAL reentry;
{
/*
.....Define MODIFY settings
*/
	if (!reentry)
	{
		ytog[0] = taxis->adjust_flag;
		ncl_sprintf(ymod[0],&taxis->adjust.up_offset,1);
		ncl_sprintf(ymod[1],&taxis->adjust.fwd_offset,1);
		ncl_sprintf(ymod[2],&taxis->adjust.right_offset,1);
		ncl_sprintf(ymod[3],&taxis->adjust.fwd_tilt,1);
		ncl_sprintf(ymod[4],&taxis->adjust.right_tilt,1);
	}
	traverse[MOD_UP] = traverse[MOD_RGT] = traverse[MOD_LFT] = ytog[0];
	traverse[MOD_FWD] = traverse[MOD_ANG] = ytog[0];
}

/*********************************************************************
**    I_FUNCTION     : S_set_modify(taxis,ytog,ymod)
**       Sets the field parameters for the Modify Tool Axis fields
**       based on the active tool axis mode.
**    PARAMETERS
**       INPUT  :
**          ytog[0]  = TLAXIS/MODIFY is in effect.
**          ymod     = Modify parameters.
**       OUTPUT :
**          taxis    = Current tool axis settings.
**          caxis    = Textual form responses.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_set_modify(taxis,caxis,ytog,ymod)
NCLX_mot_tlaxis *taxis;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
int ytog[];
char ymod[5][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
{
	int i,nc;
	static int isub[]={8,7,6,10,9};
/*
.....Define MODIFY settings
*/
	taxis->adjust_flag = ytog[0];
	if (ytog[0] == 0)
	{
		strcpy(ymod[0],"0."); strcpy(ymod[1],"0.");
		strcpy(ymod[2],"0."); strcpy(ymod[3],"0.");
		strcpy(ymod[4],"0.");
	}
	else
	{
		for (i=0;i<5;i++)
		{
			nc = strlen(ymod[i]);
			ul_strip_blanks(ymod[i],&nc);
			if (nc == 0) strcpy(ymod[i],"0.");
			strcpy(caxis[isub[i]],ymod[i]);
		}
	}
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_mode(fieldno, val, stat)
**       Handles the tool axis mode toggle fields.
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
static UD_FSTAT S_toggle_mode(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,ival;
	char str[80];
	UD_DDATA data;
	static int mode[]={MODE_FIXED, MODE_TANTO, MODE_FAN, MODE_COMBIN,
		MODE_NORMAL, MODE_ATANGL, MODE_POINT, MODE_CURVE, MODE_INTERP};
	int NMODES = sizeof(mode) / sizeof(int);
/*
.....Disable all Tool axis modes
.....except for current selection
*/
	ud_default_method(fieldno, val, stat);
	data.frmstr = str;

	for (i=0;i<NMODES;i++)
	{
		if (mode[i] == *fieldno)
		{
			S_toggle_page(mode[i],UU_TRUE);
			ival = 1;
		}
		else
		{
			ud_getfrm_field(Sfrm1,mode[i],data,UU_FALSE);
			if (data.frmint[0] == 1) S_toggle_page(mode[i],UU_FALSE);
			ival = 0;
		}
		ud_dispfrm_update_answer(Sfrm1,mode[i],&ival);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_option(fieldno, val, stat)
**       Handles the tool axis options toggle fields.
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
static UD_FSTAT S_toggle_option(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int inc,ist,ien,i,itog;
	UD_DDATA data;
/*
.....Change status of requested tool axis option section
*/
	ud_default_method(fieldno, val, stat);
	itog = val->frmint[0];
	switch (*fieldno)
	{
	case MODE_TILT:
		ist = TLT_FWD; ien = TLT_RGT;
		inc = 0;
		break;
	case MODE_GUIDE:
		ist = GDE_GEO; ien = GDE_CON;
		if (itog == 1)
		{
			data.frmint = &i;
			ud_getfrm_field(Sfrm1,GDE_CTN,data,UU_FALSE);
			if (data.frmint[0] == 1)
			{
				ien = GDE_CTN;
				ud_setfrm_traverse_mask(Sfrm1,GDE_DIS,UU_FALSE);
				ud_setfrm_traverse_mask(Sfrm1,GDE_CON,UU_FALSE);
			}
		}
		inc = 1;
		break;
	case MODE_GOUGE:
		ist = 0; ien = -1;
		inc = 2;
		break;
	case MODE_LOCK:
		ist = LCK_TYP; ien = LCK_MOV;
		if (itog == 1)
		{
			
			data.frmint = &i;
			ud_getfrm_field(Sfrm1,LCK_TYP,data,UU_FALSE);
			if (data.frmint[0] == 2)
			{
				ist = LCK_LIN;
				itog = 0;
			}
			else
			{
				data.frmint = &i;
				ud_getfrm_field(Sfrm1,LCK_TRN,data,UU_FALSE);
				if (data.frmint[0] == 0)
				{
					ien = LCK_TRN;
					ud_setfrm_traverse_mask(Sfrm1,LCK_FAN,UU_FALSE);
					ud_setfrm_traverse_mask(Sfrm1,LCK_MOV,UU_FALSE);
				}
			}
		}
		inc = 3;
		break;
	case MODE_MODIFY:
		ist = MOD_UP; ien = MOD_ANG;
		inc = 4;
		break;
	}
/*
.....Set proper section color
*/
	if (itog == 0)
		ud_form_section_color(Sfrm1,Soption[inc],Sorange,UU_FALSE);
	else
		ud_form_section_color(Sfrm1,Soption[inc],Sorange,UU_TRUE);
/*
.....Set traversal fields
*/
	for (i=ist;i<=ien;i++) ud_setfrm_traverse_mask(Sfrm1,i,val->frmint[0]);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_fixed(fieldno, val, stat)
**       Handles the FIXED toggle fields.
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
static UD_FSTAT S_toggle_fixed(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char lvec[80];
	UM_vector vector;
	UV_vport vport;
	UV_view view;

	ud_default_method(fieldno, val, stat);

	switch(*fieldno)
	{
	case FXD_MODE: 
		switch (val->frmint[0])
		{
		case 0:
			ud_setfrm_traverse_mask(Sfrm1,FXD_VEC,UU_FALSE);
			break;
		case 1:
			ud_setfrm_traverse_mask(Sfrm1,FXD_VEC,UU_TRUE);
			break;
		case 2:
			if (UV_act_screen[0].nvports > 1) ud_form_invis();
			if (uvu_pickvp(&vport) != UU_SUCCESS) return(UU_FAILURE);
			if (UV_act_screen[0].nvports > 1) ud_form_vis();
			uv_getvid(vport.cur_view,&view);
			ncl_wcstomcs(1,view.cur_pln_norm,vector);
			ncl_sprintf(lvec,vector,3);
			ud_dispfrm_update_answer(Sfrm1,FXD_VEC,lvec);
			ud_setfrm_traverse_mask(Sfrm1,FXD_VEC,UU_FALSE);
			break;
		}
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_tanto(fieldno, val, stat)
**       Handles the TANTO toggle fields.
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
static UD_FSTAT S_toggle_tanto(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char str[80];
	UD_DDATA data;

	ud_default_method(fieldno, val, stat);

	switch(*fieldno)
	{
	case TAN_PARL:
		ud_setfrm_traverse_mask(Sfrm1,TAN_SURF,!val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,TAN_4AX,!val->frmint[0]);
		if (val->frmint[0] == 1)
		{
			ud_setfrm_traverse_mask(Sfrm1,TAN_VEC,0);
			ud_setfrm_traverse_mask(Sfrm1,TAN_MNT,0);
		}
		else
		{
			data.frmstr = str;
			ud_getfrm_field(Sfrm1,TAN_4AX,data,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,TAN_VEC,data.frmint[0]);
			ud_setfrm_traverse_mask(Sfrm1,TAN_MNT,data.frmint[0]);
		}
		break;
	case TAN_4AX: 
		ud_setfrm_traverse_mask(Sfrm1,TAN_VEC,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,TAN_MNT,val->frmint[0]);
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_fan(fieldno, val, stat)
**       Handles the FAN toggle fields.
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
static UD_FSTAT S_toggle_fan(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char str[80];
	UD_DDATA data;

	ud_default_method(fieldno, val, stat);

	switch(*fieldno)
	{
	case FAN_SMO: 
		ud_setfrm_traverse_mask(Sfrm1,FAN_DEG,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,FAN_RAT,val->frmint[0]);
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_combin(fieldno, val, stat)
**       Handles the COMBIN toggle fields.
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
static UD_FSTAT S_toggle_combin(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char str[80];
	UD_DDATA data;

	ud_default_method(fieldno, val, stat);

	switch(*fieldno)
	{
	case COM_PARL:
		ud_setfrm_traverse_mask(COM_SURF,!val->frmint[0]);
		break;
	case COM_SMO: 
		ud_setfrm_traverse_mask(Sfrm1,COM_DEG,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,COM_RAT,val->frmint[0]);
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_normal(fieldno, val, stat)
**       Handles the NORMAL toggle fields.
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
static UD_FSTAT S_toggle_normal(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	ud_default_method(fieldno, val, stat);

	switch(*fieldno)
	{
	case NOR_4AX: 
		ud_setfrm_traverse_mask(Sfrm1,NOR_VEC,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,NOR_MNT,val->frmint[0]);
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_atangl(fieldno, val, stat)
**       Handles the ATANGL toggle fields.
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
static UD_FSTAT S_toggle_atangl(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char str[80];
	UD_DDATA data;

	ud_default_method(fieldno, val, stat);

	switch(*fieldno)
	{
	case ATA_4AX: 
		ud_setfrm_traverse_mask(Sfrm1,ATA_VEC,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,ATA_MNT,val->frmint[0]);
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_interp(fieldno, val, stat)
**       Handles the INTERP toggle fields.
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
static UD_FSTAT S_toggle_interp(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char str[80];
	UD_DDATA data;

	ud_default_method(fieldno, val, stat);

	switch(*fieldno)
	{
	case INT_SMO: 
		ud_setfrm_traverse_mask(Sfrm1,INT_DEG,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,INT_RAT,val->frmint[0]);
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_guide(fieldno, val, stat)
**       Handles the GUIDE,CV toggle fields.
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
static UD_FSTAT S_toggle_guide(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	ud_default_method(fieldno, val, stat);

	switch(*fieldno)
	{
	case GDE_CTN: 
		if (val->frmint[0] == 1)
		{
			ud_setfrm_traverse_mask(Sfrm1,GDE_DIS,0);
			ud_setfrm_traverse_mask(Sfrm1,GDE_CON,0);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm1,GDE_DIS,1);
			ud_setfrm_traverse_mask(Sfrm1,GDE_CON,1);
		}
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_lock(fieldno, val, stat)
**       Handles the LOCK toggle fields.
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
static UD_FSTAT S_toggle_lock(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	UU_LOGICAL ifl;
	UD_DDATA data;
	ud_default_method(fieldno, val, stat);

	switch(*fieldno)
	{
	case LCK_TYP: 
		ifl = UU_TRUE;
		if (val->frmint[0] == 2) ifl = UU_FALSE;
		for (i=LCK_LIN;i<=LCK_TRN;i++) ud_setfrm_traverse_mask(Sfrm1,i,ifl);
		if (ifl)
		{
			data.frmint = &i;
			ud_getfrm_field(Sfrm1,LCK_TRN,data,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,LCK_FAN,data.frmint[0]);
			ud_setfrm_traverse_mask(Sfrm1,LCK_MOV,data.frmint[0]);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm1,LCK_FAN,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,LCK_MOV,UU_FALSE);
		}
		break;
	case LCK_TRN:
		ud_setfrm_traverse_mask(Sfrm1,LCK_FAN,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,LCK_MOV,val->frmint[0]);
		break;
	}
	return(stat);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_advanced(fieldno, val, stat)
**       Handles the ADVANCED toggle field.
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
static UD_FSTAT S_toggle_advanced(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,ival;
	static int adv[]={FXD_NORM, TAN_SURF,TAN_4AX,TAN_VEC,TAN_MNT, FAN_CEN,
		FAN_SMO,FAN_DEG,FAN_RAT,COM_SURF,COM_CEN,COM_SMO,COM_DEG,COM_RAT,
		NOR_SURF,NOR_4AX,NOR_VEC,NOR_MNT, ATA_SURF,ATA_CNT,ATA_4AX,ATA_VEC,
		ATA_MNT, CRV_DIS, INT_SMO,INT_DEG,INT_RAT};
	int NADV=sizeof(adv) / sizeof(int);
/*
.....Set advanced field display
*/
	ud_default_method(fieldno, val, stat);

	for (i=0;i<NADV;i++)
		ud_setfrm_display_mask(Sfrm1,UD_INPUTF,adv[i],val->frmint[0]);
	return(stat);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_page(fieldno, flag)
**       Disables/Enables a tool axis mode page.
**    PARAMETERS
**       INPUT  :
**          fieldno = Tool axis mode form field to disable/enable.
**          flag    = UU_FALSE = disable page, UU_TRUE = enable page.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_toggle_page(fieldno, flag)
int fieldno;
UU_LOGICAL flag;
{
	int i,ist,ien;
	char str[80];
	UD_DDATA data;
/*
.....Disable requested page
*/
	if (!flag)
	{
		switch (fieldno)
		{
		case MODE_FIXED:
			ist = FXD_MODE; ien = FXD_NORM;
			ud_form_section_color(Sfrm1,Smode[Fixed],Sblack,UU_FALSE);
			S_modify_pages(UU_TRUE);
			break;
		case MODE_TANTO:
			ist = TAN_HGT; ien = TAN_MNT;
			ud_form_section_color(Sfrm1,Smode[Tanto],Sblack,UU_FALSE);
			break;
		case MODE_FAN:
			ist = FAN_HGT; ien = FAN_RAT;
			ud_form_section_color(Sfrm1,Smode[Fan],Sblack,UU_FALSE);
			break;
		case MODE_COMBIN:
			ist = COM_HGT; ien = COM_RAT;
			ud_form_section_color(Sfrm1,Smode[Combin],Sblack,UU_FALSE);
			break;
		case MODE_NORMAL:
			ist = NOR_SURF; ien = NOR_MNT;
			ud_form_section_color(Sfrm1,Smode[Normal],Sblack,UU_FALSE);
			break;
		case MODE_ATANGL:
			ist = ATA_ANG; ien = ATA_MNT;
			ud_form_section_color(Sfrm1,Smode[Atangl],Sblack,UU_FALSE);
			break;
		case MODE_POINT:
			ist = PNT_GEO; ien = PNT_GEO;
			ud_form_section_color(Sfrm1,Smode[Point],Sblack,UU_FALSE);
			break;
		case MODE_CURVE:
			ist = CRV_GEO; ien = CRV_DIS;
			ud_form_section_color(Sfrm1,Smode[Curve],Sblack,UU_FALSE);
			break;
		case MODE_INTERP:
			ist = INT_GEO; ien = INT_RAT;
			ud_form_section_color(Sfrm1,Smode[Interp],Sblack,UU_FALSE);
			break;
		}
		for (i=ist;i<=ien;i++)
			ud_setfrm_traverse_mask(Sfrm1,i,UU_FALSE);
	}

/*
.....Enable requested page
*/
	else
	{
		data.frmstr = str;
		switch (fieldno)
		{
/*
........FIXED page
*/
		case MODE_FIXED:
			ud_setfrm_traverse_mask(Sfrm1,FXD_MODE,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,FXD_NORM,UU_TRUE);
			ud_getfrm_field(Sfrm1,FXD_MODE,data,UU_FALSE);
			if (data.frmint[0] == 0 || data.frmint[0] == 2)
				ud_setfrm_traverse_mask(Sfrm1,FXD_VEC,UU_FALSE);
			else
				ud_setfrm_traverse_mask(Sfrm1,FXD_VEC,UU_TRUE);
			ud_form_section_color(Sfrm1,Smode[Fixed],Sgreen,UU_TRUE);
			S_modify_pages(UU_FALSE);
			break;
/*
........TANTO page
*/
		case MODE_TANTO:
			ud_setfrm_traverse_mask(Sfrm1,TAN_HGT,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,TAN_PARL,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,TAN_SURF,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,TAN_4AX,UU_TRUE);
			ud_getfrm_field(Sfrm1,TAN_4AX,data,UU_FALSE);
			if (data.frmint[0] == 1)
			{
				ud_setfrm_traverse_mask(Sfrm1,TAN_VEC,UU_TRUE);
				ud_setfrm_traverse_mask(Sfrm1,TAN_MNT,UU_TRUE);
			}
			else
			{
				ud_setfrm_traverse_mask(Sfrm1,TAN_VEC,UU_FALSE);
				ud_setfrm_traverse_mask(Sfrm1,TAN_MNT,UU_FALSE);
			}
			ud_form_section_color(Sfrm1,Smode[Tanto],Sgreen,UU_TRUE);
			break;
/*
........FAN page
*/
		case MODE_FAN:
			ud_setfrm_traverse_mask(Sfrm1,FAN_HGT,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,FAN_CEN,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,FAN_SMO,UU_TRUE);
			ud_getfrm_field(Sfrm1,FAN_SMO,data,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,FAN_DEG,data.frmint[0]);
			ud_setfrm_traverse_mask(Sfrm1,FAN_RAT,data.frmint[0]);
			ud_form_section_color(Sfrm1,Smode[Fan],Sgreen,UU_TRUE);
			break;
/*
........COMBIN page
*/
		case MODE_COMBIN:
			ud_setfrm_traverse_mask(Sfrm1,COM_HGT,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,COM_PARL,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,COM_LVD,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,COM_APD,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,COM_SURF,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,COM_CEN,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,COM_SMO,UU_TRUE);
			ud_getfrm_field(Sfrm1,COM_SMO,data,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,COM_DEG,data.frmint[0]);
			ud_setfrm_traverse_mask(Sfrm1,COM_RAT,data.frmint[0]);
			ud_form_section_color(Sfrm1,Smode[Combin],Sgreen,UU_TRUE);
			break;
/*
........NORMAL page
*/
		case MODE_NORMAL:
			ud_setfrm_traverse_mask(Sfrm1,NOR_SURF,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,NOR_4AX,UU_TRUE);
			ud_getfrm_field(Sfrm1,NOR_4AX,data,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,NOR_VEC,data.frmint[0]);
			ud_setfrm_traverse_mask(Sfrm1,NOR_MNT,data.frmint[0]);
			ud_form_section_color(Sfrm1,Smode[Normal],Sgreen,UU_TRUE);
			break;
/*
........ATANGL page
*/
		case MODE_ATANGL:
			ud_setfrm_traverse_mask(Sfrm1,ATA_ANG,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,ATA_CLD,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,ATA_SURF,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,ATA_CNT,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,ATA_4AX,UU_TRUE);
			ud_getfrm_field(Sfrm1,TAN_4AX,data,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,ATA_VEC,data.frmint[0]);
			ud_setfrm_traverse_mask(Sfrm1,ATA_MNT,data.frmint[0]);
			ud_form_section_color(Sfrm1,Smode[Atangl],Sgreen,UU_TRUE);
			break;
/*
........POINT page
*/
		case MODE_POINT:
			ud_setfrm_traverse_mask(Sfrm1,PNT_GEO,UU_TRUE);
			ud_form_section_color(Sfrm1,Smode[Point],Sgreen,UU_TRUE);
			break;
/*
........CURVE page
*/
		case MODE_CURVE:
			ud_setfrm_traverse_mask(CRV_GEO,UU_TRUE);
			ud_setfrm_traverse_mask(CRV_DIS,UU_TRUE);
			ud_form_section_color(Sfrm1,Smode[Curve],Sgreen,UU_TRUE);
			break;
/*
........INTERP page
*/
		case MODE_INTERP:
			ud_setfrm_traverse_mask(Sfrm1,INT_GEO,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrm1,INT_SMO,UU_TRUE);
			ud_getfrm_field(INT_SMO,data,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm1,INT_DEG,data.frmint[0]);
			ud_setfrm_traverse_mask(Sfrm1,INT_RAT,data.frmint[0]);
			ud_form_section_color(Sfrm1, Smode[Interp],Sgreen,UU_TRUE);
			break;
		}
	}
}

/*********************************************************************
**    I_FUNCTION     : S_modify_pages(flag)
**       Disables/Enables the tool axis modifier pages.
**    PARAMETERS
**       INPUT  :
**          flag    = UU_FALSE = disable pages, UU_TRUE = enable pages.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_modify_pages(flag)
UU_LOGICAL flag;
{
	int i;
	for (i=0;i<NOPT-1;i++) ud_form_section_enable(Sfrm1, Soption[i],flag);
}
