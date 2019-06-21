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

#define LABEL_NUM 9
enum
{
	FXD_MODE,
	FXD_VEC,
	FXD_NORM,
	FMODE_MODIFY,
	FMOD_UP,
	FMOD_RGT,
	FMOD_LFT,
	FMOD_FWD,
	FMOD_ANG,
	FXD_VIDEO,

	TAN_HGT,
	TAN_PARL,
	TAN_SURF,
	TAN_4AX,
	TAN_VEC,
	TAN_MNT,
	TAN_VIDEO,

	FAN_HGT,
	FAN_CEN,
	FAN_SMO,
	FAN_DEG,
	FAN_RAT,
	FAN_VIDEO,

	COM_HGT,
	COM_PARL,
	COM_LVD,
	COM_APD,
	COM_SURF,
	COM_CEN,
	COM_SMO,
	COM_DEG,
	COM_RAT,
	COM_VIDEO,

	NOR_SURF,
	NOR_4AX,
	NOR_VEC,
	NOR_MNT,
	NOR_VIDEO,

	ATA_ANG,
	ATA_CLD,
	ATA_SURF,
	ATA_CNT,
	ATA_4AX,
	ATA_VEC,
	ATA_MNT,
	ATA_VIDEO,

	PNT_GEO,
	PNT_VIDEO,

	CRV_GEO,
	CRV_DIS,
	CRV_VIDEO,

	INT_GEO,
	INT_SMO,
	INT_DEG,
	INT_RAT,
	INT_VIDEO,
	TAX_MODY,
	TAX_MODYB,
	TAX_FCO
};
enum
{
	MODE_TILT,
	TLT_FWD,
	TLT_RGT,
	TLT_VIDEO,

	MODE_GUIDE,
	GDE_GEO,
	GDE_CTN,
	GDE_DIS,
	GDE_CON,
	GDE_VIDEO,

	MODE_GOUGE,
	GOUGE_VIDEO,

	MODE_LOCK,
	LCK_TYP,
	LCK_LIN,
	LCK_DIS,
	LCK_TRN,
	LCK_FAN,
	LCK_MOV,
	LCK_VIDEO,

	MODE_MODIFY,
	MOD_UP,
	MOD_RGT,
	MOD_LFT,
	MOD_FWD,
	MOD_ANG,
	MOD_VIDEO,
};

extern UD_METHOD UD_initfrm_intry;
extern UD_METHOD UD_form_secsel;

static int S_option_first = 1; 
UU_LOGICAL S_close_enable=1,Sreentry = 0;
static int S_act_page = 0, S_focus_page = 0;
static int S_focus_page2 = 0;
static int Sfrm1=-1, Sfrm2 = -1;
static UU_LOGICAL Sactive1 = UU_FALSE,Sfixed;
static UU_LOGICAL Sactive2 = UU_FALSE;
static NCLX_mot_tlaxis Staxis;
static char Smaxis[12][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char Scaxis[6][NCL_MAX_LABEL_AND_SUBSCRIPT+1];

static NCLX_mot_tlaxis Sav_taxis;
static NCLX_mot_tlaxis T_taxis1, T_taxis2, T_taxis3, T_taxis4, T_taxis5, T_taxis6, T_taxis7, T_taxis8;
static char Sav_maxis[12][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char T_maxis1[12][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char T_maxis2[12][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char T_maxis3[12][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char T_maxis4[12][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char T_maxis5[12][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char T_maxis6[12][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char T_maxis7[12][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
static char T_maxis8[12][NCL_MAX_LABEL_AND_SUBSCRIPT+1];

static char Sav_caxis[6][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
enum {Fixed, Tanto, Fan, Combin, Normal, Atangl, Point, Curve, Interp};
	
#define NOPT 5

static int Sblack[3]={0,0,0}, Sgreen[3]={0,180,0};
static int Sred[3]={210,0,0}, Syellow[3]={180,180,0}, Sorange[3]={180,116,0};
static char *Smode[]={"Fixed", "Tanto DS", "Fan", "Combine", "Normal PS",
	"Atangle PS", "Thru Point", "Thru Curve", "Interpolate"};
static char *Soption[5]={"Tilt Angles", "Guide Curve", "Gouge Check",
	"Lock Tlaxis","Modify"};

static int Sftog[3],Sdtog[4],Sntog[3],Sctog[4],Smtog[3],Satog[4],Sptog[2],Svtog[2];
static int Srtog[2],Sttog[2],Sgtog[3],Sktog[2],Sltog[5],Sytog[2];

static int T_ttog[8], T_gtog[8][3], T_ktog[8], T_ltog[8][5], T_ytog[8];

static int Fytog[2];
static char Fymod[5][NCL_MAX_LABEL_AND_SUBSCRIPT+1];

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
static int STlx_mod=0, STlx_output=0;
static int Save_mod[8] = {0,0,0,0,0,0,0,0}, Save_Fytog;

int nclu_tlaxis_command();

static UD_FSTAT S_init_form();
static UD_FSTAT S_form_secsel();
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
static UD_FSTAT S_OnModify(), S_OnModifyBut();
//static UD_FSTAT S_toggle_mode();
static UD_FSTAT S_toggle_option();
static UD_FSTAT S_toggle_fixed(), S_toggle_fixed_mod();
static UD_FSTAT S_toggle_tanto();
static UD_FSTAT S_toggle_fan();
static UD_FSTAT S_toggle_combin();
static UD_FSTAT S_toggle_normal();
static UD_FSTAT S_toggle_atangl();
static UD_FSTAT S_toggle_interp();
static UD_FSTAT S_toggle_guide();
static UD_FSTAT S_toggle_lock();
static UD_FSTAT OnClose(),S_video_play(), S_toggle_main(), S_video_play2();
static UD_FSTAT S_edit_main(), S_edit_main_mod();
static void S_init_otions();


static void S_copy_tlaxis(taxis1, taxis2)
NCLX_mot_tlaxis *taxis1, *taxis2;
{	
	taxis2->mode = taxis1->mode;
	taxis2->normal = taxis1->normal;
	taxis2->vector[0] = taxis1->vector[0];
	taxis2->vector[1] = taxis1->vector[1];
	taxis2->vector[2] = taxis1->vector[2];
	taxis2->perpto_flag = taxis1->perpto_flag;
	taxis2->perpto[0] = taxis1->perpto[0];
	taxis2->perpto[1] = taxis1->perpto[1];
	taxis2->perpto[2] = taxis1->perpto[2];
	taxis2->angle = taxis1->angle;
	taxis2->contact = taxis1->contact;
	taxis2->heel = taxis1->heel;
	taxis2->height = taxis1->height;
	taxis2->cmb_depart = taxis1->cmb_depart;
	taxis2->cmb_approach = taxis1->cmb_approach;
	taxis2->parelm = taxis1->parelm;
	taxis2->point[0] = taxis1->point[0];
	taxis2->point[1] = taxis1->point[1];
	taxis2->point[2] = taxis1->point[2];
	taxis2->curve = taxis1->curve;
	taxis2->curve_dist = taxis1->curve_dist;
	taxis2->adjust_flag = taxis1->adjust_flag;
	taxis2->center = taxis1->center;

	taxis2->modify.angle_flag = taxis1->modify.angle_flag;
	taxis2->modify.right_angle = taxis1->modify.right_angle;
	taxis2->modify.fwd_angle = taxis1->modify.fwd_angle;
	taxis2->modify.guide_flag = taxis1->modify.guide_flag;
	taxis2->modify.guide = taxis1->modify.guide;
	taxis2->modify.guide_contact = taxis1->modify.guide_contact;
	taxis2->modify.guide_cond = taxis1->modify.guide_cond;
	taxis2->modify.guide_offset = taxis1->modify.guide_offset;
	taxis2->modify.secps_flag = taxis1->modify.secps_flag;
	taxis2->modify.secps = taxis1->modify.secps;
	taxis2->modify.gouge = taxis1->modify.gouge;
	taxis2->modify.lock_mode = taxis1->modify.lock_mode;
	taxis2->modify.lock_transition = taxis1->modify.lock_transition;
	taxis2->modify.lock_radius = taxis1->modify.lock_radius;
	taxis2->modify.lock_dist = taxis1->modify.lock_dist;
	taxis2->modify.lock_interp_dist = taxis1->modify.lock_interp_dist;

	taxis2->adjust.right_offset = taxis1->adjust.right_offset;
	taxis2->adjust.fwd_offset = taxis1->adjust.fwd_offset;
	taxis2->adjust.up_offset = taxis1->adjust.up_offset;
	taxis2->adjust.right_tilt = taxis1->adjust.right_tilt;
	taxis2->adjust.fwd_tilt = taxis1->adjust.fwd_tilt;
}

static void S_copy_tlaxis2(taxis1, taxis2)
NCLX_mot_tlaxis *taxis1, *taxis2;
{	
	taxis2->modify.angle_flag = taxis1->modify.angle_flag;
	taxis2->modify.guide_flag = taxis1->modify.guide_flag;
	taxis2->modify.guide_contact = taxis1->modify.guide_contact;
	taxis2->modify.guide_cond = taxis1->modify.guide_cond;
	taxis2->modify.gouge = taxis1->modify.gouge;
	taxis2->modify.lock_mode = taxis1->modify.lock_mode;
	taxis2->modify.lock_transition = taxis1->modify.lock_transition;
	taxis2->modify.lock_radius = taxis1->modify.lock_radius;
	taxis2->adjust_flag = taxis1->adjust_flag;
	taxis2->adjust.right_offset = taxis1->adjust.right_offset;
	taxis2->adjust.fwd_offset = taxis1->adjust.fwd_offset;
	taxis2->adjust.up_offset = taxis1->adjust.up_offset;
	taxis2->adjust.right_tilt = taxis1->adjust.right_tilt;
	taxis2->adjust.fwd_tilt = taxis1->adjust.fwd_tilt;
}

void S_save_axis()
{
	int i, nc;

	nc = (int)strlen(Stfwd);
	ul_strip_blanks(Stfwd, &nc);
	strcpy(Sav_maxis[0],Stfwd);
	nc = (int)strlen(Strgt);
	ul_strip_blanks(Strgt, &nc);
	strcpy(Sav_maxis[1],Strgt);
	nc = (int)strlen(Sggeo);
	ul_strip_blanks(Sggeo, &nc);
	strcpy(Sav_maxis[2],Sggeo);
	nc = (int)strlen(Sgdis);
	ul_strip_blanks(Sgdis, &nc);
	strcpy(Sav_maxis[3],Sgdis);

	nc = strlen(Slds1);
	ul_strip_blanks(Slds1,&nc);
	if (nc == 0) strcpy(Sav_maxis[4],"0.");
	else strcpy(Sav_maxis[4],Slds1);
	nc = strlen(Slds2);
	ul_strip_blanks(Slds2,&nc);
	if (nc == 0) strcpy(Sav_maxis[5],"0.");
	else strcpy(Sav_maxis[5],Slds2);

	ncl_sprintf(Sav_maxis[8],&Staxis.adjust.up_offset,1);
	ncl_sprintf(Sav_maxis[7],&Staxis.adjust.fwd_offset,1);
	ncl_sprintf(Sav_maxis[6],&Staxis.adjust.right_offset,1);
	ncl_sprintf(Sav_maxis[10],&Staxis.adjust.fwd_tilt,1);
	ncl_sprintf(Sav_maxis[9],&Staxis.adjust.right_tilt,1);

	for (i=0;i<12;i++)
	{
		if (Sav_maxis[i][0]!='\0')
		{
			strcpy(T_maxis1[i], Sav_maxis[i]);
			strcpy(T_maxis2[i], Sav_maxis[i]);
			strcpy(T_maxis3[i], Sav_maxis[i]);
			strcpy(T_maxis4[i], Sav_maxis[i]);
			strcpy(T_maxis5[i], Sav_maxis[i]);
			strcpy(T_maxis6[i], Sav_maxis[i]);
			strcpy(T_maxis7[i], Sav_maxis[i]);
			strcpy(T_maxis8[i], Sav_maxis[i]);
		}
	}
	for (i=0;i<8;i++)
	{
		T_ttog[i] = Sttog[0];
		T_gtog[i][0] = Sgtog[0];
		T_gtog[i][1] = Sgtog[1];
		T_gtog[i][2] = Sgtog[2];
		T_ktog[i] = Sktog[0];
		T_ltog[i][0] = Sltog[0];
		T_ltog[i][1] = Sltog[1];
		T_ltog[i][2] = Sltog[2];
		T_ltog[i][3] = Sltog[3];
		T_ltog[i][4] = Sltog[4];
		T_ytog[i] = Sytog[0];
	}
	if (Staxis.mode==NCLX_MOT_TLAXIS_FIXED)
	{
		nc = (int)strlen(Sfvec);
		ul_strip_blanks(Sfvec, &nc);
		strcpy(Sav_caxis[0],Sfvec);
	}
	else if (Staxis.mode==NCLX_MOT_TLAXIS_TANTO)
	{
		nc = (int)strlen(Sdhgt);
		ul_strip_blanks(Sdhgt, &nc);
		strcpy(Sav_caxis[0],Sdhgt);
		nc = (int)strlen(Sdsurf);
		ul_strip_blanks(Sdsurf, &nc);
		strcpy(Sav_caxis[1],Sdsurf);
		nc = (int)strlen(Sdvec);
		ul_strip_blanks(Sdvec, &nc);
		strcpy(Sav_caxis[2],Sdvec);
		for (i=0;i<12;i++)
		{
			if (Sav_maxis[i][0]!='\0')
			{
				strcpy(T_maxis1[i], Sav_maxis[i]);
			}
		}
	}
	else if (Staxis.mode==NCLX_MOT_TLAXIS_FAN)
	{
		nc = (int)strlen(Snhgt);
		ul_strip_blanks(Snhgt, &nc);
		strcpy(Sav_caxis[0],Snhgt);
		nc = (int)strlen(Sndeg);
		ul_strip_blanks(Sndeg, &nc);
		strcpy(Sav_caxis[1],Sndeg);
		nc = (int)strlen(Snrat);
		ul_strip_blanks(Snrat, &nc);
		strcpy(Sav_caxis[2],Snrat);
		for (i=0;i<12;i++)
		{
			if (Sav_maxis[i][0]!='\0')
			{
				strcpy(T_maxis2[i], Sav_maxis[i]);
			}
		}
	}
	else if (Staxis.mode==NCLX_MOT_TLAXIS_COMBIN)
	{
		nc = (int)strlen(Schgt);
		ul_strip_blanks(Schgt, &nc);
		strcpy(Sav_caxis[0],Schgt);
		nc = (int)strlen(Sclvd);
		ul_strip_blanks(Sclvd, &nc);
		strcpy(Sav_caxis[1],Sclvd);
		nc = (int)strlen(Scapd);
		ul_strip_blanks(Scapd, &nc);
		strcpy(Sav_caxis[2],Scapd);
		nc = (int)strlen(Scsurf);
		ul_strip_blanks(Scsurf, &nc);
		strcpy(Sav_caxis[3],Scsurf);
		nc = (int)strlen(Scdeg);
		ul_strip_blanks(Scdeg, &nc);
		strcpy(Sav_caxis[4],Scdeg);
		nc = (int)strlen(Scrat);
		ul_strip_blanks(Scrat, &nc);
		strcpy(Sav_caxis[5],Scrat);
		for (i=0;i<12;i++)
		{
			if (Sav_maxis[i][0]!='\0')
			{
				strcpy(T_maxis3[i], Sav_maxis[i]);
			}
		}
	}
	else if (Staxis.mode==NCLX_MOT_TLAXIS_NORMAL)
	{
		nc = (int)strlen(Smsurf);
		ul_strip_blanks(Smsurf, &nc);
		strcpy(Sav_caxis[0],Smsurf);
		nc = (int)strlen(Smvec);
		ul_strip_blanks(Smvec, &nc);
		strcpy(Sav_caxis[1],Smvec);
		for (i=0;i<12;i++)
		{
			if (Sav_maxis[i][0]!='\0')
			{
				strcpy(T_maxis4[i], Sav_maxis[i]);
			}
		}
	}
	else if (Staxis.mode==NCLX_MOT_TLAXIS_ATANGL)
	{
		nc = (int)strlen(Saang);
		ul_strip_blanks(Saang, &nc);
		strcpy(Sav_caxis[0],Saang);
		nc = (int)strlen(Sadis);
		ul_strip_blanks(Sadis, &nc);
		strcpy(Sav_caxis[1],Sadis);
		nc = (int)strlen(Sasurf);
		ul_strip_blanks(Sasurf, &nc);
		if (nc>0)
			strcpy(Sav_caxis[2],Sasurf);
		else
			Sav_caxis[2][0] = '\0';
		nc = (int)strlen(Savec);
		ul_strip_blanks(Savec, &nc);
		strcpy(Sav_caxis[3],Savec);
		for (i=0;i<12;i++)
		{
			if (Sav_maxis[i][0]!='\0')
			{
				strcpy(T_maxis5[i], Sav_maxis[i]);
			}
		}
	}
	else if (Staxis.mode==NCLX_MOT_TLAXIS_POINT)
	{
		nc = (int)strlen(Spgeo);
		ul_strip_blanks(Spgeo, &nc);
		strcpy(Sav_caxis[0],Spgeo);
		for (i=0;i<12;i++)
		{
			if (Sav_maxis[i][0]!='\0')
			{
				strcpy(T_maxis6[i], Sav_maxis[i]);
			}
		}
	}
	else if (Staxis.mode==NCLX_MOT_TLAXIS_CURVE)
	{
		nc = (int)strlen(Svgeo);
		ul_strip_blanks(Svgeo, &nc);
		strcpy(Sav_caxis[0],Svgeo);
		nc = (int)strlen(Svdis);
		ul_strip_blanks(Svdis, &nc);
		strcpy(Sav_caxis[1],Svdis);
		for (i=0;i<12;i++)
		{
			if (Sav_maxis[i][0]!='\0')
			{
				strcpy(T_maxis7[i], Sav_maxis[i]);
			}
		}
	}
	else if (Staxis.mode==NCLX_MOT_TLAXIS_INTERP)
	{
		nc = (int)strlen(Srgeo);
		ul_strip_blanks(Srgeo, &nc);
		strcpy(Sav_caxis[0],Srgeo);
		nc = (int)strlen(Srdeg);
		ul_strip_blanks(Srdeg, &nc);
		strcpy(Sav_caxis[1],Srdeg);
		nc = (int)strlen(Srrat);
		ul_strip_blanks(Srrat, &nc);
		strcpy(Sav_caxis[2],Srrat);
		for (i=0;i<12;i++)
		{
			if (Sav_maxis[i][0]!='\0')
			{
				strcpy(T_maxis8[i], Sav_maxis[i]);
			}
		}
	}
}
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
		status = nclu_tlaxis_command(&cmdbuf,&taxis,caxis,maxis,UU_TRUE,UU_TRUE);
		if (status==0)
		{
			ncl_set_cmdmode(UU_TRUE);
			ncl_call(&cmdbuf);
		}
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
	int status,irtn,fieldno,stat;
	UD_DDATA val;
	UD_METHOD save_entry, save_sel_fun;
	char traverse2[50], display2[50];

	static int but_mod = 0;
	static char traverse[] =
	{
		1,1, 1,1,1,1,1,1,1,1, 					/* FIXED */
		1,1, 1,1,1,1,1,			/* TANTO,DS */
		1, 1,1,1,1,1,				/* FAN */
		1,1,1,1,1,1,1,1,1,1,	/* COMBIN */
		1,1,1,1,1,					/* NORMAL,PS */
		1,1,1,1,1,1,1,1,			/* ATANGL,PS */
		1,1,		 					/* THRU,PT */
		1,1,1,	 					/* THRU,CV */
		1,1,1,1,1,					/* INTERP */
		1,1,1      /*All*/
	};
	static char display[] =
	{
//labels
		1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,					/* FIXED */
		1,1,1, 1,1,1, 1,		/* TANTO,DS */
		1,1, 1,1,1,1,				/* FAN */
		1,1,1,1,1, 1,1,1,1,1,	/* COMBIN */
		1, 1,1,1,1,					/* NORMAL,PS */
		1,1,1, 1,1,1,1,1,			/* ATANGL,PS */
		1,1,		 					/* THRU,PT */
		1,1,1,						/* THRU,CV */
		1,1, 1,1,1,				/* INTERP */
		1,1,1
	};
	static UD_METHOD methods[] =
	{
		S_toggle_fixed,S_edit_main, S_toggle_fixed,
		S_toggle_fixed_mod,S_edit_main_mod,S_edit_main_mod,S_edit_main_mod,
		S_edit_main_mod,S_edit_main_mod,
		S_video_play,
		S_edit_main,S_toggle_tanto, S_edit_main, S_toggle_tanto, S_edit_main,S_toggle_tanto,
			S_video_play,
		S_edit_main,S_toggle_fan, S_toggle_fan,S_edit_main,S_edit_main,S_video_play,
		S_edit_main,S_toggle_combin,S_edit_main,S_edit_main, S_edit_main,S_toggle_combin,
			S_toggle_combin,S_edit_main,S_edit_main, S_video_play,
		S_edit_main,S_toggle_normal,S_edit_main,S_toggle_normal,S_video_play,
		S_edit_main,S_edit_main, S_edit_main,S_toggle_atangl,S_toggle_atangl,S_edit_main,
			S_edit_main,S_video_play,
		S_edit_main,S_video_play,
		S_edit_main, S_edit_main,S_video_play,
		S_edit_main, S_toggle_interp,S_edit_main,S_edit_main,S_video_play,
		
		S_OnModify, S_OnModifyBut, S_toggle_main,
		OnClose
	};

	static char called[] =
	{
		6,6, 6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,
		6,6, 6,6,6,6,6,
		6,6,6,6,6,6,6,6,6,6,
		6,6,6,6,
		6,6,6,6,6,6,6,6,
		6,
		6, 6,
		6,6,6,6,6,
		6,6,
		6,6,6,6, 		
		6,6,6,
		
		6,
		6,6,6
	};
	static int *ans[] =
	{
/*
.....Fixed
*/
		&Sftog[1],(int *)Sfvec, &Sftog[2],
		&Fytog[0],(int *)Fymod[0],(int *)Fymod[1],(int *)Fymod[2],(int *)Fymod[3],
			(int *)Fymod[4],
		UU_NULL,
/*
.....Tanto DS
*/
		(int *)Sdhgt,&Sdtog[1], (int *)Sdsurf, &Sdtog[2],(int *)Sdvec,
			&Sdtog[3],UU_NULL,
/*
......Fan
*/
		(int *)Snhgt, &Sntog[1],&Sntog[2],(int *)Sndeg,(int *)Snrat,UU_NULL,
/*
......Combine
*/
		(int *)Schgt,&Sctog[1],(int *)Sclvd,(int *)Scapd, (int *)Scsurf,
			&Sctog[2],&Sctog[3],(int *)Scdeg,(int *)Scrat,UU_NULL,
/*
......Normal PS
*/
		(int *)Smsurf,&Smtog[1],(int *)Smvec,&Smtog[2],UU_NULL,
/*
......Atangle PS
*/
		(int *)Saang,(int *)Sadis,(int *)Sasurf,&Satog[1],&Satog[2],
			(int *)Savec,&Satog[3],UU_NULL,
/*
......Thru Point
*/
		(int *)Spgeo,UU_NULL,
/*
.....Thru Curve
*/
		(int *)Svgeo,(int *)Svdis,UU_NULL,
/*
.....Interpolate
*/
		(int *)Srgeo,&Srtog[1],(int *)Srdeg,(int *)Srrat,UU_NULL,
/*
.....ALL
*/
		&STlx_mod, &but_mod, &STlx_output
	};
/*
.....Initialize routine
*/
	STlx_mod = 0;
	STlx_output = 0;
	Staxis = *taxis;
	Sfixed = fixed;
	Sreentry = reentry;

	if ((Staxis.mode==NCLX_MOT_TLAXIS_FIXED)
		|| (Staxis.mode==NCLX_MOT_TLAXIS_SAME))
	{
		S_act_page = 0;
		Fytog[0] = 0;
		if ((Staxis.modify.angle_flag == 1)|| (taxis->modify.guide_flag == 1)
			|| (taxis->modify.gouge == 1) || (taxis->modify.lock_mode != NCLX_LOCK_OFF)
			|| (taxis->adjust_flag == 1))
		{
			Fytog[0] = 1;
		}
		Fytog[0] = Save_Fytog;	
	}
	else if (Staxis.mode==NCLX_MOT_TLAXIS_TANTO)
	{
		S_act_page = 1;
		if ((Staxis.modify.angle_flag == 1)|| (taxis->modify.guide_flag == 1)
			|| (taxis->modify.gouge == 1) || (taxis->modify.lock_mode != NCLX_LOCK_OFF)
			|| (taxis->adjust_flag == 1))
		{
			STlx_mod = 1;
		}
		Save_mod[0] = STlx_mod;
	}
	else if (Staxis.mode==NCLX_MOT_TLAXIS_FAN)
	{
		S_act_page = 2;
		if ((Staxis.modify.angle_flag == 1)|| (taxis->modify.guide_flag == 1)
			|| (taxis->modify.gouge == 1) || (taxis->modify.lock_mode != NCLX_LOCK_OFF)
			|| (taxis->adjust_flag == 1))
		{
			STlx_mod = 1;
		}
		Save_mod[1] = STlx_mod;
	}
	else if (Staxis.mode==NCLX_MOT_TLAXIS_COMBIN)
	{
		S_act_page = 3;
		if ((Staxis.modify.angle_flag == 1)|| (taxis->modify.guide_flag == 1)
			|| (taxis->modify.gouge == 1) || (taxis->modify.lock_mode != NCLX_LOCK_OFF)
			|| (taxis->adjust_flag == 1))
		{
			STlx_mod = 1;
		}
		Save_mod[2] = STlx_mod;
	}
	else if (Staxis.mode==NCLX_MOT_TLAXIS_NORMAL)
	{
		S_act_page = 4;
		if ((Staxis.modify.angle_flag == 1)|| (taxis->modify.guide_flag == 1)
			|| (taxis->modify.gouge == 1) || (taxis->modify.lock_mode != NCLX_LOCK_OFF)
			|| (taxis->adjust_flag == 1))
		{
			STlx_mod = 1;
		}
		Save_mod[3] = STlx_mod;
	}
	else if (Staxis.mode==NCLX_MOT_TLAXIS_ATANGL)
	{
		S_act_page = 5;
		if ((Staxis.modify.angle_flag == 1)|| (taxis->modify.guide_flag == 1)
			|| (taxis->modify.gouge == 1) || (taxis->modify.lock_mode != NCLX_LOCK_OFF)
			|| (taxis->adjust_flag == 1))
		{
			STlx_mod = 1;
		}
		Save_mod[4] = STlx_mod;
	}
	else if (Staxis.mode==NCLX_MOT_TLAXIS_POINT)
	{
		S_act_page = 6;
		if ((Staxis.modify.angle_flag == 1)|| (taxis->modify.guide_flag == 1)
			|| (taxis->modify.gouge == 1) || (taxis->modify.lock_mode != NCLX_LOCK_OFF)
			|| (taxis->adjust_flag == 1))
		{
			STlx_mod = 1;
		}
		Save_mod[5] = STlx_mod;
	}
	else if (Staxis.mode==NCLX_MOT_TLAXIS_CURVE)
	{
		S_act_page = 7;
		if ((Staxis.modify.angle_flag == 1)|| (taxis->modify.guide_flag == 1)
			|| (taxis->modify.gouge == 1) || (taxis->modify.lock_mode != NCLX_LOCK_OFF)
			|| (taxis->adjust_flag == 1))
		{
			STlx_mod = 1;
		}
		Save_mod[6] = STlx_mod;
	}
	else if (Staxis.mode==NCLX_MOT_TLAXIS_INTERP)
	{
		S_act_page = 8;
		if ((Staxis.modify.angle_flag == 1)|| (taxis->modify.guide_flag == 1)
			|| (taxis->modify.gouge == 1) || (taxis->modify.lock_mode != NCLX_LOCK_OFF)
			|| (taxis->adjust_flag == 1))
		{
			STlx_mod = 1;
		}
		Save_mod[7] = STlx_mod;
	}
	else
		S_act_page = 0;

	S_focus_page2 = -1;
	S_focus_page = S_act_page;
/*
.....Set up form fields
*/
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
/*
.....initial modify form value too
*/
	S_define_tilt(&Staxis,Sttog,Stfwd,Strgt,display2,traverse2,Sreentry);
	S_define_guide(&Staxis,Sgtog,Sggeo,Sgdis,display2,traverse2,Sreentry);
	S_define_gougck(&Staxis,Sktog,display2,traverse2,Sreentry);
	S_define_lock(&Staxis,Sltog,Slds1,Slds2,display2,traverse2,Sreentry);
	S_define_modify(&Staxis,Sytog,Symod,display2,traverse2,Sreentry);

	S_copy_tlaxis(&Staxis, &Sav_taxis);
	S_copy_tlaxis2(&Staxis, &T_taxis1);
	S_copy_tlaxis2(&Staxis, &T_taxis2);
	S_copy_tlaxis2(&Staxis, &T_taxis3);
	S_copy_tlaxis2(&Staxis, &T_taxis4);
	S_copy_tlaxis2(&Staxis, &T_taxis6);
	S_copy_tlaxis2(&Staxis, &T_taxis6);
	S_copy_tlaxis2(&Staxis, &T_taxis7);
	S_copy_tlaxis2(&Staxis, &T_taxis8);
	S_save_axis();

	if ((Staxis.mode==NCLX_MOT_TLAXIS_FIXED)
		|| (Staxis.mode==NCLX_MOT_TLAXIS_SAME))
	{
		display[TAX_MODY+LABEL_NUM] = 0;
		display[TAX_MODYB+LABEL_NUM] = 0;
		S_define_modify(taxis,Fytog,Fymod,display,traverse,reentry);
	}
	else
	{
		display[TAX_MODY+LABEL_NUM] = 1;
		display[TAX_MODYB+LABEL_NUM] = 1;
	}
	if (Fytog[0]==0)
	{
		traverse[FMOD_UP] = 0; 
		traverse[FMOD_RGT] = 0; 
		traverse[FMOD_LFT] = 0;  
		traverse[FMOD_FWD] = 0; 
		traverse[FMOD_ANG] = 0; 
	}
	else
	{
		traverse[FMOD_UP] = 1;
		traverse[FMOD_RGT] =  1;
		traverse[FMOD_LFT] =  1;
		traverse[FMOD_FWD] =  1;
		traverse[FMOD_ANG] =  1;
	}
/*
.....if modify checked, need initial output modify value maxis, since the maxis is empty.
*/
	if (STlx_mod)
		S_init_otions();
/*
.....Define form entry routine
*/
	save_entry = UD_initfrm_intry;
	save_sel_fun = UD_form_secsel;
/*
.....Get the Form input
*/
again:;
	if (modal)
	{
		Sfrm1 = 0;
		UD_initfrm_intry = S_init_form;
		UD_form_secsel = S_form_secsel;
		status = ud_form1("ntlaxis.frm",ans,ans,methods,called,display,traverse);
		UD_initfrm_intry = save_entry;
		UD_form_secsel = save_sel_fun;
		Sfrm1 = -1;
		S_option_first = 1;
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
	S_copy_tlaxis(&Staxis, taxis);
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
	int i,irtn;
/*
.....Store active tool axis settings
*/
	if (S_focus_page == 0)
	{
		irtn = S_set_fixed(taxis,caxis,Sftog,Sfvec);
		S_set_modify(taxis,maxis,Fytog,Fymod);
		taxis->mode = NCLX_MOT_TLAXIS_FIXED;
	}
	else if (S_focus_page == 1)
	{
		irtn = S_set_tanto(taxis,caxis,Sdtog,Sdhgt,Sdsurf,Sdvec);
		taxis->mode = NCLX_MOT_TLAXIS_TANTO;
	}
	else if (S_focus_page == 2)
	{
		irtn = S_set_fan(taxis,caxis,Sntog,Snhgt,Sndeg,Snrat);
		taxis->mode = NCLX_MOT_TLAXIS_FAN;
	}
	else if (S_focus_page == 3)
	{
		irtn = S_set_combin(taxis,caxis,Sctog,Schgt,Sclvd,Scapd,Scsurf,Scdeg,
			Scrat);
		taxis->mode = NCLX_MOT_TLAXIS_COMBIN;
	}
	else if (S_focus_page == 4)
	{
		irtn = S_set_normal(taxis,caxis,Smtog,Smsurf,Smvec);
		taxis->mode = NCLX_MOT_TLAXIS_NORMAL;
	}
	else if (S_focus_page == 5)
	{
		irtn = S_set_atangl(taxis,caxis,Satog,Saang,Sadis,Sasurf,Savec);
		taxis->mode = NCLX_MOT_TLAXIS_ATANGL;
	}
	else if (S_focus_page == 6)
	{
		irtn = S_set_point(taxis,caxis,Sptog,Spgeo);
		taxis->mode = NCLX_MOT_TLAXIS_POINT;
	}
	else if (S_focus_page == 7)
	{
		irtn = S_set_curve(taxis,caxis,Svtog,Svgeo,Svdis);
		taxis->mode = NCLX_MOT_TLAXIS_CURVE;
	}
	else if (S_focus_page == 8)
	{
		irtn = S_set_interp(taxis,caxis,Srtog,Srgeo,Srdeg,Srrat);
		taxis->mode = NCLX_MOT_TLAXIS_INTERP;
	}
/*
.....Store active tool axis options
*/
	if (S_focus_page2!=-1)
	{
		if (S_focus_page!=0)
		{
			irtn = irtn + S_set_tilt(taxis,maxis,Sttog,Stfwd,Strgt);
			irtn = irtn + S_set_guide(taxis,maxis,Sgtog,Sggeo,Sgdis);
			irtn = irtn + S_set_gougck(taxis,maxis,Sktog);
			irtn = irtn + S_set_lock(taxis,maxis,Sltog,Slds1,Slds2);
			irtn = irtn + S_set_modify(taxis,maxis,Sytog,Symod);
		}
	}
/*
.....if not open, store the current one into maxis
*/
	else
	{
		if (S_focus_page!=0)
		{
			for (i=0;i<11;i++)
			{
				if (Smaxis[i][0]!='\0')
				{
					strcpy(maxis[i], Smaxis[i]);
				}
				else
				{
					maxis[i][0] = '\0';
				}
			}
		}
	}
/*
.....End of routine
*/
	return(irtn);
}
/*
.....only check the accept/focus mode changes
*/
/*********************************************************************
**    E_FUNCTION     : S_if_tlaxis_changed(taxis,caxis,maxis, kfl, form_flag)
**       Check the input tlaxis value with the saved tlaxis value and see if it is changed
**    PARAMETERS
**       INPUT  : none
**       OUTPUT :
**          taxis    = input tool axis parameters.
**          caxis    = Textual form responses.
**          maxis    = Textual form responses for tool modify parameters.
**			kfl:	the old paramter for tlaxis form, just pass
**			form_flag: 0: check for both main form and modify form input difference
**						1: check for main form only
**						2: check for toolaxis modify form only
**    RETURNS      : 1: changed
**					0: not changed
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_if_tlaxis_changed(taxis,caxis,maxis, kfl, form_flag)
NCLX_mot_tlaxis *taxis;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
char maxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
UU_LOGICAL kfl;
int form_flag;
{
	int i;
	if ((form_flag==0)&&(Sav_taxis.mode!=taxis->mode))
		return 1;
	if (kfl==UU_FALSE)
		return 1;
	if (kfl)
	{
		if (taxis->mode==NCLX_MOT_TLAXIS_FIXED)
		{
			if (form_flag!=2)
			{
				if (stricmp(Sav_caxis[0], caxis[0])!=0)
					return 1;
				if (Sav_taxis.normal!=taxis->normal)
					return 1;
			}
			if (form_flag==1)
				return 0;
			if (Sav_taxis.adjust_flag!=taxis->adjust_flag)
				return 1;
			if (taxis->adjust_flag==1)
			{
				for (i=6;i<=10;i++)
				{
					if (stricmp(Sav_maxis[i], maxis[i])!=0)
						return 1;
				}
			}
			return 0;
		}
		if (form_flag==2)
			goto subform;
		if (taxis->mode==NCLX_MOT_TLAXIS_TANTO)
		{
			if (Save_mod[0]!=STlx_mod)
				return 1;
			if (stricmp(Sav_caxis[0], caxis[0])!=0)
				return 1;
			if (stricmp(Sav_caxis[1], caxis[1])!=0)
				return 1;
			if (stricmp(Sav_caxis[2], caxis[2])!=0)
				return 1;
			if (Sav_taxis.parelm!=taxis->parelm)
				return 1;
			if ((taxis->parelm==0)&&(Sav_taxis.perpto_flag!=taxis->perpto_flag))
				return 1;
		}
		if (taxis->mode==NCLX_MOT_TLAXIS_FAN)
		{
			if (Save_mod[1]!=STlx_mod)
				return 1;
			if (stricmp(Sav_caxis[0], caxis[0])!=0)
				return 1;
			if (Sav_taxis.normal!=taxis->normal)
				return 1;
			if (Sav_taxis.center!=taxis->center)
				return 1;
			if (taxis->normal)
			{
				if (stricmp(Sav_caxis[1], caxis[1])!=0)
					return 1;
				if (stricmp(Sav_caxis[2], caxis[2])!=0)
					return 1;
			}
		}		
		if (taxis->mode==NCLX_MOT_TLAXIS_COMBIN)
		{
			if (Save_mod[2]!=STlx_mod)
				return 1;
			if (stricmp(Sav_caxis[0], caxis[0])!=0)
				return 1;
			if (stricmp(Sav_caxis[1], caxis[1])!=0)
				return 1;
			if (stricmp(Sav_caxis[2], caxis[2])!=0)
				return 1;
			if (stricmp(Sav_caxis[3], caxis[3])!=0)
				return 1;
			if (Sav_taxis.parelm!=taxis->parelm)
				return 1;
			if (Sav_taxis.center!=taxis->center)
				return 1;
			if (taxis->normal)
			{
				if (stricmp(Sav_caxis[4], caxis[4])!=0)
					return 1;
				if (stricmp(Sav_caxis[5], caxis[5])!=0)
					return 1;
			}
		}		
		if (taxis->mode==NCLX_MOT_TLAXIS_NORMAL)
		{
			if (Save_mod[3]!=STlx_mod)
				return 1;
			if (Sav_taxis.perpto_flag!=taxis->perpto_flag)
				return 1;
			if (stricmp(Sav_caxis[0], caxis[0])!=0)
				return 1;
			if (stricmp(Sav_caxis[1], caxis[1])!=0)
				return 1;
		}
		if (taxis->mode==NCLX_MOT_TLAXIS_ATANGL)
		{
			if (Save_mod[4]!=STlx_mod)
				return 1;
			if (stricmp(Sav_caxis[0], caxis[0])!=0)
				return 1;
			if (stricmp(Sav_caxis[1], caxis[1])!=0)
				return 1;
			if (stricmp(Sav_caxis[2], caxis[2])!=0)
				return 1;
			if (Sav_taxis.perpto_flag!=taxis->perpto_flag)
				return 1;
			if (Sav_taxis.contact!=taxis->contact)
				return 1;
			if (taxis->perpto_flag)
			{
				if (stricmp(Sav_caxis[3], caxis[3])!=0)
					return 1;
			}
		}
		if (taxis->mode==NCLX_MOT_TLAXIS_POINT)
		{
			if (Save_mod[5]!=STlx_mod)
				return 1;
			if (stricmp(Sav_caxis[0], caxis[0])!=0)
				return 1;
		}
		if (taxis->mode==NCLX_MOT_TLAXIS_CURVE)
		{
			if (Save_mod[6]!=STlx_mod)
				return 1;
			if (stricmp(Sav_caxis[0], caxis[0])!=0)
				return 1;
			if (stricmp(Sav_caxis[1], caxis[1])!=0)
				return 1;
		}
		if (taxis->mode==NCLX_MOT_TLAXIS_INTERP)
		{
			if (Save_mod[7]!=STlx_mod)
				return 1;
			if (stricmp(Sav_caxis[0], caxis[0])!=0)
				return 1;
			if (Sav_taxis.normal!=taxis->normal)
				return 1;
			if (taxis->normal)
			{
				if (stricmp(Sav_caxis[1], caxis[1])!=0)
					return 1;
				if (stricmp(Sav_caxis[2], caxis[2])!=0)
					return 1;
			}
		}
		if (form_flag==1)
			return 0;
subform:;
		if ((taxis->mode!=NCLX_MOT_TLAXIS_FIXED)
			&& (taxis->mode!=NCLX_MOT_TLAXIS_SAME))
		{
			if (Sav_taxis.modify.angle_flag!=taxis->modify.angle_flag)
				return 1;
			if (taxis->modify.angle_flag==1)
			{
				for (i=0;i<=1;i++)
				{
					if (stricmp(Sav_maxis[i], maxis[i])!=0)
						return 1;
				}
			}
			if (Sav_taxis.modify.guide_flag!=taxis->modify.guide_flag)
				return 1;
			if (taxis->modify.guide_flag==1)
			{
				if (stricmp(Sav_maxis[2], maxis[2])!=0)
					return 1;
				if (Sav_taxis.modify.guide_cond!=taxis->modify.guide_cond)
					return 1;
				if (taxis->modify.guide_cond != 1)
				{
					if (Sav_taxis.modify.guide_contact!=taxis->modify.guide_contact)
						return 1;
					if (stricmp(Sav_maxis[3], maxis[3])!=0)
						return 1;
				}
			}
			if (Sav_taxis.modify.gouge!=taxis->modify.gouge)
				return 1;
			if (Sav_taxis.modify.lock_mode!=taxis->modify.lock_mode)
				return 1;
			if (taxis->modify.lock_mode!=NCLX_LOCK_END)
			{
				for (i=4;i<=5;i++)
				{
					if (stricmp(Sav_maxis[i], maxis[i])!=0)
						return 1;
				}
				if (Sav_taxis.modify.lock_radius!=taxis->modify.lock_radius)
					return 1;
				if (atof(maxis[5])!=0.0)
				{
					if (Sav_taxis.modify.lock_transition!=taxis->modify.lock_transition)
						return 1;
				}
			}
			if (Sav_taxis.adjust_flag!=taxis->adjust_flag)
				return 1;
			if (taxis->adjust_flag==1)
			{
				for (i=6;i<=10;i++)
				{
					if (stricmp(Sav_maxis[i], maxis[i])!=0)
						return 1;
				}
			}
		}
	}
	return 0;
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
int nclu_tlaxis_command(cmdbuf,taxis,caxis,maxis,kfl,flag)
NCL_cmdbuf *cmdbuf;
NCLX_mot_tlaxis *taxis;
char caxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
char maxis[][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
UU_LOGICAL kfl,flag;
{
	int nc,inum,i,j,ist,ideg, changed, changed1, changed2;
	UU_REAL rval[3], rval2[3], rate;
	char str[80];
	char *cptr[6],cbuf[6][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	char *mptr[12],mbuf[12][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	char tempstr[256];
	int	subscript;
	int status, rel_num;
	UM_int4 nclkey;

	if (kfl)
	{
/*
.....check if main form value changed
*/
		changed = S_if_tlaxis_changed(taxis,caxis,maxis, kfl, 0);
		if ((changed==0)&&(STlx_output==0))
			return 1;
	}
/*
.....check if form changed
*/
	if (kfl)
	{
		for (i=0;i<6;i++) cptr[i] = caxis[i];
		for (i=0;i<11;i++) mptr[i] = maxis[i];
	}
	else
	{
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
		for (i=0;i<6;i++) cptr[i] = cbuf[i];
		for (i=0;i<11;i++) mptr[i] = mbuf[i];
	}
/*
.....Initialize command
*/
	ncl_init_cmdbuf(cmdbuf);
	if (Sav_taxis.mode==NCLX_MOT_TLAXIS_FIXED)
		goto mod1;
/*
.....All modify statement with flag turn off (if before is on) have to execute before main statement
*/
	if ((taxis->modify.angle_flag == 0)&&(Sav_taxis.modify.angle_flag==1))
	{		
		if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_tlaxis,NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_fwd,NCL_comma);
		ncl_add_token(cmdbuf,"0.0",NCL_comma);
		ncl_add_token(cmdbuf,NCL_right,NCL_comma);
		ncl_add_token(cmdbuf,"0.0",NCL_comma);
		ncl_add_cmdbuf(cmdbuf);
	}
	if ((taxis->modify.guide_flag == 0)&&(Sav_taxis.modify.guide_flag==1))
	{		
		if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_tlaxis,NCL_nocomma);
		ncl_add_token(cmdbuf,"GUIDE",NCL_comma);
		ncl_add_token(cmdbuf,NCL_off,NCL_comma);
		ncl_add_cmdbuf(cmdbuf);
	}
	if ((taxis->modify.gouge == 0)&&(Sav_taxis.modify.gouge==1))
	{
		if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_tlaxis,NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_gougck1,NCL_comma);
		ncl_add_token(cmdbuf,NCL_off,NCL_comma);
		ncl_add_cmdbuf(cmdbuf);
	}
	if (((taxis->modify.lock_mode == NCLX_LOCK_OFF)&&(Sav_taxis.modify.lock_mode!=NCLX_LOCK_OFF))
		|| ((taxis->modify.lock_mode != NCLX_LOCK_OFF)&&(Sav_taxis.modify.lock_mode!=NCLX_LOCK_OFF)&&(taxis->modify.lock_dist==0.0)))
	{
		if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_tlaxis,NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_lock,NCL_comma);
		ncl_add_token(cmdbuf,NCL_off,NCL_nocomma);
		ncl_add_cmdbuf(cmdbuf);
	}
mod1:;
	if ((taxis->adjust_flag == 0)&&(Sav_taxis.adjust_flag==1))
	{
		if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
		ncl_add_token(cmdbuf,"TLAXIS/MODIFY,0,0,0,0,0",NCL_nocomma);
		ncl_add_cmdbuf(cmdbuf);
	}
	changed1 = S_if_tlaxis_changed(taxis,caxis,maxis, kfl, 1);
	if ((changed1==0)&&(STlx_output==0))
		goto ToolModify;
/*
.....main statement
*/
	if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
	ncl_add_token(cmdbuf,NCL_tlaxis,NCL_nocomma);
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
		if (taxis->normal) ncl_add_token(cmdbuf,NCL_normal,NCL_nocomma);
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
				ncl_add_token(cmdbuf,cptr[2],NCL_nocomma);
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
			ncl_add_token(cmdbuf,cptr[2],NCL_nocomma);
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
			ncl_add_token(cmdbuf,cptr[5],NCL_nocomma);
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
/*
......if there is surface, output, surface is not required
*/
		if (nc != 0)
			ncl_add_token(cmdbuf,cptr[0],NCL_comma);
		if (taxis->perpto_flag != 0)
		{
/*
.....if 4-axis is checked, vector is required
*/
			ncl_add_token(cmdbuf,NCL_perpto,NCL_comma);
/*
......both 4-axis and Maintain Perpto Vector checked: taxis->perpto_flag = 2
*/
			if (taxis->perpto_flag == 2)
				ncl_add_token(cmdbuf,NCL_last,NCL_comma);
			nc = strlen(cptr[1]);
			ul_strip_blanks(cptr[1],&nc);
/*
......if there is vector value, output
*/
			if (ul_to_reals(rval,&inum,3,cptr[1]) == UU_SUCCESS)
			{
				sprintf(str,"(VECTOR/%s)",cptr[1]);
				strcpy(cptr[1],str);
			}
			ncl_add_token(cmdbuf,cptr[1],NCL_nocomma);
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
			ncl_add_token(cmdbuf,cptr[3],NCL_nocomma);
		}
		break;
/*
.....TLAXIS/THRU,point
*/
	case NCLX_MOT_TLAXIS_POINT:
		ncl_add_token(cmdbuf,NCL_thru,NCL_comma);
		nc = strlen(cptr[0]);
		ul_strip_blanks(cptr[0],&nc);
/*
.....per Ken Specs, if Coord Value specified instead of label, use (PT/x,y,x)
*/
		strcpy(tempstr, cptr[0]);
		ncl_parse_subnum (tempstr, &subscript);
		ul_to_upper(tempstr);
		status = ncl_vxchk (tempstr,subscript, &nclkey, &rel_num);
		if ((rel_num==NCL_POINT_REL) || (rel_num==NCL_POINTVEC_REL) || (rel_num==UM_POINT_REL))
		{
			ncl_add_token(cmdbuf,cptr[0],NCL_comma);
		}
		else
		{
			ncl_add_token(cmdbuf,"(PT/",NCL_nocomma);
			ncl_add_token(cmdbuf,cptr[0],NCL_nocomma);
			ncl_add_token(cmdbuf,")",NCL_nocomma);
		}
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
			ncl_add_token(cmdbuf,cptr[1],NCL_nocomma);
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
			ncl_add_token(cmdbuf,cptr[2],NCL_nocomma);
		}
		break;
	}
/*
......seperate main command with modify command, one command is too long
*/
	ncl_add_cmdbuf(cmdbuf);
/*
...... nothing changed, but only forced out put, not output modify value
*/
	if (changed==0)
		goto done;
ToolModify:;
	changed2 = S_if_tlaxis_changed(taxis,caxis,maxis, kfl, 2);
	if (changed2==0)
		goto done;

	switch (taxis->mode)
	{
	case NCLX_MOT_TLAXIS_SAME:
	case NCLX_MOT_TLAXIS_FIXED:
		if ((Fytog[0]==Save_Fytog)&&(Fytog[0]==0))
			goto done;
		break;
/*
.....TLAXIS/TANTO,DS
*/
	case NCLX_MOT_TLAXIS_TANTO:
		if ((Save_mod[0]==STlx_mod)&&(STlx_mod==0))
			goto done;
		break;
/*
.....TLAXIS/FAN
*/
	case NCLX_MOT_TLAXIS_FAN:
		if ((Save_mod[1]==STlx_mod)&&(STlx_mod==0))
			goto done;
		break;

/*
.....TLAXIS/COMBIN
*/
	case NCLX_MOT_TLAXIS_COMBIN:
		if ((Save_mod[2]==STlx_mod)&&(STlx_mod==0))
			goto done;
		break;
/*
.....TLAXIS/NORMAL,PS
*/
	case NCLX_MOT_TLAXIS_NORMAL:
		if ((Save_mod[3]==STlx_mod)&&(STlx_mod==0))
			goto done;
		break;
/*
.....TLAXIS/ATANGL
*/
	case NCLX_MOT_TLAXIS_ATANGL:
		if ((Save_mod[4]==STlx_mod)&&(STlx_mod==0))
			goto done;
		break;
/*
.....TLAXIS/THRU,point
*/
	case NCLX_MOT_TLAXIS_POINT:
		if ((Save_mod[5]==STlx_mod)&&(STlx_mod==0))
			goto done;
		break;
/*
.....TLAXIS/THRU,curve
*/
	case NCLX_MOT_TLAXIS_CURVE:
		if ((Save_mod[6]==STlx_mod)&&(STlx_mod==0))
			goto done;
		break;
/*
.....TLAXIS/INTERP
*/
	case NCLX_MOT_TLAXIS_INTERP:
		if ((Save_mod[7]==STlx_mod)&&(STlx_mod==0))
			goto done;
		break;
	}
/*
.....TLAXIS/FWD,RIGHT
*/
	if ((taxis->modify.angle_flag == 1)&&
		((stricmp(Sav_maxis[0], maxis[0])!=0)
		||(stricmp(Sav_maxis[1], maxis[1])!=0)))
	{		
		if (ul_to_reals(rval,&inum,1,mptr[0]) != UU_SUCCESS)
			rval[0] = 1.;
		if (ul_to_reals(rval2,&inum,1,mptr[1]) != UU_SUCCESS)
			rval2[0] = 1.;
		if ((rval[0] != 0.)||(rval2[0] != 0.))
		{
			if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
			ncl_add_token(cmdbuf,NCL_tlaxis,NCL_nocomma);
		}
		if (rval[0] != 0.)
		{
			ncl_add_token(cmdbuf,NCL_fwd,NCL_comma);
			ncl_add_token(cmdbuf,mptr[0],NCL_comma);
		}
		if (rval2[0] != 0.)
		{
			ncl_add_token(cmdbuf,NCL_right,NCL_comma);
			ncl_add_token(cmdbuf,mptr[1],NCL_comma);
		}
		if ((rval[0] != 0.)||(rval2[0] != 0.))
			ncl_add_cmdbuf(cmdbuf);
	}
/*
.....TLAXIS/GUIDE,cv
*/
	if (taxis->modify.guide_flag == 1)
	{
		if (stricmp(Sav_maxis[2], maxis[2])!=0)
			goto guide_cmd;
		if (Sav_taxis.modify.guide_cond!=taxis->modify.guide_cond)
			goto guide_cmd;
		if (taxis->modify.guide_cond != 1)
		{
			if (Sav_taxis.modify.guide_contact!=taxis->modify.guide_contact)
				goto guide_cmd;
			if (stricmp(Sav_maxis[3], maxis[3])!=0)
				goto guide_cmd;
		}
		goto next;
guide_cmd:;
		if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_tlaxis,NCL_nocomma);
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
		ncl_add_cmdbuf(cmdbuf);
	}
next:;
/*
.....TLAXIS/GOUGCK
*/
	if (taxis->modify.gouge == 1)
	{
		if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_tlaxis,NCL_nocomma);

		ncl_add_token(cmdbuf,NCL_gougck1,NCL_comma);
		ncl_add_token(cmdbuf,NCL_on,NCL_comma);
		ncl_add_cmdbuf(cmdbuf);
	}
/*
.....TLAXIS/LOCK
*/
	if (taxis->modify.lock_mode != NCLX_LOCK_OFF)
	{
		rval[0] = 0.0;
		if (ul_to_reals(rval,&inum,1,maxis[5]) != UU_SUCCESS)
			rval[0] = 0.;
		for (i=4;i<=5;i++)
		{
			if (stricmp(Sav_maxis[i], maxis[i])!=0)
				goto lock_cmd;
		}
		if (Sav_taxis.modify.lock_radius!=taxis->modify.lock_radius)
			goto lock_cmd;
		if (rval[0]!=0.0)
		{
			if (Sav_taxis.modify.lock_transition!=taxis->modify.lock_transition)
				goto lock_cmd;
		}
		goto modify;
lock_cmd:;
		if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_tlaxis,NCL_nocomma);

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
		ncl_add_cmdbuf(cmdbuf);
	}
/*
.....TLAXIS/MODIFY
*/
modify:;
	if (taxis->adjust_flag == 1)
	{
		for (i=6;i<=10;i++)
		{
			if (stricmp(Sav_maxis[i], maxis[i])!=0)
				goto modify_cmd;
		}
modify_cmd:;
		if (!flag) ncl_add_token(cmdbuf,"*",NCL_nocomma);
		ncl_add_token(cmdbuf,NCL_tlaxis,NCL_nocomma);

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
		ncl_add_cmdbuf(cmdbuf);
	}
/*
.....Output NCL command
*/
	ncl_add_cmdbuf(cmdbuf);
done:;
	return 0;
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
	Sfrm1 = -1;
	S_option_first = 1;
	return(UD_FLDOK);
}

/***********************************************************************
**     I_FUNCTION   :   OnClose2()
**        Method called Modify form is closed.
**     PARAMETERS
**        INPUT  :
**           none
**        OUTPUT :
**           none
**        RETURNS      : none
**        SIDE EFFECTS : none
**        WARININGS    : none
************************************************************************/
static UD_FSTAT OnClose2(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i, status;
	S_enable_buttons2();
	if ((S_close_enable==0)||(*fieldno==-1))
/*
.....it is from system "x" close button
......we will consider it as "canceled" and not save
*/
		return(UD_FLDOK);
	if (S_focus_page!=0)
	{
		status = S_set_tilt(&Staxis,Smaxis,Sttog,Stfwd,Strgt);
		if (status!=0)
			return UD_BADREQ;
		status = S_set_guide(&Staxis,Smaxis,Sgtog,Sggeo,Sgdis);
		if (status!=0)
			return UD_BADREQ;
		status = S_set_gougck(&Staxis,Smaxis,Sktog);
		if (status!=0)
			return UD_BADREQ;
		status = S_set_lock(&Staxis,Smaxis,Sltog,Slds1,Slds2);
		if (status!=0)
			return UD_BADREQ;
		status = S_set_modify(&Staxis,Smaxis,Sytog,Symod);
	}
	if (status!=0)
		return UD_BADREQ;
/*
.....copy value to temp saving
*/
	if (S_focus_page==1)
	{
		S_copy_tlaxis2(&Staxis, &T_taxis1);
		for (i=0;i<11;i++)
		{
			if (Smaxis[i][0]!='\0')
			{
				strcpy(T_maxis1[i], Smaxis[i]);
			}
			else
			{
				T_maxis1[i][0] = '\0';
			}
		}
		T_ttog[0] = Sttog[0];
		T_gtog[0][0] = Sgtog[0];
		T_gtog[0][1] = Sgtog[1];
		T_gtog[0][2] = Sgtog[2];
		T_ktog[0] = Sktog[0];
		T_ltog[0][0] = Sltog[0];
		T_ltog[0][1] = Sltog[1];
		T_ltog[0][2] = Sltog[2];
		T_ltog[0][3] = Sltog[3];
		T_ltog[0][4] = Sltog[4];
		T_ytog[0] = Sytog[0];
	}
	else if (S_focus_page==2)
	{
		S_copy_tlaxis2(&Staxis, &T_taxis2);
		for (i=0;i<11;i++)
		{
			if (Smaxis[i][0]!='\0')
			{
				strcpy(T_maxis2[i], Smaxis[i]);
			}
			else
			{
				T_maxis2[i][0] = '\0';
			}
		}
		T_ttog[1] = Sttog[0];
		T_gtog[1][0] = Sgtog[0];
		T_gtog[1][1] = Sgtog[1];
		T_gtog[1][2] = Sgtog[2];
		T_ktog[1] = Sktog[0];
		T_ltog[1][0] = Sltog[0];
		T_ltog[1][1] = Sltog[1];
		T_ltog[1][2] = Sltog[2];
		T_ltog[1][3] = Sltog[3];
		T_ltog[1][4] = Sltog[4];
		T_ytog[1] = Sytog[0];
	}
	else if (S_focus_page==3)
	{
		S_copy_tlaxis2(&Staxis, &T_taxis3);
		for (i=0;i<11;i++)
		{
			if (Smaxis[i][0]!='\0')
			{
				strcpy(T_maxis3[i], Smaxis[i]);
			}
			else
			{
				T_maxis3[i][0] = '\0';
			}
		}
		T_ttog[2] = Sttog[0];
		T_gtog[2][0] = Sgtog[0];
		T_gtog[2][1] = Sgtog[1];
		T_gtog[2][2] = Sgtog[2];
		T_ktog[2] = Sktog[0];
		T_ltog[2][0] = Sltog[0];
		T_ltog[2][1] = Sltog[1];
		T_ltog[2][2] = Sltog[2];
		T_ltog[2][3] = Sltog[3];
		T_ltog[2][4] = Sltog[4];
		T_ytog[2] = Sytog[0];
	}
	else if (S_focus_page==4)
	{
		S_copy_tlaxis2(&Staxis, &T_taxis4);
		for (i=0;i<11;i++)
		{
			if (Smaxis[i][0]!='\0')
			{
				strcpy(T_maxis4[i], Smaxis[i]);
			}
			else
			{
				T_maxis4[i][0] = '\0';
			}
		}
		T_ttog[3] = Sttog[0];
		T_gtog[3][0] = Sgtog[0];
		T_gtog[3][1] = Sgtog[1];
		T_gtog[3][2] = Sgtog[2];
		T_ktog[3] = Sktog[0];
		T_ltog[3][0] = Sltog[0];
		T_ltog[3][1] = Sltog[1];
		T_ltog[3][2] = Sltog[2];
		T_ltog[3][3] = Sltog[3];
		T_ltog[3][4] = Sltog[4];
		T_ytog[3] = Sytog[0];
	}
	else if (S_focus_page==5)
	{
		S_copy_tlaxis2(&Staxis, &T_taxis5);
		for (i=0;i<11;i++)
		{
			if (Smaxis[i][0]!='\0')
			{
				strcpy(T_maxis5[i], Smaxis[i]);
			}
			else
			{
				T_maxis5[i][0] = '\0';
			}
		}
		T_ttog[4] = Sttog[0];
		T_gtog[4][0] = Sgtog[0];
		T_gtog[4][1] = Sgtog[1];
		T_gtog[4][2] = Sgtog[2];
		T_ktog[4] = Sktog[0];
		T_ltog[4][0] = Sltog[0];
		T_ltog[4][1] = Sltog[1];
		T_ltog[4][2] = Sltog[2];
		T_ltog[4][3] = Sltog[3];
		T_ltog[4][4] = Sltog[4];
		T_ytog[4] = Sytog[0];
	}
	else if (S_focus_page==6)
	{
		S_copy_tlaxis2(&Staxis, &T_taxis6);
		for (i=0;i<11;i++)
		{
			if (Smaxis[i][0]!='\0')
			{
				strcpy(T_maxis6[i], Smaxis[i]);
			}
			else
			{
				T_maxis6[i][0] = '\0';
			}
		}
		T_ttog[5] = Sttog[0];
		T_gtog[5][0] = Sgtog[0];
		T_gtog[5][1] = Sgtog[1];
		T_gtog[5][2] = Sgtog[2];
		T_ktog[5] = Sktog[0];
		T_ltog[5][0] = Sltog[0];
		T_ltog[5][1] = Sltog[1];
		T_ltog[5][2] = Sltog[2];
		T_ltog[5][3] = Sltog[3];
		T_ltog[5][4] = Sltog[4];
		T_ytog[5] = Sytog[0];
	}
	else if (S_focus_page==7)
	{
		S_copy_tlaxis2(&Staxis, &T_taxis7);
		for (i=0;i<11;i++)
		{
			if (Smaxis[i][0]!='\0')
			{
				strcpy(T_maxis7[i], Smaxis[i]);
			}
			else
			{
				T_maxis7[i][0] = '\0';
			}
		}
		T_ttog[6] = Sttog[0];
		T_gtog[6][0] = Sgtog[0];
		T_gtog[6][1] = Sgtog[1];
		T_gtog[6][2] = Sgtog[2];
		T_ktog[6] = Sktog[0];
		T_ltog[6][0] = Sltog[0];
		T_ltog[6][1] = Sltog[1];
		T_ltog[6][2] = Sltog[2];
		T_ltog[6][3] = Sltog[3];
		T_ltog[6][4] = Sltog[4];
		T_ytog[6] = Sytog[0];
	}
	else if (S_focus_page==8)
	{
		S_copy_tlaxis2(&Staxis, &T_taxis8);
		for (i=0;i<11;i++)
		{
			if (Smaxis[i][0]!='\0')
			{
				strcpy(T_maxis8[i], Smaxis[i]);
			}
			else
			{
				T_maxis8[i][0] = '\0';
			}
		}
		T_ttog[7] = Sttog[0];
		T_gtog[7][0] = Sgtog[0];
		T_gtog[7][1] = Sgtog[1];
		T_gtog[7][2] = Sgtog[2];
		T_ktog[7] = Sktog[0];
		T_ltog[7][0] = Sltog[0];
		T_ltog[7][1] = Sltog[1];
		T_ltog[7][2] = Sltog[2];
		T_ltog[7][3] = Sltog[3];
		T_ltog[7][4] = Sltog[4];
		T_ytog[7] = Sytog[0];
	}
	Sactive2 = UU_FALSE;
	Sfrm2 = -1;
	S_close_enable = 1;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_form_secsel(fieldno,val,stat)
**       callback for page selection.
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
static UD_FSTAT S_form_secsel(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,frmid, page;
	int old_focus_page, sav_focus_page, close_frm = -1;

	frmid = *fieldno;
	page = val->frmint[0];
	if (frmid==Sfrm1)
	{
		if (S_focus_page!=page)
		{
			close_frm = Sfrm2;
			old_focus_page = S_focus_page;
			Save_mod[S_focus_page-1] = STlx_mod;
		}
		S_focus_page = page;
		if (page==S_act_page)
		{
			ud_form_section_color(frmid, Smode[page], Sgreen, UU_TRUE);
/*
.....all others will be black
*/
			for (i=Fixed;i<=Interp;i++)
			{
				if (i!=page)
					ud_form_section_color(frmid, Smode[i], Sblack, UU_FALSE);
			}
/*
.....enable "Force Output" check box
*/
			ud_setfrm_traverse_mask(Sfrm1,TAX_FCO,UU_TRUE);	
		}
		else
		{
			ud_form_section_color(frmid, Smode[page], Sblack, UU_TRUE);
			for (i=Fixed;i<=Interp;i++)
			{
				if (i==S_act_page)
					ud_form_section_color(frmid, Smode[i], Sgreen, UU_TRUE);
				else if (i!=page)
					ud_form_section_color(frmid, Smode[i], Sblack, UU_FALSE);
			}
/*
.....disable "Force Output" check box
*/
			ud_setfrm_traverse_mask(Sfrm1,TAX_FCO,UU_FALSE);		
			STlx_output = 0;
			ud_update_answer(TAX_FCO,&STlx_output);
		}
		if (S_focus_page==0)
		{
/*
.....no sub form
*/
			ud_setfrm_display_mask(Sfrm1, UD_INPUTF, TAX_MODY, UU_FALSE);
			ud_setfrm_display_mask(Sfrm1, UD_INPUTF, TAX_MODYB, UU_FALSE);
		}
		else if (Sfrm2>0)
		{
			ud_setfrm_display_mask(Sfrm1, UD_INPUTF, TAX_MODY, UU_TRUE);
			ud_setfrm_display_mask(Sfrm1, UD_INPUTF, TAX_MODYB, UU_TRUE);
		}
		else
		{
			ud_setfrm_display_mask(Sfrm1, UD_INPUTF, TAX_MODY, UU_TRUE);
			ud_setfrm_display_mask(Sfrm1, UD_INPUTF, TAX_MODYB, UU_TRUE);
			STlx_mod = 0;
			ud_dispfrm_update_answer(Sfrm1, TAX_MODY, &STlx_mod);
		}
/*
......set value to saved one
*/
		if (old_focus_page!=S_focus_page)
		{
			if (S_focus_page!=0)
				STlx_mod = Save_mod[S_focus_page-1];
			ud_dispfrm_update_answer(Sfrm1, TAX_MODY, &STlx_mod);
		}
		ud_update_form(Sfrm1);
	
		if (close_frm!=-1)
		{
/*
......if subform opened, close current subform
*/
			uw_set_curfrm_sav(1);
			sav_focus_page = S_focus_page;
			S_focus_page = old_focus_page;
			ud_close_dispfrm (close_frm); 
			S_focus_page = sav_focus_page;
			uw_set_curfrm_sav(0);
		}	
	}
	else if (frmid==Sfrm2)
	{
		S_focus_page2 = page;
		ud_form_section_color(Sfrm2,Soption[page],Sblack,UU_TRUE);
		S_enable_buttons2();
	}
	S_enable_buttons();
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
	if ((Staxis.mode==NCLX_MOT_TLAXIS_FIXED)
		|| (Staxis.mode==NCLX_MOT_TLAXIS_SAME))
	{
		S_act_page = 0;
	}
	else if (Staxis.mode==NCLX_MOT_TLAXIS_TANTO)
		S_act_page = 1;
	else if (Staxis.mode==NCLX_MOT_TLAXIS_FAN)
		S_act_page = 2;
	else if (Staxis.mode==NCLX_MOT_TLAXIS_COMBIN)
		S_act_page = 3;
	else if (Staxis.mode==NCLX_MOT_TLAXIS_NORMAL)
		S_act_page = 4;
	else if (Staxis.mode==NCLX_MOT_TLAXIS_ATANGL)
		S_act_page = 5;
	else if (Staxis.mode==NCLX_MOT_TLAXIS_POINT)
		S_act_page = 6;
	else if (Staxis.mode==NCLX_MOT_TLAXIS_CURVE)
		S_act_page = 7;
	else if (Staxis.mode==NCLX_MOT_TLAXIS_INTERP)
		S_act_page = 8;
	else
		S_act_page = 0;
	ud_form_section_color(Sfrm1, Smode[S_act_page], Sgreen, UU_TRUE);
	S_focus_page2 = -1;
	S_focus_page = S_act_page;
	ud_form_section_active(Sfrm1, Smode[S_act_page]);
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
static UD_FSTAT S_init_form2(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....if it is fixed
*/
	if (S_focus_page==0)
	{
		ud_form_section_enable(Sfrm2, Soption[0], 0);
		ud_form_section_enable(Sfrm2, Soption[1], 0);
		ud_form_section_enable(Sfrm2, Soption[2], 0);
		ud_form_section_enable(Sfrm2, Soption[3], 0);
		S_focus_page2 = 4;
		ud_form_section_enable(Sfrm2, Soption[4], 1);
		ud_form_section_active(Sfrm2, Soption[4]);
	}
	else
	{
		ud_form_section_enable(Sfrm2, Soption[0], 1);
		ud_form_section_enable(Sfrm2, Soption[1], 1);
		ud_form_section_enable(Sfrm2, Soption[2], 1);
		ud_form_section_enable(Sfrm2, Soption[3], 1);
		S_focus_page2 = 0;
		ud_form_section_enable(Sfrm2, Soption[4], 1);
		ud_form_section_active(Sfrm2, Soption[0]);
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
		
	ftog[0] = 1;
	traverse[FXD_MODE] = traverse[FXD_NORM] = 1;
	if (ftog[1] == 0 || ftog[1] == 2)
		traverse[FXD_VEC] = 0;
	else
		traverse[FXD_VEC] = 1;
/*
.....Set Advanced fields
*/
	display[FXD_NORM+LABEL_NUM] = 1;
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
	taxis->normal = ftog[2];
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
	dtog[0] = 1;

	if (dtog[1] == 1) 
	{
		traverse[TAN_4AX] = 0;
		traverse[TAN_VEC] = traverse[TAN_MNT] = traverse[TAN_SURF] = 0;
	}
	else
	{
		traverse[TAN_4AX] = 1;
		traverse[TAN_SURF] = 1;
		traverse[TAN_VEC] = traverse[TAN_MNT] = dtog[2];
/*
......if "Perpto vector" is empty, disable the following checkbox
......Ken added, make sense, so keep it
*/
		if (dvec[0]=='\0')
			traverse[TAN_MNT] = 0;
	}
/*
.....Set Advanced fields
*/
	display[TAN_SURF+LABEL_NUM] = 1;
	display[TAN_VEC+LABEL_NUM] = 1;
	display[TAN_MNT+LABEL_NUM] = 1;
	display[TAN_4AX+LABEL_NUM] = 1;
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
	
	taxis->perpto_flag = dtog[2];
	if (dtog[2] == 1 && dtog[3] == 1) taxis->perpto_flag = 2;
	if (taxis->parelm == 0)
	{
		strcpy(caxis[1],dsurf);
		if (dtog[2] != 0) strcpy(caxis[2],dvec);
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
	ntog[0] = 1;
	traverse[FAN_HGT] = traverse[FAN_CEN] = 1;
	traverse[FAN_SMO] = 1;
	traverse[FAN_DEG] = traverse[FAN_RAT] = ntog[2];
/*
.....Set Advanced fields
*/
	display[FAN_CEN+LABEL_NUM] = 1;
	display[FAN_SMO+LABEL_NUM] = 1;
	display[FAN_DEG+LABEL_NUM] = 1;
	display[FAN_RAT+LABEL_NUM] = 1;
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
	taxis->center = ntog[1];
	taxis->normal = ntog[2];
	if (taxis->normal == 1)
	{
		strcpy(caxis[1],ndeg);
		strcpy(caxis[2],nrat);
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
	ctog[0] = 1;
	traverse[COM_HGT] = traverse[COM_PARL] = traverse[COM_LVD] = 1;
	traverse[COM_APD] = traverse[COM_SURF] = traverse[COM_CEN] = 1;
	traverse[COM_SMO] = 1;
	traverse[COM_DEG] = traverse[COM_RAT] = ctog[3];
/*
.....Set Advanced fields
*/
	display[COM_SURF+LABEL_NUM] = 1;
	display[COM_CEN+LABEL_NUM] = 1;
	display[COM_SMO+LABEL_NUM] = 1;
	display[COM_DEG+LABEL_NUM] = 1;
	display[COM_RAT+LABEL_NUM] = 1;
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
	strcpy(caxis[3],csurf);
	taxis->center = ctog[2];
	taxis->normal = ctog[3];
	if (taxis->normal == 1)
	{
		strcpy(caxis[4],cdeg);
		strcpy(caxis[5],crat);
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
	mtog[0] = 1;
	traverse[NOR_SURF] = traverse[NOR_4AX] = 1;
	traverse[NOR_VEC] = traverse[NOR_MNT] = mtog[1];
/*
.....Set Advanced fields
*/
	display[NOR_SURF+LABEL_NUM] = 1;
	display[NOR_4AX+LABEL_NUM] = 1;
	display[NOR_VEC+LABEL_NUM] = 1;
	display[NOR_MNT+LABEL_NUM] = 1;
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
	taxis->perpto_flag = mtog[1];
	if (mtog[1] == 1 && mtog[2] == 1) taxis->perpto_flag = 2;
	strcpy(caxis[0],msurf);
	if (taxis->perpto_flag != 0) strcpy(caxis[1],mvec);
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
	atog[0] = 1;
	atog[1] = taxis->contact;
	traverse[ATA_ANG] = traverse[ATA_CLD] = traverse[ATA_SURF] = 1;
	traverse[ATA_CNT] = traverse[ATA_4AX] = 1;
	traverse[ATA_VEC] = traverse[ATA_MNT] = atog[2];
/*
.....Set Advanced fields
*/
	display[ATA_SURF+LABEL_NUM] = 1;
	display[ATA_CNT+LABEL_NUM] = 1;
	display[ATA_4AX+LABEL_NUM] = 1;
	display[ATA_VEC+LABEL_NUM] = 1;
	display[ATA_MNT+LABEL_NUM] = 1;
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
	taxis->contact = atog[1];
	taxis->perpto_flag = atog[2];
	if (atog[2] == 1 && atog[3] == 1) taxis->perpto_flag = 2;
	strcpy(caxis[2],asurf);
	if (atog[2] != 0) strcpy(caxis[3],avec);
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
	ptog[0] = 1;
	traverse[PNT_GEO] = 1;
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
	vtog[0] = 1;
	traverse[CRV_GEO] = traverse[CRV_DIS] = 1;
/*
.....Set Advanced fields
*/
	display[CRV_DIS+LABEL_NUM] = 1;
	display[CRV_GEO+LABEL_NUM] = 1;
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
	if (ul_to_reals(&rval,&inum,1,vdis) != UU_SUCCESS || rval != 0.)
		strcpy(caxis[1],vdis);
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
		rtog[1] = 0;
		if (ideg > 1 && rate != 0.) rtog[1] = 1;
		sprintf(rdeg,"%d",ideg);
		ncl_sprintf(rrat,&rate,1);
	}
/*
.....Disable INTERP fields
*/
	flag = taxis->mode == NCLX_MOT_TLAXIS_INTERP;
	if (reentry) flag = rtog[0];
	rtog[0] = 1;
	traverse[INT_GEO] = traverse[INT_SMO] = 1;
	traverse[INT_DEG] = traverse[INT_RAT] = rtog[1];
/*
.....Set Advanced fields
*/
	display[INT_SMO+LABEL_NUM] = 1;
	display[INT_DEG+LABEL_NUM] = 1;
	display[INT_RAT+LABEL_NUM] = 1;
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
	taxis->normal = rtog[1];
	if (taxis->normal == 1)
	{
		strcpy(caxis[1],rdeg);
		strcpy(caxis[2],rrat);
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
	if (gtog[1] == 1) 
	{
		traverse[GDE_DIS] = traverse[GDE_CON] = 0;
	}
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
	traverse[LCK_TYP] = ltog[0];
	if (ltog[1] == 2)
	{
		for (i=LCK_LIN;i<=LCK_MOV;i++) traverse[i] = 0;
	}
	else
	{
		for (i=LCK_LIN;i<=LCK_TRN;i++) traverse[i] = ltog[0];
		traverse[LCK_FAN] = traverse[LCK_MOV] = ltog[3] && ltog[0];
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
	nc = strlen(lds1);
	ul_strip_blanks(lds1,&nc);
	if (nc == 0) strcpy(caxis[4],"0.");
	else strcpy(caxis[4],lds1);

	if (ltog[3] == 1) 
	{
		taxis->modify.lock_transition = ltog[4];
		nc = strlen(lds2);
		ul_strip_blanks(lds2,&nc);
		if (nc == 0) strcpy(caxis[5],"0.");
		else strcpy(caxis[5],lds2);
	}
	else
	{
		strcpy(caxis[5],"0.");
	}
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
	static int isub[]={8,7,6,10,9};
/*
.....Define MODIFY settings
*/
	if (!reentry)
	{
		if (S_focus_page==S_act_page)
		{
			ytog[0] = taxis->adjust_flag;
			ncl_sprintf(ymod[0],&taxis->adjust.up_offset,1);
			ncl_sprintf(ymod[1],&taxis->adjust.fwd_offset,1);
			ncl_sprintf(ymod[2],&taxis->adjust.right_offset,1);
			ncl_sprintf(ymod[3],&taxis->adjust.fwd_tilt,1);
			ncl_sprintf(ymod[4],&taxis->adjust.right_tilt,1);
		}
		else
		{
			ytog[0] = 0;
			strcpy(ymod[0],"0."); strcpy(ymod[1],"0.");
			strcpy(ymod[2],"0."); strcpy(ymod[3],"0.");
			strcpy(ymod[4],"0.");
		}
	}
	if (S_focus_page==0)
	{
		Fytog[0] = taxis->adjust_flag;
		traverse[FMODE_MODIFY] = 1;
		traverse[FMOD_UP] = traverse[FMOD_RGT] = traverse[FMOD_LFT] = Fytog[0];
		traverse[FMOD_FWD] = traverse[FMOD_ANG] = Fytog[0];
	}
	else
	{
		traverse[MOD_UP] = traverse[MOD_RGT] = traverse[MOD_LFT] = ytog[0];
		traverse[MOD_FWD] = traverse[MOD_ANG] = ytog[0];
	}
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
	if ((ytog[0] == 0)&&(S_option_first==1))
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
**    I_FUNCTION     : S_enable_buttons2()
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
static UU_LOGICAL S_enable_buttons2()
{
	int nc;
	UU_LOGICAL sec11,sec12,sec21, sec22,sec41, sec42;
	UU_LOGICAL sec51,sec52,sec53, sec54,sec55;
	UU_LOGICAL ifl;
	char tempstr[256];
	int	subscript;
	int status, rel_num;
	UM_int4 nclkey;
	UU_REAL rval;
/*
.....Tilt section
*/
	sec11 = sec12 = UU_TRUE;
	nc = (int)strlen(Stfwd);
	ul_strip_blanks(Stfwd, &nc);
	if (nc>0)
	{
		sec11 = UU_TRUE;
		ud_dispfrm_set_attribs(Sfrm2, TLT_FWD, UM_BLACK, UM_WHITE);
	}
	else
	{
		if (Sttog[0])
		{
			ud_dispfrm_set_attribs(Sfrm2, TLT_FWD, UM_BLACK, UM_RED);
			sec11 = UU_FALSE;
		}
		else
		{
			sec11 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm2, TLT_FWD, UM_BLACK, UM_WHITE);
		}
	}
	nc = (int)strlen(Strgt);
	ul_strip_blanks(Strgt, &nc);
	if (nc>0)
	{
		sec12 = UU_TRUE;
		ud_dispfrm_set_attribs(Sfrm2, TLT_RGT, UM_BLACK, UM_WHITE);
	}
	else
	{
		if (Sttog[0])
		{
			ud_dispfrm_set_attribs(Sfrm2, TLT_RGT, UM_BLACK, UM_RED);
			sec12 = UU_FALSE;
		}
		else
		{
			sec12 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm2, TLT_RGT, UM_BLACK, UM_WHITE);
		}
	}
/*
.....guide section
*/
	sec21 = sec22 = UU_TRUE;
	if (Sgtog[0])
	{
		nc = (int)strlen(Sggeo);
		ul_strip_blanks(Sggeo, &nc);
		if (nc>0)
		{
			strcpy(tempstr, Sggeo);
			ncl_parse_subnum (tempstr, &subscript);
			ul_to_upper(tempstr);
			status = ncl_vxchk (tempstr,subscript, &nclkey, &rel_num);
			if ((rel_num==NCL_LINE_REL) || (rel_num==NCL_CIRCLE_REL)
					|| (rel_num==NCL_CURVE_REL) || (rel_num==NCL_EVALCV_REL)
					|| (rel_num==UM_LINE_REL) || (rel_num==UM_CIRCLE_REL)
					|| (rel_num==UM_CONIC_REL) || (rel_num==UM_AGCRV_REL)
					|| (rel_num==UM_COMPCRV_REL) || (rel_num==UM_RBSPLCRV_REL)
					)
				sec21 = UU_TRUE;
			else
			{
				sec21 = UU_FALSE;
			}
		}
		else
		{
			sec21 = UU_FALSE;
		}
		if (sec21 == UU_TRUE)
		{
			ud_dispfrm_set_attribs(Sfrm2, GDE_GEO, UM_BLACK, UM_WHITE);
		}
		else
		{
			ud_dispfrm_set_attribs(Sfrm2, GDE_GEO, UM_BLACK, UM_RED);
		}
		nc = (int)strlen(Sgdis);
		ul_strip_blanks(Sgdis, &nc);
		if (nc>0)
		{
			sec22 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm2, GDE_DIS, UM_BLACK, UM_WHITE);
		}
		else
		{
			sec22 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm2, GDE_DIS, UM_BLACK, UM_RED);
		}
	}
	else
	{
		sec21 = sec22 = UU_TRUE;
		ud_dispfrm_set_attribs(Sfrm2, GDE_GEO, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(Sfrm2, GDE_DIS, UM_BLACK, UM_WHITE);
	}
/*
.....Lock section
*/
	sec41 = sec42 = UU_TRUE;
	if ((Sltog[0])&&(Sltog[1]!=2))
	{
		nc = (int)strlen(Slds1);
		ul_strip_blanks(Slds1, &nc);
		if (nc>0)
		{
			if ((ncl_get_scalar_value(Slds1, &rval)!=-1)&&(rval>=0))
			{
				sec41 = UU_TRUE;
				ud_dispfrm_set_attribs(Sfrm2, LCK_DIS, UM_BLACK, UM_WHITE);
			}
			else
			{
				sec41 = UU_FALSE;
				ud_dispfrm_set_attribs(Sfrm2, LCK_DIS, UM_BLACK, UM_RED);
			}
		}
		else
		{
			sec41 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm2, LCK_DIS, UM_BLACK, UM_RED);
		}
		nc = (int)strlen(Slds2);
		ul_strip_blanks(Slds2, &nc);
		if (nc>0)
		{
			if ((ncl_get_scalar_value(Slds2, &rval)!=-1)&&(rval>=0))
			{
				sec42 = UU_TRUE;
				ud_dispfrm_set_attribs(Sfrm2, LCK_MOV, UM_BLACK, UM_WHITE);
			}
			else if (Sltog[3])
			{
				sec42 = UU_FALSE;
				ud_dispfrm_set_attribs(Sfrm2, LCK_MOV, UM_BLACK, UM_RED);
			}
		}
		else
		{
			if (Sltog[3])
			{
				sec42 = UU_FALSE;
				ud_dispfrm_set_attribs(Sfrm2, LCK_MOV, UM_BLACK, UM_RED);
			}
			else
			{
				sec42 = UU_TRUE;
				ud_dispfrm_set_attribs(Sfrm2, LCK_MOV, UM_BLACK, UM_WHITE);
			}
		}
	}
	else
	{
		sec41 = sec42 = UU_TRUE;
		ud_dispfrm_set_attribs(Sfrm2, LCK_DIS, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(Sfrm2, LCK_MOV, UM_BLACK, UM_WHITE);
	}
/*
.....Modify section
*/
	sec51 = sec52 = sec53 = sec54 = sec55 = UU_TRUE;
	if (Sytog[0])
	{
		nc = (int)strlen(Symod[0]);
		ul_strip_blanks(Symod[0], &nc);
		if (nc>0)
		{
			sec51 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm2, MOD_UP, UM_BLACK, UM_WHITE);
		}
		else
		{
			sec51 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm2, MOD_UP, UM_BLACK, UM_RED);
		}
		nc = (int)strlen(Symod[1]);
		ul_strip_blanks(Symod[1], &nc);
		if (nc>0)
		{
			sec52 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm2, MOD_RGT, UM_BLACK, UM_WHITE);
		}
		else
		{
			sec52 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm2, MOD_RGT, UM_BLACK, UM_RED);
		}
		nc = (int)strlen(Symod[2]);
		ul_strip_blanks(Symod[2], &nc);
		if (nc>0)
		{
			sec53 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm2, MOD_LFT, UM_BLACK, UM_WHITE);
		}
		else
		{
			sec53 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm2, MOD_LFT, UM_BLACK, UM_RED);
		}
		nc = (int)strlen(Symod[3]);
		ul_strip_blanks(Symod[3], &nc);
		if (nc>0)
		{
			sec54 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm2, MOD_FWD, UM_BLACK, UM_WHITE);
		}
		else
		{
			sec54 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm2, MOD_FWD, UM_BLACK, UM_RED);
		}
		nc = (int)strlen(Symod[4]);
		ul_strip_blanks(Symod[4], &nc);
		if (nc>0)
		{
			sec55 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm2, MOD_ANG, UM_BLACK, UM_WHITE);
		}
		else
		{
			sec55 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm2, MOD_ANG, UM_BLACK, UM_RED);
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm2, MOD_UP, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(Sfrm2, MOD_RGT, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(Sfrm2, MOD_LFT, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(Sfrm2, MOD_FWD, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(Sfrm2, MOD_ANG, UM_BLACK, UM_WHITE);
	}
	if (sec11 && sec12)
	{
		if (Sttog[0])
			ud_form_section_color(Sfrm2,Soption[0],Sgreen,UU_TRUE);
		else
		{
			ud_form_section_color(Sfrm2,Soption[0],Sblack,UU_FALSE);
		}
	}
	else
	{
		ud_form_section_color(Sfrm2,Soption[0],Sred,UU_TRUE);
	}
	if (sec21 && sec22)
	{
		if (Sgtog[0])
			ud_form_section_color(Sfrm2,Soption[1],Sgreen,UU_TRUE);
		else
		{
			ud_form_section_color(Sfrm2,Soption[1],Sblack,UU_FALSE);
		}
	}
	else
	{
		ud_form_section_color(Sfrm2,Soption[1],Sred,UU_TRUE);
	}
	if (Sktog[0])
		ud_form_section_color(Sfrm2,Soption[2],Sgreen,UU_TRUE);
	else
	{
		ud_form_section_color(Sfrm2,Soption[2],Sblack,UU_FALSE);
	}

	if (sec41 && sec42)
	{
		if (Sltog[0])
			ud_form_section_color(Sfrm2,Soption[3],Sgreen,UU_TRUE);
		else
			ud_form_section_color(Sfrm2,Soption[3],Sblack,UU_FALSE);
	}
	else
	{
		ud_form_section_color(Sfrm2,Soption[3],Sred,UU_TRUE);
	}
	if (sec51 && sec52&& sec53&& sec54&& sec55)
	{
		if (Sytog[0])
			ud_form_section_color(Sfrm2,Soption[4],Sgreen,UU_TRUE);
		else
			ud_form_section_color(Sfrm2,Soption[4],Sblack,UU_FALSE);
	}
	else
	{
		ud_form_section_color(Sfrm2,Soption[4],Sred,UU_TRUE);
	}
/*
.....Set Action Buttons
*/
	ifl = sec11 && sec12 && sec21 && sec22 && sec41 && sec42;
	ifl = ifl && sec51 && sec52&& sec53&& sec54&& sec55;
	S_close_enable = ifl;
	ud_frm_enable_close(Sfrm2, ifl);
	ud_update_form(Sfrm2);
	return UU_TRUE;
}

/*********************************************************************
**    I_FUNCTION     : S_edit_option(fieldno, val, stat)
**       Handles the tool axis options edit fields.
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
static UD_FSTAT S_edit_option(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch (*fieldno)
	{
	case TLT_FWD:
	case TLT_RGT:
	case GDE_GEO:
	case GDE_DIS:
	case LCK_DIS:
	case LCK_MOV:
	case MOD_UP:
	case MOD_RGT:
	case MOD_LFT:
	case MOD_FWD:
	case MOD_ANG:
		S_enable_buttons2();
	}
	return(UD_FLDOK);
}
static void S_init_otions()
{
	int i;
	if (S_focus_page==1)
	{
		S_copy_tlaxis2(&T_taxis1, &Staxis);
		for (i=0;i<11;i++)
		{
			if (T_maxis1[i][0]!='\0')
			{
				strcpy(Smaxis[i], T_maxis1[i]);
			}
			else
			{
				Smaxis[i][0] = '\0';
			}
		}
	}
	else if (S_focus_page==2)
	{
		S_copy_tlaxis2(&T_taxis2, &Staxis);
		for (i=0;i<11;i++)
		{
			if (T_maxis2[i][0]!='\0')
			{
				strcpy(Smaxis[i], T_maxis2[i]);
			}
			else
			{
				Smaxis[i][0] = '\0';
			}
		}
	}
	else if (S_focus_page==3)
	{
		S_copy_tlaxis2(&T_taxis3, &Staxis);
		for (i=0;i<11;i++)
		{
			if (T_maxis3[i][0]!='\0')
			{
				strcpy(Smaxis[i], T_maxis3[i]);
			}
			else
			{
				Smaxis[i][0] = '\0';
			}
		}
	}
	else if (S_focus_page==4)
	{
		S_copy_tlaxis2(&T_taxis4, &Staxis);
		for (i=0;i<11;i++)
		{
			if (T_maxis4[i][0]!='\0')
			{
				strcpy(Smaxis[i], T_maxis4[i]);
			}
			else
			{
				Smaxis[i][0] = '\0';
			}
		}
	}
	else if (S_focus_page==5)
	{
		S_copy_tlaxis2(&T_taxis5, &Staxis);
		for (i=0;i<11;i++)
		{
			if (T_maxis5[i][0]!='\0')
			{
				strcpy(Smaxis[i], T_maxis5[i]);
			}
			else
			{
				Smaxis[i][0] = '\0';
			}
		}
	}
	else if (S_focus_page==6)
	{
		S_copy_tlaxis2(&T_taxis6, &Staxis);
		for (i=0;i<11;i++)
		{
			if (T_maxis6[i][0]!='\0')
			{
				strcpy(Smaxis[i], T_maxis6[i]);
			}
			else
			{
				Smaxis[i][0] = '\0';
			}
		}
	}
	else if (S_focus_page==7)
	{
		S_copy_tlaxis2(&T_taxis7, &Staxis);
		for (i=0;i<11;i++)
		{
			if (T_maxis7[i][0]!='\0')
			{
				strcpy(Smaxis[i], T_maxis7[i]);
			}
			else
			{
				Smaxis[i][0] = '\0';
			}
		}
	}
	else if (S_focus_page==8)
	{
		S_copy_tlaxis2(&T_taxis8, &Staxis);
		for (i=0;i<11;i++)
		{
			if (T_maxis8[i][0]!='\0')
			{
				strcpy(Smaxis[i], T_maxis8[i]);
			}
			else
			{
				Smaxis[i][0] = '\0';
			}
		}
	}
}
/*********************************************************************
**    E_FUNCTION     : S_display_modify_form()
**       Dsplay the Tool Axis Modify form
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void S_display_modify_form()
{
	int i, fieldno, stat, page;
	UD_DDATA val;
	static char traverse[] =
	{
		1,1,1,1,						/* TILT */
		1,1,1,1,1,1,					/* GUIDE CV */
		1,1,								/* GOUGCK */
		1,1,1,1,1,1,1,1, 			/* LOCK */
		1,1,1,1,1,1,1				/* MODIFY */
	};
	static char display[] =
	{
		1,1,1,1,						/* TILT */
		1,1,1,1,1,1,					/* GUIDE CV */
		1,1,								/* GOUGCK */
		1,1,1,1,1,1,1,1, 			/* LOCK */
		1,1,1,1,1,1,1,1				/* MODIFY */
	};
	static UD_METHOD methods[] =
	{		
		S_toggle_option, S_edit_option,S_edit_option,S_video_play2,
		S_toggle_option, S_edit_option,S_toggle_guide,S_edit_option,S_toggle_option,S_video_play2,
		S_toggle_option, S_video_play2,
		S_toggle_option, S_toggle_lock,S_toggle_option,S_edit_option,S_toggle_lock,S_toggle_option,
			S_edit_option, S_video_play2,
		S_toggle_option, S_edit_option,S_edit_option,S_edit_option,S_edit_option,S_edit_option, S_video_play2,
		OnClose2
	};

	static char called[] =
	{
		6,6,6,6,6,
		6,6,6,6,6,6,6,
		6,6,6,
		6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6
	};
	static int *ans[] =
	{
		&Sttog[0],(int *)Stfwd,(int *)Strgt, UU_NULL,
		&Sgtog[0],(int *)Sggeo,&Sgtog[1],(int *)Sgdis,&Sgtog[2],UU_NULL,
		&Sktog[0],UU_NULL,
		&Sltog[0],&Sltog[1],&Sltog[2],(int *)Slds1,&Sltog[3],&Sltog[4],
			(int *)Slds2, UU_NULL,
		&Sytog[0],(int *)Symod[0],(int *)Symod[1],(int *)Symod[2],(int *)Symod[3],
			(int *)Symod[4], UU_NULL
	};
	static int isub[]={8,7,6,10,9};
/*
.....reassign Staxis for 
*/
	S_init_otions();
	S_define_tilt(&Staxis,Sttog,Stfwd,Strgt,display,traverse,Sreentry);
	S_define_guide(&Staxis,Sgtog,Sggeo,Sgdis,display,traverse,Sreentry);
	S_define_gougck(&Staxis,Sktog,display,traverse,Sreentry);
	S_define_lock(&Staxis,Sltog,Slds1,Slds2,display,traverse,Sreentry);
	S_define_modify(&Staxis,Sytog,Symod,display,traverse,Sreentry);

	if (S_option_first==0)
	{
		strcpy(Stfwd, Smaxis[0]);
		strcpy(Strgt, Smaxis[1]);
		strcpy(Sggeo, Smaxis[2]);
		strcpy(Sgdis, Smaxis[3]);
		strcpy(Slds1, Smaxis[4]);
		strcpy(Slds2, Smaxis[5]);
		for (i=0;i<5;i++)
		{
			strcpy(Symod[i], Smaxis[isub[i]]);
		}
		page = S_focus_page-1;
		if (page>=0)
		{
			Sttog[0] = T_ttog[page];
			traverse[TLT_FWD] = traverse[TLT_RGT] = Sttog[0];
			Sgtog[0] = T_gtog[page][0];
			Sgtog[1] = T_gtog[page][1];
			Sgtog[2] = T_gtog[page][2];
			traverse[GDE_GEO] = traverse[GDE_CTN] = Sgtog[0];
			traverse[GDE_DIS] = traverse[GDE_CON] = Sgtog[0];
			if (Sgtog[1] == 1) 
			{
				traverse[GDE_DIS] = traverse[GDE_CON] = 0;
			}
			Sktog[0] = T_ktog[page];
			Sltog[0] =T_ltog[page][0];
			Sltog[1] =T_ltog[page][1];
			Sltog[2] =T_ltog[page][2];
			Sltog[3] =T_ltog[page][3];
			Sltog[4] =T_ltog[page][4];
			traverse[LCK_TYP] = Sltog[0];
			if (Sltog[1] == 2)
			{
				for (i=LCK_LIN;i<=LCK_MOV;i++) traverse[i] = 0;
			}
			else
			{
				for (i=LCK_LIN;i<=LCK_TRN;i++) traverse[i] = Sltog[0];
				traverse[LCK_FAN] = traverse[LCK_MOV] = Sltog[3] && Sltog[0];
			}
			Sytog[0] = T_ytog[page];
			traverse[MOD_UP] = traverse[MOD_RGT] = traverse[MOD_LFT] = Sytog[0];
			traverse[MOD_FWD] = traverse[MOD_ANG] = Sytog[0];
		}
	}
	else
	{
		page = S_focus_page-1;
		if (page>=0)
		{
			T_ttog[page] = Sttog[0];
			T_gtog[page][0] = Sgtog[0];
			T_gtog[page][1] = Sgtog[1];
			T_gtog[page][2] = Sgtog[2];
			T_ktog[page] = Sktog[0];
			T_ltog[page][0] = Sltog[0];
			T_ltog[page][1] = Sltog[1];
			T_ltog[page][2] = Sltog[2];
			T_ltog[page][3] = Sltog[3];
			T_ltog[page][4] = Sltog[4];
			T_ytog[page] = Sytog[0];
		}
	}
	Sfrm2 = ud_form_display1("ntlaxis_modify.frm",ans,ans,methods,called,display,traverse);
	if (Sfrm2 != 0)
	{
		Sactive2 = UU_TRUE;
		fieldno = stat = 0;
		S_init_form2(&fieldno,&val,stat);
		S_option_first = 0;
	}
}
/*********************************************************************
**    I_FUNCTION     : S_OnModify(fieldno, val, stat)
**       Handles the tool axis Modify toggle fields.
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
static UD_FSTAT S_OnModify(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (val->frmint[0]==1)
		S_display_modify_form();
/*
......return *fieldno = -1;
......to let the main form not redisplay
*/
	*fieldno = -1;
	return(UD_FLDOK);
}

static UD_FSTAT S_OnModifyBut(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (STlx_mod)
		S_display_modify_form();
	else
	{
		STlx_mod = 1;
		ud_dispfrm_update_answer(Sfrm1, TAX_MODY, &STlx_mod);
		ud_update_form(Sfrm1);
	}
/*
......return *fieldno = -1;
......to let the main form not redisplay
*/
	*fieldno = -1;
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

	inc = -1;
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
			ud_getfrm_field(Sfrm2,GDE_CTN,data,UU_FALSE);
			if (data.frmint[0] == 1)
			{
				ien = GDE_CTN;
				ud_setfrm_traverse_mask(Sfrm2,GDE_DIS,UU_FALSE);
				ud_setfrm_traverse_mask(Sfrm2,GDE_CON,UU_FALSE);
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
			ud_getfrm_field(Sfrm2,LCK_TYP,data,UU_FALSE);
			if (data.frmint[0] == 2)
			{
				ist = LCK_LIN;
				itog = 0;
				ud_setfrm_traverse_mask(Sfrm2,LCK_TYP,1);
			}
			else
			{
				data.frmint = &i;
				ud_getfrm_field(Sfrm2,LCK_TRN,data,UU_FALSE);
				if (data.frmint[0] == 0)
				{
					ien = LCK_TRN;
					ud_setfrm_traverse_mask(Sfrm2,LCK_FAN,UU_FALSE);
					ud_setfrm_traverse_mask(Sfrm2,LCK_MOV,UU_FALSE);
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
	if (inc!=-1)
	{
		if (val->frmint[0]==0)
			ud_form_section_color(Sfrm2,Soption[inc],Sblack, UU_FALSE);
		else
			ud_form_section_color(Sfrm2,Soption[inc],Sgreen, UU_FALSE);
		for (i=ist;i<=ien;i++) ud_setfrm_traverse_mask(Sfrm2,i,itog);
	}
	S_enable_buttons2();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_toggle_fixed_mod(fieldno, val, stat)
**       Handles the FIXED Modification toggle fields.
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
static UD_FSTAT S_toggle_fixed_mod(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (val->frmint[0])
	{
		ud_setfrm_traverse_mask(Sfrm1,FMOD_UP,UU_TRUE);
		ud_setfrm_traverse_mask(Sfrm1,FMOD_RGT,UU_TRUE);
		ud_setfrm_traverse_mask(Sfrm1,FMOD_LFT,UU_TRUE);
		ud_setfrm_traverse_mask(Sfrm1,FMOD_FWD,UU_TRUE);
		ud_setfrm_traverse_mask(Sfrm1,FMOD_ANG,UU_TRUE);
	}
	else
	{
		ud_setfrm_traverse_mask(Sfrm1,FMOD_UP,UU_FALSE);
		ud_setfrm_traverse_mask(Sfrm1,FMOD_RGT,UU_FALSE);
		ud_setfrm_traverse_mask(Sfrm1,FMOD_LFT,UU_FALSE);
		ud_setfrm_traverse_mask(Sfrm1,FMOD_FWD,UU_FALSE);
		ud_setfrm_traverse_mask(Sfrm1,FMOD_ANG,UU_FALSE);
	}
	S_enable_buttons();
	return(UD_FLDOK);
}

static UD_FSTAT S_video_play(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	S_enable_buttons();
	return(UD_FLDOK);
}


static UD_FSTAT S_video_play2(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	S_enable_buttons2();
	return(UD_FLDOK);
}

static UD_FSTAT S_toggle_main(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_edit_main_mod(fieldno, val, stat)
**       Handles the FIXED Modification toggle fields.
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
static UD_FSTAT S_edit_main_mod(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	S_enable_buttons();
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
	S_enable_buttons();
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
			if (data.frmint[0])
			{
				ud_setfrm_traverse_mask(Sfrm1,TAN_VEC,UU_TRUE);
				ud_getfrm_field(Sfrm1,TAN_VEC,data,UU_TRUE);
				if (strlen(str)!=0)
					ud_setfrm_traverse_mask(Sfrm1,TAN_MNT,UU_TRUE);
				else
					ud_setfrm_traverse_mask(Sfrm1,TAN_MNT,UU_FALSE);
			}
		}
		break;
	case TAN_4AX:
		ud_setfrm_traverse_mask(Sfrm1,TAN_VEC,val->frmint[0]);
		data.frmstr = str;
		ud_getfrm_field(Sfrm1,TAN_VEC,data,UU_TRUE);
		if (strlen(str)!=0)
			ud_setfrm_traverse_mask(Sfrm1,TAN_MNT,UU_TRUE);
		else
			ud_setfrm_traverse_mask(Sfrm1,TAN_MNT,UU_FALSE);
		break;
	}
	S_enable_buttons();
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
	switch(*fieldno)
	{
	case FAN_SMO: 
		ud_setfrm_traverse_mask(Sfrm1,FAN_DEG,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,FAN_RAT,val->frmint[0]);
		break;
	}
	S_enable_buttons();
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
	switch(*fieldno)
	{
	case COM_PARL:
		ud_setfrm_traverse_mask(Sfrm1,COM_SURF,!val->frmint[0]);
		break;
	case COM_SMO: 
		ud_setfrm_traverse_mask(Sfrm1,COM_DEG,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,COM_RAT,val->frmint[0]);
		break;
	}
	S_enable_buttons();
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
	switch(*fieldno)
	{
	case NOR_4AX: 
		ud_setfrm_traverse_mask(Sfrm1,NOR_VEC,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,NOR_MNT,val->frmint[0]);
		break;
	}
	S_enable_buttons();
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
	switch(*fieldno)
	{
	case ATA_4AX: 
		ud_setfrm_traverse_mask(Sfrm1,ATA_VEC,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,ATA_MNT,val->frmint[0]);
		break;
	}
	S_enable_buttons();
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
	switch(*fieldno)
	{
	case INT_SMO: 
		ud_setfrm_traverse_mask(Sfrm1,INT_DEG,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm1,INT_RAT,val->frmint[0]);
		break;
	}
	S_enable_buttons();
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
	switch(*fieldno)
	{
	case GDE_CTN: 
		if (val->frmint[0] == 1)
		{
			ud_setfrm_traverse_mask(Sfrm2,GDE_DIS,0);
			ud_setfrm_traverse_mask(Sfrm2,GDE_CON,0);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm2,GDE_DIS,1);
			ud_setfrm_traverse_mask(Sfrm2,GDE_CON,1);
		}
		break;
	}
	S_enable_buttons2();
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
	int status, nc;
	UU_LOGICAL sec1,sec21, sec22,sec23, sec31, sec32, sec33;
	UU_LOGICAL sec11,sec12,sec13, sec14,sec15;
	UU_LOGICAL sec41,sec42, sec43,sec44, sec45, sec46;
	UU_LOGICAL sec51,sec52,sec61, sec62, sec63, sec64,
		sec7, sec81, sec82, sec91, sec92, sec93;
	UU_LOGICAL ifl;
	char tempstr[256], string[256];
	int	subscript;
	int rel_num;
	UM_int4 nclkey;
	UU_REAL rval;
/*
.....Fixed section
*/
	sec1 = UU_TRUE;
	nc = (int)strlen(Sfvec);
	ul_strip_blanks(Sfvec, &nc);
	if (nc>0)
	{
/*
.....check if the string is a vector/point vector
*/
		if (Sftog[1]==1)
		{
			strcpy(tempstr, Sfvec);
			ncl_parse_subnum (tempstr, &subscript);
			ul_to_upper(tempstr);
			status = ncl_vxchk (tempstr,subscript, &nclkey, &rel_num);
			if ((rel_num==NCL_VECTOR_REL) || (rel_num==NCL_POINTVEC_REL))
				sec1 = UU_TRUE;
			else
			{
				status = ncl_parse_scalar_values(tempstr, string, UD_SCAVEC2);
				if (status==-1)
				{
					sec1 = UU_FALSE;
				}
			}
		}
		else
		{
			sec1 = UU_TRUE;
		}
	}
	else
	{
		if (Sftog[1]==1)
		{
			sec1 = UU_FALSE;
		}
		else
		{
			sec1 = UU_TRUE;
		}
	}
	if (sec1!=UU_TRUE)
	{
		ud_dispfrm_set_attribs(Sfrm1, FXD_VEC, UM_BLACK, UM_RED);
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm1, FXD_VEC, UM_BLACK, UM_WHITE);
	}
/*
.....Modify flag
*/
	sec11 = sec12 = sec13 = sec14 = sec15 = UU_TRUE;
	if (Fytog[0])
	{
		nc = (int)strlen(Fymod[0]);
		ul_strip_blanks(Fymod[0], &nc);
		if (nc>0)
		{
			sec11 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, FMOD_UP, UM_BLACK, UM_WHITE);
		}
		else
		{
			sec11 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, FMOD_UP, UM_BLACK, UM_RED);
		}
		nc = (int)strlen(Fymod[1]);
		ul_strip_blanks(Fymod[1], &nc);
		if (nc>0)
		{
			sec12 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, FMOD_RGT, UM_BLACK, UM_WHITE);
		}
		else
		{
			sec12 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, FMOD_RGT, UM_BLACK, UM_RED);
		}
		nc = (int)strlen(Fymod[2]);
		ul_strip_blanks(Fymod[2], &nc);
		if (nc>0)
		{
			sec13 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, FMOD_LFT, UM_BLACK, UM_WHITE);
		}
		else
		{
			sec13 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, FMOD_LFT, UM_BLACK, UM_RED);
		}
		nc = (int)strlen(Fymod[3]);
		ul_strip_blanks(Fymod[3], &nc);
		if (nc>0)
		{
			sec14 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, FMOD_FWD, UM_BLACK, UM_WHITE);
		}
		else
		{
			sec14 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, FMOD_FWD, UM_BLACK, UM_RED);
		}
		nc = (int)strlen(Fymod[4]);
		ul_strip_blanks(Fymod[4], &nc);
		if (nc>0)
		{
			sec15 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, FMOD_ANG, UM_BLACK, UM_WHITE);
		}
		else
		{
			sec15 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, FMOD_ANG, UM_BLACK, UM_RED);
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm1, FMOD_UP, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(Sfrm1, FMOD_RGT, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(Sfrm1, FMOD_LFT, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(Sfrm1, FMOD_FWD, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(Sfrm1, FMOD_ANG, UM_BLACK, UM_WHITE);
	}
/*
.....Tanto DS
*/
	sec21 = sec22 = sec23 = UU_TRUE;
	nc = (int)strlen(Sdhgt);
	ul_strip_blanks(Sdhgt, &nc);
	if (nc>0)
	{
		sec21 = UU_TRUE;
		ud_dispfrm_set_attribs(Sfrm1, TAN_HGT, UM_BLACK, UM_WHITE);
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm1, TAN_HGT, UM_BLACK, UM_RED);
		sec21 = UU_FALSE;
	}
	nc = (int)strlen(Sdsurf);
	ul_strip_blanks(Sdsurf, &nc);
	if (nc>0)
	{
		if (Sdtog[1]==0)
		{
			strcpy(tempstr, Sdsurf);
			ncl_parse_subnum (tempstr, &subscript);
			ul_to_upper(tempstr);
			status = ncl_vxchk (tempstr,subscript, &nclkey, &rel_num);
			if ((rel_num==NCL_SURF_REL) || (rel_num==NCL_PLN_REL)
				|| (rel_num==UM_RBSPLSRF_REL) || (rel_num==NCL_TRIMSF_REL)
				|| (rel_num==NCL_QUILTSURF_REL) || (rel_num==NCL_REVSURF_REL)
				|| (rel_num==NCL_NETSF_REL) || (rel_num==NCL_MESHSURF_REL)
				|| (rel_num==NCL_EVALSF_REL)
				|| (rel_num==UM_AGSRF_REL) || (rel_num==UM_RBSPLSRF_REL))
				sec22 = UU_TRUE;
			else
			{
				sec22 = UU_FALSE;
			}
		}
		else
			sec22 = UU_TRUE;
	}
	else
	{
/*
.....allow empty surface
*/
		sec22 = UU_TRUE;
	}
	if (sec22==UU_FALSE)
	{
		ud_dispfrm_set_attribs(Sfrm1, TAN_SURF, UM_BLACK, UM_RED);
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm1, TAN_SURF, UM_BLACK, UM_WHITE);
	}
	nc = (int)strlen(Sdvec);
	ul_strip_blanks(Sdvec, &nc);
	if (Sdtog[1]==0)
	{	
		if (nc>0)
		{
			strcpy(tempstr, Sdvec);
			ncl_parse_subnum (tempstr, &subscript);
			ul_to_upper(tempstr);
			status = ncl_vxchk (tempstr,subscript, &nclkey, &rel_num);
			if ((rel_num==NCL_VECTOR_REL) || (rel_num==NCL_POINTVEC_REL))
			{
				sec23 = UU_TRUE;
				ud_setfrm_traverse_mask(Sfrm1,TAN_MNT,UU_TRUE);
			}
			else
			{
				status = ncl_parse_scalar_values(tempstr, string, UD_SCAVEC2);
				if (status==-1)
				{
					sec23 = UU_FALSE;
					ud_setfrm_traverse_mask(Sfrm1,TAN_MNT,UU_FALSE);
				}
				else
					ud_setfrm_traverse_mask(Sfrm1,TAN_MNT,UU_TRUE);
			}
		}
		else
		{
			if (Sdtog[2]==0)
				sec23 = UU_TRUE;
			else
				sec23 = UU_FALSE;
			ud_setfrm_traverse_mask(Sfrm1,TAN_MNT,UU_FALSE);
		}
	}
	else
	{
		sec23 = UU_TRUE;
		ud_setfrm_traverse_mask(Sfrm1,TAN_MNT,UU_FALSE);
	}
	if (sec23==UU_TRUE)
	{
		ud_dispfrm_set_attribs(Sfrm1, TAN_VEC, UM_BLACK, UM_WHITE);
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm1, TAN_VEC, UM_BLACK, UM_RED);
	}
/*
.....Fan
*/
	sec31 = sec32 = sec33 = UU_TRUE;
	nc = (int)strlen(Snhgt);
	ul_strip_blanks(Snhgt, &nc);
	if (nc>0)
	{
		if (ncl_get_scalar_value(Snhgt, &rval)==-1)
		{
			sec31 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, FAN_HGT, UM_BLACK, UM_RED);
		}
		else
		{	sec31 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, FAN_HGT, UM_BLACK, UM_WHITE);
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm1, FAN_HGT, UM_BLACK, UM_RED);
		sec31 = UU_FALSE;
	}
	nc = (int)strlen(Sndeg);
	ul_strip_blanks(Sndeg, &nc);
	if (nc>0)
	{
		if ((ncl_get_scalar_value(Sndeg, &rval)==-1)&&(Sntog[2]))
			sec32 = UU_FALSE;
		else if ((rval<2)&&(Sntog[2]))
			sec32 = UU_FALSE;
		else
		{
/*
.....must be integer
*/
			sprintf(Sndeg, "%d", (int)(rval+0.5));
		}
	}
	else
	{
		if (Sntog[2])
		{
			sec32 = UU_FALSE;
		}
		else
		{
			sec32 = UU_TRUE;
		}
	}
	if (sec32==UU_TRUE)
	{
		ud_dispfrm_set_attribs(Sfrm1, FAN_DEG, UM_BLACK, UM_WHITE);
	}
	else
		ud_dispfrm_set_attribs(Sfrm1, FAN_DEG, UM_BLACK, UM_RED);

	nc = (int)strlen(Snrat);
	ul_strip_blanks(Snrat, &nc);
	if (nc>0)
	{
		if ((ncl_get_scalar_value(Snrat, &rval)==-1)&&(Sntog[2]))
			sec33 = UU_FALSE;
		if ((rval<=1)&&(rval>=0))
			sec33 = UU_TRUE;
		else if (Sntog[2])
			sec33 = UU_FALSE;
	}
	else
	{
		if (Sntog[2])
		{
			sec33 = UU_FALSE;
		}
		else
		{
			sec33 = UU_TRUE;
		}
	}
	if (sec33==UU_TRUE)
	{
		ud_dispfrm_set_attribs(Sfrm1, FAN_RAT, UM_BLACK, UM_WHITE);
	}
	else
		ud_dispfrm_set_attribs(Sfrm1, FAN_RAT, UM_BLACK, UM_RED);
/*
......Combine
*/
	sec41 = sec42 = sec43 = sec44 = sec45 = sec46 = UU_TRUE;
	nc = (int)strlen(Schgt);
	ul_strip_blanks(Schgt, &nc);
	if (nc>0)
	{
		sec41 = UU_TRUE;
		ud_dispfrm_set_attribs(Sfrm1, COM_HGT, UM_BLACK, UM_WHITE);
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm1, COM_HGT, UM_BLACK, UM_RED);
		sec41 = UU_FALSE;
	}

	nc = (int)strlen(Sclvd);
	ul_strip_blanks(Sclvd, &nc);
	if (nc>0)
	{
		status = ncl_get_scalar_value(Sclvd, &rval);
		if ((status==-1)||(rval<0))
		{
			sec42 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, COM_LVD, UM_BLACK, UM_RED);
		}
		else
		{
			sec42 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, COM_LVD, UM_BLACK, UM_WHITE);
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm1, COM_LVD, UM_BLACK, UM_RED);
		sec42 = UU_FALSE;
	}

	nc = (int)strlen(Scapd);
	ul_strip_blanks(Scapd, &nc);
	if (nc>0)
	{
		status = ncl_get_scalar_value(Scapd, &rval);
		if ((status==-1)||(rval<0))
		{
			ud_dispfrm_set_attribs(Sfrm1, COM_APD, UM_BLACK, UM_RED);
			sec43 = UU_FALSE;
		}
		else
		{
			sec43 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, COM_APD, UM_BLACK, UM_WHITE);
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm1, COM_APD, UM_BLACK, UM_RED);
		sec43 = UU_FALSE;
	}
	nc = (int)strlen(Scsurf);
	ul_strip_blanks(Scsurf, &nc);
	if (nc>0)
	{
		if (Sctog[1]==1)
		{
			sec44 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, COM_SURF, UM_BLACK, UM_WHITE);
		}
		else
		{
/*
.....allow empty, but if there is a input, it must be Plane and surface
*/
			strcpy(tempstr, Scsurf);
			ncl_parse_subnum (tempstr, &subscript);
			ul_to_upper(tempstr);
			status = ncl_vxchk (tempstr,subscript, &nclkey, &rel_num);
			if ((rel_num==NCL_SURF_REL) || (rel_num==NCL_PLN_REL)
				|| (rel_num==UM_RBSPLSRF_REL) || (rel_num==NCL_TRIMSF_REL)
				|| (rel_num==NCL_QUILTSURF_REL) || (rel_num==NCL_REVSURF_REL)
				|| (rel_num==NCL_NETSF_REL) || (rel_num==NCL_MESHSURF_REL)
				|| (rel_num==NCL_EVALSF_REL)
				|| (rel_num==UM_AGSRF_REL) || (rel_num==UM_RBSPLSRF_REL))
				sec44 = UU_TRUE;
			else
			{
				sec44 = UU_FALSE;
			}
		}
	}
	else
	{
/*
.....allow empty for surface
*/
		sec44 = UU_TRUE;
	}
	if (sec44==UU_FALSE)
	{
		ud_dispfrm_set_attribs(Sfrm1, COM_SURF, UM_BLACK, UM_RED);
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm1, COM_SURF, UM_BLACK, UM_WHITE);
	}

	nc = (int)strlen(Scdeg);
	ul_strip_blanks(Scdeg, &nc);
	if (nc>0)
	{
		if ((ncl_get_scalar_value(Scdeg, &rval)==-1)&&(Sctog[3]))
		{
			sec45 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, COM_DEG, UM_BLACK, UM_RED);
		}
		else if ((rval<2)&&(Sctog[3]))
		{
			sec45 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, COM_DEG, UM_BLACK, UM_RED);
		}
		else
		{
			sec45 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, COM_DEG, UM_BLACK, UM_WHITE);
/*
.....must be integer
*/
			sprintf(Scdeg, "%d", (int)(rval+0.5));
		}
	}
	else
	{
		if (Sctog[3])
		{
			ud_dispfrm_set_attribs(Sfrm1, COM_DEG, UM_BLACK, UM_RED);
			sec45 = UU_FALSE;
		}
		else
		{
			sec45 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, COM_DEG, UM_BLACK, UM_WHITE);
		}
	}
	nc = (int)strlen(Scrat);
	ul_strip_blanks(Scrat, &nc);
	if (nc>0)
	{
		if ((ncl_get_scalar_value(Scrat, &rval)==-1)&&(Sctog[3]))
		{
			sec46 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, COM_RAT, UM_BLACK, UM_RED);
		}
		else if (((rval<0)||(rval>1))&&(Sctog[3]))
		{
			sec46 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, COM_RAT, UM_BLACK, UM_RED);
		}
		else
		{
			sec46 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, COM_RAT, UM_BLACK, UM_WHITE);
		}
	}
	else
	{
		if (Sctog[3])
		{
			ud_dispfrm_set_attribs(Sfrm1, COM_RAT, UM_BLACK, UM_RED);
			sec46 = UU_FALSE;
		}
		else
		{
			sec46 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, COM_RAT, UM_BLACK, UM_WHITE);
		}
	}
/*
......Normal PS
*/
	sec51 = sec52 = UU_TRUE;
	nc = (int)strlen(Smsurf);
	ul_strip_blanks(Smsurf, &nc);
	if (nc>0)
	{
		strcpy(tempstr, Smsurf);
		ncl_parse_subnum (tempstr, &subscript);
		ul_to_upper(tempstr);
		status = ncl_vxchk (tempstr,subscript, &nclkey, &rel_num);
		if ((rel_num==NCL_SURF_REL) || (rel_num==NCL_PLN_REL)
			|| (rel_num==UM_RBSPLSRF_REL) || (rel_num==NCL_TRIMSF_REL)
			|| (rel_num==NCL_QUILTSURF_REL) || (rel_num==NCL_REVSURF_REL)
			|| (rel_num==NCL_NETSF_REL) || (rel_num==NCL_MESHSURF_REL)
			|| (rel_num==NCL_EVALSF_REL)
			|| (rel_num==UM_AGSRF_REL) || (rel_num==UM_RBSPLSRF_REL))
		{
			sec51 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, NOR_SURF, UM_BLACK, UM_WHITE);
		}
		else
		{
			sec51 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, NOR_SURF, UM_BLACK, UM_RED);
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm1, NOR_SURF, UM_BLACK, UM_WHITE);
		sec51 = UU_TRUE;
	}
	nc = (int)strlen(Smvec);
	ul_strip_blanks(Smvec, &nc);
	if (nc>0)
	{			
		if (Smtog[1])
		{
			strcpy(tempstr, Smvec);
			ncl_parse_subnum (tempstr, &subscript);
			ul_to_upper(tempstr);
			status = ncl_vxchk (tempstr,subscript, &nclkey, &rel_num);
			if ((rel_num==NCL_VECTOR_REL) || (rel_num==NCL_POINTVEC_REL))
			{
				sec52 = UU_TRUE;
				ud_dispfrm_set_attribs(Sfrm1, NOR_VEC, UM_BLACK, UM_WHITE);
			}
			else
			{
				status = ncl_parse_scalar_values(tempstr, string, UD_SCAVEC2);
				if (status==-1)
				{
					sec52 = UU_FALSE;
					ud_dispfrm_set_attribs(Sfrm1, NOR_VEC, UM_BLACK, UM_RED);
				}
				else
				{
					sec52 = UU_TRUE;
					ud_dispfrm_set_attribs(Sfrm1, NOR_VEC, UM_BLACK, UM_WHITE);
				}
			}
		}
		else
		{
			sec52 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, NOR_VEC, UM_BLACK, UM_WHITE);
		}
	}
	else
	{
		if (Smtog[1])
/*
.....Vector required
*/
		{
			sec52 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, NOR_VEC, UM_BLACK, UM_RED);
		}
		else
		{
			sec52 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, NOR_VEC, UM_BLACK, UM_WHITE);
		}
	}
/*
......Atangle PS
*/
	sec61 = sec62 = sec63 = sec64 = UU_TRUE;
	nc = (int)strlen(Saang);
	ul_strip_blanks(Saang, &nc);
	if (nc>0)
	{
		if (ncl_get_scalar_value(Saang, &rval)!=-1)
		{
			sec61 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, ATA_ANG, UM_BLACK, UM_WHITE);
		}
		else
		{
			sec61 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, ATA_ANG, UM_BLACK, UM_RED);
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm1, ATA_ANG, UM_BLACK, UM_RED);
		sec61 = UU_FALSE;
	}

	nc = (int)strlen(Sadis);
	ul_strip_blanks(Sadis, &nc);
	if (nc>0)
	{
		if ((ncl_get_scalar_value(Sadis, &rval)!=-1)&&(rval>=0))
		{
			sec62 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, ATA_CLD, UM_BLACK, UM_WHITE);
		}
		else
		{
			ud_dispfrm_set_attribs(Sfrm1, ATA_CLD, UM_BLACK, UM_RED);
			sec62 = UU_FALSE;
		}
	}
	else
	{
/*
.....optional, can be empty
*/
		sec62 = UU_TRUE;
		ud_dispfrm_set_attribs(Sfrm1, ATA_CLD, UM_BLACK, UM_WHITE);
	}

	nc = (int)strlen(Sasurf);
	ul_strip_blanks(Sasurf, &nc);
	if (nc>0)
	{
/*
.....allow empty, but if there is a input, it must be Plane or surface
*/
		strcpy(tempstr, Sasurf);
		ncl_parse_subnum (tempstr, &subscript);
		ul_to_upper(tempstr);
		status = ncl_vxchk (tempstr,subscript, &nclkey, &rel_num);
		if ((rel_num==NCL_SURF_REL) || (rel_num==NCL_PLN_REL)
				|| (rel_num==UM_RBSPLSRF_REL) || (rel_num==NCL_TRIMSF_REL)
				|| (rel_num==NCL_QUILTSURF_REL) || (rel_num==NCL_REVSURF_REL)
				|| (rel_num==NCL_NETSF_REL) || (rel_num==NCL_MESHSURF_REL)
				|| (rel_num==NCL_EVALSF_REL)
				|| (rel_num==UM_AGSRF_REL) || (rel_num==UM_RBSPLSRF_REL))
		{
			sec63 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, ATA_SURF, UM_BLACK, UM_WHITE);
		}
		else
		{
			sec63 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, ATA_SURF, UM_BLACK, UM_RED);
		}
	}
	else
	{
/*
.....optional, can be empty
*/
		sec63 = UU_TRUE;
		ud_dispfrm_set_attribs(Sfrm1, ATA_SURF, UM_BLACK, UM_WHITE);
	}

	nc = (int)strlen(Savec);
	ul_strip_blanks(Savec, &nc);
	if (nc>0)
	{
/*
.....check if vector or point
*/
		if (Satog[2])
		{
			strcpy(tempstr, Savec);
			ncl_parse_subnum (tempstr, &subscript);
			ul_to_upper(tempstr);
			status = ncl_vxchk (tempstr,subscript, &nclkey, &rel_num);
			if ((rel_num==NCL_VECTOR_REL) || (rel_num==NCL_POINTVEC_REL))
			{
				sec64 = UU_TRUE;
				ud_dispfrm_set_attribs(Sfrm1, ATA_VEC, UM_BLACK, UM_WHITE);
			}
			else
			{
				status = ncl_parse_scalar_values(tempstr, string, UD_SCAVEC2);
				if (status==-1)
				{
					sec64 = UU_FALSE;
					ud_dispfrm_set_attribs(Sfrm1, ATA_VEC, UM_BLACK, UM_RED);
				}
				else
				{
					sec64 = UU_TRUE;
					ud_dispfrm_set_attribs(Sfrm1, ATA_VEC, UM_BLACK, UM_WHITE);
				}
			}
		}
		else
		{
			sec64 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, ATA_VEC, UM_BLACK, UM_WHITE);
		}
	}
	else
	{
		if (Satog[2])
		{
			ud_dispfrm_set_attribs(Sfrm1, ATA_VEC, UM_BLACK, UM_RED);
			sec64 = UU_FALSE;
		}
		else
		{
			sec64 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, ATA_VEC, UM_BLACK, UM_WHITE);
		}
	}
/*
......Thru Point
*/
	sec7 = UU_TRUE;
	nc = (int)strlen(Spgeo);
	ul_strip_blanks(Spgeo, &nc);
	if (nc>0)
	{
/*
......have to be a point/pv
*/
		strcpy(tempstr, Spgeo);
		ncl_parse_subnum (tempstr, &subscript);
		ul_to_upper(tempstr);
		status = ncl_vxchk (tempstr,subscript, &nclkey, &rel_num);
		if ((rel_num==NCL_POINT_REL) || (rel_num==NCL_POINTVEC_REL) || (rel_num==UM_POINT_REL))
		{
			sec7 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, PNT_GEO, UM_BLACK, UM_WHITE);
		}
		else
		{
			status = ncl_parse_scalar_values(tempstr, string, UD_SCACART2);
			if (status==-1)
			{
				sec7 = UU_FALSE;
				ud_dispfrm_set_attribs(Sfrm1, PNT_GEO, UM_BLACK, UM_RED);
			}
			else
			{
				sec7 = UU_TRUE;
				ud_dispfrm_set_attribs(Sfrm1, PNT_GEO, UM_BLACK, UM_WHITE);
			}
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm1, PNT_GEO, UM_BLACK, UM_RED);
		sec7 = UU_FALSE;
	}
/*
.....Thru Curve
*/
	sec81 = sec82 = UU_TRUE;
	nc = (int)strlen(Svgeo);
	ul_strip_blanks(Svgeo, &nc);
	if (nc>0)
	{
		strcpy(tempstr, Svgeo);
		ncl_parse_subnum (tempstr, &subscript);
		ul_to_upper(tempstr);
		status = ncl_vxchk (tempstr,subscript, &nclkey, &rel_num);
		if ((rel_num==NCL_LINE_REL) || (rel_num==NCL_CIRCLE_REL)
			|| (rel_num==NCL_CURVE_REL) || (rel_num==NCL_EVALCV_REL)
			|| (rel_num==UM_LINE_REL) || (rel_num==UM_CIRCLE_REL)
			|| (rel_num==UM_CONIC_REL) || (rel_num==UM_AGCRV_REL)
			|| (rel_num==UM_COMPCRV_REL) || (rel_num==UM_RBSPLCRV_REL))
		{
			ud_dispfrm_set_attribs(Sfrm1, CRV_GEO, UM_BLACK, UM_WHITE);
			sec81 = UU_TRUE;
		}
		else
		{
			ud_dispfrm_set_attribs(Sfrm1, CRV_GEO, UM_BLACK, UM_RED);
			sec81 = UU_FALSE;
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm1, CRV_GEO, UM_BLACK, UM_RED);
		sec81 = UU_FALSE;
	}
	nc = (int)strlen(Svdis);
	ul_strip_blanks(Svdis, &nc);
	if (nc>0)
	{
		if (ncl_get_scalar_value(Svdis, &rval)!=-1)
		{
			sec82 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, CRV_DIS, UM_BLACK, UM_WHITE);
		}
		else
		{
			ud_dispfrm_set_attribs(Sfrm1, CRV_DIS, UM_BLACK, UM_RED);
			sec82 = UU_FALSE;
		}
	}
	else
	{
/*
.....allow empty, optional item
*/
		ud_dispfrm_set_attribs(Sfrm1, CRV_DIS, UM_BLACK, UM_WHITE);
		sec82 = UU_TRUE;
	}
/*
.....Interpolate
*/
	sec91 = sec92 = sec93 = UU_TRUE;
	nc = (int)strlen(Srgeo);
	ul_strip_blanks(Srgeo, &nc);
	if (nc>0)
	{
		strcpy(tempstr, Srgeo);
		ncl_parse_subnum (tempstr, &subscript);
		ul_to_upper(tempstr);
		status = ncl_vxchk (tempstr,subscript, &nclkey, &rel_num);
		if ((rel_num==NCL_VECTOR_REL) || (rel_num==NCL_POINTVEC_REL))
		{
			sec91 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, INT_GEO, UM_BLACK, UM_WHITE);
		}
		else
		{
			status = ncl_parse_scalar_values(tempstr, string, UD_SCAVEC2);
			if (status==-1)
			{
				sec91 = UU_FALSE;
				ud_dispfrm_set_attribs(Sfrm1, INT_GEO, UM_BLACK, UM_RED);
			}
			else
			{
				sec91 = UU_TRUE;
				ud_dispfrm_set_attribs(Sfrm1, INT_GEO, UM_BLACK, UM_WHITE);
			}
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm1, INT_GEO, UM_BLACK, UM_RED);
		sec91 = UU_FALSE;
	}
	nc = (int)strlen(Srdeg);
	ul_strip_blanks(Srdeg, &nc);
	if (nc>0)
	{
		if ((ncl_get_scalar_value(Srdeg, &rval)==-1)&&(Srtog[1]))
		{
			sec92 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, INT_DEG, UM_BLACK, UM_RED);
		}
		else if ((rval<2)&&(Srtog[1]))
		{
			sec92 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, INT_DEG, UM_BLACK, UM_RED);
		}
		else
		{
			sec92 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, INT_DEG, UM_BLACK, UM_WHITE);
/*
.....must be integer
*/
			sprintf(Srdeg, "%d", (int)(rval+0.5));
		}
	}
	else
	{
		if (Srtog[1])
		{
			ud_dispfrm_set_attribs(Sfrm1, INT_DEG, UM_BLACK, UM_RED);
			sec92 = UU_FALSE;
		}
		else
		{
			sec92 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, INT_DEG, UM_BLACK, UM_WHITE);
		}
	}
	nc = (int)strlen(Srrat);
	ul_strip_blanks(Srrat, &nc);
	if (nc>0)
	{
		if ((ncl_get_scalar_value(Srrat, &rval)==-1)&&(Srtog[1]))
		{
			sec93 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, INT_RAT, UM_BLACK, UM_RED);
		}
		else if (((rval<0)||(rval>1))&&(Srtog[1]))
		{
			sec93 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm1, INT_RAT, UM_BLACK, UM_RED);
		}
		else
		{
			sec93 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, INT_RAT, UM_BLACK, UM_WHITE);
		}
	}
	else
	{
		if (Srtog[1])
		{
			ud_dispfrm_set_attribs(Sfrm1, INT_RAT, UM_BLACK, UM_RED);
			sec93 = UU_FALSE;
		}
		else
		{
			sec93 = UU_TRUE;
			ud_dispfrm_set_attribs(Sfrm1, INT_RAT, UM_BLACK, UM_WHITE);
		}
	}
	if ((sec11 && sec12&& sec13&& sec14&& sec15&& sec1)
		|| (S_focus_page!=0))
	{
		ud_form_section_color(Sfrm1,Smode[0],Sblack,UU_FALSE);
	}
	else
	{
		ud_form_section_color(Sfrm1,Smode[0],Sred,UU_TRUE);
	}
	if ((sec21 && sec22&& sec23)|| (S_focus_page!=1))
	{
		ud_form_section_color(Sfrm1,Smode[1],Sblack,UU_FALSE);
	}
	else
	{
		ud_form_section_color(Sfrm1,Smode[1],Sred,UU_TRUE);
	}
	if ((sec31 && sec32&& sec33)|| (S_focus_page!=2))
	{
		ud_form_section_color(Sfrm1,Smode[2],Sblack,UU_FALSE);
	}
	else
	{
		ud_form_section_color(Sfrm1,Smode[2],Sred,UU_TRUE);
	}
	if ((sec41 && sec42&& sec43&& sec44&& sec45&& sec46)
		|| (S_focus_page!=3))
	{
		ud_form_section_color(Sfrm1,Smode[3],Sblack,UU_FALSE);
	}
	else
	{
		ud_form_section_color(Sfrm1,Smode[3],Sred,UU_TRUE);
	}
	if ((sec51 && sec52)|| (S_focus_page!=4))
	{
		ud_form_section_color(Sfrm1,Smode[4],Sblack,UU_FALSE);
	}
	else
	{
		ud_form_section_color(Sfrm1,Smode[4],Sred,UU_TRUE);
	}
	if ((sec61 && sec62&& sec63 && sec64)|| (S_focus_page!=5))
	{
		ud_form_section_color(Sfrm1,Smode[5],Sblack,UU_FALSE);
	}
	else
	{
		ud_form_section_color(Sfrm1,Smode[5],Sred,UU_TRUE);
	}
	if ((sec7)|| (S_focus_page!=6))
	{
		ud_form_section_color(Sfrm1,Smode[6],Sblack,UU_FALSE);
	}
	else
	{
		ud_form_section_color(Sfrm1,Smode[6],Sred,UU_TRUE);
	}
	if ((sec81 && sec82)|| (S_focus_page!=7))
	{
		ud_form_section_color(Sfrm1,Smode[7],Sblack,UU_FALSE);
	}
	else
	{
		ud_form_section_color(Sfrm1,Smode[7],Sred,UU_TRUE);
	}
	if ((sec91 && sec92&& sec93)|| (S_focus_page!=8))
	{
		ud_form_section_color(Sfrm1,Smode[8],Sblack,UU_FALSE);
	}
	else
	{
		ud_form_section_color(Sfrm1,Smode[8],Sred,UU_TRUE);
	}
/*
......always show active page green color
*/
	ud_form_section_color(Sfrm1, Smode[S_act_page], Sgreen, UU_TRUE);

	if (S_focus_page==0)
		ifl = sec11 && sec12&& sec13&& sec14&& sec15&& sec1;
	else if (S_focus_page==1)
		ifl = sec21 && sec22&& sec23;
	else if (S_focus_page==2)
		ifl = sec31 && sec32&& sec33;
	else if (S_focus_page==3)
		ifl = sec41 && sec42&& sec43&& sec44&& sec45&& sec46;
	else if (S_focus_page==4)
		ifl = sec51 && sec52;
	else if (S_focus_page==5)
		ifl = sec61 && sec62&& sec63 && sec64;
	else if (S_focus_page==6)
		ifl = sec7;
	else if (S_focus_page==7)
		ifl = sec81 && sec82;
	else if (S_focus_page==8)
		ifl = sec91 && sec92&& sec93;
	ud_frm_enable_ok(ifl);
	ud_update_form(Sfrm1);
	return 1;
}
/*********************************************************************
**    I_FUNCTION     : S_edit_main(fieldno, val, stat)
**       Handles the edit fields of main form.
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
static UD_FSTAT S_edit_main(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (*fieldno==TAN_VEC)
	{
		if (Sdvec[0]=='\0')
		{
/*
.....disable the "perpto vector" box
*/
			ud_setfrm_traverse_mask(Sfrm1,TAN_MNT,UU_FALSE);			
		}
	}
	S_enable_buttons();
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

	switch(*fieldno)
	{
	case LCK_TYP: 
		ifl = UU_TRUE;
		if (val->frmint[0] == 2) ifl = UU_FALSE;
		for (i=LCK_LIN;i<=LCK_TRN;i++) ud_setfrm_traverse_mask(Sfrm2,i,ifl);
		if (ifl)
		{
			data.frmint = &i;
			ud_getfrm_field(Sfrm2,LCK_TRN,data,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm2,LCK_FAN,data.frmint[0]);
			ud_setfrm_traverse_mask(Sfrm2,LCK_MOV,data.frmint[0]);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm2,LCK_FAN,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrm2,LCK_MOV,UU_FALSE);
		}
		break;
	case LCK_TRN:
		ud_setfrm_traverse_mask(Sfrm2,LCK_FAN,val->frmint[0]);
		ud_setfrm_traverse_mask(Sfrm2,LCK_MOV,val->frmint[0]);
		break;
	}
	S_enable_buttons2();
	return(stat);
}

