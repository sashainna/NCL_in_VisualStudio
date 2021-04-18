/*********************************************************************
**    NAME         :  nupokmod.c
**       CONTAINS:
**           nclu_vmpmod
**           nclu_vmpmod_parm
**           nclu_vmpmod_cmd
**
**    COPYRIGHT 2011 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nuvmpmod.c , 26.3
**     DATE AND TIME OF LAST MODIFICATION
**       07/24/18 , 12:24:44
**
*********************************************************************/
#include <string.h>
#include <stdlib.h>
#include "mdattr.h"
#include "mdpick.h"
#include "mfort.h"
#include "nkeywd.h"
#include "ncluvmp.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"

extern int NVMILL;

/*
.....Choicebox choices
*/
#define RAMP 0
#define HELIX 1

#define ROUGH 0
#define SMOOTH 1

#define SLOT 0
#define SIDE 1

#define LEVEL 0
#define DEEP 1

#define CLW 0
#define CCLW 1

#define DIS 0
#define PLN 1

/*
.....Section definitions
*/
enum {Pocketing, Entry, Options, TParameters, FRates, SSpeeds};
static char Sav_valuestr1[VSTRL], Sav_valuestr2[VSTRL], Sav_valuestr3[VSTRL], 
			Sav_valuestr4[VSTRL], Sav_valuestr5[VSTRL], Sav_valuestr6[VSTRL],
			Sav_pvaluestr1[VSTRL], Sav_pvaluestr2[VSTRL];
static char *Smode[]={"Pocketing", "Entry / Exit", "Options", "Tool Parameters", 
			"Feed Rates", "Spindle Speeds"};
static int Sred[3]={180,0,0}, Syellow[3]={180,148,0}, Sgreen[3]={0,180,0};
static int Sblack[3]={0,0,0};
static int *Ssecol[]={Sblack,Sblack,Sblack,Sblack,Sblack,Sblack,Sblack,Sblack};
static int Sacc[6];
static UU_LOGICAL Smodal;
extern UD_METHOD UD_initfrm_intry;

static UD_FSTAT OnSelect(),OnClose();
static void S_section_changed();
static void S_enable_buttons();
static void S_init_form();
static void S_save_form();

static Sctype_old = 0;
static int S_nlable = 1;
enum
{	
/*
.....Pocketing
*/
	POCKRG1,POCKRG2,POCKRG3,POCKRG4,
/*
.....Entry Method fields
*/
	ENTRG1,ENTRG2,ENTRG3,ENTRG4,ENTRG5,ENTRG6,ENTRG7,ENTRG8,ENTRG9,
/*
.....Options
*/
	OPTRG1,OPTRG2,OPTRG3,OPTRG4,OPTRG5,OPTRG6,OPTRG7,OPTRG8,OPTRG9,
/*
.....Tool Parameters
*/
	TOOLRG1,TOOLRG2,TOOLRG3,TOOLRG4,TOOLRG5,
/*
.....Feed rates
*/
	FEEDRG1,FEEDRG2,FEEDRG3,FEEDRG4,FEEDRG5,FEEDRG6,FEEDRG7,FEEDRG8,FEEDRG9,
	FEEDRG10,FEEDRG11,FEEDRG12,FEEDRG13,FEEDRG14,FEEDRG15,FEEDRG16,
/*
.....Spindle Speeds
*/
	SSPRG1, SSPRG2,SSPRG3,SSPRG4, SSPRG5,SSPRG6,SSPRG7,
/*
.....Common fields
*/
	COMRG1, COMRG2, FVIDEO
};

/*
.....Common variables
*/
static UU_LOGICAL Sactive = UU_FALSE;
UU_LOGICAL Svmpmod_saved = UU_FALSE;
static int Sfrm;
static UU_REAL Scutrad;
/*
.....Pocket entry variables
*/
static int Sdir=1, Sdeptype=0;
char Smaxstep[VSTRL], Smaxdep[VSTRL];
static int Tdir=1, Tdeptype=0;
char Tmaxstep[VSTRL], Tmaxdep[VSTRL];
/*
.....Entry /Exit
*/
static int Setype, Sctype;
char Scpln_val[VSTRL]="", Scpln_pn[VSTRL]="";
char Seangle[VSTRL], Shrad[VSTRL], Scpln[VSTRL], Sret1[VSTRL], Sret2[VSTRL], Srapto[VSTRL];
static int Tetype, Tctype;
char Tcpln_val[VSTRL]="", Tcpln_pn[VSTRL]="";
char Teangle[VSTRL], Thrad[VSTRL], Tcpln[VSTRL], Tret1[VSTRL], Tret2[VSTRL], Trapto[VSTRL];
/*
....Options
*/
static int Scontour,Slevel, Smooth, Sside;
char Sminrad[VSTRL], Smang[VSTRL], Sfinrad[VSTRL], Strandep[VSTRL], Stranstep[VSTRL];
static int Tcontour,Tlevel, Tmooth, Tside;
char Tminrad[VSTRL], Tmang[VSTRL], Tfinrad[VSTRL], Ttrandep[VSTRL], Ttranstep[VSTRL];
/*
.....Tool Parameters
*/
char Sflutes[VSTRL], Sflute_len[VSTRL], Stool_len[VSTRL], Sdrill_dia[VSTRL], Sdrill_ang[VSTRL];
char Tflutes[VSTRL], Tflute_len[VSTRL], Ttool_len[VSTRL], Tdrill_dia[VSTRL], Tdrill_ang[VSTRL];
/*
.....Feedrates
*/
char Sfeed[7][VSTRL];
static int Sftype[6];
static int Sadjust,Suse_min;
char Tfeed[7][VSTRL];
static int Tftype[6];
static int Tadjust,Tuse_min;
static char Sfeed_valuestr[VSTRL];

/*
.....Spindle speeds
*/
static int Sptype[4];
char Speed[4][VSTRL];
static int Tptype[4];
char Tpeed[4][VSTRL];
/*
.....All
*/
static int Sfrcmod=0,Sfrcprm=1;
static int Tfrcmod=0,Tfrcprm=1;
static int *Sanswer[] = {
/*
.....Pocket entry variables
*/
		&Tdir, (int *)Tmaxstep, &Tdeptype, (int *)Tmaxdep,
/*
.....Entry /Exit
*/
		&Tetype, (int *)Teangle, (int *)Thrad, &Tctype, (int *)Tcpln,

		UU_NULL, (int *)Tret1, (int *)Tret2, (int *)Trapto,
/*
....Options
*/
		&Tcontour, &Tlevel, (int *)Tminrad,
		&Tmooth, (int *)Tmang, (int *)Tfinrad, 
		&Tside, (int *)Ttrandep, (int *)Ttranstep,
/*
.....Tool Parameters
*/
		(int *)Tflutes, (int *)Tflute_len, (int *)Ttool_len, 
		(int *)Tdrill_dia, (int *)Tdrill_ang,
/*
.....Feedrates
*/
		&Tftype[0],(int *)Tfeed[0], &Tftype[1],(int *)Tfeed[1],
		&Tftype[2],(int *)Tfeed[2], &Tftype[3],(int *)Tfeed[3],
		&Tftype[4],(int *)Tfeed[5], &Tftype[5],(int *)Tfeed[6],
		&Tuse_min, (int *)Tfeed[4], &Tadjust,&Sfeed_valuestr,
/*
.....Spindle speeds
*/
		&Tptype[0],(int *)Tpeed[0],&Tptype[1],(int *)Tpeed[1],
		&Tptype[2],(int *)Tpeed[2],(int *)Tpeed[3],
/*
.....All
*/
		&Tfrcprm, &Tfrcmod, UU_NULL
};

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
	if ((*fieldno>POCKRG1)&&(*fieldno<POCKRG4))
		Sacc[Pocketing] = 1;
	if ((*fieldno>ENTRG1)&&(*fieldno<ENTRG9))
		Sacc[Entry] = 1;
	if ((*fieldno>OPTRG1)&&(*fieldno<OPTRG9))
		Sacc[Options] = 1;
	if ((*fieldno>TOOLRG1)&&(*fieldno<TOOLRG5))
		Sacc[TParameters] = 1;
	if ((*fieldno>FEEDRG1)&&(*fieldno<FEEDRG15))
		Sacc[FRates] = 1;
	if ((*fieldno>SSPRG1)&&(*fieldno<SSPRG7))
		Sacc[SSpeeds] = 1;
	S_enable_buttons();
	return(UD_FLDOK);
}
/*********************************************************************
**    I_FUNCTION     : OnChcSel()
**			Choicebox selection callback
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
static UD_FSTAT OnChcSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char valuestr[VSTRL];
	int nc;
/*
.....Enable correct fields
.....based on toggle
*/
	switch (*fieldno)
	{
	case ENTRG1:
//		ud_setfrm_traverse_mask(Sfrm, ENTRG3,Tetype==HELIX);
//		ud_setfrm_display_mask(Sfrm, UD_INPUTF, ENTRG3+S_nlable, Tetype==HELIX);
		ud_setfrm_display_mask(Sfrm, UD_INPUTF, ENTRG3, Tetype==HELIX);
		break;

	case ENTRG4:
		ud_setfrm_traverse_mask(Sfrm,ENTRG6,Tctype==PLN);
		if (Sctype_old==0)
		{
			strcpy(Tcpln_val, Tcpln);
		}
		else
		{
			strcpy(Tcpln_pn, Tcpln);
		}
		if (Tctype==0)
		{
			strcpy(Tcpln, Tcpln_val);
		}
		else
		{
			strcpy(Tcpln, Tcpln_pn);
		}
		ud_dispfrm_update_answer(Sfrm, ENTRG5,(int *)&Tcpln);
		if (Tctype==PLN)
		{
			nc = strlen(Tcpln);
			ul_strip_blanks(Tcpln,&nc);
			if (nc<=0)
			{
				ud_dispfrm_set_attribs(Sfrm, ENTRG5, UM_WHITE, UM_RED);
				ud_dispfrm_set_attribs(Sfrm, ENTRG6, UM_WHITE, UM_RED);
			}
			else
			{
				ud_dispfrm_set_attribs(Sfrm, ENTRG5, UM_BLACK, UM_WHITE);
				ud_dispfrm_set_attribs(Sfrm, ENTRG6, UM_BLACK, -1);
			}
		}
		Sctype_old = Tctype;
		break;

	case OPTRG4:
		ud_setfrm_traverse_mask(Sfrm,OPTRG6,Tmooth==SMOOTH);
		ud_setfrm_traverse_mask(Sfrm,OPTRG5,Tmooth==SMOOTH);
		break;

	case OPTRG7:
		ud_setfrm_traverse_mask(Sfrm,OPTRG8,Tside==SLOT);
		ud_setfrm_traverse_mask(Sfrm,OPTRG9,Tside==SLOT);
		break;
	case FEEDRG1:
		if (Tftype[0]==0)
		{
			ud_setfrm_traverse_mask(Sfrm,FEEDRG2,UU_FALSE);
//			strcpy(valuestr, Sfeed_valuestr);
			valuestr[0] = '\0';
			strcpy(Sav_valuestr1, Tfeed[0]);
			ud_dispfrm_update_answer(Sfrm,FEEDRG2,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm,FEEDRG2,UU_TRUE);
			strcpy(Tfeed[0], Sav_valuestr1);
			ud_dispfrm_update_answer(Sfrm,FEEDRG2,(int *)Tfeed[0]);
		}
		break;
	case FEEDRG3:
		if (Tftype[1]==0)
		{
			ud_setfrm_traverse_mask(Sfrm,FEEDRG4,UU_FALSE);
//			strcpy(valuestr, Sfeed_valuestr);
			valuestr[0] = '\0';
			strcpy(Sav_valuestr2, Tfeed[1]);
			ud_dispfrm_update_answer(Sfrm,FEEDRG4,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm,FEEDRG4,UU_TRUE);
			strcpy(Tfeed[1], Sav_valuestr2);
			ud_dispfrm_update_answer(Sfrm,FEEDRG4,(int *)Tfeed[1]);
		}
		break;
	case FEEDRG5:
		if (Tftype[2]==0)
		{
			ud_setfrm_traverse_mask(Sfrm,FEEDRG6,UU_FALSE);
//			strcpy(valuestr, Sfeed_valuestr);
			valuestr[0] = '\0';
			strcpy(Sav_valuestr3, Tfeed[2]);
			ud_dispfrm_update_answer(Sfrm,FEEDRG6,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm,FEEDRG6,UU_TRUE);
			strcpy(Tfeed[2], Sav_valuestr3);
			ud_dispfrm_update_answer(Sfrm,FEEDRG6,(int *)Tfeed[2]);
		}	
		break;
	case FEEDRG7:
		if (Tftype[3]==0)
		{
			ud_setfrm_traverse_mask(Sfrm,FEEDRG8,UU_FALSE);
//			strcpy(valuestr, Sfeed_valuestr);
			valuestr[0] = '\0';
			strcpy(Sav_valuestr4, Tfeed[3]);
			ud_dispfrm_update_answer(Sfrm,FEEDRG8,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm,FEEDRG8,UU_TRUE);
			strcpy(Tfeed[3], Sav_valuestr4);
			ud_dispfrm_update_answer(Sfrm,FEEDRG8,(int *)Tfeed[3]);
		}	
		break;
	case FEEDRG9:
		if (Tftype[4] == 0)
		{
			ud_setfrm_traverse_mask(Sfrm,FEEDRG10,UU_FALSE);
			valuestr[0] = '\0';
			strcpy(Sav_valuestr5, Tfeed[5]);
			ud_dispfrm_update_answer(Sfrm,FEEDRG10,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm,FEEDRG10,UU_TRUE);
			strcpy(Tfeed[5], Sav_valuestr5);
			ud_dispfrm_update_answer(Sfrm,FEEDRG10,(int *)Tfeed[5]);
		}
		break;
	case FEEDRG11:
		if (Tftype[5] == 0)
		{
			ud_setfrm_traverse_mask(Sfrm,FEEDRG12,UU_FALSE);
			valuestr[0] = '\0';
			strcpy(Sav_valuestr6, Tfeed[6]);
			ud_dispfrm_update_answer(Sfrm,FEEDRG12,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm,FEEDRG12,UU_TRUE);
			strcpy(Tfeed[6], Sav_valuestr6);
			ud_dispfrm_update_answer(Sfrm,FEEDRG12,(int *)Tfeed[6]);		
		}
		break;
	case FEEDRG13:
		if (Tuse_min == 0)
			ud_setfrm_traverse_mask(Sfrm,FEEDRG14,UU_FALSE);
		else
			ud_setfrm_traverse_mask(Sfrm,FEEDRG14,UU_TRUE);
		break;
	case SSPRG1:
		if (Tptype[0] == 0)
			ud_setfrm_traverse_mask(Sfrm,SSPRG2,UU_FALSE);
		else
			ud_setfrm_traverse_mask(Sfrm,SSPRG2,UU_TRUE);
		break;
	case SSPRG3:
		if (Tptype[1] == 0)
		{
			ud_setfrm_traverse_mask(Sfrm,SSPRG4,UU_FALSE);
			valuestr[0] = '\0';
			strcpy(Sav_pvaluestr1, Tpeed[1]);
			ud_dispfrm_update_answer(Sfrm,SSPRG4,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm,SSPRG4,UU_TRUE);
			strcpy(Tpeed[1], Sav_pvaluestr1);
			ud_dispfrm_update_answer(Sfrm,SSPRG4,(int *)Tpeed[1]);		
		}
		break;
	case SSPRG5:
		if (Tptype[2] == 0)
		{
			ud_setfrm_traverse_mask(Sfrm,SSPRG6,UU_FALSE);
			valuestr[0] = '\0';
			strcpy(Sav_pvaluestr2, Tpeed[2]);
			ud_dispfrm_update_answer(Sfrm,SSPRG6,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm,SSPRG6,UU_TRUE);
			strcpy(Tpeed[2], Sav_pvaluestr2);
			ud_dispfrm_update_answer(Sfrm,SSPRG6,(int *)Tpeed[2]);		
		}
		break;
	}
	if ((*fieldno>POCKRG1)&&(*fieldno<POCKRG4))
		Sacc[Pocketing] = 1;
	if ((*fieldno>ENTRG1)&&(*fieldno<ENTRG9))
		Sacc[Entry] = 1;
	if ((*fieldno>OPTRG1)&&(*fieldno<OPTRG9))
		Sacc[Options] = 1;
	if ((*fieldno>TOOLRG1)&&(*fieldno<TOOLRG5))
		Sacc[TParameters] = 1;
	if ((*fieldno>FEEDRG1)&&(*fieldno<FEEDRG15))
		Sacc[FRates] = 1;
	if ((*fieldno>SSPRG1)&&(*fieldno<SSPRG7))
		Sacc[SSpeeds] = 1;
	S_enable_buttons();
	return(UD_FLDOK);
}
static void S_enable_buttons()
{
	int nc;
	UU_REAL rval;
	UU_LOGICAL sfpk1,sfpk2, spsec1, spsec2, fdsec1,fdsec2,fdsec3,
			fdsec4, fdsec5, fdsec6;
	sfpk1 = UU_TRUE;
	sfpk2 = UU_TRUE;
	spsec1 = spsec2 = UU_TRUE;
	fdsec1 = fdsec2 = fdsec3 = fdsec4 = fdsec5 = fdsec6 = UU_TRUE;
/*
.....display red if the current value is more than 0.9*current cutter Radius
*/
	rval = atof(Tminrad);
	if ((rval<0)||(rval>0.9*Scutrad))
	{
		ud_dispfrm_set_attribs(Sfrm, OPTRG3, UM_WHITE, UM_RED);
		sfpk1 = UU_FALSE;
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm, OPTRG3, UM_BLACK, UM_WHITE);
	}
	nc = strlen(Tcpln);
	ul_strip_blanks(Tcpln,&nc);
	if (Tctype==PLN)
	{
		if (nc<=0)
		{
			ud_dispfrm_set_attribs(Sfrm, ENTRG5, UM_WHITE, UM_RED);
			ud_dispfrm_set_attribs(Sfrm, ENTRG6, UM_WHITE, UM_RED);
			sfpk2 = UU_FALSE;
		}
		else
		{
			ud_dispfrm_set_attribs(Sfrm, ENTRG5, UM_BLACK, UM_WHITE);
			ud_dispfrm_set_attribs(Sfrm, ENTRG6, UM_BLACK, -1);
		}
	}
	else
	{
		if (nc<=0)
		{
			ud_dispfrm_set_attribs(Sfrm, ENTRG5, UM_WHITE, UM_RED);
			sfpk2 = UU_FALSE;
		}
		else
			ud_dispfrm_set_attribs(Sfrm, ENTRG5, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(Sfrm, ENTRG6, UM_BLACK, -1);
	}
/*
.....Feed Rate
*/
	if (Tftype[0]!=0)
	{
		nc = (int)strlen(Tfeed[0]);
		ul_strip_blanks(Tfeed[0], &nc);
		if (nc<=0)
		{
			fdsec1 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm,FEEDRG2,UM_WHITE,UM_RED);
		}
		else
		{
			rval = atof(Tfeed[0]);
			if (rval<=0)
				fdsec1 =  UU_FALSE;
			if (fdsec1) 
			{
				ud_dispfrm_set_attribs(Sfrm,FEEDRG2,UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(Sfrm,FEEDRG2,UM_WHITE,UM_RED);
			}
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm,FEEDRG2,UM_BLACK, UM_WHITE);
	}
	if (Tftype[1]!=0)
	{
		nc = (int)strlen(Tfeed[1]);
		ul_strip_blanks(Tfeed[1], &nc);
		if (nc<=0)
		{
			fdsec2 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm,FEEDRG4,UM_WHITE,UM_RED);
		}
		else
		{
			rval = atof(Tfeed[1]);
			if (rval<=0)
				fdsec2 =  UU_FALSE;
			if (fdsec2) 
			{
				ud_dispfrm_set_attribs(Sfrm,FEEDRG4,UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(Sfrm,FEEDRG4,UM_WHITE,UM_RED);
			}
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm,FEEDRG4,UM_BLACK, UM_WHITE);
	}
	if (Tftype[2]!=0)
	{
		nc = (int)strlen(Tfeed[2]);
		ul_strip_blanks(Tfeed[2], &nc);
		if (nc<=0)
		{
			fdsec3 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm,FEEDRG6,UM_WHITE,UM_RED);
		}
		else
		{
			rval = atof(Tfeed[2]);
			if (rval<=0)
				fdsec3 =  UU_FALSE;
			if (fdsec3) 
			{
				ud_dispfrm_set_attribs(Sfrm,FEEDRG6,UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(Sfrm,FEEDRG6,UM_WHITE,UM_RED);
			}
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm,FEEDRG6,UM_BLACK, UM_WHITE);
	}
	if (Tftype[3]!=0)
	{
		nc = (int)strlen(Tfeed[3]);
		ul_strip_blanks(Tfeed[3], &nc);
		if (nc<=0)
		{
			fdsec4 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm,FEEDRG8,UM_WHITE,UM_RED);
		}
		else
		{
			rval = atof(Tfeed[3]);
			if (rval<=0)
				fdsec4 =  UU_FALSE;
			if (fdsec4) 
			{
				ud_dispfrm_set_attribs(Sfrm,FEEDRG8,UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(Sfrm,FEEDRG8,UM_WHITE,UM_RED);
			}
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm,FEEDRG8,UM_BLACK, UM_WHITE);
	}
	if (Tftype[4]!=0)
	{
		nc = (int)strlen(Tfeed[5]);
		ul_strip_blanks(Tfeed[5], &nc);
		if (nc<=0)
		{
			fdsec5 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm,FEEDRG10,UM_WHITE,UM_RED);
		}
		else
		{
			rval = atof(Tfeed[5]);
			if (rval<=0)
				fdsec5 =  UU_FALSE;
			if (fdsec5) 
			{
				ud_dispfrm_set_attribs(Sfrm,FEEDRG10,UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(Sfrm,FEEDRG10,UM_WHITE,UM_RED);
			}
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm,FEEDRG10,UM_BLACK, UM_WHITE);
	}
	if (Tftype[5]!=0)
	{
		nc = (int)strlen(Tfeed[6]);
		ul_strip_blanks(Tfeed[6], &nc);
		if (nc<=0)
		{
			fdsec6 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm,FEEDRG12,UM_WHITE,UM_RED);
		}
		else
		{
			rval = atof(Tfeed[6]);
			if (rval<=0)
				fdsec6 =  UU_FALSE;
			if (fdsec6) 
			{
				ud_dispfrm_set_attribs(Sfrm,FEEDRG12,UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(Sfrm,FEEDRG12,UM_WHITE,UM_RED);
			}
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm,FEEDRG12,UM_BLACK, UM_WHITE);
	}
/*
....Spindle speed
*/
	if (Tptype[1] != 0)
	{
		nc = (int)strlen(Tpeed[1]);
		ul_strip_blanks(Tpeed[1], &nc);
		if (nc<=0)
		{
			spsec1 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm,SSPRG4,UM_WHITE,UM_RED);
		}
		else
		{
			rval = atof(Tpeed[1]);
			if (rval<=0)
				spsec1 =  UU_FALSE;
			if (spsec1) 
			{
				ud_dispfrm_set_attribs(Sfrm,SSPRG4,UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(Sfrm,SSPRG4,UM_WHITE,UM_RED);
			}
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm,SSPRG4,UM_BLACK, UM_WHITE);
	}
	if (Tptype[2] != 0)
	{
		nc = (int)strlen(Tpeed[2]);
		ul_strip_blanks(Tpeed[2], &nc);
		if (nc<=0)
		{
			spsec2 = UU_FALSE;
			ud_dispfrm_set_attribs(Sfrm,SSPRG6,UM_WHITE,UM_RED);
		}
		else
		{
			rval = atof(Tpeed[2]);
			if (rval<=0)
				spsec2 =  UU_FALSE;
			if (spsec2) 
			{
				ud_dispfrm_set_attribs(Sfrm,SSPRG6,UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(Sfrm,SSPRG6,UM_WHITE,UM_RED);
			}
		}
	}
	else
	{
		ud_dispfrm_set_attribs(Sfrm,SSPRG6,UM_BLACK, UM_WHITE);
	}
	if (sfpk1)
	{
		if (Sacc[Options]==0)
		{
			Ssecol[Options] = Sblack;
			S_section_changed(Options,UU_FALSE);
		}
		else
		{	
			Ssecol[Options] = Sgreen; 
			S_section_changed(Options,UU_TRUE);
		}
	}
	else
	{
		Ssecol[Options] = Sred; 
		S_section_changed(Options,UU_FALSE);
	}
	if (sfpk2)
	{
		if (Sacc[Entry]==0)
		{
			Ssecol[Entry] = Sblack;
			S_section_changed(Entry,UU_FALSE);
		}
		else
		{	
			Ssecol[Entry] = Sgreen; 
			S_section_changed(Entry,UU_TRUE);
		}
	}
	else
	{
		Ssecol[Entry] = Sred; 
		S_section_changed(Entry,UU_FALSE);
	}

	if (Sacc[Pocketing]==0)
	{
		Ssecol[Pocketing] = Sblack;
		S_section_changed(Pocketing,UU_FALSE);
	}
	else
	{	
		Ssecol[Pocketing] = Sgreen; 
		S_section_changed(Pocketing,UU_TRUE);
	}

	if (Sacc[TParameters]==0)
	{
		Ssecol[TParameters] = Sblack;
		S_section_changed(TParameters,UU_FALSE);
	}
	else
	{	
		Ssecol[TParameters] = Sgreen; 
		S_section_changed(TParameters,UU_TRUE);
	}

	if (fdsec1&&fdsec2&&fdsec3&&fdsec4&&fdsec5&&fdsec6)
	{
		if (Sacc[FRates]==0)
		{
			Ssecol[FRates] = Sblack;
			S_section_changed(FRates,UU_FALSE);
		}
		else
		{	
			Ssecol[FRates] = Sgreen; 
			S_section_changed(FRates,UU_TRUE);
		}
	}
	else
	{
		Ssecol[FRates] = Sred; 
		S_section_changed(FRates,UU_FALSE);
	}
	if (spsec1&&spsec2)
	{
		if (Sacc[SSpeeds]==0)
		{
			Ssecol[SSpeeds] = Sblack;
			S_section_changed(SSpeeds,UU_FALSE);
		}
		else
		{	
			Ssecol[SSpeeds] = Sgreen; 
			S_section_changed(SSpeeds,UU_TRUE);
		}
	}
	else
	{
		Ssecol[SSpeeds] = Sred; 
		S_section_changed(SSpeeds,UU_FALSE);
	}
	if (Smodal)
		ud_frm_enable_ok(sfpk1&&sfpk2&&spsec2&&spsec2
			&&fdsec1&&fdsec2&&fdsec3&&fdsec5&&fdsec5&&fdsec6);
	else
		ud_frm_enable_close(Sfrm, sfpk1&&sfpk2&&spsec2&&spsec2
			&&fdsec1&&fdsec2&&fdsec3&&fdsec5&&fdsec5&&fdsec6);
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
	S_enable_buttons();
	return(UD_FLDOK);
}
/*********************************************************************
**    E_FUNCTION     : nclu_vmpmod()
**       Controlling routine for the Volumill Pocket Modals form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : >=0: good, form ID
**					-1: canceled or error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_vmpmod(modal)
UU_LOGICAL modal;
{
	int i;
	int iclf[10];
	UU_REAL cbuf[6],dbuf[20];
	char sym[80];
	UD_METHOD save_entry;
/*
.....Set up form fields
*/
	static char traverse[] = {
		1,1,1,1,
		1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,
		1,1,1,1,1, 
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,
		1, 1, 1};
	static char display[] = {
		1,1,1,1,
		1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,
		1,1,1,1,1, 
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,
		1, 1, 1, 1 };
	static char called[] = {
		6,6,6,6,
		6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6,
		6,6,6,6,6, 
		6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,
		6, 6, 6, 6 };
	static UD_METHOD methods[] = {
/*
.....Pocket entry variables
*/
		OnChcSel, OnEditTxt, OnChcSel, OnEditTxt,
/*
.....Entry /Exit
*/
		OnChcSel, OnEditTxt, OnEditTxt, OnChcSel, OnEditTxt,

		OnSelect, OnEditTxt, OnEditTxt, OnEditTxt,
/*
....Options
*/
		OnChcSel, OnChcSel, OnEditTxt,
		OnChcSel, OnEditTxt, OnEditTxt, 
		OnChcSel, OnEditTxt, OnEditTxt,
/*
.....Tool Parameters
*/
		OnEditTxt, OnEditTxt, OnEditTxt, 
		OnEditTxt, OnEditTxt, 
/*
.....Feedrates
*/
		OnChcSel,OnEditTxt, OnChcSel,OnEditTxt,
		OnChcSel,OnEditTxt, OnChcSel,OnEditTxt,
		OnChcSel,OnEditTxt, OnChcSel,OnEditTxt,
		OnChcSel, OnEditTxt, OnChcSel, UU_NULL,
/*
.....Spindle speeds
*/
		OnChcSel, OnEditTxt, OnChcSel, OnEditTxt,
		OnChcSel, OnEditTxt, OnEditTxt,
/*
.....All
*/
		OnChcSel, OnChcSel, OnVideo, OnClose
	};
/*
.....Set traverse masks
*/
	display[ENTRG3+S_nlable] = (Setype == HELIX);
	traverse[ENTRG6] = (Sctype == PLN);
	traverse[OPTRG5] = traverse[OPTRG6] = (Smooth == SMOOTH);
	traverse[OPTRG8] = traverse[OPTRG9] = (Sside == SLOT);
	

	for (i=0;i<6;i++)
	{
		traverse[FEEDRG1+i*2+1] = UU_TRUE;
		if (Sftype[i] == 0) traverse[FEEDRG1+i*2+1] = UU_FALSE;
	}
	if (Suse_min)
		traverse[FEEDRG14] = UU_TRUE;
	else
		traverse[FEEDRG14] = UU_FALSE;
	for (i=0;i<3;i++)
	{
		traverse[SSPRG1+i*2+1] = UU_TRUE;
		if (Sptype[i] == 0) 
		{
			traverse[SSPRG1+i*2+1] = UU_FALSE;
		}
	}
	Smodal = modal;
/*
.....Get the Form input
*/
	Sctype_old = Sctype;
/*
.....Get the current cutter parameters (will use some value)
*/
	cutget(cbuf,dbuf,iclf,sym,sym,sym);
	Scutrad = cbuf[0] / 2.;
	
	Sacc[Pocketing] = Sacc[Entry] = Sacc[Options] = Sacc[TParameters] = 0;
	Sacc[FRates] = Sacc[SSpeeds] = 0;
	S_init_form();
	save_entry = UD_initfrm_intry;
	UD_initfrm_intry = S_enter_form;
	if (modal)
	{
		Sfrm = 0;
		display[COMRG2+S_nlable] = 0;
		Tfrcmod = Sfrcmod = 1;
		Tfrcprm = Sfrcprm = 1;
		Sfrm = ud_form1("vmpmod.frm",Sanswer,Sanswer,methods,called,display,
			traverse);
		if (Sfrm!=-1)
			S_save_form();
		Sactive = UU_FALSE;
	}
	else if (!Sactive) 
	{
		display[COMRG2+S_nlable] = 1;
		Tfrcmod = Sfrcmod = 0;
		Tfrcprm = Sfrcprm = 1;
		Sfrm = ud_form_display1("vmpmod.frm",Sanswer,Sanswer,methods,called,display,
			traverse);
		if (Sfrm != -1) Sactive = UU_TRUE;
	}
	UD_initfrm_intry = save_entry;
done:;
	return Sfrm;
}

/*********************************************************************
**    E_FUNCTION     : nclu_vmpmod_parm(modals)
**       Get the current Pocket Modals from Fortran common block and
**       initialize the form variables.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          kparm   = Integer VMPMOD settings.
**          gparm   = Real VMPMOD settings.
**          gpos    = Real VMPMOD positioning variables.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_vmpmod_parm (modals)
NCLU_vmpmod *modals;
{
	int i,kparm[20];
	UU_REAL gparm[30],gpos[20];
	char cbot[NCL_MAX_LABEL_AND_SUBSCRIPT+1],ctop[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	char cclr[NCL_MAX_LABEL_AND_SUBSCRIPT+1],sbuf[80];
/*
.....Get the VMPMOD settings
*/
	vmppar(kparm,gparm,gpos,cbot,ctop,cclr);
/*
.....Set the form variables
*/
	Sdir = kparm[0];
	Smooth = kparm[1];
	sprintf(Sflutes,"%d",kparm[2]);
	Setype = kparm[3] - 1;
	Slevel = kparm[4];
	Sside = kparm[15];
	Sadjust = kparm[17];
	Scontour = kparm[18];
	Suse_min = kparm[19];

	if (gparm[0] < 0.)
	{
		Sdeptype = 0;
		gparm[0] *= -1.;
	}
	else
		Sdeptype = kparm[16] + 1;
	ncl_sprintf(Smaxdep,&gparm[0],1);
	ncl_sprintf(Strandep,&gparm[1],1);
	ncl_sprintf(Smaxstep,&gparm[2],1);
	ncl_sprintf(Stranstep,&gparm[3],1);
	ncl_sprintf(Sminrad,&gparm[4],1);
	ncl_sprintf(Sfinrad,&gparm[5],1);
	ncl_sprintf(Smang,&gparm[6],1);
	ncl_sprintf(Seangle,&gparm[7],1);
	ncl_sprintf(Shrad,&gparm[8],1);
	ncl_sprintf(Sret1,&gparm[17],1);
	ncl_sprintf(Sret2,&gparm[18],1);
	ncl_sprintf(Sflute_len,&gparm[19],1);
	ncl_sprintf(Stool_len,&gparm[20],1);
	ncl_sprintf(Sfeed[4],&gparm[21],1);
	ncl_sprintf(Sfeed[5],&gparm[22],1);
	ncl_sprintf(Sfeed[6],&gparm[23],1);
	ncl_sprintf(Sdrill_dia,&gparm[24],1);
	ncl_sprintf(Sdrill_ang,&gparm[25],1);

	for (i=0;i<4;i++)
	{
		Sftype[i] = kparm[i+7];
		ncl_sprintf(Sfeed[i],&gparm[i+9],1);
		if (i != 3) Sptype[i] = kparm[i+11];
		ncl_sprintf(Speed[i],&gparm[i+13],1);
	}
	if (atof(Sfeed[5])==0.0)
		Sftype[4] = 0;
	else
		Sftype[4] = 1;
	if (atof(Sfeed[5])==0.0)
		Sftype[5] = 0;
	else
		Sftype[5] = 1;
	ncl_sprintf(Srapto,&gpos[12],1);
	if (kparm[6] == 1)
	{
		Sctype = DIS;
		ncl_sprintf(Scpln,&gpos[8],1);
		strcpy(Scpln_val, Scpln);
	}
	else if (kparm[6] == 2)
	{
		Sctype = PLN;
		ncl_sprintf(sbuf,&gpos[8],4);
		sprintf(Scpln,"(PL/%s)",sbuf);
		strcpy(Scpln_pn, Scpln);
	}
	else
	{
		Sctype = PLN;
		strcpy(Scpln,cclr);
		strcpy(Scpln_pn, Scpln);
	}
	Sctype_old = Sctype;
	ncl_sprintf(Srapto,&gpos[12],1);
/*
.....Save initial modals settings
*/
	modals->deptype = Sdeptype;
	modals->dir = Sdir;
	modals->smooth = Smooth;
	strcpy(modals->flutes,Sflutes);
	modals->etype = Setype;
	modals->level = Slevel;
	modals->adjfed = Sadjust;
	modals->use_contour = Scontour;
	modals->use_min = Suse_min;

	strcpy(modals->maxdep,Smaxdep);
	strcpy(modals->trandep,Strandep);
	strcpy(modals->maxstep,Smaxstep);
	strcpy(modals->transtep,Stranstep);
	strcpy(modals->minrad,Sminrad);
	strcpy(modals->finrad,Sfinrad);
	strcpy(modals->smang,Smang);
	strcpy(modals->eangle,Seangle);
	strcpy(modals->hrad,Shrad);
	strcpy(modals->feed[0],Sfeed[0]);
	strcpy(modals->feed[1],Sfeed[1]);
	strcpy(modals->feed[2],Sfeed[2]);
	strcpy(modals->feed[3],Sfeed[3]);
	strcpy(modals->feed[4],Sfeed[4]);
	strcpy(modals->feed[5],Sfeed[5]);
	strcpy(modals->feed[6],Sfeed[6]);
	strcpy(modals->speed[0],Speed[0]);
	strcpy(modals->speed[1],Speed[1]);
	strcpy(modals->speed[2],Speed[2]);
	strcpy(modals->speed[3],Speed[3]);
	strcpy(modals->ret1,Sret1);
	strcpy(modals->ret2,Sret2);
	strcpy(modals->flutelen,Sflute_len);
	strcpy(modals->toollen,Stool_len);
	strcpy(modals->drilldia,Sdrill_dia);
	strcpy(modals->drillang,Sdrill_ang);

	if (kparm[14] == 2)
	{
		modals->btype = PLN;
		ncl_sprintf(sbuf,&gpos[0],4);
		sprintf(modals->botpln,"(PL/%s)",sbuf);
	}
	else
	{
		modals->btype = PLN;
		strcpy(modals->botpln,cbot);
	}

	if (kparm[5] == 1)
	{
		modals->ttype = DIS;
		ncl_sprintf(modals->toppln,&gpos[4],1);
	}
	else if (kparm[5] == 2)
	{
		modals->ttype = PLN;
		ncl_sprintf(sbuf,&gpos[4],4);
		sprintf(modals->toppln,"(PL/%s)",sbuf);
	}
	else
	{
		modals->ttype = PLN;
		strcpy(modals->toppln,ctop);
	}

	modals->ctype = Sctype;
	strcpy(modals->cpln,Scpln);
	strcpy(modals->rapto,Srapto);

	return;
}

/*********************************************************************
**    E_FUNCTION     : nclu_vmpmod_cmd (modals,flag)
**       Outputs the VMPMOD command.
**    PARAMETERS
**       INPUT  :
**          modals  = Original VMPMOD settings when form first displayed.
**          flag    = UU_FALSE = Output *command,
**                    UU_TRUE = Output actual command.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_vmpmod_cmd (modals,flag)
NCLU_vmpmod *modals;
UU_LOGICAL flag;
{
	int i;
	NCL_cmdbuf cmdbuf;
	char buf[VSTRL+1];
/*
.....Determine if VMPMOD command should be output
*/
	while (!(Sfrcmod && flag))
	{
		if (Sdeptype != modals->deptype) break;
		if (Sdir != modals->dir) break;
		if (Smooth != modals->smooth) break;
		if (strcmp(Sflutes,modals->flutes) != 0) break;
		if (Setype != modals->etype) break;
		if (Slevel != modals->level) break;
		if (Sadjust != modals->adjfed) break;
		if (Scontour != modals->use_contour) break;
		if (Suse_min != modals->use_min) break;

		if (strcmp(Smaxdep,modals->maxdep) != 0) break;
		if (strcmp(Strandep,modals->trandep) != 0) break;
		if (strcmp(Smaxstep,modals->maxstep) != 0) break;
		if (strcmp(Stranstep,modals->transtep) != 0) break;
		if (strcmp(Sminrad,modals->minrad) != 0) break;
		if (strcmp(Sfinrad,modals->finrad) != 0) break;
		if (strcmp(Smang,modals->smang) != 0) break;
		if (strcmp(Seangle,modals->eangle) != 0) break;
		if (strcmp(Shrad,modals->hrad) != 0) break;
		if (Sside != modals->side) break;
		if (strcmp(Sfeed[0],modals->feed[0]) != 0) break;
		if (strcmp(Sfeed[1],modals->feed[1]) != 0) break;
		if (strcmp(Sfeed[2],modals->feed[2]) != 0) break;
		if (strcmp(Sfeed[3],modals->feed[3]) != 0) break;
		if (strcmp(Sfeed[4],modals->feed[4]) != 0) break;
		if (strcmp(Sfeed[5],modals->feed[5]) != 0) break;
		if (strcmp(Sfeed[6],modals->feed[6]) != 0) break;
		if (strcmp(Speed[0],modals->speed[0]) != 0) break;
		if (strcmp(Speed[1],modals->speed[1]) != 0) break;
		if (strcmp(Speed[2],modals->speed[2]) != 0) break;
		if (strcmp(Speed[3],modals->speed[3]) != 0) break;
		if (strcmp(Sret1,modals->ret1) != 0) break;
		if (strcmp(Sret2,modals->ret2) != 0) break;
		if (strcmp(Scpln,modals->cpln) != 0) break;
		if (strcmp(Srapto,modals->rapto) != 0) break;
		if (strcmp(Sflute_len,modals->flutelen) != 0) break;
		if (strcmp(Stool_len,modals->toollen) != 0) break;
		if (strcmp(Sdrill_dia,modals->drilldia) != 0) break;
		if (strcmp(Sdrill_ang,modals->drillang) != 0) break;
		return;
	}
/*
.....Save VMPMOD settings
.....if *VMPMOD command is being output
*/
	if (!flag && !Svmpmod_saved)
	{
		vmpsav();
		Svmpmod_saved = UU_TRUE;
	}
/*
.....Initialize command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
	if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
	ncl_add_token(&cmdbuf, NCL_vmpmod, NCL_nocomma);
/*
.....RAMP/HELIX
*/
	if ((Setype != modals->etype || strcmp(Seangle,modals->eangle) != 0 ||
		(Setype == HELIX && strcmp(Shrad,modals->hrad) != 0)) || Sfrcprm)
	{
		if (Setype == RAMP)
		{
			ncl_add_token(&cmdbuf,NCL_ramp,NCL_comma);
			ncl_add_token(&cmdbuf,Seangle,NCL_comma);
		}
		else
		{
			ncl_add_token(&cmdbuf,NCL_helix,NCL_comma);
			ncl_add_token(&cmdbuf,Seangle,NCL_comma);
			ncl_add_token(&cmdbuf,Shrad,NCL_comma);
		}
	}
/*
.....CLW/CCLW
*/
	if (Sdir != modals->dir || Sfrcprm)
	{
		if (Sdir == CLW)
			ncl_add_token(&cmdbuf,NCL_clw,NCL_comma);
		else
			ncl_add_token(&cmdbuf,NCL_ccw,NCL_comma);
	}
/*
.....CLRSRF
*/
	if (strcmp(Scpln,modals->cpln) != 0 || Sfrcprm)
	{
		ncl_add_token(&cmdbuf,NCL_clrsrf,NCL_comma);
		ncl_add_token(&cmdbuf,Scpln,NCL_comma);
	}
/*
.....RAPTO
*/
	if (strcmp(Srapto,modals->rapto) != 0 || Sfrcprm)
	{
		ncl_add_token(&cmdbuf,NCL_rapto,NCL_comma);
		ncl_add_token(&cmdbuf,Srapto,NCL_comma);
	}
/*
.....RTRCTO
*/
	if (strcmp(Sret1,modals->ret1) != 0 || strcmp(Sret2,modals->ret2) != 0 ||
		Sfrcprm)
	{
		ncl_add_token(&cmdbuf,NCL_rtrcto,NCL_comma);
		ncl_add_token(&cmdbuf,Sret1,NCL_comma);
		ncl_add_token(&cmdbuf,Sret2,NCL_comma);
	}
/*
.....DEPTH
*/
	if (Sdeptype != modals->deptype || strcmp(Smaxdep,modals->maxdep) != 0 ||
		Sfrcprm)
	{
		if (Sdeptype == 0)
		{
			ncl_add_token(&cmdbuf,NCL_depth,NCL_comma);
			ncl_add_token(&cmdbuf,"-",NCL_nocomma);
			ncl_add_token(&cmdbuf,Smaxdep,NCL_comma);
		}
		else
		{
			if (Sdeptype == 1)
				ncl_add_token(&cmdbuf,"DIST",NCL_comma);
			else
				ncl_add_token(&cmdbuf,NCL_depth,NCL_comma);
			ncl_add_token(&cmdbuf,Smaxdep,NCL_comma);
		}
	}
/*
.....STEP
*/
	if (strcmp(Smaxstep,modals->maxstep) != 0 || Sfrcprm)
	{
		ncl_add_token(&cmdbuf,NCL_run_step,NCL_comma);
		ncl_add_token(&cmdbuf,Smaxstep,NCL_comma);
	}
/*
.....RADIUS
*/
	if (strcmp(Sminrad,modals->minrad) != 0 || Sfrcprm)
	{
		ncl_add_token(&cmdbuf,NCL_radius,NCL_comma);
		ncl_add_token(&cmdbuf,Sminrad,NCL_comma);
	}
/*
.....SLOT/SIDE
*/
	if ((Sside != modals->side || strcmp(Stranstep,modals->transtep) != 0 ||
		strcmp(Strandep,modals->trandep) != 0) || Sfrcprm)
	{
		if (Sside == SIDE)
		{
			ncl_add_token(&cmdbuf,NCL_side,NCL_comma);
		}
		else
		{
			ncl_add_token(&cmdbuf,NCL_slot,NCL_comma);
			ncl_add_token(&cmdbuf,Strandep,NCL_comma);
			ncl_add_token(&cmdbuf,Stranstep,NCL_comma);
		}
	}
/*
.....SMOOTH/ROUGH
*/
	if ((Smooth != modals->smooth || strcmp(Smang,modals->smang) != 0 ||
		strcmp(Sfinrad,modals->finrad) != 0) || Sfrcprm)
	{
		if (Smooth == SMOOTH)
		{
			ncl_add_token(&cmdbuf,NCL_smooth,NCL_comma);
			ncl_add_token(&cmdbuf,Smang,NCL_comma);
			ncl_add_token(&cmdbuf,Sfinrad,NCL_comma);
		}
		else
		{
			ncl_add_token(&cmdbuf,NCL_rough,NCL_comma);
		}
	}
/*
.....FLUTES
*/
	if (strcmp(Sflutes,modals->flutes) != 0 ||
		strcmp(Sflute_len,modals->flutelen) != 0 || 
		strcmp(Stool_len,modals->toollen) != 0 || Sfrcprm)
	{
		ncl_add_token(&cmdbuf,NCL_flutes,NCL_comma);
		ncl_add_token(&cmdbuf,Sflutes,NCL_comma);
		ncl_add_token(&cmdbuf,Sflute_len,NCL_comma);
		ncl_add_token(&cmdbuf,Stool_len,NCL_comma);
	}
/*
.....DRILL
*/
	if (strcmp(Sdrill_dia,modals->drilldia) != 0 ||
		strcmp(Sdrill_ang,modals->drillang) != 0 || Sfrcprm)
	{
		ncl_add_token(&cmdbuf,"DRILL",NCL_comma);
		ncl_add_token(&cmdbuf,Sdrill_dia,NCL_comma);
		ncl_add_token(&cmdbuf,Sdrill_ang,NCL_comma);
	}
/*
.....DEEP/LEVEL
*/
	if (Slevel != modals->level || Sfrcprm)
	{
		if (Slevel == LEVEL)
			ncl_add_token(&cmdbuf,NCL_level,NCL_comma);
		else
			ncl_add_token(&cmdbuf,NCL_deep,NCL_comma);
	}
/*
.....FEDRAT
*/
	if (strcmp(Sfeed[0],modals->feed[0]) != 0 ||
		strcmp(Sfeed[1],modals->feed[1]) != 0 ||
		strcmp(Sfeed[2],modals->feed[2]) != 0 ||
		strcmp(Sfeed[3],modals->feed[3]) != 0 ||
		Sadjust != modals->adjfed || Sfrcprm)
	{
		ncl_add_token(&cmdbuf,NCL_feedrat,NCL_comma);
		for (i=0;i<4;i++)
		{
			if (Sftype[i] == 0) strcpy(buf,"0");
			else if (Sftype[i] == 2) sprintf(buf,"-%s",Sfeed[i]);
			else strcpy(buf,Sfeed[i]);
			ncl_add_token(&cmdbuf,buf,NCL_comma);
		}
/*
.....Adjust entry feed rate.
*/
		if (Sadjust) ncl_add_token(&cmdbuf,NCL_out,NCL_comma);
		else ncl_add_token(&cmdbuf,NCL_in,NCL_comma);
	}
/*
.....SPINDL
*/
	if (strcmp(Speed[0],modals->speed[0]) != 0 ||
		strcmp(Speed[1],modals->speed[1]) != 0 ||
		strcmp(Speed[2],modals->speed[2]) != 0 ||
		strcmp(Speed[3],modals->speed[3]) != 0 || Sfrcprm)
	{
		ncl_add_token(&cmdbuf,NCL_spindl,NCL_comma);
		for (i=0;i<3;i++)
		{
			if (Sptype[i] == 0) strcpy(buf,"0");
			else if (Sptype[i] == 2) sprintf(buf,"-%s",Speed[i]);
			else strcpy(buf,Speed[i]);
			ncl_add_token(&cmdbuf,buf,NCL_comma);
		}
      ncl_add_token(&cmdbuf,NCL_dwell,NCL_comma);
		ncl_add_token(&cmdbuf,Speed[3],NCL_comma);
	}
/*
.....SMALL/OFF(ON)
*/
	if (Scontour != modals->use_contour || Sfrcprm)
	{
		ncl_add_token(&cmdbuf,NCL_small,NCL_comma);
		if (Scontour)
			ncl_add_token(&cmdbuf,NCL_on,NCL_comma);
		else
			ncl_add_token(&cmdbuf,NCL_off,NCL_comma);
	}
/*
.....MINFED/value(OFF)
*/
	if (Suse_min != modals->use_min ||
		(Suse_min && strcmp(Sfeed[4],modals->feed[4]) != 0 ) || Sfrcprm)
	{
		ncl_add_token(&cmdbuf,"MINFED",NCL_comma);
		if (Suse_min)
			ncl_add_token(&cmdbuf,Sfeed[4],NCL_comma);
		else
			ncl_add_token(&cmdbuf,NCL_off,NCL_comma);
	}
/*
.....RAPID/xyrapid,zrapid
*/
	if (strcmp(Sfeed[5],modals->feed[5]) != 0 ||
		strcmp(Sfeed[6],modals->feed[6]) != 0 || Sfrcprm)
	{
		ncl_add_token(&cmdbuf,NCL_rapid,NCL_comma);
/*
		if (Sftype[4] == 0) strcpy(buf,Sfeed[3]);
*/
        if (Sftype[4] == 0) 
		{
			if (strcmp(Sfeed[3],"") != 0)
				strcpy(buf,Sfeed[3]);
			else
				strcpy(buf,modals->feed[3]);
		}
		else strcpy(buf,Sfeed[5]);
		ncl_add_token(&cmdbuf,buf,NCL_comma);
/*
		if (Sftype[5] == 0) strcpy(buf,Sfeed[3]);
*/
        if (Sftype[5] == 0)
        {
			if (strcmp(Sfeed[3],"") != 0)
				strcpy(buf,Sfeed[3]);
			else
			    strcpy(buf,modals->feed[3]);
		}
		else strcpy(buf,Sfeed[6]);
		ncl_add_token(&cmdbuf,buf,NCL_comma);
	}
/*
.....Output command
*/
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);

	return;
}

/*********************************************************************
**    I_FUNCTION     :  OnSelect(filedno, val, stat)
**       Routine to select the Clearance Plane.
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
static UD_FSTAT OnSelect(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int nc, numint;
	UM_PLOCREC pick;
	UU_LOGICAL cmdreject;

	if (*fieldno != ENTRG6) return (UD_FLDOK);
/*
.....Take down forms
*/
	ud_form_invis();
	ud_dspfrm_invis(Sfrm);
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0)	goto done;
/*
.....Set the appropriate selection mask
*/
	ud_lgeo(UU_TRUE,UD_ncl_allsfpl);
/*
.....Get the next geometry selection
*/
	ua_dl_pldas(UD_DASPCKLOC,UA_NCL,472,&pick,1,&numint,1);
	if (numint == 0) goto done;

	ncl_picktostr(&pick,Scpln);
	ud_dispfrm_update_answer(Sfrm,ENTRG5,(int *)Scpln);
	if (Sctype==PLN)
	{
		nc = strlen(Scpln);
		ul_strip_blanks(Scpln,&nc);
		if (nc<=0)
		{
			ud_dispfrm_set_attribs(Sfrm, ENTRG5, UM_WHITE, UM_RED);
			ud_dispfrm_set_attribs(Sfrm, ENTRG6, UM_WHITE, UM_RED);
		}
		else
		{
			ud_dispfrm_set_attribs(Sfrm, ENTRG5, UM_BLACK, UM_WHITE);
			ud_dispfrm_set_attribs(Sfrm, ENTRG6, UM_BLACK, -1);
		}
	}
/*
.....End of routine
.....Redisplay forms
*/
done:;
	ud_unlimit();
	ud_form_vis();
	ud_dspfrm_vis(Sfrm);

	Sacc[Entry] = 1;
	S_enable_buttons();

	UD_UNMARK(cmdreject);
	return(UD_FLDOK);
}
/*********************************************************************
**    I_FUNCTION     : S_section_changed(section, reset)
**			Updates all answers in the form.
**    PARAMETERS   
**       INPUT  :
**          section   = Label of section that changed.
**          reset     = UU_TRUE = Enable Reset button.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_section_changed(section,reset)
int section;
UU_LOGICAL reset;
{
	UU_LOGICAL bold;
/*
.....Change color of section label
*/
	bold = Ssecol[section] != Sblack;
	ud_form_section_color(Sfrm, Smode[section], Ssecol[section], bold);
}
/*********************************************************************
**    S_FUNCTION     :  OnClose()
**       Method called when Pocket Modals form is closed.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnClose(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (*fieldno!=-1)
		S_save_form();
	Sactive = UU_FALSE;
	return(UD_FLDOK);
}

static void S_init_form()
{
	int i;
	NCLX_mot_feedrate fedrat;
	UU_REAL feedrate;

	NclxMotGetFeedrate(&fedrat);
	feedrate = fedrat.base_feedrate;
	ncl_val2str (feedrate, Sfeed_valuestr);

	Tfrcmod = Sfrcmod;
	Tfrcprm = Sfrcprm;
/*
.....Pocket entry variables
*/
	Tdir = Sdir; 
	Tdeptype = Sdeptype;
	strcpy(Tmaxstep, Smaxstep);
	strcpy(Tmaxdep, Smaxdep);
/*
.....Entry /Exit
*/
	Tetype = Setype;
	Tctype = Sctype;
	strcpy (Tcpln_val, Scpln_val);
	strcpy (Tcpln_pn, Scpln_pn);
	strcpy (Teangle, Seangle);
	strcpy (Thrad, Shrad);
	strcpy (Tcpln, Scpln);
	strcpy (Tret1, Sret1);
	strcpy (Tret2, Sret2);
	strcpy (Trapto, Srapto);
/*
....Options
*/
	Tcontour = Scontour;
	Tlevel = Slevel;
	Tmooth = Smooth;
	Tside = Sside;
	strcpy (Tminrad, Sminrad);
	strcpy (Tmang, Smang);
	strcpy (Tfinrad, Sfinrad);
	strcpy (Ttrandep,Strandep);
	strcpy (Ttranstep, Stranstep);
/*
.....Tool Parameters
*/
	strcpy (Tflutes, Sflutes);
	strcpy (Tflute_len, Sflute_len);
	strcpy (Ttool_len, Stool_len);
	strcpy (Tdrill_dia, Sdrill_dia);
	strcpy (Tdrill_ang, Sdrill_ang);
/*
.....Feedrates
*/
	for (i=0;i<7;i++)
	{
		strcpy(Tfeed[i], Sfeed[i]);
	}

	strcpy(Sav_valuestr1, Sfeed[0]);
	strcpy(Sav_valuestr2, Sfeed[1]);
	strcpy(Sav_valuestr3, Sfeed[2]);
	strcpy(Sav_valuestr4, Sfeed[3]);
	strcpy(Sav_valuestr5, Sfeed[5]);
	strcpy(Sav_valuestr6, Sfeed[6]);

	for (i=0;i<6;i++)
	{
		Tftype[i] = Sftype[i];
		if (Tftype[i]==0)
		{
			if (i<=3)
				Tfeed[i][0] = '\0';
			if (i==4)
				Tfeed[5][0] = '\0';
			if (i==5)
				Tfeed[6][0] = '\0';
		}
	}
	Tadjust = Sadjust;
	Tuse_min = Suse_min;
/*
.....Spindle speeds
*/
	for (i=0;i<4;i++)
	{
		Tptype[i] = Sptype[i];
		strcpy(Tpeed[i], Speed[i]);
	}
	if (Tptype[1]==0)
		Tpeed[1][0] = '\0';
	if (Tptype[2]==0)
		Tpeed[2][0] = '\0';
	strcpy(Sav_pvaluestr1, Speed[1]);
	strcpy(Sav_pvaluestr1, Speed[2]);
}
static void S_save_form()
{
	int i;
	Sfrcmod = Tfrcmod;
	Sfrcprm = Tfrcprm;
/*
.....Pocket entry variables
*/
	Sdir = Tdir; 
	Sdeptype = Tdeptype;
	strcpy(Smaxstep, Tmaxstep);
	strcpy(Smaxdep, Tmaxdep);
/*
.....Entry /Exit
*/
	Setype = Tetype;
	Sctype = Tctype;
	strcpy (Scpln_val, Tcpln_val);
	strcpy (Scpln_pn, Tcpln_pn);
	strcpy (Seangle, Teangle);
	strcpy (Shrad, Thrad);
	strcpy (Scpln, Tcpln);
	strcpy (Sret1, Tret1);
	strcpy (Sret2, Tret2);
	strcpy (Srapto, Trapto);
/*
....Options
*/
	Scontour = Tcontour;
	Slevel = Tlevel;
	Smooth = Tmooth;
	Sside = Tside;
	strcpy (Sminrad, Tminrad);
	strcpy (Smang, Tmang);
	strcpy (Sfinrad, Tfinrad);
	strcpy (Strandep, Ttrandep);
	strcpy (Stranstep, Ttranstep);
/*
.....Tool Parameters
*/
	strcpy (Sflutes, Tflutes);
	strcpy (Sflute_len, Tflute_len);
	strcpy (Stool_len, Ttool_len);
	strcpy (Sdrill_dia, Tdrill_dia);
	strcpy (Sdrill_ang, Tdrill_ang);
/*
.....Feedrates
*/
	for (i=0;i<7;i++)
		strcpy(Sfeed[i], Tfeed[i]);
	for (i=0;i<6;i++)
		Sftype[i] = Tftype[i];
	Sadjust = Tadjust;
	Suse_min = Tuse_min;
/*
.....Spindle speeds
*/
	for (i=0;i<4;i++)
	{
		Sptype[i] = Tptype[i];
		strcpy(Speed[i], Tpeed[i]);
	}
}

void nclu_vmp_mod()
{
	int status;
	NCLU_vmpmod vmpmod;
	NCL_cmdbuf cmdbuf;
	if (NVMILL == 0)
	{
		ud_wrerr("You are not authorized to run this product.");
		return;
	}		
/*
.....Get VMPMOD settings and setup form data
*/	
	nclu_vmpmod_parm(&vmpmod);
	status = nclu_vmpmod(UU_TRUE);
/*
.....Output the Pocket mode command
*/
	if (status == UU_SUCCESS)
	{
		nclu_vmpmod_cmd(&vmpmod, UU_TRUE);
	}
}


