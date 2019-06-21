/********************************************************************* 
**    NAME         :  nuprofil.c
**       CONTAINS:
**		             nclu_profile()
**
**    COPYRIGHT 2005 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nuprofil.c , 25.9
**    DATE AND TIME OF LAST MODIFICATION
**       10/27/16 , 13:34:28
*********************************************************************/
#include <string.h>
#include "class.h"
#include "dselmask.h"
#include "nclmplay.h"
#include "mdattr.h"
#include "mdrel.h"
#include "mgeom.h"
#include "mdcpln.h"
#include "mdpick.h"
#include "mxxx.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nclmplay.h"
#include "nkeywd.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "uhep.h"
#include "nclvx.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"

enum
{
/*
.....Curve Fields
*/
	FDCV1, FDCV2, FDCV3, FDCV4, FDCV5, FDCV6,
/*
.....Start/End fields
*/
	FDST1, FDST2, FDST3, FDEN1, FDEN2, FDEN3, FDDR1, FDDR2, FDDR3, FDDR4, FDDR5,
/*
.....Part Surface fields
*/
	FPSF1, FPSF2, FPSF3, FPTHK, FPAX1, FPAX2, FPCS1, FPCS2, FPCS3, FPTL1, FPTL2,
	FPTL3, FPTL4, FPTL5, FPTL6, FPTL7, FPTL8,
/*
.....Entry / Exit fields
*/
	FETYP, FEDIS, FEANG, FERS1, FERAD, FERS2, FXTYP, FXDIS, FXANG, FXRS1, FXRAD,
	FXRS2,
/*
.....Positioning fields
*/
	FPCL1, FPCL2, FPCL3, FPRP1, FPRP2, FPRP3, FPRT1, FPRT2, FPRT3, FPLOP,
/*
.....Loop fields passes
*/
	FLOF1, FLOF2, FLOF3, FLOS1, FLOS2, FLDP1, FLDP2, FLDP3, FLDS1, FLDS2, FLFST,
/*
.....Options form fields
*/
	FOSCB, FOSTP, FOFCB, FOFIL, FOFBT, FTCOM, FTCSD, FTCPL, FTCTX, FTPDS, FTSRT,
/*
.....Feed Rate fields
*/
	FFTG1, FFED1, FFTG2, FFED2, FFTG3, FFED3, FFTG4, FFED4, FFTG5, FFED5, FFTG6,
	FFED6,FFED7,
/*
.....Color fields
*/
	FCOLA, FCOLB, FCOLC, FCOLE, FCOLF, FCOLG, FCOLH, FCOLI, FCOLJ, FCOLK,
	FCOLL,
/*
.....Action item fields
*/
	FAPV, FAPY, FARS, FAPB, FAVE, FAGE, FAVW, FVIDEO
};
/*
.....CUTCOM variables
*/
#define LEFT 0
#define RIGHT 1
#define ON 2

#define XYPLAN 0
#define YZPLAN 1
#define ZXPLAN 2
#define NONE 3

#define ALL 0
#define START 0

#define HIDE_KEY 0
#define CONTOUR_KEY 1

/*
.....Global variables
*/
extern int NAUTIPV;
extern int NCL_preview_mot;
/*
.....Section definitions
*/
enum {Curve, Start, Part, Entry, Posit, Passes, Options, Feeds, Colors};
static char *Smode[]={"Curve", "Start / End", "Part Surface", "Entry / Exit",
	"Positioning", "Passes", "Options", "Feed Rates", "Colors"};
static int Sred[3]={180,0,0}, Syellow[3]={180,148,0}, Sgreen[3]={0,180,0};
static int Sblack[3]={0,0,0};
static int Sacc[9];
static char Sfeed_valuestr[65];
static char Sav_valuestr1[2][65], Sav_valuestr2[2][65], Sav_valuestr3[2][65],
			Sav_valuestr4[2][65], Sav_valuestr5[2][65], Sav_valuestr6[2][65];
static int Ssav_fchc[6];
/*
.....Main variables
*/
static UU_LOGICAL Sform_init=UU_FALSE, Sarcfl=UU_FALSE;
static UN_motseg *Smptr=0,Smotatt;
static UN_mot_vpbox_struc Smvpbox[UV_NVPORTS];
static UU_LIST Skey_list;
static int Snkeys;
static UU_LOGICAL Skey_init;
static UU_REAL Scdia;
static UM_vector Sfwd,Svecs[4];
static NCLX_mdl_pntvec Stend;
/*
.....Drive Surface variables
*/
static int Sdscon,Tdscon,Sdsofd,Tdsofd,Sdsthfl,Tdsthfl;
static UU_REAL Tdsthk;
static char Sdsthk_str[65], Tdsthk_str[65], Scurve[65],Tcurve[65];
static int Slapflg,Tlapflg;
static char Slapdis[21],Tlapdis[21];
static int Sstart,Tstart;
static char Sstpt[81],Tstpt[81];
static int Sdir,Tdir;
static char Sdirvec[81],Tdirvec[81];
static int Send,Tend;
static char Senpt[81],Tenpt[81];
/*
.....Part Surface variables
*/
static char Spsf[81],Tpsf[81];
static int Spftax,Tpftax;
static UU_REAL Tpfthk;
static char Spfthk_str[65], Tpfthk_str[65];
static char Spfcs[81],Tpfcs[81];
static char Spfang_str[65],Tpfang_str[65];
static int Spftilt[2],Tpftilt[2],Spfdtyp[2],Tpfdtyp[2];
static char Spftan_str[2][65],Tpftan_str[2][65];
static char Spftds_str[2][65],Tpftds_str[2][65];
/*
.....Entry / Exit variables
*/
static int Sentry,Tentry,Sexit,Texit;
/*
.....Option box variables
*/
static int Sfilbox,Tfilbox,Smaxdpbox,Tmaxdpbox;
static char Sfilang_str[65],Tfilang_str[65],Smaxdp_str[65],Tmaxdp_str[65];
static NCLX_mot_fillet Sfillet;
static char Scfill[6][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
/*
.....Cutcom variables
*/
static int Scutcom,Tcutcom,Scutcomdir,Tcutcomdir,Scutcompln,Tcutcompln,
		   Scutcomsrt,Tcutcomsrt;
static UU_REAL Tcutcompds;
static char Scutcom_str[80],Tcutcom_str[80];
static char Scutcompds_str[65],Tcutcompds_str[65];
/*
.....Entry / Exit form variables
*/
static int Sentry,Tentry,Sexit,Texit;
static UU_REAL Tenris1,Tenris2;
static UU_REAL Texris1,Texris2;
static char Sendis_str[65],Tendis_str[65],Senrad_str[65],Tenrad_str[65],
		Senang_str[65],Tenang_str[65],Senris1_str[65],Tenris1_str[65],
		Senris2_str[65],Tenris2_str[65];
static char Sexdis_str[65],Texdis_str[65],Sexrad_str[65],Texrad_str[65],
		Sexang_str[65],Texang_str[65],Sexris1_str[65],Texris1_str[65],
		Sexris2_str[65],Texris2_str[65];
/*
.....Positioning variables
*/
static int Spoclr,Tpoclr;
static char Spocpln[81],Tpocpln[81];
static char Tpocpln_str1[81], Tpocpln_str2[81], Tpocpln_str3[81];
static char Tpoppln_str1[81], Tpoppln_str2[81],Tpoppln_str3[81];
static char Tporpln_str1[81],Tporpln_str2[81],Tporpln_str3[81];
static char Tlpofth_str1[65], Tlpofth_str2[65];
static char Tlpofst_str1[65], Tlpofst_str2[65];
static int Sporap,Tporap;
static char Spoppln[81],Tpoppln[81];
static int Sporet,Tporet;
static char Sporpln[81],Tporpln[81];
static UU_REAL Tportl;
static char Sportl_str[65],Tportl_str[65];
/*
.....Loop variables
*/
static int Slpoft,Tlpoft,Slpofs,Tlpofs,Slpofx,Tlpofx;
static char Slpofth_str[65],Tlpofth_str[65],Slpofst_str[65],Tlpofst_str[65];
static int Slpdpt,Tlpdpt;
static char Slptpl[81],Tlptpl[81];
static char Tlptpl_str1[81], Tlptpl_str2[81], Tlptpl_str3[81];
static int Slpdps,Tlpdps,Slpdfi,Tlpdfi;
static char Slpdpst_str[65],Tlpdpst_str[65];
static char Tlpdpst_str1[65],Tlpdpst_str2[65];
/*
.....Feed Rate variables
*/
static int Sfopt[6],Tfopt[6];
static char Sfeed_str[6][65],Tfeed_str[6][65];
/*
.....Color variables
*/
static int Sdscol,Tdscol;
static int Sstcol,Tstcol;
static int Sencol,Tencol;
static int Spfcol,Tpfcol;
static int Scscol,Tcscol;
static int Spoccol,Tpoccol;
static int Spopcol,Tpopcol;
static int Sporcol,Tporcol;
static int Slpdcol,Tlpdcol;
static int Tgeotog,Tgeocol;
static UU_LOGICAL Sgeo_redraw;
/*
.....Geometry variables
*/
static UM_sgeo Sgcv,Sgspt,Sgept,Sgpsf,Sgpsf2,Sgcp,Sgret,Sgtpl,Sgrap;
static UU_LOGICAL Sgeo_init;
static UU_LIST Sglst;
static UU_LOGICAL Sgeo_apply = UU_FALSE;
/*
.....Main form callback routines
*/
static UD_FSTAT OnDsSel(),OnDsTog();
static UD_FSTAT OnStSel(),OnStTog(),OnStTxt();
static UD_FSTAT OnPsSel(),OnPsTog(),OnPsTxt();
static UD_FSTAT OnEnTog(),OnEnTxt();
static UD_FSTAT OnOpTog(),OnCoTog();
static UD_FSTAT OnAction(),OnEditTxt(),OnVideo();
/*
.....Final pass form routines
*/
static UD_FSTAT OnCutcomTog(),OnFinlTxt();
/*
.....Positioning form routines
*/
static UD_FSTAT OnPoTog(),OnPoSel(),OnPoTxt();
/*
.....Loop form routines
*/
static UD_FSTAT OnLpTog(),OnLpSel(),OnLpTxt();
/*
.....Feed Rate form routines
*/
static UD_FSTAT OnFdTog(),OnFeedTxt();
/*
.....Local routines
*/
static void S_init_form(),S_save_form(),S_hilite_entity(),S_unhilite_entity();
static void S_init_traverse(),S_form_invis(),S_form_vis(),S_init_geo();
static void S_push_geo();
static void S_unhilite_entities(),S_storvals(),S_unhilite_all();
static void S_update_answers(),S_section_changed(),S_draw_indir();
static int S_select_geo(),S_build_command();
static UD_FSTAT S_enter_form();
static void S_create_key_list(),S_add_key();
static UU_LOGICAL S_enable_buttons();

extern UD_METHOD UD_initfrm_intry;

static int SfrmPlay=0;
static int Serrflg;
static UU_LOGICAL Spreviewflg,Schgmade;
/*
.....Form answers
*/
static int *Sanswers[] = {UU_NULL,(int *)Tcurve,&Tdscon,&Tdsthfl,
	(int *)Tdsthk_str,&Tdsofd,

	&Tstart,UU_NULL,(int *)&Tstpt, &Tend,UU_NULL,(int *)&Tenpt,
	&Tdir,UU_NULL,(int *)&Tdirvec, &Tlapflg,(int *)&Tlapdis,

	UU_NULL,(int *)Tpsf,UU_NULL,
	(int *)Tpfthk_str,
	&Tpftax,(int *)&Tpfang_str,
	UU_NULL,(int *)Tpfcs,UU_NULL,
	&Tpftilt[0],&Tpftilt[1],(int *)Tpftan_str[0],(int *)Tpftan_str[1],
	&Tpfdtyp[0],(int *)Tpftds_str[0],&Tpfdtyp[1],(int *)Tpftds_str[1],

	&Tentry, (int *)Tendis_str, (int *)Tenang_str,
	(int *)Tenris1_str, (int *)Tenrad_str, (int *)Tenris2_str,
	&Texit, (int *)Texdis_str, (int *)Texang_str, (int *)Texris1_str,
	(int *)Texrad_str, (int *)Texris2_str,

	&Tpoclr,(int *)Tpocpln,UU_NULL,
	&Tporap,(int *)&Tpoppln,UU_NULL,
	&Tporet,(int *)Tporpln,UU_NULL,
	(int *)&Tportl_str,

	&Tlpoft,(int *)&Tlpofth_str, &Tlpofs,(int *)&Tlpofst_str, &Tlpofx,
	&Tlpdpt,(int *)Tlptpl,UU_NULL, &Tlpdps,(int *)&Tlpdpst_str,
	&Tlpdfi,

	&Tmaxdpbox,(int *)&Tmaxdp_str, &Tfilbox,(int *)&Tfilang_str,UU_NULL,
	&Tcutcom,&Tcutcomdir,&Tcutcompln,(int *)Tcutcom_str,
	(int *)Tcutcompds_str,&Tcutcomsrt,

	&Tfopt[0],(int *)&Tfeed_str[0],&Tfopt[1],(int *)&Tfeed_str[1],
	&Tfopt[2],(int *)&Tfeed_str[2],&Tfopt[3],(int *)&Tfeed_str[3],
	&Tfopt[4],(int *)&Tfeed_str[4],&Tfopt[5],(int *)&Tfeed_str[5],
	&Sfeed_valuestr,

	&Tdscol,&Tstcol,&Tencol,&Tpfcol,&Tcscol,&Tpoccol,&Tpopcol,
	&Tporcol,&Tlpdcol,&Tgeotog,&Tgeocol,

	UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL};

/*********************************************************************
**    E_FUNCTION     : nclu_profile()
**       Controlling routine for the Profile Machining form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_profile()
{
	int status,flag;
	UU_LOGICAL cmdreject;
	UD_METHOD save_entry;
/*
.....Set up form fields
*/
	static char traverse[] = {1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,1,1,1,1,1,1,1};
	static char display[] = {1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,
      1,1,1,1,1,1,1,1,1};
	static char called[] = {6,6,6,6,6,6, 6,6,6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6,6,6,6, 6,6,6,6,6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6};

	static UD_METHOD methods[] = {OnDsSel,OnEditTxt,OnDsTog,OnDsTog,OnEditTxt,
		OnDsTog,

		OnStTog,OnStSel,OnStTxt, OnStTog,OnStSel,OnStTxt, OnStTog,OnStSel,OnStTxt,
		OnStTog, OnEditTxt,

		OnPsSel,OnPsTxt,OnPsSel, OnEditTxt, OnPsTog,OnEditTxt,
		OnPsSel,OnPsTxt,OnPsSel,
		OnPsTog,OnPsTog,
		OnEditTxt,OnEditTxt, OnPsTog,OnEditTxt,OnPsTog,OnEditTxt,

		OnEnTog,OnEnTxt,OnEnTxt,OnEnTxt,OnEnTxt,OnEnTxt,
		OnEnTog,OnEnTxt,OnEnTxt,OnEnTxt,OnEnTxt,OnEnTxt,

		OnPoTog,OnPoTxt,OnPoSel, OnPoTog,OnPoTxt,OnPoSel, OnPoTog,OnPoTxt,OnPoSel,
		OnPoTxt,

		OnLpTog,OnLpTxt, OnLpTog, OnLpTxt, OnLpTog,
		OnLpTog, OnLpTxt,OnLpSel, OnLpTog,OnLpTxt, OnLpTog,

		OnOpTog,OnEditTxt, OnOpTog,OnEditTxt,OnOpTog,
		OnCutcomTog,OnCutcomTog,OnCutcomTog,OnFinlTxt,
		OnFinlTxt,OnCutcomTog,

		OnFdTog,OnFeedTxt,OnFdTog,UU_NULL,
		OnFdTog,UU_NULL,OnFdTog,UU_NULL, OnFdTog,UU_NULL,OnFdTog,UU_NULL,
		UU_NULL,

		OnCoTog,OnCoTog,OnCoTog,OnCoTog,OnCoTog,OnCoTog,OnCoTog,
		OnCoTog,OnCoTog,OnCoTog,OnCoTog,

		OnAction,OnAction,OnAction,OnAction,OnAction,OnAction,OnAction,OnVideo};
/*
.....Get subform settings
*/
	NclxMotGetFillet(&Sfillet);
/*
.....Initialize form answers
*/
	S_init_form();
	Serrflg = 0;
	Spreviewflg = UU_FALSE;
	Schgmade = UU_FALSE;
	save_entry = UD_initfrm_intry;
	SfrmPlay = 0;
	Sarcfl = UU_FALSE;
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
	Sacc[Curve] = Sacc[Start] = Sacc[Part] = Sacc[Entry] = Sacc[Posit] = 0;
	Sacc[Passes] = Sacc[Options] = Sacc[Feeds] = Sacc[Colors] = 0;
	NCL_preview_mot = 1;
	S_init_traverse(display,traverse);
	UD_initfrm_intry = S_enter_form;
/*
.....Get the Form input
*/
	status = ud_form1("nprofil.frm",Sanswers,Sanswers,methods,called,display,
		traverse);
	UD_initfrm_intry = save_entry;
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
.....A curve must be selected
*/
	if ((!Sgeo_init || UU_LIST_LENGTH(&Sglst) == 0) && !Sgeo_apply)
	{
		ud_wrerr("You must select a curve to profile.");
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
	if (status != UU_SUCCESS || Serrflg != 0) goto repeat;
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
	ud_delete_assist_segs();
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
	int status,i;
/*
.....Process Drive Surface pushbuttons
*/
	switch (*fieldno)
	{
	case FDCV1:
		status = S_select_geo(&Sgcv,&Sglst,UD_ncl_offcv,UU_TRUE,423,Tdscol,0,
			FDCV2,Tcurve);
		if (UU_LIST_LENGTH(&Sglst) > 0)
		{
			ud_set_traverse_mask(FAPV,1);
			ud_set_traverse_mask(FAPY,1);
			ud_set_traverse_mask(FAGE,1);
			S_section_changed(Smode[Curve],Sgreen,UU_TRUE,UU_TRUE);
			ud_frm_enable_ok(UU_TRUE);
			ud_dispfrm_set_attribs(0,FDCV1,UM_BLACK,Tdscol);
			Sacc[Curve] = 1;
		}
		break;
	}
/*
.....Check for change since preview
*/
	if (status == UU_SUCCESS)
	{
		Schgmade = UU_TRUE;
		Sacc[Curve] = 1;
	}
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnDsTog(fieldno,val,stat)
**       Method called when a Drive Surface toggle field is changed.
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
static UD_FSTAT OnDsTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	UU_LOGICAL sfl;
/*
.....Process Drive Surface toggle field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FDCV3:
		if (Tdscon == 0 && Tdsthfl == 0)
			ud_set_traverse_mask(FDCV6,0);
		else
			ud_set_traverse_mask(FDCV6,1);
		if (Tdscon == 0)
			for (i=FLOF1;i<=FLOS2;i++) ud_set_traverse_mask(i,0);
		else
		{
			ud_set_traverse_mask(FLOF1,1);
			sfl = UU_FALSE;
			if (Tlpoft != 0) sfl = UU_TRUE;
			for (i=FLOF2;i<=FLOS2;i++) ud_set_traverse_mask(i,sfl);
		}
		if (Sdscon != Tdscon)
		{
			Schgmade = UU_TRUE;
			Sacc[Curve] = 1;
		}
		break;
	case FDCV4:
		ud_set_traverse_mask(FDCV5,Tdsthfl);
		if (Tdsthfl == 1) ud_set_traverse_mask(FDCV6,1);
		else if (Tdscon == 0) ud_set_traverse_mask(FDCV6,0);
		if (Sdsthfl != Tdsthfl)
		{
			Schgmade = UU_TRUE;
			Sacc[Curve] = 1;
		}
		break;
	case FDCV6:
		if (Sdsofd != Tdsofd)
		{
			Schgmade = UU_TRUE;
			Sacc[Curve] = 1;
		}
		break;
	}
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnStSel(fieldno,val,stat)
**       Method called when a Start/End Select button is pressed.
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
static UD_FSTAT OnStSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status,iseg,numint;
	UU_LOGICAL cmdreject;
	UM_coord pt;
	UM_vector vec;
	UM_PLOCREC pick;
	struct NCL_fixed_databag ent;
/*
.....Process Start/End pushbuttons
*/
	switch (*fieldno)
	{
	case FDST2:
		status = S_select_geo(&Sgspt,UU_NULL,UD_ncl_ptpv,UU_FALSE,658,Tstcol,0,
			FDST3,Tstpt);
		break;
	case FDEN2:
		status = S_select_geo(&Sgept,UU_NULL,UD_ncl_ptpv,UU_FALSE,497,Tencol,0,
			FDEN3,Tenpt);
		break;
/*
.....Select the direction
*/
	case FDDR2:
		if (Tdir == 0 || Tdir == 1)
		{
			S_form_invis();
			UD_MARK (cmdreject, UU_TRUE);
			if (cmdreject == 0)
			{
				iseg = ud_pick_assist_seg("Pick the new forward vector.");
				if (iseg != 0)
				{
					um_vctovc(Svecs[iseg-1],Sfwd);
					ncl_sprintf(Tdirvec,Sfwd,3);
					ud_update_answer(FDDR3,Tdirvec);
					if (Tdir == 0)
					{
						Tdir = 1;
						ud_update_answer(FDDR1,&Tdir);
					}
				}
			}
			S_draw_indir();
			S_form_vis();
			UD_UNMARK(cmdreject);
		}
		else if (Tdir == 2)
		{
			S_form_invis();
			do
			{
				UD_MARK (cmdreject, UU_TRUE);
				if (cmdreject != 0) break;
				ud_unlimit();
				ud_ldas(UD_DASPCKLOC,UA_NCL,715,&pick,1,&numint,1,UD_NODEFAULT);
				if (numint == 0) break;
				iseg = ncl_get_pkpt1(&pick,&ent,.001,pt,vec);
				if (iseg == UU_FAILURE)
					ud_wrerr ("Failed to locate point on entity");
				else
				{
					um_vcmnvc(pt,Stend.pt,Sfwd); um_unitvc(Sfwd,Sfwd);
					ncl_sprintf(Tdirvec,Sfwd,3);
					ud_update_answer(FDDR3,Tdirvec);
				}
			} while (iseg == UU_FAILURE);
			S_draw_indir();
			S_form_vis();
			UD_UNMARK(cmdreject);
		}
		break;
	}
/*
.....Check for change since preview
*/
	if (status == UU_SUCCESS)
	{
		Schgmade = UU_TRUE;
		Sacc[Start] = 1;
	}
	S_section_changed(Smode[Start],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnStTog(fieldno,val,stat)
**       Method called when a Start/End toggle field is changed.
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
static UD_FSTAT OnStTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int fno;
	UU_LOGICAL sfl;
/*
.....Process Start/End toggle field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FDST1:
		if (Tstart!=2)
		{
			ud_set_traverse_mask(FDST2,Tstart);
			ud_set_traverse_mask(FDST3,Tstart);
		}
		else				
		{		
			ud_set_traverse_mask(FDST2,UU_FALSE);
			ud_set_traverse_mask(FDST3,UU_FALSE);
		}
		if (Sstart != Tstart)
		{
			Schgmade = UU_TRUE;
			Sacc[Start] = 1;
		}
		break;
	case FDEN1:
		ud_set_traverse_mask(FDEN2,Tend);
		ud_set_traverse_mask(FDEN3,Tend);
		if (Send != Tend)
		{
			Schgmade = UU_TRUE;
			Sacc[Start] = 1;
		}
		break;
	case FDDR1:
		if (Tdir == 1 || Tdir == 2) sfl = UU_TRUE;
		else sfl = UU_FALSE;
		S_draw_indir();
		ud_set_traverse_mask(FDDR2,sfl);
		ud_set_traverse_mask(FDDR3,sfl);
		if (Sdir != Tdir)
		{
			Schgmade = UU_TRUE;
			Sacc[Start] = 1;
		}
		if (sfl)
		{
			fno = FDDR2;
			OnStSel(&fno,val,stat);
		}
		break;
	case FDDR4:
		if (Tlapflg == 0) ud_set_traverse_mask(FDDR5,UU_FALSE);
		else ud_set_traverse_mask(FDDR5,UU_TRUE);
		if (Slapflg != Tlapflg)
		{
			Schgmade = UU_TRUE;
			Sacc[Start] = 1;
		}
		break;
	}
	S_section_changed(Smode[Start],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnStTxt(fieldno,val,stat)
**       Method called when a Start/End text field is changed.
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
static UD_FSTAT OnStTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Process Start/End text field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FDST3:
		ul_to_upper(Tstpt);
		if (strcmp(Sstpt,Tstpt) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Start] = 1;
		}
		S_init_geo(&Sgspt,Tstpt,Tstcol);
		break;
	case FDEN3:
		ul_to_upper(Tenpt);
		if (strcmp(Senpt,Tenpt) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Start] = 1;
		}
		S_init_geo(&Sgept,Tenpt,Tencol);
		break;
	case FDDR3:
		ul_to_upper(Tdirvec);
		if (strcmp(Sdirvec,Tdirvec) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Start] = 1;
		}
		break;
	}
	S_section_changed(Smode[Start],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnPsSel(fieldno,val,stat)
**       Method called when a Part Surface Select button is pressed.
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
static UD_FSTAT OnPsSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,status,fno;
	UD_DDATA tval;
/*
.....Process Part Surface pushbuttons
*/
	switch (*fieldno)
	{
	case FPSF1:
		status = S_select_geo(&Sgpsf,UU_NULL,UD_ncl_allsfpl,UU_FALSE,415,Tpfcol,
			0,FPSF2,Tpsf);
		if (Sgpsf.key != 0)
		{
			ud_set_traverse_mask(FPSF3,1);
			ud_set_traverse_mask(FPAX1,1);
			ud_set_traverse_mask(FPAX2,1);
			ud_set_traverse_mask(FPCS1,1);
			ud_set_traverse_mask(FPCS2,1);
			if (Sgpsf2.key != 0) ud_set_traverse_mask(FPCS3,1);
			fno = FPAX1; tval.frmint = &Tpftax; OnPsTog(&fno,&tval,stat);
		}
		break;
	case FPSF3:
		Tpsf[0] = '\0';
		ud_update_answer(FPSF2,(int *)Tpsf);
		S_unhilite_entity(&Sgpsf);
		ud_set_traverse_mask(FPSF3,0);
		for (i=FPAX1;i<=FPTL8;i++) ud_set_traverse_mask(i,0);
		break;
	case FPCS1:
		status = S_select_geo(&Sgpsf2,UU_NULL,UD_ncl_allsfpl,UU_FALSE,668,Tcscol,
			0,FPCS2,Tpfcs);
		if (Sgpsf2.key != 0) ud_set_traverse_mask(FPCS3,1);
		break;
	case FPCS3:
		Tpfcs[0] = '\0';
		ud_update_answer(FPCS2,(int *)Tpfcs);
		S_unhilite_entity(&Sgpsf2);
		ud_set_traverse_mask(FPCS3,0);
		break;
	}
/*
.....Check for change since preview
*/
	if (status == UU_SUCCESS)
	{
		Schgmade = UU_TRUE;
		Sacc[Part] = 1;
	}
	S_section_changed(Smode[Part],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnPsTog(fieldno,val,stat)
**       Method called when a Part Surface toggle field is changed.
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
static UD_FSTAT OnPsTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int fno;
	UU_LOGICAL ifl;
	UD_DDATA tval;
/*
.....Process Part Surface toggle field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FPAX1:
		ifl = Tpftax != 0;
		ud_set_traverse_mask(FPAX2,Tpftax==2);
		ud_set_traverse_mask(FPTL1,Tpftax==1);
		ud_set_traverse_mask(FPTL2,Tpftax==1);
		ud_set_traverse_mask(FPCS1,ifl);
		ud_set_traverse_mask(FPCS2,ifl);
		if (Tpftax != 0 && Sgpsf2.key != 0) ud_set_traverse_mask(FPCS3,1);
		else ud_set_traverse_mask(FPCS3,0);
		if (Tpftax != 1) Tpftilt[0] = Tpftilt[1] = 0;
		fno = FPTL1; tval.frmint = &Tpftilt[0]; OnPsTog(&fno,&tval,stat);
		fno = FPTL2; tval.frmint = &Tpftilt[1]; OnPsTog(&fno,&tval,stat);
		if (Spftax != Tpftax)
		{
			Schgmade = UU_TRUE;
			Sacc[Part] = 1;
		}
		break;
	case FPTL1:
		ud_set_traverse_mask(FPTL3,Tpftilt[0]);
		ud_set_traverse_mask(FPTL5,Tpftilt[0]);
		ud_set_traverse_mask(FPTL6,Tpftilt[0]);
		break;
	case FPTL2:
		ud_set_traverse_mask(FPTL4,Tpftilt[1]);
		ud_set_traverse_mask(FPTL7,Tpftilt[1]);
		ud_set_traverse_mask(FPTL8,Tpftilt[1]);
		break;
	}
	S_section_changed(Smode[Part],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnPsTxt(fieldno,val,stat)
**       Method called when a Part Surface text field is changed.
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
static UD_FSTAT OnPsTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int ifl;
/*
.....Process Part Surface text field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FPSF2:
		ul_to_upper(Tpsf);
		if (strcmp(Spsf,Tpsf) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Part] = 1;
		}
		S_init_geo(&Sgpsf,Tpsf,Tpfcol);
		ifl = Sgpsf.key != 0;
		ud_set_traverse_mask(FPSF3,ifl);
		ud_set_traverse_mask(FPAX1,ifl);
		break;
	case FPCS2:
		ul_to_upper(Tpfcs);
		if (strcmp(Spfcs,Tpfcs) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Part] = 1;
		}
		S_init_geo(&Sgpsf2,Tpfcs,Tcscol);
		ifl = Sgpsf2.key != 0;
		ud_set_traverse_mask(FPCS3,ifl);
		break;
	}
	S_section_changed(Smode[Part],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnOpTog(fieldno,val,stat)
**       Method called when an Option box checkbox field is changed.
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
static UD_FSTAT OnOpTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Process Option toggle field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FOSCB:
		if (Smaxdpbox != Tmaxdpbox) 
		{
			Schgmade = UU_TRUE;
			Sacc[Options] = 1;
		}
		ud_set_traverse_mask(FOSTP,Tmaxdpbox);
		break;
	case FOFCB:
		if (Sfilbox != Tfilbox) 
		{
			Schgmade = UU_TRUE;
			Sacc[Options] = 1;
		}
		ud_set_traverse_mask(FOFIL,Tfilbox);
		ud_set_traverse_mask(FOFBT,Tfilbox);
		break;
	case FOFBT:
		nclu_arcslp_fillet_form(&Sfillet,&Scfill,UU_FALSE,Sarcfl);
		Sarcfl = UU_TRUE;
		break;
	}
	S_section_changed(Smode[Options],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
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
	int nc;
	char *spt,*tpt,*mpt;
/*
.....Check for change since preview
*/
	switch(*fieldno)
	{
	case FDCV2:
		spt = Scurve; tpt = Tcurve; mpt = UU_NULL;
		S_unhilite_entities(&Sglst);
		nc = strlen(Tcurve);
		ul_strip_blanks(Tcurve,&nc);
		if (nc > 0)
		{
			ud_set_traverse_mask(FAPV,1);
			ud_set_traverse_mask(FAPY,1);
			ud_set_traverse_mask(FAGE,1);
			S_section_changed(Smode[Curve],Sgreen,UU_TRUE,UU_TRUE);
			ud_frm_enable_ok(UU_TRUE);
			ud_dispfrm_set_attribs(0,FDCV1,UM_BLACK,Tdscol);
			S_init_geo(&Sgcv,Tcurve,Tdscol);
			S_push_geo(&Sglst,&Sgcv,&Sgeo_init);
		}
		else
		{
			ud_set_traverse_mask(FAPV,0);
			ud_set_traverse_mask(FAPY,0);
			ud_set_traverse_mask(FAGE,0);
			S_section_changed(Smode[Curve],Sred,UU_TRUE,UU_FALSE);
			ud_frm_enable_ok(UU_FALSE);
			ud_dispfrm_set_attribs(0,FDCV1,UM_WHITE,UM_RED);
		}
		if (stricmp(Tcurve, Scurve)!=0)
			Sacc[Curve] = 1;
		break;
	case FDCV5:
		spt = Sdsthk_str; tpt = Tdsthk_str; mpt = UU_NULL;
		if (strcmp(spt,tpt) != 0)
			Sacc[Curve] = 1;
		break;
	case FDDR5:
		spt = Slapdis; tpt = Tlapdis; mpt = Smode[Start];
		if (strcmp(spt,tpt) != 0)
			Sacc[Part] = 1;
		break;
	case FPTHK:
		spt = Spfthk_str; tpt = Tpfthk_str; mpt = Smode[Part];
		if (strcmp(spt,tpt) != 0)
			Sacc[Part] = 1;
		break;
	case FPAX2:
		spt = Spfang_str; tpt = Tpfang_str; mpt = Smode[Part];
		if (strcmp(spt,tpt) != 0)
			Sacc[Part] = 1;
		break;
	case FPTL3:
		spt = Spftan_str[0]; tpt = Tpftan_str[0]; mpt = Smode[Part];
		if (strcmp(spt,tpt) != 0)
			Sacc[Part] = 1;
		break;
	case FPTL4:
		spt = Spftan_str[1]; tpt = Tpftan_str[1]; mpt = Smode[Part];
		if (strcmp(spt,tpt) != 0)
			Sacc[Part] = 1;
		break;
	case FPTL6:
		spt = Spftds_str[0]; tpt = Tpftds_str[0]; mpt = Smode[Part];
		if (strcmp(spt,tpt) != 0)
			Sacc[Part] = 1;
		break;
	case FPTL8:
		spt = Spftds_str[1]; tpt = Tpftds_str[1]; mpt = Smode[Part];
		if (strcmp(spt,tpt) != 0)
			Sacc[Part] = 1;
		break;
	case FOFIL:
		spt = Sfilang_str; tpt = Tfilang_str; mpt = Smode[Options];
		if (strcmp(spt,tpt) != 0)
			Sacc[Options] = 1;
		break;
	case FOSTP:
		spt = Smaxdp_str; tpt = Tmaxdp_str; mpt = Smode[Options];
		if (strcmp(spt,tpt) != 0)
			Sacc[Options] = 1;
		break;
	}

	if (strcmp(spt,tpt) != 0)
	{
		Schgmade = UU_TRUE;
		if (mpt != UU_NULL) S_section_changed(mpt,Sgreen,UU_TRUE,UU_TRUE);
	}
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnEnTog(fieldno,val,stat)
**       Method called when an Entry / Exit form toggle field is changed.
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
static UD_FSTAT OnEnTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL ifl1,ifl2;
/*
.....Process Entry / Exit toggle field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FETYP:
		ifl1 = ifl2 = UU_FALSE;
		if (Tentry == 1 || Tentry == 3) ifl1 = UU_TRUE;
		if (Tentry == 2 || Tentry == 3) ifl2 = UU_TRUE;
		ud_set_traverse_mask(FEDIS,ifl1);
		ud_set_traverse_mask(FEANG,ifl1);
		ud_set_traverse_mask(FERS1,ifl1);
		ud_set_traverse_mask(FERAD,ifl2);
		ud_set_traverse_mask(FERS2,ifl2);
		if (Sentry != Tentry)
		{
			Schgmade = UU_TRUE;
			Sacc[Entry] = 1;
		}
		break;
	case FXTYP:
		ifl1 = ifl2 = UU_FALSE;
		if (Texit == 1 || Texit == 3) ifl1 = UU_TRUE;
		if (Texit == 2 || Texit == 3) ifl2 = UU_TRUE;
		ud_set_traverse_mask(FXDIS,ifl1);
		ud_set_traverse_mask(FXANG,ifl1);
		ud_set_traverse_mask(FXRS1,ifl1);
		ud_set_traverse_mask(FXRAD,ifl2);
		ud_set_traverse_mask(FXRS2,ifl2);
		if (Sexit != Texit)
		{
			Schgmade = UU_TRUE;
			Sacc[Entry] = 1;
		}
		break;
	}
	S_section_changed(Smode[Entry],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnEnTxt(fieldno,val,stat)
**       Method called when an Entry / Exit text field is traversed.
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
static UD_FSTAT OnEnTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Check for change since preview
*/
	switch(*fieldno)
	{
	case FEDIS:
		if (strcmp(Sendis_str,Tendis_str) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Entry] = 1;
		}
		break;
	case FEANG:
		if (strcmp(Senang_str,Tenang_str) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Entry] = 1;
		}
		break;
	case FERS1:
		if (strcmp(Senris1_str,Tenris1_str) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Entry] = 1;
		}
		break;
	case FERAD:
		if (strcmp(Senrad_str,Tenrad_str) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Entry] = 1;
		}
		break;
	case FERS2:
		if (strcmp(Senris2_str,Tenris2_str) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Entry] = 1;
		}
		break;
	case FXDIS:
		if (strcmp(Sexdis_str,Texdis_str) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Entry] = 1;
		}
		break;
	case FXANG:
		if (strcmp(Sexang_str,Texang_str) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Entry] = 1;
		}
		break;
	case FXRS1:
		if (strcmp(Sexris1_str,Texris1_str) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Entry] = 1;
		}
		break;
	case FXRAD:
		if (strcmp(Sexrad_str,Texrad_str) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Entry] = 1;
		}
		break;
	case FXRS2:
		if (strcmp(Sexris2_str,Texris2_str) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Entry] = 1;
		}
		break;
	}
	S_section_changed(Smode[Entry],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnFgTog(fieldno,val,stat)
**       Method called when an Final Pass form toggle field is changed.
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
static UD_FSTAT OnCutcomTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Check for change since preview
*/
	switch(*fieldno)
	{
	case FTCOM:
 		if (Tcutcom)
		{
			ud_set_traverse_mask(FTCSD,UU_TRUE);
			ud_set_traverse_mask(FTCPL,UU_TRUE);
			ud_set_traverse_mask(FTCTX,UU_TRUE);
			ud_set_traverse_mask(FTPDS,UU_TRUE);
			ud_set_traverse_mask(FTSRT,UU_TRUE);
		}
		else
		{
			ud_set_traverse_mask(FTCSD,UU_FALSE);
			ud_set_traverse_mask(FTCPL,UU_FALSE);
			ud_set_traverse_mask(FTCTX,UU_FALSE);
			ud_set_traverse_mask(FTPDS,UU_FALSE);
			ud_set_traverse_mask(FTSRT,UU_FALSE);
		}
		if (Scutcom != Tcutcom)
		{
			Schgmade = UU_TRUE;
			Sacc[Options] = 1;
		}
		break;
	case FTCSD:
		if (Scutcomdir != Tcutcomdir)
		{
			Schgmade = UU_TRUE;
			Sacc[Options] = 1;
		}
		break;
	case FTCPL:
		if (Scutcompln != Tcutcompln)
		{
			Schgmade = UU_TRUE;
			Sacc[Options] = 1;
		}
		break;
	case FTSRT:
		if (Scutcomsrt != Tcutcomsrt)
		{
			Schgmade = UU_TRUE;
			Sacc[Options] = 1;
		}
		break;
	}
	S_section_changed(Smode[Options],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnFinlTxt(fieldno,val,stat)
**       Method called when a Final Pass text field is traversed.
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
static UD_FSTAT OnFinlTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Check for change since preview
*/
	switch(*fieldno)
	{
	case FTCTX:
		if (strcmp(Scutcom_str,Tcutcom_str) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Options] = 1;
		}
		break;
	case FTPDS:
		if (strcmp(Scutcompds_str,Tcutcompds_str) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Options] = 1;
		}
		break;
	}
	S_section_changed(Smode[Options],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
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
		status = S_select_geo(&Sgcp,UU_NULL,UD_ncl_pl,UU_FALSE,472,Tpoccol,0,FPCL2,Tpocpln);
		strcpy(Tpocpln_str1, Tpocpln);
		break;
	case FPRP3:
		status = S_select_geo(&Sgrap,UU_NULL,UD_ncl_pl,UU_FALSE,659,Tpopcol,0,FPRP2,Tpoppln);
		strcpy(Tpoppln_str1, Tpoppln);
		break;
	case FPRT3:
		status = S_select_geo(&Sgret,UU_NULL,UD_ncl_pl,UU_FALSE,660,Tporcol,0,FPRT2,Tporpln);
		strcpy(Tporpln_str1, Tporpln);
		break;
	}
	if (status == UU_SUCCESS)
	{
		Schgmade = UU_TRUE;
		Sacc[Posit] = 1;
	}
	S_section_changed(Smode[Posit],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
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
			strcpy(Tpocpln, Tpocpln_str1);
			ud_update_answer(FPCL2, &Tpocpln);
		}
		else
		{
			ud_set_traverse_mask(FPCL2,1);
			ud_set_traverse_mask(FPCL3,0);
			if (Tpoclr == 2)
			{
				strcpy(Tpocpln, Tpocpln_str2);
				ud_update_answer(FPCL2, &Tpocpln);
			}
			else
			{
				strcpy(Tpocpln, Tpocpln_str3);
				ud_update_answer(FPCL2, &Tpocpln);
			}
		}
		if (Spoclr != Tpoclr)
		{
			Schgmade = UU_TRUE;
			Sacc[Posit] = 1;
		}
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
			strcpy(Tpoppln, Tpoppln_str1);
			ud_update_answer(FPRP2, &Tpoppln);
		}
		else
		{
			ud_set_traverse_mask(FPRP2,1);
			ud_set_traverse_mask(FPRP3,0);
			if (Tporap == 2)
			{
				strcpy(Tpoppln, Tpoppln_str2);
				ud_update_answer(FPRP2, &Tpoppln);
			}
			else
			{
				strcpy(Tpoppln, Tpoppln_str3);
				ud_update_answer(FPRP2, &Tpoppln);
			}
		}
		if (Sporap != Tporap)
		{
			Schgmade = UU_TRUE;
			Sacc[Posit] = 1;
		}
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
			strcpy(Tporpln, Tporpln_str1);
			ud_update_answer(FPRT2, &Tporpln);
		}
		else
		{
			ud_set_traverse_mask(FPRT2,1);
			ud_set_traverse_mask(FPRT3,0);
			if (Tporet == 3)
			{
				strcpy(Tporpln, Tporpln_str2);
				ud_update_answer(FPRT2, &Tporpln);
			}
			else
			{
				strcpy(Tporpln, Tporpln_str3);
				ud_update_answer(FPRT2, &Tporpln);
			}
		}
		if (Sporet != Tporet)
		{
			Schgmade = UU_TRUE;
			Sacc[Posit] = 1;
		}
		break;
	}
	S_section_changed(Smode[Posit],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
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
/*
.....Process Positioning text field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FPCL2:
		if (strcmp(Spocpln,Tpocpln) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Posit] = 1;
		}
		ul_to_upper(Tpocpln);
/*
.....only if the choice is plain
*/
		if (Tpoclr==1)
		{
			S_init_geo(&Sgcp,Tpocpln,Tpoccol);
			strcpy(Tpocpln_str1, Tpocpln);
		}
		else if (Tpoclr==2)
		{
			strcpy(Tpocpln_str2, Tpocpln);
		}
		else if (Tpoclr==3)
		{
			strcpy(Tpocpln_str3, Tpocpln);
		}
		break;
	case FPRP2:
		if (strcmp(Spoppln,Tpoppln) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Posit] = 1;
		}
		ul_to_upper(Tpoppln);
/*
.....only if the choice is plain
*/
		if (Tporap==1)
		{
			S_init_geo(&Sgrap,Tpoppln,Tpopcol);
			strcpy(Tpoppln_str1, Tpoppln);
		}
		else if (Tporap==2)
		{
			strcpy(Tpoppln_str2, Tpoppln);
		}
		else if (Tporap==3)
		{
			strcpy(Tpoppln_str3, Tpoppln);
		}
		break;
	case FPRT2:
		if (strcmp(Sporpln,Tporpln) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Posit] = 1;
		}
		ul_to_upper(Tporpln);
/*
.....only if the choice is plain
*/
		if (Tporet==1)
		{
			S_init_geo(&Sgret,Tporpln,Tporcol);
			strcpy(Tporpln_str1, Tporpln);
		}
		else if (Tporet==2)
		{
			strcpy(Tporpln_str2, Tporpln);
		}
		else if (Tporet==3)
		{
			strcpy(Tporpln_str3, Tporpln);
		}
		break;
	case FPLOP:
		if (strcmp(Sportl_str,Tportl_str) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Posit] = 1;
		}
		break;
	}
	S_section_changed(Smode[Posit],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnLpSel(fieldno,val,stat)
**       Method called when a Loop button is pressed.
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
static UD_FSTAT OnLpSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status;
/*
.....Process Loop pushbuttons
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FLDP3:
		status = S_select_geo(&Sgtpl,UU_NULL,UD_ncl_pl,UU_FALSE,669,Tlpdcol,0,FLDP2,Tlptpl);
		strcpy(Tlptpl_str1, Tlptpl);
		break;
	}
	if (status == UU_SUCCESS)
	{
		Schgmade = UU_TRUE;
		Sacc[Passes] = 1;
	}
	S_section_changed(Smode[Passes],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnLpTog(fieldno,val,stat)
**       Method called when a Loop toggle field is changed.
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
static UD_FSTAT OnLpTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Process Loop toggle field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FLOF1:
		if (Tlpoft == 0)
		{
			ud_set_traverse_mask(FLOF2,0);
			ud_set_traverse_mask(FLOF3,0);
			ud_set_traverse_mask(FLOS1,0);
			ud_set_traverse_mask(FLOS2,0);
		}
		else
		{
			ud_set_traverse_mask(FLOF2,1);
			ud_set_traverse_mask(FLOF3,1);
			ud_set_traverse_mask(FLOS1,1);
			ud_set_traverse_mask(FLOS2,1);
			if (Tlpoft == 2 && Tlpofs != 1)
			{
				Tlpofs = 1;
/*
.....field match wrong here
.....yurong	
*/
//				ud_update_answer(FLOS1,&Tlpofs);
				ud_update_answer(FLOF3,&Tlpofs);
			}
			if (Tlpoft == 1)
				strcpy(Tlpofth_str, Tlpofth_str1);
			else if (Tlpoft == 2)
				strcpy(Tlpofth_str, Tlpofth_str2);
			ud_update_answer(FLOF2,&Tlpofth_str);
		}
		if (Tlpoft != 0 && Tlpdpt != 0) ud_set_traverse_mask(FLFST,1);
		else ud_set_traverse_mask(FLFST,0);
		if (Slpoft != Tlpoft)
		{
			Schgmade = UU_TRUE;
			Sacc[Passes] = 1;
		}
		break;
	case FLOS2:
		if (Slpofx != Tlpofx)
		{
			Schgmade = UU_TRUE;
			Sacc[Passes] = 1;
		}
		break;
	case FLOF3:
		if (Tlpoft == 2 && Tlpofs != 1)
		{
			Tlpofs = 1;
/*
.....field match wrong here
.....yurong	
*/
//			ud_update_answer(FLOS1,&Tlpofs);
			ud_update_answer(FLOF3,&Tlpofs);
		}
		if (Slpofs != Tlpofs)
		{
			Schgmade = UU_TRUE;
			Sacc[Passes] = 1;
		}
		if (Tlpofs == 0)
			strcpy(Tlpofst_str, Tlpofst_str1);
		else if (Tlpoft == 1)
			strcpy(Tlpofst_str, Tlpofst_str2);
		ud_update_answer(FLOS1, &Tlpofst_str);
		break;
	case FLDP1:
		if (Tlpdpt == 0)
		{
			ud_set_traverse_mask(FLDP2,0);
			ud_set_traverse_mask(FLDP3,0);
			ud_set_traverse_mask(FLDS1,0);
			ud_set_traverse_mask(FLDS2,0);
		}
		else if (Tlpdpt == 1)
		{
			ud_set_traverse_mask(FLDP2,1);
			ud_set_traverse_mask(FLDP3,1);
			ud_set_traverse_mask(FLDS1,1);
			ud_set_traverse_mask(FLDS2,1);
			strcpy(Tlptpl, Tlptpl_str1);
			ud_update_answer(FLDP2, &Tlptpl);
		}
		else
		{
			ud_set_traverse_mask(FLDP2,1);
			ud_set_traverse_mask(FLDP3,0);
			ud_set_traverse_mask(FLDS1,1);
			ud_set_traverse_mask(FLDS2,1);
			if (Tlpdpt == 3 && Tlpdps != 1)
			{
				Tlpdps = 1;
				ud_update_answer(FLDS1,&Tlpdps);
			}
		}
		if (Tlpoft != 0 && Tlpdpt != 0) ud_set_traverse_mask(FLFST,1);
		else ud_set_traverse_mask(FLFST,0);
		if (Slpdpt != Tlpdpt)
		{
			Schgmade = UU_TRUE;
			Sacc[Passes] = 1;
		}
		if (Tlpdpt == 2)
			strcpy(Tlptpl, Tlptpl_str2);
		else if (Tlpdpt == 3)
			strcpy(Tlptpl, Tlptpl_str3);
		ud_update_answer(FLDP2, &Tlptpl);
		break;
	case FLDS1:
		if (Tlpdpt == 3 && Tlpdps != 1)
		{
			Tlpdps = 1;
			ud_update_answer(FLDS1,&Tlpdps);
		}
		if (Slpdps != Tlpdps)
		{
			Schgmade = UU_TRUE;
			Sacc[Passes] = 1;
		}
		if (Tlpdps == 0)
			strcpy(Tlpdpst_str, Tlpdpst_str1);
		else if (Tlpdps == 1)
			strcpy(Tlpdpst_str, Tlpdpst_str2);
		ud_update_answer(FLDS2, &Tlpdpst_str);
		break;
	case FLFST:
		if (Slpdfi != Tlpdfi)
		{
			Schgmade = UU_TRUE;
			Sacc[Passes] = 1;
		}
		break;
	}
	S_section_changed(Smode[Passes],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnLpTxt(fieldno,val,stat)
**       Method called when a Loop text field is changed.
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
static UD_FSTAT OnLpTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Process Loop text field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FLOF2:
		if (strcmp(Slpofth_str,Tlpofth_str) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Passes] = 1;
		}
		if (Tlpoft==1)
			strcpy(Tlpofth_str1, Tlpofth_str);
		else if (Tlpoft==2)
			strcpy(Tlpofth_str2, Tlpofth_str);
		break;
	case FLOS1:
		if (strcmp(Slpofst_str,Tlpofst_str) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Passes] = 1;
		}
		if (Tlpofs==0)
			strcpy(Tlpofst_str1, Tlpofst_str);
		else if (Tlpoft==1)
			strcpy(Tlpofst_str2, Tlpofst_str);
		break;
	case FLDP2:
		if (strcmp(Slpofth_str,Tlpofth_str) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Passes] = 1;
		}
		ul_to_upper(Tlptpl);
/*
.....only for plain
*/
		if (Tlpdpt == 1)
		{
			S_init_geo(&Sgtpl,Tlptpl,Tlpdcol);
			strcpy(Tlptpl_str1, Tlptpl);
		}
		else if (Tlpdpt == 2)
			strcpy(Tlptpl_str2, Tlptpl);
		else if (Tlpdpt == 3)
			strcpy(Tlptpl_str3, Tlptpl);
		break;
	case FLDS2:
		if (strcmp(Slpdpst_str,Tlpdpst_str) != 0)
		{
			Schgmade = UU_TRUE;
			Sacc[Passes] = 1;
		}
		break;
	}
	S_section_changed(Smode[Passes],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
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
	int i;
/*
.....Process Feed Rate toggle field
*/
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
	case FFTG5:
		i = 4;
		if (Tfopt[4]==0)
		{
			ud_setfrm_traverse_mask(0,FFED5,UU_FALSE);
			valuestr[0] = '\0';
			if (Tfeed_str[4][0]!='\0')
			{
				if (Ssav_fchc[4]==1)
					strcpy(Sav_valuestr5[0], Tfeed_str[4]);
				else if (Ssav_fchc[4]==3)
					strcpy(Sav_valuestr5[1], Tfeed_str[4]);
			}
			ud_dispfrm_update_answer(0,FFED5,(int *)valuestr);
		}
		else if (Tfopt[4]==2)
		{
/*
.....rapid
*/
			ud_setfrm_traverse_mask(0,FFED5,UU_FALSE);
			if (Tfeed_str[4][0]!='\0')
			{
				if (Ssav_fchc[4]==1)
					strcpy(Sav_valuestr5[0], Tfeed_str[4]);
				else if (Ssav_fchc[4]==3)
					strcpy(Sav_valuestr5[1], Tfeed_str[4]);
			}
			valuestr[0] = '\0';
			ud_dispfrm_update_answer(0,FFED5,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(0,FFED5,UU_TRUE);
			if (Tfopt[4]==1)
			{
				if (Tfeed_str[4][0]!='\0')
				{
					if (Ssav_fchc[4]==3)
						strcpy(Sav_valuestr5[1], Tfeed_str[4]);
				}
				strcpy(Tfeed_str[4], Sav_valuestr5[0]);
			}
			else if (Tfopt[4]==3)
			{
				if (Tfeed_str[4][0]!='\0')
				{
					if (Ssav_fchc[4]==1)
						strcpy(Sav_valuestr5[0], Tfeed_str[4]);
				}
				strcpy(Tfeed_str[4], Sav_valuestr5[1]);
			}
			ud_dispfrm_update_answer(0,FFED5,(int *)Tfeed_str[4]);
		}	
		Ssav_fchc[4] = *(val->frmint);
		break;
	case FFTG6:
		i = 5;
		if (Tfopt[5]==0)
		{
			ud_setfrm_traverse_mask(0,FFED6,UU_FALSE);
			valuestr[0] = '\0';
			if (Tfeed_str[5][0]!='\0')
			{
				if (Ssav_fchc[5]==1)
					strcpy(Sav_valuestr6[0], Tfeed_str[5]);
				else if (Ssav_fchc[5]==3)
					strcpy(Sav_valuestr6[1], Tfeed_str[5]);
			}
			ud_dispfrm_update_answer(0,FFED6,(int *)valuestr);
		}
		else if (Tfopt[5]==2)
		{
/*
.....rapid
*/
			ud_setfrm_traverse_mask(0,FFED6,UU_FALSE);
			if (Tfeed_str[5][0]!='\0')
			{
				if (Ssav_fchc[5]==1)
					strcpy(Sav_valuestr6[0], Tfeed_str[5]);
				else if (Ssav_fchc[5]==3)
					strcpy(Sav_valuestr6[1], Tfeed_str[5]);
			}
			valuestr[0] = '\0';
			ud_dispfrm_update_answer(0,FFED6,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(0,FFED6,UU_TRUE);
			if (Tfopt[5]==1)
			{
				if (Ssav_fchc[5]==3)
				{
					if (Tfeed_str[5][0]!='\0')
						strcpy(Sav_valuestr6[1], Tfeed_str[5]);
				}
				strcpy(Tfeed_str[5], Sav_valuestr6[0]);
			}
			else if (Tfopt[5]==3)
			{
				if (Ssav_fchc[5]==1)
				{
					if (Tfeed_str[5][0]!='\0')
						strcpy(Sav_valuestr6[0], Tfeed_str[5]);
				}
				strcpy(Tfeed_str[5], Sav_valuestr6[1]);
			}
			ud_dispfrm_update_answer(0,FFED6,(int *)Tfeed_str[5]);
		}	
		Ssav_fchc[5] = *(val->frmint);
		break;
	}
/*
.....Check for change since preview
*/
	if (Sfopt[i] != Tfopt[i])
	{
		Schgmade = UU_TRUE;
		Sacc[Feeds] = 1;
	}
/*
	if (*(val->frmint) == 0 || *(val->frmint) == 2)
		ud_set_traverse_mask((*fieldno)+1,UU_FALSE);
	else
		ud_set_traverse_mask((*fieldno)+1,UU_TRUE);
*/
	S_section_changed(Smode[Feeds],Sgreen,UU_TRUE,UU_TRUE);
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnFeedTxt(fieldno,val,stat)
**       Method called the Rmill Thicks text fields are traversed to
**       look for changes.
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
static UD_FSTAT OnFeedTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i = -1;

	switch(*fieldno)
	{
	case FFED1:
		i = 0;
		break;
	case FFED2:
		i = 1;
		break;
	case FFED3:
		i = 2;
		break;
	case FFED4:
		i = 3;
		break;
	case FFED5:
		i = 4;
		break;
	case FFED6:
		i = 5;
		break;
	}
/*
.....Check for change since preview
*/
	S_section_changed(Smode[Feeds],Sgreen,UU_TRUE,UU_TRUE);
	if (i >= 0 && strcmp(Sfeed_str[i],Tfeed_str[i]) != 0)
	{
		Schgmade = UU_TRUE;
		Sacc[Feeds] = 1;
	}
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
		ud_dispfrm_set_attribs(0,FDST2,UM_BLACK,Tstcol);
		S_hilite_entity(&Sgspt,Tstcol);
		break;
	case FCOLC:
		ud_dispfrm_set_attribs(0,FDEN2,UM_BLACK,Tencol);
		S_hilite_entity(&Sgept,Tencol);
		break;
	case FCOLE:
		ud_dispfrm_set_attribs(0,FPSF1,UM_BLACK,Tpfcol);
		S_hilite_entity(&Sgpsf,Tpfcol);
		break;
	case FCOLF:
		ud_dispfrm_set_attribs(0,FPCS1,UM_BLACK,Tcscol);
		S_hilite_entity(&Sgpsf2,Tcscol);
		break;
	case FCOLG:
		ud_dispfrm_set_attribs(0,FPCL3,UM_BLACK,Tpoccol);
		S_hilite_entity(&Sgcp,Tpoccol);
		break;
	case FCOLH:
		ud_dispfrm_set_attribs(0,FPRP3,UM_BLACK,Tpopcol);
		break;
	case FCOLI:
		ud_dispfrm_set_attribs(0,FPRT3,UM_BLACK,Tporcol);
		S_hilite_entity(&Sgret,Tporcol);
		break;
	case FCOLJ:
		ud_dispfrm_set_attribs(0,FLDP3,UM_BLACK,Tlpdcol);
		S_hilite_entity(&Sgtpl,Tlpdcol);
	case FCOLK:
		Tgeotog = val->frmint[0];
	case FCOLL:
		if (Sgeo_redraw)
		{
			Sgeo_redraw = UU_FALSE;
			fno = FAGE;
			OnAction(&fno,val,stat);
		}
		break;
	}
	Sacc[Colors] = 1;
	S_enable_buttons();
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
**       Method called when the View button is pressed.
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
			ud_wrerr("You must select a curve to profile.");
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
		{
			break;
		}	
/*
.....Modified as shown above		
			S_build_command(UU_TRUE);
*/
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
		Sarcfl = UU_FALSE;
		Sacc[Curve] = Sacc[Start] = Sacc[Part] = Sacc[Entry] = Sacc[Posit] = 0;
		Sacc[Passes] = Sacc[Options] = Sacc[Feeds] = Sacc[Colors] = 0;
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
	S_enable_buttons();
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
.....Initialize Curve fields
*/
	traverse[FDCV5] = Tdsthfl;
	traverse[FDCV6] = 1;
	if (Tdscon == 0 && Tdsthfl == 0) traverse[FDCV6] = 0;
/*
.....Initialize Drive surface fields
*/
	if (Tlapflg == 0) traverse[FDDR5] = 0;
	else traverse[FDDR5] = 1;
	traverse[FDST2] = traverse[FDST3] = Tstart;
	if (Tdir == 1 || Tdir == 2)
		traverse[FDDR2] = traverse[FDDR3] = 1;
	else
		traverse[FDDR2] = traverse[FDDR3] = 0;
	traverse[FDEN2] = traverse[FDEN3] = Tend;
/*
.....Part surface fields
*/
	if (Sgpsf.key == 0)
	{
		traverse[FPSF3] = 0;
		for (i=FPAX1;i<=FPTL8;i++) traverse[i] = 0;
	}
	else
	{
		traverse[FPSF3] = traverse[FPAX1] = 1;
		traverse[FPAX2] = Tpftax == 2;
		if (Tpftax != 1)
		{
			for (i=FPTL1;i<=FPTL8;i++) traverse[i] = 0;
		}
		else
		{
			traverse[FPTL1] = traverse[FPTL2] = 1;
			traverse[FPTL3] = traverse[FPTL5] = traverse[FPTL6] = Tpftilt[0];
			traverse[FPTL4] = traverse[FPTL7] = traverse[FPTL8] = Tpftilt[1];
		}
		traverse[FPCS1] = 0; traverse[FPCS2] =traverse[FPCS3] = 0;
		if (Tpftax != 0)
		{
			traverse[FPCS1] = traverse[FPCS2] = 1;
			if (Sgpsf2.key != 0) traverse[FPCS3] = 1;
		}
	}
/*
.....Entry / Exit fields
*/
	if (Tentry == 1 || Tentry == 3)
		traverse[FEDIS] = traverse[FEANG] = traverse[FERS1] = 1;
	else
		traverse[FEDIS] = traverse[FEANG] = traverse[FERS1] = 0;
	if (Tentry == 2 || Tentry == 3)
		traverse[FERAD] = traverse[FERS2] = 1;
	else
		traverse[FERAD] = traverse[FERS2] = 0;
	if (Texit == 1 || Texit == 3)
		traverse[FXDIS] = traverse[FXANG] = traverse[FXRS1] = 1;
	else
		traverse[FXDIS] = traverse[FXANG] = traverse[FXRS1] = 0;
	if (Texit == 2 || Texit == 3)
		traverse[FXRAD] = traverse[FXRS2] = 1;
	else
		traverse[FXRAD] = traverse[FXRS2] = 0;
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
.....Passes fields
*/
	if (Tdscon == 0)
	{
		traverse[FLOF1] = 0;
		Tlpoft = 0;
	}
	if (Tlpoft == 0)
		traverse[FLOF2] = traverse[FLOF3] = traverse[FLOS1] = traverse[FLOS2] = 0;
	else
		traverse[FLOF2] = traverse[FLOF3] = traverse[FLOS1] = traverse[FLOS2] = 1;
	if (Tlpdpt == 0)
	{
		traverse[FLDP2] = traverse[FLDP3] = 0;
		traverse[FLDS1] = traverse[FLDS2] = 0;
	}
	else if (Tlpdpt == 1)
	{
		traverse[FLDP2] = traverse[FLDP3] = 1;
		traverse[FLDS1] = traverse[FLDS2] = 1;
	}
	else
	{
		traverse[FLDP2] = 1;
		traverse[FLDP3] = 0;
		traverse[FLDS1] = traverse[FLDS2] = 1;
	}
	if (Tlpoft != 0 && Tlpdpt != 0) traverse[FLFST] = 1;
	else traverse[FLFST] = 0;
/*
.....Final Pass fields
*/
	if (Tcutcom == 1)
	{
		traverse[FTCSD] = traverse[FTCPL] = traverse[FTCTX] = UU_TRUE;
		traverse[FTPDS] = traverse[FTSRT] = UU_TRUE;
	}
	else
	{
		traverse[FTCSD] = traverse[FTCPL] = traverse[FTCTX] = UU_FALSE;
		traverse[FTPDS] = traverse[FTSRT] = UU_FALSE;
	}
/*
.....Feed rate fields
*/
	for (i=0;i<6;i++)
	{
		if (Tfopt[i] == 0 || Tfopt[i] == 2) traverse[i*2+FFTG1+1] = 0;
		else traverse[i*2+FFTG1+1] = 1;
	}
/*
.....Option fields
*/
	traverse[FOSTP] = Tmaxdpbox;
	traverse[FOFIL] = Tfilbox;
	traverse[FOFBT] = Tfilbox;
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
	int i,typ;
	int fno;
	UD_DDATA tval;
	UD_FSTAT stat;
	UU_REAL cutr[6], feedrate;
	NCLX_mot_feedrate fedrat;
	UM_real8 fthick1,fthick2,toolaxangle, tiltanglebegin, tiltangleend, tildistbegin, tiltdistend,
		endis, enangle, enrise, enrad, enrise2,exdis, exangle, exrise, exrad, exrise2,clrdis, rptdis, rtrdis, lrtrdis, npas,
		cutcpdst, fdrt1, fdrt2, fdrt3, fdrt4, fdrt5, fdrt6 , fdrt[6];
	UM_int4 fnorm;
	prfpar (&fthick1, &fthick2, &toolaxangle, &tiltanglebegin, &tiltangleend, &tildistbegin, &tiltdistend,
		&endis, &enangle, &enrise, &enrad, &enrise2,&exdis, &exangle, &exrise, &exrad, &exrise2, &clrdis, &rptdis, &rtrdis, &lrtrdis,
		&npas, &cutcpdst, &fdrt1, &fdrt2, &fdrt3, &fdrt4, &fdrt5, &fdrt6, &fnorm );

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
.....Initialize the Drive Surface settings
*/
	if (!Sform_init)
	{
		Sdscon = 0;
		Sdscol = NCLX_SEA_GREEN;
		Sdsofd = 1;
		Sdsthfl = 0;
		ncl_sprintf(Sdsthk_str,&fthick1,1);
		Slapflg = 0;
		Slapdis[0] = '\0';
		Sstart = 0;
		Sstpt[0] = '\0';
		Sstcol = NCLX_PURPLE;
		Sdir = 0;
		Sdirvec[0] = '\0';
		Send = 0;
		Senpt[0] = '\0';
		Sencol = NCLX_PURPLE;
		Spsf[0] = '\0';
		Spfcol = NCLX_ORANGE;
		Spftax = 0;
		strcpy(Spfthk_str, "0.");
		Spfcs[0] = '\0';
		Scscol = NCLX_BROWN;
	}

	obcutr(cutr,&typ);
	UM_len_exttoint(cutr[0],Scdia);
	Scdia += .75;
	NclxMotGetTool(&Stend);
	UM_cc_exttoint(Stend.pt,Stend.pt);
	NclxMotGetFwd(Sfwd);
	ncl_sprintf(Sdirvec,Sfwd,3);
	Scurve[0] = '\0';
	Sdir = 0;

	strcpy(Tcurve,Scurve);
	Tdscon = Sdscon;
	Tdscol = Sdscol;
	Tdsofd = Sdsofd;
	Tdsthfl = Sdsthfl;
	ncl_sprintf(Sdsthk_str,&fthick1,1);
	strcpy(Tdsthk_str, Sdsthk_str);
	Tlapflg = Slapflg;
	strcpy(Tlapdis,Slapdis);
	Tstart = Sstart;
	strcpy(Tstpt,Sstpt);
	Tstcol = Sstcol;
	Tdir = Sdir;
	strcpy(Tdirvec,Sdirvec);
	Tend = Send;
	strcpy(Tenpt,Senpt);
	Tencol = Sencol;
/*
.....Initialize the Part Surface settings
*/
	if (!Sform_init)
	{
		Spsf[0] = '\0';
		Spfcol = NCLX_ORANGE;
		if (fnorm==1)
			Spftax = 1;		//to enable "normal" value in Tool Axis field
		if (Spftax == 1)
		{
			Tpftax = Spftax;
		}
		Tpftax = Spftax;
		ncl_sprintf(Spfthk_str,&fthick2,1);
		Spfcs[0] = '\0';
		Scscol = NCLX_BROWN;
		ncl_sprintf(Spfang_str,&toolaxangle,1);


		ncl_sprintf(Spftan_str[0],&tiltanglebegin,1);
		ncl_sprintf(Spftan_str[1],&tiltangleend,1);
		ncl_sprintf(Spftds_str[0],&tildistbegin,1);
		ncl_sprintf(Spftds_str[1],&tiltdistend,1);
		Spfdtyp[0] = Spfdtyp[1] = 0;

	}
	strcpy(Tpsf,Spsf);
	Tpfcol = Spfcol;
	Tpftax = Spftax;
	ncl_sprintf(Spfthk_str,&fthick2,1);
	strcpy(Tpfthk_str, Spfthk_str);
	strcpy(Tpfcs,Spfcs);
	Tcscol = Scscol;
	ncl_sprintf(Spfang_str,&toolaxangle,1);
	ncl_sprintf(Spftan_str[0],&tiltanglebegin,1);
	ncl_sprintf(Spftan_str[1],&tiltangleend,1);
	ncl_sprintf(Spftds_str[0],&tildistbegin,1);
	ncl_sprintf(Spftds_str[1],&tiltdistend,1);
	strcpy(Tpfang_str,Spfang_str);
	Tpftilt[0] = Spftilt[0]; Tpftilt[1] = Spftilt[1];
	strcpy(Tpftan_str[0],Spftan_str[0]); strcpy(Tpftan_str[1],Spftan_str[1]);
	Tpfdtyp[0] = Spfdtyp[0]; Tpfdtyp[1] = Spfdtyp[1];
	strcpy(Tpftds_str[0],Spftds_str[0]); strcpy(Tpftds_str[1],Spftds_str[1]);
/*
.....Initialize the Entry / Exit settings
*/
	if (!Sform_init)
	{
		Sentry = 0;

		ncl_sprintf(Sendis_str,&endis,1);
		ncl_sprintf(Senang_str,&enangle,1);
		ncl_sprintf(Senris1_str,&enrise,1);
		ncl_sprintf(Senrad_str,&enrad,1);
		ncl_sprintf(Senris2_str,&enrise2,1);
		Sexit = 0;
		ncl_sprintf(Sexdis_str,&exdis,1);
		ncl_sprintf(Sexang_str,&exangle,1);
		ncl_sprintf(Sexris1_str,&exrise,1);
		ncl_sprintf(Sexrad_str,&exrad,1);
		ncl_sprintf(Sexris2_str,&exrise2,1);
	}
	Tentry = Sentry;
	ncl_sprintf(Sendis_str,&endis,1);
	ncl_sprintf(Senang_str,&enangle,1);
	ncl_sprintf(Senris1_str,&enrise,1);
	ncl_sprintf(Senrad_str,&enrad,1);
	ncl_sprintf(Senris2_str,&enrise2,1);
	Sexit = 0;
	ncl_sprintf(Sexdis_str,&exdis,1);
	ncl_sprintf(Sexang_str,&exangle,1);
	ncl_sprintf(Sexris1_str,&exrise,1);
	ncl_sprintf(Sexrad_str,&exrad,1);
	ncl_sprintf(Sexris2_str,&exrise2,1);
	strcpy(Tendis_str, Sendis_str);
	strcpy(Tenrad_str, Senrad_str);
	strcpy(Tenang_str, Senang_str);
	strcpy(Tenris1_str, Senris1_str);
	strcpy(Tenris2_str, Senris2_str);
	Texit = Sexit;
	strcpy(Texdis_str, Sexdis_str);
	strcpy(Texrad_str, Sexrad_str);
	strcpy(Texang_str, Sexang_str);
	strcpy(Texris1_str, Sexris1_str);
	strcpy(Texris2_str, Sexris2_str);
/*
.....Initialize the Final Pass CUTCOM settings
*/
	if (!Sform_init)
	{
		Scutcom = 0;
		Scutcomdir = NONE;
		Scutcompln = NONE;
		strcpy(Scutcom_str, "");
		ncl_sprintf(Scutcompds_str,&cutcpdst,1);
		
		Scutcomsrt = ALL;
	}
	Tcutcom = Scutcom;
	Tcutcomdir = Scutcomdir;
	Tcutcompln = Scutcompln;
	strcpy(Tcutcom_str, Scutcom_str);
	ncl_sprintf(Scutcompds_str,&cutcpdst,1);
	strcpy(Tcutcompds_str, Scutcompds_str);
	Tcutcomsrt = Scutcomsrt;
/*
.....Initialize Option Box settings
*/
	if (!Sform_init)
	{
		Sfilbox = 0;
		Smaxdpbox = 0;
		strcpy(Sfilang_str, "30.");
		strcpy(Smaxdp_str, "0.");
	}
	Tfilbox = Sfilbox;
	Tmaxdpbox = Smaxdpbox;
	strcpy(Tfilang_str, Sfilang_str);
	strcpy(Tmaxdp_str, Smaxdp_str);
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
	Tpocpln_str1[0] = '\0';
	Tpoppln_str1[0] = '\0';
	Tporpln_str1[0] = '\0';
	strcpy(Tpocpln_str2, "0.");
	strcpy(Tpoppln_str2, "0.");
	strcpy(Tporpln_str2, "0.");
	strcpy(Tpocpln_str3, "0");
	strcpy(Tpoppln_str3, "0");
	strcpy(Tporpln_str3, "0");

	strcpy(Tlpofth_str1, "0.");
	strcpy(Tlpofth_str2, "1");
	strcpy(Tlpofst_str1, "1");
	strcpy(Tlpofst_str2, "0");

	Tlptpl_str1[0] = '\0';
	strcpy(Tlptpl_str2, "0.0");
	strcpy(Tlptpl_str3, "1");
	strcpy(Tlpdpst_str1, "1");
	strcpy(Tlpdpst_str2, "0");

	Tpoccol = Spoccol;
	Tporap = Sporap;
	strcpy(Tpoppln,Spoppln);
	Tpopcol = Spopcol;
	Tporet = Sporet;
	strcpy(Tporpln,Sporpln);
	Tporcol = Sporcol;
	strcpy(Tportl_str, Sportl_str);
/*
.....Initialize the Loop settings
*/
	if (!Sform_init)
	{
		Slpoft = 0;
		strcpy(Slpofth_str, "0.");
		Slpofs = 0;
		strcpy(Slpofst_str, "1.");
		Slpofx = 0;
		Slpdpt = 0;
		Slptpl[0] = '\0';
		Slpdcol = NCLX_TAN;
		Slpdps = npas;
		ncl_sprintf(Slpdpst_str,&npas,1);
		Slpdfi = 0;
	}
	Tlpoft = Slpoft;
	strcpy(Tlpofth_str, Slpofth_str);
	Tlpofs = Slpofs;
	strcpy(Tlpofst_str, Slpofst_str);
	Tlpofx = Slpofx;
	Tlpdpt = Slpdpt;
	strcpy(Tlptpl,Slptpl);
	Tlpdcol = Slpdcol;
	Slpdps = npas;
	Tlpdps = Slpdps;
	ncl_sprintf(Slpdpst_str,&npas,1);
	strcpy(Tlpdpst_str, Slpdpst_str);
	Tlpdfi = Slpdfi;
/*
.....Initialize Feed Rate settings
*/
	if (!Sform_init)
	{
		for (i=0;i<6;i++)
		{
			Sfopt[i] = 0;
			ncl_sprintf(Sfeed_str[i],&fdrt[i],1);
		}
		Sfopt[1] = Sfopt[2] = 2;
		strcpy(Sfeed_str[1], "0.0");
		strcpy(Sfeed_str[2], "0.0");
	}
	for (i=0;i<6;i++)
	{
		Tfopt[i] = Sfopt[i];

		ncl_sprintf(Sfeed_str[i],&fdrt[i],1);
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
	strcpy(Sav_valuestr4[1], Sfeed_valuestr);
	strcpy(Sav_valuestr5[0], Sfeed_valuestr);
	strcpy(Sav_valuestr5[1], Sfeed_valuestr);
	strcpy(Sav_valuestr6[0], Sfeed_valuestr);
	strcpy(Sav_valuestr6[1], Sfeed_valuestr);*/

	strcpy(Sav_valuestr1[0], Sfeed_str[0]);
	strcpy(Sav_valuestr1[1], Sfeed_str[0]);
	strcpy(Sav_valuestr2[0], Sfeed_str[1]);
	strcpy(Sav_valuestr2[1], Sfeed_str[1]);
	strcpy(Sav_valuestr3[0], Sfeed_str[2]);
	strcpy(Sav_valuestr3[1], Sfeed_str[2]);
	strcpy(Sav_valuestr4[0], Sfeed_str[3]);
	strcpy(Sav_valuestr4[1], Sfeed_str[3]);
	strcpy(Sav_valuestr5[0], Sfeed_str[4]);
	strcpy(Sav_valuestr5[1], Sfeed_str[4]);
	strcpy(Sav_valuestr6[0], Sfeed_str[5]);
	strcpy(Sav_valuestr6[1], Sfeed_str[5]);
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
	Sgcv.key = Sgspt.key = Sgept.key = 0;
	Sgcv.color = Sgspt.color = Sgept.color = -1;
	Sgcv.label[0] = Sgspt.label[0] = Sgept.label[0] = '\0';

	Sgpsf.key = Sgpsf2.key = 0;
	Sgpsf.color = Sgpsf2.color = -1;
	Sgpsf.label[0] = Sgpsf2.label[0] = '\0';
	S_init_geo(&Sgpsf,Tpsf,Tpfcol);
	S_init_geo(&Sgpsf2,Tpfcs,Tcscol);

	Sgcp.key = Sgrap.key = Sgret.key = 0;
	Sgcp.color = Sgrap.color = Sgret.color = -1;
	Sgcp.label[0] = Sgrap.label[0] = Sgret.label[0] = '\0';
	S_init_geo(&Sgcp,Tpocpln,Tpoccol);
	S_init_geo(&Sgrap,Tpoppln,Tpopcol);
	S_init_geo(&Sgret,Tporpln,Tporcol);

	Sgtpl.key = 0;
	Sgtpl.color = -1;
	Sgtpl.label[0] = '\0';
	S_init_geo(&Sgtpl,Tlptpl,Tlpdcol);

	
/*
.....Erase the motion &
.....Display the current cutter position
*/
/*
......don't erase motion now
*/
/*	nclu_erase_motion(); */
	nclu_disp_cut();
/*
.....Display forward direction
*/
	S_draw_indir();
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
	if (!Serrflg)
	{
		S_section_changed(Smode[Curve],Sred,UU_TRUE,UU_FALSE);
		ud_frm_enable_ok(UU_FALSE);
		if (*fieldno != -1)
		{
			for (i=Start;i<=Feeds;i++)
				S_section_changed(Smode[i],Sblack,UU_FALSE,UU_FALSE);
		}
/*
.....Set the mandatory fields to red
*/
		ud_dispfrm_set_attribs(0,FDCV1,UM_WHITE,UM_RED);
/*
.....Set Pick buttons to correct color
*/
		ud_dispfrm_set_attribs(0,FDST2,UM_BLACK,Tstcol);
		ud_dispfrm_set_attribs(0,FDEN2,UM_BLACK,Tencol);
		ud_dispfrm_set_attribs(0,FPSF1,UM_BLACK,Tpfcol);
		ud_dispfrm_set_attribs(0,FPCS1,UM_BLACK,Tcscol);
		ud_dispfrm_set_attribs(0,FPCL3,UM_BLACK,Tpoccol);
		ud_dispfrm_set_attribs(0,FPRP3,UM_BLACK,Tpopcol);
		ud_dispfrm_set_attribs(0,FPRT3,UM_BLACK,Tporcol);
		ud_dispfrm_set_attribs(0,FLDP3,UM_BLACK,Tlpdcol);
	}

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
.....Save the Drive Surface settings
*/

	UM_real8 frptdis, frtrdis, fclrdis;

	Sdscon = Tdscon;
	Sdscol = Tdscol;
	Sdsofd = Tdsofd;
	Sdsthfl = Tdsthfl;
	strcpy(Sdsthk_str, Tdsthk_str);
	Slapflg = Tlapflg;
	strcpy(Slapdis,Tlapdis);
	Sstart = Tstart;
/*	strcpy(Sstpt,Tstpt);*/
	Sstcol = Tstcol;
	Sdir = Tdir;
/*	strcpy(Sdirvec,Tdirvec);*/
	Send = Tend;
/*	strcpy(Senpt,Tenpt);*/
	Sencol = Tencol;
/*
.....Save the Part Surface settings
*/
	strcpy(Spsf,Tpsf);
	Spfcol = Tpfcol;
	Spftax = Tpftax;
	strcpy(Spfthk_str, Tpfthk_str);
	strcpy(Spfcs,Tpfcs);
	Scscol = Tcscol;
/*
.....Save the Entry / Exit settings
*/
	Sentry = Tentry;
	strcpy(Sendis_str, Tendis_str);
	strcpy(Senrad_str, Tenrad_str);
	strcpy(Senang_str, Tenang_str);
	strcpy(Senris1_str, Tenris1_str);
	strcpy(Senris2_str, Tenris2_str);
	Sexit = Texit;
	strcpy(Sexdis_str, Texdis_str);
	strcpy(Sexrad_str, Texrad_str);
	strcpy(Sexang_str, Texang_str);
	strcpy(Sexris1_str, Texris1_str);
	strcpy(Sexris2_str, Texris2_str);
/*
.....Save the Final Pass CUTCOM settings
*/
	Scutcom = Tcutcom;
	Scutcomdir = Tcutcomdir;
	Scutcompln = Tcutcompln;
	strcpy(Scutcom_str, Tcutcom_str);
	strcpy(Scutcompds_str, Tcutcompds_str);
	Scutcomsrt = Tcutcomsrt;
/*
.....Save Option Box settings
*/
	Sfilbox = Tfilbox;
	Smaxdpbox = Tmaxdpbox;
	strcpy(Sfilang_str, Tfilang_str);
	strcpy(Smaxdp_str, Tmaxdp_str);
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
.....Save the Loop settings
*/
	Slpoft = Tlpoft;
	strcpy(Slpofth_str, Tlpofth_str);
	Slpofs = Tlpofs;
	strcpy(Slpofst_str, Tlpofst_str);
	Slpofx = Tlpofx;
	Slpdpt = Tlpdpt;
	strcpy(Slptpl,Tlptpl);
	Slpdcol = Tlpdcol;
	Slpdps = Tlpdps;
	strcpy(Slpdpst_str, Tlpdpst_str);
	Slpdfi = Tlpdfi;
/*
.....Save the Feed Rate settings
*/
	for (i=0;i<6;i++)
	{
		Sfopt[i] = Tfopt[i];
		strcpy(Sfeed_str[i], Tfeed_str[i]);
	}
/*
.....Save geometry colors
*/
	UN_unused_geo_flag = Tgeotog;
	UN_unused_geo_color = Tgeocol;
	nclu_save_preview_modals();

	frptdis = atof (Spoppln);
	frtrdis = atof (Sporpln);
	fclrdis = atof (Spocpln);

	prfsavepar (&frptdis, &frtrdis, &fclrdis);
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
**    S_FUNCTION     :  S_select_geo(sfpt,sflst,mask,multi,prmno,color,frm,fieldno,label);
**       Routine to select geometry for the Profile form.  The associated
**       text field will be updated with the geometry label and
**       the geometry will be highlighted.
**    PARAMETERS
**       INPUT  :
**          sfpt     Pointer to selected geometry structure.
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
.....Unhighlight the previous selection
*/
	if (sfpt != UU_NULL) S_unhilite_entity(sfpt);
	if (sflst != UU_NULL) S_unhilite_entities(sflst);
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
			else ud_update_answer(frm,fieldno,(int *)label);
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
/*
........Update the text field with the entity label
*/
		else if (fieldno != -1)
		{
			sfpt = (UM_sgeo *)UU_LIST_ARRAY(sflst);
			strcpy(label,sfpt->label);
			if (UU_LIST_LENGTH(sflst) > 1) strcat(label,",...");
			if (frm == 0) ud_update_answer(fieldno,(int *)label);
			else ud_dispfrm_update_answer(frm,fieldno,(int *)label);
		}
	}
/*
.....End of routine
.....Redisplay form
*/
done:;
	ud_unlimit();
	S_draw_indir();
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
		strncpy(sfpt->label,label, NCL_MAX_LABEL);
		S_hilite_entity(sfpt,color);
	}
}

/*********************************************************************
**    I_FUNCTION     :  S_push_geo(glst,geo,init)
**       Pushes a picked geometry structure onto the geometry list.
**    PARAMETERS
**       INPUT  :
**          glst     List to push geometry on.
**          geo      Geometry structure to push onto list.
**          init     UU_TRUE = list is already initialized.
**       OUTPUT :
**          init     UU_TRUE = list is initialized.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_push_geo(glst,geo,init)
UU_LIST *glst;
UM_sgeo *geo;
UU_LOGICAL *init;
{
/*
.....Initialize the list
*/
	if (!(*init)) uu_list_init(glst,sizeof(UM_sgeo),10,10);
	uu_list_push(glst,geo);
	if (init != UU_NULL) *init = UU_TRUE;
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
	S_unhilite_entity(&Sgspt);
	S_unhilite_entity(&Sgept);
	S_unhilite_entity(&Sgpsf);
	S_unhilite_entity(&Sgpsf2);
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
.....Restore geometry color
*/
	if (sfpt->color != -1)
	{
		if (sfpt->key != 0)
		{
			e.key = sfpt->key;
			if (ncl_retrieve_data_fixed(&e) != 0) return;
			if (sfpt->color == -1) ncl_get_geo_color (e.key,&sfpt->color);
/*
.....Unhighlight entity
*/
			if (Sgeo_redraw) ncl_remove_override_geo(e.key);
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
	S_add_key(which,Sgspt.key);
	S_add_key(which,Sgept.key);
	S_add_key(which,Sgpsf.key);
	S_add_key(which,Sgpsf2.key);
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
**    I_FUNCTION     : S_draw_indir()
**			Draws the Assist vectors used to pick the forward direction.
**    PARAMETERS   
**       INPUT  :
**          none
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_draw_indir()
{
	int icol;
	UM_vector fvec,tvec;
/*
.....Draw 4 vectors to choose from
*/
	ud_delete_assist_segs();
	if (Tdir == 1)
	{
		um_vctovc(Sfwd,Svecs[0]);
		um_vctovc(Stend.vec,tvec);
		icol = NCLX_CYAN;
		if (um_vcparall(Stend.vec,Sfwd))
		{
			Svecs[0][0] = 1.;
			Svecs[0][1] = 0.;
			Svecs[0][2] = 0.;
			tvec[0] = 0.;
			tvec[1] = 0.;
			tvec[2] = 1.;
			icol = NCLX_WHITE;
		}
		um_vctmsc(Svecs[0],Scdia,fvec);
		ud_assist_vector1(Stend.pt,fvec,icol);

		um_cross(tvec,Svecs[0],Svecs[1]); um_unitvc(Svecs[1],Svecs[1]);
		um_vctmsc(Svecs[1],Scdia,fvec);
		ud_assist_vector1(Stend.pt,fvec,NCLX_WHITE);

		um_vctmsc(Svecs[0],-Scdia,Svecs[2]);
		ud_assist_vector1(Stend.pt,Svecs[2],NCLX_WHITE);

		um_cross(Svecs[0],tvec,Svecs[3]); um_unitvc(Svecs[3],Svecs[3]);
		um_vctmsc(Svecs[3],Scdia,fvec);
		ud_assist_vector1(Stend.pt,fvec,NCLX_WHITE);
	}
/*
.....Draw the forward direction
*/
	else
	{
		um_vctovc(Sfwd,Svecs[0]); um_vctmsc(Svecs[0],Scdia,fvec);
		ud_assist_vector1(Stend.pt,fvec,NCLX_CYAN);
	}
	ug_wflush();
	uw_ntflush_win();
}

/*********************************************************************
**    I_FUNCTION     : S_build_cmd(flag)
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
	int i,j,nc,nc1,nline,slen,listlen;
	UU_LOGICAL ofl;
	char buf[80],*p;
	NCL_cmdbuf cmdbuf;
	UM_sgeo *geo;
	UM_f77_str outcmd;
	NCLX_mot_fillet fillet;
	char cfill[6][NCL_MAX_LABEL_AND_SUBSCRIPT+1];

	if (flag)
		NCL_preview_mot = 0;
	else
		NCL_preview_mot = 1;
/*
.....Output subform commands
*/
	ofl = UU_FALSE;
	if (Sarcfl)
	{
		nclu_arcslp_set_fillet(&fillet,cfill);
		ofl = nclu_arcslp_command(&cmdbuf,&fillet,cfill,UU_TRUE,flag);
		ncl_set_cmdmode(UU_TRUE);
		ncl_call(&cmdbuf);
	}
/*
.....Initialize command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
/*
.....Loop through selections
*/
	geo = (UM_sgeo *)UU_LIST_ARRAY(&Sglst);
	listlen = UU_LIST_LENGTH(&Sglst);
	if (listlen == 0) 
	{
		NCL_preview_mot = 1;
		return (UU_FAILURE);
	}
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
		if (Tdscon == 1) ncl_add_token(&cmdbuf,NCL_off,NCL_comma);
		else ncl_add_token(&cmdbuf,NCL_on,NCL_comma);
		ncl_add_token(&cmdbuf,geo[i].label,NCL_comma);
		ncl_get_scalar_value(Tdsthk_str, &Tdsthk);
		if (Tdsthfl == 1 && Tdsthk != 0.)
			ncl_add_token(&cmdbuf,Tdsthk_str,NCL_comma);

		if (Tdscon != 0 || Tdsthfl)
		{
			if (Tdsofd == 1) ncl_add_token(&cmdbuf,NCL_left,NCL_comma);
			else if (Tdsofd == 2) ncl_add_token(&cmdbuf,NCL_right,NCL_comma);
		}
		nc = strlen(Tlapdis);
		ul_strip_blanks(Tlapdis,&nc);
		if (Tlapflg != 0 && nc != 0)
		{
			ncl_add_token(&cmdbuf,NCL_past,NCL_comma);
			p = strchr(Tlapdis,',');
			if (p != UU_NULL || Tlapflg == 1)
				ncl_add_token(&cmdbuf,Tlapdis,NCL_comma);
			else if (Tlapflg == 2)
			{
				sprintf(buf,"%s,0",Tlapdis);
				ncl_add_token(&cmdbuf,buf,NCL_comma);
			}
			else
			{
				sprintf(buf,"0,%s",Tlapdis);
				ncl_add_token(&cmdbuf,buf,NCL_comma);
			}
		}
/*
.....PS parameters
*/
		nc = strlen(Tpsf);
		nc1 = strlen(Tpfcs);
		ul_strip_blanks(Tpsf,&nc);
		ul_strip_blanks(Tpfcs,&nc1);
		ncl_get_scalar_value(Tpfthk_str, &Tpfthk);
		if (nc != 0 || (Tpftax != 0 && nc1 != 0) || Tpfthk != 0.)
		{
			ncl_add_token(&cmdbuf,NCL_ps,NCL_comma);
			if (nc != 0) ncl_add_token(&cmdbuf,Tpsf,NCL_comma);
			if (Tpfthk != 0.)
			{
#if 0
				ncl_sprintf(buf,&Tpfthk,1);
				ncl_add_token(&cmdbuf,buf,NCL_comma);
#endif
				ncl_add_token(&cmdbuf,Tpfthk_str,NCL_comma);
			}
			if (Tpftax != 0)
			{
				if (Tpftax == 1)
				{
					ncl_add_token(&cmdbuf,NCL_normal,NCL_comma);
					if (nc1 != 0) ncl_add_token(&cmdbuf,Tpfcs,NCL_comma);
					if (Tpftilt[0] == 1)
					{
						ncl_add_token(&cmdbuf,NCL_out,NCL_comma);
						if (Tpfdtyp[0] == 1)
							ncl_add_token(&cmdbuf,NCL_percnt,NCL_comma);
						ncl_add_token(&cmdbuf,Tpftds_str[0],NCL_comma);
						ncl_add_token(&cmdbuf,Tpftan_str[0],NCL_comma);
					}
					if (Tpftilt[1] == 1)
					{
						ncl_add_token(&cmdbuf,NCL_at,NCL_comma);
						if (Tpfdtyp[1] == 1)
							ncl_add_token(&cmdbuf,NCL_percnt,NCL_comma);
						ncl_add_token(&cmdbuf,Tpftds_str[1],NCL_comma);
						ncl_add_token(&cmdbuf,Tpftan_str[1],NCL_comma);
					}
				}
				else
				{
					ncl_add_token(&cmdbuf,NCL_atangl,NCL_comma);
					ncl_add_token(&cmdbuf,Tpfang_str,NCL_comma);
				}
/*
.....put this line after NCL_normal above.
*/		
//				if (nc1 != 0) ncl_add_token(&cmdbuf,Tpfcs,NCL_comma);
			}
		} 
/*
.....START parameters
*/
		nc = strlen(Tstpt);
		ul_strip_blanks(Tstpt,&nc);
		if ((Tstart == 1 && nc > 0) || Tstart == 2 || Tdir != 0 || Tentry != 0)
		{
			ncl_add_token(&cmdbuf,NCL_start,NCL_comma);
			if (Tstart == 1 && nc > 0)
			{
				ncl_add_token(&cmdbuf,NCL_nearpt,NCL_comma);
				ncl_add_token(&cmdbuf,Tstpt,NCL_comma);
			}
			else if (Tstart == 2)
			{			
				ncl_add_token(&cmdbuf,NCL_end,NCL_comma);
			}

			if (Tdir == 1 || Tdir == 2)
			{
				nc = strlen(Tdirvec);
				ul_strip_blanks(Tdirvec,&nc);
				if (nc != 0) ncl_add_token(&cmdbuf,Tdirvec,NCL_comma);
			}
			else if (Tdir == 3)
				ncl_add_token(&cmdbuf,NCL_clw,NCL_comma);
			else if (Tdir == 4)
				ncl_add_token(&cmdbuf,NCL_ccw,NCL_comma);
			if (Tentry == 1 || Tentry == 3)
			{
				ncl_add_token(&cmdbuf,NCL_atangl,NCL_comma);
				ncl_add_token(&cmdbuf,Tendis_str,NCL_comma);
				ncl_add_token(&cmdbuf,Tenang_str,NCL_comma);
				ncl_get_scalar_value(Tenris1_str, &Tenris1);
				if (Tenris1 != 0.)
					ncl_add_token(&cmdbuf,Tenris1_str,NCL_comma);
			}
			if (Tentry == 2 || Tentry == 3)
			{
				ncl_add_token(&cmdbuf,NCL_arc,NCL_comma);
				ncl_add_token(&cmdbuf,Tenrad_str,NCL_comma);
				ncl_get_scalar_value(Tenris2_str, &Tenris2);
				if (Tenris2 != 0.)
					ncl_add_token(&cmdbuf,Tenris2_str,NCL_comma);
			}
		} 
/*
.....CS parameters
*/
		nc = strlen(Tenpt);
		ul_strip_blanks(Tenpt,&nc);
		if (Tend == 1 && nc > 0)
		{
			ncl_add_token(&cmdbuf,NCL_cs,NCL_comma);
			if (Tend == 1 && nc > 0)
			{
				ncl_add_token(&cmdbuf,NCL_nearpt,NCL_comma);
				ncl_add_token(&cmdbuf,Tenpt,NCL_comma);
			}
		}
/*
.....OFFSET parameters
*/
		if (Tlpoft != 0)
		{
			ncl_add_token(&cmdbuf,NCL_offset,NCL_comma);
			if (Tlpoft == 1)
				ncl_add_token(&cmdbuf,Tlpofth_str,NCL_comma);
			if (Tlpoft == 2 || Tlpofs == 0)
			{
				ncl_add_token(&cmdbuf,NCL_pass,NCL_comma);
				if (Tlpoft == 2)
					ncl_add_token(&cmdbuf,Tlpofth_str,NCL_comma);
				else
					ncl_add_token(&cmdbuf,Tlpofst_str,NCL_comma);
			}
			if (Tlpofx == 1)
				ncl_add_token(&cmdbuf,NCL_up,NCL_comma);
			else if (Tlpofx == 2)
				ncl_add_token(&cmdbuf,NCL_off,NCL_comma);
			if (Tlpofs == 1)
				ncl_add_token(&cmdbuf,Tlpofst_str,NCL_comma);
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
					ncl_add_token(&cmdbuf,Tlpdpst_str,NCL_comma);
			}
			if (Tlpdps == 1)
				ncl_add_token(&cmdbuf,Tlpdpst_str,NCL_comma);
			if (Tlpdfi == 1) ncl_add_token(&cmdbuf,NCL_deep,NCL_comma);
		}
/*
.....Fillet parameters
*/
		nc = strlen(Tenpt);
		ul_strip_blanks(Tenpt,&nc);
		if (Tfilbox)
		{
			ncl_add_token(&cmdbuf,NCL_fillet,NCL_comma);
			ncl_add_token(&cmdbuf,Tfilang_str,NCL_comma);
		} 
/*
.....Max step size parameters
*/
		nc = strlen(Tenpt);
		ul_strip_blanks(Tenpt,&nc);
		if (Tmaxdpbox)
		{
			ncl_add_token(&cmdbuf,"MAXDP",NCL_comma);
			ncl_add_token(&cmdbuf,Tmaxdp_str,NCL_comma);
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
		if (Texit != 0 || Tporet != 0)
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
			if (Texit == 1 || Texit == 3)
			{
				ncl_get_scalar_value(Texris1_str, &Texris1);
				ncl_add_token(&cmdbuf,NCL_atangl,NCL_comma);
				ncl_add_token(&cmdbuf,Texdis_str,NCL_comma);
				ncl_add_token(&cmdbuf,Texang_str,NCL_comma);
				if (Texris1 != 0.)
					ncl_add_token(&cmdbuf,Texris1_str,NCL_comma);
			}
			if (Texit == 2 || Texit == 3)
			{
				ncl_get_scalar_value(Texris2_str, &Texris2);
				ncl_add_token(&cmdbuf,NCL_arc,NCL_comma);
				ncl_add_token(&cmdbuf,Texrad_str,NCL_comma);
				if (Texris2 != 0.)
					ncl_add_token(&cmdbuf,Texris2_str,NCL_comma);
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
		if (Tfopt[0] != 0 || Tfopt[1] != 2 || Tfopt[2] != 2 || Tfopt[3] != 0 ||
			Tfopt[4] != 0 || Tfopt[5] != 0)
		{
			ncl_add_token(&cmdbuf,NCL_feedrat,NCL_comma);
			for (j=0;j<6;j++)
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
.....CUTCOM paramaters
*/
		if (Tcutcom && (Tcutcomdir != NONE || Tcutcompln != NONE ||
			strlen(Tcutcom_str) > (unsigned int)0))
		{
			ncl_add_token (&cmdbuf, NCL_cutcom, NCL_comma);
			if (Tcutcomdir == LEFT)
				ncl_add_token (&cmdbuf, NCL_left, NCL_comma);
			else if (Tcutcomdir == RIGHT)
				ncl_add_token (&cmdbuf, NCL_right, NCL_comma);
			else if (Tcutcomdir == ON)
				ncl_add_token (&cmdbuf, NCL_on, NCL_comma);
			if (Tcutcompln == XYPLAN)
				ncl_add_token (&cmdbuf, NCL_xyplan, NCL_comma);
			else if (Tcutcompln == YZPLAN)
				ncl_add_token (&cmdbuf, NCL_yzplan, NCL_comma);
			else if (Tcutcompln == ZXPLAN)
				ncl_add_token (&cmdbuf, NCL_zxplan, NCL_comma);
			if (strlen(Tcutcom_str) > (unsigned int)0)
				ncl_add_token (&cmdbuf, Tcutcom_str, NCL_comma);
/*
.....Positional distance
*/
			ncl_get_scalar_value(Tcutcompds_str, &Tcutcompds);
			if (Tcutcompds != 0.0)
			{
				ncl_add_token (&cmdbuf, NCL_offset, NCL_comma);
				ncl_add_token (&cmdbuf, Tcutcompds_str, NCL_comma);
				if (Tcutcomsrt == 0)
					ncl_add_token (&cmdbuf, NCL_all, NCL_comma);
				else if (Tcutcomsrt == 1)			
					ncl_add_token (&cmdbuf, NCL_start, NCL_comma);
			}

			ncl_add_token(&cmdbuf, NCL_nomore, NCL_nocomma);
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
/*
.....End of Loop through selections
*/
	}
/*
.....Reset ARCSLP command
*/
	if (ofl)
	{
		nclu_arcslp_command(&cmdbuf,&Sfillet,Scfill,UU_FALSE,flag);
		ncl_set_cmdmode(UU_TRUE);
		ncl_call(&cmdbuf);
	}
	NCL_preview_mot = 1;
	return(UU_SUCCESS);
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
	int nc, status, number, nc1, nc2, nc3, nc4, nc5, nc6, nc7, nc8;
	char label[2*65], tmpstr[NCL_MAX_LABEL_AND_SUBSCRIPT];
	UU_REAL rval;
	UU_LOGICAL cvpk1, cvpk2, startpt1, startpt2, start3, start4, 
		part1, part2, posit1, posit2, posit3, posit4, pass11, pass12, pass01, pass02,
		fdsec1,fdsec2,fdsec3,fdsec4,fdsec5,fdsec6;
	UU_LOGICAL ifl;
/*
.....Section curve parameter
*/
/*
.....Curve parameter
*/
	nc = (int)strlen(Tcurve);
	ul_strip_blanks(Tcurve,&nc);
	if (nc>0)
		cvpk1 = UU_TRUE;
	else
		cvpk1 = UU_FALSE;
/*
.....Define button colors
*/
	if (cvpk1) 
		ud_dispfrm_set_attribs(0,FDCV1,UM_BLACK,Tdscol);
	else 
		ud_dispfrm_set_attribs(0,FDCV1,UM_WHITE,UM_RED);

	cvpk2 = UU_TRUE;
	if (Tdsthfl)
	{
		if (strlen(Tdsthk_str)>0)
		{
			if (ncl_get_scalar_value(Tdsthk_str, &Tdsthk)==-1)
				cvpk2 = UU_FALSE;
		}
		else
			cvpk2 = UU_FALSE;
		if (cvpk2)
			ud_dispfrm_set_attribs(0,FDCV5,UM_BLACK,UM_WHITE);
		else 
			ud_dispfrm_set_attribs(0,FDCV5,UM_WHITE,UM_RED);
	}

/*
.....Set section color
*/
	if ((cvpk1)&&(cvpk2))
	{
		if (Sacc[Curve]==0)
		{
			S_section_changed(Smode[Curve],Sblack,UU_FALSE,UU_FALSE);
		}
		else
		{	
			S_section_changed(Smode[Curve],Sgreen,UU_TRUE,UU_TRUE);
		}
	}
	else
	{
		S_section_changed(Smode[Curve],Sred,UU_TRUE,UU_FALSE);
	}
/*
.....Section Start/End parameter
*/
	startpt1 = UU_TRUE;
	if (Tstart==1)
	{
/*
.....start Point
*/
		nc = (int)strlen(Tstpt);
		ul_strip_blanks(Tstpt,&nc);
		if (nc>0)
			startpt1 = UU_TRUE;
		else
			startpt1 = UU_FALSE;
/*
.....Define button colors
*/
		if (startpt1) 
		{
			ud_dispfrm_set_attribs(0,FDST2,UM_BLACK,Tstcol);
			ud_dispfrm_set_attribs(0,FDST3,UM_BLACK,UM_WHITE);
		}
		else 
		{
			ud_dispfrm_set_attribs(0,FDST2,UM_WHITE,UM_RED);
			ud_dispfrm_set_attribs(0,FDST3,UM_WHITE,UM_RED);
		}
	}
	else
	{
/*
......reset point button color to normal
*/
		ud_dispfrm_set_attribs(0,FDST2,UM_BLACK,Tstcol);
		ud_dispfrm_set_attribs(0,FDST3,UM_BLACK,UM_WHITE);
	}
	startpt2 = UU_TRUE;
	if (Tend==1)
	{
/*
.....start Point
*/
		nc = (int)strlen(Tenpt);
		ul_strip_blanks(Tenpt,&nc);
		if (nc>0)
			startpt2 = UU_TRUE;
		else
			startpt2 = UU_FALSE;
/*
.....Define button colors
*/
		if (startpt2) 
		{
			ud_dispfrm_set_attribs(0,FDEN2, UM_BLACK,Tencol);
			ud_dispfrm_set_attribs(0,FDEN3,UM_BLACK,UM_WHITE);
		}
		else 
		{
			ud_dispfrm_set_attribs(0,FDEN2, UM_WHITE,UM_RED);
			ud_dispfrm_set_attribs(0,FDEN3,UM_WHITE,UM_RED);
		}
	}
	else
	{
/*
......reset point button color to normal
*/
		ud_dispfrm_set_attribs(0,FDEN2,UM_BLACK,Tencol);
		ud_dispfrm_set_attribs(0,FDEN3,UM_BLACK,UM_WHITE);
	}
	start3 = UU_TRUE;
	if ((Tdir==1)||(Tdir==2))
	{
		nc = (int)strlen(Tdirvec);
		ul_strip_blanks(Tdirvec,&nc);
		if (nc>0)
			start3 = UU_TRUE;
		else
			start3 = UU_FALSE;
		if (start3) 
			ud_dispfrm_set_attribs(0,FDDR2, UM_BLACK,UM_WHITE);
		else 
			ud_dispfrm_set_attribs(0,FDDR2, UM_WHITE,UM_RED);
	}
	else
	{
/*
......reset point button color to normal
*/
		ud_dispfrm_set_attribs(0,FDDR2,UM_BLACK,UM_WHITE);
	}
	start4 = UU_TRUE;
	if (Tlapflg!=0)
	{
//should we check the number > 0?
		if (strlen(Tlapdis)>0)
		{
			if (ncl_get_scalar_value(Tlapdis, &rval)==-1)
				start4 = UU_FALSE;
		}
		else
			start4 = UU_FALSE;
		if (start4)
			ud_dispfrm_set_attribs(0,FDDR5,UM_BLACK,UM_WHITE);
		else 
			ud_dispfrm_set_attribs(0,FDDR5,UM_WHITE,UM_RED);
	}
	else
	{
/*
......reset point button color to normal
*/
		ud_dispfrm_set_attribs(0,FDDR5,UM_BLACK,UM_WHITE);
	}
/*
.....Set section color
*/
	if ((startpt1)&&(startpt2)&&(start3)&&(start4))
	{
		if (Sacc[Start]==0)
		{
			S_section_changed(Smode[Start],Sblack,UU_FALSE,UU_FALSE);
		}
		else
		{	
			S_section_changed(Smode[Start],Sgreen,UU_TRUE,UU_TRUE);
		}
	}
	else
	{
		S_section_changed(Smode[Start],Sred,UU_TRUE,UU_FALSE);
	}
/*
.....Section Part Surface parameter
.....need check more?
*/
	part1 = UU_TRUE; 
	part2 = UU_TRUE; 
	nc = (int)strlen(Tpsf);
	ul_strip_blanks(Tpsf,&nc);
	if (nc>0)
	{
		if (Tpftax==1)
		{
			if (Tpftilt[0])
			{
				nc = (int)strlen(Tpftds_str[0]);
				ul_strip_blanks(Tpftds_str[0],&nc);
				if (nc>0)
				{
					ncl_get_scalar_value(Tpftds_str[0], &rval);
					if (rval<0)
						part1 = UU_FALSE;
				}
				else
					part1 = UU_FALSE;
				if (part1) 
				{
					ud_dispfrm_set_attribs(0,FPTL6,UM_BLACK,UM_WHITE);
				}
				else 
				{
					ud_dispfrm_set_attribs(0,FPTL6, UM_WHITE,UM_RED);
				}
			}
			if (Tpftilt[1])
			{
				nc = (int)strlen(Tpftds_str[1]);
				ul_strip_blanks(Tpftds_str[1],&nc);
				if (nc>0)
				{
					ncl_get_scalar_value(Tpftds_str[1], &rval);
					if (rval<0)
						part2 = UU_FALSE;
				}
				else
					part2 = UU_FALSE;
				if (part2) 
				{
					ud_dispfrm_set_attribs(0,FPTL8,UM_BLACK,UM_WHITE);
				}
				else 
				{
					ud_dispfrm_set_attribs(0,FPTL8, UM_WHITE,UM_RED);
				}
			}
		}
	}
	if (part1&&part2)
	{
		if (Sacc[Part]==0)
		{
			S_section_changed(Smode[Part],Sblack,UU_FALSE,UU_FALSE);
		}
		else
		{	
			S_section_changed(Smode[Part],Sgreen,UU_TRUE,UU_TRUE);
		}
	}
	else
	{
		S_section_changed(Smode[Part],Sred,UU_TRUE,UU_FALSE);
	}
/*
.....Section Entry parameter
.....need check more?
*/
		if (Sacc[Entry]==0)
		{
			S_section_changed(Smode[Entry],Sblack,UU_FALSE,UU_FALSE);
		}
		else
		{	
			S_section_changed(Smode[Entry],Sgreen,UU_TRUE,UU_TRUE);
		}
/*
.....Section Positioning parameter
*/
	posit1 = UU_TRUE;
	if (Tpoclr==1)
	{
		nc = (int)strlen(Tpocpln);
		ul_strip_blanks(Tpocpln,&nc);
		if (nc>0)
			posit1 = UU_TRUE;
		else
			posit1 = UU_FALSE;
/*
.....Define button colors
*/
		if (posit1) 
		{
			ud_dispfrm_set_attribs(0,FPCL3, UM_BLACK,Tpoccol);
			ud_dispfrm_set_attribs(0,FPCL2,UM_BLACK,UM_WHITE);
		}
		else 
		{
			ud_dispfrm_set_attribs(0,FPCL3, UM_WHITE,UM_RED);
			ud_dispfrm_set_attribs(0,FPCL2,UM_BLACK,UM_RED);
		}
	}
	else
	{
/*
......reset point button color to normal
*/
		ud_dispfrm_set_attribs(0,FPCL3,UM_BLACK,Tpoccol);
/*
.....check the number
*/
		if ((Tpoclr==2)||(Tpoclr==3))
		{
			nc = (int)strlen(Tpocpln);
			ul_strip_blanks(Tpocpln,&nc);
			if (nc>0)
			{
				if (ncl_get_scalar_value(Tpocpln, &rval)==-1)
					posit1 = UU_FALSE;
			}
			else
				posit1 = UU_FALSE;
		}
		else
			posit1 = UU_TRUE;
		if (posit1)
			ud_dispfrm_set_attribs(0,FPCL2,UM_BLACK,UM_WHITE);
		else
			ud_dispfrm_set_attribs(0,FPCL2,UM_BLACK,UM_RED);
	}
	posit2 = UU_TRUE;
	if (Tporap==1)
	{
		nc = (int)strlen(Tpoppln);
		ul_strip_blanks(Tpoppln,&nc);
		if (nc>0)
			posit2 = UU_TRUE;
		else
			posit2 = UU_FALSE;
/*
.....Define button colors
*/
		if (posit2) 
		{
			ud_dispfrm_set_attribs(0,FPRP3, UM_BLACK,Tpopcol);
			ud_dispfrm_set_attribs(0,FPRP2, UM_BLACK,UM_WHITE);
		}
		else 
		{
			ud_dispfrm_set_attribs(0,FPRP3, UM_WHITE,UM_RED);
			ud_dispfrm_set_attribs(0,FPRP2, UM_BLACK,UM_RED);
		}
	}
	else
	{
/*
......reset point button color to normal
*/
		ud_dispfrm_set_attribs(0,FPRP3,UM_BLACK,Tpopcol);
/*
.....check the number
*/
		if ((Tporap==2)||(Tporap==3))
		{
			nc = (int)strlen(Tpoppln);
			ul_strip_blanks(Tpoppln,&nc);
			if (nc>0)
			{
				if (ncl_get_scalar_value(Tpoppln, &rval)==-1)
					posit2 = UU_FALSE;
			}
			else
				posit2 = UU_FALSE;
		}
		else
			posit2 = UU_TRUE;
		if (posit2)
			ud_dispfrm_set_attribs(0,FPRP2, UM_BLACK,UM_WHITE);
		else
			ud_dispfrm_set_attribs(0,FPRP2,UM_BLACK,UM_RED);
	}
	posit3 = UU_TRUE;
	if (Tporet==2)
	{
		nc = (int)strlen(Tporpln);
		ul_strip_blanks(Tporpln,&nc);
		if (nc>0)
			posit3 = UU_TRUE;
		else
			posit3 = UU_FALSE;
/*
.....Define button colors
*/
		if (posit3)
		{
			ud_dispfrm_set_attribs(0,FPRT3, UM_BLACK,Tporcol);
			ud_dispfrm_set_attribs(0,FPRT2, UM_BLACK,UM_WHITE);
		}
		else 
		{
			ud_dispfrm_set_attribs(0,FPRT3, UM_WHITE,UM_RED);
			ud_dispfrm_set_attribs(0,FPRT2, UM_BLACK,UM_RED);
		}
	}
	else
	{
/*
......reset point button color to normal
*/
		ud_dispfrm_set_attribs(0,FPRT3,UM_BLACK,Tporcol);
/*
.....check the number
*/
		if ((Tporet==4)||(Tporet==3))
		{
			nc = (int)strlen(Tporpln);
			ul_strip_blanks(Tporpln,&nc);
			if (nc>0)
			{
				if (ncl_get_scalar_value(Tporpln, &rval)==-1)
					posit3 = UU_FALSE;
			}
			else
				posit3 = UU_FALSE;
		}
		else
			posit3 = UU_TRUE;
		if (posit3)
			ud_dispfrm_set_attribs(0,FPRT2, UM_BLACK,UM_WHITE);
		else
			ud_dispfrm_set_attribs(0,FPRT2,UM_BLACK,UM_RED);
	}
//need check Tportl_str is empty or not?
/*
.....Set section color
*/
	if ((posit1)&&(posit2)&&(posit3))
	{
		if (Sacc[Posit]==0)
		{
			S_section_changed(Smode[Posit],Sblack,UU_FALSE,UU_FALSE);
		}
		else
		{	
			S_section_changed(Smode[Posit],Sgreen,UU_TRUE,UU_TRUE);
		}
	}
	else
	{
		S_section_changed(Smode[Posit],Sred,UU_TRUE,UU_FALSE);
	}
/*
.....Section Pass parameter
*/
	pass01 = UU_TRUE;
	pass02 = UU_TRUE;
	if (Tdscon == 1)
	{
		if ((Tlpoft==1)||(Tlpoft==2))
		{
			nc = (int)strlen(Tlpofth_str);
			ul_strip_blanks(Tlpofth_str,&nc);
			if (nc>0)
			{
				if (ncl_get_scalar_value(Tlpofth_str, &rval)==-1)
					pass01 = UU_FALSE;
				if (Tlpoft==1)
				{
					if (rval<0)
						pass01 = UU_FALSE;
				}
				else if (Tlpoft==2)
				{
					if (rval<1)
						pass01 = UU_FALSE;
				}
			}
			else
				pass01 = UU_FALSE;
		}
		if (pass01)
		{
			ud_dispfrm_set_attribs(0,FLOF2,UM_BLACK,UM_WHITE);
		}
		else 
		{
			ud_dispfrm_set_attribs(0,FLOF2,UM_WHITE,UM_RED);
		}
		if (Tlpoft!=0)
		{
			nc = (int)strlen(Tlpofst_str);
			ul_strip_blanks(Tlpofst_str,&nc);
			if (nc>0)
			{
				if (ncl_get_scalar_value(Tlpofst_str, &rval)==-1)
					pass02 = UU_FALSE;
				if (Tlpofs==0)
				{
					if (rval<1)
						pass02 = UU_FALSE;
				}
				else if ((Tlpoft==0)&&(Tlpoft==1))
				{
					if (rval<=0)
						pass02 = UU_FALSE;
				}
			}
			else
				pass02 = UU_FALSE;
		}
		if (pass02)
		{
			ud_dispfrm_set_attribs(0,FLOS1,UM_BLACK,UM_WHITE);
		}
		else 
		{
			ud_dispfrm_set_attribs(0,FLOS1,UM_WHITE,UM_RED);
		}
	}
	pass11 = UU_TRUE;
	pass12 = UU_TRUE;
	if (Tlpdpt==1)
	{
		nc = (int)strlen(Tlptpl);
		ul_strip_blanks(Tlptpl,&nc);
		if (nc>0)
			pass11 = UU_TRUE;
		else
			pass11 = UU_FALSE;
/*
.....Define button colors
*/
		if (pass11) 
			ud_dispfrm_set_attribs(0,FLDP3, UM_BLACK,Tlpdcol);
		else 
			ud_dispfrm_set_attribs(0,FLDP3, UM_WHITE,UM_RED);
	}
	else
	{
/*
......reset point button color to normal
*/
		ud_dispfrm_set_attribs(0,FLDP3,UM_BLACK,Tlpdcol);
		if ((Tlpdpt==3)||(Tlpdpt==2))
		{
			nc = (int)strlen(Tlptpl);
			ul_strip_blanks(Tlptpl,&nc);
			if (nc>0)
			{
				if (ncl_get_scalar_value(Tlptpl, &rval)==-1)
					pass11 = UU_FALSE;
				if (Tlpoft==3)
				{
					if (rval<1)
						pass11 = UU_FALSE;
				}
			}
			else
				pass11 = UU_FALSE;
		}
		if (pass11)
		{
			ud_dispfrm_set_attribs(0,FLDP2,UM_BLACK,UM_WHITE);
		}
		else 
		{
			ud_dispfrm_set_attribs(0,FLDP2,UM_WHITE,UM_RED);
		}
	}
	if (Tlpdpt!=0)
	{
		nc = (int)strlen(Tlpdpst_str);
		ul_strip_blanks(Tlpdpst_str,&nc);
		if (nc>0)
		{
			if (ncl_get_scalar_value(Tlpdpst_str, &rval)==-1)
				pass12 = UU_FALSE;
			if (Tlpdps==0)
			{
				if (rval<1)
					pass12 = UU_FALSE;
			}
		}
		else
			pass12 = UU_FALSE;
		if (pass12)
		{
			ud_dispfrm_set_attribs(0,FLDS2,UM_BLACK,UM_WHITE);
		}
		else 
		{
			ud_dispfrm_set_attribs(0,FLDS2,UM_WHITE,UM_RED);
		}
	}
/*
.....Set section color
*/
	if (pass11&&pass12&&pass01&&pass02)
	{
		if (Sacc[Passes]==0)
		{
			S_section_changed(Smode[Passes],Sblack,UU_FALSE,UU_FALSE);
		}
		else
		{	
			S_section_changed(Smode[Passes],Sgreen,UU_TRUE,UU_TRUE);
		}
	}
	else
	{
		S_section_changed(Smode[Passes],Sred,UU_TRUE,UU_FALSE);
	}
/*
.....Section Option parameter
.....need check more?
*/
		if (Sacc[Options]==0)
		{
			S_section_changed(Smode[Options],Sblack,UU_FALSE,UU_FALSE);
		}
		else
		{	
			S_section_changed(Smode[Options],Sgreen,UU_TRUE,UU_TRUE);
		}
/*
.....Section Feed Rates parameter
*/
	fdsec1 = fdsec2 = fdsec3 = fdsec4 = fdsec5 = fdsec6 = UU_TRUE; 
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
	if ((Tfopt[4]==0)||(Tfopt[4]==2))
	{
		ud_dispfrm_set_attribs(0,FFED5,UM_BLACK, UM_WHITE);
	}
	else
	{
		nc = (int)strlen(Tfeed_str[4]);
		ul_strip_blanks(Tfeed_str[4], &nc);
		if (nc<=0)
		{
			fdsec5 = UU_FALSE;
			ud_dispfrm_set_attribs(0,FFED5,UM_WHITE,UM_RED);
		}
		else
		{
			rval = atof(Tfeed_str[4]);
			if (rval<=0)
				fdsec5 =  UU_FALSE;
			if (fdsec5) 
			{
				ud_dispfrm_set_attribs(0,FFED5,UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(0,FFED5,UM_WHITE,UM_RED);
			}
		}
	}
	if ((Tfopt[5]==0)||(Tfopt[5]==2))
	{
		ud_dispfrm_set_attribs(0,FFED6,UM_BLACK, UM_WHITE);
	}
	else
	{
		nc = (int)strlen(Tfeed_str[5]);
		ul_strip_blanks(Tfeed_str[5], &nc);
		if (nc<=0)
		{
			fdsec6 = UU_FALSE;
			ud_dispfrm_set_attribs(0,FFED6,UM_WHITE,UM_RED);
		}
		else
		{
			rval = atof(Tfeed_str[5]);
			if (rval<=0)
				fdsec6 =  UU_FALSE;
			if (fdsec6) 
			{
				ud_dispfrm_set_attribs(0,FFED6,UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(0,FFED6,UM_WHITE,UM_RED);
			}
		}
	}
	if (fdsec1&&fdsec2&&fdsec3&&fdsec4&&fdsec5&&fdsec6)
	{
		if (Sacc[Feeds]==0)
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
.....Section Color parameter
.....need check more?
*/
	if (Sacc[Colors]==0)
	{
		S_section_changed(Smode[Colors],Sblack,UU_FALSE,UU_FALSE);
	}
	else
	{	
		S_section_changed(Smode[Colors],Sgreen,UU_TRUE,UU_TRUE);
	}
/*
.....Set Action Buttons
*/
	ifl = cvpk1 && cvpk2 && startpt1 && startpt2 && start3 && start4 
		&& posit1 && posit2 && posit3 && pass01&& pass02&& pass11&& pass12;
	ud_frm_enable_ok(ifl);
	ud_set_traverse_mask(FAPV,ifl);
	ud_set_traverse_mask(FAPY,ifl);
	ifl = Sacc[Curve] + Sacc[Start] + Sacc[Part] + Sacc[Entry] +
			Sacc[Posit] + Sacc[Passes] + Sacc[Options] + Sacc[Feeds] + Sacc[Colors];
	ud_set_traverse_mask(FARS,ifl);
/*
.....if geom entered
......
	ul_to_upper(Tsfnam);
	strcpy(label, Tsfnam);
	nc1 = (int)strlen(label);
	ul_strip_blanks(label,&nc1);
	ul_to_upper(Tcvnam);
	strcpy(label, Tcvnam);
	nc2 = (int)strlen(label);
	ul_strip_blanks(label,&nc2);
	if (Traptotyp>1)
	{
		ul_to_upper(Traptoval);
		strcpy(label, Traptoval);
		nc3 = (int)strlen(label);
		ul_strip_blanks(label,&nc3);
	}
	else
		nc3 = 0;
	if (Tretrcttyp>1)
	{
		ul_to_upper(Tretrctval);
		strcpy(label, Tretrctval);
		nc4 = (int)strlen(label);
		ul_strip_blanks(label,&nc4);
	}
	else
		nc4 = 0;
	if (Tstarttyp==2)
	{
		ul_to_upper(Tstartstr);
		strcpy(label, Tstartstr);
		nc5 = (int)strlen(label);
		ul_strip_blanks(label,&nc5);
	}
	else
		nc5 = 0;
	if (Traptotyp1>0)
	{
		ul_to_upper(Traptoval1);
		strcpy(label, Traptoval1);
		nc6 = (int)strlen(label);
		ul_strip_blanks(label,&nc6);
	}
	else
		nc6 = 0;
	if (Tretrcttyp1>0)
	{
		ul_to_upper(Tretrctval1);
		strcpy(label, Tretrctval1);
		nc7 = (int)strlen(label);
		ul_strip_blanks(label,&nc7);
	}
	else
		nc7 = 0;
	nc8 = UU_LIST_LENGTH(&Scslst);
	if ((nc1>0)||(nc2>0)||(nc3>0)||(nc4>0)||(nc5>0)
		||(nc6>0)||(nc7>0)||(nc8>0))
	{
		ud_set_traverse_mask(FAGE,1);
	}
	else
		ud_set_traverse_mask(FAGE,0);
	if (nc8>0)
		ud_set_traverse_mask(BOUNDRG9, UU_TRUE);
	else
		ud_set_traverse_mask(BOUNDRG9, UU_FALSE);
*/
	return 0;
}