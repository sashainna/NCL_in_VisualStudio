/********************************************************************* 
**    NAME         :  nurmill.c
**       CONTAINS:
**		             nclu_rmill()
**
**    COPYRIGHT 2010 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nurmill.c , 25.4
**    DATE AND TIME OF LAST MODIFICATION
**       10/27/16 , 13:36:02
*********************************************************************/
#include <string.h>
#include "class.h"
#include "dselmask.h"
#include "nclmplay.h"
#include "mdrel.h"
#include "mgeom.h"
#include "mdpick.h"
#include "mxxx.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclx.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "uhep.h"
#include "nclvx.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"

#define MILLRG1		0
#define MILLRG2		1
/*
#define MILLRG3		2
#define MILLRG4		3
#define MILLRG5		4
#define MILLRG6		5
#define MILLRG7		6
#define MILLRG8		7
#define MILLRG9		8
#define MILLRG10	9
#define MILLRG11	10
#define MILLRG12	11
#define MILLRG13	12
#define MILLRG14	13
*/
#define MILLRG3		8
#define MILLRG4		9
#define MILLRG5		10
#define MILLRG6		11
#define MILLRG7		12
#define MILLRG8		13
#define MILLRG9		2
#define MILLRG10	3
#define MILLRG11	4
#define MILLRG12	5
#define MILLRG13	6
#define MILLRG14	7
/*
......add near point
*/
#define MILLRG15	14
#define MILLRG16	15

#define OPTFMTYP	16 
#define OPTFMPAS	17
#define OPTFMST1	18
#define OPTFMST2	19

#define ENEXTRG1	20
#define ENEXTRG2	21
#define ENEXTRG3	22
#define ENEXTRG4	23
#define ENEXTRG5	24
#define ENEXTRG6	25
#define ENEXTRG7	26

#define FEEDRATEDISP	27
#define FEEDRATECHC1	28
#define FEEDRATERG1		29
#define FEEDRATECHC2	30
#define FEEDRATERG2		31
#define FEEDRATECHC3	32
#define FEEDRATERG3		33

#define THICKSRG1	34
#define THICKSRG2	35
#define THICKSRG3	36
#define THICKSRG4	37
#define THICKSRG5	38
#define THICKSRG6	39
#define THICKSRG7	40
#define THICKSRG8	41

#define SFSCOLORS	42
#define D1COLORS	45
#define D2COLORS	46
#define C1COLORS	43
#define C2COLORS	44
#define NPTCOLORS	47
#define CPCOLORS	48
#define RPCOLORS	49
#define UNUCOLCHC	50
#define UNUSECOL	51
#define FAPV	52
#define FAPY	53
#define FARS	54
#define FAPB	55
#define FAVE	56
#define FAGE	57
#define FAVW	58
/*
.....add video button
*/
#define FVIDEO    59

#define HIDE_KEY 0
#define CONTOUR_KEY 1

typedef struct
{
	UM_int2 mot_typ, fpas, stp_mod, rghthk, fin, fret,fclr;
	UM_real4 feedrate, pos_fedr, ramp_fedr, ret_val;
	UM_real8 clr_dist, plun_dist, stp_dist, rthk[8];
	char clr_label[65], ret_label[65];
} NCL_rmill_value;

static char Sav_valuestr1[80], Sav_valuestr2[80], Sav_valuestr3[80];
static NCL_rmill_value S_rmill_vals;
static S_rmill_set = 0;

static int S_form_cont = 0;
static char Sfeed_valuestr[80];

extern int NCL_preview_mot;
/*
.....Global variables
*/
extern int NAUTIPV;
extern UD_METHOD UD_initfrm_intry;

static int SfrmPlay=0;
static int Serrflg;
static UU_LOGICAL Spreviewflg,Schgmade;

static UU_LIST Skey_list;
static int Snkeys;
static UU_LOGICAL Skey_init;
/*
.....Section definitions
*/
//enum {Mill, Entry, Options, Thicks, Feeds, Colors};
enum {Mill, MotionT, Entry, Feeds, Thicks, Colors};
static char *Smode[]={"RMill", "Motion Type", "Entry / Exit", "Feed Rates", "Thicks", "Colors"};
static int Sred[3]={180,0,0}, Syellow[3]={180,148,0}, Sgreen[3]={0,180,0};
static int Sblack[3]={0,0,0};
static int *Ssecol[]={Sblack,Sblack,Sblack,Sblack,Sblack,Sblack};
static int Sacc[6];
/*
.....Main variables
*/
static UU_LOGICAL Sform_init = UU_FALSE;
static UN_motseg *Smptr=0,Smotatt;
static UN_mot_vpbox_struc Smvpbox[UV_NVPORTS];
/*
.....Geometry variables
*/
static UM_sgeo Sgsrf,Sgds1,Sgds2,Sgcs1,Sgcs2,Sgclr,Sgret, Sgpt;
static UU_LOGICAL Sgeo_apply = UU_FALSE;

/*
.....Geometry tab variables 'S' final save value; 'T' temp form value
*/
static char Ssf[80],Tsf[80];
static char Sd1[80],Td1[80],Sd2[80],Td2[80];
static char Sc1[80],Tc1[80],Sc2[80],Tc2[80];
static char Tnpt[80];
static int Ssfcol,Tsfcol,Sd1con,Td1con,Sd2con,Td2con;
static int Sc1con,Tc1con, Sc2con,Tc2con;
static char Snpt[80];
/*
.....Entry / Exit tab variables
*/
static int Sclrtyp,Tclrtyp, Srettyp,Trettyp;
static char Sclr[80],Tclr[80],Srap[80],Trap[80],Sret[80],Tret[80];
static UU_LOGICAL Sscrub_rapto = UU_FALSE;
/*
.....Motion Type tab variables
*/
static int Smtyp,Tmtyp,Smpas,Tmpas,Smstep,Tmstep;
static char Smdis[80],Tmdis[80];
/*
.....Thick tab variables
*/
static char Srthk[4][80],Trthk[4][80]; /* roughing */
static char Sfthk[4][80],Tfthk[4][80]; /* finishing */
/*
.....Feed Rate tab variables
*/
static int Sfopt[3],Tfopt[3]; 
static char Sfeed[3][80],Tfeed[3][80]; /*three edit value */
/*
.....Color tab variables 'S' final save value; 'T' temp form value
*/
static int Sd1col,Td1col,Sd2col,Td2col,Sc1col, Sptcol, Tc1col,Sc2col,Tc2col, Tptcol;
static int Sretcol,Tretcol,Sclrcol,Tclrcol;
static int Tgeotog,Tgeocol;
static UU_LOGICAL Sgeo_redraw;
/*
.....Main form callback routines
*/
static UD_FSTAT OnMillSel(),OnMillTog(),OnMillTxt();
/*
.....options tab
*/
static UD_FSTAT OnOptTog(),OnOptTxt();
/*
.....Entry/exit tab
*/
static UD_FSTAT OnEnSel(),OnEnTog(),OnEnTxt();
/*
.....All tab
*/
static UD_FSTAT OnAction(),OnVideo();
/*
.....Thick tab routines
*/
static UD_FSTAT OnThkTxt();
/*
.....Feed Rate tab routines
*/
static UD_FSTAT OnFdTog(),OnFeedTxt();
/*
......colors tab
*/
static UD_FSTAT OnColorPicked();
/*
.....Local routines
*/
static void S_init_form(),S_save_form(),S_hilite_entity(),S_unhilite_entity();
static void S_unhilite_all();
static void S_init_traverse(),S_form_invis(),S_form_vis(),S_init_geo();
static void S_deselect_all();
static int S_select_geo(),S_build_command(),S_valid_command();

static int Serrflg;
static UU_LOGICAL Spreviewflg,Schgmade;

static int *Sanswers[] = {
/*
.....RMill
*/
		UU_NULL,(int *)&Tsf,
		&Tc1con,UU_NULL,(int *)&Tc1,
		&Tc2con,UU_NULL,(int *)&Tc2,
		&Td1con,UU_NULL,(int *)&Td1,
		&Td2con,UU_NULL,(int *)&Td2,
		UU_NULL,(int *)&Tnpt,
/*
.....Motion Type
*/
		&Tmtyp,&Tmpas, &Tmstep,(int *)&Tmdis,
/*
.....Entry/Exit
*/
		&Tclrtyp, (int *)&Tclr, UU_NULL, 
		(int *)&Trap,
		&Trettyp, (int *)&Tret, UU_NULL,
/*
.....Feed Rates
*/
		&Sfeed_valuestr, &Tfopt[0],(int *)&Tfeed[0],&Tfopt[1],(int *)&Tfeed[1],
		&Tfopt[2],(int *)&Tfeed[2],
/*
.....Thick
*/
		(int *)&Trthk[0],(int *)&Trthk[1],(int *)&Trthk[2],
		(int *)&Trthk[3], (int *)&Tfthk[0],(int *)&Tfthk[1],
		(int *)&Tfthk[2], (int *)&Tfthk[3],
/*
.....Colors
*/
		&Tsfcol,&Tc1col,&Tc2col,&Td1col, &Td2col, &Tptcol, &Tclrcol, &Tretcol,
		&Tgeotog,&Tgeocol,
		UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL};

static UD_FSTAT S_enter_form();
static void S_section_changed();
static void S_create_key_list(),S_add_key();

/*********************************************************************
**    E_FUNCTION     : nclu_rmill()
**       Controlling routine for the Regional Milling form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_rmill()
{
	int status,flag;
	UU_LOGICAL cmdreject;
	UD_METHOD save_entry;
/*
.....Set up form fields
*/
	static char traverse[] = {
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
	static char display[] = {
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
	static char called[] = {
		6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
		6, 6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6,6,6,6,6,6,6};

	static UD_METHOD methods[] = {
		OnMillSel, OnMillTxt,
		OnMillTog,OnMillSel,OnMillTxt,
		OnMillTog,OnMillSel,OnMillTxt,
		OnMillTog,OnMillSel,OnMillTxt,
		OnMillTog,OnMillSel,OnMillTxt,
		OnMillSel,OnMillTxt,

		OnOptTog,OnOptTog, OnOptTog,OnOptTxt,

		OnEnTog,OnEnTxt,OnEnSel,OnEnTxt,
		OnEnTog,OnEnTxt,OnEnSel,

		UU_NULL, OnFdTog,OnFeedTxt,OnFdTog,OnFeedTxt,OnFdTog,OnFeedTxt,
		
		OnThkTxt,OnThkTxt,OnThkTxt,OnThkTxt,
		OnThkTxt,OnThkTxt,OnThkTxt,OnThkTxt,
		
		OnColorPicked, OnColorPicked, OnColorPicked, OnColorPicked,
		OnColorPicked, OnColorPicked, OnColorPicked, OnColorPicked, OnColorPicked, OnColorPicked,
		OnAction,OnAction,OnAction,
		OnAction,OnAction,OnAction,OnAction,OnVideo};
/*
.....Initialize form answers
*/
	S_init_form();
	Serrflg = 0;
	Spreviewflg = UU_FALSE;
	Schgmade = UU_FALSE;
	save_entry = UD_initfrm_intry;
	SfrmPlay = 0;
	Skey_init = UU_FALSE;
	Snkeys = 0;
	Sacc[Mill] = Sacc[Entry] = Sacc[MotionT] = Sacc[Thicks] = Sacc[Feeds] = Sacc[Colors] = 0;
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0)	goto done;
	S_init_traverse(display,traverse);
/*
.....Initialize form fields
*/
repeat:;
	NCL_preview_mot = 1;
	UD_initfrm_intry = S_enter_form;
/*
.....Get the Form input
*/
	status = ud_form1("nrmill.frm",Sanswers,Sanswers,methods,called,display,traverse);
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
.....Validate form answers
*/
	if (!S_valid_command(UU_TRUE)) 
		goto repeat;
/*
.....Save the form settings
*/
	S_save_form();
/*
.....Output the command
*/
	S_build_command(UU_TRUE);
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
	NCL_preview_mot = 0;
	UD_UNMARK(cmdreject); 	
	return;
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
	int nc, status, nc1, nc2, nc3, nc4, nc5, nc6, nc7;
	char label[2*65];
	UU_REAL rval;
	UU_LOGICAL sfpk1,sfpk2,sfpk3,sfpk4,sfpk5,sfpk6,sfpk7, 
				fdsec1, fdsec2, fdsec3;
	UU_LOGICAL valpk1, valpk2, ifl;
/*
.....Surface parameter
*/
	nc = strlen(Tsf);
	ul_strip_blanks(Tsf,&nc);
	if (nc>0)
		sfpk1 = UU_TRUE;
	else
		sfpk1 = UU_FALSE;
/*
.....Drive surface parameter
*/
	nc = strlen(Td1);
	ul_strip_blanks(Td1,&nc);
	if (nc>0)
		sfpk2 = UU_TRUE;
	else
		sfpk2 = UU_FALSE;
	nc = strlen(Td2);
	ul_strip_blanks(Td2,&nc);
	if (nc>0)
		sfpk3 = UU_TRUE;
	else
		sfpk3 = UU_FALSE;
/*
.....Check surface parameters
*/
	nc = strlen(Tc1);
	ul_strip_blanks(Tc1,&nc);
	if (nc>0)
		sfpk4 = UU_TRUE;
	else
		sfpk4 = UU_FALSE;
	nc = strlen(Tc2);
	ul_strip_blanks(Tc2,&nc);
	if (nc>0)
		sfpk5 = UU_TRUE;
	else
		sfpk5 = UU_FALSE;
/*
.....Clearance plane & rapto distance
*/
	sfpk6 = UU_TRUE;
	if (Tmtyp == 0 || Tmtyp == 1)
	{
		if (Tmtyp == 0)
		{
			nc = strlen(Tclr);
			ul_strip_blanks(Tclr,&nc);
			if (nc<=0)
				sfpk6 = UU_FALSE;
		}
		nc = strlen(Trap);
		ul_strip_blanks(Trap,&nc);
		status = UU_SUCCESS;
		valpk1 = UU_TRUE;
		if (nc > 0 && Tmtyp == 1)
		{
			status = ncl_get_scalar_value(Trap,&rval);
			if (!(status != -1 && rval > 0.))
			{
				valpk1 = UU_FALSE;
				ud_dispfrm_set_attribs(0, ENEXTRG4, UM_WHITE, UM_RED);
			}
			else
			{
				ud_dispfrm_set_attribs(0, ENEXTRG4, UM_BLACK, UM_WHITE);
			}
		}
		else
		{
			ud_dispfrm_set_attribs(0, ENEXTRG4, UM_BLACK, UM_WHITE);
		}
	}
	sfpk7 = UU_TRUE;
	if (Trettyp != 0)
	{
		nc = strlen(Tret);
		ul_strip_blanks(Tret,&nc);
		if (nc == 0)
		{
			sfpk7 = UU_FALSE;
		}
	}
/*
.....Step over distance
*/
	ncl_get_scalar_value(Tmdis,&rval);
	if (rval > 0.)
	{
		valpk2 = UU_TRUE;
		ud_dispfrm_set_attribs(0, OPTFMST2, UM_BLACK, UM_WHITE);
	}
	else
	{
		valpk2 = UU_FALSE;
		ud_dispfrm_set_attribs(0, OPTFMST2, UM_WHITE, UM_RED);
	}
/*
.....Define button colors
*/
	if (sfpk1) 
		ud_dispfrm_set_attribs(0,MILLRG1,UM_BLACK,Tsfcol);
	else 
		ud_dispfrm_set_attribs(0,MILLRG1,UM_WHITE,UM_RED);

	if (sfpk2) 
		ud_dispfrm_set_attribs(0,MILLRG4,UM_BLACK,Td1col);
	else 
		ud_dispfrm_set_attribs(0,MILLRG4,UM_WHITE,UM_RED);

	if (sfpk3) 
		ud_dispfrm_set_attribs(0,MILLRG7,UM_BLACK,Td2col);
	else 
		ud_dispfrm_set_attribs(0,MILLRG7,UM_WHITE,UM_RED);

	if (sfpk4) 
		ud_dispfrm_set_attribs(0,MILLRG10,UM_BLACK,Tc1col);
	else 
		ud_dispfrm_set_attribs(0,MILLRG10,UM_WHITE,UM_RED);

	if (sfpk5) 
		ud_dispfrm_set_attribs(0,MILLRG13,UM_BLACK,Tc2col);
	else 
		ud_dispfrm_set_attribs(0,MILLRG13,UM_WHITE,UM_RED);

	ud_dispfrm_set_attribs(0,MILLRG15,UM_BLACK,Tptcol);

	if (sfpk6) 
		ud_dispfrm_set_attribs(0,ENEXTRG3,UM_BLACK,Tclrcol);
	else 
		ud_dispfrm_set_attribs(0,ENEXTRG3,UM_WHITE,UM_RED);
	if (sfpk7) 
		ud_dispfrm_set_attribs(0,ENEXTRG7,UM_BLACK,Tretcol);
	else 
		ud_dispfrm_set_attribs(0,ENEXTRG7,UM_WHITE,UM_RED);		
/*
.....Feed rate
*/
	fdsec1 = fdsec2 = fdsec3 = UU_TRUE;
	if (Tfopt[0]==1)
	{
		nc = (int)strlen(Tfeed[0]);
		ul_strip_blanks(Tfeed[0], &nc);
		if (nc<=0)
		{
			fdsec1 = UU_FALSE;
			ud_dispfrm_set_attribs(0,FEEDRATERG1,UM_WHITE,UM_RED);
		}
		else
		{
			rval = atof(Tfeed[0]);
			if (rval<=0)
				fdsec1 =  UU_FALSE;
			if (fdsec1) 
			{
				ud_dispfrm_set_attribs(0,FEEDRATERG1,UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(0,FEEDRATERG1,UM_WHITE,UM_RED);
			}
		}
	}
	else
	{
		ud_dispfrm_set_attribs(0,FEEDRATERG1,UM_BLACK, UM_WHITE);
	}
	if (Tfopt[1]==1)
	{
		nc = (int)strlen(Tfeed[1]);
		ul_strip_blanks(Tfeed[1], &nc);
		if (nc<=0)
		{
			fdsec2 = UU_FALSE;
			ud_dispfrm_set_attribs(0,FEEDRATERG2,UM_WHITE,UM_RED);
		}
		else
		{
			rval = atof(Tfeed[1]);
			if (rval<=0)
				fdsec2 =  UU_FALSE;
			if (fdsec2) 
			{
				ud_dispfrm_set_attribs(0,FEEDRATERG2,UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(0,FEEDRATERG2,UM_WHITE,UM_RED);
			}
		}
	}
	else
	{
		ud_dispfrm_set_attribs(0,FEEDRATERG2,UM_BLACK, UM_WHITE);
	}
	if (Tfopt[2]==1)
	{
		nc = (int)strlen(Tfeed[2]);
		ul_strip_blanks(Tfeed[2], &nc);
		if (nc<=0)
		{
			fdsec3 = UU_FALSE;
			ud_dispfrm_set_attribs(0,FEEDRATERG3,UM_WHITE,UM_RED);
		}
		else
		{
			rval = atof(Tfeed[2]);
			if (rval<=0)
				fdsec3 =  UU_FALSE;
			if (fdsec3) 
			{
				ud_dispfrm_set_attribs(0,FEEDRATERG3,UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(0,FEEDRATERG3,UM_WHITE,UM_RED);
			}
		}
	}
	else
	{
		ud_dispfrm_set_attribs(0,FEEDRATERG3,UM_BLACK, UM_WHITE);
	}
/*
.....Set section color
*/
	if (sfpk1 && sfpk2 && sfpk3 && sfpk4 && sfpk5)
	{
		if (Sacc[Mill]==0)
		{
			Ssecol[Mill] = Sblack;
			S_section_changed(Mill,UU_FALSE);
		}
		else
		{	
			Ssecol[Mill] = Sgreen; 
			S_section_changed(Mill,UU_TRUE);
		}
	}
	else
	{
		Ssecol[Mill] = Sred; 
		S_section_changed(Mill,UU_FALSE);
	}

	if (sfpk6 && sfpk7 && valpk1)
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
	
	if (valpk2)
	{
		if (Sacc[MotionT]==0)
		{
			Ssecol[MotionT] = Sblack;
			S_section_changed(MotionT,UU_FALSE);
		}
		else
		{
			Ssecol[MotionT] = Sgreen; 
			S_section_changed(MotionT,UU_TRUE);
		}
	}
	else
	{
		Ssecol[MotionT] = Sred; S_section_changed(MotionT,UU_FALSE);
	}
	
	if (fdsec1 && fdsec2 && fdsec3)
	{
		if (Sacc[Feeds]==0)
		{
			Ssecol[Feeds] = Sblack;
			S_section_changed(Feeds,UU_FALSE);
		}
		else
		{
			Ssecol[Feeds] = Sgreen; 
			S_section_changed(Feeds,UU_TRUE);
		}
	}
	else
	{
		Ssecol[Feeds] = Sred; 
		S_section_changed(Feeds,UU_FALSE);
	}
/*
.....Set Action Buttons
*/
	ifl = sfpk1 && sfpk2 && sfpk3 && sfpk4 && sfpk5 && sfpk6 && sfpk7 && valpk1 && valpk2
			&&fdsec1&&fdsec2&&fdsec3;
	ud_frm_enable_ok(ifl);
	ud_set_traverse_mask(FAPV,ifl);
	ud_set_traverse_mask(FAPY,ifl);

	ifl = Sacc[Mill] + Sacc[Entry] + Sacc[MotionT] + Sacc[Thicks] + Sacc[Feeds] + Sacc[Colors]; 
	ud_set_traverse_mask(FARS,ifl);
/*
.....if geom entered
*/
	ul_to_upper(Tsf);
	strcpy(label, Tsf);
	nc1 = strlen(label);
	ul_strip_blanks(label,&nc1);
	ul_to_upper(Tc1);
	strcpy(label, Tc1);
	nc2 = strlen(label);
	ul_strip_blanks(label,&nc2);
	ul_to_upper(Tc2);
	strcpy(label, Tc2);
	nc3 = strlen(label);
	ul_strip_blanks(label,&nc3);
	ul_to_upper(Td1);
	strcpy(label, Td1);
	nc4 = strlen(label);
	ul_strip_blanks(label,&nc4);
	ul_to_upper(Td2);
	strcpy(label, Td2);
	nc5 = strlen(label);
	ul_strip_blanks(label,&nc5);

	if (Tclrtyp == 0)
	{
		ul_to_upper(Tclr);
		strcpy(label, Tclr);
		nc6 = strlen(label);
		ul_strip_blanks(label,&nc6);
	}
	else
		nc6 = 0;
	if (Trettyp == 1)
	{
		ul_to_upper(Tret);
		strcpy(label, Tret);
		nc7 = strlen(label);
		ul_strip_blanks(label,&nc7);
	}
	else
		nc7 = 0;
	if ((nc1>0)||(nc2>0)||(nc3>0)||(nc4>0)||(nc5>0)||(nc6>0)||(nc7>0))
	{
		ud_set_traverse_mask(FAGE,1);
	}
	else
		ud_set_traverse_mask(FAGE,0);

	return UU_TRUE;
}

/*********************************************************************
**    I_FUNCTION     : OnMillSel(fieldno,val,stat)
**       Method called when a Geometry Select button is pressed.
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
static UD_FSTAT OnMillSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status;
/*
.....Process Geometry pushbuttons
*/
	switch (*fieldno)
	{
/*
........Geometry selection buttons
*/
	case MILLRG1:
		status = S_select_geo(&Sgsrf,UU_NULL,UD_ncl_rmill_sf,0,442,Tsfcol,0,MILLRG2,Tsf);
		break;
	case MILLRG4:
		S_form_cont = 1;
		status = S_select_geo(&Sgds1,UU_NULL,UD_ncl_lnpl,0,445,Td1col,0,MILLRG5,Td1);
		S_form_cont = 0;
		if (status==-1)
			break;
	case MILLRG7:
		status = S_select_geo(&Sgds2,UU_NULL,UD_ncl_lnpl,0,446,Td2col,0,MILLRG8,Td2);
		break;
	case MILLRG10:
		S_form_cont = 1;
		status = S_select_geo(&Sgcs1,UU_NULL,UD_ncl_rmill_geo,0,443,Tc1col,0,MILLRG11,Tc1);
		S_form_cont = 0;
		if (status==-1)
			break;
	case MILLRG13:
		status = S_select_geo(&Sgcs2,UU_NULL,UD_ncl_rmill_geo,0,444,Tc2col,0,MILLRG14,Tc2);
		break;
	case MILLRG15:
		status = S_select_geo(&Sgpt, UU_NULL, UD_ncl_pt, 0, 658, Tptcol, 0, MILLRG16, Tnpt);
		break;
	}
/*
.....Check for change since preview
*/
/*??*/
	if (status == UU_SUCCESS&& Spreviewflg) Schgmade = 1;
	if (status == UU_SUCCESS)
	{
		Sacc[Mill] = 1;
		ud_set_traverse_mask(FAGE,1);
	}
	S_enable_buttons();
	S_form_vis();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnMillTog(fieldno,val,stat)
**       Method called when a Geometry toggle field is changed.
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
static UD_FSTAT OnMillTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Process Geometry toggle field
*/
	Sacc[Mill] = 1;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnMillTog(fieldno,val,stat)
**       Method called when a Geometry toggle field is changed.
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
static UD_FSTAT OnColorPicked(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int fno;
	UU_LOGICAL sfl;
/*
.....Process Geometry toggle field
*/
	switch (*fieldno)
	{
	case SFSCOLORS:
		S_hilite_entity(&Sgsrf,Tsfcol);
		break;
	case D1COLORS:
		S_hilite_entity(&Sgds1,Td1col);
		break;
	case D2COLORS:
		S_hilite_entity(&Sgds2,Td2col);
		break;
	case C1COLORS:
		S_hilite_entity(&Sgcs1,Tc1col);
		break;
	case C2COLORS:
		S_hilite_entity(&Sgcs2,Tc2col);
		break;
	case NPTCOLORS:
		S_hilite_entity(&Sgpt, Tptcol);
		break;
	case CPCOLORS:
		S_hilite_entity(&Sgclr,Tclrcol);
		break;
	case RPCOLORS:
		S_hilite_entity(&Sgret,Tretcol);
		break;
	case UNUCOLCHC:
		Tgeotog = val->frmint[0];
	case UNUSECOL:
		if (Sgeo_redraw)
		{
			Sgeo_redraw = UU_FALSE;
			fno = FAGE;
			OnAction(&fno,val,stat);
		}
		break;
	}
	Sacc[Colors] = 1;
	Ssecol[Colors] = Sgreen; 
	S_section_changed(Colors,UU_TRUE);
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnMillTxt(fieldno,val,stat)
**       Method called when a Geometry text field is changed.
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
static UD_FSTAT OnMillTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int nc;
	char label[2*65];
/*
.....do not save the value until all form save (click OK)
*/
/*
.....Process Geometry text field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case MILLRG2:
		if (strcmp(Ssf,Tsf) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
		}
		S_init_geo(&Sgsrf,Tsf,Tsfcol);
		break;
	case MILLRG5:
		if (strcmp(Sd1,Td1) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
		}
		S_init_geo(&Sgds1,Td1,Td1col);
		break;
	case MILLRG8:
		if (strcmp(Sd2,Td2) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
		}
		S_init_geo(&Sgds2,Td2,Td2col);
		break;
	case MILLRG11:
		if (strcmp(Sc1,Tc1) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
		}
		S_init_geo(&Sgcs1,Tc1,Tc1col);
		break;
	case MILLRG14:
		if (strcmp(Sc2,Tc2) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
		}
		S_init_geo(&Sgcs2,Tc2,Tc2col);
		break;
	case MILLRG16:
		if (strcmp(Snpt,Tnpt) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
		}
		S_init_geo(&Sgpt, Tnpt, Tptcol);
		break;
	}
	Sacc[Mill] = 1;
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnOptTog(fieldno,val,stat)
**       Method called when a Motion toggle field is changed.
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
static UD_FSTAT OnOptTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
/*
.....Process Motion toggle field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case OPTFMTYP:
		if (Tmtyp == 0)
		{
			ud_set_traverse_mask(ENEXTRG1,UU_TRUE);
			if (Tclrtyp == 0)
			{
				ud_set_traverse_mask(ENEXTRG2,UU_TRUE);
				ud_set_traverse_mask(ENEXTRG3,UU_TRUE);
				ud_set_traverse_mask(ENEXTRG4,UU_TRUE);
				ud_set_traverse_mask(CPCOLORS,UU_TRUE);
			}
			else
			{
				ud_set_traverse_mask(ENEXTRG2,UU_TRUE);
				ud_set_traverse_mask(ENEXTRG3,UU_FALSE);
				ud_set_traverse_mask(ENEXTRG4,UU_FALSE);
				ud_set_traverse_mask(CPCOLORS,UU_FALSE);
			}
		}
		else
		{
			ud_set_traverse_mask(ENEXTRG1,UU_FALSE);
			ud_set_traverse_mask(ENEXTRG2,UU_FALSE);
			ud_set_traverse_mask(ENEXTRG3,UU_FALSE);
			ud_set_traverse_mask(ENEXTRG4,UU_TRUE);
			ud_set_traverse_mask(CPCOLORS,UU_FALSE);
		}
		if (Tmtyp != Smtyp)
		{
			if (Spreviewflg) Schgmade = 1;
		}
		break;
	case OPTFMPAS:
		ud_set_traverse_mask(THICKSRG5, Tmpas);
		ud_set_traverse_mask(THICKSRG6, Tmpas);
		ud_set_traverse_mask(THICKSRG7, Tmpas);
		ud_set_traverse_mask(THICKSRG8, Tmpas);
		ud_update_form(0);
		if (Tmpas != Smpas)
		{
			if (Spreviewflg) Schgmade = 1;
		}
		break;
	}
	Sacc[MotionT] = 1;
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnMoTxt(fieldno,val,stat)
**       Method called when a Motion text field is changed.
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
static UD_FSTAT OnOptTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (strcmp(Smdis,Tmdis) != 0)
	{
/*
.....Change made since preview
*/
		if (Spreviewflg) Schgmade = 1;
	}
	Sacc[MotionT] = 1;
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnEnSel(fieldno,val,stat)
**       Method called when a Entry/Exit button is pressed.
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
static UD_FSTAT OnEnSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status;
/*
.....Process Entry/Exit pushbuttons
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case ENEXTRG3:
		status = S_select_geo(&Sgclr,UU_NULL,UD_ncl_pl,0,472,Tclrcol,0,ENEXTRG2,Tclr);
		break;
	case ENEXTRG7:
		status = S_select_geo(&Sgret,UU_NULL,UD_ncl_ptpl,0,400,Tretcol,0,ENEXTRG6,Tret);
		break;
	}
	if (status == UU_SUCCESS && Spreviewflg) Schgmade = 1; 
	if (status == UU_SUCCESS)
		ud_set_traverse_mask(FAGE,1);

	Sacc[Entry] = 1;
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnEnTog(fieldno,val,stat)
**       Method called when an Entry/Exit toggle field is changed.
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
/*
.....Process Geometry toggle field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case ENEXTRG1:
		if (Tclrtyp == 0)
		{
			ud_set_traverse_mask(ENEXTRG2,UU_TRUE);
			ud_set_traverse_mask(ENEXTRG3,UU_TRUE);
			ud_set_traverse_mask(ENEXTRG4,UU_TRUE);
			ud_set_traverse_mask(CPCOLORS,UU_TRUE);
			Tclr[0] = '\0';
			ud_update_answer(ENEXTRG2,(int *)Tclr);
		}
		else
		{
			ud_set_traverse_mask(ENEXTRG2,UU_TRUE);
			ud_set_traverse_mask(ENEXTRG3,UU_FALSE);
			ud_set_traverse_mask(CPCOLORS,UU_FALSE);
			ud_set_traverse_mask(ENEXTRG4,UU_FALSE);
			strcpy(Tclr,"0.");
			ud_update_answer(ENEXTRG2,(int *)Tclr);
			S_unhilite_entity(&Sgclr);
		}
		if (Tclrtyp != Sclrtyp)
		{
			if (Spreviewflg) Schgmade = 1;
		}
		break;
	case CPCOLORS:
		S_hilite_entity(&Sgclr,Tclrcol);
		break;
	case ENEXTRG5:
		if (Trettyp == 0)
		{
			ud_set_traverse_mask(ENEXTRG6,UU_FALSE);
			ud_set_traverse_mask(ENEXTRG7,UU_FALSE);
			ud_set_traverse_mask(RPCOLORS,UU_FALSE);
			Tret[0] = '\0';
			ud_update_answer(ENEXTRG6,(int *)Tret);
			S_unhilite_entity(&Sgret);
		}
		else if (Trettyp == 2)
		{
			ud_set_traverse_mask(ENEXTRG6,UU_TRUE);
			ud_set_traverse_mask(ENEXTRG7,UU_FALSE);
			ud_set_traverse_mask(RPCOLORS,UU_FALSE);
			strcpy(Tret,"0.");
			ud_update_answer(ENEXTRG6,(int *)Tret);
			S_unhilite_entity(&Sgret);
		}
		else
		{
			ud_set_traverse_mask(ENEXTRG6,UU_TRUE);
			ud_set_traverse_mask(ENEXTRG7,UU_TRUE);
			ud_set_traverse_mask(RPCOLORS,UU_TRUE);
			Tret[0] = '\0';
			ud_update_answer(ENEXTRG6,(int *)Tret);
		}
		if (Trettyp != Srettyp)
		{
			if (Spreviewflg) Schgmade = 1;
		}
		break;
	case RPCOLORS:
		S_hilite_entity(&Sgret,Tretcol);
		break;
	}
	Sacc[Entry] = 1;
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnEnTxt(fieldno,val,stat)
**       Method called when an Entry/Exit text field is changed.
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
	int nc;
	char label[65*2];
/*
.....Process Geometry text field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case ENEXTRG2:
		if (strcmp(Sclr,Tclr) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
		}
		S_init_geo(&Sgclr,Tclr,Tclrcol);
		break;
	case ENEXTRG4:
		if (strcmp(Srap,Trap) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
		}
	case ENEXTRG6:
		if (strcmp(Sret,Tret) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
		}
		S_init_geo(&Sgret,Tret,Tretcol);
		break;
	}
	Sacc[Entry] = 1;
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_section_changed_old(section,color,bold,reset)
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
static void S_section_changed_old(section,color,bold,reset)
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
**    I_FUNCTION     : S_section_changed(section,color,bold,reset)
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
	ud_form_section_color(0, Smode[section], Ssecol[section], bold);
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
	S_add_key(which, Sgsrf.key);
	S_add_key(which, Sgds1.key);
	S_add_key(which, Sgds2.key);
	S_add_key(which, Sgcs1.key);
	S_add_key(which, Sgcs2.key);
	S_add_key(which, Sgclr.key);
	S_add_key(which, Sgret.key);
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
	int status,i,flag= 0;
	switch (*fieldno)
	{
/*
.....Preview command
*/
	case FAPV:
/*
........Reset motion settings
*/
		if (Spreviewflg) moinfr(&flag);
		Spreviewflg = UU_FALSE;
		Schgmade = UU_FALSE;
		Serrflg = 0;
/*
.....Validate form answers
*/
		if (!S_valid_command(UU_TRUE)) break;
/*
........Erase last previewed motion
*/
		if (Smptr != UU_NULL) 
			ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
		Smptr = UU_NULL;
/*
........Save motion settings
*/
		moinfs();
		ncl_mark_mdisplay(&Smptr,&Smotatt,Smvpbox,UU_TRUE);
/*
........Output RMILL command
*/
		status = S_build_command(UU_FALSE);
		if (status != UU_SUCCESS) 
			moinfr(&flag);
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
/*
........Output RMILL command
*/
		if (!S_valid_command(UU_TRUE)) 
			break;
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
			break;
/*
.....reset all geometries fields per Ken
*/
		S_deselect_all();
		Sgeo_apply = UU_TRUE;
		for (i=FAPV;i<FAVW;i++) 
			ud_set_traverse_mask(i,0);
		i = -1;
		S_enter_form(&i, val, stat);
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
		if (Smptr != UU_NULL) 
			ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
		Smptr = UU_NULL;
		if (Spreviewflg) 
			moinfr(&flag);
		S_init_form();
		S_enter_form(fieldno,val,stat);
		for (i=FAPV;i<FAVW;i++) 
			ud_set_traverse_mask(i,0);
		S_update_answers();
		Schgmade = Spreviewflg = UU_FALSE;
		Sacc[Mill] = Sacc[Entry] = Sacc[MotionT] = Sacc[Thicks] = Sacc[Feeds] = Sacc[Colors] = 0;
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
	return(UD_FLDOK);
}


/*********************************************************************
**    I_FUNCTION     : OnThkTxt(fieldno,val,stat)
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
static UD_FSTAT OnThkTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,j;

	i = j = -1;

	switch(*fieldno)
	{
	case THICKSRG1:
		i = 0;
		break;
	case THICKSRG2:
		i = 1;
		break;
	case THICKSRG3:
		i = 2;
		break;
	case THICKSRG4:
		i = 3;
		break;
	case THICKSRG5:
		j = 0;
		break;
	case THICKSRG6:
		j = 1;
		break;
	case THICKSRG7:
		j = 2;
		break;
	case THICKSRG8:
		j = 3;
		break;
	}
/*
.....Check for change since preview
*/
	if ((i >= 0 && strcmp(Srthk[i],Trthk[i]) != 0) ||
		(j >= 0 && strcmp(Sfthk[j],Tfthk[j]) != 0))
	{
		if (Spreviewflg) Schgmade = 1;
		if (i >= 0) strcpy(Srthk[i],Trthk[i]);
		else strcpy(Sfthk[j],Tfthk[j]);
		Sacc[Thicks] = 1;
		Ssecol[Thicks] = Sgreen; 
		S_section_changed(Thicks,UU_TRUE);
	}
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
	case FEEDRATERG1:
		i = 0;
		break;
	case FEEDRATERG2:
		i = 1;
		break;
	case FEEDRATERG3:
		i = 2;
		break;
	}
/*
.....Check for change since preview
*/
	if (i >= 0 && strcmp(Srthk[i],Trthk[i]) != 0)
	{
		if (Spreviewflg) Schgmade = 1;
		strcpy(Srthk[i],Trthk[i]);
		Sacc[Feeds] = 1;
		Ssecol[Feeds] = Sgreen; 
		S_section_changed(Feeds,UU_TRUE);
	}
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
	char valuestr[80];
	 int i = -1;
/*
.....Process Feed Rate toggle field
*/
	switch(*fieldno)
	{
	case FEEDRATECHC1:
		i = 0;
		if (Tfopt[0]==0)
		{
			ud_setfrm_traverse_mask(0,FEEDRATERG1,UU_FALSE);
			strcpy(Sav_valuestr1, Tfeed[0]);
			valuestr[0] = '\0';
			ud_dispfrm_update_answer(0,FEEDRATERG1,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(0,FEEDRATERG1,UU_TRUE);
			strcpy(Tfeed[0], Sav_valuestr1);
			ud_dispfrm_update_answer(0,FEEDRATERG1,(int *)Tfeed[0]);
		}
		break;
	case FEEDRATECHC2:
		i = 1;
		if ((Tfopt[1]==0)||(Tfopt[1]==2))
		{
			ud_setfrm_traverse_mask(0,FEEDRATERG2,UU_FALSE);
			valuestr[0] = '\0';
			if (Tfeed[1][0]!='\0')
				strcpy(Sav_valuestr2, Tfeed[1]);
			ud_dispfrm_update_answer(0,FEEDRATERG2,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(0,FEEDRATERG2,UU_TRUE);
			strcpy(Tfeed[1], Sav_valuestr2);
			ud_dispfrm_update_answer(0,FEEDRATERG2,(int *)Tfeed[1]);
		}
		break;
	case FEEDRATECHC3:
		i = 2;
		if (Tfopt[2]==0)
		{
			ud_setfrm_traverse_mask(0,FEEDRATERG3,UU_FALSE);
			valuestr[0] = '\0';
			strcpy(Sav_valuestr3, Tfeed[2]);
			ud_dispfrm_update_answer(0,FEEDRATERG3,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(0,FEEDRATERG3,UU_TRUE);
			strcpy(Tfeed[2], Sav_valuestr3);
			ud_dispfrm_update_answer(0,FEEDRATERG3,(int *)Tfeed[2]);
		}
	}
/*
.....Check for change since preview
*/
	if (Sfopt[i] != Tfopt[i])
	{
		if (Spreviewflg) Schgmade = 1;
		Sfopt[i] = Tfopt[i];
		Sacc[Feeds] = 1;
		Ssecol[Feeds] = Sgreen; 
		S_section_changed(Feeds,UU_TRUE);
	}
/*
	if (*(val->frmint) == 0 || *(val->frmint) == 2)
		ud_set_traverse_mask((*fieldno)+1, UU_FALSE);
	else
		ud_set_traverse_mask((*fieldno)+1, UU_TRUE);
*/
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
.....Initialize Entry / Exit fields
*/
	if (Tmtyp == 0)
	{
		traverse[ENEXTRG1] = 1;
		traverse[ENEXTRG4] = 1;
		if (Tclrtyp == 0)
			traverse[ENEXTRG2] = traverse[ENEXTRG3] = traverse[CPCOLORS] = 1;
		else
		{
			traverse[ENEXTRG2] = 1;
			traverse[ENEXTRG3] = traverse[CPCOLORS] = 0;
		}
	}
	else
	{
		traverse[ENEXTRG1] = traverse[ENEXTRG2] = traverse[ENEXTRG3] = traverse[CPCOLORS] = 0;
	}

	if (Trettyp == 0)
		traverse[ENEXTRG6] = traverse[ENEXTRG7] = traverse[RPCOLORS] = 0;
	else if (Trettyp == 2)
	{
		traverse[ENEXTRG6] = 1;
		traverse[ENEXTRG7] = traverse[RPCOLORS] = 0;
	}
	else
		traverse[ENEXTRG6] = traverse[ENEXTRG7] = traverse[RPCOLORS] = 1;

	for (i=0;i<4;i++)
	{
		if (Tmpas == 0) 
			traverse[THICKSRG1+i+4] = 0;
		else 
			traverse[THICKSRG1+i+4] = 1;
	}
	if (Tfopt[0] == 0)
		traverse[FEEDRATERG1] = 0;
	else
		traverse[FEEDRATERG1] = 1;
	if ((Tfopt[1] == 0)|| (Tfopt[1] == 2))
		traverse[FEEDRATERG2] = 0;
	else
		traverse[FEEDRATERG2] = 1;
	if (Tfopt[2] == 0)
		traverse[FEEDRATERG3] = 0;
	else
		traverse[FEEDRATERG3] = 1;
/*
.....Action item fields
*/
	for (i=FAPV;i<FAVW;i++) traverse[i] = 0;
}

/*********************************************************************
**    I_FUNCTION     : S_init_form()
**       Initializes the Regional Milling form variables.
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
	int i;
	int iclf[10];
	UU_REAL cbuf[6],dbuf[20], rval, feedrate;
	char sym[80];
	NCLX_mot_feedrate fedrat;
	UM_int2 mot_typ, stp_mod, fpas, rghthk, fin, fret, fclr;
	UM_real8 clr_dist, plun_dist, stp_dist, rthk[8];
	UM_real4 feedr, pos_fedr, ramp_fedr, ret_val;
	char ret_label[65], clr_label[65];
	void ncl_get_rmill_values();
/*
.....Initialize the form settings
*/
	if (!Sform_init)
	{
		Ssfcol = NCLX_SEA_GREEN;
		Sd1con = 0;
		Sd1col = NCLX_ORANGE;
		Sd2con = 0;
		Sd2col = NCLX_ORANGE;
		Sc1con = 0;
		Sc1col = NCLX_PURPLE;
		Sc2con = 0;
		Sc2col = NCLX_PURPLE;
		Sptcol = NCLX_PINK;
		Sclrcol = NCLX_LT_BLUE;
		Sretcol = NCLX_BROWN;
		Sclrtyp = 1;
		Sfopt[0] = 0; Sfopt[1] = 2; Sfopt[2] = 0;
		Srettyp = 0;
		Sret[0] = '\0';
	}
	NclxMotGetFeedrate(&fedrat);
	feedrate = fedrat.base_feedrate;
	ncl_val2str (feedrate, Sfeed_valuestr);

	ncl_get_rmill_values(&mot_typ, &stp_mod, &fpas, &fclr,&clr_dist, clr_label, &plun_dist, 
		&stp_dist, &feedr, &pos_fedr, &ramp_fedr, &fret, &ret_val, ret_label, &rghthk, &fin, rthk);
	Smtyp = mot_typ-1;
	Smpas = fpas;
	Smstep = stp_mod-1;
	ncl_sprintf(Smdis,&stp_dist,1);
	Sclrtyp = fclr;
	if (Sclrtyp==0)
		Sclr[0] = '\0';
	else
		ncl_sprintf(Sclr,&clr_dist,1);
	ncl_sprintf(Srap,&plun_dist,1);
	Srettyp = fret;
	rval = ret_val;
	ncl_sprintf(Sret,&rval,1);
	for (i=0;i<4;i++)
	{
		ncl_sprintf(Srthk[i],&rthk[i],1);
		ncl_sprintf(Sfthk[i],&rthk[i+4],1);
	}
	rval = feedr;
	ncl_sprintf(Sfeed[0],&rval,1);
	strcpy(Sav_valuestr1, Sfeed[0]);
	if ((rval!=feedrate)&&(rval!=0.0))
		Sfopt[0] = 1;
	else
	{
		Sfopt[0] = 0;
	}
	rval = ramp_fedr;
	ncl_sprintf(Sfeed[2],&rval,1);
	strcpy(Sav_valuestr3, Sfeed[2]);
	if ((rval!=feedrate)&&(rval!=0.0))
		Sfopt[2] = 1;
	else
		Sfopt[2] = 0;
	if ((pos_fedr==-1)||(pos_fedr==0.0))
	{
/*
......rapid
*/
		Sfopt[1] = 2;
		Sfeed[1][0] = '\0';
	}
	else
	{
		Sfopt[1] = 1;
		rval = pos_fedr;
		ncl_sprintf(Sfeed[1],&rval,1);
		strcpy(Sav_valuestr2, Sfeed[1]);
	}
/*
.....always empty for surfaces per Ken
*/
	Ssf[0] = '\0';
	Sd1[0] = '\0';
	Sd2[0] = '\0';
	Sc1[0] = '\0';
	Sc2[0] = '\0';
	if (Sclrtyp==0)
	{
		Sclr[0] = '\0';
		if (clr_label[0]!='\0')
			strcpy(Sclr, clr_label);
	}
	if (Srettyp == 0)
		Sret[0] = '\0';
	else if (Srettyp==1)
	{
		Sret[0] = '\0';
		if (ret_label[0]!='\0')
			strcpy(Sret, ret_label);
	}

	strcpy(Tsf,Ssf);
	Tsfcol = Ssfcol;

	strcpy(Td1,Sd1);
	Td1con = Sd1con;
	Td1col = Sd1col;
	strcpy(Td2,Sd2);
	Td2con = Sd2con;
	Td2col = Sd2col;

	strcpy(Tc1,Sc1);
	Tc1con = Sc1con;
	Tc1col = Sc1col;
	strcpy(Tc2,Sc2);
	Td2con = Sc2con;
	Tc2col = Sc2col;

	strcpy(Tnpt,Snpt);
	Tptcol = Sptcol;

	Tmtyp = Smtyp;
	Tmpas = Smpas;
	Tmstep = Smstep;
	strcpy(Tmdis,Smdis);

	Tclrtyp = Sclrtyp;
	strcpy(Tclr,Sclr);
	Tclrcol = Sclrcol;
	strcpy(Trap,Srap);
	Trettyp = Srettyp;
	strcpy(Tret,Sret);
	Tretcol = Sretcol;
/*
.....Initialize Thick settings
*/
	for (i=0;i<4;i++)
	{
		strcpy(Trthk[i],Srthk[i]);
		strcpy(Tfthk[i],Sfthk[i]);
	}
/*
.....Initialize Feed Rate settings
*/
	for (i=0;i<3;i++)
	{
		Tfopt[i] = Sfopt[i];
		if (Tfopt[i]!=0)		
			strcpy(Tfeed[i],Sfeed[i]);
		else
			Tfeed[i][0] = '\0';
	}
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
	Sgsrf.key = Sgds1.key = Sgds2.key = Sgcs1.key = Sgcs2.key = Sgpt.key = 0;
	Sgsrf.color = Sgds1.color = Sgds2.color = Sgcs1.color = Sgcs2.color = Sgpt.color = -1;
	Sgsrf.label[0] = Sgds1.label[0] = Sgds2.label[0] = Sgcs1.label[0] = '\0';
	Sgcs2.label[0] = Sgpt.label[0] = '\0';
	S_init_geo(&Sgsrf,Tsf,Tsfcol);
	S_init_geo(&Sgds1,Td1,Td1col);
	S_init_geo(&Sgds2,Td2,Td2col);
	S_init_geo(&Sgcs1,Tc1,Tc1col);
	S_init_geo(&Sgcs2,Tc2,Tc2col);
	S_init_geo(&Sgpt, Tnpt, Tptcol);

	Sgclr.key = Sgret.key = 0;
	Sgclr.color = Sgret.color = -1;
	Sgclr.label[0] = Sgret.label[0] = '\0';
	S_init_geo(&Sgclr,Tclr,Tclrcol);
	S_init_geo(&Sgret,Tret,Tretcol);
/*
.....Erase the motion &
.....Display the current cutter position
*/
/*
......don't erase motion now
*/
/*	nclu_erase_motion(); */
	nclu_disp_cut();
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
.....Save main form settings
*/
	strcpy(Ssf,Tsf);
	Ssfcol = Tsfcol;

	strcpy(Sd1,Td1);
	Sd1con = Td1con;
	Sd1col = Td1col;
	strcpy(Sd2,Td2);
	Sd2con = Td2con;
	Sd2col = Td2col;

	strcpy(Sc1,Tc1);
	Sc1con = Tc1con;
	Sc1col = Tc1col;
	strcpy(Sc2,Tc2);
	Sd2con = Tc2con;
	Sc2col = Tc2col;

	strcpy(Snpt,Tnpt);
	Sptcol = Tptcol;

	Smtyp = Tmtyp;
	Smpas = Tmpas;
	Smstep = Tmstep;
	strcpy(Smdis,Tmdis);

	Sclrtyp = Tclrtyp;
	strcpy(Sclr,Tclr);
	Sclrcol = Tclrcol;
	strcpy(Srap,Trap);
	Srettyp = Trettyp;
	strcpy(Sret,Tret);
	Sretcol = Tretcol;
/*
.....Save Thick settings
*/
	for (i=0;i<4;i++)
	{
		strcpy(Srthk[i],Trthk[i]);
		strcpy(Sfthk[i],Tfthk[i]);
	}
/*
.....Save Feed Rate settings
*/
	for (i=0;i<3;i++)
	{
		Sfopt[i] = Tfopt[i];
		strcpy(Sfeed[i],Tfeed[i]);
	}
/*
.....Save geometry colors
*/
	UN_unused_geo_flag = Tgeotog;
	UN_unused_geo_color = Tgeocol;
	nclu_save_preview_modals();
}

/*********************************************************************
**    I_FUNCTION     : S_form_invis()
**       Takes down all Rmill forms.
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
**       Redisplays all Rmill forms.
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
**    I_FUNCTION     :  S_select_geo(sfpt,sflst,mask,multi,prmno,color,frm,
**                                   fieldno,label)
**       Routine to select geometry for the form.  The associated
**       text field will be updated with the geometry label and
**       the geometry will be highlighted.
**    PARAMETERS
**       INPUT  :
**          mask     Picking mask.
**          multi    0 = Single selection, 1 = multiple selection,
**                   2 = Single selection (calling routine handles invisibling
**                   of form).
**          prmno    Prompt number to use while in pick mode.
**          color    Color to highlight the picked entity.
**                   -1 = Don't highlight entity.
**          frm      Form number that the field resides on. 0 = Main form.
**          fieldno  Text field to update with label of selected entity.
**                   -1 = No field to updated.
**          label    Text buffer that belongs to 'fieldno'.
**       OUTPUT :
**          sfpt     Pointer to selected geometry structure for single entity.
**          sflst    Pointer to selected geometry structure list when
**                   multiple entities can be picked (multi).
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
	if (multi != 2)
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
	if (multi != 2)
	{
		if (sfpt != UU_NULL) S_unhilite_entity(sfpt);
	}
	else
	{
		sfpt->key = 0;
		sfpt->color = -1;
		sfpt->label[0] = '\0';
	}
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
		if (multi != 1)
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
	if (multi != 1)
	{
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
			else ud_dispfrm_update_answer(frm,fieldno,(int *)label);
		}
	}
/*
.....Multiple selection
*/
	else
	{
/*
........Store the entities' data
*/
		uu_list_init(sflst,sizeof(UM_sgeo),numint,10);
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
	if ((multi != 2)&&(S_form_cont==0)) 
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
**    S_FUNCTION     :  S_deselect_all()
**       Deselects all geometry in the Geomtry section.  The associated
**       text fields will be cleared and the geometry will be unhighlighted.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_deselect_all()
{
/*
.....Unhilite any selected entities
*/
	S_unhilite_entity(&Sgsrf);
	S_unhilite_entity(&Sgds1);
	S_unhilite_entity(&Sgds2);
	S_unhilite_entity(&Sgcs1);
	S_unhilite_entity(&Sgcs2);
	S_unhilite_entity(&Sgpt);

	Tsf[0] = Td1[0] = Td2[0] = Tc1[0] = Tc2[0] = Tnpt[0] = '\0';
	if (Tclrtyp == 0)
	{
		S_unhilite_entity(&Sgclr);
		Tclr[0] = '\0';
	}
	if (Trettyp == 1)
	{
		S_unhilite_entity(&Sgret);
		Tret[0] = '\0';
	}
/*
.....Initialize geometry
*/
	Sgsrf.key = Sgds1.key = Sgds2.key = Sgcs1.key = Sgcs2.key = Sgclr.key = Sgret.key = 0;
	Sgpt.key = 0;
	Sgsrf.color = Sgds1.color = Sgds2.color = Sgcs1.color = Sgcs2.color = -1;
	Sgpt.color = Sgret.color = Sgclr.color = -1;
	Sgsrf.label[0] = Sgds1.label[0] = Sgds2.label[0] = Sgcs1.label[0] = '\0';
	Sgcs2.label[0] = Sgpt.label[0] = '\0';
	Sgret.label[0] = Sgclr.label[0] = '\0';

	ud_update_answer(MILLRG2,(int *)Tsf);
	ud_update_answer(MILLRG5,(int *)Td1);
	ud_update_answer(MILLRG8,(int *)Td2);
	ud_update_answer(MILLRG11,(int *)Tc1);
	ud_update_answer(MILLRG14,(int *)Tc2);
	ud_update_answer(MILLRG16,(int *)Tnpt);
	ud_update_answer(ENEXTRG2,(int *)Tclr);
	ud_update_answer(ENEXTRG6,(int *)Tret);
}

static void S_unhilite_all()
{
	S_unhilite_entity(&Sgsrf);
	S_unhilite_entity(&Sgds1);
	S_unhilite_entity(&Sgds2);
	S_unhilite_entity(&Sgcs1);
	S_unhilite_entity(&Sgcs2);
	S_unhilite_entity(&Sgpt);
	S_unhilite_entity(&Sgclr);
	S_unhilite_entity(&Sgret);
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
	int i, nc;
	UU_REAL rval;
/*
.....Set the Section colors
*/
	if (!Serrflg)
	{
		Ssecol[Mill] = Sred;
		Ssecol[Entry] = Sred;
		ncl_get_scalar_value(Tmdis,&rval);
		if (rval <= 0.)
			Ssecol[MotionT] = Sred;

		ud_frm_enable_ok(UU_FALSE);
		if (*fieldno != -1)
		{
			for (i=Mill;i<=Colors;i++)
				S_section_changed(i,UU_FALSE);
		}
		ud_frm_enable_ok(UU_FALSE);
	}
	else
		S_enable_buttons();
/*
.....Set the mandatory fields to red
*/
	ud_dispfrm_set_attribs(0,MILLRG1,UM_WHITE,UM_RED);
	ud_dispfrm_set_attribs(0,MILLRG4,UM_WHITE,UM_RED);
	ud_dispfrm_set_attribs(0,MILLRG7,UM_WHITE,UM_RED);
	ud_dispfrm_set_attribs(0,MILLRG10,UM_WHITE,UM_RED);
	ud_dispfrm_set_attribs(0,MILLRG13,UM_WHITE,UM_RED);
	ud_dispfrm_set_attribs(0,MILLRG13,UM_WHITE,UM_RED);
	ud_dispfrm_set_attribs(0,MILLRG15,UM_BLACK,Tptcol);
	if (Tclrtyp == 0)
		ud_dispfrm_set_attribs(0,ENEXTRG3,UM_WHITE,UM_RED);
	else
	{
		ud_dispfrm_set_attribs(0,ENEXTRG3,UM_BLACK,Tclrcol);
	}
	if (Trettyp != 0)
	{
		nc = strlen(Tret);
		ul_strip_blanks(Tret,&nc);
		if (nc == 0)
		{
			ud_dispfrm_set_attribs(0,ENEXTRG7,UM_WHITE,UM_RED);
		}
		else
			ud_dispfrm_set_attribs(0,ENEXTRG7,UM_BLACK,Tretcol);
	}
	else
		ud_dispfrm_set_attribs(0,ENEXTRG7,UM_BLACK,Tretcol);
/*
.....Set Pick buttons to correct color
*/
	ud_dispfrm_set_attribs(0,SFSCOLORS,UM_BLACK,Tsfcol);
	ud_dispfrm_set_attribs(0,D1COLORS,UM_BLACK,Td1col);
	ud_dispfrm_set_attribs(0,D2COLORS,UM_BLACK,Td2col);
	ud_dispfrm_set_attribs(0,C1COLORS,UM_BLACK,Tc1col);
	ud_dispfrm_set_attribs(0,C2COLORS,UM_BLACK,Tc2col);
	ud_dispfrm_set_attribs(0,NPTCOLORS,UM_BLACK,Tptcol);
	ud_dispfrm_set_attribs(0,CPCOLORS,UM_BLACK,Tclrcol);
	ud_dispfrm_set_attribs(0,RPCOLORS,UM_BLACK,Tretcol);
	ud_dispfrm_set_attribs(0,UNUSECOL,UM_BLACK,Tgeocol);
	S_enable_buttons();
	return(UD_FLDOK);
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
			ug_wflush();
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
.....Unhighlight entity
*/
			if (Sgeo_redraw) ncl_remove_override_geo(e.key);
			ncl_update_geo_color(e.key,sfpt->color,UU_FALSE);
			uc_display(&e);
			ug_wflush();
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
**    I_FUNCTION     : S_valid_cmd(flag)
**			Determines if all paramters are specified for a valid command.
**    PARAMETERS   
**       INPUT  : flag    = UU_TRUE  = Output error message if invalid
**                                     command.
**			                   UU_FALSE = Don't output error message.
**       OUTPUT :  none.
**    RETURNS      : UU_TRUE if form parameters are valid,
**                   UU_FALSE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL S_valid_command(flag)
UU_LOGICAL flag;
{
	int nc,status;
	UU_REAL rval;
	NCL_cmdbuf cmdbuf;
/*
.....Surface parameter
*/
	nc = strlen(Tsf);
	ul_strip_blanks(Tsf,&nc);
	if (nc == 0)
	{
		if (flag) ud_wrerr("A surface must be selected.");
		return(UU_FALSE);
	}
/*
.....Check surface parameter
*/
	nc = strlen(Tc1);
	ul_strip_blanks(Tc1,&nc);
	if (nc == 0)
	{
		if (flag) ud_wrerr("Check surface 1 must be selected.");
		return(UU_FALSE);
	}

	nc = strlen(Tc2);
	ul_strip_blanks(Tc2,&nc);
	if (nc == 0)
	{
		if (flag) ud_wrerr("Check surface 2 must be selected.");
		return(UU_FALSE);
	}
/*
.....Drive surface parameters
*/
	nc = strlen(Td1);
	ul_strip_blanks(Td1,&nc);
	if (nc == 0)
	{
		if (flag) ud_wrerr("Drive surface 1 must be selected.");
		return(UU_FALSE);
	}

	nc = strlen(Td2);
	ul_strip_blanks(Td2,&nc);
	if (nc == 0)
	{
		if (flag) ud_wrerr("Drive surface 2 must be selected.");
		return(UU_FALSE);
	}
/*
.....Clearance plane & rapto distance
.......Added rapto distance for SCRUB - Andrew 11/20/12
*/
	if (Tmtyp == 0 || Tmtyp == 1)
	{
		if (Tmtyp == 0)
		{
			nc = strlen(Tclr);
			ul_strip_blanks(Tclr,&nc);
			if (nc == 0)
			{
				if (flag) ud_wrerr("A clearance plane/distance must be selected.");
				return(UU_FALSE);
			}
		}
		nc = strlen(Trap);
		ul_strip_blanks(Trap,&nc);
		status = UU_SUCCESS;
		Sscrub_rapto = UU_FALSE;
		if (nc > 0 && Tmtyp == 1)
		{
			status = ncl_get_scalar_value(Trap,&rval);
			if (status != -1 && rval > 0.) Sscrub_rapto = UU_TRUE;
			else if (flag && status == -1) ud_wrerr("Invalid rapto distance specified.");
		}
		else if (nc == 0 && Tmtyp == 0)
		{
			if (flag) ud_wrerr("A rapto distance must be specified.");
			return(UU_FALSE);
		}
	}
/*
.....Step over distance
*/
	ncl_get_scalar_value(Tmdis,&rval);
	if (rval <= 0.)
	{
		if (flag) ud_wrerr("Step over distance must be a positive value.");
		return(UU_FALSE);
	}
/*
.....Feed rates
*/
/*
.....Retract plane
*/
	if (Trettyp != 0)
	{
		nc = strlen(Tret);
		ul_strip_blanks(Tret,&nc);
		if (nc == 0)
		{
			if (flag) ud_wrerr("A retract plane/distance must be selected.");
			return(UU_FALSE);
		}
	}
/*
.....Thicks
*/
	return(UU_TRUE);
}

/*********************************************************************
**    I_FUNCTION     : S_build_cmd(flag)
**			Builds and outputs the RMILL command.
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
	int i,nc;
	UM_int2 idx;
	UU_REAL thk[4],rval;
	char fbuf[20];
	NCL_cmdbuf cmdbuf;
	if (flag)
		NCL_preview_mot = 0;
	else
		NCL_preview_mot = 1;
/*
.....Initialize command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
/*
.....RMILL command
*/
	if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
	ncl_add_token(&cmdbuf, NCL_rmill, NCL_nocomma);
/*
.....Surface parameter
*/
	nc = strlen(Tsf);
	ul_strip_blanks(Tsf,&nc);
	ncl_add_token(&cmdbuf,Tsf,NCL_comma);
/*
.....Check surface parameters
*/
	if (Tc1con == 0) ncl_add_token(&cmdbuf,NCL_to,NCL_comma);
	else if (Tc1con == 1) ncl_add_token(&cmdbuf,NCL_on,NCL_comma);
	else ncl_add_token(&cmdbuf,NCL_past,NCL_comma);
	nc = strlen(Tc1);
	ul_strip_blanks(Tc1,&nc);
	ncl_add_token(&cmdbuf,Tc1,NCL_comma);

	if (Tc2con == 0) ncl_add_token(&cmdbuf,NCL_to,NCL_comma);
	else if (Tc2con == 1) ncl_add_token(&cmdbuf,NCL_on,NCL_comma);
	else ncl_add_token(&cmdbuf,NCL_past,NCL_comma);
	nc = strlen(Tc2);
	ul_strip_blanks(Tc2,&nc);
	ncl_add_token(&cmdbuf,Tc2,NCL_comma);
/*
.....Drive surface parameters
*/
	if (Td1con == 0) ncl_add_token(&cmdbuf,NCL_to,NCL_comma);
	else if (Td1con == 1) ncl_add_token(&cmdbuf,NCL_on,NCL_comma);
	else ncl_add_token(&cmdbuf,NCL_past,NCL_comma);
	nc = strlen(Td1);
	ul_strip_blanks(Td1,&nc);
	ncl_add_token(&cmdbuf,Td1,NCL_comma);

	if (Td2con == 0) ncl_add_token(&cmdbuf,NCL_to,NCL_comma);
	else if (Td2con == 1) ncl_add_token(&cmdbuf,NCL_on,NCL_comma);
	else ncl_add_token(&cmdbuf,NCL_past,NCL_comma);
	nc = strlen(Td2);
	ul_strip_blanks(Td2,&nc);
	ncl_add_token(&cmdbuf,Td2,NCL_comma);
/*
.....Motion type
*/
	if (Tmtyp == 0 && !Tmpas) ncl_add_token(&cmdbuf,"1",NCL_comma);
	else if (Tmtyp == 0 && Tmpas) ncl_add_token(&cmdbuf,"-1",NCL_comma);
	else if (Tmtyp == 1 && !Tmpas) ncl_add_token(&cmdbuf,"2",NCL_comma);
	else if (Tmtyp == 1 && Tmpas) ncl_add_token(&cmdbuf,"-2",NCL_comma);
/*
.....Clearance plane & rapto distance
.......Added SCRUB rapto distance - Andrew 11/20/12
*/
	if (Tmtyp == 0 || Tmtyp == 1)
	{
		if (Tmtyp == 0)
		{
			nc = strlen(Tclr);
			ul_strip_blanks(Tclr,&nc);
			ncl_add_token(&cmdbuf,Tclr,NCL_comma);
		}
		else if (Sscrub_rapto)
			ncl_add_token(&cmdbuf,"0",NCL_comma);
		if (Tmtyp == 0 || (Tmtyp == 1 && Sscrub_rapto))
		{
			nc = strlen(Trap);
			ul_strip_blanks(Trap,&nc);
			ncl_add_token(&cmdbuf,Trap,NCL_comma);
		}
	}
/*
.....Step over type
*/
	if (Tmstep == 0) ncl_add_token(&cmdbuf,"1",NCL_comma);
	else ncl_add_token(&cmdbuf,"2",NCL_comma);
	nc = strlen(Tmdis);
	ul_strip_blanks(Tmdis,&nc);
	ncl_add_token(&cmdbuf,Tmdis,NCL_comma);
/*
.....Feed rates
*/
	idx = 123; getsc(&idx,&rval);
	ncl_sprintf(fbuf,&rval,1);
	for (i=0;i<3;i++)
	{
		if (Tfopt[i] == 0) 
			ncl_add_token(&cmdbuf,fbuf,NCL_comma);
		else if (Tfopt[i] == 1)
			ncl_add_token(&cmdbuf,Tfeed[i],NCL_comma);
		else
			ncl_add_token(&cmdbuf,"RAPID",NCL_comma);
	}
/*
.....Retract plane
*/
	if (Trettyp != 0)
	{
		nc = strlen(Tret);
		ul_strip_blanks(Tret,&nc);
		ncl_add_token(&cmdbuf,Tret,NCL_comma);
	}
/*
.....Thicks
*/
		for (i=0;i<4;i++) ncl_get_scalar_value(Trthk[i],&thk[i]);
		if (thk[0] != 0. || thk[1] != 0. || thk[2] != 0. || thk[3] != 0.)
		{
			ncl_add_token(&cmdbuf,NCL_rough,NCL_comma);
			for (i=0;i<4;i++)
			{
				nc = strlen(Trthk[i]);
				ul_strip_blanks(Trthk[i],&nc);
				ncl_add_token(&cmdbuf,Trthk[i],NCL_comma);
			}
		}

		if (Tmpas == 1)
		{
			for (i=0;i<4;i++) ncl_get_scalar_value(Tfthk[i],&thk[i]);
			if (thk[0] != 0. || thk[1] != 0. || thk[2] != 0. || thk[3] != 0.)
			{
				ncl_add_token(&cmdbuf,NCL_finish,NCL_comma);
				for (i=0;i<4;i++)
				{
					nc = strlen(Tfthk[i]);
					ul_strip_blanks(Tfthk[i],&nc);
					ncl_add_token(&cmdbuf,Tfthk[i],NCL_comma);
				}
			}
		}
/*
......add near point
*/
	if (strlen(Tnpt)>0)
	{
		ncl_add_token(&cmdbuf,"NEARPT",NCL_comma);
		ncl_add_token(&cmdbuf,Tnpt,NCL_comma);
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
	NCL_preview_mot = 1;
	return(UU_SUCCESS);
}
/*
C       ISC10(2) MOTION-TYPE- CUTTING MODE: 1=LACE, 2=BACK AND FORTH.
C      ? SC(16)   CLEARANCE-PLANE/DIST - CLEARANCE PLANE OR DISTANCE
C                             CAN AND MUST ONLY BE SPECIFIED FOR LACE MOTION-TYPE.
C       SC(17)   PLUNGE-DIST - THE PLUNGE DIST 
C                             CAN AND MUST ONLY BE SPECIFIED FOR LACE MOTION-TYPE.
C       ISC10(3) STEPOVER-MODE - TYPE OF STEPOVER BETWEEN PASSES
C                                 1 = FIXED STEPOVER DISTANCE
C                                 2 = STEPOVER BASED UPON SCALLOP HEIGHT 
C       SC(18)   STEPOVER-DIST - SPECIFIES EITHER STEP-OVER DIST OR SCALLOP
C                             HEIGHT BASED UPON STEP-OVER MODE.
C       RSC19(1) GENERAL-FEDRAT - GENERAL MACHINING FEED RATE.
C       RSC19(2) POSITION-FEDRAT - POSITIONING FEEDRATE.
C       RSC19(3) RAMPING-FEDRAT - RAMPING FEEDRATE.
C       SC(21)   RETRACT-PLANE/POINT/DIST - RETRACT PLANE, POINT OR DISTANCE
C   
*/
void nclf_save_rmill_values(mot_typ, stp_mod, fclr, clr_dist, clr_label, plun_dist, 
             stp_dist, feedrate, pos_fedr, ramp_fedr, fret, ret_val, ret_label,
			 rghthk, fin, rthk)
UM_int2 *mot_typ, *stp_mod, *rghthk, *fin, *fret, *fclr;
UM_real8 *clr_dist, *plun_dist, *stp_dist, rthk[8];
UM_real4 *feedrate, *pos_fedr, *ramp_fedr, *ret_val;
UM_f77_str clr_label, ret_label;
{
	int i;
	S_rmill_vals.fclr = *fclr;
	S_rmill_vals.fret = *fret;
	S_rmill_vals.ret_val = *ret_val;
	strncpy(S_rmill_vals.ret_label, ret_label,64);
	S_rmill_vals.ret_label[64] = '\0';
	strncpy(S_rmill_vals.clr_label, clr_label,64);
	S_rmill_vals.clr_label[64] = '\0';
	if (*mot_typ>0)
	{
		S_rmill_vals.fpas = 0;
		S_rmill_vals.mot_typ = *mot_typ;
	}	
	else
	{
		S_rmill_vals.fpas = 1;
		S_rmill_vals.mot_typ = (-1)*(*mot_typ);
	}
	if (*rghthk)
		S_rmill_vals.rghthk = 1;
	else
		S_rmill_vals.rghthk = 0;
	if (*fin)
		S_rmill_vals.fin = 1;
	else
		S_rmill_vals.fin = 0;
	S_rmill_vals.stp_mod = *stp_mod;
	S_rmill_vals.clr_dist = *clr_dist;
	S_rmill_vals.plun_dist = *plun_dist;
	S_rmill_vals.stp_dist = *stp_dist;
	S_rmill_vals.feedrate = *feedrate;
	S_rmill_vals.pos_fedr= *pos_fedr;
	S_rmill_vals.ramp_fedr = *ramp_fedr;
	for (i=0;i<8;i++)
		S_rmill_vals.rthk[i] = rthk[i];
	S_rmill_set = 1;
}

void ncl_get_rmill_values(mot_typ, stp_mod, fpas, fclr,clr_dist, clr_label, plun_dist, 
             stp_dist, feedrate, pos_fedr, ramp_fedr, fret, ret_val, ret_label,
			 rghthk, fin, rthk)
UM_int2 *mot_typ, *stp_mod, *fpas, *rghthk, *fin, *fret, *fclr;
UM_real8 *clr_dist, *plun_dist, *stp_dist, rthk[8];
UM_real4 *feedrate, *pos_fedr, *ramp_fedr, *ret_val;
char *ret_label, *clr_label;
{
	int i;
	if (S_rmill_set)
	{
		*fpas = S_rmill_vals.fpas;
		*fclr = S_rmill_vals.fclr;
		*rghthk = S_rmill_vals.rghthk;
		*fin = S_rmill_vals.fin;
		*mot_typ = S_rmill_vals.mot_typ;
		*stp_mod = S_rmill_vals.stp_mod;
		*clr_dist = S_rmill_vals.clr_dist;
		*plun_dist = S_rmill_vals.plun_dist;
		*stp_dist = S_rmill_vals.stp_dist;
		*feedrate = S_rmill_vals.feedrate;
		*pos_fedr = S_rmill_vals.pos_fedr;
		*ramp_fedr = S_rmill_vals.ramp_fedr;
		for (i=0;i<8;i++)
			rthk[i] = S_rmill_vals.rthk[i];
		*fret = S_rmill_vals.fret;
		*ret_val = S_rmill_vals.ret_val;
		strcpy(ret_label, S_rmill_vals.ret_label);
		strcpy(clr_label, S_rmill_vals.clr_label);
	}
	else
	{
		ret_label[0] = '\0';
		clr_label[0] = '\0';
		*fclr = 0;
		*fpas = 0;
		*rghthk = 0;
		*fin = 0;
		*mot_typ = 1;
		*stp_mod = 1;
		*clr_dist = 0.0;
		*plun_dist = 0.0;
		*stp_dist = 0.0;
		*feedrate = 0.0;
		*pos_fedr = 0.0;
		*ramp_fedr = 0.0;
		for (i=0;i<8;i++)
			rthk[i] = 0.0;
		*fret = 0;
		*ret_val = 0;
	}
}
