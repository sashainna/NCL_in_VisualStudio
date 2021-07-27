/*********************************************************************
**    NAME         :  nupmill.c
**       CONTAINS: User interface routines for port surfaces milling.
**
**       nclu_pmill()
**
**    COPYRIGHT 2019 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       
**    DATE AND TIME OF LAST MODIFICATION
**       
*********************************************************************/

#include "usysdef.h"
#include "wsgl.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "class.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mxxx.h"
#include "modef.h"
#include "mdcoord.h"
#include "mdcpln.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdeval.h"
#include "udforms.h"

#include "nccs.h"
#include "ncl.h"
#include "nclcmd.h"
#include "nclfc.h"
#include "nclinp.h"
#include "nclx.h"
#include "nkeywd.h"
#include "nclmodals.h"
#include "nclvx.h"
#include "driver.h"
#include "mgeom.h"
#include "lcom.h"
#include "nclmplay.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"

#define MXLAB NCL_MAX_LABEL+1
#define EDGTO 0
#define EDGPAST 1
#define EDGON 2
#define CONTCT 3
/*
.....PMill
*/
#define MILLRG1		0
#define MILLRG2		1
#define MILLRG3		2
/*
.....Motion Type
*/
#define MOTIONTRG1		3 //type choice
#define MOTIONTRG2		4 //rad text input
#define MOTIONTRG3		5 //orientation choice
#define MOTIONTRG4		6 //step text input
#define MOTIONTRG5		7 //Tolerance choice
#define MOTIONTRG6		8 //Tolerance text input

#define MOTIONTRG8		9 //sf1 text input
#define MOTIONTRG9		10 //Start button
#define MOTIONTRG10		12 //sf2 text input
#define MOTIONTRG11		13 //End button
#define MOTIONTRG12		15 // Retract choice
#define MOTIONTRG13		16 // Retract
#define MOTIONTRG14		17 // Retract
#define MOTIONTRG15		18 // Fedrat choice
#define MOTIONTRG16		19 // Fedrat
#define MOTIONTRG17		11 //sf1 select button
#define MOTIONTRG18		14 //sf2 select button
/*
.....Entry /Exit
*/
#define ENEXTRG1	20
#define ENEXTRG2	21
#define ENEXTRG3	22
/*
.....Colors
*/
#define CLRRG1	23
#define CLRRG2	24
#define CLRRG3	25
#define CLRRG6	26
#define CLRRG7	27
#define CLRRG8	28
/*
.....Action Buttons
*/
#define FAPV	29
#define FAPY	30
#define FARS	31
#define FAPB	32
#define FAVE	33
#define FAGE	34
#define FAVW	35
#define FVIDEO	36


#define HIDE_KEY 0
#define CONTOUR_KEY 1
extern int NCL_preview_mot;
/*
.....Section definitions
*/
enum {PMill, MotionT, Boundary, Entry, Colors};
static char *Smode[]={"PMill", "Motion Type", "Boundaries", "Entry / Exit", "Colors"};
static int Sred[3]={180,0,0}, Syellow[3]={180,148,0}, Sgreen[3]={0,180,0};
static int Sblack[3]={0,0,0};
static int *Ssecol[]={Sblack,Sblack,Sblack,Sblack,Sblack,Sblack};
static int Sacc[6];
static char Sfeed_valuestr[80], Sav_valuestr[80], Sav_valuestr1[80],Sav_valuestr2[80];
static int Ssav_fchc, Ssav_fchc1, Ssav_fchc2;

extern int NAUTIPV;
extern UD_METHOD UD_initfrm_intry;

static UU_LIST Skey_list;
static int Snkeys;
static UU_LOGICAL Skey_init;
static int Sfrm;
static UU_LOGICAL Sactive = UU_FALSE;

static UU_LIST Ssurf;
static int Snsurf = 0;
static UU_LIST Sbnd;
static int Snbnd = 0;
static int Scolor;
static int Tcolor;
static int Tstptclr, Tdirclr,Tpassclr,
	Sstptclr, Sdirclr, Spassclr, Spassclr;
static int Tgeotog, Tgeocol, Sgeotog, Sgeocol;

static UU_LOGICAL Sgeo_init;
static UU_LOGICAL Sgeo_redraw;

static UM_sgeo Sgsf,Sgspt, Sgrap, Sgret, SgLrap, Sgdrv1, Sgdrv2, SgBpl1,SgBpl2;

static UU_LOGICAL Spmfrm_init = UU_FALSE;

static char Sptnam[MXLAB];
static char SLraptoval[MXLAB];

static char Smethod_str[MXLAB];
static char Sorient_str[MXLAB];

static char Tptnam[MXLAB];
static char TLraptoval[MXLAB];

static char Tmethod_str[MXLAB];
static char Torient_str[MXLAB];

static int Sstarttyp=0, Stoltyp=0;
static int Smethod=0, SLraptotyp=0, SLraptofed=0;
static UU_REAL Sstp=0.0, Stoler=0.0;
static UU_REAL Trad=0.0, Srad=0.0;
static int Tstarttyp=0, Ttoltyp=0;
static int Tmethod=0, Torient=0,    TLraptotyp=0, TLraptofed=0;

static UN_motseg *Smptr=0,Smotatt;
static UN_mot_vpbox_struc Smvpbox[UV_NVPORTS];

static UU_LOGICAL loaded = UU_FALSE;

static UU_LOGICAL firstSelected = UU_FALSE;

static char Sstp_str[65]="0.0", Srad_str[65]="0.0",
		Stoler_str[65]="0.0",
		SLraptofrv_str[65]="0.0";
static char Tstp_str[65]="0.0", Trad_str[65]="0.0",
		Ttoler_str[65]="0.0",
		TLraptofrv_str[65]="0.0", Tcmd_str[8][65];

//static char Sdirval1[65]="-1.0";
//static char Tdirval1[65]="-1.0";
//
//static char Sdirval2[65]="-1.0";
//static char Tdirval2[65]="-1.0";

//char Sdirval1[65]="-1.0";
//char Tdirval1[65]="-1.0";
//
//char Sdirval2[65]="-1.0";
//char Tdirval2[65]="-1.0";

static int SfrmPlay=0;
static int Serrflg,Spreviewflg,Schgmade,Smenu_ch[14];
static UD_FSTAT OnColors(), OnAction(), OnVideo();
static UD_FSTAT OnPText(), OnPButton();
static UD_FSTAT OnMChcPick(), OnMText(), OnMButton();
static UD_FSTAT OnBChcPick(), OnBChkPick();
static UD_FSTAT OnEChcPick(), OnEText(), OnEButton();
static void S_unhilite_all(), S_hilite_entity(), S_hilite_entities(),
		S_init_form(), S_create_key_list(), S_add_key(), S_section_changed(),
		S_form_invis(), S_update_answers(), S_unhilite_entities();
static int S_build_command();
static char Sselsrf[NCL_MAX_LABEL_AND_SUBSCRIPT] = "", Tselsrf[NCL_MAX_LABEL_AND_SUBSCRIPT] = ""; 
static char Sdirval1[NCL_MAX_LABEL_AND_SUBSCRIPT] = "-1.0", Tdirval1[NCL_MAX_LABEL_AND_SUBSCRIPT] = "-1.0"; 
static char Sdirval2[NCL_MAX_LABEL_AND_SUBSCRIPT] = "-1.0", Tdirval2[NCL_MAX_LABEL_AND_SUBSCRIPT] = "-1.0"; 



int point_coordinates();
int nclu_pl_from_crv(char*);
int nclu_pl_on_crv();
int nclu_pt_from_crv(char*);
int nclu_pt_on_crv_start(char*, char*);
int nclu_pt_on_crv_end(char*, char*);
int create_point();

static UU_REAL x,y,z,min_x,min_y,min_z,max_x, max_y,max_z;
static	unsigned UU_LONG key_max, key_min,key_select;

static	UM_coord ndc_min, ndc_max;

UU_REAL uval1 = 0;
UU_REAL uval2 = 0;

static int *Sanswers[] = {
/*
.....PMill  3
*/
		&Tselsrf, UU_NULL, UU_NULL,
/*
......Motion Type 16
*/
		&Tmethod, &Trad, &Torient, (int *)&Tstp_str, 
		&Ttoltyp,(int *)&Ttoler_str,
		/*(int *)&Tdirval1,  UU_NULL, UU_NULL, 
		(int *)&Tdirval2,  UU_NULL, UU_NULL, */
		&Tdirval1,  UU_NULL, UU_NULL, 
		&Tdirval2,  UU_NULL, UU_NULL, 
		&TLraptotyp, (int *)&TLraptoval,  UU_NULL,
		/*&Tdirval1,  UU_NULL, UU_NULL, 
		&Tdirval2,  UU_NULL, UU_NULL, */
		&TLraptofed, (int *)&TLraptofrv_str,
/*
.....Entry/Exit
*/
		&Tstarttyp, (int *)&Tptnam, UU_NULL, 	
/*
.....Colors
*/
		&Tcolor, &Tstptclr, &Tdirclr,
		&Tpassclr, &Tgeotog, &Tgeocol,
		UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,UU_NULL
	};
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
	int nc, nc1, nc2, nc3, nc5, nc6, nc7, nc8;
	char label[2*65];
	UU_REAL rval;
	UU_LOGICAL sect11, sect2, sect20, sect21, sect22, sect23, sect24, sect25, sect26,
		sect41, sect42, sect43;
	UU_LOGICAL ifl;
	char buf[10];
/*
......PMill
*/
	Snsurf = UU_LIST_LENGTH(&Ssurf);
	if (Snsurf<=0)
		sect11 = UU_FALSE;
	else
		sect11 = UU_TRUE;
	if (sect11)
	{
		ud_dispfrm_set_attribs(0,MILLRG1,UM_BLACK,UM_WHITE);
		ud_dispfrm_set_attribs(0,MILLRG2,UM_BLACK,Tcolor);
	}
	else
	{
		ud_dispfrm_set_attribs(0,MILLRG1,UM_WHITE,UM_RED);
		ud_dispfrm_set_attribs(0,MILLRG2,UM_WHITE,UM_RED);
	}
/*
.......motion type
*/
	sect2 = UU_TRUE;
	sect20 = UU_TRUE;
	if (Tmethod==0)
	{
		strcpy(Tmethod_str,NCL_linear);
		if (Torient==0)
			strcpy(Torient_str,"CLW");
		else if (Torient==1)
			strcpy(Torient_str,"CCLW");
	}
	if (Tmethod==1)
	{
		strcpy(Tmethod_str,NCL_helix);
		if (Torient==0)
			strcpy(Torient_str,"CLW");
		else if (Torient==1)
			strcpy(Torient_str,"CCLW");
	}
	
	if (Tmethod==2)
	{
		strcpy(Tmethod_str,NCL_arc);
		nc = (int)strlen(Trad_str);
		ul_strip_blanks(Trad_str, &nc);
		if (nc<0)
			sect20 = UU_FALSE;
		rval = atof(Trad_str);
		if (rval<=0)
			sect20 =  UU_FALSE;
		if (sect20) 
			ud_dispfrm_set_attribs(0, MOTIONTRG2, UM_BLACK, UM_WHITE);
		else 
		{
			ud_dispfrm_set_attribs(0, MOTIONTRG2,UM_WHITE,UM_RED);
		}
	}
	sect21 = UU_TRUE;
	nc = (int)strlen(Tstp_str);
	ul_strip_blanks(Tstp_str, &nc);
	if (nc<0)
		sect21 = UU_FALSE;
	rval = atof(Sstp_str);
	if (rval<=0)
		sect21 =  UU_FALSE;
	if (sect21) 
		ud_dispfrm_set_attribs(0, MOTIONTRG4, UM_BLACK, UM_WHITE);
	else 
	{
		ud_dispfrm_set_attribs(0, MOTIONTRG4,UM_WHITE,UM_RED);
	}
	sect22 = UU_TRUE;
	nc = (int)strlen(Ttoler_str);
	ul_strip_blanks(Ttoler_str, &nc);
	if (nc<=0)
	{
		ud_dispfrm_set_attribs(0,MOTIONTRG6,UM_WHITE,UM_RED);
		sect22 = UU_FALSE;
	}
	else
	{
		if (Ttoltyp!=0)
		{
			rval = atof(Ttoler_str);
			if (rval<0)
				sect22 =  UU_FALSE;
			if (sect22) 
				ud_dispfrm_set_attribs(0, MOTIONTRG6, UM_BLACK, UM_WHITE);
			else 
			{
				ud_dispfrm_set_attribs(0, MOTIONTRG6,UM_WHITE,UM_RED);
			}
		}
		else
			ud_dispfrm_set_attribs(0, MOTIONTRG6, UM_BLACK, UM_WHITE);
	}
	sect23 = UU_TRUE;
	nc = (int)strlen(Tdirval1);
	ul_strip_blanks(Tdirval1, &nc);
	if (nc<=0)
	{
		ud_dispfrm_set_attribs(0,MOTIONTRG8,UM_WHITE,UM_WHITE);
		ud_dispfrm_set_attribs(0,MOTIONTRG9,UM_WHITE,UM_GREEN);
		ud_dispfrm_set_attribs(0,MOTIONTRG17,UM_WHITE,UM_RED);
		sect23 = UU_FALSE;
	}
	else
	{
		ud_dispfrm_set_attribs(0, MOTIONTRG8, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(0,MOTIONTRG9, UM_BLACK, UM_GREEN);
		ud_dispfrm_set_attribs(0,MOTIONTRG17,UM_WHITE,UM_RED);
		sect23 = UU_TRUE;
	}
	/*strcpy(label, Tdirval1);
	ul_to_upper(label);
	if (!strcmp(label,"PL1"))
		ud_dispfrm_set_attribs(0,MOTIONTRG17,UM_WHITE,UM_RED);*/

	sect24 = UU_TRUE;
	//if (Tdir==0)
	{
		nc = (int)strlen(Tdirval2);
		ul_strip_blanks(Tdirval2, &nc);
		if (nc<=0)
		{
			ud_dispfrm_set_attribs(0,MOTIONTRG10,UM_WHITE,UM_WHITE);
			ud_dispfrm_set_attribs(0,MOTIONTRG11,UM_WHITE,UM_GREEN);
			ud_dispfrm_set_attribs(0,MOTIONTRG18,UM_WHITE,UM_RED);
			sect24 = UU_FALSE;
		}
		else
		{
			ud_dispfrm_set_attribs(0, MOTIONTRG10, UM_BLACK, UM_WHITE);
			ud_dispfrm_set_attribs(0,MOTIONTRG11, UM_BLACK, UM_GREEN);
			ud_dispfrm_set_attribs(0,MOTIONTRG18,UM_WHITE,UM_RED);
			sect24 = UU_TRUE;
		}
		/*strcpy(label, Tdirval2);
		ul_to_upper(label);
		if (!strcmp(label,"PL2"))
		ud_dispfrm_set_attribs(0,MOTIONTRG18,UM_WHITE,UM_RED);*/
	}
 	sect25 = UU_TRUE;
	sect26 = UU_TRUE;
	if (TLraptotyp==0)
	{
		ud_dispfrm_set_attribs(0, MOTIONTRG14, UM_BLACK, Tpassclr);
		ud_dispfrm_set_attribs(0, MOTIONTRG13, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(0, MOTIONTRG16,UM_BLACK, UM_WHITE);
	}
	else 
	{
		nc = (int)strlen(TLraptoval);
		ul_strip_blanks(TLraptoval, &nc);
		if (nc<=0)
		{
			ud_dispfrm_set_attribs(0,MOTIONTRG13,UM_WHITE,UM_RED);
			if (TLraptotyp==2)
				ud_dispfrm_set_attribs(0,MOTIONTRG14,UM_WHITE,UM_RED);
			else
				ud_dispfrm_set_attribs(0, MOTIONTRG14, UM_BLACK, Tpassclr);
			sect25 = UU_FALSE;
		}
		else
		{
			if (TLraptotyp==1)
			{
				rval = atof(TLraptoval);
				if (rval<=0)
					sect25 =  UU_FALSE;
				if (sect25) 
				{
					ud_dispfrm_set_attribs(0, MOTIONTRG14, UM_BLACK, Tpassclr);
					ud_dispfrm_set_attribs(0, MOTIONTRG13, UM_BLACK, UM_WHITE);
				}
				else 
				{
					ud_dispfrm_set_attribs(0, MOTIONTRG13, UM_WHITE,UM_RED);
				}
			}
			else
			{
				ud_dispfrm_set_attribs(0, MOTIONTRG14, UM_BLACK, Tpassclr);
				ud_dispfrm_set_attribs(0, MOTIONTRG13, UM_BLACK, UM_WHITE);
			}
		}
		if (TLraptofed==2)
		{
			nc = (int)strlen(TLraptofrv_str);
			ul_strip_blanks(TLraptofrv_str, &nc);
			if (nc<=0)
			{
				ud_dispfrm_set_attribs(0,MOTIONTRG16,UM_WHITE,UM_RED);
				sect26 = UU_FALSE;
			}
			else
			{
				rval = atof(TLraptofrv_str);
				if (rval<=0)
					sect26 =  UU_FALSE;
				if (sect26) 
					ud_dispfrm_set_attribs(0, MOTIONTRG16, UM_BLACK, UM_WHITE);
				else 
				{
					ud_dispfrm_set_attribs(0, MOTIONTRG16,UM_WHITE,UM_RED);
				}
			}
		}
		else
			ud_dispfrm_set_attribs(0, MOTIONTRG16, UM_BLACK, UM_WHITE);
	}
/*
......Entry /Exit
*/
	sect41 = UU_TRUE;
	sect42 = UU_TRUE;
	sect43 = UU_TRUE;
	if (Tstarttyp==1)
	{
		nc = (int)strlen(Tptnam);
		ul_strip_blanks(Tptnam, &nc);
		if (nc<=0)
		{
			ud_dispfrm_set_attribs(0,ENEXTRG3, UM_WHITE, UM_RED);
			ud_dispfrm_set_attribs(0, ENEXTRG2, UM_WHITE, UM_RED);
			sect41 = UU_FALSE;
		}
		else
		{
			ud_dispfrm_set_attribs(0,ENEXTRG3,UM_BLACK,Tstptclr);
			ud_dispfrm_set_attribs(0, ENEXTRG2, UM_BLACK, UM_WHITE);
			sect41 = UU_TRUE;
		}
	}
	else
	{
		ud_dispfrm_set_attribs(0, ENEXTRG2, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(0, ENEXTRG3, UM_BLACK, Tstptclr);
	}
/*
.....Set section color
*/
	if (sect11)
	{
		if (Sacc[PMill]==0)
		{
			Ssecol[PMill] = Sblack;
			S_section_changed(PMill,UU_FALSE);
		}
		else
		{	
			Ssecol[PMill] = Sgreen; 
			S_section_changed(PMill,UU_TRUE);
		}
	}
	else
	{
		Ssecol[PMill] = Sred; 
		S_section_changed(PMill,UU_FALSE);
	}
	if ((sect2)&&(sect20)&&(sect21)&&(sect22)
		&&(sect23)&&(sect24)&&(sect25)&&(sect26))
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
		Ssecol[MotionT] = Sred; 
		S_section_changed(MotionT,UU_FALSE);
	}
		
	if (Sacc[Boundary]==0)
	{
		Ssecol[Boundary] = Sblack;
		S_section_changed(Boundary,UU_FALSE);
	}
	else
	{
		Ssecol[Boundary] = Sgreen; 
		S_section_changed(Boundary,UU_TRUE);
	}

	if ((sect41)&&(sect42)&&(sect43))
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
	if (Sacc[Colors]==0)
	{
		Ssecol[Colors] = Sblack;
		S_section_changed(Colors,UU_FALSE);
	}
	else
	{
		Ssecol[Colors] = Sgreen; 
		S_section_changed(Colors,UU_TRUE);
	}
/*
.....Set Action Buttons
*/
	ifl = (sect11) && (sect2)&&(sect21)&&(sect22)&&(sect23)&&(sect24)&&(sect25)&&(sect26);
	ifl = ifl && (sect41)&&(sect42)&&(sect43);
	ud_frm_enable_ok(ifl);
	ud_set_traverse_mask(FAPV,ifl);
	ud_set_traverse_mask(FAPY,ifl);

	ifl = Sacc[PMill] + Sacc[MotionT] + Sacc[Boundary] + Sacc[Entry] + Sacc[Colors];
	ud_set_traverse_mask(FARS,ifl);
/*
.....if geom entered
*/
	nc8 = UU_LIST_LENGTH(&Ssurf);
	ul_to_upper(Tdirval1);
	strcpy(label, Tdirval1);
	nc1 = (int)strlen(label);
	ul_strip_blanks(label,&nc1);
	nc2 = 0;
	//if (Tdir==0)
	{
		ul_to_upper(Tdirval2);
		strcpy(label, Tdirval2);
		nc2 = (int)strlen(label);
		ul_strip_blanks(label,&nc2);
	}
	nc3 = 0;
	if (TLraptotyp==2)
	{
		ul_to_upper(SLraptoval);
		strcpy(label, SLraptoval);
		nc3 = (int)strlen(label);
		ul_strip_blanks(label,&nc3);
	}
	nc5 = 0;
	if (Tstarttyp==1)
	{
		ul_to_upper(Tptnam);
		strcpy(label, Tptnam);
		nc5 = (int)strlen(label);
		ul_strip_blanks(label,&nc5);
	}
	nc6 = 0;
	nc7 = 0;
	if ((nc1>0)||(nc2>0)||(nc3>0)||(nc5>0)
		||(nc6>0)||(nc7>0)||(nc8>0))
	{
		ud_set_traverse_mask(FAGE,1);
	}
	else
		ud_set_traverse_mask(FAGE,0);

	if (sect11)
		ud_set_traverse_mask(MILLRG3, UU_TRUE);
	else
		ud_set_traverse_mask(MILLRG3, UU_FALSE);

	return UU_TRUE;
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
**    I_FUNCTION     :  S_hilite_entities(sflst,color)
**       Highlights the selected geometry entities.
**    PARAMETERS
**       INPUT  :
**          sflst    Pointer to list of selected geometry structures.
**          color    Color to highlight entities.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_hilite_entities(sflst,color)
UU_LIST *sflst;
{
	int i;
	UM_sgeo *geo;
/*
.....Loop through list
*/
	geo = (UM_sgeo *)UU_LIST_ARRAY(sflst);
	for (i=0;i<UU_LIST_LENGTH(sflst);i++) S_hilite_entity(&geo[i],color);
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
/*
.....Save main form settings
*/
	Scolor = Tcolor;
	Sstptclr = Tstptclr;
	Sdirclr = Tdirclr;
	Spassclr = Tpassclr;
	strcpy(Sselsrf, Tselsrf);
	strcpy(Sdirval1, Tdirval1);
	strcpy(Sdirval2, Tdirval2);
	SLraptotyp = TLraptotyp;
	Smethod = Tmethod;
	strcpy(Srad_str, Trad_str);
	strcpy(Sstp_str, Tstp_str);
	
	strcpy(SLraptoval, TLraptoval);
	if ((TLraptotyp!=0)&&(TLraptofed==2))
	{
		strcpy(SLraptofrv_str, TLraptofrv_str);
	}
	Sstarttyp = Tstarttyp;
	strcpy(Sptnam, Tptnam);
/*
.....Save geometry colors
*/
	UN_unused_geo_flag = Tgeotog;
	UN_unused_geo_color = Tgeocol;
	nclu_save_preview_modals();
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
		Ssecol[PMill] = Sred;

		ud_frm_enable_ok(UU_FALSE);
		if (*fieldno != -1)
		{
			for (i=PMill;i<=Colors;i++)
				S_section_changed(i,UU_FALSE);
		}
		ud_frm_enable_ok(UU_FALSE);
	}
	else
		S_enable_buttons();
/*
.....Set the mandatory fields to red
*/
		ud_dispfrm_set_attribs(0, MILLRG2, UM_WHITE,UM_RED);
/*
.....Set Pick buttons to correct color
*/
	ud_dispfrm_set_attribs(0,CLRRG1,UM_BLACK,Tcolor);
	ud_dispfrm_set_attribs(0,CLRRG2,UM_BLACK,Tstptclr);
	ud_dispfrm_set_attribs(0,CLRRG3,UM_BLACK,Tdirclr);
	ud_dispfrm_set_attribs(0,CLRRG6,UM_BLACK,Tpassclr);
	ud_dispfrm_set_attribs(0,CLRRG8,UM_BLACK,Tgeocol);
	S_enable_buttons();
	return(UD_FLDOK);
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
	S_unhilite_all();
/*
.....Initialize geomtry variables
*/
	Sgspt.key = Sgrap.key = Sgret.key = SgLrap.key = 0;
	Sgdrv1.key = Sgdrv2.key = 0;
	Sgspt.color = Sgrap.color = Sgret.color = SgLrap.color = -1;
	Sgdrv1.color = Sgdrv2.color = -1;
	Sgspt.label[0] = Sgrap.label[0] = Sgret.label[0] = SgLrap.label[0] = '\0';
	Sgdrv1.label[0] = Sgdrv2.label[0] = '\0';
	/*Tdirval1[0] = '\0';
	Tdirval2[0] = '\0';*/
	ud_update_answer(MOTIONTRG8,(int *)Tdirval1);
	ud_update_answer(MOTIONTRG10,(int *)Tdirval2);
	/*ud_update_answer(MOTIONTRG8,Tdirval1);
	ud_update_answer(MOTIONTRG10,Tdirval2);*/
	if (TLraptotyp==2)
	{
		TLraptoval[0] = '\0';			
		ud_update_answer(MOTIONTRG13,(int *)TLraptoval);
	}
	if (Tstarttyp==1)
	{
		Tptnam[0] = '\0';
		ud_update_answer(ENEXTRG2,(int *)Tptnam);
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
	S_unhilite_entities(&Ssurf);
	Snsurf = 0; 
	S_unhilite_entities(&Sbnd);
	Snbnd = 0; 
	S_unhilite_entity(&Sgspt);
	S_unhilite_entity(&Sgrap);
	S_unhilite_entity(&Sgret);
	S_unhilite_entity(&SgLrap);
	S_unhilite_entity(&Sgdrv1);
	S_unhilite_entity(&Sgdrv2);
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
	NCL_cmdbuf cmdbuf;

	switch (*fieldno)
	{
/*
.....Preview command
*/
	case FAPV:
/*
.....Reset motion settings if preview was already pressed
*/
		if (Spreviewflg) moinfr(&flag);
		Spreviewflg = UU_FALSE;
		Schgmade = UU_FALSE;
		Serrflg = 0;
/*
.....Erase last previewed motion
*/
		if (Smptr != UU_NULL) ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
		Smptr = UU_NULL;
		moinfs();
		ncl_mark_mdisplay(&Smptr,&Smotatt,Smvpbox,UU_TRUE);
		status = S_build_command(UU_FALSE);
		if (status != UU_SUCCESS) 
			moinfr(&flag);
		else
		{
			Spreviewflg = UU_TRUE;
			ud_set_traverse_mask(FAPB,1);
			ud_set_traverse_mask(FAVE,NAUTIPV);
		}
		ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf,"PT2", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "=", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "POINT/", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "X+0.25", NCL_comma);
		status = ncl_add_token(&cmdbuf, "Y-0.5", NCL_comma);
		status = ncl_add_token(&cmdbuf, "Z", NCL_nocomma);
		
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		ncl_init_cmdbuf(&cmdbuf);
		break;
/*
.....APPLY
.....A curve must be selected
*/
	case FAPY:
/*
........Output command
*/
/*
.....Modified by KC, used the same method as shown in the ngoto.c modified by Eduard
*/
		if (Spreviewflg)
		{
			if (!Schgmade && Serrflg == 0) flag = UU_TRUE;
			else flag = UU_FALSE;
			moinfr(&flag);
		}
/*
.....End modification by KC
*/
		status = S_build_command(UU_TRUE);
		ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf,"PT2", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "=", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "POINT/", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "X+0.25", NCL_comma);
		status = ncl_add_token(&cmdbuf, "Y-0.5", NCL_comma);
		status = ncl_add_token(&cmdbuf, "Z", NCL_nocomma);
		
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		ncl_init_cmdbuf(&cmdbuf);
		
		/*status = ncl_add_token(&cmdbuf,"goto/", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "X+3", NCL_comma);
		status = ncl_add_token(&cmdbuf, "Y-0.5", NCL_comma);
		status = ncl_add_token(&cmdbuf, "Z", NCL_nocomma);*/
		
		status = ncl_add_token(&cmdbuf,"goto/", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "PT2", NCL_comma);
		
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		Spreviewflg = UU_FALSE;
		Smptr = UU_NULL;
		if (status != UU_SUCCESS) 
		{
			break;
		}
		S_deselect_all();
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
		if (Spreviewflg) moinfr(&flag);
		S_init_form();
		S_enter_form(fieldno,val,stat);
		for (i=FAPV;i<FAVW;i++) ud_set_traverse_mask(i,0);
		S_update_answers();
		Schgmade = Spreviewflg = UU_FALSE;
		Sacc[PMill] = Sacc[MotionT] = Sacc[Boundary] = Sacc[Entry] = Sacc[Colors] = 0;
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
**    I_FUNCTION     : OnColors(fieldno, val, stat)
**			Color change callback.  Changes the color of all entities.
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
static UD_FSTAT OnColors(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int fno;

	ud_default_method(fieldno, val, stat);
	switch (*fieldno)
	{
	case CLRRG1:
		S_hilite_entities(Ssurf,Tcolor);
		break;
	case CLRRG2:
		S_hilite_entity(&Sgspt,Tstptclr);
		break;
	case CLRRG3:
		S_hilite_entity(&Sgdrv1,Tdirclr);
		S_hilite_entity(&Sgdrv2,Tdirclr);
		break;
	case CLRRG6:
		S_hilite_entity(&SgLrap,Tpassclr);
		break;
	case CLRRG7:
		Tgeotog = val->frmint[0];
	case CLRRG8:
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
	return (UD_FLDOK);
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
static UD_FSTAT OnPText(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch (*fieldno)
	{
		case MILLRG1:
			break;
	}
	Sacc[PMill] = 1;
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     : OnDeselect(fieldno, val, stat)
**       Method called when the DeSelect ALL button is pushed.
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
static UD_FSTAT OnDeselect(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Check for change since preview
*/
	Snsurf = UU_LIST_LENGTH(&Ssurf);
	if (Snsurf > 0)
	{
		S_unhilite_entities(&Ssurf);
		Snsurf = 0;
		if (Spreviewflg) Schgmade = 1;
	}
	ud_set_traverse_mask(MILLRG3, UU_FALSE);
	Tselsrf[0] = '\0';
	ud_update_answer(MILLRG1,&Tselsrf);
	return (UD_FLDOK);
}

static UD_FSTAT OnPButton(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int pr, *mask;
	int j1 = 0;
	int status;

	if (loaded)
	{
		S_deselect_all();
		/*strcpy(Sdirval1, "-1.0");
		strcpy(Sdirval2, "-1.0");

		strcpy(Tdirval1, "-1.0");
		strcpy(Tdirval2, "-1.0");*/

		ncl_hide_geo(-1,-1,-1,-1,UU_TRUE,UU_NULL,0);

		ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
	}
/*
.....Set the appropriate selection mask and prompt
*/
	if (*fieldno == MILLRG2)
	{
		mask = (int *)UD_ncl_allsfsh;
		pr = 508;

		status = S_select_geo(&Sgsf,&Ssurf,mask,0,pr,Tcolor,0,MILLRG1,Tselsrf/*, &uval*/);

		if (!strcmp(Sgsf.label, "") )
		{
			
			uw_ntdispmsg("No port in scene");
			return(UD_BADACT);
		}

		//@@@@@@@@@@@@@@@

		status = key_of_label("CV3", status);
		if (status<=0)
			status = nclu_cv_on_srf(Sgsf.label, "CV3", 3, 0.5, 50);
		status = key_of_label("PT1", status);
		if (status<=0)
			status = nclu_pt_on_crv_start("PT1","CV3");
		/*status = key_of_label("PT3", status);
		if (status<=0)
			status = nclu_pt_on_crv_start("PT3","CV4");
		status = key_of_label("PT4", status);
		if (status<=0)
			status = nclu_pt_on_crv_end("PT4","CV4");*/
		status = key_of_label("PT3", status);
		if (status<=0)
			status = nclu_pt_on_crv_end("PT3","CV3");
		//@@@@@@@@@@@@

		Snsurf = UU_LIST_LENGTH(&Ssurf);
		if (Snsurf>0)
			ud_set_traverse_mask(MILLRG3, UU_TRUE);
		loaded = UU_TRUE;
	}
	else if (*fieldno == MILLRG3)
	{
		OnDeselect(fieldno, val, stat);
		nclu_cv_from_srf("CV3");
		ud_set_traverse_mask(MILLRG3, UU_FALSE);
		loaded = UU_FALSE;
	}
	Sacc[PMill] = 1;
	S_enable_buttons();
	return(UD_FLDOK);
}

static UD_FSTAT OnMChcPick(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{

	char valuestr[80];
	switch (*fieldno)
	{
	case MOTIONTRG1:
//why remove? it will never active the last few line in Motion page. Yurong
		if (*(val->frmint) != 2)
		{
			ud_set_traverse_mask(MOTIONTRG12,UU_FALSE);
			ud_set_traverse_mask(MOTIONTRG13,UU_FALSE);
			ud_set_traverse_mask(MOTIONTRG14,UU_FALSE);
			ud_set_traverse_mask(MOTIONTRG15,UU_FALSE);
			ud_set_traverse_mask(MOTIONTRG16,UU_FALSE);
			ud_set_traverse_mask(CLRRG6,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(MOTIONTRG12,UU_TRUE);
			if (TLraptotyp==0)
			{
				ud_set_traverse_mask(MOTIONTRG13,UU_FALSE);
				ud_set_traverse_mask(MOTIONTRG14,UU_FALSE);
				ud_set_traverse_mask(CLRRG6,UU_FALSE);
				ud_set_traverse_mask(MOTIONTRG15,UU_FALSE);
				ud_set_traverse_mask(MOTIONTRG16,UU_FALSE);
			}
			else if (TLraptotyp==1)
			{
				ud_set_traverse_mask(MOTIONTRG13,UU_TRUE);
				ud_set_traverse_mask(MOTIONTRG14,UU_FALSE);
				ud_set_traverse_mask(CLRRG6,UU_FALSE);
				ud_set_traverse_mask(MOTIONTRG15,UU_TRUE);
				ud_set_traverse_mask(MOTIONTRG16,UU_TRUE);
			}
			else
			{
				ud_set_traverse_mask(MOTIONTRG13,UU_TRUE);
				ud_set_traverse_mask(MOTIONTRG14,UU_TRUE);
				ud_set_traverse_mask(CLRRG6,UU_TRUE);
				ud_set_traverse_mask(MOTIONTRG15,UU_TRUE);
				ud_set_traverse_mask(MOTIONTRG16,UU_TRUE);
			}
		}


		if (*(val->frmint) == 2) //Arc
		{
			ud_set_display_mask(UD_INPUTF, MOTIONTRG3, 0);
			ud_set_display_mask(UD_INPUTF, MOTIONTRG2, 1);
		}
		else
		{
			ud_set_display_mask(UD_INPUTF, MOTIONTRG2, 0);
			ud_set_display_mask(UD_INPUTF, MOTIONTRG3, 1);
		}
		Smenu_ch[6] = *(val->frmint);
		break;
	case MOTIONTRG5:
		if (*(val->frmint)==0)
		{
			ud_set_traverse_mask(MOTIONTRG6,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(MOTIONTRG6,UU_TRUE);
		}
		Smenu_ch[5] = *(val->frmint);
		break;
	case MOTIONTRG12:
		if (*(val->frmint)==0)
		{
			TLraptoval[0] = '\0';	
			ud_update_answer(MOTIONTRG13,(int *)TLraptoval);
			ud_set_traverse_mask(MOTIONTRG13,UU_FALSE);
			ud_set_traverse_mask(MOTIONTRG14,UU_FALSE);
			ud_set_traverse_mask(CLRRG6,UU_FALSE);
			ud_set_traverse_mask(MOTIONTRG15,UU_FALSE);
			ud_set_traverse_mask(MOTIONTRG16,UU_FALSE);
		}
		else if (*(val->frmint)==1)
		{
			strcpy(TLraptoval, "0.0");	
			ud_update_answer(MOTIONTRG13,(int *)TLraptoval);
			ud_set_traverse_mask(MOTIONTRG13,UU_TRUE);
			ud_set_traverse_mask(MOTIONTRG14,UU_FALSE);
			ud_set_traverse_mask(CLRRG6,UU_FALSE);
			ud_set_traverse_mask(MOTIONTRG15,UU_TRUE);
			if (TLraptofed==2)
				ud_set_traverse_mask(MOTIONTRG16,UU_TRUE);
			else
				ud_set_traverse_mask(MOTIONTRG16,UU_FALSE);
		}
		else
		{
			strcpy(TLraptoval, SgLrap.label);	
			ud_update_answer(MOTIONTRG13,(int *)TLraptoval);
			ud_set_traverse_mask(MOTIONTRG13,UU_TRUE);
			ud_set_traverse_mask(MOTIONTRG14,UU_TRUE);
			ud_set_traverse_mask(CLRRG6,UU_TRUE);
			ud_set_traverse_mask(MOTIONTRG15,UU_TRUE);
			if (TLraptofed==2)
				ud_set_traverse_mask(MOTIONTRG16,UU_TRUE);
			else
				ud_set_traverse_mask(MOTIONTRG16,UU_FALSE);
		}
		Smenu_ch[7] = *(val->frmint);
		break;
	case MOTIONTRG15:
		if (*(val->frmint)==2)
		{
			ud_set_traverse_mask(MOTIONTRG16,UU_TRUE);
			strcpy(TLraptofrv_str, Sav_valuestr);
			ud_dispfrm_update_answer(0,MOTIONTRG16,(int *)TLraptofrv_str);
		}
		else
		{
			ud_set_traverse_mask(MOTIONTRG16,UU_FALSE);
			if (*(val->frmint)==0)
			{
				strcpy(valuestr, Sfeed_valuestr);
				if (Ssav_fchc==2)
					strcpy(Sav_valuestr, TLraptofrv_str);
				ud_dispfrm_update_answer(0,MOTIONTRG16,(int *)valuestr);
			}
			if (*(val->frmint)==1)
			{
/*
.....rapid
*/
				if (Ssav_fchc==2)
					strcpy(Sav_valuestr, TLraptofrv_str);
				valuestr[0] = '\0';
				ud_dispfrm_update_answer(0,MOTIONTRG16,(int *)valuestr);
			}
		}
		Smenu_ch[8] = *(val->frmint);
		Ssav_fchc = *(val->frmint);
		break;
	}
	Sacc[MotionT] = 1;
	S_enable_buttons();
	return(UD_FLDOK);
}

static UD_FSTAT OnMText(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch(*fieldno)
	{
	case MOTIONTRG3: 
		break;
	}
	Sacc[MotionT] = 1;
	S_enable_buttons();
	return(UD_FLDOK);
	return(UD_FLDOK);
}
static UD_FSTAT OnMSButton(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int pr, namfld, *mask;
	char *namp;
	int status;	
	char label[3];
	/*strcpy(label, Tdirval1);
	ul_to_upper(label);
	if (!strcmp(label,"PL1"))
		ud_dispfrm_set_attribs(0,MOTIONTRG17,UM_WHITE,UM_RED);
	else
	{
		uw_ntdispmsg("Value should PL1");
			return(UD_BADACT);
	}*/
	/*strcpy(label, Tdirval2);
	ul_to_upper(label);
	if (!strcmp(label,"PL2"))
		ud_dispfrm_set_attribs(0,MOTIONTRG18,UM_WHITE,UM_RED);
	else
	{
		uw_ntdispmsg("Value should PL2");
			return(UD_BADACT);
	}*/

	if (*fieldno == MOTIONTRG17) //for select button1
	{
		/*strcpy(label, Tdirval1);
		ul_to_upper(label);
		if (!strcmp(label,"PL1"))
			ud_dispfrm_set_attribs(0,MOTIONTRG17,UM_WHITE,UM_RED);
		else
	{
		uw_ntdispmsg("Value should PL1");
			return(UD_BADACT);
	}*/
		/*mask = (int *)UD_ncl_allsfpl;*/
		mask = (int *)UD_ncl_pl;
		//UD_ncl_lnpl
		
		//pr = 663;
		pr = 514;
		namp = Tdirval1;
		namfld = MOTIONTRG17;
		status = S_select_geo(&SgBpl1,&Sbnd,mask,0,pr,Spassclr,0,namfld,Tdirval1);
		if (!strcmp(SgBpl1.label, "") )
		{
			
			uw_ntdispmsg("No bounding plane selected");
			return(UD_BADACT);
		}
		//status = S_select_geo(&Sgsf,&Ssurf,mask,0,pr,Tcolor,0,MILLRG1,Tselsrf/*, &uvalSgBpl1.label
		//strcpy(Tdirval1,namp);
		strcpy(Tdirval1, SgBpl1.label);
		strcpy(Sdirval1, SgBpl1.label);
		//status = nclu_pl_selected(Tdirval1);
		Snbnd = UU_LIST_LENGTH(&Sbnd);
		if (Snbnd>0)
			ud_set_traverse_mask(MOTIONTRG17, UU_TRUE);
		S_save_form();
		 S_update_answers();
		//S_enable_buttons();
		firstSelected = UU_TRUE;
	}
	if (*fieldno == MOTIONTRG18) //for select button2

	{
		/*strcpy(label, Tdirval2
		namp =
		ul_to_upper(label);
		if (!strcmp(label,"PL2"))
			ud_dispfrm_set_attribs(0,MOTIONTRG18,UM_WHITE,UM_RED);
		else
		{
			uw_ntdispmsg("Value should PL2");
				return(UD_BADACT);
		}*/
		mask = (int *)UD_ncl_pl;
		//mask = (int *)UD_ncl_allsfpl;
		pr = 515;
		namp = Tdirval2;
		namfld = MOTIONTRG18;
		status = S_select_geo(&SgBpl2, &Sbnd,mask,0,pr,Spassclr,0,namfld,namp);
		if (!strcmp(SgBpl2.label, "") )
		{
			
			uw_ntdispmsg("No bounding plane selected");
			return(UD_BADACT);
		}
		strcpy(Tdirval2, SgBpl2.label);
		strcpy(Sdirval2, SgBpl2.label);
		Snbnd = UU_LIST_LENGTH(&Sbnd);
		if (Snbnd>0)
			ud_set_traverse_mask(MOTIONTRG18, UU_TRUE);
		S_save_form();
		S_update_answers();
	}	
	if (status==0)
	{
		Sacc[MotionT] = 1;
		S_enable_buttons();	
		return(UD_FLDOK);
	}
	return(UD_BADACT);

}
 
static UD_FSTAT OnMButton(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	
	UM_coord ndc;
	int pr, namfld, *mask;
	/*UU_REAL uval1 = 0;
	UU_REAL uval2 = 0;*/
	char *namp;
	char label[65];
	int status;	//=-1;
	if (loaded==UU_FALSE)
	{
			uw_ntdispmsg("No port surface exists or selected");
			return(UD_BADACT);
	}
	strcpy(label, "0");

	if (*fieldno == MOTIONTRG9)
	{
		//strcpy(label, Tdirval1);
		/*ul_to_upper(Tdirval1);
		if (!strcmp("PL1",Tdirval1))
		{
			uw_ntdispmsg("Value should be input 0 <= u <= 1");
			return(UD_BADACT);
		}*/
		/*if (Tdirval1[0]=='.')
		{
			strcat(label,Tdirval1);
			strcpy(Tdirval1,label);
		}*/
		uval1=atof(Tdirval1);
		if ((uval1<0.0)|| (uval1 >1))
		{
			uw_ntdispmsg("Value should be input 0 <= u <= 1");
			return(UD_BADACT);
		}
		status = key_of_label("PT1", status);
		if (status<=0)
		{
			uw_ntdispmsg("No port surface exists ");
			return(UD_BADACT);
		}
		status = key_of_label("PT3", status);
		if (status<=0)
		{
			uw_ntdispmsg("No port surface exists");
			return(UD_BADACT);
		}
		/*status = nclu_pl_on_crv("PL1", "CV4", uval1);*/
		status = nclu_pl_on_crv("PL1", "CV3", uval1);

		strcpy(Ttoler_str, Stoler_str);

		firstSelected = UU_TRUE;
	}
	else if (*fieldno == MOTIONTRG11)
	{

		if (firstSelected == UU_FALSE)
		{
			uw_ntdispmsg("First plane not created");
			return(UD_BADACT);
		}
		uval2=atof(Tdirval2);
		if ((uval2<0.0)|| (uval2 >1))
		{
			uw_ntdispmsg("Value should be put 0 <= u <= 1");
			return(UD_BADACT);
		}
		/*status = nclu_pl_on_crv("PL2", "CV4", uval2);*/
		status = nclu_pl_on_crv("PL2", "CV3", uval2);
		/*if (uval1<uval2)
			go_to_start("PT3");
		else
			go_to_start("PT4");*/

		if (uval1<uval2)
			go_to_start("PT1");
		else
			go_to_start("PT3");
	}
	else if (*fieldno == MOTIONTRG14)
	{
		mask = (int *)UD_allcurves;
		pr = 663;
		namp = TLraptoval;
		namfld = MOTIONTRG13;
		status = S_select_geo(&SgLrap,UU_NULL,mask,0,pr,Spassclr,0,namfld,namp/*,&uval*/);
	}
	if (status==0)
	{
		Sacc[MotionT] = 1;
		S_enable_buttons();	
		return(UD_FLDOK);
	}
	return(UD_BADACT);
}

static UD_FSTAT OnBChcPick(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	Sacc[Boundary] = 1;
	S_enable_buttons();	
	return(UD_FLDOK);
}

static UD_FSTAT OnBChkPick(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	Sacc[Boundary] = 1;
	S_enable_buttons();	
	return(UD_FLDOK);
}

static UD_FSTAT OnEChcPick(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char valuestr[80];
	switch (*fieldno)
	{
	case ENEXTRG1:
		if (*(val->frmint) == 0)
		{
			ud_set_traverse_mask(ENEXTRG2,UU_FALSE);
			ud_set_traverse_mask(ENEXTRG3,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(ENEXTRG2,UU_TRUE);
			ud_set_traverse_mask(ENEXTRG3,UU_TRUE);
		}
		Smenu_ch[1] = *(val->frmint);
		break;
	}
	Sacc[Entry] = 1;
	S_enable_buttons();	
	return(UD_FLDOK);
}
static UD_FSTAT OnEText(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	Sacc[Entry] = 1;
	S_enable_buttons();	
	return(UD_FLDOK);
}
static UD_FSTAT OnEButton(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;

{
	int pr, namfld, *mask;
	char *namp;
	int status=-1;
	if (*fieldno == ENEXTRG3)
	{
		mask = (int *)UD_ncl_pt;
		pr = 516;
		namp = Tptnam;
		namfld = ENEXTRG2;
		status = S_select_geo(&Sgspt,UU_NULL,mask,0,pr,Tstptclr,0,namfld,namp/*, &uval*/);
	}
	if (status==0)
	{
		Sacc[Entry] = 1;
		S_enable_buttons();	
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_free_repaint (listptr)
*********************************************************************/
/*********************************************************************
**    E_FUNCTION     : S_build_command(flg)
**       Build the PMill statement.
**    PARAMETERS
**       INPUT  : flag    = UU_TRUE = output command to source file.
**			                   UU_FALSE = Preview command only.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_build_command(flg)
UU_LOGICAL flg;
{
	int i;
	NCL_cmdbuf cmdbuf;
	char buf[64];
	char a[64];
	char ab[64];
	//a[0] = '\0';
	a[0]='0';
	//b[0] = '\0';
	ab[0]='0';

	if (flg)
		NCL_preview_mot = 0;
	else
		NCL_preview_mot = 1;
	Snsurf = UU_LIST_LENGTH(&Ssurf);
	ncl_init_cmdbuf(&cmdbuf);

	if (!flg) ncl_add_token(&cmdbuf, "*", NCL_nocomma);

	ncl_add_token(&cmdbuf, NCL_pmill, NCL_nocomma);

	if (Snsurf > 0)
	{
		UM_sgeo *geo = (UM_sgeo *) UU_LIST_ARRAY(&Ssurf);
		for (i = 0; i < Snsurf; i++)
		{
			ncl_add_token(&cmdbuf, geo[i].label, NCL_comma);
		}
	}

	if (Tdirval1[0]=='.')
		{
			strcat(a,Tdirval1);
			strcpy(Tdirval1,a);
			//memset(abel, '0', 64);
			//strcpy(abel, "");
			//abel[0] = '\0';
			//abel[0] = '0';
		}
		if ((Tdirval1[0]!='0')&&(Tdirval1[0]!='1')/*&&(Tdirval1[0]!='-')*/)
		{
			strcpy(Tdirval1,SgBpl1.label);
			//ncl_add_token(&cmdbuf, "PL1", NCL_comma);
			ncl_add_token(&cmdbuf, SgBpl1.label, NCL_comma);
		}
		else
			ncl_add_token(&cmdbuf, "PL1", NCL_comma);

		if (Tdirval2[0]=='.')
		{
			strcat(ab,Tdirval2);
			strcpy(Tdirval2,ab);
			//abel[0] = '\0';
			//abel[0] = '0';
		}
		if ((Tdirval2[0]!='0')&&(Tdirval2[0]!='1'))
		{
			strcpy(Tdirval2,SgBpl2.label);
			//ncl_add_token(&cmdbuf, "PL1", NCL_comma);
			ncl_add_token(&cmdbuf, SgBpl2.label, NCL_comma);
		}
		else
			ncl_add_token(&cmdbuf, "PL2", NCL_comma);
	ncl_add_token(&cmdbuf, NCL_run_step, NCL_comma);
	strcpy(buf, Tstp_str);
	ncl_add_token(&cmdbuf, buf, NCL_comma);

	if (Tstarttyp)
	{
		if (Tstarttyp == 1 && Tptnam[0])
		{
			ncl_add_token(&cmdbuf, NCL_start, NCL_comma);
			ncl_add_token(&cmdbuf, Tptnam, NCL_comma);
		}
	}


	if (Tmethod == 0)
	{
		ncl_add_token(&cmdbuf, NCL_linear, NCL_comma);
		ncl_add_token(&cmdbuf, Torient_str, NCL_comma);

	}

	else if (Tmethod == 1)
	{
		ncl_add_token(&cmdbuf, NCL_helix, NCL_comma);
		ncl_add_token(&cmdbuf, Torient_str, NCL_comma);
	}
	else if (Tmethod == 2)
	{
		ncl_add_token(&cmdbuf, NCL_arc, NCL_comma);
		ncl_add_token(&cmdbuf, Trad_str, NCL_comma);
		if (TLraptotyp && TLraptoval[0])
		{
			ncl_add_token(&cmdbuf, TLraptoval, NCL_comma);
			if (TLraptofed == 1)
				ncl_add_token(&cmdbuf, NCL_rapid, NCL_comma);
			else if (TLraptofed == 2)			
				ncl_add_token(&cmdbuf, TLraptofrv_str, NCL_comma);
		}
	}

	if (Ttoltyp > 0)
	{
		ncl_add_token(&cmdbuf, NCL_show_toler, NCL_comma);
		strcpy(buf, Ttoler_str);
		ncl_add_token(&cmdbuf, buf, NCL_comma);
	}
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
				ud_updatews(UG_SUPPRESS);
				uw_ntflush_paint();
				uw_ntflush_win();

	return 0;
}
/*********************************************************************
**    S_FUNCTION     :  S_storvals()
**       Method called to store current settings as basis for later
**       comparisons.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT S_storvals()
{
/*
.....Store values to check for changes  temp for PMILL
*/
	Smenu_ch[1] = Sstarttyp;
	Smenu_ch[5] = Stoltyp;
	Smenu_ch[6] = Smethod;
	Smenu_ch[7] = SLraptotyp;
	Smenu_ch[8] = SLraptofed;;
	return(UD_FLDOK);
}
/*********************************************************************
**    I_FUNCTION     : S_init_form()
**       Initializes the Flowline Milling form variables.
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
	UM_real8 tol, feedrate;
	NCLX_mot_feedrate fedrat;

	int sc10_3;
	char buf[10];
/*
.....Initialize the form settings
*/
	if (!Spmfrm_init)
	{
		Spmfrm_init = UU_TRUE;
		Snsurf = 0;

		Sstarttyp = 0;
		Stoltyp = 0;
		Smethod = 0; 
		SLraptotyp = 0;
		SLraptofed = 0;
		Sstp = 0.1;
		Stoler = 0.0;
		Scolor = NCLX_BROWN;
		Sstptclr = NCLX_SEA_GREEN;
		Spassclr = NCLX_PURPLE;
		Sdirclr = NCLX_GREY;
		strcpy(Sstp_str, "0.1");
		strcpy(Srad_str, "0.05");
		ncl_sprintf(Stoler_str,&Stoler,1);
	}
	strcpy(Sstp_str, "0.1");
	strcpy(Tstp_str, Sstp_str);
/*
.....always empty for surfaces per Ken
*/
	Sptnam[0] = '\0';
	
	Sdirval2[0] = '\0';
	Sdirval1[0] = '\0';
	SLraptoval[0] = '\0';
	if (SLraptotyp==1)
	{
		strcpy(TLraptoval,SLraptoval);
	}
	Stoler = atof(Stoler_str);
	if (Stoler < UM_DFUZZ)
	{
		gettol(&tol);
		Stoler = tol;
	}
	Sstp = atof(Sstp_str);
	if (Sstp < UM_DFUZZ)
	{
		Sstp = 10.*Stoler;
	}
	Srad = atof(Srad_str);
	if (Srad < UM_DFUZZ)
	{
		Srad = 10.*Stoler;
	}
	if (Stoler < UM_DFUZZ)
	{
		gettol(&tol);
		Stoler = tol;
	}
	ncl_sprintf(Sstp_str, &Sstp,1);
	ncl_sprintf(Srad_str, &Srad,1);
	ncl_sprintf(Stoler_str,&Stoler,1);
	Tstarttyp = Sstarttyp;
	Ttoltyp = Stoltyp;
	Tmethod = Smethod; 
	TLraptotyp = SLraptotyp;
	TLraptofed = SLraptofed;
	strcpy(Ttoler_str, Stoler_str);
	NclxMotGetFeedrate(&fedrat);
	feedrate = fedrat.base_feedrate;
	strcpy(Sav_valuestr,SLraptofrv_str);
	strcpy(TLraptofrv_str, SLraptofrv_str);
	Tcolor = Scolor;
	Tstptclr = Sstptclr;
	Tpassclr = Spassclr;
	Tdirclr = Sdirclr;
	Tgeotog = UN_unused_geo_flag;
	Tgeocol = UN_unused_geo_color;
}

static void S_init_traverse(display,traverse)
char *display,*traverse;
{	
	int i;
	traverse[MILLRG1] = 1;
		traverse[MILLRG2] = 1;
		traverse[CLRRG1] = 1;

		traverse[MOTIONTRG1] = 1;
	if (Tmethod==2)
	{
		traverse[MOTIONTRG12] = 1;
		traverse[MOTIONTRG15] = 1;
		if (TLraptotyp==0)
		{
			traverse[MOTIONTRG13] = 0;
			traverse[MOTIONTRG14] = 0;
			traverse[CLRRG6] = 0;
			traverse[MOTIONTRG15] = 0;
			traverse[MOTIONTRG16] = 0;
		}
		else if (TLraptotyp==1)
		{
			traverse[MOTIONTRG13] = 1;
			traverse[MOTIONTRG14] = 0;
			traverse[CLRRG6] = 0;
			traverse[MOTIONTRG15] = 1;
			traverse[MOTIONTRG16] = 1;
			if (SLraptofed==2)
				traverse[MOTIONTRG16] = 1;
			else
				traverse[MOTIONTRG16] = 0;		}
		else
		{
			traverse[MOTIONTRG13] = 1;
			traverse[MOTIONTRG14] = 1;
			traverse[CLRRG6] = 1;
			traverse[MOTIONTRG15] = 1;
			traverse[MOTIONTRG16] = 1;
			if (SLraptofed==2)
				traverse[MOTIONTRG16] = 1;
			else
				traverse[MOTIONTRG16] = 0;
		}
	}
	else
	{
		traverse[MOTIONTRG12] = 0;
		traverse[MOTIONTRG13] = 0;
		traverse[MOTIONTRG14] = 0;
		traverse[MOTIONTRG15] = 0;
		traverse[MOTIONTRG16] = 0;
		traverse[CLRRG6] = 0;
	}

	traverse[MOTIONTRG2] = 1; 
	traverse[MOTIONTRG5] = 1;
	if (Ttoltyp==0)
		traverse[MOTIONTRG6] = 0;
	else
		traverse[MOTIONTRG6] = 1;
		
	traverse[MOTIONTRG8] = 1;
	traverse[MOTIONTRG9] = 1;
	traverse[MOTIONTRG10] = 1;
	traverse[MOTIONTRG11] = 1;
	traverse[CLRRG3] = 1;
/*
.....Entry/Exit
*/
	traverse[ENEXTRG1] = 1;
	if (Sstarttyp==0)
	{
		traverse[ENEXTRG2] = 0;
		traverse[ENEXTRG3] = 0;
	}
	else
	{
		traverse[ENEXTRG2] = 1;
		traverse[ENEXTRG3] = 1;
	}
/*
.....Action item fields
*/
	for (i=FAPV;i<FAVW;i++) traverse[i] = 0; 
	if (Tmethod == 2)
	{
		display[MOTIONTRG2+1] = 1;
		display[MOTIONTRG3+1] = 0;
	}
	else
	{
		display[MOTIONTRG2+1] = 0;
		display[MOTIONTRG3+1] = 1;
	}
}

/*********************************************************************
**    E_FUNCTION     : nclu_pmill()
**       Processes the NCL Port Surface Milling form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_pmill()
{
	int flag,ifl,val;
	UU_LOGICAL cmdreject;
	UD_METHOD save_entry;

	NCL_cmdbuf cmdbuf;
	int status;
/*
.....Set up form fields
*/
	static UD_METHOD methods[] = {
/*
.....PMill
*/
		OnPText, OnPButton, OnPButton,
/*
......Motion Type
*/
		OnMChcPick, OnMText, OnMChcPick,
		OnMText, 
		OnMChcPick, OnMText,
		OnMText, OnMButton, OnMSButton, 
		OnMText, OnMButton, OnMSButton, 
		OnMChcPick, OnMText, OnMButton, 
		OnMChcPick, OnMText, 
/*
.....Entry / Exit
*/
		OnEChcPick, OnEText, OnEButton, 
/*
.....Colors
*/
		OnColors, OnColors, OnColors,
		OnColors, OnColors, 
		OnColors, 
		OnAction,OnAction,OnAction,
		OnAction,OnAction,OnAction,OnAction,OnVideo
	};


	static char traverse[]= {
		1,1,1,
		1,1,1,1,1,1,1,1,1, 1,1,
		1,1,1,1,1,1,
		1,1,1,
		1,1,1,1,1,1,
		1,1,1,1,1,1,1,1};
	static char called[]  = {
		6,6,6,
		6,6,6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,
		6,6,6,
		6,6,6,6,6,6,
		6,6,6,6,6,6,6,6};
	static char display[] = {
		1,1,1,
		1,1,1,1,1,1,1,1,1, 1,1,
		1,1,1,1,1,1,
		1,1,1,
		1,1,1,1,1,1,
		1,1,1,1,1,1,1,1};
/*
.....Initialize routine
*/
	Serrflg = 0;
	Spreviewflg = 0;
	Schgmade = 0;
/*
.....Initialize form answers
*/
	S_init_form();
	S_storvals();
	Serrflg = 0;
	Spreviewflg = UU_FALSE;
	Schgmade = UU_FALSE;
	save_entry = UD_initfrm_intry;
	SfrmPlay = 0;
	Skey_init = UU_FALSE;
	Snkeys = 0;
	Sacc[PMill] = Sacc[MotionT] = Sacc[Boundary] = Sacc[Entry] = Sacc[Colors] = 0;
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0) 
		goto done;
/*
..... Initialize answers
*/
	uu_list_init(&Ssurf,sizeof(UM_sgeo),50,50);
	Tselsrf[0] = '\0';
	/*Tdirval1[0]= '\0';
	Tdirval2[0]= '\0';*/
	S_init_traverse(display,traverse);
/*
.....Display the Form
*/
repeat:
	NCL_preview_mot = 1;
	UD_initfrm_intry = S_enter_form;
	Sfrm = ud_form1("npmill.frm", Sanswers, Sanswers,methods,called,display,traverse);
	
	if (Smptr != UU_NULL && (!Spreviewflg || Schgmade || Sfrm == -1))
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
		if (!Schgmade && Sfrm != -1 && Serrflg == 0) flag = UU_TRUE;
		else flag = UU_FALSE;
		moinfr(&flag);
	}
	if (Sfrm == -1) goto done;
	Snsurf = UU_LIST_LENGTH(&Ssurf);
/*
.....Save the form settings
*/
	S_save_form();
/*
.....Output the command
*/
	S_build_command(UU_TRUE);
	ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf,"PT2", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "=", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "POINT/", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "X+0.25", NCL_comma);
		status = ncl_add_token(&cmdbuf, "Y-0.5", NCL_comma);
		status = ncl_add_token(&cmdbuf, "Z", NCL_nocomma);
		
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		//ncl_init_cmdbuf(&cmdbuf);
		ncl_init_cmdbuf(&cmdbuf);
		
		/*status = ncl_add_token(&cmdbuf,"goto/", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "X+3", NCL_comma);
		status = ncl_add_token(&cmdbuf, "Y-0.5", NCL_comma);
		status = ncl_add_token(&cmdbuf, "Z", NCL_nocomma);*/
		
		status = ncl_add_token(&cmdbuf,"goto/", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "PT2", NCL_comma);
		//status = ncl_add_token(&cmdbuf, "Y-0.5", NCL_comma);
		//status = ncl_add_token(&cmdbuf, "Z", NCL_nocomma);
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);


done:;
	if (Sgeo_redraw)
		ncl_hide_geo(-1,-1,-1,-1,UU_TRUE,UU_NULL,0);
	if (Skey_init)
		uu_list_free(&Skey_list);
	Skey_init = UU_FALSE;
	S_unhilite_all();
/*
.....Reset ignore inner boundary flag
*/
//	ifl = 394; val = 0; setifl(&ifl,&val);
	NCL_preview_mot = 0;
	UD_UNMARK(cmdreject);
	return;
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
	int i, n;
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
	n = UU_LIST_LENGTH(&Ssurf);
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(&Ssurf);
	for (i=0;i<n;i++) S_add_key(which,mgeo[i].key);
	S_add_key(which, Sgspt.key);
	S_add_key(which, Sgrap.key);
	S_add_key(which, Sgret.key);
	S_add_key(which, SgLrap.key);
	S_add_key(which, Sgdrv1.key);
	S_add_key(which, Sgdrv2.key);
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
	ud_form_section_color(0, Smode[section], Ssecol[section], bold);
/*
.....Enable Reset button
*/
	if (reset) ud_set_traverse_mask(FARS,UU_TRUE);
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

static int S_select_geo(sfpt,sflst,mask,multi,prmno,color,frm,fieldno,label/*,uval*/)
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
	UM_int2 store;

	UM_coord			ndc;

	char paracoord[50];

/*
.....Take down form
*/
	iret = UU_SUCCESS;
	if (multi != 2) S_form_invis();
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
	if ((multi != 2)&&(multi != 3))
	{
		if (sfpt != UU_NULL) S_unhilite_entity(sfpt);
		if (sflst != UU_NULL) S_unhilite_entities(sflst);
	}
	else if (multi!=3)
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
		if ((multi != 1)&&(multi != 3))
		{
			ua_dl_pldas(UD_DASPCKLOC,UA_NCL,prmno,&pick,1,&numint,1);
			if (numint == 0) goto failed;
			e.key = um_get_pickkey(&(pick.pent),1);
/*
........Screen location picked
*/
			if (e.key == 0)
			{
				
				strcpy(label,pick.ploc.label);
			}
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
	if ((multi != 1)&&(multi != 3))
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
		if (sflst!=UU_NULL)
		{
			if(sflst->data==UU_NULL)
				uu_list_init(sflst,sizeof(UM_sgeo),numint,10);
			uu_list_push(sflst,sfpt);
			Sgeo_init = UU_TRUE;
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
		if (multi != 3)
		{
			S_unhilite_entities(sflst);
		}
/*
........Store the entities' data
*/
		if(sflst->data==UU_NULL)
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
				nc = (int)strlen(sfpt->label);
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
	if (multi != 2) S_form_vis();
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
**    E_FUNCTION     : nclu_cv_on_srf(srfname,name, sector, coord)
**       creates a spline that lies on given surface
**       at given sector and given parametric coordinate
**    PARAMETERS   
**       INPUT  : 
**          srfname of surface
**			name of curve
**			sector on surface
**			parametric coord
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_cv_on_srf(srfname, name, sector, coord)
char* srfname;
char* name;
int sector;
UU_REAL coord;
{
	NCL_cmdbuf cmdbuf;
	int status;
	char paracoord[50];

		ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf, name, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "=", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, NCL_spline, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, srfname, NCL_comma);
		itoa(sector,paracoord,10);
		status = ncl_add_token(&cmdbuf, paracoord, NCL_comma);
		sprintf(paracoord,"%0.2f",coord);
		status = ncl_add_token(&cmdbuf, paracoord, NCL_nocomma);

		ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);

			//D5=DIST(PT6,PL1)

		/*ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf, "CV4", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "=", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, NCL_spline, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "OFFSET", NCL_comma);
		status = ncl_add_token(&cmdbuf, name, NCL_comma);
		status = ncl_add_token(&cmdbuf, "ZS", NCL_comma);
		sprintf(paracoord,"%f",1.0);
		status = ncl_add_token(&cmdbuf, paracoord, NCL_nocomma);

		ncl_set_cmdmode(UU_TRUE);
			ncl_add_cmdbuf(&cmdbuf);
			ncl_call(&cmdbuf);*/
			

done:;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_cv_from_srf(name)
**       deletes a spline that lies on given surface
**      
**    PARAMETERS   
**       INPUT  : 
**			name of curve

**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_cv_from_srf(name)
char* name;
{
	NCL_cmdbuf cmdbuf;
	int status;

		ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf,"remove/", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, name, NCL_nocomma);

		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);

		ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf,"remove/", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "CV4", NCL_nocomma);

		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
			

done:;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_pl_on_crv(plname,name, coord)
**       creates a plane orthogonal to curve
**       at given point
**    PARAMETERS   
**       INPUT  : 
**          plname of plane
**			name of curve
**			parametric coord
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pl_on_crv(plname, name, coord)
char* plname;
char* name;
UU_REAL coord;
{
	
	NCL_cmdbuf cmdbuf;
	int status;
	char paracoord[50];

	if ((coord<0.0) || (coord>1.0))
	{

		uw_ntdispmsg("Value should be selected 0 <= u <= 1");
		return(UD_BADACT);
	}
	/*if (coord<=0.01)
		coord = 0.02;
	if (coord>=0.9)
		coord = 0.85;*/
	
		ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf, plname, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "=", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "PL/(PV/ON", NCL_comma);
		status = ncl_add_token(&cmdbuf, name, NCL_comma);
		sprintf(paracoord,"%0.2f",coord);
		status = ncl_add_token(&cmdbuf, paracoord, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, ")", NCL_nocomma);

		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);

done:;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_pl_selected(plname)
**       creates a plane orthogonal to curve
**       at given point
**    PARAMETERS   
**       INPUT  : 
**          plname of plane
**			
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pl_selected(plname)
char* plname;
//char* name;
//UU_REAL coord;
{
	
	NCL_cmdbuf cmdbuf;
	int status;
	//char paracoord[50];

	/*if (strcmp(plname,"PL1") && strcmp(plname,"PL2"))
	{

		uw_ntdispmsg("Bounding plane name should be either PL1 or PL2");
		return(UD_BADACT);
	}*/
	/*if (coord<=0.01)
		coord = 0.02;
	if (coord>=0.9)
		coord = 0.85;*/
	
		ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf, plname, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "=", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "PL/(PV/ON", NCL_nocomma);
		//status = ncl_add_token(&cmdbuf, name, NCL_comma);
		//sprintf(paracoord,"%0.2f",coord);
		//status = ncl_add_token(&cmdbuf, paracoord, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, ")", NCL_nocomma);

		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);

done:;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_pl_from_crv(name)
**       deletes a plane orthog to curve
**      
**    PARAMETERS   
**       INPUT  : 
**			name of curve

**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pl_from_crv(name)
char* name;
{
	NCL_cmdbuf cmdbuf;
	int status;

		ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf,"remove/", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, name, NCL_nocomma);
		
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
			

done:;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_pt_on_crv_start(point_name, curve_name)
**       creates a start point on curve
**       at given point
**    PARAMETERS   
**       INPUT  : 
**			name of curve
**			name of point
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pt_on_crv_start(point_name, curve_name)
char* point_name;
char* curve_name;
{
	NCL_cmdbuf cmdbuf;
	int status;
		ncl_init_cmdbuf(&cmdbuf);
		status = ncl_add_token(&cmdbuf,point_name, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "=", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "POINT/YSMALL", NCL_comma);
		status = ncl_add_token(&cmdbuf, "ENDPT", NCL_comma);
		status = ncl_add_token(&cmdbuf, curve_name, NCL_nocomma);

		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);

		ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf,"CU/", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "0.2", NCL_comma);
		status = ncl_add_token(&cmdbuf, "0.1", NCL_comma);
		status = ncl_add_token(&cmdbuf, "0.2", NCL_comma);
		status = ncl_add_token(&cmdbuf, "-89", NCL_nocomma);
		
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);

		ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf,"OB/PT1", NCL_comma);
		status = ncl_add_token(&cmdbuf, "X", NCL_comma);
		status = ncl_add_token(&cmdbuf, "Y", NCL_comma);
		status = ncl_add_token(&cmdbuf, "Z", NCL_nocomma);
		
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		
		if (!strcmp(point_name, "PT1") || !strcmp(point_name, "PT2"))
		{

		ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf,"PT2", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "=", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "POINT/", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "X+0.25", NCL_comma);
		status = ncl_add_token(&cmdbuf, "Y-0.5", NCL_comma);
		status = ncl_add_token(&cmdbuf, "Z", NCL_nocomma);
		
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		ncl_init_cmdbuf(&cmdbuf);
		
		
		status = ncl_add_token(&cmdbuf, "th/0.1", NCL_nocomma);
		
		
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);

		/*ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf,"goto/", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "X+3", NCL_comma);
		status = ncl_add_token(&cmdbuf, "Y-0.5", NCL_comma);
		status = ncl_add_token(&cmdbuf, "Z", NCL_nocomma);
		
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);*/

		ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf,"goto/", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "PT2", NCL_comma);
		/*status = ncl_add_token(&cmdbuf, "Y-0.5", NCL_comma);
		status = ncl_add_token(&cmdbuf, "Z", NCL_nocomma);*/
		
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		}
done:;
	return (0);
}


/*********************************************************************
**    E_FUNCTION     : nclu_pt_on_crv_end(point_name, curve_name)
**       creates end point on curve
**       at given point
**    PARAMETERS   
**       INPUT  : 
**			name of curve
**			name of point
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pt_on_crv_end(point_name, curve_name)
char* point_name;
char* curve_name;
{
	NCL_cmdbuf cmdbuf;
	int status;
		ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf,point_name, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "=", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "POINT/YLARGE", NCL_comma);
		status = ncl_add_token(&cmdbuf, "ENDPT", NCL_comma);
		status = ncl_add_token(&cmdbuf, curve_name, NCL_nocomma);

		
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		done:;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_pl_from_crv(name)
**       deletes a plane orthog to curve
**      
**    PARAMETERS   
**       INPUT  : 
**			name of curve

**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pt_from_crv(name)
char* name;
{
	NCL_cmdbuf cmdbuf;
	int status;

		ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf,"remove/", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, name, NCL_nocomma);
		
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
			

done:;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : go_to_start
**       deletes a plane orthog to curve
**      
**    PARAMETERS   
**       INPUT  : 
**			name of curve

**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int go_to_start(name)
char* name;
{
	NCL_cmdbuf cmdbuf;
	int status;

		ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf,"goto/", NCL_nocomma);
		status = ncl_add_token(&cmdbuf, name, NCL_nocomma);
		
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);

		ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf,"TA/THRU", NCL_comma);
		status = ncl_add_token(&cmdbuf, name, NCL_nocomma);
		
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
			

done:;
	return (0);
}


/*********************************************************************
**    E_FUNCTION     : key_of_label
**       returns key of entity with label
**      
**    PARAMETERS   
**       INPUT  : 
**			name of curve

**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int key_of_label(label, key)
char label[MXLAB];  //PT label here
UU_KEY_ID key;
{
	int i,nc;
	UM_f77_str f77_str;
	nc = strlen(label);
	UM_init_f77_str(f77_str,label,64);
	getkey(UM_addr_of_f77_str(f77_str),&key);
	done:;
	return (key);
}

/*********************************************************************
**    E_FUNCTION     : point_coord
**       returns coord of point by its key
**      
**    PARAMETERS   
**       INPUT  : 
**			name of curve

**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int point_coordinates(key, ndc)
UU_KEY_ID key;
UM_coord			ndc;
{

	struct NCL_fixed_databag e;
	struct NCL_nclpt_rec *pt_rec;
	
	e.key = key;  
	ncl_retrieve_data (&e); 
	if ((e.rel_num==UM_POINT_REL)|| (e.rel_num==NCL_POINT_REL))  {
		pt_rec = (struct NCL_nclpt_rec *) &e;
		ndc[0] = pt_rec->pt[0];
		ndc[1] = pt_rec->pt[1]; 	
		ndc[2] = pt_rec->pt[2];
		
	}
	done:;
	return (0);
}

int create_point(point_name, Surf_name, u,v)
	char* point_name, *Surf_name;
UU_REAL u,v;
{
	NCL_cmdbuf cmdbuf;
	int status;
	char paracoord[50];

		ncl_init_cmdbuf(&cmdbuf);
		status = ncl_add_token(&cmdbuf,point_name, NCL_nocomma);
		status = ncl_add_token(&cmdbuf, "=", NCL_nocomma);
		status = ncl_add_token(&cmdbuf,"point/on", NCL_comma);
		status = ncl_add_token(&cmdbuf,Surf_name, NCL_comma);
		sprintf(paracoord,"%.2f",u);
		status = ncl_add_token(&cmdbuf, paracoord, NCL_comma);
		sprintf(paracoord,"%.2f",v);
		status = ncl_add_token(&cmdbuf, paracoord, NCL_nocomma);
		
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
		done:;
	return (0);
}