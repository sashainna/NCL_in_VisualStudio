/********************************************************************* 
**    NAME         :  nupocket1.c
**       CONTAINS:
**				nclu_pocket_advanced()
**				ncl_set_open_att()
** 		   ncl_get_indlst()
**    		ncl_reset_indlst()
**    		ncl_push_indlst()
**				ncl_pocket_draw_open()
**				ncl_pocket_checkind()
** 		   ncl_update_aform()
**
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nupocket1.c , 25.7
**     DATE AND TIME OF LAST  MODIFICATION
**       01/20/17 , 11:05:49
**       
*********************************************************************/
#include <string.h>
#include "class.h"
#include "nclmplay.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mxxx.h"
#include "nccs.h"
#include "ncl.h"
#include "nclupok.h"
#include "nclx.h"
#include "mdeval.h"
#include "mcrv.h"
#include "mattr.h"

extern int NCL_preview_mot;
/*
.....Boundaries fields
*/
enum
{
	FBPER,FBPED,FBPTX, FBOPN,FBODE, FBINR, FBSRF, FBISL,FBISD,FBITX, FBTHK,FBOTH,
	FBPIC,
/*
.....Levels fields
*/
	FLBOT,FLBTX, FLBTH, FLTOP,FLTTX,FLTSE,
/*
.....Options
*/
	FOTAX,FOTTX,FOTSE, FOEND,FOSTR,FOETX,FOESE, FOLAB,FOLTX,FOMAX,FOMTX,
	FOBOF,FOBTX,FOOFF,FOOTH,FAXIS,
/*
.....Color Fields
*/
	FCOPE,FCOIS,FCOOP,FCOLS,FCOBT,FCOTP,FCOTA,FCOEN,FCOGT,FCOGE,
/*
.....Global Fields
*/
	FPOKM,
/*
.....Action item fields
*/
   FAPV, FAPY, FARS, FAPB, FAVE, FAGE, FAVW, FVIDEO
};
/*
.....Defined values
*/
#define HIDE_KEY 0
#define CONTOUR_KEY 1

#define IN 0
#define OUT 1
#define ON 2
#define OFFST 3

#define DIS 0
#define PLN 1

#define DEFALT 0
#define ELEM 1
#define STPNT 1
#define POINT 2

#define PSNORM 1
#define SECPS 2

typedef struct
{
	UU_KEY_ID key;
	char label[NCL_MAX_LABEL];
	int start;
	int end;
	int total;
	int *colors;
	int *lintyp;
} Sopen_struc;

int taxis5;

/*
.....Global variables
*/
int NCL_lace_flag=0;
UU_LOGICAL NCL_pockform_active = UU_FALSE;
UU_KEY_ID Sopen_key;
extern int NAUTIPV;
extern UD_METHOD UD_initfrm_intry;
/*
.....Section definitions
*/
enum {Boundaries, Levels, Options, Colors};
static char *Smode[]={"Boundaries", "Levels", "Options", "Colors"};
static int Sred[3]={180,0,0}, Syellow[3]={180,148,0}, Sgreen[3]={0,180,0};
static int Sblack[3]={0,0,0};
static int *Ssecol[]={Sblack,Sblack,Sblack,Sblack};
/*
.....Main variables
*/
static int Sfrm0,Sfrma;
static int Sclick_pokmod,Sperim_prompt;
static int SfrmPlay=0;
static int Serrflg,Spreviewflg,Schgmade;
static UU_LOGICAL Sform_init = UU_FALSE;
static UU_LOGICAL Spokmod_saved = UU_FALSE;
static UU_LOGICAL Spsthk,Sdsthk,Soffthk,Sopenthk;
static UU_LOGICAL Sislace = UU_FALSE;
unsigned *Sperim_mask;
static NCLU_pokmod Smodals;
static UN_motseg *Smptr=0,Smotatt;
static UN_mot_vpbox_struc Smvpbox[UV_NVPORTS];
static UU_LIST Skey_list;
static int Snkeys;
static UU_LOGICAL Skey_init;
/*
.....Boundary variables
*/
static int Sdirmodp,Tdirmodp,Sislperim,Tislperim,Sbotperim,Tbotperim;
static int Sdirmodi,Tdirmodi,Spic_ans;
static char Sperim[STRL],Tperim[STRL];
static char Sds_thk[STRL],Tds_thk[STRL],Sopen_thk[STRL],Topen_thk[STRL];
static char Sisle[STRL],Tisle[STRL];
/*
.....Level variables
*/
static int Stoptype,Ttoptype;
static char Spoc_bot[STRL],Tpoc_bot[STRL],Sps_thk[STRL],Tps_thk[STRL];
static char Spoc_top[STRL],Tpoc_top[STRL];
/*
.....Option variables
*/
static int Stamode,Ttamode,Spockend,Tpockend,Slabel,Tlabel,Smaxloop,Tmaxloop;
static int Sfinthk,Tfinthk,Soffpart,Toffpart;
static char Ssec_ps[STRL],Tsec_ps[STRL],Spoc_end[STRL],Tpoc_end[STRL];
static char Spoc_lab[STRL],Tpoc_lab[STRL],Smax_loop[STRL],Tmax_loop[STRL];
static char Sfin_thk[STRL],Tfin_thk[STRL],Soff_thk[STRL],Toff_thk[STRL];

static int Saxis5;
static int Taxis5;

static int Sacc[4];

/*
.....Color variables
*/
static int Scolorp,Tcolorp,Scolorop,Tcolorop,Slintyp,Tlintyp,Scolori,Tcolori;
static int Scolorb,Tcolorb,Scolort,Tcolort,Scolors,Tcolors,Scolore,Tcolore;
static int Tgeotog,Tgeocol;
static UU_LOGICAL Sgeo_redraw;
/*
.....Geometry variables
*/
static int Snperim = 0,Snisles = 0;
static UU_LOGICAL Sallsf = UU_FALSE;
static UU_LOGICAL Sgeo_init[2],Sgeo_apply=UU_FALSE;
static UU_LIST Splst,Silst;
static UM_sgeo Sptop,Spbot,Spend,Spsec,Sgperim,Sgisle;
static Sopen_struc Sopen;
/*
.....Open side variables
*/
static UU_LOGICAL Sind_init = UU_FALSE;
static UU_LIST Sindlst;
static int Sopentyp;
/*
.....Callback routines
*/
static UD_FSTAT OnBnTog(),OnBnTxt(),OnBnSel(),OnLvTog(),OnLvTxt(),OnLvSel();
static UD_FSTAT OnOpTog(),OnOpTxt(),OnOpSel(),OnCoTog(),OnPokMod(),OnAction(),OnVideo();
static UD_FSTAT OnPicToga(),OnClosea();
/*
.....Local routines
*/
static void S_save_form(),S_init_geo(),S_push_geo(),S_enable_buttons();
static UD_FSTAT S_enter_form();
static void S_init_form(),S_init_traverse(),S_form_invis(),S_form_vis();
static void S_hilite_entities(),S_hilite_entity(),S_unhilite_all();
static void S_unhilite_entities(),S_unhilite_entity(),S_update_answers();
static void S_section_changed(),S_select_open(),S_create_key_list();
static void S_add_key();
static int S_select_geo(),S_build_command();
/*
.....Global routines
*/
void ncl_pocket_draw_open(),ncl_push_indlst();
/*
.....Form answers
*/
static int *Sanswers[] = {
	UU_NULL,&Tdirmodp,(int *)Tperim, UU_NULL,UU_NULL,
		&Tislperim, &Tbotperim, UU_NULL,&Tdirmodi,(int *)Tisle,
		(int *)Tds_thk,(int *)Topen_thk, UU_NULL,

	UU_NULL,(int *)Tpoc_bot, (int *)Tps_thk, &Ttoptype,(int *)Tpoc_top,UU_NULL,

	&Ttamode,(int *)Tsec_ps,UU_NULL, &Tpockend,&Tpockend,(int *)Tpoc_end,UU_NULL,
		&Tlabel,(int *)Tpoc_lab,&Tmaxloop,(int *)Tmax_loop,
		&Tfinthk,(int *)Tfin_thk, &Toffpart,(int *)Toff_thk,  &Taxis5,

	&Tcolorp,&Tcolori,&Tcolorop,&Tlintyp,&Tcolorb,&Tcolort,&Tcolors,&Tcolore,
		&Tgeotog,&Tgeocol,

	UU_NULL,
	
	UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL};

/*********************************************************************
**    E_FUNCTION     : nclu_pocket_advanced()
**       Controlling routine for the pocket form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_pocket_advanced()
{
	static char PLZ[] = "(PL/0,0,1,0)";
	UU_LOGICAL cmdreject;
	UU_REAL rnum;
	int i,flag;
	UM_sgeo *geo;
	UM_int2 pclik,dirp,diri,ttyp,etyp,eelm,lofp,ltyp,lops,ifl;
	UM_real4 tdis,fthk;
	UM_real8 thk;
	UD_METHOD save_entry;

	extern char ffnam[520];
	char inpf[520];
	char* exte;

	//char* exte;
/*
.....Set up form fields
*/
	static char traverse[] = {
		1,1,1, 1,1, 1, 1, 1,1,1, 1,1, 0,
		1,1, 1, 1,1,1,
		1,1,1, 1,0,1,1, 1,1,1,1, 1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,
		1,
		1,1,1,1,1,1,1,1,
		1};
	static char display[] = {
		1,1,1, 1,1, 1, 1, 1,1,1, 1,1, 0,
		1,1, 1, 1,1,1,
		1,1,1, 1,0,1,1, 1,1,1,1, 1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,
		1,
		1,1,1,1,1,1,1,1,
		1};
	static char called[] = {
		6,6,6, 6,6, 6, 6, 6,6,6, 6,6, 6,
		6,6, 6, 6,6,6,
		6,6,6, 6,6,6,6, 6,6,6,6, 6,6,6,6,
		6,6,6,6,6,6,6,6,6,6,
		6,
		6,6,6,6,6,6,6,6,
		6};

	static UD_METHOD methods[] = {
		OnBnSel,OnBnTog,OnBnTxt, OnBnSel,OnBnSel, OnBnTog, OnBnTog,
			OnBnSel,OnBnTog,OnBnTxt, OnBnTxt,OnBnTxt, OnBnTog,
		OnLvSel,OnLvTxt, OnLvTxt, OnLvTog,OnLvTxt,OnLvSel,
		OnOpTog,OnOpTxt,OnOpSel, OnOpTog,OnOpTog,OnOpTxt,OnOpSel,
			OnOpTog,OnOpTxt,OnOpTog,OnOpTxt, OnOpTog,OnOpTxt, OnOpTog,OnOpTxt, OnOpTog,OnOpTxt,
		OnCoTog,OnCoTog,OnCoTog,OnCoTog,OnCoTog,OnCoTog,OnCoTog,OnCoTog,OnCoTog,
			OnCoTog,
		OnPokMod,
		OnAction,OnAction,OnAction,OnAction,OnAction,OnAction,OnAction,OnVideo};

/*
.....Initialize routine
*/
	if (NCL_pockform_active) goto done;
	NCL_pockform_active = UU_TRUE;
	Spokmod_saved = UU_FALSE;
	Sclick_pokmod = UU_FALSE;
	Serrflg = 0;
	Spreviewflg = 0;
	Schgmade = 0;
	save_entry = UD_initfrm_intry;
/*
......always initial to -1, any number >= 0 is a valid form number
*/
	Sfrma = -1;
	Sperim_prompt = 0;
	Sperim_mask = UU_NULL;
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0) goto done;
/*
.....Initialize the form parameters
*/
	S_init_form();
/*
.....Initialize field traversals
*/
	S_init_traverse(display,traverse);
/*
.....Get the Form input
*/
repeat:;
	NCL_preview_mot = 1;
	UD_initfrm_intry = S_enter_form;
/*
......Sfrm0 is always be 0 unless return failure since it is the main MODAL form, 
......(it will not return assign value until form closed), but we need this value for callback function
*/
	Sfrm0 = 0;
	Sfrm0 = ud_form1("apocket.frm",Sanswers,Sanswers,methods,called,display,
		traverse);
	UD_initfrm_intry = save_entry;
/*
.....Erase last previewed motion
*/
	if (Smptr != UU_NULL && (!Spreviewflg || Schgmade || Sfrm0 == -1))
		ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
	Smptr = UU_NULL;
/*
.....Free the motion settings created during preview if a change has been
.....made and ok was pressed or if cancel was pressed
*/
	if (Spreviewflg)
	{
		flag = UU_FALSE;
		if (!Schgmade && Sfrm0 != -1 && Serrflg == 0) flag = UU_TRUE;
		moinfr(&flag);
		if (!flag)
		{
			if (Spokmod_saved)
			{
				pmdrst();
				Spokmod_saved = UU_FALSE;
			}
			pokrst();
		}
	}

	exte = strstr(inpf,".pp");
	if (exte!=NULL)
	{
		if (Taxis5 == UU_TRUE /*&& allsf*/)
		{
			traverse[FAXIS] = UU_TRUE;	//comm out ? Sasha Apr18, 2021
			
			taxis5 =  UU_TRUE;
		}
		else
		{
			traverse[FAXIS] = UU_TRUE;
			
		}
	}
	else
		traverse[FAXIS] = UU_TRUE;

	NCL_pockform_active = UU_FALSE;
	if (Sfrm0 == -1) goto done;
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
	S_unhilite_all();
	if (Sgeo_init[0])
	{
		uu_list_free(&Splst); Snperim = 0; Sgeo_init[0] = UU_FALSE;
	}
	if (Sgeo_init[1])
	{
		uu_list_free(&Silst); Snisles = 0; Sgeo_init[1] = UU_FALSE;
	}
	ncl_pocket_draw_open(UU_TRUE);
	Sopentyp = 0;
	NCL_preview_mot = 0;
	UD_UNMARK(cmdreject);
	return;
}

/*********************************************************************
**    I_FUNCTION     :  OnBnTog(fieldno, val, stat)
**       Callback routine for toggle fields.
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
static UD_FSTAT OnBnTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int yesno;
	UU_LOGICAL ifl;
	static char traverse[]={0};
	static char display[]={0};
	static char called[]={6};
	static UD_METHOD methods[]={OnPicToga,OnClosea};
	static int *ans[]={&Spic_ans};

	switch (*fieldno)
	{
	case FBPED:
		if (Tdirmodp == OFFST)
		{
			ud_update_answer(FBISD,&Tdirmodp);
			if (Snperim > 0)
			{
				ud_set_traverse_mask(FBSRF,UU_FALSE);
				ud_set_traverse_mask(FLBOT,UU_FALSE);
				ud_set_traverse_mask(FLBTX,UU_FALSE);
				ud_set_traverse_mask(FBOTH,UU_TRUE);
				yesno = 1;
				ud_update_answer(FBSRF,&yesno);
			}
		}
		else
		{
			if (Snperim > 0)
			{
				ud_set_traverse_mask(FBSRF,UU_TRUE);
				ud_set_traverse_mask(FLBOT,UU_TRUE);
				ud_set_traverse_mask(FLBTX,UU_TRUE);
			}
			ud_set_traverse_mask(FBOTH,UU_FALSE);
		}
		if (Tdirmodp != Sdirmodp) Schgmade = UU_TRUE;
		break;
	case FBINR:
		ud_set_traverse_mask(FBISL,!Tislperim);
		ud_set_traverse_mask(FBITX,!Tislperim);
		if (Tislperim != Sislperim) Schgmade = UU_TRUE;
		break;
	case FBSRF:
		ifl = !(Tbotperim == UU_TRUE && Sallsf);
		ud_set_traverse_mask(FLBOT,ifl);
		ud_set_traverse_mask(FLBTX,ifl);
		if (Tbotperim != Sbotperim) Schgmade = UU_TRUE;
		break;
	case FBISD:
		if (Tdirmodi != Sdirmodi) Schgmade = UU_TRUE;
		break;
	case FBPIC:
		if (Sfrma != -1) break;
		Sfrma = ud_form_display1("apocketa.frm",ans,ans,methods,called,display,
			traverse);
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  OnBnTxt(fieldno, val, stat)
**       Callback routine for text fields.
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
static UD_FSTAT OnBnTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_REAL thk;

	switch (*fieldno)
	{
	case FBPTX:
		S_unhilite_entities(&Splst,&Sgeo_init[0]);
		ul_to_upper(Tperim);
		if (strcmp(Tperim,Sperim) != 0)
		{
			Schgmade = 1;
			S_init_geo(&Sgperim,Tperim,Scolorp);
			S_push_geo(&Splst,&Sgperim,&Sgeo_init[0]);
		}
		S_enable_buttons();
		break;
	case FBITX:
		S_unhilite_entities(&Silst,&Sgeo_init[1]);
		ul_to_upper(Tisle);
		if (strcmp(Tisle,Sisle) != 0)
		{
			Schgmade = 1;
			S_init_geo(&Sgisle,Tisle,Scolori);
			S_push_geo(&Silst,&Sgisle,&Sgeo_init[1]);
		}
		break;
	case FBTHK:
		ul_to_upper(Tds_thk);
		if (strcmp(Tds_thk,Sds_thk) != 0) Schgmade = 1;
		ncl_get_scalar_value(Tds_thk,&thk);
		if (thk != 0.) Sdsthk = UU_TRUE;
		else Sdsthk = UU_FALSE;
		break;
	case FBOTH:
		ul_to_upper(Topen_thk);
		if (strcmp(Topen_thk,Sopen_thk) != 0) Schgmade = 1;
		ncl_get_scalar_value(Topen_thk,&thk);
		if (thk != 0.) Sopenthk = UU_TRUE;
		else Sopenthk = UU_FALSE;
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  OnBnSel(fieldno, val, stat)
**       Routine to select Boundary geometry.
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
static UD_FSTAT OnBnSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status,prmpt;
	unsigned *mask;
/*
.....Select the perimeter
*/
	switch (*fieldno)
	{
	case FBPER:
		S_unhilite_entities(&Splst,&Sgeo_init[0]);
		ncl_pocket_draw_open(UU_TRUE);
		S_form_invis();
		mask = UD_pocket_perimeter;
		prmpt = 495;
		if (Sperim_prompt != 0)
		{
			prmpt = Sperim_prompt;
			mask = Sperim_mask;
			Sperim_prompt = 0;
			Sperim_mask = UU_NULL;
		}
		status = S_select_geo(&Sgperim,&Splst,mask,3,prmpt,Tcolorp,0,FBPTX,
			&Tperim,&Sgeo_init[0]);
		Snperim = UU_LIST_LENGTH(&Splst);
		if (Snperim > 0)
		{
			Sgeo_init[0] = UU_TRUE;
			nclu_mot_check_colors(&Splst,1,&Silst,1,&Tpoc_bot,0);
		}
		S_form_vis();
		S_enable_buttons();
		break;
/*
.....Select the open boundaries
*/
	case FBOPN:
		S_select_open();
		S_enable_buttons();
		break;
/*
.....Clear the open boundaries
*/
	case FBODE:
		ncl_pocket_draw_open(UU_TRUE);
		Sopenthk = UU_FALSE;
		ud_set_traverse_mask(FBOTH,UU_FALSE);
		break;
/*
.....Select the islands
*/
	case FBISL:
		S_unhilite_entities(&Silst,&Sgeo_init[1]);
		S_form_invis();
		mask = UD_pocket_perimeter;
		prmpt = 496;
		status = S_select_geo(&Sgisle,&Silst,mask,3,prmpt,Tcolori,0,FBISL,
			Tisle,&Sgeo_init[1]);
		Snisles = UU_LIST_LENGTH(&Silst);
		if (Snisles > 0)
		{
			Sgeo_init[1] = UU_TRUE;
			nclu_mot_check_colors(&Silst,1,&Splst,1,&Tpoc_bot,0);
		}
		S_form_vis();
		break;
	}
/*
.....Check for change since preview
*/
	if (status == UU_SUCCESS) Schgmade = 1;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  OnLvTog(fieldno, val, stat)
**       Callback routine for toggle fields.
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
static UD_FSTAT OnLvTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL ifl,chg;

	switch (*fieldno)
	{
	case FLTOP:
		ifl = !(Ttoptype == DIS);
		ud_set_traverse_mask(FLTSE,ifl);
		if (Ttoptype != Stoptype) Schgmade = chg = UU_TRUE;
		break;
	}
	if (chg)
	{
		Ssecol[Levels] = Sgreen;
		S_section_changed(Levels,UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  OnLvTxt(fieldno, val, stat)
**       Callback routine for text fields.
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
static UD_FSTAT OnLvTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status;
	UU_LOGICAL chg;
	UU_REAL thk;

	switch (*fieldno)
	{
	case FLBTX:
		S_unhilite_entity(&Spbot);
		ul_to_upper(Tpoc_bot);
		if (strcmp(Tpoc_bot,Spoc_bot) != 0)
		{
			Schgmade = chg = UU_TRUE;
			S_init_geo(&Spbot,Tpoc_bot,Tcolorb);
		}
		break;
	case FLBTH:
		ul_to_upper(Tps_thk);
		if (strcmp(Tps_thk,Sps_thk) != 0) Schgmade = chg = UU_TRUE;
		ncl_get_scalar_value(Tps_thk,&thk);
		if (thk != 0.) Spsthk = UU_TRUE;
		else Spsthk = UU_FALSE;
		break;
	case FLTTX:
		S_unhilite_entity(&Sptop);
		ul_to_upper(Tpoc_top);
		status = ncl_get_scalar_value(Tpoc_top,&thk);
		if (status == UU_SUCCESS) Ttoptype = DIS;
		else Ttoptype = PLN;
		ud_update_answer(FLTOP,&Ttoptype);
		ud_update_form(Sfrm0);
		if (strcmp(Tpoc_top,Spoc_top) != 0)
		{
			Schgmade = chg = UU_TRUE;
			S_init_geo(&Sptop,Tpoc_top,Tcolort);
		}
		break;
	}
	if (chg)
	{
		Ssecol[Levels] = Sgreen;
		S_section_changed(Levels,UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  OnLvSel(fieldno, val, stat)
**       Routine to select geometry.
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
static UD_FSTAT OnLvSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status,prmpt;
	unsigned *mask;
/*
.....Select the perimeter
*/
	switch (*fieldno)
	{
	case FLBOT:
		S_unhilite_entity(&Spbot);
		mask = UD_ncl_allsfpl;
		prmpt = 493;
		status = S_select_geo(&Spbot,UU_NULL,mask,0,prmpt,Tcolorb,0,FLBTX,
			&Tpoc_bot,UU_NULL);
		nclu_mot_check_colors(&Spbot,0,&Splst,1,&Silst,1);
		break;
	case FLTSE:
		S_unhilite_entity(&Sptop);
		mask = UD_ncl_allsfpl;
		prmpt = 415;
		status = S_select_geo(&Sptop,UU_NULL,mask,0,prmpt,Tcolort,0,FLTTX,
			&Tpoc_top,UU_NULL);
		if (status == UU_SUCCESS)
		{
			Ttoptype = PLN;
			ud_update_answer(FLTOP,&Ttoptype);
		}
		break;
	}
/*
.....Check for change since preview
*/
	if (status == UU_SUCCESS)
	{
		Schgmade = 1;
		Ssecol[Levels] = Sgreen;
		S_section_changed(Levels,UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  OnOpTog(fieldno, val, stat)
**       Callback routine for toggle fields.
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
static UD_FSTAT OnOpTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL ifl,chg;
	int status;

	extern char ffnam[520];
	char inpf[520];
	char* exte;

	switch (*fieldno)
	{
	case FOTAX:
		if (Ttamode == SECPS)
		{
			ud_set_traverse_mask(FOTTX,UU_TRUE);
			ud_set_traverse_mask(FOTSE,UU_TRUE);
		}
		else
		{
			ud_set_traverse_mask(FOTTX,UU_FALSE);
			ud_set_traverse_mask(FOTSE,UU_FALSE);
		}
		if (Ttamode != Stamode) Schgmade = chg = UU_TRUE;
		break;
	case FOEND:
		ifl = (Tpockend == POINT);
		ud_set_traverse_mask(FOESE,ifl);
		ifl = !(Tpockend == DEFALT);
		ud_set_traverse_mask(FOETX,ifl);
		if (Tpockend != Spockend) Schgmade = chg = UU_TRUE;
		break;
	case FOSTR:
		ifl = (Tpockend == STPNT);
		ud_set_traverse_mask(FOESE,ifl);
		ifl = !(Tpockend == DEFALT);
		ud_set_traverse_mask(FOETX,ifl);
		if (Tpockend != Spockend) Schgmade = chg = UU_TRUE;
		break;
	case FOMAX:
		ud_set_traverse_mask(FOMTX,Tmaxloop);
		if (Tmaxloop != Smaxloop) Schgmade = chg = UU_TRUE;
		break;
	case FOLAB:
		ud_set_traverse_mask(FOLTX,Tlabel);
		if (Tlabel != Slabel) Schgmade = chg = UU_TRUE;
		break;
	case FOBOF:
		ud_set_traverse_mask(FOBTX,Tfinthk);
		if (Tfinthk != Sfinthk) Schgmade = chg = UU_TRUE;
		break;
	case FOOFF:
		ud_set_traverse_mask(FOOTH,Toffpart);
		if (Toffpart != Soffpart) Schgmade = chg = UU_TRUE;
		break;
		/*
	case FAXIS:
		ud_set_traverse_mask(FAXIS,Taxis5);
		if (Taxis5 != Saxis5) Schgmade = chg = UU_TRUE;
		break;
		*/
	case FAXIS:
		exte = strstr(inpf,".pp");
		if (exte!=NULL)
		{
			if (Taxis5 == UU_TRUE /*&& allsf*/)
			{
				ud_set_traverse_mask(FAXIS,UU_TRUE);	// comm out Sasha, Apr.18, 2021
				status = afive_axis_output();
				taxis5 =  UU_TRUE;
				S_build_command(UU_FALSE);
			}
			else
			{
				ud_set_traverse_mask(FAXIS,UU_TRUE);
			}
			if (Saxis5 != Taxis5)
			{
				if (Spreviewflg) Schgmade = 1;
				Saxis5 = Taxis5;
				Sacc[Options] = 1;
			}
		}
		else
			ud_set_traverse_mask(FAXIS,UU_TRUE);

		break;
	default:
		break;
	}
	if (chg)
	{
		Ssecol[Options] = Sgreen;
		S_section_changed(Options,UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  OnOpTxt(fieldno, val, stat)
**       Callback routine for text fields.
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
static UD_FSTAT OnOpTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL chg;
	UU_REAL thk;

	switch (*fieldno)
	{
	case FOTTX:
		S_unhilite_entity(&Spsec);
		ul_to_upper(Tsec_ps);
		if (strcmp(Tsec_ps,Ssec_ps) != 0)
		{
			Schgmade = chg = UU_TRUE;
			S_init_geo(&Spsec,Tsec_ps,Tcolors);
		}
		break;
	case FOETX:
		S_unhilite_entity(&Spend);
		ul_to_upper(Tpoc_end);
		if (strcmp(Tpoc_end,Spoc_end) != 0)
		{
			Schgmade = chg = UU_TRUE;
			S_init_geo(&Spend,Tpoc_end,Tcolore);
		}
		break;
	case FOLTX:
		ul_to_upper(Tpoc_lab);
		if (strcmp(Tpoc_lab,Spoc_lab) != 0) Schgmade = chg = UU_TRUE;
		if (strlen(Tpoc_lab) != 0)
		{
			Tlabel = UU_TRUE;
			ud_update_answer(FOLAB,&Tlabel);
			ud_update_form(Sfrm0);
		}
		break;
	case FOMTX:
		ul_to_upper(Tmax_loop);
		if (strcmp(Tmax_loop,Smax_loop) != 0) Schgmade = chg = UU_TRUE;
		break;
	case FOBTX:
		ul_to_upper(Tfin_thk);
		if (strcmp(Tfin_thk,Sfin_thk) != 0) Schgmade = chg = UU_TRUE;
		Tfinthk = UU_TRUE;
		ud_update_answer(FOBOF,&Tfinthk);
		break;
	case FOOTH:
		ncl_get_scalar_value(Toff_thk,&thk);
		if (thk != 0.) Soffthk = UU_TRUE;
		else Soffthk = UU_FALSE;
		if (strcmp(Toff_thk,Soff_thk) != 0) Schgmade = chg = UU_TRUE;
		Toffpart = UU_TRUE;
		ud_update_answer(FOOFF,&Toffpart);
		ud_update_form(Sfrm0);
	}
	if (chg)
	{
		Ssecol[Options] = Sgreen;
		S_section_changed(Options,UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  OnOpSel(fieldno, val, stat)
**       Routine to select geometry.
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
static UD_FSTAT OnOpSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status,prmpt;
	unsigned *mask;
/*
.....Select the geometry
*/
	switch (*fieldno)
	{
	case FOTSE:
		S_unhilite_entity(&Spsec);
		mask = UD_ncl_allsfpl;
		prmpt = 415;
		status = S_select_geo(&Spsec,UU_NULL,mask,0,prmpt,Tcolors,0,FOTTX,
			&Tsec_ps,UU_NULL);
		if (status == UU_SUCCESS)
		{
			Ttamode = SECPS;
			ud_update_answer(FOTAX,&Ttamode);
			ud_update_form(Sfrm0);
		}
		break;
	case FOESE:
		S_unhilite_entity(&Spend);
		mask = UD_ncl_pt;
		prmpt = 497;
		status = S_select_geo(&Spend,UU_NULL,mask,0,prmpt,Tcolore,0,FOETX,
			&Tpoc_end,UU_NULL);
		break;
	}
/*
.....Check for change since preview
*/
	if (status == UU_SUCCESS)
	{
		Schgmade = 1;
		Ssecol[Levels] = Sgreen;
		S_section_changed(Levels,UU_TRUE);
	}
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
	case FCOPE:
		if (Sgeo_init[0])
			ud_dispfrm_set_attribs(0,FBPER,UM_BLACK,Tcolorp);
		S_hilite_entities(&Splst,Tcolorp);
		break;
	case FCOOP:
	case FCOLS:
		ncl_pocket_draw_open(UU_FALSE);
		break;
	case FCOIS:
		if (Sgeo_init[1])
			ud_dispfrm_set_attribs(0,FBISL,UM_BLACK,Tcolori);
		S_hilite_entities(&Silst,Tcolori);
		break;
	case FCOBT:
		S_hilite_entity(&Spbot);
		break;
	case FCOTP:
		S_hilite_entity(&Sptop);
		break;
	case FCOTA:
		S_hilite_entity(&Spsec);
		break;
	case FCOEN:
		S_hilite_entity(&Spend);
		break;
	case FCOGT:
		Tgeotog = val->frmint[0];
	case FCOGE:
		if (Sgeo_redraw)
		{
			Sgeo_redraw = UU_FALSE;
			fno = FAGE;
			OnAction(&fno,val,stat);
		}
		break;
	}
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
**       Method called when an Action button is pressed.
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
........Output command
*/
		status = S_build_command(UU_FALSE);
		if (status != UU_SUCCESS || Serrflg != 0)
		{
/*
........Delete the temp cl file if no curve was selected
........since no cmd call was made
........The temp cl file needs to be deleted so that
........if another preview occurs or OK is pressed
........the pointers are at the right locations after the error from
........no curves being selected
*/
			moinfr(&flag);
		}
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
.....A perimeter must be selected
*/
	case FAPY:
		if (!Sgeo_init[0])
		{
			ud_wrerr("You must select a Pocket Perimeter.");
			break;
		}
		if (Spreviewflg)
		{
			if (!Schgmade && Serrflg == 0) flag = UU_TRUE;
			else flag = UU_FALSE;
			moinfr(&flag);
			if (!flag)
			{
				if (Spokmod_saved)
				{
					pmdrst();
					Spokmod_saved = UU_FALSE;
				}
				pokrst();
			}
		}
/*
........Output command
*/
		status = S_build_command(UU_TRUE);
		Spreviewflg = UU_FALSE;
		Smptr = UU_NULL;
		if (status != UU_SUCCESS || Serrflg != 0) break;
		S_unhilite_entities(&Splst,&Sgeo_init[0]);
		S_unhilite_entities(&Silst,&Sgeo_init[1]);
		Sgeo_apply = UU_TRUE;
		for (i=FAPV;i<FAVW;i++) ud_set_traverse_mask(i,0);
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
		if (Smptr != UU_NULL) ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
		Smptr = UU_NULL;
		if (Spreviewflg) moinfr(&flag);
		S_init_form();
		S_enter_form(fieldno,val,stat);
		for (i=FAPV;i<FAVW;i++) ud_set_traverse_mask(i,0);
		S_update_answers();
		Schgmade = Spreviewflg = UU_FALSE;
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
**    I_FUNCTION     :  OnPokMod(fieldno, val, stat)
**       Routine to display the Pocket Modals form.
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
static UD_FSTAT OnPokMod (fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	nclu_pokmod(&Smodals, UU_FALSE, Sclick_pokmod);
	Sclick_pokmod = UU_TRUE;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnPicToga(fieldno,val,stat)
**       Method called when a picture field is changed.
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
static UD_FSTAT OnPicToga(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int fno;

	switch (*fieldno)
	{
	case 0:
		ud_close_dispfrm(Sfrma);
		Sfrma = -1;
		switch (Spic_ans)
		{
/*
.....Trimmed surface (OFFSET)
*/
		case 0:
			Tdirmodp = OFFST;
			ud_update_answer(FBPED,&Tdirmodp);
			Tislperim = 1;
			ud_update_answer(FBINR,&Tislperim);
			Tbotperim = 1;
			ud_update_answer(FBSRF,&Tbotperim);
			Sperim_prompt = 504;
			Sperim_mask = UD_ncl_trimsf;
			break;
/*
.....Surface
*/
		case 1:
			Tdirmodp = IN;
			ud_update_answer(FBPED,&Tdirmodp);
			Tislperim = 1;
			ud_update_answer(FBINR,&Tislperim);
			Tbotperim = 1;
			ud_update_answer(FBSRF,&Tbotperim);
			Sperim_prompt = 504;
			Sperim_mask = UD_ncl_allsf;
			break;
/*
.....Closed curve
*/
		case 2:
			fno = FBODE;
			OnBnTog(&fno,val,stat);
			Tdirmodp = IN;
			ud_update_answer(FBPED,&Tdirmodp);
			Sperim_prompt = 505;
			Sperim_mask = UD_ncl_allcv;
			break;
/*
.....Open curve
*/
		case 3:
			Tdirmodp = IN;
			ud_update_answer(FBPED,&Tdirmodp);
			Sperim_prompt = 505;
			Sperim_mask = UD_ncl_allcv;
			break;
		}
/*
.....Get the pocket perimeter
*/
		fno = FBPER;
		OnBnSel(&fno,val,stat);
/*
.....Get the open sides
*/
		if (Spic_ans == 3)
		{
			fno = FBOPN;
			OnBnSel(&fno,val,stat);
		}
/*
.....Update the form
*/
		ud_update_form(Sfrm0);
/*
.....have to let the form know there is no need the update the form anyway
.....in this case, form already closed
*/
		*fieldno = -1;
		return(UD_FRMCLOSE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  OnClosea()
**       Method called when Colors form is closed.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnClosea()
{
	Sfrma = -1;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_init_form()
**       Initializes the form variables.
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
	static char PLZ[] = "(PL/0,0,1,0)";
	int i;
	UU_REAL rnum;
	UM_int2 pclik,dirp,diri,ttyp,etyp,eelm,lofp,ltyp,lops,ifl;
	UM_real4 tdis,fthk;
	UM_real8 thk;
/*
.....Get the Pocket Modals
*/
	nclu_pokpar (&Smodals);
	pokfrm (&pclik,&dirp,&diri,&ttyp,&tdis,&etyp,&eelm,&lofp,&ltyp,&lops,&fthk);
/*,
..... Initialize Boundaries
*/
	if (!Sform_init)
	{
		Sform_init = UU_TRUE;
		Scolorp = NCLX_BROWN;
		Scolori = NCLX_TAN;
		Scolorb = NCLX_LT_BLUE;
		Scolort = NCLX_SEA_GREEN;
		Scolors = NCLX_PURPLE;
		Scolore = NCLX_ORANGE;
		Scolorop = NCLX_WHITE;
		Slintyp = 7;
		Sislperim = Sbotperim = UU_FALSE;
		strcpy (Spoc_bot,PLZ);
		Slabel = UU_FALSE;
		Spoc_lab[0] = '\0';
	}
	Sfinthk = Soffpart = Spsthk = Sdsthk = Soffthk = Sopenthk = UU_FALSE;
	strcpy(Sfin_thk, "0.0");
	strcpy(Sopen_thk, "0.0");
	Stamode = DEFALT;
	Ssec_ps[0] = '\0';
	if (!pclik)
	{
		Sdirmodp = IN; Sdirmodi = OUT;
		Stoptype = DIS;
		rnum = 0.;
		ncl_sprintf (Spoc_top,&rnum,1);
		Spockend = Stamode = DEFALT;
		Spoc_end[0] = '\0';
		Smaxloop = UU_FALSE;
		strcpy(Smax_loop, "0");
	}
	else
	{
		Sdirmodp = dirp; Sdirmodi = diri;
		Stoptype = ttyp;
		rnum = tdis;
		ncl_sprintf (Spoc_top,&rnum,1);
		Spockend = etyp;
		if (Spockend == ELEM)
		{
			i = eelm; sprintf(Spoc_end,"%d",i);
		}
		else
			Spoc_end[0] = '\0';

		Smaxloop = ltyp;
		if (Smaxloop)
		{
			i = lops; sprintf (Smax_loop,"%d",i);
		}
		else
			strcpy(Smax_loop, "0");

		if (lofp == 1) Soffpart = UU_TRUE;
		if (fthk != 0.)
		{
			Sfinthk = UU_TRUE;
			rnum = fthk;
			ncl_sprintf (Sfin_thk,&rnum,1);
		}
	}

	ifl = 23; getsc(&ifl,&thk);
	rnum = thk;
	ncl_sprintf (Sps_thk,&rnum,1);
	if (fabs(rnum) > UM_FUZZ) Spsthk = UU_TRUE;

	ifl = 24; getsc(&ifl,&thk);
	rnum = thk;
	ncl_sprintf (Sds_thk,&rnum,1);
	if (fabs(rnum) > UM_FUZZ) Sdsthk = UU_TRUE;
	
	ifl = 202; getsc(&ifl,&thk);
	rnum = thk;
	ncl_sprintf (Soff_thk,&rnum,1);
	if (fabs(rnum) > UM_FUZZ) Soffthk = UU_TRUE;

	nclf_genpocket_get_offthk(&thk);
	rnum = thk;
	ncl_sprintf (Sopen_thk,&rnum,1);

	uu_list_init(&Splst,sizeof(UM_sgeo),10,10);
	uu_list_init(&Silst,sizeof(UM_sgeo),10,10);
	Snperim = Snisles = 0;
/*
.....Store Boundary fields
*/
	Tdirmodp = Sdirmodp;
	strcpy(Tperim,Sperim);
	Tislperim = Sislperim;
	Tbotperim = Sbotperim;
	Tdirmodi = Sdirmodi;
	strcpy(Tds_thk,Sds_thk);
	strcpy(Topen_thk,Sopen_thk);
/*
.....Store Level fields
*/
	strcpy(Tpoc_bot,Spoc_bot);
	strcpy(Tps_thk,Sps_thk);
	Ttoptype = Stoptype;
	strcpy(Tpoc_top,Spoc_top);
/*
.....Store Option fields
*/
	Ttamode = Stamode;
	strcpy(Tsec_ps,Ssec_ps);
	Tpockend = Spockend;
	strcpy(Tpoc_end,Spoc_end);
	Tlabel = Slabel;
	strcpy(Tpoc_lab,Spoc_lab);
	Tmaxloop = Smaxloop;
	strcpy(Tmax_loop,Smax_loop);
	Tfinthk = Sfinthk;
	strcpy(Tfin_thk,Sfin_thk);
	Toffpart = Soffpart;
	strcpy(Toff_thk,Soff_thk);
/*
.....Store color fields
*/
	Tcolorp = Scolorp;
	Tcolorop = Scolorop;
	Tlintyp = Slintyp;
	Tcolori = Scolori;
	Tcolorb = Scolorb;
	Tcolort = Scolort;
	Tcolors = Scolors;
	Tcolore = Scolore;
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
.....Initialize geometry variables
*/
	Sgeo_init[0] = Sgeo_init[1] = 0.;
	Sptop.color = Spbot.color = Spend.color = Spsec.color = -1;
	Sptop.key = Spbot.key = Spend.key = Spsec.key = -1;
	Sptop.label[0] = Spbot.label[0] = Spend.label[0] = Spsec.label[0] = -1;
	S_init_geo(&Sptop,Tpoc_top,Tcolort);
	S_init_geo(&Spbot,Tpoc_bot,Tcolorb);
	S_init_geo(&Spend,Tpoc_end,Tcolore);
	S_init_geo(&Spsec,Tsec_ps,Tcolors);
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
**    I_FUNCTION     : S_init_traverse()
**       Initializes the form field traverse flags.
**    PARAMETERS
**       INPUT  :
**          display     Field display settings.
**          traverse    Field traverse settings.
**       OUTPUT :
**          display     Updated field display settings.
**          traverse    Updated field traverse settings.
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_init_traverse(display,traverse)
char *traverse,*display;
{
	int i;
/*
.....Initialize Boundary fields
*/
	traverse[FBOPN] = UU_FALSE;
	traverse[FBODE] = UU_FALSE;
	traverse[FBINR] = UU_FALSE;
	traverse[FBSRF] = UU_FALSE;
	traverse[FBISL] = !(Tislperim);
	traverse[FBITX] = !(Tislperim);
/*
.....Initialize Level fields
*/
	traverse[FLTSE] = !(Ttoptype == DIS);
/*
.....Initialize Option fields
*/
	traverse[FOTTX] = traverse[FOTSE] = UU_FALSE;
	Sislace = (NCL_lace_flag > 0);
	if (Sislace)
	{
		traverse[FOEND] = display[FOEND] = UU_FALSE;
		traverse[FOSTR] = display[FOSTR] = UU_TRUE;
		traverse[FOOFF] = traverse[FOBOF] = traverse[FOBTX] = 
		traverse[FOMAX] = traverse[FOMTX] = UU_FALSE;
	}
	else
	{
		traverse[FOEND] = display[FOEND] = UU_TRUE;
		traverse[FOSTR] = display[FOSTR] = UU_FALSE;
		traverse[FOOFF] = traverse[FOBOF] = traverse[FOMAX] = UU_TRUE;
		traverse[FOMTX] = (Smaxloop)? UU_TRUE: UU_FALSE;
		traverse[FOBTX] = (Sfinthk)? UU_TRUE: UU_FALSE;
	}
	traverse[FOTTX] = traverse[FOTSE] = (Stamode == SECPS);

	traverse[FOETX] = !(Spockend == DEFALT);
	traverse[FOESE] = (Spockend == POINT);
	traverse[FOLTX] = (Slabel);
	if (Sdirmodp == OFFST)
	{
		traverse[FBOTH] = UU_TRUE;
//		traverse[FBSRF] = UU_FALSE;
		traverse[FLBOT] = UU_FALSE;
		traverse[FLBTX] = UU_FALSE;
		Sbotperim = 1;
	}
	else
	{
		traverse[FBOTH] = UU_FALSE;
//		traverse[FBSRF] = UU_TRUE;
		traverse[FLBOT] = UU_TRUE;
		traverse[FLBTX] = UU_TRUE;
		Sbotperim = 0;
	}
	if (Sbotperim == UU_TRUE && Sallsf)
	{
		traverse[FLBOT] = UU_FALSE;
		traverse[FLBTX] = UU_FALSE;
	}
	else
	{
		traverse[FLBTX] = UU_TRUE;
		traverse[FLBOT] = UU_TRUE;
	}
	if (Soffpart) traverse[FOOTH] = UU_TRUE;
	else traverse[FOOTH] = UU_FALSE;
/*
.....Action item fields
*/
	for (i=FAPV;i<FAVW;i++) traverse[i] = 0;
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
	int i,ifl;
/*
.....Initial entry
.....Set standard colors
*/
	if (!Serrflg)
	{
		Ssecol[Boundaries] = Sred;
		Ssecol[Levels] = Sblack;
		Ssecol[Options] = Sblack;
/*
.....Set section colors
*/
		S_section_changed(Boundaries,UU_FALSE);
		S_section_changed(Levels,UU_FALSE);
		S_section_changed(Options,UU_FALSE);
	}
	else
		S_enable_buttons();
/*
.....Set the pick buttons to correct color
*/
	ud_dispfrm_set_attribs(0,FBISL,UM_BLACK,Tcolori);
	ud_dispfrm_set_attribs(0,FLBOT,UM_BLACK,Tcolorb);
	ud_dispfrm_set_attribs(0,FLTSE,UM_BLACK,Tcolort);
	ud_dispfrm_set_attribs(0,FOTSE,UM_BLACK,Tcolors);
	ud_dispfrm_set_attribs(0,FOESE,UM_BLACK,Tcolore);
/*
.....Set the mandatory fields to red
*/
	if (UU_LIST_LENGTH(&Splst) == 0) 
	{
		ud_dispfrm_set_attribs(0,FBPER,UM_WHITE,UM_RED);
		ud_frm_enable_ok(UU_FALSE);
	}
	else
		ud_dispfrm_set_attribs(0,FBPER,UM_BLACK,Tcolorp);
/*
.....End of routine
*/
	Serrflg = UU_FALSE;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_save_form()
**       Saves the form variables.
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
.....Save the settings
*/
	Scolorp = Tcolorp;
	Scolori = Tcolori;
	Scolorb = Tcolorb;
	Scolort = Tcolort;
	Scolors = Tcolors;
	Scolore = Tcolore;
	Scolorop = Tcolorop;
	Slintyp = Tlintyp;

	UN_unused_geo_flag = Tgeotog;
	UN_unused_geo_color = Tgeocol;
	nclu_save_preview_modals();
}

/*********************************************************************
**    I_FUNCTION     : S_form_invis()
**       Takes down all forms.
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
**       Redisplays all forms.
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
	S_unhilite_entities(&Splst,&Sgeo_init[0]);
	S_unhilite_entities(&Silst,&Sgeo_init[1]);
	S_unhilite_entity(&Spbot);
	S_unhilite_entity(&Sptop);
	S_unhilite_entity(&Spend);
	S_unhilite_entity(&Spsec);
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
static void S_unhilite_entities(sflst,geo_init)
UU_LIST *sflst;
UU_LOGICAL *geo_init;
{
	int i;
	UM_sgeo *geo;
/*
.....Loop through list
*/
	if (*geo_init == UU_TRUE)
	{
		geo = (UM_sgeo *)UU_LIST_ARRAY(sflst);
		for (i=0;i<UU_LIST_LENGTH(sflst);i++) S_unhilite_entity(&geo[i]);
		uu_list_free(sflst);
		*geo_init = UU_FALSE;
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
.....Highlight entity
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
	ud_form_section_color(0,Smode[section],Ssecol[section],bold);
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
	n = UU_LIST_LENGTH(&Splst);
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(&Splst);
	for (i=0;i<n;i++) S_add_key(which,mgeo[i].key);

	n = UU_LIST_LENGTH(&Silst);
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(&Silst);
	for (i=0;i<n;i++) S_add_key(which,mgeo[i].key);
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
**    I_FUNCTION     : S_enable_buttons()
**			Determines which fields should be enabled and which buttons
**       marked as necessary, but unfulfilled based on the Part, Drive,
**       and Check surface selections.
**    PARAMETERS   
**       INPUT  :
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_enable_buttons()
{
	int i,relnum,nc,isub;
	UU_LOGICAL ifl,ifl1,ifl2,nsf,ntrim;
	UM_sgeo *sgeo;
	struct NCL_trimsf_rec tsf;
	static char *picture[]={"Advanced_Pocket.jpg","Advanced_Pocket_Trim_Surf.jpg",
	    "Advanced_Pocket_Surf.jpg","Advanced_Pocket_ClosedCurve.jpg",
		"Advanced_Pocket_OpenSideCurve.jpg"};
/*
.....Determine type of Pocket Perimeter selected
*/
	sgeo = (UM_sgeo *)UU_LIST_ARRAY(&Splst);
	Sallsf = UU_TRUE;
	Sopentyp = 0;
/*
.....Determine if Islands can be picked
*/
	for (i=0;i<Snperim;i++)
	{
		ur_retrieve_data_relnum(sgeo[i].key,&relnum);
		if (sgeo[i].relnum == NCL_POINT_REL ||
			sgeo[i].relnum == UM_POINT_REL || sgeo[i].relnum == UM_COMPCRV_REL)
		{
			Sopentyp++; Sopen_key = sgeo[i].key;
			ud_set_traverse_mask(FBOPN,UU_TRUE);
			ud_set_traverse_mask(FBODE,UU_TRUE);
		}
	}
	if (Snperim > 1)
	{
		ud_set_traverse_mask(FBISL,UU_FALSE);
		ud_set_traverse_mask(FBISD,UU_FALSE);
		ud_set_traverse_mask(FBITX,UU_FALSE);
	}
	if (Sopentyp == 0)
	{
		ud_set_traverse_mask(FBOPN,UU_FALSE);
		ud_set_traverse_mask(FBODE,UU_FALSE);
	}
/*
.....Determine if Trimmed Surface options should be enabled
*/
	ifl1 = ifl2 = UU_FALSE;
	if (Snperim > 0)
	{
		nsf = ntrim = UU_FALSE;
		Sallsf = UU_TRUE;
		for (i=0;i<Snperim && (!ntrim || Sallsf); i++)
		{
			if (i > 0 && ntrim)
			{
				Sallsf = (uc_super_class(sgeo[i].relnum) == UC_SURFACE_CLASS);
				continue;
			}
			ntrim = (sgeo[i].relnum == NCL_TRIMSF_REL);
			nsf = (ntrim || uc_super_class(sgeo[i].relnum) == UC_SURFACE_CLASS);
			if (i == 0) Sallsf = nsf;
			if (ntrim)
			{
				tsf.key = sgeo[i].key;
				ncl_retrieve_data_fixed(&tsf);
				ntrim = (tsf.no_ibndykey > 0);
			}
		}
		if (ntrim) ifl1 = UU_TRUE;
		if (nsf)
		{
			ifl2 = UU_TRUE;
			if (Sdirmodp == OFFST)
			{
				ud_set_traverse_mask(FBOTH,UU_TRUE);
				ifl2 = UU_FALSE;
				i = 1; ud_update_answer(FBSRF,&i);
			}
		}
		ifl = !(Sbotperim && Sallsf);
		ud_set_traverse_mask(FLBOT,ifl);
		ud_set_traverse_mask(FLBTX,ifl);
	}
/*
.....Set Trimmed surface fields
*/
	ud_set_traverse_mask(FBINR,ifl1);
	ud_set_traverse_mask(FBSRF,ifl2);
/*
.....Define Perimeter picture
*/
	isub = 0;
	if (Tdirmodp == OFFST) isub = 1;
	else if (ifl1 && ifl2) isub = 2;
	else if (Snperim > 0 && !ifl1 && !ifl2)
	{
		isub = 3;
		if (Sopentyp == 1 && UU_LIST_LENGTH(&Sindlst) > 0) isub = 4;
	}
	ud_dispfrm_update_frmpic(Sfrm0,0,picture[isub]);
/*
.....Define button colors
*/
	if (Snperim > 0) ud_dispfrm_set_attribs(0,FBPER,UM_BLACK,Tcolorp);
	else ud_dispfrm_set_attribs(0,FBPER,UM_WHITE,UM_RED);
/*
.....Set section colors
*/
	if (Snperim > 0)
	{
		Ssecol[Boundaries] = Sgreen; S_section_changed(Boundaries,UU_TRUE);
	}
	else
	{
		Ssecol[Boundaries] = Sred; S_section_changed(Boundaries,UU_FALSE);
	}
/*
.....Set Action Buttons
*/
	ifl = (Snperim > 0);
	ud_frm_enable_ok(ifl);
	ud_set_traverse_mask(FAPV,ifl);
	ud_set_traverse_mask(FAPY,ifl);
	ud_set_traverse_mask(FARS,ifl);
	ud_set_traverse_mask(FAGE,ifl);
	return;
}

/*********************************************************************
**    I_FUNCTION     :  S_select_geo(sfpt,sflst,mask,multi,prmno,color,frm,
**                                   fieldno,label,geo_init)
**       Routine to select geometry for the form.  The associated
**       text field will be updated with the geometry label and
**       the geometry will be highlighted.
**    PARAMETERS
**       INPUT  :
**          mask     Picking mask.
**          multi    0 = Single selection, 1 = multiple selection,
**                   2 = Single selection (calling routine handles invisibling
**                   of form), 3 = multiple selection (call routine handles
**                   invisibling of form).
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
**          geo_init UU_TRUE if geometry has been picked from the screen.
**    RETURNS      : UU_SUCCESS if an entity is picked.  UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_select_geo(sfpt,sflst,mask,multi,prmno,color,frm,fieldno,label,
	geo_init)
UM_sgeo *sfpt;
UU_LIST *sflst;
unsigned int *mask;
UU_LOGICAL multi;
int prmno,color,fieldno,frm;
char *label;
UU_LOGICAL *geo_init;
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
	if (multi < 2) S_form_invis();
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
	if (sflst != UU_NULL) S_unhilite_entities(sflst,geo_init);
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
		if (multi != 1 && multi != 3)
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
	if (multi != 1 && multi != 3)
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
........Unhighlight the previous selections
*/
/*		S_unhilite_entities(sflst);*/
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
	if (multi < 2) S_form_vis();
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
**    I_FUNCTION     :  S_select_open()
**       Routine to select open regions of perimeter geometry.
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
static void S_select_open()
{
	int numint,i,start,end,irtn,status,npts,sub,rel;
	int save_ptype,limit_key = -1;
	int total,pick_mask[UD_NMENTWD],*mask;
	struct NCL_fixed_databag e,e2;
	struct UM_compcrv_rec *ccv;
	struct UM_crvdatabag crv;
	struct NCL_nclpt_rec *ptt,pt1,pt2;
	struct UM_attrdata_rec att;
	UM_transf tfmat;
	UM_vector fwd,bwd;
	UU_REAL u,thk;
	UU_KEY_ID key,nclkey;
	UU_LOGICAL found,cmdreject;
	struct UM_evcrvout evout;
	Gwpoint3 pt, ve;
	UM_PLOCREC pick;
	Sopen_struc open;
	char ncllabel[NCL_MAX_LABEL];
	UM_f77_str_ptr flab;
	UM_int4 ipg, iel;
	UM_int2 nwds, ietype;
/*
.....Take down form
*/
	ud_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK (cmdreject, UU_TRUE);
	if (cmdreject != 0)
	{
		ud_setpick_type(save_ptype);
		if (limit_key != -1) ud_limit_entity(UU_FALSE, limit_key);
		limit_key = -1;
		goto done;
	}
/*
.....Set the appropriate selection mask
*/
	for (i=0;i<UD_NMENTWD; i++) pick_mask[i] = UD_ncl_pt[i] | UD_compcrv[i];
	mask = (int *)pick_mask;
	ud_lgeo(UU_TRUE,mask);
	limit_key = -1;
	if (Sopentyp == 1)
	{
		limit_key = Sopen_key;
		ud_limit_entity(UU_TRUE, limit_key);
		if (limit_key != -1) ud_limit_entity(UU_FALSE, limit_key);
	}
	ua_dl_pldas(UD_DASPCKLOC,UA_NCL,506,&pick,1,&numint,1);
	if (numint > 0)
	{
		e.key = um_get_pickkey(&pick.pent, 1);
		open.key = e.key;
		ncl_retrieve_data_fixed(&e);
		rel = e.rel_num;
		if (e.rel_num == UM_COMPCRV_REL)
		{
/*
.....Get component picked.
*/
			ccv = (struct UM_compcrv_rec *)&e;
			key = um_get_pickkey(&pick.pent, 2);
			found = UU_FALSE;
			for (i = 0; i < ccv->no_cid && !found; i++)
				found = (key == ccv->cid[i].crvid);
			if (found) start = end = i;
/*
.....Get order to use for components.
*/
			u = 0.5;
			uc_retrieve_transf(key,tfmat);
			crv.key = key; ncl_retrieve_data_fixed(&crv);
			uc_init_evcrvout (&crv,&evout);
			uc_evcrv(UM_FRSTDERIV,u,&crv,tfmat,&evout);
			um_unitvc(evout.dcdu,evout.dcdu);
			if (ccv->cid[i-1].reverse) um_negvc(evout.dcdu,fwd);
			else um_vctovc(evout.dcdu,fwd);
			um_negvc(fwd,bwd);
			pt.x = evout.cp[0]; pt.y = evout.cp[1]; pt.z = evout.cp[2];
			ve.x = fwd[0]; ve.y = fwd[1]; ve.z = fwd[2];
			ud_assist_vector(pt,ve);
			ve.x = bwd[0]; ve.y = bwd[1]; ve.z = bwd[2];
			ud_assist_vector(pt,ve);

			save_ptype = ud_getpick_type();
			ud_unlimit ();
			irtn = ud_pick_assist_seg("Pick the direction to chain components:");
			ud_setpick_type(save_ptype);
/*
.....Pick ending component.
*/
			if (irtn != 0)
			{
				limit_key = e.key;
				ud_limit_entity(UU_TRUE, limit_key);
				ua_dl_pldas(UD_DASPCKLOC,UA_NCL,507,&pick,1,&numint,1);
/*
.....Get component picked.
*/
				if (numint > 0)
				{					
					key = um_get_pickkey(&pick.pent, 2);
					found = UU_FALSE;
					for (i = 0; i < ccv->no_cid && !found; i++)
						found = (key == ccv->cid[i].crvid);
					if (found) end = i;
				}
			}
/*
.....Removed so user can pick single component without having to 
.....pick direction - ASF 1/21/14
				else
				goto done;
*/
		}
		else
		{
/*
.....Get number of points using the given label.
*/
			ptt = (struct NCL_nclpt_rec *)&e;
			strcpy(ncllabel,e.label);
			for (i=strlen(ncllabel);i<NCL_MAX_LABEL;i++) ncllabel[i] = ' ';
			UM_init_f77_str(flab,ncllabel,NCL_MAX_LABEL);
			sub = 0;
			status = vxchk(UM_addr_of_f77_str(flab),&sub,&nclkey,&ipg,&iel,
				&nwds,&ietype);
			npts = ipg;
			start = e.subscr;
			sub = e.subscr + 1;
			if (sub > npts) sub = 1;
			status = vxchk(UM_addr_of_f77_str(flab),&sub,&pt1.key,&ipg,&iel,
				&nwds,&ietype);
			sub = e.subscr - 1;
			if (sub < 1) sub = npts;
			status = vxchk(UM_addr_of_f77_str(flab),&sub,&pt2.key,&ipg,&iel,
				&nwds,&ietype);
			ncl_retrieve_data_fixed(&pt1); ncl_retrieve_data_fixed(&pt2);
/*
.....Get direction for index.
*/
			pt.x = ptt->pt[0]; pt.y = ptt->pt[1]; pt.z = ptt->pt[2];
			um_vcmnvc(pt1.pt,ptt->pt,fwd); um_unitvc(fwd,fwd);
			um_vcmnvc(pt2.pt,ptt->pt,bwd); um_unitvc(bwd,bwd);
			ve.x = fwd[0]; ve.y = fwd[1]; ve.z = fwd[2];
			ud_assist_vector(pt,ve);
			ve.x = bwd[0]; ve.y = bwd[1]; ve.z = bwd[2];
			ud_assist_vector(pt,ve);
			irtn = ud_pick_assist_seg("Pick the direction to chain points:");
			if (irtn == 0) goto done;
/*
.....Set the appropriate selection mask
*/
			for (i=0;i<UD_NMENTWD; i++)
				pick_mask[i] = UD_points[i] & UD_pocket_perimeter[i];
			mask = (int *)pick_mask;
			ud_lgeo(UU_TRUE,mask);
			ua_dl_pldas(UD_DASPCKLOC,UA_NCL,216,&pick,1,&numint,1);
			e2.key = um_get_pickkey(&pick.pent, 1);
			ncl_retrieve_data_fixed(&e2);
			end = e2.subscr;
			if (strcmp(e.label,e2.label) != 0)
			{
				ud_wrerr("Invalid Point. Not a member of the point array.");
				goto done;
			}
			strcpy(open.label,e.label);
		}
/*
.....Fix order if necessary.
*/
		if (irtn != 1)
		{
			i = start;
			start = end;
			end = i;
		}
		open.start = start;
		open.end = end;
/*
.....Store the open region.
*/
		open.total = npts;
		if (rel != UM_COMPCRV_REL)
		{
			if (end < start) total = npts - start + end + 1;
			else total = end - start + 1;
			open.colors = (int *)uu_malloc(total*sizeof(int));
			for (i=0;i<total;i++)
			{
				sub = start + i;
				if (sub > npts) sub -= npts;
				status = vxchk(UM_addr_of_f77_str(flab),&sub,&nclkey,&ipg,&iel,
					&nwds,&ietype);
				ncl_get_geo_color(nclkey,&open.colors[i]);
			}
		}
		ncl_push_indlst(&open);
		if (Spreviewflg) Schgmade = 1;
		ud_set_traverse_mask(FLBOT,UU_TRUE);
		ncl_get_scalar_value(Sopen_thk,&thk);
		if (thk != 0.) Sopenthk = UU_TRUE;
		else Sopenthk = UU_FALSE;
		ncl_pocket_draw_open(UU_FALSE);
	}
done:
	if (limit_key != -1) ud_limit_entity(UU_FALSE, limit_key);
	ud_form_vis();
	ud_unlimit();
	UD_UNMARK(cmdreject);
	return;
}

/*********************************************************************
**    I_FUNCTION     :  S_init_geo(sfpt,label,color)
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
		strcpy(sfpt->label,label);
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
**    I_FUNCTION     : S_build_command(flag)
**			Builds and outputs the POCKET/ command.
**    PARAMETERS   
**       INPUT  : flag    = UU_TRUE = output command to source file.
**			                UU_FALSE = Preview command only.
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
	int i,j,dirmodp,dirmodi,nline,slen,status;
	NCL_cmdbuf cmdbuf;
	UM_sgeo *geop,*geoi;
	char endbuf[96],tstr[8];
	UU_LOGICAL cmdreject,nsf,ntrim,found;
	UM_f77_str outcmd;
	Sopen_struc *open;

	if (Snperim == 0) return (0);
	if (flag)
		NCL_preview_mot = 0;
	else
		NCL_preview_mot = 1;

	if (Taxis5 == UU_TRUE)
	{
		status = afive_axis_output();
		taxis5 = 1;
	}
/*
.....Output POKMOD command
*/
	if (Sclick_pokmod == 1) nclu_pokmod_cmd1(&Smodals,flag);

	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto fini;

	dirmodp = Tdirmodp;
	dirmodi = Tdirmodi;

	geop = (UM_sgeo *) UU_LIST_ARRAY (&Splst);
	if (Snisles > 0) geoi = (UM_sgeo *) UU_LIST_ARRAY(&Silst);
	for (i = 0; i < Snperim; i++)
	{
		ncl_set_cmdmode(UU_TRUE);
/*
.....Initialize command buffer
*/
		ncl_init_cmdbuf(&cmdbuf);

		if (!flag)
			ncl_add_token(&cmdbuf, "*", NCL_nocomma);
		else if (Tlabel)
		{
			if (Tpoc_lab[0] == '\0')
			{
				ud_wrerr("Pocket label not entered.");
				return (0);
			}
			ncl_add_token(&cmdbuf, Tpoc_lab, NCL_nocomma);
			ncl_add_token(&cmdbuf, "=", NCL_nocomma);
		}
		ntrim = (Tislperim && geop[i].relnum == NCL_TRIMSF_REL);
		nsf = (Tbotperim && (geop[i].relnum == NCL_TRIMSF_REL || 
						geop[i].relnum == NCL_SURF_REL || 
						geop[i].relnum == UM_RBSPLSRF_REL || 
						geop[i].relnum ==  NCL_REVSURF_REL));
		if (ntrim)
		{
			struct NCL_trimsf_rec eptr;
			eptr.key = geop[i].key;
			ntrim = (ncl_retrieve_data_fixed (&eptr) == UU_SUCCESS && 
				eptr.no_ibndykey > 0);
		}

/*		if (i < Snperim - 1)
			dirmodp = geop[i].inout;
		else*/
			dirmodp = Tdirmodp;

		ncl_add_token(&cmdbuf, NCL_pocket, NCL_nocomma);
		if (Tbotperim && nsf)
			ncl_add_token(&cmdbuf,geop[i].label,NCL_comma);
		else
			ncl_add_token(&cmdbuf,Tpoc_bot,NCL_comma);

		if (Ttamode == PSNORM || Ttamode == SECPS)
		{
			ncl_add_token(&cmdbuf,NCL_normal,NCL_comma);
			if (Stamode == SECPS)
			{
				ncl_add_token(&cmdbuf,NCL_ps,NCL_comma);
				ncl_add_token(&cmdbuf,Tsec_ps,NCL_comma);
			}
		}

		ncl_add_token(&cmdbuf,Tpoc_top,NCL_comma);
		if (dirmodp == ON)
			ncl_add_token(&cmdbuf,NCL_on,NCL_comma);
		else if (dirmodp == OUT)
			ncl_add_token(&cmdbuf,NCL_out,NCL_comma);
		else if (dirmodp == OFFST)
		{
			ncl_add_token(&cmdbuf,NCL_offset,NCL_comma);
			if (Soffthk) ncl_add_token(&cmdbuf,Toff_thk,NCL_comma);
		}

		if (geop[i].relnum == NCL_POINT_REL || geop[i].relnum == UM_POINT_REL)
		{
			nclu_unsubscript (geop[i].label,tstr);
			ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
		}
		else
			ncl_add_token(&cmdbuf,geop[i].label,NCL_nocomma);

		for (j = 0; i == 0 && !ntrim && j < Snisles; j++)
		{
			if (j < Snisles - 1)
				dirmodi = geoi[j].inout;
			else
				dirmodi = Tdirmodi;

			ncl_add_token(&cmdbuf, ",", NCL_nocomma);
			if (dirmodi == ON)
				ncl_add_token(&cmdbuf,NCL_on,NCL_comma);
			else if (dirmodi == IN)
				ncl_add_token(&cmdbuf,NCL_in,NCL_comma);
			else if (dirmodi == OFFST)
				ncl_add_token(&cmdbuf,NCL_offset,NCL_comma);

			if (geoi[j].relnum == NCL_POINT_REL || geoi[j].relnum == UM_POINT_REL)
			{
				nclu_unsubscript (geoi[j].label,tstr);
				ncl_add_token(&cmdbuf,tstr,NCL_nocomma);
			}
			else
				ncl_add_token(&cmdbuf,geoi[j].label,NCL_nocomma);
		}
		if (ntrim)
		{
			ncl_add_token(&cmdbuf, ",", NCL_nocomma);
			if (Sdirmodi == ON)
				ncl_add_token(&cmdbuf,NCL_on,NCL_comma);
			else if (Sdirmodi == IN)
				ncl_add_token(&cmdbuf,NCL_in,NCL_comma);
			else if (dirmodi == OFFST)
				ncl_add_token(&cmdbuf,NCL_offset,NCL_comma);
			ncl_add_token(&cmdbuf,geop[i].label,NCL_nocomma);
		}
		endbuf[0] = '\0';
		if (Sislace)
		{
			if (Tpockend)
			{
				sprintf (endbuf,",START,%s",Tpoc_end);
				ncl_add_token(&cmdbuf, endbuf, NCL_nocomma);
			}

			if (Spsthk)
			{
				sprintf (endbuf,",PS,THICK,%s",Tps_thk);
				ncl_add_token(&cmdbuf, endbuf, NCL_nocomma);
			}
			
			if (Sdsthk || Sopenthk)
			{
				if (Sopenthk)
					sprintf (endbuf,",DS,THICK,%s,%s",Tds_thk,Topen_thk);
				else
					sprintf (endbuf,",DS,THICK,%s",Tds_thk);
				ncl_add_token(&cmdbuf, endbuf, NCL_nocomma);
			}
		}
		else
		{
			if (Tpockend)
			{
				sprintf (endbuf,",END,%s",Tpoc_end);
				ncl_add_token(&cmdbuf, endbuf, NCL_nocomma);
			}
			if (Tmaxloop)
			{
				sprintf (endbuf,",%s",Tmax_loop);
				ncl_add_token(&cmdbuf, endbuf, NCL_nocomma);
			}

			if (Spsthk)
			{
				sprintf (endbuf,",PS,THICK,%s",Tps_thk);
				ncl_add_token(&cmdbuf, endbuf, NCL_nocomma);
			}

			if (Sdsthk || Sopenthk)
			{
				if (Sopenthk)
					sprintf (endbuf,",DS,THICK,%s,%s",Tds_thk,Topen_thk);
				else
					sprintf (endbuf,",DS,THICK,%s",Tds_thk);
				ncl_add_token(&cmdbuf, endbuf, NCL_nocomma);
			}

			if (Tfinthk)
			{
				sprintf (endbuf,",FINISH,THICK,%s",Tfin_thk);
				ncl_add_token(&cmdbuf, endbuf, NCL_nocomma);
			}

			if (Toffpart)
			{
				if (Soffthk) 
					sprintf (endbuf,",OFF,PART,%s",Toff_thk);
				else
					sprintf (endbuf,",OFF,PART");
				ncl_add_token(&cmdbuf, endbuf, NCL_nocomma);
			}
		}
		if (Sopentyp > 0)
		{
			open = (Sopen_struc *) UU_LIST_ARRAY(&Sindlst);
			found = UU_FALSE;
			for (j=0;j<Sindlst.cur_cnt;j++)
			{
/*				if (ncl_pocket_match_open(geop[i].key,j))*/
				if (geop[i].key == open[j].key)
				{
					if (!found)
					{
						found = UU_TRUE;
						sprintf (endbuf,",OPEN");
						ncl_add_token(&cmdbuf, endbuf, NCL_nocomma);
					}
					if (open[j].start != open[j].end)
						sprintf (endbuf,",%d,THRU,%d",open[j].start,open[j].end);
					else
						sprintf (endbuf,",%d",open[j].start);
					ncl_add_token(&cmdbuf, endbuf, NCL_nocomma);
				}
			}
		}
		
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
	}
	
fini:
	NCL_preview_mot = 1;
	UD_UNMARK(cmdreject);
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_set_open_att(flag)
**       Delete memory for index list.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_set_open_att(color,lintyp)
int color,lintyp;
{
	Tcolorop = color;
	Tlintyp = lintyp;
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_indlst(flag)
**       Delete memory for index list.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_get_indlst(list)
UU_LIST **list;
{
	*list = &Sindlst;
}

/*********************************************************************
**    E_FUNCTION     : ncl_reset_indlst(flag)
**       Delete memory for index list.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_reset_indlst()
{
	if (Sind_init)
	{
		Sind_init = UU_FALSE;
		uu_list_free(&Sindlst);
	}
}

/*********************************************************************
**    I_FUNCTION     : ncl_push_indlst(open)
**       Push open index data onto list.
**    PARAMETERS
**       INPUT  :
**         open - Open side data.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_push_indlst(open)
Sopen_struc *open;
{
	if (!Sind_init)
	{
		Sind_init = UU_TRUE;
		uu_list_init(&Sindlst,sizeof(Sopen_struc),20,20);
	}
	uu_list_push(&Sindlst,open);
}

/*********************************************************************
**    E_FUNCTION     : ncl_pocket_draw_open(flag)
**       Display or reset display open pocked boundary regions. Delete
**       data if display is being reset.
**    PARAMETERS
**       INPUT  :
**         flag - Delete entries after drawing iff UU_TRUE.
**       OUTPUT :
**         opentyp - Updated number of valid open type perimeters
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pocket_draw_open(flag)
UU_LOGICAL flag;
{
	int i,j,sub,npts,status;
	struct NCL_fixed_databag e;
	struct UM_attrdata_rec att;
	Sopen_struc *open;
	UM_transf tfmat;
	UU_KEY_ID nclkey;
	char ncllabel[NCL_MAX_LABEL];
	UM_f77_str_ptr flab;
	UM_int4 ipg, iel;
	UM_int2 nwds, ietype;

	if (!flag) um_set_attrfl();
	else um_reset_attrfl();
	if (Sind_init)
	{
		open = (Sopen_struc *)UU_LIST_ARRAY(&Sindlst);
		for (i=0;i<Sindlst.cur_cnt;i++,open++)
		{
			e.key = open->key;
			ncl_retrieve_data_fixed(&e);
			if (e.rel_num != UM_COMPCRV_REL)
			{
				strcpy(ncllabel,e.label);
				for (j=strlen(ncllabel);j<NCL_MAX_LABEL;j++) ncllabel[j] = ' ';
				UM_init_f77_str(flab,ncllabel,NCL_MAX_LABEL);
				if (open->end < open->start)
					npts = open->total - open->start + open->end + 1;
				else
					npts = open->end - open->start + 1;
				for (j=0;j<npts;j++)
				{
					sub = open->start + j;
					if (sub > open->total) sub -= open->total;
					status = vxchk(UM_addr_of_f77_str(flab),&sub,&nclkey,&ipg,&iel,
					&nwds,&ietype);
					if (!flag) ncl_update_geo_color(nclkey,Tcolorop,UU_TRUE);
					else ncl_update_geo_color(nclkey,open->colors[j],UU_FALSE);
					e.key = nclkey; ncl_retrieve_data_fixed(&e);
					uc_display(&e);
				}
			}
			else
				uc_display(&e);
		}
		if (flag) ncl_reset_indlst();
		ud_updatews(UG_SUPPRESS);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     :  ncl_pocket_checkind(key,ind)
**       Determine if an index is for a component in an open region.
**    PARAMETERS
**       INPUT  :
**         key - Composite curve key.
**         ind - Index to check.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_pocket_checkind(key,ind,attr)
UU_KEY_ID key;
int ind;
struct UM_attrdata_rec *attr;
{
	int i;
	Sopen_struc *open;

	open = (Sopen_struc *)UU_LIST_ARRAY(&Sindlst);
	for (i=0;i<Sindlst.cur_cnt;i++)
	{
		if (open[i].key == key)
		{
			if (ind == open[i].start || ind == open[i].end) goto done;
			if (open[i].start < ind && ind < open[i].end) goto done;
			if (open[i].end < open[i].start)
			{
				if (ind < open[i].end || ind > open[i].start) goto done;
			}
		}
	}
	return(UU_FALSE);
done:
	attr->color = Tcolorop;
	attr->line_style = Tlintyp + 1;
	attr->line_weight = 3;
	return(UU_TRUE);
}

/*********************************************************************
**    E_FUNCTION     :  ncl_update_aform()
**       Update the form after OnModals return.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_update_aform()
{
	UU_LOGICAL lflg;

	Sislace = (NCL_lace_flag > 0);
	
	ud_set_display_mask(UD_INPUTF,FOEND,!Sislace);
	ud_set_traverse_mask(FOEND,!Sislace);
	ud_set_display_mask(UD_INPUTF,FOSTR,Sislace);
	ud_set_traverse_mask(FOSTR,Sislace);

	ud_set_traverse_mask(FOMAX,!Sislace);
	lflg = (!Sislace && Smaxloop == 1);
	ud_set_traverse_mask(FOMTX,lflg);

	ud_set_traverse_mask(FOBOF,!Sislace);
	lflg = (!Sislace && Sfinthk == 1);
	ud_set_traverse_mask(FOBTX,lflg);

	ud_set_traverse_mask(FOOFF,!Sislace);
/*
	ud_set_traverse_mask(FOTAX,!Sislace);
	lflg = (!Sislace && Stamode == SECPS);
*/
	lflg = (Stamode == SECPS);
	ud_set_traverse_mask(FOTTX,lflg);
	ud_set_traverse_mask(FOTSE,lflg);

	ud_update_form (Sfrm0);

	return (0);
}

void nclu_pocket_pokmod()
{
	int status;
	NCLU_pokmod pokmod;
	NCL_cmdbuf cmdbuf;
/*
.....Get the Pocket Modals
*/
	nclu_pokpar (&pokmod);
	status = nclu_pokmod(&pokmod, UU_TRUE, 0);
/*
.....Output the Pocket mode command
*/
	if (status == UU_SUCCESS)
	{
		nclu_pokmod_cmd1(&pokmod,UU_TRUE);
	}
}

int afive_axis_output()
//char* name;
{
	NCL_cmdbuf cmdbuf;
	int status;
	if (Taxis5==1)
	{

		ncl_init_cmdbuf(&cmdbuf);
		
		status = ncl_add_token(&cmdbuf,"multax/on", NCL_nocomma);
		//status = ncl_add_token(&cmdbuf, name, NCL_nocomma);
		
		ncl_set_cmdmode(UU_TRUE);
		ncl_add_cmdbuf(&cmdbuf);
		ncl_call(&cmdbuf);
	}	
	done:;
	return (0);
}


