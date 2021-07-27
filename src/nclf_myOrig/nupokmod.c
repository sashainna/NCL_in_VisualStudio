/*********************************************************************
**    NAME         :  nupokmod.c
**       CONTAINS:
**		      nclu_pokmod
**		      nclu_pokmod_cmd1
**		      nclu_pokmod_cmd
**		      nclu_pokpar
**
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nupokmod.c , 25.6
**     DATE AND TIME OF LAST MODIFICATION
**       01/20/17 , 11:09:06
**
*********************************************************************/
#include <string.h>
#include "mdpick.h"
#include "mfort.h"
#include "nccs.h"
#include "nkeywd.h"
#include "nclupok.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"
#include "mdattr.h"

enum
{
/*
.....Pocketing
*/
	FCTYP,FCBND,FCPOD,FCSPD,FCCUD,FCCUX,FCCUS,FCSMN,FCSCA,FCSMX,FCSTP,FCSTX,
	FCTRN,FCPIC,
/*
.....Entry Method fields
*/
	FETYP,FETAN,FEMXA,FENRM,FERMD,FEGOU,FERAP,FECLF,FECLX,FEARC,FEARI,FERDH,
	FERDV,
/*
.....Final pass
*/
	FPDIR,FPOUT,FPINS,FPRAD,FPSLW,FPSLA,FPCU1,FPCU2,FPCU3,FPCU4,
/*
.....Tool Retract
*/
	FRCLP,FRCPX,FRCSE,FRRTE,FRRTL,FRRTS,
/*
.....Feed rates
*/
	FRGEN,FRGEX,FRENT,FRENX,FRFST,FRFSX,FRTRA,FRTRX,FRFIN,FRFIX,FRRET,FRREX,
	FRPOS,FRPOX,FRFEED,
/*
.....Common fields
*/
	FOUT, FVIDEO
};
/*
.....Choicebox choices
*/
#define RAMP 0
#define HELIX 1
#define HELCW 2
#define HARC 3
#define HARCW 4
#define PLUNGE 5
#define OMIT 6
#define OFFPART 7

#define DEFALT 0
#define COUPLE 1
#define CYCLE 2
#define TEXTVAR 3

#define NRAMPS 0
#define MAXANG 1

#define NLEVS 0
#define LEVST 1
#define LEVDIS 1
#define LEVDEP 2

#define WARN 0
#define NOWARN 1
#define AVOID 2
#define AVOIDN 3

#define CCLW 0
#define CLW 1

#define COLAPS 0
#define LACE 1
#define LBOTH 2
#define SCRUB 3
#define SBOTH 4

#define SAME 1
#define BCCW 2
#define BCLW 3

#define SHARP 0
#define ARC 1

#define FPSAM 0
#define FPNON 1
#define FPCCW 2
#define FPCLW 3
#define FPREV 4

#define POKMOD_IN 1
#define POKMOD_OUT 0

#define POSX 0
#define NEGX 1
#define POSY 2
#define NEGY 3
#define LONG 4
#define SHORT 5
#define VECT 6

#define IN 0
#define OUT 1
#define ON 2

#define DOWN 0
#define UP 1

#define DIS 0
#define PLN 1

#define CLDIS 0
#define CLPLN 1

#define LEFT 0
#define RIGHT 1

#define XYPLAN 0
#define YZPLAN 1
#define ZXPLAN 2
#define NONE 3

#define CUR 0
#define FED 1
#define RPD 0
#define GEN 0
#define FAC 2

static UU_LOGICAL Smodal = UU_TRUE;
static char Sfeed_valuestr[80];
static int S_nlable = 1;
/*
.....Common variables
*/
static UU_LOGICAL Sactive=UU_FALSE,Spokmod_saved=UU_FALSE;
static UU_LOGICAL Sfed_chg = UU_FALSE;
static int Sfrm,Sfrma;
static UU_LOGICAL Spokfrm_saved=UU_TRUE;
static char Sav_valuestr1[STRL], Sav_valuestr2[STRL], Sav_valuestr3[STRL], Sav_valuestr4[STRL],
		Sav_valuestr5[STRL], Sav_valuestr6[STRL], Sav_valuestr7[STRL];
/*
.....Pocket entry variables
*/
static int Tlaceflag;
static int Sentry,Stanto,Smaxang,Swarn,Sclopt,Sarcperim,Sarcisle;
static char Snramps[STRL],Srampdis[STRL],Srapto[STRL],Sclcmd[STRL];
static int Tentry,Ttanto,Tmaxang,Twarn,Tclopt,Tarcperim,Tarcisle;
static char Tnramps[STRL],Trampdis[STRL],Trapto[STRL],Tclcmd[STRL];
/*
.....Pocketing variables
*/
static int Sbndry_dir,Spock_dir,Spiral_dir,Scut_dir;
static int Sstep_type,Strans_type;
static char Slace_vec[STRL],Scallop_hgt[STRL],Sstep_val[STRL];
static char Smin_step[STRL],Smax_step[STRL];
static int Tbndry_dir,Tpock_dir,Tpiral_dir,Tcut_dir;
static int Tstep_type,Ttrans_type;
static char Tlace_vec[STRL],Tcallop_hgt[STRL],Tstep_val[STRL];
static char Tmin_step[STRL],Tmax_step[STRL];
/*
.....Final Pass
*/
static int Sfinal_dir,Sfinal_out,Sfinal_in,Sfinal_feed;
static int Sfinal_cutcom,Sfinal_ccdir,Sfinal_ccplan;
static char Sfinal_rad[STRL],Sfinal_cctext[STRL],Sfinal_ang[STRL];
static int Tfinal_dir,Tfinal_out,Tfinal_in,Tfinal_feed;
static int Tfinal_cutcom,Tfinal_ccdir,Tfinal_ccplan;
static char Tfinal_rad[STRL],Tfinal_cctext[STRL],Tfinal_ang[STRL];
/*
.....Tool Retract variables
*/
static int Sclpl_type,Sret_end,Sret_level,Sret_sect;
static char Sclpl[STRL],Shret_dis[STRL],Svret_dis[STRL];
static int Tclpl_type,Tret_end,Tret_level,Tret_sect;
static char Tclpl[STRL],Thret_dis[STRL],Tvret_dis[STRL];
/*
.....Feed rate variables
*/
static int Sfeed_entry,Sfeed_first,Sfeed_gen,Sfeed_trans,Sfeed_fin;
static int Sfeed_ret,Sfeed_pos;
static char Sfeed_tentry[STRL],Sfeed_tfirst[STRL],Sfeed_tgen[STRL];
static char Sfeed_ttrans[STRL],Sfeed_tfin[STRL],Sfeed_tret[STRL];
static char Sfeed_tpos[STRL];
static int Tfeed_entry,Tfeed_first,Tfeed_gen,Tfeed_trans,Tfeed_fin;
static int Tfeed_ret,Tfeed_pos;
static char Tfeed_tentry[STRL],Tfeed_tfirst[STRL],Tfeed_tgen[STRL];
static char Tfeed_ttrans[STRL],Tfeed_tfin[STRL],Tfeed_tret[STRL];
static char Tfeed_tpos[STRL];
/*
.....Common section
*/
static int Sforce_cmd=UU_FALSE;
static NCLU_pokmod Spokmod;
/*
.....External variables
*/
extern UD_METHOD UD_initfrm_intry;
extern UU_LOGICAL NCL_pockform_active;
extern int NCL_lace_flag;
/*
.....Section definitions
*/
enum {Pocketing, Entry, Final, Retract, Feeds};
static char *Smode[]={"Pocketing", "Entry / Exit", "Final Pass",
	"Tool Retract", "Feed Rates"};
static int Sred[3]={180,0,0}, Syellow[3]={180,148,0}, Sgreen[3]={0,180,0};
static int Sblack[3]={0,0,0};
static int *Ssecol[]={Sblack,Sblack,Sblack,Sblack,Sblack};
/*
.....Callback routines
*/
static UD_FSTAT OnEnTog(),OnEnTxt(), OnReTog(),OnReTxt(),OnSelect();
static UD_FSTAT OnCuTog(),OnCuTxt(), OnFpTog(),OnFpTxt();
static UD_FSTAT OnFdTog(),OnFdTxt(),OnClose();
static UD_FSTAT OnPicToga(),OnClosea();
static UD_FSTAT S_enter_form();
static void S_init_form(), S_save_form();
/*
.....Internal routines
*/
static void S_define_pokmod(),S_form_enter(),S_section_changed();
void nclu_pokmod_cmd();

/*********************************************************************
**    I_FUNCTION     : S_enable_buttons()
**			Determines which fields should be enabled and which buttons
**       marked as necessary.
**			since the code in function already handle some, we just add more here
**    PARAMETERS   
**       INPUT  :
**          none.
**       OUTPUT :  
**          none.
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_enable_buttons()
{
	int nc;
	UU_REAL rval;
	UU_LOGICAL ifl;
	UU_LOGICAL sec1, sec2, sec3, sec4, sec5, sec6, sec7;
	sec1 = sec2 = sec3 = sec4 = sec5 = sec6 = sec7 = UU_TRUE;

	if (Tfeed_gen == 0)
	{
		ud_dispfrm_set_attribs(Sfrm, FRGEX, UM_BLACK, UM_WHITE);
	}
	else 
	{
		nc = (int)strlen(Tfeed_tgen);
		ul_strip_blanks(Tfeed_tgen, &nc);
		if (nc<=0)
		{
			ud_dispfrm_set_attribs(Sfrm,FRGEX,UM_WHITE,UM_RED);
			sec1 = UU_FALSE;
		}
		else
		{
			rval = atof(Tfeed_tgen);
			if (rval<=0)
				sec1 =  UU_FALSE;
			if (sec1) 
			{
				ud_dispfrm_set_attribs(Sfrm, FRGEX, UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(Sfrm, FRGEX, UM_WHITE,UM_RED);
			}
		}
	}
	if (Tfeed_entry == 0)
	{
		ud_dispfrm_set_attribs(Sfrm, FRENX, UM_BLACK, UM_WHITE);
	}
	else 
	{
		nc = (int)strlen(Tfeed_tentry);
		ul_strip_blanks(Tfeed_tentry, &nc);
		if (nc<=0)
		{
			ud_dispfrm_set_attribs(Sfrm,FRENX,UM_WHITE,UM_RED);
			sec2 = UU_FALSE;
		}
		else
		{
			rval = atof(Tfeed_tentry);
			if (rval<=0)
				sec2 =  UU_FALSE;
			if (sec2) 
			{
				ud_dispfrm_set_attribs(Sfrm, FRENX, UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(Sfrm, FRENX, UM_WHITE,UM_RED);
			}
		}
	}
	if (Tfeed_first == 0)
	{
		ud_dispfrm_set_attribs(Sfrm, FRFSX, UM_BLACK, UM_WHITE);
	}
	else 
	{
		nc = (int)strlen(Tfeed_tfirst);
		ul_strip_blanks(Tfeed_tfirst, &nc);
		if (nc<=0)
		{
			ud_dispfrm_set_attribs(Sfrm,FRFSX,UM_WHITE,UM_RED);
			sec3 = UU_FALSE;
		}
		else
		{
			rval = atof(Tfeed_tfirst);
			if (rval<=0)
				sec3 =  UU_FALSE;
			if (sec3) 
			{
				ud_dispfrm_set_attribs(Sfrm, FRFSX, UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(Sfrm, FRFSX, UM_WHITE,UM_RED);
			}
		}
	}
	if (Tfeed_trans == 0)
	{
		ud_dispfrm_set_attribs(Sfrm, FRTRX, UM_BLACK, UM_WHITE);
	}
	else 
	{
		nc = (int)strlen(Tfeed_ttrans);
		ul_strip_blanks(Tfeed_ttrans, &nc);
		if (nc<=0)
		{
			ud_dispfrm_set_attribs(Sfrm,FRTRX,UM_WHITE,UM_RED);
			sec4 = UU_FALSE;
		}
		else
		{
			rval = atof(Tfeed_ttrans);
			if (rval<=0)
				sec4 =  UU_FALSE;
			if (sec4) 
			{
				ud_dispfrm_set_attribs(Sfrm, FRTRX, UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(Sfrm, FRTRX, UM_WHITE,UM_RED);
			}
		}
	}
	if (Tfeed_fin == 0)
	{
		ud_dispfrm_set_attribs(Sfrm, FRFIX, UM_BLACK, UM_WHITE);
	}
	else 
	{
		nc = (int)strlen(Tfeed_tfin);
		ul_strip_blanks(Tfeed_tfin, &nc);
		if (nc<=0)
		{
			ud_dispfrm_set_attribs(Sfrm,FRFIX,UM_WHITE,UM_RED);
			sec5 = UU_FALSE;
		}
		else
		{
			rval = atof(Tfeed_tfin);
			if (rval<=0)
				sec5 =  UU_FALSE;
			if (sec5) 
			{
				ud_dispfrm_set_attribs(Sfrm, FRFIX, UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(Sfrm, FRFIX, UM_WHITE,UM_RED);
			}
		}
	}
	if (Tfeed_ret == 0)
	{
		ud_dispfrm_set_attribs(Sfrm, FRREX, UM_BLACK, UM_WHITE);
	}
	else 
	{
		nc = (int)strlen(Tfeed_tret);
		ul_strip_blanks(Tfeed_tret, &nc);
		if (nc<=0)
		{
			ud_dispfrm_set_attribs(Sfrm,FRREX,UM_WHITE,UM_RED);
			sec6 = UU_FALSE;
		}
		else
		{
			rval = atof(Tfeed_tret);
			if (rval<=0)
				sec6 =  UU_FALSE;
			if (sec6) 
			{
				ud_dispfrm_set_attribs(Sfrm, FRREX, UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(Sfrm, FRREX, UM_WHITE,UM_RED);
			}
		}
	}
	if (Tfeed_pos == 0)
	{
		ud_dispfrm_set_attribs(Sfrm, FRPOX, UM_BLACK, UM_WHITE);
	}
	else 
	{
		nc = (int)strlen(Tfeed_tpos);
		ul_strip_blanks(Tfeed_tpos, &nc);
		if (nc<=0)
		{
			ud_dispfrm_set_attribs(Sfrm,FRPOX,UM_WHITE,UM_RED);
			sec7 = UU_FALSE;
		}
		else
		{
			rval = atof(Tfeed_tpos);
			if (rval<=0)
				sec7 =  UU_FALSE;
			if (sec7) 
			{
				ud_dispfrm_set_attribs(Sfrm, FRPOX, UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(Sfrm, FRPOX, UM_WHITE,UM_RED);
			}
		}
	}
/*
.....Set section color
*/
	ifl = sec1&&sec2&&sec3&&sec4&&sec5&&sec6&&sec7;
	if (ifl)
	{
		Ssecol[Feeds] = Sgreen;
		if (Sfed_chg==0)
		{
			Ssecol[Feeds] = Sblack;
			S_section_changed(Feeds,UU_TRUE);
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
		S_section_changed(Feeds,UU_TRUE);
	}
/*
.....Set Action Buttons
*/
	if (Sfrm==0)
		ud_frm_enable_ok(ifl);
	else
		ud_frm_enable_close(Sfrm, ifl);
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
**    E_FUNCTION     : nclu_pokmod(reentry)
**       Controlling routine for the POKMOD form. Set Pocket Modals.
**    PARAMETERS
**       INPUT  :
**          pokmod   =  Active Pocket Modal parameters.
**          modal     - UU_TRUE = Form is modal (standard),
**                      UU_FALSE = Form is display type subform.
**          reentry   - UU_TRUE = Form is being reentered for the 2nd
**                      time.  Only used for nonmodal form type.     
**       OUTPUT :
**          pokmod   =  Active Pocket Modal parameters.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclu_pokmod(pokmod, modal, reentry)
NCLU_pokmod *pokmod;
UU_LOGICAL modal, reentry;
{
/*
.....Set up form fields
*/
	static char traverse[] = {
		1,1, 1,1, 0,0,0, 1,1, 1,1,1, 1, 0,
		1,1,1, 1,1, 1,1, 1,1, 1,1, 1,1,
		1,1, 1,1, 1,1, 1,1,1,1,
		1,1,1, 1, 1,1,
		1,1,1,1, 1,1,1,1, 1,1, 1,1,1,1,1,1,
		1, 1};

	static char display[] = {
		1,1, 1,1, 0,0,0, 1,1, 1,1,1, 1, 0,
		1,1,1, 1,1, 1,1, 1,1, 1,1, 1,1,
		1,1, 1,1, 1,1, 1,1,1,1,
		1,1,1, 1, 1,1,
		1,1,1,1, 1,1,1,1, 1,1, 1,1,1,1,1,1,
		1, 1};

	static char called[] = {
		6,6, 6,6,6,6,6, 6,6, 6,6,6, 6, 6,
		6,6,6, 6,6, 6,6, 6,6, 6,6, 6,6,
		6,6, 6,6, 6,6, 6,6,6,6,
		6,6,6, 6, 6,6,
		6,6,6,6, 6,6,6,6, 6,6, 6,6,6,6,6,
		6, 6};

	static UD_METHOD methods[] = {
		OnCuTog,OnCuTog, OnCuTog,OnCuTog,OnCuTog,OnCuTxt,OnSelect,
		OnCuTxt,OnCuTxt,
		OnCuTxt,OnCuTog,OnCuTxt, OnCuTog, OnCuTog,

		OnEnTog,OnEnTog,OnEnTog, OnEnTxt,OnEnTxt, OnEnTog,OnEnTxt,
		OnEnTog,OnEnTxt, OnEnTog,OnEnTog, OnEnTxt,OnEnTxt,

		OnFpTog,OnFpTog, OnFpTog,OnFpTxt, OnFpTog,OnFpTxt,
		OnFpTog,OnFpTog,OnFpTog,OnFpTxt,

		OnReTog,OnReTxt,OnSelect, OnReTog, OnReTog,OnReTog,

		OnFdTog,OnFdTxt,OnFdTog,OnFdTxt, OnFdTog,OnFdTxt,OnFdTog,OnFdTxt,
		OnFdTog,OnFdTxt, OnFdTog,OnFdTxt,OnFdTog,OnFdTxt,
		UU_NULL,
		UU_NULL, OnVideo, OnClose};

	static int *ans[] = {
		&Tlaceflag, &Tbndry_dir,
		&Tpock_dir,&Tpiral_dir,&Tcut_dir,(int *)Tlace_vec,UU_NULL,
		(int *)Tmin_step,(int *)Tcallop_hgt,
		(int *)Tmax_step,&Tstep_type,(int *)Tstep_val, &Ttrans_type, UU_NULL,

		&Tentry, &Ttanto, &Tmaxang,
		(int *)Tnramps,(int *)Trampdis, &Twarn,(int *)Trapto,
		&Tclopt,(int *)&Tclcmd, &Tarcperim,&Tarcisle, (int *)Thret_dis,
		(int *)Tvret_dis,

		&Tfinal_dir,&Tfinal_out,&Tfinal_in,(int *)Tfinal_rad,
		&Tfinal_feed,(int *)Tfinal_ang, &Tfinal_cutcom,&Tfinal_ccdir,
		&Tfinal_ccplan,(int *)Tfinal_cctext,

		&Tclpl_type,(int *)Tclpl, UU_NULL, &Tret_end, &Tret_level,&Tret_sect,

		&Tfeed_gen,(int *)Tfeed_tgen, 
		&Tfeed_entry,(int *)Tfeed_tentry,		
		&Tfeed_first,(int *)Tfeed_tfirst,		
		&Tfeed_trans,(int *)Tfeed_ttrans,
		&Tfeed_fin,(int *)Tfeed_tfin,
		&Tfeed_ret,(int *)Tfeed_tret,
		&Tfeed_pos,(int *)Tfeed_tpos,
		&Sfeed_valuestr,
		&Sforce_cmd, UU_NULL};

	int lace,nolace,activv,both;
	UD_METHOD save_entry;
/*
.....Initalize form fields
*/
	if (Sactive) goto done;
/*
......always initial to -1, any number >= 0 is a valid form number
*/
	Sfrma = -1;
	S_define_pokmod(pokmod,reentry);
	Smodal = modal;
	if ((modal)||(reentry==0)||(Spokfrm_saved==UU_FALSE))
		Spokmod = *pokmod;
	Spokfrm_saved = UU_TRUE;
	S_init_form();
/*
.....Get the Form input
*/
	save_entry = UD_initfrm_intry;
	UD_initfrm_intry = S_enter_form;
	if (modal)
	{
		display[FOUT+S_nlable] = 0;
		Sforce_cmd = UU_TRUE;
/*
.....always for the main from as 0 when open, there won't return until done.
*/
		Sfrm = 0; 
		Sfrm = ud_form1("pokmod.frm",ans,ans,methods,called,display,
			traverse);
		UD_initfrm_intry = save_entry;
		if (Sfrm == -1) return -1;
		S_save_form();
	}
	else
	{
		display[FOUT+S_nlable] = 1;
		Sforce_cmd = UU_FALSE;
		Sfrm = ud_form_display1("pokmod.frm",ans,ans,methods,called,display,
			traverse);
		if (Sfrm != -1) Sactive = UU_TRUE;
		UD_initfrm_intry = save_entry;
/*
.....for display type form, nclu_pokmode_formsav will not 
.....set until it close, using nclu_pokmode_formsav later
.....in calling routine
*/
	}
done:;
	return 0;
}

/*********************************************************************
**    I_FUNCTION     : OnCuTog(fieldno,val,stat)
**       Method called when a toggle field is changed.
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
static UD_FSTAT OnCuTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL chg=UU_FALSE;
	static char traverse[]={0};
	static char display[]={0};
	static char called[]={6};
	static UD_METHOD methods[]={OnPicToga,OnClosea};
	static int *ans[]={&Tlaceflag};

	switch (*fieldno)
	{
	case FCTYP:
		if (Tlaceflag != Spokmod.lacefl) chg = UU_TRUE;
		S_enter_form(fieldno,val,stat);
		break;
	case FCBND:
		if (Tbndry_dir != Spokmod.scrdir) chg = UU_TRUE;
		break;
	case FCPOD:
		if (Tpock_dir != Spokmod.pocdir) chg = UU_TRUE;
		break;
	case FCSPD:
		if (Tpiral_dir != Spokmod.spiral) chg = UU_TRUE;
		break;
	case FCCUD:
		if (Tcut_dir != Spokmod.lcdir) chg = UU_TRUE;
		S_enter_form(fieldno,val,stat);
		break;
	case FCPIC:
		if (Sfrma != -1) break;
		Sfrma = ud_form_display1("pokmoda.frm",ans,ans,methods,called,display,
			traverse);
		break;
	}
/*
.....Mark section as changed
*/
	if (chg)
	{
		Ssecol[Pocketing] = Sgreen;
		S_section_changed(Pocketing,UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnCuTxt(fieldno,val,stat)
**       Method called when a text field is changed.
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
static UD_FSTAT OnCuTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL chg=UU_FALSE;

	switch (*fieldno)
	{
	case FCCUX:
		if (strlen(Tlace_vec) != 0) chg = UU_TRUE;
		break;
	case FCSMN:
		if (strcmp(Tmin_step,Spokmod.min_step) != 0) chg = UU_TRUE;
		break;
	case FCSCA:
		if (strcmp(Tcallop_hgt,Spokmod.scallop) != 0) chg = UU_TRUE;
		break;
	case FCSMX:
		if (strcmp(Tmax_step,Spokmod.max_step) != 0) chg = UU_TRUE;
		break;
	case FCSTX:
		if (strcmp(Tstep_val,Spokmod.num_levs) != 0) chg = UU_TRUE;
		break;
	}
/*
.....Mark section as changed
*/
	if (chg)
	{
		Ssecol[Pocketing] = Sgreen;
		S_section_changed(Pocketing,UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnEnTog(fieldno,val,stat)
**       Method called when a toggle field is changed.
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
	UU_LOGICAL chg=UU_FALSE;

	switch (*fieldno)
	{
	case FETYP:
		if (Tentry != Spokmod.entrtype) chg = UU_TRUE;
		S_enter_form(fieldno,val,stat);
		break;
	case FETAN:
		if (Ttanto != Spokmod.tanto) chg = UU_TRUE;
		break;
	case FEMXA:
		if (Tmaxang != Spokmod.nrampstype) chg = UU_TRUE;
		S_enter_form(fieldno,val,stat);
		break;
	case FEGOU:
		if (Twarn != Spokmod.warn) chg = UU_TRUE;
		break;
	case FECLF:
		if (Tclopt != Spokmod.clfile) chg = UU_TRUE;
		S_enter_form(fieldno,val,stat);
	case FEARC:
		if (Tarcperim != Spokmod.perexit) chg = UU_TRUE;
		break;
	case FEARI:
		if (Tarcisle != Spokmod.islexit) chg = UU_TRUE;
		break;
	}
/*
.....Mark section as changed
*/
	if (chg)
	{
		Ssecol[Entry] = Sgreen;
		S_section_changed(Entry,UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnEnTxt(fieldno,val,stat)
**       Method called when a text field is changed.
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
	UU_LOGICAL chg=UU_FALSE;

	switch (*fieldno)
	{
	case FENRM:
		if (strcmp(Tnramps,Spokmod.num_ramps) != 0) chg = UU_TRUE;
		break;
	case FERMD:
		if (strcmp(Trampdis,Spokmod.ramp_dist) != 0) chg = UU_TRUE;
		break;
	case FECLX:
		if (strlen(Tclcmd) != 0) chg = UU_TRUE;
		break;
	case FERDH:
		if (strcmp(Thret_dis,Spokmod.hret_dis) != 0) chg = UU_TRUE;
		break;
	case FERDV:
		if (strcmp(Tvret_dis,Spokmod.vret_dis) != 0) chg = UU_TRUE;
		break;
	}
/*
.....Mark section as changed
*/
	if (chg)
	{
		Ssecol[Entry] = Sgreen;
		S_section_changed(Entry,UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnFpTog(fieldno,val,stat)
**       Method called when a toggle field is changed.
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
static UD_FSTAT OnFpTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL chg=UU_FALSE;

	switch (*fieldno)
	{
	case FPDIR:
		if (Tfinal_dir != Spokmod.lcfin) chg = UU_TRUE;
		break;
	case FPOUT:
		if (Tfinal_out != Spokmod.autocorner) chg = UU_TRUE;
		break;
	case FPINS:
		if (Tfinal_in != Spokmod.arcrad) chg = UU_TRUE;
		S_enter_form(fieldno,val,stat);
		break;
	case FPSLW:
		if (Tfinal_feed != Spokmod.appslowang) chg = UU_TRUE;
		S_enter_form(fieldno,val,stat);
		break;
	case FPCU1:
		if (Tfinal_cutcom != Spokmod.cutcom) chg = UU_TRUE;
		S_enter_form(fieldno,val,stat);
		break;
	case FPCU2:
		if (Tfinal_ccdir != Spokmod.cutcomdir) chg = UU_TRUE;
		break;
	case FPCU3:
		if (Tfinal_ccplan != Spokmod.cutcompln) chg = UU_TRUE;
		break;
	}
/*
.....Mark section as changed
*/
	if (chg)
	{
		Ssecol[Final] = Sgreen;
		S_section_changed(Final,UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnFpTxt(fieldno,val,stat)
**       Method called when a text field is changed.
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
static UD_FSTAT OnFpTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL chg=UU_FALSE;

	switch (*fieldno)
	{
	case FPRAD:
		if (strcmp(Tfinal_rad,Spokmod.arc_rad) != 0) chg = UU_TRUE;
		break;
	case FPSLA:
		if (strcmp(Tfinal_ang,Spokmod.slow_ang) != 0) chg = UU_TRUE;
		break;
	case FPCU4:
		if (strcmp(Tfinal_cctext,Spokmod.cut_com) != 0) chg = UU_TRUE;
		break;
	}
/*
.....Mark section as changed
*/
	if (chg)
	{
		Ssecol[Final] = Sgreen;
		S_section_changed(Final,UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnReTog(fieldno,val,stat)
**       Method called when a toggle field is changed.
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
static UD_FSTAT OnReTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL chg=UU_FALSE;

	switch (*fieldno)
	{
	case FRCLP:
		if (Tclpl_type != Spokmod.clpltype) chg = UU_TRUE;
		S_enter_form(fieldno,val,stat);
		break;
	case FRRTE:
		if (Tret_end != Spokmod.endretract) chg = UU_TRUE;
		break;
	case FRRTL:
		if (Tret_level != Spokmod.levretract) chg = UU_TRUE;
		break;
	case FRRTS:
		if (Tret_sect != Spokmod.secretract) chg = UU_TRUE;
		break;
	}
/*
.....Mark section as changed
*/
	if (chg)
	{
		Ssecol[Retract] = Sgreen;
		S_section_changed(Retract,UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnReTxt(fieldno,val,stat)
**       Method called when a text field is changed.
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
static UD_FSTAT OnReTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL chg=UU_FALSE;

	switch (*fieldno)
	{
	case FRCPX:
		if (strcmp(Tclpl,Spokmod.cclrlv) != 0) chg = UU_TRUE;
		break;
	}
/*
.....Mark section as changed
*/
	if (chg)
	{
		Ssecol[Retract] = Sgreen;
		S_section_changed(Retract,UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnFdTog(fieldno,val,stat)
**       Method called when a toggle field is changed.
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
	char valuestr[STRL];
	UU_LOGICAL chg=UU_FALSE;

	switch (*fieldno)
	{
	case FRENT:
		if (Tfeed_entry != Spokmod.entfrtyp) chg = UU_TRUE;
		S_enter_form(fieldno,val,stat);
		if (Tfeed_entry==0)
		{
			ud_setfrm_traverse_mask(Sfrm,FRENX,UU_FALSE);
//			strcpy(valuestr, Sfeed_valuestr);
			valuestr[0] = '\0';
			strcpy(Sav_valuestr1, Tfeed_tentry);
			ud_dispfrm_update_answer(Sfrm,FRENX,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm,FRENX,UU_TRUE);
			strcpy(Tfeed_tentry, Sav_valuestr1);
			ud_dispfrm_update_answer(Sfrm,FRENX,(int *)Tfeed_tentry);
		}
		break;
	case FRFST:
		if (Tfeed_first != Spokmod.frsfrtyp) chg = UU_TRUE;
		S_enter_form(fieldno,val,stat);
		if (Tfeed_first==0)
		{
			valuestr[0] = '\0';
			strcpy(Sav_valuestr5, Tfeed_tfirst);
			ud_dispfrm_update_answer(Sfrm,FRFSX,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm,FRFSX,UU_TRUE);
			strcpy(Tfeed_tfirst, Sav_valuestr5);
			ud_dispfrm_update_answer(Sfrm,FRFSX,(int *)Tfeed_tfirst);
		}
		break;
	case FRGEN:
		if (Tfeed_gen != Spokmod.genfrtyp) chg = UU_TRUE;
		if (Tfeed_gen==0)
		{
			ud_setfrm_traverse_mask(Sfrm,FRGEX,UU_FALSE);
/*
......display nothing
*/
//			strcpy(valuestr, Sfeed_valuestr);
			strcpy(Sav_valuestr2, Tfeed_tgen);
			valuestr[0] = '\0';
			ud_dispfrm_update_answer(Sfrm,FRGEX,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm,FRGEX,UU_TRUE);
			strcpy(Tfeed_tgen, Sav_valuestr2);
			ud_dispfrm_update_answer(Sfrm,FRGEX,(int *)Tfeed_tgen);
		}
		break;
	case FRTRA:
		if (Tfeed_trans != Spokmod.trnfrtyp) chg = UU_TRUE;
		S_enter_form(fieldno,val,stat);
		if (Tfeed_entry != Spokmod.entfrtyp) chg = UU_TRUE;
		S_enter_form(fieldno,val,stat);
		if (Tfeed_trans==0)
		{
			ud_setfrm_traverse_mask(Sfrm,FRTRX,UU_FALSE);
//			strcpy(valuestr, Sfeed_valuestr);
			valuestr[0] = '\0';
			strcpy(Sav_valuestr3, Tfeed_ttrans);
			ud_dispfrm_update_answer(Sfrm,FRTRX,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm,FRTRX,UU_TRUE);
			strcpy(Tfeed_ttrans, Sav_valuestr3);
			ud_dispfrm_update_answer(Sfrm,FRTRX,(int *)Tfeed_ttrans);
		}
		break;
	case FRFIN:
		if (Tfeed_fin != Spokmod.finfrtyp) chg = UU_TRUE;
		S_enter_form(fieldno,val,stat);
		if (Tfeed_fin==0)
		{
			ud_setfrm_traverse_mask(Sfrm,FRFIX,UU_FALSE);
//			strcpy(valuestr, Sfeed_valuestr);
			valuestr[0] = '\0';
			strcpy(Sav_valuestr4, Tfeed_tfin);
			ud_dispfrm_update_answer(Sfrm,FRFIX,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm,FRFIX,UU_TRUE);
			strcpy(Tfeed_tfin, Sav_valuestr4);
			ud_dispfrm_update_answer(Sfrm,FRFIX,(int *)Tfeed_tfin);
		}
		break;
	case FRRET:
		if (Tfeed_ret != Spokmod.retfrtyp) chg = UU_TRUE;
		S_enter_form(fieldno,val,stat);
		if (Tfeed_ret==0)
		{
			valuestr[0] = '\0';
			strcpy(Sav_valuestr6, Tfeed_tret);
			ud_dispfrm_update_answer(Sfrm,FRREX,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm,FRREX,UU_TRUE);
			strcpy(Tfeed_tret, Sav_valuestr6);
			ud_dispfrm_update_answer(Sfrm,FRREX,(int *)Tfeed_tret);
		}
		break;
	case FRPOS:
		if (Tfeed_pos != Spokmod.posfrtyp) chg = UU_TRUE;
		S_enter_form(fieldno,val,stat);
		if (Tfeed_pos==0)
		{
			valuestr[0] = '\0';
			strcpy(Sav_valuestr7, Tfeed_tpos);
			ud_dispfrm_update_answer(Sfrm,FRPOX,(int *)valuestr);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrm,FRPOX,UU_TRUE);
			strcpy(Tfeed_tpos, Sav_valuestr7);
			ud_dispfrm_update_answer(Sfrm,FRPOX,(int *)Tfeed_tpos);
		}
		break;
	}
/*
.....Mark section as changed
*/
	if (chg)
	{
		Sfed_chg = UU_TRUE;
		S_enable_buttons();
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnFdTxt(fieldno,val,stat)
**       Method called when a text field is changed.
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
static UD_FSTAT OnFdTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL chg=UU_FALSE;

	switch (*fieldno)
	{
	case FRENX:
		if (strcmp(Tfeed_tentry,Spokmod.ent_fr) != 0) chg = UU_TRUE;
		break;
	case FRFSX:
		if (strcmp(Tfeed_tfirst,Spokmod.frs_fr) != 0) chg = UU_TRUE;
		break;
	case FRGEX:
		if (strcmp(Tfeed_tgen,Spokmod.gen_fr) != 0) chg = UU_TRUE;
		break;
	case FRTRX:
		if (strcmp(Tfeed_ttrans,Spokmod.trn_fr) != 0) chg = UU_TRUE;
		break;
	case FRFIX:
		if (strcmp(Tfeed_tfin,Spokmod.fin_fr) != 0) chg = UU_TRUE;
		break;
	case FRREX:
		if (strcmp(Tfeed_tret,Spokmod.ret_fr) != 0) chg = UU_TRUE;
		break;
	case FRPOX:
		if (strcmp(Tfeed_tpos,Spokmod.pos_fr) != 0) chg = UU_TRUE;
		break;
	}
/*
.....Mark section as changed
*/
	if (chg)
	{
		Sfed_chg = UU_TRUE;
		S_enable_buttons();
	}
	return(UD_FLDOK);
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
	int numint;
	UM_PLOCREC pick;
	UU_LOGICAL cmdreject;

	if (*fieldno != FRCSE && *fieldno != FCCUS) return (UD_FLDOK);
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
	if (*fieldno == FRCSE)
	{
		ud_lgeo(UU_TRUE,UD_ncl_allsfpl);
/*
.....Get the next geometry selection
*/
		ua_dl_pldas(UD_DASPCKLOC,UA_NCL,472,&pick,1,&numint,1);
		if (numint == 0) goto done;

		ncl_picktostr(&pick,Tclpl);
		ud_dispfrm_update_answer(Sfrm,FRCPX,(int *)Tclpl);
	}
	else
	{
		ud_lgeo(UU_TRUE,UD_ncl_vepv);
/*
.....Get the next geometry selection
*/
		ua_dl_pldas(UD_DASPCKLOC,UA_NCL,709,&pick,1,&numint,1);
		if (numint == 0) goto done;

		ncl_picktostr(&pick,Tlace_vec);
		ud_dispfrm_update_answer(Sfrm,FCCUX,(int *)Tlace_vec);
	}
/*
.....End of routine
.....Redisplay form
*/

done:;
	ud_unlimit();
	ud_form_vis();
	ud_dspfrm_vis(Sfrm);
	UD_UNMARK(cmdreject);
	return(UD_FLDOK);
}
static void S_save_form()
{
	NCL_lace_flag = Tlaceflag;
	Sentry = Tentry;
	Stanto = Ttanto;
	Smaxang = Tmaxang;
	strcpy(Snramps, Tnramps);
	strcpy(Srampdis, Trampdis);
	Swarn = Twarn;
	strcpy(Srapto, Trapto);
	Sclopt = Tclopt;
	strcpy(Sclcmd, Tclcmd);
	Sarcperim = Tarcperim;
	Sarcisle = Tarcisle;
	strcpy(Shret_dis, Thret_dis);
	strcpy(Svret_dis, Tvret_dis);

	Sbndry_dir = Tbndry_dir;
	Spock_dir = Tpock_dir;
	Spiral_dir = Tpiral_dir;
	Scut_dir = Tcut_dir;
	strcpy(Slace_vec, Tlace_vec);
	strcpy(Smin_step, Tmin_step);
	strcpy(Scallop_hgt, Tcallop_hgt);
	strcpy(Smax_step, Tmax_step);
	Sstep_type = Tstep_type;
	strcpy(Sstep_val, Tstep_val);
	Strans_type = Ttrans_type;
	Sfinal_dir = Tfinal_dir;
	Sfinal_out = Tfinal_out;
	Sfinal_in = Tfinal_in;
	strcpy(Sfinal_rad, Tfinal_rad);
	Sfinal_feed = Tfinal_feed;
	strcpy(Sfinal_ang, Tfinal_ang);
	Sfinal_cutcom = Tfinal_cutcom;
	Sfinal_ccdir = Tfinal_ccdir;
	Sfinal_ccplan = Tfinal_ccplan;
	strcpy(Sfinal_cctext, Tfinal_cctext);
	Sclpl_type = Tclpl_type;
	strcpy(Sclpl, Tclpl);
	Sret_end = Tret_end;
	Sret_level = Tret_level;
	Sret_sect = Tret_sect;

	Sfeed_entry = Tfeed_entry;
	Sfeed_first = Tfeed_first;
	Sfeed_gen = Tfeed_gen;
	Sfeed_trans = Tfeed_trans;
	Sfeed_fin = Tfeed_fin;
	Sfeed_ret = Tfeed_ret;
	Sfeed_pos = Tfeed_pos;
	strcpy(Sfeed_tentry, Tfeed_tentry);
	strcpy(Sfeed_tfirst, Tfeed_tfirst);
	strcpy(Sfeed_tgen, Tfeed_tgen);
	strcpy(Sfeed_ttrans, Tfeed_ttrans);
	strcpy(Sfeed_tfin, Tfeed_tfin);
	strcpy(Sfeed_tret, Tfeed_tret);
	strcpy(Sfeed_tpos, Tfeed_tpos);
}
static void S_init_form()
{
	NCLX_mot_feedrate fedrat;
	UU_REAL feedrate;

	NclxMotGetFeedrate(&fedrat);
	feedrate = fedrat.base_feedrate;
	ncl_val2str (feedrate, Sfeed_valuestr);

	Tlaceflag = NCL_lace_flag;
	Tentry = Sentry;
	Ttanto = Stanto;
	Tmaxang = Smaxang;
	strcpy(Tnramps, Snramps);
	strcpy(Trampdis, Srampdis);
	Twarn = Swarn;
	strcpy(Trapto, Srapto);
	Tclopt = Sclopt;
	strcpy(Tclcmd, Sclcmd);
	Tarcperim = Sarcperim;
	Tarcisle = Sarcisle;
	strcpy(Thret_dis, Shret_dis);
	strcpy(Tvret_dis, Svret_dis);

	Tbndry_dir = Sbndry_dir;
	Tpock_dir = Spock_dir;
	Tpiral_dir = Spiral_dir;
	Tcut_dir = Scut_dir;
	strcpy(Tlace_vec, Slace_vec);
	strcpy(Tmin_step, Smin_step);
	strcpy(Tcallop_hgt, Scallop_hgt);
	strcpy(Tmax_step, Smax_step);
	Tstep_type = Sstep_type;
	strcpy(Tstep_val, Sstep_val);
	Ttrans_type = Strans_type;
	Tfinal_dir = Sfinal_dir;
	Tfinal_out = Sfinal_out;
	Tfinal_in = Sfinal_in;
	strcpy(Tfinal_rad, Sfinal_rad);
	Tfinal_feed = Sfinal_feed;
	strcpy(Tfinal_ang, Sfinal_ang);
	Tfinal_cutcom = Sfinal_cutcom;
	Tfinal_ccdir = Sfinal_ccdir;
	Tfinal_ccplan = Sfinal_ccplan;
	strcpy(Tfinal_cctext, Sfinal_cctext);
	Tclpl_type = Sclpl_type;
	strcpy(Tclpl, Sclpl);
	Tret_end = Sret_end;
	Tret_level = Sret_level;
	Tret_sect = Sret_sect;

	Tfeed_entry = Sfeed_entry;
	Tfeed_first = Sfeed_first;
	Tfeed_gen = Sfeed_gen;
	Tfeed_trans = Sfeed_trans;
	Tfeed_fin = Sfeed_fin;
	Tfeed_ret = Sfeed_ret;
	Tfeed_pos = Sfeed_pos;
	if (Tfeed_entry)
		strcpy(Tfeed_tentry, Sfeed_tentry);
	else
		Tfeed_tentry[0] = '\0';
	if (Tfeed_first)
		strcpy(Tfeed_tfirst, Sfeed_tfirst);
	else
		Tfeed_tfirst[0] = '\0';
	if (Tfeed_gen)
		strcpy(Tfeed_tgen, Sfeed_tgen);
	else
		Tfeed_tgen[0] = '\0';
	if (Tfeed_trans)
		strcpy(Tfeed_ttrans, Sfeed_ttrans);
	else
		Tfeed_ttrans[0] = '\0';
	if (Tfeed_fin)
		strcpy(Tfeed_tfin, Sfeed_tfin);
	else
		Tfeed_tfin[0] = '\0';
	if (Tfeed_ret)
		strcpy(Tfeed_tret, Sfeed_tret);
	else
		Tfeed_tret[0] = '\0';
	if (Tfeed_pos)
		strcpy(Tfeed_tpos, Sfeed_tpos);
	else
		Tfeed_tpos[0] = '\0';

	strcpy(Sav_valuestr1, Sfeed_tentry);
	strcpy(Sav_valuestr2, Sfeed_tgen);
	strcpy(Sav_valuestr3, Sfeed_ttrans);
	strcpy(Sav_valuestr4, Sfeed_tfin);
	strcpy(Sav_valuestr5, Sfeed_tfirst);
	strcpy(Sav_valuestr6, Sfeed_tret);
	strcpy(Sav_valuestr7, Sfeed_tpos);
}
/*********************************************************************
**    I_FUNCTION     :  OnClose()
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
static UD_FSTAT OnClose(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	Sactive = UU_FALSE;
	if ((NCL_pockform_active) && (*fieldno!=-1))
		ncl_update_aform();
	if (*fieldno==-1)
	{
		Spokfrm_saved = UU_FALSE;
	}
	else
	{
		S_save_form();
		Spokfrm_saved = UU_TRUE;
	}
	Sfrm = -1;
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
	switch (*fieldno)
	{
	case 0:
		ud_close_dispfrm(Sfrma);
		Sfrma = -1;
		ud_dispfrm_update_answer(Sfrm,FCTYP,&Tlaceflag);
		ud_update_form(Sfrm);
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
**    I_FUNCTION     :  S_define_pokmod(pokmod,reentry)
**       Defines the form answers.
**    PARAMETERS
**       INPUT  :
**          pokmod   = Pocket modals.
**          reentry  = UU_TRUE = form is being re-entered.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_define_pokmod(pokmod,reentry)
NCLU_pokmod *pokmod;
UU_LOGICAL reentry;
{
/*
.....Initialize routine
*/
	if (reentry) 
		return;
	Spokmod_saved = UU_FALSE;
/*
.....Initialize Entry fields
*/
	Sentry = pokmod->entrtype;
	Stanto = pokmod->tanto;
	Smaxang = pokmod->nrampstype;
	strcpy(Snramps,pokmod->num_ramps);
	strcpy(Srampdis,pokmod->ramp_dist);
	Swarn = pokmod->warn;
	strcpy(Srapto,pokmod->ret_dist);
	Sclopt = pokmod->clfile;
	Sclcmd[0] = '\0';
	Sarcperim = pokmod->perexit;
	Sarcisle = pokmod->islexit;
	strcpy(Shret_dis,pokmod->hret_dis);
	strcpy(Svret_dis,pokmod->vret_dis);
/*
.....Initialize Pocketing fields
*/
	NCL_lace_flag = pokmod->lacefl;
	Sbndry_dir = pokmod->scrdir;
	Spock_dir = pokmod->pocdir;
	Spiral_dir = pokmod->spiral;
	Scut_dir = pokmod->lcdir;
	Slace_vec[0] = '\0';
	strcpy(Smin_step,pokmod->min_step);
	strcpy(Scallop_hgt,pokmod->scallop);
	strcpy(Smax_step,pokmod->max_step);
	Sstep_type = pokmod->nlevstype;
	strcpy(Sstep_val,pokmod->num_levs);
	Strans_type = pokmod->arctrans;
/*
.....Initialize Final Pass fields
*/
	Sfinal_dir = pokmod->lcfin;
	Sfinal_out = pokmod->autocorner;
	Sfinal_in = pokmod->arcrad;
	strcpy(Sfinal_rad,pokmod->arc_rad);
	Sfinal_feed = pokmod->appslowang;
	strcpy(Sfinal_ang,pokmod->slow_ang);
	Sfinal_cutcom = pokmod->cutcom;
	Sfinal_ccdir = pokmod->cutcomdir;
	Sfinal_ccplan = pokmod->cutcompln;
	strcpy(Sfinal_cctext,pokmod->cut_com);
/*
.....Initialize Retract fields
*/
	Sclpl_type = pokmod->clpltype;
	strcpy(Sclpl,pokmod->cclrlv);
	Sret_end = pokmod->endretract;
	Sret_level = pokmod->levretract;
	Sret_sect = pokmod->secretract;
/*
.....Initialize Feed Rate fields
*/
	Sfeed_entry = pokmod->entfrtyp;
	Sfeed_first = pokmod->frsfrtyp;
	Sfeed_gen = pokmod->genfrtyp;
	Sfeed_trans = pokmod->trnfrtyp;
	Sfeed_fin = pokmod->finfrtyp;
	Sfeed_ret = pokmod->retfrtyp;
	Sfeed_pos = pokmod->posfrtyp;
	strcpy(Sfeed_tentry,pokmod->ent_fr);
	strcpy(Sfeed_tfirst,pokmod->frs_fr);
	strcpy(Sfeed_tgen,pokmod->gen_fr);
	strcpy(Sfeed_ttrans,pokmod->trn_fr);
	strcpy(Sfeed_tfin,pokmod->fin_fr);
	strcpy(Sfeed_tret,pokmod->ret_fr);
	strcpy(Sfeed_tpos,pokmod->pos_fr);
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
	int ifl,frm;
	static char *picture[]={"Pokmod_Collapse.jpg","Pokmod_Lace.jpg",
	    "Pokmod_Lace_Optimized.jpg","Pokmod_Scrub.jpg",
		"Pokmod_Scrub_Optimized.jpg"};
/*
.....Initialize routine
*/
	if (Smodal)
		frm = 0;
	else
		frm = Sactive ? Sfrm : *fieldno;
/*
.....Pocket Entry method
*/
	if (!Sactive || *fieldno == FETYP)
	{
		switch (Tentry)
		{
		case RAMP:
		case PLUNGE:
		case OMIT:
			if (Tmaxang)
				ud_dispfrm_update_prompt(frm,FENRM,"Maximum Ramp Angle:");
			else
				ud_dispfrm_update_prompt(frm,FENRM,"Number of Ramps:");
			ud_dispfrm_update_prompt(frm,FERMD,"Ramp Distance:");
			ifl = Tentry == RAMP;
			ud_setfrm_traverse_mask(frm,FETAN,ifl);
			ud_setfrm_traverse_mask(frm,FEMXA,ifl);
			ud_setfrm_traverse_mask(frm,FENRM,ifl);
			ud_setfrm_traverse_mask(frm,FERMD,ifl);
			ud_setfrm_traverse_mask(frm,FECLF,UU_FALSE);
			ud_setfrm_traverse_mask(frm,FECLX,UU_FALSE);
			break;
		case HELIX:
		case HELCW:
			if (Tmaxang)
				ud_dispfrm_update_prompt(frm,FENRM,"Maximum Helix Angle:");
			else
				ud_dispfrm_update_prompt(frm,FENRM,"Number of Revolutions:");
			ud_dispfrm_update_prompt(frm,FERMD,"Helix Radius:");
			ud_setfrm_traverse_mask(frm,FETAN,UU_TRUE);
			ud_setfrm_traverse_mask(frm,FEMXA,UU_TRUE);
			ud_setfrm_traverse_mask(frm,FENRM,UU_TRUE);
			ud_setfrm_traverse_mask(frm,FERMD,UU_TRUE);
			ud_setfrm_traverse_mask(frm,FECLF,UU_TRUE);
			ud_setfrm_traverse_mask(frm,FECLX,UU_TRUE);
			break;
		case HARC:
		case HARCW:
			ud_dispfrm_update_prompt(frm,FENRM,"Arc Size (degrees):");
			ud_dispfrm_update_prompt(frm,FERMD,"Arc Radius:");
			ud_setfrm_traverse_mask(frm,FETAN,UU_TRUE);
			ud_setfrm_traverse_mask(frm,FEMXA,UU_FALSE);
			ud_setfrm_traverse_mask(frm,FENRM,UU_TRUE);
			ud_setfrm_traverse_mask(frm,FERMD,UU_TRUE);
			ud_setfrm_traverse_mask(frm,FECLF,UU_TRUE);
			ud_setfrm_traverse_mask(frm,FECLX,UU_TRUE);
			break;
		}
	}
/*
.....Maximum Angle
*/
	if (*fieldno == FEMXA)
	{
		if (Tmaxang)
		{
			if (Tentry == RAMP)
				ud_dispfrm_update_prompt(frm,FENRM,"Maximum Ramp Angle:");
			else
				ud_dispfrm_update_prompt(frm,FENRM,"Maximum Helix Angle:");
		}
		else
		{
			if (Tentry == RAMP)
				ud_dispfrm_update_prompt(frm,FENRM,"Number of Ramps:");
			else
				ud_dispfrm_update_prompt(frm,FENRM,"Number of Revolutions:");
		}
	}
/*
.....Clfile Options
*/
	if (!Sactive || *fieldno == FECLF)
	{
		ifl = Tclopt == TEXTVAR;
		ud_setfrm_traverse_mask(frm,FECLX,ifl);
	}
/*
.....Pocketing method
*/
	if (!Sactive || *fieldno == FCTYP)
	{
		switch (Tlaceflag)
		{
		case COLAPS:
			ud_setfrm_traverse_mask(frm,FCBND,UU_FALSE);
			ud_setfrm_traverse_mask(frm,FCPOD,UU_TRUE);
			ud_setfrm_display_mask(frm,UD_INPUTF,FCPOD,UU_TRUE);
			ud_setfrm_traverse_mask(frm,FCSPD,UU_TRUE);
			ud_setfrm_display_mask(frm,UD_INPUTF,FCSPD,UU_TRUE);
			ud_setfrm_traverse_mask(frm,FCCUD,UU_FALSE);
			ud_setfrm_display_mask(frm,UD_INPUTF,FCCUD,UU_FALSE);
			ud_setfrm_traverse_mask(frm,FCCUX,UU_FALSE);
			ud_setfrm_display_mask(frm,UD_INPUTF,FCCUX,UU_FALSE);
			ud_setfrm_traverse_mask(frm,FCCUS,UU_FALSE);
			ud_setfrm_display_mask(frm,UD_INPUTF,FCCUS,UU_FALSE);
			ud_setfrm_traverse_mask(frm,FCSCA,UU_TRUE);
			ud_setfrm_traverse_mask(frm,FCTRN,UU_TRUE);
			ud_setfrm_traverse_mask(frm,FPDIR,UU_FALSE);
			break;
		case LACE:
		case LBOTH:
		case SCRUB:
		case SBOTH:
			ifl = (Tlaceflag == LBOTH || Tlaceflag == SBOTH);
			ud_setfrm_traverse_mask(frm,FCBND,ifl);
			ud_setfrm_traverse_mask(frm,FCPOD,UU_FALSE);
			ud_setfrm_display_mask(frm,UD_INPUTF,FCPOD,UU_FALSE);
			ud_setfrm_traverse_mask(frm,FCSPD,UU_FALSE);
			ud_setfrm_display_mask(frm,UD_INPUTF,FCSPD,UU_FALSE);
			ifl = Tcut_dir == VECT;
			ud_setfrm_traverse_mask(frm,FCCUD,UU_TRUE);
			ud_setfrm_display_mask(frm,UD_INPUTF,FCCUD,UU_TRUE);
			ud_setfrm_traverse_mask(frm,FCCUX,ifl);
			ud_setfrm_display_mask(frm,UD_INPUTF,FCCUX,UU_TRUE);
			ud_setfrm_traverse_mask(frm,FCCUS,ifl);
			ud_setfrm_display_mask(frm,UD_INPUTF,FCCUS,UU_TRUE);
			ud_setfrm_traverse_mask(frm,FCSCA,UU_FALSE);
			ud_setfrm_traverse_mask(frm,FCTRN,UU_FALSE);
			ud_setfrm_traverse_mask(frm,FPDIR,UU_TRUE);
			break;
		}
		ud_dispfrm_update_frmpic(frm,0,picture[Tlaceflag]);
	}
/*
.....Cutting Direction
*/
	if (!Sactive || *fieldno == FCCUD)
	{
		ifl = Tcut_dir == VECT;
		ud_setfrm_traverse_mask(frm,FCCUX,ifl);
		ud_setfrm_traverse_mask(frm,FCCUS,ifl);
	}
/*
.....Inside Corners
*/
	if (!Sactive || *fieldno == FPINS)
	{
		ifl = Tfinal_in == ARC;
		ud_setfrm_traverse_mask(frm,FPRAD,ifl);
	}
/*
.....Slowdown Feed rate
*/
	if (!Sactive || *fieldno == FPSLW)
	{
		ifl = Tfinal_feed;
		ud_setfrm_traverse_mask(frm,FPSLA,ifl);
	}
/*
.....Cutcom
*/
	if (!Sactive || *fieldno == FPCU1)
	{
		ifl = Tfinal_cutcom;
		ud_setfrm_traverse_mask(frm,FPCU2,ifl);
		ud_setfrm_traverse_mask(frm,FPCU3,ifl);
		ud_setfrm_traverse_mask(frm,FPCU4,ifl);
	}
/*
.....Clearance Plane
*/
	if (!Sactive || *fieldno == FRCLP)
	{
		ifl = Tclpl_type == PLN;
		ud_setfrm_traverse_mask(frm,FRCSE,ifl);
	}
/*
.....General Feed
*/	
	if (!Sactive || *fieldno == FRGEN)
	{
		ifl = Tfeed_gen != 0;
		ud_setfrm_traverse_mask(frm,FRGEX,ifl);
	}
/*
.....Entry Feed
*/
	if (!Sactive || *fieldno == FRENT)
	{
		ifl = Tfeed_entry != 0;
		ud_setfrm_traverse_mask(frm,FRENX,ifl);
	}
/*
.....First Pass Feed
*/
	if (!Sactive || *fieldno == FRFST)
	{
		ifl = Tfeed_first != 0;
		ud_setfrm_traverse_mask(frm,FRFSX,ifl);
	}
/*
.....Transition Feed
*/
	if (!Sactive || *fieldno == FRTRA)
	{
		ifl = Tfeed_trans != 0;
		ud_setfrm_traverse_mask(frm,FRTRX,ifl);
	}
/*
.....Finish Feed
*/
	if (!Sactive || *fieldno == FRFIN)
	{
		ifl = Tfeed_fin != 0;
		ud_setfrm_traverse_mask(frm,FRFIX,ifl);
	}
/*
.....Retract Feed
*/
	if (!Sactive || *fieldno == FRRET)
	{
		ifl = Tfeed_ret != 0;
		ud_setfrm_traverse_mask(frm,FRREX,ifl);
	}
/*
.....Positioning Feed
*/
	if (!Sactive || *fieldno == FRPOS)
	{
		ifl = Tfeed_pos != 0;
		ud_setfrm_traverse_mask(frm,FRPOX,ifl);
	}
	S_enable_buttons();
	return(UD_FLDOK);
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
	ud_form_section_color(Sfrm,Smode[section],Ssecol[section],bold);
/*
.....Enable Reset button
*/
/*	if (reset) ud_set_traverse_mask(FARS,UU_TRUE);*/
}

/*********************************************************************
**    E_FUNCTION     : nclu_pokpar(modals)
**       Get the current Pocket Modals from Fortran common rrdm block.
**       Initialize the form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_pokpar (modals)
NCLU_pokmod *modals;
{
/*
..... POKMOD current setting temporary variables
*/
	UM_int2 fnramp,fentry,fnwrn,frtrct,fcltyp,fpdir,fspirl,fslift,fcornr,
		fjctcm,fctcm1,fctcm2,flvdep,ftrans,fperim,fisle,flcflg,flcdir,fhldir,
		flcfin,fscrdr;
	UM_int4 fkytxt;
	UM_real4 farcrd, fslang, frmpds, fnumlv, fretds, fmaxst, fminst, fhscal,
		fgenfr,fposfr, fretfr, fentfr, ftrnfr, ffinfr, ffrsfr, fmxang, fhretd,
		fvretd;
	UM_real8 fclrlv;
	UU_REAL rnum;
	int i,nc,entry;
	UM_f77_str clrstr,fctcom;
	NCLX_mot_feedrate fedrat;
	UU_REAL feedrate;

	NclxMotGetFeedrate(&fedrat);
	feedrate = fedrat.base_feedrate;
/*
..... Initialize character string for clearance plane PLANE ID string.
..... and CUTCOM string.
*/
	UM_init_f77_str (clrstr, modals->cclrlv, STRL);
	UM_init_f77_str (fctcom, modals->cut_com, 80);
/*
..... Get the POKMOD values currently in use
*/
	pokpar (&fnramp, &fentry, &fhldir, &fkytxt,
		&fnwrn, &frtrct, &fcltyp, &flcflg, &fpdir, &fspirl, &flcdir,
		&flcfin, &fslift, &fcornr, &farcrd, &fslang, &ftrans, &fperim,
		&fisle, &frmpds, &fnumlv, &flvdep, &fretds, &fhretd, &fvretd, &fmaxst,
		&fhscal, &fminst,
		&fgenfr, &fposfr, &fretfr, &fentfr, &ftrnfr, &ffinfr, &ffrsfr, &fclrlv,
		UM_addr_of_f77_str(clrstr),
		&fmxang, &fjctcm, &fctcm1, &fctcm2, UM_addr_of_f77_str(fctcom),&fscrdr);
	modals->cut_com[79] = '\0';
	nc = 79;
	ul_strip_blanks(modals->cut_com,&nc);
    modals->cutcom = UU_FALSE;
/*
							INIT POKMOD PARAMETERS

..... Initialize Entry Method
*/
	modals->entrtype = RAMP;    /* fentry = 854 & default */
	modals->tanto = 0;
	modals->clfile = DEFALT;
	if (fentry < 0)
	{
		modals->tanto = 1; fentry = -fentry;
	}
	if (fentry == 855 || fentry == 1049 || fentry == 1054)
	{
		modals->entrtype = HELIX;
		if (fentry == 1049)
			modals->clfile = COUPLE;
		else if (fentry == 1054)
		{
			if (fkytxt > 0)
				modals->clfile = TEXTVAR;
			else
				modals->clfile = CYCLE;
		}
		if (fhldir == 60) modals->entrtype = HELCW;
	}
	else if (fentry == 1001)
		modals->entrtype = PLUNGE;
	else if (fentry == 172)
		modals->entrtype = OMIT;
/*	else if (fentry == 653)
		modals->entrtype = OFFPART; */

	if (fentry == 855 && fnramp == 1 && fmxang > 0.)
	{
		modals->entrtype = HARC;
		if (fhldir == 60) modals->entrtype = HARCW;
	}

	entry = modals->entrtype;
	if (entry == RAMP || entry == HELIX || entry == HELCW ||
		entry == HARC || entry == HARCW)
	{
		modals->warn = WARN;    /* fnwrn = 0 - the default */
		if (fnwrn == 1)
			modals->warn = NOWARN;
		else if (fnwrn == -1)
			modals->warn = AVOID;
		else if (fnwrn == -2)
			modals->warn = AVOIDN;

		rnum = frmpds; ncl_sprintf (modals->ramp_dist,&rnum,1);
		if (entry == HARC || entry == HARCW)
		{
			modals->nrampstype = NRAMPS;
			i = (int)(fmxang * 360. + 0.5);
			sprintf(modals->num_ramps,"%d",i);
		}
		else if (fnramp == 0 && fmxang > 0.)
		{
			modals->nrampstype = MAXANG; rnum = fmxang;
			ncl_sprintf (modals->num_ramps,&rnum,1);
		}
		else
		{
			modals->nrampstype = NRAMPS;
			i = fnramp;	if (i < 1) i = 1;
			sprintf(modals->num_ramps,"%d",i);
		}
	}
	else
	{
		modals->nrampstype = NRAMPS;
		modals->warn = WARN;
		rnum = 0.; ncl_sprintf (modals->ramp_dist,&rnum,1);
		i = 0; sprintf(modals->num_ramps,"%d",i);
	}

	if (fnumlv < 0.)
	{
		modals->nlevstype = NLEVS;
		i = (int)(-fnumlv); if (i < 1) i = 1;
		sprintf(modals->num_levs,"%d",i);
	}
	else
	{
		if (flvdep == 1)
			modals->nlevstype = LEVDEP;
		else
			modals->nlevstype = LEVST;
		rnum = fnumlv;
		ncl_sprintf (modals->num_levs,&rnum,1);
	}
	strcpy (Sstep_val,modals->num_levs);
/*
..... Initialize Positioning
*/
	modals->endretract = frtrct;

	modals->levretract = 0;
	if (fcltyp == 1)
	{
		modals->clpltype = PLN;
		for (i = 0; i < STRL; i++)
		{
			if (modals->cclrlv[i] == ' ')
			{
				modals->cclrlv[i] = '\0';
				break;
			}
		}
	}
	else
	{
		modals->clpltype = DIS; rnum = fclrlv;
		ncl_sprintf (modals->cclrlv,&rnum,1);
		if (fcltyp == 2) modals->levretract = 1;
	}

	rnum = fretds;
	ncl_sprintf (modals->ret_dist,&rnum,1);

	rnum = fhretd;
	ncl_sprintf (modals->hret_dis,&rnum,1);

	rnum = fvretd;
	ncl_sprintf (modals->vret_dis,&rnum,1);

	modals->secretract = UP;     /* fslift = 112 & default */
	if (fslift == 113) modals->secretract = DOWN;

	modals->perexit = modals->islexit = SHARP;
	if (fperim == 182) modals->perexit = ARC;
	if (fisle == 182) modals->islexit = ARC;
/*
..... Initialize Pocketing Parameters
*/
	modals->lacefl = flcflg;

	if (modals->lacefl == COLAPS)
	{
		modals->pocdir = CCLW;  /* fpdir == 59 & default */
		if (fpdir == 60) modals->pocdir = CLW;
	}
	else
		modals->lcdir = flcdir;

	modals->spiral = POKMOD_OUT;   /* fspirl == 653 & default */
	if (fspirl == 652) modals->spiral = POKMOD_IN;

	modals->arctrans = SHARP;  /* ftrans == 183 & default */
	if (ftrans == 182) modals->arctrans = ARC;

	rnum = fmaxst;
	ncl_sprintf (modals->max_step, &rnum,1);
	rnum = fminst;
	ncl_sprintf (modals->min_step, &rnum,1);

	rnum = fhscal;
	if (rnum > 0 && rnum < 1.e-5) rnum = 0;
	ncl_sprintf (modals->scallop, &rnum,1);

	Sforce_cmd = UU_FALSE;
/*
..... Initialize Final Pass subform
*/
	modals->autocorner = SHARP;  /* fcornr == 183 & default */
	if (fcornr == 182) modals->autocorner = ARC;

	if (farcrd <= 0)
	{
		modals->arcrad = UU_FALSE; rnum = 0.0;
	}
	else
	{
		modals->arcrad = UU_TRUE; rnum = farcrd;
	}
	ncl_sprintf (modals->arc_rad,&rnum,1);

	if (fslang > 130)
	{
		modals->appslowang = UU_FALSE; rnum = 90.0;
	}
	else
	{
		modals->appslowang = UU_TRUE; rnum = fslang;
	}
	ncl_sprintf (modals->slow_ang,&rnum,1);

	modals->cutcomdir = modals->cutcompln = NONE;
	if (modals->cutcom == UU_TRUE)
	{
		modals->cutcomdir = fctcm1;
		modals->cutcompln = fctcm2;
	}
/*
..... Initialize Feedrates subform
*/
	rnum = fgenfr;
//	modals->genfrtyp = (rnum == 0.)? CUR: FED;
	modals->genfrtyp = (rnum == feedrate)? CUR: FED;
	ncl_sprintf (modals->gen_fr,&rnum,1);

	rnum = fposfr;
	if (rnum == 0.)
		modals->posfrtyp = RPD;
	else if (rnum > 0.)
		modals->posfrtyp = FED;
	else
	{
		rnum = -rnum;
		modals->posfrtyp = FAC;
	}
	ncl_sprintf (modals->pos_fr,&rnum,1);

	rnum = fretfr;
	if (rnum == 0.)
		modals->retfrtyp = RPD;
	else if (rnum > 0.)
		modals->retfrtyp = FED;
	else
	{
		rnum = -rnum;
		modals->retfrtyp = FAC;
	}
	ncl_sprintf (modals->ret_fr,&rnum,1);

	rnum = fentfr;
	if (rnum == 0.)
		modals->entfrtyp = GEN;
	else if (rnum > 0.)
		modals->entfrtyp = FED;
	else
	{
		rnum = -rnum;
		modals->entfrtyp = FAC;
	}
	ncl_sprintf (modals->ent_fr,&rnum,1);

	rnum = ftrnfr;
	if (rnum == 0.)
		modals->trnfrtyp = GEN;
	else if (rnum > 0.)
		modals->trnfrtyp = FED;
	else
	{
		rnum = -rnum;
		modals->trnfrtyp = FAC;
	}
	ncl_sprintf (modals->trn_fr,&rnum,1);

	rnum = ffinfr;
	if (rnum == 0.)
		modals->finfrtyp = GEN;
	else if (rnum > 0.)
		modals->finfrtyp = FED;
	else
	{
		rnum = -rnum;
		modals->finfrtyp = FAC;
	}
	ncl_sprintf (modals->fin_fr,&rnum,1);

	rnum = ffrsfr;
	if (rnum == 0.)
		modals->frsfrtyp = GEN;
	else if (rnum > 0.)
		modals->frsfrtyp = FED;
	else
	{
		rnum = -rnum;
		modals->frsfrtyp = FAC;
	}
	ncl_sprintf (modals->frs_fr,&rnum,1);

	return;
}

/*********************************************************************
**    E_FUNCTION     : nclu_pokmod_cmd1(modals,flag)
**       Outputs the POKMOD command from an interface routine.
**    PARAMETERS
**       INPUT  :
**          modals   = Pocket modals.
**          flag     = UU_TRUE - output standard POKMOD command.
**                     UU_FALSE - output *POKMOD command.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_pokmod_cmd1(modals,flag)
NCLU_pokmod *modals;
UU_LOGICAL flag;
{
	NCL_cmdbuf cmdbuf;
/*
.....Output POKMOD command
*/
	if (Spokfrm_saved || Smodal)
	{
		nclu_pokmod_cmd(&cmdbuf,modals,flag,UU_TRUE);
		ncl_set_cmdmode(UU_TRUE);
		ncl_call(&cmdbuf);
	}
}

/*********************************************************************
**    E_FUNCTION     : nclu_pokmod_cmd(cmdbuf,modals,flag,kfl)
**       Outputs the POKMOD command.
**    PARAMETERS
**       INPUT  :
**          modals   = Pocket modals to be compared (when check with kfl).
**          flag     = UU_TRUE - output standard POKMOD command.
**                     UU_FALSE - output *POKMOD command.
**          kfl      = UU_TRUE = Command is generated from the interface.
**                     Check if changed from previous settings.
**       OUTPUT :
**          cmdbuf   = Output command buffer.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_pokmod_cmd (cmdbuf,modals,flag,kfl)
NCL_cmdbuf *cmdbuf;
NCLU_pokmod *modals;
UU_LOGICAL flag,kfl;
{
	UM_int2 ietype,ifl2;
	UU_KEY_ID nclkey;
	UU_REAL rnum,rnum1,rnum2,asn,pln[4];
	char buf[STRL+1],sbuf[80];
	struct NCL_fixed_databag e;
	UM_f77_str clrstr;

	ncl_init_cmdbuf(cmdbuf);
/*
.....Check to see if the command has changed
*/
	while (!Sforce_cmd && flag && kfl)
	{
		if (Sentry != modals->entrtype) break;
		if (Smaxang != modals->nrampstype) break;
		if (Sclopt != modals->clfile) break;
		if (Stanto != modals->tanto) break;
		if (Sarcperim != modals->perexit) break;
		if (Sarcisle != modals->islexit) break;
		if (Swarn != modals->warn) break;
		if (Sstep_type != modals->nlevstype) break;
		if (Sclpl_type != modals->clpltype) break;
		if (Sret_end != modals->endretract) break;
		if (Sret_level != modals->levretract) break;
		if (Sret_sect != modals->secretract) break;
		if (NCL_lace_flag != modals->lacefl) break;
		if (NCL_lace_flag == COLAPS)
		{
			if (Spiral_dir != modals->spiral) break;
			if (Spock_dir != modals->pocdir) break;
		}
		else
		{
			if (Scut_dir != modals->lcdir) break;
			if (Sfinal_dir != modals->lcfin) break;
		}
		if (Strans_type != modals->arctrans) break;
		if (Sfeed_gen != modals->genfrtyp) break;
		if (Sfeed_pos != modals->posfrtyp) break;
		if (Sfeed_ret != modals->retfrtyp) break;
		if (Sfeed_entry != modals->entfrtyp) break;
		if (Sfeed_trans != modals->trnfrtyp) break;
		if (Sfeed_fin != modals->finfrtyp) break;
		if (Sfinal_out != modals->autocorner) break;
		if (Sfinal_in != modals->arcrad) break;
		if (Sfinal_feed != modals->appslowang) break;
		if (modals->cutcom == 0 && Sfinal_cutcom == 1 &&
			(Sfinal_ccdir != NONE || Sfinal_ccplan != NONE ||
			strlen(Sfinal_cctext) > (unsigned int)0)) break;
		if (modals->cutcom == 1 && Sfinal_cutcom == 0) break;
		if (strcmp(Snramps,modals->num_ramps) != 0) break;
		if (strcmp(Srampdis,modals->ramp_dist) != 0) break;
		if (strcmp(Sstep_val,modals->num_levs) != 0) break;
		if (strcmp(Sclpl,modals->cclrlv) != 0) break;
		if (strcmp(Srapto,modals->ret_dist) != 0) break;
		if (strcmp(Shret_dis,modals->hret_dis) != 0) break;
		if (strcmp(Svret_dis,modals->vret_dis) != 0) break;
		if (strcmp(Smax_step,modals->max_step) != 0) break;
		if (strcmp(Scallop_hgt,modals->scallop) != 0) break;
		if (strcmp(Smin_step,modals->min_step) != 0) break;
		if (strcmp(Sfeed_tgen,modals->gen_fr) != 0) break;
		if (strcmp(Sfeed_tpos,modals->pos_fr) != 0) break;
		if (strcmp(Sfeed_tret,modals->ret_fr) != 0) break;
		if (strcmp(Sfeed_tentry,modals->ent_fr) != 0) break;
		if (strcmp(Sfeed_ttrans,modals->trn_fr) != 0) break;
		if (strcmp(Sfeed_tfin,modals->fin_fr) != 0) break;
		if (strcmp(Sfeed_tfirst,modals->frs_fr) != 0) break;
		if (Sfinal_feed && strcmp(Sfinal_rad,modals->arc_rad) != 0) break;
		if (Sfinal_feed && strcmp(Sfinal_ang,modals->slow_ang) != 0) break;
		if (Sfinal_cutcom && (Sfinal_ccdir != modals->cutcomdir ||
			Sfinal_ccplan != modals->cutcompln ||
			strcmp(Sfinal_cctext,modals->cut_com) != 0)) break;
		return;
	}

	if (!flag && !Spokmod_saved)
	{
		pmdsav();
		Spokmod_saved = UU_TRUE;
	}
	if (!flag) ncl_add_token(cmdbuf, "*", NCL_nocomma);
	ncl_add_token(cmdbuf, NCL_pokmod, NCL_nocomma);

	if (Sentry == HELCW && Stanto) Sentry = HELIX;
	if (Sentry == HARCW && Stanto) Sentry = HARC;

	if ((Sentry == RAMP || Sentry == HELIX || Sentry == HELCW) && Smaxang == MAXANG)
	{
		sprintf (buf,"-%s",Snramps);
		ncl_add_token(cmdbuf, buf, NCL_comma);
	}
	else if (Sentry == HARC || Sentry == HARCW)
	{
		ncl_get_scalar_value(Snramps, &rnum);
		rnum = rnum / 360.;
		ncl_sprintf (buf,&rnum,1);
		ncl_add_token(cmdbuf, buf, NCL_comma);
	}
	else
		ncl_add_token(cmdbuf, Snramps, NCL_comma);

	if (Sentry == RAMP)
		ncl_add_token(cmdbuf, NCL_ramp, NCL_comma);
	else if (Sentry == HELIX || Sentry == HELCW || Sentry == HARC || Sentry == HARCW)
	{
		if (Sclopt == COUPLE)
			ncl_add_token(cmdbuf, NCL_couple, NCL_comma);
		else if (Sclopt == CYCLE)
			ncl_add_token(cmdbuf, NCL_cycle, NCL_comma);
		else if (Sclopt == TEXTVAR && strlen(Sclcmd) > 0)
			ncl_add_token(cmdbuf, Sclcmd, NCL_comma);
		else
		{
			ncl_add_token(cmdbuf, NCL_helix, NCL_comma);
			if (Sentry == HELCW || Sentry == HARCW)
			ncl_add_token(cmdbuf, NCL_clw, NCL_comma);
		}
	}
	else if (Sentry == PLUNGE)
		ncl_add_token(cmdbuf, NCL_plunge, NCL_comma);
	else if (Sentry == OMIT)
		ncl_add_token(cmdbuf, NCL_omit, NCL_comma);
	else if (Sentry == OFFPART)
		ncl_add_token(cmdbuf, NCL_out, NCL_comma);

	if ((Sentry == RAMP || Sentry == HELIX || Sentry == HARC) && Stanto)
		ncl_add_token(cmdbuf, NCL_tanto, NCL_comma);

	if (Swarn == NOWARN)
		ncl_add_token(cmdbuf, NCL_nowarn, NCL_comma);
	else if (Swarn == AVOID)
		ncl_add_token(cmdbuf, NCL_avoid, NCL_comma);
	else if (Swarn == AVOIDN)
	{
		ncl_add_token(cmdbuf, NCL_avoid, NCL_comma);
		ncl_add_token(cmdbuf, NCL_nowarn, NCL_comma);
	}
	else if (Sentry == RAMP || Sentry == HELIX || Sentry == HELCW)
		ncl_add_token(cmdbuf, NCL_warn, NCL_comma);

	ncl_add_token(cmdbuf, Srampdis, NCL_comma);

	if (Sarcperim == ARC)
	{
		ncl_add_token (cmdbuf, NCL_out, NCL_comma);
		ncl_add_token (cmdbuf, NCL_arc, NCL_comma);
	}
	if (Sarcisle == ARC)
	{
		ncl_add_token (cmdbuf, NCL_island, NCL_comma);
		ncl_add_token (cmdbuf, NCL_arc, NCL_comma);
	}

	ncl_add_token(cmdbuf, NCL_retrct, NCL_comma);
	if (Sret_end == 0)
		ncl_add_token(cmdbuf, NCL_indent_off, NCL_comma);
	else if (Sret_end == 1)
		ncl_add_token(cmdbuf, NCL_on, NCL_comma);
	else
		ncl_add_token(cmdbuf, NCL_run_step, NCL_comma);

	if (kfl)
		ncl_add_token(cmdbuf, Sclpl, NCL_comma);
	else
	{
		UM_init_f77_str (clrstr,Sclpl,STRL);
		getkey(Sclpl,&nclkey);
		if (nclkey != 0)
		{
			e.key = nclkey;
			ncl_retrieve_data_fixed(&e);
			ncl_get_type(e.rel_num,&ietype);
			ptdesc(&nclkey,&ietype,&asn);
			ifl2 = 0;
			gtplt(&asn,&ifl2,pln);
			ncl_add_token(cmdbuf,"(PLANE/",NCL_nocomma);
			ncl_sprintf(sbuf,pln,4);
			ncl_add_token(cmdbuf,sbuf,NCL_nocomma);
			ncl_add_token(cmdbuf,")",NCL_comma);
		}
	}

	if (Sret_level == 1 && Sclpl_type == DIS)
		ncl_add_token(cmdbuf, NCL_incr, NCL_comma);

	if (Sstep_type == NLEVS)
	{
		sprintf (buf,"-%s",Sstep_val);
		ncl_add_token(cmdbuf, buf, NCL_comma);
	}
	else
	{
		ncl_add_token(cmdbuf, Sstep_val, NCL_comma);
		if (Sstep_type == LEVDEP)
			ncl_add_token(cmdbuf, NCL_depth, NCL_comma);
	}
	ncl_add_token(cmdbuf, Srapto, NCL_comma);

	ncl_get_scalar_value(Srapto, &rnum);
	ncl_get_scalar_value(Shret_dis, &rnum1);
	ncl_get_scalar_value(Svret_dis, &rnum2);
	if (rnum1 != rnum || rnum2 != rnum)
	{
		ncl_add_token(cmdbuf, Shret_dis, NCL_comma);
		if (rnum2 != rnum1)
			ncl_add_token(cmdbuf, Svret_dis, NCL_comma);
	}

	if (NCL_lace_flag == COLAPS)
	{
		if (Spock_dir == CLW)
			ncl_add_token (cmdbuf, NCL_clw, NCL_comma);
		else
			ncl_add_token (cmdbuf, NCL_ccw, NCL_comma);

		if (Spiral_dir == POKMOD_IN)
			ncl_add_token (cmdbuf, NCL_in, NCL_comma);
		else
			ncl_add_token (cmdbuf, NCL_out, NCL_comma);
	}
	else
	{
		if (NCL_lace_flag == SCRUB || NCL_lace_flag == SBOTH)
			ncl_add_token(cmdbuf, NCL_scrub0, NCL_comma);
		else
			ncl_add_token(cmdbuf, NCL_lace, NCL_comma);
		if (Scut_dir == VECT)
			ncl_add_token(cmdbuf,Slace_vec,NCL_comma);
		else if (Scut_dir == POSX)
			ncl_add_token(cmdbuf,NCL_posx,NCL_comma);
		else if (Scut_dir == NEGX)
			ncl_add_token(cmdbuf,NCL_negx,NCL_comma);
		else if (Scut_dir == POSY)
			ncl_add_token(cmdbuf,NCL_posy,NCL_comma);
		else if (Scut_dir == NEGY)
			ncl_add_token(cmdbuf,NCL_negy,NCL_comma);
		else if (Scut_dir == LONG)
			ncl_add_token(cmdbuf,"LONG",NCL_comma);
		else if (Scut_dir == SHORT)
			ncl_add_token(cmdbuf,"SHORT",NCL_comma);

		if (Sfinal_dir != FPSAM)
		{
			ncl_add_token(cmdbuf, NCL_finish, NCL_comma);
			if (Sfinal_dir == FPCLW)
				ncl_add_token(cmdbuf, NCL_clw, NCL_comma);
			else if (Sfinal_dir == FPCCW)
				ncl_add_token(cmdbuf, NCL_ccw, NCL_comma);
			else if (Sfinal_dir == FPNON)
				ncl_add_token(cmdbuf, NCL_omit, NCL_comma);
			else if (Sfinal_dir == FPREV)
				ncl_add_token(cmdbuf, NCL_revers, NCL_comma);
			else
				ncl_add_token(cmdbuf, NCL_same, NCL_comma);
		}

		if (NCL_lace_flag == LBOTH || NCL_lace_flag == SBOTH)
		{
			ncl_add_token(cmdbuf, NCL_both, NCL_comma);
			if (Sbndry_dir == SAME)
				ncl_add_token(cmdbuf, NCL_same, NCL_comma);
			else if (Sbndry_dir == BCLW)
				ncl_add_token(cmdbuf, NCL_clw, NCL_comma);
			else if (Sbndry_dir == BCCW)
				ncl_add_token(cmdbuf, NCL_ccw, NCL_comma);
		}
	}

	if (Sret_sect == DOWN)
		ncl_add_token (cmdbuf, NCL_down, NCL_comma);
	else
		ncl_add_token (cmdbuf, NCL_up, NCL_comma);

	if (Sfinal_out == ARC)
		ncl_add_token (cmdbuf, NCL_arc, NCL_comma);
	else
		ncl_add_token (cmdbuf, NCL_sharp, NCL_comma);

	if (Sfinal_in == ARC)
	{
		ncl_add_token (cmdbuf, NCL_in, NCL_comma);
		ncl_add_token (cmdbuf, Sfinal_rad, NCL_comma);
	}

	if (Sfinal_feed)
	{
		ncl_add_token (cmdbuf, NCL_atangl, NCL_comma);
		ncl_add_token (cmdbuf, Sfinal_ang, NCL_comma);
	}

	if (Strans_type == ARC && NCL_lace_flag == COLAPS)
	{
		ncl_add_token (cmdbuf, NCL_trans, NCL_comma);
		ncl_add_token (cmdbuf, NCL_arc, NCL_comma);
	}

	ncl_add_token(cmdbuf, Smax_step, NCL_comma);
	ncl_add_token(cmdbuf, Smin_step, NCL_comma);

	if (NCL_lace_flag == COLAPS)
	{
		ncl_get_scalar_value(Scallop_hgt, &rnum);
		if (rnum > 0)
		{
			ncl_add_token(cmdbuf, NCL_height, NCL_comma);
			ncl_add_token(cmdbuf, Scallop_hgt, NCL_comma);
		}
	}
//Yurong ask ken
	if (Sfeed_gen == CUR)
		ncl_add_token(cmdbuf, "0", NCL_comma);
	else
		ncl_add_token(cmdbuf, Sfeed_tgen, NCL_comma);

	if (Sfeed_pos == RPD)
		ncl_add_token(cmdbuf, NCL_rapid, NCL_comma);
	else if (Sfeed_pos == FAC)
	{
		sprintf (buf,"-%s",Sfeed_tpos);
		ncl_add_token(cmdbuf, buf, NCL_comma);
	}
	else
		ncl_add_token(cmdbuf, Sfeed_tpos, NCL_comma);

	if (Sfeed_ret == RPD)
		ncl_add_token(cmdbuf, NCL_rapid, NCL_comma);
	else if (Sfeed_ret == FAC)
	{
		sprintf (buf,"-%s",Sfeed_tret);
		ncl_add_token(cmdbuf, buf, NCL_comma);
	}
	else
		ncl_add_token(cmdbuf, Sfeed_tret, NCL_comma);

	if (Sfeed_entry == GEN)
		ncl_add_token(cmdbuf, "0", NCL_comma);
	else if (Sfeed_entry == FAC)
	{
		sprintf (buf,"-%s",Sfeed_tentry);
		ncl_add_token(cmdbuf, buf, NCL_comma);
	}
	else
		ncl_add_token(cmdbuf, Sfeed_tentry, NCL_comma);

	if (Sfeed_trans == GEN)
		ncl_add_token(cmdbuf, "0", NCL_comma);
	else if (Sfeed_trans == FAC)
	{
		sprintf (buf,"-%s",Sfeed_ttrans);
		ncl_add_token(cmdbuf, buf, NCL_comma);
	}
	else
		ncl_add_token(cmdbuf, Sfeed_ttrans, NCL_comma);

	if (Sfeed_fin == GEN)
		ncl_add_token(cmdbuf, "0", NCL_comma);
	else if (Sfeed_fin == FAC)
	{
		sprintf (buf,"-%s",Sfeed_tfin);
		ncl_add_token(cmdbuf, buf, NCL_comma);
	}
	else
		ncl_add_token(cmdbuf, Sfeed_tfin, NCL_comma);

	if (Sfeed_first == GEN)
		ncl_add_token(cmdbuf, "0", NCL_comma);
	else if (Sfeed_first == FAC)
	{
		sprintf (buf,"-%s",Sfeed_tfirst);
		ncl_add_token(cmdbuf, buf, NCL_comma);
	}
	else
		ncl_add_token(cmdbuf, Sfeed_tfirst, NCL_comma);

	if (Sfinal_cutcom && (Sfinal_ccdir != NONE || Sfinal_ccplan != NONE ||
		strlen(Sfinal_cctext) > (unsigned int)0))
	{
		ncl_add_token (cmdbuf, NCL_cutcom, NCL_comma);
		if (Sfinal_ccdir == LEFT)
			ncl_add_token (cmdbuf, NCL_left, NCL_comma);
		else if (Sfinal_ccdir == RIGHT)
			ncl_add_token (cmdbuf, NCL_right, NCL_comma);
		else if (Sfinal_ccdir == ON)
			ncl_add_token (cmdbuf, NCL_on, NCL_comma);
		if (Sfinal_ccplan == XYPLAN)
			ncl_add_token (cmdbuf, NCL_xyplan, NCL_comma);
		else if (Sfinal_ccplan == YZPLAN)
			ncl_add_token (cmdbuf, NCL_yzplan, NCL_comma);
		else if (Sfinal_ccplan == ZXPLAN)
			ncl_add_token (cmdbuf, NCL_zxplan, NCL_comma);
		if (strlen(Sfinal_cctext) > (unsigned int)0)
			ncl_add_token (cmdbuf, Sfinal_cctext, NCL_comma);
		ncl_add_token(cmdbuf, NCL_nomore, NCL_nocomma);
	}
	ncl_add_cmdbuf(cmdbuf);

	return;
}

