/*********************************************************************
**    NAME         :  nufmill.c
**       CONTAINS: User interface routines for flowline milling.
**
**       static UD_FSTAT OnSelect(fieldno, val, stat)
**       static UD_FSTAT OnPickType(fieldno, val, stat)
**       static UD_FSTAT OnView(fieldno, val, stat)
**       void nclu_fmill()
**
**    COPYRIGHT 2003 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nufmill.c , 25.3
**    DATE AND TIME OF LAST MODIFICATION
**       10/27/16 , 13:16:48
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

#define CONTCT 0
#define EDGON 1
#define EDGTO 2
#define EDGPAST 3

#define DEFALT 0
#define THRU 1
#define OVER 2
#define DOWN 3
#define BOTH 4

#define DEFALT 0
#define SAME 1
#define CCLW 2
#define CLW 3

#define DIS 0
#define PLN 1
#define SURF 2

#define CUR 0
#define RPD 1
#define FDR 2
/*
.....FMill
*/
#define MILLRG1		0
#define MILLRG2		1
#define MILLRG3		2
#define MILLRG4		3
/*
.....Motion Type
*/
#define OPTRG1	4
#define OPTRG2	5
#define OPTRG3	6
#define OPTRG4	7
#define OPTRG5	8
#define OPTRG6	9
#define OPTRG7	10
#define OPTRG8	11
#define OPTRG9	12
/*
......Boundaries
*/
#define BOUNDRG1	13
#define BOUNDRG2	14
#define BOUNDRG3	15
#define RETRG1	16
#define RETRG2	17
#define RETRG3	18
#define RETRG4	19
#define RETRG5	20
#define RETRG6	21
#define RETRG7	22
#define RETRG8	23
#define RETRG9	24
#define RETRG10	25
#define AVOIDCHK	26		/*avoid check button */
#define BOUNDRG4	28
#define BOUNDRG5	27
#define BOUNDRG6	30
#define BOUNDRG7	31
#define BOUNDRG8	32
#define BOUNDRG9	29  /*DeSelectAll */
/*
.....Entry /Exit
*/
#define ENEXTRG1	33
#define ENEXTRG2	34
#define ENEXTRG3	35
#define ENEXTRG4	36
#define ENEXTRG5	37
#define ENEXTRG6	38
#define ENEXTRG7	39
#define ENEXTRG8	40
#define ENEXTRG9	41
#define ENEXTRG10	42
/*
.....Colors
*/
#define CLRRG1	43
#define CLRRG2	44
#define CLRRG3	45
#define CLRRG4	46
#define CLRRG5	47
#define CLRRG6	48
#define CLRRG7	49
#define CLRRG8	50
#define CLRRG9	51
#define CLRRG10	52
/*
.....Action Buttons
*/
#define FAPV	53
#define FAPY	54
#define FARS	55
#define FAPB	56
#define FAVE	57
#define FAGE	58
#define FAVW	59
#define FVIDEO	60

#define HIDE_KEY 0
#define CONTOUR_KEY 1

extern int NCL_preview_mot;

/*
.....Section definitions
*/
enum {FMill, MotionT, Boundary, Entry, Colors};
static char *Smode[]={"FMill", "Motion Type", "Boundaries", "Entry / Exit", "Colors"};
static int Sred[3]={180,0,0}, Syellow[3]={180,148,0}, Sgreen[3]={0,180,0};
static int Sblack[3]={0,0,0};
static int *Ssecol[]={Sblack,Sblack,Sblack,Sblack,Sblack,Sblack};
static int Sacc[6], Sav_chc1, Sav_chc2, Sav_chc01, Sav_chc02;

extern int NAUTIPV;
extern UD_METHOD UD_initfrm_intry;

static UU_LIST Skey_list;
static int Snkeys;
static UU_LOGICAL Skey_init;

/*
.....colors
*/
static int Tcsclr = 12;
static int Tclrnew[] = {8,9,11,10,13,14,15};
static int Tclrhld[] = {-1,-1,-1,-1,-1,-1,-1};
static int Tgeotog,Tgeocol;
static int Scsclr = 12;
/*
.....Color tab variables 'S' final save value; 'T' temp form value
*/
static int Ssfcol,Tsfcol,Scvcol,Tcvcol,Serapcol,Terapcol,Seretcol,Teretcol;
static int Sstptcol, Tstptcol,Sbrapcol,Tbrapcol, Sbretcol,Tbretcol;
static int Savdcol,Tavdcol;
static int Sgeotog,Sgeocol;
static char Scmd1_str[2][NCL_MAX_LABEL_AND_SUBSCRIPT];
static char Scmd2_str[4][NCL_MAX_LABEL_AND_SUBSCRIPT];
static char Scmd3_str[6][NCL_MAX_LABEL_AND_SUBSCRIPT];
static char Scmd4_str[2][NCL_MAX_LABEL_AND_SUBSCRIPT];
static char Scmd5_str[4][NCL_MAX_LABEL_AND_SUBSCRIPT];
/*
.....Local routines
*/
static void S_init_form(),S_save_form(),S_hilite_entity(),S_unhilite_entity();
static UD_FSTAT S_enter_form();
static void S_init_traverse(), S_form_invis(),S_form_vis(),S_init_geo();
static void S_hilite_entities(),S_unhilite_all(),S_unhilite_entities();
static void S_update_answers(),S_section_changed();
static void S_add_key(),S_create_key_list(),S_draw_indir(),S_push_geo();
static void S_get_modals(),S_create_table();
static void S_update_table(),S_format_table(),S_look_avoid();
static void S_delete_geo(),S_display_ds();
static int S_select_geo(), S_build_command();

static UD_FSTAT OnAction(),OnVideo();
static int SfrmPlay=0;
static int Serrflg,Spreviewflg,Schgmade;

static UU_KEY_ID Skeyhld[] = {0,0,0,0,0,0,0,0};

static char Ssfnam[NCL_MAX_LABEL_AND_SUBSCRIPT], Tsfnam[NCL_MAX_LABEL_AND_SUBSCRIPT];
static char Scvnam[NCL_MAX_LABEL_AND_SUBSCRIPT], Tcvnam[NCL_MAX_LABEL_AND_SUBSCRIPT];
static char Sptnam[NCL_MAX_LABEL_AND_SUBSCRIPT], Tptnam[NCL_MAX_LABEL_AND_SUBSCRIPT];
static char Sraptoval[NCL_MAX_LABEL_AND_SUBSCRIPT], Traptoval[NCL_MAX_LABEL_AND_SUBSCRIPT];
static char Sretrctval[NCL_MAX_LABEL_AND_SUBSCRIPT], Tretrctval[NCL_MAX_LABEL_AND_SUBSCRIPT];

static int Spasstyp=0, Sstarttyp=0, Sdir=0, Sptcalc=0, Somit=0;
static int Tpasstyp=0, Tstarttyp=0, Tdir=0, Tptcalc=0, Tomit=0;
static int Sraptotyp=0, Sraptofed=0, Sretrcttyp=0, Sretrctfed=0;
static int Traptotyp=0, Traptofed=0, Tretrcttyp=0, Tretrctfed=0;
static int Tavoid_chk = 0, Savoid_chk = 0;
static UU_REAL Sscalht=0.0, Stoler=0.0;
static UU_REAL Tscalht=0.0, Ttoler=0.0;

static int Sedgetype = 0,Siotype = 0,Sdowndir = 0,Sfinpass = 0;
static int Sraptotyp1=0, Sraptofed1=0, Sretrcttyp1=0, Sretrctfed1=0, Sfinfed1=0;
static int Tedgetype = 0, Tiotype = 0, Tdowndir = 0, Tfinpass = 0;
static int Traptotyp1=0, Traptofed1=0, Tretrcttyp1=0, Tretrctfed1=0, Tfinfed1=0;
static char Sraptoval1[NCL_MAX_LABEL], Traptoval1[NCL_MAX_LABEL];
static char Sretrctval1[NCL_MAX_LABEL], Tretrctval1[NCL_MAX_LABEL];
static UU_REAL Scsthk = 0., Tcsthk = 0.;

static int Scsnum = 0;
static UU_LIST Scslst;
static UU_LOGICAL Sgeo_init;
static UU_LOGICAL Sgeo_redraw;
static UM_sgeo Sgsf,Sgcv,Sgerap, Sgeret, Sgspt, Sgavd, Sgbrap, Sgbret;

static UU_LOGICAL Sform_init = UU_FALSE;
static UN_motseg *Smptr=0,Smotatt;
static UN_mot_vpbox_struc Smvpbox[UV_NVPORTS];
static UU_LIST Skey_list;
static int Snkeys;
static UU_LOGICAL Skey_init;

static char Spassstr[65], Sstartstr[65], Ssteps_str[65], Suvval_str[65] = "0.0, 0.0";
static char Tpassstr[65], Tstartstr[65], Tsteps_str[65], Tuvval_str[65] = "0.0, 0.0";

static char Sscalht_str[65]="0.0", Snpass_str[65]="0",
		Stoler_str[65]="0.0", Snstep_str[65]="0",
		Sraptofrv_str[65]="0.0", Sretrctfrv_str[65]="0.0",
		Sraptofrv1_str[65]="0.0", Sretrctfrv1_str[65]="0.0",
		Sfinfrv1_str[65]="0.0", Scsthk_str[65]="0.0",
		Sav_finfrv1_str[65], Sav_retrctfrv1_str[65], Sav_raptofrv1_str[65],
		Sav_raptofrv_str[65]="0.0", Sav_retrctfrv_str[65]="0.0";

static char Tscalht_str[65]="0.0", Tnpass_str[65]="0",
		Ttoler_str[65]="0.0", Tnstep_str[65]="0",
		Traptofrv_str[65]="0.0", Tretrctfrv_str[65]="0.0",
		Traptofrv1_str[65]="0.0", Tretrctfrv1_str[65]="0.0",
		Tfinfrv1_str[65]="0.0", Tcsthk_str[65]="0.0";

static char NCL_both[] = "BOTH";
static char NCL_last[] = ",LAST";
static char Sfeed_valuestr[80];

static int Serrflg, Spreviewflg, Schgmade, Smenu_ch[20],  Stog_ch1;
static int *Sanswers[] = {
		UU_NULL, (int *)&Tsfnam,UU_NULL, (int *)&Tcvnam,
/*
......Motion Type
*/
		&Tpasstyp, (int *)&Tpassstr,
		&Tstarttyp, (int *)&Tstartstr, UU_NULL,
		&Tdir, &Tptcalc, (int *)&Tsteps_str, &Tomit,
/*
......Boundaries
*/
		&Tedgetype, &Tiotype, &Tdowndir,

		&Traptotyp1, (int *)&Traptoval1,  UU_NULL,
		&Traptofed1, (int *)&Traptofrv1_str,
		&Tretrcttyp1, (int *)&Tretrctval1, UU_NULL,
		&Tretrctfed1, (int *)&Tretrctfrv1_str,

		&Tavoid_chk, (int *)&Tcsthk_str, UU_NULL, UU_NULL, 
		&Tfinpass, &Tfinfed1, (int *)&Tfinfrv1_str,
/*
.....Entry/Exit
*/
		&Traptotyp, (int *)&Traptoval,  UU_NULL,
		&Traptofed, (int *)&Traptofrv_str,
		&Tretrcttyp, (int *)&Tretrctval, UU_NULL,
		&Tretrctfed, (int *)&Tretrctfrv_str,
/*
.....colors
*/
		&Tsfcol, &Tcvcol, &Terapcol, &Teretcol,
		&Tstptcol, &Tcsclr, &Tbrapcol, &Tbretcol,
		&Tgeotog, &Tgeocol,
		UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,UU_NULL
	};
/*
.....string should be "u,v" format
*/
static int S_parse_uv_value(char *uvstr)
{
	int status;
	char *tok, tmpstr[256], tmpstr1[256], tmpstr2[256];
	if (!((uvstr!=NULL)&&(strlen(uvstr)>0)))
		return -1;
	strncpy(tmpstr, uvstr, 255);
	tmpstr[255] = '\0';
	tok = strtok(tmpstr, " \t,\r\n");
	if (tok==NULL)
		return -1;
	strcpy(tmpstr1, tok);
	tok = strtok(NULL, " \t,\r\n");
	if (tok==NULL)
		return -1;
	strcpy(tmpstr2, tok);
	tok = strtok(NULL, " \t,\r\n");
	if (tok!=NULL)
		return -1;
	status = ncl_parse_scalar_values(tmpstr1, tmpstr1, UD_SCAVAL);
	if (status==-1)
		return -1;
	status = ncl_parse_scalar_values(tmpstr2, tmpstr2, UD_SCAVAL);
	if (status==-1)
		return -1;
	return 0;
}

/*********************************************************************
**    E_FUNCTION     : S_build_command(flg)
**       Build the FMILL statement.
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
	int j;
	UM_sgeo *geo;
	NCL_cmdbuf cmdbuf;
	char buf[64];

	if (flg)
		NCL_preview_mot = 0;
	else
		NCL_preview_mot = 1;
	ncl_init_cmdbuf(&cmdbuf);

	if (!flg) ncl_add_token(&cmdbuf, "*", NCL_nocomma);

	ncl_add_token(&cmdbuf, NCL_fmill, NCL_nocomma);
	ncl_add_token(&cmdbuf, Tsfnam, NCL_comma);

	if (Tpasstyp == 1)
	{
		ncl_add_token(&cmdbuf, NCL_pass, NCL_comma);
		strcpy(buf, Tnpass_str);
	}
	else
	{
		ncl_add_token(&cmdbuf, NCL_height, NCL_comma);
		strcpy(buf, Tscalht_str);
	}
	ncl_add_token(&cmdbuf, buf, NCL_comma);

	if (Tedgetype)
	{
		if (Tedgetype == EDGON)
			ncl_add_token(&cmdbuf, NCL_on, NCL_comma);
		else if (Tedgetype == EDGTO)
			ncl_add_token(&cmdbuf, NCL_to, NCL_comma);
		else if (Tedgetype == EDGPAST)
			ncl_add_token(&cmdbuf, NCL_past, NCL_comma);
	}
	Scsnum = UU_LIST_LENGTH(&Scslst);
	if (Scsnum > 0)
	{
		geo = (UM_sgeo *) UU_LIST_ARRAY (&Scslst);

		ncl_add_token(&cmdbuf, NCL_cs, NCL_comma);
		for (j = 0; j < Scsnum; j++)
		{
			ncl_add_token (&cmdbuf,geo[j].label,NCL_comma);
			if (geo[j].thick > UM_DFUZZ)
			{
				ncl_sprintf (buf,&geo[j].thick,1);
				ncl_add_token (&cmdbuf,buf,NCL_comma);
			}
		}
	}

	if (Tstarttyp)
	{
		if (Tstarttyp == 1)
		{
			ncl_add_token(&cmdbuf, NCL_start, NCL_comma);
			ncl_add_token(&cmdbuf, Tuvval_str, NCL_comma);
		}
		else if (Tstarttyp == 2 && Tptnam[0])
		{
			ncl_add_token(&cmdbuf, NCL_start, NCL_comma);
			ncl_add_token(&cmdbuf, Tptnam, NCL_comma);
		}
	}

	ncl_add_token(&cmdbuf, NCL_fwd, NCL_comma);
	sprintf(buf,"%d",Tdir);
	ncl_add_token(&cmdbuf, buf, NCL_comma);

	if (Tptcalc > 0)
	{
		if (Tptcalc == 2)
		{
			ncl_add_token(&cmdbuf, NCL_run_step, NCL_comma);
			strcpy(buf, Tnstep_str);
		}
		else
		{
			ncl_add_token(&cmdbuf, NCL_show_toler, NCL_comma);
			strcpy(buf, Ttoler_str);
		}
		ncl_add_token(&cmdbuf, buf, NCL_comma);
	}

	if (Tomit > 0)
	{
		ncl_add_token(&cmdbuf, NCL_omit, NCL_comma);
		if (Tomit == 1)
			ncl_add_token(&cmdbuf, NCL_start, NCL_comma);
		else if (Tomit == 2)
			ncl_add_token(&cmdbuf, NCL_end, NCL_comma);
		else if (Tomit == 3)
			ncl_add_token(&cmdbuf, NCL_both, NCL_comma);
	}

	if (Tcvnam[0])
	{
		ncl_add_token(&cmdbuf, NCL_on, NCL_comma);
		ncl_add_token(&cmdbuf, Tcvnam, NCL_comma);
	}

	if (Traptotyp && Traptoval[0])
	{
		ncl_add_token(&cmdbuf, NCL_rapto, NCL_comma);
		ncl_add_token(&cmdbuf, Traptoval, NCL_comma);
		if (Traptofed == 1)
		{
			ncl_add_token(&cmdbuf, NCL_rapid, NCL_comma);
		}
		else if (Traptofed == 2)
		{
			ncl_add_token(&cmdbuf, Traptofrv_str, NCL_comma);
		}
	}

	if (Tretrcttyp && Tretrctval[0])
	{
		ncl_add_token(&cmdbuf, NCL_retrct, NCL_comma);
		ncl_add_token(&cmdbuf, Tretrctval, NCL_comma);
		if (Tretrctfed == 1)
		{
			ncl_add_token(&cmdbuf, NCL_rapid, NCL_comma);
		}
		else if (Tretrctfed == 2)
		{
			ncl_add_token(&cmdbuf, Tretrctfrv_str, NCL_comma);
		}
	}

	if (Tiotype > 0)
	{
		ncl_add_token(&cmdbuf, NCL_avoid, NCL_comma);
		if (Tiotype == BOTH)
			ncl_add_token(&cmdbuf, NCL_both, NCL_comma);
		if (Tiotype == BOTH)
		{
			if (Tdowndir == SAME)
				ncl_add_token(&cmdbuf, NCL_same, NCL_comma);
			else if (Tdowndir == CLW)
				ncl_add_token(&cmdbuf, NCL_clw, NCL_comma);
			else if (Tdowndir == CCLW)
				ncl_add_token(&cmdbuf, NCL_ccw, NCL_comma);
		}
		if (Tiotype == THRU)
			ncl_add_token(&cmdbuf, NCL_thru, NCL_nocomma);
		else if (Tiotype == DOWN)
		{
			if (Tdowndir == 0)
				ncl_add_token(&cmdbuf, NCL_down, NCL_nocomma);
			else
			{
				ncl_add_token(&cmdbuf, NCL_down, NCL_comma);
				if (Tdowndir == SAME)
					ncl_add_token(&cmdbuf, NCL_same, NCL_comma);
				else if (Tdowndir == CLW)
					ncl_add_token(&cmdbuf, NCL_clw, NCL_comma);
				else if (Tdowndir == CCLW)
					ncl_add_token(&cmdbuf, NCL_ccw, NCL_comma);
			}
		}
		else
		{
			ncl_add_token(&cmdbuf, Tretrctval1, NCL_comma);
			if (Tretrctfed1 == 1)
			{
				ncl_add_token(&cmdbuf, NCL_rapid, NCL_comma);
			}
			else if (Tretrctfed1 == 2)
			{
				ncl_add_token(&cmdbuf, NCL_feedrat, NCL_comma);
				ncl_add_token(&cmdbuf, Tretrctfrv1_str, NCL_comma);
			}
			if (Traptofed1 > 0)
			{
				ncl_add_token(&cmdbuf, Traptoval1, NCL_comma);
				if (Traptofed1 == 1)
				{
					ncl_add_token(&cmdbuf, NCL_rapid, NCL_nocomma);
				}
				else if (Traptofed1 == 2)
				{
					ncl_add_token(&cmdbuf, NCL_feedrat, NCL_comma);
					ncl_add_token(&cmdbuf, Traptofrv1_str, NCL_nocomma);
				}
			}
			else
				ncl_add_token(&cmdbuf, Traptoval1, NCL_nocomma);

		}

		if (Tfinpass == 1)
		{
			ncl_add_token(&cmdbuf, NCL_last, NCL_comma);
			if (Tfinfed1 == 1)
			{
				ncl_add_token(&cmdbuf, NCL_feedrat, NCL_comma);
				ncl_add_token(&cmdbuf, Tfinfrv1_str, NCL_nocomma);
			}
		}
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
	return(UU_SUCCESS);
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
	if (multi != 2)
	{
		if (sfpt != UU_NULL) S_unhilite_entity(sfpt);
		if (sflst != UU_NULL) S_unhilite_entities(sflst);
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
**    I_FUNCTION     : S_form_vis()
**       Redisplays all Fmill forms.
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

	UM_real8 fhight, ffpstart, fspstart, fstep, frapto,frapto_dis,frtrct,frtrct_dis,fstopfdrt,fthick,favasn2,favfr2, favasn1,favfr1 ;
	int fnpas, sc10_2,sc10_3,sc10_4;

	const char tok[] = ",";
	char * tmp; 

	strcpy(Ssfnam, Tsfnam);
	strcpy(Scvnam, Tcvnam);
	Sraptotyp = Traptotyp;
	strcpy(Sraptoval, Traptoval);
	frapto = atof (Traptoval);
	Sraptofed = Traptofed;
	strcpy(Sraptofrv_str, Traptofrv_str);
	Sretrcttyp = Tretrcttyp;
	strcpy(Sretrctval, Tretrctval);
	frtrct = atof (Tretrctval);
	Sretrctfed = Tretrctfed;
	strcpy(Sretrctfrv_str, Tretrctfrv_str);
	Spasstyp = Tpasstyp;
	strcpy(Spassstr, Tpassstr);
	if (Tpasstyp == 0)
	{
		strcpy(Sscalht_str, Tpassstr);
		fhight = atof (Tpassstr);
		fnpas = 0;
	}
	else
	{
		strcpy(Snpass_str, Tpassstr);
		fnpas = atof (Tpassstr);
	}
	Sstarttyp = Tstarttyp;
	strcpy(Sstartstr, Tstartstr);
	if (Tstarttyp==2)
	{
		strcpy(Sptnam, Sstartstr);
		ffpstart = 0.0;
		fspstart = 0.0;
	}
	else if (Tstarttyp==1)
	{
		strcpy(Suvval_str, Sstartstr);
		tmp = strtok (Suvval_str,",");
		ffpstart = atof (tmp);
		while (tmp!= NULL)
		{
			if (tmp!=NULL)
			{
				tmp = strtok (NULL, ",");
				if (tmp!=NULL)
					fspstart = atof (tmp);
			}
		}

	}
	Sdir = Tdir;
	Sptcalc = Tptcalc;
	Somit = Tomit;
	strcpy(Ssteps_str, Tsteps_str);
	fstep = atof (Tsteps_str);
	Sedgetype = Tedgetype;
	Siotype = Tiotype;
	Sdowndir = Tdowndir;
	strcpy(Scsthk_str, Tcsthk_str);
	fthick=atof (Tcsthk_str);
	Sfinpass = Tfinpass;
	Sfinfed1 = Tfinfed1;
	strcpy(Sfinfrv1_str, Tfinfrv1_str);
	fstopfdrt = atof(Tfinfrv1_str);
	Sraptotyp1 = Traptotyp1;
	strcpy(Sraptoval1, Traptoval1);
	favasn2 = atof(Traptoval1);
	Sraptofed1 = Traptofed1;
	frapto_dis = atof(Traptofrv1_str);
	strcpy(Sraptofrv1_str, Traptofrv1_str);
	favfr2 = atof(Traptofrv1_str);
	Sretrcttyp1 = Tretrcttyp1;
	strcpy(Sretrctval1, Tretrctval1);
	frtrct_dis = atof(Tretrctval1);
	favasn1 = atof(Tretrctval1);
	Sretrctfed1 = Tretrctfed1;
	strcpy(Sretrctfrv1_str, Tretrctfrv1_str);
	favfr1 = atof(Tretrctfrv1_str);

	Ssfcol = Tsfcol;
	Scvcol = Tcvcol;
	Serapcol = Terapcol;
	Seretcol = Teretcol;
	Sstptcol = Tstptcol;
	Scsclr = Tcsclr;
	Sbrapcol = Tbrapcol;
	Sbretcol = Tbretcol;
/*
.....Save geometry colors
*/
	UN_unused_geo_flag = Tgeotog;
	UN_unused_geo_color = Tgeocol;
	nclu_save_preview_modals();

	fmillsav (&fnpas, &fhight, &ffpstart, &fspstart, &fstep, 
     &frapto,&frapto_dis,&frtrct,&frtrct_dis,&fstopfdrt,&fthick,&favasn2,&favfr2, &favasn1,&favfr1,&sc10_2,
     &sc10_3, &sc10_4);
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
		Ssecol[FMill] = Sred;

		ud_frm_enable_ok(UU_FALSE);
		if (*fieldno != -1)
		{
			for (i=FMill;i<=Colors;i++)
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
/*
.....Set Pick buttons to correct color
*/
	ud_dispfrm_set_attribs(0,CLRRG1,UM_BLACK,Tsfcol);
	ud_dispfrm_set_attribs(0,CLRRG2,UM_BLACK,Tcvcol);
	ud_dispfrm_set_attribs(0,CLRRG3,UM_BLACK,Terapcol);
	ud_dispfrm_set_attribs(0,CLRRG4,UM_BLACK,Teretcol);
	ud_dispfrm_set_attribs(0,CLRRG5,UM_BLACK,Tstptcol);
	ud_dispfrm_set_attribs(0,CLRRG6,UM_BLACK,Tcsclr);
	ud_dispfrm_set_attribs(0,CLRRG7,UM_BLACK,Tbrapcol);
	ud_dispfrm_set_attribs(0,CLRRG8,UM_BLACK,Tbretcol);
	ud_dispfrm_set_attribs(0,CLRRG10,UM_BLACK,Tgeocol);
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnCSDes1filedno, val, stat)
**       Method called when the Select button is pushed.
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
static UD_FSTAT OnDeselectAll(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Check for change since preview
*/
	Scsnum = UU_LIST_LENGTH(&Scslst);
	if (Scsnum > 0)
	{
		S_unhilite_entities(&Scslst);
		Scsnum = 0;
		if (Spreviewflg) Schgmade = 1;
	}
	ud_set_traverse_mask(BOUNDRG9, UU_FALSE);
	return (UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnSelect(filedno, val, stat)
**       Method called when the Select button is pushed.
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
	int pr, namfld, *mask, j;
	int j1 = 0;
	char *namp;
	int status;
/*
.....Set the appropriate selection mask and prompt
*/
	if (*fieldno == MILLRG1)
	{
		mask = (int *)UD_ncl_allsfpl;
		pr = 656;
		namp = Tsfnam;
		namfld = MILLRG2;
		j = 0;
		status = S_select_geo(&Sgsf,UU_NULL,mask,0,pr,Tsfcol,0,namfld,namp);
	}
	else if (*fieldno == MILLRG3)
	{
		mask = (int *)UD_ncl_offcv;
		pr = 657;
		namp = Tcvnam;
		namfld = MILLRG4;
		j = 1;
		status = S_select_geo(&Sgcv,UU_NULL,mask,0,pr,Tcvcol,0,namfld,namp);
	}
	else if (*fieldno == OPTRG5)
	{
		mask = (int *)UD_ncl_pt;
		pr = 658;
		namp = Tptnam;
		namfld = OPTRG4;
		j = 4;
		status = S_select_geo(&Sgspt,UU_NULL,mask,0,pr,Tstptcol,0,namfld,namp);
	}
	else if (*fieldno == ENEXTRG3)
	{
		mask = (int *)UD_ncl_allsfpl;
		pr = 659;
		namp = Traptoval;
		namfld = ENEXTRG2;
		j = 2;
		status = S_select_geo(&Sgerap,UU_NULL,mask,0,pr,Terapcol,0,namfld,namp);
	}
	else if (*fieldno == ENEXTRG8)
	{
		mask = (int *)UD_ncl_allsfpl;
		pr = 660;
		namp = Tretrctval;
		namfld = ENEXTRG7;
		j = 3;
		status = S_select_geo(&Sgeret,UU_NULL,mask,0,pr,Teretcol,0,namfld,namp);
	}
	else if (*fieldno == BOUNDRG4)
	{
		mask = (int *)UD_ncl_rmill_geo;
		pr = 424;
		status = S_select_geo(&Sgavd,&Scslst,mask,1,pr,Tcsclr,0,-1,NULL);
		Scsnum = UU_LIST_LENGTH(&Scslst);
		if (status!=-1)
			ud_set_traverse_mask(BOUNDRG9, UU_TRUE);
	}
	else if (*fieldno == RETRG8)
	{
		mask = (int *)UD_ncl_allsfpl;
		pr = 660;
		namp = Tretrctval;
		namfld = RETRG7;
		j = 7;
		status = S_select_geo(&Sgbret,UU_NULL,mask,0,pr,Tbretcol,0,namfld,namp);
	}
	else if (*fieldno == RETRG3)
	{
		mask = (int *)UD_ncl_allsfpl;
		pr = 659;
		namp = Traptoval1;
		namfld = RETRG2;
		j = 6; j1 = 5;
		status = S_select_geo(&Sgbrap,UU_NULL,mask,0,pr,Tbrapcol,0,namfld,namp);
	}
	else
		goto done;
	if (status!=-1)
	{
		if (Spreviewflg) Schgmade = 1;
		if ((*fieldno>=MILLRG1)&&(*fieldno<=4))
			Sacc[FMill] = 1;
		if ((*fieldno>=ENEXTRG1)&&(*fieldno<=ENEXTRG10))
			Sacc[Entry] = 1;
		if ((*fieldno>=OPTRG1)&&(*fieldno<=OPTRG9))
			Sacc[MotionT] = 1;
		if ((*fieldno>=BOUNDRG1)&&(*fieldno<=BOUNDRG8))
			Sacc[Boundary] = 1;
		if ((*fieldno>=RETRG1)&&(*fieldno<=RETRG10))
			Sacc[Boundary] = 1;
		S_enable_buttons();
	}
done:;
	S_enable_buttons();	
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnColor(fieldno, val, stat)
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
static UD_FSTAT OnColor(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int fno;

	ud_default_method(fieldno, val, stat);
	switch (*fieldno)
	{
	case CLRRG1:
		S_hilite_entity(&Sgsf,Tsfcol);
		break;
	case CLRRG2:
		S_hilite_entity(&Sgcv,Tcvcol);
		break;
	case CLRRG3:
		S_hilite_entity(&Sgerap,Terapcol);
		break;
	case CLRRG4:
		S_hilite_entity(&Sgeret,Teretcol);
		break;
	case CLRRG5:
		S_hilite_entity(&Sgspt,Tstptcol);
		break;
	case CLRRG6:
		S_hilite_entities(Scslst,Tcsclr);
		break;
	case CLRRG7:
		S_hilite_entity(&Sgbrap,Tbrapcol);
		break;
	case CLRRG8:
		S_hilite_entity(&Sgbret, Tbretcol);
		break;
	case CLRRG9:
		Tgeotog = val->frmint[0];
	case CLRRG10:
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
**    S_FUNCTION     :  OnAvoidchk(filedno, val, stat)
**       Method called to set traverse flags.
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
static UD_FSTAT OnAvoidchk(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i = -1;

	if (*fieldno!=AVOIDCHK)
	{
		*fieldno = -1;
		return(UD_FLDOK);
	}
	if (*(val->frmint) == 1)
	{
		ud_set_traverse_mask(BOUNDRG4,UU_TRUE);
		ud_set_traverse_mask(BOUNDRG5,UU_TRUE);
		if (UU_LIST_LENGTH(&Scslst)>0)
		{
			ud_set_traverse_mask(BOUNDRG9,UU_TRUE);
		}
		else
			ud_set_traverse_mask(BOUNDRG9,UU_FALSE);
	}
	else
	{
		ud_set_traverse_mask(BOUNDRG4,UU_FALSE);
		ud_set_traverse_mask(BOUNDRG5,UU_FALSE);
		ud_set_traverse_mask(BOUNDRG9,UU_FALSE);
	}
	if (*val->frmint != Savoid_chk) if (Spreviewflg) Schgmade = 1;
	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     :  OnPickType(filedno, val, stat)
**       Method called to set traverse flags.
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
static UD_FSTAT OnPickType(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i = -1;

	switch (*fieldno)
	{
	case OPTRG1:
		if (*(val->frmint) == 0)
		{
/*
.....remember the old value
*/
			if (Smenu_ch[4]!=Tpasstyp)
				strcpy(Tnpass_str, Tpassstr);
/*
......update the value to Sscalht_str 
*/
			strcpy(Tpassstr, Tscalht_str);
		}
		else
		{
			if (Smenu_ch[4]!=Tpasstyp)
				strcpy(Tscalht_str, Tpassstr);
/*
......update the value to Snpass_str 
*/
			strcpy(Tpassstr, Tnpass_str);
		}
		ud_update_answer(OPTRG2,(int *)Tpassstr);
		i = 4;
		break;
	case OPTRG3:
		if (*(val->frmint) == 0)
		{
			ud_set_traverse_mask(OPTRG4, UU_FALSE);
			ud_set_traverse_mask(OPTRG5, UU_FALSE);
			ud_set_traverse_mask(CLRRG5,UU_FALSE);
		}
		else if (*(val->frmint) == 1)
		{
/*
.....updated 'u,v' value
*/
			if (Smenu_ch[5]==2)
			{
				strcpy(Tptnam, Tstartstr);
			}
			ud_set_traverse_mask(OPTRG4, UU_TRUE);
			ud_set_traverse_mask(OPTRG5, UU_FALSE);
			ud_set_traverse_mask(CLRRG5,UU_FALSE);
			strcpy(Tstartstr, Tuvval_str);
			S_unhilite_entity(&Sgspt);
		}
		else
		{
/*
.....point
*/
			if (Smenu_ch[5]==1)
			{
				strcpy(Tuvval_str, Tstartstr);
			}
			ud_set_traverse_mask(OPTRG4, UU_TRUE);
			ud_set_traverse_mask(OPTRG5, UU_TRUE);
			ud_set_traverse_mask(CLRRG5,UU_TRUE);
			strcpy(Tstartstr, Tptnam);
			S_hilite_entity(&Sgspt,Tstptcol);
		}
		ud_update_answer(OPTRG4,(int *)Tstartstr);
		i = 5;
		break;
	case OPTRG6:
		i = 6;
		break;
	case OPTRG7:
		if (*(val->frmint) == 0)
		{
			ud_set_traverse_mask(OPTRG8,UU_FALSE);
		}
		else if (*(val->frmint) == 1)
		{
			if (Smenu_ch[7]==2)
				strcpy(Tnstep_str, Tsteps_str);
			ud_set_traverse_mask(OPTRG8,UU_TRUE);
			strcpy(Tsteps_str, Ttoler_str);
			ud_update_answer(OPTRG8,(int *)Tsteps_str);
		}
		else
		{
			if (Smenu_ch[7]==1)
				strcpy(Ttoler_str, Tsteps_str);
			ud_set_traverse_mask(OPTRG8,UU_TRUE);
			strcpy(Tsteps_str, Tnstep_str);
			ud_update_answer(OPTRG8,(int *)Tsteps_str);
		}
		i = 7;
		break;
	case ENEXTRG1:
		if (*(val->frmint) == 0)
		{
			ud_set_traverse_mask(ENEXTRG2,UU_FALSE);
			ud_set_traverse_mask(ENEXTRG3,UU_FALSE);
			ud_set_traverse_mask(CLRRG3,UU_FALSE);
			ud_set_traverse_mask(ENEXTRG4,UU_FALSE);
			ud_set_traverse_mask(ENEXTRG5,UU_FALSE);
		}
		else if (*(val->frmint) == 1)
		{
			ud_set_traverse_mask(ENEXTRG2,UU_TRUE);
			ud_set_traverse_mask(ENEXTRG3,UU_FALSE);
			ud_set_traverse_mask(CLRRG3,UU_FALSE);
			ud_set_traverse_mask(ENEXTRG4,UU_TRUE);
			if (Sraptofed == 2)
				ud_set_traverse_mask(ENEXTRG5,UU_TRUE);
			else
				ud_set_traverse_mask(ENEXTRG5,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(ENEXTRG2,UU_TRUE);
			ud_set_traverse_mask(ENEXTRG3,UU_TRUE);
			ud_set_traverse_mask(CLRRG3,UU_TRUE);
			ud_set_traverse_mask(ENEXTRG4,UU_TRUE);
			if (Sraptofed == 2)
				ud_set_traverse_mask(ENEXTRG5,UU_TRUE);
			else
				ud_set_traverse_mask(ENEXTRG5,UU_FALSE);
		}

	 	if (Traptotyp <= 1)
		{
			if (Smenu_ch[0]!=Traptotyp)
			{
				S_unhilite_entity(&Sgerap);
				strcpy(Traptoval, "0.");
			}
			ud_update_answer(ENEXTRG2,(int *)Traptoval);
		}
		else
		{
			if (Smenu_ch[0]!=Traptotyp)
			{
				strcpy(Traptoval, Sgerap.label);
			}
			S_hilite_entity(&Sgerap,Terapcol);
			ud_update_answer(ENEXTRG2,(int *)Traptoval);
		}
		i = 0;
		break;
	case ENEXTRG4:
		if (*(val->frmint) == 2)
		{
			ud_set_traverse_mask(ENEXTRG5,UU_TRUE);
			strcpy(Traptofrv_str, Sav_raptofrv_str);
			ud_update_answer(ENEXTRG5,(int *)Traptofrv_str);
		}
		else if (*(val->frmint) == 0)
		{
			ud_set_traverse_mask(ENEXTRG5,UU_FALSE);
			if (Sav_chc01==2)
				strcpy(Sav_raptofrv_str, Traptofrv_str);
			strcpy(Traptofrv_str, Sfeed_valuestr);
			ud_update_answer(ENEXTRG5,(int *)Traptofrv_str);
		}
		else
		{
			ud_set_traverse_mask(ENEXTRG5,UU_FALSE);
			if (Sav_chc01==2)
				strcpy(Sav_raptofrv_str, Traptofrv_str);
			Traptofrv_str[0] = '\0';
			ud_update_answer(ENEXTRG5,(int *)Traptofrv_str);
		}
		Sav_chc01 = *(val->frmint);
		i = 1;
		break;
	case ENEXTRG6:
		if (*(val->frmint) == 0)
		{
			ud_set_traverse_mask(ENEXTRG7,UU_FALSE);
			ud_set_traverse_mask(ENEXTRG8,UU_FALSE);
			ud_set_traverse_mask(CLRRG4,UU_FALSE);
			ud_set_traverse_mask(ENEXTRG9,UU_FALSE);
			ud_set_traverse_mask(ENEXTRG10,UU_FALSE);
		}
		else if (*(val->frmint) == 1)
		{
			ud_set_traverse_mask(ENEXTRG7,UU_TRUE);
			ud_set_traverse_mask(ENEXTRG8,UU_FALSE);
			ud_set_traverse_mask(CLRRG4,UU_FALSE);
			ud_set_traverse_mask(ENEXTRG9,UU_TRUE);
			if (Tretrctfed == 2)
				ud_set_traverse_mask(ENEXTRG10,UU_TRUE);
			else
				ud_set_traverse_mask(ENEXTRG10,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(ENEXTRG7,UU_TRUE);
			ud_set_traverse_mask(ENEXTRG8,UU_TRUE);
			ud_set_traverse_mask(CLRRG4,UU_TRUE);
			ud_set_traverse_mask(ENEXTRG9,UU_TRUE);
			if (Sretrctfed == 2)
				ud_set_traverse_mask(ENEXTRG10,UU_TRUE);
			else
				ud_set_traverse_mask(ENEXTRG10,UU_FALSE);
		}

		if (Tretrcttyp <= 1)
		{
			if (Smenu_ch[2]!=Tretrcttyp)
			{
				S_unhilite_entity(&Sgeret);
				strcpy(Tretrctval, "0.");
			}
			ud_update_answer(ENEXTRG7,(int *)Sretrctval);
		}
		else
		{
			if (Smenu_ch[2]!=Tretrcttyp)
			{
				strcpy(Sretrctval, Sgeret.label);
				ud_update_answer(ENEXTRG7,(int *)Sretrctval);
			}
			S_hilite_entity(&Sgeret,Teretcol);
		}
		i = 2;
		break;
	case ENEXTRG9:
		if (*(val->frmint) == 2)
		{
			ud_set_traverse_mask(ENEXTRG10,UU_TRUE);
			strcpy(Tretrctfrv_str, Sav_retrctfrv_str);
			ud_update_answer(ENEXTRG10,(int *)Tretrctfrv_str);
		}
		else if (*(val->frmint) == 0)
		{
			ud_set_traverse_mask(ENEXTRG10,UU_FALSE);
			if (Sav_chc02==2)
				strcpy(Sav_retrctfrv_str, Tretrctfrv_str);
			strcpy(Tretrctfrv_str, Sfeed_valuestr);
			ud_update_answer(ENEXTRG10,(int *)Tretrctfrv_str);
		}
		else
		{
			ud_set_traverse_mask(ENEXTRG10,UU_FALSE);
			if (Sav_chc02==2)
				strcpy(Sav_retrctfrv_str, Tretrctfrv_str);
			Tretrctfrv_str[0] = '\0';
			ud_update_answer(ENEXTRG10,(int *)Tretrctfrv_str);
		}
		Sav_chc02 = *(val->frmint);
		i = 3;
		break;
	case OPTRG9:
/*
....omit
*/
		i = 8;
		break;
	case BOUNDRG1:
		i = 9;
		break;
	case BOUNDRG2:
		if (*(val->frmint) != OVER && *(val->frmint) != BOTH)
		{
			ud_set_traverse_mask(RETRG1,UU_FALSE);
			ud_set_traverse_mask(RETRG2,UU_FALSE);
			ud_set_traverse_mask(RETRG3,UU_FALSE);
			ud_set_traverse_mask(RETRG4,UU_FALSE);
			ud_set_traverse_mask(RETRG5,UU_FALSE);
			ud_set_traverse_mask(RETRG6,UU_FALSE);
			ud_set_traverse_mask(RETRG7,UU_FALSE);
			ud_set_traverse_mask(RETRG8,UU_FALSE);
			ud_set_traverse_mask(RETRG9,UU_FALSE);
			ud_set_traverse_mask(RETRG10,UU_FALSE);
			ud_set_traverse_mask(CLRRG7,UU_FALSE);
			ud_set_traverse_mask(CLRRG8,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(RETRG1,UU_TRUE);
			ud_set_traverse_mask(RETRG2,UU_TRUE);
			ud_set_traverse_mask(RETRG4,UU_TRUE);
			ud_set_traverse_mask(RETRG6,UU_TRUE);
			ud_set_traverse_mask(RETRG7,UU_TRUE);
			ud_set_traverse_mask(RETRG9,UU_TRUE);
			if (Tretrcttyp1 == DIS)
			{
				ud_set_traverse_mask(RETRG8,UU_FALSE);
				ud_set_traverse_mask(CLRRG8,UU_FALSE);
			}
			else
			{
				ud_set_traverse_mask(RETRG8,UU_TRUE);
				ud_set_traverse_mask(CLRRG8,UU_TRUE);
			}
			if (Tretrctfed1 == 2)
				ud_set_traverse_mask(RETRG10,UU_TRUE);
			else
				ud_set_traverse_mask(RETRG10,UU_FALSE);

			if (Traptotyp1 == DIS)
			{
				ud_set_traverse_mask(RETRG3,UU_FALSE);
				ud_set_traverse_mask(CLRRG7,UU_FALSE);
			}
			else
			{
				ud_set_traverse_mask(RETRG3,UU_TRUE);
				ud_set_traverse_mask(CLRRG7,UU_TRUE);
			}
			if (Traptofed1 == 2)
				ud_set_traverse_mask(RETRG5,UU_TRUE);
			else
				ud_set_traverse_mask(RETRG5,UU_FALSE);

		}
		if (*(val->frmint) == DOWN || *(val->frmint) == BOTH)
			ud_set_traverse_mask(BOUNDRG3,UU_TRUE);
		else
			ud_set_traverse_mask(BOUNDRG3,UU_FALSE);
		i = 10;
		break;
	case BOUNDRG3:
		i = 11;
		break;

	case RETRG6:
		if (*(val->frmint) == DIS)
		{
			ud_set_traverse_mask(RETRG8,UU_FALSE);
			ud_set_traverse_mask(CLRRG8,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(RETRG8,UU_TRUE);
			ud_set_traverse_mask(CLRRG8,UU_TRUE);
		}
		if (Tretrctfed1 == 2)
			ud_set_traverse_mask(RETRG10,UU_TRUE);
		else
			ud_set_traverse_mask(RETRG10,UU_FALSE);

		if (Tretrcttyp1 < 1)
		{
			if (Smenu_ch[16]!=Tretrcttyp1)
			{
				S_unhilite_entity(&Sgbret);
				strcpy(Tretrctval1, "0.");
			}
			ud_update_answer(RETRG7,(int *)Tretrctval1);
		}
		else
		{
			if (Smenu_ch[16]!=Tretrcttyp1)
			{
				strcpy(Tretrctval1, Sgbret.label);
				ud_update_answer(RETRG7,(int *)Tretrctval1);
			}
			S_hilite_entity(&Sgbret,Teretcol);
		}
		i = 16;
		break;

	case RETRG9:
		if (*(val->frmint) == 2)
		{
			ud_set_traverse_mask(RETRG10,UU_TRUE);
			strcpy(Tretrctfrv1_str, Sav_retrctfrv1_str);
			ud_update_answer(RETRG10,(int *)Tretrctfrv1_str);
		}
		else if (*(val->frmint) == 0)
		{
			ud_set_traverse_mask(RETRG10,UU_FALSE);
			if (Sav_chc2==2)
				strcpy(Sav_retrctfrv1_str, Tretrctfrv1_str);
			strcpy(Tretrctfrv1_str, Sfeed_valuestr);
			ud_update_answer(RETRG10,(int *)Tretrctfrv1_str);
		}
		else
		{
			ud_set_traverse_mask(RETRG10,UU_FALSE);
			if (Sav_chc2==2)
				strcpy(Sav_retrctfrv1_str, Tretrctfrv1_str);
			Tretrctfrv1_str[0] = '\0';
			ud_update_answer(RETRG10,(int *)Tretrctfrv1_str);
		}
		Sav_chc2 = *(val->frmint);
		i = 17;
		break;

	case RETRG1:
		if (*(val->frmint) == DIS)
		{
			ud_set_traverse_mask(RETRG3,UU_FALSE);
			ud_set_traverse_mask(CLRRG7,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(RETRG3,UU_TRUE);
			ud_set_traverse_mask(CLRRG7,UU_TRUE);
		}
		if (Traptofed1 == 2)
			ud_set_traverse_mask(RETRG5,UU_TRUE);
		else
			ud_set_traverse_mask(RETRG5,UU_FALSE);

		if (Traptotyp1 < 1)
		{
			if (Smenu_ch[14]!=Traptotyp1)
			{
				S_unhilite_entity(&Sgbrap);
				strcpy(Traptoval1, "0.");
			}
			ud_update_answer(RETRG2,(int *)Traptoval1);
		}
		else
		{
			if (Smenu_ch[14]!=Traptotyp1)
			{
				strcpy(Traptoval1, Sgbrap.label);
				ud_update_answer(RETRG2,(int *)Traptoval1);
			}
			S_hilite_entity(&Sgbrap,Terapcol);
		}
		i = 14;
		break;

	case RETRG4:
		if (*(val->frmint) == 2)
		{
			ud_set_traverse_mask(RETRG5,UU_TRUE);
			strcpy(Traptofrv1_str, Sav_raptofrv1_str);
			ud_update_answer(RETRG5,(int *)Traptofrv1_str);
		}
		else if (*(val->frmint) == 0)
		{
			ud_set_traverse_mask(RETRG5,UU_FALSE);
			if (Sav_chc1==2)
				strcpy(Sav_raptofrv1_str, Traptofrv1_str);
			strcpy(Traptofrv1_str, Sfeed_valuestr);
			ud_update_answer(RETRG5,(int *)Traptofrv1_str);
		}
		else
		{
			ud_set_traverse_mask(RETRG5,UU_FALSE);
			if (Sav_chc1==2)
				strcpy(Sav_raptofrv1_str, Traptofrv1_str);
			Traptofrv1_str[0] = '\0';
			ud_update_answer(RETRG5,(int *)Traptofrv1_str);
		}
		Sav_chc1 = *(val->frmint);
		i = 15;
		break;
	case BOUNDRG6:
		if (*(val->frmint) == 1)
		{
			ud_set_traverse_mask(BOUNDRG7,UU_TRUE);
			if (Tfinfed1 == 1)
				ud_set_traverse_mask(BOUNDRG8,UU_TRUE);
			else
				ud_set_traverse_mask(BOUNDRG8,UU_FALSE);
		}
		else
			ud_set_traverse_mask(BOUNDRG7,UU_FALSE);
		if (*val->frmint != Stog_ch1) if (Spreviewflg) Schgmade = 1;
		i = 12;
		break;
	case BOUNDRG7:
		if (*(val->frmint) == 1)
		{
			ud_set_traverse_mask(BOUNDRG8,UU_TRUE);
			strcpy(Tfinfrv1_str, Sav_finfrv1_str);
			ud_update_answer(BOUNDRG8,(int *)Tfinfrv1_str);
		}
		else
		{
			ud_set_traverse_mask(BOUNDRG8,UU_FALSE);
			strcpy(Sav_finfrv1_str, Tfinfrv1_str);
			strcpy(Tfinfrv1_str, Sfeed_valuestr);
			ud_update_answer(BOUNDRG8,(int *)Tfinfrv1_str);
		}
		i = 13;
		break;
	}
/*
.....Check for change since preview
*/
	if (i >= 0 && Smenu_ch[i] != *val->frmint)
	{
		Smenu_ch[i] = *val->frmint;
		if (Spreviewflg) Schgmade = 1;
		if ((*fieldno>=ENEXTRG1)&&(*fieldno<=ENEXTRG10))
			Sacc[Entry] = 1;
		if ((*fieldno>=OPTRG1)&&(*fieldno<=OPTRG9))
			Sacc[MotionT] = 1;
		if ((*fieldno>=BOUNDRG1)&&(*fieldno<=BOUNDRG8))
			Sacc[Boundary] = 1;
		if ((*fieldno>=RETRG1)&&(*fieldno<=RETRG10))
			Sacc[Boundary] = 1;
		S_enable_buttons();
	}
	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     :  OnCSThk1(fieldno, val, stat)
**       Method called when the Thick edit field is executed.
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
static UD_FSTAT OnCSThk1(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UM_sgeo *geo;
/*
.....Check for change since preview
*/
	Scsnum = UU_LIST_LENGTH(&Scslst);
	if (Scsnum > 0)
	{		
		geo = (UM_sgeo *) UU_LIST_ARRAY (&Scslst);
		Tcsthk = atof(Tcsthk_str);
		geo[Scsnum - 1].thick = Tcsthk;
	}
	if (strcmp(Scmd4_str[0],val->frmstr) != 0)
	{
		if (Spreviewflg) Schgmade = 1;
		strcpy(Scmd4_str[0],val->frmstr);
	}
	return (UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnView(filedno, val, stat)
**       Routine to enter dynamic viewing from the Fmill form.
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
static UD_FSTAT OnView(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	ud_form_invis();
	uz_dyn_mouse();
	ud_form_vis();
	
	return(UD_FLDOK);
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
	nc = (int)strlen(label);
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
	int j;

	j = -1;

	if ((*fieldno>=MILLRG2)&&(*fieldno<=MILLRG4))
	{
		switch(*fieldno)
		{
			case MILLRG2:
				j = 0;
				S_init_geo(&Sgsf, Tsfnam, Tsfcol);
				break;
			case MILLRG4:
				j = 1;
				S_init_geo(&Sgcv, Tcvnam, Tcvcol);
				break;
		}
		if ((j >= 0 && strcmp(Scmd1_str[j], val->frmstr) != 0))
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd1_str[j],val->frmstr);
		}
		Sacc[FMill] = 1;
		S_enable_buttons();
		return(UD_FLDOK);
	}
	if ((*fieldno>=ENEXTRG2)&&(*fieldno<=ENEXTRG10))
	{
		switch(*fieldno)
		{
			case ENEXTRG2:
				if (Traptotyp>1)
					S_init_geo(&Sgerap, Traptoval, Terapcol);
				j = 0;
				break;
			case ENEXTRG5:
				j = 1;
				break;
			case ENEXTRG7:
				if (Tretrcttyp>1)
					S_init_geo(&Sgeret, Tretrctval, Teretcol);
				j = 2;
				break;
			case ENEXTRG10:
				j = 3;
				break;
		}
		if ((j >= 0 && strcmp(Scmd2_str[j], val->frmstr) != 0))
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd2_str[j],val->frmstr);
		}
		Sacc[Entry] = 1;
		S_enable_buttons();
		return(UD_FLDOK);
	}
	if ((*fieldno>=OPTRG2)&&(*fieldno<=OPTRG8))
	{
		switch(*fieldno)
		{
			case OPTRG2:
				if (Tpasstyp==0)
					j = 0;
				else
					j = 1;
				if (Tpasstyp == 0)
				{
/*
......update the value to Sscalht_str 
*/
					strcpy(Tscalht_str, Tpassstr);
				}
				else
				{
/*
......update the value to Snpass_str 
*/
					strcpy(Tnpass_str, Tpassstr);
				}
				break;
			case OPTRG4:
				if (Tstarttyp==1)
				{
					j = 2;
				}
				else if (Tstarttyp==2)
				{
					S_init_geo(&Sgspt, Sstartstr, Tstptcol);
					j = 3;
				}
				if (Tstarttyp==2)
				{
					strcpy(Tptnam, Tstartstr);
				}
				else if (Tstarttyp==1)
				{
					strcpy(Tuvval_str, Tstartstr);
				}
				break;
			case OPTRG8:
				if (Tptcalc==1)
					j = 4;
				else if (Sstarttyp==2)
					j = 5;
				break;
		}
		if ((j >= 0 && strcmp(Scmd3_str[j], val->frmstr) != 0))
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd3_str[j],val->frmstr);
		}
		Sacc[MotionT] = 1;
		S_enable_buttons();
		return(UD_FLDOK);
	}
	if ((*fieldno>=BOUNDRG5)&&(*fieldno<=BOUNDRG8))
	{
		switch(*fieldno)
		{
			case BOUNDRG5:
				j = 0;
				break;
			case BOUNDRG8:
				j = 1;
				break;
		}
		if ((j >= 0 && strcmp(Scmd4_str[j], val->frmstr) != 0))
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd4_str[j],val->frmstr);
		}
		Sacc[Boundary] = 1;
		S_enable_buttons();
		return(UD_FLDOK);
	}
	if ((*fieldno>=RETRG2)&&(*fieldno<=RETRG10))
	{
		switch(*fieldno)
		{
			case RETRG2:
				j = 0;
				if (Traptotyp1>0)
					S_init_geo(&Sgbrap, Traptoval1, Tbrapcol);
				break;
			case RETRG5:
				j = 1;
				break;
			case RETRG7:
				j = 2;
				if (Tretrcttyp1>0)
					S_init_geo(&Sgbret, Tretrctval1, Tbretcol);
				break;
			case RETRG10:
				j = 3;
				break;
		}
		if ((j >= 0 && strcmp(Scmd5_str[j], val->frmstr) != 0))
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd5_str[j],val->frmstr);
		}
		Sacc[Boundary] = 1;
		S_enable_buttons();
		return(UD_FLDOK);
	}
	return(UD_FLDOK);
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
	Sgsf.key = Sgcv.key = Sgerap.key = Sgeret.key = Sgspt.key = 0;
	Sgavd.key = Sgbrap.key = Sgbret.key = 0;
	Sgsf.color = Sgcv.color = Sgerap.color = Sgeret.color = Sgspt.color = -1;
	Sgavd.color = Sgbrap.color = Sgbret.color = -1;
	Sgsf.label[0] = Sgcv.label[0] = Sgerap.label[0] = Sgeret.label[0] = Sgspt.label[0] = '\0';
	Sgavd.label[0] = Sgbrap.label[0] = Sgbret.label[0] ='\0';
	Tsfnam[0] = Tcvnam[0] = '\0';
	if (Traptotyp>1)
		Traptoval[0] = '\0';
	if (Tretrcttyp>1)
		Tretrctval[0] = '\0';
	if (Tstarttyp==2)
		Tstartstr[0] = '\0';
	if (Traptotyp1>0)
		Traptoval1[0] = '\0';
	if (Tretrcttyp1>0)
		Tretrctval1[0] = '\0';
	ud_update_answer(MILLRG2,(int *)Tsfnam);
	ud_update_answer(MILLRG4,(int *)Tcvnam);
	if (Traptotyp>1)
		ud_update_answer(ENEXTRG2,(int *)Traptoval);
	if (Tretrcttyp>1)
		ud_update_answer(ENEXTRG7,(int *)Tretrctval);
	if (Tstarttyp==2)
		ud_update_answer(OPTRG4,(int *)Tstartstr);
	if (Traptotyp1>0)
		ud_update_answer(RETRG2,(int *)Traptoval1);
	if (Tretrcttyp1>0)
		ud_update_answer(RETRG7,(int *)Tretrctval1);
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
	S_unhilite_entities(&Scslst);
	Scsnum = 0; 
	S_unhilite_entity(&Sgsf);
	S_unhilite_entity(&Sgcv);
	S_unhilite_entity(&Sgerap);
	S_unhilite_entity(&Sgeret);
	S_unhilite_entity(&Sgspt);
	S_unhilite_entity(&Sgbrap);
	S_unhilite_entity(&Sgbret);
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
**    S_FUNCTION     :  OnAction()
**       Method called when Direction menu is traversed.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : UU_SUCCESS
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
		break;
/*
.....APPLY
.....A curve must be selected
*/
	case FAPY:
/*
........Output command
*/
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
			Spreviewflg = UU_FALSE;
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
		Sacc[FMill] = Sacc[MotionT] = Sacc[Entry] = Sacc[Boundary] = Sacc[Colors] = 0;
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
.....Store values to check for changes
*/
/*
.....FMill
*/
	strcpy(Scmd1_str[0],Tsfnam);
	strcpy(Scmd1_str[1],Tcvnam);
/*
.....Entry/Exit
*/
	strcpy(Scmd2_str[0],Traptoval);
	strcpy(Scmd2_str[1],Traptofrv_str);
	strcpy(Scmd2_str[2],Tretrctval);
	strcpy(Scmd2_str[3],Tretrctfrv_str);
/*
.....Motion Type
*/
	strcpy(Scmd3_str[0], Tscalht_str);
	strcpy(Scmd3_str[1], Tnpass_str);
	strcpy(Scmd3_str[2], Tuvval_str);
	strcpy(Scmd3_str[3], Tptnam);
	strcpy(Scmd3_str[4], Ttoler_str);
	strcpy(Scmd3_str[5], Tnstep_str);
/*
......boundaries
*/
	strcpy(Scmd4_str[0], Tcsthk_str);
	strcpy(Scmd4_str[1], Tfinfrv1_str);
	strcpy(Scmd5_str[0],Traptoval1);
	strcpy(Scmd5_str[1],Traptofrv1_str);
	strcpy(Scmd5_str[2],Tretrctval1);
	strcpy(Scmd5_str[3],Tretrctfrv1_str);
/*
....Entry
*/
	Smenu_ch[0] = Traptotyp;
	Smenu_ch[1] = Traptofed;
	Smenu_ch[2] = Tretrcttyp;
	Smenu_ch[3] = Tretrctfed;
/*
.....Motion Type
*/
	Smenu_ch[4] = Tpasstyp;
	Smenu_ch[5] = Tstarttyp;
	Smenu_ch[6] = Tdir;
	Smenu_ch[7] = Tptcalc;
	Smenu_ch[8] = Tomit;
/*
.....Boundaries
*/
	Smenu_ch[9] = Tedgetype;
	Smenu_ch[10] = Tiotype;
	Smenu_ch[11] = Tdowndir;

	Smenu_ch[12] = Tfinpass;
	Smenu_ch[13] = Tfinfed1;
	Stog_ch1 = Tfinpass;
	Smenu_ch[14] = Traptotyp1;
	Smenu_ch[15] = Traptofed1;
	Smenu_ch[16] = Tretrcttyp1;
	Smenu_ch[17] = Tretrctfed1;

	return(UD_FLDOK);
}

static void S_init_traverse(display,traverse)
char *display,*traverse;
{
	int i;
/*
.....Set traverse fields
*/
	if (Tstarttyp == 0)
	{
		traverse[OPTRG4]  = 0;
		traverse[OPTRG5]  = 0;
		traverse[CLRRG5]  = 0;
	}
	else if (Tstarttyp == 1)
	{
		traverse[OPTRG4]  = 1;
		traverse[OPTRG5]  = 0;
		traverse[CLRRG5]  = 0;
	}
	else
	{
		traverse[OPTRG4]  = 1;
		traverse[OPTRG5]  = 1;
		traverse[CLRRG5]  = 1;
	}

	if (Tptcalc == 0)
	{
		traverse[OPTRG8] = 0;
	}
	else
	{
		traverse[OPTRG8] = 0;
	}

	if (Traptotyp == 0)
	{
		traverse[ENEXTRG2] = 0;
		traverse[ENEXTRG3] = 0;
		traverse[ENEXTRG4] = 0;
		traverse[ENEXTRG5] = 0;
		traverse[CLRRG3]  = 0;
	}
	else 
	{
		traverse[ENEXTRG4] = 1;
		if (Traptofed == 2)
		{
			traverse[ENEXTRG5] = 1;
		}
		else
		{
			traverse[ENEXTRG5] = 0;
		}
		if (Traptotyp == 1)
		{
			traverse[ENEXTRG2] = 1;
			traverse[ENEXTRG3] = 0;
			traverse[CLRRG3] = 0;
		}
		else
		{
			traverse[ENEXTRG2] = 1;
			traverse[ENEXTRG3] = 1;
			traverse[CLRRG3] = 1;
		}
	}
	if (Tretrcttyp == 0)
	{
		traverse[ENEXTRG7] = 0;
		traverse[ENEXTRG8] = 0;
		traverse[ENEXTRG9] = 0;
		traverse[ENEXTRG10] = 0;
		traverse[CLRRG4]  = 0;
	}
	else
	{
		traverse[ENEXTRG9] = 1;
		if (Tretrctfed == 2)
		{
			traverse[ENEXTRG10] = 1;
		}
		else
		{
			traverse[ENEXTRG10] = 0;
		}
		if (Tretrcttyp == 1)
		{
			traverse[ENEXTRG7] = 1;
			traverse[ENEXTRG8] = 0;
			traverse[CLRRG4]  = 0;
		}
		else
		{
			traverse[ENEXTRG7] = 1;
			traverse[ENEXTRG8] = 1;
			traverse[CLRRG4]  = 1;
		}
	}
	if (Tiotype != OVER && Tiotype != BOTH)
	{
		traverse[RETRG1]=traverse[RETRG2]=traverse[RETRG3] = UU_FALSE;
		traverse[RETRG4]=traverse[RETRG5] = UU_FALSE;
		traverse[RETRG6]=traverse[RETRG7]=traverse[RETRG8] = UU_FALSE;
		traverse[RETRG9]=traverse[RETRG10] = UU_FALSE;
		traverse[CLRRG7]=traverse[CLRRG8] = UU_FALSE;
	}
	else
	{
		traverse[RETRG1]=traverse[RETRG2]=UU_TRUE;
		traverse[RETRG4]=UU_TRUE;

		if (Traptotyp1 == DIS)
		{
			traverse[RETRG3] = traverse[CLRRG7] = 0;
		}
		else
		{
			traverse[RETRG3] = traverse[CLRRG7] = 1;
		}

		if (Traptofed1 == FDR)
			traverse[RETRG5] = 1;
		else
			traverse[RETRG5] = 0;

		traverse[RETRG6]=traverse[RETRG7]=UU_TRUE;
		traverse[RETRG9]=UU_TRUE;
		if (Tretrcttyp1 == DIS)
		{
			traverse[RETRG8] = traverse[CLRRG8] = 0;
		}
		else
		{
			traverse[RETRG8] = traverse[CLRRG8] = 1;
		}
		if (Tretrctfed1 == FDR)
			traverse[RETRG10] = 1;
		else
			traverse[RETRG10] = 0;
	}
	if (Tiotype == DOWN || Tiotype == BOTH)
		traverse[BOUNDRG3] = 1;
	else
		traverse[BOUNDRG3] = 0;

	if (Tfinpass == 1)
	{
		traverse[BOUNDRG7] = 1;
		if (Tfinfed1==0)
			traverse[BOUNDRG8] = 0;
		else
			traverse[BOUNDRG8] = 1;
	}
	else
	{
		traverse[BOUNDRG7] = 0;
		traverse[BOUNDRG8] = 0;
	}
	if (Tavoid_chk)
	{
		traverse[BOUNDRG4] = 1;
		traverse[BOUNDRG5] = 1;
	}
	else
	{
		traverse[BOUNDRG4] = 0;
		traverse[BOUNDRG5] = 0;
	}
	traverse[BOUNDRG9] = 0;
/*
.....Action item fields
*/
	for (i=FAPV;i<FAVW;i++) traverse[i] = 0;
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

	UM_real8 fhight, ffpstart, fspstart, fstep, frapto,frapto_dis,frtrct,frtrct_dis,fstopfdrt,fthick,favasn2,favfr2, favasn1,favfr1;
	int fnpas, sc10_2,sc10_3,sc10_4;

	char buf[10];

	char fpstartval[NCL_MAX_LABEL_AND_SUBSCRIPT];
	char spstartval[NCL_MAX_LABEL_AND_SUBSCRIPT];
	char fstep_str[NCL_MAX_LABEL_AND_SUBSCRIPT];
	char frtrct_str[NCL_MAX_LABEL_AND_SUBSCRIPT];
	char frtrct_dis_str[NCL_MAX_LABEL_AND_SUBSCRIPT];
	char frapto_dis_str[NCL_MAX_LABEL_AND_SUBSCRIPT];
	char fstopfdrt_str[NCL_MAX_LABEL_AND_SUBSCRIPT];
	char favasn2_str[NCL_MAX_LABEL_AND_SUBSCRIPT];
	char favfr2_str[NCL_MAX_LABEL_AND_SUBSCRIPT];
	char favasn1_str[NCL_MAX_LABEL_AND_SUBSCRIPT];
	char favfr1_str[NCL_MAX_LABEL_AND_SUBSCRIPT];

	fmilpar(&fnpas, &fhight, &ffpstart, &fspstart, &fstep, &frapto,&frapto_dis,&frtrct,&frtrct_dis,&fstopfdrt,&fthick,&favasn2,&favfr2, &favasn1,&favfr1, &sc10_2,&sc10_3,&sc10_4 );
/*
.....Initialize the form settings
*/
	NclxMotGetFeedrate(&fedrat);
	feedrate = fedrat.base_feedrate;
	ncl_val2str (feedrate, Sfeed_valuestr);
	if (!Sform_init)
	{
		Ssfnam[0] = '\0';
		Ssfcol = NCLX_BROWN;
		Scvnam[0] = '\0';
		Scvcol = NCLX_TAN;

		Sraptotyp = 1;
		Serapcol = NCLX_LT_BLUE;
		
		Sraptofed = 0;
		Sretrctfed = 0;

		Sretrcttyp = 0;
		Sretrctval[0] = '\0';

		Seretcol = NCLX_PINK;
		Sstptcol = NCLX_SEA_GREEN;
		Scsclr = NCLX_ORANGE;

		Sbrapcol = NCLX_PURPLE;
		Sbretcol = NCLX_GREY;
		Sraptotyp1 = 0;
	}	
	if (Sraptofed==0)
		strcpy(Sraptofrv_str,Sfeed_valuestr);
	else if (Sraptofed==1)
		Sraptofrv_str[0] = '\0';
	if (Sretrctfed==0)
		strcpy(Sretrctfrv_str, Sfeed_valuestr);
	else if (Sretrctfed==1)
		Sretrctfrv_str[0] = '\0';
	strcpy(Sav_raptofrv_str, Sraptofrv_str);
	strcpy(Sav_retrctfrv_str, Sretrctfrv_str);

	ncl_sprintf(Sraptoval, &frapto,1);
	ncl_sprintf(frtrct_str, &frtrct,1);
	strcpy(Sretrctval,frtrct_str);
	ncl_sprintf(Sscalht_str, &fhight,1);
	itoa(fnpas,buf,10);  
	strcpy(Spassstr, buf);
	strcpy(Snpass_str, Spassstr);

	ncl_sprintf(fpstartval, &ffpstart,1);
	ncl_sprintf(spstartval, &fspstart,1);
	if (Tstarttyp==1)
	{
		strcpy(Sstartstr, fpstartval);
		strcat(Sstartstr, ",");
		strcat(Sstartstr, spstartval);
	}
	ncl_sprintf(fstep_str, &fstep,1);
	strcpy(Ssteps_str,fstep_str);

	ncl_sprintf(fstopfdrt_str,&fthick,1);
	strcpy(Scsthk_str, fstopfdrt_str);

	ncl_sprintf(fstopfdrt_str,&fstopfdrt,1);
	if (Sfinfed1==0)
	{
/*
......current
*/
		strcpy(Sfinfrv1_str, Sfeed_valuestr);
	}
	else
	{
/*
......value
*/
		strcpy(Sfinfrv1_str, fstopfdrt_str);
	}
	strcpy(Sav_finfrv1_str, Sfinfrv1_str);
	ncl_sprintf(favasn2_str,&favasn2,1);
	strcpy(Sraptoval1, favasn2_str);
	ncl_sprintf(favfr2_str,&favfr2,1);
	if (Sraptofed1==2)
		strcpy(Sraptofrv1_str, favfr2_str);
	else if (Sraptofed1==0)
//current
		strcpy(Sraptofrv1_str, Sfeed_valuestr);
	else
//rapid
		Sraptofrv1_str[0] = '\0';
	strcpy(Sav_raptofrv1_str, Sraptofrv1_str);
	Sav_chc1 = Sraptofed1;

	ncl_sprintf(favasn1_str,&favasn1,1);
	strcpy(Sretrctval1, favasn1_str);
	ncl_sprintf(favfr1_str,&favfr1,1);
	if (Sretrctfed1==2)
		strcpy(Sretrctfrv1_str, favfr1_str);
	else if (Sretrctfed1==0)
//current
		strcpy(Sretrctfrv1_str, Sfeed_valuestr);
	else
//rapid
		Sretrctfrv1_str[0] = '\0';
	strcpy(Sav_retrctfrv1_str, Sretrctfrv1_str);
	Sav_chc2 = Sretrctfed1;

	Stoler = atof(Stoler_str);
	if (Stoler < UM_DFUZZ)
	{
		gettol(&tol);
		Stoler = tol;
	}
	Sscalht = atof(Sscalht_str);
	if (Sscalht < UM_DFUZZ)
	{
		Sscalht = Stoler * 10;
	}

	itoa(fnpas,buf,10);  
	strcpy(Spassstr, buf);
	strcpy(Snpass_str, Spassstr);
	ncl_sprintf(Sscalht_str, &fhight,1);
	ncl_sprintf(Stoler_str, &Stoler,1);
	strcpy(Tscalht_str, Sscalht_str);
	strcpy(Tnpass_str, Snpass_str);
	strcpy(Ttoler_str, Stoler_str);
	ncl_sprintf(Sraptoval, &frapto,1);

	strcpy(Tsfnam, Ssfnam);
	strcpy(Tcvnam, Scvnam);
	Traptotyp = Sraptotyp;
	strcpy(Traptoval, Sraptoval);
	Traptofed = Sraptofed;
	strcpy(Traptofrv_str, Sraptofrv_str);
	Tretrcttyp = Sretrcttyp;
	strcpy(Tretrctval, Sretrctval);
	Tretrctfed = Sretrctfed;
	strcpy(Tretrctfrv_str, Sretrctfrv_str);
	Tpasstyp = Spasstyp;

	if (Tpasstyp == 0)
	{
/*
......update the value to Sscalht_str 
*/
		strcpy(Tpassstr, Tscalht_str);
	}
	else
	{
/*
......update the value to Snpass_str 
*/
		strcpy(Tpassstr, Tnpass_str);
	}
	ncl_sprintf(fpstartval, &ffpstart,1);
	ncl_sprintf(spstartval, &fspstart,1);
	if (Tstarttyp==1)
	{
		strcpy(Sstartstr, fpstartval);
		strcat(Sstartstr, ",");
		strcat(Sstartstr, spstartval);
	}

//  Sstartstr[0] = '\0';
	ncl_sprintf(fstep_str, &fstep,1);
	strcpy(Ssteps_str,fstep_str);
	Tstarttyp = Sstarttyp;
	strcpy(Tstartstr, Sstartstr);
	strcpy(Tptnam, Sptnam);
	strcpy(Tuvval_str, Suvval_str);
	if (Tstarttyp==2)
	{
		strcpy(Tstartstr, Tptnam);
	}
	else if (Tstarttyp==1)
	{
		strcpy(Tstartstr, Tuvval_str);
	}
	Tdir = Sdir;
	Tptcalc = Sptcalc;
	Tomit = Somit;
	strcpy(Tsteps_str, Ssteps_str);
	Tedgetype = Sedgetype;
	Tiotype = Siotype;
	Tdowndir = Sdowndir;
	strcpy(Tcsthk_str, Scsthk_str);
	Tfinpass = Sfinpass;
	Tfinfed1 = Sfinfed1;
	strcpy(Tfinfrv1_str, Sfinfrv1_str);
	Traptotyp1 = Sraptotyp1;
	strcpy(Traptoval1, Sraptoval1);
	Traptofed1 = Sraptofed1;
	strcpy(Traptofrv1_str, Sraptofrv1_str);
	Tretrcttyp1 = Sretrcttyp1;
	strcpy(Tretrctval1, Sretrctval1);
	Tretrctfed1 = Sretrctfed1;
	ncl_sprintf(favasn2_str,&favasn2,1);
	strcpy(Sraptoval1, favasn2_str);
	ncl_sprintf(favfr2_str,&favfr2,1);
//this value assigned above already, why here? different?
//	strcpy(Sraptofrv1_str, favfr2_str);
	ncl_sprintf(favasn1_str,&favasn1,1);
	strcpy(Sretrctval1, favasn1_str);
	ncl_sprintf(favfr1_str,&favfr1,1);
//this value assigned above already, why here? different?
//	strcpy(Sretrctfrv1_str, favfr1_str);
	strcpy(Tretrctfrv1_str, Sretrctfrv1_str);
	Tsfcol = Ssfcol;
	Tcvcol = Scvcol;
	Terapcol = Serapcol;
	Teretcol = Seretcol;
	Tstptcol = Sstptcol;
	Tcsclr = Scsclr;
	Tbrapcol = Sbrapcol;
	Tbretcol = Sbretcol;
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
	Sgsf.key = Sgcv.key = Sgerap.key = Sgeret.key = Sgspt.key = 0;
	Sgavd.key = Sgbrap.key = Sgbret.key = 0;
	Sgsf.color = Sgcv.color = Sgerap.color = Sgeret.color = Sgspt.color = -1;
	Sgavd.color = Sgbrap.color = Sgbret.color = -1;
	Sgsf.label[0] = Sgcv.label[0] = Sgerap.label[0] = Sgeret.label[0] = Sgspt.label[0] = '\0';
	Sgavd.label[0] = Sgbrap.label[0] = Sgbret.label[0] ='\0';

	S_init_geo(&Sgsf,Tsfnam,Tsfcol);
	S_init_geo(&Sgcv,Tcvnam,Tcvcol);
	if (Traptotyp>1)
		S_init_geo(&Sgerap,Traptoval,Terapcol);
	if (Tretrcttyp>1)
		S_init_geo(&Sgeret,Tretrctval,Teretcol);
	if (Tstarttyp==2)
		S_init_geo(&Sgspt,Tstartstr,Tstptcol);
	if (Traptotyp1>0)
		S_init_geo(&Sgbrap,Traptoval1,Tbrapcol);
	if (Tretrcttyp1>0)
		S_init_geo(&Sgbret,Tretrctval1,Tbretcol);
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
**    E_FUNCTION     : nclu_fmill()
**       Processes the NCL Flowline Milling form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_fmill()
{
	int flag, status;
	UU_LOGICAL cmdreject;
	UD_METHOD save_entry;
/*
.....Set up form fields
*/
	static UD_METHOD methods[] = {
/*
.....FMill
*/
		OnSelect,OnEditTxt, OnSelect,OnEditTxt,
/*
.....Motion Type
*/
		OnPickType,OnEditTxt,
		OnPickType,OnEditTxt, OnSelect, 
		OnPickType,
		OnPickType, OnEditTxt,
		OnPickType,
/*
.....boundaries
*/
		OnPickType, OnPickType, OnPickType,

		OnPickType, OnEditTxt, OnSelect, 
		OnPickType, OnEditTxt,
		OnPickType, OnEditTxt,OnSelect, 
		OnPickType, OnEditTxt,

		OnAvoidchk, OnCSThk1, OnSelect, OnDeselectAll,
		OnPickType, OnPickType, OnEditTxt,
/*
......Entry/Exit
*/
		OnPickType,OnEditTxt,OnSelect, OnPickType, OnEditTxt,
		OnPickType,OnEditTxt,OnSelect, OnPickType, OnEditTxt,
/*
.....Color
*/
		OnColor, OnColor, OnColor, OnColor, 
		OnColor, OnColor, OnColor, OnColor, 
		OnColor, OnColor, 
/*
......action buttons
*/
		OnAction,OnAction,OnAction,
		OnAction,OnAction,OnAction,OnAction,OnVideo
	};
	static char traverse[]= {
		1,1,1,1,
		1,1,1,1,1,1,1,1,1,
		1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1};
	static char called[] = {
		6,6,6,6,
		6,6,6,6,6,6,6,6,6,
		6,6,6,
		6,6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6};
	static char display[] = {
		1,1,1,1,
		1,1,1,1,1,1,1,1,1,
		1,1,1,
		1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,
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
	Sacc[FMill] = Sacc[MotionT] = Sacc[Entry] = Sacc[Boundary] = Sacc[Colors] = 0;
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0) 
		goto done;

	uu_list_init(&Scslst,sizeof(UM_sgeo),0,10);
	S_init_traverse(display,traverse);
/*
.....Initialize form fields
*/
repeat:;
	NCL_preview_mot = 1;
	UD_initfrm_intry = S_enter_form;

/*
.....Display the Form
*/
	status = ud_form1("nfmill.frm",Sanswers,Sanswers,methods,called,display,traverse);

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
	if (strlen(Tsfnam)<=0)
	{
		ud_wrerr("You must select a Surface to Machine.");
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
	int nc, status, number, nc1, nc2, nc3, nc4, nc5, nc6, nc7, nc8;
	char label[2*65], tmpstr[NCL_MAX_LABEL_AND_SUBSCRIPT];
	UU_REAL rval;
	UU_LOGICAL sfpk1,sfpk2, sfpk21, sfpk22, sfpk31, sfpk32, sfpk41, sfpk42;
	UU_LOGICAL bdsec1, bdsec2, bdsec3, ensec1, ensec2;
	UU_LOGICAL ifl;
/*
.....Surface parameter
*/
	nc = (int)strlen(Tsfnam);
	ul_strip_blanks(Tsfnam,&nc);
	if (nc>0)
		sfpk1 = UU_TRUE;
	else
		sfpk1 = UU_FALSE;
/*
.....Define button colors
*/
	if (sfpk1) 
		ud_dispfrm_set_attribs(0,MILLRG1,UM_BLACK,Tsfcol);
	else 
		ud_dispfrm_set_attribs(0,MILLRG1,UM_WHITE,UM_RED);
	
	ud_dispfrm_set_attribs(0,MILLRG3,UM_BLACK,Tcvcol);
	ud_dispfrm_set_attribs(0,ENEXTRG3,UM_BLACK,Terapcol);
	ud_dispfrm_set_attribs(0,ENEXTRG8,UM_BLACK,Teretcol);
	if (Tpasstyp==1)
	{
		number = atoi(Tpassstr);
		if (number>0)
			sfpk2 = UU_TRUE;
		else
			sfpk2 = UU_FALSE;
		if (sfpk2) 
			ud_dispfrm_set_attribs(0, OPTRG2, UM_BLACK, UM_WHITE);
		else 
		{
			ud_dispfrm_set_attribs(0, OPTRG2,UM_WHITE,UM_RED);
		}
	}
	else
	{
		ud_dispfrm_set_attribs(0, OPTRG2, UM_BLACK, UM_WHITE);
		sfpk2 = UU_TRUE;
	}
	ud_dispfrm_set_attribs(0,OPTRG5,UM_BLACK,Tstptcol);
	nc = (int)strlen(Tstartstr);
	ul_strip_blanks(Tstartstr,&nc);
	if (Tstarttyp==1)
	{
/*
.....check u,v value
*/
		if (nc<=0)
			status = -1;
		else
		{
			strcpy(tmpstr, Tstartstr);
			status = S_parse_uv_value(tmpstr);
		}
		if (status==0)
		{
			ud_dispfrm_set_attribs(0, OPTRG4, UM_BLACK, UM_WHITE);
			ud_dispfrm_set_attribs(0, OPTRG5, UM_BLACK, Tstptcol);
			sfpk22 = UU_TRUE;
		}
		else
		{
			ud_dispfrm_set_attribs(0, OPTRG4, UM_WHITE, UM_RED);
			sfpk22 = UU_FALSE;
		}
	}
	else if (Tstarttyp==2)
	{
		if (nc>0)
		{
			ud_dispfrm_set_attribs(0, OPTRG4, UM_BLACK, UM_WHITE);
			ud_dispfrm_set_attribs(0, OPTRG5, UM_BLACK, Tstptcol);
			sfpk22 = UU_TRUE;
		}
		else
		{
			ud_dispfrm_set_attribs(0, OPTRG4, UM_BLACK, UM_WHITE);
			ud_dispfrm_set_attribs(0, OPTRG5, UM_WHITE,UM_RED);
			sfpk22 = UU_FALSE;
		}
	}
	else
	{
		ud_dispfrm_set_attribs(0, OPTRG4, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(0, OPTRG5, UM_BLACK, Tstptcol);
		sfpk22 = UU_TRUE;
	}

	if (Tptcalc>0)
	{
		rval = atof(Tsteps_str);
		if (rval>0)
			sfpk21= UU_TRUE;
		else
			sfpk21 = UU_FALSE;
		if (sfpk21) 
			ud_dispfrm_set_attribs(0, OPTRG8, UM_BLACK, UM_WHITE);
		else 
		{
			ud_dispfrm_set_attribs(0, OPTRG8,UM_WHITE,UM_RED);
		}
	}
	else
	{
		ud_dispfrm_set_attribs(0, OPTRG8, UM_BLACK, UM_WHITE);
		sfpk21= UU_TRUE;
	}
	ud_dispfrm_set_attribs(0,BOUNDRG4,UM_BLACK,Tcsclr);
/*
.....boundaries
*/
	nc = (int)strlen(Traptoval1);
	ul_strip_blanks(Traptoval1,&nc);
	if (Traptotyp1>0)
	{
		if (nc>0)
			ud_dispfrm_set_attribs(0,RETRG3,UM_BLACK,Tbrapcol);
		else
			ud_dispfrm_set_attribs(0,RETRG3,UM_WHITE,UM_RED);
		ud_dispfrm_set_attribs(0,RETRG2,UM_BLACK,UM_WHITE);
	}
	else
	{
		ud_dispfrm_set_attribs(0,RETRG3,UM_BLACK,Tbrapcol);
		if (nc<0)
			ud_dispfrm_set_attribs(0,RETRG2, UM_WHITE, UM_RED);
		else
			ud_dispfrm_set_attribs(0,RETRG2,UM_BLACK,UM_WHITE);
	}
	if (nc>0)
		sfpk31 = UU_TRUE;
	else
		sfpk31 = UU_FALSE;
	nc = (int)strlen(Tretrctval1);
	ul_strip_blanks(Tretrctval1,&nc);
	if (Tretrcttyp1>0)
	{
		if (nc>0)
			ud_dispfrm_set_attribs(0,RETRG8,UM_BLACK,Tbretcol);
		else
			ud_dispfrm_set_attribs(0,RETRG8,UM_WHITE,UM_RED);
		ud_dispfrm_set_attribs(0,RETRG7,UM_BLACK,UM_WHITE);
	}
	else
	{
		ud_dispfrm_set_attribs(0,RETRG8,UM_BLACK,Tbretcol);
		if (nc<0)
			ud_dispfrm_set_attribs(0,RETRG7, UM_WHITE, UM_RED);
		else
			ud_dispfrm_set_attribs(0,RETRG7,UM_BLACK,UM_WHITE);
	}
	if (nc>0)
		sfpk32 = UU_TRUE;
	else
		sfpk32 = UU_FALSE;
	bdsec1 = bdsec2 = bdsec3 = UU_TRUE;
	if ((Tiotype==2)||(Tiotype==4))
	{
		if (Traptofed1==2)
		{
			nc = (int)strlen(Traptofrv1_str);
			ul_strip_blanks(Traptofrv1_str,&nc);
			if (nc>0)
			{
				rval = atof(Traptofrv1_str);
				if (rval>0)
					bdsec1= UU_TRUE;
				else
					bdsec1 = UU_FALSE;
			}
		}
		else
			bdsec1 = UU_TRUE;
		if (bdsec1==UU_TRUE)
			ud_dispfrm_set_attribs(0,RETRG5,UM_BLACK,UM_WHITE);
		else
			ud_dispfrm_set_attribs(0,RETRG5,UM_WHITE, UM_RED);
		if (Tretrctfed1==2)
		{
			nc = (int)strlen(Tretrctfrv1_str);
			ul_strip_blanks(Tretrctfrv1_str,&nc);
			if (nc>0)
			{
				rval = atof(Tretrctfrv1_str);
				if (rval>0)
					bdsec2= UU_TRUE;
				else
					bdsec2 = UU_FALSE;
			}
		}
		else
			bdsec2 = UU_TRUE;
		if (bdsec2==UU_TRUE)
			ud_dispfrm_set_attribs(0,RETRG10,UM_BLACK,UM_WHITE);
		else
			ud_dispfrm_set_attribs(0,RETRG10,UM_WHITE, UM_RED);
	}
	else
	{
		bdsec1 = UU_TRUE;
		bdsec2 = UU_TRUE;
		ud_dispfrm_set_attribs(0,RETRG5,UM_BLACK,UM_WHITE);
		ud_dispfrm_set_attribs(0,RETRG10,UM_BLACK,UM_WHITE);
	}
	if (Tfinpass)
	{
		if (Tfinfed1==1)
		{
			nc = (int)strlen(Tfinfrv1_str);
			ul_strip_blanks(Tfinfrv1_str,&nc);
			if (nc>0)
			{
				rval = atof(Tfinfrv1_str);
				if (rval>0)
					bdsec3= UU_TRUE;
				else
					bdsec3 = UU_FALSE;
			}
		}
		else
			bdsec3 = UU_TRUE;
		if (bdsec3==UU_TRUE)
			ud_dispfrm_set_attribs(0,BOUNDRG8,UM_BLACK,UM_WHITE);
		else
			ud_dispfrm_set_attribs(0,BOUNDRG8,UM_WHITE, UM_RED);
	}
	else
	{
		bdsec3 = UU_TRUE;
		ud_dispfrm_set_attribs(0,BOUNDRG8,UM_BLACK,UM_WHITE);
	}
/*
.....Entry/exit
*/
	nc = (int)strlen(Traptoval);
	ul_strip_blanks(Traptoval,&nc);
	if (Traptotyp>1)
	{
		if (nc>0)
			ud_dispfrm_set_attribs(0,ENEXTRG3,UM_BLACK,Terapcol);
		else
			ud_dispfrm_set_attribs(0,ENEXTRG3,UM_WHITE,UM_RED);
		ud_dispfrm_set_attribs(0,ENEXTRG2,UM_BLACK,UM_WHITE);
	}
	else if (Traptotyp==1)
	{
		ud_dispfrm_set_attribs(0,ENEXTRG3,UM_BLACK,Terapcol);
		if (nc<0)
			ud_dispfrm_set_attribs(0,ENEXTRG2, UM_WHITE, UM_RED);
		else
			ud_dispfrm_set_attribs(0,ENEXTRG2,UM_BLACK,UM_WHITE);
	}
	else
	{
/*
.....None
*/
		ud_dispfrm_set_attribs(0,ENEXTRG3,UM_BLACK,Tbretcol);
		ud_dispfrm_set_attribs(0,ENEXTRG2,UM_BLACK,UM_WHITE);
		nc = 1;
	}
	if (nc>0)
		sfpk41 = UU_TRUE;
	else
		sfpk41 = UU_FALSE;
	nc = (int)strlen(Tretrctval);
	ul_strip_blanks(Tretrctval,&nc);
	if (Tretrcttyp>1)
	{
		if (nc>0)
			ud_dispfrm_set_attribs(0,ENEXTRG8,UM_BLACK,Tbretcol);
		else
			ud_dispfrm_set_attribs(0,ENEXTRG8,UM_WHITE,UM_RED);
		ud_dispfrm_set_attribs(0,ENEXTRG7,UM_BLACK,UM_WHITE);
	}
	else if (Tretrcttyp==1)
	{
		ud_dispfrm_set_attribs(0,ENEXTRG8,UM_BLACK,Tbretcol);
		if (nc<0)
			ud_dispfrm_set_attribs(0,ENEXTRG7, UM_WHITE, UM_RED);
		else
			ud_dispfrm_set_attribs(0,ENEXTRG7,UM_BLACK,UM_WHITE);
	}
	else
	{
/*
.....None
*/
		ud_dispfrm_set_attribs(0,ENEXTRG8,UM_BLACK,Tbretcol);
		ud_dispfrm_set_attribs(0,ENEXTRG7,UM_BLACK,UM_WHITE);
		nc = 1;
	}
	if (nc>0)
		sfpk42 = UU_TRUE;
	else
		sfpk42 = UU_FALSE;

	ensec1 = ensec2 = UU_TRUE;
	if (Traptotyp!=0)
	{
		if (Traptofed==2)
		{
			nc = (int)strlen(Traptofrv_str);
			ul_strip_blanks(Traptofrv_str,&nc);
			if (nc>0)
			{
				rval = atof(Traptofrv_str);
				if (rval>0)
					ensec1= UU_TRUE;
				else
					ensec1 = UU_FALSE;
			}
		}
		else
			ensec1 = UU_TRUE;
	}
	else
		ensec1 = UU_TRUE;
	if (ensec1==UU_TRUE)
		ud_dispfrm_set_attribs(0,ENEXTRG5,UM_BLACK,UM_WHITE);
	else
		ud_dispfrm_set_attribs(0,ENEXTRG5,UM_WHITE, UM_RED);
	if (Tretrcttyp!=0)
	{
		if (Tretrctfed==2)
		{
			nc = (int)strlen(Tretrctfrv_str);
			ul_strip_blanks(Tretrctfrv_str,&nc);
			if (nc>0)
			{
				rval = atof(Tretrctfrv_str);
				if (rval>0)
					ensec2= UU_TRUE;
				else
					ensec2 = UU_FALSE;
			}
		}
		else
			ensec2 = UU_TRUE;
	}
	else
		ensec2 = UU_TRUE;
	if (ensec2==UU_TRUE)
		ud_dispfrm_set_attribs(0,ENEXTRG10,UM_BLACK,UM_WHITE);
	else
		ud_dispfrm_set_attribs(0,ENEXTRG10,UM_WHITE, UM_RED);
/*
.....Set section color
*/
	if (sfpk1)
	{
		if (Sacc[FMill]==0)
		{
			Ssecol[FMill] = Sblack;
			S_section_changed(FMill,UU_FALSE);
		}
		else
		{	
			Ssecol[FMill] = Sgreen; 
			S_section_changed(FMill,UU_TRUE);
		}
	}
	else
	{
		Ssecol[FMill] = Sred; 
		S_section_changed(FMill,UU_FALSE);
	}
	if ((sfpk2)&&(sfpk21)&&(sfpk22))
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

	if ((sfpk31)&&(sfpk32)&&bdsec1&&bdsec2&&bdsec3)
	{
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
	}
	else
	{
		Ssecol[Boundary] = Sred; 
		S_section_changed(Boundary,UU_FALSE);
	}
	if ((sfpk41)&&(sfpk42))
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
	ifl = sfpk1 && sfpk2 && sfpk21 && sfpk22 ;
	ifl = ifl && sfpk31&&sfpk32&&bdsec1&&bdsec2&&bdsec3&&ensec1&&ensec2;
	ud_frm_enable_ok(ifl);
	ud_set_traverse_mask(FAPV,ifl);
	ud_set_traverse_mask(FAPY,ifl);

	ifl = Sacc[FMill] + Sacc[MotionT] + Sacc[Entry] + Sacc[Boundary] + Sacc[Colors];
	ud_set_traverse_mask(FARS,ifl);
/*
.....if geom entered
*/
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

	return UU_TRUE;
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
	n = UU_LIST_LENGTH(&Scslst);
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(&Scslst);
	for (i=0;i<n;i++) S_add_key(which,mgeo[i].key);
	S_add_key(which, Sgsf.key);
	S_add_key(which, Sgcv.key);
	S_add_key(which, Sgerap.key);
	S_add_key(which, Sgeret.key);
	S_add_key(which, Sgspt.key);
	S_add_key(which, Sgbrap.key);
	S_add_key(which, Sgbret.key);
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
