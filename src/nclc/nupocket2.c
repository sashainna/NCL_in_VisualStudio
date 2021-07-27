/*********************************************************************
**    NAME         :  nupocket2.c
**       CONTAINS:
**		void nclu_waterline_cut()	Waterline Roughing interface
**		void S_build_command
**
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nupocket2.c , 25.6
**     DATE AND TIME OF LAST MODIFICATION
**       01/20/17 , 11:07:56
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
#include "nclupok.h"
#include "ncluvmp.h"

#define PIC_INDEX1 0
#define PIC_INDEX2	1
#define PIC_INDEX3	2
#define PIC_INDEX4	3
#define PIC_INDEX5	4
#define PIC_INDEX6	5
#define PIC_INDEX7	6
#define PIC_INDEX8	7
#define PIC_INDEX9	8
#define PIC_INDEX10	9
#define PIC_INDEX11	10
#define PIC_INDEX12	11
#define PIC_INDEX13	12
#define PIC_INDEX14	13
#define PIC_INDEX15	14
#define PIC_INDEX16	15

#define PIC_SINDEX1 17
#define PIC_SINDEX2	18
#define PIC_SINDEX3	19
#define PIC_SINDEX4	20

#define PIC_LINDEX1 21
#define PIC_LINDEX2	22
#define PIC_LINDEX3	23
#define PIC_LINDEX4	24

#define PIC_OINDEX1 25
#define PIC_OINDEX2	26
#define PIC_OINDEX3	27
#define PIC_OINDEX4	28

#define CONTOUR 0
#define BOX 1

#define STOCK 0
#define PLANE 1
#define DIST 2
#define DEPTH 2
#define ZLEV 3
#define DIST1 1
#define ZLEV1 2

#define RANDOM 0
#define NEGX 1
#define POSX 2
#define NEGY 3
#define POSY 4
#define NEGZ 5
#define POSZ 6
#define NEARPT 7
#define INSIDE 8
#define OUTSID 9
#define SMALST 10
#define LARGST 11

#define ISECT 0
#define CONNECT 1
#define POCKET 2
/* one label in the form */
#define LABELS	1
/*
.....Pocket Motion
*/
#define POCKETRG1		0
#define POCKETRG2		1
#define POCKETRG3		2
#define POCKETRG4		3
#define POCKETRG5		4
#define POCKETRG6		5
/*
.....Geometry
*/
#define GEOMRG1		6
#define GEOMRG2		7
#define GEOMRG3		8
#define GEOMRG4		9
#define GEOMRG5		10
#define GEOMRG6		11
/*
.....STOCK
*/
#define STOCKRG1		12
#define STOCKRG2		13
#define STOCKRG3		14
#define STOCKRG4		15
#define STOCKRG5		16
#define STOCKRG6		17
#define STOCKRG7		18
#define STOCKRG8		19
#define STOCKRG9		20
#define STOCKRG10		21
#define STOCKRG11		22
#define STOCKRG12		23
#define STOCKRG13		24
#define STOCKRG14		25
#define STOCKRG15		26
#define STOCKRG16		27
#define STOCKRG17		28
#define STOCKRG18		29
/*
.....Level
*/
#define LEVELRG1		30  
#define LEVELRG2		31
#define LEVELRG3		32
#define LEVELRG4		33
#define LEVELRG5		34
#define LEVELRG6		35
#define LEVELRG7		36
#define LEVELRG8		37
#define LEVELRG9		38
#define LEVELRG10		39
#define LEVELRG11		40
#define LEVELRG12		41
#define LEVELRG13		42
#define LEVELRG14		43
#define LEVELRG15		44
/*
.....Options
*/
#define OPTRG1		45
#define OPTRG2		46
#define OPTRG3		47
#define OPTRG4		48
#define OPTRG5		49
#define OPTRG6		50
#define OPTRG7		51
#define OPTRG8		52
#define OPTRG9		53
#define OPTRG10		54
#define OPTRG11		55
/*
.....Zones
*/
#define ZONERG1		56
#define ZONERG2		57
#define ZONERG3		58
#define ZONERG4		59
#define ZONERG5		60
#define ZONERG6		61
#define ZONERG7		62
#define ZONERG8		63
/*
.....Colors
*/
#define CLRRG1		64
#define CLRRG2		65
#define CLRRG3		66
#define CLRRG4		67
#define CLRRG5		68
#define CLRRG6		69
#define CLRRG7		70
#define CLRRG8		71
#define CLRRG9		72
#define CLRRG10		73
#define CLRRG11		74
/*
......Pocket Modals
*/
#define POCKETMOD	75
/*
.....Action Buttons
*/
#define FAPV	76
#define FAPY	77
#define FARS	78
#define FAPB	79
#define FAVE	80
#define FAGE	81
#define FAVW	82

#define MXLAB NCL_MAX_LABEL+1
#define HIDE_KEY 0
#define CONTOUR_KEY 1

/*
.....Section definitions
*/
enum {PocketM, Geometry, Stock, Levels, Options, Zones, Colors};
static char *Smode[]={"Pocket Motion", "Geometry", "Stock", "Levels", 
			"Options", "Zones", "Colors"};
static int Sred[3]={180,0,0}, Syellow[3]={180,148,0}, Sgreen[3]={0,180,0};
static int Sblack[3]={0,0,0};
static int *Ssecol[]={Sblack,Sblack,Sblack,Sblack,Sblack,Sblack,Sblack,Sblack};
static int Sacc[7];
static int S_act_pic=0;

static char Sav_poc_bot_str[MXLAB] = "";
static char Sav_poc_bot_val[MXLAB] = "0.0";
static char Sav_poc_top_str[MXLAB] = "";
static char Sav_poc_top_val[MXLAB] = "0.0";

extern int NAUTIPV;
extern UD_METHOD UD_initfrm_intry;

static UN_motseg *Smptr=0,Smotatt;
static UN_mot_vpbox_struc Smvpbox[UV_NVPORTS];

static UU_LIST Skey_list;
static int Snkeys;
static UU_LOGICAL Skey_init;
static UU_LOGICAL Sgeo_init[5];
static UU_LOGICAL Sgeo_redraw;
static UM_sgeo Sgrev, Sgsf, Sbsrf1, Sbsrf2, Sgbot, Sgtop, Sgstpt, Sghol, Sgzcv;

static UU_LIST *Slaylist = UU_NULL;
static UU_LIST Ssurf;
static int nsurf = 0;
static int Scolor,Slayerdef,Sstock,Signore,Sstocktype,Sbottype,Slayer;
static int Slistlayer,Stoptype,Smaxloop,Soffpart,Sconpoc,Sfrom,Szone,Sdeep;
static int Scolorb,Stoptype,Scolort,Scolore,Srevsf,Scolorv,Sbottyp1,Stoptyp1;
static int Sfinis,Smethod=0,Sclick_pokmod,Sstep, Spktype,Sclick_vmpmod;
static int Sstksf, Tstksf, Savdsf, Tavdsf;

static char Spoc_bot[MXLAB],Spoc_top[MXLAB],Spoc_end[MXLAB],Srev_surf[MXLAB],
	Sstat_answer[MXLAB];
static char Slay_num[MXLAB],Smax_gap[MXLAB],Sstock_off[MXLAB],Sbot_off[MXLAB],
	Stop_off[MXLAB],Smax_loop[MXLAB],Spart_off[MXLAB];
static char Sadj_up[MXLAB],Sadj_down[MXLAB],Sinc_hgt[MXLAB];
static char Scmd_str[20][MXLAB];

static int Tcolor,Tlayerdef,Tstock,Tignore,Tstocktype,Tbottype,Tlayer;
static int Tlistlayer,Ttoptype,Tmaxloop,Toffpart,Tconpoc,Tfrom,Tzone,Tdeep;
static int Tcolorb,Ttoptype,Tcolort,Tcolore,Trevsf,Tcolorv,Tbottyp1,Ttoptyp1;
static int Tfinis,Tmethod=0,Tstep, Tpktype;

static char Tpoc_bot[MXLAB],Tpoc_top[MXLAB],Tpoc_end[MXLAB],Trev_surf[MXLAB],
	Tstat_answer[MXLAB];
static char Tlay_num[MXLAB], Tmax_gap[MXLAB], Tstock_off[MXLAB], Tbot_off[MXLAB],
	Ttop_off[MXLAB], Tmax_loop[MXLAB], Tpart_off[MXLAB];
static char Tadj_up[MXLAB], Tadj_down[MXLAB], Tinc_hgt[MXLAB];

static int Szcolor, Szlast, Sztype;
static int Tzcolor, Tzlast, Tztype;
static int Tgeotog,Tgeocol;
static int Sgeotog,Sgeocol;
static int TBcolor[2], SBcolor[2];
static int TBaddlayer[2], TBlayincl[2];
static int SBaddlayer[2], SBlayincl[2];
static int Tcolorh,Scolorh;
static char TBlay_num[2][MXLAB];
static char SBlay_num[2][MXLAB];

static UU_LIST *Blaylist[2] = {UU_NULL,UU_NULL};
static UU_LIST Bsurf[2];
static int Bsfnum[2] = {0,0};
static int Blayer[2], Blistlayer[2];
static UU_LOGICAL Shole_init=UU_FALSE;
static UU_LIST Sholes;
static int Snholes=0;

static void S_init_form(),S_save_form(),S_hilite_entity(),S_unhilite_entity();
static UD_FSTAT S_enter_form();
static void S_init_traverse(), S_form_invis(),S_form_vis(),S_init_geo();
static void S_hilite_entities(),S_unhilite_all(),S_unhilite_entities();
static void S_update_answers(),S_section_changed(), S_deselect_all();

static void S_add_key(),S_create_key_list(),S_draw_indir(),S_push_geo();
static void S_get_modals(),S_create_table();
static void S_update_table(),S_format_table(),S_look_avoid();
static int S_select_geo(), S_build_command();

static UD_FSTAT OnAction(),OnVideo();

static int Spic_ans;
static UD_FSTAT OnPicTog(),OnClosePic();

static int SfrmPlay=-1;
static int Sfrmpic = -1;
static UU_LOGICAL Swatfrm_init = UU_FALSE;
static UU_LOGICAL Spokmod_saved = UU_FALSE;

static NCLU_pokmod Smodals;
static NCLU_vmpmod Svmodals;

static UU_LOGICAL Bactive = UU_FALSE;
static UU_LOGICAL Zactive = UU_FALSE;

#define LIST_A 0
#define LIST_B 1
#define LIST_C 2

extern int NVMILL;

#define ALL 0
#define PART 1
#define MAIN 2

static UD_LIST stat_list;
static UU_LIST strings;

static int nzones = 0;
static int zcurnam = -1;
static UU_LIST Zcurves;

/*..... Selection of Surfaces ...., */
#define BSLS 0
#define BCOL 1
#define BIOL 2  /* Include on Layer */
#define BDES 3
/*..... Layer ..... */
#define BLSL 4
#define BLAY 5
#define BLAD 6
#define BLSH 7

UD_FSTAT ncl_show_layer();

static int Serrflg,Spreviewflg,Schgmade,Sstock_ch[2],Szone_ch[2],Stog_ch[11],
				Smenu_ch[10];
static char Sstock_str[2][MXLAB];
static int *Sanswers[] = {
/*
.....Pocket Motion
*/
		&Tpktype, &Tmethod, &Trevsf, UU_NULL,(int *)Trev_surf, UU_NULL,
/*
.....Geometry
*/
		UU_NULL, (int *)&Tlay_num, UU_NULL, &Tlayerdef,UU_NULL, UU_NULL,
/*
.....Stock
*/
		&Tstock, &Tignore, &Tstocktype, (int *)Tstock_off, 
		&Tstksf, UU_NULL, &(TBlayincl[0]), UU_NULL, (int *)TBlay_num[0],
		UU_NULL, UU_NULL,
		&Tavdsf, UU_NULL, &(TBlayincl[1]), UU_NULL, (int *)TBlay_num[1],
		UU_NULL, UU_NULL,
/*
......Level
*/
		&Tbottype, &Tbottyp1, (int *)Tpoc_bot, UU_NULL,
		(int *)Tbot_off,
		&Ttoptype, &Ttoptyp1, (int *)Tpoc_top, UU_NULL,
		&Tstep, (int *)Tinc_hgt, (int *)Ttop_off,
		&Tfinis, (int *)Tadj_up, (int *)Tadj_down,
/*
.....Options
*/
		&Tdeep, &Tfrom, (int *)Tpoc_end, UU_NULL, 
		(int *)Tmax_gap, &Tmaxloop, (int *)Tmax_loop,
		&Toffpart, (int *)Tpart_off, UU_NULL, UU_NULL,
/*
.....Zones
*/
		UU_NULL, UU_NULL, (int *)&stat_list, UU_NULL, UU_NULL, UU_NULL,
		&Tztype, &Tzlast, 
/*
.....colors
*/
		&Tcolor, &Tcolorv, &TBcolor[0], &TBcolor[1],
		&Tcolorb, &Tcolort,
		&Tcolore, &Tcolorh, &Tzcolor, 
		&Tgeotog, &Tgeocol,
		UU_NULL, 
		UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL
	};

/*********************************************************************
**    I_FUNCTION     : S_empty_repaint (listptr,nlist)
*********************************************************************/
static void S_empty_repaint (listptr,nlist)
UU_LIST *listptr;
int *nlist;
{
	nclu_list_repaint (listptr,-1);
	UU_LIST_EMPTY (listptr);
	*nlist = 0;
}

/*********************************************************************
**    I_FUNCTION     : S_free_repaint (listptr)
*********************************************************************/
static void S_free_repaint (listptr)
UU_LIST *listptr;
{
	nclu_list_repaint (listptr,-1);
	uu_list_free (listptr);
}

/*********************************************************************
**    I_FUNCTION     : S_close_sflist(ilst)
*********************************************************************/
static void S_close_sflist(ilst)
int ilst;
{
	UU_LIST *listptr,**listptr1;

	if (ilst == LIST_A)
	{
		listptr = &Ssurf;
		listptr1 = &Slaylist;
		nsurf = 0;
		Slistlayer = -1;
	}
	else if (ilst == LIST_B)
	{
		listptr = &Bsurf[0];
		listptr1 = &Blaylist[0];
		Bsfnum[0] = 0;
		Blistlayer[0] = -1;
	}
	else if (ilst == LIST_C)
	{
		listptr = &Bsurf[1];
		listptr1 = &Blaylist[1];
		Bsfnum[1] = 0;
		Blistlayer[1] = -1;
	}
	else
		return;

	S_free_repaint (listptr);
	if (*listptr1 != NULLST)
	{
		S_free_repaint (*listptr1);
		UU_FREE (*listptr1);
		*listptr1 = UU_NULL;
	}
}

/*********************************************************************
*********************************************************************/
static void Z_init_flist(list,items,n1,n2)
UD_LIST *list;
UU_LIST *items;
int n1,n2;
{
	uu_list_init (items,sizeof(char *),n1,n2);
	list->item = (char **) UU_LIST_ARRAY(items);
	list->num_item = 0;
}

/*********************************************************************
*********************************************************************/
static void Z_free_items(list)
UD_LIST *list;
{
	int i;

	for (i=0; i<list->num_item; i++)
	{
		uu_free(list->item[i]);
	}
	list->num_item = 0;
	list->item = UU_NULL;
}

/*********************************************************************
*********************************************************************/
static void Z_free_flist(list,items)
UD_LIST *list;
UU_LIST *items;
{
	Z_free_items (list);
	uu_list_free (items);
	list->answer = NULL;
}

/*********************************************************************
*********************************************************************/
static void Z_empty_flist(list,items)
UD_LIST *list;
UU_LIST *items;
{
	Z_free_items (list);
	UU_LIST_EMPTY (items);
}

/*********************************************************************
*********************************************************************/
static void Z_put_list(list,items,sbuf)
UD_LIST *list;
UU_LIST *items;
char *sbuf;
{
	char *buf;

	buf = (char *) uu_malloc((strlen(sbuf)+1)*sizeof(char));
	strcpy (buf,sbuf);
	uu_list_push (items,&buf);

	list->item = (char **) UU_LIST_ARRAY(items);
	list->num_item = items->cur_cnt;
	strcpy(list->answer,sbuf);
}
/*********************************************************************
**    I_FUNCTION     : S_build_command(flag)
**			Execute the Waterline Roughing. (Build and output command)
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
	NCL_cmdbuf cmdbuf;
	UU_LOGICAL cmdreject;
	int lbtyp,lttyp;
	int i;
	UM_sgeo *geo;
	char NCL_adjust[] = "ADJUST";

/*
.....Output POKMOD or VMPMOD command
*/
	if (Sclick_pokmod == 1)
	{
		if (Tmethod == 0) 
		{
			nclu_pokmod_cmd1(&Smodals,flag);
		}
	}
	if (Sclick_vmpmod == 1)
	{
		if (Tmethod != 0) 
		{
			nclu_vmpmod_cmd(&Svmodals,flag);
		}
	}
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) 
	{
		Spreviewflg = 1;
		goto fini;
	}
/*
.....Output layer creation command(s)
*/
	if (Tlayerdef && nsurf > 0) nclu_laycmd(&Ssurf,Tlay_num,flag);
	for (i = 0; i < 2; i++)
	{
		if (TBlayincl[i] && Bsfnum[i] > 0)
			nclu_laycmd(&Bsurf[i],TBlay_num[i],flag);
	}

	ncl_init_cmdbuf(&cmdbuf);

	if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
	ncl_add_token(&cmdbuf, NCL_pocket, NCL_nocomma);
	ncl_add_token(&cmdbuf, NCL_layer, NCL_nocomma);
	ncl_add_token(&cmdbuf, Tlay_num, NCL_comma);
	if (Tmethod == 1)
		ncl_add_token(&cmdbuf, "VMPOCK", NCL_comma);
	else if (Tmethod == 2)
		ncl_add_token(&cmdbuf, "VMP3AX", NCL_comma);
	else if (Tmethod == 3)
		ncl_add_token(&cmdbuf, "VMP5AX", NCL_comma);

	if (Trevsf == 1)
	{
		ncl_add_token(&cmdbuf, NCL_revolv, NCL_comma);
		ncl_add_token(&cmdbuf,Trev_surf,NCL_comma);

		lbtyp = STOCK;
		if (Tbottyp1 == DIST1)
			lbtyp = DIST;
		else if (Tbottyp1 == ZLEV1)
			lbtyp = ZLEV;

		lttyp = DIST;
		if (Ttoptyp1 == STOCK)
			lttyp = STOCK;
		else if (Ttoptyp1 == ZLEV1)
			lttyp = ZLEV;
	}
	else
	{
		lbtyp = Tbottype;
		lttyp = Ttoptype;
	}

	if (lbtyp == STOCK)
	{
		ncl_add_token(&cmdbuf,NCL_stock,NCL_comma);
		ncl_add_token(&cmdbuf,Tbot_off,NCL_comma);
	}
	else if (lbtyp == PLANE)
	{
		ncl_add_token(&cmdbuf,Tpoc_bot,NCL_comma);
		ncl_add_token(&cmdbuf,Tbot_off,NCL_comma);
	}
	else if (lbtyp == DEPTH)
	{
		ncl_add_token(&cmdbuf,NCL_depth,NCL_comma);
		ncl_add_token(&cmdbuf,Tpoc_bot,NCL_comma);
	}
	else
	{
		ncl_add_token(&cmdbuf,NCL_at,NCL_comma);
		ncl_add_token(&cmdbuf,Tpoc_bot,NCL_comma);
	}

	if (lttyp == STOCK)
	{
		ncl_add_token(&cmdbuf,NCL_stock,NCL_comma);
		ncl_add_token(&cmdbuf,Ttop_off,NCL_comma);
	}
	else if (lttyp == PLANE)
	{
		ncl_add_token(&cmdbuf,Tpoc_top,NCL_comma);
		ncl_add_token(&cmdbuf,Ttop_off,NCL_comma);
	}
	else if (lttyp == DIST)
	{
		ncl_add_token(&cmdbuf,NCL_offset,NCL_comma);
		ncl_add_token(&cmdbuf,Tpoc_top,NCL_comma);
	}
	else
	{
		ncl_add_token(&cmdbuf,NCL_at,NCL_comma);
		ncl_add_token(&cmdbuf,Tpoc_top,NCL_comma);
	}

	if (Tfinis)
	{
		ncl_add_token(&cmdbuf, NCL_finish, NCL_comma);
		ncl_add_token(&cmdbuf, NCL_level, NCL_comma);
		if (Tmethod < 2)
		{
			ncl_add_token(&cmdbuf, NCL_adjust, NCL_comma);
			ncl_add_token(&cmdbuf, NCL_up, NCL_comma);
			ncl_add_token(&cmdbuf, Tadj_up, NCL_comma);
			ncl_add_token(&cmdbuf, NCL_down, NCL_comma);
			ncl_add_token(&cmdbuf, Tadj_down, NCL_comma);
		}
	}

	if (Tstock)
	{
		if (Tstocktype == CONTOUR)
			ncl_add_token(&cmdbuf,NCL_part,NCL_comma);
		else
			ncl_add_token(&cmdbuf,NCL_fit,NCL_nocomma);
		ncl_add_token(&cmdbuf,Tstock_off,NCL_comma);
	}
	else
	{
		if (Tignore)
			ncl_add_token(&cmdbuf,NCL_in,NCL_comma);
		else if (TBaddlayer[0] || Bsfnum[0] > 0)
		{
			ncl_add_token(&cmdbuf, NCL_bound, NCL_comma);
			if (TBaddlayer[0])
			{
				ncl_add_token(&cmdbuf, NCL_layer, NCL_nocomma);
				ncl_add_token(&cmdbuf, TBlay_num[0], NCL_comma);
				ncl_add_token(&cmdbuf,Tstock_off,NCL_comma);
			}

			if (!TBlayincl[0] && Bsfnum[0] > 0)
			{
				geo = (UM_sgeo *) UU_LIST_ARRAY(&Bsurf[0]);

				for (i = 0; i < Bsfnum[0]; i++)
				{
					ncl_add_token(&cmdbuf, geo[i].label, NCL_comma);
				}									
				ncl_add_token(&cmdbuf,Tstock_off,NCL_comma);
			}
		}
		else
			ncl_add_token(&cmdbuf,NCL_omit,NCL_comma);
	}

	if (TBaddlayer[1] || Bsfnum[1] > 0)
	{
		ncl_add_token(&cmdbuf,NCL_avoid,NCL_comma);
		if (TBaddlayer[1])
		{
			ncl_add_token(&cmdbuf, NCL_layer, NCL_nocomma);
			ncl_add_token(&cmdbuf, TBlay_num[1], NCL_comma);
		}

		if (!TBlayincl[1] && Bsfnum[1] > 0)
		{
			geo = (UM_sgeo *) UU_LIST_ARRAY(&Bsurf[1]);

			for (i = 0; i < Bsfnum[1]; i++)
			{
				ncl_add_token(&cmdbuf, geo[i].label, NCL_comma);
			}
		}
	}

	ncl_add_token(&cmdbuf,NCL_error,NCL_comma);
	if (Tfrom > RANDOM && Tfrom <= LARGST)
	{
		ncl_add_token(&cmdbuf,Tmax_gap,NCL_comma);
		ncl_add_token(&cmdbuf,NCL_start,NCL_comma);
		if (Tfrom == NEGX)
			ncl_add_token(&cmdbuf,NCL_negx,NCL_comma);
		else if (Tfrom == POSX)
			ncl_add_token(&cmdbuf,NCL_posx,NCL_comma);
		else if (Tfrom == NEGY)
			ncl_add_token(&cmdbuf,NCL_negy,NCL_comma);
		else if (Tfrom == POSY)
			ncl_add_token(&cmdbuf,NCL_posy,NCL_comma);
		else if (Tfrom == NEGZ)
			ncl_add_token(&cmdbuf,NCL_negz,NCL_comma);
		else if (Tfrom == POSZ)
			ncl_add_token(&cmdbuf,NCL_posz,NCL_comma);
		else if (Tfrom == NEARPT)
		{
			ncl_add_token(&cmdbuf,NCL_nearpt,NCL_comma);
			ncl_add_token(&cmdbuf,Tpoc_end,NCL_comma);
		}
		else if (Tfrom == OUTSID)
			ncl_add_token(&cmdbuf,NCL_out,NCL_comma);
		else if (Tfrom == INSIDE)
			ncl_add_token(&cmdbuf,NCL_in,NCL_comma);
		else if (Tfrom == LARGST)
			ncl_add_token(&cmdbuf,NCL_large,NCL_comma);
		else if (Tfrom == SMALST)
			ncl_add_token(&cmdbuf,NCL_small,NCL_comma);
	}
	else
		ncl_add_token(&cmdbuf,Tmax_gap,NCL_comma);

	if (nzones > 0)
	{
		UM_sgeo *geo;
		int i;

		ncl_add_token(&cmdbuf, "ZONE", NCL_comma);

		geo = (UM_sgeo *) UU_LIST_ARRAY (&Zcurves);
		for (i = 0; i < nzones; i++)
			ncl_add_token(&cmdbuf, geo[i].label, NCL_comma);

		if (Tzlast == 1) ncl_add_token(&cmdbuf, "LAST", NCL_comma);

		if (Tztype == PART)
			ncl_add_token(&cmdbuf, "PART", NCL_comma);
		else if (Tztype == MAIN)
			ncl_add_token(&cmdbuf, "MAIN", NCL_comma);
		else
			ncl_add_token(&cmdbuf, "ALL", NCL_comma);
	}

	if (Tmaxloop)
	{
		ncl_add_token(&cmdbuf, Smax_loop, NCL_comma);
	}

	if (Tconpoc == CONNECT)
		ncl_add_token(&cmdbuf, "COMPOS", NCL_comma);
	else if (Tconpoc == ISECT)
		ncl_add_token(&cmdbuf, "CURVE", NCL_comma);

	if (Tdeep)
		ncl_add_token(&cmdbuf, "DEEP", NCL_comma);

	if (Toffpart)
	{
		ncl_add_token(&cmdbuf, "OFF,PART", NCL_comma);
		ncl_add_token(&cmdbuf,Tpart_off,NCL_comma);
	}
	if (Tstep)
	{
		ncl_add_token(&cmdbuf,"STEP",NCL_comma);
		ncl_add_token(&cmdbuf,NCL_up,NCL_comma);
		ncl_add_token(&cmdbuf,Tinc_hgt,NCL_comma);
	}
/*
.....Add POSITN holes
*/
	if (Snholes > 0 && Shole_init)
	{
		geo = (UM_sgeo *)UU_LIST_ARRAY(&Sholes);
		ncl_add_token(&cmdbuf,"POSITN",NCL_comma);
		for (i=0;i<Snholes;i++)
		{
			ncl_add_token(&cmdbuf,geo[i].label,NCL_comma);
		}
	}
/*
.....Insert the command at the current line, but do not execute
.......Only need to write the command if Preview was already pressed
.......and no changes were made since the command was already executed
*/
	ncl_set_cmdmode(UU_TRUE);
	ncl_add_cmdbuf(&cmdbuf);
	if (Spreviewflg && !Schgmade) 
	{
		if (Serrflg==0)
			ncl_call_input(&cmdbuf,UU_TRUE);
	}
/*
.....Execute command if a change has been made since the last preview or no
.....preview was created
*/
	else 
		Serrflg = ncl_call(&cmdbuf);

fini:
	UD_UNMARK(cmdreject);
	if (!flag && Slayerdef && nsurf > 0)
		nclu_restore_layers (&Ssurf);

	return (UU_SUCCESS);
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
	int nc, status, nc1, nc2, nc3, nc4;
	char label[2*65];
	UU_REAL rval;
	UU_LOGICAL sfpk1,sfpk2,sfpk31,sfpk32,sfpk33,
		sfpk41,sfpk42,sfpk43,sfpk44,sfpk45,sfpk46,
		sfpk51, sfpk52, sfpk53, sfpk54, sfpk6,sfpk7;
	UU_LOGICAL ifl;
/*
.....Pocket Motion section
*/
	sfpk1 = UU_TRUE;
	if (Tmethod==0)
	{
		if (Trevsf==1)
		{
			nc = strlen(Trev_surf);
			ul_strip_blanks(Trev_surf,&nc);
			if (nc>0)
				sfpk1 = UU_TRUE;
			else
				sfpk1 = UU_FALSE;
		}
	}
/*
.....Define button colors
*/
	if (sfpk1) 
	{
		ud_dispfrm_set_attribs(0, POCKETRG4, UM_BLACK, Tcolorv);
	}
	else 
	{
		ud_dispfrm_set_attribs(0, POCKETRG4, UM_WHITE,UM_RED);
	}
/*
.....Geometry section
.....Optional, so always not read
*/
	sfpk2 = UU_TRUE;
	if (sfpk2) 
		ud_dispfrm_set_attribs(0, GEOMRG5, UM_BLACK, Tcolor);
	else 
		ud_dispfrm_set_attribs(0, GEOMRG5, UM_WHITE,UM_RED);
	nsurf = UU_LIST_LENGTH(&Ssurf);
	if (nsurf>0)
	{
		ud_set_traverse_mask(GEOMRG6,UU_TRUE);
	}
	else
		ud_set_traverse_mask(GEOMRG6,UU_FALSE);
/*
.....Stock section
*/
	sfpk31 = UU_TRUE;
	nc = (int)strlen(Tstock_off);
	ul_strip_blanks(Tstock_off, &nc);
	if (nc>0)
		sfpk31 = UU_TRUE;
	else
		sfpk31 = UU_FALSE;
	if (sfpk31) 
		ud_dispfrm_set_attribs(0, STOCKRG4, UM_BLACK, UM_WHITE);
	else 
		ud_dispfrm_set_attribs(0, STOCKRG4, UM_WHITE, UM_RED);

	sfpk32 = UU_TRUE;
	if ((Tstksf)&&(TBlayincl[0]))
	{
		nc = (int)strlen(TBlay_num[0]);
		ul_strip_blanks(TBlay_num[0], &nc);
		if (nc>0)
		{
			sfpk32 = UU_TRUE;
			ud_set_traverse_mask(STOCKRG10, UU_TRUE);
		}
		else
		{
			sfpk32 = UU_FALSE;
			ud_set_traverse_mask(STOCKRG10,UU_FALSE);
		}
	}
	else
	{
		ud_set_traverse_mask(STOCKRG10,UU_FALSE);
	}
	if (sfpk32)
	{
		ud_dispfrm_set_attribs(0, STOCKRG9, UM_BLACK, UM_WHITE);
	}
	else 
	{
		ud_dispfrm_set_attribs(0, STOCKRG9, UM_WHITE, UM_RED);
	}
	Bsfnum[0] = Bsurf[0].cur_cnt;
	if (Bsfnum[0]>0)
	{
		ud_set_traverse_mask(STOCKRG11,UU_TRUE);
	}
	else
		ud_set_traverse_mask(STOCKRG11,UU_FALSE);
	sfpk33 = UU_TRUE;
	if ((Tavdsf)&&(TBlayincl[1]))
	{
		nc = (int)strlen(TBlay_num[1]);
		ul_strip_blanks(TBlay_num[1], &nc);
		if (nc>0)
		{
			ud_set_traverse_mask(STOCKRG17,UU_TRUE);
			sfpk33 = UU_TRUE;
		}
		else
		{
			sfpk33 = UU_FALSE;
			ud_set_traverse_mask(STOCKRG17,UU_FALSE);
		}
	}
	else
	{
		ud_set_traverse_mask(STOCKRG17,UU_FALSE);
	}
	if (sfpk33)
	{
		ud_dispfrm_set_attribs(0, STOCKRG16, UM_BLACK, UM_WHITE);
	}
	else 
	{
		ud_dispfrm_set_attribs(0, STOCKRG16, UM_WHITE, UM_RED);
	}
	Bsfnum[1] = Bsurf[1].cur_cnt;
	if (Bsfnum[1]>0)
	{
		ud_set_traverse_mask(STOCKRG18,UU_TRUE);
	}
	else
		ud_set_traverse_mask(STOCKRG18,UU_FALSE);
/*
......Level Session
*/
	sfpk41 = UU_TRUE;
	if ((Trevsf==0)&&(Tbottype == STOCK)
		|| (Trevsf==1)&&(Tbottyp1 == STOCK))
	{
		sfpk41 = UU_TRUE;
		ud_dispfrm_set_attribs(0, LEVELRG3, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(0, LEVELRG4, UM_BLACK, -1);
	}
	else
	{
		nc = (int)strlen(Tpoc_bot);
		ul_strip_blanks(Tpoc_bot, &nc);
		if (nc>0)
		{
			ud_dispfrm_set_attribs(0, LEVELRG3, UM_BLACK, UM_WHITE);
			sfpk41 = UU_TRUE;
			ud_dispfrm_set_attribs(0, LEVELRG4, UM_BLACK, Tcolorb);
		}
		else
		{
			sfpk41 = UU_FALSE;
			ud_dispfrm_set_attribs(0, LEVELRG3, UM_WHITE, UM_RED);
			if ((Trevsf==0)&&(Tbottype==PLANE))
				ud_dispfrm_set_attribs(0, LEVELRG4, UM_BLACK, UM_RED);
			else
				ud_dispfrm_set_attribs(0, LEVELRG4, UM_BLACK, Tcolorb);
		}
/*
.....if disabled, display default color
*/
		if ((Trevsf==0)&&(Tbottype != PLANE)
			|| (Trevsf==1))
		{
			ud_dispfrm_set_attribs(0, LEVELRG4, UM_BLACK, -1);
			ud_dispfrm_set_attribs(0, LEVELRG3, UM_BLACK, UM_WHITE);
		}
	}
	sfpk42 = UU_TRUE;
	if ((Trevsf==0)&&((Tbottype != STOCK)&&(Tbottype != PLANE))
		|| (Trevsf==1)&&(Tbottyp1 != STOCK))
	{
/*
.....disabled
*/
		sfpk42 = UU_TRUE;
		ud_dispfrm_set_attribs(0, LEVELRG5, UM_BLACK, UM_WHITE);
		ud_set_traverse_mask(LEVELRG5, UU_FALSE);
	}
	else
	{
		nc = (int)strlen(Tbot_off);
		ul_strip_blanks(Tbot_off, &nc);
		if (nc>0)
		{
			ud_dispfrm_set_attribs(0, LEVELRG5, UM_BLACK, UM_WHITE);
			sfpk42 = UU_TRUE;
		}
		else
		{
			sfpk42 = UU_FALSE;
			ud_dispfrm_set_attribs(0, LEVELRG5, UM_WHITE, UM_RED);
		}
		ud_set_traverse_mask(LEVELRG5, UU_TRUE);
	}
	sfpk43 = UU_TRUE;
	if ((Trevsf==0)&&(Ttoptype == STOCK)
		|| (Trevsf==1)&&(Ttoptyp1 == STOCK))
	{
		sfpk43 = UU_TRUE;
		ud_dispfrm_set_attribs(0, LEVELRG8, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(0, LEVELRG9, UM_BLACK, -1);
	}
	else
	{
		nc = (int)strlen(Tpoc_top);
		ul_strip_blanks(Tpoc_top, &nc);
		if (nc>0)
		{
			ud_dispfrm_set_attribs(0, LEVELRG8, UM_BLACK, UM_WHITE);
			sfpk43 = UU_TRUE;
			ud_dispfrm_set_attribs(0, LEVELRG9, UM_BLACK, Tcolort);
		}
		else
		{
			sfpk43 = UU_FALSE;
			ud_dispfrm_set_attribs(0, LEVELRG8, UM_WHITE, UM_RED);
			if ((Trevsf==0)&&(Ttoptype==PLANE))
				ud_dispfrm_set_attribs(0, LEVELRG9, UM_BLACK, UM_RED);
			else
				ud_dispfrm_set_attribs(0, LEVELRG9, UM_BLACK, Tcolort);
		}
/*
.....if disabled, display default color
*/
		if ((Trevsf==0)&&(Ttoptype != PLANE)
			|| (Trevsf==1))
		{
			ud_dispfrm_set_attribs(0, LEVELRG9, UM_BLACK, -1);
			ud_dispfrm_set_attribs(0, LEVELRG8, UM_BLACK, UM_WHITE);
		}
	}
	sfpk44 = UU_TRUE;
	if ((Tmethod > 1)&&(Tstep == UU_TRUE))
	{
/*
......LEVELRG11 enabled
*/
		nc = (int)strlen(Tinc_hgt);
		ul_strip_blanks(Tinc_hgt, &nc);
		if (nc>0)
		{
			ud_dispfrm_set_attribs(0, LEVELRG11, UM_BLACK, UM_WHITE);
			sfpk44 = UU_TRUE;
		}
		else
		{
			sfpk44 = UU_FALSE;
			ud_dispfrm_set_attribs(0, LEVELRG11, UM_WHITE, UM_RED);
		}
	}
	sfpk45 = UU_TRUE;
	if ((Trevsf==0)&&((Ttoptype != STOCK)&&(Ttoptype != PLANE))
		|| (Trevsf==1)&&(Ttoptyp1 != STOCK))
	{
		sfpk45 = UU_TRUE;
		ud_dispfrm_set_attribs(0, LEVELRG12, UM_BLACK, UM_WHITE);
		ud_set_traverse_mask(LEVELRG12, UU_FALSE);
	}
	else
	{
		nc = (int)strlen(Ttop_off);
		ul_strip_blanks(Ttop_off, &nc);
		if (nc>0)
		{
			ud_dispfrm_set_attribs(0, LEVELRG12, UM_BLACK, UM_WHITE);
			sfpk45 = UU_TRUE;
		}
		else
		{
			sfpk45 = UU_FALSE;
			ud_dispfrm_set_attribs(0, LEVELRG12, UM_WHITE, UM_RED);
		}
		ud_set_traverse_mask(LEVELRG12, UU_TRUE);
	}
	sfpk46 = UU_TRUE;
	if ((Trevsf==0)&&(Tfinis))
	{
		nc = (int)strlen(Tadj_up);
		ul_strip_blanks(Tadj_up, &nc);
		if (nc>0)
		{
			ud_dispfrm_set_attribs(0, LEVELRG14, UM_BLACK, UM_WHITE);
		}
		else
		{
			sfpk46 = UU_FALSE;
			ud_dispfrm_set_attribs(0, LEVELRG14, UM_WHITE, UM_RED);
		}
		nc = (int)strlen(Tadj_down);
		ul_strip_blanks(Tadj_down, &nc);
		if (nc>0)
		{
			ud_dispfrm_set_attribs(0, LEVELRG15, UM_BLACK, UM_WHITE);
		}
		else
		{
			sfpk46 = UU_FALSE;
			ud_dispfrm_set_attribs(0, LEVELRG15, UM_WHITE, UM_RED);
		}
	}
/*
......Options Section
*/
	sfpk51 = UU_TRUE;
	if (Tfrom == NEARPT)
	{
		nc = (int)strlen(Tpoc_end);
		ul_strip_blanks(Tpoc_end, &nc);
		if (nc>0)
		{
			ud_dispfrm_set_attribs(0, OPTRG3, UM_BLACK, UM_WHITE);
			ud_dispfrm_set_attribs(0, OPTRG4, UM_BLACK, Tcolore);
		}
		else
		{
			sfpk51 = UU_FALSE;
			ud_dispfrm_set_attribs(0, OPTRG3, UM_WHITE, UM_RED);
			ud_dispfrm_set_attribs(0, OPTRG4, UM_WHITE, UM_RED);
		}
	}
	else
	{
		ud_dispfrm_set_attribs(0, OPTRG3, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(0, OPTRG4, UM_BLACK, -1);
	}

		
	nc = (int)strlen(Tmax_gap);
	ul_strip_blanks(Tmax_gap, &nc);
	if (nc>0)
	{
		rval = atof(Tmax_gap);
		if (rval>=0)
		{
			ud_dispfrm_set_attribs(0, OPTRG5, UM_BLACK, UM_WHITE);
			sfpk52 = UU_TRUE;
		}
		else
		{
			sfpk52 = UU_FALSE;
			ud_dispfrm_set_attribs(0, OPTRG5, UM_WHITE, UM_RED);
		}
	}
	else
	{
		sfpk52 = UU_FALSE;
		ud_dispfrm_set_attribs(0, OPTRG5, UM_WHITE, UM_RED);
	}
	sfpk53 = UU_TRUE;
	sfpk54 = UU_TRUE;
	if (Tmethod > 0)
	{
/*
.....fields disabled
*/
		ud_dispfrm_set_attribs(0, OPTRG7, UM_BLACK, UM_WHITE);
		ud_dispfrm_set_attribs(0, OPTRG9, UM_BLACK, UM_WHITE);
	}
	else
	{
		if (Tmaxloop == UU_TRUE)
		{
			nc = (int)strlen(Tmax_loop);
			ul_strip_blanks(Tmax_loop, &nc);
			if (nc>0)
			{
				rval = atof(Tmax_loop);
				if (rval>=0)
				{
					ud_dispfrm_set_attribs(0, OPTRG7, UM_BLACK, UM_WHITE);
				}
				else
				{
					sfpk53 = UU_FALSE;
					ud_dispfrm_set_attribs(0, OPTRG7, UM_WHITE, UM_RED);
				}
			}
			else
			{
				sfpk53 = UU_FALSE;
				ud_dispfrm_set_attribs(0, OPTRG7, UM_WHITE, UM_RED);
			}
		}
		if (Toffpart == UU_TRUE)
		{
			nc = (int)strlen(Tpart_off);
			ul_strip_blanks(Tpart_off, &nc);
			if (nc>0)
			{
				rval = atof(Tpart_off);
				if (rval>=0)
				{
					ud_dispfrm_set_attribs(0, OPTRG9, UM_BLACK, UM_WHITE);
				}
				else
				{
					sfpk54 = UU_FALSE;
					ud_dispfrm_set_attribs(0, OPTRG9, UM_WHITE, UM_RED);
				}
			}
			else
			{
				sfpk54 = UU_FALSE;
				ud_dispfrm_set_attribs(0, OPTRG9, UM_WHITE, UM_RED);
			}
		}
	}
/*
......Zones Section
......we don't requied select anything
*/
	sfpk6 = UU_TRUE;
	nzones = UU_LIST_LENGTH(&Zcurves);
	if (nzones>0)
	{
		ud_set_traverse_mask(ZONERG2,UU_TRUE);
		ud_set_traverse_mask(ZONERG4,UU_TRUE);
		if (nzones>=2)
		{
			ud_set_traverse_mask(ZONERG5,UU_TRUE);
			ud_set_traverse_mask(ZONERG6,UU_TRUE);
		}
		else
		{
			ud_set_traverse_mask(ZONERG5,UU_FALSE);
			ud_set_traverse_mask(ZONERG6,UU_FALSE);
		}
	}
	else
	{
		ud_set_traverse_mask(ZONERG2,UU_FALSE);
		ud_set_traverse_mask(ZONERG4,UU_FALSE);
		ud_set_traverse_mask(ZONERG5,UU_FALSE);
		ud_set_traverse_mask(ZONERG6,UU_FALSE);
	}
/*
......Colors Section
......we don't requied select anything
*/
	sfpk7 = UU_TRUE;
/*
.....Set section color
*/
	if (sfpk1)
	{
		if (Sacc[PocketM]==0)
		{
			Ssecol[PocketM] = Sblack;
			S_section_changed(PocketM,UU_FALSE);
		}
		else
		{	
			Ssecol[PocketM] = Sgreen; 
			S_section_changed(PocketM,UU_TRUE);
		}
	}
	else
	{
		Ssecol[PocketM] = Sred; 
		S_section_changed(PocketM,UU_FALSE);
	}
	if (sfpk2)
	{
		if (Sacc[Geometry]==0)
		{
			Ssecol[Geometry] = Sblack;
			S_section_changed(Geometry,UU_FALSE);
		}
		else
		{	
			Ssecol[Geometry] = Sgreen; 
			S_section_changed(Geometry,UU_TRUE);
		}
	}
	else
	{
		Ssecol[Geometry] = Sred; 
		S_section_changed(Geometry,UU_FALSE);
	}
	if (sfpk31&&sfpk32&&sfpk33)
	{
		if (Sacc[Stock]==0)
		{
			Ssecol[Stock] = Sblack;
			S_section_changed(Stock,UU_FALSE);
		}
		else
		{	
			Ssecol[Stock] = Sgreen; 
			S_section_changed(Stock,UU_TRUE);
		}
	}
	else
	{
		Ssecol[Stock] = Sred; 
		S_section_changed(Stock,UU_FALSE);
	}
	if (sfpk41&&sfpk42&&sfpk43&&sfpk44&&sfpk45&&sfpk46)
	{
		if (Sacc[Levels]==0)
		{
			Ssecol[Levels] = Sblack;
			S_section_changed(Levels,UU_FALSE);
		}
		else
		{	
			Ssecol[Levels] = Sgreen; 
			S_section_changed(Levels,UU_TRUE);
		}
	}
	else
	{
		Ssecol[Levels] = Sred; 
		S_section_changed(Levels,UU_FALSE);
	}
	if (sfpk51&&sfpk52&&sfpk53&&sfpk54)
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
	if (sfpk6)
	{
		if (Sacc[Zones]==0)
		{
			Ssecol[Zones] = Sblack;
			S_section_changed(Zones,UU_FALSE);
		}
		else
		{	
			Ssecol[Zones] = Sgreen; 
			S_section_changed(Zones,UU_TRUE);
		}
	}
	else
	{
		Ssecol[Zones] = Sred; 
		S_section_changed(Zones,UU_FALSE);
	}
	if (sfpk7)
	{
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
	}
	else
	{
		Ssecol[Colors] = Sred; 
		S_section_changed(Colors,UU_FALSE);
	}
/*
.....Set Action Buttons
*/
	ifl = sfpk1 && sfpk2 && sfpk31 && sfpk32 &&sfpk33
		&&sfpk41&&sfpk42&&sfpk43&&sfpk44&&sfpk45&&sfpk46
		&&sfpk51&&sfpk52&&sfpk53&&sfpk54
		&& sfpk6 && sfpk7;
	ud_frm_enable_ok(ifl);
	ud_set_traverse_mask(FAPV,ifl);
	ud_set_traverse_mask(FAPY,ifl);

	ifl = Sacc[PocketM] + Sacc[Geometry] + Sacc[Stock] + Sacc[Levels] 
				+ Sacc[Options] + Sacc[Zones] + Sacc[Colors]; 
	ud_set_traverse_mask(FARS,ifl);
/*
.....if geom entered
*/
	ul_to_upper(Trev_surf);
	strcpy(label, Trev_surf);
	nc1 = strlen(label);
	ul_strip_blanks(label,&nc1);

	if ((Trevsf==0)&&(Tbottype == PLANE))
	{
		ul_to_upper(Tpoc_bot);
		strcpy(label, Tpoc_bot);
		nc2 = strlen(label);
		ul_strip_blanks(label,&nc2);
	}
	else
		nc2 = 0;
	if ((Trevsf==0)&&(Ttoptype == PLANE))
	{
		ul_to_upper(Tpoc_top);
		strcpy(label, Tpoc_top);
		nc3 = strlen(label);
		ul_strip_blanks(label,&nc3);
	}
	else
		nc3 = 0;
	if (Tfrom == NEARPT)
	{
		ul_to_upper(Tpoc_end);
		strcpy(label, Tpoc_end);
		nc4 = strlen(label);
		ul_strip_blanks(label,&nc4);
	}
	else
		nc4 = 0;
	Snholes = UU_LIST_LENGTH(&Sholes);
	if (Snholes<=0)
	{
		ud_set_traverse_mask(OPTRG11,UU_FALSE);
	}
	if ((nc1>0)||(nc2>0)||(nc3>0)||(nc4>0)||(nsurf>0)||(Bsfnum[0]>0)||(Bsfnum[1]>0)
		|| (nzones>0) || (Snholes > 0 ))
	{
		ud_set_traverse_mask(FAGE,1);
	}
	else
		ud_set_traverse_mask(FAGE,0);

	return UU_TRUE;
}


/*********************************************************************
**    I_FUNCTION     : OnPreview()
**			Calculates and displays the motion for the contour command
**			without creating an NCL statement.
**    PARAMETERS
**       INPUT  :
**          fieldno  Field number being changed.
**          val      Current field value.
**          stat     Field status.
**       OUTPUT :
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS :
**			Saves and restores motion parameters and the clfile position/
**			status, so that is appears this motion was never generated.
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnPreview(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_REAL value;
	UM_int2 maxloop;
	int flag = 0;

	if (Spreviewflg)
	{
/*
.....Reset motion settings
*/
		moinfr(&flag);
		if (Spokmod_saved)
		{
			pmdrst();
			Spokmod_saved = UU_FALSE;
		}

		pokrst();
	}
	Spreviewflg = 0;
	Schgmade = 0;
	Serrflg = 0;
/*
.....Erase last previewed motion
*/
	if (Smptr != UU_NULL) ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
	Smptr = UU_NULL;
/*
.....Save motion settings
*/
	if (Smaxloop == UU_FALSE)
		maxloop = 0;
	else
	{
		ncl_get_scalar_value(Smax_loop, &value);
		maxloop = (UM_int2)value;
	}
	poksav(&maxloop);
	moinfs();
	ncl_mark_mdisplay(&Smptr,&Smotatt,Smvpbox,UU_TRUE);
/*
.....Output *POCKET command
*/
	S_build_command(UU_FALSE);
/*
.....End of routine
*/
	if (Spreviewflg != 0) 
	{
		moinfr(&flag);
		Spreviewflg = 0;
	}
	else Spreviewflg = 1;
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : OnLayerSel()
**       Method called on the "Layers" push button in the Waterline form.
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
static UD_FSTAT OnLayerSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LIST laylst;
	struct UM_layer_rec *layptr;
	int status,iact,nlay;
/*
.....Load the defined layers into a list
*/
	status = umu_load_layers(&laylst,&nlay,&iact);
/*
.....Get the requested layer from the user
*/
	layptr = (struct UM_layer_rec *)UU_LIST_ARRAY(&laylst);
	status = umu_layer_select(layptr,nlay,1,&iact);
/*
.....Update the main form with
.....the selected layer
*/
	if (status == UU_SUCCESS)
	{
		if (*fieldno == GEOMRG1)
		{
			Tlayer = layptr[iact].num;
			sprintf(Tlay_num,"%d",Tlayer);
			ud_update_answer(GEOMRG2,(int *)Tlay_num);
		}
		else if (*fieldno == STOCKRG8)
		{
			Blayer[0] = layptr[iact].num;
			sprintf(TBlay_num[0], "%d", layptr[iact].num);
			ud_update_answer(STOCKRG9,(int *)TBlay_num[0]);
		}
		else if (*fieldno == STOCKRG15)
		{
			Blayer[1] = layptr[iact].num;
			sprintf(TBlay_num[1], "%d", layptr[iact].num);
			ud_update_answer(STOCKRG16,(int *)TBlay_num[1]);
		}
		ud_update_form (0);
/*
.....Change made since preview
*/
		if (Spreviewflg) Schgmade = 1;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnDeleteAll(fieldno, val, stat)
**			Displays all surfaces in the layer.
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
static UD_FSTAT OnDeleteAll(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (*fieldno == GEOMRG6)
	{
		nsurf = UU_LIST_LENGTH(&Ssurf);
		if (nsurf > 0)
		{
			S_unhilite_entities(&Ssurf, &(Sgeo_init[0]));
			nsurf = 0;
			if (Spreviewflg) Schgmade = 1;
		}
		Tlayerdef = 0;
		ud_update_answer(GEOMRG4,&Tlayerdef);
		Tlayer = 1;
		sprintf(Tlay_num,"%d",Tlayer);
		ud_update_answer(GEOMRG2,&Tlay_num);
		ud_set_traverse_mask(GEOMRG6,0);
	}
	else if (*fieldno == STOCKRG11)
	{
		Bsfnum[0] = UU_LIST_LENGTH(&(Bsurf[0]));
		if (Bsfnum[0] > 0)
		{
			S_unhilite_entities(&(Bsurf[0]), &(Sgeo_init[1]));
			Bsfnum[0] = 0;
			if (Spreviewflg) Schgmade = 1;
		}
		ud_set_traverse_mask(STOCKRG11,0);
	}
	else if (*fieldno == STOCKRG18)
	{
		Bsfnum[1] = UU_LIST_LENGTH(&(Bsurf[1]));
		if (Bsfnum[1] > 0)
		{
			S_unhilite_entities(&(Bsurf[1]), &(Sgeo_init[2]));
			Bsfnum[1] = 0;
			if (Spreviewflg) Schgmade = 1;
		}
		ud_set_traverse_mask(STOCKRG18,0);
	}
	else if (*fieldno == ZONERG2)
	{
		nzones = UU_LIST_LENGTH(&Zcurves);
		if (nzones > 0)
		{
			S_unhilite_entities(&Zcurves, &(Sgeo_init[4]));
			nzones = 0;
			Z_empty_flist(&stat_list,&strings);
			strcpy(stat_list.answer," ");
			ud_dispfrm_update_answer(0, ZONERG3, (int *)&stat_list);
			ud_update_form(0);
			if (Spreviewflg) Schgmade = 1;
		}
		ud_set_traverse_mask(ZONERG2,0);
	}
	else if (*fieldno == OPTRG11)
	{
		Snholes = UU_LIST_LENGTH(&Sholes);
		if (Snholes > 0)
		{
			S_unhilite_entities(&Sholes, &(Sgeo_init[3]));
			Snholes = 0;
			if (Spreviewflg) Schgmade = 1;
		}
		ud_set_traverse_mask(OPTRG11,0);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnShow(fieldno, val, stat)
**			Displays all surfaces in the layer.
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
static UD_FSTAT OnShow(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (*fieldno == GEOMRG3)
	{
		if (!Slaylist)
		{
			Slaylist = (UU_LIST *) uu_malloc (sizeof (UU_LIST));
			uu_list_init(Slaylist,sizeof(UM_sgeo),0,100);
		}
		return (ncl_show_layer(Slaylist,Tcolor,Tlayer,&Tlistlayer,2));
	}
	else if (*fieldno == STOCKRG10)
	{
		if (!Blaylist[0])
		{
			Blaylist[0] = (UU_LIST *) uu_malloc (sizeof (UU_LIST));
			uu_list_init(Blaylist[0],sizeof(UM_sgeo),0,100);
		}
		return (ncl_show_layer(Blaylist[0],TBcolor[0],Blayer[0],&Blistlayer[0],2));
	}
	else if (*fieldno == STOCKRG17)
	{
		if (!Blaylist[1])
		{
			Blaylist[1] = (UU_LIST *) uu_malloc (sizeof (UU_LIST));
			uu_list_init(Blaylist[1],sizeof(UM_sgeo),0,100);
		}
		return (ncl_show_layer(Blaylist[1],TBcolor[1], Blayer[1],&Blistlayer[1],2));
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnLayer(fieldno, val, stat)
**			Deselects all surfaces from the list if the layer changed.
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
static UD_FSTAT OnLayer(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i = atoi(Slay_num);
	char *name = "Pocket Geometry";

	if (i != Slayer)
	{
		Slayer = i;
		um_set_layer_num(i);
		um_set_layer_name(i,name);
/*
.....Change made since preview
*/
		if (Spreviewflg) Schgmade = 1;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnModals()
**       Displays the Pocket or VoluMill Modals form depending on
**       the selected pocketing method.
**    PARAMETERS
**       INPUT  : fieldno = Not used.
**                val     = Not used, form initiated through pushbutton.
**                stat    = Not used.
**       OUTPUT :
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnModals(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	if (Tmethod == 0)
	{
		nclu_pokmod(&Smodals, UU_FALSE, Sclick_pokmod);
		Sclick_pokmod = UU_TRUE;
	}
	else
	{
		nclu_vmpmod(UU_FALSE);
		Sclick_vmpmod = UU_TRUE;
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
**    S_FUNCTION     :  OnAction()
**       Method called when additional setting fields are traversed
**    PARAMETERS
**       INPUT  : fieldno = Not used.
**                val     = Not used, form initiated through pushbutton.
**                stat    = Not used.
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
		S_deselect_all();
		if (Smptr != UU_NULL) 
			ncl_erase_mdisplay(&Smptr,&Smotatt,Smvpbox);
		Smptr = UU_NULL;
		if (Spreviewflg) moinfr(&flag);
		S_init_form();
		S_enter_form(fieldno,val,stat);
		for (i=FAPV;i<FAVW;i++) ud_set_traverse_mask(i,0);
		S_update_answers();
		Schgmade = Spreviewflg = UU_FALSE;
		Sacc[PocketM] = Sacc[Geometry] = Sacc[Stock] = Sacc[Levels] = 0;
		Sacc[Options] = Sacc[Zones] = Sacc[Colors] = 0;
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
**    I_FUNCTION     : OnColors()
**			Color button callback
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
		S_hilite_entities(&Ssurf, Tcolor);
		break;
	case CLRRG2:
		S_hilite_entity(&Sgrev,Tcolorv);
		break;
	case CLRRG3:
		S_hilite_entities(&Bsurf[0], TBcolor[0]);
		break;
	case CLRRG4:
		S_hilite_entities(&Bsurf[1], TBcolor[1]);
		break;
	case CLRRG5:
		S_hilite_entity(&Sgbot,Tcolorb);
		break;
	case CLRRG6:
		S_hilite_entity(&Sgtop,Tcolort);
		break;
	case CLRRG7:
		S_hilite_entity(&Sgstpt,Tcolore);
		break;
	case CLRRG8:
		S_hilite_entities(&Sholes,Tcolorh);
		break;
	case CLRRG9:
		S_hilite_entities(&Zcurves, Tzcolor);
		break;
	case CLRRG10:
		Tgeotog = val->frmint[0];
	case CLRRG11:
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
**    I_FUNCTION     : OnZoneList()
**			Zones List select callback
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
static UD_FSTAT OnZoneList(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UM_sgeo *geo;
	int i;
/*
.....Remove all entities in this list
*/
	geo = (UM_sgeo *) UU_LIST_ARRAY(&Zcurves);
	for (i=0; i<nzones; i++)
	{
		if (strcmp(geo[i].label,val->frmstr) == 0)
		{
			zcurnam = i;
			break;
		}
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : Onchk()
**			check box callback
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
static UD_FSTAT Onchk(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch(*fieldno)
	{
		case GEOMRG4:
			if (Stog_ch[0]!=Tlayerdef)
			{
				Stog_ch[0] = Tlayerdef; 
				Sacc[Geometry] = 1;
			}
			if (Tlayerdef == 0)
			{
				ud_set_traverse_mask(GEOMRG5,0);
				ud_set_traverse_mask(GEOMRG6,0);
				ud_set_traverse_mask(CLRRG1,0);
			}
			else
			{
				ud_set_traverse_mask(GEOMRG5,1);
				nsurf = UU_LIST_LENGTH(&Ssurf);
				if (nsurf>0)
					ud_set_traverse_mask(GEOMRG6,1);
				ud_set_traverse_mask(CLRRG1,1);
			}
			break;
		case STOCKRG2:
			if (Stog_ch[1]!=Tignore)
			{
				Stog_ch[1] = Tignore; 
				Sacc[Stock] = 1;
			}
			break;
		case STOCKRG5:
			if (Stog_ch[7]!=Tstksf)
			{
				Stog_ch[7] = Tstksf; 
				Sacc[Stock] = 1;
			}
			if (Tstksf) 
			{
				ud_set_traverse_mask(STOCKRG6,UU_TRUE);
				ud_set_traverse_mask(STOCKRG7,UU_TRUE);
				ud_set_traverse_mask(CLRRG3,UU_TRUE);
				if (TBlayincl[0]) 
				{
					ud_set_traverse_mask(STOCKRG8,UU_TRUE);
					ud_set_traverse_mask(STOCKRG9,UU_TRUE);
					ud_set_traverse_mask(STOCKRG10,UU_TRUE);
				}
				else 
				{
					ud_set_traverse_mask(STOCKRG8,UU_FALSE);
					ud_set_traverse_mask(STOCKRG9,UU_FALSE);
					ud_set_traverse_mask(STOCKRG10,UU_FALSE);
				}
				Bsfnum[0] = Bsurf[0].cur_cnt;
				if (Bsfnum[0]>0)
					ud_set_traverse_mask(STOCKRG11,UU_TRUE);
				else
					ud_set_traverse_mask(STOCKRG11,UU_FALSE);
			}
			else 
			{
				ud_set_traverse_mask(CLRRG3,UU_FALSE);
				ud_set_traverse_mask(STOCKRG6,UU_FALSE);
				ud_set_traverse_mask(STOCKRG7,UU_FALSE);
				ud_set_traverse_mask(STOCKRG8,UU_FALSE);
				ud_set_traverse_mask(STOCKRG9,UU_FALSE);
				ud_set_traverse_mask(STOCKRG10,UU_FALSE);
				ud_set_traverse_mask(STOCKRG11,UU_FALSE);
			}
			break;
		case STOCKRG12:
			if (Stog_ch[8]!=Tavdsf)
			{
				Stog_ch[8] = Tavdsf; 
				Sacc[Stock] = 1;
			}
			if (Tavdsf) 
			{
				ud_set_traverse_mask(STOCKRG13,UU_TRUE);
				ud_set_traverse_mask(STOCKRG14,UU_TRUE);
				ud_set_traverse_mask(CLRRG2,UU_TRUE);
				if (TBlayincl[1]) 
				{
					ud_set_traverse_mask(STOCKRG15,UU_TRUE);
					ud_set_traverse_mask(STOCKRG16,UU_TRUE);
					ud_set_traverse_mask(STOCKRG17,UU_TRUE);
				}
				else 
				{
					ud_set_traverse_mask(STOCKRG15,UU_FALSE);
					ud_set_traverse_mask(STOCKRG16,UU_FALSE);
					ud_set_traverse_mask(STOCKRG17,UU_FALSE);
				}
				Bsfnum[1] = Bsurf[1].cur_cnt;
				if (Bsfnum[1]>0)
					ud_set_traverse_mask(STOCKRG18,UU_TRUE);
				else
					ud_set_traverse_mask(STOCKRG18,UU_FALSE);
			}
			else 
			{
				ud_set_traverse_mask(CLRRG2,UU_FALSE);
				ud_set_traverse_mask(STOCKRG13,UU_FALSE);
				ud_set_traverse_mask(STOCKRG14,UU_FALSE);
				ud_set_traverse_mask(STOCKRG15,UU_FALSE);
				ud_set_traverse_mask(STOCKRG16,UU_FALSE);
				ud_set_traverse_mask(STOCKRG17,UU_FALSE);
				ud_set_traverse_mask(STOCKRG18,UU_FALSE);
			}
			break;
		case STOCKRG7:
			if (Stog_ch[9]!=TBlayincl[0])
			{
				Stog_ch[9] = TBlayincl[0]; 
				Sacc[Stock] = 1;
			}
			if (TBlayincl[0]) 
			{
				ud_set_traverse_mask(STOCKRG8,UU_TRUE);
				ud_set_traverse_mask(STOCKRG9,UU_TRUE);
				ud_set_traverse_mask(STOCKRG10,UU_TRUE);
			}
			else 
			{
				ud_set_traverse_mask(STOCKRG8,UU_FALSE);
				ud_set_traverse_mask(STOCKRG9,UU_FALSE);
				ud_set_traverse_mask(STOCKRG10,UU_FALSE);
			}
			break;
		case STOCKRG14:
			if (Stog_ch[10]!=TBlayincl[1])
			{
				Stog_ch[10] = TBlayincl[1]; 
				Sacc[Stock] = 1;
			}
			if (TBlayincl[1]) 
			{
				ud_set_traverse_mask(STOCKRG15,UU_TRUE);
				ud_set_traverse_mask(STOCKRG16,UU_TRUE);
				ud_set_traverse_mask(STOCKRG17,UU_TRUE);
			}
			else 
			{
				ud_set_traverse_mask(STOCKRG15,UU_FALSE);
				ud_set_traverse_mask(STOCKRG16,UU_FALSE);
				ud_set_traverse_mask(STOCKRG17,UU_FALSE);
			}
			break;
		case LEVELRG10:
			if (Stog_ch[6]!=Tstep)
			{
				Stog_ch[6] = Tstep; 
				Sacc[Levels] = 1;
			}
			if (Tstep == UU_TRUE) ud_set_traverse_mask(LEVELRG11,UU_TRUE);
			else ud_set_traverse_mask(LEVELRG11,UU_FALSE);
			break;
		case LEVELRG13:
			if (Stog_ch[2]!=Tfinis)
			{
				Stog_ch[2] = Tfinis; 
				Sacc[Levels] = 1;
			}
			if (Tfinis == UU_TRUE && Tmethod < 2)
			{
				ud_set_traverse_mask(LEVELRG14,UU_TRUE);
				ud_set_traverse_mask(LEVELRG15,UU_TRUE);
			}
			else
			{
				ud_set_traverse_mask(LEVELRG14,UU_FALSE);
				ud_set_traverse_mask(LEVELRG15,UU_FALSE);
			}
			break;
		case OPTRG1:
			if (Stog_ch[3]!=Tdeep)
			{
				Stog_ch[3] = Tdeep; 
				Sacc[Options] = 1;
			}
			break;
		case OPTRG6:
			if (Stog_ch[4]!=Tmaxloop)
			{
				Stog_ch[4] = Tmaxloop; 
				Sacc[Options] = 1;
			}
			if (Tmaxloop == UU_TRUE)
				ud_set_traverse_mask(OPTRG7,UU_TRUE);
			else
				ud_set_traverse_mask(OPTRG7,UU_FALSE);
			break;
		case OPTRG8:
			if (Stog_ch[5]!=Toffpart)
			{
				Stog_ch[5] = Toffpart; 
				Sacc[Options] = 1;
			}
			break;
	}
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnSelect()
**			buttons callback
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
static UD_FSTAT OnSelect(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i, pr, namfld, *mask;
	UM_sgeo *zcv;
	int j1 = 0;
	char *namp;
	int status;

	if (*fieldno == POCKETRG4)
	{
		mask = (int *)UD_ncl_allsfsh;
		pr = 94;
		namp = Trev_surf;
		namfld = POCKETRG5;
		status = S_select_geo(&Sgrev,UU_NULL,mask,0,pr,Tcolorv,0,namfld,namp, NULL);
	}
	else if (*fieldno == GEOMRG5)
	{
		mask = (int *)UD_ncl_allsfsh;
		pr = 478;
		status = S_select_geo(&Sgsf,&Ssurf,mask,3,pr,Tcolor,0,-1,NULL, &(Sgeo_init[0]));
	}
	else if (*fieldno == STOCKRG6)
	{
		mask = (int *)UD_ncl_allsfsh;
		pr = 478;
		status = S_select_geo(&Sbsrf1,&Bsurf[0],mask,3,pr,TBcolor[0],0,-1,NULL, &(Sgeo_init[1]));
	}
	else if (*fieldno == STOCKRG13)
	{
		mask = (int *)UD_ncl_allsfsh;
		pr = 478;
		status = S_select_geo(&Sbsrf2,&Bsurf[1],mask,3,pr,TBcolor[1],0,-1,NULL, &(Sgeo_init[2]));
	}
	else if (*fieldno == LEVELRG4)
	{
		mask = (int *)UD_ncl_allsfpl;
		pr = 493;
		namp = Tpoc_bot;
		namfld = LEVELRG3;
		status = S_select_geo(&Sgbot,UU_NULL,mask,0,pr,Tcolorb,0,namfld,namp,NULL);
	}
	else if (*fieldno == LEVELRG9)
	{
		mask = (int *)UD_ncl_allsfpl;
		pr = 494;
		namp = Tpoc_top;
		namfld = LEVELRG8;
		status = S_select_geo(&Sgtop,UU_NULL,mask,0,pr,Tcolort,0,namfld,namp,NULL);
	}
	else if (*fieldno == OPTRG4)
	{
		mask = (int *)UD_ncl_pt;
		pr = 484;
		namp = Tpoc_end;
		namfld = OPTRG3;
		status = S_select_geo(&Sgstpt,UU_NULL,mask,0,pr,Tcolore,0,namfld,namp,NULL);
	}
	else if (*fieldno == OPTRG10)
	{
		mask = (int *)UD_ncl_ptpv;
		pr = 503;
		status = S_select_geo(&Sghol, &Sholes, mask,3,pr,Tcolorh,0,-1,NULL, &(Sgeo_init[3]));
		Snholes = UU_LIST_LENGTH(&Sholes);
		if (Snholes > 0)
			ud_set_traverse_mask(OPTRG11,1);
	}
	else if (*fieldno == ZONERG1)
	{
		mask = (int *)UD_ncl_offcv;
		pr = 657;
		status = S_select_geo(&Sgzcv, &Zcurves, mask,3,pr,Tzcolor,0,-1,NULL, &(Sgeo_init[4]));
		nzones = Zcurves.cur_cnt;
		if (nzones > 0)
		{
			Z_empty_flist(&stat_list,&strings);
			zcv = (UM_sgeo *) UU_LIST_ARRAY (&Zcurves);
			for (i = 0; i < nzones; i++)
			{
				Z_put_list(&stat_list,&strings,zcv[i].label);
				ud_dispfrm_update_answer(0,ZONERG3,(int *)&stat_list);
				ud_update_form(0);
			}
		}
	}
	else if ((*fieldno == GEOMRG1)
		|| (*fieldno == STOCKRG8)
		|| (*fieldno == STOCKRG15))
	{
		OnLayerSel(fieldno, val, stat);
		status = 0;
	}
	else if ((*fieldno == GEOMRG3)
		|| (*fieldno == STOCKRG10)
		|| (*fieldno == STOCKRG17))
	{
		OnShow(fieldno, val, stat);
		status = 0;
	}
	else if ((*fieldno == GEOMRG6)
		|| (*fieldno == STOCKRG11)
		|| (*fieldno == STOCKRG18)
		|| (*fieldno == ZONERG2)
		|| (*fieldno == OPTRG11))
	{
		OnDeleteAll(fieldno, val, stat);
		status = 0;
	}
	if (status!=-1)
	{
		if (Spreviewflg) Schgmade = 1;
		if (*fieldno == POCKETRG4)
			Sacc[PocketM] = 1;
		if (*fieldno == GEOMRG5)
			Sacc[Geometry] = 1;
		if ((*fieldno==STOCKRG6)||(*fieldno==STOCKRG6))
			Sacc[Stock] = 1;
		if ((*fieldno==LEVELRG4)||(*fieldno==LEVELRG9))
			Sacc[Levels] = 1;
		if ((*fieldno==OPTRG4)||(*fieldno==OPTRG11))
			Sacc[Options] = 1;
		if (*fieldno == ZONERG1)
			Sacc[Zones] = 1;
		S_enable_buttons();
	}
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
	UD_DDATA inval;
	int lval, fno, tempnum;

	switch(*fieldno)
	{
		case POCKETRG1:
			if (Smenu_ch[0]!=Tpktype)
			{
				Smenu_ch[0] = Tpktype; 
				Sacc[PocketM] = 1;
			}
			if (Tpktype == 0)
			{
/*
.....Entire Part
*/
				Tstock = 1;
				ud_update_answer(STOCKRG1,(int *)&Tstock);
				ud_set_traverse_mask(STOCKRG1,0);
				ud_set_display_mask(UD_INPUTF, STOCKRG2, UU_FALSE);
				ud_set_display_mask(UD_INPUTF, STOCKRG3, UU_TRUE);
				ud_set_display_mask(UD_INPUTF, STOCKRG4, UU_TRUE);
			}
			else if (Tpktype == 1)
			{
/*
......Single Pocket
*/
				Tstock = 0;
				ud_update_answer(STOCKRG1,(int *)&Tstock);
				ud_set_traverse_mask(STOCKRG1,0);			
				ud_set_display_mask(UD_INPUTF, STOCKRG3, UU_FALSE);
				ud_set_display_mask(UD_INPUTF, STOCKRG2, UU_TRUE);
				ud_set_traverse_mask(STOCKRG3, 0);
				ud_set_traverse_mask(STOCKRG2, 0);
				ud_set_display_mask(UD_INPUTF, STOCKRG4, UU_FALSE);
				ud_set_traverse_mask(STOCKRG5, 1);

				Tignore = 0;
				ud_update_answer(STOCKRG2,(int *)&Tignore);
			}
			else if (Tpktype == 2)
			{
/*
.....Inner Pocket
*/
				Tstock = 0;
				ud_update_answer(STOCKRG1,(int *)&Tstock);
				ud_set_traverse_mask(STOCKRG1,0);
				ud_set_display_mask(UD_INPUTF, STOCKRG3, UU_FALSE);
				ud_set_display_mask(UD_INPUTF, STOCKRG2, UU_TRUE);
				ud_set_display_mask(UD_INPUTF, STOCKRG4, UU_TRUE);
				Tignore = 1;
				ud_update_answer(STOCKRG2,(int *)&Tignore);
			}
			else if (Tpktype == 3)
			{
/*
.....custom
*/
/*
.....enable but same value
*/
				ud_set_traverse_mask(STOCKRG1,1);
				lval = (Tstock == UU_TRUE)? 1: 0;
				ud_set_display_mask(UD_INPUTF, STOCKRG2, 1-lval);
				ud_set_traverse_mask(STOCKRG2, 1-lval);
				ud_set_traverse_mask(STOCKRG5, 1-lval);
				ud_set_display_mask(UD_INPUTF, STOCKRG3, lval);
				ud_set_display_mask(UD_INPUTF, STOCKRG4, UU_TRUE);
				ud_set_traverse_mask(STOCKRG3,lval);
				ud_update_answer(STOCKRG2,(int *)&Tignore);
			}
/*			break; */
		case STOCKRG1:
			if (Smenu_ch[3]!=Tstock)
			{
				Smenu_ch[3] = Trevsf; 
				Sacc[Stock] = 1;
			}
			lval = (Tstock == UU_TRUE)? 1: 0;
			ud_set_display_mask(UD_INPUTF, STOCKRG2, 1-lval);
			ud_set_display_mask(UD_INPUTF, STOCKRG3, lval);
			ud_set_traverse_mask(STOCKRG5, 1-lval);
			Tstksf = 1-lval;
			ud_update_answer(STOCKRG5,(int *)&Tstksf);
			if ((Tpktype == 3)||(Tpktype == 0))
			{
				ud_set_traverse_mask(STOCKRG2, 1-lval);
				ud_set_traverse_mask(STOCKRG3,lval);
			}
			else if ((Tpktype == 1)||(Tpktype == 2))
			{
				ud_set_traverse_mask(STOCKRG3,0);
				ud_set_traverse_mask(STOCKRG2,0);
			}
/*
.....do not show Expension field when Stock choice is "include", per Ken
*/
			if (Tstock==0)
				ud_set_display_mask(UD_INPUTF, STOCKRG4, 0);
			else
				ud_set_display_mask(UD_INPUTF, STOCKRG4, 1);
			fno = STOCKRG5;
			inval.frmint = (int*)&tempnum;
			inval.frmint[0] = Tstksf;
			Onchk(&fno, &inval, stat);
			ud_update_form(0);
			break;
		case POCKETRG2:
			if (Smenu_ch[1]!=Tmethod)
			{
				Smenu_ch[1] = Tmethod; 
				Sacc[PocketM] = 1;
			}
			ud_set_traverse_mask(LEVELRG10,0);
			ud_set_traverse_mask(LEVELRG11,0);
/*
.....always show Finished Level
*/
			ud_set_display_mask(UD_INPUTF, LEVELRG13, 1);
			if (Tmethod > 0)
			{
				Trevsf = 0; 
				ud_set_traverse_mask(POCKETRG3,0);
				ud_set_traverse_mask(OPTRG6,0);
				ud_set_traverse_mask(OPTRG7,0);
				ud_set_traverse_mask(OPTRG8,0);
				ud_set_traverse_mask(OPTRG9,0);
				if (Tmethod > 1)
				{
					ud_set_traverse_mask(LEVELRG10,1);
					if (Tstep) ud_set_traverse_mask(LEVELRG11,1);
				}
				ud_set_traverse_mask(OPTRG10,1);
				ud_update_answer(POCKETRG3,(int *)&Trevsf);
				ud_set_traverse_mask(CLRRG8,1);
/*
.....hiding all those items, per Ken
*/
				ud_set_display_mask(UD_INPUTF, POCKETRG3, 0);
				ud_set_display_mask(UD_INPUTF, POCKETRG4, 0);
				ud_set_display_mask(UD_INPUTF, POCKETRG5, 0);
				ud_set_display_mask(UD_INPUTF, OPTRG6, 0);
				ud_set_display_mask(UD_INPUTF, OPTRG7, 0);
				ud_set_display_mask(UD_INPUTF, OPTRG8, 0);
				ud_set_display_mask(UD_INPUTF, OPTRG9, 0);
/*
.....show
*/
				ud_set_display_mask(UD_INPUTF, OPTRG10, 1);
				ud_set_display_mask(UD_INPUTF, OPTRG11, 1);
/*per Ken*/
				if (Tmethod > 1)
				{
					ud_set_display_mask(UD_INPUTF, LEVELRG11, 1);
					ud_set_display_mask(UD_INPUTF, LEVELRG10, 1);
/*
.....Only show Av up/Av Down for advance and 2D
*/
					ud_set_display_mask(UD_INPUTF, LEVELRG14, 0);
					ud_set_display_mask(UD_INPUTF, LEVELRG15, 0);
				}
				else
				{
					ud_set_display_mask(UD_INPUTF, LEVELRG11, 0);
					ud_set_display_mask(UD_INPUTF, LEVELRG10, 0);
/*
.....Only show Av up/Av Down for advance and 2D
*/
					ud_set_display_mask(UD_INPUTF, LEVELRG14, 1);
					ud_set_display_mask(UD_INPUTF, LEVELRG15, 1);
				}
			}
			else
			{
				ud_set_traverse_mask(POCKETRG3,1);
				ud_set_traverse_mask(OPTRG6,1);
				ud_set_traverse_mask(OPTRG7,1);
				ud_set_traverse_mask(OPTRG8,1);
				ud_set_traverse_mask(OPTRG9,1);
				ud_set_traverse_mask(OPTRG10,0);
				ud_set_traverse_mask(OPTRG11,0);
				ud_set_traverse_mask(CLRRG8,0);
/*
.....reshow
*/
				ud_set_display_mask(UD_INPUTF, POCKETRG3, 1);
				ud_set_display_mask(UD_INPUTF, POCKETRG4, 1);
				ud_set_display_mask(UD_INPUTF, POCKETRG5, 1);
				ud_set_display_mask(UD_INPUTF, OPTRG6, 1);
				ud_set_display_mask(UD_INPUTF, OPTRG7, 1);
				ud_set_display_mask(UD_INPUTF, OPTRG8, 1);
				ud_set_display_mask(UD_INPUTF, OPTRG9, 1);
				if (Trevsf == 0)
				{
					ud_set_display_mask(UD_INPUTF, LEVELRG13, 1);
					ud_set_display_mask(UD_INPUTF, LEVELRG14, 1);
					ud_set_display_mask(UD_INPUTF, LEVELRG15, 1);
				}
				else
				{
					ud_set_display_mask(UD_INPUTF, LEVELRG13, 0);
					ud_set_display_mask(UD_INPUTF, LEVELRG14, 0);
					ud_set_display_mask(UD_INPUTF, LEVELRG15, 0);
				}
				ud_set_display_mask(UD_INPUTF, OPTRG10, 0);
				ud_set_display_mask(UD_INPUTF, OPTRG11, 0);
/*per Ken*/
				ud_set_display_mask(UD_INPUTF, LEVELRG11, 0);
				ud_set_display_mask(UD_INPUTF, LEVELRG10, 0);
			}
			break;
		case POCKETRG3:
			if (Smenu_ch[2]!=Trevsf)
			{
				Smenu_ch[2] = Trevsf; 
				Sacc[PocketM] = 1;
			}
			lval = (Trevsf == 1)? 1: 0;
			ud_set_traverse_mask(POCKETRG4, lval);
			ud_set_traverse_mask(POCKETRG5, lval);
			ud_set_traverse_mask(CLRRG2, lval);

			ud_set_traverse_mask(LEVELRG2,lval);
			ud_set_traverse_mask(LEVELRG7,lval);
			ud_set_display_mask(UD_INPUTF,LEVELRG2,lval);
			ud_set_display_mask(UD_INPUTF,LEVELRG7,lval);

			ud_set_traverse_mask(LEVELRG1,1-lval);
			ud_set_traverse_mask(LEVELRG6,1-lval);
			ud_set_display_mask(UD_INPUTF,LEVELRG1,1-lval);
			ud_set_display_mask(UD_INPUTF,LEVELRG6,1-lval);
			ud_set_traverse_mask(LEVELRG13,1-lval);
/*
.....don't display Level top/bottom select button
*/
			 if (Trevsf == 1)
			 {
				ud_set_display_mask(UD_INPUTF,LEVELRG4,0);
				ud_set_display_mask(UD_INPUTF,LEVELRG9,0);
				ud_set_display_mask(UD_INPUTF, LEVELRG13, 0);
				ud_set_display_mask(UD_INPUTF, LEVELRG14, 0);
				ud_set_display_mask(UD_INPUTF, LEVELRG15, 0);

			 }
			 else
			 {
				ud_set_display_mask(UD_INPUTF,LEVELRG4,1);
				ud_set_display_mask(UD_INPUTF,LEVELRG9,1);
				if (Tmethod==0)
				{
					ud_set_display_mask(UD_INPUTF, LEVELRG13, 1);
					ud_set_display_mask(UD_INPUTF, LEVELRG14, 1);
					ud_set_display_mask(UD_INPUTF, LEVELRG15, 1);
				}
			 }
			break;
		case STOCKRG3:
			if (Smenu_ch[4]!=Tstocktype)
			{
				Smenu_ch[4] = Tstocktype; 
				Sacc[Stock] = 1;
			}
			break;
		case LEVELRG1:
/*
.....save value
*/
			if (Smenu_ch[5] == PLANE)
			{
				strcpy(Sav_poc_bot_str, Tpoc_bot);
			}
			else if (Smenu_ch[5] != STOCK)
			{
				strcpy(Sav_poc_bot_val, Tpoc_bot);
			}
			if (Smenu_ch[5]!=Tbottype)
			{
				Smenu_ch[5] = Tbottype; 
				Sacc[Levels] = 1;
			}
			if (Tbottype == STOCK)
			{
				ud_set_traverse_mask(LEVELRG3,UU_FALSE);
				ud_set_traverse_mask(LEVELRG4,UU_FALSE);
				ud_set_traverse_mask(CLRRG5,UU_FALSE);
				ud_set_traverse_mask(LEVELRG5,UU_TRUE);
			}
			else if (Tbottype == PLANE)
			{
				strcpy(Tpoc_bot, Sav_poc_bot_str);
				ud_update_answer(LEVELRG3, (int *)Tpoc_bot);
				ud_set_traverse_mask(LEVELRG3,UU_TRUE);
				ud_set_traverse_mask(LEVELRG4,UU_TRUE);
				ud_set_traverse_mask(CLRRG5,UU_TRUE);
				ud_set_traverse_mask(LEVELRG5,UU_TRUE);
			}
			else
			{
				strcpy(Tpoc_bot, Sav_poc_bot_val);
				ud_update_answer(LEVELRG3, (int *)Tpoc_bot);
				ud_set_traverse_mask(LEVELRG3,UU_TRUE);
				ud_set_traverse_mask(LEVELRG4,UU_FALSE);
				ud_set_traverse_mask(CLRRG5,UU_FALSE);
				ud_set_traverse_mask(LEVELRG5,UU_FALSE);
			}
			break;
		case LEVELRG2:
			if (Smenu_ch[6]!=Tbottyp1)
			{
				Smenu_ch[6] = Tbottyp1; 
				Sacc[Levels] = 1;
			}
			if (Tbottyp1 == STOCK)
			{
				ud_set_traverse_mask(LEVELRG3,UU_FALSE);
				ud_set_traverse_mask(LEVELRG4,UU_FALSE);
				ud_set_traverse_mask(CLRRG5,UU_FALSE);
				ud_set_traverse_mask(LEVELRG5,UU_TRUE);
			}
			else
			{
				ud_set_traverse_mask(LEVELRG3,UU_TRUE);
				ud_set_traverse_mask(LEVELRG4,UU_FALSE);
				ud_set_traverse_mask(CLRRG5,UU_FALSE);
				ud_set_traverse_mask(LEVELRG5,UU_FALSE);
			}
			break;
		case LEVELRG6:
/*
.....save value
*/
			if (Smenu_ch[7] == PLANE)
			{
				strcpy(Sav_poc_top_str, Tpoc_top);
			}
			else if (Smenu_ch[7] != STOCK)
			{
				strcpy(Sav_poc_top_val, Tpoc_top);
			}
			if (Smenu_ch[7]!=Ttoptype)
			{
				Smenu_ch[7] = Ttoptype; 
				Sacc[Levels] = 1;
			}
			if (Ttoptype == PLANE)
			{
				strcpy(Tpoc_top, Sav_poc_top_str);
				ud_update_answer(LEVELRG8, (int *)Tpoc_top);
				ud_set_traverse_mask(LEVELRG8,UU_TRUE);
				ud_set_traverse_mask(LEVELRG9,UU_TRUE);
				ud_set_traverse_mask(CLRRG6,UU_TRUE);
				ud_set_traverse_mask(LEVELRG12,UU_TRUE);
			}
			else if (Ttoptype == STOCK)
			{
				ud_set_traverse_mask(LEVELRG8,UU_FALSE);
				ud_set_traverse_mask(LEVELRG9,UU_FALSE);
				ud_set_traverse_mask(CLRRG6,UU_FALSE);
				ud_set_traverse_mask(LEVELRG12,UU_TRUE);
			}
			else
			{
				strcpy(Tpoc_top, Sav_poc_top_val);
				ud_update_answer(LEVELRG8, (int *)Tpoc_top);
				ud_set_traverse_mask(LEVELRG8,UU_TRUE);
				ud_set_traverse_mask(LEVELRG9,UU_FALSE);
				ud_set_traverse_mask(CLRRG6,UU_FALSE);
				ud_set_traverse_mask(LEVELRG12,UU_FALSE);
			}
			break;
		case LEVELRG7:
			if (Smenu_ch[8]!=Ttoptyp1)
			{
				Smenu_ch[8] = Ttoptyp1; 
				Sacc[Levels] = 1;
			}
			if (Ttoptyp1 == STOCK)
			{
				ud_set_traverse_mask(LEVELRG8,UU_FALSE);
				ud_set_traverse_mask(LEVELRG9,UU_FALSE);
				ud_set_traverse_mask(CLRRG6,UU_FALSE);
				ud_set_traverse_mask(LEVELRG12,UU_TRUE);
			}
			else
			{
				ud_set_traverse_mask(LEVELRG8,UU_TRUE);
				ud_set_traverse_mask(LEVELRG9,UU_FALSE);
				ud_set_traverse_mask(CLRRG6,UU_FALSE);
				ud_set_traverse_mask(LEVELRG12,UU_FALSE);
			}
			break;
		case OPTRG2:
			if (Smenu_ch[9]!=Tfrom)
			{
				Smenu_ch[9] = Tfrom; 
				Sacc[Options] = 1;
			}
			lval = (Tfrom == NEARPT)? 1: 0;
			ud_set_traverse_mask(OPTRG3,lval);
			ud_set_traverse_mask(OPTRG4,lval);
			ud_set_traverse_mask(CLRRG7,lval);
			break;
	}
	if ((*fieldno==POCKETRG1)||(*fieldno==POCKETRG2)||(*fieldno==POCKETRG3))
	{
/*
......update picture
*/
		ud_dispfrm_show_frmpic(0, PIC_INDEX1, 0);
		ud_dispfrm_show_frmpic(0, PIC_INDEX2, 0);
		ud_dispfrm_show_frmpic(0, PIC_INDEX3, 0);
		ud_dispfrm_show_frmpic(0, PIC_INDEX4, 0);
		ud_dispfrm_show_frmpic(0, PIC_INDEX5, 0);
		ud_dispfrm_show_frmpic(0, PIC_INDEX6, 0);
		ud_dispfrm_show_frmpic(0, PIC_INDEX7, 0);
		ud_dispfrm_show_frmpic(0, PIC_INDEX8, 0);
		ud_dispfrm_show_frmpic(0, PIC_INDEX9, 0);
		ud_dispfrm_show_frmpic(0, PIC_INDEX10, 0);
		ud_dispfrm_show_frmpic(0, PIC_INDEX11, 0);
		ud_dispfrm_show_frmpic(0, PIC_INDEX12, 0);
		ud_dispfrm_show_frmpic(0, PIC_INDEX13, 0);
		ud_dispfrm_show_frmpic(0, PIC_INDEX14, 0);
		ud_dispfrm_show_frmpic(0, PIC_INDEX15, 0);
		ud_dispfrm_show_frmpic(0, PIC_INDEX16, 0);

		ud_dispfrm_show_frmpic(0, PIC_SINDEX1, 0);
		ud_dispfrm_show_frmpic(0, PIC_SINDEX2, 0);
		ud_dispfrm_show_frmpic(0, PIC_SINDEX3, 0);
		ud_dispfrm_show_frmpic(0, PIC_SINDEX4, 0);

		ud_dispfrm_show_frmpic(0, PIC_LINDEX1, 0);
		ud_dispfrm_show_frmpic(0, PIC_LINDEX2, 0);
		ud_dispfrm_show_frmpic(0, PIC_LINDEX3, 0);
		ud_dispfrm_show_frmpic(0, PIC_LINDEX4, 0);

		ud_dispfrm_show_frmpic(0, PIC_OINDEX1, 0);
		ud_dispfrm_show_frmpic(0, PIC_OINDEX2, 0);
		ud_dispfrm_show_frmpic(0, PIC_OINDEX3, 0);
		ud_dispfrm_show_frmpic(0, PIC_OINDEX4, 0);

		if (Tpktype == 0)
		{
/*
.....Entire Part
*/
			if (Tmethod==0)
			{
/*
......advanced
*/
				if (Trevsf==0)
				{
/*
....planar
*/
					ud_dispfrm_show_frmpic(0, PIC_INDEX1, 1);
					ud_dispfrm_show_frmpic(0, PIC_SINDEX1, 1);
					ud_dispfrm_show_frmpic(0, PIC_LINDEX1, 1);
					ud_dispfrm_show_frmpic(0, PIC_OINDEX1, 1);
					S_act_pic = 0;
				}
				else
				{
/*
.....Revolve
*/
					ud_dispfrm_show_frmpic(0, PIC_INDEX2, 1);
					ud_dispfrm_show_frmpic(0, PIC_SINDEX2, 1);
					ud_dispfrm_show_frmpic(0, PIC_LINDEX2, 1);
					ud_dispfrm_show_frmpic(0, PIC_OINDEX2, 1);
					S_act_pic = 1;
				}
			}
			else if (Tmethod==1)
			{
/*
......2 Axis
*/
				ud_dispfrm_show_frmpic(0, PIC_INDEX3, 1);
				ud_dispfrm_show_frmpic(0, PIC_SINDEX3, 1);
				ud_dispfrm_show_frmpic(0, PIC_LINDEX3, 1);
				ud_dispfrm_show_frmpic(0, PIC_OINDEX3, 1);
				S_act_pic = 2;
			}
			else if (Tmethod==2)
			{
/*
......3 Axis
*/
				ud_dispfrm_show_frmpic(0, PIC_INDEX4, 1);
				ud_dispfrm_show_frmpic(0, PIC_SINDEX4, 1);
				ud_dispfrm_show_frmpic(0, PIC_LINDEX4, 1);
				ud_dispfrm_show_frmpic(0, PIC_OINDEX4, 1);
				S_act_pic = 3;
			}
		}
		else if (Tpktype == 1)
		{
/*
......Single Pocket
*/
			if (Tmethod==0)
			{
/*
......advanced
*/
				if (Trevsf==0)
/*
....planar
*/
				{
					ud_dispfrm_show_frmpic(0, PIC_INDEX5, 1);
					ud_dispfrm_show_frmpic(0, PIC_SINDEX1, 1);
					ud_dispfrm_show_frmpic(0, PIC_LINDEX1, 1);
					ud_dispfrm_show_frmpic(0, PIC_OINDEX1, 1);
					S_act_pic = 4;
				}
				else
/*
.....Revolve
*/
				{
					ud_dispfrm_show_frmpic(0, PIC_INDEX6, 1);
					ud_dispfrm_show_frmpic(0, PIC_SINDEX2, 1);
					ud_dispfrm_show_frmpic(0, PIC_LINDEX2, 1);
					ud_dispfrm_show_frmpic(0, PIC_OINDEX2, 1);
					S_act_pic = 5;
				}
			}
			else if (Tmethod==1)
			{
/*
......2 Axis
*/
				ud_dispfrm_show_frmpic(0, PIC_INDEX7, 1);
				ud_dispfrm_show_frmpic(0, PIC_SINDEX3, 1);
				ud_dispfrm_show_frmpic(0, PIC_LINDEX3, 1);
				ud_dispfrm_show_frmpic(0, PIC_OINDEX3, 1);
				S_act_pic = 6;
			}
			else if (Tmethod==2)
			{
/*
......3 Axis
*/
				ud_dispfrm_show_frmpic(0, PIC_INDEX8, 1);
				ud_dispfrm_show_frmpic(0, PIC_SINDEX4, 1);
				ud_dispfrm_show_frmpic(0, PIC_LINDEX4, 1);
				ud_dispfrm_show_frmpic(0, PIC_OINDEX4, 1);
				S_act_pic = 7;
			}
		}
		else if (Tpktype == 2)
		{
/*
.....Inner Pocket
*/
			if (Tmethod==0)
			{
/*
......advanced
*/
				if (Trevsf==0)
/*
....planar
*/
				{
					ud_dispfrm_show_frmpic(0, PIC_INDEX9, 1);
					ud_dispfrm_show_frmpic(0, PIC_SINDEX1, 1);
					ud_dispfrm_show_frmpic(0, PIC_LINDEX1, 1);
					ud_dispfrm_show_frmpic(0, PIC_OINDEX1, 1);
					S_act_pic = 8;
				}
				else
/*
.....Revolve
*/
				{
					ud_dispfrm_show_frmpic(0, PIC_INDEX10, 1);
					ud_dispfrm_show_frmpic(0, PIC_SINDEX2, 1);
					ud_dispfrm_show_frmpic(0, PIC_LINDEX2, 1);
					ud_dispfrm_show_frmpic(0, PIC_OINDEX2, 1);
					S_act_pic = 9;
				}
			}
			else if (Tmethod==1)
			{
/*
......2 Axis
*/
				ud_dispfrm_show_frmpic(0, PIC_INDEX11, 1);
				ud_dispfrm_show_frmpic(0, PIC_SINDEX3, 1);
				ud_dispfrm_show_frmpic(0, PIC_LINDEX3, 1);
				ud_dispfrm_show_frmpic(0, PIC_OINDEX3, 1);
				S_act_pic = 10;
			}
			else if (Tmethod==2)
			{
/*
......3 Axis
*/
				ud_dispfrm_show_frmpic(0, PIC_INDEX12, 1);
				ud_dispfrm_show_frmpic(0, PIC_SINDEX4, 1);
				ud_dispfrm_show_frmpic(0, PIC_LINDEX4, 1);
				ud_dispfrm_show_frmpic(0, PIC_OINDEX4, 1);
				S_act_pic = 11;
			}
		}
		else if (Tpktype == 3)
		{
/*
.....custom
*/
			if (Tmethod==0)
			{
/*
......advanced
*/
				if (Trevsf==0)
/*
....planar
*/
				{
					ud_dispfrm_show_frmpic(0, PIC_INDEX13, 1);
					ud_dispfrm_show_frmpic(0, PIC_SINDEX1, 1);
					ud_dispfrm_show_frmpic(0, PIC_LINDEX1, 1);
					ud_dispfrm_show_frmpic(0, PIC_OINDEX1, 1);
					S_act_pic = 12;
				}
				else
/*
.....Revolve
*/
				{
					ud_dispfrm_show_frmpic(0, PIC_INDEX14, 1);
					ud_dispfrm_show_frmpic(0, PIC_SINDEX2, 1);
					ud_dispfrm_show_frmpic(0, PIC_LINDEX2, 1);
					ud_dispfrm_show_frmpic(0, PIC_OINDEX2, 1);
					S_act_pic = 13;
				}
			}
			else if (Tmethod==1)
			{
/*
......2 Axis
*/
				ud_dispfrm_show_frmpic(0, PIC_INDEX15, 1);
				ud_dispfrm_show_frmpic(0, PIC_SINDEX3, 1);
				ud_dispfrm_show_frmpic(0, PIC_LINDEX3, 1);
				ud_dispfrm_show_frmpic(0, PIC_OINDEX3, 1);
				S_act_pic = 14;
			}
			else if (Tmethod==2)
			{
/*
......3 Axis
*/
				ud_dispfrm_show_frmpic(0, PIC_INDEX16, 1);
				ud_dispfrm_show_frmpic(0, PIC_SINDEX4, 1);
				ud_dispfrm_show_frmpic(0, PIC_LINDEX4, 1);
				ud_dispfrm_show_frmpic(0, PIC_OINDEX4, 1);
				S_act_pic = 15;
			}
		}
	}
done:;
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
	switch(*fieldno)
	{
	case POCKETRG5:
		if (strcmp(Scmd_str[0],Trev_surf) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd_str[0], Trev_surf);
			Sacc[PocketM] = 1;
		}
		break;
	case GEOMRG2:
		if (strcmp(Scmd_str[14], Tlay_num) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd_str[14], Tlay_num);
			Sacc[Geometry] = 1;
		}
		break;
	case STOCKRG4:
		if (strcmp(Scmd_str[1], Tstock_off) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd_str[1], Tstock_off);
			Sacc[Stock] = 1;
		}
		break;
	case STOCKRG9:
		if (strcmp(Scmd_str[15], TBlay_num[0]) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd_str[15], TBlay_num[0]);
			Sacc[Stock] = 1;
		}
		break;
	case STOCKRG16:
		if (strcmp(Scmd_str[16], TBlay_num[1]) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd_str[16], TBlay_num[1]);
			Sacc[Stock] = 1;
		}
		break;
	case LEVELRG3:
		if (strcmp(Scmd_str[2],Tpoc_bot) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd_str[2],Tpoc_bot);
			Sacc[Levels] = 1;
		}
		break;
	case LEVELRG5:
		if (strcmp(Scmd_str[3],Tbot_off) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd_str[3],Tbot_off);
			Sacc[Levels] = 1;
		}
		break;
	case LEVELRG8:
		if (strcmp(Scmd_str[4],Tpoc_top) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd_str[4],Tpoc_top);
			Sacc[Levels] = 1;
		}
		break;
	case LEVELRG11:
		if (strcmp(Scmd_str[12],Tinc_hgt) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd_str[12],Tinc_hgt);
			Sacc[Levels] = 1;
		}
		break;
	case LEVELRG12:
		if (strcmp(Scmd_str[5],Ttop_off) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd_str[5],Ttop_off);
			Sacc[Levels] = 1;
		}
		break;
	case LEVELRG14:
		if (strcmp(Scmd_str[6],Tadj_up) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd_str[6],Tadj_up);
			Sacc[Levels] = 1;
		}
		break;
	case LEVELRG15:
		if (strcmp(Scmd_str[7],Tadj_down) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd_str[7],Tadj_down);
			Sacc[Levels] = 1;
		}
		break;
	case OPTRG3:
		if (strcmp(Scmd_str[8],Tpoc_end) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd_str[8],Tpoc_end);
			Sacc[Options] = 1;
		}
		break;
	case OPTRG5:
		if (strcmp(Scmd_str[9],Tmax_gap) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd_str[9],Tmax_gap);
			Sacc[Options] = 1;
		}
		break;
	case OPTRG7:
		if (strcmp(Scmd_str[10],Tmax_loop) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd_str[10],Tmax_loop);
			Sacc[Options] = 1;
		}
		break;
	case OPTRG9:
		if (strcmp(Scmd_str[11],Tpart_off) != 0)
		{
			if (Spreviewflg) Schgmade = 1;
			strcpy(Scmd_str[11], Tpart_off);
			Sacc[Options] = 1;
		}
		break;
	default:
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
static void S_storvals()
{
	strcpy(Scmd_str[0], Trev_surf);
	strcpy(Scmd_str[1], Tstock_off);
	strcpy(Scmd_str[2], Tpoc_bot);
	strcpy(Scmd_str[3], Tbot_off);
	strcpy(Scmd_str[4], Tpoc_top);
	strcpy(Scmd_str[5], Ttop_off);
	strcpy(Scmd_str[6], Tadj_up);
	strcpy(Scmd_str[7], Tadj_down);
	strcpy(Scmd_str[8], Tpoc_end);
	strcpy(Scmd_str[9], Tmax_gap);
	strcpy(Scmd_str[10], Tmax_loop);
	strcpy(Scmd_str[11], Tpart_off);
	strcpy(Scmd_str[13], Tinc_hgt);
	strcpy(Scmd_str[14], Tlay_num);
	strcpy(Scmd_str[15], TBlay_num[0]);
	strcpy(Scmd_str[16], TBlay_num[1]);

	Smenu_ch[0] = Tpktype;
	Smenu_ch[1] = Tmethod;
	Smenu_ch[2] = Trevsf;

	Smenu_ch[3] = Tstock;
	Smenu_ch[4] = Tstocktype;

	Smenu_ch[5] = Tbottype;
	Smenu_ch[6] = Tbottyp1;
	Smenu_ch[7] = Ttoptype;
	Smenu_ch[8] = Ttoptyp1;
	Smenu_ch[9] = Tfrom;

	Stog_ch[0] = Tlayerdef;
	Stog_ch[1] = Tignore;
	Stog_ch[2] = Tfinis;
	Stog_ch[3] = Tdeep;
	Stog_ch[4] = Tmaxloop;
	Stog_ch[5] = Toffpart;
	Stog_ch[6] = Tstep;
	Stog_ch[7] = Tstksf;
	Stog_ch[8] = Tavdsf;
	Stog_ch[9] = TBlayincl[0];
	Stog_ch[10] = TBlayincl[1];
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
	ud_dispfrm_show_frmpic(0, PIC_INDEX1, 0);
	ud_dispfrm_show_frmpic(0, PIC_INDEX2, 0);
	ud_dispfrm_show_frmpic(0, PIC_INDEX3, 0);
	ud_dispfrm_show_frmpic(0, PIC_INDEX4, 0);
	ud_dispfrm_show_frmpic(0, PIC_INDEX5, 0);
	ud_dispfrm_show_frmpic(0, PIC_INDEX6, 0);
	ud_dispfrm_show_frmpic(0, PIC_INDEX7, 0);
	ud_dispfrm_show_frmpic(0, PIC_INDEX8, 0);
	ud_dispfrm_show_frmpic(0, PIC_INDEX9, 0);
	ud_dispfrm_show_frmpic(0, PIC_INDEX10, 0);
	ud_dispfrm_show_frmpic(0, PIC_INDEX11, 0);
	ud_dispfrm_show_frmpic(0, PIC_INDEX12, 0);
	ud_dispfrm_show_frmpic(0, PIC_INDEX13, 0);
	ud_dispfrm_show_frmpic(0, PIC_INDEX14, 0);
	ud_dispfrm_show_frmpic(0, PIC_INDEX15, 0);
	ud_dispfrm_show_frmpic(0, PIC_INDEX16, 0);

	ud_dispfrm_show_frmpic(0, PIC_SINDEX1, 0);
	ud_dispfrm_show_frmpic(0, PIC_SINDEX2, 0);
	ud_dispfrm_show_frmpic(0, PIC_SINDEX3, 0);
	ud_dispfrm_show_frmpic(0, PIC_SINDEX4, 0);

	ud_dispfrm_show_frmpic(0, PIC_LINDEX1, 0);
	ud_dispfrm_show_frmpic(0, PIC_LINDEX2, 0);
	ud_dispfrm_show_frmpic(0, PIC_LINDEX3, 0);
	ud_dispfrm_show_frmpic(0, PIC_LINDEX4, 0);

	ud_dispfrm_show_frmpic(0, PIC_OINDEX1, 0);
	ud_dispfrm_show_frmpic(0, PIC_OINDEX2, 0);
	ud_dispfrm_show_frmpic(0, PIC_OINDEX3, 0);
	ud_dispfrm_show_frmpic(0, PIC_OINDEX4, 0);

	if (Tpktype == 0)
	{
/*
.....Entire Part
*/
		if (Tmethod==0)
		{
/*
......advanced
*/
			if (Trevsf==0)
			{
/*
....planar
*/
				ud_dispfrm_show_frmpic(0, PIC_INDEX1, 1);
				ud_dispfrm_show_frmpic(0, PIC_SINDEX1, 1);
				ud_dispfrm_show_frmpic(0, PIC_LINDEX1, 1);
				ud_dispfrm_show_frmpic(0, PIC_OINDEX1, 1);
				S_act_pic = 0;
			}
			else
			{
/*
.....Revolve
*/
				ud_dispfrm_show_frmpic(0, PIC_INDEX2, 1);
				ud_dispfrm_show_frmpic(0, PIC_SINDEX2, 1);
				ud_dispfrm_show_frmpic(0, PIC_LINDEX2, 1);
				ud_dispfrm_show_frmpic(0, PIC_OINDEX2, 1);
				S_act_pic = 1;
			}
		}
		else if (Tmethod==1)
		{
/*
......2 Axis
*/
			ud_dispfrm_show_frmpic(0, PIC_INDEX3, 1);
			ud_dispfrm_show_frmpic(0, PIC_SINDEX3, 1);
			ud_dispfrm_show_frmpic(0, PIC_LINDEX3, 1);
			ud_dispfrm_show_frmpic(0, PIC_OINDEX3, 1);
			S_act_pic = 2;
		}
		else if (Tmethod==2)
		{
/*
......3 Axis
*/
			ud_dispfrm_show_frmpic(0, PIC_INDEX4, 1);
			ud_dispfrm_show_frmpic(0, PIC_SINDEX4, 1);
			ud_dispfrm_show_frmpic(0, PIC_LINDEX4, 1);
			ud_dispfrm_show_frmpic(0, PIC_OINDEX4, 1);
			S_act_pic = 3;
		}
	}
	else if (Tpktype == 1)
	{
/*
......Single Pocket
*/
		if (Tmethod==0)
		{
/*
......advanced
*/
			if (Trevsf==0)
/*
....planar
*/
			{
				ud_dispfrm_show_frmpic(0, PIC_INDEX5, 1);
				ud_dispfrm_show_frmpic(0, PIC_SINDEX1, 1);
				ud_dispfrm_show_frmpic(0, PIC_LINDEX1, 1);
				ud_dispfrm_show_frmpic(0, PIC_OINDEX1, 1);
				S_act_pic = 4;
			}
			else
/*
.....Revolve
*/
			{
				ud_dispfrm_show_frmpic(0, PIC_INDEX6, 1);
				ud_dispfrm_show_frmpic(0, PIC_SINDEX2, 1);
				ud_dispfrm_show_frmpic(0, PIC_LINDEX2, 1);
				ud_dispfrm_show_frmpic(0, PIC_OINDEX2, 1);
				S_act_pic = 5;
			}
		}
		else if (Tmethod==1)
		{
/*
......2 Axis
*/
			ud_dispfrm_show_frmpic(0, PIC_INDEX7, 1);
			ud_dispfrm_show_frmpic(0, PIC_SINDEX3, 1);
			ud_dispfrm_show_frmpic(0, PIC_LINDEX3, 1);
			ud_dispfrm_show_frmpic(0, PIC_OINDEX3, 1);
			S_act_pic = 6;
		}
		else if (Tmethod==2)
		{
/*
......3 Axis
*/
			ud_dispfrm_show_frmpic(0, PIC_INDEX8, 1);
			ud_dispfrm_show_frmpic(0, PIC_SINDEX4, 1);
			ud_dispfrm_show_frmpic(0, PIC_LINDEX4, 1);
			ud_dispfrm_show_frmpic(0, PIC_OINDEX4, 1);
			S_act_pic = 7;
		}
	}
	else if (Tpktype == 2)
	{
/*
.....Inner Pocket
*/
		if (Tmethod==0)
		{
/*
......advanced
*/
			if (Trevsf==0)
/*
....planar
*/
			{
				ud_dispfrm_show_frmpic(0, PIC_INDEX9, 1);
				ud_dispfrm_show_frmpic(0, PIC_SINDEX1, 1);
				ud_dispfrm_show_frmpic(0, PIC_LINDEX1, 1);
				ud_dispfrm_show_frmpic(0, PIC_OINDEX1, 1);
				S_act_pic = 8;
			}
			else
/*
.....Revolve
*/
			{
				ud_dispfrm_show_frmpic(0, PIC_INDEX10, 1);
				ud_dispfrm_show_frmpic(0, PIC_SINDEX2, 1);
				ud_dispfrm_show_frmpic(0, PIC_LINDEX2, 1);
				ud_dispfrm_show_frmpic(0, PIC_OINDEX2, 1);
				S_act_pic = 9;
			}
		}
		else if (Tmethod==1)
		{
/*
......2 Axis
*/
			ud_dispfrm_show_frmpic(0, PIC_INDEX11, 1);
			ud_dispfrm_show_frmpic(0, PIC_SINDEX3, 1);
			ud_dispfrm_show_frmpic(0, PIC_LINDEX3, 1);
			ud_dispfrm_show_frmpic(0, PIC_OINDEX3, 1);
			S_act_pic = 10;
		}
		else if (Tmethod==2)
		{
/*
......3 Axis
*/
			ud_dispfrm_show_frmpic(0, PIC_INDEX12, 1);
			ud_dispfrm_show_frmpic(0, PIC_SINDEX4, 1);
			ud_dispfrm_show_frmpic(0, PIC_LINDEX4, 1);
			ud_dispfrm_show_frmpic(0, PIC_OINDEX4, 1);
			S_act_pic = 11;
		}
	}
	else if (Tpktype == 3)
	{
/*
.....custom
*/
		if (Tmethod==0)
		{
/*
......advanced
*/
			if (Trevsf==0)
/*
....planar
*/
			{
				ud_dispfrm_show_frmpic(0, PIC_INDEX13, 1);
				ud_dispfrm_show_frmpic(0, PIC_SINDEX1, 1);
				ud_dispfrm_show_frmpic(0, PIC_LINDEX1, 1);
				ud_dispfrm_show_frmpic(0, PIC_OINDEX1, 1);
				S_act_pic = 12;
			}
			else
/*
.....Revolve
*/
			{
				ud_dispfrm_show_frmpic(0, PIC_INDEX14, 1);
				ud_dispfrm_show_frmpic(0, PIC_SINDEX2, 1);
				ud_dispfrm_show_frmpic(0, PIC_LINDEX2, 1);
				ud_dispfrm_show_frmpic(0, PIC_OINDEX2, 1);
				S_act_pic = 13;
			}
		}
		else if (Tmethod==1)
		{
/*
......2 Axis
*/
			ud_dispfrm_show_frmpic(0, PIC_INDEX15, 1);
			ud_dispfrm_show_frmpic(0, PIC_SINDEX3, 1);
			ud_dispfrm_show_frmpic(0, PIC_LINDEX3, 1);
			ud_dispfrm_show_frmpic(0, PIC_OINDEX3, 1);
			S_act_pic = 14;
		}
		else if (Tmethod==2)
		{
/*
......3 Axis
*/
			ud_dispfrm_show_frmpic(0, PIC_INDEX16, 1);
			ud_dispfrm_show_frmpic(0, PIC_SINDEX4, 1);
			ud_dispfrm_show_frmpic(0, PIC_LINDEX4, 1);
			ud_dispfrm_show_frmpic(0, PIC_OINDEX4, 1);
			S_act_pic = 15;
		}
	}
	ud_dispfrm_set_attribs(0,CLRRG1,UM_BLACK,Tcolor);
	ud_dispfrm_set_attribs(0,CLRRG2,UM_BLACK,Tcolorv);
	ud_dispfrm_set_attribs(0,CLRRG3,UM_BLACK,TBcolor[0]);
	ud_dispfrm_set_attribs(0,CLRRG4,UM_BLACK,TBcolor[1]);
	ud_dispfrm_set_attribs(0,CLRRG5,UM_BLACK,Tcolorb);
	ud_dispfrm_set_attribs(0,CLRRG6,UM_BLACK,Tcolort);
	ud_dispfrm_set_attribs(0,CLRRG7,UM_BLACK,Tcolore);
	ud_dispfrm_set_attribs(0,CLRRG8,UM_BLACK,Tcolorh);
	ud_dispfrm_set_attribs(0,CLRRG9,UM_BLACK,Tzcolor);
	ud_dispfrm_set_attribs(0,CLRRG11,UM_BLACK,Tgeocol);
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
	int i,lbtyp,lttyp;

	display[LABELS+POCKETRG6] = 0;

	traverse[POCKETRG2] = NVMILL;
	traverse[LEVELRG10] = traverse[LEVELRG11] = UU_FALSE;
	if (Tpktype == 0)
	{
		Tstock = 1;
		traverse[STOCKRG1] = 0;
		display[LABELS+STOCKRG2] = 0;
		display[LABELS+STOCKRG3] = 1;
	}
	else if (Tpktype == 1)
	{
		Tstock = 0;
		traverse[STOCKRG1] = 0;
		display[LABELS+STOCKRG2] = 1;
		display[LABELS+STOCKRG3] = 0;
	}
	else if (Tpktype == 2)
	{
		Tstock = 0;
		traverse[STOCKRG1] = 0;
		display[LABELS+STOCKRG2] = 1;
		display[LABELS+STOCKRG3] = 0;
		Tignore = 1;
	}
	display[LABELS+LEVELRG13] = 1;
	if (Tmethod > 0)
	{
		Trevsf = 0; traverse[POCKETRG3] = traverse[POCKETRG4] = traverse[POCKETRG5] = 0;
		display[LABELS+POCKETRG3] = display[LABELS+POCKETRG4] = display[LABELS+POCKETRG5] = 0;
		traverse[OPTRG6] = traverse[OPTRG7] = 0;
		traverse[OPTRG8] = traverse[OPTRG9] = 0;
		display[LABELS+OPTRG6] = display[LABELS+OPTRG7] = 0;
		display[LABELS+OPTRG8] = display[LABELS+OPTRG9] = 0;
		if (Tmethod > 1) 
			traverse[LEVELRG10] = 1;
		traverse[OPTRG10] = display[LABELS+OPTRG10] = 1;
		traverse[OPTRG11] = display[LABELS+OPTRG11] = 1;
		traverse[CLRRG8] = 1;
		if (Tmethod > 1) 
		{
			display[LABELS+LEVELRG10] = 1;
			display[LABELS+LEVELRG11] = 1;
			display[LABELS+LEVELRG14] = 0;
			display[LABELS+LEVELRG15] = 0;
		}
		else
		{
			display[LABELS+LEVELRG10] = 0;
			display[LABELS+LEVELRG11] = 0;
			display[LABELS+LEVELRG14] = 1;
			display[LABELS+LEVELRG15] = 1;
		}
	}
	else
	{
		traverse[POCKETRG3] = 1;
		display[LABELS+POCKETRG3] = display[LABELS+POCKETRG4] = display[LABELS+POCKETRG5] = 1;
		traverse[OPTRG6] = traverse[OPTRG7] = 1;
		traverse[OPTRG8] = traverse[OPTRG9] = 1;
		traverse[OPTRG10] = display[LABELS+OPTRG10] = 0;
		traverse[OPTRG11] = display[LABELS+OPTRG11] = 0;
		traverse[CLRRG8] = 0;
		display[LABELS+LEVELRG10] = 0;
		display[LABELS+LEVELRG11] = 0;
	}
	if (Tlayerdef == UU_TRUE)
	{
		traverse[GEOMRG5] = traverse[GEOMRG6] = UU_TRUE;
		traverse[CLRRG1] = UU_TRUE;
	}
	else
	{
		traverse[GEOMRG5] = traverse[GEOMRG6] = UU_FALSE;
		traverse[CLRRG1] = UU_FALSE;
	}
	if (Trevsf == 1)
	{
		traverse[POCKETRG4] = traverse[POCKETRG5] = traverse[CLRRG2] = UU_TRUE;
		traverse[LEVELRG2] = traverse[LEVELRG7] = display[LABELS+LEVELRG2] = display[LABELS+LEVELRG7] = UU_TRUE;
		traverse[LEVELRG1] = traverse[LEVELRG6] = display[LABELS+LEVELRG1] = display[LABELS+LEVELRG6] = UU_FALSE;
		traverse[LEVELRG13] = UU_FALSE;
		display[LABELS+LEVELRG13] = display[LABELS+LEVELRG14] = display[LABELS+LEVELRG15] = 0;

		lbtyp = STOCK;
		if (Tbottyp1 == DIST1)
			lbtyp = DIST;
		else if (Tbottyp1 == ZLEV1)
			lbtyp = ZLEV;

		lttyp = DIST;
		if (Ttoptyp1 == STOCK)
			lttyp = STOCK;
		else if (Ttoptyp1 == ZLEV1)
			lttyp = ZLEV;
	}
	else
	{
		traverse[POCKETRG4] = traverse[POCKETRG5] = traverse[CLRRG2] = UU_FALSE;
		traverse[LEVELRG2] = traverse[LEVELRG7] = display[LABELS+LEVELRG2] = display[LABELS+LEVELRG7] = UU_FALSE;
		traverse[LEVELRG1] = traverse[LEVELRG6] = display[LABELS+LEVELRG1] = display[LABELS+LEVELRG6] = UU_TRUE;
		traverse[LEVELRG13] = UU_TRUE;
		lbtyp = Tbottype;
		lttyp = Ttoptype;
	}
	display[LABELS+STOCKRG2] = traverse[STOCKRG2] = traverse[STOCKRG5] = 1 - Tstock;
	display[LABELS+STOCKRG3] = traverse[STOCKRG3] = Tstock;
	traverse[STOCKRG5] = 1 - Tstock;
	traverse[STOCKRG11] = 0;
	if ((traverse[STOCKRG5]) && (Tstksf))
	{
		traverse[STOCKRG6] = 1;
		traverse[STOCKRG7] = 1;
		traverse[CLRRG3] = 1;
		if (TBlayincl[0]) 
		{
			traverse[STOCKRG8] = 1;
			traverse[STOCKRG9] = 1;
			traverse[STOCKRG10] = 1;
		}
		else 
		{
			traverse[STOCKRG8] = 0;
			traverse[STOCKRG9] = 0;
			traverse[STOCKRG10] = 0;
		}
	}
	else 
	{
		traverse[CLRRG3] = 0;
		traverse[STOCKRG6] = 0;
		traverse[STOCKRG7] = 0;
		traverse[STOCKRG8] = 0;
		traverse[STOCKRG9] = 0;
		traverse[STOCKRG10] = 0;
		traverse[CLRRG3] = 0;
	}
	traverse[STOCKRG18] = 0;
	if (Tavdsf) 
	{
		traverse[CLRRG2] = 1;
		traverse[STOCKRG13] = 1;
		traverse[STOCKRG14] = 1;
		if (TBlayincl[1]) 
		{
			traverse[STOCKRG15] = 1;
			traverse[STOCKRG16] = 1;
			traverse[STOCKRG17] = 1;
		}
		else 
		{
			traverse[STOCKRG15] = 0;
			traverse[STOCKRG16] = 0;
			traverse[STOCKRG17] = 0;
		}
	}
	else 
	{
		traverse[CLRRG2] = 0;
		traverse[STOCKRG13] = 0;
		traverse[STOCKRG14] = 0;
		traverse[STOCKRG15] = 0;
		traverse[STOCKRG16] = 0;
		traverse[STOCKRG17] = 0;
	}
	if (lbtyp == STOCK)
	{
		traverse[LEVELRG3] = traverse[LEVELRG4] = traverse[CLRRG5] = UU_FALSE;
		traverse[LEVELRG5] = UU_TRUE;
	}
	else if (lbtyp == PLANE)
	{
		traverse[LEVELRG3] = traverse[LEVELRG4] = traverse[CLRRG5] = traverse[LEVELRG5] =
			UU_TRUE;
	}
	else
	{
		traverse[LEVELRG3] = UU_TRUE;
		traverse[LEVELRG5] = traverse[LEVELRG4] = traverse[CLRRG5] = UU_FALSE;
	}
	if (lttyp == PLANE)
	{
		traverse[LEVELRG8] = traverse[LEVELRG9] = traverse[CLRRG6] = traverse[LEVELRG12] =
			UU_TRUE;
	}
	else if (lttyp == STOCK)
	{
		traverse[LEVELRG9] = traverse[CLRRG6] = traverse[LEVELRG8] = UU_FALSE;
		traverse[LEVELRG12] = UU_TRUE;
	}
	else
	{
		traverse[LEVELRG8] = UU_TRUE;
		traverse[LEVELRG9] = traverse[CLRRG6] = traverse[LEVELRG12] = UU_FALSE;
	}
	traverse[LEVELRG14] = traverse[LEVELRG15] = Tfinis;

	if (Tmaxloop == UU_TRUE)
		traverse[OPTRG7] = UU_TRUE;
	else
		traverse[OPTRG7] = UU_FALSE;

	if (Tfrom == NEARPT)
		traverse[OPTRG3] = traverse[OPTRG4] = traverse[CLRRG7] = UU_TRUE;
	else
		traverse[OPTRG3] = traverse[OPTRG4] = traverse[CLRRG7] = UU_FALSE;

	if (Tstep == UU_TRUE)
		traverse[LEVELRG11] = UU_TRUE;
	else
		traverse[LEVELRG11] = UU_FALSE;
/*
.....Action item fields
*/
	for (i=FAPV;i<FAVW;i++) traverse[i] = 0; 
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
	int i;
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
	S_add_key(which, Sgrev.key);
	S_add_key(which, Sgbot.key);
	S_add_key(which, Sgtop.key);
	S_add_key(which, Sgstpt.key);

	Snholes = UU_LIST_LENGTH(&Sholes);
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sholes);
	for (i=0;i<Snholes;i++) S_add_key(which,mgeo[i].key);

	nzones = UU_LIST_LENGTH(&Zcurves);
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(&Zcurves);
	for (i=0;i<nzones;i++) S_add_key(which,mgeo[i].key);

	Bsfnum[0] = UU_LIST_LENGTH(&Bsurf[0]);
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(&Bsurf[0]);
	for (i=0;i<Bsfnum[0];i++) S_add_key(which,mgeo[i].key);

	Bsfnum[1] = UU_LIST_LENGTH(&Bsurf[1]);
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(&Bsurf[1]);
	for (i=0;i<Bsfnum[1];i++) S_add_key(which,mgeo[i].key);

	nsurf = UU_LIST_LENGTH(&Ssurf);
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(&Ssurf);
	for (i=0;i<nsurf;i++) S_add_key(which,mgeo[i].key);
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
**					3: multiple selection, not erase the old selection, but add into it.
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
		if (sflst != UU_NULL) S_unhilite_entities(sflst, geo_init);
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
			S_unhilite_entities(sflst, geo_init);
		}
/*
........Store the entities' data
*/
		if(sflst->data==UU_NULL)
			uu_list_init(sflst,sizeof(UM_sgeo),numint,10);
		*geo_init = UU_TRUE;
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
static void S_unhilite_entities(sflst, geo_init)
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
	int fno;
	UD_DDATA val;
	UD_FSTAT stat = UD_FLDOK;

	S_unhilite_entity(&Sgrev);
	S_unhilite_entity(&Sgbot);
	S_unhilite_entity(&Sgtop);
	S_unhilite_entity(&Sgstpt);

	fno = GEOMRG6;
	OnDeleteAll(&fno, &val, stat);
	fno = STOCKRG11;
	OnDeleteAll(&fno, &val, stat);
	fno = STOCKRG18;
	OnDeleteAll(&fno, &val, stat);
	fno = ZONERG2;
	OnDeleteAll(&fno, &val, stat);
	fno = OPTRG11;
	OnDeleteAll(&fno, &val, stat);
/*
.....Initialize geomtry variables
*/
	Sgrev.key = Sgsf.key = Sbsrf1.key = Sbsrf2.key = Sgbot.key = 0;
	Sgtop.key = Sgstpt.key = Sghol.key = Sgzcv.key = 0;
	Sgrev.color = Sgsf.color = Sbsrf1.color = Sbsrf2.color = Sgbot.color = -1;
	Sgtop.color = Sgstpt.color = Sghol.color = Sgzcv.color = -1;
	Sgrev.label[0] = Sgsf.label[0] = Sbsrf1.label[0] = Sbsrf2.label[0] = Sgbot.label[0] = '\0';
	Sgtop.label[0] = Sgstpt.label[0] = Sghol.label[0] = Sgzcv.label[0] = '\0';

	Trev_surf[0] = '\0';
	ud_update_answer(POCKETRG5,(int *)Trev_surf);
	if ((Trevsf==0)&&(Tbottype == PLANE))
	{
		Tpoc_bot[0] = '\0';
		ud_update_answer(LEVELRG3,(int *)Tpoc_bot);
	}
	if (Tfrom == NEARPT)
	{
		Tpoc_end[0] = '\0';
		ud_update_answer(OPTRG3,(int *)Tpoc_end);
	}
	if ((Trevsf==0)&&(Ttoptype == PLANE))
	{
		Tpoc_top[0] = '\0';
		ud_update_answer(LEVELRG8,(int *)Tpoc_top);
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
	S_unhilite_entity(&Sgrev);
	S_unhilite_entity(&Sgbot);
	S_unhilite_entity(&Sgtop);
	S_unhilite_entity(&Sgstpt);

	S_unhilite_entities(&Sholes, &(Sgeo_init[3]));
	Snholes = 0;
	S_unhilite_entities(&Zcurves, &(Sgeo_init[4]));
	nzones = 0;
	S_unhilite_entities(&(Bsurf[0]), &(Sgeo_init[1]));
	Bsfnum[0] = 0;
	S_unhilite_entities(&(Bsurf[1]), &(Sgeo_init[2]));
	Bsfnum[1] = 0;
	S_unhilite_entities(&Ssurf, &(Sgeo_init[0]));
	nsurf = 0;
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
	Scolor = Tcolor;
	Slayerdef = Tlayerdef;
	Sstock = Tstock;
	Signore = Tignore;
	Sstocktype = Tstocktype;
	Sbottype = Tbottype;
	Slayer = Tlayer;
	Slistlayer = Tlistlayer;
	Stoptype = Ttoptype;
	Smaxloop = Tmaxloop;
	Soffpart = Toffpart;
	Sconpoc = Tconpoc;
	Sfrom = Tfrom;
	Szone = Tzone;
	Sdeep = Tdeep;
	Scolorb = Tcolorb;
	Stoptype = Ttoptype;
	Scolort = Tcolort;
	Scolore= Tcolore;
	Srevsf = Trevsf;
	Scolorv = Tcolorv;
	Sbottyp1 = Tbottyp1;
	Stoptyp1 = Ttoptyp1;
	Sfinis = Tfinis;
	Smethod = Tmethod;
	Sstep = Tstep;
	Spktype = Tpktype;
	Sstksf = Tstksf;
	Savdsf = Tavdsf;
	Szcolor = Tzcolor;
	Szlast = Tzlast;
	Sztype = Tztype;
	Scolorh = Tcolorh;
	SBcolor[0] = TBcolor[0];
	SBcolor[1] = TBcolor[1];
	SBaddlayer[0] = TBaddlayer[0];
	SBaddlayer[1] = TBaddlayer[1];
	SBlayincl[0] = TBlayincl[0];
	SBlayincl[1] = TBlayincl[1];

	UN_unused_geo_flag = Tgeotog;
	UN_unused_geo_color = Tgeocol;

	strcpy(Spoc_bot, Tpoc_bot);
	strcpy(Spoc_top, Tpoc_top);
	strcpy(Spoc_end, Tpoc_end);
	strcpy(Srev_surf, Trev_surf);
	strcpy(Sstat_answer, Tstat_answer);
	strcpy(Slay_num, Tlay_num);
	strcpy(Smax_gap, Tmax_gap);
	strcpy(Sstock_off, Tstock_off);
	strcpy(Sbot_off, Tbot_off);
	strcpy(Stop_off, Ttop_off);
	strcpy(Smax_loop, Tmax_loop);
	strcpy(Spart_off, Tpart_off);
	strcpy(SBlay_num[0], TBlay_num[0]);
	strcpy(SBlay_num[1], TBlay_num[1]);
	strcpy(Sadj_up, Tadj_up);
	strcpy(Sadj_down, Tadj_down);
	strcpy(Sinc_hgt, Tinc_hgt);
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
	int i;
	UU_REAL rnum;
	static UU_REAL ZERO = 0.;
	UM_int2 ifl;
	UM_real8 tdia,tol;
	UM_real4 botp,topp,stkp,gap,poff;
	UM_real4 dup,ddn,step;
	UM_int2 wclik,layn,btyp,ttyp,stock,ltyp,lops,from,lofp,ztyp,zlst,deep,fins;

	if (!Swatfrm_init)
	{
		Sstksf = 1;
		Tavdsf = 1;
		nclu_pokpar (&Smodals);
		nclu_vmpmod_parm (&Svmodals);
		Slayer = 1;
		for (i = 0; i < 2; i++)
		{
			Bsfnum[i] = 0;
			Blayer[i] = 999;
			sprintf(SBlay_num[i],"%d",Blayer[i]);
			SBaddlayer[i] = SBlayincl[i] = UU_FALSE;
			Blistlayer[i] = -1;
		}
		SBcolor[0] = 13; 
		SBcolor[1] = 14;
		Swatfrm_init = UU_TRUE;
		Slayerdef = UU_TRUE;
		Scolor = 8; Scolorv = 9; Scolorb = 10; Scolort = 11; Scolore = 12;
		Scolorh = 14;
		Sconpoc = POCKET;
	}
	watfrm (&wclik,&layn,&btyp,&ttyp,&stock,&ltyp,&lops,&botp,&topp,&stkp,&gap,
		&from,&lofp,&poff,&ztyp,&zlst,&deep,&step);
	wgtfinis (&fins,&dup,&ddn);
	Soffpart = Szone = Sdeep = Sfinis = Sstep = UU_FALSE;
	Szlast = 0;
	Sztype = PART;
	Szcolor = 9;

	rnum = 0.;
	ncl_sprintf (Sadj_up,&rnum,1);
	ncl_sprintf (Sadj_down,&rnum,1);
	ncl_sprintf (Sinc_hgt,&rnum,1);

	if (wclik)
	{
		if (layn >= 0 && layn < 4097)
		{
			Slayerdef = UU_FALSE; Slayer = layn;
			sprintf(Slay_num,"%d",Slayer);
		}
		Stoptype = ttyp;
		Sbottype = btyp;
		rnum = botp;
		if (btyp < 2)
		{
			ncl_sprintf (Sbot_off,&rnum,1);
			ncl_sprintf (Spoc_bot,&ZERO,1);
		}
		else
		{
			ncl_sprintf (Spoc_bot,&rnum,1);
			ncl_sprintf (Sbot_off,&ZERO,1);
		}
		rnum = topp;
		if (ttyp < 2)
		{
			ncl_sprintf (Stop_off,&rnum,1);
			ncl_sprintf (Spoc_top,&ZERO,1);
		}
		else
		{
			ncl_sprintf (Spoc_top,&rnum,1);
			ncl_sprintf (Stop_off,&ZERO,1);
		}

		Sstock = Signore = UU_FALSE; Sstocktype = CONTOUR;
		if (stock == 1)
			Sstock = UU_TRUE;
		else if (stock == 2)
		{
			Sstock = UU_TRUE; Sstocktype = BOX;
		}
		else if (stock == 3)
			Signore = UU_TRUE;
		rnum = stkp;
		ncl_sprintf (Sstock_off,&rnum,1);
		rnum = poff;
		ncl_sprintf (Spart_off,&rnum,1);
		rnum = gap;
		ncl_sprintf (Smax_gap,&rnum,1);
		Smaxloop = ltyp;
		i = lops; sprintf (Smax_loop,"%d",i);
		Sfrom = from;
		if (Sfrom >= 200) Sfrom = Sfrom - 200;
		if (Sfrom >= 100) Sfrom = Sfrom - 100;
		if (zlst == 1) Szlast = 1;
		if (ztyp == 0)
			Sztype = ALL;
		else if (ztyp == 2)
			Sztype = MAIN;
		if (fins == 1)
		{
			Sfinis = UU_TRUE;
			rnum = dup;
			ncl_sprintf (Sadj_up,&rnum,1);
			rnum = ddn;
			ncl_sprintf (Sadj_down,&rnum,1);
		}
		if (deep == 1) Sdeep = UU_TRUE;
		if (lofp == 1) Soffpart = UU_TRUE;
		rnum = step;
		if (fabs(rnum) > UM_FUZZ) Sstep = UU_TRUE;
		ncl_sprintf (Sinc_hgt,&rnum,1);
	}
	else
	{
		Slistlayer = -1;
		sprintf(Slay_num,"%d",Slayer);
		Sstocktype = CONTOUR;
		getsct (&tol);
		ifl = 28;
		getsc (&ifl,&tdia);
		rnum = (tdia < 4.*tol)? 2.*tol: 0.5*tdia;
		ncl_sprintf (Sstock_off,&rnum,1);
		ncl_sprintf (Sbot_off,&ZERO,1);
		ncl_sprintf (Stop_off,&ZERO,1);
		ncl_sprintf (Spoc_bot,&ZERO,1);
		ncl_sprintf (Spoc_top,&ZERO,1);
		ncl_sprintf (Spart_off,&rnum,1);
		Stoptype = DIST;
		Sbottype = STOCK;
		rnum = 32.*tol;
		ncl_sprintf (Smax_gap,&rnum,1);
		Sstock = Signore = Smaxloop = UU_FALSE;
		strcpy(Smax_loop, "0");
		Sfrom = 0; Spoc_end[0] = '\0';
	}
	Srevsf = 0;
	Srev_surf[0] = '\0';
	Sbottyp1 = STOCK;
	Stoptyp1 = DIST1;

	Tcolor = Scolor;
	Tlayerdef = Slayerdef;
	Tstock = Sstock;
	Tignore = Signore;
	Tstocktype = Sstocktype;
	Tbottype = Sbottype;
	Tlayer = Slayer;
	Tlistlayer = Slistlayer;
	Ttoptype = Stoptype;
	Tmaxloop = Smaxloop;
	Toffpart = Soffpart;
	Tconpoc = Sconpoc;
	Tfrom = Sfrom;
	Tzone = Szone;
	Tdeep = Sdeep;
	Tcolorb = Scolorb;
	Ttoptype = Stoptype;
	Tcolort = Scolort;
	Tcolore= Scolore;
	Trevsf = Srevsf;
	Tcolorv = Scolorv;
	Tbottyp1 = Sbottyp1;
	Ttoptyp1 = Stoptyp1;
	Tfinis = Sfinis;
	Tmethod = Smethod;
	Tstep = Sstep;
	Tpktype = Spktype;
	Tstksf = Sstksf;
	Tavdsf = Savdsf;
	Tzcolor = Szcolor;
	Tzlast = Szlast;
	Tztype = Sztype;
	Tcolorh = Scolorh;
	TBcolor[0] = SBcolor[0];
	TBcolor[1] = SBcolor[1];
	TBaddlayer[0] = SBaddlayer[0];
	TBaddlayer[1] = SBaddlayer[1];
	TBlayincl[0] = SBlayincl[0];
	TBlayincl[1] = SBlayincl[1];
	Tgeotog = UN_unused_geo_flag;
	Tgeocol = UN_unused_geo_color;

	strcpy(Tpoc_bot, Spoc_bot);
	strcpy(Tpoc_top, Spoc_top);
	strcpy(Tpoc_end, Spoc_end);
	strcpy(Trev_surf, Srev_surf);
	strcpy(Tstat_answer, Sstat_answer);
	strcpy(Tlay_num, Slay_num);
	strcpy(Tmax_gap, Smax_gap);
	strcpy(Tstock_off, Sstock_off);
	strcpy(Tbot_off, Sbot_off);
	strcpy(Ttop_off, Stop_off);
	strcpy(Tmax_loop, Smax_loop);
	strcpy(Tpart_off, Spart_off);
	strcpy(TBlay_num[0], SBlay_num[0]);
	strcpy(TBlay_num[1], SBlay_num[1]);
	strcpy(Tadj_up, Sadj_up);
	strcpy(Tadj_down, Sadj_down);
	strcpy(Tinc_hgt, Sinc_hgt);
}
/*********************************************************************
**    I_FUNCTION     : OnZDel(fieldno, val, stat)
**			Deselects all surfaces from the list.
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
static UD_FSTAT OnZDel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UM_sgeo *geo;
	int i,j;
/*
.....Remove all entities in this list
*/
	geo = (UM_sgeo *) UU_LIST_ARRAY(&Zcurves);
	j = zcurnam;
	if (j >= 0 && j < nzones)
	{
		nclu_repaint (&geo[j],1,-1);

		uu_list_delete (&Zcurves,j,1);
		nzones--;

		Z_empty_flist(&stat_list,&strings);

		if (nzones > 0)
		{
			geo = (UM_sgeo *) UU_LIST_ARRAY (&Zcurves);
			for (i = 0; i < nzones; i++)
			{
				Z_put_list(&stat_list,&strings,geo[i].label);
			}
		}
		else
			strcpy(stat_list.answer," ");

		ud_update_answer(ZONERG3,(int *)&stat_list);
	}
	if (*fieldno!=-1)
	{
		Sacc[Zones] = 1;
		S_enable_buttons();
/*
.....Change made since preview
*/
		if (Spreviewflg) Schgmade = 1;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnZMovUp(fieldno, val, stat)
**			Deselects all surfaces from the list.
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
static UD_FSTAT OnZMovUp(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UM_sgeo *geo;
	int i,j;
	char *tmp,**s;
/*
.....Remove all entities in this list
*/
	geo = (UM_sgeo *) UU_LIST_ARRAY(&Zcurves);
	j = zcurnam;
	if (j >= 1 && j < nzones)
	{
		i = j-1;
		nclu_swap_sgeo (&Zcurves,i,j);

		s = (char **) UU_LIST_ARRAY(&strings);
		tmp = s[i];
		s[i] = s[j];
		s[j] = tmp;

		stat_list.item = (char **) UU_LIST_ARRAY(&strings);

		strcpy(stat_list.answer,geo[i].label);

		ud_update_answer(ZONERG3,(int *)&stat_list);
	}
	Sacc[Zones] = 1;
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnZMovDown(fieldno, val, stat)
**			Deselects all surfaces from the list.
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
static UD_FSTAT OnZMovDown(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UM_sgeo *geo;
	int i,j;
	char *tmp,**s;
/*
.....Remove all entities in this list
*/
	geo = (UM_sgeo *) UU_LIST_ARRAY(&Zcurves);
	j = zcurnam;
	if (j >= 0 && j < nzones-1)
	{
		i = j+1;
		nclu_swap_sgeo (&Zcurves,i,j);

		s = (char **) UU_LIST_ARRAY(&strings);
		tmp = s[i];
		s[i] = s[j];
		s[j] = tmp;

		stat_list.item = (char **) UU_LIST_ARRAY(&strings);

		strcpy(stat_list.answer,geo[i].label);
		ud_update_answer(ZONERG3,(int *)&stat_list);
	}
	Sacc[Zones] = 1;
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnPicTog(fieldno,val,stat)
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
static UD_FSTAT OnPicTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch (*fieldno)
	{
	case 0:
		ud_close_dispfrm(Sfrmpic);
		Sfrmpic = -1;
		Tpktype = Spic_ans;
		ud_update_answer(POCKETRG1, (int *)&Tpktype);
/*
.....Update the form
*/
		ud_update_form(0);
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
static UD_FSTAT OnClosePic()
{
	Sfrmpic = -1;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_initpock_form()
**       Method called when the pocket Form is first displayed.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_initpock_form()
{
	ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX1, 0);
	ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX2, 0);
	ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX3, 0);
	ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX4, 0);
	ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX5, 0);
	ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX6, 0);
	ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX7, 0);
	ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX8, 0);
	ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX9, 0);
	ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX10, 0);
	ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX11, 0);
	ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX12, 0);
	ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX13, 0);
	ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX14, 0);
	ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX15, 0);
	ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX16, 0);
		
	if (Tmethod==0)
	{
/*
......advanced
*/
		if (Trevsf==0)
		{
/*
....planar
*/
			ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX1, 1);
			ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX5, 1);
			ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX9, 1);
			ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX13, 1);
		}
		else
		{
/*
.....Revolve
*/
			ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX2, 1);
			ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX6, 1);
			ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX10, 1);
			ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX14, 1);
		}
	}
	else if (Tmethod==1)
	{
/*
......2 Axis
*/
		ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX3, 1);
		ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX7, 1);
		ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX11, 1);
		ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX15, 1);
	}
	else if (Tmethod==2)
	{
/*
......3 Axis
*/
		ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX4, 1);
		ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX8, 1);
		ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX12, 1);
		ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX16, 1);
	}
	else if (Tmethod==3)
	{
/*
......5 Axis
*/
		ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX4, 1);
		ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX8, 1);
		ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX12, 1);
		ud_dispfrm_show_frmpic(Sfrmpic, PIC_INDEX16, 1);
	}				 
	ud_update_form(Sfrmpic);
}
/*********************************************************************
**    I_FUNCTION     :  OnPicSel(fieldno, val, stat)
**       Callback routine for picture selection.
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
static UD_FSTAT OnPicSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	static char traverse[]={0};
	static char display[]={0};
	static char called[]={6};
	static UD_METHOD methods[]={OnPicTog,OnClosePic};
	static int *ans[]={&Spic_ans};

	if (Sfrmpic != -1) 
		return(UD_FLDOK);
/*
.....different picture can call different method
*/
/*
	if (S_act_pic==0)
	{
		methods[0] = OnPicTog1;
	}
	else if if (S_act_pic==1)
	{
		methods[0] = OnPicTog2;
	}
*/
	Sfrmpic = ud_form_display1("wpockett.frm",ans,ans,methods,called,display,
			traverse);
	S_initpock_form();
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : nclu_waterline_cut()
**       Controlling routine for the waterline roughing form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_waterline_cut()
{
	int flag, status;
	UU_LOGICAL cmdreject;
	UD_METHOD save_entry;
/*
.....Set up form fields
*/
	static char traverse[] = {
		1,1,1,1,1,1,
		1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1, 
		1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1
	};
	static char display[] = {
		1,
		1,1,1,1,1,1,
		1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1, 
		1,1,1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1
	};
	static UD_METHOD methods[] = {
/*
.....Pocket Motion
*/
		OnChcSel, OnChcSel, OnChcSel, OnSelect, OnEditTxt, OnPicSel,
/*
.....Geometry
*/
		OnSelect, OnEditTxt, OnSelect, Onchk, OnSelect, OnSelect, 
/*
.....STOCK
*/
		OnChcSel, Onchk, OnChcSel, OnEditTxt, 
		Onchk, OnSelect, Onchk, OnSelect, OnEditTxt, OnSelect, OnSelect, 
		Onchk, OnSelect, Onchk, OnSelect, OnEditTxt, OnSelect, OnSelect, 
/*
.....Level
*/
		OnChcSel, OnChcSel, OnEditTxt, OnSelect,
		OnEditTxt, 
		OnChcSel, OnChcSel, OnEditTxt, OnSelect, Onchk, OnEditTxt,
		OnEditTxt, 
		Onchk, OnEditTxt, OnEditTxt, 
/*
.....Options
*/
		Onchk, 
		OnChcSel, OnEditTxt, OnSelect,
		OnEditTxt, Onchk, OnEditTxt, 
		Onchk, OnEditTxt, OnSelect, OnSelect,
/*
.....Zones
*/
		OnSelect, OnSelect,
		OnZoneList,
		OnZDel,OnZMovUp,OnZMovDown,
		OnChcSel, 
		Onchk,
/*
.....Colors
*/
		OnColors, OnColors, OnColors, OnColors,
		OnColors, OnColors, OnColors, OnColors,
		OnColors, OnColors, OnColors, 
/*
......Pocket Modals
*/
		OnModals,
/*
.....Action Buttons
*/
		OnAction,OnAction,OnAction,
		OnAction,OnAction,OnAction,OnAction,OnVideo
	};
	static char called[] = {
		6,6,6,6,6,6,
		6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6,6, 6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6,6, 6,6,6,6,6,
		6,6,6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6, 
		6,6,6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6
	}
	uu_denter(UU_MTRC,(us,"nclu_waterline_cut()"));
/*
.....Initialize routine
*/
	Spokmod_saved = UU_FALSE;
	Sclick_pokmod = UU_FALSE;
	Sclick_vmpmod = UU_FALSE;

	Snholes = 0; Shole_init = 0;
	uu_list_init(&Ssurf,sizeof(UM_sgeo),50,50);
	nsurf = 0;
	uu_list_init(&Bsurf[1],sizeof(UM_sgeo),0,10);
	uu_list_init(&Bsurf[0],sizeof(UM_sgeo),0,10);
	Zcurves.cur_cnt = 0;
	Zcurves.data = UU_NULL;
	uu_list_init(&Zcurves,sizeof(UM_sgeo),0,10);
	Z_init_flist(&stat_list,&strings,0,10);
	stat_list.answer = Sstat_answer;
	strcpy(stat_list.answer," ");
/*
.....Initialize form answers
*/
	S_init_form();
/*
.....Store values to check for changes
*/
	S_storvals();
	Serrflg = 0;
	Spreviewflg = 0;
	Schgmade = 0;
	save_entry = UD_initfrm_intry;
	SfrmPlay = -1;
	Sfrmpic = -1;
	Sacc[PocketM] = Sacc[Geometry] = Sacc[Stock] = Sacc[Levels] = 0;
	Sacc[Options] = Sacc[Zones] = Sacc[Colors] = 0;
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0)	goto fini;	
repeat:
/*
.....Get the Form input
*/
	S_init_traverse(display, traverse);
	UD_initfrm_intry = S_enter_form;
	status = ud_form1("waterline.frm",Sanswers,Sanswers,methods,called,display,traverse);
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
*//*
.....Free the motion settings created during preview if a change has been
.....made and ok was pressed or if cancel was pressed
*/
	if (Spreviewflg)
	{
		flag = UU_FALSE;
		if (!Schgmade && status != -1 && Serrflg == 0) flag = UU_TRUE;
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
	if (status == -1) goto done;
	if (Stoptype == Sbottype && Sbottype == DEPTH)
	{
		ud_wrerr("Bottom or Top Undefined.");
		goto repeat;
	}
/*	if (Slayerdef == UU_TRUE && nsurf == 0) goto repeat; */
	
/*
.....Save the form settings
*/
	S_save_form();
	S_build_command(UU_TRUE);

done:
	if (Sgeo_redraw)
		ncl_hide_geo(-1,-1,-1,-1,UU_TRUE,UU_NULL,0);
	if (Skey_init)
		uu_list_free(&Skey_list);
	Skey_init = UU_FALSE;
	S_unhilite_all();

	S_close_sflist(LIST_A);
	S_close_sflist(LIST_B);
	S_close_sflist(LIST_C);

	Z_free_flist(&stat_list,&strings);
fini:;
	UD_UNMARK(cmdreject); 
	return;
}

