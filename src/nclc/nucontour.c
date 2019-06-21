/********************************************************************* 
**    NAME         :  nucontour.c
**       CONTAINS:
**		             nclu_contour()
**
**    COPYRIGHT 2015 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nucontour.c , 25.5
**    DATE AND TIME OF LAST MODIFICATION
**       10/27/16 , 13:14:00
*********************************************************************/
#include <string.h>
#include "class.h"
#include "dselmask.h"
#include "nclmplay.h"
#include "mdrel.h"
#include "mgeom.h"
#include "mdcpln.h"
#include "mdpick.h"
#include "mxxx.h"
#include "mxxx.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclx.h"
#include "nclxmdl.h"
#include "nclxmot.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "uhep.h"
#include "ulist.h"
#include "lcom.h"

extern int NCL_preview_mot;
enum
{
/*
.....Contour fields
*/
	FGPS,FGPCO,FGPTX, FGDRV,FGDCO,FGDTX, FGCHK,FGCCO,FGCTX, FGDSP,FGTYP,
	FGIND,FGITX,FGISE, FGTA1,FGTA2,FGTA3, /*FGFD1,*/ FGFD2,FGFD3,FGFD4,
/*
.....Option Fields
*/
	FOEXT,FOLOK,FOCON,FOASS,FOFIL,
/*
.....Condition Fields
*/
	FCDIR,FCDRV,FCCON,FCINT,FCNP,FCNPT,FCLST,FCDELET, FCCHK, FCINSERT, FCAFTER,
/*
.....Color Fields
*/
	FCOLA,FCOLB,FCOLC, FCOLE,FCOLF, FCOLG,FCOLH,
/*
.....Action item fields
*/
	FAPV, FAPY, FARS, FAPB, FAVE, FAGE, FAVW, FVIDEO
};

#define HIDE_KEY 0
#define CONTOUR_KEY 1
/*
.....Global variables
*/
extern UD_METHOD UD_initfrm_intry;
extern int NAUTIPV;
/*
.....Section definitions
*/
enum {Contour, Options, Conditions, Colors};
static char *Smode[]={"Contour", "Options", "Conditions", "Colors"};
static int Sred[3]={180,0,0}, Syellow[3]={180,148,0}, Sgreen[3]={0,180,0};
static int Sblack[3]={0,0,0};
static int *Ssecol[]={Sblack,Sblack,Sblack,Sblack};
/*
.....Main variables
*/
enum {GOFWDA,EXPLICIT,IMPLIED};
enum {TLOFPS,TLONPS,AUTOPS,NOPS};
static UU_LOGICAL Sform_init = UU_FALSE, Sfedfl=UU_FALSE, Staxfl=UU_FALSE;
static UU_LOGICAL Sfilfl=UU_FALSE, Sassfl=UU_FALSE, Screate_table=UU_FALSE;
static UN_motseg *Smptr=0,Smotatt;
static UN_mot_vpbox_struc Smvpbox[UV_NVPORTS];
static UU_LIST Skey_list;
static int Snkeys;
static UU_LOGICAL Skey_init;
static UU_REAL Scdia;
static UM_vector Sfwd,Svecs[4];
static NCLX_mdl_pntvec Stend;
/*
.....GO variables
*/
static char Sdrive[65],Tdrive[65],Spart[65],Tpart[65],Scheck[65],Tcheck[65];
static int Sdscon,Tdscon,Spscon,Tpscon,Scscon,Tcscon;
static int Smove,Tmove,Staxav,Ttaxav,Sgtfmode,Tgtfmode,Sgtfdav,Tgtfdav;
static int Stype,Ttype,Sdscs,Tdscs;
static char Sindir[65],Tindir[65],Staxis[65],Ttaxis[65],Sfeed[65],Tfeed[65];
static NCLX_mot_feedrate Sgtfd,Tgtfd;
static NCLX_mot_tlaxis Sgtaxis,Tgtaxis;
/*
.....Option variables
*/
static int Sextens,Textens,Slook,Tlook,Scontact,Tcontact;
static int Sgougck[3],Tgougck[3],Sitoler[2],Titoler[2];
static UU_REAL Siter[3],Titer[3];
static NCLX_mot_fillet Sfillet,Tfillet;
static NCLX_mot_maxdp Smaxdp,Tmaxdp;
static NCLX_mot_thick Sthick,Tthick;
static NCLX_mot_toler Stoler,Ttoler;
/*
.....Condition variables
*/
enum {GOFWD,GOLFT,GORGT,GOBACK,GOUP,GODOWN};
static char *Sdirs[]={"Gofwd","Golft","Gorgt","Goback","Goup","Godown"};
enum {AUTO,TO,ON,PAST,TANTO,PSTAN};
static char *Scons[]={"Auto","To","On","Past","Tanto", "Pstan"};
typedef struct
{
	int direction;
	char drive[65];
	int cscon;
	char intof[65];
	char nearpt[65];
} List_struct;
static int Scdir,Tcdir,Sccon,Tccon;
static int Slast_entry;
static char Scdrive[65],Tcdrive[65],Scnpt[65],Tcnpt[65],Scint[65],Tcint[65];
static UD_TLIST Sclist;
static List_struct *Scents=UU_NULL;
static int Ttoend = 0;
static int Tafter = 0;
/*
.....Color variables
*/
static int Sdscol,Tdscol;
static int Spscol,Tpscol;
static int Scscol,Tcscol;
static int Sd1col,Td1col;
static int Sc1col,Tc1col;
static int Tgeotog,Tgeocol;
static UU_LOGICAL Sgeo_redraw;
/*
.....Geometry variables
*/
static int Sdstype;
static UU_LIST Sdlst,Splst,Sclst;
static UM_sgeo Sgds,Sgps,Sgcs,Tgps;
static UU_LOGICAL Sgeo_init[3];
static UU_LOGICAL Sgeo_apply = UU_FALSE;
/*
.....Contour callback routines
*/
static UD_FSTAT OnGfSel(),OnGfTxt(),OnGfTog();
/*
.....Option callbacks
*/
static UD_FSTAT OnOpTog();
/*
.....Condition callbacks
*/
static UD_FSTAT OnCnTog(),OnCnSel(),OnCnTxt();
/*
.....Action callback routines
*/
static UD_FSTAT OnCoTog(),OnAction(),OnVideo();
/*
.....Local routines
*/
static void S_init_form(),S_save_form(),S_hilite_entity(),S_unhilite_entity();
static UD_FSTAT S_enter_form();
static void S_init_traverse(),S_form_invis(),S_form_vis(),S_init_geo();
static void S_hilite_entities(),S_unhilite_all(),S_unhilite_entities();
static void S_update_answers(),S_section_changed();
static void S_add_key(),S_create_key_list(),S_draw_indir(),S_push_geo();
static void S_get_modals(),S_create_table();
static void S_update_table(),S_look_avoid();
static int S_format_table();
static void S_delete_geo(),S_display_ds();
static int S_select_geo(),S_build_command();
static void S_modify_table();
static void S_remove_ds_label();

static int SfrmPlay=0;
static int Serrflg,Spreviewflg,Schgmade;

static int *Sanswers[] = {
	UU_NULL,&Tpscon,(int *)Tpart, UU_NULL,&Tdscon,(int *)Tdrive,
	UU_NULL,&Tcscon,(int *)Tcheck, &Tdscs,&Ttype,
	&Tmove,(int *)Tindir,UU_NULL,
	(int *)Ttaxis,&Ttaxav,UU_NULL,
	/*&Tgtfmode,*/(int *)&Tfeed,&Tgtfdav,UU_NULL,

	&Textens,&Tlook,&Tcontact,UU_NULL,UU_NULL,

	&Tcdir,(int *)Tcdrive,&Tccon,(int *)Tcint,UU_NULL,(int *)Tcnpt,
		(int *)&Sclist, UU_NULL, (int *)&Ttoend, UU_NULL, (int *)&Tafter,
	
	&Tpscol,&Tdscol,&Tcscol,&Td1col,&Tc1col,&Tgeotog,&Tgeocol,

	UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL};

static void S_entity_label_on(UM_sgeo *sfpt);
static int Sfrm = -1;
/*********************************************************************
**    E_FUNCTION     : nclu_contour()
**       Controlling routine for the Flank Contouring form.
**       GO--- statements are output from this form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_contour()
{
	int status,flag;
	UU_LOGICAL cmdreject;
	UD_METHOD save_entry;
/*
.....Set up form fields
*/
	static char traverse[] = {1,1,1, 1,1,1, 1,1,1, 1,1,
		1,1,1, 1,1,1,  1,1,1,1,
		1,1,1,1,1,1,1,
		1,0,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1};
	static char display[] = {1,1,1, 1,1,1, 1,1,1, 1,1,
		1,1,1, 1,1,1,  1,1,1,1,
		1,1,1,1,1, 1,1,1,1,
		1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1};
	static char called[] = {6,6,6, 6,6,6, 6,6,6, 6,6,
		6,6,6, 6,6,6,  6,6,6,6,
		6,6,6,6,6,
		6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6};

	static UD_METHOD methods[] = {OnGfSel,OnGfTog,OnGfTxt,
		OnGfSel,OnGfTog,OnGfTxt,
		OnGfSel,OnGfTog,OnGfTxt,
		OnGfTog,OnGfTog,
		OnGfTog,OnGfTxt,OnGfSel,
		OnGfTxt,OnGfTog,OnGfTog,
		/*OnGfTog,*/OnGfTxt,OnGfTog,OnGfTog,

		OnOpTog,OnOpTog,OnOpTog,OnOpTog,OnOpTog,

		OnCnTog,UU_NULL,OnCnTog,OnCnTxt,OnCnSel,OnCnTxt,OnCnTog,
		OnCnTog,OnCnTog,OnCnTog,OnCnTog,

		OnCoTog,OnCoTog,OnCoTog,OnCoTog,OnCoTog,OnCoTog,OnCoTog,

		OnAction,OnAction,OnAction,OnAction,OnAction,OnAction,OnAction,OnVideo};
/*
.....Get the current tool axis and feed rate
*/
	S_get_modals();
/*
.....Initialize form answers
*/
	S_init_form();
	Serrflg = 0;
	Spreviewflg = 0;
	Schgmade = UU_FALSE;
	save_entry = UD_initfrm_intry;
	SfrmPlay = 0;
	Sfedfl = UU_FALSE;
	Staxfl = UU_FALSE;
	Sfilfl = UU_FALSE;
	Sassfl = UU_FALSE;
	Skey_init = UU_FALSE;
	Snkeys = 0;
	Slast_entry = -1;
	Sdstype = 0;
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0)	goto done;
/*
.....Initialize form fields
*/
	S_init_traverse(display,traverse);
/*
.....Get the Form input
*/
repeat:;
	NCL_preview_mot = 1;
	UD_initfrm_intry = S_enter_form;
	status = ud_form1("ncontour.frm",Sanswers,Sanswers,methods,called,display,
		traverse);
	Sfrm = -1;
	if (Serrflg)
	{
/*
.....removed the displayed label
*/		
		S_remove_ds_label();
	}
	uw_nt_reset_hlt_gfont();
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
.....A drive surface must be selected
*/
	if (Sgds.key == 0 && !Sgeo_apply)
	{
		ud_wrerr("You must select a drive surface.");
		goto repeat;
	}
/*
.....Save the form settings
*/
	S_save_form();
/*
.....Output the command
*/
	status = S_build_command(UU_TRUE);
	if (status != UU_SUCCESS || Serrflg != 0)
	{
		S_display_ds();
		goto repeat;
	}
/*
.....End of routine
*/
done:;
	if (Sgeo_redraw)
		ncl_hide_geo(-1,-1,-1,-1,UU_TRUE,UU_NULL,0);
	if (Skey_init)
		uu_list_free(&Skey_list);
	ud_free_tlist(&Sclist);
	if (Scents != UU_NULL) uu_free(Scents);
	Scents = UU_NULL; Slast_entry = -1;
	Skey_init = UU_FALSE;
	S_unhilite_all();
	NCL_preview_mot = 0;
	ud_delete_assist_segs();
	UD_UNMARK(cmdreject);

	return;
}

/*********************************************************************
**    I_FUNCTION     : OnGfSel(fieldno,val,stat)
**       Method called when a Select button is pressed.
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
static UD_FSTAT OnGfSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status,prmpt,fno,iseg,numint,multi;
	unsigned *mask;
	UU_LOGICAL cmdreject;
	UM_coord pt;
	UM_vector vec;
	UM_PLOCREC pick;
	UM_sgeo *sgeo,tgeo;
	struct NCL_fixed_databag ent;
	static UU_LOGICAL flag=UU_FALSE;
/*
.....Select the part surface
*/
	switch (*fieldno)
	{
	case FGPS:
		S_unhilite_entities(&Splst,&Sgeo_init[1]);
		S_form_invis();
		mask = UD_ncl_psis;
		prmpt = 415;
		status = S_select_geo(&Sgps,&Splst,mask,3,prmpt,Tpscol,0,FGPTX,
			Tpart,&Sgeo_init[1]);
		if (UU_LIST_LENGTH(&Splst) > 0)
		{
			Sgeo_init[1] = UU_TRUE;
			nclu_mot_check_colors(&Splst,1,&Sdlst,1,&Sclst,1);
		}
		S_enable_buttons();
		S_form_vis();
		break;
/*
.....Select the drive surface
*/
	case FGDRV:
		S_unhilite_entities(&Sdlst,&Sgeo_init[0]);
		S_form_invis();
		multi = 3;
		mask = UD_ncl_motion;
		prmpt = 423;
		if (Tpscon == NOPS)
		{
/*			mask = UD_ncl_allcv; */
			mask = UD_ncl_lncicv;
			multi = 2;
		}
		status = S_select_geo(&Sgds,&Sdlst,mask,multi,prmpt,Tdscol,0,FGDTX,
			Tdrive,&Sgeo_init[0]);
		if (status == UU_SUCCESS && multi == 2)
			S_push_geo(&Sdlst,&Sgds,&Sgeo_init[0]);
/*
.....Store 1st DS as Last DS
*/
		if (UU_LIST_LENGTH(&Sdlst) > 0)
		{
			Sgeo_init[0] = UU_TRUE;
			if (Tdscs)
			{
				sgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sdlst);
				tgeo = sgeo[0];
				S_push_geo(&Sdlst,&tgeo,&Sgeo_init[0]);
			}
			nclu_mot_check_colors(&Sdlst,1,&Splst,1,&Sclst,1);
		}
/*
........Create the DS table
*/
		S_create_table(&Sdlst);
/*
........Get the check surface
*/
		if (UU_LIST_LENGTH(&Sdlst) > 0)
		{
			if (UU_LIST_LENGTH(&Sclst) == 0 && !S_enable_buttons())
			{
				flag = UU_TRUE;
				fno = FGCHK; OnGfSel(&fno,val,stat);
				flag = UU_FALSE;
			}
		}
		S_form_vis();
		break;
/*
.....Select the check surface
*/
	case FGCHK:
		S_unhilite_entities(&Sclst,&Sgeo_init[2]);
		S_form_invis();
		mask = UD_ncl_motion;
		prmpt = 424;
		if (Ttype == GOFWDA)
			status = S_select_geo(&Sgcs,&Sclst,mask,3,prmpt,Tcscol,0,FGCTX,
				Tcheck,&Sgeo_init[2]);
		else
		{
			status = S_select_geo(&Sgcs,&Sclst,mask,0,prmpt,Tcscol,0,FGCTX,
				Tcheck,&Sgeo_init[2]);
			if (status == UU_SUCCESS) S_push_geo(&Sclst,&Sgcs,&Sgeo_init[2]);
		}
		if (UU_LIST_LENGTH(&Sclst) > 0)
		{
			Sgeo_init[2] = UU_TRUE;
			nclu_mot_check_colors(&Sclst,1,&Splst,1,&Sdlst,1);
		}
		S_enable_buttons();
		S_form_vis();
		break;
/*
.....Select the direction
*/
	case FGISE:
		if (Tmove == 0 || Tmove == 1)
		{
			S_form_invis();
			UD_MARK (cmdreject, UU_TRUE);
			if (cmdreject == 0)
			{
				iseg = ud_pick_assist_seg("Pick the new forward vector.");
				if (iseg != 0)
				{
					um_vctovc(Svecs[iseg-1],Sfwd);
					ncl_sprintf(Tindir,Sfwd,3);
					ud_update_answer(FGITX,Tindir);
					if (Tmove == 0)
					{
						Tmove = 1;
						ud_update_answer(FGIND,&Tmove);
					}
				}
			}
			S_draw_indir();
			S_form_vis();
			UD_UNMARK(cmdreject);
		}
		else if (Tmove == 2)
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
					ncl_sprintf(Tindir,Sfwd,3);
					ud_update_answer(FGITX,Tindir);
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
	if (status == UU_SUCCESS) Schgmade = 1;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnGfTog(fieldno,val,stat)
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
static UD_FSTAT OnGfTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,fno,inc, chg;
	char cfed[16][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	char caxis[6][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	char maxis[12][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	UM_sgeo *sgeo,tgeo;
	UU_REAL d6[6];
	int autops;
/*
.....Process GO toggle fields
*/
	switch (*fieldno)
	{
	case FGPCO:
		if (Spscon != Tpscon) Schgmade = 1;
		if (Tpscon == AUTOPS || Tpscon == NOPS)
		{
			ud_set_traverse_mask(FGPS,UU_FALSE);
			ud_set_traverse_mask(FGPTX,UU_FALSE);
			S_unhilite_entities(&Splst,&Sgeo_init[1]);
			Tpart[0] = '\0';
			ud_update_answer(FGPTX,Tpart);
			if (Tpscon == NOPS)
			{
				ud_set_traverse_mask(FGCHK,UU_FALSE);
				ud_set_traverse_mask(FGCTX,UU_FALSE);
				ud_set_traverse_mask(FGDSP,UU_FALSE);
				ud_set_traverse_mask(FGTYP,UU_FALSE);
				S_unhilite_entities(&Sclst,&Sgeo_init[2]);
				Tcheck[0] = '\0';
				ud_update_answer(FGCTX,Tcheck);
				ud_set_traverse_mask(FOEXT,UU_FALSE);
				ud_set_traverse_mask(FOLOK,UU_FALSE);
			}
			else
			{
				ud_set_traverse_mask(FGCHK,UU_TRUE);
				ud_set_traverse_mask(FGCTX,UU_TRUE);
				ud_set_traverse_mask(FGDSP,UU_TRUE);
				ud_set_traverse_mask(FGTYP,UU_TRUE);
/*
.....get d table value and set i,j,k,d in the text area
*/
				ifatops(&autops);
				if (autops)
				{
					getdtbl6(d6);
					sprintf (Tpart, "PL/%g,%g,%g,%g", d6[2], d6[3], d6[4], d6[5]);
					ud_update_answer(FGPTX,Tpart);
				}
				if (Ttype == GOFWDA)
				{
					ud_set_traverse_mask(FOEXT,UU_TRUE);
					ud_set_traverse_mask(FOLOK,UU_TRUE);
				}
			}
		}
		else
		{
			ud_set_traverse_mask(FGPS,UU_TRUE);
			ud_set_traverse_mask(FGPTX,UU_TRUE);
			ud_set_traverse_mask(FGCHK,UU_TRUE);
			ud_set_traverse_mask(FGCTX,UU_TRUE);
			ud_set_traverse_mask(FGDSP,UU_TRUE);
			ud_set_traverse_mask(FGTYP,UU_TRUE);
		}
		S_enable_buttons();
		break;
	case FGDCO:
		if (Spscon != Tpscon) Schgmade = 1;
		break;
	case FGCCO:
		if (Scscon != Tcscon) Schgmade = 1;
		if (Sclist.num_item > 0)
		{
			inc = Sclist.num_item - 1;
			Scents[inc].cscon = Tcscon + 1;
			chg = S_format_table(inc);
			if (chg)
				ud_update_answer(FCLST,&Sclist);
			if (Sclist.answer == inc)
			{
				Tccon = Scents[inc].cscon;
				ud_update_answer(FCCON,&Tccon);
			}
		}
		break;
	case FGDSP:
		if (Sgeo_init[0])
		{
			if (Tdscs)
			{
				sgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sdlst);
				tgeo = sgeo[0];
				S_push_geo(&Sdlst,&tgeo,&Sgeo_init[0]);
			}
			else
				S_delete_geo(&Sdlst,UU_LIST_LENGTH(&Sdlst)-1,&Sgeo_init[0]);
			S_create_table(&Sdlst);
		}
		if (Sdscs != Tdscs) Schgmade = 1;
		break;
	case FGTYP:
		if (Stype != Ttype) Schgmade = 1;
		if (Ttype == GOFWDA) /*AUTO */
		{
			Ttoend = 1;
			ud_update_answer(FCCHK,&Ttoend);
			ud_set_traverse_mask(FOEXT,UU_TRUE);
			ud_set_traverse_mask(FOLOK,UU_TRUE);
/*
			ud_set_traverse_mask(FCDIR,UU_FALSE);
			ud_set_traverse_mask(FCCON,UU_TRUE);
			ud_set_traverse_mask(FCINT,UU_TRUE);
			ud_set_traverse_mask(FCNP,UU_TRUE);
			ud_set_traverse_mask(FCNPT,UU_TRUE);
*/
/*
.....disable all condition field per Ken request
*/
			ud_set_traverse_mask(FCDRV,UU_FALSE);
			ud_set_traverse_mask(FCDIR,UU_FALSE);
			ud_set_traverse_mask(FCCON,UU_FALSE);
			ud_set_traverse_mask(FCINT,UU_FALSE);
			ud_set_traverse_mask(FCNP, UU_FALSE);
			ud_set_traverse_mask(FCNPT,UU_FALSE);
			ud_set_traverse_mask(FCLST,UU_FALSE);
			ud_set_traverse_mask(FCDELET,UU_FALSE);
			ud_set_traverse_mask(FCCHK,UU_FALSE);
			ud_set_traverse_mask(FCINSERT,UU_FALSE);
			ud_set_traverse_mask(FCAFTER,UU_FALSE);
		}
		else
		{
			Ttoend = 0;
			ud_update_answer(FCCHK,&Ttoend);
			ud_set_traverse_mask(FOEXT,UU_FALSE);
			ud_set_traverse_mask(FOLOK,UU_FALSE);
			ud_set_traverse_mask(FCDIR,UU_TRUE);
			ud_set_traverse_mask(FCLST,UU_TRUE);
			ud_set_traverse_mask(FCDELET,UU_TRUE);
			ud_set_traverse_mask(FCCHK,UU_TRUE);
			ud_set_traverse_mask(FCINSERT,UU_TRUE);
			ud_set_traverse_mask(FCAFTER,UU_TRUE);
			if (Ttype == IMPLIED)
			{
				ud_set_traverse_mask(FCCON,UU_FALSE);
				ud_set_traverse_mask(FCINT,UU_FALSE);
				ud_set_traverse_mask(FCNP,UU_FALSE);
				ud_set_traverse_mask(FCNPT,UU_FALSE);
			}
			else
			{
				ud_set_traverse_mask(FCCON,UU_TRUE);
				ud_set_traverse_mask(FCINT,UU_TRUE);
				ud_set_traverse_mask(FCNP,UU_TRUE);
				ud_set_traverse_mask(FCNPT,UU_TRUE);
			}
		}
		S_enable_buttons();
		break;
/*
.....Initial Direction
*/
	case FGIND:
		i = Tmove != 0;
		ud_set_traverse_mask(FGITX,i);
		ud_set_traverse_mask(FGISE,i);
		S_draw_indir();
		if (i == 1)
		{
			fno = FGISE;
			OnGfSel(&fno,val,stat);
		}
		break;
/*
.....Tool axis
*/
	case FGTA2:
		ud_set_traverse_mask(FGTA3,Ttaxav);
		Schgmade = 1;
		break;
	case FGTA3:
		nclu_tlaxis_form(&Sgtaxis,caxis,maxis,UU_FALSE,UU_FALSE,Staxfl);
		Staxfl = UU_TRUE;
		break;
/*
.....Feed rate
*/
/****
	case FGFD1:
		if (Sgtfmode != Tgtfmode) Schgmade = 1;
		if (Tgtfmode == 1)
		{
			ud_set_traverse_mask(FGFD2,UU_TRUE);
			ud_set_traverse_mask(FGFD3,UU_TRUE);
			ud_set_traverse_mask(FGFD4,Tgtfdav);
		}
		else
		{
			Sfedfl = UU_FALSE;
/* Leave as it is */
/*			Tgtfdav = 0; ud_update_answer(FGFD3,&Tgtfdav); */
/****			ud_set_traverse_mask(FGFD2,UU_FALSE);
			ud_set_traverse_mask(FGFD3,UU_FALSE);
			ud_set_traverse_mask(FGFD4,UU_FALSE);
			
		}
*/		break;
	case FGFD3:
//		ud_set_traverse_mask(FGFD4, Tgtfmode&&Tgtfdav);
		ud_set_traverse_mask(FGFD4, Tgtfdav);
		Schgmade = 1;
//		if (!(Tgtfmode&&Tgtfdav))
		if (!(Tgtfdav))
			break; 
	case FGFD4:
		Sgtfd.base_feedrate = atof(Tfeed);
		if (Sgtfd.mode==3)
			Sgtfd.mode = 1;
		nclu_fedrat_form(&Sgtfd,cfed,UU_FALSE,Sfedfl, FGFD2);
		Sfedfl = UU_TRUE;
		break;
	}
	return(UD_FLDOK);
}
int nclc_get_fedrate_fldnum()
{
	return FGFD2;
}
/*********************************************************************
**    I_FUNCTION     : OnGfTxt(fieldno,val,stat)
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
static UD_FSTAT OnGfTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Point label text field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FGPTX:
		S_unhilite_entities(&Splst,&Sgeo_init[1]);
		ul_to_upper(Tpart);
		if (strcmp(Spart,Tpart) != 0)
		{
			Schgmade = 1;
			S_init_geo(&Sgps,Tpart,Tpscol);
			S_push_geo(&Splst,&Sgps,&Sgeo_init[1]);
		}
		S_enable_buttons();
		break;
	case FGDTX:
/*
.....if Tdrive is "GEOM,...", ignore it since it is assigned by
.....picking, don't change it
*/
		if (strstr(Tdrive,",...")!=NULL)
			break;
		S_unhilite_entities(&Sdlst,&Sgeo_init[0]);
		ul_to_upper(Tdrive);
		if (strcmp(Sdrive,Tdrive) != 0)
		{
			Schgmade = 1;
			Ssecol[Contour] = Sgreen; S_section_changed(Contour,UU_TRUE);
			S_init_geo(&Sgds,Tdrive,Tdscol);
			S_push_geo(&Sdlst,&Sgds,&Sgeo_init[0]);
			S_create_table(&Sdlst);
/*
........Enable the Action buttons
*/
			S_enable_buttons();
		}
		break;
	case FGCTX:
/*
.....if Tdrive is "GEOM,...", ignore it since it is assigned by
.....picking, don't change it
*/
		if (strstr(Tcheck,",...")!=NULL)
			break;
		S_unhilite_entity(&Sgcs);
		ul_to_upper(Tcheck);
		if (strcmp(Scheck,Tcheck) != 0)
		{
			Schgmade = 1;
			S_init_geo(&Sgcs,Tcheck,Tcscol);
			S_push_geo(&Sclst,&Sgcs,&Sgeo_init[2]);
			ud_dispfrm_set_attribs(0,FGCHK,UM_BLACK,Tcscol);
/*
........Enable the Action buttons
*/
			S_enable_buttons();
		}
		break;
	case FGTA1:
		ul_to_upper(Ttaxis);
		if (strcmp(Staxis,Ttaxis) != 0) Schgmade = 1;
		break;
	case FGFD2:
		S_enable_buttons();
		break;
	case FGFD3:
		ul_to_upper(Tfeed);
		if (strcmp(Sfeed,Tfeed) != 0) Schgmade = 1;
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnOpTog(fieldno,val,stat)
**       Method called when an Option toggle field is changed.
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
	char caxis[16][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
/*
.....Process Option toggle fields
*/
	switch (*fieldno)
	{
	case FOEXT:
		if (Sextens != Textens) Schgmade = 1;
		break;
	case FOLOK:
		if (Slook != Tlook) Schgmade = 1;
		break;
	case FOCON:
		if (Scontact != Tcontact) Schgmade = 1;
		break;
/*
.....Motion Assist
*/
	case FOASS:
		nclu_tool_calc_form(&Smaxdp,Siter,&Sthick,&Stoler,Sitoler,Sgougck,caxis,
			UU_FALSE,Sassfl);
		Sassfl = UU_TRUE;
		Schgmade = 1;
		break;
/*
.....Corner Rounding
*/
	case FOFIL:
		nclu_arcslp_fillet_form(&Sfillet,caxis,UU_FALSE,Sfilfl);
		Sfilfl = UU_TRUE;
		Schgmade = 1;
		break;
	}
	if (Schgmade)
	{
		Ssecol[Options] = Sgreen;
		S_section_changed(Options,UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnCnSel(fieldno,val,stat)
**       Method called when a Select button is pressed.
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
static UD_FSTAT OnCnSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int iseg,numint,relnum;
	UU_LOGICAL cmdreject;
	UM_coord pt;
	UM_vector vec;
	UD_PLOCREC pick;
	UM_PICKENT pent;
	struct NCL_fixed_databag ent;

	switch (*fieldno)
	{
/*
.....Select the near point
*/
	case FCNP:
		S_form_invis();
		do
		{
			UD_MARK(cmdreject,UU_TRUE);
			if (cmdreject != 0) break;
			ud_lgeo(UU_TRUE,UD_ncl_motion);
			ud_ldas(UD_DASPCKLOC,UA_NCL,650,&pick,1,&numint,1,UD_NODEFAULT);
			if (numint == 0) break;
			um_d_pickresolve(&(pick.ppath),1,&pent);
			ent.key = um_get_pickkey(&pent,1);
			ur_retrieve_data_relnum(ent.key,&relnum);
			if (relnum == UM_POINT_REL || relnum == NCL_POINTVEC_REL)
			{
				ncl_retrieve_data_fixed(&ent);
				ncl_get_label(&ent,Tcnpt);
				iseg = UU_SUCCESS;
			}
			else
			{
				iseg = ncl_get_pkpt1(&pick,&ent,.001,pt,vec);
				if (iseg == UU_FAILURE)
				{
					ud_wrerr ("Failed to locate point on entity");
					continue;
				}
				else
				{
					UM_cc_inttoext(pt,pt);
					ncl_sprintf(Tcnpt,pt,3);
				}
			}
			ud_update_answer(FCNPT,Tcnpt);
			Schgmade = 1;
			Ssecol[Conditions] = Sgreen; S_section_changed(Conditions,UU_TRUE);
		} while (iseg == UU_FAILURE);
		S_form_vis();
		UD_UNMARK(cmdreject);
		break;
	}
	return(UD_FLDOK);
}

static void S_delete_sel()
{
	int i,last;
	UM_sgeo *sgeo;
/*
.....Delete item from the list
*/
	if (Sgeo_init[0]==0) 
		return;
	last = Slast_entry;
	if (Ttoend)
		last = UU_LIST_LENGTH(&Sdlst)-1;

	sgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sdlst);
/*
.....added last one into check geom
*/
	if (Ttoend)
		strcpy(Tcheck, sgeo[Slast_entry].label);

	for (i=Slast_entry;i<=last;i++)
	{
		S_unhilite_entity(&(sgeo[i]));
	}
	if (Ttoend)
		S_unhilite_entities(&Sclst,&Sgeo_init[2]);
	
	for (i=Slast_entry;i<=last;i++)
		uu_list_delete(&Sdlst,i,1);
	last = Slast_entry;
/*
........Create the DS table
*/
	S_modify_table(Slast_entry);
	if (Ttoend)
	{
		ud_update_answer(FGCTX,Tcheck);
		S_init_geo(&Sgcs,Tcheck,Tcscol);
		S_push_geo(&Sclst,&Sgcs,&Sgeo_init[2]);
		S_hilite_entities(&Sclst,Tcscol);
	}
	S_hilite_entities(&Sdlst,Tdscol);
	if ((last>1)&&(Ttoend))
	{
		Sclist.answer = last-1;
		ud_update_answer(FCLST,(int *)&Sclist);
	}
	ud_dispfrm_set_attribs(0,FGCHK,UM_BLACK,Tcscol);
/*
.....enable CS condition
*/
	ud_set_traverse_mask(FCCON, UU_TRUE);
}

/*********************************************************************
**    I_FUNCTION     :  S_select_insert_geo(sfpt,sflst,mask,prmno,color,geo_init)
**        
**       Routine to select a single geometry and insert into an existing list. If the exist list is empty
**			create one. The geometry will be highlighted.
**    PARAMETERS
**       INPUT  :
**          mask     Picking mask.
**          prmno    Prompt number to use while in pick mode.
**          color    Color to highlight the picked entity.
**                   -1 = Don't highlight entity.
**          sflst    Pointer to existing geometry structure list. Could be NULL if not exist.
**       OUTPUT :
**          sfpt     Pointer to selected geometry structure for single entity.
**          sflst    Pointer to updated existing geometry structure list.
**          geo_init:  UU_TRUE if geometry has been picked from the screen.
**    RETURNS      : UU_SUCCESS if an entity is picked.  UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_select_insert_geo(sfpt,sflst,indx, mask,prmno,color, geo_init)
UM_sgeo *sfpt;
UU_LIST *sflst;
unsigned int *mask;
int prmno,color;
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
	char label[65];
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
	if (*geo_init!=UU_TRUE)
		uu_list_init(sflst,sizeof(UM_sgeo),numint,10);
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
	} while (!doend);
/*
........Store the entity's data
*/
	sfpt->key = e.key;
	sfpt->relnum = e.rel_num;
	if (sfpt->key != 0) ncl_get_label(&e,sfpt->label);
	else strcpy(sfpt->label,label);
	sfpt->label_on = -1;
/*
........Highlight the selected entity
*/
	S_hilite_entity(sfpt,color);
	init = UU_TRUE;
	nc = strlen(sfpt->label);
	ul_strip_blanks(sfpt->label,&nc);
	if (nc != 0)
	{
/*
........insert the entity onto the stack
*/
		if (indx<0) indx = 0;
		if (Tafter==0)
			uu_list_insert(sflst, indx, sfpt);
		else
			uu_list_insert(sflst, indx+1, sfpt);
	}
	else
	{
		ud_wrerr("No CAM entities selected.");
		goto repeat;
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
**    I_FUNCTION     :  S_select_insert_geos(sfpt,sflst,mask,prmno,color,geo_init,num_add)
**        
**       Routine to select one or more geometries and insert into an existing list. If the exist list is empty
**			create one. The geometry will be highlighted.
**    PARAMETERS
**       INPUT  :
**          mask     Picking mask.
**          prmno    Prompt number to use while in pick mode.
**          color    Color to highlight the picked entity.
**                   -1 = Don't highlight entity.
**          sflst    Pointer to existing geometry structure list. Could be NULL if not exist.
**       OUTPUT :
**          sfpt     Pointer to selected geometry structure for single entity.
**          sflst    Pointer to updated existing geometry structure list.
**          geo_init:  UU_TRUE if geometry has been picked from the screen.
**			num_add: number of geometries added
**    RETURNS      : UU_SUCCESS if an entity is picked.  UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_select_insert_geos(sfpt,sflst,indx, mask,prmno,color, geo_init,num_add)
UM_sgeo *sfpt;
UU_LIST *sflst;
unsigned int *mask;
int prmno,color,*num_add;
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
	char label[65];
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
	if (*geo_init!=UU_TRUE)
		uu_list_init(sflst,sizeof(UM_sgeo),numint,10);
/*
.....Get the next geometry selection
*/
	*num_add = 0;
repeat:;
	ud_lgeo(UU_TRUE,imask);
	ud_ldas(UD_DASSELECT,UA_NCL,prmno,NULL,1,&numint,UD_NODEFAULT);
	if (numint == 0) goto failed;

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
			sfpt->label_on = -1;
			ncl_get_label(&e,sfpt->label);
			nc = strlen(sfpt->label);
			ul_strip_blanks(sfpt->label,&nc);
/*
........Highlight the selected entity
*/
			if (nc != 0)
			{
				S_hilite_entity(sfpt,color);
				if (indx<0) indx = 0;
				if (Tafter==0)
				{
					uu_list_insert(sflst, indx, sfpt);
					indx++;
				}
				else
				{
					uu_list_insert(sflst, indx+1, sfpt);
					indx++;
				}
				(*num_add)++;
			}
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

static void S_Insert_Geom()
{
/*
.....save the select entity
*/
	unsigned *mask;
	UM_sgeo *sgeo,tgeo;
	int num_add, prmpt, last = Slast_entry;
		
	mask = UD_ncl_motion;
	prmpt = 423;
	if (Tpscon == NOPS)
	{
		mask = UD_ncl_lncicv;
	}
	num_add = 0;
	S_select_insert_geos(&Sgds,&Sdlst,last, mask,prmpt, Tdscol, &Sgeo_init[0], &num_add);
	if (num_add==0)
		return;
/*
.....Store 1st DS as Last DS
*/
	if (UU_LIST_LENGTH(&Sdlst) > 0)
	{
		Sgeo_init[0] = UU_TRUE;
		if (Tdscs)
		{
			sgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sdlst);
			tgeo = sgeo[0];
			S_push_geo(&Sdlst,&tgeo,&Sgeo_init[0]);
		}
		nclu_mot_check_colors(&Sdlst,1,&Splst,1,&Sclst,1);
	}
	S_create_table(&Sdlst);

	S_hilite_entities(&Sdlst,Tdscol);
	if (Tafter==0)
		Sclist.answer = last;
	else
		Sclist.answer = last + 1;
	ud_update_answer(FCLST,(int *)&Sclist);
/*
.....enable CS condition
*/
	ud_set_traverse_mask(FCCON, UU_TRUE);
}

/*********************************************************************
**    I_FUNCTION     : OnCnTog(fieldno,val,stat)
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
static UD_FSTAT OnCnTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL idid;
	UD_TABLEINFO *info;
/*
.....Process Condition toggle fields
*/
	idid = UU_FALSE;
	switch (*fieldno)
	{
	case FCDIR:
		if (Slast_entry == -1) break;
		if (Tcdir != Scents[Slast_entry].direction) Schgmade = idid = 1;
		break;
	case FCCON:
		if (Slast_entry == -1) break;
		if (Tccon != Scents[Slast_entry].cscon) Schgmade = idid = 1;
		if (Sclist.answer == UU_LIST_LENGTH(&Sdlst)-1)
		{
			Tcscon = Tccon - 1;
			ud_update_answer(FGCCO,&Tcscon);
		}
		break;
/*
........List selection
*/
	case FCLST:
		if (Sclist.num_item == 0) break;
		info = (UD_TABLEINFO *)(val->frmint);
		Sclist.answer = info->row;
		S_update_table(UU_TRUE);
		break;
	case FCCHK:
		break;
	case FCDELET:
		S_delete_sel();
		break;
	case FCAFTER:
		break;
	case FCINSERT:
		S_Insert_Geom();
		break;
	}
	if (idid)
	{
		Ssecol[Conditions] = Sgreen;
		S_section_changed(Conditions,UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnCnTxt(fieldno,val,stat)
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
static UD_FSTAT OnCnTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Point label text field
.......Check for change since preview
*/
	switch (*fieldno)
	{
	case FCINT:
		if (Slast_entry == -1) break;
		if (strcmp(Tcint,Scents[Slast_entry].intof) != 0) Schgmade = 1;
		break;
	case FGPTX:
		if (Slast_entry == -1) break;
		if (strcmp(Tcnpt,Scents[Slast_entry].nearpt) != 0) Schgmade = 1;
		break;
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
/*
.....Process Color toggle field
*/
	switch (*fieldno)
	{
	case FCOLA:
		ud_dispfrm_set_attribs(0,FGPS,UM_BLACK,Tpscol);
		S_hilite_entities(&Splst,Tpscol);
		break;
	case FCOLB:
		if (UU_LIST_LENGTH(&Sdlst) != 0)
			ud_dispfrm_set_attribs(0,FGDRV,UM_BLACK,Tdscol);
		S_hilite_entities(&Sdlst,Tdscol);
		break;
	case FCOLC:
		ud_dispfrm_set_attribs(0,FGCHK,UM_BLACK,Tcscol);
		S_hilite_entities(&Sclst,Tcscol);
		break;
	case FCOLG:
		Tgeotog = val->frmint[0];
	case FCOLH:
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
	UM_int2 idx,i2val;
	int autops;
	UU_REAL d6[6];
	UM_sgeo *sgeo;
	int status,i,flag = 0;

	switch (*fieldno)
	{
/*
.....Preview command
*/
	case FAPV:
		if (Spreviewflg) moinfr(&flag);
		Spreviewflg = UU_FALSE;
		Schgmade = UU_FALSE;
		if (Serrflg)
		{
/*
.....removed the displayed label
*/		
			S_remove_ds_label();
		}
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
		if (status != UU_SUCCESS || Serrflg != 0)
		{
/*
.....enable list-table
*/
			ud_set_traverse_mask(FCLST,UU_TRUE);
			ud_set_traverse_mask(FCDELET,UU_TRUE);
			ud_set_traverse_mask(FCCHK,UU_TRUE);
			ud_set_traverse_mask(FCINSERT,UU_TRUE);
			ud_set_traverse_mask(FCAFTER,UU_TRUE);
			S_display_ds();
/*
.....if error, show the label
*/
			sgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sdlst);
			if (Slast_entry-1>=0)
				S_entity_label_on(&(sgeo[Slast_entry-1]));
			S_entity_label_on(&(sgeo[Slast_entry]));
			if (Slast_entry < UU_LIST_LENGTH(&Sdlst)-1)
				S_entity_label_on(&(sgeo[Slast_entry+1]));
			ud_update_answer(FCLST,&Sclist);
			uw_nt_reset_hlt_gfont();
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
.....A curve must be selected
*/
	case FAPY:
		if (Serrflg)
		{
/*
.....removed the displayed label
*/		
			S_remove_ds_label();
		}
/*
........Output GO command
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
		if (status != UU_SUCCESS || Serrflg != 0)
		{
			S_display_ds();
/*
.....if error, show the label
*/
			sgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sdlst);
			if (Slast_entry-1>=0)
				S_entity_label_on(&(sgeo[Slast_entry-1]));
			S_entity_label_on(&(sgeo[Slast_entry]));
			if (Slast_entry < UU_LIST_LENGTH(&Sdlst)-1)
				S_entity_label_on(&(sgeo[Slast_entry+1]));
			ud_update_answer(FCLST,&Sclist);
			uw_nt_reset_hlt_gfont();
			break;
		}
		S_unhilite_entities(&Sdlst,&Sgeo_init[0]);
		S_unhilite_entities(&Splst,&Sgeo_init[1]);
		S_unhilite_entities(&Sclst,&Sgeo_init[2]);
/*
......reset surface to original
*/
		Sgeo_init[0] = Sgeo_init[1] = Sgeo_init[2] = UU_FALSE;
		Sgds.color = Sgps.color = Sgcs.color = -1;
		Sgds.label_on = Sgps.label_on = Sgcs.label_on = -1;
		Sgds.key = Sgps.key = Sgcs.key = 0;
		Sgds.label[0] = Sgps.label[0] = Sgcs.label[0] = '\0';
		moinfo(&Sgps.key, &Tpscon, &Tdscon); Tdscon++;
		if (Tdscon == 1) Tcscon = 1;
		idx = 276; getifl(&idx, &i2val);
		if (i2val == 1) Tpscon = NOPS;
		ifatops(&autops);
		if (autops)
			Tpscon = AUTOPS;
		if (autops)
		{
			getdtbl6(d6);
			sprintf (Tpart, "PL/%g,%g,%g,%g", d6[2], d6[3], d6[4], d6[5]);
			ud_update_answer(FGPTX,Tpart);
		}
		Sdscon = Tdscon;
		Spscon = Tpscon;
		Scscon = Tcscon;

		if (Sgps.key != 0)
		{
			status = ncl_get_label_with_key(Sgps.key,Tpart);
			S_init_geo(&Sgps,Tpart,Tpscol);
			S_push_geo(&Splst,&Sgps,&Sgeo_init[1]);
		}

		Tgps = Sgps;
		Tcheck[0] = '\0';
		Tdrive[0] = '\0';
		S_init_geo(&Sgds,Tdrive,Tdscol);
		S_init_geo(&Sgps,Tpart,Tpscol);
		S_init_geo(&Sgcs,Tcheck,Tcscol);
		ud_update_answer(FGPTX,Tpart);
		ud_update_answer(FGCTX,Tcheck);
		ud_update_answer(FGDTX,Tdrive);
		ud_update_answer(FGPCO, &Tpscon);
		ud_update_answer(FGDCO, &Tdscon);
		ud_update_answer(FGCCO, &Tcscon);
/*
.....reset Fedrat and TLAXIS
*/
		S_get_modals();
		Tgtfd = Sgtfd;
		Sgtfdav = Tgtfd.accel_flag || Tgtfd.slowdown_flag || (Tgtfd.mode==2);
		Sgtfmode = 0;
		Tgtaxis = Sgtaxis;
		Staxav = Tgtaxis.mode != NCLX_MOT_TLAXIS_SAME &&
				Tgtaxis.mode != NCLX_MOT_TLAXIS_FIXED;
		Tgtfmode = Sgtfmode;
		Ttaxav = Staxav;
		Tgtfdav = Sgtfdav;
//		ud_update_answer(FGFD1,&Tgtfmode);
		ud_update_answer(FGFD3, &Tgtfdav);
//		ud_set_traverse_mask(FGFD2,UU_FALSE);
		ud_set_traverse_mask(FGFD3,UU_FALSE);
		ud_set_traverse_mask(FGFD4,UU_FALSE);
	
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
		Sfedfl = UU_FALSE;
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
**    I_FUNCTION     : S_get_modals()
**       Gets the motion modals/settings and stores them in local
**       structures.
**    PARAMETERS
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_get_modals()
{
	int inum;
	UM_int2 idx,i2;
/*
.....Get motion settings
*/
	NclxMotGetTlaxis(&Sgtaxis);
	NclxMotGetFeedrate(&Sgtfd);
	NclxMotGetFillet(&Sfillet);
	NclxMotGetMaxdp(&Smaxdp);
	NclxMotGetNumpts(&inum);
	Siter[0] = inum;
	NclxMotGetMaxang(&Siter[1]);
	NclxMotGetThick(&Sthick);
	NclxMotGetToler(&Stoler);
	Stoler.start_cos = acos(Stoler.start_cos) * UM_RADIAN;
	idx = 2; getlfl(&idx,&i2); Sitoler[0] = i2;
	idx = 363; getifl(&idx,&i2); Sitoler[1] = i2;
	NclxMotGetGougck1(&Sgougck[0],&Sgougck[1],&Sgougck[2]);
	NclxMotGetContact(&Scontact);
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
	if (Tpscon == AUTOPS || Tpscon == NOPS)
	{
		traverse[FGPS] = 0;
		traverse[FGPTX] = 0;
		Tpart[0] = '\0';
		if (Tpscon == NOPS)
		{
			traverse[FGCHK] = 0;
			traverse[FGCTX] = 0;
			traverse[FGDSP] = 0;
			traverse[FGTYP] = 0;
			Tcheck[0] = '\0';		
		}
		else
		{
			traverse[FGCHK] = 1;
			traverse[FGCTX] = 1;
			traverse[FGDSP] = 1;
			traverse[FGTYP] = 1;
		}
	}
	else
	{
		traverse[FGPS] = 1;
		traverse[FGPTX] = 1;
		traverse[FGCHK] = 1;
		traverse[FGCTX] = 1;
		traverse[FGDSP] = 1;
		traverse[FGTYP] = 1;
	}
/*
.....Initialize Go fields
*/
/****	if (Tgtfmode < 1)
	{
		traverse[FGFD2] = traverse[FGFD3] = traverse[FGFD4] = 0;
	}
	else
****/
	{
		traverse[FGFD2] = traverse[FGFD3] = 1;
/****		traverse[FGFD4] = Tgtfdav&&Tgtfmode;
*/
		traverse[FGFD4] = Tgtfdav;
	}
	traverse[FGTA3] = Ttaxav;
/*
.....Option fields
*/
	if ((Ttype == GOFWDA)&&(Tpscon!=NOPS))
		traverse[FOEXT] = traverse[FOLOK] = 1;
	else
		traverse[FOEXT] = traverse[FOLOK] = 0;

	traverse[FOCON] = 0;
/*
.....Initialize Condition fields
*/
	if (Ttype == GOFWDA)
	{
		traverse[FCDRV] = 0;
		traverse[FCDIR] = 0;
		traverse[FCCON] = 0;
		traverse[FCINT] = 0;
		traverse[FCNP] = 0;
		traverse[FCNPT] = 0;
		traverse[FCLST] = 0;
		traverse[FCDELET] = 0;
		traverse[FCCHK] = 0;
		traverse[FCINSERT] = 0;
		traverse[FCAFTER] = 0;
	}
	else if (Ttype == IMPLIED)
	{
		traverse[FCDIR] = 1;
		traverse[FCCON] = 0;
		traverse[FCINT] = 0;
		traverse[FCNP] = 0;
	}
	else
	{
		traverse[FCDIR] = 1;
		traverse[FCCON] = 1;
		traverse[FCINT] = 1;
		traverse[FCNP] = 1;
	}
/*
.....Action item fields
*/
	for (i=FAPV;i<FAVW;i++) traverse[i] = 0;
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
	int i,typ,status;
	UM_int2 idx,i2val;
	double cutr[6];
	UU_KEY_ID pskey;
	int autops;
	UU_REAL d6[6];
	UU_REAL hgt;
	int chc;
/*
.....Determine if Advanced Tlaxis & Feedrates are active
*/
	Tgtfd = Sgtfd;
	nclc_get_ctr_hgt(&chc, &hgt);
	Sgtfdav = Tgtfd.accel_flag || Tgtfd.slowdown_flag || (hgt!=0.0);
/*
.....always default to Same
	Sgtfmode = Sgtfdav ? 1:0;
*/
	Sgtfmode = 0;
	Tgtaxis = Sgtaxis;
	Staxav = Tgtaxis.mode != NCLX_MOT_TLAXIS_SAME &&
		Tgtaxis.mode != NCLX_MOT_TLAXIS_FIXED;

/*
.....Initialize Assist modals
*/
	Tfillet = Sfillet;
	Tmaxdp = Smaxdp;
	Tthick = Sthick;
	Ttoler = Stoler;
	for (i=0;i<3;i++)
	{
		Titer[i] = Siter[i];
		Tgougck[i] = Sgougck[i];
	}
	Titoler[0] = Sitoler[0];
	Titoler[1] = Sitoler[1];
/*
.....Initialize the Form settings
*/
	if (!Sform_init)
	{
		Sdscs = UU_FALSE;
		Stype = GOFWDA;
		Sdscol = NCLX_ORANGE;
		Spscol = NCLX_SEA_GREEN;
		Scscol = NCLX_PURPLE;
		Sd1col = NCLX_LT_BLUE;
		Sc1col = NCLX_TAN;
	}
	Scscon = 0;
	Sdrive[0] = '\0';
	Spart[0] = '\0';
	Scheck[0] = '\0';
	Smove = 0;
	obcutr(cutr,&typ);
	UM_len_exttoint(cutr[0],Scdia);
	Scdia = Scdia + .75;
	NclxMotGetTool(&Stend);
	UM_cc_exttoint(Stend.pt,Stend.pt);
	NclxMotGetFwd(Sfwd);
	ncl_sprintf(Sindir,Sfwd,3);
	ncl_sprintf(Staxis,&Sgtaxis.vector,3);
	ncl_sprintf(Sfeed,&Sgtfd.base_feedrate,1);
/*
.....Get previous PS/DS conditions
*/
	moinfo(&pskey,&Spscon,&Sdscon); Sdscon++;
	if (Sdscon == 1) Scscon = 1;
	idx = 276; getifl(&idx,&i2val);
	if (i2val == 1) Spscon = NOPS;
	ifatops(&autops);
	if (autops)
		Spscon = AUTOPS;
	if (autops)
	{
		getdtbl6(d6);
		sprintf (Spart, "PL/%g,%g,%g,%g", d6[2], d6[3], d6[4], d6[5]);
	}
	Tdscol = Sdscol;
	Tpscol = Spscol;
	Tcscol = Scscol;
	Td1col = Sd1col;
	Tc1col = Sc1col;
	Tdscs = Sdscs;
	Ttype = Stype;
	Tdscon = Sdscon;
	Tpscon = Spscon;
	Tcscon = Scscon;
	Tgtfmode = Sgtfmode;
	Ttaxav = Staxav;
	Tgtfdav = Sgtfdav;
	strcpy(Tdrive,Sdrive);
	strcpy(Tpart,Spart);
	strcpy(Tcheck,Scheck);
	Tmove = Smove;
	Tcontact = Scontact;
	strcpy(Tindir,Sindir);
	strcpy(Ttaxis,Staxis);
	strcpy(Tfeed,Sfeed);
/*
.....Initialize Conditions
*/
	Scdir = Tcdir = GOFWD;
	Scdrive[0] = Tcdrive[0] = '\0';
	Sccon = Tccon = AUTO;
	strcpy(Scint,"1."); strcpy(Tcint,Scint);
	Scnpt[0] = Tcnpt[0] = '\0';
	Sclist.num_col = 5;
	Sclist.num_item = 0;
	Sclist.answer = 0;
	Sclist.data = UU_NULL;
	Sclist.col_label = (char **)uu_malloc(Sclist.num_col*sizeof(char*));
	for (i=0;i<Sclist.num_col;i++)
		Sclist.col_label[i] = (char *)uu_malloc(20*sizeof(char));
	strcpy (Sclist.col_label[0],"Direction");
	strcpy (Sclist.col_label[1],"Drive");
	strcpy (Sclist.col_label[2],"CS Condition");
	strcpy (Sclist.col_label[3],"Intof");
	strcpy (Sclist.col_label[4],"Nearpt");
	Ttoend = 0;
	Tafter = 0;
		
	if (Ttype == GOFWDA)
		Ttoend = 1;
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
	Sgeo_init[0] = Sgeo_init[1] = Sgeo_init[2] = UU_FALSE;
	Sgds.color = Sgps.color = Sgcs.color = -1;
	Sgds.label_on = Sgps.label_on = Sgcs.label_on = -1;
	Sgds.key = Sgps.key = Sgcs.key = 0;
	Sgds.label[0] = Sgps.label[0] = Sgcs.label[0] = '\0';
	if (pskey != 0)
	{
		Sgps.key = pskey;
		status = ncl_get_label_with_key(Sgps.key,Tpart);
		S_init_geo(&Sgps,Tpart,Tpscol);
		S_push_geo(&Splst,&Sgps,&Sgeo_init[1]);
	}
	Tgps = Sgps;
	S_init_geo(&Sgds,Tdrive,Tdscol);
	S_init_geo(&Sgps,Tpart,Tpscol);
	S_init_geo(&Sgcs,Tcheck,Tcscol);
/*
.....Erase the motion &
.....Display the current cutter position
*/
/*
......don't erase motion now test
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
	Sfrm = 0;
/*
.....Initial entry
.....Set standard colors
*/
	if (!Serrflg)
	{
		Ssecol[Contour] = Sred;
		Ssecol[Options] = Sblack;
		Ssecol[Conditions] = Sblack;
/*
.....Set section colors
*/
		S_section_changed(Contour,UU_FALSE);
		S_section_changed(Options,UU_FALSE);
		S_section_changed(Conditions,UU_FALSE);
	}
//yurong
//	else
//		S_enable_buttons();
/*
.....Set the field traversals
*/
//	ud_set_traverse_mask(FGFD4,Tgtfdav&&(Tgtfmode!=0));
	ud_set_traverse_mask(FGFD4,Tgtfdav);
/*
.....Set the pick buttons to correct color
*/
	ud_dispfrm_set_attribs(0,FGPS,UM_BLACK,Tpscol);
/*
.....Set the mandatory fields to red
*/
	if (UU_LIST_LENGTH(&Sdlst) == 0) 
	{
		ud_dispfrm_set_attribs(0,FGDRV,UM_WHITE,UM_RED);
		ud_frm_enable_ok(UU_FALSE);
	}
	else
		ud_dispfrm_set_attribs(0,FGDRV,UM_BLACK,Tdscol);
	if (strlen(Tcheck) == 0 && Tpscon != NOPS)
	{
		ud_dispfrm_set_attribs(0,FGCHK,UM_WHITE,UM_RED);
		ud_frm_enable_ok(UU_FALSE);
	}
	else
		ud_dispfrm_set_attribs(0,FGCHK,UM_BLACK,Tcscol);
//Yurong
//add here for all
	S_enable_buttons();
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
	Sdscol = Tdscol;
	Spscol = Tpscol;
	Scscol = Tcscol;
	Sd1col = Td1col;
	Sc1col = Tc1col;

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
		sfpt->label_on = -1;
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
				sfpt->label_on = -1;
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

static void S_reset_entity_label(sfpt)
UM_sgeo *sfpt;
{
	int status;
	struct NCL_fixed_databag e;
	struct UC_attributedatabag attr;

	if (sfpt->label_on != -1)
	{
		if (sfpt->key != 0)
		{
			e.key = sfpt->key;
			if (ncl_retrieve_data_fixed(&e) != 0) return;
			if (ncl_label_type(e.rel_num) == UU_SUCCESS)
			{
				attr.key = e.key;
				status = ur_retrieve_attr(&attr);
				if (status == 0)
				{
					if (sfpt->label_on==0)
					{
						uw_nt_hlt_gfont();
						ncl_set_label_on(&attr.label_on,0);
					}
					else
					{
						uw_nt_reset_hlt_gfont();
						ncl_set_label_on(&attr.label_on,1);
					}
					ur_update_attr(&attr); 
				}
			}
			uc_display(&e);
			ug_wflush();
		}
	}
	sfpt->label_on = -1;
	uw_nt_reset_hlt_gfont();
}

static void S_entity_label_on(sfpt)
UM_sgeo *sfpt;
{
	int status, savclr;
	struct NCL_fixed_databag e;
	struct UC_attributedatabag attr;

	if (sfpt->key != 0)
	{
		e.key = sfpt->key;
		if (ncl_retrieve_data_fixed(&e) != 0) return;
		if (ncl_label_type(e.rel_num) == UU_SUCCESS)
		{
			attr.key = e.key;
			status = ur_retrieve_attr(&attr);
			if (status == 0)
			{
				if (ncl_get_label_on(attr.label_on))
					sfpt->label_on = 1;
				else
					sfpt->label_on = 0;
				ncl_set_label_on(&attr.label_on,1);
				ur_update_attr(&attr); 
			}
			savclr = UW_label_clr;
			UW_label_clr = 3;
			uw_nt_hlt_gfont();
			uc_display(&e);
			ug_wflush();
			UW_label_clr = savclr;
		}
	}
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
**    I_FUNCTION     :  S_delete_geo(glst,kinc,init)
**       Pushes a picked geometry structure onto the geometry list.
**    PARAMETERS
**       INPUT  :
**          glst     List to delete geometry from.
**          kinc     Geometry item # to delete from list.
**          init     UU_TRUE = list is already initialized.
**       OUTPUT : none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_delete_geo(glst,kinc,init)
UU_LIST *glst;
int kinc;
UU_LOGICAL *init;
{
/*
.....Delete item from the list
*/
	if (!(*init) || kinc >= UU_LIST_LENGTH(glst)) return;
	uu_list_delete(glst,kinc,1);
}
/*********************************************************************
**    I_FUNCTION     :  S_modify_table(entry_indx)
**       update form table: delete the entry_indx (to end if the Ttoend is on)
**    PARAMETERS
**       INPUT  :
**          entry_indx: current select entity index
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_modify_table(entry_indx)
int entry_indx;
{
	int i,j,nc;
	UD_TLIST Temp_clist;
	List_struct *Temp_cents;

	if (Scents == UU_NULL)
		return;
	if (entry_indx == -1)
		return;
	if (Ttoend)
		Temp_clist.num_item = entry_indx;
	else
		Temp_clist.num_item = Sclist.num_item - 1;
	Temp_clist.num_col = 5;
	
	if (Ttoend)
		Temp_clist.answer = Sclist.answer-1;
	else
	{
		if (Sclist.answer>=Temp_clist.num_item)
			Sclist.answer = Temp_clist.num_item - 1;
		Temp_clist.answer = Sclist.answer;
	}
	Temp_clist.col_label = (char **)uu_malloc(Temp_clist.num_col*sizeof(char*));
	for (i=0;i<Temp_clist.num_col;i++)
		Temp_clist.col_label[i] = (char *)uu_malloc(20*sizeof(char));
	strcpy (Temp_clist.col_label[0],"Direction");
	strcpy (Temp_clist.col_label[1],"Drive");
	strcpy (Temp_clist.col_label[2],"CS Condition");
	strcpy (Temp_clist.col_label[3],"Intof");
	strcpy (Temp_clist.col_label[4],"Nearpt");
	if (Temp_clist.num_item>0)
		Temp_clist.data =
				(UD_ITEMDATA *)uu_malloc(Temp_clist.num_item*sizeof(UD_ITEMDATA));
	else
		Temp_clist.data = UU_NULL;
	Temp_cents = (List_struct *)uu_malloc(Temp_clist.num_item*sizeof(List_struct));
	for (i=0,j=0;i<Sclist.num_item&&j<Temp_clist.num_item;i++) 
	{
		if (i==entry_indx)
		{
			if (Ttoend)
				break;
			else
				continue;
		}
		Temp_clist.data[j].itemnum = Sclist.data[i].itemnum;
		Temp_clist.data[j].data_items =
				(char **)uu_malloc(Sclist.num_col*sizeof(char*));

		Temp_cents[j].direction = Scents[i].direction;

		nc = 65;
		Temp_clist.data[j].data_items[0] = (char *)uu_malloc(nc*sizeof(char));
		strcpy(Temp_cents[j].drive, Scents[i].drive); 
	
		nc = 65;
		Temp_clist.data[j].data_items[1] = (char *)uu_malloc(nc*sizeof(char));
		Temp_cents[j].cscon = Scents[i].cscon;
	
		nc = 65;
		Temp_clist.data[j].data_items[2] = (char *)uu_malloc(nc*sizeof(char));
		strcpy(Temp_cents[j].intof, Scents[i].intof); 

		nc = 65;
		Temp_clist.data[j].data_items[3] = (char *)uu_malloc(nc*sizeof(char));
		strcpy(Temp_cents[j].nearpt, Scents[i].nearpt); 
		
		nc = 65;
		Temp_clist.data[j].data_items[4] = (char *)uu_malloc(nc*sizeof(char));
	
		strcpy(Temp_clist.data[j].data_items[0], Sclist.data[i].data_items[0]);
		strcpy(Temp_clist.data[j].data_items[1], Sclist.data[i].data_items[1]);
		strcpy(Temp_clist.data[j].data_items[2], Sclist.data[i].data_items[2]);
		strcpy(Temp_clist.data[j].data_items[3], Sclist.data[i].data_items[3]);
		strcpy(Temp_clist.data[j].data_items[4], Sclist.data[i].data_items[4]);
		j++;
	}
	if (Scents != UU_NULL) 
		uu_free(Scents);
	ud_free_tlist(&Sclist);
	ud_tlist_copy(&Temp_clist, &Sclist);
	ud_free_tlist(&Temp_clist);
			
	Scents = (List_struct *)uu_malloc(Sclist.num_item*sizeof(List_struct));
	for (i=0;i<Sclist.num_item;i++)
	{
		Scents[i].direction = Temp_cents[i].direction;
		Scents[i].cscon = Temp_cents[i].cscon;
		strcpy(Scents[i].drive, Temp_cents[i].drive);
		strcpy(Scents[i].intof, Temp_cents[i].intof);
		strcpy(Scents[i].nearpt, Temp_cents[i].nearpt);
	}
/*
.....Update the form list
*/
	Slast_entry = Sclist.answer;
	ud_update_answer(FCLST,(int *)&Sclist);
	S_update_table(UU_FALSE);
}

/*********************************************************************
**    I_FUNCTION     :  S_create_table(glst)
**       Places all picked geometry onto the Conditions form table.
**    PARAMETERS
**       INPUT  :
**          glst     List containing geometry to place in the form table.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_create_table(glst)
UU_LIST *glst;
{
	int i,nc;
	UM_sgeo *sgeo;
/*
.....Free the existing list
*/
	Screate_table = UU_TRUE;
	if (Scents != UU_NULL) uu_free(Scents);
	Scents = UU_NULL; Slast_entry = -1;
	if (Sclist.data != UU_NULL)
	{
		for (i=0;i<Sclist.num_item;i++) uu_free(Sclist.data[i].data_items);
		uu_free(Sclist.data);
	}
	Sclist.data = UU_NULL;
	Sclist.answer = 0;
	Slast_entry = -1;
/*
.....Load up the current lis
*/
	sgeo = (UM_sgeo *)UU_LIST_ARRAY(glst);
	Sclist.num_item = UU_LIST_LENGTH(glst);
	if (Sclist.num_item > 0)
	{
		Sclist.data =
			(UD_ITEMDATA *)uu_malloc(Sclist.num_item*sizeof(UD_ITEMDATA));
		Scents = (List_struct *)uu_malloc(Sclist.num_item*sizeof(List_struct));
		for (i=0;i<Sclist.num_item;i++)
		{
			Sclist.data[i].itemnum = Sclist.num_col;
			Sclist.data[i].data_items =
				(char **)uu_malloc(Sclist.num_col*sizeof(char*));

			Scents[i].direction = 0;
			nc = 65;
			Sclist.data[i].data_items[0] = (char *)uu_malloc(nc*sizeof(char));

			strcpy(Scents[i].drive,sgeo[i].label); nc = 65;
			Sclist.data[i].data_items[1] = (char *)uu_malloc(nc*sizeof(char));

			if (i == Sclist.num_item-1) Scents[i].cscon = Tcscon + 1;
			else Scents[i].cscon = 0;
			nc = 65;
			Sclist.data[i].data_items[2] = (char *)uu_malloc(nc*sizeof(char));

			strcpy(Scents[i].intof,"1"); nc = 65;
			Sclist.data[i].data_items[3] = (char *)uu_malloc(nc*sizeof(char));

			Scents[i].nearpt[0] = '\0'; nc = 65;
			Sclist.data[i].data_items[4] = (char *)uu_malloc(nc*sizeof(char));

			S_format_table(i);
		}
	}
	else
		Sclist.data = UU_NULL;
/*
.....Update the form list
*/
	ud_update_answer(FCLST,(int *)&Sclist);
	S_update_table(UU_FALSE);
/*
.....Mark the Conditions section as unchanged
*/
	Ssecol[Conditions] = Sblack;
	S_section_changed(Conditions,UU_FALSE);
	Screate_table = UU_FALSE;
}

/*********************************************************************
**    I_FUNCTION     :  S_update_table(flag)
**       Updates the Conditions section with the selected Drive Surface
**       from the list.
**    PARAMETERS
**       INPUT  :
**          flag   = UU_TRUE = Highlight geometry with current Drive and
**                   Check Surface colors.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_update_table(flag)
UU_LOGICAL flag;
{
	UM_sgeo *sgeo;
	int chg;

	if (Sfrm==-1)
		return;
/*
.....Update last selected entry
*/
	if (Slast_entry != -1 && Slast_entry != Sclist.answer)
	{
		Scents[Slast_entry].direction = Tcdir;
		strcpy(Scents[Slast_entry].drive,Tcdrive);
		Scents[Slast_entry].cscon = Tccon;
		strcpy(Scents[Slast_entry].intof,Tcint);
		strcpy(Scents[Slast_entry].nearpt,Tcnpt);
		chg = S_format_table(Slast_entry);
		if (chg)
			ud_update_answer(FCLST,&Sclist);
		if (flag && !Screate_table)
		{
			sgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sdlst);
			S_hilite_entity(&(sgeo[Slast_entry]),Tdscol);
			if (Slast_entry < UU_LIST_LENGTH(&Sdlst)-1)
				S_hilite_entity(&(sgeo[Slast_entry+1]),Tdscol);
		}
	}
/*
.....Update the Conditions fields
*/
	if (Sclist.num_item > 0)
	{
		if (Sclist.answer>=0)
		{
			Slast_entry = Sclist.answer;
		}
		else
		{
/*
.....user click on empty space of listing, no selection
.....we updated the last select entry already, now we will
.....stay the selecting in this case
*/
			Sclist.answer = Slast_entry;
			ud_update_answer(FCLST,&Sclist);
		}
		Tcdir = Scents[Sclist.answer].direction;
		strcpy(Tcdrive,Scents[Sclist.answer].drive);
		Tccon = Scents[Sclist.answer].cscon;
		strcpy(Tcint,Scents[Sclist.answer].intof);
		strcpy(Tcnpt,Scents[Sclist.answer].nearpt);
	}
	ud_update_answer(FCDIR,&Tcdir);
	ud_update_answer(FCDRV,(int *)Tcdrive);
	ud_update_answer(FCCON,&Tccon);
	ud_update_answer(FCINT,(int *)Tcint);
	ud_update_answer(FCNPT,(int *)Tcnpt);
/*
.....Update the Conditions fields
*/
	if (flag && !Screate_table)
	{
		sgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sdlst);
		S_hilite_entity(&(sgeo[Slast_entry]),Td1col);
		if (Slast_entry < UU_LIST_LENGTH(&Sdlst)-1)
			S_hilite_entity(&(sgeo[Slast_entry+1]),Tc1col);
	}
}

/*********************************************************************
**    I_FUNCTION     :  S_format_table(kinc)
**       Formats an entry in the Conditions table.
**    PARAMETERS
**       INPUT  :
**          kinc     Table entry to format.
**       OUTPUT :
**          none
**    RETURNS      : 1: table value changed
**					0: table value not changed
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_format_table(kinc)
int kinc;
{
	int chg = 0;
/*
.....Format table entry
*/
	if (stricmp(Sclist.data[kinc].data_items[0],Sdirs[Scents[kinc].direction])!=0)
	{
		strcpy(Sclist.data[kinc].data_items[0],Sdirs[Scents[kinc].direction]);
		chg = 1;
	}
	if (stricmp(Sclist.data[kinc].data_items[1],Scents[kinc].drive)!=0)
	{
		strcpy(Sclist.data[kinc].data_items[1],Scents[kinc].drive);
		chg = 1;
	}
	if (stricmp(Sclist.data[kinc].data_items[2],Scons[Scents[kinc].cscon])!=0)
	{
		strcpy(Sclist.data[kinc].data_items[2],Scons[Scents[kinc].cscon]);
		chg = 1;
	}
	if (stricmp(Sclist.data[kinc].data_items[3], Scents[kinc].intof)!=0)
	{
		strcpy(Sclist.data[kinc].data_items[3],Scents[kinc].intof);
		chg = 1;
	}
	if (stricmp(Sclist.data[kinc].data_items[4],Scents[kinc].nearpt)!=0)
	{
		strcpy(Sclist.data[kinc].data_items[4],Scents[kinc].nearpt);
		chg = 1;
	}
	return chg;
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
	int savclr;
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
			savclr = UW_label_clr;
			if (sfpt->label_on!=-1)
			{
				UW_label_clr = 3;
				uw_nt_hlt_gfont();
			}
			uc_display(&e);
			ug_wflush();
			UW_label_clr = savclr;
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
	S_unhilite_entities(&Sdlst,&Sgeo_init[0]);
	S_unhilite_entities(&Splst,&Sgeo_init[1]);
	S_unhilite_entities(&Sclst,&Sgeo_init[2]);
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
		if (sflst == &Sdlst) S_create_table(sflst);
		*geo_init = UU_FALSE;
	}
}
			
/*********************************************************************
**    S_FUNCTION     :  S_remove_ds_label
**       Remove hilight labels.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_remove_ds_label()
{		
	UM_sgeo *sgeo, *sfpt;
	int i, status;
	struct NCL_fixed_databag e;
	struct UC_attributedatabag attr;

	sgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sdlst);
	for (i=0;i<UU_LIST_LENGTH(&Sdlst);i++) 
	{
		sfpt = &sgeo[i];
		if (sfpt->label_on != -1)
		{
			if (sfpt->key != 0)
			{
				e.key = sfpt->key;
				if (ncl_retrieve_data_fixed(&e) != 0) return;
				if (ncl_label_type(e.rel_num) == UU_SUCCESS)
				{
					attr.key = e.key;
					status = ur_retrieve_attr(&attr);
					if (status == 0)
					{
						if (sfpt->label_on==0)
						{
							uw_nt_hlt_gfont();
							ncl_set_label_on(&attr.label_on,0);
						}
						else
						{
							uw_nt_reset_hlt_gfont();
							ncl_set_label_on(&attr.label_on,1);
						}
						ur_update_attr(&attr); 
					}
				}
				uc_display(&e);
				ug_wflush();
			}
		}
		sfpt->label_on = -1;
	}
	uw_nt_reset_hlt_gfont();
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
	int savclr;
	struct NCL_fixed_databag e;

	if (sfpt->key == 0)
		return;
/*
.....reset label here too
*/
	S_reset_entity_label(sfpt);
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
			savclr = UW_label_clr;
			if (sfpt->label_on!=-1)
			{
				UW_label_clr = 3;
				uw_nt_hlt_gfont();
			}
			uc_display(&e);
			ug_wflush();
			UW_label_clr = savclr;
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
	if (Sfrm==-1)
		return;
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
	n = UU_LIST_LENGTH(&Sdlst);
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sdlst);
	for (i=0;i<n;i++) S_add_key(which,mgeo[i].key);

	n = UU_LIST_LENGTH(&Splst);
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(&Splst);
	for (i=0;i<n;i++) S_add_key(which,mgeo[i].key);

	n = UU_LIST_LENGTH(&Sclst);
	mgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sclst);
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
	if (Tmove == 1)
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
	int i,relnum,nc;
	UU_LOGICAL psfl,dsfl,csfl,fdfl, ifl;
	UM_sgeo *sgeo;
	UU_REAL rval;
/*
.....Determine type of Drive Surface(s) selected
*/
	nc = 0;
	if (Sgeo_init[0]) nc = UU_LIST_LENGTH(&Sdlst);
	if (nc == 0) Sdstype = 0;
	else
	{
		sgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sdlst);
		if (nc == 1)
		{
			Sdstype = 1;
			ur_retrieve_data_relnum(sgeo[0].key,&relnum);
			if (relnum == UM_COMPCRV_REL && Ttype == GOFWDA) Sdstype = 2;
			else if (uc_super_class(relnum) == UC_CURVE_CLASS) Sdstype = 3;
		}
		else
		{
////////ken
			Sdstype = 1;
			ur_retrieve_data_relnum(sgeo[0].key,&relnum);
			if (relnum == UM_COMPCRV_REL && Ttype == GOFWDA) Sdstype = 2;
			else if (uc_super_class(relnum) == UC_CURVE_CLASS) Sdstype = 3;
			for (i=0;i<nc;i++)
			{
				ur_retrieve_data_relnum(sgeo[i].key,&relnum);
				if (relnum == UM_COMPCRV_REL && Ttype == GOFWDA) Sdstype = 2;
				else if (uc_super_class(relnum) == UC_CURVE_CLASS) 
				{
					Sdstype = 3;
					break;
				}
			}
/////////
/*
.....no matter if include a surface or not, the contact mode will active
.....Ken suggest
*/
/*			Sdstype = 3;
			for (i=0;i<nc;i++)
			{
				ur_retrieve_data_relnum(sgeo[0].key,&relnum);
				if (uc_super_class(relnum) == UC_SURFACE_CLASS) Sdstype = 1;
			}
*/		}
	}
/*
.....Set PS field
*/
	psfl = UU_FALSE;
	if (Tpscon == AUTOPS || Tpscon == NOPS) psfl = UU_TRUE;
	else if (Sgeo_init[1] && UU_LIST_LENGTH(&Splst) != 0) psfl = UU_TRUE;
/*
.....Set DS field
*/
	dsfl = Sdstype != 0;
/*
.....Set CS field
*/
	csfl = (Sgeo_init[2] && UU_LIST_LENGTH(&Sclst) != 0);

	if (Tpscon == NOPS) csfl = UU_TRUE;
	if (Sdstype == 2) csfl = UU_TRUE;
/*
.....Define button colors
*/
	if (psfl) ud_dispfrm_set_attribs(0,FGPS,UM_BLACK,Tpscol);
	else ud_dispfrm_set_attribs(0,FGPS,UM_WHITE,UM_RED);

	if (dsfl) ud_dispfrm_set_attribs(0,FGDRV,UM_BLACK,Tdscol);
	else ud_dispfrm_set_attribs(0,FGDRV,UM_WHITE,UM_RED);

	if (csfl) ud_dispfrm_set_attribs(0,FGCHK,UM_BLACK,Tcscol);
	else ud_dispfrm_set_attribs(0,FGCHK,UM_WHITE,UM_RED);
	
	fdfl = UU_TRUE;	
	nc = (int)strlen(Tfeed);
	ul_strip_blanks(Tfeed, &nc);
	if (nc<=0)
	{
		ud_dispfrm_set_attribs(0,FGFD2,UM_WHITE,UM_RED);
		fdfl = UU_FALSE;
	}
	else
	{
		rval = atof(Tfeed);
		if (rval<=0)
			fdfl =  UU_FALSE;
		if (fdfl) 
		{
			ud_dispfrm_set_attribs(0, FGFD2, UM_BLACK, UM_WHITE);
		}
		else 
		{
			ud_dispfrm_set_attribs(0, FGFD2, UM_WHITE,UM_RED);
		}
	}
/*
.....Set section color
*/
	if (psfl && dsfl && csfl&&fdfl)
	{
		Ssecol[Contour] = Sgreen; S_section_changed(Contour,UU_TRUE);
	}
	else
	{
		Ssecol[Contour] = Sred; S_section_changed(Contour,UU_FALSE);
	}
/*
.....Set Contact On traversal
*/
	ifl = Sdstype == 3;
	ud_set_traverse_mask(FOCON,ifl);
/*
.....Set Action Buttons
*/
	ifl = psfl && dsfl && csfl&&fdfl;
	ud_frm_enable_ok(ifl);
	ud_set_traverse_mask(FAPV,ifl);
	ud_set_traverse_mask(FAPY,ifl);
	ud_set_traverse_mask(FARS,ifl);
	ud_set_traverse_mask(FAGE,ifl);
	return(csfl);
}

/*********************************************************************
**    I_FUNCTION     : S_display_ds()
**			Selects the Drive Surface from the list table and highlights
**       the active drive and check surfaces after a command failure.
**    PARAMETERS   
**       INPUT  :
**			   none.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS :
**			Saves and restores motion parameters when previewing the command.
**			The motion will remain displayed though.
**    WARNINGS     : none
*********************************************************************/
static void S_display_ds()
{
	int i;
	UU_KEY_ID dskey,pskey,cskey;
	UM_sgeo *sgeo;
/*
.....Get the active drive surface key
*/
	obdsrf(&dskey,&pskey,&cskey);
	sgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sdlst);
	for (i=0;i<UU_LIST_LENGTH(&Sdlst);i++)
	{
		if (dskey == sgeo[i].key)
		{
			Sclist.answer = i;
			S_update_table(UU_TRUE);
			break;
		}
	}
}

/*********************************************************************
**    I_FUNCTION     : S_build_command(flag)
**			Builds and outputs the GOFWDA and GO--- command.
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
	int i,nc,ifl,ndrv,nck,ist,ival,dtype,status,maxchk;
	char sbuf[80];
	NCL_cmdbuf cmdbuf;
	UM_sgeo *sgeo,*cgeo;
	char cfed[16][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	char caxis[6][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	char maxis[12][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	NCLX_mot_feedrate fedrat;
	NCLX_mot_fillet fillet;
	NCLX_mot_tlaxis tlaxis;
	static char *ps[]={NCL_tlofps,NCL_tlonps,NCL_autops,NCL_nops};
	static char *ds[]={NCL_tllft,NCL_tlon,NCL_tlrgt};
	static char *cs[]={NCL_to,NCL_on,NCL_past,NCL_tanto,NCL_pstan};
	static char *dir[]={NCL_gofwd,NCL_golft,NCL_gorgt,NCL_goback,NCL_goup,
		NCL_godown};
	if (flag)
		NCL_preview_mot = 0;
	else
		NCL_preview_mot = 1;
/*
.....Initialize routine
*/
	dtype = Ttype;
	sgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sdlst);
	ndrv = UU_LIST_LENGTH(&Sdlst);
	nck = UU_LIST_LENGTH(&Sclst);
/*
.....Initialize command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
/*
.....Make sure only a single curve selected
.....with NOPS setting
*/
	if (Tpscon == NOPS)
	{
		if (ndrv != 1) goto curve_required;
		if (uc_super_class(sgeo[0].relnum) != UC_CURVE_CLASS) goto curve_required;
		dtype = EXPLICIT;
	}
/*
.....Only allow 5 CS for GOFWDA &
.....1 for GOFWD
*/
	maxchk = (Ttype == GOFWDA) ? 5:1;
	if (nck > maxchk) goto too_many_checks;
/*
.....Output Assist commands
*/
	ncl_init_cmdbuf(&cmdbuf);
	if (Sassfl)
	{
		nclu_tool_calc_set(&Smaxdp,Siter,&Sthick,&Stoler,Sitoler,Sgougck,caxis);
		nclu_tool_calc_command(&cmdbuf,&Smaxdp,Siter,&Sthick,&Stoler,Sitoler,
			Sgougck,caxis,UU_TRUE,flag);
	}
/*
.....Output advanced Tlaxis command
*/
	if (Ttaxav && Staxfl)
	{
		nclu_tlaxis_set_tlaxis(&tlaxis,caxis,maxis);
		nclu_tlaxis_command(&cmdbuf,&tlaxis,caxis,maxis,UU_TRUE,flag);
	}
/*
.....Output standard Tlaxis command
*/
	else if (strcmp(Staxis,Ttaxis) != 0)
	{
		if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
		ncl_add_token(&cmdbuf,NCL_tlaxis,NCL_nocomma);
		ncl_add_token(&cmdbuf,Ttaxis,NCL_comma);
		ncl_add_cmdbuf(&cmdbuf);
	}
/*
.....Output advanced feedrate command if not Preview
*/
	if (Tgtfdav && Sfedfl && flag)
	{
		nclu_fedrat_set_fedrat(&fedrat,cfed);
		nclu_fedrat_command(&cmdbuf,&fedrat,cfed,UU_TRUE,flag);
	}
/*
..... Output simple FEDRAT/value command if not Preview
*/
	else
	{
		if (!Tgtfdav)    
		{   
//			if (Tgtfmode == 1 && strcmp(Sfeed,Tfeed) != 0 && flag)
			if (strcmp(Sfeed,Tfeed) != 0 && flag)
			{
				ncl_add_token(&cmdbuf,NCL_fedrat,NCL_nocomma);
				ncl_add_token(&cmdbuf,Tfeed,NCL_nocomma);
				ncl_add_cmdbuf(&cmdbuf);
			}
		}
	}
/*
.....Output ARCSLP command
*/
	if (Sfilfl)
	{
		nclu_arcslp_set_fillet(&fillet,caxis);
		nclu_arcslp_command(&cmdbuf,&fillet,caxis,UU_TRUE,flag);
	}
/*
.....Output Contact command
*/
	if (Tcontact != Scontact)
	{
		if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
		if (Tcontact == 0) ncl_add_token(&cmdbuf,NCL_contct_off,NCL_nocomma);
		else ncl_add_token(&cmdbuf,NCL_contct_on,NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
	}
/*
.....Output INDIR command
*/
	nc = strlen(Tindir); ul_strip_blanks(Tindir,&nc);
	if (Tmove != 0 && nc != 0)
	{
		if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
		ncl_add_token(&cmdbuf,NCL_indirv,NCL_nocomma);
		ncl_add_token(&cmdbuf,Tindir,NCL_comma);
		ncl_add_cmdbuf(&cmdbuf);
	}
/*
.....PS condition
*/
//	if (Tpscon != Spscon || Tpscon == AUTOPS || Tpscon == NOPS || (!flag))
	if (Tpscon != Spscon || (!flag))
	{
		if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
		ncl_add_token(&cmdbuf, ps[Tpscon], NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
	}
/*
.....PSIS
*/
	if (Tpscon != AUTOPS && Tpscon != NOPS)
	{
		sgeo = (UM_sgeo *)UU_LIST_ARRAY(&Splst);
		nc = UU_LIST_LENGTH(&Splst);
		if (nc > 0 && (nc > 1 || Sgps.key != Tgps.key))
		{
			if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
			ncl_add_token(&cmdbuf, NCL_psis, NCL_nocomma);
			if (nc == 1)
			{
				ncl_add_token(&cmdbuf,sgeo[0].label,NCL_nocomma);
			}
			else
			{
				ncl_add_token(&cmdbuf,"(",NCL_nocomma);
				ncl_add_token(&cmdbuf,NCL_sf,NCL_nocomma);
				ifl = NCL_comma;
				for (i=0;i<nc;i++)
				{
					if (i+1 == nc) ifl = NCL_nocomma;
					ncl_add_token(&cmdbuf, sgeo[i].label, ifl);
				}
				ncl_add_token(&cmdbuf,")",NCL_nocomma);
			}
			ncl_add_cmdbuf(&cmdbuf);
		}
	}
/*
.....Output RAPID command
*/
/*
.....no RAPID per KEN
*/
/*
	if (!Tgtfdav)
	{
		if (Tgtfmode == 1)
		{
			if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
			ncl_add_token(&cmdbuf,NCL_rapid,NCL_comma);
			ncl_add_cmdbuf(&cmdbuf);
		}
	}
*/
/*
.....DS condition
*/
	if ((Tdscon != Sdscon)||(!flag))
	{
		if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
		ncl_add_token(&cmdbuf, ds[Tdscon], NCL_nocomma);
		ncl_add_cmdbuf(&cmdbuf);
	}
/*
.....GOFWDA command
*/
	sgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sdlst);
	if (dtype == GOFWDA)
	{
		if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_gofwda, NCL_nocomma);
	}
	cgeo = (UM_sgeo *)UU_LIST_ARRAY(&Sclst);
/*
.....Single composite curve being driven
*/
	if (Sdstype == 2)
	{
		S_look_avoid(&cmdbuf);
		ncl_add_token(&cmdbuf, sgeo[0].label, NCL_comma);
	}
/*
.....1st Drive Surface
*/
	else
	{
		ist = 0;
		if (dtype == GOFWDA)
		{
			ncl_add_token(&cmdbuf, sgeo[0].label, NCL_comma);
			if (ndrv > 1)
			{
				S_look_avoid(&cmdbuf);
				ist = 1;
			}
		}
/*
.....Rest of Drive Surfaces
*/
		for (i=ist;i<ndrv;i++)
		{
/*
........Tool direction
*/
			if (dtype != GOFWDA)
			{
				if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
				ncl_add_token(&cmdbuf, dir[Scents[i].direction], NCL_nocomma);
			}
/*
........Drive surface
*/
			if (dtype != GOFWDA || i != 0)
				ncl_add_token(&cmdbuf, sgeo[i].label, NCL_comma);
/*
........Start of specified check surfaces
*/
			if (dtype == GOFWDA && i == ndrv-1)
				ncl_add_token(&cmdbuf, NCL_check, NCL_comma);
/*
........Next drive surface condition
*/
			if (Tpscon != NOPS)
			{
				if (i == ndrv-1)
				{
					ival = Tcscon;
					ncl_add_token(&cmdbuf, cs[ival], NCL_comma);
				}
				else if (dtype == EXPLICIT)
				{
					ival = Scents[i].cscon - 1;
					if (ival == -1) ival = 0;
					ncl_add_token(&cmdbuf, cs[ival], NCL_comma);
				}
				else if (dtype == GOFWDA)
				{
					ival = Scents[i].cscon - 1;
					if (ival != -1) ncl_add_token(&cmdbuf, cs[ival], NCL_comma);
				}
/*
........Intof
*/
				if (dtype == GOFWDA || dtype == EXPLICIT)
				{
					if (strcmp(Scents[i].intof,"1") != 0)
					{
						ncl_add_token(&cmdbuf,Scents[i].intof,NCL_comma);
						ncl_add_token(&cmdbuf,NCL_intof,NCL_comma);
					}
				}
/*
........Check surface
*/
				if (dtype == EXPLICIT || i == ndrv-1)
				{
					if (i == ndrv-1)
						ncl_add_token(&cmdbuf,cgeo[0].label,NCL_comma);
					else
						ncl_add_token(&cmdbuf,sgeo[i+1].label,NCL_comma);
/*
........Near point
*/
					if (dtype != IMPLIED)
					{
						if (strlen(Scents[i].nearpt) != 0)
						{
							ncl_add_token(&cmdbuf,NCL_nearpt,NCL_comma);
							ncl_add_token(&cmdbuf,Scents[i].nearpt,NCL_comma);
						}
					}
				}
			}
/*
........Output command
*/
			if (dtype == IMPLIED || dtype == EXPLICIT)
			{
				ncl_add_cmdbuf(&cmdbuf);
				ncl_set_cmdmode(UU_TRUE);
				if (Spreviewflg && !Schgmade)
					ncl_call_input(&cmdbuf,UU_TRUE);
				else
				{
					Serrflg = ncl_call(&cmdbuf);
					if (Serrflg != 0) goto failed;
				}
				ncl_init_cmdbuf(&cmdbuf);
			}
		}
	}
/*
.....GOFWD Check surfaces
*/
	if (dtype == GOFWDA)
	{
		if (nck > 1)
		{
			for (i=1;i<nck;i++)
				ncl_add_token(&cmdbuf, sgeo[i].label, NCL_comma);
		}
		ncl_add_cmdbuf(&cmdbuf);
/*
.....Call the command
*/
		ncl_set_cmdmode(UU_TRUE);
/*
.....Insert the command at the current line, but do not execute
.......Only need to write the command if Preview was already pressed
.......and no changes were made since the command was already executed
*/
		if (Spreviewflg && !Schgmade)
			ncl_call_input(&cmdbuf,UU_TRUE);
/*
.....Execute command if a change has been made since the last preview or no
.....preview was created
*/
		else
		{
			Serrflg = ncl_call(&cmdbuf);
			if (Serrflg != 0) goto failed;
		}
	}
	status = UU_SUCCESS;
	goto done;
/*
.....Single curve required with NOPS
*/
curve_required:
	ud_wrerr("A single curve is required with NOPS.");
	status = UU_FAILURE;
	goto done;
/*
.....Too many checks
*/
too_many_checks:
	sprintf(sbuf,"A maximum of %d Check Surface(s) can be specified.",maxchk);
	ud_wrerr(sbuf);
	status = UU_FAILURE;
	goto done;
/*
.....Command Failed
*/
failed:;
	status = UU_SUCCESS;
/*
.....End of routine
*/
done:;
	if (status == UU_FAILURE) Serrflg = UU_TRUE;
	NCL_preview_mot = 1;
	return(status);
}

/*********************************************************************
**    I_FUNCTION     : S_look_avoid(cmdbuf)
**			Adds the LOOK and AVOID qualifiers to the GOFWDA command.
**    PARAMETERS   
**       INPUT  : cmdbuf  = NCL command buffer.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS :
**			none
**    WARNINGS     : none
*********************************************************************/
static void S_look_avoid(cmdbuf)
NCL_cmdbuf *cmdbuf;
{
	char sbuf[80];
/*
.....LOOK modifier
*/
	if (Tlook != 0)
	{
		ncl_add_token(cmdbuf, NCL_look, NCL_comma);
		sprintf(sbuf,"%d",Tlook+1);
		ncl_add_token(cmdbuf, sbuf, NCL_comma);
	}
/*
.....AVOID modifier
*/
	if (Textens == 0)
		ncl_add_token(cmdbuf, NCL_avoid, NCL_comma);
	else
		ncl_add_token(cmdbuf, NCL_find, NCL_comma);
}
