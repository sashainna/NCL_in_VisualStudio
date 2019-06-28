/********************************************************************* 
**    NAME         :  nugo.c
**       CONTAINS:
**		             nclu_go()
**
**    COPYRIGHT 2015 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nugo.c , 26.3
**    DATE AND TIME OF LAST MODIFICATION
**       09/20/18 , 11:39:04
*********************************************************************/
#include <string.h>
#include "class.h"
#include "dselmask.h"
#include "nclmplay.h"
#include "mdrel.h"
#include "mgeom.h"
#include "mdcpln.h"
#include "mdunits.h"
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

extern int NCL_preview_mot;
enum
{
/*
.....Goto fields
*/
	FGENT,FGDRV,FGDCO,FGDTX, FGNPS,FGPS,FGPCO,FGPTX, FGCHK,FGCCO,FGCTX,
	FGIND,FGITX,FGISE, FGTA1,FGTA2,FGTA3, FGFD1,FGFD2,FGFD3,FGFD4,
	FGCON,
/*
.....Side fields
*/
	FGDSD,FGDST, FGCSD,FGCST,
/*
.....Thick fields
*/
	FGDTH,FGPTH,FGCTH,
/*
.....Position fields
*/
	FGDOF,FGPOF,FGCOF, FGPAP,FGPTP,FGPAE,FGPTE,
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
enum {Go, Side, Thicks, Position, Colors};
static char *Smode[]={"Go", "To Side", "Thicks", "Position", "Colors"};
static int Sred[3]={180,0,0}, Syellow[3]={180,148,0}, Sgreen[3]={0,180,0};
static int Sblack[3]={0,0,0};
static int *Ssecol[]={Sblack,Sblack,Sblack,Sblack,Sblack};
static char Sav_valuestr[65];
/*
.....Main variables
*/
static UU_LOGICAL Sform_init = UU_FALSE, Sfedfl=UU_FALSE, Staxfl=UU_FALSE;
static UN_motseg *Smptr=0,Smotatt;
static UU_LOGICAL Sgo_chg = UU_FALSE;
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
static int Sentry,Tentry,Sdscon,Tdscon,Snops,Tnops,Spscon,Tpscon,Scscon,Tcscon;
static int Smove,Tmove,Staxav,Ttaxav,Sgtfmode,Tgtfmode,Sgtfdav,Tgtfdav;
static int Sfdmod, Tfdmod, Scontact,Tcontact;
static char Sindir[65],Tindir[65],Staxis[65],Ttaxis[65],Sfeed[65],Tfeed[65];
static NCLX_mot_feedrate Sgtfd,Tgtfd;
static NCLX_mot_tlaxis Sgtaxis,Tgtaxis;
/*
.....Surface side variables
*/
static char Sdside[65],Tdside[65],Scside[65],Tcside[65];
/*
.....Thick variables
*/
static char Sdthk[65],Tdthk[65],Spthk[65],Tpthk[65],Scthk[65],Tcthk[65];
/*
.....Position variables
*/
static char Sdofs[65],Tdofs[65],Spofs[65],Tpofs[65],Scofs[65],Tcofs[65];
static char Sabpart[65],Tabpart[65],Sabentry[65],Tabentry[65];
/*
.....Color variables
*/
static int Sdscol,Tdscol;
static int Spscol,Tpscol;
static int Scscol,Tcscol;
static int Sapcol,Tapcol;
static int Saecol,Taecol;
static int Tgeotog,Tgeocol;
static UU_LOGICAL Sgeo_redraw;
/*
.....Geometry variables
*/
static UM_sgeo Sgds,Sgps,Sgcs,Sgap,Sgae;
static UU_LOGICAL Sgeo_apply = UU_FALSE;
/*
.....Goto callback routines
*/
static UD_FSTAT OnGoSel(),OnGoTxt(),OnGoTog();
/*
.....To Side callback routines
*/
static UD_FSTAT OnSdSel(),OnSdTxt();
/*
.....Thick callback routines
*/
static UD_FSTAT OnThTxt();
/*
.....Position callback routines
*/
static UD_FSTAT OnPoSel(),OnPoTxt();
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
static void S_hilite_entities(),S_unhilite_all();
static void S_update_answers(),S_section_changed();
static void S_add_key(),S_create_key_list(),S_draw_indir();
static int S_select_geo(),S_build_command();

static int SfrmPlay=0;
static int Serrflg,Spreviewflg,Schgmade;

static int *Sanswers[] = {
	&Tentry,UU_NULL,&Tdscon,(int *)Tdrive, &Tnops,UU_NULL,&Tpscon,(int *)Tpart,
	UU_NULL,&Tcscon,(int *)Tcheck,
	&Tmove,(int *)Tindir,UU_NULL,
	(int *)Ttaxis,&Ttaxav,UU_NULL,
	&Tgtfmode,(int *)&Tfeed,&Tgtfdav,&Tfdmod,
	&Tcontact,

	UU_NULL,(int *)Tdside,UU_NULL,(int *)Tcside,

	(int *)Tdthk,(int *)Tpthk,(int *)Tcthk,

	(int *)Tdofs,(int *)Tpofs,(int *)Tcofs,
	UU_NULL,(int *)Tabpart,UU_NULL,(int *)Tabentry,

	&Tdscol,&Tpscol,&Tcscol,&Tapcol,&Taecol,&Tgeotog,&Tgeocol,

	UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL};

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
	UU_LOGICAL sec1, sec2;
	sec1 = sec2 = UU_TRUE;
	if (Tgtfmode != 0)
	{
		ud_dispfrm_set_attribs(0, FGFD2, UM_BLACK, UM_WHITE);
	}
	else 
	{
		nc = (int)strlen(Tfeed);
		ul_strip_blanks(Tfeed, &nc);
		if (nc<=0)
		{
			ud_dispfrm_set_attribs(0,FGFD2,UM_WHITE,UM_RED);
			sec1 = UU_FALSE;
		}
		else
		{
			rval = atof(Tfeed);
			if (rval<=0)
				sec1 =  UU_FALSE;
			if (sec1) 
			{
				ud_dispfrm_set_attribs(0, FGFD2, UM_BLACK, UM_WHITE);
			}
			else 
			{
				ud_dispfrm_set_attribs(0, FGFD2, UM_WHITE,UM_RED);
			}
		}
	}
	if (strlen(Tdrive) <= 0)
		sec2 = UU_FALSE;
/*
.....Set Action Buttons
*/
	if (sec1&&sec2)
	{
		Ssecol[Go] = Sgreen;
		S_section_changed(Go,UU_TRUE);
		if (Sgo_chg==0)
		{
			Ssecol[Go] = Sblack;
			S_section_changed(Go,UU_TRUE);
		}
		else
		{	
			Ssecol[Go] = Sgreen;
			S_section_changed(Go,UU_TRUE);
		}
	}
	else
	{
		Ssecol[Go] = Sred;
		S_section_changed(Go,UU_TRUE);
	}
	ifl = sec1&&sec2;
	ud_frm_enable_ok(ifl);
	ud_set_traverse_mask(FAPV,ifl);
	ud_set_traverse_mask(FAPY,ifl);
	ud_set_traverse_mask(FARS,ifl);
	ud_set_traverse_mask(FAGE,ifl);
}

/*********************************************************************
**    E_FUNCTION     : nclu_go()
**       Controlling routine for the GO Position form.
**       GO statements are output from this form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_go()
{
	int status,flag;
	UU_LOGICAL cmdreject;
	UD_METHOD save_entry;
/*
.....Set up form fields
*/
	static char traverse[] = {1,1,1,1, 1,1,1,1, 1,1,1,
		1,1,1, 1,1,1,  1,1,1,1,1,
		1,1,1,1,
		1,1,1,
		1,1,1, 1,1,1,1,
		1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1};
	static char display[] = {1,1,1,1, 1,1,1,1, 1,1,1,
		1,1,1, 1,1,1,  1,1,1,1,1,
		1,1,1,1,
		1,1,1,
		1,1,1, 1,1,1,1,
		1,1,1,1,1,1,1,
		1,1,1,1,1,1,1,1};
	static char called[] = {6,6,6,6, 6,6,6,6, 6,6,6,
		6,6,6, 6,6,6,  6,6,6,6,6,
		6,6,6,6,
		6,6,6,
		6,6,6, 6,6,6,6,
		6,6,6,6,6,6,6,
		6,6,6,6,6,6,6,6};

	static UD_METHOD methods[] = {OnGoTog,OnGoSel,OnGoTog,OnGoTxt,
		OnGoTog,OnGoSel,OnGoTog,OnGoTxt,
		OnGoSel,OnGoTog,OnGoTxt,
		OnGoTog,OnGoTxt,OnGoSel,
		OnGoTxt,OnGoTog,OnGoTog,
		OnGoTog,OnGoTxt,OnGoTog,OnGoTog,
		OnGoTog,

		OnSdSel,OnSdTxt,OnSdSel,OnSdTxt,

		OnThTxt,OnThTxt,OnThTxt,

		OnPoTxt,OnPoTxt,OnPoTxt, OnPoSel,OnPoTxt,OnPoSel,OnPoTxt,

		OnCoTog,OnCoTog,OnCoTog,OnCoTog,OnCoTog,OnCoTog,OnCoTog,

		OnAction,OnAction,OnAction,OnAction,OnAction,OnAction,OnAction,OnVideo};
/*
.....Get the current tool axis and feed rate
*/
	NclxMotGetTlaxis(&Sgtaxis);
	NclxMotGetFeedrate(&Sgtfd);
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
	S_init_traverse(display,traverse);
/*
.....Get the Form input
*/
repeat:;
	NCL_preview_mot = 1;
	UD_initfrm_intry = S_enter_form;
	Sgo_chg = 0;
	status = ud_form1("ngo.frm",Sanswers,Sanswers,methods,called,display,
		traverse);
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
	if (status != UU_SUCCESS) goto repeat;
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
	ud_delete_assist_segs();
	UD_UNMARK(cmdreject);

	return;
}

/*********************************************************************
**    I_FUNCTION     : OnGoSel(fieldno,val,stat)
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
static UD_FSTAT OnGoSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int status,prmpt,fno,iseg,numint;
	unsigned *mask;
	UU_LOGICAL cmdreject;
	UM_coord pt;
	UM_vector vec;
	UM_PLOCREC pick;
	int save_ptype;
	struct NCL_fixed_databag ent;
	static UU_LOGICAL flag=UU_FALSE;
/*
.....Select the drive surface
*/
	switch (*fieldno)
	{
	case FGDRV:
		S_unhilite_entity(&Sgds);
		S_form_invis();
		mask = UD_ncl_motion;
		prmpt = 423;
		status = S_select_geo(&Sgds,UU_NULL,mask,2,prmpt,Tdscol,0,FGDTX,
			Tdrive);
/*
........Enable the Action buttons
*/
		if (strlen(Tdrive) > 0)
		{
			ud_set_traverse_mask(FAPV,1);
			ud_set_traverse_mask(FAPY,1);
			ud_set_traverse_mask(FAGE,1);
			Ssecol[Go] = Sgreen; S_section_changed(Go,UU_TRUE);
			ud_frm_enable_ok(UU_TRUE);
			ud_dispfrm_set_attribs(0,FGDRV,UM_BLACK,Tdscol);
			if ((strlen(Tpart) == 0) && (Tnops==0))
			{
				flag = UU_TRUE;
				fno = FGPS; OnGoSel(&fno,val,stat);
				if ((strlen(Tcheck) == 0) && (Tnops==0))
				{
					fno = FGCHK; OnGoSel(&fno,val,stat);
				}
				flag = UU_FALSE;
			}
		}
		S_form_vis();
		break;
/*
.....Select the part surface
*/
	case FGPS:
		S_unhilite_entity(&Sgps);
		S_form_invis();
		mask = UD_ncl_psis;
		prmpt = 415;
		status = S_select_geo(&Sgps,UU_NULL,mask,2,prmpt,Tpscol,0,FGPTX,
			Tpart);
		if (!flag) S_form_vis();
		break;
/*
.....Select the check surface
*/
	case FGCHK:
		S_unhilite_entity(&Sgcs);
		S_form_invis();
		mask = UD_ncl_motion;
		prmpt = 424;
		status = S_select_geo(&Sgcs,UU_NULL,mask,2,prmpt,Tcscol,0,FGCTX,
			Tcheck);
		S_form_vis();
		break;
/*
.....Select the direction
*/
	case FGISE:
		if (Tmove == 1)
		{
			S_form_invis();
			save_ptype = ud_getpick_type();
			ud_unlimit ();
			UD_MARK (cmdreject, UU_TRUE);
			if (cmdreject == 0)
			{
				iseg = ud_pick_assist_seg("Pick the new forward vector.");
				if (iseg != 0)
				{
					um_vctovc(Svecs[iseg-1],Sfwd);
					ncl_sprintf(Tindir,Sfwd,3);
					ud_update_answer(FGITX,Tindir);
				}
			}
			S_draw_indir();
			S_form_vis();
			ud_setpick_type(save_ptype);
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
	if (((strlen(Tdrive)>0)&&(strlen(Tcheck)>0))&&(Tnops==0))
	{
		ud_set_traverse_mask(FGDOF,1);
		ud_set_traverse_mask(FGPOF,1);
		ud_set_traverse_mask(FGCOF,1);
		ud_set_traverse_mask(FGPAP,1);
		ud_set_traverse_mask(FGPTP,1);
		ud_set_traverse_mask(FGPAE,1);
		ud_set_traverse_mask(FGPTE,1);
	}
	else
	{
		ud_set_traverse_mask(FGDOF,0);
		ud_set_traverse_mask(FGPOF,0);
		ud_set_traverse_mask(FGCOF,0);
		ud_set_traverse_mask(FGPAP,0);
		ud_set_traverse_mask(FGPTP,0);
		ud_set_traverse_mask(FGPAE,0);
		ud_set_traverse_mask(FGPTE,0);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnGoTog(fieldno,val,stat)
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
static UD_FSTAT OnGoTog(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,fno;
	UU_KEY_ID dskey,pskey,cskey;
	char caxis[6][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	char maxis[12][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	char cfed[16][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	UD_DDATA tval;
	char valuestr[65];
/*
.....Process GO toggle fields
*/
	switch (*fieldno)
	{
/*
.....Entry/Exit
.....Automatically enter DS,PS,CS labels
.....If Exit with no geometry selected yet
*/
	case FGENT:
		if (Tentry == 1)
		{
			if (strlen(Tdrive) == 0 && strlen(Tpart) == 0 && strlen(Tcheck) == 0)
			{
				obdsrf(&dskey,&pskey,&cskey);
				ncl_get_label_with_key(dskey,Tdrive);
				ud_update_answer(FGDTX,Tdrive);
				fno = FGDTX; tval.frmstr = Tdrive; OnGoTxt(&fno,&tval,stat);

				ncl_get_label_with_key(pskey,Tpart);
				ud_update_answer(FGPTX,Tpart);
				fno = FGPTX; tval.frmstr = Tpart; OnGoTxt(&fno,&tval,stat);

				ncl_get_label_with_key(cskey,Tcheck);
				ud_update_answer(FGCTX,Tcheck);
				fno = FGCTX; tval.frmstr = Tcheck; OnGoTxt(&fno,&tval,stat);
			}
		}
		if (Sentry != Tentry) 
		{
			Schgmade = 1;
			Sgo_chg = 1;
		}
		break;
	case FGDCO:
		if (Sdscon != Tdscon) 
		{
			Schgmade = 1;
			Sgo_chg = 1;
		}
		break;
	case FGPCO:
		if (Spscon != Tpscon) 
		{
			Schgmade = 1;
			Sgo_chg = 1;
		}
		break;
	case FGCCO:
		if (Scscon != Tcscon) 
		{
			Schgmade = 1;
			Sgo_chg = 1;
		}
		break;
/*
.....NOPS
*/
	case FGNPS:
		ud_set_traverse_mask(FGPS,!Tnops);
		ud_set_traverse_mask(FGPCO,!Tnops);
		ud_set_traverse_mask(FGPTX,!Tnops);
		ud_set_traverse_mask(FGCHK,!Tnops);
		ud_set_traverse_mask(FGCCO,!Tnops);
		ud_set_traverse_mask(FGCTX,!Tnops);
		if (((strlen(Tdrive)>0)&&(strlen(Tcheck)>0))&&(Tnops==0))
		{
			ud_set_traverse_mask(FGDOF,1);
			ud_set_traverse_mask(FGPOF,1);
			ud_set_traverse_mask(FGCOF,1);
			ud_set_traverse_mask(FGPAP,1);
			ud_set_traverse_mask(FGPTP,1);
			ud_set_traverse_mask(FGPAE,1);
			ud_set_traverse_mask(FGPTE,1);
		}
		else
		{
			ud_set_traverse_mask(FGDOF,0);
			ud_set_traverse_mask(FGPOF,0);
			ud_set_traverse_mask(FGCOF,0);
			ud_set_traverse_mask(FGPAP,0);
			ud_set_traverse_mask(FGPTP,0);
			ud_set_traverse_mask(FGPAE,0);
			ud_set_traverse_mask(FGPTE,0);
		}

		break;
/*
.....Initial Direction
*/
	case FGIND:
		i = Tmove != 0;
		ud_set_traverse_mask(FGITX,i);
		ud_set_traverse_mask(FGISE,i);
		S_draw_indir();
		if (i == 1) fno = FGISE; OnGoSel(&fno,val,stat);
		break;
/*
.....Tool axis
*/
	case FGTA2:
		ud_set_traverse_mask(FGTA3,Ttaxav);
		Schgmade = 1;
		Sgo_chg = 1;
		break;
	case FGTA3:
		nclu_tlaxis_form(&Sgtaxis,caxis,maxis,UU_FALSE,UU_FALSE,Staxfl);
		Staxfl = UU_TRUE;
		Sgo_chg = 1;
		break;
/*
.....Feed rate
*/
	case FGFD1:
		if (Sgtfmode != Tgtfmode) 
		{
			Schgmade = 1;
			Sgo_chg = 1;
		}
/*
......Tgtfmode, now only "current" and "Rapid"
*/
		if (Tgtfmode == 0)
		{
			ud_set_traverse_mask(FGFD2,UU_TRUE);
			ud_set_traverse_mask(FGFD3,UU_TRUE);
			strcpy(Tfeed, Sav_valuestr);
			ud_update_answer(FGFD2,(int *)Tfeed);
			ud_set_traverse_mask(FGFD4,Tgtfdav);
		}
		else
		{
/*
.....RAPID
*/
			Sfedfl = UU_FALSE;
			Tgtfdav = 0; ud_update_answer(FGFD3,&Tgtfdav);
			ud_set_traverse_mask(FGFD2,UU_FALSE);
			ud_set_traverse_mask(FGFD3,UU_FALSE);
			ud_set_traverse_mask(FGFD4,UU_FALSE);
/*
.....update text field to empty
*/
			valuestr[0] = '\0';
			strcpy(Sav_valuestr, Tfeed);
			ud_update_answer(FGFD2,(int *)valuestr);
		}
		break;
	case FGFD3:
		ud_set_traverse_mask(FGFD4,Tgtfdav);
		Schgmade = 1;
		Sgo_chg = 1;
		if (!(Tgtfdav))
			break;
	case FGFD4:
		Tgtfd.base_feedrate = atof(Tfeed);
		nclu_fedrat_form(&Tgtfd, &Sgtfd, cfed,UU_FALSE,Sfedfl, FGFD2);
		Sfedfl = UU_TRUE;
		Sgo_chg = 1;
		break;
	case FGCON:
		if (Scontact != Tcontact) 
		{
			Schgmade = 1;
			Sgo_chg = 1;
		}
		break;
	}
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnGoTxt(fieldno,val,stat)
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
static UD_FSTAT OnGoTxt(fieldno, val, stat)
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
	case FGDTX:
		S_unhilite_entity(&Sgds);
		ul_to_upper(Sdrive);
		if (strcmp(Sdrive,Tdrive) != 0)
		{
			Schgmade = 1;
			Sgo_chg = 1;
			Ssecol[Go] = Sgreen; S_section_changed(Go,UU_TRUE);
			S_init_geo(&Sgds,Tdrive,Tdscol);
/*
........Enable the Action buttons
*/
			if (strlen(Tdrive) > 0)
			{
				ud_set_traverse_mask(FAPV,1);
				ud_set_traverse_mask(FAPY,1);
				ud_set_traverse_mask(FAGE,1);
				Ssecol[Go] = Sgreen; S_section_changed(Go,UU_TRUE);
				ud_frm_enable_ok(UU_TRUE);
				ud_dispfrm_set_attribs(0,FGDRV,UM_BLACK,Tdscol);
			}
		}
		else if (Tdrive[0]=='\0')
		{
			Ssecol[Go] = Sred; S_section_changed(Go,UU_TRUE);
			ud_set_traverse_mask(FAPV,0);
			ud_set_traverse_mask(FAPY,0);
			ud_set_traverse_mask(FAGE,0);
			ud_dispfrm_set_attribs(0,FGDRV,UM_WHITE,UM_RED);
			ud_frm_enable_ok(UU_FALSE);
		}
		break;
	case FGPTX:
		S_unhilite_entity(&Sgps);
		ul_to_upper(Spart);
		if (strcmp(Spart,Tpart) != 0)
		{
			Schgmade = 1;
			Sgo_chg = 1;
			S_init_geo(&Sgps,Tpart,Tpscol);
		}
		break;
	case FGCTX:
		S_unhilite_entity(&Sgcs);
		ul_to_upper(Scheck);
		if (strcmp(Scheck,Tcheck) != 0)
		{
			Schgmade = 1;
			Sgo_chg = 1;
			S_init_geo(&Sgcs,Tcheck,Tcscol);
		}
		break;
	case FGITX:
		ul_to_upper(Sindir);
		if (strcmp(Sindir,Tindir) != 0) 
		{
			Schgmade = 1;
			Sgo_chg = 1;
		}
		break;
	case FGTA1:
		ul_to_upper(Ttaxis);
		if (strcmp(Staxis,Ttaxis) != 0) 
		{
			Schgmade = 1;
			Sgo_chg = 1;
		}
		break;
	case FGFD3:
		ul_to_upper(Tfeed);
		if (strcmp(Sfeed,Tfeed) != 0) 
		{
			Schgmade = 1;
			Sgo_chg = 1;
		}
		break;
	}
	if (((strlen(Tdrive)>0)&&(strlen(Tcheck)>0))&&(Tnops==0))
	{
		ud_set_traverse_mask(FGDOF,1);
		ud_set_traverse_mask(FGPOF,1);
		ud_set_traverse_mask(FGCOF,1);
		ud_set_traverse_mask(FGPAP,1);
		ud_set_traverse_mask(FGPTP,1);
		ud_set_traverse_mask(FGPAE,1);
		ud_set_traverse_mask(FGPTE,1);
	}
	else
	{
		ud_set_traverse_mask(FGDOF,0);
		ud_set_traverse_mask(FGPOF,0);
		ud_set_traverse_mask(FGCOF,0);
		ud_set_traverse_mask(FGPAP,0);
		ud_set_traverse_mask(FGPTP,0);
		ud_set_traverse_mask(FGPAE,0);
		ud_set_traverse_mask(FGPTE,0);
	}
	S_enable_buttons();
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnSdSel(fieldno,val,stat)
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
static UD_FSTAT OnSdSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int iseg,numint,relnum,fno;
	int save_ptype,limit_key = -1;
	UU_LOGICAL cmdreject,idid;
	UU_REAL uu_angle();
	char *sptr;
	UM_coord pt;
	UM_vector vec,tvec,vpnorm;
	UM_sgeo *geo;
	UD_PLOCREC pick;
	struct NCL_fixed_databag ent;
	switch (*fieldno)
	{
/*
.....Select the surface side
*/
	case FGDSD:
	case FGCSD:
		if (*fieldno == FGDSD)
		{
			geo = &Sgds;
			fno = FGDST;
			sptr = Tdside;
		}
		else
		{
			geo = &Sgcs;
			fno = FGCST;
			sptr = Tcside;
		}
		if (geo->key != 0)
		{
			S_form_invis();
			limit_key = -1;
			UD_MARK(cmdreject, UU_TRUE);
			if (cmdreject == 0)
			{
				do
				{
					limit_key = geo->key;
					ud_limit_entity(UU_TRUE,geo->key);
					ud_ldas(UD_DASPCKLOC,UA_NCL,716,&pick,1,&numint,1,UD_NODEFAULT);
					ud_limit_entity(UU_FALSE,geo->key);
					limit_key = -1;
					if (numint == 0) 
					{
						ud_setpick_type(save_ptype);
						if (limit_key != -1) ud_limit_entity(UU_FALSE, limit_key);
						limit_key = -1;		
						S_draw_indir();
						S_form_vis();
						UD_UNMARK(cmdreject);
						break;
					}
					iseg = ncl_get_pkpt1(&pick,&ent,.001,pt,vec);
					if (iseg == UU_FAILURE)
						ud_wrerr ("Failed to locate point on entity");
/*
........Automatically calculate the TO side
........When a surface is picked
*/
					else
					{
						idid = UU_FALSE;
						um_retrieve_data_relnum(ent.key,&relnum);
						if (uc_super_class(relnum) == UC_SURFACE_CLASS)
						{
							um_vpnorm(pick.pndc.transform,vpnorm);
							idid = UU_TRUE;
							if (um_angle(vec,vpnorm) < UM_PI/2.)
								um_vctmsc(vec,-1.,vec);
							else if (um_angle(vec,vpnorm) == UM_PI/2.)
								idid = UU_FALSE;
						}
/*
........Get side of surface to consider TO side
*/
						if (!idid)
						{
							ud_delete_assist_segs();
							um_cross(vec,Stend.vec,vec); um_unitvc(vec,vec);
							ud_assist_vector(pt,vec);
							um_vctmsc(vec,-1.,tvec);
							ud_assist_vector(pt,tvec);
							iseg = ud_pick_assist_seg(
								"Pick the side of the entity to position the tool TO");
							if (iseg == 0)
							{
								iseg = UU_FAILURE;
								break;
							}
							if (iseg == 1) um_vctovc(tvec,vec);
						}
						ncl_sprintf(sptr,vec,3);
						ud_update_answer(fno,sptr);
						Schgmade = 1;
						Ssecol[Side] = Sgreen; S_section_changed(Side,UU_TRUE);
					}
				} while (iseg == UU_FAILURE);
			}
			else
			{
				ud_setpick_type(save_ptype);
				if (limit_key != -1) ud_limit_entity(UU_FALSE, limit_key);
				limit_key = -1;
			}
			S_draw_indir();
			S_form_vis();
			UD_UNMARK(cmdreject);
		}
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnSdTxt(fieldno,val,stat)
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
static UD_FSTAT OnSdTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Label text field
*/
	switch (*fieldno)
	{
	case FGDST:
		if (strcmp(Sdside,Tdside) != 0)
		{
			Schgmade = 1;
			Ssecol[Side] = Sgreen; S_section_changed(Side,UU_TRUE);
		}
		break;
	case FGCST:
		if (strcmp(Scside,Tcside) != 0)
		{
			Schgmade = 1;
			Ssecol[Side] = Sgreen; S_section_changed(Side,UU_TRUE);
		}
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnThTxt(fieldno,val,stat)
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
static UD_FSTAT OnThTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Text field
*/
	switch (*fieldno)
	{
	case FGDTH:
		if (strcmp(Sdthk,Tdthk) != 0)
		{
			Schgmade = 1;
			Ssecol[Thicks] = Sgreen; S_section_changed(Thicks,UU_TRUE);
		}
		break;
	case FGPTH:
		if (strcmp(Spthk,Tpthk) != 0)
		{
			Schgmade = 1;
			Ssecol[Thicks] = Sgreen; S_section_changed(Thicks,UU_TRUE);
		}
		break;
	case FGCTH:
		if (strcmp(Scthk,Tcthk) != 0)
		{
			Schgmade = 1;
			Ssecol[Thicks] = Sgreen; S_section_changed(Thicks,UU_TRUE);
		}
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnPoSel(fieldno,val,stat)
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
static UD_FSTAT OnPoSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int prmpt,status;
	unsigned *mask;
	switch (*fieldno)
	{
/*
.....Above part
*/
	case FGPAP:
		S_unhilite_entity(&Sgap);
		S_form_invis();
		mask = UD_ncl_pl;
		prmpt = 660;
		status = S_select_geo(&Sgap,UU_NULL,mask,2,prmpt,Tapcol,0,FGPTP,
			Tabpart);
/*
........Mark the form as changed
*/
		if (strlen(Tabpart) > 0)
		{
			Schgmade = 1;
			Ssecol[Position] = Sgreen; S_section_changed(Position,UU_TRUE);
		}
		S_form_vis();
		break;
/*
.....Above entry
*/
	case FGPAE:
		S_unhilite_entity(&Sgae);
		S_form_invis();
		mask = UD_ncl_pl;
		prmpt = 659;
		status = S_select_geo(&Sgae,UU_NULL,mask,2,prmpt,Taecol,0,FGPTE,
			Tabentry);
/*
........Mark the form as changed
*/
		if (strlen(Tabentry) > 0)
		{
			Schgmade = 1;
			Ssecol[Position] = Sgreen; S_section_changed(Position,UU_TRUE);
		}
		S_form_vis();
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnPoTxt(fieldno,val,stat)
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
static UD_FSTAT OnPoTxt(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Label text field
*/
	switch (*fieldno)
	{
	case FGDOF:
		if (strcmp(Sdofs,Tdofs) != 0)
		{
			Schgmade = 1;
			Ssecol[Position] = Sgreen; S_section_changed(Position,UU_TRUE);
		}
		break;
	case FGPOF:
		if (strcmp(Spofs,Tpofs) != 0)
		{
			Schgmade = 1;
			Ssecol[Position] = Sgreen; S_section_changed(Position,UU_TRUE);
		}
		break;
	case FGCOF:
		if (strcmp(Scofs,Tcofs) != 0)
		{
			Schgmade = 1;
			Ssecol[Position] = Sgreen; S_section_changed(Position,UU_TRUE);
		}
		break;
	case FGPTP:
		S_unhilite_entity(&Sgap);
		ul_to_upper(Tabpart);
		if (strcmp(Sabpart,Tabpart) != 0)
		{
			Schgmade = 1;
			Ssecol[Position] = Sgreen; S_section_changed(Position,UU_TRUE);
			S_init_geo(&Sgap,Tabpart,Tapcol);
		}
		break;
	case FGPTE:
		S_unhilite_entity(&Sgae);
		ul_to_upper(Tabentry);
		if (strcmp(Sabentry,Tabentry) != 0)
		{
			Schgmade = 1;
			Ssecol[Position] = Sgreen; S_section_changed(Position,UU_TRUE);
			S_init_geo(&Sgap,Tabpart,Tapcol);
		}
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
		if (Sgds.key != 0)
			ud_dispfrm_set_attribs(0,FGDRV,UM_BLACK,Tdscol);
		S_hilite_entity(&Sgds,Tdscol);
		break;
	case FCOLB:
		ud_dispfrm_set_attribs(0,FGPS,UM_BLACK,Tpscol);
		S_hilite_entity(&Sgps,Tpscol);
		break;
	case FCOLC:
		ud_dispfrm_set_attribs(0,FGCHK,UM_BLACK,Tcscol);
		S_hilite_entity(&Sgcs,Tcscol);
		break;
	case FCOLE:
		ud_dispfrm_set_attribs(0,FGPAP,UM_BLACK,Tapcol);
		S_hilite_entity(&Sgap,Tapcol);
		break;
	case FCOLF:
		ud_dispfrm_set_attribs(0,FGPAE,UM_BLACK,Taecol);
		S_hilite_entity(&Sgae,Taecol);
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
	int status,i,flag= 0;

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
		if (status != UU_SUCCESS)
			break;
		S_unhilite_entity(&Sgds);
		S_unhilite_entity(&Sgps);
		S_unhilite_entity(&Sgcs);
/*
.....removed "SAME" changed per Ken
*/
/*		if (Tgtfmode == 2)
		{
			Tgtfmode = 0;
			ud_update_answer(FGFD1,&Tgtfmode);
			Tgtfdav = 0; 
			ud_update_answer(FGFD3,&Tgtfdav);
			ud_set_traverse_mask(FGFD2,UU_FALSE);
			ud_set_traverse_mask(FGFD3,UU_FALSE);
			ud_set_traverse_mask(FGFD4,UU_FALSE);
		}
*/
		Sgeo_apply = UU_TRUE;
		for (i=FAPV;i<FAVW;i++) ud_set_traverse_mask(i,0);
		i = -1;
		S_enter_form(&i, val, stat);
		ud_frm_enable_ok(UU_FALSE);
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
		ud_frm_enable_ok(UU_FALSE);
		break;
/*
.....Playback motion
*/
	case FAPB:
		SfrmPlay = nclu_playback_preview();
/* 
..... Added to force main form displaying back if encountered error
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
..... Added to force main form displaying back if encountered error
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
.....Initialize Go fields
*/
	if (Tmove == 0)
		traverse[FGITX] = traverse[FGISE] = 0;
	else
		traverse[FGITX] = traverse[FGISE] = 1;
/*
.....Now Tgtfmode only "Current" and "Rapid" now, per Ken
*/
/*	if (Tgtfmode < 2)
	{
		traverse[FGFD2] = traverse[FGFD3] = traverse[FGFD4] = 0;
	}
	else
*/
	{
		traverse[FGFD2] = traverse[FGFD3] = 1;
		traverse[FGFD4] = Tgtfdav;
	}
	traverse[FGTA3] = Ttaxav;
/*
.....if the drive and check area is empty
.....if NOPS is checked
.....disable all position field
*/
	if (((strlen(Tdrive)>0)&&(strlen(Tcheck)>0))&&(Tnops==0))
	{
		traverse[FGDOF] = 1;
		traverse[FGPOF] = 1;
		traverse[FGCOF] = 1;
		traverse[FGPAP] = 1;
		traverse[FGPTP] = 1;
		traverse[FGPAE] = 1;
		traverse[FGPTE] = 1;
	}
	else
	{
		traverse[FGDOF] = 0;
		traverse[FGPOF] = 0;
		traverse[FGCOF] = 0;
		traverse[FGPAP] = 0;
		traverse[FGPTP] = 0;
		traverse[FGPAE] = 0;
		traverse[FGPTE] = 0;
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
	int typ;
	double cutr[6];
	NCLX_mot_thick thick;
	UU_REAL hgt;
	int chc;
/*
.....Determine if Advanced Tlaxis & Feedrates are active
*/
	Tgtfd = Sgtfd;

	nclc_get_ctr_hgt(&chc, &hgt);
	Sgtfdav = Tgtfd.accel_flag || Tgtfd.slowdown_flag||(hgt!=0.0);
//it only have 2 choice now, 0 or 1, Yurong
//adevance value now is current, which is 0, always set 0 now
//	Sgtfmode = Sgtfdav ? 2:0;
	Sgtfmode = 0;
	Sfdmod = Sgtfd.mode-1;	
	Tgtaxis = Sgtaxis;
	Staxav = Tgtaxis.mode != NCLX_MOT_TLAXIS_SAME &&
		Tgtaxis.mode != NCLX_MOT_TLAXIS_FIXED;
/*
.....Initialize the Form settings
*/
	if (!Sform_init)
	{
		Sdscol = NCLX_ORANGE;
		Spscol = NCLX_SEA_GREEN;
		Scscol = NCLX_PURPLE;
		Sapcol = NCLX_LT_BLUE;
		Saecol = NCLX_TAN;
	}
	Sentry = 0;
	Sdscon = 0;
	Spscon = 0;
	Scscon = 0;
	Sdrive[0] = '\0';
	Spart[0] = '\0';
	Scheck[0] = '\0';
	Snops = 0;
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

	NclxMotGetContact(&Scontact);
	Sdside[0] = Scside[0] = '\0';
	NclxMotGetThick(&thick);
	ncl_sprintf(Sdthk,&thick.ds,1);
	ncl_sprintf(Spthk,&thick.ps,1);
	ncl_sprintf(Scthk,&thick.cs,1);

	strcpy(Sdofs,"0.");
	strcpy(Spofs,"0.");
	strcpy(Scofs,"0.");
	strcpy(Sabpart,"0.");
	strcpy(Sabentry,"0.");

	Tdscol = Sdscol;
	Tpscol = Spscol;
	Tcscol = Scscol;
	Tapcol = Sapcol;
	Taecol = Saecol;
	Tentry = Sentry;
	Tdscon = Sdscon;
	Tpscon = Spscon;
	Tcscon = Scscon;
	Tgtfmode = Sgtfmode;
	Ttaxav = Staxav;
	Tgtfdav = Sgtfdav;
	Tfdmod = Sfdmod;
	Tcontact = Scontact;
	strcpy(Tdrive,Sdrive);
	strcpy(Tpart,Spart);
	strcpy(Tcheck,Scheck);
	Tnops = Snops;
	Tmove = Smove;
	strcpy(Tindir,Sindir);
	strcpy(Ttaxis,Staxis);
	strcpy(Tfeed,Sfeed);
	if (Tgtfmode!=0)
/*
.....rapid
*/
	{
		Tfeed[0] = '\0';
	}
	strcpy(Sav_valuestr, Sfeed);

	strcpy(Tdside,Sdside);
	strcpy(Tcside,Scside);
	
	strcpy(Tdthk,Sdthk);
	strcpy(Tpthk,Spthk);
	strcpy(Tcthk,Scthk);

	strcpy(Tdofs,Sdofs);
	strcpy(Tpofs,Spofs);
	strcpy(Tcofs,Scofs);
	strcpy(Tabpart,Sabpart);
	strcpy(Tabentry,Sabentry);
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
	Sgds.key = Sgps.key = Sgcs.key = 0;
	Sgap.key = Sgae.key = 0;
	Sgds.color = Sgps.color = Sgcs.color = -1;
	Sgap.color = Sgae.color = -1;
	Sgds.label[0] = Sgps.label[0] = Sgcs.label[0] = '\0';
	Sgap.label[0] = Sgae.label[0] = '\0';
	S_init_geo(&Sgds,Tdrive,Tdscol);
	S_init_geo(&Sgps,Tpart,Tpscol);
	S_init_geo(&Sgcs,Tcheck,Tcscol);
	S_init_geo(&Sgap,Tabpart,Tapcol);
	S_init_geo(&Sgae,Tabentry,Taecol);
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
/*
.....Initial entry
.....Set standard colors
*/
	if (!Serrflg)
	{
		Ssecol[Go] = Sred;
		Ssecol[Side] = Sblack;
		Ssecol[Thicks] = Sblack;
		Ssecol[Position] = Sblack;
	}
/*
.....Set section colors
*/
	S_section_changed(Go,UU_FALSE);
	S_section_changed(Side,UU_FALSE);
	S_section_changed(Thicks,UU_FALSE);
	S_section_changed(Position,UU_FALSE);
/*
.....Set the field traversals
*/
	ud_set_traverse_mask(FGFD4,Tgtfdav);
		ud_dispfrm_set_attribs(0,FGDRV,UM_WHITE,UM_RED);
/*
.....Set the pick buttons to correct color
*/
	ud_dispfrm_set_attribs(0,FGPS,UM_BLACK,Tpscol);
	ud_dispfrm_set_attribs(0,FGCHK,UM_BLACK,Tcscol);
	ud_dispfrm_set_attribs(0,FGPAP,UM_BLACK,Tapcol);
	ud_dispfrm_set_attribs(0,FGPAE,UM_BLACK,Taecol);
/*
.....Set the mandatory fields to red
*/
	if (strlen(Tdrive) == 0) 
	{
		ud_dispfrm_set_attribs(0,FGDRV,UM_WHITE,UM_RED);
		ud_frm_enable_ok(UU_FALSE);
	}
	else
		ud_dispfrm_set_attribs(0,FGDRV,UM_BLACK,Tdscol);
	Serrflg = UU_FALSE;
	S_enable_buttons();
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
	Sapcol = Tapcol;
	Saecol = Taecol;

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
/*		if (sflst != UU_NULL) S_unhilite_entities(sflst);*/
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
	S_unhilite_entity(&Sgds);
	S_unhilite_entity(&Sgps);
	S_unhilite_entity(&Sgcs);
	S_unhilite_entity(&Sgap);
	S_unhilite_entity(&Sgae);
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
	S_add_key(which,Sgds.key);
	S_add_key(which,Sgps.key);
	S_add_key(which,Sgcs.key);
	S_add_key(which,Sgap.key);
	S_add_key(which,Sgae.key);
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
**    I_FUNCTION     : S_build_command(flag)
**			Builds and outputs the GO command.
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
	int nc,nc1,nc2,stat;
	UU_REAL rval,rval1,rval2;
	char caxis[6][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	char maxis[12][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	NCL_cmdbuf cmdbuf;
	NCLX_mot_tlaxis tlaxis;
	char cfed[16][NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	NCLX_mot_feedrate fedrat;

	if (flag)
		NCL_preview_mot = 0;
	else
		NCL_preview_mot = 1;
/*
.....Output Thick command
*/
	ncl_init_cmdbuf(&cmdbuf);
	if (strcmp(Tdthk,Sdthk) != 0 || strcmp(Tpthk,Spthk) != 0 ||
		strcmp(Tcthk,Scthk) != 0)
	{
		if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
		ncl_add_token(&cmdbuf,NCL_thick,NCL_nocomma);
		ncl_add_token(&cmdbuf,Tpthk,NCL_comma);
		ncl_add_token(&cmdbuf,Tdthk,NCL_comma);
		ncl_add_token(&cmdbuf,Tcthk,NCL_comma);
		ncl_add_cmdbuf(&cmdbuf);
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
		if (flag==UU_TRUE)
			strcpy(Staxis,Ttaxis);
	}
/*
.....Output advanced feedrate command
*/
	if (Tgtfdav && Sfedfl && flag)
	{
		nclu_fedrat_set_fedrat(&fedrat,cfed);
		nclu_fedrat_command(&cmdbuf,&fedrat,cfed,UU_TRUE,flag);
	}
	else
	{
/*
.....RAPID
*/
		if (Tgtfmode == 1)
		{
			if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
			ncl_add_token(&cmdbuf,NCL_rapid,NCL_comma);
			ncl_add_cmdbuf(&cmdbuf);
		}
		else
		{
/*
........Output FEDRAT/value command if not Preview
*/
			if (Tgtfmode == 0 && !Tgtfdav && strcmp(Tfeed,Sfeed) != 0 && flag)
			{
				ncl_add_token(&cmdbuf,NCL_fedrat,NCL_nocomma);          
				ncl_add_token(&cmdbuf,Tfeed,NCL_comma);
				ncl_add_cmdbuf(&cmdbuf);
			}
		}
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
.....Output SRFVCT command
*/
	nc = strlen(Tdside); ul_strip_blanks(Tdside,&nc);
	nc1 = strlen(Tcside); ul_strip_blanks(Tcside,&nc1);
	if (nc != 0 || nc1 != 0)
	{
		if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
		ncl_add_token(&cmdbuf,NCL_srfvct,NCL_nocomma);
		if (nc != 0)
			ncl_add_token(&cmdbuf,Tdside,NCL_comma);
		else
			ncl_add_token(&cmdbuf,",",NCL_nocomma);
		if (nc1 != 0)
			ncl_add_token(&cmdbuf,Tcside,NCL_comma);
		ncl_add_cmdbuf(&cmdbuf);
	}
/*
.....GO command
*/
	if (!flag) ncl_add_token(&cmdbuf, "*", NCL_nocomma);
/*
........NOPS button
*/
	if (Tnops) ncl_add_token(&cmdbuf,NCL_nops,NCL_comma);
/*
........Drive surface
*/
	ncl_add_token(&cmdbuf,NCL_go,NCL_nocomma);
	if (Tdscon == 1) ncl_add_token(&cmdbuf,NCL_on,NCL_comma);
	else if (Tdscon == 2) ncl_add_token(&cmdbuf,NCL_past,NCL_comma);
	ncl_add_token(&cmdbuf,Tdrive,NCL_comma);
/*
..... Skip the Part surface, Check surface, START/END, OffSET, RTRCTO
      RAPTO items if NOPS button is checked. Modified by KC
*/
	if (!Tnops)
	{
/*
........Part surface
*/
		nc = strlen(Tpart); ul_strip_blanks(Tpart,&nc);
		nc1 = strlen(Tcheck); ul_strip_blanks(Tcheck,&nc1);
		if ((nc != 0 || nc1 != 0) && !Tnops)
		{
			if (nc == 0)
				ncl_add_token(&cmdbuf,NCL_nops,NCL_comma);
			else
			{
				if (Tpscon == 1) ncl_add_token(&cmdbuf,NCL_on,NCL_comma);
				ncl_add_token(&cmdbuf,Tpart,NCL_comma);
			}
/*
........Check surface
*/
			if (nc1 != 0)
			{
				if (Tcscon == 1) ncl_add_token(&cmdbuf,NCL_on,NCL_comma);
				else if (Tcscon == 2) ncl_add_token(&cmdbuf,NCL_past,NCL_comma);
				ncl_add_token(&cmdbuf,Tcheck,NCL_comma);
			}
		}
/*
........ start/end
*/
		if (Tentry == 1)
			ncl_add_token(&cmdbuf,"END",NCL_comma);	
/*
........OFFSET
*/
		stat = ul_to_reals(&rval,&nc,1,Tdofs);
		stat += ul_to_reals(&rval1,&nc1,1,Tpofs);
		stat += ul_to_reals(&rval2,&nc2,1,Tcofs);
		if (stat != 0 || rval != 0. || rval1 != 0. || rval2 != 0.)
		{
			ncl_add_token(&cmdbuf,NCL_offset,NCL_comma);
			ncl_add_token(&cmdbuf,Tdofs,NCL_comma);
			ncl_add_token(&cmdbuf,Tpofs,NCL_comma);
			ncl_add_token(&cmdbuf,Tcofs,NCL_comma);
		}
/*
........RTRCTO
*/
		nc = strlen(Tabpart);
		ul_strip_blanks(Tabpart,&nc);
		if (nc>0)
		{
			stat = ul_to_reals(&rval,&nc,1,Tabpart);
			if (stat != 0 || rval != 0)
			{
				ncl_add_token(&cmdbuf,NCL_rtrcto,NCL_comma);
				ncl_add_token(&cmdbuf,Tabpart,NCL_comma);
			}
		}
/*
........RAPTO
*/
		nc = strlen(Tabentry);
		ul_strip_blanks(Tabentry,&nc);
		if (nc>0)
		{
			stat = ul_to_reals(&rval,&nc,1,Tabentry);
			if (stat != 0 || rval != 0)
			{
				ncl_add_token(&cmdbuf,NCL_rapto,NCL_comma);
				ncl_add_token(&cmdbuf,Tabentry,NCL_comma);
			}
		}
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
	else
	{
		Serrflg = ncl_call(&cmdbuf);
		if (Serrflg != 0) goto failed;
	}
	NCL_preview_mot = 1;
	return(UU_SUCCESS);
/*
.....Failed to create command
*/
failed:;
	return(UU_FAILURE);
	NCL_preview_mot = 1;
}

