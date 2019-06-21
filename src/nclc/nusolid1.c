/********************************************************************* 
**    NAME         :  nusolid1.c
**       CONTAINS:
**      nclu_solid_contour
**      nclu_solid_save
**      nclu_solid_bound
**      nclu_solid_compos
**      nclu_stl_save
**
**    COPYRIGHT 2004 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nusolid1.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:16
*********************************************************************/
#include <string.h>
#include "dselmask.h"
#include "nclmplay.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mxxx.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nkeywd.h"
#include "nclvx.h"
#include "nclupok.h"

#define FOFF 0
#define FBOX 1
/*
.....Selection of Surfaces
*/
#define FSEL 2
#define FCOL 3
#define FLIO 4
#define FDES 5
#define FLAY 6
#define FLAN 7
#define FLSE 8
#define FLSH 9
/*
.....Project to
*/
#define FPRJ 10
#define FPLV 11
#define FPSE 12
#define FPCO 13
#define FPBO 14
#define FPTO 15
#define FPHT 16
#define FPDR 17
#define FPVE 18
/*
.....Action buttons
*/
#define FLAB 19
#define FVIW 20
#define FPRE 21
#define FAPP 22

/*
.....Save solid fields
*/
#define FSSE 0
#define FSCO 1
#define FSDE 2
#define FSFI 3
#define FSFN 4
#define FSTY 5
/*
.....Bounding solid fields
*/
#define FBTY 0
#define FBSE 1
#define FBCO 2
#define FBDE 3
#define FBIO 4
#define FBLS 5
#define FBLA 6
#define FBLN 7
#define FBSH 8
#define FBLX 9
#define FBLY 10
#define FBLZ 11
#define FBUX 12
#define FBUY 13
#define FBUZ 14
#define FBEX 15
#define FBEY 16
#define FBEZ 17
#define FBLB 18
#define FBVW 19
#define FBPR 20
#define FBAP 21

#define FCMP 0
#define FCSE 1
#define FCCO 2
#define FCDE 3
#define FCCL 4
#define FCEN 5

#define ZLEV 0
#define PLANE 1
#define LEVEL 2

#define MXLAB NCL_MAX_LABEL_AND_SUBSCRIPT+1

static UU_LIST *Slaylist = UU_NULL;
static UU_LIST Ssurf;
static int Snsurf = 0;

static int Sprojto,Slayer,Slistlayer,Sfrmtyp;
static int Scolor=8,Scolorb=10,Sbox;
static int Saddlayer,Slayincl,Soffchk;
static int Sstype=0,Sformat=0;

static char Sboff[MXLAB],Slay_num[STRL],Soffset[STRL],Sheight[MXLAB];
static char Svector[MXLAB],Slabel[MXLAB],Stoff[MXLAB],Szlev[MXLAB];
static char Sfile[UX_MAX_PATH_LEN],Stoler[MXLAB];
static char *Stmplab={"@UZRY"};
static UM_sgeo *Spbot = UU_NULL;

static int Sbtype=0;
static UU_REAL Sbound[6];
static char Sexp[3][MXLAB];

static Scomp=0,Sclosed=1,Sentact=0;

static UD_FSTAT OnSrfSel(),OnColor(),OnToggle(),OnDesel(),OnLayerSel();
static UD_FSTAT OnLayer(),OnShow(),OnPlane(),OnPlnSel(),OnVecSel(),OnAction();
static UD_FSTAT OnBrowse(),OnToggle1(),OnToggle2(),OnApply();
static void S_calc_box(),S_delete_preview();
static int S_build_cmd(),S_build_savecmd(),S_build_boxcmd();

UD_FSTAT ncl_show_layer();

/*********************************************************************
**    E_FUNCTION     : nclu_solid_contour()
**       Interface for creating SOLID/PART command.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_solid_contour()
{
	int nc,status;
	UU_LOGICAL cmdreject;
	UU_REAL rnum;
	UM_sgeo *geo;
/*
.....Set up form fields
*/
	static char traverse[] = {1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1, 1,1,1,1};
	static char called[]   = {6,6, 6,6,6,6,6,6,6,6, 6,6,6,6,6,6,6,6,6, 6,6,6,6};
	static char display[]  = {1,1, 1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1, 1,1,1,1};

	static UD_METHOD methods[] = {
		UU_NULL,UU_NULL,
		OnSrfSel,OnColor,OnToggle,OnDesel,
		OnLayerSel,OnLayer,OnToggle,OnShow,
		OnToggle,OnPlane,OnPlnSel,OnColor,UU_NULL,UU_NULL,UU_NULL,OnVecSel,
		UU_NULL,
		UU_NULL,OnAction,OnAction,OnAction};

	static int *ans[] = {(int *)&Soffset, &Sbox,
		UU_NULL, &Scolor, &Slayincl, UU_NULL,
		UU_NULL, (int *)Slay_num, &Saddlayer, UU_NULL,
		&Sprojto, (int *)&Szlev, UU_NULL, &Scolorb,
		(int *)Sboff, (int *)Stoff, (int *)Sheight,
		UU_NULL, (int *)Svector,
		(int *)Slabel, UU_NULL, UU_NULL, UU_NULL};
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0)	goto done;
/*
..... Initialize answers
*/
	Sfrmtyp = 0;
	uu_list_init(&Ssurf,sizeof(UM_sgeo),50,50);
	Snsurf = 0;

	Slayer = 999;
	sprintf(Slay_num,"%d",Slayer);
	rnum = 0.;
	Saddlayer = Slayincl = UU_FALSE;
	Slistlayer = -1;

	ncl_sprintf(Soffset,&rnum,1);

	ncl_sprintf(Szlev,&rnum,1);
	ncl_sprintf(Sheight,&rnum,1);
	ncl_sprintf(Sboff,&rnum,1);
	ncl_sprintf(Stoff,&rnum,1);
	Svector[0] = '\0';
	Sprojto = LEVEL;
	Spbot = UU_NULL;

	Slabel[0] = '\0';
/*
.....Set traversal flags
*/
	if (Slayincl == 1) Saddlayer = UU_TRUE;
	traverse[FLAY] = traverse[FLAN] = traverse[FLSH] = Saddlayer;

	if (Sprojto == ZLEV)
	{
		traverse[FPLV] = traverse[FPHT] = UU_TRUE;
		traverse[FPSE] = traverse[FPCO] = UU_FALSE;
		traverse[FPBO] = traverse[FPTO] = UU_FALSE;
/*		traverse[FPDR] = traverse[FPVE] = UU_TRUE;*/
	}
	else if (Sprojto == PLANE)
	{
		traverse[FPLV] = traverse[FPHT] = UU_TRUE;
		traverse[FPSE] = traverse[FPCO] = UU_TRUE;
		traverse[FPBO] = traverse[FPTO] = UU_FALSE;
/*		traverse[FPDR] = traverse[FPVE] = UU_TRUE;*/
	}
	else
	{
		traverse[FPLV] = traverse[FPHT] = UU_FALSE;
		traverse[FPSE] = traverse[FPCO] = UU_FALSE;
		traverse[FPBO] = traverse[FPTO] = UU_TRUE;
/*		traverse[FPDR] = traverse[FPVE] = UU_FALSE;*/
	}
	traverse[FPRE] = 1 - Slayincl;
/*
.....Get the Form input
*/
repeat:
	status = ud_form1("solcontour.frm",ans,ans,methods,called,display,traverse);
/*
.....Delete last previewed curve
*/
	S_delete_preview();
	if (status == -1) goto done;
	if (Slayincl == UU_TRUE && Snsurf == 0) goto repeat;
/*
.....Output the SOLID command
*/
	S_build_cmd(UU_FALSE);
/*
.....End of routine
*/
done:
	geo = (UM_sgeo *)UU_LIST_ARRAY(&Ssurf); nc = Ssurf.cur_cnt;
	nclu_repaint (geo,nc,-1);
	uu_list_free(&Ssurf); Snsurf = 0;
	if (Slaylist)
	{
		geo = (UM_sgeo *) UU_LIST_ARRAY(Slaylist); nc = UU_LIST_LENGTH(Slaylist);
		nclu_repaint (geo,nc,-1);
		uu_list_free(Slaylist); uu_free(Slaylist); 
		Slaylist = UU_NULL; Slistlayer = -1;
	}
	if (Spbot)
	{
		nclu_repaint(Spbot,1,-1);
		uu_free(Spbot);
	}

	S_delete_preview();
	UD_UNMARK(cmdreject); 	
	return;
}

/*********************************************************************
**    E_FUNCTION     : nclu_solid_save()
**       Interface for creating SAVE/SOLID command.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_solid_save()
{
	int nc,stat,status;
	UU_LOGICAL cmdreject;
	UM_sgeo *geo;
/*
.....Set up form fields
*/
	static char traverse[] = {1,1,1, 1,1, 1};
	static char called[]   = {6,6,6, 6,6, 6};
	static char display[]  = {1,1,1, 1,1, 1};

	static UD_METHOD methods[] = {
		OnSrfSel,OnColor,OnDesel, OnBrowse,UU_NULL, UU_NULL};

	static int *ans[] = {
		UU_NULL, &Scolor, UU_NULL, UU_NULL, (int *)Sfile, &Sstype};
/*
.....Trap Reject Op
*/
	stat = UU_FAILURE;
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0)	goto done;
/*
..... Initialize answers
*/
	Sfrmtyp = 1;
	uu_list_init(&Ssurf,sizeof(UM_sgeo),50,50);
	Snsurf = 0;

	Saddlayer = Slayincl = UU_FALSE;

	Sfile[0] = '\0';
/*
.....Get the Form input
*/
repeat:
	status = ud_form1("savesolid.frm",ans,ans,methods,called,display,traverse);
	if (status == -1) goto done;
/*
.....Make sure a filename was specified
*/
	nc = ul_cut_string(Sfile,UX_MAX_PATH_LEN);
	if (nc == 0)
	{
		ud_wrerr("A filename must be specified.");
		goto repeat;
	}
	stat = UU_SUCCESS;
/*
.....End of routine
*/
done:
	geo = (UM_sgeo *)UU_LIST_ARRAY(&Ssurf); nc = UU_LIST_LENGTH(&Ssurf);
	nclu_repaint (geo,nc,-1);
/*
.....Output the SOLID command
*/
	if (stat == UU_SUCCESS)
		S_build_savecmd(UU_FALSE);
	uu_list_free(&Ssurf); Snsurf = 0;
	UD_UNMARK(cmdreject); 	
	return;
}

/*********************************************************************
**    E_FUNCTION     : nclu_solid_bound()
**       Interface for creating a bounding box SOLID.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_solid_bound()
{
	int i,nc,status;
	UU_LOGICAL cmdreject;
	UM_sgeo *geo;
/*
.....Set up form fields
*/
	static char traverse[] = {1,1,1,1, 1,1, 1,1,1, 1,1,1, 1,1,1, 1,1,1, 1,1,1,1};
	static char called[]   = {6,6,6,6, 6,6, 6,6,6, 6,6,6, 6,6,6, 6,6,6, 6,6,6,6};
	static char display[]  = {1,1,1,1, 6,6, 6,6,6, 1,1,1, 1,1,1, 1,1,1, 1,1,1,
		1,1,1,1};

	static UD_METHOD methods[] = {
		OnToggle1,OnSrfSel,OnColor,OnDesel, OnToggle1,OnToggle1,
		OnLayerSel,OnLayer,OnShow, UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,UU_NULL,UU_NULL, UU_NULL,UU_NULL,UU_NULL, UU_NULL,
		OnAction,OnAction,OnAction};

	static int *ans[] = {&Sbtype, UU_NULL, &Scolor, UU_NULL, &Slayincl,
		&Saddlayer, UU_NULL, (int *)Slay_num, UU_NULL, (int *)&Sbound[0],
		(int *)&Sbound[1], (int *)&Sbound[2], (int *)&Sbound[3],
		(int *)&Sbound[4], (int *)&Sbound[5], (int *)&Sexp[0], (int *)&Sexp[1],
		(int *)&Sexp[2], (int *)&Slabel, UU_NULL, UU_NULL, UU_NULL};
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0)	goto done;
/*
..... Initialize answers
*/
	Sfrmtyp = 2;
	uu_list_init(&Ssurf,sizeof(UM_sgeo),50,50);
	Snsurf = 0;

	Slayer = 999;
	sprintf(Slay_num,"%d",Slayer);
	Saddlayer = Slayincl = UU_FALSE;
	Slistlayer = -1;

	Slabel[0] = '\0';

	for (i=0;i<6;i++) Sbound[i] = 0.;
	for (i=0;i<3;i++) strcpy(Sexp[i],"0.");
/*
.....Set traversal flags
*/
	if (Sbtype == 0)
	{
		if (Slayincl == 1) Saddlayer = UU_TRUE;
		traverse[FBLA] = traverse[FBLN] = traverse[FBSH] = Saddlayer;
		traverse[FBPR] = 1 - Slayincl;
	}
	else
	{
		traverse[FBIO] = traverse[FBLS] = traverse[FBLA] = 0;
		traverse[FBLN] = traverse[FBSH] = 0;
	}
/*
.....Get the Form input
*/
repeat:
	status = ud_form1("solbox.frm",ans,ans,methods,called,display,traverse);
/*
.....Delete last previewed curve
*/
	S_delete_preview();
	if (status == -1) goto done;
	if (Sbtype == 0 && Slayincl == UU_TRUE && Snsurf == 0) goto repeat;
/*
.....Make sure geometry was selected
*/
	if (Sbtype == 0 && Snsurf == 0 && Saddlayer == UU_FALSE)
	{
		ud_wrerr("No geometry has been selected.");
		goto repeat;
	}
/*
.....Output the SOLID command
*/
	S_build_boxcmd(UU_FALSE);
/*
.....End of routine
*/
done:
	geo = (UM_sgeo *)UU_LIST_ARRAY(&Ssurf); nc = Ssurf.cur_cnt;
	nclu_repaint (geo,nc,-1);
	uu_list_free(&Ssurf); Snsurf = 0;
	if (Slaylist)
	{
		geo = (UM_sgeo *) UU_LIST_ARRAY(Slaylist); nc = UU_LIST_LENGTH(Slaylist);
		nclu_repaint (geo,nc,-1);
		uu_list_free(Slaylist); uu_free(Slaylist);
		Slaylist = UU_NULL; Slistlayer = -1;
	}

	S_delete_preview();
	UD_UNMARK(cmdreject); 	
	return;
}

/*********************************************************************
**    E_FUNCTION     : nclu_solid_compos()
**       Interface for creating SOLID/COMPOS command.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_solid_compos()
{
	int nc,stat,status;
	UM_int2 idx;
	UU_REAL rnum;
	UU_LOGICAL cmdreject;
	UM_sgeo *geo;
/*
.....Set up form fields
*/
	static char traverse[] = {1,1,1,1, 1,1,1};
	static char called[]   = {6,6,6,6, 6,6,6};
	static char display[]  = {1,1,1,1, 1,1,1};

	static UD_METHOD methods[] = {
		OnToggle2,OnSrfSel,OnColor,OnDesel, UU_NULL,UU_NULL, OnApply};

	static int *ans[] = {
		&Scomp, UU_NULL, &Scolor, UU_NULL, &Sclosed, &Sentact};
/*
.....Trap Reject Op
*/
	stat = UU_FAILURE;
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0)	goto done;
/*
..... Initialize answers
*/
	Sfrmtyp = 4;
	uu_list_init(&Ssurf,sizeof(UM_sgeo),50,50);
	Snsurf = 0;

	Saddlayer = Slayincl = UU_FALSE;
/*
.....Initialize the field traversals
*/
	if (Scomp == 2)
		traverse[FCSE] = traverse[FCCO] = traverse[FCDE] = UU_FALSE;
	else
		traverse[FCSE] = traverse[FCCO] = traverse[FCDE] = UU_TRUE;
/*
.....Get the Form input
*/
repeat:
	status = ud_form1("solcompos.frm",ans,ans,methods,called,display,traverse);
	if (status == -1) goto done;
	stat = UU_SUCCESS;
/*
.....End of routine
*/
done:
	geo = (UM_sgeo *)UU_LIST_ARRAY(&Ssurf); nc = UU_LIST_LENGTH(&Ssurf);
	nclu_repaint (geo,nc,-1);
/*
.....Output the SOLID/COMPOS command
*/
	if (stat == UU_SUCCESS)
		S_build_compcmd();
	uu_list_free(&Ssurf); Snsurf = 0;
	UD_UNMARK(cmdreject); 	
	return;
}

/*********************************************************************
**    E_FUNCTION     : nclu_stl_save()
**       Interface for creating SAVE/STL command.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_stl_save()
{
	int nc,stat,status;
	UM_int2 idx;
	UU_REAL rnum;
	UU_LOGICAL cmdreject;
	UM_sgeo *geo;
/*
.....Set up form fields
*/
	static char traverse[] = {1,1,1, 1,1, 1,1};
	static char called[]   = {6,6,6, 6,6, 6,6};
	static char display[]  = {1,1,1, 1,1, 1,1};

	static UD_METHOD methods[] = {
		OnSrfSel,OnColor,OnDesel, OnBrowse,UU_NULL, UU_NULL,UU_NULL};

	static int *ans[] = {
		UU_NULL, &Scolor, UU_NULL, UU_NULL, (int *)Sfile, &Sformat,
		(int *)&Stoler};
/*
.....Trap Reject Op
*/
	stat = UU_FAILURE;
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0)	goto done;
/*
..... Initialize answers
*/
	Sfrmtyp = 3;
	uu_list_init(&Ssurf,sizeof(UM_sgeo),50,50);
	Snsurf = 0;

	Saddlayer = Slayincl = UU_FALSE;

	Sfile[0] = '\0';
	idx = 27; getsc(&idx,&rnum);
	rnum = rnum * 5.;
	ncl_sprintf(Stoler,&rnum,1);
/*
.....Get the Form input
*/
repeat:
	status = ud_form1("savestl.frm",ans,ans,methods,called,display,traverse);
	if (status == -1) goto done;
/*
.....Make sure a filename was specified
*/
	nc = ul_cut_string(Sfile,UX_MAX_PATH_LEN);
	if (nc == 0)
	{
		ud_wrerr("A filename must be specified.");
		goto repeat;
	}
	stat = UU_SUCCESS;
/*
.....End of routine
*/
done:
	geo = (UM_sgeo *)UU_LIST_ARRAY(&Ssurf); nc = UU_LIST_LENGTH(&Ssurf);
	nclu_repaint (geo,nc,-1);
/*
.....Output the SAVE/STL command
*/
	if (stat == UU_SUCCESS)
		S_build_stlcmd(UU_FALSE);
	uu_list_free(&Ssurf); Snsurf = 0;
	UD_UNMARK(cmdreject); 	
	return;
}


/*********************************************************************
**    S_FUNCTION     :  OnSrfSel(fieldno, val, stat)
**       Routine to select a list of surfaces.
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
static UD_FSTAT OnSrfSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int numint,init,color,pr;
	struct NCL_fixed_databag e;
	UU_LOGICAL cmdreject;
	UM_sgeo geo;
/*
.....Calculate bounding motion box
*/
	if (Sfrmtyp == 2 && (Sbtype == 1 || Sbtype == 2))
	{
		S_calc_box();
		return(UD_FLDOK);
	}
/*
.....Take down form
*/
	ud_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK (cmdreject, UU_TRUE);
	if (cmdreject != 0) goto done;
/*
.....Set the appropriate selection mask
*/
	if (Sfrmtyp == 0)
	{
		ud_lgeo(UU_TRUE,UD_ncl_allsf);
		pr = 478;
	}
	else if (Sfrmtyp == 1)
	{
		ud_lgeo(UU_TRUE,UD_solid);
		pr = 705;
	}
	else if (Sfrmtyp == 3)
	{
		ud_lgeo(UU_TRUE,UD_ncl_allsfsh);
		pr = 705;
	}
	else if (Sfrmtyp == 4)
	{
		if (Scomp == 0)
		{
			ud_lgeo(UU_TRUE,UD_ncl_allsf);
			pr = 478;
		}
		else
		{
			ud_lgeo(UU_TRUE,UD_solid);
			pr = 705;
		}
	}
	else
	{
		ud_lgeo(UU_TRUE,UD_solid_bound);
		pr = 238;
	}
/*
.....Get the next geometry selection
*/
	ud_ldas(UD_DASSELECT,UA_NCL,pr,UU_NULL,1,&numint,UD_NODEFAULT);
	if (numint == 0) goto done;
/*
.....Loop through selections
*/
	init = UU_TRUE;
	color = Scolor;
	while(ud_gnxt(init,UU_NULL,&e.key,1))
	{
		init = UU_FALSE;
/*
.....Store this item in the list
*/
		if (ncl_retrieve_data_fixed(&e) != 0) continue;
		geo.key = e.key;
		geo.relnum = e.rel_num;
		ncl_get_label(&e,geo.label);
		ncl_get_geo_color(e.key,&geo.color);
		nclu_add_list(&Ssurf,&geo,&color);
/*
.....Update the entities color
*/
		if (color != -1)
			ncl_update_geo_color(e.key,color,UU_TRUE);
		else
			ncl_update_geo_color(e.key,geo.color,UU_FALSE);

		uc_display(&e);
		color = Scolor;
	}
	Snsurf = UU_LIST_LENGTH(&Ssurf);

	if ((Sfrmtyp == 0 || Sfrmtyp == 2) && Snsurf > 40)
	{
		Slayincl = Saddlayer = 1;
		if (Sfrmtyp == 0)
		{
			ud_update_answer(FLIO,&Slayincl);
			ud_update_answer(FLSE,&Saddlayer);
			ud_set_traverse_mask(FLIO,0);
			ud_set_traverse_mask(FLSE,0);
			ud_set_traverse_mask(FLAY,1);
			ud_set_traverse_mask(FLAN,1);
			ud_set_traverse_mask(FLSH,0);
			ud_set_traverse_mask(FPRE,0);
		}
		else
		{
			ud_update_answer(FBIO,&Slayincl);
			ud_update_answer(FBLS,&Saddlayer);
			ud_set_traverse_mask(FBIO,0);
			ud_set_traverse_mask(FBLS,0);
			ud_set_traverse_mask(FBLA,1);
			ud_set_traverse_mask(FBLN,1);
			ud_set_traverse_mask(FBSH,0);
			ud_set_traverse_mask(FBPR,0);
		}
	}
/*
.....End of routine
.....Redisplay form
*/
done:;
	ud_unlimit();
	ud_form_vis();
	if (Sfrmtyp == 2) S_calc_box();
	UD_UNMARK(cmdreject);

	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnColor(fieldno, val, stat)
**			Color change callback.  Changes the color of all entities.
**    PARAMETERS   
**       INPUT  : fieldno = Form field which initiated this call.
**			         val     = Current field value.
**			         stat    = Not used.
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
	int nc,color;
	UM_sgeo *geo = UU_NULL;

/*
.....Call the default method
.....This causes the answer field to be updated
*/
	ud_default_method(fieldno, val, stat);
/*
.....Reference correct geometry list
.....Depending on which button was pushed
*/
	switch (*fieldno)
	{
	case FCOL:
	case FSCO:
	case FBCO:
		if (Snsurf > 0)
		{
			geo = (UM_sgeo *) UU_LIST_ARRAY (&Ssurf); nc = Snsurf;
			color = Scolor;
		}
		break;
	case FPCO:
		geo = Spbot; nc = 1;
		color = Scolorb;
		break;
	default:
		return(UD_BADRNG);
	}
	if (!geo) return (UD_FLDOK);
/*
.....Change the color of all entities
.....in this list
*/
	nclu_repaint (geo,nc,color);

	return (UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnToggle()
**       Method called when a toggle makes other fields active/inactive.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnToggle(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int ltf;

	switch (*fieldno)
	{
/*
.....Include on Layer
*/
	case FLIO:
		if (Slayincl == 1)
		{
			Saddlayer = 1;
			ud_update_answer(FLSE,&Saddlayer);
			ud_set_traverse_mask(FLSE,0);
			ud_set_traverse_mask(FLAY,1);
			ud_set_traverse_mask(FLAN,1);
			ud_set_traverse_mask(FLSH,0);
			ud_set_traverse_mask(FPRE,0);
		}
		else
		{
			Saddlayer = 0;
			ud_update_answer(FLSE,&Saddlayer);
			ud_set_traverse_mask(FLSE,1);
			ud_set_traverse_mask(FLAY,0);
			ud_set_traverse_mask(FLAN,0);
			ud_set_traverse_mask(FLSH,0);
			ud_set_traverse_mask(FPRE,1);
		}
		break;
/*
.....Select by Layer
*/
	case FLSE:
		ltf = Saddlayer;
		ud_set_traverse_mask(FLAY,ltf);
		ud_set_traverse_mask(FLAN,ltf);
		ud_set_traverse_mask(FLSH,ltf);
		break;
/*
.....Projection type
*/
	case FPRJ:
		if (Sprojto == ZLEV)
		{
			ud_set_traverse_mask(FPLV,1);
			ud_set_traverse_mask(FPHT,1);
			ud_set_traverse_mask(FPSE,0);
			ud_set_traverse_mask(FPCO,0);
			ud_set_traverse_mask(FPBO,0);
			ud_set_traverse_mask(FPTO,0);
/*			ud_set_traverse_mask(FPDR,1);*/
/*			ud_set_traverse_mask(FPVE,1);*/
		}
		else if (Sprojto == PLANE)
		{
			ud_set_traverse_mask(FPLV,1);
			ud_set_traverse_mask(FPHT,1);
			ud_set_traverse_mask(FPSE,1);
			ud_set_traverse_mask(FPCO,1);
			ud_set_traverse_mask(FPBO,0);
			ud_set_traverse_mask(FPTO,0);
/*			ud_set_traverse_mask(FPDR,1);*/
/*			ud_set_traverse_mask(FPVE,1);*/
		}
		else
		{
			ud_set_traverse_mask(FPLV,0);
			ud_set_traverse_mask(FPHT,0);
			ud_set_traverse_mask(FPSE,0);
			ud_set_traverse_mask(FPCO,0);
			ud_set_traverse_mask(FPBO,1);
			ud_set_traverse_mask(FPTO,1);
/*			ud_set_traverse_mask(FPDR,0);*/
/*			ud_set_traverse_mask(FPVE,0);*/
		}
		break;

	default:
		break;
	}

	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnToggle1()
**       Method called when a toggle makes other fields active/inactive.
**       Called from Solid Bounds form.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnToggle1(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int ltf;

	switch (*fieldno)
	{
/*
.....Bounding box type
*/
	case FBTY:
		if (Sbtype == 0)
		{
			ud_set_traverse_mask(FBIO,1);
			if (Slayincl == 1)
			{
				ud_set_traverse_mask(FBLS,0);
				ud_set_traverse_mask(FBLA,1);
				ud_set_traverse_mask(FBLN,1);
				ud_set_traverse_mask(FBSH,0);
				ud_set_traverse_mask(FBPR,0);
			}
			{
				ltf = Saddlayer;
				ud_set_traverse_mask(FBLS,1);
				ud_set_traverse_mask(FBLA,ltf);
				ud_set_traverse_mask(FBLN,ltf);
				ud_set_traverse_mask(FBSH,ltf);
				ud_set_traverse_mask(FBPR,1);
			}
		}
		else
		{
			ud_set_traverse_mask(FBIO,0);
			ud_set_traverse_mask(FBLS,0);
			ud_set_traverse_mask(FBLA,0);
			ud_set_traverse_mask(FBLN,0);
			ud_set_traverse_mask(FBSH,0);
			ud_set_traverse_mask(FBPR,1);
		}
		break;
/*
.....Include on Layer
*/
	case FBIO:
		if (Slayincl == 1)
		{
			Saddlayer = 1;
			ud_update_answer(FBLS,&Saddlayer);
			ud_set_traverse_mask(FBLS,0);
			ud_set_traverse_mask(FBLA,1);
			ud_set_traverse_mask(FBLN,1);
			ud_set_traverse_mask(FBSH,0);
			ud_set_traverse_mask(FBPR,0);
		}
		else
		{
			Saddlayer = 0;
			ud_update_answer(FBLS,&Saddlayer);
			ud_set_traverse_mask(FBLS,1);
			ud_set_traverse_mask(FBLA,0);
			ud_set_traverse_mask(FBLN,0);
			ud_set_traverse_mask(FBSH,0);
			ud_set_traverse_mask(FBPR,1);
		}
		break;
/*
.....Select by Layer
*/
	case FBLS:
		ltf = Saddlayer;
		ud_set_traverse_mask(FBLA,ltf);
		ud_set_traverse_mask(FBLN,ltf);
		ud_set_traverse_mask(FBSH,ltf);
		break;

	default:
		break;
	}

	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnToggle2()
**       Method called when a toggle makes other fields active/inactive.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnToggle2(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int ltf;

	switch (*fieldno)
	{
/*
.....Composite solid entity type
*/
	case FCMP:
		if (Scomp == 2)
		{
			ud_set_traverse_mask(FCSE,UU_FALSE);
			ud_set_traverse_mask(FCCO,UU_FALSE);
			ud_set_traverse_mask(FCDE,UU_FALSE);
		}
		else
		{
			ud_set_traverse_mask(FCSE,UU_TRUE);
			ud_set_traverse_mask(FCCO,UU_TRUE);
			ud_set_traverse_mask(FCDE,UU_TRUE);
		}
		break;
	default:
		break;
	}

	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnDesel(fieldno, val, stat)
**			Deselects all surfaces from the list.
**    PARAMETERS   
**       INPUT  : fieldno = Form field which initiated this call.
**			         val     = Not used, form initiated through pushbutton.
**			         stat    = Not used.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnDesel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UM_sgeo *geo;
/*
.....Remove all entities in this list
*/
	geo = (UM_sgeo *) UU_LIST_ARRAY(&Ssurf);
	nclu_repaint (geo,Snsurf,-1);
	UU_LIST_EMPTY(&Ssurf); Snsurf = 0;
	Slayincl = Saddlayer = 0;
	if (Sfrmtyp == 0)
	{
		ud_update_answer(FLIO,&Slayincl);
		ud_update_answer(FLSE,&Saddlayer);
		ud_set_traverse_mask(FLIO,1);
		ud_set_traverse_mask(FLSE,1);
		ud_set_traverse_mask(FLAY,0);
		ud_set_traverse_mask(FLAN,0);
		ud_set_traverse_mask(FLSH,0);
		ud_set_traverse_mask(FPRE,1);
	}
	else if (Sfrmtyp == 2)
	{
		ud_update_answer(FBIO,&Slayincl);
		ud_update_answer(FBLS,&Saddlayer);
		ud_set_traverse_mask(FBIO,1);
		ud_set_traverse_mask(FBLS,1);
		ud_set_traverse_mask(FBLA,0);
		ud_set_traverse_mask(FBLN,0);
		ud_set_traverse_mask(FBSH,0);
		ud_set_traverse_mask(FBPR,1);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : OnLayerSel()
**       Method called on the "Layers" push button in the Waterline form.
**    PARAMETERS
**       INPUT  :
**          none.
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
		Slayer = layptr[iact].num;
		sprintf(Slay_num,"%d",Slayer);
		if (Sfrmtyp == 0) ud_update_answer(FLAN,(int *)Slay_num);
		else if (Sfrmtyp == 2) ud_update_answer(FBLN,(int *)Slay_num);
		ud_update_form (0);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnLayer(fieldno, val, stat)
**			Deselects all surfaces from the list if the layer changed.
**    PARAMETERS   
**       INPUT  : fieldno = Form field which initiated this call.
**			         val     = Not used, form initiated through pushbutton.
**			         stat    = Not used.
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
	char *name = "Solid Geometry";

	if (i != Slayer)
	{
		Slayer = i;
		um_set_layer_num(i);
		um_set_layer_name(i,name);
/*		return(OnDesel(fieldno, val, stat));*/
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnShow(fieldno, val, stat)
**			Displays all surfaces in the layer.
**    PARAMETERS   
**       INPUT  : fieldno = Form field which initiated this call.
**			         val     = Not used, form initiated through pushbutton.
**			         stat    = Not used.
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
	int flag;

	if (!Slaylist)
	{
		Slaylist = (UU_LIST *) uu_malloc (sizeof (UU_LIST));
		uu_list_init(Slaylist,sizeof(UM_sgeo),0,100); 
	}

	flag = 2;
	if (Sfrmtyp == 2) flag = 3;
	return (ncl_show_layer(Slaylist,Scolor,Slayer,&Slistlayer,flag));
}

/*********************************************************************
*********************************************************************/
static UD_FSTAT OnPlane(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnPlnSel(filedno, val, stat)
**       Routine to select the Projection plane.
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
static UD_FSTAT OnPlnSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int numint,pr,color;
	unsigned int *mask;
	struct NCL_fixed_databag e;
	UM_PLOCREC pick;
	UU_LOGICAL cmdreject;
	UM_sgeo *sfpt = UU_NULL;

	if (*fieldno != FPSE) return (UD_BADRNG);

	if (!Spbot) Spbot = (UM_sgeo *)uu_malloc(sizeof (UM_sgeo));
	sfpt = Spbot;
	color = Scolorb;
	pr = 493;
	mask = UD_ncl_allsfpl;

	sfpt->key = 0; sfpt->relnum = 0; sfpt->color = -1;
/*
.....Take down form
*/
	ud_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK (cmdreject, UU_TRUE);
	if (cmdreject != 0) goto done;
/*
.....Set the appropriate selection mask
*/
	ud_lgeo(UU_TRUE,mask);
/*
.....Get the next geometry selection
*/
	ua_dl_pldas(UD_DASPCKLOC,UA_NCL,pr,&pick,1,&numint,1);
	if (numint == 0) goto done;

	e.key = um_get_pickkey(&(pick.pent),1);
	if (ncl_retrieve_data_fixed(&e) != 0) goto done;

	if (Sprojto == PLANE && e.rel_num != NCL_PLN_REL)
	{
		UM_int4 sfkey;
		UM_int2 primtyp;

		sfkey = e.key;
		ncl_get_sf_primtyp (&sfkey,&primtyp);

		if (primtyp != NCLSF_PLANE)
		{
			ud_wrerr("Entity Picked is not Planar.");
			goto done;
		}
	}

	sfpt->key = e.key;
	sfpt->relnum = e.rel_num;
	ncl_get_label(&e,sfpt->label);
	ncl_get_geo_color (e.key,&sfpt->color);
/*
.....Update the entities color
*/
	if (color != -1)
	{
		ncl_update_geo_color(e.key,color,UU_TRUE);
		uc_display(&e);
	}
	strncpy(Szlev,Spbot->label, NCL_MAX_LABEL);
	ud_update_answer(FPLV,(int *)Szlev);
/*
.....End of routine
.....Redisplay form
*/
done:;
	ud_unlimit();
	ud_form_vis();
	UD_UNMARK(cmdreject);

	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnVecSel(filedno, val, stat)
**       Routine to select the offset vector.
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
static UD_FSTAT OnVecSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int numint;
	char label[MXLAB];
	struct NCL_fixed_databag e;
	UM_PLOCREC pick;
	UU_LOGICAL cmdreject;

	if (*fieldno != FPDR) return (UD_BADRNG);
/*
.....Take down form
*/
	ud_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK (cmdreject, UU_TRUE);
	if (cmdreject != 0) goto done;
/*
.....Set the appropriate selection mask
*/
	ud_lgeo(UU_TRUE,UD_ncl_ve);
/*
.....Get the next geometry selection
*/
	ua_dl_pldas(UD_DASPCKLOC,UA_NCL,703,&pick,1,&numint,1);
	if (numint == 0) goto done;

	e.key = um_get_pickkey(&(pick.pent),1);
	if (ncl_retrieve_data_fixed(&e) != 0) goto done;

	ncl_get_label(&e,label);
	strcpy(Svector,label);
	ud_update_answer(FPVE,(int *)Svector);
/*
.....End of routine
.....Redisplay form
*/
done:;
	ud_unlimit();
	ud_form_vis();
	UD_UNMARK(cmdreject);

	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnAction(fieldno, val, stat)
**			Method called when an Action button is pressed.
**    PARAMETERS   
**       INPUT  : fieldno = Form field which initiated this call.
**			         val     = Current field value.
**			         stat    = Not used.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnAction(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int isw;
/*
.....Set correct switch
.....depending on form type that is active
*/
	isw = *fieldno;
	if (Sfrmtyp == 2)
	{
		if (isw == FBVW) isw = FVIW;
		else if (isw == FBPR) isw = FPRE;
		else if (isw == FBAP) isw = FAPP;
	}
	switch (isw)
	{
/*
.....Enter viewing mode
*/
	case FVIW:
		ud_form_invis();
		uz_dyn_mouse();
		ud_form_vis();
		break;
/*
.....Preview command
*/
	case FPRE:
		if (Sfrmtyp == 0)
		{
			if (Slayincl == 0)
			{
				S_delete_preview();
				S_build_cmd(UU_TRUE);
			}
		}
		else if (Sfrmtyp == 2)
		{
			S_delete_preview();
			S_build_boxcmd(UU_TRUE);
		}
		break;
/*
.....Apply - Output command
*/
	case FAPP:
		if (Sfrmtyp == 0)
		{
			if (Slayincl == UU_FALSE || Snsurf > 0)
			{
				S_delete_preview();
				S_build_cmd(UU_FALSE);
			}
		}
		else if (Sfrmtyp == 2)
		{
			if (Slayincl == UU_FALSE || Snsurf > 0)
			{
				S_delete_preview();
				S_build_boxcmd(UU_FALSE);
			}
		}
	}
	return (UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : OnApply(fieldno, val, stat)
**			Method called when the Apply button in the Composite Solid form
**       is pressed.
**    PARAMETERS   
**       INPUT  : fieldno = Form field which initiated this call.
**			         val     = Current field value.
**			         stat    = Not used.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnApply(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int nc;
	UM_sgeo *geo;
/*
.....Deselect geometry
*/
	geo = (UM_sgeo *)UU_LIST_ARRAY(&Ssurf); nc = UU_LIST_LENGTH(&Ssurf);
	nclu_repaint (geo,nc,-1);
/*
.....Output the SOLID/COMPOS command
*/
	S_build_compcmd();
	UU_LIST_EMPTY(&Ssurf); Snsurf = 0;
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  OnBrowse(filedno, val, stat)
**       Routine to get the Stock file name.
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
static UD_FSTAT OnBrowse(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char sbuf[80],ext[UX_SUFFIX_LEN],descrip[80];
	int inum;
/*
.....Get filename to load
*/
	if (Sfrmtyp == 3)
	{
		strcpy(sbuf,"Save STL File");
		strcpy(ext,"*.stl");
		strcpy(descrip,"STL Files (*.stl)");
	}
	else
	{
		strcpy(sbuf,"Save Stock File");
		strcpy(ext,"*.stk");
		strcpy(descrip,"Stock Files (*.stk)");
	}
	inum = 0;
	Sfile[0] = '\0';
	ud_get_filename(sbuf,sbuf,ext,Sfile,&inum,descrip, 1, UU_TRUE);
	if (inum != 0)
	{
		if (Sfrmtyp == 3) ux_add_ftype("stl",Sfile,UX_NPRTERRS);
		else ux_add_ftype("stk",Sfile,UX_NPRTERRS);
		ud_update_answer(FSFN,(int *)Sfile);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_calc_box()
**       Calculates the bounding box and updates the form with the
**       lower left and upper right coordinates.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS :
**       none
**    WARNINGS     : none
*********************************************************************/
static void S_calc_box()
{
	int i,nt,status,modals[20],icurpt,ietype;
	UU_REAL box[6];
	UU_KEY_ID skey;
	UN_clstruc *iclpt[4];
	UM_sgeo *geo;
/*
.....Calculate motion bounding box
*/
	if (Sbtype == 2)
	{
		ncl_play_initscan(modals,iclpt,&icurpt,UU_FALSE);
		nt = 0;
		ncl_motion_playback(modals,2,Sbound,&nt);
		ncl_play_resetscan(iclpt,icurpt);
		status = UU_SUCCESS;
	}
/*
.....Calculate bounding box for all geometry
*/
	else if (Sbtype == 1)
	{
		Sbound[0] = Sbound[1] = Sbound[2] = 100000.;
		Sbound[3] = Sbound[4] = Sbound[5] = -100000.;
		vxlfst();
		while (UU_TRUE)
		{
/*
........Get next geometry entity
*/
			if (!ncl_vxlnxt(&skey,&ietype)) break;
			if (ietype == NCLI_MPARM || ietype == NCLI_LABEL || skey == 0)
				continue;
/*
........Calculate box
*/
			status = ncl_geo_box(skey,box);
/*
........Merge boxes
*/
			if (status == UU_SUCCESS)
			{
				ncl_update_box(&box[0],Sbound);
				ncl_update_box(&box[3],Sbound);
			}
		}
		if (Sbound[3] < Sbound[0]) status = UU_FAILURE;
	}
/*
.....Calculate bounding box using geometry list
*/
	else if (Snsurf != 0)
	{
		Sbound[0] = Sbound[1] = Sbound[2] = 100000.;
		Sbound[3] = Sbound[4] = Sbound[5] = -100000.;
		geo = (UM_sgeo *)UU_LIST_ARRAY(&Ssurf);
		for (i=0;i<Snsurf;i++)
		{
/*
........Calculate box
*/
			status = ncl_geo_box(geo[i].key,box);
/*
........Merge boxes
*/
			if (status == UU_SUCCESS)
			{
				ncl_update_box(&box[0],Sbound);
				ncl_update_box(&box[3],Sbound);
			}
		}
		if (Sbound[3] < Sbound[0]) status = UU_FAILURE;
	}
/*
.....No geometry selected
*/
	else
		status = UU_FAILURE;
/*
.....Update the form
*/
	if (status == UU_SUCCESS)
	{
		for (i=0;i<6;i++) ud_update_answer(FBLX+i,&Sbound[i]);
	}
	return;
}

/*********************************************************************
**    I_FUNCTION     : S_delete_preview()
**       Deletes the temporary geometry created by the Preview button.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS :
**       none
**    WARNINGS     : none
*********************************************************************/
static void S_delete_preview()
{
	int nc,inc;
	UU_KEY_ID key;
	UM_f77_str f77_str;
/*
.....See if preview geometry exists
*/
	nc = strlen(Stmplab);
	UM_init_f77_str(f77_str,Stmplab,nc);
	getkey(UM_addr_of_f77_str(f77_str),&key);
/*
.....Delete preview geometry
*/
	if (key != 0) dlgeom(&key);
}

/*********************************************************************
**    I_FUNCTION     : S_build_cmd(previw)
**		   Build and output command(s).
**    PARAMETERS   
**       INPUT  :
**          previw  = UU_TRUE = Previewing temporary command.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS :
**			May output a DRAFT/MODIFY=sf1,...,LAYER=1 command
**    WARNINGS     : none
*********************************************************************/
static int S_build_cmd(previw)
UU_LOGICAL previw;
{
	NCL_cmdbuf cmdbuf;
	UU_LOGICAL cmdreject;
	int i = 1,nc;
	char buf[MXLAB];
/*
.....Trap command reject
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto done;
/*
.....Initialize command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
/*
.....Output layer creation command
*/
	if (Slayincl && Snsurf > 0) nclu_laycmd(&Ssurf,Slay_num,i);
/*
.....Add Preview style label
*/
	if (previw)
	{
		sprintf(buf,"*%s=",Stmplab);
		ncl_add_token(&cmdbuf,buf,NCL_nocomma);
}
/*
.....Add label
*/
	else
	{
		nc = strlen(Slabel);
		ul_strip_blanks(Slabel,&nc);
		if (nc != 0)
		{
			sprintf(buf,"%s=",Slabel);
			ncl_add_token(&cmdbuf,buf,NCL_nocomma);
		}
	}

	ncl_add_token(&cmdbuf, NCL_so, NCL_nocomma);
	ncl_add_token(&cmdbuf, NCL_part, NCL_comma);
	ncl_add_token(&cmdbuf, Soffset, NCL_comma);

	if (Saddlayer)
	{
		ncl_add_token(&cmdbuf, NCL_layer, NCL_nocomma);
		ncl_add_token(&cmdbuf, Slay_num, NCL_comma);
	}

	if (!Slayincl && Snsurf > 0)
	{
		UM_sgeo *geo = (UM_sgeo *) UU_LIST_ARRAY(&Ssurf);

		for (i = 0; i < Snsurf; i++)
		{
			ncl_add_token(&cmdbuf, geo[i].label, NCL_comma);
		}
	}

	ncl_add_token(&cmdbuf,NCL_at,NCL_comma);
	if (Sprojto == ZLEV || Sprojto == PLANE)
	{
		ncl_add_token(&cmdbuf,Szlev,NCL_comma);
		ncl_add_token(&cmdbuf,Sheight,NCL_comma);
	}
	else
	{
		ncl_add_token(&cmdbuf,NCL_level,NCL_comma);
		ncl_add_token(&cmdbuf,Sboff,NCL_comma);
		ncl_add_token(&cmdbuf,Stoff,NCL_comma);
	}

/*	if (Sprojto != LEVEL)*/
	{
		nc = strlen(Svector);
		ul_strip_blanks(Svector,&nc);
		if (nc != 0)
		{
			ncl_add_token(&cmdbuf,NCL_offset,NCL_comma);
			ncl_add_token(&cmdbuf,Svector,NCL_comma);
		}
	}

	if (Sbox == 0) ncl_add_token(&cmdbuf,NCL_prof,NCL_comma);
	else ncl_add_token(&cmdbuf,NCL_box,NCL_comma);

	ncl_set_cmdmode(UU_TRUE);		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
/*
.....End of routine
*/
done:
	UD_UNMARK(cmdreject);
	return (UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_build_savecmd()
**		   Build and output SAVE/SOLID command.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_build_savecmd()
{
	NCL_cmdbuf cmdbuf;
	UU_LOGICAL cmdreject;
	int i;
	UM_sgeo *geo;
/*
.....Trap command reject
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto done;
/*
.....Initialize command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);

	ncl_add_token(&cmdbuf, NCL_save, NCL_nocomma);
	ncl_add_token(&cmdbuf, NCL_solid, NCL_comma);

	if (Sstype == 0) ncl_add_token(&cmdbuf, NCL_stock, NCL_comma);
	else ncl_add_token(&cmdbuf, NCL_fixtur, NCL_comma);

	ul_add_quotes(Sfile,Sfile);
	ncl_add_token(&cmdbuf, Sfile, NCL_comma);

	geo = (UM_sgeo *) UU_LIST_ARRAY(&Ssurf);
	for (i=0; i<Snsurf; i++)
		ncl_add_token(&cmdbuf, geo[i].label, NCL_comma);

	ncl_set_cmdmode(UU_TRUE);		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
/*
.....End of routine
*/
done:
	UD_UNMARK(cmdreject);
	return (UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_build_boxcmd(previw)
**		   Build and output command(s).
**    PARAMETERS   
**       INPUT  :
**          previw  = UU_TRUE = Previewing temporary command.
**       OUTPUT :  
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_build_boxcmd(previw)
UU_LOGICAL previw;
{
	NCL_cmdbuf cmdbuf;
	UU_LOGICAL cmdreject;
	int i,nc,inum,status;
	UU_REAL exp[3];
	char buf[MXLAB];
	UM_sgeo *geo;
/*
.....Don't output command if no surfaces
*/
	if (Snsurf == 0 && Sbtype == 0 && Saddlayer == UU_FALSE) return(UU_FAILURE);
/*
.....Trap command reject
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto done;
/*
.....Initialize command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
/*
.....Output layer creation command
*/
	if (Slayincl && Snsurf > 0) nclu_laycmd(&Ssurf,Slay_num,i);
/*
.....Add Preview style label
*/
	if (previw)
	{
		sprintf(buf,"*%s=",Stmplab);
		ncl_add_token(&cmdbuf,buf,NCL_nocomma);
}
/*
.....Add label
*/
	else
	{
		nc = strlen(Slabel);
		ul_strip_blanks(Slabel,&nc);
		if (nc != 0)
		{
			sprintf(buf,"%s=",Slabel);
			ncl_add_token(&cmdbuf,buf,NCL_nocomma);
		}
	}
/*
.....Construct basic command
*/
	ncl_add_token(&cmdbuf, NCL_so, NCL_nocomma);
	ncl_add_token(&cmdbuf, NCL_box, NCL_comma);
	ncl_add_token(&cmdbuf, NCL_bound, NCL_comma);
/*
.....Output expansion values
*/
	for (i=0;i<3;i++)
	{
		nc = strlen(Sexp[i]);
		ul_strip_blanks(Sexp[i],&nc);
		if (nc == 0)
		{
			exp[i] = 0;
			strcpy(Sexp[i],"0.");
		}
		else
		{
			status = ul_to_reals(&exp[i],&inum,1,Sexp[i]);
			if (status != UU_SUCCESS) exp[i] = i * 1000;
		}
	}
	if (exp[0] != 0. || exp[1] != 0. || exp[2] != 0.)
	{
		ncl_add_token(&cmdbuf,Sexp[0],NCL_comma);
		if (exp[1] != exp[0] || exp[2] != exp[0])
		{
			ncl_add_token(&cmdbuf,Sexp[1],NCL_comma);
			ncl_add_token(&cmdbuf,Sexp[2],NCL_comma);
		}
	}
/*
.....Add geometry
*/
	if (Sbtype == 0)
	{
		if (Saddlayer)
		{
			ncl_add_token(&cmdbuf,NCL_layer,NCL_nocomma);
			ncl_add_token(&cmdbuf,Slay_num,NCL_comma);
		}
		if (!Slayincl && Snsurf > 0)
		{
			geo = (UM_sgeo *)UU_LIST_ARRAY(&Ssurf);

			for (i = 0; i < Snsurf; i++)
				ncl_add_token(&cmdbuf, geo[i].label, NCL_comma);
		}
	}
	else if (Sbtype == 1)
		ncl_add_token(&cmdbuf, NCL_all, NCL_comma);
	else
		ncl_add_token(&cmdbuf, NCL_cmd_motion, NCL_comma);
/*
.....Output command
*/
	ncl_set_cmdmode(UU_TRUE);		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
/*
.....End of routine
*/
done:
	UD_UNMARK(cmdreject);
	return (UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_build_compcmd()
**		   Build and output SOLID/COMP command.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_build_compcmd()
{
	NCL_cmdbuf cmdbuf;
	UU_LOGICAL cmdreject;
	int i;
	UM_sgeo *geo;
/*
.....Trap command reject
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto done;
/*
.....Initialize command buffer
*/
	if (Snsurf == 0 && Scomp != 2) goto done;
	ncl_init_cmdbuf(&cmdbuf);

	ncl_add_token(&cmdbuf, NCL_so, NCL_nocomma);
	ncl_add_token(&cmdbuf, NCL_compos, NCL_nocomma);

	if (Sclosed == 1) ncl_add_token(&cmdbuf, NCL_close, NCL_comma);
	else ncl_add_token(&cmdbuf, NCL_open, NCL_comma);

	if (Sentact == 0) ncl_add_token(&cmdbuf, "INVIS", NCL_comma);
	else if (Sentact == 1) ncl_add_token(&cmdbuf, NCL_retain, NCL_comma);
	else ncl_add_token(&cmdbuf, "REMOVE", NCL_comma);

	if (Scomp == 2)
		ncl_add_token(&cmdbuf, NCL_all, NCL_nocomma);
	else
	{
		geo = (UM_sgeo *) UU_LIST_ARRAY(&Ssurf);
		for (i=0; i<Snsurf; i++)
			ncl_add_token(&cmdbuf, geo[i].label, NCL_comma);
	}

	ncl_set_cmdmode(UU_TRUE);		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
/*
.....End of routine
*/
done:
	UD_UNMARK(cmdreject);
	return (UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_build_stlcmd()
**		   Build and output SAVE/STL command.
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT : none
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_build_stlcmd()
{
	NCL_cmdbuf cmdbuf;
	UU_LOGICAL cmdreject;
	int i;
	UM_sgeo *geo;
/*
.....Trap command reject
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0) goto done;
/*
.....Initialize command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);

	ncl_add_token(&cmdbuf, NCL_save, NCL_nocomma);
	ncl_add_token(&cmdbuf, NCL_stl, NCL_comma);

	if (Sformat == 0) ncl_add_token(&cmdbuf, "1", NCL_comma);
	else ncl_add_token(&cmdbuf, "2", NCL_comma);

	ncl_add_token(&cmdbuf,Stoler,NCL_comma);

	ul_add_quotes(Sfile,Sfile);
	ncl_add_token(&cmdbuf, Sfile, NCL_comma);

	geo = (UM_sgeo *) UU_LIST_ARRAY(&Ssurf);
	for (i=0; i<Snsurf; i++)
		ncl_add_token(&cmdbuf, geo[i].label, NCL_comma);

	ncl_set_cmdmode(UU_TRUE);		
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
/*
.....End of routine
*/
done:
	UD_UNMARK(cmdreject);
	return (UU_SUCCESS);
}
