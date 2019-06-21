/*********************************************************************
**    NAME         :  nucv1.c
**       CONTAINS:
**      nclu_cv_sfs()
**      nclu_ssplin_compos()
**
**    COPYRIGHT 2004 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nucv1.c , 25.4
**     DATE AND TIME OF LAST  MODIFICATION
**       06/01/15 , 07:44:34
*********************************************************************/
#include <string.h>
#include "nclmplay.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mxxx.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclmodals.h"
#include "nclvx.h"
#include "nclupok.h"

#define WCMP 0 /* All or Composite */
#define WOFF 0 /* Offset factor */
/*..... Selection of Surfaces ...., */
#define WSLS 1
#define WCOL 2
#define WIOL 3 /* Include on Layer */
#define WDES 4
/*..... Layer ..... */
#define WLSL 5
#define WLAY 6
#define WLAD 7
#define WLSH 8
/*..... Level .....*/
#define WBTT 9
#define WBOT 10
#define WSLB 11
#define WCOB 12
/*..... Offset .....*/
#define WOCK 13
#define WODS 14
#define WODR 15

#define WSCA 16
#define WICL 17
#define WVIW 18
#define WPVW 19
#define WAPY 20

#define ALL 0
#define COMP 1

#define ZLEV 0
#define PLANE 1
#define SURF 2

#define XLARGE 0
#define XSMALL 1
#define YLARGE 2
#define YSMALL 3
#define ZLARGE 4
#define ZSMALL 5

#define MXLAB NCL_MAX_LABEL_AND_SUBSCRIPT+1

static UU_LIST *Slaylist = UU_NULL;
static UU_LIST Ssurf;
static int nsurf = 0;

static int Sbottype,Slayer,Slistlayer,Scomp,Stype;
static int Scolor,Scolorb;
static int Saddlayer,Slayincl,Soffchk,Soffdir;

static char Spoc_bot[MXLAB],Slay_num[STRL],Soff_set[STRL],Soff_dis[STRL];
static char Snum_cvs[MXLAB],Sini_lab[MXLAB];
static char *Stmplab={"@UZRY"};
static UM_sgeo *Spbot = UU_NULL;

static UU_LIST strings;

UD_FSTAT ncl_show_layer();

/*********************************************************************
**    I_FUNCTION     : build_sfs_cmd(flag,previw)
**		Build and output command(s).
**    PARAMETERS
**       INPUT  : flag    = 0: output CV/INTOF,ALL or CV/INTOF,COMPOS command.
**			                   1: output CV/PART command
**                previw  = UU_TRUE = Previewing temporary command.
**       OUTPUT :
**          none.
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS :
**			May output a DRAFT/MODIFY=sf1,...,LAYER=1 command
**    WARNINGS     : none
*********************************************************************/
static int build_sfs_cmd(flag,previw)
int flag;
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
	if (cmdreject != 0) goto fini;
/*
.....Initialize command buffer
*/
	ncl_init_cmdbuf(&cmdbuf);
/*
.....Output layer creation command
*/
	if (Slayincl && nsurf > 0) nclu_laycmd(&Ssurf,Slay_num,i);
/*
.....Add Preview style label
*/
	if (previw)
	{
		sprintf(buf,"*%s(1)=",Stmplab);
		ncl_add_token(&cmdbuf,buf,NCL_nocomma);
	}
/*
.....Add label
*/
	else
	{
		nc = strlen(Sini_lab);
		ul_strip_blanks(Sini_lab,&nc);
		if (nc != 0)
		{
			sprintf(buf,"%s=",Sini_lab);
			ncl_add_token(&cmdbuf,buf,NCL_nocomma);
		}
	}

	if (flag == 0)
	{
		ncl_add_token(&cmdbuf, NCL_cv, NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_intof, NCL_comma);
		if (Scomp == ALL)
			ncl_add_token(&cmdbuf, NCL_all, NCL_comma);
		else
			ncl_add_token(&cmdbuf, NCL_compos, NCL_comma);
	}
	else
	{
		ncl_add_token(&cmdbuf, NCL_spline, NCL_nocomma);
		ncl_add_token(&cmdbuf, NCL_part, NCL_comma);
		ncl_add_token(&cmdbuf, Soff_set, NCL_comma);
	}

	if (Saddlayer)
	{
		ncl_add_token(&cmdbuf, NCL_layer, NCL_nocomma);
		ncl_add_token(&cmdbuf, Slay_num, NCL_comma);
	}

	if (!Slayincl && nsurf > 0)
	{
		UM_sgeo *geo = (UM_sgeo *) UU_LIST_ARRAY(&Ssurf);

		for (i = 0; i < nsurf; i++)
		{
			ncl_add_token(&cmdbuf, geo[i].label, NCL_comma);
		}
	}

	ncl_add_token(&cmdbuf,NCL_at,NCL_comma);
	if (Sbottype > ZLEV && Soffchk)
	{
		ncl_add_token(&cmdbuf,Spoc_bot,NCL_comma);
		if (Soffdir == XLARGE)
			ncl_add_token(&cmdbuf,NCL_xlarge,NCL_comma);
		else if (Soffdir == XSMALL)
			ncl_add_token(&cmdbuf,NCL_xsmall,NCL_comma);
		else if (Soffdir == YLARGE)
			ncl_add_token(&cmdbuf,NCL_ylarge,NCL_comma);
		else if (Soffdir == YSMALL)
			ncl_add_token(&cmdbuf,NCL_ysmall,NCL_comma);
		else if (Soffdir == ZLARGE)
			ncl_add_token(&cmdbuf,NCL_zlarge,NCL_comma);
		else
			ncl_add_token(&cmdbuf,NCL_zsmall,NCL_comma);
		ncl_add_token(&cmdbuf,Soff_dis,NCL_comma);
	}
	else
		ncl_add_token(&cmdbuf,Spoc_bot,NCL_comma);

	nc = strlen(Snum_cvs);
	ul_strip_blanks(Snum_cvs,&nc);
	if (flag == 0 && nc != 0 && !previw)
		ncl_add_token(&cmdbuf,Snum_cvs,NCL_nocomma);

	ncl_set_cmdmode(UU_TRUE);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);

fini:
	UD_UNMARK(cmdreject);

	return (UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     : S_put_list()
**       Adds a geometry selection to the internal list.
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
static void S_put_list(list,sbuf)
UD_LIST *list;
char *sbuf;
{
	char *buf;

	buf = (char *) uu_malloc((strlen(sbuf)+1)*sizeof(char));
	strcpy (buf,sbuf);
	uu_list_push (&strings,&buf);

	list->item = (char **) UU_LIST_ARRAY(&strings);
	list->num_item++;
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
	int nc,inc,i;
	char label[MXLAB];
	UU_KEY_ID key;
	UM_f77_str f77_str;
/*
.....See if preview geometry exists
*/
	inc = 0;
	do
	{
		inc++;
		sprintf(label,"%s(%d)",Stmplab,inc);
		nc = strlen(label);
		UM_init_f77_str(f77_str,label,64);
		for (i=nc;i<64;i++) label[i] = ' ';
		getkey(UM_addr_of_f77_str(f77_str),&key);
/*
.....Delete preview geometry
*/
		if (key != 0) dlgeom(&key);
	} while (key != 0);
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
		ud_update_answer(WLAY,(int *)Slay_num);
		ud_update_form (0);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnSurfSel(filedno, val, stat)
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
static UD_FSTAT OnSurfSel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int numint,init,color;
	struct NCL_fixed_databag e;
	UU_LOGICAL cmdreject;
	UM_sgeo geo;

	if (*fieldno != WSLS) return (UD_FLDOK);
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
	ud_lgeo(UU_TRUE,UD_ncl_allsfsh);
/*
.....Get the next geometry selection
*/
	ud_ldas(UD_DASSELECT,UA_NCL,478,UU_NULL,1,&numint,UD_NODEFAULT);
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
	nsurf = Ssurf.cur_cnt;

	if (nsurf > 40)
	{
		Slayincl = Saddlayer = 1;
		ud_update_answer(WIOL,&Slayincl);
		ud_update_answer(WLAD,&Saddlayer);
		ud_set_traverse_mask(WIOL,0);
		ud_set_traverse_mask(WLAD,0);
		ud_set_traverse_mask(WLSL,1);
		ud_set_traverse_mask(WLAY,1);
		ud_set_traverse_mask(WLSH,0);
		ud_set_traverse_mask(WPVW,0);
	}
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
	if (*fieldno != WLSH) return (UD_FLDOK);

	if (!Slaylist)
	{
		Slaylist = (UU_LIST *) uu_malloc (sizeof (UU_LIST));
		uu_list_init(Slaylist,sizeof(UM_sgeo),0,100);
	}

	return (ncl_show_layer(Slaylist,Scolor,Slayer,&Slistlayer,2));
}

/*********************************************************************
**    I_FUNCTION     : OnSfsDesel(fieldno, val, stat)
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
static UD_FSTAT OnSfsDesel(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UM_sgeo *geo;
/*
.....Remove all entities in this list
*/
	geo = (UM_sgeo *) UU_LIST_ARRAY(&Ssurf);
	nclu_repaint (geo,nsurf,-1);
	Ssurf.cur_cnt = 0; nsurf = 0;
	Slayincl = Saddlayer = 0;
	ud_update_answer(WIOL,&Slayincl);
	ud_update_answer(WLAD,&Saddlayer);
	ud_set_traverse_mask(WIOL,1);
	ud_set_traverse_mask(WLAD,1);
	ud_set_traverse_mask(WLSL,0);
	ud_set_traverse_mask(WLAY,0);
	ud_set_traverse_mask(WLSH,0);
	ud_set_traverse_mask(WPVW,1);
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
	char *name = "Pocket Geometry";

	if (i != Slayer)
	{
		Slayer = i;
		um_set_layer_num(i);
		um_set_layer_name(i,name);
/*		return(OnSfsDesel(fieldno, val, stat));*/
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnWatEnable()
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
static UD_FSTAT OnSfsEnable(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int ltf;

	switch (*fieldno)
	{
	case WIOL:
		if (Slayincl == 1)
		{
			Saddlayer = 1;
			ud_update_answer(WLAD,&Saddlayer);
			ud_set_traverse_mask(WLAD,0);
			ud_set_traverse_mask(WLSL,1);
			ud_set_traverse_mask(WLAY,1);
			ud_set_traverse_mask(WLSH,0);
			ud_set_traverse_mask(WPVW,0);
		}
		else
		{
			Saddlayer = 0;
			ud_update_answer(WLAD,&Saddlayer);
			ud_set_traverse_mask(WLAD,1);
			ud_set_traverse_mask(WLSL,0);
			ud_set_traverse_mask(WLAY,0);
			ud_set_traverse_mask(WLSH,0);
			ud_set_traverse_mask(WPVW,1);
		}
		break;

	case WLAD:
		ltf = Saddlayer;
		ud_set_traverse_mask(WLSL,ltf);
		ud_set_traverse_mask(WLAY,ltf);
		ud_set_traverse_mask(WLSH,ltf);
		break;

	case WBTT:
		ltf = (Sbottype > ZLEV);
		ud_set_traverse_mask(WSLB,ltf);
		ud_set_traverse_mask(WCOB,ltf);
		ud_set_traverse_mask(WOCK,ltf);
		ltf = (ltf && Soffchk);
		ud_set_traverse_mask(WODS,ltf);
		ud_set_traverse_mask(WODR,ltf);
		break;

	case WOCK:
		ud_set_traverse_mask(WODS,Soffchk);
		ud_set_traverse_mask(WODR,Soffchk);
		break;

	default:
		break;
	}

	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnPlnSel(filedno, val, stat)
**       Routine to select the Bottom, Top, or First Level plane.
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

	if (*fieldno != WSLB) return (UD_BADRNG);

	if (!Spbot) Spbot = (UM_sgeo *) uu_malloc (sizeof (UM_sgeo));
	sfpt = Spbot;
	color = Scolorb;
	pr = 493;
	mask = (Sbottype == PLANE)? UD_ncl_allsfpl: UD_ncl_allsf;

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

	if (Sbottype >= PLANE && e.rel_num != NCL_PLN_REL)
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
	strncpy(Spoc_bot,Spbot->label, NCL_MAX_LABEL);
	ud_update_answer(WBOT,(int *)Spoc_bot);
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
**    I_FUNCTION     : OnWatColor(fieldno, val, stat)
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
static UD_FSTAT OnSfsColor(fieldno, val, stat)
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
	case WCOL:
		if (nsurf > 0)
		{
			geo = (UM_sgeo *) UU_LIST_ARRAY (&Ssurf); nc = nsurf;
			color = Scolor;
		}
		break;
	case WCOB:
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
	switch (*fieldno)
	{
/*
.....Enter viewing mode
*/
	case WVIW:
		ud_form_invis();
		uz_dyn_mouse();
		ud_form_vis();
		break;
/*
.....Preview command
*/
	case WPVW:
		if (Slayincl == 0)
		{
			S_delete_preview();
			build_sfs_cmd(Stype,UU_TRUE);
		}
		break;
/*
.....Apply - Output command
*/
	case WAPY:
		if (Slayincl == UU_FALSE || nsurf > 0)
		{
			S_delete_preview();
			build_sfs_cmd(Stype,UU_FALSE);
		}
	}
	return (UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : nclu_cv_sfs(flag)
**       Interface for creating
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_cv_sfs(flag)
int flag;
{
	int nc,status;
	UU_LOGICAL cmdreject;
	UU_REAL rnum;
	UM_sgeo *geo;
/*
.....Set up form fields
*/
	static char traverse[] = {1, 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1, 1,1, 1,1,1};
	static char called[] =  {6, 6,6,6,6, 6,6,6,6, 6,6,6,6, 6,6,6, 6,6, 6,6,6};
	static char display[] = {1, 1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1, 1,1, 1,1,1};
	static UD_METHOD methods[] = {UU_NULL,
		OnSurfSel,OnSfsColor,OnSfsEnable,OnSfsDesel,
		OnLayerSel,OnLayer,OnSfsEnable,OnShow,
		OnSfsEnable,UU_NULL,OnPlnSel,OnSfsColor,
		OnSfsEnable,UU_NULL,UU_NULL, UU_NULL,UU_NULL,
		OnAction,OnAction,OnAction};
	static int *ans[] = {&Scomp,
		UU_NULL,&Scolor,&Slayincl,UU_NULL,
		UU_NULL,(int *)Slay_num,&Saddlayer,UU_NULL,
		&Sbottype,(int *)Spoc_bot,UU_NULL,&Scolorb,
		&Soffchk,(int *)Soff_dis,&Soffdir, (int *)Snum_cvs,(int *)Sini_lab,
		UU_NULL,UU_NULL,UU_NULL};
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_FALSE);
	if (cmdreject != 0)	goto fini;
/*
..... Initialize answers
*/
	Stype = flag;
	uu_list_init(&Ssurf,sizeof(UM_sgeo),50,50);
	nsurf = 0;

	if (flag == 0) ans[0] = &Scomp;
	else ans[0] = (int *)&Soff_set;
	Slayer = 999;
	Scolor = 8;
	sprintf(Slay_num,"%d",Slayer);
	rnum = 0.;
	Saddlayer = Slayincl = UU_FALSE;
	Slistlayer = -1;

	Scomp = ALL;
	ncl_sprintf (Soff_set,&rnum,1);

	ncl_sprintf (Spoc_bot,&rnum,1);
	Sbottype = ZLEV;
	Spbot = UU_NULL;
	Scolorb = 10;

	Soffchk = 0;
	ncl_sprintf (Soff_dis,&rnum,1);
	Soffdir = ZLARGE;

	Snum_cvs[0] = '\0';
	Sini_lab[0] = '\0';
/*
.....Set traversal flags
*/
	if (Slayincl == 1) Saddlayer = UU_TRUE;
	traverse[WLSL] = traverse[WLAY] = traverse[WLSH] = Saddlayer;
	traverse[WSLB] = traverse[WCOB] = (Sbottype > ZLEV);
	traverse[WOCK] = traverse[WODS] = traverse[WODR] = 0;
	traverse[WPVW] = 1 - Slayincl;
/*
........Surface intersection form
*/
	if (flag == 0)
	{
		display[WSCA] = traverse[WSCA] = 1;
		display[WOCK] = display[WODS] = display[WODR] = 1;
		if (Sbottype > ZLEV)
		{
			traverse[WOCK] = 1;
			traverse[WODS] = traverse[WODR] = Soffchk;
		}
	}
/*
........Surface contour form
*/
	else
	{
		display[WSCA] = traverse[WSCA] = 0;
		display[WOCK] = display[WODS] = display[WODR] = 0;
		traverse[WOCK] = traverse[WODS] = traverse[WODR] = 0;
	}
/*
.....Get the Form input
*/
repeat:
	if (flag == 0)
		status = ud_form1("sfsio.frm",ans,ans,methods,called,display,traverse);
	else
		status = ud_form1("sfscontour.frm",ans,ans,methods,called,display,
			traverse);
/*
.....Delete last previewed curve
*/
	S_delete_preview();

	if (status == -1) goto done;
	if (Slayincl == UU_TRUE && nsurf == 0) goto repeat;

	build_sfs_cmd(flag,UU_FALSE);

done:
	geo = (UM_sgeo *) UU_LIST_ARRAY (&Ssurf); nc = Ssurf.cur_cnt;
	nclu_repaint (geo,nc,-1);
	uu_list_free(&Ssurf); nsurf = 0;
	if (Slaylist)
	{
		geo = (UM_sgeo *) UU_LIST_ARRAY (Slaylist); nc = Slaylist->cur_cnt;
		nclu_repaint (geo,nc,-1);
		uu_list_free(Slaylist); uu_free(Slaylist);
		Slaylist = UU_NULL; Slistlayer = -1;
	}
	if (Spbot)
	{
		nclu_repaint (Spbot,1,-1);
		uu_free (Spbot);
	}

fini:
	S_delete_preview();
	UD_UNMARK(cmdreject); 
	return;
}

/*********************************************************************
**    E_FUNCTION     : nclu_ssplin_compos()
**       Interface for creating a composite curve of Surface-splines
**       from the edges of surface(s).
**    PARAMETERS
**       INPUT  :
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_ssplin_compos()
{
	int i,stat,ncv,inc;
	UM_int2 ifl,ifl394,idx;
	UU_KEY_ID *keys;
	char label[NCL_MAX_LABEL_AND_SUBSCRIPT];
	UU_LIST sflist;
	NCL_cmdbuf cmdbuf;
	struct NCL_fixed_databag e;
/*
.....Only generate the outer boundary
*/
	ifl = 394; getifl(&ifl,&ifl394);
	idx = 1; setifl(&ifl,&idx);
/*
.....Get the chain of surface splines
*/
	stat = nclu_sfedge_chain(&sflist,UU_TRUE);
	if (stat != UU_SUCCESS) goto done;
/*
uu_list_init (&sflist,sizeof(UU_KEY_ID),100,100);
i = 4586; uu_list_push(&sflist,&i);
i = 2; uu_list_push(&sflist,&i);
i = 4625; uu_list_push(&sflist,&i);
i = 0; uu_list_push(&sflist,&i);
i = 3079; uu_list_push(&sflist,&i);
i = 0; uu_list_push(&sflist,&i);
i = 3079; uu_list_push(&sflist,&i);
i = 8; uu_list_push(&sflist,&i);
i = 3079; uu_list_push(&sflist,&i);
i = 7; uu_list_push(&sflist,&i);
*/
/*
.....Output the SSPLIN/COMPOS command
........If auto label is off, prompt user for label
*/
	ncl_init_cmdbuf(&cmdbuf);
	if (!NCL_auto_label)
		stat = ncl_add_name(&cmdbuf,1);
	ncl_add_token(&cmdbuf,NCL_ssplin,NCL_nocomma);
	ncl_add_token(&cmdbuf,NCL_compos,NCL_nocomma);
/*
.....Add surface names and boundary curve numbers
*/
	inc = 0;
	ncv = UU_LIST_LENGTH(&sflist);
	keys = (UU_KEY_ID *)UU_LIST_ARRAY(&sflist);
	for (i=0;i<ncv;i=i+2)
	{
		if (i == 0 || (i > 1 && keys[i] != keys[i-2]) || i == ncv-2)
		{
			if (inc >= 1)
			{
				if (keys[i-1] == keys[i-3]+1 || (keys[i-1] == 0 && keys[i-3] != 1))
					ncl_add_token(&cmdbuf,"CLW",NCL_comma);
				else
					ncl_add_token(&cmdbuf,"CCLW",NCL_comma);
			}
			if (inc >= 1 && (i > 1 && keys[i] != keys[i-2]))
			{
				sprintf(label,"%d",keys[i-1]+1);
				ncl_add_token(&cmdbuf,label,NCL_comma);
			}
			inc = 0;
			if (i == 0 || (i > 1 && keys[i] != keys[i-2]))
			{
				e.key = keys[i];
				ncl_retrieve_data_fixed(&e);
				ncl_get_label(&e,label);
				ncl_add_token(&cmdbuf,label,NCL_comma);
			}
		}
		else if (i != 0) inc++;
		if (inc == 0 || i == ncv-2)
		{
			sprintf(label,"%d",keys[i+1]+1);
			ncl_add_token(&cmdbuf,label,NCL_comma);
		}
	}
/*
.....Output command
*/
	ncl_set_cmdmode(UU_TRUE);
	ncl_add_cmdbuf(&cmdbuf);
	ncl_call(&cmdbuf);
/*
.....End of routine
*/
done:;
	setifl(&ifl,&ifl394);
	uu_list_free(&sflist);
}
