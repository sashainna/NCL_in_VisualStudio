/*********************************************************************
**    NAME         :  lipvtool.c
**       CONTAINS:
**				ul_ipv_tool_list()
**				ul_ipv_tool_modals()
**				ulf_ipv_set_rapid()
**    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvtool.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       12/01/15 , 08:21:15
*********************************************************************/

#include "usysdef.h"
#include "stdio.h"
#include "lcom.h"
#include "lipv.h"
#include "mfort.h"
#include "mdcpln.h"
#include "nclfc.h"
#include "nclmplay.h"
#include "nclfile.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "uhep.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "driver.h"

#define MAXDEL 5

static UD_FSTAT OnText(),OnChoice(),OnShank(),OnApply(),OnTool(),OnDelete();
static UD_FSTAT OnUndelete(),OnReset(),OnRescan(),OnInitForm();

static void S_update_form();

static void S_load_tools(),S_load_list(),S_default_tool();
static void S_fmt_tool(),S_update_toolnum();
static void S_tool_ctype();

extern UD_METHOD UD_initfrm_intry;

static int Scut_color,Scolor[3],Stransp[3],Strans0,Sclash,Ssymfl[3],Stype;
static int Sctr0,Sntools,Scfl[3],Sedge0,Sedge[3],Sedgcol0,Sedgcol[3];
static int Sced0,Scec0;
static UU_REAL Stoler,Sminhgt,Smaxhgt,Smindia,Scutr[6],Smaxang,Srapid;
static char Ssymbol[3][MAXSYMLEN],Satt[12][NCL_MAX_LABEL+1];
static char Spv[3][NCL_MAX_LABEL+1];
static UU_LIST Smylist;
static UD_LIST Stool_list;

static int Spseudo=0;
static int Stoolnum=0;
static int Sndel=0;
static int Slast_del[MAXDEL]={-1,-1,-1,-1,-1};
static UN_cutter_list *Scpt=UU_NULL;

static int myntl;

#define NLAB 1

#define FDIA 0
#define FRAD 1
#define FHGT 2
#define FANG 3
#define FZHT 4
#define FFLA 5
#define FCUT 6
#define FTOL 7
#define FMAX 8
#define FRAP 9
#define FEDG 10
#define FEDC 11
#define FTRS 12
#define FSY1 13
#define FSY2 14
#define FSY3 15
#define FCO1 16
#define FSH1 17
#define FSH2 18
#define FSH3 19
#define FCO2 20
#define FHD1 21
#define FHD2 22
#define FHD3 23
#define FCO3 24
#define FMNH 25
#define FMXH 26
#define FMND 27
#define FAPP 28
#define FLST 29
#define FDEL 30
#define FUND 31
#define FRES 32
#define FRSC 33

/*********************************************************************
**    E_FUNCTION     : ul_ipv_tool_list()
**       Processes the NCLIPV Edit Tool List form.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ul_ipv_tool_list()
{
	int i,status,ifl;
	UD_METHOD save_entry;
	UU_LIST temp;

	static int redisp_form = 0;
	static int mainmarkval=0;
/*
.....Set up form fields
*/
	static char traverse[]     = {1,1,1,1,1,1, 1,1,1,1, 1,1,1,
		1,1,1,1, 1,1,1,1, 1,1,1,1, 1,1,1,1, 1, 1,0,1,1};

	static UD_METHOD methods[] = {OnText,OnText,OnText,OnText,OnText,OnText,
		UU_NULL,OnText,OnText,OnText,
		OnText,OnText,OnText,
		OnChoice,OnShank,UU_NULL,UU_NULL,
		OnChoice,OnShank,UU_NULL,UU_NULL,
		OnChoice,OnShank,UU_NULL,UU_NULL,
		OnText,OnText,OnText,OnApply,
		OnTool,
		OnDelete,OnUndelete,OnReset,OnRescan};

	static char called[]     = {6,6,6,6,6,6, 6,6,6,6, 6,6,6,
		6,6,6,6, 6,6,6,6, 6,6,6,6, 6,6,6,6, 6, 6,6,6,6};

	static char display[] = {1, 1,1,1,1,1,1, 1,1,1,1,1, 1,1,1,
		1,1,1,1, 1,1,1,1,  1,1,1,1,1, 1,1,1,1, 1, 1,1,1,1};

	static int *ans[] = {(int *)&Scutr[0],(int *)&Scutr[1],
		(int *)&Scutr[2],(int *)&Scutr[3],(int *)&Scutr[4],(int *)&Scutr[5],
		&Scut_color,(int *)&Stoler,(int *)&Smaxang,(int *)&Srapid, 
		&Sedge0, &Sedgcol0, &Strans0,
		&Ssymfl[0],UU_NULL,(int *)Ssymbol[0],&Scolor[0],
		&Ssymfl[1],UU_NULL,(int *)Ssymbol[1],&Scolor[1],
		&Ssymfl[2],UU_NULL,(int *)Ssymbol[2],&Scolor[2],
		(int *)&Sminhgt,(int *)&Smaxhgt,(int *)&Smindia,UU_NULL,
		(int *)&Stool_list,
		UU_NULL,UU_NULL,UU_NULL,UU_NULL};
/*
.....Initialize routine
*/
	Stool_list.num_item = 0;
	Stool_list.item = UU_NULL;
	Stool_list.answer = UU_NULL;
	Stoolnum = 0;
/*
.....Command Reject
*/
	save_entry = 0;
	UD_MARK (mainmarkval,UU_FALSE);
	if (mainmarkval != 0)
	{
		ud_free_flist(&Stool_list);
		redisp_form = UU_FALSE;
		UD_initfrm_intry = save_entry;
		nclu_tool_free();
		UD_UNMARK (mainmarkval);
		return(UU_SUCCESS);
	}
/*
.....Get available Tools
*/
	if (!redisp_form)
	{
		S_load_tools(UU_FALSE,&Smylist,&myntl);
		if (myntl == 0) goto err1;
		nclu_tool_profs(NULL);
		nclu_tool_syms(-1);
/*
.....Set the form defaults
.....based on the first tool
*/
		Stype = 0;
		if (Scpt[0].type == NCL_CUTTER_LATHE) Stype = 10;
		S_update_toolnum(Stoolnum,0);
	}
/*
....Set the size limits
*/
	UM_len_inttoext(LW_tool_limit[0],Sminhgt);
	UM_len_inttoext(LW_tool_limit[1],Smaxhgt);
	UM_len_inttoext(LW_tool_limit[2],Smindia);
/*
.....No tools left
.....Disable form (except Reset)
*/
	ifl = UU_TRUE;
	if (Stoolnum < 0) ifl = UU_FALSE;
	for (i=0;i<FRES;i++)
	{
		display[i] = UU_TRUE;
		traverse[i] = ifl;
	}
	traverse[FUND] = UU_FALSE;
	traverse[FRES] = UU_TRUE;
	traverse[FRSC] = UU_TRUE;
/*
.....Set traverse fields
*/
	traverse[FSY2] = traverse[FSY3] = Ssymfl[0];
	traverse[FSH2] = traverse[FSH3] = traverse[FCO2] = Ssymfl[1];
	traverse[FHD2] = traverse[FHD3] = traverse[FCO3] = Ssymfl[2];
/*
.....Disable cutter parameters
.....if cutter is symbol or geometry
*/
	if (Ssymfl[0])
	{
		for (i=FDIA;i<=FFLA;i++) traverse[i] = 0;
	}
/*
.....Get the Form input
*/
form:;
	redisp_form = 1;
	save_entry = UD_initfrm_intry;
	UD_initfrm_intry = OnInitForm;
	status = ud_form1("ipvtool.frm", ans, ans, methods, called, display, traverse);
	UD_initfrm_intry = save_entry;
	ud_free_flist(&Stool_list);
	if (status == -1) goto done;
/*
.....Save the active form tool
*/
	S_update_toolnum(Stoolnum,1);
/*
.....Store the updated list
*/
	if (LW_ntool > 0) uu_list_free(&LW_tool_list);
	uu_move_byte(&LW_tool_list,&temp,sizeof(UU_LIST));
	uu_move_byte(&Smylist,&LW_tool_list,sizeof(UU_LIST));
	uu_move_byte(&temp,&Smylist,sizeof(UU_LIST));
	LW_ntool = myntl;
	myntl = 0;
	if (Scpt[LW_act_tool[0]].color[0] != -1)
		LW_tool_material = Scpt[LW_act_tool[0]].color[0];
	if (Scpt[LW_act_tool[0]].cut_color != -1)
		LW_cut_material = Scpt[LW_act_tool[0]].cut_color;
	UM_len_exttoint(Sminhgt,LW_tool_limit[0]);
	UM_len_exttoint(Smaxhgt,LW_tool_limit[1]);
	UM_len_exttoint(Smindia,LW_tool_limit[2]);
	goto done;
/*
.....No tools defined in clfile
*/
err1:;
	ud_wrerr("No tools are defined in clfile.");
done:;
	nclu_tool_free();
	redisp_form = 0;
	UD_UNMARK(mainmarkval);
	return(UU_SUCCESS);
}

/*********************************************************************
**    I_FUNCTION     :  OnInitForm(fieldno, val, stat)
**       Method called at the Edit Tool List form is initialized.
**    PARAMETERS
**       INPUT  :
**          fieldno  Ignored.
**          val      Ignored.
**          stat     Ignored.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnInitForm(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Set Cutter field labels
*/
	if (Scpt[Stoolnum].type == NCL_CUTTER_LATHE)
	{
		ud_update_prompt(FDIA,"Tool Radius:");
		ud_update_prompt(FRAD,"Diameter:");
		ud_update_prompt(FANG,"Angle:");
		ud_update_prompt(FZHT,"Mount/Length:");
		ud_update_prompt(FFLA," ");
	}
	else if (Scpt[Stoolnum].type == NCL_CUTTER_BLADE)
	{
		ud_update_prompt(FDIA,"Width:");
		ud_update_prompt(FRAD,"Chizel:");
		ud_update_prompt(FANG,"Angle:");
		ud_update_prompt(FZHT," ");
		ud_update_prompt(FFLA," ");
	}
	else
	{
		ud_update_prompt(FDIA,"Diameter:");
		ud_update_prompt(FRAD,"Corner Radius:");
		ud_update_prompt(FANG,"Side Angle/Radius:");
		ud_update_prompt(FZHT,"Z-Height:");
		ud_update_prompt(FFLA,"Flat Angle:");
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnTool(fieldno, val, stat)
**       Method called at when  tool in the tool listbox
**			is selected
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
static UD_FSTAT OnTool(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char temp[80], *tok, *strtok();

	if (*fieldno != FLST) return(UD_FLDOK);
/*
.....Save the current settings
*/
	S_update_toolnum(Stoolnum,1);
/*
.....val.frmstr contains the selected string
....."toolnum CUTTER/..."
*/
	if (val->frmstr!=NULL)
	{
		strcpy(Stool_list.answer, val->frmstr);
		strcpy(temp, val->frmstr);
		tok = strtok(temp, " ");
		if (tok!=NULL)
		{
			Stoolnum = atoi(tok);
		}
/*
.....Set the form fields
*/
		Stoolnum--;
		S_update_form(Stoolnum);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnText(fieldno, val, stat)
**       Method called at when text field is changed.
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
static UD_FSTAT OnText(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL disp;
	int ifld;
	if (Stoolnum < 0 ) return(UD_FLDOK);
	disp = UU_FALSE;
	switch (*fieldno)
	{
/*
.....Cutter parameters
*/
	case FDIA:
	case FRAD:
	case FHGT:
	case FANG:
	case FZHT:
	case FFLA:
		ifld = *fieldno;
		Scutr[ifld] = *val->frmflt;
		break;
/*
.....Tolerance
*/
	case FTOL:
		Stoler = *val->frmflt;
		break;
/*
.....Maxang
*/
	case FMAX:
		Smaxang = *val->frmflt;
		break;
/*
.....Rapid
*/
	case FRAP:
		Srapid = *val->frmflt;
		break;
/*
.....Edge Display
*/
	case FEDG:
		Sedge0 = *val->frmint;
		if (val->frmint[0] != Sced0)
		{
			Sedge[0] = Sedge[1] = Sedge[2] = val->frmint[0];
			nclu_tool_shade_fields(-1,val->frmint[0],-1,-1);
			Sced0 = val->frmint[0];
		}
		break;
/*
.....Edge Color
*/
	case FEDC:
		Sedgcol0 = *val->frmint;
		if (val->frmint[0] != Scec0)
		{
			Sedgcol[0] = Sedgcol[1] = Sedgcol[2] = val->frmint[0];
			nclu_tool_shade_fields(-1,-1,val->frmint[0],-1);
			Scec0 = val->frmint[0];
		}
		break;
/*
.....Translucency
*/
	case FTRS:
		Strans0 = *val->frmint;
		if (val->frmint[0] != Sctr0)
		{
			Stransp[0] = Stransp[1] = Stransp[2] = val->frmint[0];
			nclu_tool_shade_fields(val->frmint[0],-1,-1,-1);
			Sctr0 = val->frmint[0];
		}
		break;
/*
.....Minimum height
*/
	case FMNH:
		Sminhgt = *val->frmflt;
		break;
/*
.....Maximum height
*/
	case FMXH:
		Smaxhgt = *val->frmflt;
		break;
/*
.....Minimum diameter
*/
	case FMND:
		Smindia = *val->frmflt;
		break;
	}
/*
.....Display the updated tool list
*/
	if (disp)
	{
		S_update_toolnum(Stoolnum,1);
		S_load_list(UU_TRUE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  static OnChoice(fieldno, val, stat)
**       Method called when tool symbol checkbox is changed.
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
static UD_FSTAT OnChoice(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
/*
.....Modify the active fields
*/
	switch (*fieldno)
	{
	case FSY1:
		Ssymfl[0] = val->frmint[0];
		ud_set_traverse_mask(FSY2,Ssymfl[0]);
		ud_set_traverse_mask(FSY3,Ssymfl[0]);
		for (i=FDIA;i<=FFLA;i++) ud_set_traverse_mask(i,!Ssymfl[0]);
		if (!Ssymfl[0]) nclu_tool_close_form(0);
		break;
	case FSH1:
		Ssymfl[1] = val->frmint[0];
		ud_set_traverse_mask(FSH2,Ssymfl[1]);
		ud_set_traverse_mask(FSH3,Ssymfl[1]);
		ud_set_traverse_mask(FCO2,Ssymfl[1]);
		if (!Ssymfl[1]) nclu_tool_close_form(1);
		break;
	case FHD1:
		Ssymfl[2] = val->frmint[0];
		ud_set_traverse_mask(FHD2,Ssymfl[2]);
		ud_set_traverse_mask(FHD3,Ssymfl[2]);
		ud_set_traverse_mask(FCO3,Ssymfl[2]);
		if (!Ssymfl[2]) nclu_tool_close_form(2);
		break;
	}
	S_tool_ctype(Scfl);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  OnShank (fieldno, val, stat)
**       Method called when Symbol, Shank, or Holder button is pressed.
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
static UD_FSTAT OnShank(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int ifl,inc,fld[3],ityp;
	ifl = 0;
	ityp = 2;
	if (*fieldno == FSH2)
	{
		ifl = 1;
		ityp = 3;
	}
	else if (*fieldno == FHD2) ifl = 2;
/*
.....Display the symbol form
*/
	inc = ifl * 4;
	fld[0] = FSY3; fld[1] = FSH3; fld[2] = FHD3;
	nclu_tool_form(ifl,Ssymbol[ifl],Spv[ifl],&Stransp[ifl],&Sedge[ifl],
		&Sedgcol[ifl],&Sclash,Satt[inc],&Stype,&Scfl[ifl],&Spseudo,fld[ifl],ityp, UU_NULL, -1);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnApply(fieldno, val, stat)
**       Applies the minimum and maximum tool dimensions to the entire
**       tool list.
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
static UD_FSTAT OnApply(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	UU_REAL mindia,minhgt,maxhgt;
/*
.....Set heights and diameters on all tools
*/
	UM_len_exttoint(Smindia,mindia);
	UM_len_exttoint(Sminhgt,minhgt);
	UM_len_exttoint(Smaxhgt,maxhgt);
	for (i=0;i<Sntools;i++)
	{
		if (mindia > 0. && Scpt[i].cutter[0] < mindia)
			Scpt[i].cutter[0] = mindia;
		if (Scpt[i].type == NCL_CUTTER_MILL)
		{
			if (minhgt > 0. && Scpt[i].cutter[2] < minhgt)
				Scpt[i].cutter[2] = minhgt;
			if (maxhgt > 0. && Scpt[i].cutter[2] > maxhgt)
				Scpt[i].cutter[2] = maxhgt;
		}
	}
/*
.....Display the updated tool list
*/
	S_load_list(UU_TRUE);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnDelete(fieldno, val, stat)
**       Method called at when "Delete" button is pushed
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
static UD_FSTAT OnDelete(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	if (Stoolnum <0) return(UD_FLDOK);
/*
.....Delete the current tool
*/
	if (Sndel == MAXDEL)
	{
		for (i=0;i<MAXDEL-1;i++) Slast_del[i] = Slast_del[i+1];
		Sndel--;
	}
	Slast_del[Sndel] = Stoolnum;
	Sndel++;
	Scpt[Stoolnum].used = UU_FALSE;
	ud_set_traverse_mask(FUND,UU_TRUE);
/*
.....Display the updated tool list
*/
	S_load_list(UU_TRUE);
/*
.....No tools left
.....Disable form (except Reset)
*/
	if (Stoolnum < 0)
	{
		for (i=0;i<FUND;i++) ud_set_traverse_mask(i,UU_FALSE);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnUndelete(fieldno, val, stat)
**       Method called at when "Undelete" button is pushed
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
static UD_FSTAT OnUndelete(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int it,i;
/*
.....Restore the last deleted tool
*/
	it = Stoolnum;
	if (Sndel > 0)
	{
		Sndel--;
		Scpt[Slast_del[Sndel]].used = UU_TRUE;
		Stoolnum = Slast_del[Sndel];
	}
	if (Sndel == 0) ud_set_traverse_mask(FUND,UU_FALSE);
/*
.....Display the updated tool list
*/
	S_load_list(UU_TRUE);
/*
.....Store default answers
*/
	if (it < 0)
	{
		for (i=0;i<FDEL;i++) ud_set_traverse_mask(i,UU_TRUE);
	}
	S_update_form(0);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnReset(fieldno, val, stat)
**       Method called at when "Reset" button is pushed
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
static UD_FSTAT OnReset(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,it;
/*
.....Restore all deleted tools
*/
	it = Stoolnum;
	for (i=0;i<Sntools;i++) Scpt[i].used = UU_TRUE;
	Sndel = 0;
/*
.....Set field traversals
*/
	if (it < 0)
	{
		for (i=0;i<FUND;i++) ud_set_traverse_mask(i,UU_TRUE);
	}
	ud_set_traverse_mask(FUND,UU_FALSE);
/*
.....Display the updated tool list
*/
/*	Stoolnum = 0;*/
	S_load_list(UU_TRUE);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnRescan(fieldno, val, stat)
**       Method called at when "Reset" button is pushed
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
static UD_FSTAT OnRescan(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_REAL rsav[3];
/*
.....Rescan clfile for tools
*/
	LW_ntool = 0;
	rsav[0] = LW_tool_limit[0];
	rsav[1] = LW_tool_limit[1];
	rsav[2] = LW_tool_limit[2];
	UM_len_exttoint(Sminhgt,LW_tool_limit[0]);
	UM_len_exttoint(Smaxhgt,LW_tool_limit[1]);
	UM_len_exttoint(Smindia,LW_tool_limit[2]);
	S_load_tools(UU_TRUE,&Smylist,&myntl);
	LW_tool_limit[0] = rsav[0];
	LW_tool_limit[1] = rsav[1];
	LW_tool_limit[2] = rsav[2];
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  S_update_form(tnum)
**       Updates the form fields depending on the selected tool.
**    PARAMETERS
**       INPUT  :
**          tnum     Currently selected tool.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_update_form(tnum)
int tnum;
{
	int i,t;
	int fieldno;
	UD_DDATA val;
	UD_FSTAT stat;
/*
.....Find next used tool
.....Start search with specified tool number
*/
	if (tnum < 0) return;
	for (t=tnum;t<Sntools;t++)
	{
		if (Scpt[t].used) break;
	}
/*
.....Update answers
*/
	if (t<Sntools)
	{
		S_update_toolnum(t,0);
/*
.....Update form fields
*/
		for (i=0;i<6;i++)
		{
			ud_update_answer(i, (int*)&Scutr[i]);
		}
		ud_update_answer(FCUT, (int*)&Scut_color);
		ud_update_answer(FTOL, (int*)&Stoler);
		ud_update_answer(FMAX, (int*)&Smaxang);
		ud_update_answer(FRAP, (int*)&Srapid);

		ud_update_answer(FEDG, (int*)&Sedge0);
		ud_update_answer(FEDC, (int*)&Sedgcol0);
		ud_update_answer(FTRS, (int*)&Strans0);
		ud_update_answer(FSY1, (int*)&Ssymfl[0]);
		ud_update_answer(FSY3, (int*)&Ssymbol[0]);
		ud_update_answer(FCO1, (int*)&Scolor[0]);
		ud_update_answer(FSH1, (int*)&Ssymfl[1]);
		ud_update_answer(FSH3, (int*)&Ssymbol[1]);
		ud_update_answer(FCO2, (int*)&Scolor[1]);
		ud_update_answer(FHD1, (int*)&Ssymfl[2]);
		ud_update_answer(FHD3, (int*)&Ssymbol[2]);
		ud_update_answer(FCO3, (int*)&Scolor[2]);

		for (i=FDIA;i<=FFLA;i++) ud_set_traverse_mask(i,!Ssymfl[0]);
		ud_set_traverse_mask(FSY2,Ssymfl[0]);
		ud_set_traverse_mask(FSY3,Ssymfl[0]);
/*		ud_set_traverse_mask(FCO1,Ssymfl[0]);*/
		ud_set_traverse_mask(FSH2,Ssymfl[1]);
		ud_set_traverse_mask(FSH3,Ssymfl[1]);
		ud_set_traverse_mask(FCO2,Ssymfl[1]);
		ud_set_traverse_mask(FHD2,Ssymfl[2]);
		ud_set_traverse_mask(FHD3,Ssymfl[2]);
		ud_set_traverse_mask(FCO3,Ssymfl[2]);
	}
/*
.....Initialize cutter prompts
*/
	stat = UD_FLDOK;
	OnInitForm(&fieldno,&val,stat);
/*
.....Update holder field traversals
*/
	nclu_tool_shank_fields(UU_TRUE);
}

/*********************************************************************
**    I_FUNCTION     :  S_update_toolnum(tnum,idir)
**       Updates the form storage based on the selected tool number.
**    PARAMETERS
**       INPUT  :
**          tnum   = Currently selected tool.
**				idir   = 0 - Store tool in form, 1 - Store form fields in tool.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_update_toolnum(tnum,idir)
int tnum,idir;
{
	int i,j,t,inc;
	UU_REAL rnum;
	char *p,*index();
/*
.....No tool active
*/
	if (tnum < 0) return;
/*
.....Find next used tool
.....Start search with specified tool number
*/
	if (idir == 0)
	{
		for (t=tnum;t<Sntools;t++)
		{
			if (Scpt[t].used) break;
		}
/*
.....Update form fields
*/
		if (t<Sntools)
		{
			for (i=0;i<6;i++)
			{
				Scutr[i] = Scpt[t].cutter[i];
				if (i <= 2 || (Scpt[t].type == NCL_CUTTER_MILL && i == 4) ||
					(Scpt[t].type == NCL_CUTTER_MILL && Scpt[t].ncparm > 4 &&
					i == 3) ||
					(Scpt[t].type == NCL_CUTTER_LATHE && i == 4 && Scutr[3] == 0))
						UM_len_inttoext(Scutr[i],Scutr[i]);
			}
			Scolor[0] = Scpt[t].color[0];
			Scut_color = Scpt[t].cut_color;
			UM_len_inttoext(Scpt[t].toler,Stoler);
			Smaxang = Scpt[t].maxang;
			UM_len_inttoext(Scpt[t].rapid,Srapid);
			Sclash = Scpt[t].shank_clash;
/*
........Display paramters
*/
			for (j=0;j<3;j++)
			{
				Scfl[j] = Scpt[t].ctype[j];
				if (Scpt[t].ctype[j] <= 0 || (j == 0 && Scpt[t].ctype[j] <= 1))
					Ssymfl[j] = UU_FALSE;
				else Ssymfl[j] = UU_TRUE;
				Scolor[j] = Scpt[t].color[j];
				Sedge[j] = Scpt[t].edge[j];
				Sedgcol[j] = Scpt[t].edge_color[j];
				Stransp[j] = Scpt[t].trans[j];
				strcpy(Ssymbol[j],Scpt[t].symbol[j]);
/*
...........Break out Point Vector from any symbol label
*/
				p = index(Ssymbol[j],'&');
				if (p != UU_NULL)
				{
					strcpy(Spv[j],p+1);
					*p = '\0';
				}
				else
					Spv[j][0] = '\0';
/*
...........Format attach points
*/
				inc = j * 4;
				if (Scpt[t].ctype[j] == 0)
				{
					strcpy(Satt[inc++],"0");
					strcpy(Satt[inc++],"0");
					strcpy(Satt[inc++],"0");
					strcpy(Satt[inc++],"0");
				}
				else
				{
					UM_len_inttoext(Scpt[t].parms[j][0],rnum);
					sprintf(Satt[inc++],"%g",rnum);
					UM_len_inttoext(Scpt[t].parms[j][1],rnum);
					sprintf(Satt[inc++],"%g",rnum);
					if (j == 0)
					{
						strcpy(Satt[inc++],"0");
						strcpy(Satt[inc++],"0");
					}
					else
					{
						if (Scpt[t].ctype[j] == 1) rnum = Scpt[t].parms[j][2];
						else
						{
							UM_len_inttoext(Scpt[t].parms[j][2],rnum);
						}
						sprintf(Satt[inc++],"%g",rnum);
						UM_len_inttoext(Scpt[t].parms[j][3],rnum);
						sprintf(Satt[inc++],"%g",rnum);
					}
				}
			}
			Sctr0 = Strans0 = Stransp[0];
			Sced0 = Sedge0 = Sedge[0];
			Scec0 = Sedgcol0 = Sedgcol[0];
		}
	}
/*
.....Store form answers into current tool
*/
	else
	{
		S_tool_ctype(Scfl);
		Scpt[tnum].ncparm = 0.;
		for (i=0;i<6;i++)
		{
			Scpt[tnum].cutter[i] = Scutr[i];
			if (Scutr[i] != 0. && i+1 > Scpt[tnum].ncparm) Scpt[tnum].ncparm = i+1;
			if (Scpt[tnum].type == NCL_CUTTER_MILL)
			{
				if (i <= 2 || i == 4 || Scpt[tnum].ncparm > 4 && i == 3)
					UM_len_exttoint(Scpt[tnum].cutter[i],Scpt[tnum].cutter[i]);
			}
			else if (Scpt[tnum].type == NCL_CUTTER_LATHE)
			{
				if (i <= 2 || (i == 4 && Scpt[tnum].cutter[3] == 0.))
					UM_len_exttoint(Scpt[tnum].cutter[i],Scpt[tnum].cutter[i]);
			}
		}
		Scpt[tnum].cut_color = Scut_color;
		UM_len_exttoint(Stoler,Scpt[tnum].toler);
		Scpt[tnum].maxang = Smaxang;
		UM_len_exttoint(Srapid,Scpt[tnum].rapid);
		Scpt[tnum].shank_clash = Sclash;
/*
........Display paramters
*/
		for (j=0;j<3;j++)
		{
			if (Ssymfl[j] == 0)
			{
				if (j == 0) Scpt[tnum].ctype[j] = 1;
				else Scpt[tnum].ctype[j] = 0;
			}
			else
				Scpt[tnum].ctype[j] = Scfl[j];
			Scpt[tnum].color[j] = Scolor[j];
			Scpt[tnum].edge[j] = Sedge[j];
			Scpt[tnum].edge_color[j] = Sedgcol[j];
			Scpt[tnum].trans[j] = Stransp[j];
			strcpy(Scpt[tnum].symbol[j],Ssymbol[j]);
/*
...........Store Point Vector with symbol label
*/
			if (strlen(Ssymbol[j]) != 0 && strlen(Spv[j]) != 0)
			{
				strcat(Scpt[tnum].symbol[j],"&");
				strcat(Scpt[tnum].symbol[j],Spv[j]);
			}
/*
...........Format attach points
*/
			inc = j * 4;
			for (i=0;i<4;i++)
			{
				if (strlen(Satt[inc+i]) == 0)
					Scpt[tnum].parms[j][i] = 0.;
				else
				{
					if (ncl_get_scalar_value(Satt[inc+i],&rnum) == UU_SUCCESS)
					{
						if (Scpt[tnum].ctype[j] == 1 && i == 2)
							Scpt[tnum].parms[j][2] = rnum;
						else
						{
							UM_len_exttoint(rnum,Scpt[tnum].parms[j][i]);
						}
					}
				}
			}
		}
	}
}

/*********************************************************************
**    S_FUNCTION     : S_load_tools(chg,clist,ntl)
**       Loads the tools used in the active clfile and stores them
**			into the form list.
**    PARAMETERS
**       INPUT  :   chg   = UU_TRUE if changing existing list.
**       OUTPUT :   clist = List array to receive tool list.
**                  ntl   = Number of tools in 'clist'.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_load_tools(chg,clist,ntl)
UU_LIST *clist;
UU_LOGICAL chg;
int *ntl;
{
/*
.....Load the tool list
*/
	ul_ipv_load_tools(clist,ntl);
/*
.....Set the local variables
*/
	Sntools = *ntl;
	Scpt = (UN_cutter_list *) UU_LIST_ARRAY(clist);
	if (Sntools == 0)
	{
		Stoolnum = -1;
		goto done;
	}
/*
.....Merge tools with active list
*/
	else
	{
		if (!UN_playback_active) ul_ipv_merge_tools(clist,Sntools);
		Stoolnum = 0;
/*
.....Store tools in form list
*/
		S_load_list(chg);
	}
done:;
}

/*********************************************************************
**    E_FUNCTION     : S_load_list(chg)
**       Loads the tools used in the active clfile and stores them
**			into the form list.
**    PARAMETERS
**       INPUT  :   chg   = UU_TRUE if changing existing list.
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_load_list(chg)
UU_LOGICAL chg;
{
	int i,nt;
	char buf[80];
/*
.....Initialize list
*/
	ud_free_flist(&Stool_list);
	nt = Sntools;
	if (nt == 0) nt = 1;
	Stool_list.item = (char **)uu_malloc(nt*sizeof(char*));
	Stool_list.num_item = 0;
	Stool_list.answer = (char *)uu_malloc(sizeof(char)*80);
/*
.....No tools in list
*/
	if (nt == 0 || Stoolnum == -1)
	{
		Stool_list.item[Stool_list.num_item] = (char*)uu_malloc(81*sizeof(char));
		strcpy(Stool_list.item[0], " ");
		Stool_list.num_item++;
	}
/*
.....Store tools in form list
*/
	else
	{
		for (i=0;i<Sntools;i++)
		{
/*
........Format tool for list
*/
			if (Scpt[i].used)
			{
				S_fmt_tool(i,buf);
/*
........Store tool in list
*/
				Stool_list.item[Stool_list.num_item] =
					(char*)uu_malloc(81*sizeof(char));
				strcpy(Stool_list.item[Stool_list.num_item], buf);
				Stool_list.num_item++;
			}
		}
/*
.....Determine default tool
*/
		S_default_tool(Stoolnum,chg);
	}
}

/*********************************************************************
**    E_FUNCTION     : S_default_tool(tool,chg)
**       Finds the next available tool (not deleted) in the list to use
**			as the default tool.
**    PARAMETERS
**       INPUT  :   tool  = Tool number to start looking from.
**                  chg   = UU_TRUE if changing existing list.
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : Sets the default tool in the form list.
**    WARNINGS     : none
*********************************************************************/
static void S_default_tool(tool,chg)
int tool;
UU_LOGICAL chg;
{
	int i,j,t;
	int found;
/*
.....Initialize routine
*/
	found = -1;
	t = tool;
	if (t < 0) t = 0;
/*
.....Find next available tool
*/
	for (i=t;i<Sntools;i++)
	{
		if (Scpt[i].used)
		{
			S_fmt_tool(i,Stool_list.answer);
			found = i;
			break;
		}
	}
/*
.....Start searching to beginning
.....if not found
*/
	if (found < 0)
	{
		for (j=t-1;j>=0;j--)
		{
			if (Scpt[j].used)
			{
				S_fmt_tool(j,Stool_list.answer);
				found = j;
				break;
			}
		}
	}
/*
.....No active tools
*/
	if (found < 0)
	{
		Stoolnum = -1;
		Stool_list.answer[0] = '\0';
	}
/*
.....Set form field defaults
*/
	else
	{
		Stoolnum = found;
/*
.....Update data fields
*/
		if (chg)
		{
			S_update_form(Stoolnum);
			ud_update_answer(FLST,(int *)&Stool_list);
		}
		else
			S_update_toolnum(Stoolnum,0);
	}
}

/*********************************************************************
**    E_FUNCTION     : S_fmt_tool(tool,buf)
**       Formats a tool for inclusion in the tool list.
**    PARAMETERS
**       INPUT  :   tool  = Tool number to format.
**       OUTPUT :   buf   = Formatted string for list.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_fmt_tool(tool,buf)
int tool;
char *buf;
{
	int j;
	char lnum[30];
	UU_REAL rnum;
/*
.....Format tool data
*/
	sprintf(buf,"%04d   %6d   %6d   CUTTER/",tool+1,Scpt[tool].isn,
		Scpt[tool].clrec);
	if (Scpt[tool].type == NCL_CUTTER_LATHE) strcat(buf,"LATHE,");
	else if (Scpt[tool].type == NCL_CUTTER_BLADE) strcat(buf,"BLADE,");
	if (Scpt[tool].ctype[0] <= 1)
	{
		UM_len_inttoext(Scpt[tool].cutter[0],rnum);
		sprintf(lnum,"%g",rnum);
		strcat(buf,lnum);
		for (j=1;j<Scpt[tool].ncparm;j++)
		{
			if (j <= 2 || (Scpt[tool].type == NCL_CUTTER_MILL && j == 4) ||
				(Scpt[tool].type == NCL_CUTTER_MILL && Scpt[tool].ncparm > 4 &&
				j == 3) ||
				(Scpt[tool].type == NCL_CUTTER_LATHE && j == 4 &&
				Scpt[tool].cutter[3] == 0))
			{
				UM_len_inttoext(Scpt[tool].cutter[j],rnum);
			}
/*
			else if (Scpt[tool].type == NCL_CUTTER_BLADE && j == 3)
				rnum = asin(Scpt[tool].cutter[j]) * (180./UM_PI);
*/
			else
				rnum = Scpt[tool].cutter[j];
			sprintf(lnum,", %g",rnum);
			if (strlen(buf)+strlen(lnum) >= 80) goto done;
			strcat(buf,lnum);
		}
	}
	else
		strcat(buf,Scpt[tool].symbol[0]);
done:;
}

/*********************************************************************
**    I_FUNCTION     : S_tool_ctype(cfl)
**       Determines the actual cutter type if tool is a symbol.
**    PARAMETERS
**       INPUT  :
**           cfl   = Current cutter types.  Cutter, Shank, Holder.
**       OUTPUT :
**           cfl   = Adjusted cutter types.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_tool_ctype(cfl)
int cfl[];
{
	int i,tmp[10],ictype;
	UN_motseg_symgeo symgeo;
/*
.....Get the tool component types
*/
	tmp[0] = cfl[0]; tmp[4] = cfl[1]; tmp[5] = cfl[2];
	nclu_tool_ctype(Ssymfl,Ssymbol,0,tmp);
	cfl[0] = tmp[0]; cfl[1] = tmp[4]; cfl[2] = tmp[5];
/*
.....If any of these are symbols
.....Then determine if it is a
.....surface of revolution curve
*/
	for (i=0;i<3;i++)
	{
		if (cfl[i] == 2)
		{
			strcpy(symgeo.symbol,Ssymbol[i]);
			symgeo.shaded = 0;
			ictype = Scpt[Stoolnum].type == NCL_CUTTER_LATHE ? 1 : 2;
			if (ncl_get_cutsym(&symgeo,0,ictype) == UU_SUCCESS)
			{
				cfl[i] = symgeo.type;
			}
		}
	}
}

/*********************************************************************
**	 E_FUNCTION : ul_ipv_tool_modals()
**			This function handles the NCLIPV tool modals form.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS:
*********************************************************************/
void ul_ipv_tool_modals()
{
	int status;
	int transp,edge,edgcol;
	UU_REAL tol,maxang,rapid,minhgt,maxhgt,mindia;
	UU_LOGICAL tfrom;
	int tshank;
	int *ans[12];	/* default answers/answers for form */
	UU_LOGICAL cmdreject;
/*
.....Load the input values into
.....local storage area
*/
	UM_len_inttoext(LW_default_tool.toler,tol);
	maxang = LW_default_tool.maxang;
	UM_len_inttoext(LW_default_tool.rapid,rapid);
	transp = LW_default_tool.translucency;
	edge = LW_default_tool.edge;
	edgcol = LW_default_tool.edge_color;
	UM_len_inttoext(LW_tool_limit[0],minhgt);
	UM_len_inttoext(LW_tool_limit[1],maxhgt);
	UM_len_inttoext(LW_tool_limit[2],mindia);
	tfrom = LW_tool_from;
	tshank = LW_default_tool.shank_clash;
/*
.....Field 0 Tolerance
*/
	ans[0] = (int *)&tol;
/*
.....Field 1 is Maxang
*/
	ans[1] = (int *)&maxang;
/*
.....Field 2 is Rapid Rate
*/
	ans[2] = (int *)&rapid;
/*
.....Field 3 is Translucency
*/
	ans[3] = (int *)&transp;
/*
.....Field 4 is Edge Display
*/
	ans[4] = (int *)&edge;
/*
.....Field 5 is Edge Color
*/
	ans[5] = (int *)&edgcol;
/*
.....Field 6 is Min Height
*/
	ans[6] = (int *)&minhgt;
/*
.....Field 7 is Max Height
*/
	ans[7] = (int *)&maxhgt;
/*
.....Field 8 is Min Diameter
*/
	ans[8] = (int *)&mindia;
/*
.....Field 9 is From flag
*/
	ans[9] = (int *)&tfrom;
/*
.....Field 10 is From flag
*/
	ans[10] = (int *)&tshank;
/*
.....Get the Form input
*/
	UD_MARK(cmdreject, UU_FALSE);
	if (!cmdreject)
	{
		status = ud_form("ipvtlmod.frm", ans, ans);
		if (status==-1)
			goto done;
	}	
	else
		goto done;
/*
.....Store IPV Modals
*/
	
	UM_len_exttoint(tol,LW_default_tool.toler);
	LW_default_tool.maxang = maxang;
	UM_len_exttoint(rapid,LW_default_tool.rapid);
	LW_default_tool.translucency = transp;
	LW_default_tool.edge = edge;
	LW_default_tool.edge_color = edgcol;
	UM_len_exttoint(minhgt,LW_tool_limit[0]);
	UM_len_exttoint(maxhgt,LW_tool_limit[3]);
	UM_len_exttoint(mindia,LW_tool_limit[2]);
	LW_tool_from = tfrom;
	LW_default_tool.shank_clash = tshank;
done:
	UD_UNMARK(cmdreject);
	return;
}

/*********************************************************************
**    E_FUNCTION     :  ulf_ipv_set_rapid(rapid)
**       Defines the default rapid rate to use for subsequent tools.
**    PARAMETERS
**       INPUT  :
**          rapid   = New default rapid rate.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ulf_ipv_set_rapid(rapid)
UU_REAL *rapid;
{
	UM_len_exttoint(*rapid,LW_default_tool.rapid);
}
