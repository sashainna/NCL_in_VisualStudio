/*********************************************************************
**    NAME         :  nucutr1.c
**       CONTAINS:
**				nclu_tool_form()
**				nclu_tool_profs()
**				nclu_tool_syms()
**				nclu_tool_ctype()
**				nclu_tool_is_curve()
**				nclu_tool_shank_fields()
**				nclu_tool_shade_fields()
**				nclu_tool_close_form()
**				nclu_tool_free()
**
**          Tool Display form routines.
**    COPYRIGHT 2006 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nucutr1.c , 25.4
**    DATE AND TIME OF LAST  MODIFICATION
**       01/20/17 , 10:58:58
*********************************************************************/

#include "stdio.h"
#include "usysdef.h"
#include "lipv.h"
#include "class.h"
#include "dselmask.h"
#include "lcom.h"
#include "mdrel.h"
#include "mfort.h"
#include "mxxx.h"
#include "mdpick.h"
#include "nccs.h"
#include "nclfc.h"
#include "nclmplay.h"
#include "nclfile.h"
#include "nclvx.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "uhep.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "driver.h"

void nclu_tool_shank_fields();
void nclu_tool_load_profs();
void nclu_tool_load_syms();
void nclu_tool_free();
static UD_FSTAT OnDisSelect(), OnDisSelect0(), OnDisSelect1(), OnDisSelect2();
static UD_FSTAT OnDisText(),OnDisText0(),OnDisText1(),OnDisText2();
static UD_FSTAT OnDisTog(),OnDisTog0(),OnDisTog1(),OnDisTog2();
static UD_FSTAT OnDisLoad(),OnDisLoad0(),OnDisLoad1(),OnDisLoad2();
static UD_FSTAT OnDisList(),OnDisList0(),OnDisList1(),OnDisList2();
static UD_FSTAT OnDisClose(),OnDisClose0(),OnDisClose1(),OnDisClose2();
static void S_filter_profs();
static void S_axis_tog();
static void S_form_invis();
static void S_form_vis();

#define FDSM1 0
#define FDSM2 1
#define FDAX1 2
#define FDAX2 3
#define FDSHD 4
#define FDCSH 5
#define FDAT1 6
#define FDAT2 7
#define FDAT3 8
#define FDAT4 9
#define FDEDG 10
#define FDEDC 11
#define FDTRA 12
#define FDCS1 13
#define FDCLA 14
#define FDLOD 15
#define FDLS1 16
#define FDLS2 17
#define FDSLIB 18

#define LABOFS 3

typedef struct
{
	char label[21];
	char class[21];
} Sprof_struc;

static int *Sshade[3],*Sclash[3],Stype,*Scfl[3],*Spseudo,Sfld[3],Sfrmtyp,Slibfld;
static int *Sedgcol[3];
static UU_LOGICAL *Sedge[3];
static char *Ssymbol[3],*Satt[3][4],*Spv[3];

static int Snclass=0,Snprofs=0;
static UD_LIST Sprof_list[3],Ssym_list,Sclass_list[3];
static Sprof_struc *Sclist = UU_NULL;

static int Sfrmdis[3] = {-1,-1,-1};
static UU_LOGICAL Supdate = UU_FALSE;
static char Slast_class[3][21] = {"All","All","All"};
static int Sfrmsym = -1;
static char Slast_class_sym[21] = "All";
static UD_LIST Sprof_list_sym, Ssym_list_sym, Sclass_list_sym;
static Sprof_struc *Sclist_sym = UU_NULL;
static int Snclass_sym=0,Snprofs_sym=0;
static char Ssymsel[41] = "";
static int Sfrm_cancel = -1;
/*********************************************************************
**    E_FUNCTION     : nclu_tool_form(which,symbol,pv,shade,clash,attstr,
**		                                type,icfl,pseudo,fsym,frmtyp)
**       Processes the Tool Display form used to define the Cutter,
**			Shank, and Holder symbols.
**    PARAMETERS
**       INPUT  :
**          which  = 0 = Process Cutter symbol, 1 = Shank, 2 = Holder.
**          symbol = Tool symbol.
**          pv     = Point-vector axis for curve symbol.
**          shade  = Shaded flag for NCL or Translucency setting for IPV.
**          edge   = Display edges flag for IPV.
**          edgcol = Edge color setting for IPV.
**          clash  = Shank clash flag for IPV.
**          attstr = Attach point formatted text strings.
**          type   = Cutter type , >=10 = Lathe cutter.
**          icfl   = Symbol type , 1 = Parameters, 2+ = Symbol/Geo/Profile.
**          pseudo = 1 = Pseudo cutter is being defined.
**          fsym   = Main form field for symbol name.
**          frmtyp = 0 = NCL cutter/holder form, 1 = NCL shank form,
**                   2 = NCLIPV tool form, 3 = NCLIPV shank form.
**       OUTPUT :
**          symbol = Tool symbol.
**          pv     = Point-vector axis for curve symbol.
**          shade  = Shaded flag.
**          clash  = Shank clash flag.
**          attstr = Attach point formatted text strings.
**          icfl   = Symbol type , 1 = Parameters, 2+ = Symbol/Geo/Profile.
**    RETURNS      : none
**    SIDE EFFECTS :
**			The profile and symbol lists must be initialize prior to
**			call this routine by calling the routines 'nclu_tool_profs'
**			and 'nclu_tool_syms'.
**    WARNINGS     : none
*********************************************************************/
void nclu_tool_form(which,symbol,pv,shade,edge,edgcol,clash,attstr,type,icfl,
	pseudo,fsym,frmtyp, symlib, libfld)
int which;
char *symbol,*pv, *symlib;
UU_LOGICAL *edge;
int *shade,*clash,*edgcol;
char attstr[4][NCL_MAX_LABEL+1];
int *type,*icfl,*pseudo,fsym,frmtyp, libfld;
{
	int i,inc,idum;
	char label[20];
/*
.....Set up form fields
*/
	UD_METHOD methods[FDLS2+2];
	static UD_METHOD msel[3] = {OnDisSelect0,OnDisSelect1,OnDisSelect2};
	static UD_METHOD mtxt[3] = {OnDisText0,OnDisText1,OnDisText2};
	static UD_METHOD mtog[3] = {OnDisTog0,OnDisTog1,OnDisTog2};
	static UD_METHOD mlod[3] = {OnDisLoad0,OnDisLoad1,OnDisLoad2};
	static UD_METHOD mlst[3] = {OnDisList0,OnDisList1,OnDisList2};
	static UD_METHOD mcls[3] = {OnDisClose0,OnDisClose1,OnDisClose2};

	static char traverse[] = {
		1,1,0,0,1,0, 1,1,0,0, 0,0,0,0, 1,1,1,1};
/*
.....need one more value for 'close' callback function
*/
	static char called[] = {
		6,6,6,6,6,6, 6,6,6,6, 6,6,6,6, 6,6,6,6, 6};

	static char display[] = {1,1,1,
		1,1,1,1,1,0, 1,1,1,1, 1,1,1,0, 1,1,1,1};

	int *ans[FDLS2+1];
/*
.....Form is already open
*/
	if (Sfrmdis[which] != -1) return;
/*
.....Store pointers to provided data
*/
	Ssymbol[which] = symbol;
	Spv[which] = pv;
	Sclash[which] = clash;
	Sedge[which] = edge;
	Sedgcol[which] = edgcol;
	Sshade[which] = shade;
	for (i=0;i<4;i++) Satt[which][i] = attstr[i];
	Stype = *type;
	Scfl[which] = icfl;
	Spseudo = pseudo;
	Sfld[which] = fsym;
	Slibfld = libfld;
	Sfrmtyp = frmtyp;
/*
.....Fill the Profile List
*/
	S_filter_profs(Slast_class[which],which,UU_FALSE);
/*
.....Set up the default form fields
*/
	ans[FDSM1] = &idum;
	ans[FDSM2] = (int *)Ssymbol[which];
	ans[FDAX1] = &idum;
	ans[FDAX2] = (int *)Spv[which];
	ans[FDCSH] = Sclash[which];
	ans[FDEDG] = Sedge[which];
	ans[FDEDC] = Sedgcol[which];
	if (Sfrmtyp == 0 || Sfrmtyp == 1)
	{
		ans[FDSHD] = Sshade[which];
		ans[FDTRA] = &idum;
	}
	else
	{
		ans[FDSHD] = &idum;
		ans[FDTRA] = Sshade[which];
	}
	ans[FDCS1] = Sclash[which];
	inc = which * 4;
	for (i=0;i<4;i++) ans[FDAT1+i] = (int *)Satt[which][i];
	ans[FDCLA] = (int *)&Sclass_list[which];
	ans[FDLOD] = &idum;
	strcpy(Sclass_list[which].answer,Slast_class[which]);
	ans[FDLS1] = (int *)&Sprof_list[which];
	strcpy(Sprof_list[which].answer,Ssymbol[which]);
	ans[FDLS2] = (int *)&Ssym_list;
	strcpy(Ssym_list.answer, Ssymbol[which]);
/*
.....Set up the traverse fields
*/
	if (Stype < 10 && nclu_tool_is_curve(Ssymbol[which]))
		traverse[FDAX1] = traverse[FDAX2] = UU_TRUE;
	else
		traverse[FDAX1] = traverse[FDAX2] = UU_FALSE;
/*
.....Set up display fields
*/
	if (Sfrmtyp == 0 || Sfrmtyp == 1)
	{
		display[LABOFS+FDSHD] = traverse[FDSHD] = UU_TRUE;
		display[LABOFS+FDCSH] = traverse[FDCSH] = UU_FALSE;
		display[LABOFS+FDEDG] = traverse[FDEDG] = UU_FALSE;
		display[LABOFS+FDEDC] = traverse[FDEDC] = UU_FALSE;
		display[LABOFS+FDTRA] = traverse[FDTRA] = UU_FALSE;
		if (Sfrmtyp == 0)
			display[LABOFS+FDCS1] = traverse[FDCS1] = UU_FALSE;
		else
			display[LABOFS+FDCS1] = traverse[FDCS1] = UU_TRUE;
	}
	else
	{
		display[LABOFS+FDSHD] = traverse[FDSHD] = UU_FALSE;
		display[LABOFS+FDEDG] = traverse[FDEDG] = UU_TRUE;
		display[LABOFS+FDEDC] = traverse[FDEDC] = UU_TRUE;
		display[LABOFS+FDTRA] = traverse[FDTRA] = UU_TRUE;
		display[LABOFS+FDCS1] = traverse[FDCS1] = UU_FALSE;
		if (Sfrmtyp == 3)
			display[LABOFS+FDCSH] = traverse[FDCSH] = UU_TRUE;
		else
			display[LABOFS+FDCSH] = traverse[FDCSH] = UU_FALSE;
	}
/*
.....Define the Form callbacks
*/
	methods[FDSM1] = msel[which];
	methods[FDSM2] = mtxt[which];
	methods[FDAX1] = msel[which];
	methods[FDAX2] = mtxt[which];
	methods[FDSHD] = UU_NULL;
	methods[FDCSH] = UU_NULL;
	methods[FDAT1] = mtxt[which];
	methods[FDAT2] = mtxt[which];
	methods[FDAT3] = mtxt[which];
	methods[FDAT4] = mtxt[which];
	methods[FDEDG] = UU_NULL;
	methods[FDEDC] = UU_NULL;
	methods[FDTRA] = UU_NULL;
	methods[FDCS1] = UU_NULL;
	methods[FDCLA] = mtog[which];
	methods[FDLOD] = mlod[which];
	methods[FDLS1] = mlst[which];
	methods[FDLS2] = mlst[which];
	methods[FDLS2+1] = mcls[which];
/*
.....Don't display Symbol fields
.....if Standalone NCLIPV
*/
	if (LW_nclipv == LW_STANDALONE)
	{
		display[LABOFS+FDLOD] = traverse[FDLOD] = UU_FALSE;
		display[LABOFS+FDLS2] = traverse[FDLS2] = UU_FALSE;
		display[LABOFS-1] = UU_FALSE;
	}
/*
.....Get the Form input
*/
	Sfrmdis[which] = ud_form_display1("cutdis.frm", ans, ans, methods, called,
		display, traverse);
/*
.....Update the header text
*/
	if (which == 0) strcpy(label,"Cutter Display");
	else if (which == 1) strcpy(label,"Shank Display");
	else strcpy(label,"Holder Display");
	ud_dispfrm_update_label(Sfrmdis[which],0,label);
/*
.....Update the parameter fields
*/
	nclu_tool_shank_fields(UU_FALSE);
	return;
}

/*********************************************************************
**    I_FUNCTION     :  static OnDisSelect(fieldno, val, stat)
**       Method called when Symbol or Axis-PV buttons are pressed from
**			the Tool Display form.
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
static UD_FSTAT OnDisSelect0(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(OnDisSelect(*fieldno,val,0));
}

static UD_FSTAT OnDisSelect1(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(OnDisSelect(*fieldno,val,1));
}

static UD_FSTAT OnDisSelect2(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(OnDisSelect(*fieldno,val,2));
}

static UD_FSTAT OnDisSelect(fieldno, val, which)
int fieldno;
UD_DDATA *val;
int which;
{
	int numint,cmdreject,pr;
	UM_PLOCREC pick;
	struct NCL_fixed_databag e;
	char label[NCL_MAX_LABEL_AND_SUBSCRIPT];
/*
......for NCLIPV application, ignore picking function now
*/
	if (LW_nclipv==LW_STANDALONE) return(UD_FLDOK);
/*
.....Select a curve or surface
*/
	if (fieldno == FDSM1)
	{
		pr = 670;
		ud_lgeo(UU_TRUE,UD_ncl_holder);
	}
/*
.....Select a point vector
*/
	else
	{
		pr = 671;
		ud_lgeo(UU_TRUE,UD_ncl_pv);
	}
/*
.....Take down forms
*/
	S_form_invis();
/*
.....Trap Reject Op
*/
	UD_MARK(cmdreject,UU_TRUE);
	if (cmdreject != 0)
	{
		S_form_vis();
		goto done;
	}
/*
.....Let the user pick the geometry
*/
	um_dl_pldas(UD_DASPCKLOC,UA_NCL,pr,&pick,1,&numint,1);
/*
.....visible the forms first, then update the form value
*/
	S_form_vis();
	if (numint == 0) goto done;
/*
.....Get the geometry label
*/
	e.key = um_get_pickkey(&pick.pent,1);
	ur_retrieve_data_fixed(&e);
/*
.....Format the label string
*/
	ncl_format_label(e.label,e.subscr,&label,0);
/*
.....Update the symbol field
*/
	if (fieldno == FDSM1)
	{
		strcpy(Ssymbol[which],label);
		ud_dispfrm_update_answer(Sfrmdis[which],FDSM2,Ssymbol[which]);
		S_axis_tog(which,Ssymbol[which]);
	}
/*
.....Update the Axis-PV field
*/
	else
	{
		strcpy(Spv[which],e.label);
		ud_dispfrm_update_answer(Sfrmdis[which],FDAX2,Spv[which]);
	}
/*
.....End of routine
*/
done:;
/*
	S_form_vis();
*/
	UD_UNMARK(cmdreject);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  static OnDisText(fieldno, val, stat)
**       Method called when a text field is changed from the
**			Tool Display form.
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
static UD_FSTAT OnDisText0(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(OnDisText(*fieldno,val,0));
}

static UD_FSTAT OnDisText1(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(OnDisText(*fieldno,val,1));
}

static UD_FSTAT OnDisText2(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(OnDisText(*fieldno,val,2));
}

static UD_FSTAT OnDisText(fieldno, val, which)
int fieldno;
UD_DDATA *val;
int which;
{
	switch (fieldno)
	{
	case FDSM2:
		strcpy(Ssymbol[which],val->frmstr);
		S_axis_tog(which,val->frmstr);
		break;
	case FDAX2:
		strcpy(Spv[which],val->frmstr);
		break;
	case FDAT1:
		strcpy(Satt[which][0],val->frmstr);
		break;
	case FDAT2:
		strcpy(Satt[which][1],val->frmstr);
		break;
	case FDAT3:
		strcpy(Satt[which][2],val->frmstr);
		break;
	case FDAT4:
		strcpy(Satt[which][3],val->frmstr);
		break;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  static OnDisTog(fieldno, val, stat)
**       Method called when the Profile Class field is changed.
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
static UD_FSTAT OnDisTog0(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(OnDisTog(*fieldno,val,0));
}

static UD_FSTAT OnDisTog1(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(OnDisTog(*fieldno,val,1));
}

static UD_FSTAT OnDisTog2(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(OnDisTog(*fieldno,val,2));
}

static UD_FSTAT OnDisTog(fieldno, val, which)
int fieldno;
UD_DDATA *val;
int which;
{
	S_filter_profs(val->frmstr,which,UU_TRUE);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  static OnDisLoad(fieldno, val, stat)
**       Method called when the symbol Load button is pressed.
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
static UD_FSTAT OnDisLoad0(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(OnDisLoad(*fieldno,val,0));
}

static UD_FSTAT OnDisLoad1(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(OnDisLoad(*fieldno,val,1));
}

static UD_FSTAT OnDisLoad2(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(OnDisLoad(*fieldno,val,2));
}

static UD_FSTAT OnDisLoad(fieldno, val, which)
int fieldno;
UD_DDATA *val;
int which;
{
	int i,inum;
	char path[256];
	if (ubu_load_symmaster(Ssymbol[which], path) == UU_SUCCESS)
	{
		uu_free(Ssym_list.item);
		Ssym_list.item = (char **)ub_get_symmaster_name(&(Ssym_list.num_item));
		Supdate = UU_TRUE;
		for (i=0;i<3;i++)
		{
			if (Sfrmdis[i] != -1)
				ud_dispfrm_update_answer(Sfrmdis[i],FDLS2,&Ssym_list);
		}
		Supdate = UU_FALSE;
		ud_dispfrm_update_answer(Sfrmdis[which],FDSM2,Ssymbol[which]);
		S_axis_tog(which,Ssymbol[which]);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  static OnDisList(fieldno, val, stat)
**       Method called when a selection is made from the Profiles or
**			Symbols list from the Tool Display form.
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
static UD_FSTAT OnDisList0(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(OnDisList(*fieldno,val,0));
}

static UD_FSTAT OnDisList1(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(OnDisList(*fieldno,val,1));
}

static UD_FSTAT OnDisList2(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	return(OnDisList(*fieldno,val,2));
}

static UD_FSTAT OnDisList(fieldno, val, which)
int fieldno;
UD_DDATA *val;
int which;
{
	int inum;
	char dir[256], dir2[256], dir0[256], dname[256];
	if (!Supdate)
	{
		strcpy(Ssymbol[which],val->frmstr);
/*
.....break the path and label
*/
		ul_break_fname(val->frmstr, dir, Ssymbol[which]);
		ud_dispfrm_update_answer(Sfrmdis[which],FDSM2,Ssymbol[which]);
		S_axis_tog(which,Ssymbol[which]);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     :  static OnDisClose(fieldno, val, stat)
**       Method called when The Tool Display form(s) is closed.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnDisClose0()
{
	OnDisClose(0);
	return(UD_FLDOK);
}

static UD_FSTAT OnDisClose1()
{
	OnDisClose(1);
	return(UD_FLDOK);
}

static UD_FSTAT OnDisClose2()
{
	OnDisClose(2);
	return(UD_FLDOK);
}

static UD_FSTAT OnDisClose(which)
int which;
{
	struct UB_symbol_rec sym;
	UU_LOGICAL found;
	int inum;
	char symlib[256], dir[256], dir2[256], dir0[256], dname[256], name[256];
/*
.....Mark the form as closed
*/
	Sfrmdis[which] = -1;
/*
.....Update the main form with the cutter symbol name
*/
	ud_update_answer(Sfld[which],Ssymbol[which]);
	if (Slibfld!=-1)
	{
/*
.....retrieve the path of symbol
*/
		ncl_parse_label(Ssymbol[which], sym.label, &sym.subscr);
		if (ub_get_symmaster_by_name(&sym, &found,1,1) == UU_SUCCESS)
		{
			strcpy(symlib, sym.path);
			ul_remove_quotes(symlib);
			ul_break_fname(symlib, dir, name);
			inum = strlen (dir);
			if ((inum>=2)&&(dir[inum-2]=='_')&&(dir[inum-1]=='S'))
			{
				dir[inum-2] = '\0';
			}
			if (dir[0]=='\0')
				strcpy(symlib, "symlib");
			else
			{
				ul_get_full_dir(UB_libdata_rec.sys_farea, dir2);
				ux_decompose_path(dir, dir0, dname, UX_NQUOTES);
				if (stricmp(dir2, dir0)!=0)
				{
					ul_get_full_dir(UB_libdata_rec.loc_farea, dir2);
					if (stricmp(dir2, dir0)==0)
						strcpy(symlib, dname);
					else
						strcpy(symlib, dir);
				}
				else
					strcpy(symlib, dname);
			}
			ud_update_answer(Slibfld, symlib);
		}
	}
	ud_update_form(0);
	return(UD_FLDOK);
}

/*********************************************************************
**    I_FUNCTION     : S_filter_profs(filter,which,chg)
**       Loads the profiles in the specified class into the form list.
**    PARAMETERS
**       INPUT  :
**				filter   = Filter text of class to display.
**          which    = 0 = Process Cutter symbol, 1 = Shank, 2 = Holder.
**				chg      = UU_TRUE if the list already exists and needs updating.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_filter_profs(filter,which,chg)
char *filter;
int which;
UU_LOGICAL chg;
{
	int i;
/*
.....Don't do anything
.....if filter has not changed
*/
	if (strcmp(filter,Slast_class[which]) == 0 && chg) return;
	strcpy(Slast_class[which],filter);
/*
.....Initialize routine
*/
	Sprof_list[which].num_item = 0;
/*
.....Loop to get all profiles
*/
	for (i=0;i<Snprofs;i++)
	{
/*
.....Check for class match
*/
		if (strcmp(Sclist[i].class,filter) == 0 || strcmp(filter,"All") == 0)
		{
			strcpy(Sprof_list[which].item[Sprof_list[which].num_item],
				Sclist[i].label);
			Sprof_list[which].num_item++;
		}
	}
/*
.....Sort the list
*/
	ud_list_sort(&Sprof_list[which]);
/*
.....Set the list
*/
	if (chg)
	{
		Supdate = UU_TRUE;
		ud_dispfrm_update_answer(Sfrmdis[which],FDLS1,&Sprof_list[which]);
		Supdate = UU_FALSE;
	}
}

/*********************************************************************
**    I_FUNCTION     : S_axis_tog(which,label)
**       Sets the traversal flag of the Axis-PV field depending on
**			whether the cutter symbol is a curve or not and modifies
**			shank/holder parameter/attach fields.
**    PARAMETERS
**       INPUT  :
**          which    = 0 = Process Cutter symbol, 1 = Shank, 2 = Holder.
**				label    = Symbol label.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_axis_tog(which,label)
int which;
char *label;
{
/*
.....Blank label
*/
	if (ul_cut_string(label,20) == 0) *Scfl[which] = *Spseudo;
/*
.....Symbol / Curve
*/
	else *Scfl[which] = 2;
/*
.....Update shank fields
*/
	nclu_tool_shank_fields(UU_FALSE);
/*
.....Set Axis PV traverse flags
*/
	if (Stype < 10 && nclu_tool_is_curve(label))
	{
		ud_setfrm_traverse_mask(Sfrmdis[which],FDAX1,UU_TRUE);
		ud_setfrm_traverse_mask(Sfrmdis[which],FDAX2,UU_TRUE);
	}
	else
	{
		ud_setfrm_traverse_mask(Sfrmdis[which],FDAX1,UU_FALSE);
		ud_setfrm_traverse_mask(Sfrmdis[which],FDAX2,UU_FALSE);
	}
}

/*********************************************************************
**    I_FUNCTION     : S_form_invis()
**       Takes down the active forms for picking mode.
**    PARAMETERS
**       INPUT  :
**				none.
**       OUTPUT :
**          none.
**    RETURNS      : UU_TRUE if entity is a curve.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_form_invis()
{
	int i;
/*
......invisible child form first, then main form

	ud_form_invis();
*/
	for (i=0;i<3;i++)
	{
		if (Sfrmdis[i] != -1) ud_dspfrm_invis(Sfrmdis[i]);
	}
	ud_form_invis();
}

/*********************************************************************
**    I_FUNCTION     : S_form_vis()
**       Displays the active forms after picking mode.
**    PARAMETERS
**       INPUT  :
**				none.
**       OUTPUT :
**          none.
**    RETURNS      : UU_TRUE if entity is a curve.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_form_vis()
{
	int i;
	ud_form_vis();
	for (i=0;i<3;i++)
	{
		if (Sfrmdis[i] != -1) ud_dspfrm_vis(Sfrmdis[i]);
	}
}


/*********************************************************************
**    E_FUNCTION     : nclu_tool_profs(filename)
**       Loads the tool profile names 'filename', if filename=NULL, load it
**		 from the NCL_TOOL_DESC library.
**    PARAMETERS
**       INPUT  :
**				none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_tool_profs(filename)
char *filename;
{
	int i,j,inxt,ntl,stat;
/*
.....Free profile memory
*/
	nclu_tool_free();
/*
.....Open NCL Tool Profile file &
.....Initiate search
*/
	stat = ncl_open_cutprof(filename, &ntl);
	Snprofs = ntl;
	if (ntl == 0) ntl = 1;
	Snclass = 1;
/*
.....Initialize lists
*/
	for (i=0;i<3;i++)
	{
		Sprof_list[i].item = (char **)uu_malloc(ntl*sizeof(char*));
		Sprof_list[i].answer = (char*)uu_malloc(21*sizeof(char));
		Sprof_list[i].answer[0] = '\0';
		Sprof_list[i].num_item = 0;

		Sclass_list[i].item = (char **)uu_malloc((ntl+1)*sizeof(char*));
		Sclass_list[i].answer = (char*)uu_malloc(21*sizeof(char));
		Sclass_list[i].answer[0] = '\0';
		Sclass_list[i].num_item = 1;
		Sclass_list[i].item[0] = (char*)uu_malloc(21*sizeof(char));
		strcpy(Sclass_list[i].item[0],"All");
	}

	Sclist = (Sprof_struc *)uu_malloc(sizeof(Sprof_struc)*ntl);
/*
.....Library does not exist
*/
	if (stat != UU_SUCCESS)
	{
		for (i=0;i<3;i++)
		{
			Sprof_list[i].num_item = 1;
			Sprof_list[i].item[0] = (char*)uu_malloc(21*sizeof(char));
			strcpy(Sprof_list[i].item[0], " ");
		}
	}
/*
.....Loop to get all profiles
*/
	else
	{
		inxt   = 0;
		for (i=0;i<ntl;i++)
		{
/*
........Get the next profile
*/
			ncl_next_cutprof(&inxt,Sclist[i].label,Sclist[i].class);
			for (j=0;j<3;j++)
				Sprof_list[j].item[i] = (char*)uu_malloc(21*sizeof(char));
/*
........Store the class
*/
			for (j=0;j<Snclass;j++)
			{
				if (ul_compare_upper(Sclist[i].class,Sclass_list[0].item[j]) == 0)
					break;
			}
			if (j >= Snclass)
			{
				for (j=0;j<3;j++)
				{
					Sclass_list[j].item[Snclass] =
						(char*)uu_malloc(21*sizeof(char));
					strcpy(Sclass_list[j].item[Snclass],Sclist[i].class);
					Sclass_list[j].num_item++;
				}
				Snclass++;
			}
		}
/*
.....Sort the lists
*/
		for (i=0;i<3;i++) ud_list_sort(&Sclass_list[i]);
	}
}

/*********************************************************************
**    E_FUNCTION     : nclu_tool_syms()
**       Loads the CADD symbol names into the list.
**    PARAMETERS
**       INPUT  :
**				none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_tool_syms(flag)
int flag;
{
/*
.....Initialize list
*/
	Ssym_list.answer = (char*)uu_malloc(MAXSYMLEN*sizeof(char));
	Ssym_list.answer[0] = '\0';
/*
.....Load the symbol names
*/
	Ssym_list.item = (char **)ub_get_symmaster_name(&(Ssym_list.num_item));
/*
.....Sort the list
*/
	ud_list_sort(&Ssym_list);
}

nclu_tool_uptsyms()
{
	if (Ssym_list.answer!=NULL)
	{
		uu_free (Ssym_list.answer);
		Ssym_list.answer = NULL;
	}
	if ((Ssym_list.item!=NULL)&&(Ssym_list.num_item>0))
	{
		uu_free(Ssym_list.item);
		Ssym_list.item = NULL;
		Ssym_list.num_item = 0;
	}
	nclu_tool_syms(0);
}

/*********************************************************************
**    E_FUNCTION     : nclu_tool_ctype(symfl,symbol,pseudo,icfl)
**       Sets the correct display types for the cutter, shank, and holder.
**    PARAMETERS
**       INPUT  :
**          symfl  = UU_TRUE = Cutter, Shank, and Holder symbol is defined.
**          symbol = Cutter, Shank, and Holder symbol.
**          pseudo = Pseudo cutter definition flag.
**       OUTPUT :
**          icfl   = 0 = No symbol defined, 1 = Parameters, 2 = Symbol,
**				         3 = Geometry, 4 = Profile.
**    RETURNS      : none
**    SIDE EFFECTS : Sets the appropriate Scfl settings.
**    WARNINGS     : none
*********************************************************************/
void nclu_tool_ctype(symfl,symbol,pseudo,icfl)
int symfl[3],pseudo,icfl[];
char symbol[3][MAXSYMLEN];
{
	int i,j,nc,ipt[3];
	char tmpstr[300], dir[256], name[80];
/*
.....Initialize routine
*/
	ipt[0] = 0; ipt[1] = 4; ipt[2] = 5;
/*
.....Set cutter type
*/
	for (i=0;i<3;i++)
	{
		nc = MAXSYMLEN;
		ul_strip_blanks(symbol[i],&nc);
		if (symfl[i] && nc != 0)
		{
			icfl[ipt[i]] = 3;
			ul_to_upper(symbol[i]);
/*
........Check for a profile
*/
			for (j=0;j<Snprofs;j++)
			{
				if (strcmp(symbol[i],Sclist[j].label) == 0)
				{
					icfl[ipt[i]] = 4;
					break;
				}
			}
/*
........Check for a symbol
*/
			if (icfl[ipt[i]] == 3)
			{
				for (j=0;j<Ssym_list.num_item;j++)
				{
					strcpy(tmpstr, Ssym_list.item[j]);
					ul_break_fname(tmpstr, dir, name);
					if (stricmp(symbol[i],name) == 0)
					{
						icfl[ipt[i]] = 2;
						break;
					}
				}
			}
		}
/*
.....Standard cutter
*/
		else
		{
			if (i == 0) icfl[ipt[i]] = pseudo;
			else if (symfl[i]) icfl[ipt[i]] = 1;
			else icfl[ipt[i]] = 0;
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : nclu_tool_is_curve(label)
**       Determines if the cutter symbol is a curve.
**    PARAMETERS
**       INPUT  :
**				label    = Label of entity to test.
**       OUTPUT :
**          none.
**    RETURNS      : UU_TRUE if entity is a curve.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL nclu_tool_is_curve(label)
char *label;
{
	int nc,relnum;
	UU_LOGICAL iret;
	char str[NCL_MAX_LABEL_AND_SUBSCRIPT+1];
	UU_KEY_ID key;
	UM_f77_str f77_str;
/*
.....Get key of entity
*/
	strcpy(str,label);
	ul_to_upper(str);
	nc = strlen(str);
	UM_init_f77_str(f77_str,str,nc);
	getkey(UM_addr_of_f77_str(f77_str),&key);
/*
.....Determine if curve
*/
	iret = UU_FALSE;
	if (key != 0)
	{
		ur_retrieve_data_relnum(key,&relnum);
		if (uc_super_class(relnum) == UC_CURVE_CLASS) iret = UU_TRUE;
	}
	return(iret);
}
/*********************************************************************
**    E_FUNCTION     : nclu_tool_shank_fields(chg)
**       Sets up the Tool Display fields based on the cutter type
**       and the type of tool part is being displayed.
**    PARAMETERS
**       INPUT  :
**          chg   = UU_TRUE - Update the field values as well as the
**				        prompts.  UU_FALSE = Update the prompts only.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_tool_shank_fields(chg)
UU_LOGICAL chg;
{
	int i,j,ipt[3],inc;
	UU_LOGICAL itrv[4];
	char spr[4][20];
/*
.....Loop through all Tool Display forms
*/
	ipt[0] = 3; ipt[1] = 6; ipt[2] = 7;
	for (j=0;j<3;j++)
	{
		if (Sfrmdis[j] == -1) continue;
/*
.....Cutter
*/
		if (j == 0)
		{
/*
........Mill cutter / Lathe Parameters
*/
			if (Stype < 10 || *Scfl[0] < 2)
			{
				itrv[0] = itrv[1] = itrv[2] = itrv[3] = UU_FALSE;
				spr[0][0] = spr[1][0] = spr[2][0] = spr[3][0] = '\0';
			}
/*
........Lathe symbol cutter
*/
			else
			{
				itrv[0] = itrv[1] = UU_TRUE;
				itrv[2] = itrv[3] = UU_FALSE;
				strcpy(spr[0],"Z-Attach:");
				strcpy(spr[1],"Z-Depth:");
				spr[2][0] = spr[3][0] = '\0';
			}
		}
/*
.....Shank / Holder
*/
		else
		{
/*
........Mill
*/
			if (Stype < 10)
			{
/*
...........Parameters
*/
				if (*Scfl[j] < 2)
				{
					itrv[0] = itrv[1] = itrv[2] = itrv[3] = UU_TRUE;
					strcpy(spr[0],"Diameter:");
					strcpy(spr[1],"Height:");
					strcpy(spr[2],"Angle:");
					strcpy(spr[3],"Z-Attach:");
				}
/*
...........Symbol
*/
				else
				{
					itrv[0] = UU_TRUE;
					itrv[1] = itrv[2] = itrv[3] = UU_FALSE;
					strcpy(spr[0],"Z-Attach:");
					spr[1][0] = spr[2][0] = spr[3][0] = '\0';
				}
			}
/*
........Lathe
*/
			else
			{
/*
...........Parameters
*/
				if (*Scfl[j] < 2)
				{
					itrv[0] = itrv[1] = itrv[2] = itrv[3] = UU_TRUE;
					strcpy(spr[0],"Width:");
					strcpy(spr[1],"Length:");
					strcpy(spr[2],"Z-Depth:");
					strcpy(spr[3],"Y-Offset:");
				}
/*
...........Symbol
*/
				else
				{
					itrv[0] = itrv[1] = itrv[2] = itrv[3] = UU_TRUE;
					strcpy(spr[0],"X-Offset:");
					strcpy(spr[1],"Y-Offset:");
					strcpy(spr[2],"Z-Attach:");
					strcpy(spr[3],"Z-Depth:");
				}
			}
		}
/*
.....Update prompts
*/
		for (i=0;i<4;i++)
		{
			ud_dispfrm_update_prompt(Sfrmdis[j],FDAT1+i,spr[i]);
			ud_setfrm_traverse_mask(Sfrmdis[j],FDAT1+i,itrv[i]);
		}
		if (Stype < 10 && nclu_tool_is_curve(Ssymbol[j]))
		{
			ud_setfrm_traverse_mask(Sfrmdis[j],FDAX1,UU_TRUE);
			ud_setfrm_traverse_mask(Sfrmdis[j],FDAX2,UU_TRUE);
		}
		else
		{
			ud_setfrm_traverse_mask(Sfrmdis[j],FDAX1,UU_FALSE);
			ud_setfrm_traverse_mask(Sfrmdis[j],FDAX2,UU_FALSE);
		}
/*
.....Update the field values
*/
		if (chg)
		{
			ud_dispfrm_update_answer(Sfrmdis[j],FDSM2,Ssymbol[j]);
			ud_dispfrm_update_answer(Sfrmdis[j],FDAX2,Spv[j]);
			if (Sfrmtyp == 0 || Sfrmtyp == 1)
				ud_dispfrm_update_answer(Sfrmdis[j],FDSHD,Sshade[j]);
			else
			{
				ud_dispfrm_update_answer(Sfrmdis[j],FDEDG,Sedge[j]);
				ud_dispfrm_update_answer(Sfrmdis[j],FDEDC,Sedgcol[j]);
				ud_dispfrm_update_answer(Sfrmdis[j],FDTRA,Sshade[j]);
			}
			if (Sfrmtyp == 1)
				ud_dispfrm_update_answer(Sfrmdis[j],FDCS1,Sclash[j]);
			if (Sfrmtyp == 3)
				ud_dispfrm_update_answer(Sfrmdis[j],FDCSH,Sclash[j]);
			inc = j * 4;
			ud_dispfrm_update_answer(Sfrmdis[j],FDAT1,Satt[j][0]);
			ud_dispfrm_update_answer(Sfrmdis[j],FDAT2,Satt[j][1]);
			ud_dispfrm_update_answer(Sfrmdis[j],FDAT3,Satt[j][2]);
			ud_dispfrm_update_answer(Sfrmdis[j],FDAT4,Satt[j][3]);
		}
		ud_update_form(Sfrmdis[j]);
	}
}

/*********************************************************************
**    E_FUNCTION     : nclu_tool_shade_fields(shade,edge,edgcol,clash)
**       Applies the setting to the Shaded field of the Tool Display forms.
**    PARAMETERS
**       INPUT  :
**          shade  = New Shaded/Translucency field setting.
**          edge   = New Display Edges field setting.
**          edgcol = New Edge Color field setting.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_tool_shade_fields(shade,edge,edgcol)
int shade,edgcol;
UU_LOGICAL edge;
{
	int i;
/*
.....Loop through all Tool Display forms
*/
	for (i=0;i<3;i++)
	{
		if (Sfrmdis[i] != -1)
		{
			if (shade != -1)
			{
				*Sshade[i] = shade;
				if (Sfrmtyp == 0 || Sfrmtyp == 1)
					ud_dispfrm_update_answer(Sfrmdis[i],FDSHD,Sshade[i]);
				else
					ud_dispfrm_update_answer(Sfrmdis[i],FDTRA,Sshade[i]);
			}
			if (Sfrmtyp != 0 && Sfrmtyp != 1)
			{
				if (edge != -1)
					ud_dispfrm_update_answer(Sfrmdis[i],FDEDG,Sedge[i]);
				if (edgcol != -1)
					ud_dispfrm_update_answer(Sfrmdis[i],FDEDC,Sedgcol[i]);
			}
			if (Sfrmtyp == 1)
				ud_dispfrm_update_answer(Sfrmdis[i],FDCS1,Sclash[i]);
			else if (Sfrmtyp == 2)
				ud_dispfrm_update_answer(Sfrmdis[i],FDCSH,Sclash[i]);
			ud_update_form(Sfrmdis[i]);
		}
	}
}

/*********************************************************************
**    E_FUNCTION     : nclu_tool_close_form(which)
**       Closes a Tool Display form if active.
**    PARAMETERS
**       INPUT  :
**          which    = 0 = Close Cutter form, 1 = Shank, 2 = Holder.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_tool_close_form(which)
int which;
{
	if (Sfrmdis[which] != -1) ud_close_dispfrm(Sfrmdis[which]);
}

/*********************************************************************
**    E_FUNCTION     : nclu_tool_free()
**       Frees the Tool Display form lists.
**    PARAMETERS
**       INPUT  :
**          none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_tool_free()
{
	int i;
/*
.....Free the form lists
*/
	if (Snprofs != 0)
	{
		for (i=0;i<3;i++)
		{
			ud_free_flist(&Sprof_list[i]);
			ud_free_flist(&Sclass_list[i]);
		}
		if (Sclist != UU_NULL) uu_free(Sclist);
		Sclist = UU_NULL;
	}
	Snprofs = 0;
	Snclass = 0;
}


/*********************************************************************
**    I_FUNCTION     : S_filter_profs2(filter, chg)
**       Loads the profiles in the specified class into the form list. 
**			for Symbol list table
**    PARAMETERS
**       INPUT  :
**				filter   = Filter text of class to display.
**				chg      = UU_TRUE if the list already exists and needs updating.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_filter_profs2(filter,chg)
char *filter;
UU_LOGICAL chg;
{
	int i;
/*
.....Don't do anything
.....if filter has not changed
*/
	if (strcmp(filter, Slast_class_sym) == 0 && chg) return;
	strcpy(Slast_class_sym, filter);
	Sprof_list_sym.num_item = 0;
/*
.....Loop to get all profiles
*/
	for (i=0;i<Snprofs_sym;i++)
	{
/*
.....Check for class match
*/
		if (strcmp(Sclist_sym[i].class,filter) == 0 || strcmp(filter,"All") == 0)
		{
			strcpy(Sprof_list_sym.item[Sprof_list_sym.num_item],
					Sclist_sym[i].label);
			Sprof_list_sym.num_item++;
		}
	}
/*
.....Sort the list
*/
	ud_list_sort(&Sprof_list_sym);
/*
.....Set the list
*/
	if (chg)
	{
		ud_dispfrm_update_answer(Sfrmsym, 2, &Sprof_list_sym);
	}
}

/*********************************************************************
**    E_FUNCTION     : nclu_tool_profs2()
**      Fill the prof_list.
**    PARAMETERS
**       INPUT  :
**				none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_tool_profs2()
{
	int i,j, inxt,ntl,status;
	char label[21], class_label[21];
/*
.....Free the form lists
*/
	if (Snprofs_sym != 0)
	{
		ud_free_flist(&Sprof_list_sym);
		ud_free_flist(&Sclass_list_sym);
		if (Sclist_sym != UU_NULL) uu_free(Sclist_sym);
		Sclist_sym = UU_NULL;
	}
	Snprofs_sym = 0;
	Snclass_sym = 0;
/*
.....Initialize routine
*/
	Sprof_list_sym.num_item = 0;
/*
.....Open NCL Tool Profile file &
.....Initiate search
*/
	status = ncl_open_cutprof(NULL, &ntl);
	Snprofs_sym = ntl;
	if (ntl == 0) ntl = 1;
	Snclass_sym = 1;
/*
.....Initialize lists
*/
	Sprof_list_sym.item = (char **)uu_malloc(ntl*sizeof(char*));
	Sprof_list_sym.answer = (char*)uu_malloc(21*sizeof(char));
	Sprof_list_sym.answer[0] = '\0';
	Sprof_list_sym.num_item = 0;

	Sclass_list_sym.item = (char **)uu_malloc((ntl+1)*sizeof(char*));
	Sclass_list_sym.answer = (char*)uu_malloc(21*sizeof(char));
	Sclass_list_sym.answer[0] = '\0';
	Sclass_list_sym.num_item = 1;
	Sclass_list_sym.item[0] = (char*)uu_malloc(21*sizeof(char));
	strcpy(Sclass_list_sym.item[0],"All");

	Sclist_sym = (Sprof_struc *)uu_malloc(sizeof(Sprof_struc)*ntl);
/*
.....Library does not exist
*/
	if (status != UU_SUCCESS)
	{
		Sprof_list_sym.num_item = 1;
		Sprof_list_sym.item[0] = (char*)uu_malloc(21*sizeof(char));
		strcpy(Sprof_list_sym.item[0], " ");
	}
/*
.....Loop to get all profiles
*/
	else
	{
		inxt   = 0;
		for (i=0;i<ntl;i++)
		{
/*
........Get the next profile
*/
			ncl_next_cutprof(&inxt, Sclist_sym[i].label, Sclist_sym[i].class);
			Sprof_list_sym.item[i] = (char*)uu_malloc(21*sizeof(char));
/*
........Store the class
*/
			for (j=0;j<Snclass_sym;j++)
			{
				if (ul_compare_upper(Sclist_sym[i].class, Sclass_list_sym.item[j]) == 0)
					break;
			}
			if (j >= Snclass_sym)
			{
				Sclass_list_sym.item[Snclass_sym] =
						(char*)uu_malloc(21*sizeof(char));
				strcpy(Sclass_list_sym.item[Snclass_sym], Sclist_sym[i].class);
				Sclass_list_sym.num_item++;
				Snclass_sym++;
			}
		}
/*
.....Sort the lists
*/
		ud_list_sort(&Sclass_list_sym);
	}
}

static UD_FSTAT OnProfSel(fieldno, val, which)
int fieldno;
UD_DDATA *val;
int which;
{
	strcpy(Ssymsel, val->frmstr);
	ud_dispfrm_update_answer(Sfrmsym, 0, Ssymsel);

	Ssym_list_sym.answer[0] = '\0';
	ud_dispfrm_update_answer(Sfrmsym, 3, (int *)&Ssym_list_sym);
	return(UD_FLDOK);
}

static UD_FSTAT OnSymbolSel(fieldno, val, which)
int fieldno;
UD_DDATA *val;
int which;
{
	strcpy(Ssymsel, &((val->frmstr)[2]));
	strcpy(Ssym_list_sym.answer, val->frmstr);
/*
.....break the path and label
*/
	ud_dispfrm_update_answer(Sfrmsym, 0, Ssymsel);

	Sprof_list_sym.answer[0] = '\0';
	ud_dispfrm_update_answer(Sfrmsym, 2, (int *)&Sprof_list_sym);

	return(UD_FLDOK);
}


static UD_FSTAT OnFiltClass(fieldno, val, which)
int fieldno;
UD_DDATA *val;
int which;
{
	S_filter_profs2(val->frmstr, UU_TRUE);
	return(UD_FLDOK);
}

/*********************************************************************
**   I_FUNCTION: OnApplySymbol(fieldno,val,stat)
**      Callback function for a list Selecton from the Select Symbol form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnApplySymbol(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	UX_pathname tempstr, symlib, symb;
	struct UB_symbol_rec sym;
	char serror[UX_MAX_PATH_LEN+50], tmpstr[UX_MAX_PATH_LEN+50];

	if (Ssym_list_sym.answer[0]!='\0')
	{
/*
.....load the symbol
*/
		ul_break_fname(Ssymsel, symlib, symb);
		if (ubi_load_file(UU_NULL,symlib, symb, UU_NULL,"WHOLESYM",
						&sym,UX_NPRTERRS) != UU_SUCCESS)
		{
/*
.....give error and return
*/
			sprintf (serror,"Cannot Open Symbol File %s.", Ssymsel);
			ud_wrerr (serror);
			return (UD_BADREQ);
		}
		for (i=0;i<Ssym_list_sym.num_item;i++)
		{
			if (stricmp(Ssym_list_sym.answer, Ssym_list_sym.item[i])==0)
			{
				strcpy(tmpstr, Ssym_list_sym.item[i]);
				strcpy(Ssym_list_sym.item[i], "* ");
				strcat(Ssym_list_sym.item[i], &(tmpstr[2]));
				ud_dispfrm_update_answer(Sfrmsym, 3, (int *)&Ssym_list_sym);
				break;
			}
		}
/*
.....check if Ssymsel is included a path, if yes, added double quote 
.....before we update the input
*/
		strcpy (tmpstr, Ssymsel);
		if (symlib[0]!='\0')
		{
			ul_add_quotes (tmpstr, tmpstr);
		}
		ncl_update_input(tmpstr);
	}
	else if (Ssymsel[0]!='\0')
	{
/*
.....check if Ssymsel is included a path, if yes, added double quote 
.....before we update the input
*/
		strcpy (tmpstr, Ssymsel);
		ul_break_fname(tmpstr, symlib, symb);
		if (symlib[0]!='\0')
		{
			strcpy (tmpstr, Ssymsel);
			ul_add_quotes (tmpstr, tmpstr);
		}
		ncl_update_input(tmpstr);
	}
	return(UD_FLDOK);
}
/*********************************************************************
**   I_FUNCTION: OnCancelSymbol(fieldno,val,stat)
**      Callback function for a list Selecton from the Select symbol form.
**   PARAMETERS
**       INPUT  : fieldno  = Field number being changed.
**                val      = Current field value.
**                stat     = Field status.
**       OUTPUT : none.
**   RETURNS: none
**   SIDE EFFECTS: none
**   WARNINGS: none
*********************************************************************/
static UD_FSTAT OnCancelSymbol(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....User rejected the form
*/
	Sfrm_cancel = -1;
	ud_close_dispfrm(Sfrmsym);
	*fieldno = -1;
	if (Ssym_list_sym.answer!=NULL)
		Ssym_list_sym.answer[0] = '\0';
	Sfrmsym = -1;
	return(UD_FRMCLOSE);
}

/*********************************************************************
**    I_FUNCTION     :  static OnSymClose(fieldno, val, stat)
**       Method called when The Symbol form(s) is closed.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnSymClose()
{
	struct UB_symbol_rec sym;
	char serror[UX_MAX_PATH_LEN+50];
	UX_pathname tmpstr, dir, name;
/*
.....put symbol name in to current active edit field or form active edit filed
*/
	if ((Sfrm_cancel!=-1) && (Ssym_list_sym.answer[0]!='\0'))
	{
/*
.....load the symbol
*/
		if (ubi_load_file(UU_NULL,NULL,Ssymsel,UU_NULL,"WHOLESYM",
						&sym,UX_NPRTERRS) != UU_SUCCESS)
		{
/*
.....give error and return
*/
			sprintf (serror,"Cannot Open Symbol File %s.", Ssymsel);
			ud_wrerr (serror);
			return (UD_BADREQ);
		}
/*
.....check if Ssymsel is included a path, if yes, added double quote 
.....before we update the input
*/
		strcpy (tmpstr, Ssymsel);
		ul_break_fname(tmpstr, dir, name);
		if (dir[0]!='\0')
		{
			strcpy (tmpstr, Ssymsel);
			ul_add_quotes (tmpstr, tmpstr);
		}
		ncl_update_input(tmpstr);
	}
	else if ((Sfrm_cancel!=-1) &&(Ssymsel[0]!='\0'))
	{
/*
.....check if Ssymsel is included a path, if yes, added double quote 
.....before we update the input
*/
		strcpy (tmpstr, Ssymsel);
		ul_break_fname(tmpstr, dir, name);
		if (dir[0]!='\0')
		{
			strcpy (tmpstr, Ssymsel);
			ul_add_quotes (tmpstr, tmpstr);
		}
		ncl_update_input(tmpstr);
	}
 /*
.....Mark the form as closed
*/
	Sfrmsym = -1;
	if (Ssym_list_sym.answer!=NULL)
	{
		uu_free (Ssym_list_sym.answer);
		Ssym_list_sym.answer = NULL;
	}
	if ((Ssym_list_sym.item!=NULL)&&(Ssym_list_sym.num_item>0))
	{
		uu_free(Ssym_list_sym.item);
		Ssym_list_sym.item = NULL;
		Ssym_list_sym.num_item = 0;
	}
/*
.....Free the form lists
*/
	if (Snprofs_sym != 0)
	{
		ud_free_flist(&Sprof_list_sym);
		ud_free_flist(&Sclass_list_sym);
		if (Sclist_sym != UU_NULL) uu_free(Sclist_sym);
		Sclist_sym = UU_NULL;
	}
	Snprofs_sym = 0;
	Snclass_sym = 0;
 	return(UD_FLDOK);
}

/*********************************************************************
**    E_FUNCTION     : nclu_tool_syms()
**       Loads the CADD symbol names into the list.
**    PARAMETERS
**       INPUT  :
**				none.
**       OUTPUT :
**          none.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_tool_syms2()
{
	char ** S_get_symmaster_name();
/*
.....Initialize list
*/
	Ssym_list_sym.answer = (char*)uu_malloc(MAXSYMLEN*sizeof(char));
	Ssym_list_sym.answer[0] = '\0';
/*
.....Load the symbol names
*/
	Ssym_list_sym.item = (char **)S_get_symmaster_name(&(Ssym_list_sym.num_item));
/*
.....Sort the list
*/
	ud_list_sort(&Ssym_list_sym);
}

static int S_check_sym(char *name, char **symbol_array, int num)
{
	int i;
	for (i=0;i<num;i++)
	{
		if (stricmp(&(symbol_array[i][2]), name)==0)
			return 1;
	}
	return 0;
}

char ** S_get_symmaster_name(item_num)
int *item_num;
{
	char label[80];
	struct UB_symbol_rec master;
	char **symbol_name, *position;
	int i,len, status, nxtuple;
	UX_pathname fullibname, filename;
	UX_pathname dirs[40];
	char *list;
	char *listhead;	
	int load_sym, dir_num;

	*item_num = 0;
	dir_num = 0;
	symbol_name = (char **) uu_malloc(2000*sizeof(char *));
/*
......get system symbol path
*/
	if (ux_mk_chk_syspath("UB_SYS_M_SYMDIR", UU_NULL, UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,fullibname, UX_PRTERRS) != UU_SUCCESS)
		goto next;
/*
.....get the symlib under the system path, the directory with UB_SYM_AREA_SUFFIX: '_S'
*/
	if (ux_get_flist(fullibname, UX_FAREA, "UB_SYM_AREA_SUFFIX", &list,
			(UX_PRTERRS|UX_NCHK)) != UU_SUCCESS)
			goto next;
	if (list == UU_NULL)
		goto next;
	listhead = list;
	while ((status = ux_nxt_file(&list,filename,UX_PRTERRS)) == UU_SUCCESS)
	{	
		strcpy(dirs[dir_num], filename);
		dir_num++;
	}
	uu_lsdel(listhead);
next:;
/*
......get local symbol path
*/
	if (ux_mk_chk_syspath("UB_LOC_M_SYMDIR", UU_NULL, UU_NULL ,UU_NULL,UU_NULL,
		UU_NULL,fullibname, UX_PRTERRS) != UU_SUCCESS)
		goto next_2;

	if (ux_get_flist(fullibname, UX_FAREA, "UB_SYM_AREA_SUFFIX", &list,
			(UX_PRTERRS|UX_NCHK)) != UU_SUCCESS)
			goto next_2;
	if (list == UU_NULL)
		goto next_2;
	listhead = list;
	while ((status = ux_nxt_file(&list,filename,UX_PRTERRS)) == UU_SUCCESS)
	{	
		strcpy(dirs[dir_num], filename);
		dir_num++;
	}
	uu_lsdel(listhead);
next_2:;
/*
.....Get the loaded symbol first
*/
	nxtuple = 1;
	master.rel_num = UB_SYMBOL_REL;
	while (ur_get_next_data_key(master.rel_num, &nxtuple, &master.key) == 0)
	{
		nxtuple++;
		if (ub_retrieve_sym(&master, sizeof(struct UB_symbol_rec)) 
				!= UU_SUCCESS) break;
/*
.....don't return temperate data start with "@"
.....Yurong
*/
		if (master.label[0]!='@')
		{
			len = strlen(master.label);
			symbol_name[*item_num] =
				(char *)uu_malloc((UX_MAX_PATH_LEN+25)*sizeof(char));
			strcpy(symbol_name[*item_num], "* ");
			ul_remove_quotes(master.path);
			if (master.path[0]!='\0')
				strcat(symbol_name[*item_num], master.path);
			else
				strcat(symbol_name[*item_num], master.label);
			(*item_num)++;
		}
	}
	load_sym = *item_num;
/*
......get all symbol filename
*/
	for (i=0; i<dir_num;i++)
	{
		if (ux_get_flist(dirs[i], UX_NFAREA, "UB_SYM_SUFFIX", &list,
			(UX_PRTERRS|UX_NCHK)) != UU_SUCCESS)
			continue;
		if (list == UU_NULL)
			continue;
		listhead = list;
		while ( (status = ux_nxt_file(&list,filename,UX_PRTERRS)) == UU_SUCCESS)
		{	
			len = strlen(filename);
			symbol_name[*item_num] =
							(char *)uu_malloc((len+4)*sizeof(char));
/*
.....Check for symbol name in Unibase (loaded)
*/
			if (S_check_sym(filename, symbol_name, load_sym)==0)
			{
				strcpy(symbol_name[*item_num], "  ");
				strcat(symbol_name[*item_num], filename);
				(*item_num)++;
			}
		}
		uu_lsdel(listhead);
		if (*item_num>1999) 
			break;
	}
done:;
	return symbol_name;
}
/*********************************************************************
**    E_FUNCTION     : nclu_sym_table()
**       Processes the Symbol list form
**			
**    PARAMETERS
**       INPUT  :
**                None.
**       OUTPUT :
**          None
**    RETURNS      : none
**    SIDE EFFECTS :
**			None
**    WARNINGS     : none
*********************************************************************/
void nclu_sym_table()
{
	int i,inc,idum, icancel, iapply;
	char label[20];
/*
.....Set up form fields
*/
	static UD_METHOD methods[7] = {UU_NULL, OnFiltClass, OnProfSel, OnSymbolSel, 
								OnApplySymbol, OnCancelSymbol, OnSymClose};
	static char traverse[] = {1,1,1,1,1,1,1,1};
/*
.....need one more value for 'close' callback function
*/
	static char called[] = {6,6,6,6,6,6, 6,6, 6};

	static char display[] = {1,1,1,1,1,1,1,1,1};

	int *ans[6];
/*
.....Form is already open
*/
	if (Sfrmsym != -1) return;

	nclu_tool_profs2();
	nclu_tool_syms2();
/*
.....Fill the Profile List
*/
	S_filter_profs2(Slast_class_sym, UU_FALSE);

/*
.....Set up the default form fields
*/
	ans[0] = (int *)Ssymsel;
	ans[1] = (int *)&Sclass_list_sym;
	strcpy(Sclass_list_sym.answer, Slast_class_sym);
	ans[2] = (int *)&Sprof_list_sym;
	Sprof_list_sym.answer[0] = '\0';
	ans[3] = (int *)&Ssym_list_sym;
	strcpy(Ssym_list_sym.answer, Ssymsel);
	ans[4] = &icancel;
	ans[5] = &iapply;
/*
.....Get the Form input
*/
	Sfrmsym = ud_form_display1("symtable.frm", ans, ans, methods, called,
		display, traverse);
	if (Sfrmsym == -1) goto nofrm;
/*
.....Wait for the user to select a view
.....or cancel the form
*/
	Sfrm_cancel = 0;
	goto done;
nofrm:
	ud_wrerr("Could not load 'symtable.frm'.");
	Sfrm_cancel = -1;
	goto done;
/*
.....End of routine
*/
done:
	return;
}




