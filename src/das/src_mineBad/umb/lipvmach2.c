/*********************************************************************
**    NAME         :  lipvmach2.c
**       CONTAINS:
**				ul_ipv_mach_tools()
**				ul_ipv_mach_attr()
**    COPYRIGHT 2004 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       lipvmach2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:14
*********************************************************************/

#include "usysdef.h"
#include "stdio.h"
#include "lcom.h"
#include "lipv.h"
#include "lipvmach.h"
#include "mfort.h"
#include "mdcpln.h"
#include "nclfc.h"
#include "nclmplay.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "driver.h"

static UD_FSTAT OnApply2();
static UD_FSTAT OnAxis();
static UD_FSTAT OnCompon();
static UD_FSTAT OnSolid();
static UD_FSTAT OnText1();
static UD_FSTAT OnTool();
static UD_FSTAT OnClose1();
static UD_FSTAT OnClose2();
static void S_default_tool();
static void S_fmt_solid();
static void S_fmt_tool();
static void S_hilite();
static void S_load_axes();
static void S_load_list();
static void S_load_solids();
static void S_save_fields();
static void S_unhilite();
static void S_update_form();
static void S_update_form2();

static int *Sfrm1,*Sfrm2;
static UU_LOGICAL *Sactive1,*Sactive2;
static UD_LIST Slist;
static UU_LIST *Stools;
static int Stoolpt=0,Stlno,Sntl;
static UU_REAL Stlen,Stlofs;
static UN_cutter_list *Scpt=UU_NULL;

static int Smix,Ssix,Scolor,Svis,Strans,Srev,Sedge,Sedgcol;
static int Snmodel,Snsolid;
static char Saxis[80];
static LW_mach_model_struc *Smodel;
static LW_mach_solid_struc *Ssolid;
static UD_LIST Slist2[2];

#define FLTNO 0
#define FLTLN 1
#define FLOFF 2
#define FLLST 3

#define FACOL 0
#define FAVIS 1
#define FATRA 2
#define FAREV 3
#define FAEDG 4
#define FAEDC 5
#define FACMB 6
#define FACOM 7
#define FAPPL 8
#define FAAXS 9
#define FASOL 10

/*********************************************************************
**    E_FUNCTION     : ul_ipv_mach_tools(fptr,factive,clist,ntl)
**       Processes the NCLIPV Tool Lengths form.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          fptr     = Displayed form id.
**          factive  = UU_TRUE if form is successfully activated.
**          clist    = List of defined tools.
**          ntl      = Number of tools currently in list.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_mach_tools(fptr,factive,clist,ntl)
int *fptr;
UU_LOGICAL *factive;
UU_LIST *clist;
int ntl;
{
	int i,ifl;
/*
.....Set up form fields
*/
	static char traverse[]     = {1,1,1,1};
	static UD_METHOD methods[] = {UU_NULL,OnText1,OnText1,OnTool,OnClose1};
	static char called[] = {6,6,6,6};
	static char display[] = {1,1,1,1};
	static int *ans[] = {&Stlno,(int *)&Stlen,(int *)&Stlofs,(int *)&Slist};
/*
.....Initialize routine
*/
	Sfrm1 = fptr;
	Sactive1 = factive;
	Stools = clist;
	Slist.num_item = 0;
	Slist.item = UU_NULL;
	Slist.answer = UU_NULL;
	Stoolpt = 0;
/*
.....Store the tool data
*/
	Sntl = ntl;
	Scpt = (UN_cutter_list *)UU_LIST_ARRAY(clist);
/*
.....Store tools in form list
*/
	if (Sntl == 0)
	{
		Stoolpt = -1;
	}
	else
	{
		Stoolpt = 0;
		S_load_list(UU_FALSE);
	}
/*
.....If there are no tools
.....Then disable all fields
*/

	if (Sntl == 0) ifl = 0;
	else ifl = 1;
	for (i=0;i<=FLLST;i++) traverse[i] = ifl;
/*
.....Set the form defaults
.....based on the first tool
*/
	if (Sntl != 0)
	{
		Stlno = Scpt[0].tlno;
		UM_len_inttoext(Scpt[0].tlen,Stlen);
		UM_len_inttoext(Scpt[0].tlofs,Stlofs);
	}
/*
.....Get the Form input
*/
	*Sfrm1 = ud_form_display1("ipvmachtool.frm", ans, ans, methods, called,
		display, traverse);
	if (*Sfrm1 != -1) *Sactive1 = UU_TRUE;
	else *Sactive1 = UU_FALSE;
	return;
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_mach_attr(fptr,factive,mpt,spt,nmodel,nsolid)
**       Processes the Machine Component Attributes form.
**    PARAMETERS
**       INPUT  :
**          mpt      = Pointer to Machine Model structure.
**          spt      = Pointer to Machine Solid strucutre.
**          nmodel   = Number of axes defined in 'mpt'.
**          nsolid   = Number of solids defined in 'mpt'.
**       OUTPUT :
**          fptr     = Displayed form id.
**          factive  = UU_TRUE if form is successfully activated.
**          stopfl   = Modified stop flags.
**          logfl    = Modified log flags.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_mach_attr(fptr,factive,mpt,spt,nmodel,nsolid)
int *fptr;
UU_LOGICAL *factive;
LW_mach_model_struc *mpt;
LW_mach_solid_struc *spt;
int nmodel,nsolid;
{
/*
.....Set up form fields
*/
	static char traverse[]     = {1,1,1,1,1,1,1,1,1,1,1};
	static UD_METHOD methods[] = {UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,OnCompon,UU_NULL,OnApply2,OnAxis,OnSolid,OnClose2};
	static char called[] = {6,6,6,6,6,6,6,6,6,6,6};
	static char display[] = {1,1,1,1,1,1,1,1,1,1,1};
	static int *ans[] = {&Scolor,&Svis,&Strans,&Srev,&Sedge,&Sedgcol,UU_NULL,
		(int *)&Saxis,UU_NULL,(int *)&Slist2[0],(int *)&Slist2[1]};
/*
.....Initialize routine
*/
	Sfrm2 = fptr;
	Sactive2 = factive;
	Slist2[0].num_item = 0;
	Slist2[0].item = UU_NULL;
	Slist2[0].answer = UU_NULL;
	Slist2[1].num_item = 0;
	Slist2[1].item = UU_NULL;
	Slist2[1].answer = UU_NULL;
/*
.....Store the machine data
*/
	Smodel = mpt; Ssolid = spt;
	Snmodel = nmodel; Snsolid = nsolid;
	Smix = 0; Ssix = -1;
	S_load_axes(UU_FALSE);
	S_load_solids(UU_FALSE);
/*
.....Set the form defaults
.....based on the first Axis
*/
	Scolor =mpt[0].color;
	Svis = mpt[0].visible;
	Strans = mpt[0].translucency;
	Srev = mpt[0].reverse;
	Sedge = mpt[0].edge;
	Sedgcol = mpt[0].edge_color;
	strcpy(Saxis,mpt[0].axisname);
/*
.....Highlight the selected solids
*/
	S_hilite();
/*
.....Get the Form input
*/
	if (mpt[0].type == LW_MACH_NOTYPE)
		traverse[FAREV] = 0;
	else
		traverse[FAREV] = 1;
	*Sfrm2 = ud_form_display1("ipvmachattr.frm", ans, ans, methods, called,
		display, traverse);
	if (*Sfrm2 != -1) *Sactive2 = UU_TRUE;
	else *Sactive2 = UU_FALSE;
	return;
}

/*********************************************************************
**    S_FUNCTION     :  OnText1(filedno, val, stat)
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
static UD_FSTAT OnText1(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	switch (*fieldno)
	{
/*
.....Tool Length
*/
	case FLTLN:
		Stlen = *val->frmflt;
		break;
/*
.....Tool Length Offset
*/
	case FLOFF:
		Stlofs = *val->frmflt;
		break;
	}
/*
.....Update the list
*/
	UM_len_exttoint(Stlen,Scpt[Stoolpt].tlen);
	UM_len_exttoint(Stlofs,Scpt[Stoolpt].tlofs);
	S_load_list(UU_TRUE);
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnTool(filedno, val, stat)
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

	if (*fieldno != FLLST) return(UD_FLDOK);
/*
.....Save the current settings
*/
	if (Stoolpt >= 0)
	{
		UM_len_exttoint(Stlen,Scpt[Stoolpt].tlen);
		UM_len_exttoint(Stlofs,Scpt[Stoolpt].tlofs);
	}
/*
.....val.frmstr contains the selected string
....."toolnum LOADTL/..."
*/
	if (val->frmstr!=NULL)
	{
		strcpy(Slist.answer, val->frmstr);
		strcpy(temp, val->frmstr);
		tok = strtok(temp, " ");
		if (tok!=NULL)
		{
			Stoolpt = atoi(tok);
		}
/*
.....Set the form fields
*/
		Stoolpt--;
		S_update_form(Stoolpt);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnCompon(filedno, val, stat)
**       Method called when the Component button is pressed.
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
static UD_FSTAT OnCompon(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,j,status;
	UU_LOGICAL found;
	LtSessionPrim prim;
/*
.....Take down the main form
.....while in picking mode
*/
	ud_form_invis();
/*
.....Get the solid component
*/
	do
	{
		status = ul_ipv_pick_solid("Select component for attribute change.",
			&prim);
		if (status != UU_SUCCESS) break;
/*
.....Find the correct component
*/
		found = UU_FALSE;
		for (i=0;i<Snmodel;i++)
		{
			for (j=Smodel[i].beg_solid;j<=Smodel[i].end_solid;j++)
			{
/*
........Found the component
........Now make it the active component
*/
				if (prim == Ssolid[j].stock.stock)
				{
					S_unhilite();
					Smix = i;
					Ssix = j;
					strcpy(Slist2[0].answer,Smodel[i].axisname);
					S_load_solids(UU_TRUE);
					S_fmt_solid(Slist2[1].answer,&Ssolid[j]);
					ud_dispfrm_update_answer(*Sfrm2,FASOL,&Slist2[1]);
					S_update_form2();
					S_hilite();
					found = UU_TRUE;
					break;
				}
			}
			if (found) break;
		}
/*
.....Could not find component
*/
		if (!found)
			ud_wrerr("Machine component not selected.");
	} while (!found);
/*
.....End of routine
.....Redisplay the main form
*/
	ud_form_vis();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnApply2(filedno, val, stat)
**       Method called when the Apply button is pressed.
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
static UD_FSTAT OnApply2(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
/*
.....Save the form fields
*/
	S_save_fields();
/*
.....Modify the stock attributes
........Single solid
*/
	if (LW_active)
	{
		if (Ssix >= 0)
		{
			ul_ipv_modify_stock(&(Ssolid[Ssix].stock),UU_FALSE);
		}
/*
........An axis is selected
*/
		else
		{
			ul_ipv_set_defered();
			for (i=Smodel[Smix].beg_solid;i<=Smodel[Smix].end_solid;i++)
			{
				ul_ipv_modify_stock(&(Ssolid[i].stock),UU_FALSE);
			}
			ul_ipv_set_immediate(UU_TRUE);
		}
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnAxis(filedno, val, stat)
**       Method called when a machine axis is selected from the list.
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
static UD_FSTAT OnAxis(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;

	if (*fieldno != FAAXS) return(UD_FLDOK);
/*
.....Save the current settings
*/
/*	S_save_fields();*/
/*
.....val.frmstr contains the selected string
*/
	if (val->frmstr!=NULL)
	{
		strcpy(Slist2[0].answer, val->frmstr);
		for (i=0;i<Snmodel;i++)
		{
			if (strcmp(val->frmstr,Smodel[i].axisname) == 0)
			{
				S_unhilite();
				Smix = i;
				Ssix = -1;
				S_hilite();
				break;
			}
		}
/*
.....Load the solids for this axis
*/
		S_load_solids(UU_TRUE);
/*
.....Set the form fields
*/
		S_update_form2();
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnSolid(fieldno, val, stat)
**       Method called when a machine solid is selected from the list.
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
static UD_FSTAT OnSolid(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	char *tok,*strtok(),temp[80];

	if (*fieldno != FASOL) return(UD_FLDOK);
/*
.....Save the current settings
*/
/*	S_save_fields();*/
/*
.....val.frmstr contains the selected string
*/
	if (val->frmstr!=NULL)
	{
		strcpy(Slist2[1].answer, val->frmstr);
		strcpy(temp, val->frmstr);
		tok = strtok(temp, " ");
		if (tok != UU_NULL)
		{
			for (i=Smodel[Smix].beg_solid;i<=Smodel[Smix].end_solid;i++)
			{
				if (strcmp(tok,Ssolid[i].name) == 0)
				{
					S_unhilite();
					Ssix = i;
					S_hilite();
					break;
				}
			}
		}
/*
.....Set the form fields
*/
		S_update_form2();
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnClose1(filedno, val, stat)
**       Method called when the Tool Lengths form is closed.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnClose1()
{
/*
.....Save the active form tool
*/

	if (Stoolpt >= 0)
	{
		UM_len_exttoint(Stlen,Scpt[Stoolpt].tlen);
		UM_len_exttoint(Stlofs,Scpt[Stoolpt].tlofs);
	}
/*
.....Free the form list
*/
	ud_free_flist(&Slist);
	*Sactive1 = UU_FALSE;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnClose2(filedno, val, stat)
**       Method called when the Machine Clash Detection form is closed.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnClose2()
{
/*
.....Save the active form settings
*/
/*	S_save_fields();*/
/*
.....Free the form list
*/
	S_unhilite();
	ud_free_flist(&Slist2[0]);
	ud_free_flist(&Slist2[1]);
	*Sactive2 = UU_FALSE;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  S_update_form(tnum)
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
	int t;
/*
.....Find next used tool
.....Start search with specified tool number
*/
	if (tnum < 0) return;
	for (t=tnum;t<Sntl;t++)
	{
		if (Scpt[t].used) break;
	}
/*
.....Update form fields
*/
	if (t<Sntl)
	{
		Stlno = Scpt[t].tlno;
		UM_len_inttoext(Scpt[t].tlen,Stlen);
		UM_len_inttoext(Scpt[t].tlofs,Stlofs);
		ud_dispfrm_update_answer(*Sfrm1,FLTNO,&Stlno);
		ud_dispfrm_update_answer(*Sfrm1,FLTLN,(int *)&Stlen);
		ud_dispfrm_update_answer(*Sfrm1,FLOFF,(int *)&Stlofs);
/*		ud_update_form(*Sfrm1);*/
		Stoolpt = t;
	}
}

/*********************************************************************
**    S_FUNCTION     :  S_update_form2(tnum)
**       Updates the Attributes form fields depending on the selected
**       machine component.
**    PARAMETERS
**       INPUT  :
**          tnum     Currently selected tool.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_update_form2(tnum)
int tnum;
{
/*
.....Machine axis is active
*/
	if (Ssix < 0)
	{
		Scolor = Smodel[Smix].color;
		Svis = Smodel[Smix].visible;
		Strans = Smodel[Smix].translucency;
		Srev = Smodel[Smix].reverse;
		Sedge = Smodel[Smix].edge;
		Sedgcol = Smodel[Smix].edge_color;
		if (Smodel[Smix].type == LW_MACH_NOTYPE)
			ud_setfrm_traverse_mask(*Sfrm2,FAREV,UU_FALSE);
		else
			ud_setfrm_traverse_mask(*Sfrm2,FAREV,UU_TRUE);
		strcpy(Saxis,Slist2[0].answer);
	}
/*
.....Single solid is active
*/
	else
	{
		Scolor = Ssolid[Ssix].stock.color;
		Svis = Ssolid[Ssix].stock.visible;
		if (Svis == -1) Svis = Smodel[Smix].visible;
		Strans = Ssolid[Ssix].stock.translucency;
		if (Strans == -1) Strans = Smodel[Smix].translucency;
		Srev = 0;
		if (Sedge == -1) Sedge = Smodel[Smix].edge;
		else Sedge = Ssolid[Ssix].stock.edge;
		Sedgcol = Ssolid[Ssix].stock.edge_color;
		if (Sedgcol == -1) Sedgcol = Smodel[Smix].edge_color;
		ud_setfrm_traverse_mask(*Sfrm2,FAREV,UU_FALSE);
		strcpy(Saxis,Slist2[1].answer);
	}
/*
.....Update the form fields
*/
	ud_dispfrm_update_answer(*Sfrm2,FACOL,&Scolor);
	ud_dispfrm_update_answer(*Sfrm2,FAVIS,&Svis);
	ud_dispfrm_update_answer(*Sfrm2,FATRA,&Strans);
	ud_dispfrm_update_answer(*Sfrm2,FAREV,&Srev);
	ud_dispfrm_update_answer(*Sfrm2,FAEDG,&Sedge);
	ud_dispfrm_update_answer(*Sfrm2,FAEDC,&Sedgcol);
	ud_dispfrm_update_answer(*Sfrm2,FACOM,&Saxis);
}

/*********************************************************************
**    S_FUNCTION     : S_load_list(chg)
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
	ud_free_flist(&Slist);
	nt = Sntl;
	if (nt == 0) nt = 1;
	Slist.item = (char **)uu_malloc(nt*sizeof(char*));
	Slist.num_item = 0;
	Slist.answer = (char *)uu_malloc(sizeof(char)*80);
/*
.....No tools in list
*/
	if (nt == 0 || Stoolpt == -1)
	{
		Slist.item[Slist.num_item] = (char*)uu_malloc(81*sizeof(char));
		strcpy(Slist.item[0], " ");
		Slist.num_item++;
	}
/*
.....Store tools in form list
*/
	else
	{
		for (i=0;i<Sntl;i++)
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
				Slist.item[Slist.num_item] =
					(char*)uu_malloc(81*sizeof(char));
				strcpy(Slist.item[Slist.num_item], buf);
				Slist.num_item++;
			}
		}
/*
.....Determine default tool
*/
		S_default_tool(Stoolpt,chg);
	}
}

/*********************************************************************
**    S_FUNCTION     : S_load_axes(chg)
**       Loads the machine axis components and stores them
**			into the form list.
**    PARAMETERS
**       INPUT  :   chg   = UU_TRUE if changing existing list.
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_load_axes(chg)
UU_LOGICAL chg;
{
	int i,nt;
/*
.....Initialize list
*/
	ud_free_flist(&Slist2[0]);
	nt = Snmodel;
	if (nt == 0) nt = 1;
	Slist2[0].item = (char **)uu_malloc(nt*sizeof(char*));
	Slist2[0].num_item = 0;
	Slist2[0].answer = (char *)uu_malloc(sizeof(char)*80);
/*
.....Store axes in form list
*/
	for (i=0;i<Snmodel;i++)
	{
		Slist2[0].item[Slist2[0].num_item] =
			(char*)uu_malloc(81*sizeof(char));
		strcpy(Slist2[0].item[Slist2[0].num_item],Smodel[i].axisname);
		Slist2[0].num_item++;
	}
	strcpy(Slist2[0].answer,Smodel[0].axisname);
}

/*********************************************************************
**    S_FUNCTION     : S_load_solids(chg)
**       Loads the machine solid definitions and stores them
**			into the form list.
**    PARAMETERS
**       INPUT  :   chg   = UU_TRUE if changing existing list.
**       OUTPUT :   none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_load_solids(chg)
UU_LOGICAL chg;
{
	int i,nt;
	char buf[80];
/*
.....Initialize list
*/
	ud_free_flist(&Slist2[1]);
	nt = Smodel[Smix].end_solid-Smodel[Smix].beg_solid+1;
	if (nt <= 0) nt = 1;
	Slist2[1].item = (char **)uu_malloc(nt*sizeof(char*));
	Slist2[1].num_item = 0;
	Slist2[1].answer = (char *)uu_malloc(sizeof(char)*80);
/*
.....Store solids in form list
*/
	for (i=Smodel[Smix].beg_solid;i<=Smodel[Smix].end_solid;i++)
	{
		S_fmt_solid(buf,&Ssolid[i]);
		Slist2[1].item[Slist2[1].num_item] =
			(char*)uu_malloc(81*sizeof(char));
		strcpy(Slist2[1].item[Slist2[1].num_item], buf);
		Slist2[1].num_item++;
	}
	strcpy(Slist2[1].answer," ");
/*
.....Update list
*/
	if (chg) ud_dispfrm_update_answer(*Sfrm2,FASOL,&Slist2[1]);
}

/*********************************************************************
**    S_FUNCTION     : S_default_tool(tool,chg)
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
	for (i=t;i<Sntl;i++)
	{
		if (Scpt[i].used)
		{
			S_fmt_tool(i,Slist.answer);
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
				S_fmt_tool(j,Slist.answer);
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
		Stoolpt = -1;
		Slist.answer[0] = '\0';
	}
/*
.....Set form field defaults
*/
	else
	{
		Stoolpt = found;
		Stlno = Scpt[Stoolpt].tlno;
		UM_len_inttoext(Scpt[Stoolpt].tlen,Stlen);
		UM_len_inttoext(Scpt[Stoolpt].tlofs,Stlofs);
/*
.....Update data fields
*/
		if (chg)
		{
			S_update_form(Stoolpt);
			ud_dispfrm_update_answer(*Sfrm1,FLLST,&Slist);
		}
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
	UU_REAL rnum1,rnum2;
/*
.....Format tool data
*/
	UM_len_inttoext(Scpt[tool].tlen,rnum1);
	UM_len_inttoext(Scpt[tool].tlofs,rnum2);
	sprintf(buf,"%04d   LOADTL/%d,LENGTH,%g,OFFSET,%g",tool+1,Scpt[tool].tlno,
		rnum1,rnum2);
}

/*********************************************************************
**    S_FUNCTION     :  S_save_fields()
**       Saves the Attribute fields in the selected axis/solid.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_save_fields()
{
	int i,icol;
/*
.....Save the active form settings
........An individual solid is selected
*/
	if (Ssix >= 0)
	{
		Ssolid[Ssix].stock.color = Scolor;
		Ssolid[Ssix].stock.visible = Svis;
		Ssolid[Ssix].stock.translucency =  Strans;
		Ssolid[Ssix].stock.edge = Sedge;
		Ssolid[Ssix].stock.edge_color = Sedgcol;
	}
/*
........An axis is selected
*/
	else
	{
		icol = Smodel[Smix].color;
		Smodel[Smix].color = Scolor;
		Smodel[Smix].visible = Svis;
		Smodel[Smix].translucency = Strans;
		Smodel[Smix].reverse = Srev;
		Smodel[Smix].edge = Sedge;
		Smodel[Smix].edge_color = Sedgcol;
		for (i=Smodel[Smix].beg_solid;i<=Smodel[Smix].end_solid;i++)
		{
/*			if (Ssolid[i].color == icol)*/
			Ssolid[i].stock.color = Scolor;
			Ssolid[i].stock.visible = Svis;
			Ssolid[i].stock.translucency = Strans;
			Ssolid[i].stock.edge = Sedge;
			Ssolid[i].stock.edge_color = Sedgcol;
		}
	}
}

/*********************************************************************
**    S_FUNCTION     :  S_hilite()
**       Highlights the currently selected machine solids.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_hilite()
{
	int i;
	LtMaterial omat;
	LtBoolean edge;
	LtColour ecolor;
/*
.....Save the active form settings
........An individual solid is selected
*/
	if (LW_active)
	{
		if (Ssix >= 0)
		{
			ul_ipv_highlight_entity(&omat,Ssolid[Ssix].stock.stock,
				LI_ENTITY_TYPE_SOLID,LW_material[LW_highlight_color],&edge,ecolor);
		}
/*
........An axis is selected
*/
		else
		{
			ul_ipv_set_defered();
			for (i=Smodel[Smix].beg_solid;i<=Smodel[Smix].end_solid;i++)
			{
				ul_ipv_highlight_entity(&omat,Ssolid[i].stock.stock,
					LI_ENTITY_TYPE_SOLID,LW_material[LW_highlight_color],&edge,
					ecolor);
			}
			ul_ipv_set_immediate(UU_TRUE);
		}
	}
}

/*********************************************************************
**    S_FUNCTION     :  S_unhilite()
**       Unhighlights the currently selected machine solids.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_unhilite()
{
	int i,icol;
	LtMaterial omat;
	LtColour color;
/*
.....Save the active form settings
........An individual solid is selected
*/
	if (LW_active)
	{
		if (Ssix >= 0)
		{
			if ((Ssolid[Ssix].stock.edge_color == -1)||(Ssolid[Ssix].stock.edge_color == 64))
				icol = Ssolid[Ssix].stock.color;
			else 
				icol = Ssolid[Ssix].stock.edge_color;
			ul_ipv_color_set(color,icol);
			ul_ipv_highlight_entity(&omat,Ssolid[Ssix].stock.stock,
				LI_ENTITY_TYPE_SOLID,-1,&Ssolid[Ssix].stock.edge,color);
		}
/*
........An axis is selected
*/
		else
		{
			ul_ipv_set_defered();
			for (i=Smodel[Smix].beg_solid;i<=Smodel[Smix].end_solid;i++)
			{
			if ((Ssolid[i].stock.edge_color == -1)||(Ssolid[i].stock.edge_color == 64))
				icol = Ssolid[i].stock.color;
			else 
				icol = Ssolid[i].stock.edge_color;
			ul_ipv_color_set(color,icol);
				ul_ipv_highlight_entity(&omat,Ssolid[i].stock.stock,
					LI_ENTITY_TYPE_SOLID,-1,&Ssolid[i].stock.edge,color);
			}
			ul_ipv_set_immediate(UU_TRUE);
		}
	}
}

/*********************************************************************
**    S_FUNCTION     : S_fmt_solid(buf,solid)
**       Formats an individual machine solid for list output.
**    PARAMETERS
**       INPUT  :   solid  = Solid to format for output.
**       OUTPUT :   buf    = Formatted solid string.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_fmt_solid(buf,solid)
LW_mach_solid_struc *solid;
char *buf;
{
	static char *stype[] = {"Box","Revolve","Sweep","Session","Stl","Cone",
		"Cylinder", "Sphere", "Torus", "None"};
/*
.....Format solid for output
*/
	sprintf(buf,"%s  -  %s",solid->name,stype[solid->stock.type]);
}

/*********************************************************************
**    E_FUNCTION     : ul_ipv_spindle_vis(ix,vis)
**       Sets a machine spindle visibility.
**    PARAMETERS
**       INPUT  :   ix     = Spindle to set visibility for.
**       OUTPUT :   vis    = UU_FALSE = Invisible, UU_TRUE = Visible.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ul_ipv_spindle_vis(ix,vis)
int ix;
UU_LOGICAL vis;
{
	int i,six;
	LW_mach_model_struc *mpt;
	LW_mach_solid_struc *spt;
	LtData data;
/*
.....Make sure spindle exists
*/
	if (LW_spindle[ix] != -1)
	{
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
		six = LW_spindle[ix];
		mpt[six].visible = vis;
		ul_ipv_set_defered();
		LiDataSetBoolean(&data,vis);
		for (i=mpt[six].beg_solid;i<=mpt[six].end_solid;i++)
		{
			spt[i].stock.visible = vis;
			LiMWViewportSetSessPrimProperty(LW_viewport,spt[i].stock.stock,
				LI_VPSP_PROP_MW_VISIBLE,&data);
		}
		ul_ipv_set_immediate(UU_TRUE);
	}
}
