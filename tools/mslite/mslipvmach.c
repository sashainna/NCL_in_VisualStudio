/*********************************************************************
**   FILENAME: mslipvmach.c
**   CONTAINS: 
**		msl_load_machin
**		msl_place_axes()
**		msl_mach_attrfrm
**		msl_mach_attr
**		msl_simulate()
**
**     COPYRIGHT 2008 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       mslipvmach.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:12:59
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "lcom.h"
#include "mdcpln.h"
#include "mfort.h"
#include "nclfc.h"
#include "nccs.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "lipvmach.h"

#define NFORMS 5

static UU_LOGICAL Sactive=UU_FALSE;
static int Sfrm=-1;
static UX_pathname Sdir,Sfulldir,Smname;
static UU_LIST Smodel,Ssolid,Stpin;
static int Snmodel,Snsolid,Sspindle[10],Spinaxis;
static int Stpix,Sntpin;
static LW_mach_data_struc Sdesc;
static UM_coord Smpin[3],Sppin[3];
static char Sapin[80];
static UU_REAL Spos[10],Soffset[10];
static UU_REAL Delta[10];
static UU_REAL Smax[10], Smin[10];
static LW_mach_toolpin_struc *Stptr;

static void S_load_list();
static void S_place_stock();
static void S_store_pos();

static UD_LIST Slist[2];
static int Smix,Ssix,Scolor,Svis,Strans,Srev,Sedge,Sedgcol;
static int Snmodel_a,Snsolid_a;
static char Saxis[80];
static LW_mach_model_struc *Smodel_a;
static LW_mach_solid_struc *Ssolid_a;
static UD_FSTAT OnApply();
static UD_FSTAT OnAxis();
static UD_FSTAT OnCompon();
static UD_FSTAT OnSolid();
static void S_update_form();

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

#define EDIT1	0
#define EDIT2	7
#define EDIT3	14
#define EDIT4	21
#define EDIT5	28
#define EDIT6	35
#define EDIT7	42
#define EDIT8	49
#define EDIT9	56
#define EDIT10	63

#define GOEDIT1	4
#define GOEDIT2	11
#define GOEDIT3	18
#define GOEDIT4	25
#define GOEDIT5	32
#define GOEDIT6	39
#define GOEDIT7	46
#define GOEDIT8	53
#define GOEDIT9	60
#define GOEDIT10	67

#define PLUS1	1
#define MINUS1	2
#define GOTO1	3
#define LIPLUS1	5
#define LIMINUS1	6

#define	PLUS2	8
#define MINUS2	9
#define GOTO2	10
#define LIPLUS2	12
#define LIMINUS2	13

#define PLUS3	15
#define MINUS3	16
#define GOTO3	17
#define LIPLUS3	19
#define LIMINUS3	20

#define PLUS4	22
#define MINUS4	23
#define GOTO4	24
#define LIPLUS4	26
#define LIMINUS4	27

#define PLUS5	29
#define MINUS5	30
#define GOTO5	31
#define LIPLUS5	33
#define LIMINUS5	34

#define PLUS6	36
#define MINUS6	37
#define GOTO6	38
#define LIPLUS6	40
#define LIMINUS6	41

#define PLUS7	43
#define MINUS7	44
#define GOTO7	45
#define LIPLUS7	47
#define LIMINUS7	48

#define PLUS8	50
#define MINUS8	51
#define GOTO8	52
#define LIPLUS8	54
#define LIMINUS8	55

#define PLUS9	57
#define MINUS9	58
#define GOTO9	59
#define LIPLUS9	61
#define LIMINUS9	62

#define PLUS10	64
#define MINUS10	65
#define GOTO10	66
#define LIPLUS10	68
#define LIMINUS10	69

UX_pathname MSL_init_mach;
char *frm_ptr;
	
static char *d_called,*d_traverse,*d_display;
static UD_METHOD *d_methods;
static int (*(*d_ans_ptr));
void ul_ipv_diag_handler();
int MS_failed = 0;
/*********************************************************************
**    S_FUNCTION     :  msl_load_machin()
**       Routine to get load the Machine
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void msl_load_machin()
{
	char sbuf[80],title[80];
	UX_pathname fullname,fname;
	int i, j, nc, status;
	static int first = 1;
	UU_LOGICAL ifl,um_cceqcc();
	LW_mach_toolpin_struc *tptr;
/*
......Get Machine directory, if the first time get here,
......it is from initial view, if the command line accept
......an input machine name, we load it, if not, simply return
*/
	if ((first)&&(MSL_init_mach[0]!='\0'))
	{
		strcpy(fullname, MSL_init_mach);
		ul_break_fname(fullname, Sdir, fname);
		if (Sdir[0]=='\0')
			ul_getvalid_fulldir("UL_NCLIPV_MACHINES", Sdir);
		strcpy(Smname, fname);
	}
	else if (first)
	{
		first = 0;
		return;
	}
	else
	{
		strcpy(title,"Machine Description");
		ul_getvalid_fulldir("UL_NCLIPV_MACHINES",fullname);
		nc = strlen(fullname);
		uw_ntget_dirname(NULL, title, fullname, &nc);
		ul_break_fname(fullname,Sdir,fname);
		strcpy(Smname, fname);
	}
	uu_move_byte(&LW_mach_data,&Sdesc,sizeof(LW_mach_data_struc));
	uu_list_init(&Smodel,sizeof(LW_mach_model_struc),10,10);
	uu_list_init(&Ssolid,sizeof(LW_mach_solid_struc),50,20);
	uu_list_init0(&Stpin);
	Snmodel = 0;
	Snsolid = 0;
	Stptr = (LW_mach_toolpin_struc *)UU_LIST_ARRAY(&LW_mach_toolpin);
	for (i=0;i<3;i++)
	{
		um_vctovc(Stptr[Stpix].mpin[i],Smpin[0]);
		um_vctovc(Stptr[Stpix].ppin[i],Sppin[0]);
	}

	if ( (strcmp(LW_mach_name,Smname) || !LW_mach_defined))
	{
		LW_mach_simul = 1;
		if (strcmp(LW_mach_name,Smname))
			status = ul_ipv_load_mach(Sdir,Smname,Sfulldir,&Sdesc,&Smodel,
						&Ssolid,&Snmodel,&Snsolid,Sspindle,&Stpin,&Sntpin);
		if (status == UU_SUCCESS)
		{
			if (Snmodel > 0)
			{
				ul_ipv_free_mach(&LW_mach_model,&LW_mach_nmodel,
					&LW_mach_solid,&LW_mach_nsolid,&LW_mach_toolpin,
					&LW_mach_num_tpin);
				uu_move_byte(&Smodel,&LW_mach_model,sizeof(UU_LIST));
				uu_move_byte(&Ssolid,&LW_mach_solid,sizeof(UU_LIST));
				uu_move_byte(&Sdesc,&LW_mach_data,sizeof(LW_mach_data_struc));
				LW_mach_nmodel = Snmodel;
				LW_mach_nsolid = Snsolid;
			}
			uu_move_byte(&Stpin,&LW_mach_toolpin,sizeof(UU_LIST));
			LW_mach_num_tpin = Sntpin;
			if (LW_session[0] != 0) ul_ipv_reset_session();
		}
		else LW_mach_simul = UU_FALSE;
	}
	if (status == UU_SUCCESS)
	{
		LW_mach_simul = 1;
		strcpy(LW_mach_name,Smname);
		strcpy(LW_mach_dir,Sdir);
/*
........Place stock if necessary
*/
		ifl = UU_FALSE;
		tptr = (LW_mach_toolpin_struc *)UU_LIST_ARRAY(&LW_mach_toolpin);
		for (i=0;i<LW_mach_num_tpin;i++)
		{
			for (j=0;j<3;j++)
			{
				if (!um_cceqcc(Stptr[i].mpin[j],tptr[i].mpin[j]) ||
					!um_cceqcc(Stptr[i].ppin[j],tptr[i].ppin[j]))
				{
					ifl = UU_TRUE;
					break;
				}
			}
		}
		if (ifl) S_place_stock(UU_FALSE);
/*
........Store axes positions
*/
		S_store_pos(0);
	}
/*
.....Free local storage
*/
	if (status != UU_SUCCESS)
	{
		ul_ipv_free_mach(&Smodel,&Snmodel,&Ssolid,&Snsolid,&Stpin,&Sntpin);
	}
	else
	{
/*
.....Set diagnostic Callback
*/
		LiCallBackSet(LI_CALL_DIAGNOSTIC,(LtFunc)ul_ipv_diag_handler);
		ul_ipv_start_session(UU_TRUE);
		msl_open_window();
		LW_active = TRUE;
		MS_failed = 0;
	}
	if ((status != UU_SUCCESS) && (first))
		MS_failed = 1;
	first = 0;
}
/*********************************************************************
**	 I_FUNCTION : S_place_stock(resfl)
**			This function places the stocks and fixtures onto the machine.
**	 PARAMETERS	
**		 INPUT  :
**         resfl   = UU_TRUE = Reset stock to original position.
**                   UU_FALSE = Place stock on machine.
**		 OUTPUT :
**         none
**	 RETURNS: None
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
static void S_place_stock(resfl)
UU_LOGICAL resfl;
{
	int i,j;
	LW_mach_toolpin_struc *tptr;
/*
.....Set the Tooling pin locations
*/
	if (!resfl)
	{
		for (j=0;j<3;j++)
		{
			um_vctovc(Smpin[j],Stptr[Stpix].mpin[j]);
			um_vctovc(Sppin[j],Stptr[Stpix].ppin[j]);
		}
		tptr = (LW_mach_toolpin_struc *)UU_LIST_ARRAY(&LW_mach_toolpin);
		for (i=0;i<LW_mach_num_tpin;i++)
		{
			for (j=0;j<3;j++)
			{
				um_vctovc(Stptr[i].mpin[j],tptr[i].mpin[j]);
				um_vctovc(Stptr[i].ppin[j],tptr[i].ppin[j]);
			}
		}
	}
/*
.....Place the stock on the machine
*/
	ul_ipv_place_stock(UU_NULL,resfl);
/*
.....End of routine
*/
	return;
}

/*********************************************************************
**	 I_FUNCTION : msl_place_axes()
**			This function places the machine axes at their predefined
**       position.
**	 PARAMETERS	
**		 INPUT  :
**         none
**		 OUTPUT :
**         none
**	 RETURNS: None
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
void msl_place_axes()
{
	int i,ipt;
	UU_REAL posit[10];
	LW_mach_model_struc *mpt;
	if (LW_mach_naxes > 0)
	{
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		ipt = 0;
		for (i=0;i<10;i++)
		{
			if (LW_mach_axes[i] != -1)
				posit[i] = mpt[LW_mach_axes[i]].position;
		}
		ul_ipv_move_assemblies(posit,UU_TRUE,UU_FALSE);
	}
}

/*********************************************************************
**	 I_FUNCTION : S_store_pos(ifl)
**			This function copies the machine axes position from the local
**       array (Spos) to the global array (LW_mach_model.position) or
**       vice versa.  It also copies the machine offset positions.
**	 PARAMETERS	
**		 INPUT  :
**         ifl    = 0 - Copy from Spos to LW_mach_model.
**                  1 - Copy from LW_mach_model to Spos.
**		 OUTPUT :
**         none
**	 RETURNS: None
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
static void S_store_pos(ifl)
int ifl;
{
	LW_mach_model_struc *mpt;
	int i,ipt,inc;
/*
.....Store the global axis positions
.....in the local array
*/
	if (LW_mach_naxes > 0)
	{
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		ipt = 0;
		for (i=0;i<10;i++)
		{
			if (LW_mach_axes[i] != -1)
			{
				inc = LW_mach_axes[i];
				if (ifl == 0)
				{
					if (mpt[LW_mach_axes[i]].type == LW_MACH_LINEAR)
					{
						UM_len_exttoint(Spos[ipt],mpt[inc].position);
						UM_len_exttoint(Soffset[ipt],mpt[inc].offset);
					}
					else
					{
						mpt[inc].position = Spos[ipt];
						mpt[inc].offset = Soffset[ipt];
					}
				}
				else
				{
					if (mpt[LW_mach_axes[i]].type == LW_MACH_LINEAR)
					{
						UM_len_inttoext(mpt[inc].position,Spos[ipt]);
						UM_len_inttoext(mpt[inc].offset,Soffset[ipt]);
					}
					else
					{
						Spos[ipt] = mpt[inc].position;
						Soffset[ipt] = mpt[inc].offset;
					}
				}
				ipt++;
			}
		}
/*
.....Physically position the axes
*/
		if (ifl == 0) 
		{
			msl_place_axes();
		}
	}
	else if (ifl == 1)
	{
		for (i=0;i<10;i++) Spos[i] = Soffset[i] = 0.;
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
	ud_free_flist(&Slist[0]);
	nt = Snmodel_a;
	if (nt == 0) nt = 1;
	Slist[0].item = (char **)uu_malloc(nt*sizeof(char*));
	Slist[0].num_item = 0;
	Slist[0].answer = (char *)uu_malloc(sizeof(char)*80);
/*
.....Store axes in form list
*/
	for (i=0;i<Snmodel_a;i++)
	{
		Slist[0].item[Slist[0].num_item] =
			(char*)uu_malloc(81*sizeof(char));
		strcpy(Slist[0].item[Slist[0].num_item],Smodel_a[i].axisname);
		Slist[0].num_item++;
	}
	strcpy(Slist[0].answer,Smodel_a[0].axisname);
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
	static char *stype[] = {"Box","Cylinder","Cone","Sphere","Revolve","Sweep",
		"Stl","None"};
/*
.....Format solid for output
*/
	sprintf(buf,"%s  -  %s",solid->name,stype[solid->stock.type]);
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
		Ssolid_a[Ssix].stock.color = Scolor;
		Ssolid_a[Ssix].stock.visible = Svis;
		Ssolid_a[Ssix].stock.translucency = Strans;
		Ssolid_a[Ssix].stock.edge = Sedge;
		Ssolid_a[Ssix].stock.edge_color = Sedgcol;
	}
/*
........An axis is selected
*/
	else
	{
		icol = Smodel_a[Smix].color;
		Smodel_a[Smix].color = Scolor;
		Smodel_a[Smix].visible = Svis;
		Smodel_a[Smix].translucency = Strans;
		Smodel_a[Smix].reverse = Srev;
		Smodel_a[Smix].edge = Sedge;
		Smodel_a[Smix].edge_color = Sedgcol;
		for (i=Smodel_a[Smix].beg_solid;i<=Smodel_a[Smix].end_solid;i++)
		{
/*			if (Ssolid[i].color == icol)*/
			Ssolid_a[i].stock.color = Scolor;
			Ssolid_a[i].stock.visible = Svis;
			Ssolid_a[i].stock.translucency = Strans;
			Ssolid_a[i].stock.edge = Sedge;
			Ssolid_a[i].stock.edge_color = Sedgcol;
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
			ul_ipv_highlight_entity(&omat,Ssolid_a[Ssix].stock.stock,
				LI_ENTITY_TYPE_SOLID,LW_material[LW_highlight_color],&edge,ecolor);
		}
/*
........An axis is selected
*/
		else
		{
			ul_ipv_set_defered();
			for (i=Smodel_a[Smix].beg_solid;i<=Smodel_a[Smix].end_solid;i++)
			{
				ul_ipv_highlight_entity(&omat,Ssolid_a[i].stock.stock,
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
			if ((Ssolid_a[Ssix].stock.edge_color == -1)||(Ssolid_a[Ssix].stock.edge_color == 64))
				icol = Ssolid_a[Ssix].stock.color;
			else icol = Ssolid_a[Ssix].stock.edge_color;
			ul_ipv_color_set(color,icol);
			ul_ipv_highlight_entity(&omat,Ssolid_a[Ssix].stock.stock,
				LI_ENTITY_TYPE_SOLID,-1,&Ssolid_a[Ssix].stock.edge,color);
		}
/*
........An axis is selected
*/
		else
		{
			ul_ipv_set_defered();
			for (i=Smodel_a[Smix].beg_solid;i<=Smodel_a[Smix].end_solid;i++)
			{
			if ((Ssolid_a[i].stock.edge_color == -1)||(Ssolid_a[i].stock.edge_color == 64))
				icol = Ssolid_a[i].stock.color;
			else 
				icol = Ssolid_a[i].stock.edge_color;
			ul_ipv_color_set(color,icol);
				ul_ipv_highlight_entity(&omat,Ssolid_a[i].stock.stock,
					LI_ENTITY_TYPE_SOLID,-1,&Ssolid_a[i].stock.edge,color);
			}
			ul_ipv_set_immediate(UU_TRUE);
		}
	}
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
	ud_free_flist(&Slist[1]);
	nt = Smodel_a[Smix].end_solid-Smodel_a[Smix].beg_solid+1;
	if (nt <= 0) nt = 1;
	Slist[1].item = (char **)uu_malloc(nt*sizeof(char*));
	Slist[1].num_item = 0;
	Slist[1].answer = (char *)uu_malloc(sizeof(char)*80);
/*
.....Store solids in form list
*/
	for (i=Smodel_a[Smix].beg_solid;i<=Smodel_a[Smix].end_solid;i++)
	{
		S_fmt_solid(buf,&Ssolid_a[i]);
		Slist[1].item[Slist[1].num_item] =
			(char*)uu_malloc(81*sizeof(char));
		strcpy(Slist[1].item[Slist[1].num_item], buf);
		Slist[1].num_item++;
	}
	strcpy(Slist[1].answer," ");
/*
.....Update list
*/
	if (chg) ud_update_answer(10,&Slist[1]);
}

/*********************************************************************
**    S_FUNCTION     :  msl_mach_attrfrm()
**       Brings up the Machine Attributes form.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void msl_mach_attrfrm()
{
	void msl_mach_attr();
	LW_mach_model_struc *mpt;
	LW_mach_solid_struc *spt;
	if (LW_mach_naxes < 1)
	{
		ud_wrerr("A machine has yet to be loaded");
		return;
	}
/*
.....Display the Machine Attributes form
*/
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
	spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
	msl_mach_attr(mpt,spt,LW_mach_nmodel,LW_mach_nsolid);
}

/*********************************************************************
**    E_FUNCTION     : msl_mach_attr(mpt,spt,nmodel,nsolid)
**		This function is little different from NCL ul_ipv_mach_attr
**			because the MSLITE form will be modal, so it handled little different
**       Processes the Machine Component Attributes form.
**    PARAMETERS
**       INPUT  :
**          mpt      = Pointer to Machine Model structure.
**          spt      = Pointer to Machine Solid strucutre.
**          nmodel   = Number of axes defined in 'mpt'.
**          nsolid   = Number of solids defined in 'mpt'.
**       OUTPUT :
**          None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void msl_mach_attr(mpt,spt,nmodel,nsolid)
LW_mach_model_struc *mpt;
LW_mach_solid_struc *spt;
int nmodel,nsolid;
{
	int status;
/*
.....Set up form fields
*/
	static char traverse[]     = {1,1,1,1,1,1,1,1,1,1,1};
	static UD_METHOD methods[] = {UU_NULL,UU_NULL,UU_NULL,UU_NULL,UU_NULL,
		UU_NULL,OnCompon,UU_NULL,OnApply,OnAxis,OnSolid};
	static char called[] = {6,6,6,6,6,6,6,6,6,6,6};
	static char display[] = {1,1,1,1,1,1,1,1,1,1,1};
	static int *ans[] = {&Scolor,&Svis,&Strans,&Srev,&Sedge,&Sedgcol,UU_NULL,
		(int *)&Saxis,UU_NULL,(int *)&Slist[0],(int *)&Slist[1]};
/*
.....Initialize routine
*/
	Slist[0].num_item = 0;
	Slist[0].item = UU_NULL;
	Slist[0].answer = UU_NULL;
	Slist[1].num_item = 0;
	Slist[1].item = UU_NULL;
	Slist[1].answer = UU_NULL;
/*
.....Store the machine data
*/
	Smodel_a = mpt; Ssolid_a = spt;
	Snmodel_a = nmodel; Snsolid_a = nsolid;
	Smix = 0; Ssix = -1;
	S_load_axes(UU_FALSE);
	S_load_solids(UU_FALSE);
/*
.....Set the form defaults
.....based on the first Axis
*/
	Scolor = mpt[0].color;
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
		traverse[3] = 0;
	else
		traverse[3] = 1;
	status = ud_form1("ipvmachattr.frm", ans, ans, methods, called,
		display, traverse);
	if (status==-1) goto done;
/*
......save
*/
	OnApply(NULL, NULL, 0);
done:
/*
.....Free the form list
*/
	S_unhilite();
	ud_free_flist(&Slist[0]);
	ud_free_flist(&Slist[1]);
	return;
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
		for (i=0;i<Snmodel_a;i++)
		{
			for (j=Smodel_a[i].beg_solid;j<=Smodel_a[i].end_solid;j++)
			{
/*
........Found the component
........Now make it the active component
*/
				if (prim == Ssolid_a[j].stock.stock)
				{
					S_unhilite();
					Smix = i;
					Ssix = j;
					strcpy(Slist[0].answer, Smodel_a[i].axisname);
					S_load_solids(UU_TRUE);
					S_fmt_solid(Slist[1].answer,&Ssolid_a[j]);
					ud_update_answer(10,&Slist[1]);
					S_update_form();
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
**    S_FUNCTION     :  OnApply(filedno, val, stat)
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
static UD_FSTAT OnApply(fieldno, val, stat)
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
			ul_ipv_modify_stock(&(Ssolid_a[Ssix].stock));
		}
/*
........An axis is selected
*/
		else
		{
			ul_ipv_set_defered();
			for (i=Smodel_a[Smix].beg_solid;i<=Smodel_a[Smix].end_solid;i++)
			{
				ul_ipv_modify_stock(&(Ssolid_a[i].stock));
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

	if (*fieldno != 9) return(UD_FLDOK);
/*
.....val.frmstr contains the selected string
*/
	if (val->frmstr!=NULL)
	{
		strcpy(Slist[0].answer, val->frmstr);
		for (i=0;i<Snmodel;i++)
		{
			if (strcmp(val->frmstr,Smodel_a[i].axisname) == 0)
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
		S_update_form();
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

	if (*fieldno != 10) return(UD_FLDOK);
/*
.....val.frmstr contains the selected string
*/
	if ((val->frmstr!=NULL)||(val->frmstr[0]='\0'))
	{
		strcpy(Slist[1].answer, val->frmstr);
		strcpy(temp, val->frmstr);
		tok = strtok(temp, " ");
		if (tok != UU_NULL)
		{
			for (i=Smodel_a[Smix].beg_solid;i<=Smodel_a[Smix].end_solid;i++)
			{
				if (strcmp(tok,Ssolid_a[i].name) == 0)
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
		S_update_form();
	}
	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     :  OnLimit(filedno, val, stat)
**       Method called when the buttons on the simulation form is pressed.
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
static UD_FSTAT OnLimit(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	char tempval[256];
	UD_DDATA data;
	UU_REAL temp;
	LW_mach_model_struc *mpt;
	int i, cont, limit = 1;

	data.frmstr = (char*)&tempval;
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
	for (i=0; i<LW_mach_naxes;i++)
	{
		Smax[i] = mpt[LW_mach_axes[i]].parms[0];
		Smin[i] = mpt[LW_mach_axes[i]].parms[1];
	}
	switch (*fieldno)
	{
	case PLUS1:
	case MINUS1:
/*
.....Get the Spos[0] and Delta[0]
*/
		ud_getfrm_field(Sfrm, EDIT1, data);
		Delta[0] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT1, data);
		Spos[0] = atof(tempval);
		if (*fieldno==PLUS1)
			Spos[0] = Spos[0] + Delta[0];
		if (*fieldno==MINUS1)
			Spos[0] = Spos[0] - Delta[0];
		if (Smax[0]>Smin[0])
		{
			if ((Spos[0]>Smax[0])||(Spos[0]<Smin[0]))
				goto done;
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT1, &Spos[0]);
		limit = 0;
		break;
	case PLUS2:
	case MINUS2:
/*
.....Get the Spos[1] and Delta[1]
*/
		ud_getfrm_field(Sfrm, EDIT2, data);
		Delta[1] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT2, data);
		Spos[1] = atof(tempval);
		if (*fieldno==PLUS2)
			Spos[1] = Spos[1] + Delta[1];
		if (*fieldno==MINUS2)
			Spos[1] = Spos[1] - Delta[1];
		if (Smax[1]>Smin[1])
		{
			if ((Spos[1]>Smax[1])||(Spos[1]<Smin[1]))
				goto done;
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT2, &Spos[1]);
		limit = 0;
		break;
	case PLUS3:
	case MINUS3:
/*
.....Get the Spos[2] and Delta[2]
*/
		ud_getfrm_field(Sfrm, EDIT3, data);
		Delta[2] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT3, data);
		Spos[2] = atof(tempval);
		if (*fieldno==PLUS3)
			Spos[2] = Spos[2] + Delta[2];
		if (*fieldno==MINUS3)
			Spos[2] = Spos[2] - Delta[2];
		if (Smax[2]>Smin[2])
		{
			if ((Spos[2]>Smax[2])||(Spos[2]<Smin[2]))
				goto done;
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT3, &Spos[2]);
		limit = 0;
		break;
	case PLUS4:
	case MINUS4:
/*
.....Get the Spos[3] and Delta[3]
*/
		ud_getfrm_field(Sfrm, EDIT4, data);
		Delta[3] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT4, data);
		Spos[3] = atof(tempval);
		if (*fieldno==PLUS4)
			Spos[3] = Spos[3] + Delta[3];
		if (*fieldno==MINUS4)
			Spos[3] = Spos[3] - Delta[3];
		if (Smax[3]>Smin[3])
		{
			if ((Spos[3]>Smax[3])||(Spos[3]<Smin[3]))
				goto done;
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT4, &Spos[3]);
		limit = 0;
		break;
	case PLUS5:
	case MINUS5:
/*
.....Get the Spos[4] and Delta[4]
*/
		ud_getfrm_field(Sfrm, EDIT5, data);
		Delta[4] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT5, data);
		Spos[4] = atof(tempval);
		if (*fieldno==PLUS5)
			Spos[4] = Spos[4] + Delta[4];
		if (*fieldno==MINUS5)
			Spos[4] = Spos[4] - Delta[4];
		if (Smax[4]>Smin[4])
		{
			if ((Spos[4]>Smax[4])||(Spos[4]<Smin[4]))
				goto done;
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT5, &Spos[4]);
		limit = 0;
		break;
	case PLUS6:
	case MINUS6:
/*
.....Get the Spos[5] and Delta[5]
*/
		ud_getfrm_field(Sfrm, EDIT6, data);
		Delta[5] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT6, data);
		Spos[5] = atof(tempval);
		if (*fieldno==PLUS6)
			Spos[5] = Spos[5] + Delta[5];
		if (*fieldno==MINUS6)
			Spos[5] = Spos[5] - Delta[5];
		if (Smax[5]>Smin[5])
		{
			if ((Spos[5]>Smax[5])||(Spos[5]<Smin[5]))
				goto done;
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT6, &Spos[5]);
		limit = 0;
		break;
	case PLUS7:
	case MINUS7:
/*
.....Get the Spos[6] and Delta[6]
*/
		ud_getfrm_field(Sfrm, EDIT7, data);
		Delta[6] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT7, data);
		Spos[6] = atof(tempval);
		if (*fieldno==PLUS7)
			Spos[6] = Spos[6] + Delta[6];
		if (*fieldno==MINUS7)
			Spos[6] = Spos[6] - Delta[6];
		if (Smax[6]>Smin[6])
		{
			if ((Spos[6]>Smax[6])||(Spos[6]<Smin[6]))
				goto done;
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT7, &Spos[6]);
		limit = 0;
		break;
	case PLUS8:
	case MINUS8:
/*
.....Get the Spos[7] and Delta[7]
*/
		ud_getfrm_field(Sfrm, EDIT8, data);
		Delta[7] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT8, data);
		Spos[7] = atof(tempval);
		if (*fieldno==PLUS8)
			Spos[7] = Spos[7] + Delta[7];
		if (*fieldno==MINUS8)
			Spos[7] = Spos[7] - Delta[7];
		if (Smax[7]>Smin[7])
		{
			if ((Spos[7]>Smax[7])||(Spos[7]<Smin[7]))
				goto done;
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT8, &Spos[7]);
		limit = 0;
		break;
	case PLUS9:
	case MINUS9:
/*
.....Get the Spos[8] and Delta[8]
*/
		ud_getfrm_field(Sfrm, EDIT9, data);
		Delta[8] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT9, data);
		Spos[8] = atof(tempval);
		if (*fieldno==PLUS9)
			Spos[8] = Spos[8] + Delta[8];
		if (*fieldno==MINUS9)
			Spos[8] = Spos[8] - Delta[8];
		if (Smax[8]>Smin[8])
		{
			if ((Spos[8]>Smax[8])||(Spos[8]<Smin[8]))
				goto done;
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT9, &Spos[8]);
		limit = 0;
		break;
	case PLUS10:
	case MINUS10:
/*
.....Get the Spos[9] and Delta[9]
*/
		ud_getfrm_field(Sfrm, EDIT10, data);
		Delta[9] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT10, data);
		Spos[9] = atof(tempval);
		if (*fieldno==PLUS10)
			Spos[9] = Spos[9] + Delta[9];
		if (*fieldno==MINUS10)
			Spos[9] = Spos[9] - Delta[9];
		if (Smax[9]>Smin[9])
		{
			if ((Spos[9]>Smax[9])||(Spos[9]<Smin[9]))
				goto done;
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT10, &Spos[9]);
		limit = 0;
		break;
	case GOTO1:
		ud_getfrm_field(Sfrm, GOEDIT1, data);
		Spos[0] = atof(tempval);
		if (Smax[0]>Smin[0])
		{
			if (Spos[0]>Smax[0])
			{
				Spos[0] = Smax[0];
				ud_dispfrm_update_answer(Sfrm, GOEDIT1, &Spos[0]);
			}
			if (Spos[0]<Smin[0])
			{
				Spos[0] = Smin[0];
				ud_dispfrm_update_answer(Sfrm, GOEDIT1, &Spos[0]);
			}
		}
		limit = 0;
		break;
	case GOTO2:
		ud_getfrm_field(Sfrm, GOEDIT2, data);
		Spos[1] = atof(tempval);
		if (Smax[1]>Smin[1])
		{
			if (Spos[1]>Smax[1])
			{
				Spos[1] = Smax[1];
				ud_dispfrm_update_answer(Sfrm, GOEDIT2, &Spos[1]);
			}
			if (Spos[1]<Smin[1])
			{
				Spos[1] = Smin[1];
				ud_dispfrm_update_answer(Sfrm, GOEDIT2, &Spos[1]);
			}
		}
		limit = 0;
		break;
	case GOTO3:
		ud_getfrm_field(Sfrm, GOEDIT3, data);
		Spos[2] = atof(tempval);
		if (Smax[2]>Smin[2])
		{
			if (Spos[2]>Smax[2])
			{
				Spos[2] = Smax[2];
				ud_dispfrm_update_answer(Sfrm, GOEDIT3, &Spos[2]);
			}
			if (Spos[2]<Smin[2])
			{
				Spos[2] = Smin[2];
				ud_dispfrm_update_answer(Sfrm, GOEDIT3, &Spos[2]);
			}
		}
		limit = 0;
		break;
	case GOTO4:
		ud_getfrm_field(Sfrm, GOEDIT4, data);
		Spos[3] = atof(tempval);
		if (Smax[3]>Smin[3])
		{
			if (Spos[3]>Smax[3])
			{
				Spos[3] = Smax[3];
				ud_dispfrm_update_answer(Sfrm, GOEDIT4, &Spos[3]);
			}
			if (Spos[3]<Smin[3])
			{
				Spos[3] = Smin[3];
				ud_dispfrm_update_answer(Sfrm, GOEDIT4, &Spos[3]);
			}
		}
		limit = 0;
		break;
	case GOTO5:
		ud_getfrm_field(Sfrm, GOEDIT5, data);
		Spos[4] = atof(tempval);
		if (Smax[4]>Smin[4])
		{
			if (Spos[4]>Smax[4])
			{
				Spos[4] = Smax[4];
				ud_dispfrm_update_answer(Sfrm, GOEDIT5, &Spos[4]);
			}
			if (Spos[4]<Smin[4])
			{
				Spos[4] = Smin[4];
				ud_dispfrm_update_answer(Sfrm, GOEDIT5, &Spos[4]);
			}
		}
		limit = 0;
		break;
	case GOTO6:
		ud_getfrm_field(Sfrm, GOEDIT6, data);
		Spos[5] = atof(tempval);
		if (Smax[5]>Smin[5])
		{
			if (Spos[5]>Smax[5])
			{
				Spos[5] = Smax[5];
				ud_dispfrm_update_answer(Sfrm, GOEDIT6, &Spos[5]);
			}
			if (Spos[5]<Smin[5])
			{
				Spos[5] = Smin[5];
				ud_dispfrm_update_answer(Sfrm, GOEDIT6, &Spos[5]);
			}
		}
		limit = 0;
		break;
	case GOTO7:
		ud_getfrm_field(Sfrm, GOEDIT7, data);
		Spos[6] = atof(tempval);
		if (Smax[6]>Smin[6])
		{
			if (Spos[6]>Smax[6])
			{
				Spos[6] = Smax[6];
				ud_dispfrm_update_answer(Sfrm, GOEDIT7, &Spos[6]);
			}
			if (Spos[6]<Smin[6])
			{
				Spos[6] = Smin[6];
				ud_dispfrm_update_answer(Sfrm, GOEDIT7, &Spos[6]);
			}
		}
		limit = 0;
		break;
	case GOTO8:
		ud_getfrm_field(Sfrm, GOEDIT8, data);
		Spos[7] = atof(tempval);
		if (Smax[7]>Smin[7])
		{
			if (Spos[7]>Smax[7])
			{
				Spos[7] = Smax[7];
				ud_dispfrm_update_answer(Sfrm, GOEDIT8, &Spos[7]);
			}
			if (Spos[7]<Smin[7])
			{
				Spos[7] = Smin[7];
				ud_dispfrm_update_answer(Sfrm, GOEDIT8, &Spos[7]);
			}
		}
		limit = 0;
		break;
	case GOTO9:
		ud_getfrm_field(Sfrm, GOEDIT9, data);
		Spos[8] = atof(tempval);
		if (Smax[8]>Smin[8])
		{
			if (Spos[8]>Smax[8])
			{
				Spos[8] = Smax[8];
				ud_dispfrm_update_answer(Sfrm, GOEDIT9, &Spos[8]);
			}
			if (Spos[8]<Smin[8])
			{
				Spos[8] = Smin[8];
				ud_dispfrm_update_answer(Sfrm, GOEDIT9, &Spos[8]);
			}
		}
		limit = 0;
		break;
	case GOTO10:
		ud_getfrm_field(Sfrm, GOEDIT10, data);
		Spos[9] = atof(tempval);
		if (Smax[9]>Smin[9])
		{
			if (Spos[9]>Smax[9])
			{
				Spos[9] = Smax[9];
				ud_dispfrm_update_answer(Sfrm, GOEDIT10, &Spos[9]);
			}
			if (Spos[9]<Smin[9])
			{
				Spos[9] = Smin[9];
				ud_dispfrm_update_answer(Sfrm, GOEDIT10, &Spos[9]);
			}
		}
		limit = 0;
		break;
	}
	if (limit == 0)
	{
		S_store_pos(0);
		if (LW_clash_flag) 
			ul_ipv_clash_form(LW_clash_record);
		goto done;
	}
/*
.....limit button
*/
	switch (*fieldno)
	{
	case LIPLUS1:
	case LIMINUS1:
/*
.....Get the Spos[0] and Delta[0]
*/
		ud_getfrm_field(Sfrm, EDIT1, data);
		Delta[0] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT1, data);
		Spos[0] = atof(tempval);
		if (*fieldno==LIPLUS1)
		{
			if (Smax[0]>Spos[0]>=Smin[0])
			{
				temp = (Smax[0]-Spos[0])/20.0;
			}
			else
				goto done;
		}
		else if (*fieldno==LIMINUS1)
		{
			if (Smax[0]>=Spos[0]>Smin[0])
			{
				temp = (Spos[0]-Smin[0])/20.0;
			}
			else
				goto done;
		}
		if (temp>Delta[0])
			Delta[0] = temp;
		if (Delta[0]==0.0)
			Delta[0] = 1.0;
limit:;
		if (*fieldno==LIPLUS1)
		{
			if ((Spos[0] + Delta[0])>Smax[0])
				goto done;
			Spos[0] = Spos[0] + Delta[0];
		}
		if (*fieldno==LIMINUS1)
		{
			if ((Spos[0] - Delta[0])<Smin[0])
				goto done;
			Spos[0] = Spos[0] - Delta[0];
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT1, &Spos[0]);
		S_store_pos(0);
		cont = 1;
		if (LW_clash_flag) 
			cont = ul_ipv_clash_form(LW_clash_record);
		ud_update_form(Sfrm);
		if (cont==0) goto done;
		uu_delay(1);
		goto limit;
		break;
	case LIPLUS2:
	case LIMINUS2:
		ud_getfrm_field(Sfrm, EDIT2, data);
		Delta[1] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT2, data);
		Spos[1] = atof(tempval);
		if (*fieldno==LIPLUS2)
		{
			if (Smax[1]>Spos[1]>=Smin[1])
			{
				temp = (Smax[1]-Spos[1])/20.0;
			}
			else
				goto done;
		}
		else if (*fieldno==LIMINUS2)
		{
			if (Smax[1]>=Spos[1]>Smin[1])
			{
				temp = (Spos[1]-Smin[1])/20.0;
			}
			else
				goto done;
		}
		if (temp>Delta[0])
			Delta[1] = temp;
		if (Delta[1]==0.0)
			Delta[1] = 1.0;
limit1:;
		if (*fieldno==LIPLUS2)
		{
			if ((Spos[1] + Delta[1])>Smax[1])
				goto done;
			Spos[1] = Spos[1] + Delta[1];
		}
		if (*fieldno==LIMINUS2)
		{
			if ((Spos[1] - Delta[1])<Smin[1])
				goto done;
			Spos[1] = Spos[1] - Delta[1];
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT2, &Spos[1]);
		S_store_pos(0);
		cont = 1;
		if (LW_clash_flag) 
			cont = ul_ipv_clash_form(LW_clash_record);
		ud_update_form(Sfrm);
		if (cont==0) goto done;
		uu_delay(1);
		goto limit1;
		break;
	case LIPLUS3:
	case LIMINUS3:
		ud_getfrm_field(Sfrm, EDIT3, data);
		Delta[2] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT3, data);
		Spos[2] = atof(tempval);
		if (*fieldno==LIPLUS3)
		{
			if (Smax[2]>Spos[2]>=Smin[2])
			{
				temp = (Smax[2]-Spos[2])/20.0;
			}
			else
				goto done;
		}
		else if (*fieldno==LIMINUS3)
		{
			if (Smax[2]>=Spos[2]>Smin[2])
			{
				temp = (Spos[2]-Smin[2])/20.0;
			}
			else
				goto done;
		}
		if (temp>Delta[2])
			Delta[2] = temp;
		if (Delta[2]==0.0)
			Delta[2] = 1.0;
limit2:;
		if (*fieldno==LIPLUS3)
		{
			if ((Spos[2] + Delta[2])>Smax[2])
				goto done;
			Spos[2] = Spos[2] + Delta[2];
		}
		if (*fieldno==LIMINUS3)
		{
			if ((Spos[2] - Delta[2])<Smin[2])
				goto done;
			Spos[2] = Spos[2] - Delta[2];
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT3, &Spos[2]);
		S_store_pos(0);
		cont = 1;
		if (LW_clash_flag) 
			cont = ul_ipv_clash_form(LW_clash_record);
		ud_update_form(Sfrm);
		if (cont==0) goto done;
		uu_delay(1);
		goto limit2;
		break;
	case LIPLUS4:
	case LIMINUS4:
		ud_getfrm_field(Sfrm, EDIT4, data);
		Delta[3] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT4, data);
		Spos[3] = atof(tempval);
		if (*fieldno==LIPLUS4)
		{
			if (Smax[3]>Spos[3]>=Smin[3])
			{
				temp = (Smax[3]-Spos[3])/20.0;
			}
			else
				goto done;
		}
		else if (*fieldno==LIMINUS4)
		{
			if (Smax[3]>=Spos[3]>Smin[3])
			{
				temp = (Spos[3]-Smin[3])/20.0;
			}
			else
				goto done;
		}
		if (temp>Delta[3])
			Delta[3] = temp;
		if (Delta[3]==0.0)
			Delta[3] = 1.0;
limit3:;
		if (*fieldno==LIPLUS4)
		{
			if ((Spos[3] + Delta[3])>Smax[3])
				goto done;
			Spos[3] = Spos[3] + Delta[3];
		}
		if (*fieldno==LIMINUS4)
		{
			if ((Spos[3] - Delta[3])<Smin[3])
				goto done;
			Spos[3] = Spos[3] - Delta[3];
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT4, &Spos[3]);
		S_store_pos(0);
		cont = 1;
		if (LW_clash_flag) 
			cont = ul_ipv_clash_form(LW_clash_record);
		ud_update_form(Sfrm);
		if (cont==0) goto done;
		uu_delay(1);
		goto limit3;
		break;
	case LIPLUS5:
	case LIMINUS5:
		ud_getfrm_field(Sfrm, EDIT5, data);
		Delta[4] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT5, data);
		Spos[4] = atof(tempval);
		if (*fieldno==LIPLUS5)
		{
			if (Smax[4]>Spos[4]>=Smin[4])
			{
				temp = (Smax[4]-Spos[4])/20.0;
			}
			else
				goto done;
		}
		else if (*fieldno==LIMINUS5)
		{
			if (Smax[4]>=Spos[4]>Smin[4])
			{
				temp = (Spos[4]-Smin[4])/20.0;
			}
			else
				goto done;
		}
		if (temp>Delta[4])
			Delta[4] = temp;
		if (Delta[4]==0.0)
			Delta[4] = 1.0;
limit4:;
		if (*fieldno==LIPLUS5)
		{
			if ((Spos[4] + Delta[4])>Smax[4])
				goto done;
			Spos[4] = Spos[4] + Delta[4];
		}
		if (*fieldno==LIMINUS5)
		{
			if ((Spos[4] - Delta[4])<Smin[4])
				goto done;
			Spos[4] = Spos[4] - Delta[4];
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT5, &Spos[4]);
		S_store_pos(0);
		cont = 1;
		if (LW_clash_flag) 
			cont = ul_ipv_clash_form(LW_clash_record);
		ud_update_form(Sfrm);
		if (cont==0) goto done;
		uu_delay(1);
		goto limit4;
		break;
	case LIPLUS6:
	case LIMINUS6:
		ud_getfrm_field(Sfrm, EDIT6, data);
		Delta[5] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT6, data);
		Spos[5] = atof(tempval);
		if (*fieldno==LIPLUS6)
		{
			if (Smax[5]>Spos[5]>=Smin[5])
			{
				temp = (Smax[5]-Spos[5])/20.0;
			}
			else
				goto done;
		}
		else if (*fieldno==LIMINUS6)
		{
			if (Smax[5]>=Spos[5]>Smin[5])
			{
				temp = (Spos[5]-Smin[5])/20.0;
			}
			else
				goto done;
		}
		if (temp>Delta[5])
			Delta[5] = temp;
		if (Delta[5]==0.0)
			Delta[5] = 1.0;
limit5:;
		if (*fieldno==LIPLUS6)
		{
			if ((Spos[5] + Delta[5])>Smax[5])
				goto done;
			Spos[5] = Spos[5] + Delta[5];
		}
		if (*fieldno==LIMINUS6)
		{
			if ((Spos[5] - Delta[5])<Smin[5])
				goto done;
			Spos[5] = Spos[5] - Delta[5];
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT6, &Spos[5]);
		S_store_pos(0);
		cont = 1;
		if (LW_clash_flag) 
			cont = ul_ipv_clash_form(LW_clash_record);
		ud_update_form(Sfrm);
		if (cont==0) goto done;
		uu_delay(1);
		goto limit5;
		break;
	case LIPLUS7:
	case LIMINUS7:
		ud_getfrm_field(Sfrm, EDIT7, data);
		Delta[6] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT7, data);
		Spos[6] = atof(tempval);
		if (*fieldno==LIPLUS7)
		{
			if (Smax[6]>Spos[6]>=Smin[6])
			{
				temp = (Smax[6]-Spos[6])/20.0;
			}
			else
				goto done;
		}
		else if (*fieldno==LIMINUS7)
		{
			if (Smax[6]>=Spos[6]>Smin[6])
			{
				temp = (Spos[6]-Smin[6])/20.0;
			}
			else
				goto done;
		}
		if (temp>Delta[6])
			Delta[6] = temp;
		if (Delta[6]==0.0)
			Delta[6] = 1.0;
limit6:;
		if (*fieldno==LIPLUS7)
		{
			if ((Spos[6] + Delta[6])>Smax[6])
				goto done;
			Spos[6] = Spos[6] + Delta[6];
		}
		if (*fieldno==LIMINUS7)
		{
			if ((Spos[6] - Delta[6])<Smin[6])
				goto done;
			Spos[6] = Spos[6] - Delta[6];
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT7, &Spos[6]);
		S_store_pos(0);
		cont = 1;
		if (LW_clash_flag) 
			cont = ul_ipv_clash_form(LW_clash_record);
		ud_update_form(Sfrm);
		if (cont==0) goto done;
		uu_delay(1);
		goto limit6;
		break;
	case LIPLUS8:
	case LIMINUS8:
		ud_getfrm_field(Sfrm, EDIT8, data);
		Delta[7] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT8, data);
		Spos[7] = atof(tempval);
		if (*fieldno==LIPLUS8)
		{
			if (Smax[7]>Spos[7]>=Smin[7])
			{
				temp = (Smax[7]-Spos[7])/20.0;
			}
			else
				goto done;
		}
		else if (*fieldno==LIMINUS8)
		{
			if (Smax[7]>=Spos[7]>Smin[7])
			{
				temp = (Spos[7]-Smin[7])/20.0;
			}
			else
				goto done;
		}
		if (temp>Delta[7])
			Delta[7] = temp;
		if (Delta[7]==0.0)
			Delta[7] = 1.0;
limit7:;
		if (*fieldno==LIPLUS8)
		{
			if ((Spos[7] + Delta[7])>Smax[7])
				goto done;
			Spos[7] = Spos[7] + Delta[7];
		}
		if (*fieldno==LIMINUS8)
		{
			if ((Spos[7] - Delta[7])<Smin[7])
				goto done;
			Spos[7] = Spos[7] - Delta[7];
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT8, &Spos[7]);
		S_store_pos(0);
		cont = 1;
		if (LW_clash_flag) 
			cont = ul_ipv_clash_form(LW_clash_record);
		ud_update_form(Sfrm);
		if (cont==0) goto done;
		uu_delay(1);
		goto limit7;
		break;
	case LIPLUS9:
	case LIMINUS9:
		ud_getfrm_field(Sfrm, EDIT9, data);
		Delta[8] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT9, data);
		Spos[8] = atof(tempval);
		if (*fieldno==LIPLUS9)
		{
			if (Smax[8]>Spos[8]>=Smin[8])
			{
				temp = (Smax[8]-Spos[8])/20.0;
			}
			else
				goto done;
		}
		else if (*fieldno==LIMINUS9)
		{
			if (Smax[8]>=Spos[8]>Smin[8])
			{
				temp = (Spos[8]-Smin[8])/20.0;
			}
			else
				goto done;
		}
		if (temp>Delta[8])
			Delta[8] = temp;
		if (Delta[8]==0.0)
			Delta[8] = 1.0;
limit8:;
		if (*fieldno==LIPLUS9)
		{
			if ((Spos[8] + Delta[8])>Smax[8])
				goto done;
			Spos[8] = Spos[8] + Delta[8];
		}
		if (*fieldno==LIMINUS9)
		{
			if ((Spos[8] - Delta[8])<Smin[8])
				goto done;
			Spos[8] = Spos[8] - Delta[8];
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT9, &Spos[8]);
		S_store_pos(0);
		cont = 1;
		if (LW_clash_flag) 
			cont = ul_ipv_clash_form(LW_clash_record);
		ud_update_form(Sfrm);
		if (cont==0) goto done;
		uu_delay(1);
		goto limit8;
		break;
	case LIPLUS10:
	case LIMINUS10:
		ud_getfrm_field(Sfrm, EDIT10, data);
		Delta[9] = atof(tempval);
		ud_getfrm_field(Sfrm, GOEDIT10, data);
		Spos[9] = atof(tempval);
		if (*fieldno==LIPLUS10)
		{
			if (Smax[9]>Spos[9]>=Smin[9])
			{
				temp = (Smax[9]-Spos[9])/20.0;
			}
			else
				goto done;
		}
		else if (*fieldno==LIMINUS10)
		{
			if (Smax[9]>=Spos[9]>Smin[9])
			{
				temp = (Spos[9]-Smin[9])/20.0;
			}
			else
				goto done;
		}
		if (temp>Delta[9])
			Delta[9] = temp;
		if (Delta[9]==0.0)
			Delta[9] = 1.0;
limit9:;
		if (*fieldno==LIPLUS10)
		{
			if ((Spos[9] + Delta[9])>Smax[9])
				goto done;
			Spos[9] = Spos[9] + Delta[9];
		}
		if (*fieldno==LIMINUS10)
		{
			if ((Spos[9] - Delta[9])<Smin[9])
				goto done;
			Spos[9] = Spos[9] - Delta[9];
		}
		ud_dispfrm_update_answer(Sfrm, GOEDIT10, &Spos[9]);
		S_store_pos(0);
		cont = 1;
		if (LW_clash_flag) 
			cont = ul_ipv_clash_form(LW_clash_record);
		ud_update_form(Sfrm);
		if (cont==0) goto done;
		uu_delay(1);
		goto limit9;
		break;
	}
done:;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnClose()
**       Method called when simulation form is closed.
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
	*fieldno = -1;
	uu_free (d_ans_ptr);
	uu_free (d_methods);
	uu_free (d_called);
	uu_free (d_traverse);
	uu_free (d_display);
    d_ans_ptr = UU_NULL;
    d_methods = UU_NULL;
    d_called = UU_NULL;
    d_traverse = UU_NULL;
    d_display = UU_NULL;
	return(UD_FLDOK);
}
/*********************************************************************
**
**			filter_noop
**
*********************************************************************/
static UD_FSTAT filter_noop(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	*fieldno = -1;
	return(UD_FLDOK);
}

/********************************************************************* 
**    E_FUNCTION :  form_header()
**    PARAMETERS 
** 
**       OUTPUT    : none 
**    RETURNS      : none
**    SIDE EFFECTS : none 
**    WARNINGS     : none 
*********************************************************************/
static void form_header()
{
	int i, k, x_max, y_max, len_form, len_hdr;
	char tmp[FORM_MAX_INT_HEADER];

	sprintf(tmp,"#HEADER#\n");
	i = strlen(tmp);
	sprintf(&tmp[i],"/TITLE/ Machine \'%s\' Simulation\n", LW_mach_data.desc[5]);
	i = strlen(&tmp[0]);
	sprintf(&tmp[i],"/POSITION/ 0,0\n");
	i = strlen(&tmp[0]);
	if (LW_mach_naxes<=2)
	{
		x_max = 150;
	}
	else
	{
		x_max = 310;
	}
	if (LW_mach_naxes<=2)
	{
		if (LW_mach_naxes<2)
			y_max = 80;
		else
			y_max = 155;
	}
	else
	{
		if (LW_mach_naxes%2==0)
			y_max = 75*(LW_mach_naxes/2-1)+80;
		else
			y_max = 75*(LW_mach_naxes/2)+80;
	}
	sprintf(&tmp[i],"/SIZE/ %d,%d\n", x_max+20, y_max+17);
	strcpy (&frm_ptr[0],&tmp[0]);
/*
......Initialize rest of form structure
*/
	len_form = strlen(&frm_ptr[FORM_MAX_INT_HEADER]);
	len_hdr = strlen(tmp);

	for(k = FORM_MAX_INT_HEADER; k < FORM_MAX_INT_HEADER+len_form; k++)
	{
		frm_ptr[k-(FORM_MAX_INT_HEADER-len_hdr)] = frm_ptr[k];
	}
	for(k = len_form+len_hdr; k < FORM_MAX_INT_HEADER+len_form; k++)
	{
		frm_ptr[k] = '\n';            
	}
}
/*********************************************************************
**    E_FUNCTION :  form_elmt(finx, frm_ptr, axis)
**      Create an entry in a form.
**    PARAMETERS
**       INPUT
**         finx			- Index into output form data.
**         axis			- axis number.
**                   
**       OUTPUT:
**         frm_ptr      - Form data.
**         
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int form_elmt(finx, frm_ptr, mpt, axis)
char *frm_ptr;
int *finx, axis;
LW_mach_model_struc *mpt;
{	
	int i, x, y, ipt, item;
	int half;
	if (LW_mach_naxes%2==0)
		half = LW_mach_naxes/2;
	else
		half = LW_mach_naxes/2+1;
	ipt = 0;
	item = -1;
	for (i=0;i<10;i++)
	{
		if (LW_mach_axes[i] != -1)
		{
			if (ipt==axis)
			{
				item = i;
				break;
			}
			ipt++;
		}
	}
	if (item<0)
	{
		ud_winerror("Internal error, form item=-1");
		return -1;
	}
	if (axis+1<=half)
	{
		x = 10;
		y = 75*axis + 8;
	}
	else
	{
		x = 172;
		y = 75*(axis-half) + 8;
	}
	sprintf(&frm_ptr[0]+(*finx),"#FRAME#\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/TITLE/ %s\n", mpt[LW_mach_axes[item]].desc[2]);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+ (*finx) ,"/POSITION/%d,%d\n",x,y);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/SIZE/ %d,%d\n", 150, 65);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));

	if (axis+1<=half)
	{
		x = 18;
		y = 75*axis + 24;
	}
	else
	{
		x = 180;
		y = 75*(axis-half) + 24;
	}

	sprintf(&frm_ptr[0]+(*finx),"#EDIT#\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/LABEL/ Jog %s\n", mpt[LW_mach_axes[item]].axisname);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+ (*finx) ,"/POSITION/%d,%d,%d,%d\n",x,y,x+28,y-2);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/SIZE/ %d,%d\n", 60, 14);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/TYPE/ UD_DASVAL\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/PREC/ 4\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/LEN/ %d\n", 12);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));

	x = x + 87;
	sprintf(&frm_ptr[0]+(*finx),"#PUSHBUTTON#\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/LABEL/ +\n");		
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+ (*finx) ,"/POSITION/%d,%d\n",x,y-5);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/SIZE/ %d,%d\n", 40, 10);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/TYPE/ UD_DASSTRING\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));

	sprintf(&frm_ptr[0]+(*finx),"#PUSHBUTTON#\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/LABEL/ -\n");		
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+ (*finx) ,"/POSITION/%d,%d\n",x,y+6);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/SIZE/ %d,%d\n", 40, 10);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/TYPE/ UD_DASSTRING\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));

	if (axis+1<=half)
	{
		x = 15;
		y = 75*axis + 52;
	}
	else
	{
		x = 177;
		y = 75*(axis-half) + 52;
	}
	sprintf(&frm_ptr[0]+(*finx),"#PUSHBUTTON#\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/LABEL/ Goto\n");		
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+ (*finx) ,"/POSITION/%d,%d\n",x,y-2);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/SIZE/ %d,%d\n", 30, 12);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/TYPE/ UD_DASSTRING\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));

	sprintf(&frm_ptr[0]+(*finx),"#EDIT#\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/LABEL/\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+ (*finx) ,"/POSITION/%d,%d\n",x+32,y-2);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/SIZE/ %d,%d\n", 40, 14);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/TYPE/ UD_DASVAL\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/PREC/ 4\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/LEN/ %d\n", 12);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));

	x = x + 90;
	sprintf(&frm_ptr[0]+(*finx),"#PUSHBUTTON#\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/LABEL/ Limit+\n");		
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+ (*finx) ,"/POSITION/%d,%d\n",x, y-5);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/SIZE/ %d,%d\n", 40, 10);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/TYPE/ UD_DASSTRING\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));

	sprintf(&frm_ptr[0]+(*finx),"#PUSHBUTTON#\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/LABEL/ Limit-\n");		
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+ (*finx) ,"/POSITION/%d,%d\n",x,y+6);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/SIZE/ %d,%d\n", 40, 10);
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	sprintf(&frm_ptr[0]+(*finx),"/TYPE/ UD_DASSTRING\n");
	(*finx) = (*finx) + strlen(&frm_ptr[0]+(*finx));
	return 0;
}

/*********************************************************************
**    S_FUNCTION     :  msl_simulate()
**       Handlle Machine Similation form
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void msl_simulate()
{
	int i,j,finx,status;
	char sbuf[40];
	LW_mach_model_struc *mpt;

	static int idum;
	UU_LOGICAL cmdreject;
/*
.....Set form fields
*/
	S_store_pos(1);
	Delta[0] = Delta[1] = Delta[2] = Delta[3] = Delta[4] = 1.0;
	if (LW_mach_naxes < 1)
	{
		ud_wrerr("A machine has yet to be loaded");
		return;
	}
	if ( (Sactive==UU_TRUE) || (LW_mach_naxes < 2) )
	{
		return;
	}
	Sfrm = -1;
    d_ans_ptr = UU_NULL;
    d_methods = UU_NULL;
    d_called = UU_NULL;
    d_traverse = UU_NULL;
    d_display = UU_NULL;
	d_ans_ptr = (int **)uu_malloc(sizeof(int*)*(LW_mach_naxes*7));
	d_methods = (UD_METHOD *)uu_malloc(sizeof(UD_METHOD)*(LW_mach_naxes*7+1));
	d_called = (char *)uu_malloc(sizeof(char)*(LW_mach_naxes*7+1));
	d_traverse = (char *)uu_malloc(sizeof(char)*(LW_mach_naxes*7+1));
	d_display = (char *)uu_malloc(sizeof(char)*(LW_mach_naxes*7+1));
	frm_ptr = (char *)uu_lsnew();
	if (frm_ptr == 0)
	{
		ud_wrerr("Could not allocate memory for the simulation form.");
		status = UU_FAILURE;
		goto done;
	}
	frm_ptr = (char *)uu_lsinsrt(frm_ptr,13000);
	for(finx=0;finx<FORM_MAX_INT_HEADER;finx++) frm_ptr[finx] = 0;
	mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
	for (i=0;i<LW_mach_naxes;i++)
	{
		for (j=0;j<7;j++)
		{
			d_traverse[i*7+j] = 1;
			d_display[i*7+j] = 1;
		}
		status = form_elmt(&finx, frm_ptr, mpt, i);
		if (status==-1) goto done;
		for (j=0;j<7;j++)
		{
			d_called[i*7+j] = 6;
		}
		d_ans_ptr[i*7] = (int *)&Delta[i];
		d_ans_ptr[i*7+4] = (int *)&Spos[i];
		d_methods[i*7] = UU_NULL;
		d_methods[i*7+4] = UU_NULL;
		d_methods[i*7+1] = OnLimit;
		d_methods[i*7+2] = OnLimit;
		d_methods[i*7+3] = OnLimit;
		d_methods[i*7+5] = OnLimit;
		d_methods[i*7+6] = OnLimit;
	}
	d_methods[i*7] = OnClose;
	d_called[i*7] = 6;
	form_header();
/*
......set end of form
*/
	strcpy(&frm_ptr[0]+(finx),"~END\n");
/*
.....Display the form
*/
form:;
	Sfrm = ud_form_display1("INTERNAL.INTERNAL",d_ans_ptr, d_ans_ptr, d_methods,
			d_called, d_display, d_traverse);
done:;
	if (Sfrm != -1) Sactive = UU_TRUE;
	return;
}
/*********************************************************************
**    S_FUNCTION     :  S_update_form(tnum)
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
static void S_update_form(tnum)
int tnum;
{
/*
.....Machine axis is active
*/
	if (Ssix < 0)
	{
		Scolor = Smodel_a[Smix].color;
		Svis = Smodel_a[Smix].visible;
		Strans = Smodel_a[Smix].translucency;
		Srev = Smodel_a[Smix].reverse;
		Sedge = Smodel_a[Smix].edge;
		Sedgcol = Smodel_a[Smix].edge_color;
		if (Smodel_a[Smix].type == LW_MACH_NOTYPE)
			ud_setfrm_traverse_mask(0,FAREV,UU_FALSE);
		else
			ud_setfrm_traverse_mask(0,FAREV,UU_TRUE);
		strcpy(Saxis,Slist[0].answer);
	}
/*
.....Single solid is active
*/
	else
	{
		Scolor = Ssolid_a[Ssix].stock.color;
		Svis = Ssolid_a[Ssix].stock.visible;
		if (Svis == -1) Svis = Smodel_a[Smix].visible;
		Strans = Ssolid_a[Ssix].stock.translucency;
		if (Strans == -1) Strans = Smodel_a[Smix].translucency;
		Srev = 0;
		if (Sedge == -1) Sedge = Smodel_a[Smix].edge;
		else Sedge = Ssolid_a[Ssix].stock.edge;
		Sedgcol = Ssolid_a[Ssix].stock.edge_color;
		if (Sedgcol == -1) Sedgcol = Smodel_a[Smix].edge_color;
		ud_setfrm_traverse_mask(0,FAREV,UU_FALSE);
		strcpy(Saxis,Slist[1].answer);
	}
/*
.....Update the form fields
*/
	ud_dispfrm_update_answer(0,FACOL,&Scolor);
	ud_dispfrm_update_answer(0,FAVIS,&Svis);
	ud_dispfrm_update_answer(0,FATRA,&Strans);
	ud_dispfrm_update_answer(0,FAREV,&Srev);
	ud_dispfrm_update_answer(0,FAEDG,&Sedge);
	ud_dispfrm_update_answer(0,FAEDC,&Sedgcol);
	ud_dispfrm_update_answer(0,FACOM,&Saxis);
}
