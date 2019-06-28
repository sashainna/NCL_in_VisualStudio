/*********************************************************************
**   FILENAME: lipvmach.c
**   CONTAINS: ul_ipv_mach_form()
**
**     COPYRIGHT 2004 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       lipvmach.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:13
*********************************************************************/
#include <stdio.h>
#include "usysdef.h"
#include "lcom.h"
#include "mdcpln.h"
#include "mfort.h"
#include "nclfc.h"
#include "nclmplay.h"
#include "nccs.h"
#include "udfconst.h"
#include "udforms.h"
#include "udfdata.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "lipvmach.h"

#define FMCH 0
#define FBRS 1
#define FNAM 2
#define FLOD 3
#define FPIN 4
#define FOFS 5
#define FTLN 6
#define FPOS 7
#define FCLA 8
#define FDES 9

#define FPLAB 0
#define FPMOR 1
#define FPMAX 2
#define FPMFL 3
#define FPPOR 4
#define FPPAX 5
#define FPPFL 6
#define FPPLA 7
#define FPRES 8

#define NAXLAB 2
#define NFORMS 5
#define MAXFLD 10

extern int NAUTLTH;
extern int NAUTMACH;

static int Sfrm[NFORMS];
static UU_LOGICAL Sactive[NFORMS];
static UX_pathname Sdir,Sfulldir,Smname;
static UU_LIST Smodel,Ssolid,Stools,Stpin;
static UD_LIST Slist;
static int Snmodel,Snsolid,Sspindle[LW_MAX_SPINDLE],Sntool,Sntpin;
static int Stpix;
static LW_mach_data_struc Sdesc;
static UM_coord Smpin[3],Sppin[3];
static UD_LIST Sapin;
static UU_REAL Spos[LW_MAX_AXES],Soffset[LW_MAX_AXES],Sbaseofs[LW_MAX_AXES];
static LW_mach_toolpin_struc *Stptr;

static void S_load_list();
static void S_place_stock();
static void S_store_pos();
static void S_load_toolpin();

static UD_FSTAT OnMode(),OnBrowse(),OnLoad(),OnPin(),OnTlength();
static UD_FSTAT OnPos(),OnAttrib();
static UD_FSTAT OnAxis(),OnText1(),OnPlace(),OnReset(),OnClose1();
static UD_FSTAT OnText2(),OnClose2(),OnPosition();
static UD_FSTAT OnText3(),OnClose3();

/*********************************************************************
**	 E_FUNCTION : ul_ipv_mach_form()
**			This function handles the NCLIPV Machine Simulation form.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none
**	 SIDE EFFECTS: none
**	 WARNINGS:
*********************************************************************/
void ul_ipv_mach_form()
{
	int status,i,j,isav,modals[20],icurpt,nt,imach;
	static int mode,idum,ierr;
	char lmsg[80];
	UU_LOGICAL cmdreject,lsav,ifl,um_cceqcc();
	UN_clstruc *iclpt[4];
	LW_mach_toolpin_struc *tptr,tpin;
	static int *ans[] = {&mode,&idum,(int *)&Smname,&idum,&idum,&idum,&idum,
		&idum,&idum,(int *)&Slist};
	static UD_METHOD methods[] = {OnMode,OnBrowse,UU_NULL,OnLoad,OnPin,OnPos,
		OnTlength,OnPos,OnAttrib,UU_NULL};
	static char called[]       = {6,6,6,6,6,6,6,6,6,6};
	static char traverse[]     = {1,1,1,1,1,1,1,1,1,1};
LW_mach_model_struc *mpt;
mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
/*
.....Load the input values into
.....local storage area
*/
	for (i=0;i<NFORMS;i++) Sactive[i] = UU_FALSE;
	mode = LW_mach_simul;
	strcpy(Sdir,LW_mach_dir);
	if (LW_mach_name[0] == '\0')
	{
		if (UL_pworks_mdf[0] == '\0')
		{
			ncl_play_initscan(modals,iclpt,&icurpt,UU_FALSE);
			ncl_motion_playback(modals,5,UU_NULL,UU_NULL,&nt);
			ncl_play_resetscan(iclpt,icurpt);
		}
		if (strcmp(UL_pworks_mdf,"PWORKS") == 0 ||
				strcmp(UL_pworks_mdf,"PostWork") == 0)
			sprintf(Smname,"%d",UL_pworks_machs[0]);
		else
			strcpy(Smname,UL_pworks_mdf);
	}
	else
		strcpy(Smname,LW_mach_name);
	uu_move_byte(&LW_mach_data,&Sdesc,sizeof(LW_mach_data_struc));
	uu_list_init(&Smodel,sizeof(LW_mach_model_struc),10,10);
	uu_list_init(&Ssolid,sizeof(LW_mach_solid_struc),50,20);
	uu_list_init0(&Stpin);
	Snmodel = 0;
	Snsolid = 0;
	for (i=0;i<LW_MAX_SPINDLE;i++) Sspindle[i] = LW_spindle[i];
/*
.....Initialize Toolpin list
*/
	Sapin.num_item = 0;
	Sapin.item = UU_NULL;
	Sapin.answer = UU_NULL;
	S_load_toolpin();
/*
.....Load the Machine description list
*/
	Slist.num_item = 0;
	Slist.item = UU_NULL;
	Slist.answer = UU_NULL;
	S_load_list();
/*
.....Load the tool list
*/
	Sntool = 0;
	lsav = UN_playback_active;
	if (LW_ntool != 0) UN_playback_active = UU_TRUE;
	ul_ipv_load_tools(&Stools,&Sntool);
	UN_playback_active = lsav;
/*
.....Match machine definition with
.....simulation definition
*/
	if (mode && LW_mach_naxes == 0)
		ul_ipv_match_mach(UU_TRUE,&LW_mach_model,LW_mach_nmodel);
	S_store_pos(1);
/*
.....Command reject
*/
	UD_MARK(cmdreject, UU_FALSE);
	status = UU_FAILURE;
	if (!cmdreject)
	{
		do
		{
/*
.....Set traverse flags
*/
			if (LW_mach_mode == LW_VISICUT && NAUTMACH)
			{
				traverse[0] = 1;
				for (i=1;i<=FDES;i++) traverse[i] = mode;
				if (LW_mach_nmodel == 0 || mode == 0)
				{
					traverse[FPIN] = traverse[FOFS] = traverse[FTLN] = 0;
					traverse[FPOS] = traverse[FCLA] = 0;
				}
				else
				{
					traverse[FPIN] = 1;
					traverse[FOFS] = 1;
					traverse[FCLA] = 1;
					if (LW_mach_naxes != 0)
					{
						traverse[FTLN] = 1;
						traverse[FPOS] = 1;
					}
					else
					{
						traverse[FTLN] = 0;
						traverse[FPOS] = 0;
					}
				}
			}
			else
			{
				for (i=0;i<=FDES;i++) traverse[i] = 0;
			}
/*
.....Get the Form input
*/
			status = ud_form1("ipvmach.frm", ans, ans, methods, called, UU_NULL,
				traverse);
/*
.....Close all open forms
*/
			for (i=0;i<NFORMS;i++)
			{
				if (Sactive[i]) ud_close_dispfrm(Sfrm[i]);
			}
			if (status==-1) break;
/*
.....Save tooling pin fields
*/
			for (i=0;i<3;i++)
			{
				um_vctovc(Smpin[i],Stptr[Stpix].mpin[i]);
				um_vctovc(Sppin[i],Stptr[Stpix].ppin[i]);
			}
/*
.....Free the Machine description list
*/
			ud_free_flist(&Slist);
/*
.....Allocate a Machine Simulation user
*/
			if (mode && LW_session[0] != 0 && !LW_mach_defined)
			{
				pwdall("SIMULATE",lmsg,&ierr);
				if (ierr != 0) mode = UU_FALSE;
			}
/*
.....Load the machine parameters
*/
			if (mode && (strcmp(LW_mach_name,Smname) || !LW_mach_defined))
			{
				LW_mach_simul = mode;
				ifl = UU_FALSE;
				if (strcmp(LW_mach_name,Smname))
				{
					status = ul_ipv_load_mach(Sdir,Smname,Sfulldir,&Sdesc,&Smodel,
						&Ssolid,&Snmodel,&Snsolid,Sspindle,&Stpin,&Sntpin);
					ifl = UU_TRUE;
				}				
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
						if (ifl) S_store_pos(1);
					}
/*
.........Force at least 1 Tooling pin
*/
					if (Sntpin == 0)
					{
						tpin.mpin[0][0] = tpin.mpin[0][1] = tpin.mpin[0][2] = 0.;
						tpin.mpin[1][0] = tpin.mpin[1][1] = tpin.mpin[1][2] = 0.;
						tpin.mpin[2][0] = tpin.mpin[2][1] = tpin.mpin[2][2] = 0.;
						tpin.mpin[1][2] = tpin.mpin[2][0] = 1.;
						tpin.ppin[0][0] = tpin.ppin[0][1] = tpin.ppin[0][2] = 0.;
						tpin.ppin[1][0] = tpin.ppin[1][1] = tpin.ppin[1][2] = 0.;
						tpin.ppin[2][0] = tpin.ppin[2][1] = tpin.ppin[2][2] = 0.;
						tpin.ppin[1][2] = tpin.ppin[2][0] = 1.;
						strcpy(tpin.label,"X");
						tpin.axis = 0;
						um_identtf(tpin.matrix);
						um_identtf(tpin.invmx);
						uu_list_push(&Stpin,&tpin);
						Sntpin = 1;
					}
					uu_move_byte(&Stpin,&LW_mach_toolpin,sizeof(UU_LIST));
					LW_mach_num_tpin = Sntpin;
				}
				else LW_mach_simul = UU_FALSE;
			}
/*
.....Machine simulation is not active
.....Destroy the machine
*/
			else if (!mode && LW_mach_defined)
			{
				ul_ipv_destroy_assembly();
				status = UU_SUCCESS;
			}
			else status = UU_SUCCESS;
/*
.....Store form values
*/
			if (status == UU_SUCCESS)
			{
				LW_mach_simul = mode;
				strcpy(LW_mach_name,Smname);
				strcpy(LW_mach_dir,Sdir);
				for (i=0;i<LW_MAX_SPINDLE;i++) LW_spindle[i] = Sspindle[i];
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
				if (Sntool > 0)
				{
					if (LW_ntool > 0) uu_list_free(&LW_tool_list);
					uu_move_byte(&Stools,&LW_tool_list,sizeof(UU_LIST));
					LW_ntool = Sntool;
				}
/*
........Reset session
*/
				if (LW_session[0] != 0) ul_ipv_reset_session(UU_FALSE);
			}
		} while (status != UU_SUCCESS);
	}
/*
.....Free local storage
*/
	if (status != UU_SUCCESS)
	{
		ul_ipv_free_mach(&Smodel,&Snmodel,&Ssolid,&Snsolid,&Stpin,&Sntpin);
		uu_list_free(&Stools);
	}
/*
.....End of routine
*/
	ud_free_flist(&Sapin);
	UD_UNMARK(cmdreject);
	return;
}

/*********************************************************************
**    S_FUNCTION     :  OnMode(filedno, val, stat)
**       Method called Machine Simulation toggle is changed.
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
static UD_FSTAT OnMode(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	UU_LOGICAL ifl;
/*
.....Set the traverse masks
*/
	ifl = UU_FALSE;
	if (*(val->frmint) == 1) ifl = UU_TRUE;
	ud_set_traverse_mask(FBRS,ifl);
	ud_set_traverse_mask(FNAM,ifl);
	ud_set_traverse_mask(FLOD,ifl);
	ud_set_traverse_mask(FDES,ifl);
	if (LW_mach_nmodel == 0) ifl = 0;
	ud_set_traverse_mask(FPIN,ifl);
	ud_set_traverse_mask(FOFS,ifl);
	ud_set_traverse_mask(FCLA,ifl);
	if (LW_mach_naxes == 0) ifl = 0;
	ud_set_traverse_mask(FTLN,ifl);
	ud_set_traverse_mask(FPOS,ifl);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnBrowse(filedno, val, stat)
**       Routine to get the Machine directory name.
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
	char sbuf[80],title[80];
	UX_pathname fullname,fname;
	int nc;
/*
.....Get Machine directory
*/
	strcpy(sbuf,"Select the Machine Description directory");
	strcpy(title,"Machine Description");
	ul_getvalid_fulldir("UL_NCLIPV_MACHINES",fullname);
	nc = strlen(fullname);
	ud_get_dirname(sbuf,title,fullname,&nc);
	ul_break_fname(fullname,Sdir,fname);
	ud_update_answer(FNAM,(int *)fname);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnLoad(filedno, val, stat)
**       Loads the Machine simulation files.
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
static UD_FSTAT OnLoad(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,status,ierr,tsav;
	UU_LOGICAL mode;
	char lmsg[80];
	LW_mach_model_struc *mpt;
/*
........Allocate a Machine Simulation user
*/
	status = UU_SUCCESS;
	if (LW_session[0] != 0 && !LW_mach_defined)
	{
		pwdall("SIMULATE",lmsg,&ierr);
		if (ierr != 0) status = UU_FAILURE;
	}
/*
.....Load the machine parameters
*/
	if (status == UU_SUCCESS)
		status = ul_ipv_load_mach(Sdir,Smname,Sfulldir,&Sdesc,&Smodel,&Ssolid,
			&Snmodel,&Snsolid,Sspindle,&Stpin,&Sntpin);
/*
.....Store form values
*/
	if (status == UU_SUCCESS)
	{
		strcpy(LW_mach_name,Smname);
		ul_ipv_free_mach(&LW_mach_model,&LW_mach_nmodel,&LW_mach_solid,
			&LW_mach_nsolid,&LW_mach_toolpin,&LW_mach_num_tpin);
		strcpy(LW_mach_name,Smname);
		strcpy(LW_mach_dir,Sdir);
		uu_move_byte(&Smodel,&LW_mach_model,sizeof(UU_LIST));
		uu_move_byte(&Ssolid,&LW_mach_solid,sizeof(UU_LIST));
		uu_move_byte(&Stpin,&LW_mach_toolpin,sizeof(UU_LIST));
		LW_mach_nmodel = Snmodel;
		LW_mach_nsolid = Snsolid;
		LW_mach_num_tpin = Sntpin;
		uu_list_init(&Smodel,sizeof(LW_mach_model_struc),10,5);
		uu_list_init(&Ssolid,sizeof(LW_mach_solid_struc),50,20);
		uu_list_init(&Stpin,sizeof(LW_mach_toolpin_struc),10,5);
		Snmodel = 0;
		Snsolid = 0;
		uu_move_byte(&Sdesc,&LW_mach_data,sizeof(LW_mach_data_struc));
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
/*
.....Load the tooling pin fields
*/
		S_load_toolpin();
/*
.....Reset the session
*/
		S_store_pos(1);
		if (LW_session[0] != 0 || LW_active)
		{
         tsav = LW_mach_type;
			if ((LW_session[0] != 0)||(LW_nclipv==LW_STANDALONE)) 
				LW_mach_simul = UU_TRUE;
			ul_ipv_reset_session(UU_FALSE);
         LW_mach_type = tsav;
		}
/*
.....Update the list field
*/
		S_load_list();
		ud_update_answer(FDES,(int *)&Slist);
/*
.....Enable the Tooling Pin form
*/
		ud_set_traverse_mask(FPIN,UU_TRUE);
		ud_set_traverse_mask(FOFS,UU_TRUE);
		ud_set_traverse_mask(FCLA,UU_TRUE);
		if (LW_mach_naxes != 0)
		{
			ud_set_traverse_mask(FTLN,UU_TRUE);
			ud_set_traverse_mask(FPOS,UU_TRUE);
		}
	}
/*
.....Failed to load machine
*/
	else
	{
		mode = UU_FALSE;
		ud_update_answer(FMCH,&mode);
		*(val->frmint) = mode;
		OnMode(fieldno,val,stat);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnPin(fieldno, val, stat)
**       Handles the Tooling Pin form.
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
static UD_FSTAT OnPin(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	static int idum;
	static char traverse[] = {1,1,1,1,1,1,1,1,1};
	static UD_METHOD methods[] = {OnAxis,OnText1,OnText1,OnText1,
		OnText1,OnText1,OnText1,OnPlace,OnReset,OnClose1};
	static char called[] = {6,6,6,6,6,6,6,6,6};
	static char display[] = {1,1,1,1,1,1,1,1,1};
	static int *ans[] = {(int *)&Sapin,(int *)Smpin[0],(int *)Smpin[1],
		(int *)Smpin[2],(int *)Sppin[0],(int *)Sppin[1],(int *)Sppin[2],&idum,
		&idum};
/*
.....Take down the  main form
.....while in picking mode
*/
	ud_form_invis();
/*
.....Set the traverse fields
*/
	if (LW_active && LW_nstock[0]+LW_nstock[1] > 0)
	{
		traverse[FPPLA] = 1;
		traverse[FPRES] = 1;
	}
	else
	{
		traverse[FPPLA] = 0;
		traverse[FPRES] = 0;
	}
/*
.....Make sure form is not already active
*/
	if (!Sactive[0])
	{
/*
.....Display the form
*/
		UM_cc_exttoint(Smpin[1],Smpin[1]);
		UM_cc_exttoint(Smpin[2],Smpin[2]);
		UM_cc_exttoint(Sppin[1],Sppin[1]);
		UM_cc_exttoint(Sppin[2],Sppin[2]);
		Sfrm[0] = ud_form_display1("ipvmachpin.frm",ans,ans,methods,called,
			display,traverse);
		if (Sfrm[0] != -1) Sactive[0] = UU_TRUE;
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnText1(filedno, val, stat)
**       Handles the text fields in the Tooling Pin form.
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
static UD_FSTAT OnText1(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
	LW_mach_model_struc *mpt;

	switch (*fieldno)
	{
	case FPMOR:
		um_vctovc(val->frmvec,Smpin[0]);
		break;
	case FPMAX:
		um_vctovc(val->frmvec,Smpin[1]);
		break;
	case FPMFL:
		um_vctovc(val->frmvec,Smpin[2]);
		break;
	case FPPOR:
		um_vctovc(val->frmvec,Sppin[0]);
		break;
	case FPPAX:
		um_vctovc(val->frmvec,Sppin[1]);
		break;
	case FPPFL:
		um_vctovc(val->frmvec,Sppin[2]);
		break;
	}
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnPlace(filedno, val, stat)
**       Handles the Place Stock button in the Tooling Pin form.
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
static UD_FSTAT OnPlace(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	S_place_stock(UU_FALSE);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnReset(filedno, val, stat)
**       Handles the Reset Stock button in the Tooling Pin form.
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
static UD_FSTAT OnReset(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	S_place_stock(UU_TRUE);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnClose1()
**       Method called when Tooling Pin form is closed.
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
	Sactive[0] = UU_FALSE;
	ud_form_vis();
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnPos(filedno, val, stat)
**       Handles the Axes Position form.
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
static UD_FSTAT OnPos(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i,ipt,naxes,ifrm;
	char sbuf[40];
	LW_mach_model_struc *mpt;

	static int idum;
	static char traverse[] = {1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 1,1,1,1};
	static char called[] = {6,6,6,6,6,6,6,6,6,6, 6,6,6,6,6,6,6,6,6,6, 6,6,6,6};
	static char display[] = {1,1, 1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1, 
		1,1,1, 1};
	UD_METHOD methods[LW_MAX_AXES];
	int *ans[LW_MAX_AXES+1];
/*
.....Initialize routine
*/
	ifrm = 1;
	if (*fieldno == FOFS) ifrm = 2;
/*
.....Make sure form is not already active
*/
	if (!Sactive[ifrm] && LW_mach_naxes >= 2)
	{
/*
.....Determine which form to use
*/
		naxes = LW_mach_naxes;
		if (LW_mach_type == LW_STRINGER)
		{
			strcpy(sbuf,"ipvstringer.frm");
			naxes = 21;
		}
		else
		{
			if (naxes > MAXFLD) naxes = MAXFLD;
			sprintf(sbuf,"ipvaxes%d.frm",naxes);
		}
/*
.....Set form fields
*/
		for (i=0;i<naxes;i++)
		{
			traverse[i] = 1;
			display[i+NAXLAB] = 1;
			if (*fieldno == FOFS)
			{
				methods[i] = OnText3;
				ans[i] = (int *)&Soffset[i];
			}
			else
			{
				methods[i] = OnText2;
				ans[i] = (int *)&Spos[i];
			}
		}
		for (i=0;i<NAXLAB;i++) display[i] = 0;
		if (*fieldno == FOFS)
		{
			methods[naxes+1] = OnClose3;
			ans[naxes] = &idum;
			traverse[naxes] = 0;
			display[naxes+NAXLAB] = 0;
			display[1] = 1;
		}
		else
		{
			methods[naxes] = OnPosition;
			methods[naxes+1] = OnClose2;
			ans[naxes] = &idum;
			traverse[naxes] = 1;
			display[naxes+NAXLAB] = 1;
			display[0] = 1;
		}
		if (LW_mach_type == LW_STRINGER)
		{
			naxes = LW_mach_naxes;
			traverse[5] = traverse[6] = 0;
			for (i=18;i<naxes;i++) traverse[i] = display[i+NAXLAB] = 1;
			for (i=naxes;i<21;i++) traverse[i] = display[i+NAXLAB] = 0;
		}
/*
.....Display the form
*/
		Sfrm[ifrm] = ud_form_display1(sbuf,ans,ans,methods,called,display,
			traverse);
/*
.....Set the axes labels
........Standard machine
*/
		if (Sfrm[ifrm] != -1)
		{
			Sactive[ifrm] = UU_TRUE;
			mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
			ipt = (LW_mach_type == LW_STRINGER) ? 18 : 0;
			for (i=ipt;i<naxes;i++)
			{
				if (LW_mach_axes[i] != -1)
				{
					sprintf(sbuf,"%s:",mpt[LW_mach_axes[i]].axisname);
					ud_dispfrm_update_prompt(Sfrm[ifrm],ipt,sbuf);
					ipt++;
				}
			}
			ud_update_form(Sfrm[ifrm]);
		}
	}
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnPosition(fieldno,val,stat)
**       Stores the local machine position in the global array and
**       physically positions the machine if NCLIPV is active.
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
static UD_FSTAT OnPosition(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Physically position the axes
*/
	if (LW_mach_type == LW_STRINGER)
	{
		Spos[5] = Spos[0]; Spos[6] = Spos[1];
		ud_dispfrm_update_answer(Sfrm[1],5,&Spos[5]);
		ud_dispfrm_update_answer(Sfrm[1],6,&Spos[6]);
	}
	S_store_pos(0);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnText2(filedno, val, stat)
**       Handles the text fields in the Axes Position form.
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
static UD_FSTAT OnText2(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Set the axis position
*/
	Spos[*fieldno] = *val->frmflt;
	if (LW_mach_type == LW_STRINGER)
	{
		if (*fieldno == 0)
		{
			Spos[5] = Spos[0];
			ud_dispfrm_update_answer(Sfrm[1],5,&Spos[5]);
		}
		else if (*fieldno == 1)
		{
			Spos[6] = Spos[1];
			ud_dispfrm_update_answer(Sfrm[1],6,&Spos[6]);
		}
	}
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnClose2()
**       Method called when Axes Position form is closed.
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
	Sactive[1] = UU_FALSE;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnText3(filedno, val, stat)
**       Handles the text fields in the Axes Offset form.
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
static UD_FSTAT OnText3(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Set the axis position
*/
	Soffset[*fieldno] = *val->frmflt;
	if (LW_mach_type == LW_STRINGER)
	{
		if (*fieldno == 0)
		{
			Soffset[5] = Soffset[0];
			ud_dispfrm_update_answer(Sfrm[2],5,&Soffset[5]);
		}
		else if (*fieldno == 1)
		{
			Soffset[6] = Soffset[1];
			ud_dispfrm_update_answer(Sfrm[2],6,&Soffset[6]);
		}
	}
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnClose3()
**       Method called when Axes Offset form is closed.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT OnClose3()
{
	Sactive[2] = UU_FALSE;
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnTlength(filedno, val, stat)
**       Brings up the Tool Lengths form.
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
static UD_FSTAT OnTlength(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
/*
.....Display the Tool Lengths form
*/
	if (!Sactive[3])
		ul_ipv_mach_tools(&Sfrm[3],&Sactive[3],&Stools,Sntool);
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnAxis(fieldno, val, stat)
**       Updates the tooling pin fields based on the tooling pin label
**       selected by the user.
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
static UD_FSTAT OnAxis(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	int i;
/*
.....Save the current tooling pin
*/
	for (i=0;i<3;i++)
	{
		um_vctovc(Smpin[i],Stptr[Stpix].mpin[i]);
		um_vctovc(Sppin[i],Stptr[Stpix].ppin[i]);
	}
/*
.....Get the selected tooling pin
*/
	for (i=0;i<Sntpin;i++)
	{
		if (strcmp(val->frmstr,Sapin.item[i]) == 0)
		{
			Stpix = i;
			break;
		}
	}
/*
.....Update the tooling pin fields with
.....the correct values
*/
	for (i=0;i<3;i++)
	{
		um_vctovc(Stptr[Stpix].mpin[i],Smpin[i]);
		ud_dispfrm_update_answer(Sfrm[0],FPMOR+i,&Smpin[i]);
		um_vctovc(Stptr[Stpix].ppin[i],Sppin[i]);
		ud_dispfrm_update_answer(Sfrm[0],FPPOR+i,&Sppin[i]);
	}
/*
.....End of routine
*/
	return(UD_FLDOK);
}

/*********************************************************************
**    S_FUNCTION     :  OnAttrib(fieldno, val, stat)
**       Brings up the Machine Attributes form.
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
static UD_FSTAT OnAttrib(fieldno,val,stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
{
	LW_mach_model_struc *mpt;
	LW_mach_solid_struc *spt;
/*
.....Display the Machine Attributes form
*/
	if (!Sactive[4])
	{
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		spt = (LW_mach_solid_struc *)UU_LIST_ARRAY(&LW_mach_solid);
		ul_ipv_mach_attr(&Sfrm[4],&Sactive[4],mpt,spt,LW_mach_nmodel,
			LW_mach_nsolid);
	}
	return(UD_FLDOK);
}

/*********************************************************************
**	 S_FUNCTION : S_load_list()
**			This function formats the Machine description and places it in
**       the list field of the Machine Simulation form.
**	 PARAMETERS	
**		 INPUT  :  none.
**		 OUTPUT :  none.
**	 RETURNS: none.
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
static void S_load_list()
{
	int inc;
	char sbuf[132],snum[80];
	UU_REAL um_mag();
/*
.....Initialize list
*/
	ul_ipv_init_list(&Slist,NMACHDAT);
	if (LW_mach_name[0] == '\0') return;
/*
.....Format the machine description
*/
	for (inc=0;inc<NMACHDAT;inc++)
	{
		if (inc <= MTBF)
		{
			if (strlen(Sdesc.desc[inc]) != 0)
			{
				sprintf(sbuf,"%s = %s",LW_mach_desc_opt[inc],Sdesc.desc[inc]);
				ul_ipv_put_list(&Slist,sbuf);
			}
		}
		else if (inc <= MACHINEAGE)
		{
			if (Sdesc.rary[inc-MTBF-1] != 0.)
			{
				sprintf(sbuf,"%s = ",LW_mach_desc_opt[inc]);
				ncl_sprintf(snum,&(Sdesc.rary[inc-MTBF-1]),1);
				strcat(sbuf,snum);
				ul_ipv_put_list(&Slist,sbuf);
			}
		}
		else
		{
			if (um_mag(Sdesc.vec[inc-MACHINEAGE-1]) != 0.)
			{
				sprintf(sbuf,"%s = ",LW_mach_desc_opt[inc]);
				ncl_sprintf(snum,Sdesc.vec[inc-MACHINEAGE-1],3);
				strcat(sbuf,snum);
				ul_ipv_put_list(&Slist,sbuf);
			}
		}
	}
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
	UU_LOGICAL lmod;
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
	ul_ipv_place_stock(UU_NULL,resfl,&lmod);

/*
.....End of routine
*/
	return;
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
.....Zero out unused positions
*/
	if (ifl == 1)
	{
		for (i=0;i<LW_mach_max_axes;i++) Spos[i] = 0.;
	}
/*
.....Store the global axis positions
.....in the local array
*/
	if (LW_mach_naxes > 0)
	{
		mpt = (LW_mach_model_struc *)UU_LIST_ARRAY(&LW_mach_model);
		ipt = 0;
		for (i=0;i<LW_mach_max_axes;i++)
		{
			if (LW_mach_axes[i] != -1)
			{
				inc = LW_mach_axes[i];
				if (ifl == 0)
				{
					if (mpt[LW_mach_axes[i]].type == LW_MACH_LINEAR)
					{
						UM_len_exttoint(Spos[ipt],mpt[inc].position);
						UM_len_exttoint(Soffset[ipt],mpt[inc].baseofs);
						mpt[inc].offset = mpt[inc].baseofs + Sbaseofs[ipt];
					}
					else
					{
						mpt[inc].position = Spos[ipt];
						mpt[inc].baseofs = Soffset[ipt];
					}
				}
				else
				{
					if (mpt[LW_mach_axes[i]].type == LW_MACH_LINEAR)
					{
						UM_len_inttoext(mpt[inc].position,Spos[ipt]);
						UM_len_inttoext(mpt[inc].baseofs,Soffset[ipt]);
					}
					else
					{
						Spos[ipt] = mpt[inc].position;
						Soffset[ipt] = mpt[inc].baseofs;
					}
					Sbaseofs[ipt] = mpt[inc].offset - mpt[inc].baseofs;
				}
				ipt++;
			}
			else if (LW_mach_type == LW_STRINGER) ipt++;
		}
/*
.....Physically position the axes
*/
		if (ifl == 0)
		{
			if (LW_mach_defined)
				ul_ipv_place_axes();
		}
	}
	else if (ifl == 1)
	{
		for (i=0;i<LW_mach_max_axes;i++) Spos[i] = Soffset[i] = Sbaseofs[i] = 0.;
	}
}

/*********************************************************************
**	 I_FUNCTION : S_load_toolpin()
**			Loads the defined toolpin definitions into the form list.
**	 PARAMETERS	
**		 INPUT  : none
**		 OUTPUT : none
**	 RETURNS: None
**	 SIDE EFFECTS: None
**	 WARNINGS:
*********************************************************************/
static void S_load_toolpin()
{
	int i,nt;
	LW_mach_toolpin_struc *tptr;
/*
.....Initialize toolpin variables
*/
	Stpix = 0;
	Sntpin = LW_mach_num_tpin;
/*
.....Initialize local toolpin storage
*/
	if (!UU_LIST_NULLPTR(&Stpin)) uu_list_free(&Stpin);
	nt = Sntpin; if (nt == 0) nt = 1;
	uu_list_init(&Stpin,sizeof(LW_mach_toolpin_struc),10,5);
	tptr = (LW_mach_toolpin_struc *)UU_LIST_ARRAY(&LW_mach_toolpin);
	for (i=0;i<Sntpin;i++)
		uu_list_push(&Stpin,&tptr[i]);
	Stptr = (LW_mach_toolpin_struc *)UU_LIST_ARRAY(&Stpin);
/*
.....Initialize form list
*/
	ul_ipv_init_list(&Sapin,nt);
/*
.....Load toolpins into form list
*/
	for (i=0;i<LW_mach_num_tpin;i++)
		ul_ipv_put_list(&Sapin,Stptr[i].label);
	if (Sntpin > 0) strcpy(Sapin.answer,Stptr[0].label);
	else Sapin.answer[0] = '\0';
/*
.....Define tooling pin locations
*/
	um_vctovc(Stptr[Stpix].mpin[0],Smpin[0]);
	um_vctovc(Stptr[Stpix].mpin[1],Smpin[1]);
	um_vctovc(Stptr[Stpix].mpin[2],Smpin[2]);
	um_vctovc(Stptr[Stpix].ppin[0],Sppin[0]);
	um_vctovc(Stptr[Stpix].ppin[1],Sppin[1]);
	um_vctovc(Stptr[Stpix].ppin[2],Sppin[2]);
}
