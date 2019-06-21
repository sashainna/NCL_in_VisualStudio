/*********************************************************************
**    NAME         : nepick.c
**       CONTAINS:
**				ncl_init_pick()
**				nclu_set_mark()
**				nclu_set_verify()
**				nclu_set_pick_aper()
**				nclu_limit_pick()
**				ncl_lgeo()
**				ncl_verify_pick()
**				nclu_pick_modals()
**				ncl_toggle_pick()
**				ncl_set_mark()
**				ncl_nomark()
**				ncl_colordev()
**				pik_modals()
**				chk_mark_method()
**				chk_picking_method()
**				ncl_verify_pick2()
**				ud_reset_verify_list()
**				ncl_verify_pick3()
**				ncl_on_verify_segno()
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nepick.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:40
*********************************************************************/
#include "usysdef.h"
#include "xenv1.h"
#include "uhep.h"
#include "dasnog.h"
#include "dinput.h"
#include "driver.h"
#include "lumb.h"
#include "nccs.h"
#include "nclinp.h"
#include "mdrel.h"
#include "mdpick.h"
#include "dselmask.h"
#include "go3.h"		/* defines needed for segment hiliting, etc. */
/*
....forms navigation includes
*/
#include "udforms.h"
#include "udfdata.h"
/*
....redefine picking methods includes
*/
#include "gtblvar3.h"
#include "gtblvar4.h"
#include "gdidd.h"
#include "xfsys1.h"
#include "xenv1.h"

/*
.....Order of values important here, coincides with order of choices
.....in the PICK MODALS form.
*/
#define SOFTWARE_PICK 0
#define HARDWARE_PICK 1
#define HILITE_MARK 0
#define DIAMOND_MARK 1
#define NO_MARK 2
#define DYNAMIC_MARK 3
/*
.....Determines type of methods used to determine what was picked.
.....and how pick choices are marked.
*/
int NCL_pick_method = SOFTWARE_PICK;
/*#if ((UU_COMP==UU_SUN) && (UU_SUNTYPE==UU_SUN_SUN4))||(UU_COMP==UU_IRIS4D)*/

int NCL_mark_method = HILITE_MARK;
int NCL_pik_hierarchy = 1;
/*#else
int NCL_mark_method = DIAMOND_MARK;
#endif*/
/*
.....Defines size of pick aperature
*/
UU_REAL NCL_pick_aper;
/*
.....Stuff for pick verification
*/
#define MAXVERCNT 256
UU_LOGICAL NCL_pick_verify = UU_FALSE;
UU_KEY_ID NCL_verify_list[256];
int NCL_verify_segno[256];
/*
.....NCL_nopick_cnt reset to zero when: user selects valid entity
.....or, user re-enters picking through ud_pick() or ud_ploc() - das/d2hldpk.c.
*/
int NCL_nopick_cnt;
/*
.....Flag set to UU_TRUE when geometry picking can be limited by the user 
*/
UU_LOGICAL NCL_init_limit = UU_FALSE;
/*
.....Features are a special case and
.....are not included in the limit geo mask
.....Bobby  -  6/4/92
*/
UU_LOGICAL NCL_pick_feature = UU_FALSE;
/*
...Character string holding currently limited geometry.
*/
static char ncl_limit_geo_types[26][80] = {
	"POINTS",
	"LINES",
	"ARCS/CIRCLES",
	"PLANES",
	"VECTORS",
	"POINTVECTORS",
	"ALL CURVES",
	"NCL CURVES",
	"COMPOSITE CURVES",
	"B-SPLINES",
	"SURF SPLINES",
	"CONICS",
	"PATERNS",
	"ALL SURFACES",
	"NCL SURFACES",
	"N SURFACES",
	"REV SURFACES",
	"MESH SURFACES",
	"QUILT SURFACES",
	"NET SURFACES",
	"TRIMMED SURFACES",
	"DRAFTING",
	"SHAPES",
	"MATRIX",
	"FEATURES",
	"SOLIDS"};

extern int NCLHOST;		/* DEFINES WHICH GRAPHICS DEVICE WE ARE RUNNING WITH */
static int pikmod[4];  
static int verify_recursive = 0;
static int vcolor, hcolor;
extern int uw_glhicolor,uw_glvrfcolor;
void ncl_toggle_pick(),ncl_set_mark();

/*********************************************************************
**    S_FUNCTION     :  pik_modals(filedno, val, stat)
**       Method called at each change/nochange toggle field
**			in the pick modals form. (pikmod.frm)
**    PARAMETERS   
**       INPUT  : 
**          fieldno	Field number being changed.
**          val		Current field value.
**          stat		Field status.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form traverse mask.
**    WARNINGS     : none
*********************************************************************/
static UD_FSTAT pik_modals(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
	{
/* 
.....Switch traverse mask based on the field that changed.  This code
.....sets the traverse mask so that input fields associated with
.....change/nochange toggles are not traversed when the toggles value
.....is nochange.
*/

	switch(*fieldno) 
		{
		case 0:	/* Pick Aperature */
			if( pikmod[0]==0)
				ud_set_traverse_mask(1, UU_FALSE);
			else
				ud_set_traverse_mask(1, UU_TRUE);
			break;

		case 2:	/* Pick Verify */
			if( pikmod[1]==0)
				ud_set_traverse_mask(3, UU_FALSE);
			else
				ud_set_traverse_mask(3, UU_TRUE);
			break;

		case 4:	/* Pick Marking */
			if( pikmod[2]==0)
				ud_set_traverse_mask(5, UU_FALSE);
			else
				ud_set_traverse_mask(5, UU_TRUE);
			break;
		case 6:	/* Hierachy */
			if( pikmod[3]==0)
				ud_set_traverse_mask(7, UU_FALSE);
			else
				ud_set_traverse_mask(7, UU_TRUE);
			break;
		default:
			break;
		}
/* 
.....Call the default method.  This changes the toggle display, and
.....causes the answer field to be updated.
*/
	ud_default_method(fieldno, val, stat);
	return(UD_FLDOK);
}
/*********************************************************************
**    S_FUNCTION     :  chk_mark_modal(filedno, val, stat)
**       Method called when PICK MODALS, MARK field is modified.
**    PARAMETERS   
**       INPUT  : 
**          fieldno	Field number being changed.
**          val		Current field value.
**          stat		Field status.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Changes the form traverse mask.
**    WARNINGS     : none
*********************************************************************/

static UD_FSTAT chk_mark_modal(fieldno, val, stat)
int *fieldno;
UD_DDATA *val;
UD_FSTAT stat;
	{
	Gcoavail ncl_colordev();

/* 
.....Call the default method.  This changes the toggle display, and
.....causes the answer field to be updated.
*/
	ud_default_method(fieldno, val, stat);
/*
....If we are running on a MONOCHROME device and the user selects HILITE,
....issue error message and force toggle to DIAMOND.
*/
	if ((ncl_colordev() == UG_MONOCHROME) && 
		((*val->frmint == 0) && ((stat == UD_TFWD) || (stat == UD_TBAK))))
		{
		ud_default_method(fieldno, val, stat);
		ud_prmerr("MONOCHROME devices support DIAMOND and NO MARK only.");
		}
	return(UD_FLDOK);
	}
/*********************************************************************
**    E_FUNCTION    : int ncl_init_pick()
**			Initialize/reset pick modals
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**				none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE;
**    SIDE EFFECTS : Sets Pick modals to original state
**    WARNINGS     : none
*********************************************************************/
void ncl_init_pick()
	{
	char buf[80];
	char *ux_getenv(), *p;
	double atof();
/*  
.....Define default PICK APERTURE....
.....Variable 'U_PICKAPER' can be defined in users '.init' file or the
.....NCL502 run script.  Defaults to original value of 0.005.
.....Roberta Zorzynski
*/
	NCL_pick_aper = 0.005;
/*
......now set it in ncl_pick.mod file and not using the envoirment value
	p = ux_getenv("U_PICKAPER",UX_NPRTERRS);
	if (p != UU_NULL)
		{
		aper = atof(p);
		if (aper > NCL_pick_aper) 
			NCL_pick_aper = aper;
		}
*/
/*
.....If we are running with a TEK terminal, default to DIAMOND MARKING
*/
	NCL_mark_method = HILITE_MARK;
	NCL_pik_hierarchy = 1;
/*  
.....Variable 'U_MARKMETHOD' can be defined in users '.init' file or the
.....NCL502 run script.  Allow user to pre-define either HILITE, DIAMOND
.....or NOMARK marking.  
.....Roberta Zorzynski
*/
	p = ux_getenv("U_MARKMETHOD",UX_NPRTERRS);
	if (p != UU_NULL)
		{
		if (strcmp(p, "HILITE")==0)
			NCL_mark_method = HILITE_MARK;
		else if (strcmp(p, "DIAMOND")==0)
			NCL_mark_method = DIAMOND_MARK;
		else if (!strcmp(p, "NOMARK")==0)
			NCL_mark_method = NO_MARK;
		else if (!strcmp(p, "DYNAMIC")==0)
			NCL_mark_method = DYNAMIC_MARK;
		}
	ncl_set_mark(buf);
/*
.....Set/reset verify mode flags
*/
	NCL_pick_verify = UU_FALSE;
	verify_recursive = 0;

	NCL_pick_method = SOFTWARE_PICK;
	p = ux_getenv("U_PICKMETHOD",UX_NPRTERRS);
	if (p != UU_NULL)
		{
/*
.........We set the pick method opposite the desired result since the call
.........ncl_toggle_pick() will set the pick method opposite the current.
.........Only check for HARDWARE since default is SOFTWARE.
*/
		if (!strcmp(p, "HARDWARE"))
			{
			NCL_pick_method = SOFTWARE_PICK;
			ncl_toggle_pick();
			}
		}

	NCL_nopick_cnt = 0;
	}
/*********************************************************************
**    E_FUNCTION    : int nclu_set_mark()
**			Set pick mark method - single key stroke command
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**				none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE;
**    SIDE EFFECTS : Sets NCL_mark_method to new value.
**    WARNINGS     : none
*********************************************************************/
void nclu_set_mark()
	{
	int status, choice;
	char buf[80];
	Gcoavail ncl_colordev();
/*
...Display PICK METHOD pop-up and receive choice selection.
*/
	do
		{
		status = ncl_popup(/* 48 */NCL_MARK_METHOD, &choice);

		if ((status == NCL_OKINPUT) && (choice > 0))
			{
			status = NCL_NOINPUT;
/*
.............If we are on a MONOCHROME device and the user picked
.............HILITE MARK, generate error and put up pop-up again.
*/
			if ((ncl_colordev() == UG_MONOCHROME) && (choice == 1))
				ud_prmerr("MONOCHROME devices support DIAMOND and NO MARK only.");

			else if ((choice > 0) && (choice < 4))
				{
				NCL_mark_method = choice - 1;
				status = NCL_OKINPUT;
				}
			else
				ud_prmerr("Invalid pick mark method.");
			}
		}
	while(status == NCL_NOINPUT);

/*
.....If user made a valid selection, update the mark method subroutine
.....And issue the appropriate message to the user.
*/
	if (status == NCL_OKINPUT)
		{
		ncl_set_mark(buf);
		ud_prmerr(buf);
		}

	return;
	}
/*********************************************************************
**    E_FUNCTION    : int nclu_set_verify()
**			Set/reset verify mode for picking.
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**				none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE;
**    SIDE EFFECTS : Sets NCL_pick_verify to new value.
**    WARNINGS     : none
*********************************************************************/
void nclu_set_verify()
	{
	int i;
	char buf[256];

	NCL_pick_verify = !NCL_pick_verify;

	if (NCL_pick_verify)
		{
		for (i = 0; i < NCL_nopick_cnt; i++)
			NCL_verify_list[i] = 0;
		NCL_nopick_cnt = 0;
		sprintf(buf,"Verify mode ENABLED.");
		if (NCL_mark_method == DYNAMIC_MARK)
		ud_post_msg(2);
		}
	else
		{
		sprintf(buf,"Verify mode DISABLED.");
		verify_recursive = 0;
		ud_desel_vpik();
		}
	ud_prmerr(buf);

	return;
	}
/*********************************************************************
**    E_FUNCTION    : int nclu_set_pick_aper()
**			Prompt user for a new pick aperture.  This controls the size
**          of area search for a valid geometry type when picking 'by pick'.
**          The larger the area, the longer the search.
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**				none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE;
**    SIDE EFFECTS : Sets NCL_pick_aper to new value.
**    WARNINGS     : none
*********************************************************************/
void nclu_set_pick_aper()
	{
	UU_REAL aper;
	int numint;

	aper = NCL_pick_aper;

/* 
.....Get new pick aperture.  
*/
	ud_ldas(UD_DASUNITLESS, /*Enter new pick aperture */UD_DASHEP, 70, &aper, 1, 
			&numint, UD_DEFAULT);

	if (numint > 0)
		{
		if (aper < 0.005)
			NCL_pick_aper = 0.005;
		else if (aper > 1.0)
			NCL_pick_aper = 1.0;
		else
			NCL_pick_aper = aper;
		}

	return;
	}
/*********************************************************************
**    E_FUNCTION    : int nclu_limit_pick()
**      Allow user to limit selection filter through popup-menu
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**				none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE;
**    SIDE EFFECTS : set new limit values
**    WARNINGS     : none
*********************************************************************/
int nclu_limit_pick()
	{
	int status;
	int i;
	UU_LOGICAL flag;
	UD_LIMREC *lptr;
    int mask[UD_NMENTWD];
	static int types[28] = {0,0,0,0,0,0,0,
							0,0,0,0,0,0,0,
							0,0,0,0,0,0,0,
							0,0,0,0,0,0,0};
/*
.....If we are not in 'by pick' mode, return
*/
	if ((!NCL_init_limit)&&(UD_pickmode==1))
		return 0;

	status = nclu_get_geomtype(types);
	if (status==-1)
		return -1;
/*
.....Reset NCL_init_limit in case of REJECT OPs
*/
	NCL_init_limit = UU_FALSE;
	NCL_pick_feature = UU_FALSE;
/*
.....Set up pointer to active limit state
.....Modify limit state (UD_limbfptr - 1) 
.....Since getting here through an .itr file pops an additional
.....limit state onto the stack.
*/
	lptr = &UD_limbf[UD_limbfptr-1];
/*
.....If the limit state is active and this is the first time
.....through for this state, copy the limit state into local buffer
*/
	if (lptr->lsel && !lptr->llsel)
		{
		lptr->llsel = lptr->lsel;
		for (i = 0; i < UD_NMENTWD; i++)
			lptr->llselbf[i] = lptr->lselbf[i];
		}
/*
.....Else if lptr->llsel == 2 then the original limit state was unlimited.
.....Set it back.
*/
	else if (lptr->llsel == 2)
		lptr->lsel = UU_FALSE;
/*
.....Set the geometry filter mask
*/
	nclu_set_geommask(types,mask);
	flag = UU_FALSE;
	for (i=0; i<28;i++)
	{
		if (types[i]==1)
			flag = UU_TRUE;
	}
	status = ncl_lgeo(flag, mask, lptr);
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION    : int ncl_lgeo()
**       Edit limit state to restrict valid geometry types.
**    PARAMETERS   
**       INPUT  : flag = if UU_TRUE then limit DAS
**                          UU_FALSE then reset original limit
**				  bitary = if call to limit then the geometry bit array
**				  limptr = if call to limit then limit this limit state
**				
**       OUTPUT :  
**				none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE;
**    SIDE EFFECTS : reset limit state to previous value.
**    WARNINGS     : none
*********************************************************************/
int
ncl_lgeo(flag, bitary, limptr)
UU_LOGICAL flag;					/* if true then limit DAS, else unrestrict DAS */
int bitary[];						/* bit array to limit geometry */
UD_LIMREC *limptr;
	{
	int i;

	if(flag == UU_TRUE)
	{
/*
........Restrict current limit state to named entities (bitaray[]).
*/
		for(i=0; i<UD_NMENTWD; i++)
			limptr->lselbf[i] = bitary[i];
/*
........Set local limit flag to UU_TRUE if there are current limits
*/
		if (limptr->lsel)
			limptr->llsel = UU_TRUE;
/*
........Set local limit flag to 2 in case there is unlimited picking
........This flags nclu_limit_pick() to check for unlimited picking.
*/
		else
		{
			limptr->llsel = 2;	
			limptr->lsel = UU_TRUE;
		}
	}
	else
	{
/*
........Reset limit to original state
*/
		NCL_pick_feature = UU_FALSE;
		for(i=0; i<UD_NMENTWD; i++)
			limptr->lselbf[i] = limptr->llselbf[i];
	}
	return(NCL_OKINPUT);
}
/*********************************************************************
**    E_FUNCTION    : int ncl_verify_pick()
**       Verify from user that pick is correct
**    PARAMETERS   
**       INPUT  : flag = if true then limit DAS, else unrestrict DAS 
**				  bitary = if call to limit then the geometry bit array
**				  limptr = if call to limit then limit this limit state
**				
**       OUTPUT :  
**				none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE;
**    SIDE EFFECTS : reset limit state to previous value.
**    WARNINGS     : none
*********************************************************************/
UD_DASTAT
ncl_verify_pick(pick)
UD_PPICKREC *pick;
	{
	UD_DASTAT status = DE_TRUE;					/* status return cell */
	int stat, i;
	char msgbuf[80];
	int numint, jmpflag;
	Gcoavail ncl_colordev();
	UM_PICKENT pent;
	int savcount;
/*
.....Test/set flag so we don't get here recursively.
*/
	if (!verify_recursive)
		{
		verify_recursive = 1;
		}
	else
		{
/*
.........recursive entry: generate error and force them to enter DONE/REJECT OP
*/
		ud_prmerr("VERIFY ERROR: must use DONE or REJECT OP to confirm pick.");
		status = DE_AGAIN;
		return(status);
		}
/*
........If in VERIFY MODE, prompt user to confirm pick.
*/
	if (NCL_pick_verify == UU_TRUE)
		{
/*
........If we are using NO_MARK, temporarily set mark method to 
........HILITE_MARK if we are on a color device or DIAMOND_MARK
........if we are on a monochrome device.
*/
		if (NCL_mark_method == NO_MARK)
			{
			if (ncl_colordev() == UG_MONOCHROME) 
				NCL_mark_method = DIAMOND_MARK;
			else
				NCL_mark_method = HILITE_MARK;
/*
.............We choose to ignore the message buffer here
*/
			ncl_set_mark(msgbuf);
			NCL_mark_method = NO_MARK;
			}
/*
............Hilite selected geometry
*/
		gssegdet(pick->pickpath[0], UG_UNDETECTABLE);
		gsseghilite(pick->pickpath[0], UG_HIGHLIGHTED);
		ud_updatews(UG_SUPPRESS);
/*
.........Save off counter since call to ud_ldas() resets it.
*/
		savcount = NCL_nopick_cnt;
/*
............Mark looping region for a REJECT OP
*/
		UD_MARK(jmpflag, UU_FALSE);
		if(jmpflag == 0)
		{
/*
................Loop waiting for DONE or REJECT OP
*/
			stat = UU_FALSE;
			while (stat != UU_TRUE)
				stat = ud_ldas(UD_DASPICK, UA_NCL, 243, &pent, 1, &numint, 1);

			status = DE_TRUE;
			for (i = 0; i < NCL_nopick_cnt; i++)
				NCL_verify_list[i] = 0;
			NCL_nopick_cnt = 0;
			}
		else
			{
/*
................User used REJECT OP, force them to pick more
................geometry.  Add picked geometry to list to ignore.
*/
			status = DE_AGAIN;
/*
.............Reset NCL_nopick_cnt value
*/
			NCL_nopick_cnt = savcount;

			if (NCL_nopick_cnt == MAXVERCNT)
				{
				ud_prmerr("YOU HAVE EXCEEDED THE VERIFY LIMIT STACK.  RESETTING.");
				for (i = 0; i < NCL_nopick_cnt; i++)
					NCL_verify_list[i] = 0;
				NCL_nopick_cnt = 0;
				}
/*
.....Use the same method as when comparing
.....a picked entity on the REJECT stack as
.....used in 'pick_select_verify'.  The old
.....method would pick sub-entities of a composite
.....curve, instead of the composite curve itself.
.....Bobby  -  2/21/92
*/
/*			NCL_verify_list[NCL_nopick_cnt] = pick->pickpath[1];*/
			um_d_pickresolve(pick,1,&pent);
			NCL_verify_list[NCL_nopick_cnt] = um_get_pickkey(&pent,1);

			NCL_nopick_cnt++;
			}

/*
............Reset looping region for REJECT OP to previous value
*/
		jmpflag = 0;
		UD_UNMARK(jmpflag);

/*
............Un-HILITE picked geometry.
*/
		gssegdet(pick->pickpath[0], UG_DETECTABLE);
		gsseghilite(pick->pickpath[0], UG_NORMAL);
/*
........Reset mark method to NO_MARK
........We choose to ignore the message buffer here
*/
		if (NCL_mark_method == NO_MARK)
			ncl_set_mark(msgbuf);
		}
/*
.....Reset recursive flag for normal, proper exit.
*/
	verify_recursive = 0;

	return(status);
	}
/*********************************************************************
**    E_FUNCTION    : int ncl_toggle_pick()
**       Toggle from HARDWARE to SOFTWARE picking.
**       The default is SOFTWARE.  SGI and TEK ONLY!!!
**    PARAMETERS   
**       INPUT  : 
**				none
**				
**       OUTPUT :  
**				none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE;
**    SIDE EFFECTS : HARDWARE picking uses old picking routines.
**    WARNINGS     : none
*********************************************************************/
void ncl_toggle_pick()
	{
	int ug_dpik();
	char buf[80];
	Gint (**ccptr)();
	int (*pptr)();
	int i;

	if (NCL_pick_method == SOFTWARE_PICK)
	{
		pptr = ug_dpik;
		sprintf(buf,"This device supports SOFTWARE picking only.");
	}
	else
	{
		NCL_pick_method = SOFTWARE_PICK;
		pptr = ug_dpik;
		sprintf(buf,"SOFTWARE picking enabled.");
	}

	for(i=0; i<ug_gksdesctbl.maxopws; i++)
		{
		if (ug_gksstli.wsopen[i].connid != NULL &&
			ug_gksstli.wsopen[i].state==UG_ACTIVE)
			{
			ccptr = ug_gksstli.wsopen[i].connid;
			if (pptr != ccptr[UG_DPIK])
				ccptr[UG_DPIK] = pptr;
			}
		}
	ud_prmerr(buf);
	return;
	}
/*********************************************************************
**    E_FUNCTION    : int ncl_set_mark()
**       Set Mark method to Diamond, Hilite or No mark.
**       The default is Hilite on SGI and SUN, DIAMOND elsewhere.
**    PARAMETERS   
**       INPUT  : 
**				none
**				
**       OUTPUT :  
**				none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_set_mark(buf)
char *buf;
	{
	int ug_dseghilite();
/*
.....use jump table Yurong
*/
/*
	int uw_xwhilite();
#if UU_COMP == UU_IRIS4D || UU_COMP == UU_WINNT || UU_COMP == UU_SUN
#ifndef UU_RS6000
	int uw_glhilite();
#endif
#else
#if UU_COMP==UU_WIN2K
	int uw_nthilite();
#endif
#endif
***/
	Gint (**ccptr)();
	int (*pptr)();
	int ncl_nomark();
	int i;

	if (NCL_mark_method == DIAMOND_MARK)
	{
		sprintf(buf,"DIAMOND marking enabled.");
		pptr = ug_dseghilite;
	}
	else if (NCL_mark_method == HILITE_MARK)
	{
/*
...DEBUGGING - remove once we have written all the device dependant hiliting routines.
*/
		sprintf(buf,"HILITE marking enabled.");
		pptr = ug_dseghilite;
		pptr = ug_gksstli.wsopen[0].connid[UW_HILITE];
	}

	else if (NCL_mark_method == DYNAMIC_MARK)
	{
/*
...DEBUGGING - remove once we have written all the device dependant hiliting routines.
*/
		sprintf(buf,"DYNAMIC marking enabled.");
		pptr = ug_dseghilite;
		pptr = ug_gksstli.wsopen[0].connid[UW_HILITE];
	}
	else if (NCL_mark_method == NO_MARK)
		{
		pptr = ncl_nomark;
		sprintf(buf,"Marking disabled.");
		}

	for(i=0; i<ug_gksdesctbl.maxopws; i++)
		{
		if (ug_gksstli.wsopen[i].connid != NULL &&
			ug_gksstli.wsopen[i].state==UG_ACTIVE)
			{
			ccptr = ug_gksstli.wsopen[i].connid;
			if (pptr != ccptr[UG_DHILITE])
				ccptr[UG_DHILITE] = pptr;
			}
		}
	return;
	}
/*********************************************************************
**    E_FUNCTION    : int nclu_pick_modals()
**       Navigate PICK MODALS FORM and set PICK modals accordingly.
**       DISPLAYS CURRENT PICK MODALS FOR VERIFICATION
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**				none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_pick_modals()
	{
	char buf[80],tmp[80];
	int i, status, stat;
	char *ux_getenv();
	UX_pathname fname;
	FILE *fptr;

	static UU_REAL aper;
	static int verify, mark, hierachy;
	static int *form_ans[] = {(int *)&pikmod[0], (int *)&aper, 
							  (int *)&pikmod[1], (int *)&verify,
                              (int *)&pikmod[2], (int *)&mark,
							  (int *)&pikmod[3], (int *)&hierachy,
								(int *) &hcolor, (int *) &vcolor};
	static UD_METHOD methods[] = {  pik_modals, NULL, 
									pik_modals, NULL, 
									pik_modals, chk_mark_modal,
									pik_modals, NULL, 
									NULL, NULL};
	static char called[] = {6, 6,
							6, 6, 6, 6,
							6, 6, 6, 6};
	static char traverse[] = {  1, 0,
								1, 0,
								1, 0,
								1, 0,
								1, 1};
/*
.....Set form fields 'Change:' to NO everytime into the form.
*/
	for (i = 0; i < 4; i++)
		pikmod[i] = 0;
/*
.....Set up default values:
.....For toggle fields, 0 = first choice, 1 = second, etc.
.....Use global values so this form can be used as a status display.
*/
	aper    = NCL_pick_aper;
	verify  = NCL_pick_verify;
	mark    = NCL_mark_method;
	vcolor = uw_glvrfcolor;
	hcolor = uw_glhicolor;
	hierachy = NCL_pik_hierarchy;
	traverse[4] = 1;
/*
.....Traverse form:
*/
	status = ud_form1("pikmod.frm",form_ans,form_ans,methods,called,NULL,traverse);
	if (status==-1)
		return ;

/* 
.....propogate changes
*/
	NCL_pick_aper = aper;
	NCL_pick_verify = verify;
	uw_glvrfcolor = vcolor;
	uw_glhicolor = hcolor;
/*
.....If mark method has changed, set up new marking routines
*/
	if (NCL_mark_method != mark)
	{
		NCL_mark_method = mark;
		ncl_set_mark(buf);
		ud_prmerr(buf);
	}
	NCL_pik_hierarchy = hierachy;
/*
.....save the setting into pik.mod
*/
	stat = UU_SUCCESS;
	strcpy (fname, "ncl_pick.mod");
	stat = ul_open_mod_file("UU_USER_SETTINGS", "modals", UU_NULL, UU_NULL,
					fname, 3, &fptr);
	if ((stat!=UU_SUCCESS)||(fptr==UU_NULL))
	{
/*
.....can't open pick mode file for writing
*/
		return;
	}
/*
.....Store modals
*/
	ux_fputs0("#PICKING#\n", fptr);

	sprintf(tmp, "/APERTURE/ %1.4f\n", NCL_pick_aper);
	ux_fputs0(tmp, fptr);
	if (NCL_pick_verify==0)
		ux_fputs0("/VERIFY_MODE/ *OFF\n", fptr);
	else 
		ux_fputs0("/VERIFY_MODE/ *ON\n", fptr);

	if (NCL_mark_method==0)
		ux_fputs0("/MARKING/ *HILITE\n", fptr);
	else if (NCL_mark_method==1)
		ux_fputs0("/MARKING/ *DIAMOND\n", fptr);
	else if (NCL_mark_method==2)
		ux_fputs0("/MARKING/ *NONE\n", fptr);
	else 
		ux_fputs0("/MARKING/ *DYNAMIC\n", fptr);

	if (NCL_pik_hierarchy==1)
		ux_fputs0("/HIERARCHY/ *ON\n", fptr);
	else
		ux_fputs0("/HIERARCHY/ *OFF\n", fptr);

	if (hcolor==0)
		ux_fputs0("/HIGHLIGHT_COLOR/ *BLACK\n", fptr);
	else if (hcolor==1)
		ux_fputs0("/HIGHLIGHT_COLOR/ *WHITE\n", fptr);
	else if (hcolor==2)
		ux_fputs0("/HIGHLIGHT_COLOR/ *BLUE\n", fptr);
	else if (hcolor==3)
		ux_fputs0("/HIGHLIGHT_COLOR/ *RED\n", fptr);
	else if (hcolor==4)
		ux_fputs0("/HIGHLIGHT_COLOR/ *GREEN\n", fptr);
	else if (hcolor==5)
		ux_fputs0("/HIGHLIGHT_COLOR/ *MAGENTA\n", fptr);
	else if (hcolor==6)
		ux_fputs0("/HIGHLIGHT_COLOR/ *YELLOW\n", fptr);
	else if (hcolor==7)
		ux_fputs0("/HIGHLIGHT_COLOR/ *CYAN\n", fptr);
	else if (hcolor==8)
		ux_fputs0("/HIGHLIGHT_COLOR/ *BROWN\n", fptr);
	else if (hcolor==9)
		ux_fputs0("/HIGHLIGHT_COLOR/ *TAN\n", fptr);
	else if (hcolor==10)
		ux_fputs0("/HIGHLIGHT_COLOR/ *LTBLUE\n", fptr);
	else if (hcolor==11)
		ux_fputs0("/HIGHLIGHT_COLOR/ *SEAGREEN\n", fptr);
	else if (hcolor==12)
		ux_fputs0("/HIGHLIGHT_COLOR/ *ORANGE\n", fptr);
	else if (hcolor==13)
		ux_fputs0("/HIGHLIGHT_COLOR/ *PINK\n", fptr);
	else if (hcolor==14)
		ux_fputs0("/HIGHLIGHT_COLOR/ *PURPLE\n", fptr);
	else if (hcolor==15)
		ux_fputs0("/HIGHLIGHT_COLOR/ *GREY\n", fptr);

	if (vcolor==0)
		ux_fputs0("/VERIFY_COLOR/ *BLACK\n", fptr);
	else if (vcolor==1)
		ux_fputs0("/VERIFY_COLOR/ *WHITE\n", fptr);
	else if (vcolor==2)
		ux_fputs0("/VERIFY_COLOR/ *BLUE\n", fptr);
	else if (vcolor==3)
		ux_fputs0("/VERIFY_COLOR/ *RED\n", fptr);
	else if (vcolor==4)
		ux_fputs0("/VERIFY_COLOR/ *GREEN\n", fptr);
	else if (vcolor==5)
		ux_fputs0("/VERIFY_COLOR/ *MAGENTA\n", fptr);
	else if (vcolor==6)
		ux_fputs0("/VERIFY_COLOR/ *YELLOW\n", fptr);
	else if (vcolor==7)
		ux_fputs0("/VERIFY_COLOR/ *CYAN\n", fptr);
	else if (vcolor==8)
		ux_fputs0("/VERIFY_COLOR/ *BROWN\n", fptr);
	else if (vcolor==9)
		ux_fputs0("/VERIFY_COLOR/ *TAN\n", fptr);
	else if (vcolor==10)
		ux_fputs0("/VERIFY_COLOR/ *LTBLUE\n", fptr);
	else if (vcolor==11)
		ux_fputs0("/VERIFY_COLOR/ *SEAGREEN\n", fptr);
	else if (vcolor==12)
		ux_fputs0("/VERIFY_COLOR/ *ORANGE\n", fptr);
	else if (vcolor==13)
		ux_fputs0("/VERIFY_COLOR/ *PINK\n", fptr);
	else if (vcolor==14)
		ux_fputs0("/VERIFY_COLOR/ *PURPLE\n", fptr);
	else if (vcolor==15)
		ux_fputs0("/VERIFY_COLOR/ *GREY\n", fptr);
	ux_fclose0 (fptr);

	return;
	}
/*********************************************************************
**    E_FUNCTION    : int ncl_nomark()
**       Dummy routine to prevent marking of seleted entities.
**    PARAMETERS   
**       INPUT  : 
**				none
**				
**       OUTPUT :  
**				none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_nomark(prms)
int prms[4];
	{
	int ws;
	int segno;
	Gseghi h;
	static int debug = 0;
	int erase;

/* DEBUGGING */
	ws = prms[1];
	segno = prms[2];
	h = (Gseghi)prms[3];

	if (h == UG_HIGHLIGHTED)
		erase = 1;
	else
		erase = 0;

	if (debug)
	ug_viewws(ws, segno, erase);

	return(UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION    : Gcovalid ncl_colordev()
**       Determines whether or not device supports COLOR (hilighting).
**    PARAMETERS   
**       INPUT  : 
**				none
**				
**       OUTPUT :  
**				none
**    RETURNS      : 
**			UG_COLOUR  if device has color (hilighting)
**			UG_MONOCHROME  if device is monochrome
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gcoavail
ncl_colordev()
	{
	int i;
	Gcoavail c;
	UG_outwdt *outwdt;

/*
.....Find first active workstation.
*/
	for (i=0; i<ug_gksdesctbl.maxopws; i++) 
		{
		if (ug_gksstli.wsopen[i].connid != NULL &&
			ug_gksstli.wsopen[i].state==UG_ACTIVE)
			break;
		}
	outwdt = (*ug_gksstli.wsopen[i].wdtptr).outwdtpt;
	c = (*outwdt).cofac.coavail;
	return ((*outwdt).cofac.coavail);
	}
/*********************************************************************
**    E_FUNCTION    : int ncl_verify_pick2()
**       
**    PARAMETERS   
**       INPUT  : 
**				
**       OUTPUT :  
**				none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE;
**    SIDE EFFECTS : reset limit state to previous value.
**    WARNINGS     : none
*********************************************************************/
UD_DASTAT ncl_verify_pick2(pick)
UD_PPICKREC *pick;
{
	UM_PICKENT pent;
	int i,pick_type;
	UD_DASTAT status;
/*
.....Initialize routine
*/
	pick_type = ud_motion_pick_type();
/*
.....If in VERIFY MODE, prompt user to confirm pick.
*/
	if (NCL_pick_verify == UU_TRUE)
	{
		if (!pick_type)
			gssegdet(pick->pickpath[0], UG_UNDETECTABLE);
/*
.........Save off counter since call to ud_ldas() resets it.
*/
		status = DE_AGAIN;
		if (NCL_nopick_cnt == MAXVERCNT)
		{
			for (i = 0; i < NCL_nopick_cnt; i++)
				NCL_verify_list[i] = 0;
			NCL_nopick_cnt = 0;
		}
		if (!pick_type)
		{
			um_d_pickresolve(pick,1,&pent);
			NCL_verify_list[NCL_nopick_cnt] = um_get_pickkey(&pent,1);
		}
		else
			NCL_verify_list[NCL_nopick_cnt] = pick->pickpath[1];
		NCL_verify_segno[NCL_nopick_cnt] = pick->pickpath[0];
		NCL_nopick_cnt++;
	}
/*
.....Reset recursive flag for normal, proper exit.
*/
	verify_recursive = 0;
	return(status);
}

/*********************************************************************
**    E_FUNCTION    : int ud_reset_verify_list()
**       reset the verify list to normal
**    PARAMETERS   
**       INPUT  : 
**				
**       OUTPUT :  
**				none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE;
**    SIDE EFFECTS : reset limit state to previous value.
**    WARNINGS     : none
*********************************************************************/
ud_reset_verify_list()
{
	int i,segno;
	if (!ud_motion_pick_type())
	{
		for (i=0; i<NCL_nopick_cnt;i++)
		{
			segno = NCL_verify_segno[i];
			gssegdet(segno, UG_DETECTABLE);
			gsseghilite(segno, UG_NORMAL);
		}
	}
	NCL_nopick_cnt = 0;
	return 0;
}

/*********************************************************************
**    E_FUNCTION    : int ncl_verify_pick3()
**       
**    PARAMETERS   
**       INPUT  : 
**				
**       OUTPUT :  
**				none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE;
**    SIDE EFFECTS : reset limit state to previous value.
**    WARNINGS     : none
*********************************************************************/
UD_DASTAT ncl_verify_pick3(path0, path1)
int path0, path1;
{
	UM_PICKENT pent;
	int i,pick_type;
	UD_DASTAT status;
/*
.....Initialize routine
*/
	pick_type = ud_motion_pick_type();
/*
.....If in VERIFY MODE, prompt user to confirm pick.
*/
	if (NCL_pick_verify == UU_TRUE)
	{
		if (!pick_type)
			gssegdet(path0, UG_UNDETECTABLE);
		if (NCL_nopick_cnt == MAXVERCNT)
		{
			for (i = 0; i < NCL_nopick_cnt; i++)
				NCL_verify_list[i] = 0;
			NCL_nopick_cnt = 0;
		}
		if (!pick_type)
		{
			NCL_verify_list[NCL_nopick_cnt] = path0;
		}
		else
			NCL_verify_list[NCL_nopick_cnt] = path1;
		NCL_verify_segno[NCL_nopick_cnt] = path0;
		NCL_nopick_cnt++;
	}
/*
.....Reset recursive flag for normal, proper exit.
*/
	verify_recursive = 0;
	return 0;
}

/*********************************************************************
**    E_FUNCTION    : ncl_on_verify_segno(segno)
**          Checks to see if a segment is on the NCL_verify_segno list.
**       
**    PARAMETERS   
**       INPUT  : 
**				segno   = Segment to check to see if its on the Verify list.
**       OUTPUT :  
**				none
**    RETURNS      : 
**			UU_TRUE if segment is on Verify list.  UU_FALSE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ncl_on_verify_segno(segno)
int segno;
{
	int i;
/*
.....Determine if digs segment is on Verify list
*/
	for (i=0; i<NCL_nopick_cnt; i++)
		if (segno == NCL_verify_segno[i]) return(UU_TRUE);
	return(UU_FALSE);
}
