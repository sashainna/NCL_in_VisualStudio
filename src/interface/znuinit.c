/*********************************************************************
**
**    NAME         :  znuinit.c
**
**    CONTAINS:
**              znu_init_kb - init keyboard
**              znu_init_fk - init function keys
**              znu_init_ms - init mouse/puck
**              znu_init_tb - init data tablet
**              znu_init_runvars - init runtime variables
**              znu_init_units - init units from runtime variables
**              znu_copyright - display copyright notice.
**              znu_init_machinetype - init NCL_machine variable for Unibase
**                                load checks.
**
**    COPYRIGHT 1987 (c) Mills Data Systems Co. Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       znuinit.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       10/27/16 , 12:08:47
*********************************************************************/
#include "usysdef.h"
#include "driver.h"
#include "xenv1.h"
#include "mdunits.h"
#include "mfort.h"
#include "nclfc.h"
#include "nclver.h"
#include "uhep.h"		/* To include messages from iunihep.msg */
#include "dsubcom.h"	/* Defines UD_chctable[], etc */

char *nisstr;			/* Run time variable, determines NIS or NONNIS */
int U_application;		/* Run time variable to determine default sys */
int NCLHOST = 0;			/* which graphics device: */

void znu_init_units(),znu_init_machinetype();

/*********************************************************************
**    E_FUNCTION     :  int znu_init_kb()
**       Initialization keyboard for NCL502
**
**    PARAMETERS   
**       INPUT  : 
**          fptr = pointer to UD_chctable[0]
**          nstr = character string != NULL if NIS active
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : Defines keyboard layout interface
**    WARNINGS     : none
*********************************************************************/
void znu_init_kb(fptr, nstr)
int (**fptr)();
char *nstr;
	{
	extern int uz_user_key();

	*fptr = uz_user_key;
	return;
	}
/*********************************************************************
**    E_FUNCTION     :  int znu_init_fk()
**       Initialization function keys for NCL502
**
**    PARAMETERS   
**       INPUT  : 
**          fptr = pointer to UD_chctable[1]
**          nstr = character string != NULL if NIS active
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : Defines function key interface
**    WARNINGS     : none
*********************************************************************/
int 
znu_init_fk(fptr, nstr)
int (**fptr)();
char *nstr;
{
	extern int uz_user_fkey();
	*fptr = uz_user_fkey;
	return(0);
}
/*********************************************************************
**    E_FUNCTION     :  int znu_init_ms()
**       Initialization mouse/puck for NCL502
**
**    PARAMETERS   
**       INPUT  : 
**          fptr = pointer to UD_chctable[2]
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : Defines mouse or puck interface
**    WARNINGS     : none
*********************************************************************/
void znu_init_ms(fptr)
int (**fptr)();
{
	extern int ipuck4();		/* puck interface - TEK TERMINALS ONLY */
	extern int imouse3();		/* mouse routines - WORKSTATIONS */

	*fptr = imouse3;
}

/*********************************************************************
**    E_FUNCTION     :  int znu_init_tb()
**       Initialization buttons and dials for NCL502
**
**    PARAMETERS   
**       INPUT  : 
**          fptr = pointer to UD_chctable[3]
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : Defines data tablet interface
**    WARNINGS     : none
*********************************************************************/
int 
znu_init_tb(fptr)
int (**fptr)();
	{
/*	extern int zncltb();
	*fptr = zncltb;*/

	extern int uz_user_button();
	*fptr = uz_user_button;
	return(0);
	}

/*********************************************************************
**    E_FUNCTION     :  int znu_init_runvars()
**       Initialization run-time variables for NCL502
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : sets FORTRAN flags, UU_application
**    WARNINGS     : none
*********************************************************************/
void znu_init_runvars()
	{
	char *ux_getenv(), *p;
    UM_int2 ival;

	/** determine which graphics terminal we are using **/
	p = ux_getenv("ws", UX_NPRTERRS);
	if (p != UU_NULL)
		{
		if (!(strcmp(p,"host")) || !(strcmp(p, "HOST")))
			NCLHOST = 0;
		else if (!(strcmp(p, "tty")) || !(strcmp(p, "TTY")))
			NCLHOST = 1;
		else if (!(strcmp(p, "410x")) || !(strcmp(p, "410X")))
			NCLHOST = 2;
		else if (!(strcmp(p, "411x")) || !(strcmp(p, "411X")))
			NCLHOST = 3;
		}
	else NCLHOST = 0;
	
/*  NCL: -- Set INPUT UNITS            -- */
	znu_init_units();

/*  NCL: -- Set U_STOP variable for *STOP -- */
	p = ux_getenv("U_STOP",UX_NPRTERRS);
	if (p != UU_NULL)
		{
		if (ud_strcomp(p,"CM") == -1 || (ud_strcomp(p,"cm") == -1))
			{
			ival = 1;
			}
		else if (ud_strcomp(p,"BC") == -1 || (ud_strcomp(p,"bc") == -1))
			{
			ival = 0;
			}
		}
	else 
		ival=0;

	setstp(&ival);

/*  
.....Define defaults for PICK MODALS - Roberta Zorzynski
*/
	ncl_init_pick();

	/* Just to make sure the variable NCL_machine gets set. */
	znu_init_machinetype();

	return;
	}
/*********************************************************************
**    E_FUNCTION :  znu_copyright()
**       Display copyright notice.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void znu_copyright()
	{
	char *uu_uprompt0(), *p;

	if (sizeof(char *) == 8)
	    p = uu_uprompt0(UU_SIGNON, 3);			/* copyright notice */
	else
		p = uu_uprompt0(UU_SIGNON, 4);			/* copyright notice */
	ud_prmerr(p);
	return;
	}
/*********************************************************************
**    E_FUNCTION :  znu_null_devs()
**       Set input devices inactive
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
znu_null_devs()
{
	extern int uz_user_key();

/*
.....User defined function key routine
.....disables function keys at signon
.....No action required here
.....Bobby  -  10/8/92
*/

/* ICONS */
	UD_chctable[5] = uz_user_key;
	
	return(0);
}

/*********************************************************************
**    E_FUNCTION     :  int znu_init_units()
**       Initialize units from run-time variables
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : sets FORTRAN flags, UU_application
**    WARNINGS     : none
*********************************************************************/
void znu_init_units()
	{
	char *ux_getenv(), *p;
    UM_int2 ival, iunit;

/*  NCL: -- Set INPUT UNITS            -- */
	ival=0;
	iunit=264;
	p = ux_getenv("U_UNITS",UX_NPRTERRS);
	if (p != UU_NULL)
		{
		if (ud_strcomp(p, "MM") == -1 || (ud_strcomp(p, "mm") == -1))
			{
			um_setunits(UM_MM);
			ival=1;
			}
		else
			um_setunits(UM_INCH);
		}
	else
		um_setunits(UM_INCH);

	/* added to set the unit flag and TOLER and MAXDP flags. kathy */
	setifl(&iunit, &ival);
	setsc(&ival);

	return;
	}
/*********************************************************************
**    E_FUNCTION     :  int znu_init_machinetype()
**       Initialize variable NCL_machine for Unibase load checks.
**		NOTE: this was taken from xio/xf1files.c: uxi_put_file_hdr()
**            It is possible that this routine should change with
**            future releases of MPE.
**      FOR BATCH - 'cause uxi_put_file_hdr() also initializes this
**            variable and always gets called while initializing
**            the error file "UU_FERROR" (see init file) for INTERACTIVE.
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : sets NCL_machine variable.
**    WARNINGS     : none
*********************************************************************/
void znu_init_machinetype()
	{
	strcpy(NCL_machine,"*MACHINE*UNKNOWN*\n");
#if UU_COMP==UU_SUN
#if UU_SUNTYPE != UU_SUN_SUN4
	strcpy(NCL_machine,"****SUN****\n");
#endif
#if UU_SUNTYPE == UU_SUN_SUN4
	strcpy(NCL_machine,"****SUN4***\n");
#endif
#endif
#if UU_COMP==UU_CIM
	strcpy(NCL_machine,"**CIMLINC**\n");
#endif
#if UU_COMP==UU_PYRAMID
	strcpy(NCL_machine,"**PYRAMID**\n");
#endif
#if UU_COMP==UU_IRIS
	strcpy(NCL_machine,"**SG*IRIS**\n");
#endif
#if UU_COMP==UU_IRIS4D
#ifdef UU_RS6000
	strcpy(NCL_machine,"*IBM*RS/6000*\n");
#else
	strcpy(NCL_machine,"**SG*IRIS4D**\n");
#endif
#endif
#if UU_COMP==UU_HPUX
	strcpy(NCL_machine,"**HP*PA-RISC*\n");
#endif
#if UU_COMP==UU_WINNT
	strcpy(NCL_machine,"**WINDOWS*NT*\n");
#endif
/*
.....native WNT
.....3/15/00
*/
#if UU_COMP==UU_WIN2K
	strcpy(NCL_machine,"**WINDOWS*NT*\n");
#endif
#if UU_COMP==UU_386
	strcpy(NCL_machine,"**COMPAQ386**\n");
#endif
#if UU_COMP==UU_TEK
	strcpy(NCL_machine,"**TEK*4336**\n");
#endif
#if UU_COMP==UU_RIDGE
	strcpy(NCL_machine,"**RIDGE**\n");
#endif
#if UU_COMP==UU_APOLLO
	strcpy(NCL_machine,"**APOLLO**\n");
#endif
#if UU_COMP==UU_VAXVMS
#ifdef UU_ALPHA
	strcpy(NCL_machine,"*ALPHA/VMS*\n");
#else
	strcpy(NCL_machine,"**VAXVMS**\n");
#endif
#endif
#if UU_COMP==UU_VAXULTRIX
	strcpy(NCL_machine,"**VAXULT**\n");
#endif
#if UU_COMP==UU_IBMAT
	strcpy(NCL_machine,"***IBMAT***\n");
#endif
#if UU_COMP==UU_TEK6130
	strcpy(NCL_machine,"**TEK6130**\n");
#endif
#if UU_COMP==UU_MASSCOMP
	strcpy(NCL_machine,"**MASSCOMP**\n");
#endif
#if UU_COMP==UU_DECUNIX
	strcpy(NCL_machine,"**DECUNIX**\n");
#endif
	return;
	}
