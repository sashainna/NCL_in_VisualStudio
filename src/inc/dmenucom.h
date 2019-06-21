/*********************************************************************
**
**    NAME         :  dmenucom.h
**
**       CONTAINS:
**       	global data for menu subsystem
**
**    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       dmenucom.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:13
**
*********************************************************************/
/*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!                                                             !!!!!!
!!!!!!   NOTE:                                                     !!!!!!
!!!!!!      Any changes made to this module necessitate that       !!!!!!
!!!!!!      the module das/d5mudsp.c be recompiled.                !!!!!!
!!!!!!                                                             !!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*/

#ifndef DMENUCOMH

#include "usysdef.h"
/*
.....put include files before define EXT
.....because in those header file, it may 
.....change EXT define
.....Yurong
*/
#include "dtuto.h"
#include "ustdio.h"
#include	"gobas.h"
#include	"alloc.h"

#ifdef MENUPGM
#define EXT
#else
#define EXT extern
#endif

/**   --- menu constants ---   **/

#define UDI_MENU_DEPTH 	50			/* maximum depth of menu tree */
#define UDI_MU_WARN 		STACK_MAX-5	/* nesting depth when warning starts */
#define UDI_SUB_DEPTH 	50			/* maximum depth of menu subroutine stack */
#define UDI_NUM_MENU 	100		/* maximum number of menu nodes */
#define UDI_MENU_LEN 	24			/* maximum number of char per menu line */
#define UDI_MENU_STR 	600		/* maximum number of menu lines */
#define UDI_MENU_ITEM 	50			/* maximum menu item number */
#define UDI_MENU_PATH 	15			/* maximum goto path	*/
#define UDI_MUEX_DEPTH 	STACK_MAX	/* menu exit stack depth */
#define UDI_MUCALL		0
#define UDI_MUGOTO		1

/*	-- menu string table data structure -- */

	typedef	struct
	{
 		char	*sym;					/* GOTO or CALL string  */
 		int	path[UDI_MENU_PATH];  /* array for all the information 
											path[0] : 0 - CALL, 1 - GOTO
											path[1] : 0 - leaf node, 1 - non-leaf node
											path[2] : item number of the node in the menu
											path[3] : number of paths for GOTO,
												 		 menu number for CALL
											others  : menu number for the GOTO path */
	}	UDI_MUSTR;

/*	-- structure for main menu array -- */

	typedef struct
	{
	 UU_REAL		ll[2];			/* the lower left corner	*/
	 UU_REAL		ur[2];			/* the upper right corner  */
	} UDI_MUAREA;

	typedef struct
	{
		int	menuind;				/* index into text string array of this menu */
		int	menuno;				/* menu number of this node */
		Gnrect	*muarea;		/* pointer to the pull down menu area	*/
		char	*tutotb;			/* pointer to tutorial structure for this node */
		char	*iconnm;				/* pointer to icon file name */
		int	infor[UDI_MENU_ITEM];	/* sub menu information for this node */
	}	UDI_MUJTB;

/*	-- Menu Stack Structure -- */

	typedef struct
	{
		int menuno;							/* menu number pushed */
		int choiceno;						/* choice number of this array */
	} UD_MENU_STACK_REC;

/* -- Global Menu Navigator Table -- */

	typedef struct
	{
		int loopflag;			/* main navigator loop control */
		int nav_mode;			/* navigation mode flag */
		UD_MENU_STACK_REC *menu_stk;			/* base of menu traverse stack */
		int *menu_stk_ptr;	/* pointer into internal menu traverse stack */
		int *base;				/* base device number */
	} UD_MUEX_STRUCT;

/*	-- Tutorial Mode -- */

	typedef enum
	{
		UD_TUT_ON,
		UD_TUT_OFF
	} UD_TUTMODE;

#ifdef MENUPGM
	int UDI_nav_mode = 1;				/* menu navigator dispatcher switch */
	int UDI_muwhich = 0;					/* number of times "ud_munavg" being called 
													so that we don't navigate up out the 
													to level menu */
	UD_TUTMODE UD_tut_mode = UD_TUT_OFF;	/* tutorial mode on / off flag */
	char *UD_tut_ptr = NULL;		/* active tutorial pointer */
	int UDI_Muex_ptr = 1;				/* stack pointer */
	int UD_menu_init = 1;				/* menu init flag (0=init mode) */
	int UD_sstrc_ptr = 0;				/* subsystem trace stck pointer */
#else
	EXT int UDI_nav_mode;				/* menu navigator dispatcher switch */
	EXT int UDI_muwhich;					/* number of times "ud_munavg" called */
	EXT UD_TUTMODE UD_tut_mode;		/* tutorial mode on / off flag */
	EXT char *UD_tut_ptr;			/* active tutorial pointer */
	EXT int UDI_Muex_ptr;				/* stack pointer */
	EXT int UD_menu_init;				/* menu init flag (0=init mode) */
	EXT int UD_sstrc_ptr;				/* subsystem trace stck pointer */
#endif

	EXT int UDI_mu_base;					/* menu base number  */
	EXT int UDI_pop_base;				/* pop-up menu base number  */
/* NCL: enlarged to accomodate more than 32 subsystems */
/* NCL: 5-18-89 roberta */
	EXT int UDI_muload[2];					/* bit flag to signal menu loading */
	EXT int UDI_bitcont;					/* bit position for each menu */
	EXT UD_MUEX_STRUCT UDI_Muex_stk[UDI_MUEX_DEPTH];	/* menu traversal stack */
	EXT char *UD_sstrc[UDI_MUEX_DEPTH];	/* traceback stack */

#define DMENUCOMH
#undef EXT

#endif
