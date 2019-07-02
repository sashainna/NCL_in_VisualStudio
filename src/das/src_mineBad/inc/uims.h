/*********************************************************************
**    NAME         :  uims.h
**
**       CONTAINS:
**				User Interface Definition
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       uims.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:04
**
*********************************************************************/

#ifndef UIMSH

#ifdef UIMSPGM
#define EXT
#else
#ifdef __cplusplus 
#define EXT extern "C"
#else
#define EXT extern
#endif
#endif

#include "go.h"

/* define names for the 11 types of areas */

#define UD_GRAF 	0
#define UD_SPRMT 	1
#define UD_LPRMT 	2
#define UD_ERR 	3
#define UD_HLP 	4
#define UD_CHC 	5	 				/* icon menus that stay up */
#define UD_ICON 	6					/* non-pickable icons, like logos */
#define UD_SCROLL 7
#define UD_FORMS 	8
#define UD_MENU  	9					/* text menus  */
#define UD_ICONM 	10					/* Pop up icon menus	*/
#define UD_NTYPES 11

/* define the 3 kinds of UD_CHC icon areas */

#define UD_ARB 	0
#define UD_ROWCOL 1

#define UD_MAXAREAS 10 	/* the max number of each type of area */
#define UD_MAXICONS 50 	/* maximum number of icons in each array */
#define UD_MAXSCREENS 5 /* maximum number of screens in a layout	*/

typedef struct{
		char fname[40];		/* name of archive file  containing whole menu */
		int nchoice;			/* number of choices in menu */
		Gnrect *posn;			/* pointer to array of NDC positions of icons */
} UD_CONTENTS;

typedef struct {
	int color;						/* background color index of this area(0=none) */
	int bordercolor;				/* border color index of this area (0=none) */
	int contcolor;					/* color of contents, text in prompt areas */
	Gnrect posn;					/* coords corners of this area */
	int segno;						/* segment number of the border/background
											for this area.  */
	int devno;						/* For UD_CHC  areas, choice device 
											number after initialization */
	Gsegvis visible;				/* current status, VISIBLE or INVISIBLE. 
											Corresponds to ON or OFF */
	UD_CONTENTS cont;				/* different for each type of area */
} UD_AREA;							/* definition of one area */

typedef struct {					/* a single screen */
	Gdrect wswind;					/* workstation window for this screen */
	int noareas[UD_NTYPES];		/* number of areas of each type */
	UD_AREA *areas[UD_NTYPES];	/* pointer to array of areas of each type */
} UD_DUIMSDEF;

typedef struct {
	int nscreens;					/* number of UD_DUIMSDEF structs */
	UD_DUIMSDEF screen[UD_MAXSCREENS];
} UD_UIMS;

typedef struct {					/* current layout */
	int curr_lex_prompt_area;	/* current lex prompt area number */
	int curr_sem_prompt_area;	/* current semantic prompt area number */
	int curr_menu_area[10];		/* stack of menu areas. When another menu is
											requested by the menu traverser, the current
											top of stack + 1 is pushed onto the stack.
											Then the menu is placed on the area 
											specified by the top of this stack.  When 
											a menu is taken down, the stack is popped. */
	int curr_menu_stklen;		/* current length of menu stack */
	int curr_error_area;			/* current error msg area number */
	int curr_help_area;			/* current help msg area number */
	int curr_screen;				/* current screen */
} UD_CURLAYOUT;

	EXT UD_UIMS UD_duimsdeflt;			/* user interface */
	EXT UD_CURLAYOUT UD_curlayout;	/* current areas in use */

#define UD_CURLEXPRM_AREA \
	UD_duimsdeflt.screen[UD_curlayout.curr_screen].areas[UD_LPRMT] \
					[UD_curlayout.curr_lex_prompt_area].posn

#define UD_CURGRAF_AREA \
	UD_duimsdeflt.screen[UD_curlayout.curr_screen].areas[UD_GRAF][0].posn

#define UD_HELPWIN \
	(UD_duimsdeflt.screen[UD_curlayout.curr_screen].noareas[UD_HLP] == 0) ? \
	&(UD_duimsdeflt.screen[UD_curlayout.curr_screen].areas[UD_GRAF][0].posn) : \
	&(UD_duimsdeflt.screen[UD_curlayout.curr_screen].areas[UD_HLP][0].posn)

#define UD_NUM_HELPWIN \
	UD_duimsdeflt.screen[UD_curlayout.curr_screen].noareas[UD_HLP]

#define dinqgrafbrdr() \
	UD_duimsdeflt.screen[UD_curlayout.curr_screen].areas[UD_GRAF][0].bordercolor

/*   Inquire default ascii window background color */
#define dqwinback() \
	(UD_duimsdeflt.screen[UD_curlayout.curr_screen].noareas[UD_HLP] == 0) ? \
	UD_duimsdeflt.screen[UD_curlayout.curr_screen].areas[UD_GRAF][0].color : \
	UD_duimsdeflt.screen[UD_curlayout.curr_screen].areas[UD_HLP][0].color

#undef EXT
#define UIMSH

#endif
