/********************************************************************* 
**  NAME:  ws4014em.h
**
**      Include file for GKS workstation for 4014em
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       ws4014em.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:08
**
**  PARAMETERS   
**      INPUT:  none 
**      OUTPUT: none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/

#ifndef WS4014EMH

#define DEVXMAX 4096				/* maximum x coordinate (12 bit) */
#define DEVYMAX 3120				/* device maximum y coordinate (12 bit) */
/*~~~~ #define DEVXMAX 1023				/* maximum x coordinate (12 bit) */
/*~~~~ #define DEVYMAX 767				/* device maximum y coordinate (12 bit) */
#define RASXMAX 1280				/* max x raster units */
#define RASYMAX 1024				/* max y raster units */
#define ROWMAX 40					/* max alpha rows */
#define COLMAX 102				/* max alpha columns */
#define SUPERSURF -1				/* super surface */
#define MENUSURF 2				/* menu surface */
#define GRAFSURF 1				/* graphics surface */
#define DIALSURF 3				/* dialog area surface */
#define TEK10BIT 1				/* Tek. 10-bit xy report format */
#define TEK12BIT 2				/* Tek. 12-bit xy report format */
#define NUMPETS 5					/* Number of available prompt-and-echo types */
#define MAXCHOICES 50			/* max number of choices for any choice device */
#define NUMPUCKDEFS 5			/* number of defined tablet puck characters */
#define NUMFKEYDEFS 8			/* number of defined tablet puck characters */
#define TEK_ALPHA_MODE 0		/* Tektronix 4014 alphanumeric mode */
#define TEK_GRAPH_MODE 1		/* Tektronix 4014 graphics mode */
#define TEK_POINT_MODE 2		/* Tektronix 4014 point mode */
/* the following define segment offsets for various pieces of the menu */
#define D_SEG_TITLE 1			/* menu seg. offset for menu title */
#define D_SEG_PROMPT 2			/* menu seg. offset for "choice:" prompt */
#define D_SEG_BORDERS 3			/* menu seg. offset for borders and background */
#define D_SEG_UP_ARROW 4		/* menu seg. offset for "up-arrow" menu icon */
#define D_SEG_DN_ARROW 5		/* menu seg. offset for "down-arrow" menu icon */
#define D_SEG_CHOOSE_IT 6		/* menu seg. offset for "choose-it" menu icon */
#define D_SEG_END 6
/* End of offset defs. --   D_SEG_END  must always be at end of this list */
#define MSG_SEG_NUM (MAXCHOICES+D_SEG_END+1)	/* seg # of message segment */
#define MENU_CURSOR_SEG (MSG_SEG_NUM+1)	/* set # of message segment */
#define PICK_CURSOR_SEG (MSG_SEG_NUM+2)	/* set # of pick cursor */
#define LOC_CURSOR_SEG (MSG_SEG_NUM+3)		/* set # of locator cursor*/
#define MAXMENUSEGS (MAXCHOICES+10)			/* spacing of menu segments */
#define GSEGMIN (MAXMENUSEGS*40+1)			/* min graphics segment number*/
#define MENU_BKGD_INDEX 1		/* color index for the background of the menu */
#define MENU_TEXT_INDEX 2		/* color index for the menu title & prompt */
#define MENU_ITEM_INDEX 3		/* color index for all the menu item entries */
typedef struct {
		int menusegs[40];			/* number segs in each menu, or zero */
		char puckdefs[NUMPUCKDEFS];
		float menu_char_hgt;
		int curr_line_style;			/* current line style */
		int curr_line_index; 		/* current line color index */
		int curr_marker_type;		/* current marker type */
		int curr_text_index;			/* current text color index */
	} g4014dat;

#define WS4014EMH
#endif
