/********************************************************************* 
**  NAME:  ws411x.h
**      Include file for GKS workstation for Tek 411x (ws411x.c)
**	 CONTAINS:
**
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       ws411x.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:08
*********************************************************************/

#ifndef WS41XXH

#include "gobas.h"

/*    4224 defines added for Tek 4224 running in 4125 emulation mode  */
/*    Added 3-19-88 my M. Gump  */

/*** 4224 emulating 4125 coordinates ***/
#define DEV4224XMAX 4095    /* maximum x coordinate (12 bit)-100 */
#define DEV4224YMAX 3071    /* device maximum y coordinate (12 bit)-80 */
#define RAS4224XMAX 1248    /* max x raster units  corresponding to DEVXMAX */
#define RAS4224YMAX 998	    /* max y raster units  corresponds to DEVYMAX */
#define ROW4224MAX 34	    /* max alpha rows */
#define COL4224MAX 85 	    /* max alpha columns */

/*** 4115 coordinates ***/
#define DEV4115XMAX 3995		/* maximum x coordinate (12 bit)-100 */
#define DEV4115YMAX 3196		/* device maximum y coordinate (12 bit)-80 */
#define RAS4115XMAX 1248		/* max x raster units  corresponding to DEVXMAX */
#define RAS4115YMAX 998			/* max y raster units  corresponds to DEVYMAX */
#define ROW4115MAX 62			/* max alpha rows */
#define COL4115MAX 156			/* max alpha columns */

/*** 4111 coordinates ***/
#define DEV4111XMAX 4095		/* maximum x coordinate (12 bit)-100 */
#define DEV4111YMAX 3071		/* device maximum y coordinate (12 bit)-80 */
#define RAS4111XMAX 1023		/* max x raster units  corresponding to DEVXMAX */
#define RAS4111YMAX 767			/* max y raster units  corresponds to DEVYMAX */
#define ROW4111MAX 48			/* max alpha rows */
#define COL4111MAX 126			/* max alpha columns */

/*** 4109 coordinates ***/
#define DEV4109XMAX 4095		/* maximum x coordinate (12-bit) */
#define DEV4109YMAX 3071
#define RAS4109XMAX 639			/* max x raster units  corresponding to DEVXMAX */
#define RAS4109YMAX 511			/* max y raster units  corresponds to DEVYMAX */
#define ROW4109MAX 32			/* max alpha rows */
#define COL4109MAX 80			/* max alpha columns */

/*** 4107 coordinates ***/
#define DEV4107XMAX 4095		/* maximum x coordinate (12-bit) */
#define DEV4107YMAX 3071
#define RAS4107XMAX 639			/* max x raster units  corresponding to DEVXMAX */
#define RAS4107YMAX 511			/* max y raster units  corresponds to DEVYMAX */
#define ROW4107MAX 32			/* max alpha rows */
#define COL4107MAX 80			/* max alpha columns */

#define SUPERSURF -1				/* super surface */
#define MENUSURF 2				/* menu surface */
#define GRAFSURF 1				/* graphics surface */
#define DIALSURF 3				/* dialog area surface */
#define TEK10BIT 1				/* Tek. 10-bit xy report format */
#define TEK12BIT 2				/* Tek. 12-bit xy report format */
#define NUMPETS 6					/* Number of available prompt-and-echo types */
#define NCHDEV 350				/* number of choice devices */
#define MAXCHOICES 50			/* max number of choices for any choice device */
#define NUMPUCKDEFS 5			/* number of defined tablet puck characters */
#define MSG_SEG_NUM 1			/* seg # of message segment */
#define PROMPT_SEG_MIN 2		/* beginning seg # of prompt segs */
		/* 1st 11 are for gprompt calls, rest for wsdev for device prompts */
#define SCROLL_SEG_MIN 713		/* min seg # for scrolling area background */
#define MENU_CURSOR_SEG 723		/* seg # of message segment */
#define PICK_CURSOR_SEG 724		/* seg # of pick cursor */
#define LOC_CURSOR_SEG 725		/* seg # of locator cursor*/
#define TMPSEG 726				/* tmp seg for 411xopnseg */
#define DRAGSEG 727				/* start of 10 locator dragging segments */
#define EMPTYSEG 737				/* an empty seg, for 411xtrk, 411xpik */
/* define segment and pick-id's for menus */
#define GMENUSEGMIN 738			/* menu segments start here */
#define MAXMENUSEGS 2			/* max number of segments in a menu */
#define D_PKID_UP_ARROW (MAXCHOICES+1)
#define D_PKID_DN_ARROW (MAXCHOICES+2)
#define D_PKID_CHOOSE_IT (MAXCHOICES+3)
#define D_PKID_BORDERS (MAXCHOICES+4)
#define GSEGMIN (MAXMENUSEGS*NCHDEV+GMENUSEGMIN)	/* min graphics segment # */
/*
.....Added for number of function keys
.....Bobby  -  10/22/92
*/
#define NFKEYS 48
typedef struct {
		int gin_requires_echo;		/* if != 0, terminal expects gin-mode echo */
		int menu_bkgd_index;			/* color index for the background of the menu*/
		int menu_text_index;			/* color index for the menu title & prompt */
		int menu_item_index;			/* color index for all the menu item entries */
		int num_bit_planes;			/* number of graphics bit planes available */
		int num_menu_planes;			/* number of menu,prompt, etc. planes */
		int menusegs[NCHDEV];		/* number segs in each menu, or zero */
		char puckdefs[NUMPUCKDEFS];/* char-code definitions for tablet puck */
		Gfloat menu_char_hgt;			/* character height for menu text */
		int curr_line_style;			/* current line style */
		int curr_line_index; 		/* current line color index */
		int curr_marker_type;		/* current marker type */
		int curr_text_index;			/* current text color index */
		int curr_marker_index;		/* current marker color index */
		int curr_view_surf;			/* current view surf (GRAFSURF or MENUSURF)*/
		int eraseon;					/* 1=erase color on now */
		int wid;							/* workstation id of this workstation */
		struct {
			Gfloat scale; Gfloat dx,dy;
		} wsxform;	/* scale and dx,dy to map window to raster*/
		struct {
			Gfloat scalex,scaley,dx,dy;
			int flag;
		} menuxform;
		int xflag;						/* 1=redraw required from host */
		int ttfd;						/* local file-descriptor for serial I/O line */
		int segopen;					/* no. of open graphics segment, or -1 */
		int pick_loc_mode;			/* cursor segment number */
		int scrollup;					/* length of scroll area stack */
		int ras;							/* 1=8bit raster image mode */
		int fkeyflag;					/* 1 = use gwhich_trigger (1 char fctn keys), 
												3=use gwhich_trigger3 (3 char fctn keys) */
		int model;						/* model of terminal. -1=unknown. */
		int devxmax,devymax;			/* device coordinate extent */
		int rasxmax,rasymax;			/* raster coordinate extent */
		int rowmax,colmax;			/* alpha rows, cols */
		int gottablet;					/* 0=no tablet, 1=tablet */
		int rasht;						/* current tek character height */
		int twheel_loc_devno;
		int twheel_pick_devno;
		int tab_loc_devno;
		int tab_pick_devno;
		unsigned char ansienbl;		/* 1=dialog area is enabled */
		Gcobundl *savvlt;				/* address of where vlt is saved in ras mode */
	} uw_411xdat;
#define BYPASSCANCELCHAR 0
#define EOMCHAR1 13
#define EOMCHAR2 10
#define EOLCHAR 10
/* if UU_DEBUG, ws411xc.c will compile. Otherwise include ws411xc.h */
#ifdef UU_DEBUGOFF
#include "ws411xc.h"
#endif

#define WS41XXH
#endif
