
/********************************************************************* 
**  NAME:  ws433x.h
**      Include file for GKS workstation for Tek 433x (ws433x.c)
**   CONTAINS:
**
**  COPYRIGHT  1984  UNICAD, Inc.

**   MODULE NAME AND RELEASE LEVEL                                 
**       ws433x.h , 25.1
**   DATE AND TIME OF LAST MODIFICATION                            
**       04/29/15 , 15:07:08
*********************************************************************/

#ifndef WS433XH

#include <sti_cbind.h>
#include "gobas.h"

/*** 433X coordinates ***/
#define FALSE 0
#define TRUE  1
#define DEV433XXMAX 4095    /* maximum x coordinate (12 bit)-100 */
#define DEV433XYMAX 3276    /* device maximum y coordinate (12 bit)-80 */
#define RAS433XXMAX 1248    /* max x raster units  corresponding to DEVXMAX */
#define RAS433XYMAX 998      /* max y raster units  corresponds to DEVYMAX */
#define ROW433XMAX 62      /* max alpha rows */
#define COL433XMAX 156      /* max alpha columns */

/*  fast icon file data types */
#define UW_433X_COLORREP 1
#define UW_433X_SETPOPCLR 2
#define UW_433X_RASLINE 3 
#define UW_433X_FILLRAS 4
#define UW_433X_RASTEXT 5
#define UW_433X_PICKID 6
#define UW_433X_CELLRAS 7

#define SUPERSURF -1        /* super surface */
#define MENUSURF 2        /* menu surface */
#define GRAFSURF 1        /* graphics surface */
#define DIALSURF 3        /* dialog area surface */
#define NUMPETS 6          /* Number of available prompt-and-echo types */
#define NCHDEV 350        /* number of choice devices */
#define MAXCHOICES 50      /* max number of choices for any choice device */
#define NUMPUCKDEFS 3      /* number of defined puck characters */
#define MSG_SEG_NUM 1      /* seg # of message segment */
#define PROMPT_SEG_MIN 2    /* beginning seg # of prompt segs */
#define FKEY1 128           /* keyboard trigger for fist defn func key */
#define MKEY1 -155          /* trigger for left mouse key */
    /* 1st 11 are for gprompt calls, rest for wsdev for device prompts */
#define SCROLL_SEG_MIN 713    /* min seg # for scrolling area background */
#define MENU_CURSOR_SEG 723    /* seg # of message segment */
#define PICK_CURSOR_SEG 724    /* seg # of pick cursor */
#define LOC_CURSOR_SEG 725    /* seg # of locator cursor*/
#define TMPSEG 726        /* tmp seg for 433xopnseg */
#define DRAGSEG 727        /* start of 10 locator dragging segments */
#define EMPTYSEG 737        /* an empty seg, for 433xtrk, 433xpik */
/* define segment and pick-id's for menus */
#define GMENUSEGMIN 738      /* menu segments start here */
#define MAXMENUSEGS 2      /* max number of segments in a menu */
#define D_PKID_UP_ARROW (MAXCHOICES+1)
#define D_PKID_DN_ARROW (MAXCHOICES+2)
#define D_PKID_CHOOSE_IT (MAXCHOICES+3)
#define D_PKID_BORDERS (MAXCHOICES+4)
#define GSEGMIN (MAXMENUSEGS*NCHDEV+GMENUSEGMIN)  /* min graphics segment # */

#define INTSIZEOF(x) ((sizeof(x)+sizeof(int)-1)/sizeof(int))
#define INTSIZ(x) ((x+sizeof(int)-1)/sizeof(int))

typedef struct {
    int gin_requires_echo;    /* if != 0, terminal expects gin-mode echo */
    int menu_bkgd_index;      /* color index for the background of the menu*/
    int menu_text_index;      /* color index for the menu title & prompt */
    int menu_item_index;      /* color index for all the menu item entries */
    int num_bit_planes;      /* number of graphics bit planes available */
    int num_menu_planes;      /* number of menu,prompt, etc. planes */
    int menusegs[NCHDEV];    /* number segs in each menu, or zero */
    char puckdefs[NUMPUCKDEFS];/* char-code definitions for puck */
    Gfloat menu_char_hgt;      /* character height for menu text */
    int curr_line_style;      /* current line style */
    int curr_line_index;     /* current line color index */
    int curr_line_width;     /* current line width */
    int curr_marker_type;    /* current marker type */
    int curr_text_index;      /* current text color index */
    int curr_marker_index;    /* current marker color index */
    int curr_view_surf;      /* current view surf (GRAFSURF or MENUSURF)*/
    int eraseon;          /* 1=erase color on now */
    int wid;              /* workstation id of this workstation */
    struct {
      Gfloat scale; Gfloat dx,dy;
    } wsxform;  /* scale and dx,dy to map window to raster*/
    struct {
      Gfloat scalex,scaley,dx,dy;
      int flag;
    } menuxform;
    int xflag;            /* 1=redraw required from host */
    int ttfd;            /* local file-descriptor for serial I/O line */
    int segopen;          /* no. of open graphics segment, or -1 */
    int pick_loc_mode;      /* cursor segment number */
    int scrollup;          /* length of scroll area stack */
    int ras;              /* 1=8bit raster image mode */
    int fkeyflag;          /* 1 = use gwhich_trigger (1 char fctn keys), 
                        3=use gwhich_trigger3 (3 char fctn keys) */
    int model;            /* model of terminal. -1=unknown. */
    int devxmax,devymax;      /* device coordinate extent */
    int rasxmax,rasymax;      /* raster coordinate extent */
    int rowmax,colmax;      /* alpha rows, cols */
    int gottablet;          /* 0=no tablet, 1=tablet */
    int rasht;            /* current tek character height */
    int twheel_loc_devno;
    int twheel_pick_devno;
    int tab_loc_devno;
    int tab_pick_devno;
    int dbbfr;
    int *icstr;
    int icon;
    int iclen; 
    int choicex;
    int choicey;
    unsigned char ansienbl;    /* 1=dialog area is enabled */
    Gcobundl *savvlt;        /* address of where vlt is saved in ras mode */
  } uw_433xdat;


#define BYPASSCANCELCHAR 0
#define EOMCHAR1 13
#define EOMCHAR2 10
#define EOLCHAR 10

#ifdef UW_433XMAIN
#define EXT
#else
#define EXT extern
#endif

/* Workstation variables */
EXT uw_433xdat uw_433x;

#undef EXT
#define WS433XH
#endif
