#include "usysdef.h"
#if UU_COMP == UU_CIM
/*********************************************************************
**    NAME         :  wscim.h
**       CONTAINS:
**       Include file for the "wscim*.c" driver files
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       wscim.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:09
*********************************************************************/

#ifndef WSCIMH

#include <sys/types.h>
#include <sys/time.h>
#include <window.h>
#include <disp/wn.h>
#include <disp/keymap.h>
#include "wscimdefs.h"

/** hangover from sun **/
#define WIN_NAMESIZE	20
struct bbox GFX_winrect;	

typedef struct {
		int wid;				/* workstation id of this workstation */
		UG_wdt *wdtptr;				/* pointer to workstation description table */
		int ttfd;				/* file descriptor if using serial line */
		struct bbox wrect;			/* contains size of gfx window */
		int winxmax,winymax;			/* used where DEVXMAX,DEVYMAX normally are */
		int windepth;				/* depth (# bit planes) of the parent window */
		struct bbox parent_wrect;		/* defines window rect of parent window */
		int parent_winfd;			/* UNIX File-Descriptor of parent window */
		char parent_windevname[WIN_NAMESIZE];	/* UNIX device name of parent */
		int parent_winnum;			/* window number taken from above dev name */
		struct bbox *gfx_winrect;		/* defines window rect of graphics window */
		int gfx_winfd;				/* UNIX File-Descriptor of graphics window */
		/* ptr to graphics window display */
		/**
		struct pixwin *gfx_pw;		
		**/
		cim_font *gfx_font;			/* ptr to a char font used in the gfx window */
		int char_wid,char_hgt;			/* size in pixels of one hardware char box */
		struct pixrect *gfx_mpr;		/* ptr to memory copy of gfx window display */
		struct wncursor default_curs;		/* default cursor definition */
		int ras_rules[7];			/* maps D.I. ras rules to sun raster ops */
		int curr_marker_index;			/* current marker color index */
		int curr_line_index;			/* current line color index */
		int curr_text_index;			/* current text color index */
		int curr_fa_index;			/* current fill-area color index */
		int prim_rasop;				/* output primitives raster-op (draw/erase) */
		struct {
			Gfloat scale;
			Gfloat dx,dy;
			} wsxform;			/* scale and dx,dy to map window to raster*/
		unsigned char r[256],g[256],b[256];	/* color-map segment values */
		short cms_size;				/* number entries in above color-map segment */
		unsigned char chomode;			/* set to 1 while "choice mode" in effect */
		unsigned char nocolor;			/* 1 if deleting or hiliting a seg */
		unsigned char areaflag;			/* used by sun_erase_seg. set to 1 by 
									sunflarearas and cell */
		unsigned char batching_on;		/* positive if batching is enabled; 0 if not */
		short bvec_cnt,bmrk_cnt;		/* counters for batching vectors & markers */
		} uw_cimdata;

/* device coordinate space is from (0,0) to (DEVXMAX,DEVYMAX) */
#define REDUCEX  10
#define REDUCEY  10
#define DEVXMAX (1023-REDUCEX)				/* device maximum x coordinate */
#define DEVYMAX (791-REDUCEY)				/* device maximum y coordinate */

/* screen will hold ROWMAX rows of COLMAX  characters each, when using
	the standard size alpha characters */
#define ROWMAX 50				/* max alpha rows */
#define COLMAX 100				/* max alpha columns */

#define NUMPETS 5				/* Number of available prompt-and-echo types */
#define NCHDEV 350				/* number of choice devices */
#define MAXCHOICES 50				/* max number of choices for any choice device */
#define MAX_FILLAREA_PTS 400			/* max no. of fill-area pts */
#define MAX_BATCHED 200				/* max no. of output primitives to be batched */

#define CIMRASOUT(x, y, w, h, op, ras) \
	setfunction(op); \
	rasterout(x, y, w, h, ras)

#define CIMRASIN(buff, w, h, op, x, y) \
	setfunction(op); \
	rasterin(x, y, w, h, buff, 1)

#define CIMNDCTODEV(np1,rasp) \
	(rasp)->x=uw_cim.wsxform.scale*(np1)->x+uw_cim.wsxform.dx+.5;\
	(rasp)->y=uw_cim.wsxform.scale*(np1)->y+uw_cim.wsxform.dy+.5

#define	CIMRASPUT(x,y,op) \
	setfunction(op); \
	moveto((short)(x*16),(short)(y*16)); \
	drawto((short)(x*16),(short)(y*16))

#define WSCIMH
#endif
#endif UU_COMP
