/*********************************************************************
**    NAME         :  wssgp.h
**       CONTAINS:
**       Include file for the "wssgp*.c" driver files
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       wssgp.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:22
*********************************************************************/

#ifndef WSSGPH

#include "wssgpDI.h"
#include <sys/types.h>
#include <pixrect/pixrect.h>
#include <sunwindow/win_cursor.h>
#include <sunwindow/rect.h>
#include <sunwindow/pixwin.h>
#include <sunwindow/win_struct.h>
#include <sys/time.h>
#include <sunwindow/win_input.h>
#include <suntool/frame.h>
#include <suntool/canvas.h>
#include "zsysdep.h"

#define NOTRANS 15

typedef struct {
		int wid;							/* workstation id of this workstation */
		UG_wdt *wdtptr;				/* pointer to workstation description table */
      int ttfd;                  /* file descriptor if using serial line */
		struct rect wrect;			/* contains size of gfx window */
		int winxmax,winymax;			/* used where DEVXMAX,DEVYMAX normally are */
		int windepth;					/* depth (# bit planes) of the parent window */
		struct rect parent_wrect;	/* defines window rect of parent window */
		int parent_winfd;				/* UNIX File-Descriptor of parent window */
		char parent_windevname[WIN_NAMESIZE];	/* UNIX device name of parent */
		int parent_winnum;			/* window number taken from above dev name */
		struct rect gfx_winrect;	/* defines window rect of graphics window */
		int gfx_winfd;					/* UNIX File-Descriptor of graphics window */
		struct pixwin *gfx_pw;		/* ptr to graphics window display */
		struct pixfont *gfx_font;	/* ptr to a char font used in the gfx window */
		int char_wid,char_hgt;		/* size in pixels of one hardware char box */
		struct pixrect *gfx_mpr;	/* ptr to memory copy of gfx window display */
		struct cursor default_curs;/* SUN-Windows default cursor definition */
		struct inputmask inp_enab_mask;	/*masks enabling various kinds of input*/
		int ras_rules[7];				/* maps D.I. ras rules to sun raster ops */
		int curr_color;				/* currently set color index */
		int curr_width;				/* currently set line width */
		int curr_style;				/* currently set line style */
		int curr_ntran;				/* currently set normtran */
		int prim_rasop;				/* output primitives raster-op (draw/erase) */
		struct {
			Gfloat scale;
			Gfloat dx,dy;
			} wsxform;					/* scale and dx,dy to map window to raster*/
		unsigned char r[256],g[256],b[256];	/* color-map segment values */
		short cms_size;				/* number entries in above color-map segment */
		unsigned char nocolor;		/* 1 if deleting or hiliting a seg */
		GP1_handle gp1;
		Matrix3df wtran_mat[NOTRANS + 1];	/* world transformation matrices */
		Matrix3df modxf;				/* current modelling transformation matrix */
		Matrix3df ntran_mat[NOTRANS + 1];			/* normtrans */
		Matrix3df K[NOTRANS + 1];		/* clipping transformation matrices */
		int canx,cany;					/* upper left corner of canvas */
		Girect sc[NOTRANS + 1];
		Point3df scale[NOTRANS + 1],offset[NOTRANS + 1];	/* 3d viewports */
		int redraw;						/* redraw flag */
		int lockcount;
		Frame frame;
		int pickras;					/* flag to denote picking raster geometry */
		int dynamic;					/* flag to denote dynamic transforms */
		Canvas canvas;					/* needed for setting GRAB_ALL_INPUT */
		} uw_sgpdata;

/* device coordinate space is from (0,0) to (DEVXMAX,DEVYMAX) */
#define DEVXMAX 1023				/* device maximum x coordinate */
#define DEVYMAX 799				/* device maximum y coordinate */

/* screen will hold ROWMAX rows of COLMAX  characters each, when using
	the standard size alpha characters */
#define ROWMAX 34					/* max alpha rows */
#define COLMAX 80				/* max alpha columns */

#define NUMPETS 6					/* Number of available prompt-and-echo types */
#define NCHDEV 350				/* number of choice devices */
#define MAXCHOICES 50			/* max number of choices for any choice device */
#define MAX_FILLAREA_PTS 400	/* max no. of fill-area pts */
#define MAX_BATCHED 200			/* max no. of output primitives to be batched */

#define SGPRASLINE(x1,y1,x2,y2) \
	pw_vector(uw_sgp.gfx_pw,x1,uw_sgp.winymax-(y1),x2,uw_sgp.winymax-(y2),\
					uw_sgp.prim_rasop,uw_sgp.curr_line_index)

#define SGPNDCTODEV(np1,rasp) \
	(rasp)->x=uw_sgp.wsxform.scale*(np1)->x+uw_sgp.wsxform.dx + 0.499;\
	(rasp)->y=uw_sgp.wsxform.scale*(np1)->y+uw_sgp.wsxform.dy + 0.499

/* indices into the graphics context for matrix transforms */
#define CURTRAN_INDEX 0
#define WTRAN_INDEX 1
#define MOD_INDEX 2
#define NTRAN_INDEX 3
#define K_INDEX 4
#define PIK_INDEX 5

#define ug_linetype(typ) zbytecp(ug_gksstli.curprats.lnbundl.type,*(typ))
#define ug_linewidth(wd) ug_gksstli.curprats.lnbundl.width = (wd)
#define ug_textfp(fopr) ug_gksstli.curprats.txbundl.fp.font = (fopr)->font; \
							 	ug_gksstli.curprats.txbundl.fp.prec = (fopr)->prec
#define ug_charexp(e) ug_gksstli.curprats.txbundl.expn = (e)
#define ug_txplane(tpvc) zbytecp(ug_gksstli.curprats.txpvec, *(tpvc))
#define ug_charup3(upvec) zbytecp( ug_gksstli.curprats.txuv, *(upvec))
#define ug_charup(up) { \
	extern Gfloat ug_chhtsclflag[]; \
	for (i=0; i<UG_MAXNTRAN; i++) ug_chhtsclflag[i]=0; \
	ug_gksstli.curprats.txuv.x = (*up).x; \
	ug_gksstli.curprats.txuv.y = (*up).y; \
	ug_gksstli.curprats.txuv.z = 0.0; \
}
#define ug_textpath(path) ug_gksstli.curprats.txpath = (path)
#define ug_charspace(spacing) ug_gksstli.curprats.txbundl.space = (spacing);
#define ug_textalign(align) ug_gksstli.curprats.txalign.hor = (align)->hor; \
									ug_gksstli.curprats.txalign.ver = (align)->ver
#define ug_marktype(tp) ug_gksstli.curprats.mkbundl.type = (tp)
#define ug_spickid(id) ug_gksstli.curprats.pickid = (id)
#define ug_linecolor(co) ug_gksstli.curprats.lnbundl.color = (co)
#define ug_markcolor(co) ug_gksstli.curprats.mkbundl.color = (co)
#define ug_textcolor(co) ug_gksstli.curprats.txbundl.color = (co)
#define ug_fillcolor(co) ug_gksstli.curprats.flbundl.color = (co)
#define ug_sedgeflag(f) ug_gksstli.curprats.flbundl.edgeflag = (f)

#define PI     (3.1415926454)
#define DTOR   (PI/180.0)
#define RTOD   (180.0/PI)

#define WSSGPH
#endif
