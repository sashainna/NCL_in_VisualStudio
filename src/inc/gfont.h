/*********************************************************************
**    NAME         :  gfont.h
**       CONTAINS:
**       Font declares and data structures.
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gfont.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:18
*********************************************************************/
#ifndef UG_FONT

#include "usysdef.h"

#ifdef GMAIN
#define EXT
#else
#define EXT extern
#endif

typedef /*UG_FONTHEAD*/ struct { /* font objectfile header */
		int         strokedim;        /* 2 or 3 dimension for strokedata */
		int         boxheight;        /* font box height size */
		int         boxwidth;         /* font box width size */
		int         base_drop;        /* base to drop amount */
		int         waist_drop;       /* waist to drop amount */
		int         width_type;       /* width type- fixed,var,overlap*/
		int         oshift[3];        /* overlap shift 1-3 */
		UU_REAL     line_spacing;     /* line spacing amount */
		UU_REAL     char_spacing;     /* character spacing amount */
		int         nochar;           /* number characters in font */
	}  UG_FONTHEAD;

typedef /*UG_FCHARHEAD*/ struct { /* font character header */
		int         nostroke;         /* number of strokes for char */
		short       width;            /* character width */
		short       left_otype;       /* left overlap type */
		short       right_otype;      /* right overlap type */
		char        fcharesc;         /* fontchar escape char or \0 */
		char        fchar;            /* font character */
 }  UG_FCHARHEAD;

typedef /*UG_FSTROKE*/ struct {   /* stroke information
													line: x,y move or draw
													arc: two x,y pairs: center,endpt
													  (arc begpt is last x,y coord)*/
		unsigned short   x;           /* x coord: bits 1-max
												   low bit 0:  0 = line, 1= arc */
		unsigned short   y;           /* y coord: bits 1-max
													low bit 0:
														line:  0= move  1=draw
														arc center: always 0.
														arc endpt: direction
																		0= counterclock
																		1= clockwise ***/
 }  UG_FSTROKE;

typedef /*UG_FSB*/ struct ugfsb    {  /* font stroke block */
		struct ugfsb *next;            /* next block in list */
		UG_FSTROKE    fs;               /* stroke data */
 }  UG_FSB;

typedef /*UG_FCB*/ struct ugfcb    {  /* font character block */
		struct ugfcb *next;            /* next block in list */
		UG_FSB       *chain;           /* char stroke block chain */
		UU_LOGICAL   last;              /* last block used in list */
		UG_FCHARHEAD  fc;               /* character header info */
 }  UG_FCB;

typedef /*UG_FHB*/ struct ugfhb  {  /* internal font head block */
		struct ugfhb *next;            /* next block in list */
		UG_FCB      *chain;            /* character block chain */
		UU_LOGICAL  last;               /* last block in list used flag */
		char        fontname[16];       /* this fontname */
		int         fontnum;            /* this font number */
		UG_FONTHEAD fh;                 /* font header info */
	}  UG_FHB;

typedef /*UG_FCDB*/ struct   {      /* character directory block */
		UG_FSTROKE    (*fs)[];          /* font strokes array */
		UG_FCHARHEAD  fc;               /* character header info */
 }  UG_FCDB;

typedef /*UG_FB*/ struct ugfb    {  /* internal font block */
		struct ugfb *next;             /* next block in list */
		UG_FCDB     (*cd)[];            /* char directory array */
		UU_LOGICAL  last;               /* last block in list used flag */
		char        fontname[16];       /* this fontname */
		int         fontnum;            /* this font number */
		UG_FONTHEAD fh;                 /* font header info */
		UG_FCDB *ascii[256];
		int asc_inc; 			/* offset for ascii characters 					*/
									/* (ie first non-ascii char at loc asc_inc)	*/
	}  UG_FB;
#ifdef GFU
/********************************************************************
 **  control block for constructing font object file (fu utility)
 ********************************************************************/
EXT UG_FHB        *UG_fhb_p;        /* current fhb block */
EXT UG_FHB        *UG_fhb_h;        /* head fhb block */
EXT UG_FHB        *UG_fhb_t;        /* tail fhb block */

EXT UG_FCB        *UG_fcb_p;        /* current font char block */
EXT UG_FCB        *UG_fcb_h;        /* head font char block */
EXT UG_FCB        *UG_fcb_t;        /* tail font char block */

EXT UG_FSB        *UG_fsb_p;         /* current stroke block */
EXT UG_FSB        *UG_fsb_h;         /* head stroke block */
EXT UG_FSB        *UG_fsb_t;         /* tail stroke block */
#endif
/********************************************************************
 **  control block for stroking characters within GKS.
 ********************************************************************/
EXT UG_FB         *UG_fb_p;         /* current font block */
EXT UG_FB         *UG_fb_h;         /* head font block */
EXT UG_FB         *UG_fb_t;         /* tail font block */
EXT UG_FCDB       *UG_fcdb_p;       /* current char directory block */

#define PI		(UU_REAL) (3.1415926454)
EXT UU_REAL			UG_slant_ang;		/* text slant angle */
EXT int				UG_ang_for_text;	/* angle to be put to the output file name*/

#undef EXT
#define UG_FONT
#endif
