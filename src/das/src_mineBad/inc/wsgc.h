/********************************************************************* 
**  NAME:  wsgc.h 
**
**      Graphicon workstation include file.
**
**  COPYRIGHT  1987  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       wsgc.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:09
**
*********************************************************************/

#ifndef WSGCH

#include "gobas.h"
#include "gtbldef.h"

/************************* gc specific typedefs **************************/

typedef struct {

		Gint wid;					/* workstation id of this workstation */

		struct {
			Gfloat sf;
			Gfloat dx,dy;
		} wsxform;					/* sf and dx,dy to map ws window to raster*/

		struct {
			Gfloat sfx;
			Gfloat sfy;
			Gfloat sfz;
			Gfloat dx,dy,dz;
		} xform;						/* sf, dx,dy to map current window to viewport */

		int redraw_flag;				/* 1 = need to redraw. 0=don't 			*/
		short prim;						/* Current segment primitive number		*/
		int opnseg;						/* Current open segment or -1 			*/
		int doublebuffer;				/* True, double buffer redraws			*/
		int pop_surf;					/* True if popup surface is active		*/
		int render;						/* True if in surface rendering mode	*/
		unsigned char writemask;	/* current graphics write mask 			*/
		unsigned char readmask;		/* current graphics read mask 			*/

} UG_gcdat;


/********************** device specific defines ***************************/	
/* device coordinate space is from (0,0) to (DEVXMAX,DEVYMAX) */
#define DEVXMAX 1279				/* maximum x coordinate */
#define DEVYMAX 1023				/* device maximum y coordinate */

/* screen will hold ROWMAX rows of COLMAX  characters each, when using
	the standard size alpha characters */
#define ROWMAX  64				/* max alpha rows @16 pixels baseline dist.		*/
#define COLMAX  160				/* max alpha columns										*/
#define STRWDTH 8					/* width of one character in pixels					*/

/********************** magic numbers *****************************************/

#define HILITE  13				/* Segment highlight index 							*/
#define NUMPETS 5					/* Number of available prompt-and-echo types		*/
#define NCHDEV 350				/* number of choice devices							*/
#define MAXCHOICES 50			/* max number of choices for any choice device	*/
#define MENUPRMPTSIZE 20		/* Max size of menu prompt								*/
#define MAXPRMPT 50				/* max no of prompts up at one time 				*/
#define MAXSSTK 10				/* max no of scrollareas on scroll stack 			*/
#define MATSIZ 4					/* coord matrix size 									*/
#define MAXUSRCURSORS 10		/* Number of definable user-cursors					*/

/***************** handy macros ***************************/
#define MAX(a,b) ((a>b)?a:b)		/* handy macros for clipping */
#define MIN(a,b) ((a<b)?a:b)

/******************** useful constants ****************************************/
#define PI		(3.1415926454)
#define DTOR	(PI/180.0)
#define RTOD	(180.0/PI)


/* The Graphicon requires a Viewport segment and a View transformation
 * segment for each Digs normtran.  Digs segments are grafted (Graphicon
 * terminology for instanced) in the View transformation segment.  The
 * segment structure looks like....
 *
 * 			[ Viewport Segment ]						(# 18000->18014)
 *							|
 *				[ View transformation Segment ]		(# 18014->18029)
 *						  /|\
 * 		[Seg] [Seg] [Seg] .... [Seg]				(# Seg+1 (1-9000))
 *
 * Digs segments are grafted into the appropriate Graphicon View transformation
 * at gcloseseg time.  This will obviously be a big problem if a single
 * Digs segment ever contains primitives in more than one normtran.
 */

/* These macros give Graphicon segment numbers corresponding 
 * to different Digs entities, ie Normtrans, Segments,
 * Viewing transformations, Choice devices, etc.
 */
#define Digs_Seg(a)			( (a) + 1 )
#define Digs_Instance(a)	( Digs_Seg(a) + UG_MAXSEGNO )
#define Viewport_Seg(a)		( Digs_Instance(a) + UG_MAXSEGNO )
#define Viewtran_Seg(a)		( Viewport_Seg(a) + UG_MAXNTRAN )
#define Menu_Seg(a)			( Viewtran_Seg(a) + UG_MAXNTRAN )
#define Menu_Inst(a)			( Menu_Seg(a) + NCHDEV )
#define Gap_To_Digs_Seg(a)	( (a) - 1 )


/*#define uw_gcpopsurf() \
/*	Set_Write_Mask( 0xF00, &status ); \
/*	Error_Check; \
/*	uw_gc.pop_surf = 1;
/*
/*#define uw_gcgraphsurf() \
/*	Set_Write_Mask( uw_gc.writemask, &status ); \
/*	Error_Check; \
/*	uw_gc.pop_surf = 0;
/*
/*#define swapbuffers() \
/*	readmask ^= 0xff; \
/*	writemask = ( (writemask&0xf) << 4 ) | (writemask&0xf0) >> 4;
/*
/*#define frontbuffer(flag) \
/*	if( flag ) \
/*		writemask |= readmask; \
/*	else \
/*		writemask &= ~readmask;
/*
/*#define backbuffer(flag) \
/*	if( flag ) \
/*		writemask |= ~readmask; \
/*	else \
/*		writemask &=  readmask; \
*/
/* Sets a color index, if in graphics surface, the index must be
 * logically anded into both buffers.  If in popup surface, index
 * must be shifted into popup planes.
 */
#define uw_gccolor(i) \
	uw_gc.doublebuffer ? \
		( !uw_gc.pop_surf ? (i | i<<4) : ((i>6) ? 7<<8 : (i+1) << 8) ) \
	: \
		( !uw_gc.pop_surf ? (i) : ((i>6) ? 7<<8 : (i+1) << 8) )


/* This is the way the error checker is defined in GE's gsl.h */
static int status;
static char error_string[256];

#define Error_Check	if ( status != 0 ) { \
                            ermsg( status, error_string ); \
                            fprintf( stderr, "%s\n", error_string ); }


#ifdef UW_GCMAIN
#define EXT
#else
#define EXT extern
#endif

/* Workstation variables */
EXT UG_gcdat uw_gc;

/* Array to hold up to 10 user defined cursors.  Uw_gcusrcursor[i] 
	corresponds to locator device i+1. If uw_gcusrcursor[i] <  0, a user 
	cursor for this device has not been defined yet. If uw_gcusrcursor[i] > 0, 
	its value is the gc object number containing it. */
EXT int uw_gcusrcursor[MAXUSRCURSORS]
#ifdef  UW_GCMAIN
= {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1}
#endif
;
EXT Gipoint uw_gcusrattach[MAXUSRCURSORS];

/* Intersection of current vport and ws_window */
EXT Gnrect3 uw_gcclip;			

/* Visible area of screen (apply uw_wsxform to clip) */
EXT Gipoint uw_gcscrnclip[2];

#undef EXT
#define WSGCH
#endif
