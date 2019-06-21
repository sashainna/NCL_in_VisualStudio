/********************************************************************* 
**  NAME:  wsnews.h 
**
**      NeWS workstation include file.
**
**  COPYRIGHT  1987  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       wsnews.h , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:12
**
*********************************************************************/

#ifndef WSNEWSH

#include "gobas.h"
#include "gtbldef.h"
/*
#include "wsnewsps.h"
#include "wsnewsan.h"
#include "wsnewsxf.h"
#include "wsnewsin.h"
*/

/********************** magic numbers *****************************************/

#define MAX_FILLAREA_PTS 400
#define NUMPETS 6					/* Number of available prompt-and-echo types		*/
#define NCHDEV 350				/* number of choice devices							*/
#define MAXCHOICES 50			/* max number of choices for any choice device	*/
#define MAXPROMPTS 10			/* Max number of Prompts								*/
#define MAXWIN 10					/* Max number of simultaneous ansii windows		*/
#define NOTRANS 15				/* Maxi number of normalization transformations	*/
										/* NOTE: If changed, change in wsnewsps.cps		*/

/************************* NeWS specific typedefs **************************/
typedef struct { int red; int green; int blue; } newscolor;

typedef struct {

		Gint wid;					/* workstation id of this workstation */

		struct {
			Gfloat sf;
			Gfloat dx,dy;
		} wsxform;					/* sf and dx,dy to map ws window to raster*/

		/* Input device data		*/
		Giclass curdevclas;     /* currently being used device class */
		Gint curdevno;      /* currently being used device number */
		Giclass curreqclas; /* currently requested device class, or -1 */
		Gint curreqno;     /* currently requested device number */

		int normtran;
		newscolor vlt[256];			/* VLT	*/

		int redraw_flag;				/* 1 = need to redraw. 0=don't 			*/
		int curr_marker_index;     /* current marker color index */
      int curr_line_index;       /* current line color index */
      int curr_text_index;       /* current text color index */
		int nocolor;					/* Erasing if 1	*/
      int curr_fa_index;         /* current fill-area color index */
		int input;						/* 1 if waiting for input, else 0 */
		int winch;						/* 1 if window changed while input */
		int charwidth;
		int charheight;
		int no_repaint;				/* If set, don't repaint.  This will keep
												dragging object from getting hosed due to
												repainting while creating.  Also disables
												uw_newsnormtran to keep these calls out
												of drag objects.	*/
	struct {			
		int num;						/* prompt number */
		Gws ws;
		Girect rasloc;				/* raster location of prompt area */
		char *prompt;
		} promptlist[MAXPROMPTS];/* maximum 10 prompts up at once */
	int nprompts;		/* current length of promptlist */
	int ansirow[MAXWIN];
	int ansicol[MAXWIN];
	int ansinlines[MAXWIN];
	int ansincols[MAXWIN];
	int scrollup[MAXWIN];

	Gfloat curxy[2];			/* Current cursor location in NDC	*/
} UG_newsdat;

typedef enum {							/* this defines types of objects that	*/
			MENUTYP,						/* routine uw_newsmakobj is willing to	*/	
			TRANTYP,						/* make an object no for 					*/
			PRMTTYP,
			LOCTYP,
			OTHERTYP
		} UG_objtyp;


/********************** device specific defines ***************************/	
/* device coordinate space is from (0,0) to (DEVXMAX,DEVYMAX) */
#define DEVXMAX 1120				/* maximum x coordinate */
#define DEVYMAX 840				/* device maximum y coordinate */
					/* NOTE _ If these are changed, also change
						in wsnewsps.cps, line #32						 */

/* screen will hold ROWMAX rows of COLMAX  characters each, when using
	the standard size alpha characters. Probably wrong. */
#define ROWMAX  48				/* max alpha rows @16 pixels baseline dist.		*/
#define COLMAX  113				/* max alpha columns										*/

/***************** handy macros ***************************/
#define MAX(a,b) ((a>b)?a:b)		/* handy macros for clipping */
#define MIN(a,b) ((a<b)?a:b)		/* Probably not used		*/

#ifdef UW_NEWSMAIN
#define EXT
#else
#define EXT extern
#endif

/* Workstation variables */
EXT UG_newsdat uw_news;

#undef EXT
#define WSNEWSH
#endif
