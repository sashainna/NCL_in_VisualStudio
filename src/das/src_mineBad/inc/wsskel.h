/*********************************************************************
**    NAME         :  wsskel.h
**       CONTAINS:
**       Include file for the "wsskel*.c" driver files
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       wsskel.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:23
*********************************************************************/

#ifndef WSSKELH

#include "gtblws.h"

typedef struct {
		int wid;							/* workstation id of this workstation */
		UG_wdt *wdtptr;				/* pointer to workstation description table */
      int ttfd;                  /* file descriptor if using serial line */
		int curr_marker_index;		/* current marker color index */
		int curr_line_index;			/* current line color index */
		int curr_text_index;			/* current text color index */
		int curr_fa_index;			/* current fill-area color index */
		struct {
			float scale;
			float dx,dy;
			} wsxform;					/* scale and dx,dy to map window to raster*/
		} uw_skeldata;

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

#define WSSKELH
#endif
