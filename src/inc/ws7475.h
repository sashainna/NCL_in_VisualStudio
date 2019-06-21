
/*********************************************************************
**    NAME         :  ws7475.h
**    CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       ws7475.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:08
*********************************************************************/

#ifndef WS7475H

#include "gobas.h"


typedef struct {
		int wid;							/* workstation id of this workstation */
		int ttfd;						/* file descriptor if using serial line*/
		FILE *fid;							/* output disk file id */
		int 	isrotate;				/* Is rotation needed? */
		Gfloat	vpy;						/* new vewport y range */
		struct {
			Gfloat scale; Gfloat dx,dy;
		} wsxform;	/* scale and dx,dy to map window to raster*/
		} Gs7475;
/* extern	char	DISK_fnm[];		disk file name. commented kathy. */

#define	MAX_FILLAREA_PTS	400
#define	MAXJP		10000
typedef struct						/* line color, end pt information buffer */
	{
	 short	color;
/*
.....Added 'type'
.....Bobby  -  8/9/91
*/
	 short type;
	 int		pt1[2], pt2[2];
	}	WSPTINFO;

/*#define DEVXMAX 16520				/* maximum x coordinate */
/*#define DEVYMAX 10320				/* device maximum y coordinate */
/*#define DEVXMAX 16600				/* maximum x coordinate */
/*#define DEVYMAX 10320				/* device maximum y coordinate */
#define DEVXMAX 16683				/* maximum x coordinate */
#define DEVYMAX 10372				/* device maximum y coordinate */
#define DEVXHALF 0					/* origin is on the lower left corner */
#define DEVYHALF 0					/* origin is on the lower left corner */
#define	FDEVXMAX	0.0
#define	FDEVYMAX 0.0

/* screen will hold ROWMAX rows of COLMAX  characters each, when using
	the standard size alpha characters */
#define ROWMAX 34					/* max alpha rows */
#define COLMAX 80				/* max alpha columns */

#define NUMPETS 5					/* Number of available prompt-and-echo types */
#define NCHDEV 150				/* number of choice devices */
#define MAXCHOICES 50			/* max number of choices for any choice device */


/*******************MACROS ************************************/

#define	UW_7475DEVTONDC(xy,nd)\
	{ nd[0] = xy[0]/FDEVXMAX;\
	nd[1] = xy[1]/FDEVXMAX; }


#define WS7475H
#endif
