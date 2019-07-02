/********************************************************************* 
**  NAME:  ws104.h
**
**      include file for CALCOM 1043GT plotter driver
**
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ws104.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:08
**
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/

#ifndef WS104H
/*
.....added by Yurong
.....8/25/97
*/
#ifdef  WS104_FIRST 
#define EXT
#else
#define EXT extern
#endif

#include "gobas.h"

typedef struct {
		int wid;							/* workstation id of this workstation */
		int ttfd;						/* file descriptor if using serial line*/
		FILE *fid;							/* output disk file id */
		int lntype;					   /* line type */
		int 	isrotate;				/* Is rotation needed? */
		Gfloat	vpy;						/* new viewport y range */
		struct {
			Gfloat scale; Gfloat dx,dy;
		} wsxform;	/* scale and dx,dy to map window to raster*/
		} Gs104;
extern	char	DISK_fnm[];


#define	MAX_FILLAREA_PTS	400
/*Gs104 uw_104;					/* declare workstation local data */

#define	BIAS104	32						/* bias from ASCII */
#define  RADIX104	95						/* radix calculated from ASCII */
#define	STEP104	2032					/* raster units per inch */
#define	LINE104	1						/* type of line */
#define	TEXT104	2						/* type of text */
#define	MAXJP		10000					/* maximum temporary array size */
#define	BUF104SIZE	960					/* maximum plotter buffer size  */

EXT int	buflen;								/* how full the buffer is       */
typedef struct						/* line color, end pt information buffer */
	{
	 short	type;					/* line or text */
	 short	color;
	 short   lntype;				/* line type */
	 int		pt1[2], pt2[2];
	 char		*str;					/* text information */
	}	WSPTINFO;

/*
.....added by Yurong
.....removed these variables 
.....from serial files 8/25/97
*/
EXT int	prept[3];					/* previous point */

/* device coordinate space is from (0,0) to (DEVXMAX,DEVYMAX) */

#define DEVXMAX 96319				/* maximum x coordinate */
#define DEVYMAX 67759				/* device maximum y coordinate */
#define DEVXHALF 48159				/* maximum x coordinate */
#define DEVYHALF 33879				/* device maximum y coordinate */
#define	FDEVXMAX	96319.0
#define	FDEVYMAX 67759.0

/* screen will hold ROWMAX rows of COLMAX  characters each, when using
	the standard size alpha characters */
#define ROWMAX 34					/* max alpha rows */
#define COLMAX 80				/* max alpha columns */

#define NUMPETS 5					/* Number of available prompt-and-echo types */
#define NCHDEV 350				/* number of choice devices */
#define MAXCHOICES 50			/* max number of choices for any choice device */


/*******************MACROS ************************************/

#define	UW_104DEVTONDC(xy,nd)\
	{ nd[0] = xy[0]/FDEVXMAX;\
	nd[1] = xy[1]/FDEVXMAX; }

#undef EXT

#define WS104H
#endif
