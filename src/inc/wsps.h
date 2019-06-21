/*********************************************************************
**    NAME         :  wsps.h
**    CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       wsps.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:22
*********************************************************************/

#ifndef WSPSH
/*
.....added by Yurong
.....8/25/97
*/
#ifdef  WSPS_FIRST
#define EXT
#else
#ifdef __cplusplus 
#define EXT extern "C"
#else
#define EXT extern
#endif
#endif

#include "gobas.h"
#if UU_COMP!=UU_WIN2K
#define utp_ttputps uu_ttput
#endif

typedef struct {
		int wid;							/* workstation id of this workstation */
		int ttfd;						/* file descriptor if using serial line*/
		FILE *fid;							/* output disk file id */
		int 	isrotate;				/* Is rotation needed? */
		Gfloat	vpy;						/* new vewport y range */
		struct {
			Gfloat scale; Gfloat dx,dy;
		} wsxform;	/* scale and dx,dy to map window to raster*/
		} Gsps;
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
/*
.....Added 'width'
.....L.B    - 11/27/96
.....*/
	 int	width;
	 int		pt1[2], pt2[2];
	}	WSPTINFO;

/*define DEVXMAX 16520				/* maximum x coordinate */
/*#define DEVYMAX 10320				/* device maximum y coordinate */
/*#define DEVXMAX 16600				/* maximum x coordinate */
/*#define DEVYMAX 10320				/* device maximum y coordinate */
#define DEVXMAX 9600			/* maximum x coordinate, 32 inch , 300 pixel/inch */
#define DEVYMAX 6300			/* device maximum y coordinate, 21 inch */
#define DEVXHALF 0					/* origin is on the lower left corner */
#define DEVYHALF 0					/* origin is on the lower left corner */
/*  #define	FDEVXMAX	0.0
#define	FDEVYMAX 0.0   */
/* screen will hold ROWMAX rows of COLMAX  characters each, when using
	the standard size alpha characters */
#define ROWMAX 34					/* max alpha rows */
#define COLMAX 80				/* max alpha columns */
#define UU_PSMAXPEN 256       /* max pen number  */
#define NUMPETS 5					/* Number of available prompt-and-echo types */
#define NCHDEV 150				/* number of choice devices */
#define MAXCHOICES 50			/* max number of choices for any choice device */
/* hold 256 pen colors   */
/*
.....added by Yurong
.....removed these variables
.....from serial files 8/25/97
*/
EXT double UW_pscolor[257][3];    
/* 
.....Because PS default is 72 pixels per inch. That is too few. We normally set
.....more that 72 pixel/inch useing setenv UU_PSDEVX, UU_PSDEVY. We default 
.....300 pixel/inch. So, we use psrate_x, psrate_y to scale everything back to
.....PS default.
.....Yurong
*/ 
EXT double psrate_x, psrate_y;    
/*******************MACROS ************************************/

#define	UW_PSDEVTONDC(xy,nd)\
	{ nd[0] = xy[0]/DEVXMAX;\
	nd[1] = xy[1]/DEVXMAX; }

#undef EXT
#define WSPSH
#endif
