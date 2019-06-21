/********************************************************************* 
**  NAME:  ws7580.h
**
**      GKS skeleton workstation: data definitions section.
**
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ws7580.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:09
**
**  PARAMETERS   
**      INPUT:  none 
**
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/

#ifndef WS7580H

#include "gobas.h"


typedef struct {
		int wid;							/* workstation id of this workstation */
		int ttfd;						/* file descriptor if using serial line*/
		int 	isrotate;				/* Is rotation needed? */
		Gfloat	vpy;						/* new vewport y range */
		FILE	*fid;
		struct {
			Gfloat scale; Gfloat dx,dy;
		} wsxform;	/* scale and dx,dy to map window to raster*/
		} Gs7580;
/*extern	char	DISK_fnm[];   commented kathy. */

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

/* device coordinate space is from (0,0) to (DEVXMAX,DEVYMAX) */

/*
.....change the number to the biggest size
.....we are allowed 1133*809mm
.....Yurong
*/
#define DEVXMAX 45320				/* maximum x coordinate */
#define DEVYMAX 32360				/* device maximum y coordinate */
#define DEVXHALF 22660				/* maximum x coordinate */
#define DEVYHALF 16180				/* device maximum y coordinate */
#define	FDEVXMAX	45320   
#define	FDEVYMAX  32360 

/*#define DEVXMAX 32283				/* maximum x coordinate */
/*#define DEVYMAX 21163				/* device maximum y coordinate */
/*#define DEVXHALF 16141				/* maximum x coordinate */
/*#define DEVYHALF 10581				/* device maximum y coordinate */
/*#define	FDEVXMAX	32283.0   */
/*#define	FDEVYMAX 21163.0   */

/* screen will hold ROWMAX rows of COLMAX  characters each, when using
	the standard size alpha characters */
#define ROWMAX 34					/* max alpha rows */
#define COLMAX 80				/* max alpha columns */

#define NUMPETS 5					/* Number of available prompt-and-echo types */
#define NCHDEV 150				/* number of choice devices */
#define MAXCHOICES 50			/* max number of choices for any choice device */


/*******************MACROS ************************************/

#define	UW_7580DEVTONDC(xy,nd)\
	{ nd[0] = xy[0]/FDEVXMAX;\
	nd[1] = xy[1]/FDEVXMAX; }


#define WS7580H
#endif
