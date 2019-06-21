
/********************************************************************* 
**  NAME:  ws7580e.h
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ws7580e.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:09
**
**  PARAMETERS   
**      INPUT:  none 
*********************************************************************/

#ifndef WS7580EH

/* device coordinate space is from (0,0) to (DEVXMAX,DEVYMAX) */
/* This is added to put user's control point p1 p2 to the device coordinate
	space */

extern int DEVXMAX_758;				/* default maximum x coordinate */
extern int DEVYMAX_758;				/* default device maximum y coordinate */
extern int DEVXHALF_758;			/* default maximum x coordinate */
extern int DEVYHALF_758;			/* default device maximum y coordinate */

#define WS7580EH
#endif
