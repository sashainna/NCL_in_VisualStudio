
/********************************************************************* 
**  NAME:  ws7580i.h
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ws7580i.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:09
**
**  PARAMETERS   
**      INPUT:  none 
*********************************************************************/

#ifndef WS7580IH

/* device coordinate space is from (0,0) to (DEVXMAX,DEVYMAX) */
/* This is added to put user's control point p1 p2 to the device coordinate
	space */

int DEVXMAX_758 = 32283;				/* default maximum x coordinate */
int DEVYMAX_758 = 21163;				/* default device maximum y coordinate */
int DEVXHALF_758 = 16141;				/* default maximum x coordinate */
int DEVYHALF_758 = 10581;				/* default device maximum y coordinate */

#define WS7580IH
#endif
