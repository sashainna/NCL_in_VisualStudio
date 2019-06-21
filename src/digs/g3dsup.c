/*********************************************************************
**
**    NAME         :  g3dsup.c
**
**       CONTAINS:
**       	Support routines for 3D devices.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       g3dsup.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:16
*********************************************************************/

#include "gobas.h"

#define MAX(a,b) ((a>b)?a:b)
#define MIN(a,b) ((a<b)?a:b)

/*********************************************************************
**    I_FUNCTION     :  ug_dintersect(c,a,b)
**       Calculate c = intersection of a and b.  3-D workstations  may
**			use this routine to find out what viewport to set.
**			The viewport used must be the intersection of the ndc viewport
**			and the current wswindow.  This rectangle must be transformed
**			to device (ie raster) coordinates to be used as the viewport.
**
**    PARAMETERS   
**       INPUT  : 
**          a, b
**       OUTPUT :  
**          c
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ug_dintersect(c,a,b)		/* calculate c = intersection of a and b */
Gwrect3 *c,*a,*b;
{
	(*c).llf.x= MAX((*a).llf.x,(*b).llf.x);
	(*c).urb.x= MIN((*a).urb.x,(*b).urb.x);
	(*c).llf.y= MAX((*a).llf.y,(*b).llf.y);
	(*c).urb.y= MIN((*a).urb.y,(*b).urb.y);
	(*c).llf.z= MAX((*a).llf.z,(*b).llf.z);
	(*c).urb.z= MIN((*a).urb.z,(*b).urb.z);
	
}

