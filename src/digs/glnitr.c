
/********************************************************************* 
**  NAME:  glnintr.c
**
**  Contains:	ug_lineinter()
**
**  COPYRIGHT  1984  UNICAD, Inc.
**    MODULE NAME AND RELEASE LEVEL
**       glnitr.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:20
**
*********************************************************************/

#include <stdio.h>
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "gobas.h"

#define EPS (UU_REAL) 1.0e-6
#define LOWT	(UU_REAL) 0.0001
#define HIGHT	(UU_REAL) 0.9999

/*********************************************************************
**    I_FUNCTION     :  ug_lineinter(x1,y1, x2,y2, x3,y3, x4,y4, xi,yi)
**       
**    PARAMETERS   
**       INPUT  : 
**          x1,y1				Endpoint of first line segment
**          x2,y2				Endpoint of first line segment
**          x3,y3				Endpoint of second line segment
**          x4,y4				Endpoint of second line segment
**       OUTPUT :  
**          xi,yi				Intersection point if it exists.
**    RETURNS      : 1 		If intersection point found.
**							0		If no intersection found.
**    SIDE EFFECTS : none
**    WARNINGS     : Colinear lines return no interesection point.
*********************************************************************/

ug_lineintr(x1,y1, x2,y2, x3,y3, x4,y4, xi,yi)
Gfloat x1,y1, x2,y2, x3,y3, x4,y4;
Gfloat *xi,*yi;
{
	Gfloat t, denom;

	uu_denter(UU_GTRC,(us,"ug_lineintr(%f,%f to %f %f with %f %f to %f %f)",
		x1,y1, x2,y2, x3,y3, x4,y4));

	denom = (y2-y1) * (x4-x3) - (y4-y3) * (x2-x1);

	if( fabs(denom) < EPS ) {
		uu_dprint(UU_GTRC,(us,"miss, denom = %f",denom));
		uu_dexit;
		return(0);
	}

	t = ((y4-y3) * (x1-x3) + (x4-x3) * (y3-y1)) / denom;

	if( t < LOWT || t > HIGHT ) {
		uu_dprint(UU_GTRC,(us,"miss, t=%f",t));
		uu_dexit;
		return(0);
	}

	*xi = (x2-x1) * t + x1;
	*yi = (y2-y1) * t + y1;
	uu_dprint(UU_GTRC,(us,"intersection, t=%f, gives %f %f",t,*xi, *yi));

	uu_dexit;
	return(1);

}
