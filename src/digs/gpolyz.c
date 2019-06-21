
/********************************************************************* 
**  NAME:  gpolyz.c
**
**  Contains:	ug_polyz()
**
**  COPYRIGHT  1984  UNICAD, Inc.
**    MODULE NAME AND RELEASE LEVEL
**       gpolyz.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:23
**
*********************************************************************/

#include <stdio.h>
#include "umath.h"
#include "udebug.h"
#include "gobas.h"
#include "grender.h"
#include "glists.h"

#define EPS (UU_REAL) 1.0e-6
#define BIG (UU_REAL) 1.0e32
#define ZILCH(x)		( (fabs(x) < EPS) ? 1 : 0  )

/*********************************************************************
**    I_FUNCTION     :  ug_polyz
**       
**    PARAMETERS   
**       INPUT  : 
**          polygon			polygon to solve for z in.
**				x					x coordinate.
**				y					y coordinate.
**       OUTPUT :  
**          z					z coordinate at x,y.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ug_polyz(polygon, x, y, z)
Polygon *polygon;
Gfloat x, y;
Gfloat *z;
{

	uu_denter(UU_GTRC,(us,"ug_polyz(%x, %f %f)", polygon, x, y));

	/* Given x,y, solve for the z coordinate on a plane through the 
	 * given polygon's verticies.  The plane equation (ax + by + cz +d = 0)
	 * is stored in the polygon definition.
	 */

	if( ZILCH(polygon->c) ) {
		uu_dprint(UU_GTRC,(us,"polygon on edge"));
		*z = -BIG;
	}
	else {
		*z = -( polygon->a*x + polygon->b*y + polygon->d ) / polygon->c;
	}

	uu_dprint(UU_GTRC,(us,"z= %f",*z));

	uu_dexit;
}
