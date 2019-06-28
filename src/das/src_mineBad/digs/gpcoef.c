
/********************************************************************* 
**  NAME:  gpcoef.c
**
**  Contains:	ug_polycoef()
**
**  COPYRIGHT  1984  UNICAD, Inc.
**    MODULE NAME AND RELEASE LEVEL
**       gpcoef.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:22
**
*********************************************************************/

#include <stdio.h>
#include "udebug.h"
#include "gobas.h"
#include "grender.h"
#include "glists.h"

/*********************************************************************
**    I_FUNCTION     :  ug_polycoef
**       
**    PARAMETERS   
**       INPUT  : 
**          polygon			polygon to find coefficents of.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : Polygons coeffiecents are computed and save in
**							the polygon record.
**    WARNINGS     : none
*********************************************************************/

ug_polycoef(polygon)
Polygon *polygon;
{
	Gfloat a, b, c, d;
	Edge *edge, *next;

	uu_denter(UU_GTRC,(us,"ug_polycoef(%x)", polygon));


	/* Given x,y, solve for the z coordinate on a plane through the 
	 * given polygon's verticies.  The plane equation (ax + by + cz +d = 0)
	 * is found using Martin Newell's method given in "A Characterization 
	 * of Ten Hidden-Surface Algorithms"....
	 *
	 *	For verticies V[i] = (X[i], Y[i], Z[i]) (i=1 to n):
	 *
	 *	j = (if i=n then 1 else i+1)
	 *	a = SUM (Y[i]-Y[j]) * (Z[i]+Z[j])
	 *	b = SUM (Z[i]-Z[j]) * (X[i]+X[j])
	 *	c = SUM (X[i]-X[j]) * (Y[i]+Y[j])
	 *
	 *	d can then be found from any one vertex.
	 *
	 *	If the polygon is not planar, this method will produce a 
	 * plane equation closely related to the polygon, but not 
	 * the best-fit plane equation.
	 *
	 */

	a = b = c = 0.0;

	/* For each edge */
	for( edge=polygon->edge; edge->nxt != NULL; ) {

		next = edge->nxt;

		a += (edge->v1->p[1]-next->v1->p[1]) * (edge->v1->p[2]+next->v1->p[2]);
		b += (edge->v1->p[2]-next->v1->p[2]) * (edge->v1->p[0]+next->v1->p[0]);
		c += (edge->v1->p[0]-next->v1->p[0]) * (edge->v1->p[1]+next->v1->p[1]);

		edge = next;

	}

	edge = polygon->edge;
	a += (next->v1->p[1] - edge->v1->p[1]) * (next->v1->p[2] + edge->v1->p[2]);
	b += (next->v1->p[2] - edge->v1->p[2]) * (next->v1->p[0] + edge->v1->p[0]);
	c += (next->v1->p[0] - edge->v1->p[0]) * (next->v1->p[1] + edge->v1->p[1]);


	/* Find coefficient d */
	d = -( a*edge->v1->p[0] + b*edge->v1->p[1] + c*edge->v1->p[2] );

	polygon->a = a;
	polygon->b = b;
	polygon->c = c;
	polygon->d = d;

	uu_dprint(UU_GTRC,(us,"a %f, b %f, c %f, d %f", a, b, c, d));

	uu_dexit;
}
