/********************************************************************* 
**  NAME:  gysort.c
**
**  Contains:	ug_ysort()
**
**  COPYRIGHT  1984  UNICAD, Inc.
**    MODULE NAME AND RELEASE LEVEL
**       gysort.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:27
**
*********************************************************************/

#include <stdio.h>
#include "udebug.h"
#include "umath.h"
#include "gobas.h"
#include "grender.h"

/* This macro performs a window to viewport map */
#define WTOV( y, wcy, vcy, wy, vy ) \
				( ((y) - (wcy)) * (vy) / (wy) + (vcy) )

/* This macro rounds a floating point number to its nearest integer value */
#define  ROUND(x) \
	( (int)( (x) + 0.5 ) )

/*********************************************************************
**    I_FUNCTION     :  ug_ysort
**       
**    PARAMETERS   
**       INPUT  : 
**				n				Number of buckets in sort.
**				lower			Ndc coordinate at bottom scanline.
**				upper			Ndc coordinate at upper scanline.
**          polylist		List of all polygons.
**       OUTPUT :  
**          buckets		An n-bucket array of edge lists, containing 
**								an edge entry for each entering and exiting edge.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ug_ysort(n, lower, upper, polylist, buckets)
int n;
Gfloat lower, upper;
Polygon *polylist;
Edge **buckets[];
{
	Polygon *poly;
	Edge **edgepp;
	Edge *edge;
	Gfloat *p1, *p2;
	Gfloat wcy, vcy, wy, vy;			/* Window to viewport mapping */
	int j1, j2;

	uu_denter(UU_GTRC,(us,"ug_ysort(%d, %f, %f, %x, %x)",
		n, lower, upper, poly, buckets));

	/* Calculate the window to viewport map variables.  The ndc window,
	 * 0.0,0.0 -> 1.0,upper, is mapped into the raster window which
	 * is -0.5,RES-0.5 -> RES-.5,-0.5. (Really, this IS right!)
	 */
	wy = upper - lower;			/* Window width in y */
	wcy = wy / 2.0 + lower;		/* Window center */
	vy = -n;							/* Viewport width in y */
	vcy = n / 2.0 - 0.5;			/* Viewport center */

	uu_dprint(UU_GTRC,(us,"window wy %f, wcy %f, viewport vy %f, vcy %f",
		wy, wcy, vy, vcy));

	/* Bucket sort edges in each polygon into n buckets.  The 
	 * vertex of an edge is placed in the bucket corresponding to the
	 * scanline at or just below its y-coordinate.  
	 *	Some edges don't cross scanlines.  These edges will have 
	 * the same bucket indicies and won't generate spans, so we
	 * throw them out.
	 */

	/* For each polygon */
	for( poly=polylist; poly!=NULL; poly=poly->nxt ) {
		
		/* For each edge */
		for( edge = poly->edge;
			  edge != NULL;
			  edge = edge->nxt) {

			p1 = edge->v1->p;
			p2 = edge->v2->p;

			uu_dprint(UU_GTRC,(us,"edge %x %f %f %f, %f %f %f",
				edge, p1[0], p1[1], p1[2],
				p2[0], p2[1], p2[2]));

			/* Calculate edge bucket indicies, the buckets correspond to
			 * the scanline at or just below the vertex.
			 */
			uu_dprint(UU_GTRC,(us,"j1: %f %f %f %f  %f",
				p1[1], wcy, vcy, wy, vy));
			j1 = WTOV( p1[1], wcy, vcy, wy, vy ) +  1.0;
			uu_dprint(UU_GTRC,(us,"j2: %f %f %f %f  %f",
				p2[1], wcy, vcy, wy, vy));
			j2 = WTOV( p2[1], wcy, vcy, wy, vy ) +  1.0;
			
			uu_dprint(UU_GTRC,(us,"indices = %d %d", j1, j2));	

			/* If horizontal, don't need to consider this span */
			if( j1 == j2 ) {
				uu_dprint(UU_GTRC,(us,"Horizontal edge escape"));
				continue;
			}

			/* Add these edges to the bucket list */
			edgepp = (Edge **)uu_lsinsrt(buckets[j1], sizeof(Edge**));
			*edgepp = edge;

			edgepp = (Edge **)uu_lsinsrt(buckets[j2], sizeof(Edge**));
			*edgepp = edge;
		}
	}

	uu_dexit;
}
