
/********************************************************************* 
**  NAME:  genspan.c
**
**  Contains:	ug_genspans()
**
**  COPYRIGHT  1984  UNICAD, Inc.
** 
**    MODULE NAME AND RELEASE LEVEL
**       genspan.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:19
**
*********************************************************************/

#include <stdio.h>
#include "umath.h"
#include "usysdef.h"
#include "ulist.h"
#include "udebug.h"
#include "grender.h"
#include "gobas.h"
#include "gtblvar8.h"

#define EPS			(UU_REAL) 1.0e-10

/*********************************************************************
**    I_FUNCTION     : ug_genspans()
**
**		Generates the spans for a given scanline. 
**		The active list of edges is updated with
**		new edge crossings from bucket. 
**       
**    PARAMETERS   
**       INPUT  : 	bucket		A list of all edge transitions on a scanline.
**							activelist	List of all currently active edges.
**							ys				Y coordinate at this scanline.
**       OUTPUT : 	
**							none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_genspans( bucket, activelist, ys)
Edge **bucket;
Active *activelist;
Gfloat ys;
{
	Active *lactive;			/* Pointer to an element in active list */
	Active *ractive;			/* Pointer to an element in active list */
	Gfloat xr, xl;				/* Scanline intercepts of a span */
	Gfloat zr, zl;				/* Z depths at ends of a span */
	Span span;					/* One span */
	Edge *left;					/* Pointer to spans left edge */
	Edge *right;				/* Pointer to spans right edge */

	/* Generate all the spans for current scanline */
	uu_denter(UU_GTRC,(us,
		"ug_genspans( bucket %x, activelist %x, ys %f )",
		bucket, activelist, ys));

	/* Update the active list with edge transitions in bucket */
	ug_updateactive(bucket, activelist);

	/* Mark each edge in active list as unused */
	for(	lactive =  (Active *)uu_lsnext(activelist);
			lactive != NULL;
			lactive =  (Active *)uu_lsnext(lactive) ) {

		lactive->used = UU_FALSE;

	}

	/* Create the spans from the active edge list.  Each span
 	 * is the intersection of two edges of a polygon with the 
	 * current scanline.  Note, this section of code assumes
	 * convex polygons (one span per polygon).
	 */

	for(	lactive =  (Active *)uu_lsnext(activelist);
			lactive != NULL;
			lactive =  (Active *)uu_lsnext(lactive) ) {

		uu_dprint(UU_GTRC,(us,"lactive=%x, *edge=%x, used=%d",
			lactive, lactive->edge, lactive->used ));

		/* If we've shaded this span, continue to next edge in list */
		if( lactive->used == UU_TRUE ) {
			continue;
		}

		/* Find right edge of this span.  It's the next active edge which 
		 * has same parent as current active edge. 
		 */
		for(	ractive =  (Active *)uu_lsnext(lactive);
				ractive != NULL;
				ractive =  (Active *)uu_lsnext(ractive) ) {

			uu_dprint(UU_GTRC,(us,"searching for right edge"));

			if(ractive->edge->poly == lactive->edge->poly) {
				uu_dprint(UU_GTRC,(us,"found right edge"));
				break;
			}
		}

		/* Debug, complain about nonmatching spans */
		if( ractive == NULL ) {
			uu_dprint(UU_GTRC,(us,"ERROR. no right edge found"));
		}

		/* Mark both ends of span as shaded */
		lactive->used = ractive->used = UU_TRUE;

		/* Calculate x intersection points with this scanline */
		left = lactive->edge;
		right = ractive->edge;
		if( ug_edgeintr( ys, &left, &right, &xl, &xr, &zl, &zr ) ) {
			uu_dprint(UU_GTRC,(us,"xl = %f, xr = %f",xl, xr));
			uu_dprint(UU_GTRC,(us,"zl = %f, zr = %f",zl, zr));
		}

		else {		/* No span found */
			uu_dprint(UU_GTRC,(us,"no span found"));
			continue;
		}

		/* Fill in span information */
		span.poly = left->poly;
		span.x = xl;
		span.z = zl;
		span.xlen = xr - xl;

		/* Delta Z / Delta X */
		if( fabs(span.xlen) > EPS )
			span.dz = (zr - zl) / span.xlen;
		else
			span.dz = 0.0;
		uu_dprint(UU_GTRC,(us,"span.dz = %f",span.dz));

		/* Calculate necessary shading information for this span */
		ug_genshade( &span, left, right, ys );

		/* Now, shade this span */
		ug_spanout( ys, &span, 
			left->poly->parent->colorrep.red,
			left->poly->parent->colorrep.green,
			left->poly->parent->colorrep.blue);

	}

	uu_dexit;
}


/*********************************************************************
**    I_FUNCTION     : ug_updateactive(bucket, activelist)
**
**	 Update the active edge list with the contents of the current bucket. If 
**	 edge already appears in the current active edge list, delete it 
**	 from active edge list else add it to the active edge list.
**	 
**       
**    PARAMETERS   
**       INPUT  : 	bucket		A list of all edge transitions on a scanline.
**							activelist	List of all currently active edges.
**       OUTPUT : 	
**							activelist	Updated list of active edges.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static ug_updateactive(bucket, activelist)
Edge **bucket;
Active *activelist;
{
	Edge **eptr;				/* List elements in bucket */
	int del;						/* Flag indicating edge deleted */
	Active *active;			/* Pointer to one element in active list */

	uu_denter(UU_GTRC,(us,"ug_updateactive( %x %x )", bucket, activelist));

	/* Update active edge list for each edge in bucket */
	for( 	eptr =  (Edge **)uu_lsnext(bucket);
			eptr != NULL;
			eptr =  (Edge **)uu_lsnext(eptr) ) {

		uu_dprint(UU_GTRC,(us,"edge = %x", *eptr));

		/* Look at each element in current active edge list...
		 * If *eptr is in list, delete it.  Else must add it to
		 * end of active list.
		 */
		del = UU_FALSE;
		active = activelist;
		for(	active = (Active *)uu_lsnext(activelist);
				active != NULL; 
				active = (Active *)uu_lsnext(active) ) {

			if( active->edge == *eptr ) {

				uu_dprint(UU_GTRC,(us,"deleting edge %x",*eptr));

				/* Delete the edge from the active list */
				uu_lsdele(active);

				del = UU_TRUE;
				break;
			}
		}

		/* If didn't delete element, let's add it */
		if( del != UU_TRUE ) {

			uu_dprint(UU_GTRC,(us,"adding edge %x",*eptr));

			/* First, add the edge to the active list */
			active = (Active *)
				uu_lsinsrt( activelist , sizeof(Active));
			active->edge = *eptr;
		}
	} 	/* End of for each edge in bucket */

	uu_dexit;
}
