/********************************************************************* 
**  NAME:  genspan2.c
**
**  Contains:	ug_edgeintr()
**
**  COPYRIGHT  1984  UNICAD, Inc.
** 
**    MODULE NAME AND RELEASE LEVEL
**       genspan2.c , 25.1
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

#define EPS (UU_REAL) 1.0e-10

/* Gives a linear interpolation of x given t in range t0 to t1, x in range
 * x0 to x1.
 */
#define LINTRP( t, t0, t1, x0, x1 ) \
	( fabs((t1)-(t0)) < EPS ? (x0) : \
	((t)-(t0)) / ((t1)-(t0)) * ((x1)-(x0)) + (x0) )

/*********************************************************************
**    I_FUNCTION     : ug_edgeintr( left, right, xl, xr, zl, zr )
**
**		Intersects two edges with a given y value and computes the
**		x,z intersection points.
**       
**    PARAMETERS   
**       INPUT  : 	
**				Edge **left;				Pointer to spans left edge 
**				Edge **right;				Pointer to spans right edge 
**       OUTPUT : 	
**				Gfloat *xl, *xr;			Left and right intersection points
**				Gfloat *zl, *zr;			Associated z depth at above points
**
**    RETURNS      : 1 if span found at this y, else 0.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
Gint ug_edgeintr( ys, left, right, xl, xr, zl, zr )
Gfloat ys;					/* Ndc value of current scanline */
Edge **left;				/* Pointer to spans left edge */
Edge **right;				/* Pointer to spans right edge */
Gfloat *xl, *xr;			/* Left and right intersection points */
Gfloat *zl, *zr;			/* Associated z depth at above points */
{

	int irtn;							/* Return value */
	Gnpoint lmin, lmax;				/* Left edges coordinates */
	Gnpoint rmin, rmax;				/* Right edges coordinates */
	int lstraddle, rstraddle;		/* Logical edge "straddle" flags */

	uu_denter(UU_GTRC,(us,"ug_edgeintr(ys=%f, %x %x)", ys, *left, *right));

	/* Find left edges min and max in y */
	if( (*left)->v1->p[1] > (*left)->v2->p[1] ) {
		lmax.x = (*left)->v1->p[0];
		lmax.y = (*left)->v1->p[1];
		lmin.x = (*left)->v2->p[0];
		lmin.y = (*left)->v2->p[1];
	}
	else {
		lmax.x = (*left)->v2->p[0];
		lmax.y = (*left)->v2->p[1];
		lmin.x = (*left)->v1->p[0];
		lmin.y = (*left)->v1->p[1];
	}

	/* Find right edges min and max in y */
	if( (*right)->v1->p[1] > (*right)->v2->p[1] ) {
		rmax.x = (*right)->v1->p[0];
		rmax.y = (*right)->v1->p[1];
		rmin.x = (*right)->v2->p[0];
		rmin.y = (*right)->v2->p[1];
	}
	else {
		rmax.x = (*right)->v2->p[0];
		rmax.y = (*right)->v2->p[1];
		rmin.x = (*right)->v1->p[0];
		rmin.y = (*right)->v1->p[1];
	}

	uu_dprint(UU_GTRC,(us,"left min %f %f to max %f %f", 
		lmin.x, lmin.y, lmax.x, lmax.y));
	uu_dprint(UU_GTRC,(us,"right min %f %f to max %f %f",
		rmin.x, rmin.y, rmax.x, rmax.y));

	lstraddle = (lmin.y <= ys) && (lmax.y >= ys);
	rstraddle = (rmin.y <= ys) && (rmax.y >= ys);
	uu_dprint(UU_GTRC,(us,"lstraddle %d, rstraddle %d",lstraddle, rstraddle));

	/* Both edges should straddle ys */
	if( lstraddle && rstraddle ) {
		uu_dprint(UU_GTRC,(us,"both edges straddle"));

		/* Find left x intersection point */
		*xl = LINTRP( ys, lmin.y, lmax.y, lmin.x, lmax.x );

		/* Find right x intersection point */
		*xr = LINTRP( ys, rmin.y, rmax.y, rmin.x, rmax.x );

		irtn = 1;
	}

	else {
		uu_dprint(UU_GTRC,(us,
			"ERROR. ug_edgeintr-edges don't straddle scanline"));
		irtn = 0;
		uu_dexit;
		return(irtn);
	}

	uu_dprint(UU_GTRC,(us,"xl %f, xr %f",*xl,*xr));
		
	/* Set left and right pointers (Up to this point, we haven't insured
	 * that left is less than right).
	 */
	if( *xr < *xl ) {		/* Switch left and right edges */
		Edge *tmp;
		Gfloat ftmp;

		uu_dprint(UU_GTRC,(us,"swapping left and right endpoints"));
		tmp    = *left;
		*left  = *right;
		*right = tmp;

		/* Swap xr and xl */
		ftmp = *xl;
		*xl = *xr;
		*xr = ftmp;
	}	

	/* Find z depth at each edge of span */
	ug_polyz( (*left)->poly, *xl, ys, zl );
	ug_polyz( (*right)->poly, *xr, ys, zr );

	uu_dexit;
	return(irtn);
}
