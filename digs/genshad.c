
/********************************************************************* 
**  NAME:  genshad.c
**
**  Contains:	ug_genshade()
**
**  COPYRIGHT  1984  UNICAD, Inc.
** 
**    MODULE NAME AND RELEASE LEVEL
**       genshad.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:19
**
*********************************************************************/

#include <stdio.h>
#include "umath.h"
#include "udebug.h"
#include "gtbl.h"
#include "gobas.h"
#include "goatt.h"
#include "grender.h"
#include "gtblvar8.h"

#define EPS			(UU_REAL) 1.0e-10

/* Gives a linear interpolation of x given t in range t0 to t1, x in range
 * x0 to x1.
 */
#define LINTRP( t, t0, t1, x0, x1 ) \
	( fabs((t1)-(t0)) < EPS ? (x0) : \
	((t)-(t0)) / ((t1)-(t0)) * ((x1)-(x0)) + (x0) )

/*********************************************************************
**    I_FUNCTION     : ug_genshade(span, left, right)
**
**		Generates the shading information for a span.
**	
**		Phong Shading.  The surface normals, and their rates of
**		change are stored in span->norm and span->dnorm.
**
**		Gouraud Shading.  The intensity, and its rate of change is stored
**		int span->norm[0], and span->dnorm[0].
**
**		Flat Shading.  Nothing is computed.  Surface rgb color is stored
**		in span->norm.
**       
**    PARAMETERS   
**       INPUT  : 	span			The span to add shading info to.
**							left			Left edge of span.
**							right			Right edge of span.
**							ys				Y-coordinate of the span.
**       OUTPUT : 	
**							none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ug_genshade( span, left, right, ys )
Span *span;
Edge *left;
Edge *right;
Gfloat ys;
{
	Gfloat p[3];					/* 3-D point */
	Gfloat nl[3], nr[3];			/* Normals at left and right edges of span */
	Gfloat rl, gl, bl;			/* Color at left edge of span */
	Gfloat rr, gr, br;			/* Color at right edge of span */

	uu_denter(UU_GTRC,(us,"span %x, left %x, right %x,  ys=%f",
		span, left, right, ys));

	switch( ug_gksstli.rendats.intrshademethod ) {

		case 3:
		case 4:

		uu_dprint(UU_GTRC,(us,"phong shading"));

		/* First, find the normals at the left and right sides
		 * For the left end... */
		p[0] = span->x;	p[1] = ys;
		ug_normintrp( p, left->v1->p, left->v2->p, 
			nl, left->v1->n, left->v2->n );

		ug_taylor_normalize( nl );

		/* For the right end... */
		p[0] += span->xlen;
		ug_normintrp( p, right->v1->p, right->v2->p, 
			nr, right->v1->n, right->v2->n );

		ug_taylor_normalize( nr );

		/* Save normal at left edge */
		span->nx = nl[0];
		span->ny = nl[1];
		span->nz = nl[2];
		uu_dprint(UU_GTRC,(us,"span->normal %f %f %f",
			span->nx, span->ny, span->nz));

		/* Save Dnormal / Dx */
		if( fabs(span->xlen) > EPS ) {
			span->dnx = (nr[0] - nl[0]) / span->xlen;
			span->dny = (nr[1] - nl[1]) / span->xlen;
			span->dnz = (nr[2] - nl[2]) / span->xlen;
		}
		else {
			span->dnx = 0.0;
			span->dny = 0.0;
			span->dnz = 0.0;
		}

		uu_dprint(UU_GTRC,(us,"span->delnormal %f %f %f",
			span->dnx, span->dny, span->dnz));

		break;

	case 2:

		uu_dprint(UU_GTRC,(us,"gouraud shading"));

		/* Interpolate color at each vertex to find colors at
		 * each edge of this span */
		rl = LINTRP( ys, left->v1->p[1], left->v2->p[1],
						 left->v1->n[0], left->v2->n[0] );
		gl = LINTRP( ys, left->v1->p[1], left->v2->p[1],
						 left->v1->n[1], left->v2->n[1] );
		bl = LINTRP( ys, left->v1->p[1], left->v2->p[1],
						 left->v1->n[2], left->v2->n[2] );
		uu_dprint(UU_GTRC,(us,"color at left %f %f %f",rl, gl, bl));

		uu_dprint(UU_GTRC,(us,"color at v1 %f %f %f", 
			right->v1->n[0], right->v1->n[1], right->v1->n[2]));
		uu_dprint(UU_GTRC,(us,"color at v2 %f %f %f", 
			right->v2->n[0], right->v2->n[1], right->v2->n[2]));

		rr = LINTRP( ys, right->v1->p[1], right->v2->p[1],
						 right->v1->n[0], right->v2->n[0] );
		gr = LINTRP( ys, right->v1->p[1], right->v2->p[1],
						 right->v1->n[1], right->v2->n[1] );
		br = LINTRP( ys, right->v1->p[1], right->v2->p[1],
						 right->v1->n[2], right->v2->n[2] );

		uu_dprint(UU_GTRC,(us,"color at right %f %f %f",rr, gr, br));

		/* Store rgb triple at left edge of span */
		span->nx = rl;
		span->ny = gl;
		span->nz = bl;

		if( fabs(span->xlen) < EPS ) {
			span->dnx = span->dny = span->dnz = 0.0;
		}
		else {
			span->dnx = (rr-rl) / span->xlen;
			span->dny = (gr-gl) / span->xlen;
			span->dnz = (br-bl) / span->xlen;
		}

		break;

	case 1:
		
		uu_dprint(UU_GTRC,(us,"flat shading"));
		
		/* Store parent color in norm. (This is an rgb color!) */
		span->nx = span->poly->parent->colorrep.red;
		span->ny = span->poly->parent->colorrep.green;
		span->nz = span->poly->parent->colorrep.blue;
		span->dnx = span->dny = span->dnz = 0.0;
		uu_dprint(UU_GTRC,(us,"color %f %f %f",
			span->nx, span->ny, span->nz));
		break;

	}
	uu_dexit;
}
