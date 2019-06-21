
/********************************************************************* 
**  NAME:  gnintrp.c
**
**  Contains:	
**
**  COPYRIGHT  1984  UNICAD, Inc.
**    MODULE NAME AND RELEASE LEVEL
**       gnintrp.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:21
**
*********************************************************************/

#include <stdio.h>
#include "umath.h"
#include "usysdef.h"
#include "gobas.h"
#include "udebug.h"

#define EPS (UU_REAL) (1.0e-6)

/*********************************************************************
**    I_FUNCTION     :  ug_normintrp( p, p1, p2, n, n1, n2 )
**       
**    PARAMETERS   
**       INPUT  : 
**				p1, p2		Points defining a line.
**				n1, n2		Associated unit normals.
**				p				Known point in this line segment.
**       OUTPUT :  
**          n				Interpolated normal at p.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ug_normintrp( p, p1, p2, n, n1, n2 )
Gfloat p[3], p1[3], p2[3];
Gfloat n[3], n1[3], n2[3];
{
	Gfloat prcnt;

	uu_denter(UU_GTRC,(us,"ug_normintrp(%f %f between %f %f %f and %f %f %f)",
		p[0], p[1], p1[0],p1[1],p1[2], p2[0],p2[1],p2[2]));

	uu_dprint(UU_GTRC,(us,"n1 = %f %f %f", n1[0], n1[1], n1[2]));
	uu_dprint(UU_GTRC,(us,"n2 = %f %f %f", n2[0], n2[1], n2[2]));

	/* Find percentage of range */
	if( fabs(p2[0]-p1[0]) > EPS ) {
		prcnt = (p[0]-p1[0]) / (p2[0]-p1[0]);
	}
	else if( fabs(p2[1]-p1[1]) > EPS ) {
		prcnt = (p[1]-p1[1]) / (p2[1]-p1[1]);
	}
	else {
		prcnt = 0.0;
	}

	/* Debug, check value of prcnt */
	uu_dprint(UU_GTRC,(us,"prcnt = %f",prcnt));
	if( prcnt > 1.0 || prcnt < -1.0) {
		uu_dprint(-1,(us,"ERROR. ug_normintrp-prcnt out of range"));
	}

	n[0] = prcnt * (n2[0]-n1[0]) + n1[0];
	n[1] = prcnt * (n2[1]-n1[1]) + n1[1];
	n[2] = prcnt * (n2[2]-n1[2]) + n1[2];

	uu_dprint(UU_GTRC,(us,"returning vector %f %f %f",
		n[0], n[1], n[2]));

	uu_dexit;
}
