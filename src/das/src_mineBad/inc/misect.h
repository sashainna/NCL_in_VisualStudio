
/*********************************************************************
**    NAME         :  misect.h
**       CONTAINS:
**       definitions for (planar) curve intersections
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       misect.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:31
*********************************************************************/

#ifndef MISECTH


#include "usysdef.h"
#include	"mdrel.h"
#include	"mdcoord.h"

typedef	struct	{
	UM_coord	pt;		/* intersection point */
	UU_REAL	u0;		/* logical parameter of intersection point on curve 0 */
	UU_REAL	u1;		/* logical parameter of intersection point on curve 1 */
	UU_REAL	t0;		/* physical parameter of intersection point on curve 0 */
	UU_REAL	t1;		/* physical parameter of intersection point on curve 1 */
	int		order;	/* 0 means transverse, 1 means tangent, etc. */
	}	UM_isect;

/** for RBSPL intersection	**/
#define	UM_MAXISECT	25		/* maximum number of intersections to find	*/
#define	UM_ISECTMAXDEPTH	18		/* maximum recursion depth	*/
#define	UM_ISECTTOLERANCE	(UU_REAL) 1.0e-4		/* intersection absolute tolerance	*/
#define	UM_MAXITERATE	 20	/* max number of iterations in um_refine_isect	*/


/** requirements of this ordering: TRUE if A==B or if either is a RBSPLCRV **/
#define	UM_IORDER(A,B) ((A)==UM_RBSPLCRV_REL||(B)==UM_RBSPLCRV_REL||(A)<=(B))

#define	UM_ISECTPAIR(A, B)	(((A)<<8)+(B))
#define	UM_POINT_LINE	UM_ISECTPAIR(UM_POINT_REL, UM_LINE_REL)
#define	UM_POINT_CIRC	UM_ISECTPAIR(UM_POINT_REL, UM_CIRCLE_REL)
#define	UM_POINT_CONIC	UM_ISECTPAIR(UM_POINT_REL, UM_CONIC_REL)
#define	UM_POINT_CURVE	UM_ISECTPAIR(UM_POINT_REL, UM_RBSPLCRV_REL)
#define	UM_CURVE_POINT	UM_ISECTPAIR(UM_RBSPLCRV_REL, UM_POINT_REL)
#define	UM_LINE_LINE	UM_ISECTPAIR(UM_LINE_REL, UM_LINE_REL)
#define	UM_LINE_CIRC	UM_ISECTPAIR(UM_LINE_REL, UM_CIRCLE_REL)
#define	UM_LINE_CONIC	UM_ISECTPAIR(UM_LINE_REL, UM_CONIC_REL)
#define	UM_CIRC_CIRC	UM_ISECTPAIR(UM_CIRCLE_REL, UM_CIRCLE_REL)


#define MISECTH
#endif
