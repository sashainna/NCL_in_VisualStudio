
/*********************************************************************
**    NAME         :  	mcvh.h
**		CONTAINS: include file for the 2-d & 3-d convex hull routines
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       mcvh.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:28
*********************************************************************/

#ifndef MCVHH

#include "usysdef.h"


struct face {					/* data for convex hull faces	*/
	UU_REAL nvec[3];			/* face normal vector			*/
	int vert[4];				/* index of vertex points		*/
	};

#define	UM_BDPT			2		/* boundary point flag	*/
#define	UM_INNPT			1		/* inner point flag		*/
#define	UM_NEW			0		/* new point flag			*/

#define	UM_MAXPTSET		100	/* maximum number of points in
										 * convex hull.  NOT ENFORCED
										 */

/** Note that these are now bit settings	**/
#define	UM_CVH_LINEAR	1		/* identifies cvh as linear	*/
#define	UM_CVH_PLANAR	2		/* identifies cvh as planar	*/
#define	UM_CVH_COMPLEX	4		/* identifies cvh as complex	*/
#define	UM_CVH_POINT	8		/* identifies cvh as a point	*/
#define	UM_CVH_SEMINF	16		/* cvh is semi-infinite line	*/
#define	UM_CVH_SMALL	32		/*	cvh is small		*/

/** two dimensional convex hull structure	**/

struct	c2vh	{
	int		no_pt;			/* number of points in set (which is generally
									 * not comvex.
									 */
	UU_REAL	(*pt)[3];		/* points will often be in B-spline entities
									 */
	int		no_cv;			/* number of vertices in convex hull, with
									 * the first counting twice.  I.e., the convex
									 * hull of two points would have no_cv == 3
									 */
	int		*cv;           /*	these values sequentially index the array pt
									 * above, giving the convex hull of the points in
									 * the array.  cv[no_cv-1] = cv[0]
									 */
	int		min;				/* index of a distinguished point.  Set to -1
									 * if the graham algorithm wasn't done, but could
									 * be, it seems
									 */
	UU_REAL	radius;			/* every point on hull lies within radius of the
									 * point indexed by min
									 */
	int		shape;			/* values are constants defined above
									 */
	};
typedef	struct	c2vh	C2vh;



#define MCVHH
#endif
