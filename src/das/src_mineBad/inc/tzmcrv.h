/*********************************************************************
**    NAME         :  mcrv.h
**       CONTAINS: contains curve record definitions
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tzmcrv.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:00
*********************************************************************/

#ifndef TZ_MCRV

#include "usysdef.h"
#include "tzdcoord.h"

/** Buffer sizes for curve records **/
#define TZ_COMPCRV_BUFSZ 4000
#define TZ_BSPLCRV_BUFSZ 4000
#define TZ_RBSPLCRV_BUFSZ 6000
#define UM_MACHCRV_BUFSZ 2000
#define TZ_POLYLINE_BUFSZ 2000
#define TZ_TEXT_BUFSZ     1000

/** Values of conic type (from IGES) **/
#define	UM_CN_UNKNOWN		0
#define	UM_ELLIPSE			1
#define	UM_HYPERBOLA		2
#define	UM_PARABOLA			3

/** Maximum number of picks for composite curves **/
#define	UM_MAXPICK			500		/* will go away with list management */

#include "tzcrvddl.h"

typedef struct {
	int type;					/* type of conic */
	UM_coord spt;				/* coordinates of start point */
	UM_vector spt_derv;		/* derivative at start point */
	UM_coord ept;				/* coordinates of end point */
	UM_vector ept_derv;		/* derivative at end point */
	UM_length arc_length;	/* length of conic */
	UM_coord pln_point;		/* point defining plane of conic */
	UM_vector pln_normal;	/* normal to plane of conic */
	UM_coord center;			/* center of ellipse,
										center of hyperbola,
										vertex of parabola */
	UM_vector major_axis;	/* unit vector in direction of major axis */
	UM_vector minor_axis;	/* unit vector in direction of minor axis */
	UM_length major_length;	/* length of major axis */
	UM_length minor_length;	/* length of minor axis */
	UU_REAL cn[6];				/* conic equation coefficients */
	} TZ_cn_defn;

#define TZ_MCRV
#endif

