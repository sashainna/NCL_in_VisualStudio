
/*********************************************************************
**    NAME         :  msol.h
**       CONTAINS: contains solid record declarations
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       msol.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:33
*********************************************************************/
#ifndef UM_MSOL

#include "usysdef.h"

#define UM_MAXEDGE 1000

#define UM_MAX_NBR_HITS 20	/* max nbr of intersections used in fit tolerance */

/*
.....Solid types
*/
#define UM_BOX_SOLID 1
#define UM_CYLINDER_SOLID 2
#define UM_TORUS_SOLID 3
#define UM_SPHERE_SOLID 4
#define UM_CONE_SOLID 5
#define UM_EXTRUDED_SOLID 6
#define UM_CONTOUR_SOLID 7
#define UM_REVOLVED_SOLID 8
#define UM_STL_SOLID 9
#define UM_COMPOS_SOLID 10

/** Buffer sizes for solids **/
#define UM_BODY_BUFSZ     4004
#define UM_SOLID_BUFSZ    504

#include "msolddl.h"

#define UM_MSOL
#endif

