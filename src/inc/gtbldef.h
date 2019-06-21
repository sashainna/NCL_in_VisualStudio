/*********************************************************************
**    NAME         :  gtbldef.h
**       CONTAINS: definitions of constants used in gtbl*.h
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gtbldef.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:23
*********************************************************************/
#ifndef GTBLDEFH
#include "usysdef.h"
#define UG_EPS (UU_REAL) 1.0e-10			/* less than this assumed 0 */
#define UG_MAXFLOAT (UU_REAL) 1.0e+20	/* biggest allowed coordinate or float number */
#define UG_MAXOPWS 5				/* max no. of simultaneously open workstations */
#define UG_MINSEGNO 0			/* minimum segment number */
/*
.....Changed maximum # of segments
.....From 9000 to 18000
.....Bobby  -  2/24/92
.....From 18000 to 36000
.....Bobby  -  1/14/98
*/
#define UG_MAXSEGNO 150000		/* maximum segment number */
#define UG_MAXSEGPRIO 255		/* maximum segment priority */
#define UG_NDCSEGNO 36001		/* minimum NDC segment number */
#define UG_MAXNTRAN 15			/* number viewing xforms */
#define UG_UDATA_SIZE  5		/* user data size */
#define GMAXLIGHTS 15			/* number of available light sources */
#define GTBLDEFH
#endif
