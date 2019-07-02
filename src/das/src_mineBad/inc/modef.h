/*********************************************************************
**    NAME         :  modef.h
**       CONTAINS: useful constants (tolerances etc.)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       modef.h , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:32
*********************************************************************/

#ifndef UM_MODEF
#include "usysdef.h"

#define UM_PLUSINFINITY -1
#define UM_RADIAN    (UU_REAL) 57.2957795131
#define UM_PI (UU_REAL) 3.14159265359
#define UM_TWOPI (UU_REAL) 6.28318530718
#define UM_HALFPI (UU_REAL) 1.570796326795

#define UM_COS15 (UU_REAL) 0.965925826289
#define UM_COS30 (UU_REAL) 0.866025403784
#define UM_COS45 (UU_REAL) 0.707106781186
		/* the following 3 definitions are tolerances used in the modelling */
		/* subsystem */
#define UM_FUZZ (UU_REAL) 1.0e-4
/*
.....Added UM_DFUZZ for double precision numbers
.....Bobby  -  10/3/91
*/
#define UM_DFUZZ (UU_REAL) 1.0e-8
#define UM_EQPARM (UU_REAL) 1.0e-6
#define UM_TOL (UU_REAL) 1.0e-4  /* used in "evcrv"*/
#define UM_TOLR (UU_REAL) 1.0e-7 /* used in "rotatept"*/
#define  UM_MINREAL (UU_REAL) -1.0E+36
#define  UM_MAXREAL (UU_REAL) 1.0E+36

#define	UM_ZILCH(A)	(fabs((UU_REAL)(A)) < UM_FUZZ)
#define	UM_DZILCH(A)	(fabs((UU_REAL)(A)) < UM_DFUZZ)

#define UM_MODEF
#endif
