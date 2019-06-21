/*********************************************************************
**    NAME         :  asalc.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       asalc.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:11
*********************************************************************/

#ifndef ASALCH

#include "usysdef.h"


  typedef struct  /* data type definition for coordinate  */
   {
    UU_REAL  x;
    UU_REAL  y;
    UU_REAL  z;
   }  US_COORD;

  typedef struct  /* data type definition for transformation matrix */
   {
    UU_REAL  transf[4][3];
   }  US_TRANSF;


#define ASALCH
#endif
