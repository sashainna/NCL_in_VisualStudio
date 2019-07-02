
/*********************************************************************
**    NAME         : patfcifdef.h 
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       tptfcifdef.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:57
*********************************************************************/

/* underbars are not needed on the Vax and Apollo to interface Fortran to 
C and vice versa -- this file removes those underbars */


#ifndef PAT_FCIFDEF

#ifndef UUSYSDEF
#include "usysdef.h"
#endif

#if UU_COMP==UU_VAXULTRIX || UU_COMP==UU_SUN || UU_COMP==UU_RIDGE || UU_COMP==UU_MASSCOMP || UU_COMP==UU_TEK

#endif

#define PAT_FCIFDEF
#endif
