
/*********************************************************************
**    NAME         :  ag_def.h -- defines for applied geometry
**       CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ag_def.h , 23.1
**    DATE AND TIME OF LAST MODIFICATION
**       05/22/12 , 11:18:56
*********************************************************************/

#include "umath.h" 

#ifndef UAGDEFH

#if UU_COMP==UU_IRIS 
#ifdef UU_DOUBLE

/* redefine double as long float to make it true double-precision */
#define double long float

#endif
#endif

#define UAGDEFH
#endif
