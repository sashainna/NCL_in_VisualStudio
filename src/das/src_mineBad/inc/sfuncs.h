/*********************************************************************
**    NAME         : sfuncs.h 
**				Header for built-in functions
**			CONTAINS:
**       	imod = us_imod_func( i1, i2 ) 
**       	rmod = us_rmod_func( r1, r2 ) 
**       	ipow = us_ipow_func( i1, i2 ) 
**       	rpow = us_rpow_func( r1, r2 ) 
**				um_mxtomx( mxi, mxo )
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       sfuncs.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:51
*********************************************************************/

#ifndef SFUNCSH

#include "usysdef.h"

extern int		us_imod_func();
extern UU_REAL	us_rmod_func();
extern int		us_ipow_func();
extern UU_REAL	us_rpow_func();


#define SFUNCSH
#endif
