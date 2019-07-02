/*********************************************************************
**    NAME         :  gentryout.h -- DIGS entry points.
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gentryout.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:17
*********************************************************************/

#ifndef GENTRYOUTH

#include "gomisc.h"

/* output entry points (in gout.c) */

Gerror gpolyline();
Gerror gpolyline3();
Gerror gpolymarker3();
Gerror gpolymarker();
Gerror gtext();
#define GENTRYOUTH
#endif
