/*********************************************************************
**    NAME         :  gentryctl.h -- DIGS entry points.
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gentryctl.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:16
*********************************************************************/

#ifndef GENTRYCTLH
#include "gomisc.h"
#include "gows.h"

/* ctrl entry points (in gctrl.c) */

Gerror gopengks();
Gerror gclosegks();
Gws *gopenws();
Gerror gclosews();
Gerror gmessage();
Gerror gclearws();
#define GENTRYCTLH
#endif
