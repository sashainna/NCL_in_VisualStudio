/*********************************************************************
**
**    NAME         : msave.h 
**
**       CONTAINS:
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       msave.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:32
**
*********************************************************************/

#ifndef MSAVEH


#ifndef UU_SYSDEF
#include "usysdef.h"
#endif

#include "ulist.h"

UU_LIST UM_cur_layers;
UU_LIST UM_cur_screens;
UU_LIST UM_cur_vports;
UU_LIST UM_cur_views;

int	UM_BORD_SEGS[20];					/* list of the border segements, before
													a save operation */

#define MSAVEH
#endif
