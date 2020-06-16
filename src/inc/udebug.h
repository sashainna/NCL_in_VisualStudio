/*********************************************************************
**    NAME         : udebug.h 
**       CONTAINS:
**      debug defines 
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       udebug.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:03
*********************************************************************/
#ifndef UDEBUGH
/*
.....added for UU_COMP used later
*/
#include "usysdef.h"

#define UU_DEBUGOFF
#ifdef UU_DEBUGOFF 
#include "udeboff.h"
#else
#include "udebon.h"


#endif

#define UDEBUGH
#endif
