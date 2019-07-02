/*********************************************************************
**    NAME         :  zsysdep.h -- include file for sysdep dependent stuff.
**       CONTAINS:
**				zbytecp(to,from) -- structure assignment.
**				ZGETENV(var) -- system independent logical name substitution.
**				UZ_ADDRESS_HASH() -- system independent address hass function.
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       zsysdep.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:26
*********************************************************************/
#ifndef ZSYSDEPH

#include "usysdef.h"

#define zbytecp(to,from)	to = from

/*	-- warning - this will only work when the table size is a power of 2 -- */

#define UZ_ADDRESS_HASH(address, tablesize) \
		((int )address >> 2) & (tablesize - 1)

#define ZSYSDEPH
#endif
