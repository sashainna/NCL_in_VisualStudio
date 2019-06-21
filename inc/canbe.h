/*********************************************************************
**    NAME         :  canbe.h
**       CONTAINS: definitions for canbe structures
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       canbe.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:11
*********************************************************************/

#ifndef UC_CANBEH

#include "usysdef.h"

#define  UC_CANBE_ASYMBOL	0			/* can be in a symbol */
#define  UC_CANBE_AGROUP	1			/* can be in a group */
#define  UC_CANBE_ACOMP		2			/* can be in a composite curve */

#define UC_CANBE_SIZE 		3			/* can be array size */

typedef UU_LOGICAL UC_CANBE_REC[UC_CANBE_SIZE];

typedef UU_LOGICAL *UC_CANBE_REC_PTR;

#define UC_CANBEH
#endif
