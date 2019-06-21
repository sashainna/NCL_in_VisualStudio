/*********************************************************************
**    NAME         :  mdgenent.h
**       CONTAINS: defines generic entity definitions
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       mdgenent.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:29
*********************************************************************/

#ifndef UM_MGENENT

#include "usysdef.h"

/*********************************************************************
*
*	This structure must be large enough to hold any modeling entity.
*
**********************************************************************/

struct UM_entitydatabag {
	UU_KEY_ID key;
	int rel_num;
	char data[11000];
	};

/*********************************************************************
*
*	This structure must be large enough to hold any modeling curve
*	entity.
*
**********************************************************************/

struct UM_crvdatabag {
	UU_KEY_ID key;
	int rel_num;
	char label[64];
	UU_REAL labloc[3];
	UU_REAL ldrloc[3];
	int subscr;
	char data[5000];
	};

/**********************************************************************
*
*	This structure must be large enough to hold any modeling surface
*	entity.
*
**********************************************************************/

struct UM_srfdatabag {
	UU_KEY_ID key;
	int rel_num;
	char label[64];
	UU_REAL labloc[3];
	UU_REAL ldrloc[3];
	int subscr;
	char data[11000];
	};


#define UM_MGENENT
#endif
