
/*********************************************************************
**    NAME         :  class.h
**       CONTAINS: definitions for class structures
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       tzclass.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:59
*********************************************************************/

#ifndef UC_CLASS

#include "usysdef.h"

/********************************************************************
*
*		THIS STRUCTURE DEFINITION MUST BE LARGE ENOUGH TO HOLD ANY
*		RELATION TUPLE.
*
********************************************************************/

struct UC_entitydatabag {
		UU_KEY_ID key;
		int rel_num;
		char data[25000];
		};

/********************************************************************
*
*		THIS STRUCTURE DEFINITION MUST BE LARGE ENOUGH TO HOLD ANY
*		ATTRIBUTE RELATION TUPLE.
*
********************************************************************/

struct UC_attributedatabag {
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	int	color;
	int	layer;
	int	pen;
	int	line_style;
	UU_REAL line_weight;
	UU_REAL line_width;
	UU_LOGICAL displayable;
	UU_LOGICAL selectable;
	UU_LOGICAL blanked;
	int label_on;
	char data[1000];
}; 

/********************************************************************
*
*		Defined class numbers. These numbers define the order of the
*		the entries in the master table DISPATCHER.
*
********************************************************************/

#define UC_UNKNOWN_CLASS	-1
#define UC_GEOMETRY_CLASS	0
#define UC_DRAFTING_CLASS	1
#define UC_SYMBOL_CLASS		2
#define UC_TRAINING_CLASS	3
#define UC_VIEWING_CLASS	4
#define UC_SPLAN_CLASS		5
#define UC_APPLIC_CLASS		6
#define UC_CONNECTOR_CLASS 7

/* This is used to print in the "trc" file if a "uc_" routine fails. */
#ifdef UU_DEBUGON
#define UC_IF_FAILURE_PRINT_IT                       \
	if (status != UU_SUCCESS)                         \
	{                                                 \
		char us[120];                                  \
		uu_denter2(UU_MTRC,(us, "returned FAILURE"));  \
		uu_dexit;                                      \
	}                                                 
#else
#define UC_IF_FAILURE_PRINT_IT
#endif

#define UC_CLASS
#endif
