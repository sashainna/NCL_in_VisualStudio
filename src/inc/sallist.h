/*********************************************************************
**    NAME         : sallist.h 
**       SAL-Unibase list data structure
**		CONTAINS:
**			UU_LIST
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       sallist.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:50
*********************************************************************/

#ifndef SALLISTH


typedef struct uu_list
{
	int				*data;
	int				item_size;
	int				max_items;
	int				item_cnt;
	struct uu_list	*next_list;
} UU_LIST;


#define SALLISTH
#endif
