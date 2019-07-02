/*********************************************************************
**    NAME         :  slist.h
**       SALEX list management header
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       slist.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:51
*********************************************************************/

#ifndef SLISTH


/* list node */
typedef struct list_node 
	{
	struct list_node *	list_next;		/* pointer to next list node */
	int *						list_item;		/* pointer to item */
	} t_list;

extern void				us_list_free();			/* deallocate a list */
extern t_list *		us_list_add();				/* add item to a list */
extern t_list *		us_list_pop();				/* remove item from a list */
extern t_list *		us_list_concat();			/* concatenate a list to a list */

#define SLISTH
#endif
