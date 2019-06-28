/*********************************************************************
**    NAME         : ulist.c
**       Dynamic list management package
**		CONTAINS:
**			uu_list_init(list, item_size, initial_cnt, expansion_cnt)
**			uu_list_free(list)
**			uu_list_push(list, item)
**			uu_list_push_multiple(list, count, items)
**			uu_list_push_list(list1, list2)
**			uu_list_pop(list, item)
**			uu_list_pop_multiple(list, count, items)
**			uu_list_pop_list(list1, count, list2)
**			uu_list_delete(list, index, count)
**			uu_list_insert(list, index, item)
**			uu_list_insert_multiple(list, index, count, items)
**			uu_list_insert_list(list1, index, list2)
**			uu_list_sort(list, compare)
**			uu_list_expand(list, count)
**			uu_list_move(list, index1, index2, count)
**			length = UU_LIST_LENGTH(list)
**			array = UU_LIST_ARRAY(list)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       ulist.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:54
*********************************************************************/

#include "usysdef.h"
#include "ulist.h"
#include "udebug.h"

#define LIST_INDEX(list,index) (&((list)->data[(index)*((list)->item_size)]))
#define END_OF_LIST(list) LIST_INDEX((list),((list)->cur_cnt))

static void copy_down();
static void copy_up();
static void adjust_list();

extern char *uu_malloc();

#define CONTRACT_LIST(list) adjust_list(list, 0)


/*********************************************************************
**    E_FUNCTION     : void uu_list_init(list, item_size, init_cnt, exp_cnt)
**       Initialize (empty) a list
**    PARAMETERS
**       INPUT  :
**          list				pointer to UU_LIST structure
**				item_size		storage required for each item in bytes
**				init_cnt			number of items initially allocated
**				exp_cnt			number of items to expand by on overflow
**       OUTPUT :
**          list				filled in
**    RETURNS      : nothing
**    SIDE EFFECTS : storage is allocated
**    WARNINGS     : storage already held by 'list' will not be deallocated
*********************************************************************/

void uu_list_init(list, item_size, init_cnt, exp_cnt)
UU_LIST			*list;
int				item_size;
int				init_cnt;
int				exp_cnt;
{
	uu_denter(UU_UITRC,(us,
		"uu_list_init: *list=%x,size=%d,init=%d,exp=%d",
		list, item_size, init_cnt, exp_cnt
	));

	/* fill in list struct */
	list->item_size = item_size;
	list->cur_cnt = 0;
	list->max_cnt = init_cnt;
	list->exp_cnt = exp_cnt;

	/* if space to allocate, allocate space */
	if(list->max_cnt)
		list->data = uu_malloc(item_size * list->max_cnt);
	else
		list->data = UU_NULL;

	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION     : void uu_list_init0(list)
**       Initialize two list fields
**    PARAMETERS
**       INPUT  :
**          list				pointer to UU_LIST structure
**       OUTPUT :
**          list				filled in
**    RETURNS      : nothing
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uu_list_init0(list)
UU_LIST *list;
{
	list->cur_cnt = 0;
	list->data = UU_NULL;
}


/*********************************************************************
**    E_FUNCTION     : int uu_list_init1(list, item_size, init_cnt, exp_cnt)
**       Initialize a list, return status
**    PARAMETERS
**       INPUT  :
**          list				pointer to UU_LIST structure
**       OUTPUT :
**          list				filled in
**    RETURNS      :
**       UU_FAILURE iff failed to allocate memory; else UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uu_list_init1(list, item_size, init_cnt, exp_cnt)
UU_LIST			*list;
int				item_size;
int				init_cnt;
int				exp_cnt;
{
	uu_list_init(list, item_size, init_cnt, exp_cnt);
	if(init_cnt > 0 && list->data == UU_NULL)
		return (UU_FAILURE);
	else
		return (UU_SUCCESS);
}


/*********************************************************************
**    E_FUNCTION     : void uu_list_free(list)
**       Free (dellocate) a list
**    PARAMETERS
**       INPUT  :
**          list				pointer to UU_LIST structure
**       OUTPUT :
**          list				data pointer is NULLED and current count is zeroed
**    RETURNS      : nothing
**    SIDE EFFECTS : list storage is free'd
**    WARNINGS     : No check for a valid list is made
*********************************************************************/

void uu_list_free(list)
UU_LIST			*list;
{
	uu_denter(UU_UITRC,(us, "uu_list_free: *list=%x", list));

	/* if there is storage to free ... */
	if(list->data)
		/* free the storage */
		uu_free(list->data);

	/* clear counts */
	list->data = UU_NULL;
	list->cur_cnt = 0;

	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION     : void uu_list_push(list, item)
**       Push (append) an item onto a list
**    PARAMETERS
**       INPUT  :
**          list				pointer to UU_LIST structure
**				item				pointer to item
**       OUTPUT :
**          list				list is modified
**    RETURNS      : nothing
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uu_list_push(list, item)
UU_LIST			*list;
char				*item;
{
	uu_denter(UU_UITRC,(us, "uu_list_push: *list=%x,*item=%x", list, item));

	/* if there is not enough room ... */
	if(list->cur_cnt == list->max_cnt)
		adjust_list(list, 1);

	/* move data to end of list */
	copy_down(item, END_OF_LIST( list), list->item_size );

	/* bump item count */
	list->cur_cnt++ ;

	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION     : void uu_list_push_multiple(list, count, items)
**       Push (append) an array of items onto a list
**    PARAMETERS
**       INPUT  :
**          list				pointer to UU_LIST structure
**				count				number of items to push
**				items				item array
**       OUTPUT :
**          list				list is modified
**    RETURNS      : nothing
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uu_list_push_multiple(list, count, items)
UU_LIST			*list;
int				count;
char				*items;
{
	int				left = list->max_cnt - list->cur_cnt;

	uu_denter(UU_UITRC,(us,
		"uu_list_push_multiple: *list=%x,count=%d,*items=%x",
		list, count, items));

	/* if there is not enough room ... */
	if(count > left)
		adjust_list(list, count);

	/* move data to end of list */
	copy_down(items, END_OF_LIST( list), count * list->item_size );

	/* bump item count */
	list->cur_cnt += count;

	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION     : void uu_list_push_list(list1, list2)
**       Push (append) one list onto another
**    PARAMETERS
**       INPUT  :
**          list1				pointer to list on which to append
**          list2				pointer to list to be appended
**       OUTPUT :
**          list1				list is modified
**    RETURNS      : nothing
**    SIDE EFFECTS : none
**    WARNINGS     : the second list is not deallocated
*********************************************************************/

void uu_list_push_list(list1, list2)
UU_LIST			*list1;
UU_LIST			*list2;
{
	int				left = list1->max_cnt - list1->cur_cnt;

	uu_denter(UU_UITRC,(us, "uu_list_push_list: *list1=%x,*list2=%x",
		list1, list2));

	/* if there is not enough room ... */
	if(UU_LIST_LENGTH(list2) > left)
		adjust_list(list1, UU_LIST_LENGTH(list2));

	/* move data to end of list */
	copy_down(
		UU_LIST_ARRAY(list2),
		END_OF_LIST(list1),
		UU_LIST_LENGTH(list2) * list2->item_size
	);

	/* bump item count */
	list1->cur_cnt += UU_LIST_LENGTH(list2);

	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION     : void uu_list_pop(list, item)
**       Pop (remove) an item from a list
**    PARAMETERS
**       INPUT  :
**          list				pointer to list
**       OUTPUT :
**				item				data from last item
**    RETURNS      : nothing
**    SIDE EFFECTS : list is truncated
**    WARNINGS     : none
*********************************************************************/

void uu_list_pop(list, item)
UU_LIST			*list;
char				*item;
{
	uu_denter(UU_UITRC,(us, "uu_list_pop: *list=%x,*item=%x", list, item));

	/* move data from end of list */
	copy_down(END_OF_LIST(list) - list->item_size, item, list->item_size);

	/* decrement item count and
		if empty space is greater than 2 * expansion count ... */
	if((list->max_cnt - (--list->cur_cnt)) > (list->exp_cnt + list->exp_cnt))
		CONTRACT_LIST(list);

	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION     : void uu_list_pop_multiple(list, count, items)
**       Pop (remove) an array of items from a list
**    PARAMETERS
**       INPUT  :
**          list				pointer to list
**				count				number of items to remove
**       OUTPUT :
**				items				data removed from list
**    RETURNS      : nothing
**    SIDE EFFECTS : list is truncated
**    WARNINGS     : none
*********************************************************************/

void uu_list_pop_multiple(list, count, items)
UU_LIST			*list;
int				count;
char				*items;
{
	int				size = count * list->item_size;

	uu_denter(UU_UITRC,(us,
		"uu_list_pop_multiple: *list=%x,count=%d,*items=%x",
		list, count, items
	));

	/* move data from end of list */
	copy_down(END_OF_LIST(list) - size, items, size);

	/* decrement item count */
	list->cur_cnt -= count;

	/* if empty space is greater than 2 * expansion count ... */
	if((list->max_cnt - list->cur_cnt) > (list->exp_cnt + list->exp_cnt))
		CONTRACT_LIST(list);

	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION     : void uu_list_pop_list(list1, count, list2)
**       Pop (remove) a list from the end of another list
**    PARAMETERS
**       INPUT  :
**          list1				pointer to list from which to remove
**				count				number of items to remove
**       OUTPUT :
**          list1				pointer to list to which removed items are appended
**    RETURNS      : nothing
**    SIDE EFFECTS : list is truncated
**    WARNINGS     : none
*********************************************************************/

void uu_list_pop_list(list1, count, list2)
UU_LIST			*list1;
int				count;
UU_LIST			*list2;
{
	int				size = count * list1->item_size;

	uu_denter(UU_UITRC,(us,
		"uu_list_pop_list: *list1=%x,count=%d,list2=%x",
		list1, count, list2
	));

	/* append data to list2 */
	uu_list_push_multiple(list2, count, END_OF_LIST(list1) - size);

	/* decrement list1's item count */
	list1->cur_cnt -= count;

	/* if empty space is greater than 2 * expansion count ... */
	if((list1->max_cnt - list1->cur_cnt) > (list1->exp_cnt + list1->exp_cnt))
		CONTRACT_LIST(list1);

	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION     : void uu_list_delete(list, index, count)
**       Delete items from a list
**    PARAMETERS
**       INPUT  :
**          list			pointer to list
**				index			position of first item to delete
**				count			number of items to delete
**       OUTPUT :
**          list			list is updated
**    RETURNS      : nothing
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uu_list_delete(list, index, count)
UU_LIST			*list;
int				index;
int				count;
{
	int				tail = index + count;

	uu_denter(UU_UITRC,(us,
		"uu_list_delete: *list=%x,index=%d,count=%d",
		list, index, count
	));

	/* if there are items after deleted area ... */
	if(tail < list->cur_cnt)
		/* copy items down from after deleted area */
		copy_down(
			LIST_INDEX(list, tail),
			LIST_INDEX(list, index),
			list->item_size * (UU_LIST_LENGTH( list) - tail )
		);

	/* update item count */
	list->cur_cnt -= count;

	/* if empty space is greater than 2 * expansion count ... */
	if((list->max_cnt - list->cur_cnt) > (list->exp_cnt + list->exp_cnt))
		CONTRACT_LIST(list);

	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION     : void uu_list_insert(list, index, item)
**       Insert an item into a list
**    PARAMETERS
**       INPUT  :
**          list			pointer to list
**				index			position at which item is to be inserted
**				item			pointer to item
**       OUTPUT :
**          list			modified
**    RETURNS      : nothing
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uu_list_insert(list, index, item)
UU_LIST				*list;
int					index;
char					*item;
{
	uu_denter(UU_UITRC,(us, "uu_list_insert: *list=%x,index=%d,*item=%x",
		list, index, item));

	/* if there is not enough room ... */
	if(list->cur_cnt == list->max_cnt)
		adjust_list(list, 1);

	/* move the data after the insert position back one */
	copy_up(
		LIST_INDEX(list, index),
		LIST_INDEX(list, index+1),
		list->item_size * (UU_LIST_LENGTH( list) - index )
	);

	/* insert the item */
	copy_down(item, LIST_INDEX( list, index), list->item_size );

	/* bump the current count */
	list->cur_cnt++ ;

	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION     : void uu_list_insert_multiple(list, index, count, items)
**       Insert an array of items into a list
**    PARAMETERS
**       INPUT  :
**          list			pointer to list
**				index			position at which item is to be inserted
**				count			number of items to insert
**				items			pointer to array of items
**       OUTPUT :
**          list			modified
**    RETURNS      : nothing
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uu_list_insert_multiple(list, index, count, items)
UU_LIST				*list;
int					index;
int					count;
char					*items;
{
	int				left = list->max_cnt - list->cur_cnt;

	uu_denter(UU_UITRC,(us,
		"uu_list_insert_multiple: *list=%x,index=%d,count=%d,*items=%x",
		list, index, count, items
	));

	/* if there is not enough room ... */
	if(count > left)
		adjust_list(list, count);

	/* move the data after the insert position back */
	copy_up(
		LIST_INDEX(list, index),
		LIST_INDEX(list, index+count),
		list->item_size * (UU_LIST_LENGTH( list) - index )
	);

	/* move data into list */
	copy_down(items, LIST_INDEX( list, index), count * list->item_size );

	/* bump item count */
	list->cur_cnt += count;

	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION     : void uu_list_insert_list(list1, index, list2)
**       Insert a list into a list
**    PARAMETERS
**       INPUT  :
**          list1			pointer to list in which to insert
**				index			position in list1 at which list2 is to be inserted
**          list2			pointer to list to insert
**       OUTPUT :
**          list1			modified
**    RETURNS      : nothing
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void uu_list_insert_list(list1, index, list2)
UU_LIST				*list1;
int					index;
UU_LIST				*list2;
{
	uu_denter(UU_UITRC,(us,
		"uu_list_insert_list: *list1=%x,index=%d,list2=%x",
		list1, index, list2
	));

	uu_list_insert_multiple(
		list1,
		index,
		UU_LIST_LENGTH(list2),
		UU_LIST_ARRAY(list2)
	);

	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION     : void uu_list_sort(list, compare)
**       Sort a list
**    PARAMETERS
**       INPUT  :
**          list				pointer to list
**				compare			pointer to a compare function called as
**										stat = (*compare) (a, b)
**											where,
**												a, b		pointers to items to compare
**												stat		negative, zero, or positive for
**															a < b, a == b, and a > b
**       OUTPUT :
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : list is sorted
**    WARNINGS     : none
*********************************************************************/

void uu_list_sort(list, compare)
UU_LIST			*list;
int				(*compare)();
{
	uu_denter(UU_UITRC,(us,
		"uu_list_sort: *list=%x,compare=%x",
		list, compare
	));

	uu_qsort(
		UU_LIST_ARRAY(list),
		UU_LIST_LENGTH(list),
		list->item_size,
		compare
	);

	uu_dexit;
}


/*********************************************************************
**    E_FUNCTION     : void uu_list_expand(list, count)
**       Expand a list
**    PARAMETERS
**       INPUT  :
**          list				pointer to list
**				count				number of items by which to expand
**       OUTPUT :
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS : list storage is expanded by the given number of items
**								without initializing the new storage
**    WARNINGS     : none
*********************************************************************/

void uu_list_expand(list, count)
UU_LIST			*list;
int				count;
{
	int				left = list->max_cnt - list->cur_cnt;

	uu_denter(UU_UITRC,(us,
		"uu_list_expand: *list=%x,count=%d",
		list, count
	));

	/* if there is not enough room ... */
	if(count > left)
		adjust_list(list, count);

	/* bump item count */
	list->cur_cnt += count;

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     : void uu_list_move(list, index1, index2, count)
**       Move 'count' list items starting from index2 to the position
**       right after index1.
**       Note: index2 must be after index1.
**    PARAMETERS
**       INPUT  :
**          list           pointer to list
**          index1
**          index2
**          count          number of items
**       OUTPUT :
**          none
**    RETURNS      : nothing
**    SIDE EFFECTS :
**    WARNINGS     : none
*********************************************************************/
void uu_list_move(list, index1, index2, count)
UU_LIST        *list;
int            index1, index2, count;
{
   int size = 0;
   char* items;

   uu_denter(UU_UITRC,(us,
      "uu_list_move: *list=%x,index1=%d,index2=%d,count=%d",
      list, index1, index2, count
   ));

   if (index1+1 >= index2 || count<=0) goto Done;
/*
..... Make sure index2 comes after index1
*/
   if(index2+count > list->cur_cnt) goto Done;
/*
..... Make sure there are count items after index2
*/

   size = count * list->item_size;
   items = uu_malloc(size);
   copy_down(LIST_INDEX(list, index2), items, size);
   uu_list_delete(list, index2, count);
   uu_list_insert_multiple(list, index1+1, count, items);

Done:;
   uu_dexit;
}



/* Local Functions */

/* adjust a list by a number of items */

static void adjust_list(list, count)
UU_LIST			*list;
int				count;
{
	int isiz;
	char				*new_data;

	uu_denter(UU_UITRC,(us,
		"adjust_list: *list=%x,count=%d",
		list, count
	));

	/* compute new max count */
	list->max_cnt =
		list->cur_cnt + (((count-1)/list->exp_cnt)+1) * list->exp_cnt;

	/* allocate new space */
	isiz = list->item_size * list->max_cnt;
	isiz = (((isiz-1)/8)+1) * 8;
	new_data = uu_malloc(isiz);

	/* move the data */
	copy_down(list->data, new_data, list->item_size * list->cur_cnt);

	/* free the old space */
	if (list->data)					/** FIX: fixed by jimm 9/10/85pm	**/
		uu_free(list->data);

	/* update the data pointer */
	list->data = new_data;

	uu_dexit;
}

/* copy data down in-line */

static void copy_down(from, to, n)
char		*from;
char		*to;
int		n;
{
	while(n--)
		*to++ = *from++ ;
}

/* copy data up in-line */

static void copy_up(from, to, n)
char		*from;
char		*to;
int		n;
{
	to += n;
	from += n;
	while(n--)
		*(--to) = *(--from) ;
}

#if UU_DEBUG
uu_list_debug(str, list)
char	*str;
UU_LIST *list;
{
	char	us[1000];

	uu_dprint(UU_STRC,(us,"\nList debug -- \"%s\": list header 0x%x",
			str, list));
	uu_dprint(UU_STRC,(us,"\titem size %d", list->item_size));
	uu_dprint(UU_STRC,(us,"\tcur_cnt %d", list->cur_cnt));
	uu_dprint(UU_STRC,(us,"\tmax_cnt %d", list->max_cnt));
	uu_dprint(UU_STRC,(us,"\texp_cnt %d", list->exp_cnt));
	uu_dprint(UU_STRC,(us,"\tdata 0x%x", list->data));
}
#endif
