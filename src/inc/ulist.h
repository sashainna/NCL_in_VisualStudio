/*********************************************************************
**    NAME         : ulist.h
**       dynamic list management package header
**		CONTAINS:
**			length = UU_LIST_LENGTH(list)
**			array = UU_LIST_ARRAY(list)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       ulist.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:04
*********************************************************************/

#ifndef ULISTH

typedef struct
{
	int item_size;				/* storage required by each item in bytes */
	int cur_cnt;				/* current item count */
	int max_cnt;				/* maximum item count */
	int exp_cnt;				/* expansion item count */
	char *data;					/* pointer to data array */
} UU_LIST;

#define NULLST (UU_LIST *)UU_NULL

#define UU_LIST_LENGTH(list) ((*list).cur_cnt)
#define UU_LIST_ARRAY(list) ((*list).data)
#define UU_LIST_EMPTY(list) ((*list).cur_cnt = 0)
#define UU_LIST_SMALL(list,n) ((n) > 2*(*list).max_cnt)
#define UU_LIST_EXPANDS(list,n) ((n)+(*list).cur_cnt > (*list).max_cnt)
#define UU_LIST_SETEXP(list,n) ((*list).exp_cnt = (n))
#define UU_LIST_DOUBLEXP(list) ((*list).exp_cnt *= 2)
#define UU_LIST_NULLPTR(list) ((*list).data == UU_NULL)

#define ULISTH
#endif
