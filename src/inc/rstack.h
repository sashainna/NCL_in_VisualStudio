/*********************************************************************
**    NAME         :  rstack.h
**       CONTAINS:
**       Unibase stack definition, refernce include file
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rstack.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:48
*********************************************************************/

#ifndef RSTACKH
#define RSTACKH
#include "ustdio.h"

/* a unibase stack can be defined and initialize or referenced by
 * using one of the two following constructs
 *
 * definition and initialization
 *		UR_STACK(NAME,NUM_ELEM,ELEM_TYPE) = {NUM_ELEM,-1} ;
 *
 * referenced
 *		extern UR_STACK(NAME,NUM_ELEM,ELEM_TYPE) ;
 */

#define UR_STACK(NAME, NUM_ELEM, ELEM_TYPE) \
	struct { \
		int			num_elem; \
		int			curpos; \
		ELEM_TYPE	data[NUM_ELEM]; \
	} NAME
#define ur_init_stack(NAME, NUM_ELEM) \
	NAME.num_elem	=	NUM_ELEM; \
	NAME.curpos = -1;

/* an element may be pushed onto the stack by using the following 
 *		method. Note you must first get a point to the next avaiable
 *		element on the stack before putting data in it, and that after
 *		the first push you will get the stack pointer to the first element
 *
 *		stk_ptr = ur_stack_top(NAME) ;
 *
 *		stk_ptr->stuff = 1/2 ;
 *
 *		stk_ptr = ur_push(NAME) ;
 */

#define ur_push(NAME) \
	++NAME.curpos != 0 ? \
		((NAME.curpos != NAME.num_elem) ? &(NAME.data[NAME.curpos]) : NULL ) : \
		&(NAME.data[++NAME.curpos])

/* an element may be popped off the stack using:
 *		stk_ptr = ur_pop(NAME) ;
 *	thereafter stk_ptr can be used to address the data from the stack
 */

#define ur_pop(NAME) \
	(((NAME.curpos >  -1) ? --NAME.curpos : -1) >-1) ? \
		&(NAME.data[NAME.curpos]) : NULL
		
#define ur_stack_top(NAME) \
	NAME.curpos > -1 ? \
		((NAME.curpos < NAME.num_elem) ? \
			&(NAME.data[NAME.curpos]) : NULL) : &(NAME.data[0])

/* it can be determined how many unused elements exist on the stack
 *	with the following construct
 *		num_unused = ur_stack_space(NAME)
 */

#define ur_stack_space(NAME) \
		NAME.num_elem - 1 - NAME.curpos

/* define various stack elements */
struct	dstack_element
{
	long				mark		;	/* TRUE  if beginning of a delete sequence*/
	unsigned	long	del_key	;	/* key of thing deleted(phantom tuple)		*/
}	;
typedef	struct	dstack_element	dstack_element	;

/* define the various stack depths */
#define	UR_DEL_STK_DEPTH	512

#endif
