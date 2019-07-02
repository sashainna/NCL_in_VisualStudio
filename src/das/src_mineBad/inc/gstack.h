/*********************************************************************
**    NAME         :  gstack.h
**
**				A generic macro package for manipulating a Finite length
**				Bounded Stack.  (Each element must be of the same size,
**				and the maximum depth of the stack must be known at
**				compile time.) To use, must put stack in local variable 
**				declarations by declaring:
**
**					UG_FBS_STACK(stack_name, stack_length, stack_data_type);
**
**				Then, ug_fbs_initstack must be called prior to any other
**				stack macro.
**
**  CONTAINS:
**		ug_fbs_init(stack_name)					-- Initialize the stack.
**		ug_fbs_empty(stack_name)				-- Empty the stack.
**		int 
**		ug_fbs_depth(stack_name)				-- Returns current depth of
**															stack (elements 0 - length-1
**															can be accessed).
**		stack_data_type 
**		*ug_fbs_push(stack_name)				-- Push element onto stack, 
**															return pointer to it.
**		stack_data_type 
**		*ug_fbs_pop(stack_name)					-- Pop top element from stack
**															return pointer to new top 
**															of stack.
**		stack_data_type 
**		*ug_fbs_top(stack_name)					-- Return pointer to top 
**															of stack.
**		stack_data_type 
**		*ug_fbs_ielem(stack_name, ielem)		-- Returns pointer to stack
**															element #ielem.
**
**       
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       gstack.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:23
*********************************************************************/

#ifndef GSTACKH


#define UG_FBS_STACK(S, LEN, DTYPE) \
		struct { \
			DTYPE data[LEN]; \
			int maxlen; \
			int curpos; \
			int eltlen; } S

#define ug_fbs_init(S, len, elen) \
		S.maxlen = len; \
		S.curpos = -1; \
		S.eltlen = elen

#define ug_fbs_empty(S) \
		S.curpos = -1

#define ug_fbs_push(S) \
		( (S.curpos+1 < S.maxlen) ? &(S.data[++S.curpos]) : NULL )

#define ug_fbs_pop(S) \
		((( (S.curpos>-1)?--S.curpos:-1) >-1) ? &(S.data[S.curpos]) : NULL)

#define ug_fbs_top(S) \
		( (S.curpos > -1) ? &(S.data[S.curpos]) : NULL )

#define ug_fbs_depth(S) \
		( (S.curpos > -1) ? (S.curpos+1) : 0 )

#define ug_fbs_ielem(S,i) \
		( ((i <=S.curpos) && (i >= 0)) ? &(S.data[S.curpos-i]) :  NULL )


#define GSTACKH
#endif
