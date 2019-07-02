/*********************************************************************
**    NAME         : snode.h 
**       Typedef for basic nodes
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       snode.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:51
*********************************************************************/

#ifndef SNODEH


/* basic declaration node */
typedef struct t_dnode
	{
	short		tag;		/* tag = xxxxNODE */
	int *		sym;		/* pointer back to symbol */
	/* the following fields are present in data type nodes only */
	int		size;		/* size of type in (int) units */
	/* int		copy;	IMPLEMENT	/* unique number for name of copy procedure;
								0 if type not yet examined, 
								-1 if copy function not needed 
							*/
	} d_node;

/* basic statement node */
typedef struct t_snode
	{
	short					tag;			/* tag = xxxxNODE */
	struct t_snode *	st_next;		/* next statement */
	short					st_line;		/* line number */
	} s_node;

/* basic expression node */
typedef struct t_enode
	{
	short					tag;			/* tag = xxxxNODE */
	d_node *				e_type;		/* expression type */
	} e_node;

/* basic constant node */
typedef struct t_cnode
	{
	short					tag;			/* tag = xxxxNODE */
	d_node *				e_type;		/* pointer to constant type */
	d_node *				uconst;		/* pointer to constant node */
	} c_node;

#define SNODEH
#endif
