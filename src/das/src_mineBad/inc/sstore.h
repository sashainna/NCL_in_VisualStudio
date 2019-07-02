/*********************************************************************
**    NAME         : sstore.h
**       SALEX storage management header
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       sstore.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:52
*********************************************************************/

#ifndef SSTOREH


#include "snode.h"	/* general node def's */

/* size of standard storage block in (int) units */
#define	STBLKSIZE	1024		

/* storage block */
typedef struct t_block
{
	struct t_block	*blk_next;				/* next block in table */
	int				blk_left;				/* number of words left */
	/* blk_data must be on a double-word boundary for the RIDGE */
	int				blk_data[ 1 ];			/* data area to be allocated */
} st_block;

#ifndef USSTOREC
	extern st_block	*us_main_store;
#endif

extern void 		us_store_init();
extern st_block 	*us_store_create();
extern void 		us_store_free();
extern void 		us_store_current();
extern void 		us_store_push();
extern void 		us_store_pop();
extern int *		us_alloc();
extern int *		us_alloc_int();
extern d_node *	us_alloc_node();
extern char *		us_alloc_string();
extern void			us_zero();


#define SSTOREH
#endif
