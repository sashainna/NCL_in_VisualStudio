/*********************************************************************
**    NAME         : spublic.h 
**       Public symbol table header 
**		CONTAINS:
**			stat = us_public_merge()
**			us_public_init()
**			us_public_free()
**			us_public_find()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       spublic.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:52
*********************************************************************/

#ifndef SPUBLICH


#include "sexec.h"		/* executive def's */

/* structure for public symbol data entries */
typedef struct
{
	st_sym		*p_sym;			/* pointer to symbol */
	n_flist		*p_file;			/* pointer to file */
	d_node		*p_sym_data;	/* pointer to symbol data */
	int			p_flag;			/* TRUE if p_file and p_sym_data point to the
											defining (PUBLIC) occurrences of a 
											variable or a procedure */
	int			p_loc;			/* location for variables and procedures */
} n_public;

#ifndef USPUBLICC
	extern int us_global_loc;
#endif

extern int			us_public_merge();
extern void			us_public_init();
extern void			us_public_free();
extern n_public	*us_public_find();


#define SPUBLICH
#endif
