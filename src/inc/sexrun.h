/*********************************************************************
**    NAME         : sexrun.h 
**       Execute expressions header
**		CONTAINS:
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       sexrun.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:51
*********************************************************************/

#ifndef SEXRUNH


#include "usysdef.h"		/* UNICAD system defines */

#include "sdecl.h"		/* declaration def's */
#include "salloc.h"		/* allocation def's */

/* the number of int words required to hold the max sized set value */
#define SETDIM		(SETMAX / BITS_PER_INT) + 1

/* type used for declaring set temporaries */
typedef unsigned US_SET [SETDIM];

/* type used for declaring bit values */
typedef unsigned US_BIT;

extern int		us_run_integer();
extern UU_REAL	us_run_real();
extern int		us_run_logical();
extern int *	us_run_vref();
extern int *	us_run_variable();
extern void		us_run_real_range();
extern void		us_run_integer_range();

extern void		us_run_coord();
extern void		us_run_transf();
extern void		us_run_cfref_assign();

extern void		us_run_string();
extern US_BIT	us_run_bit();
extern void		us_run_set();
extern void		us_run_sindex_assign();
extern void		us_run_ssub_assign();
extern void		us_run_bindex_assign();
extern void		us_run_bsub_assign();

extern e_node *	us_run_proc();
extern int *		us_run_call();


#define SEXRUNH
#endif
