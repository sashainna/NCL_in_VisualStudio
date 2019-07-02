/*********************************************************************
**    NAME         :  riddldef.h
**       CONTAINS:
**       structure for attribute definitions in uddl
**			tokens for uddl data types and alignment size.
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tzddldef.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:59
*********************************************************************/

#ifndef RIDDLDEFH


#include "usysdef.h"

struct attr_def
{
	char	attr_name[33];
	int	attr_type;				/* attribute type identifier */
	int	attr_size;				/* size in bytes */
	int	attr_off;				/* offset to next field */
	unsigned long	attr_flags; /* parse flags */
	int	num_rows;				/* for arrays */
	int	num_cols;				/*   "  */
} ;

/* bit definitions for flags */
#define UR_ARRAY 1
#define UR_PTR 2
#define UR_CTRLLINE 3

/* definitions for UniDDL data types & alignment size */

/* tokens for types recognized in UniDDL */
#define FLOAT 1
#define DOUBLE 2
#define INT 3
#define LOGICAL 4
#define CHARACTER 5
#define KEY_ID 6
#define REAL 7
#define JOIN 8
#define STRING 9
#define REL_ID 10
#define UNKNOWN_TYPE -1

/* constant for alignment */
#define ALIGNSZ 2
#if UU_COMP==UU_VAXVMS
#undef ALIGNSZ
#define ALIGNSZ 1
#endif
#if UU_COMP==UU_PYRAMID
#undef ALIGNSZ
#define ALIGNSZ 4
#endif
#if UU_COMP==UU_VAXULTRIX
#undef ALIGNSZ
#define ALIGNSZ 4
#endif
#if UU_COMP==UU_MASSCOMP
#undef ALIGNSZ
#define ALIGNSZ 4
#endif

/* tokens for control commands */
#define UR_OUTPUT 1
#define UR_PREFIX 2
#define UR_SUFFIX 3
#define UR_UNIBASE 4

/* tokens for relation types */
#define UR_TABLE 1
#define UR_MODAL 2


#define RIDDLDEFH
#endif
