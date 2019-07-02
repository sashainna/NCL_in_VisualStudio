/*********************************************************************
**    NAME         :  rbase.h
**       CONTAINS:
**       UNIBASE generic data packet structures.
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rbase.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:42
*********************************************************************/

#ifndef RBASEH
#include "usysdef.h"

#define	UR_DATA_BUFF_MAX	25000 /* matches class buffer defined in class.h */
#define	UR_ATTR_BUFF_MAX	4096
#define	UR_TRANSF_BUFF_MAX	128
#define	UR_MAX_CHARS		8				/* max chars in rel name */
typedef unsigned long	UR_KEY_ID;
typedef unsigned long	UR_TUPLE_INDX;
typedef unsigned long   UR_REL_NUM;
typedef long            UR_USE_COUNT;
typedef char	UR_REL_NAME [ UR_MAX_CHARS+1 ];	/* array to hold a rel name */

struct UR_data		/* relation control block record structure */
{
	UU_KEY_ID		key_id	;	/* a master tuple identifier		*/
	long				rel_num	;	/* relation number of data			*/
	char				data[UR_DATA_BUFF_MAX]	;
};

struct UR_attr     /* attribute relation (generic) data structure*/
{
		UU_KEY_ID 		key_id  ;  /* master tuple identifier */
		long          rel_num ;  /* relation number */
		long          use_count; /* use count for tuple */
/*
		the following are the required attributes for each attribute bundle
*/

		int		color;
		int		layer;
		int		pen;
		int		line_style;
		UU_REAL		line_weight;
		UU_REAL		line_width;
		int		displayable;
		UU_LOGICAL		selectable;
/*
		the following is provided as a storage area for implementation
		required attributes
*/

		char          data[UR_ATTR_BUFF_MAX] ;
} ;

struct UR_transf   /* transformation relation (generic) data structure*/
{
		UU_KEY_ID 		key_id  ;  /* master tuple identifier */
		long          rel_num ;  /* relation number */
		long          use_count; /* use count for tuple */
		char          data[UR_TRANSF_BUFF_MAX] ;
} ;
#define RBASEH
#endif
