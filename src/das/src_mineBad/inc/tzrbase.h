/*********************************************************************
**    NAME         :  rbase.h
**       CONTAINS:
**       UNIBASE generic data packet structures.
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tzrbase.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:01
*********************************************************************/

#ifndef TZRBASEH
#include "usysdef.h"

#define	TZ_DATA_BUFF_MAX	25000 /* matches class buffer defined in class.h */
#define	TZ_ATTR_BUFF_MAX	4096
#define	TZ_TRANSF_BUFF_MAX	128
typedef unsigned long	TZ_KEY_ID;
typedef unsigned long	TZ_TUPLE_INDX;
typedef unsigned long   TZ_REL_NUM;
typedef long            TZ_USE_COUNT;

struct TZ_data		/* relation control block record structure */
{
	UU_KEY_ID		key_id	;	/* a master tuple identifier		*/
	long				rel_num	;	/* relation number of data			*/
	char				data[TZ_DATA_BUFF_MAX]	;
};

struct TZ_attr     /* attribute relation (generic) data structure*/
{
		UU_KEY_ID 		key_id  ;  /* master tuple identifier */
		long          rel_num ;  /* relation number */
		long          use_count; /* use count for tuple */
/*
		the following are the required attributes for each attribute bundle
*/

		int		color			;
		int		layer			;
		int		pen			;
		int		line_style	;
		UU_REAL		line_weight	;
		UU_REAL		line_width	;
		int		displayable	;
		int		selectable	;
		int		blanked		;
/*
		the following is provided as a storage area for implementation
		required attributes
*/

		char          data[TZ_ATTR_BUFF_MAX] ;
} ;

struct TZ_transf   /* transformation relation (generic) data structure*/
{
		UU_KEY_ID 		key_id  ;  /* master tuple identifier */
		long          rel_num ;  /* relation number */
		long          use_count; /* use count for tuple */
		char          data[TZ_TRANSF_BUFF_MAX] ;
} ;
#define TZRBASEH
#endif
