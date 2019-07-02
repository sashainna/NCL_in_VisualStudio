
/*********************************************************************
**    NAME         :  tabledef.h
**       CONTAINS:
**       structure definition for the table table
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       tabledef.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:53
*********************************************************************/

#ifndef TABLEDEFH


#define UTI_MAXREL 200

#include "usysdef.h"

struct tbltblent								/* entries in the table table */
{
	char	relnm[16];							/* relation name */
	int	reltyp;								/* relation type */
	int	numjoins;							/* number of joined relations */
	UU_LOGICAL	joined;						/* is it the object of join? */
	UU_LOGICAL	simp;							/* is it simple? */
	int			field_type;					/* what is field type for simple */
} ;


#define TABLEDEFH
#endif
