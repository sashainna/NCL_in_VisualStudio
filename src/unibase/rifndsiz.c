#define MODULEDBG 0
/*********************************************************************
**    NAME         :  rifndsiz.c
**       CONTAINS:
**       ur_findsiz()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rifndsiz.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:43
*********************************************************************/

#if MODULEDBG != 0
#include "udebug.h"
#endif
#include "usysdef.h"
#include "riddldef.h"

/*********************************************************************
**    I_FUNCTION     :  int ur_findsiz(type)
**       returns the size given the type.
**    PARAMETERS   
**       INPUT  : 
**          type	int	the type of field as in attribute definition.
**       OUTPUT :  
**          none
**    RETURNS      : size in bytes of the input type
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_findsiz(type)
int	type;
{
	int	siz;

	switch(type)
	{
		case FLOAT:
			siz = sizeof(float);
			break;
		case DOUBLE:
			siz = sizeof(double);
			break;
		case REAL:
			siz = sizeof(UU_REAL);
			break;
		case INT:
			siz = sizeof(int);
			break;
		case LOGICAL:
			siz = sizeof(UU_LOGICAL);
			break;
		case CHARACTER:
			siz = sizeof(char);
			break;
		case KEY_ID:
		case REL_ID:
		case KEY_REF:
			siz = sizeof(UU_KEY_ID);
			break;
		case STRING:
		case JOIN:
			siz = sizeof(char*) + sizeof(int)*2;
			break;
		default:
			siz = 0;
			break;
	}
	return(siz);
} /* ur_findsiz() */

