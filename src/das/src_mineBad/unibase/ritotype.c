#define MODULEDBG 0
/*********************************************************************
**    NAME         :  ritotype.c
**       CONTAINS:
**       ur_totype()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ritotype.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:48
*********************************************************************/

#if MODULEDBG != 0
#include "udebug.h"
#endif
#include "riddldef.h"

/*********************************************************************
**    I_FUNCTION     :  int ur_totype(type_str)
**       turns a type string into a type
**    PARAMETERS   
**       INPUT  : 
**          type_str[]	char	type string
**       OUTPUT :  
**          none
**    RETURNS      : type
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_totype(type_str)
char	type_str[];
{
	if (strcmp(type_str, "float") == 0)
	{
		return(FLOAT);
	}
	else if (strcmp(type_str, "double") == 0)
	{
		return(DOUBLE);
	}
	else if (strcmp(type_str, "real") == 0)
	{
		return(REAL);
	}
	else if (strcmp(type_str, "int") == 0)
	{
		return(INT);
	}
	else if (strcmp(type_str, "logical") == 0)
	{
		return(LOGICAL);
	}
	else if (strcmp(type_str, "character") == 0)
	{
		return(CHARACTER);
	}
	else if (strcmp(type_str, "key_id") == 0)
	{
		return(KEY_ID);
	}
	else if (strcmp(type_str, "rel_id") == 0)
	{
		return(REL_ID);
	}
	else if (strcmp(type_str, "key_ref") == 0)
	{
		return(KEY_REF);
	}
	else if (strcmp(type_str, "string") == 0)
	{
		return(STRING);
	}
	else if (strcmp(type_str, "join") == 0)
	{
		return(JOIN);
	}
	else
	{
		return(UNKNOWN_TYPE);
	}
} /* ur_totype() */

