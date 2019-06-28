/*********************************************************************
**    NAME         :  utilinit.c
**       CONTAINS:
**       uu_init_debug
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       utilinit.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:55
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"

/*********************************************************************
**    E_FUNCTION     :  int uu_init_debug()
**       description
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uu_init_debug()
{

	/*	local varibles	*/
	char	*p;
	char	*getenv();


	/* initialize the debug sybsystem	*/
#ifndef UU_DEBUGOFF
	uu_trap();
#endif
	p = getenv("UU_debmask");
	if (p == UU_NULL)
		UU_debmask = 0;
	else
		sscanf(p, "%d", &UU_debmask);

}

