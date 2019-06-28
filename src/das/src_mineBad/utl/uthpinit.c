/*********************************************************************
**    NAME         :  uthpinit.c
**       CONTAINS:
**			uu_init_hep
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       uthpinit.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:55
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"

/*********************************************************************
**    E_FUNCTION     :  int uu_init_hep()
**       Initialize the help, error, and prompt subsystem
**
**    PARAMETERS   
**       INPUT  : 
**          NONE
**       OUTPUT :  
**          NONE
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uu_init_hep()
{

	/*	Initialize the HEP subsystem	*/

	uu_inithep();
	uu_initapp();

}
