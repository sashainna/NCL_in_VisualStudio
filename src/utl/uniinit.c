/*********************************************************************
**    NAME         :  uniinit.c
**
**       CONTAINS:
**       	uu_init_unicad
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       uniinit.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:55
**
*********************************************************************/

#include "udebug.h"
#include "usysdef.h"

/*********************************************************************
**    E_FUNCTION     :  int uu_init_unicad ()
**       This routine initializes the UNICAD system.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : 
**							The order that the routines are called in
**							is very important and should not be changed.
*********************************************************************/

uu_init_unicad()
{

/* -- Initialize and reset the MPE portion of the system -- */

	uu_init_mpe();
	uu_reset_mpe();

/*	-- Initialize and reset the user application -- */

	uu_init_usr_app();
	uu_reset_usr_app();
}
