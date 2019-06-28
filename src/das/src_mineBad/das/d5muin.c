/*********************************************************************
**
**    NAME         :  d5muin.c
**    CONTAINS:
**					ud_dtmenu
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       d5muin.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:12
**
*********************************************************************/

#include "usysdef.h"
#include "dasnog.h"
#include "udebug.h"

/**************************************************************************
**
**  E_FUNCTION         :  ud_dtmenu(pet)
**      menu modification tim
**
**  PARAMETERS   
**      INPUT  : 
**
**				type = event type
**				device = device number
**          pet = new prompt and echo type
**      OUTPUT :  
**				none
**
**  RETURNS      :  none
**  SIDE EFFECTS :  updates Dtimev, Dtimpet in dascom
**  WARNINGS     :  none
**
**************************************************************************/

ud_dtmenu(pet)
int pet;							/* new prompt and echo type */
{

	UD_chcech = pet;

/*	-- time to reload popup menus -- */

	udi_mu_reset();
	return;
}
