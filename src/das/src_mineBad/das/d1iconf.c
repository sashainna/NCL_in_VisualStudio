/*********************************************************************
**
**    NAME         :  diconif
**
**       CONTAINS:
**       	ud_iconif
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d1iconf.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:02
**
*********************************************************************/

#include "usysdef.h"
#include "dasnog.h"
#include "dinput.h"
#include "dmenucom.h"
#include "udebug.h"

#include "nclicons.h"

/*********************************************************************
**
**    E_FUNCTION :  	ud_iconif()
**
**       main controller for icon only interface
**    PARAMETERS   
**
**       INPUT  : 
**          none
**
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none
**
*********************************************************************/

ud_iconif()
{

	UD_DEVENT event;						/* input event buffer */
	static char *nullstr = "";			/* null string */

	uu_denter(UU_DTRC,(us, " entering ud_iconf"));

/*	-- increment number of times menu navigator has been called -- */

	UDI_muwhich++;

	while(UU_TRUE)
	{
		ud_gevt(&event, UD_CHOICE, &nullstr, 1, 1, 1, NULL);

/*		-- if we return and we do not have a string event of "done", then
			an error ocurred, probably a bad program on the device -- */

		if(event.evclass == UD_STRING)
		{
			if(ud_strcomp(event.indata.stringdata, "done") == -1)
				break;
			else
				ug_beep();
		}
		else
			{
			if ((event.evclass== UD_CHOICE) && (event.evdev == 5))
				{
				if ((UU_application == UU_NCLCAM) && ((ICON_UP == MOTION_ICONS) || 
					(ICON_UP == MODEL_ICONS)))
					{
					UD_ICMU_DN(ICON_UP);
					ICON_UP = 0;
					ul_menu_reset();
					}
				}
			else
				ug_beep();
			}
	}
	uu_dexit;
}
