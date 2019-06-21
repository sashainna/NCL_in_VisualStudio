/*********************************************************************
**    NAME         :  jauto.c
**       CONTAINS:
**				uj_auto_test
**    		uj_auto_exe()
**    		uj_auto_setup()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       jauto.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:46
*********************************************************************/

#include "usysdef.h"
#include "ustdio.h"
#include "dasg.h"
#include "dasnog.h"
#include "dinput.h"
#include "udebug.h"

static char function_name[100];		/* pass the function name */

/*********************************************************************
**
**    E_FUNCTION :  uj_auto_test(command)
**					invoke semantic action in auto test R/P file
**       
**    PARAMETERS   
**       INPUT  : 
**				 command = command line used to invoke this function
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

uj_auto_test(command)
char	*command; 				/* command line */
{
	int (uj_auto_exe)();		/* entry point */

	uu_denter(UU_DTRC,(us, "entering ud_auto_test, command=%s", &command[4]));

	strcpy(function_name, &command[4]);
	ud_subsystem(uj_auto_exe, NULL, function_name, UU_TRUE);

	uu_dexit;
}

/*********************************************************************
**
**    I_FUNCTION :  uj_auto_exe()
**					routine to pass to ud_subsystem with no arguments
**       
**    PARAMETERS   
**       INPUT  : 
**				 none
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

static uj_auto_exe()
{
	uu_denter(UU_DTRC,(us, "entering ud_auto_exe"));

	readmu(function_name);
	ur_cond_auto_save();
	ncl_cond_auto_save();

	uu_dexit;
}

/*********************************************************************
**
**    I_FUNCTION :  uj_auto_setup()
**					routine to pass to tweak select subsystem in auto test mode
**       
**    PARAMETERS   
**       INPUT  : 
**				 none
**       OUTPUT :  
**          none
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/

uj_auto_setup()
{
	UD_DEVENT event;						/* event buffer */
	static char *xsub = "\\\\K";

	uu_denter(UU_DTRC,(us, "entering ud_auto_setup"));

	if(UD_autotest == UU_TRUE)
	{

/*		-- if playing back then start selection semantic actions -- */

		if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
		{
			ud_gevt(&event, UD_strint, "dummy", 1, UD_strdev, UD_strech, UU_NULL);
		}

/*		-- if recording then put dummy event in r/p file to end
			selection sequence when playing back -- */

		else if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		{
			event.evclass = UD_STRING;
			event.indata.stringdata = xsub;
			event.evdev = 1;
			ud_rpwr(&event);
		}
	}

	uu_dexit;
}
