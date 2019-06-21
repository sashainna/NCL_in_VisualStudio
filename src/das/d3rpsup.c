/*********************************************************************
**
**    NAME         :  d3rpsup.c
**
**       CONTAINS:
**					ud_echoline
**					ud_eraser
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d3rpsup.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:07
**
*********************************************************************/

#include "usysdef.h"
#include "dinput.h"
#include "dasnog.h"
#include "uhep.h"
#include "udebug.h"

/*********************************************************************
**    E_FUNCTION :  ud_echoline(cptr)
**       description
**    PARAMETERS   
**       INPUT  : 
**          cptr = command line used to invoke function
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ud_echoline(cptr)
char *cptr;						/* command line used to invoke subsystem */
{

	UU_LOGICAL	ud_rprd();
	UD_DEVENT event;
	char buffer[100];
	int i;

	{
		if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
		{

/*			-- echo the next line -- */

			ud_rprd(&event, UU_FALSE);
			ud_prmerr(event.indata.stringdata);
		}
		else if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		{

/*			-- get the next echo line -- */


/*---		"echo line>"	---*/

			ud_ldas(UD_DASSTRING, UD_DASHEP, 4, buffer, 100, &i, UD_NODEFAULT);
		}
	}
}

/*********************************************************************
**    E_FUNCTION :  ud_eraser(cptr)
**       front end to erase error messages
**    PARAMETERS   
**       INPUT  : 
**          cptr = command line used to invoke function
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ud_eraser(cptr)
char *cptr;						/* command line used to invoke subsystem */
{
	ud_killmsg(UD_ERRORMSG);
}
