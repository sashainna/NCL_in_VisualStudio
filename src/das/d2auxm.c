/*********************************************************************
**
**    NAME         :  auxm.c
**
**       CONTAINS:
**				UD_DASTAT ud_auxm(event)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d2auxm.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:03
**
*********************************************************************/

#include "dinput.h"
#include "dasnog.h"
#include "udebug.h"

/********************************************************************* 
**
**  I_FUNCTION		:  ud_auxm (event)
**      handles iterpretation of aux menu selection 
**
**  PARAMETERS   
**      input:  event = event buffer 
**      output: none
**
**  RETURNS      :  status of choice
**  SIDE EFFECTS :  may never return if appropriate choice is made
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_auxm(event)
UD_DEVENT *event;							/* event buffer */
{

	UD_DASTAT status;

	uu_denter(UU_DTRC,(us,"entering ud_auxm"));

	if((*event).evdev == UD_AUXMENU)
	{
		switch ((*event).indata.choicedata)
		{

/*				-- done choice --   */

			case UD_CDONE:

				status = DE_DONE;
				break;

/*				-- reject last input --   */

			case UD_CRLAST:

				status = DE_RLAST;
				break;

/*				-- reject last command --   */

			case UD_CRCOMMAND:
				status = DE_RCOMMAND;
				break;

/*				-- Restart input sequence --   */

			case UD_CRESTART:

				status = DE_RESTART;
				break;

			case UD_CDEFAULT :

				status = DE_DEFAULT;
				break ;

/*				-- Application alternate action --   */

			case UD_CALTACT:

				status = DE_ALTACT;
				break;

/*				-- Subsystem alternate action --   */

			case UD_CALTACT1:

				status = DE_ALTACT1;
				break;

/*				-- forms control input or menu navigation 
					(reject last command) --   */

			case UD_CUPFIELD:

				status = DE_UPFIELD;
				break;

			case UD_CDNFIELD:

				status = DE_DNFIELD;
				break;

			case UD_CCLFIELD:
			case UD_CTGFIELD:

			case UD_CEXITSUB:

				status = DE_RLAST;
				break;
	 	}
 	}
	uu_dprint(UU_DTRC,(us,"leave ud_auxm, status=%d", status));
	uu_dexit;
	return(status);
}
