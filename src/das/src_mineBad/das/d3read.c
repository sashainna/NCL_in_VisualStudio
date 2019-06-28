/*********************************************************************
**    NAME         : d3read.c
**       CONTAINS:
**  		ud_read(maxlen, event)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       d3read.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:06
*********************************************************************/

#include "usysdef.h"
#include "dasnog.h"
#include "dinput.h"
#include "gimisc.h"
#include "udebug.h"

/**************************************************************************
**
**  I_FUNCTION         :  ud_read(maxlen, event)
**
**      read data of length up to maxlen from DIGS input Q
**
**  PARAMETERS   
**		INPUT:
**				maxlen = length of input buffer
**				inparm = input parameter block
**      OUTPUT :  
**          data = buffer to place input data into
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/

int ud_read(maxlen, event, inparm)
int maxlen;								/* buffer size */
UD_DEVENT *event;						/* event buffer */
UD_EVENTDEF *inparm;					/* string input parameters */
{
	
	UU_LOGICAL	ud_revt();
	UU_LOGICAL	i;
	static char	tbuffer[80];
	UU_LOGICAL	loop;								/* loop control */

	Gevent *gawaitevent();					/* wait for event */
	Gevent *answer;							/* pointer to input record */

	uu_denter(UU_DTRC,(us, "entering ud_read, len=%d, buf=%x", maxlen, event));

/* -- get next event from gks -- */
			
	i = UU_FALSE;
	answer = gawaitevent((UU_REAL) 0.);

	uu_dprint(UU_DTRC,(us, "in ud_read point1, gawait=%x, stat=%d",
						answer, i));

/*		-- if no events on Q, request input -- */
/*
...... move this value to higher level function ud_gevt1
...... Yurong
*/
/*	UD_pickmode = 1;  */
	if(answer == NULL)
	{
		if((*inparm).evtype != UD_ANY)
		{
			loop = UU_TRUE;
			while(loop == UU_TRUE)
			{
				i = ud_revt(event, inparm);
				if(i == UU_TRUE)
					loop = UU_FALSE;
				else
				{
					answer = gawaitevent((UU_REAL) 0.);

/*						-- this is here to force a reissue of the read request if
						if the requested event was not satified but there was
						nothing on the Q. -- */

					if(answer != NULL)
						loop = UU_FALSE;
				}
			}
		}
		else
			answer = gawaitevent((UU_REAL) 600.);
	}

/*		-- if events on Q or operator entered unrequested input, get
		event mode input -- */
/*
...... move this value to higher level function ud_gevt1
...... Yurong
*/
/*	UD_pickmode = 0; */
	if(i == UU_FALSE)
		ud_eevt(event, answer);

#if UU_DEBUG==UU_TRUE
	if(UU_debmask & UU_DTRC)
		ud_prevent("ud_read", event);
#endif

	uu_dexit;
}
