/*********************************************************************
**
**    NAME         :  d2hldst.c
**
**       CONTAINS:
**  			ud_hldstk
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       d2hldst.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:05
**
*********************************************************************/

#include "usysdef.h"
#include "dinput.h"
#include "dasnog.h"
#include "dasg.h"
#include "uhep.h"
#include "udebug.h"

/********************************************************************* 
**
**  E_FUNCTION	:  ud_hldstk(prompt, ret_buf, bufsize, actsize, funct, interval)
**      stroke high level DAS routine
**
**  PARAMETERS   
**      INPUT:  prompt = operator prompt string
**					 bufsize = buffer size in points
**					 func = function passed to GKS
**					 interval = x, y interval
**      OUTPUT: ret_buf =  array of coordinates
**					 actsize = number of coordinates actually input
**
**  RETURNS      : status of operation
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
*********************************************************************/

UD_DASTAT ud_hldstk(prompt, ret_buf, bufsize, actsize, funct, interval)
char *prompt;	 					/* operator prompt string  */
UU_REAL ret_buf[][3];			/* coordinate to return  */
int bufsize;						/* max buffer size */
int *actsize;						/* actual number of points */
int (*funct)();					/* function name */
Gwpoint *interval;				/* u, v interval */
{

	UD_DASTAT status;						/* status return cell */
	UD_DASTAT ud_auxm();
	UD_DEVENT event;						/* event buffer */
	int i;


	uu_denter(UU_DTRC,(us,"entering ud_stk"));

	UD_stkinit.interval.x = (*interval).x;
	UD_stkinit.interval.y = (*interval).y;
	UD_stkinit.bufsiz = bufsize;
	UD_stkinit.funct = funct;

restart:

	status = DE_TRUE ;
	*actsize = 0;

/*		-- get the next interaction -- */

	ud_gevt(&event, UD_STROKE, prompt, 1, UD_stkdev, UD_stkech, NULL);
	switch( event.evclass )
	{

		case UD_STROKE :

			*actsize = event.indata.strokedata.n_points;
			for(i=0; (i<(*actsize)) && (i<bufsize); i++)
			{
				ret_buf[i][0] = event.indata.strokedata.points[i].x;
				ret_buf[i][1] = event.indata.strokedata.points[i].y;
				ret_buf[i][2] = 0;
			}
			break;

		case UD_CHOICE :

			if(event.evdev == UD_AUXMENU)
			{
				status = ud_auxm(&event);
				if(status == DE_DEFAULT)
				{
	
/*---				"no default in effect"		---*/
	
					uu_uerror0(UD_DASHEP, 3);
					goto restart;
				}
			}
			else
			{
	
	/* --		invalid choice event --*/
	
				uu_uerror0(UD_DASHEP, 73);
				goto restart;
			}
			break;

		case UD_LOCATOR :
		case UD_VALUATOR :
		case UD_PICK :
		case UD_STRING :
		case UD_VECTOR :

/* --		invalid event --*/

			uu_uerror0(UD_DASHEP, 74);
			goto restart;
			break;

		default :

			uu_uerror0(UD_DASHEP, 75);
			goto restart;
			break;
	}

	uu_dexit;
	return(status);
}
