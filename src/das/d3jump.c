/*********************************************************************
**
**    NAME         :  jump.c
**       CONTAINS:
**       	ud_jump
**				ud_jmpmark
**				ud_markover
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
** 
**    MODULE NAME AND RELEASE LEVEL
**       d3jump.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:06
**
*********************************************************************/

#include "usysdef.h"
#include "gtbl.h"
#include "g.h"
#include "dmark.h"
#include "dinput.h"
#include "dasg.h"
#include "zsysdep.h"
#include "udebug.h"
#include "uhep.h"
#include "xenv1.h"

/**************************************************************************
**
**  I_FUNCTION         :  ud_jump(val, rpf)
**      cleanup the environment and blast off to the parser
**
**  PARAMETERS   
**
**      INPUT  : 	val = value to use in longjump call
**						rpf = if UU_TRUE, then shut down playback
**      OUTPUT :  none
**
**  RETURNS      :   returns if enable jump flag is UU_TRUE
**  SIDE EFFECTS :  never returns if jump flag is UU_FALSE
**  WARNINGS     :  none
**
**************************************************************************/

ud_jump(val, rpf)
int val;						/* value to use in longjmp call */
UU_LOGICAL rpf;			/* shut R/P flag */
{
	int status;				/* status return cell */
	int i;

/* --- Start of Executable Code --- */

	if(UD_enablejmp == UU_TRUE)
	{
		if(UD_Rpstate[UD_Rpstate_ptr].flag != RPOFF && rpf == UU_TRUE)
		{

/*			-- turn off playback and close all open files -- */

			while(UD_Rpstate_ptr >= 0)
			{
				status = ux_close(UD_Rpstate[UD_Rpstate_ptr].rplun, UX_PRTERRS);
				UD_Rpstate[UD_Rpstate_ptr].flag = RPOFF;
				UD_Rpstate_ptr--;
			}
			UD_Rpstate_ptr = 0;
		}

/*		-- go off to top entry in mark stack -- */

		longjmp(UD_markstk[UD_markptr-1].markenv, val);
	}
	else
		return;
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_jmpmark(markparm)
**      jump to the current location on the mark stack
**
**  PARAMETERS   
**      INPUT  : 	val = mark parameter
**						The mark parameter may take on the following values
**
**							-1 - pop stack. If mark entry  flag is UU_TRUE then return,
**												 else jump to next environment.
**							 0 - pop stack and return
**							>0 - pop stack, decrement counter, and continue jumping
**      OUTPUT :  none
**
**  RETURNS      :   never returns if unwinding mark stack
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

ud_jmpmark(markparm)
int markparm;						/* mark parameter */
{
	UD_markstkrec popmark;		/* record to hold popped mark entry */
	int locparm;					/* local mark paramter cell */

/* --- Start of Executable Code --- */

	UD_markptr--;

/*		-- do not pop the first entry. This stuff with locparm and
			markparm is to stop the jump process when we have reached the
			bottom of the stack -- */

	if(UD_markptr == 0)
	{
		locparm = -1;
		UD_markptr = 1;
	}
	else
		locparm = markparm - 1;

/*	-- start unwinding the mark stack -- */

	if(markparm < 0)
	{

/*		-- if not yet at a meaningful stopping place, jump again -- */

		UU_stklen = UD_markstk[UD_markptr].debugsave;
		if(UD_markstk[UD_markptr].stopflag == UU_FALSE)
			longjmp(UD_markstk[UD_markptr-1].markenv, -1);
	}
	else if(markparm > 0)
	{
		UU_stklen = UD_markstk[UD_markptr].debugsave;
		longjmp(UD_markstk[UD_markptr-1].markenv, locparm);
	}

	return;
}

/**************************************************************************
**
**  I_FUNCTION         :  ud_markover()
**      mark stack overflow handler
**
**  PARAMETERS   
**      INPUT  : 	none
**      OUTPUT :  none
**
**  RETURNS      :  never returns if unwinding mark stack
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**
**************************************************************************/

ud_markover()
{
	uu_uerror0(UD_DASHEP, 86);
	ud_jump(UD_MARKLEVEL+1, UU_TRUE);
}
