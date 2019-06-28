/********************************************************************* 
**  NAME:  rioraerr.c
**			ur_o_erpt(cursor,max_cursor) 
**      prints oracle error code from cursor array 
**  COPYRIGHT  1984  UNICAD, Inc.
**  MODULE NAME AND RELEASE LEVEL 
**       rioraerr.c , 25.1
**  DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:45
**  PARAMETERS   
**      input:   
**			cursor,		address of first entry in the cursor array
**			max_cursor,	the number of cirsors in the array
**      output: none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/

#include "udebug.h"
#include "uhep.h"
#include "rerrdef.h"

#ifdef UR_ORACLE

ur_o_erpt(cursor,max_cursor)
short	cursor[][32]	;	/* address of the array of 64 byte cursors	*/
int	max_cursor		;	/* the last entry of the cursor array			*/
{
	long	register	i	;	/* an index			*/
	char	msg[80]		;	/* message buffer	*/

	uu_denter(UU_RTRC,(us,"o_erpt"));
	if(cursor[0][0])		/* check for logon error */
	{
		uu_dprint(-1,(us,"Logon error %d.", cursor[0][0]));
	}
	else						/* not logon error */
	{
		/*	find first non-zero error code		*/
		for (i = 1 ; i < max_cursor+1 && cursor[i][0] == 0; i++)
		{
		}
		/* print error message if legal cursor	*/	
		if(i > max_cursor)
		{
			uu_dprint(-1,(us,"Unknown ORACLE error."));
		}
		else
		{
			uu_dprint(-1,(us,"ORACLE error %d for op %d using cursor %d", 
				cursor[i][0],cursor[i][5],i));
			oermsg(cursor[i][0],msg);
			uu_dprint(-1,(us,"  %s", msg));
		}
	}
	uu_dexit;
	return(0)	;	
}

#else
/* stub no ORACLE */
ur_o_erpt(cursor,max_cursor)
short	cursor[][32]	;	/* address of the array of 64 byte cursors	*/
int	max_cursor		;	/* the last entry of the cursor array			*/
{
	uu_denter(UU_RTRC,(us,"o_erpt-----(not available)"));
	uu_dexit;
	return(URM_NO_RDBMS)	;	
}
#endif
