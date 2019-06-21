/*********************************************************************
**    NAME         :  reprterr.c
**       CONTAINS:
**       ur_report_error()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reprterr.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:33
*********************************************************************/

#include "dasnog.h"
#include "uhep.h"

/*********************************************************************
**    E_FUNCTION :  ur_report_error(status)
**       call the error subsystem to report the error indicated by
**			status to the user.
**    PARAMETERS   
**       INPUT  : 
**          status	int	a status returned by a Unibase routine
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : error reported to user
**    WARNINGS     : none
*********************************************************************/

ur_report_error(status)
int	status;
{
	if(status < 0)
	{
		status = -status;					/* make positive */
	}
	uu_uerror0(UU_UBASE,status);		/* simple, huh? */
												/* ...could be expanded to be more */
												/* sophisticated if needed */
}
