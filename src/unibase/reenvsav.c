/*********************************************************************
**    NAME         :  reenvsav.c
**       CONTAINS:
**       ur_save_environ()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reenvsav.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:29
*********************************************************************/

#define	UR_BLKRCL	1024	/* block record length					*/
#include "udebug.h"
#include "dasnog.h"
#include "xenv1.h"
#include "uhep.h"

/*********************************************************************
**    E_FUNCTION     :  ur_save_environ()
**       save the environment in a stand alone environment file
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_save_environ()
{
	char			envnm[16];			/* environment file name */
	int			envfd;				/* env file descriptor */
	int			status;				/* status of env loading */
	int			length;				/* DAS communication */
/*--------------------------------------------------------------------
**    Start of Executable Code
**--------------------------------------------------------------------
*/
	uu_denter(UU_RTRC,(us,"ur_save_environ"));

	/* prompt for environment file name */
	length = sizeof(envnm);
	ud_ldas(UD_DASSTRING, UU_UBASE, 1, envnm, length, &length, UD_NODEFAULT);
	uu_dprint(UU_RITRC,(us,"ur_save_environ file name=%s", envnm));

	/* deliver warning shots across the bow here if needed */

	/* save the environment */
	status = ux_create_file(envnm,0600,UU_NULL,"BLOCKED","BINARY","UX_NOEXTRA",
		&envfd, UX_PRTERRS);
	if (status != 0)
	{
		uu_uerror0(UU_UBASE,20);		/* unable to open the file */
		uu_dexit;
		return(status);
	}
	status = ur_environ_out(envfd);
	if (status != 0)
	{
		uu_uerror0(UU_UBASE,20);					/* error writing the file */
		uu_dexit;
		return(status);
	}
	status = ux_close(envfd, UX_PRTERRS);

	/* env. vars saved - announce to whoever */

	uu_dexit;
	return(0);
}
