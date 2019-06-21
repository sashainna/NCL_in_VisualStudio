/*********************************************************************
**    NAME         :  reenvlod.c
**       CONTAINS:
**       ur_load_environ()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reenvlod.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:29
*********************************************************************/

#define	UR_BLKRCL	1024	/* block record length					*/
#include "udebug.h"
#include "dasnog.h"
#include "xenv1.h"
#include "uhep.h"
#include "nclver.h"
#include "riallmod.h"
#include "rienvtab.h"
#include "rver9400.h"
#include "rver9700.h"
	
extern struct UR_env_table_rec UR_environ_table[];

/*********************************************************************
**    E_FUNCTION     :  ur_load_environ()
**       restore the working environment from one previously saved
**			in a stand alone environment file
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_load_environ()
{
	char			envnm[16];			/* environment file name */
	int			envfd;				/* env file descriptor */
	int			status;				/* status of env loading */
	int			length;				/* DAS communication */
/*--------------------------------------------------------------------
**    Start of Executable Code
**--------------------------------------------------------------------
*/
	uu_denter(UU_RTRC,(us,"ur_load_environ"));

	/* prompt for environment file name */
	length = sizeof(envnm);
	ud_ldas(UD_DASSTRING, UU_UBASE, 5, envnm, length, &length, UD_NODEFAULT);
	uu_dprint(UU_RITRC,(us,"ur_load_environ file name=%s", envnm));

	/* deliver warning shots across the bow here if needed */

	/* load the new environment */
#if UU_COMP == UU_WIN2K
	status = ux_open(envnm,"rb+","BLOCKED","BINARY",&envfd,UX_PRTERRS);
#else
	status = ux_open(envnm,"r+","BLOCKED","BINARY",&envfd,UX_PRTERRS);
#endif
	if (status != 0)
	{
		uu_uerror0(UU_UBASE,20);		/* unable to open the file */
		uu_dexit;
		return(status);
	}
	status = ur_environ_in(envfd);
	if (status != 0)
	{
		uu_uerror0(UU_UBASE,20);					/* error reading the file */
		goto  done;
	}
	status = ux_close(envfd, UX_PRTERRS);

	/* env. vars loaded into new buffer - announce to whoever */
	ur_install_new_modals();
done:;
	uu_dexit;
	return(0);
}
