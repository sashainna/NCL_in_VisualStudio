/*********************************************************************
**    NAME         :  reasenv.c
**       CONTAINS:
**       ur_autosave_environ()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reasenv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:27
*********************************************************************/

#include "udebug.h"
#include "usysdef.h"
#include "rienvtab.h"
#include "xenv1.h"

/*********************************************************************
**    E_FUNCTION     :  ur_autosave_environ(envfd)
**       save the environment to file envfd
**    PARAMETERS   
**       INPUT  : 
**				envfd	int			file descriptor for the file to save to
**       OUTPUT :  
**          none
**    RETURNS      : 0 if successful else error code
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_autosave_environ(envfd)
int			envfd;				/* file descriptor to save to */
{
	extern struct UR_env_table_rec UR_environ_table[];
	int	status;
	int	i;
	int	lrecl;
	int	nb;

	uu_denter(UU_RTRC,(us,"ur_autosave_environ(%d)", envfd));

	/* write env file from environment */
	for(i = 0; UR_environ_table[i].name[0] != '\0'; i++)
	{
		/* do blocked write directly from environment */
		lrecl = UR_environ_table[i].length;
		status = ux_write_block(envfd,UR_environ_table[i].adrs,lrecl,&nb,UX_PRTERRS);

		/* check the returned status for the write */
		if(status != 0)
		{
			/* return error message - error writing the file */
			uu_dprint(-1,(us,"ERROR:bad write status saving environment"));
			uu_dexit;
			return(status);
		}
	}
	uu_dexit;
	return(0);
}
