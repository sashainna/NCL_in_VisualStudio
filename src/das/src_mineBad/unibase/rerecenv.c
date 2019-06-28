/*********************************************************************
**    NAME         :  rerecenv.c
**       CONTAINS:
**       ur_recover_environ()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**       rerecenv.c , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:34
*********************************************************************/

#include "udebug.h"
#include "usysdef.h"
#include "rienvtab.h"
#include "rerrdef.h"
#include "xenv1.h"

/*********************************************************************
**    E_FUNCTION     :  ur_recover_environ(envfd)
**       restore the environment from file envfd
**    PARAMETERS   
**       INPUT  : 
**				envfd	int			file descriptor for the file to load from
**       OUTPUT :  
**          none
**    RETURNS      : 0 if successful else error code
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_recover_environ(envfd)
int			envfd;				/* file descriptor to load from */
{
	extern struct UR_env_table_rec UR_environ_table[];
	int	status;
	int	i;
	int	lrecl;
	int	nb;

/*--------------------------------------------------------------------
**    Start of Executable Code
**--------------------------------------------------------------------
*/
	uu_denter(UU_RTRC,(us,"ur_recover_environ(%d)", envfd));
	status = ur_chk_data_dict() ;
	if (status != 0)
	{
		uu_dexit;
		return(-1);				/* return error code */
	}

	/* read env file into actual environment */
	for(i = 0; UR_environ_table[i].name[0] != '\0'; i++)
	{
		/* do blocked read directly into environ area */
/*
.....we need add initial for lrecl and nb, we will use it in ux_read_block
.....function: this function will check if the block len read from 'as
.....start of block' less then the lrecl*nb, otherwise, if we tried to read
.....a big array with temp_rcb don't have enough space to hold it, it will
.....have a memory problem and cause a fatal error leter on
.....so we chnaged function ux_read_block, also required all call changed
.....Yurong 11/1/02
*/
		lrecl =  UR_environ_table[i].length;
		nb = 1;
		ux_read_block(envfd, UR_environ_table[i].adrs, &lrecl, &nb, UX_PRTERRS);

		/* check that the read length matches the modal length */
		if(lrecl != UR_environ_table[i].length)
		{
			/* return error message - bad environment file */
			uu_dprint(-1,(us,"ERROR:bad record length recovering environment"));
			uu_dexit;
			return(UR_BAD_ENV_FILE);
		}
	}
	uu_dexit;
	return(0);
}
