/*********************************************************************
**    NAME         :  recdd.c
**       CONTAINS:
**       ur_chk_data_dict()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       recdd.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:27
*********************************************************************/

#include "usysdef.h"
#include "ustdio.h"
#include "udebug.h"
#include "xenv1.h"

/*********************************************************************
**    E_FUNCTION :  ur_chk_data_dict()
**			check if data dictionary is initialized, and if not 
**			initialize it
**    PARAMETERS   
**    INPUT  : 
**				none
**    OUTPUT :  
**          	none
**    RETURNS    :	0 if dictionary initialized
**
**    SIDE EFFECTS : none
**    WARNINGS     :	none
**
*********************************************************************/

int	ur_chk_data_dict()
{
	extern UU_LOGICAL	UR_dd_inited;
	extern char		*ux_getenv();
	int	status;	/* return status */
	long	rel;		/* the relation */
	char	*dd_name;
	int	ufd;		/* UNICAD file desc */
	FILE	*dd_fd = NULL;

	uu_denter(UU_RTRC,(us,"ur_chk_data_dict"));
	status = 0;

	/* make sure data dictionary initialized */
	if(!UR_dd_inited)
	{
		status = ur_init_data_dict(dd_fd);	/* initialize data dictionary */
		if(status != 0)
		{
			uu_dexit;
			return(-1);
		}
		UR_dd_inited = UU_TRUE;
	} /* if !UR_dd_inited	*/
	uu_dexit;
	return(status);
}
