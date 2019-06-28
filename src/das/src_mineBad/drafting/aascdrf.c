/*********************************************************************
**
**    NAME         :  aascdrf.c
**
**       CONTAINS:
**				int ua_asc_drf_delete(associate, association)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       aascdrf.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:30
**
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"

/*********************************************************************
**    E_FUNCTION :  int ua_asc_drf_delete(associate, association)
**       drafting associativity back-end delete method
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ua_asc_drf_delete(associate, association)
UU_KEY_ID associate;					/* associate key */
UU_KEY_ID association;				/* association key */
{
	int status;

	uu_denter(UU_MTRC,(us,"enter ua_asc_drf_delete: ate=0x%x, tion=0x%x",
		associate, association));

	status = uc_delete(association);

	uu_dexit;
	return(status);
}
