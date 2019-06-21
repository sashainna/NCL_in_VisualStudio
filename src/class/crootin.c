/*********************************************************************
**
**    NAME         :  crootin.c
**
**       CONTAINS:
**    		int uc_init_root()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       crootin.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:04:59
**
*********************************************************************/

#include "class.h"
#include "udebug.h"

extern UC_TABLE_DESCRIPTOR UC_cot_descriptor, UC_root_descriptor;

/*********************************************************************
**    E_FUNCTION :  int uc_init_root()
**       initialize this class
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_root()
{
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_root()"));

	status = ucu_init_root(&UC_root_descriptor);
	UC_ADD_METHOD_NAME(&UC_root_descriptor, uc_init_root);
	uu_dprint(UU_MTRC,(us,"leave uc_init_root(), status = %d", status));

	uu_dexit;
	return(status);
}
