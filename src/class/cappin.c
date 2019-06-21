/*********************************************************************
**
**    NAME         :  cappin.c
**
**       CONTAINS:
**    		int uc_init_application_class()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**
**     MODULE NAME AND RELEASE LEVEL
**       cappin.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:04:58
*********************************************************************/

#include "class.h"
#include "udebug.h"

extern UC_TABLE_DESCRIPTOR UC_cot_descriptor;

/*********************************************************************
**    E_FUNCTION :  int uc_init_application_class()
**       initialize class procedure
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_application_class()
{
	int status = UU_SUCCESS;
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();

	uu_denter(UU_MTRC,(us,"uc_init_application()"));

/*	-- Application Class has no default methods -- */

	class_rec = ucu_init_class_rec(UC_APPLICATION_CLASS, &UC_cot_descriptor);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_application_class);

	uu_dexit;
	return(status);
}

