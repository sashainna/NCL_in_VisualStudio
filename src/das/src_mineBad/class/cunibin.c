/*********************************************************************
**
**    NAME         :  cunibin.c
**
**       CONTAINS:
**    		int uc_init_unibase_class()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       cunibin.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:00
**
*********************************************************************/

#include "class.h"
#include "udebug.h"

extern UC_TABLE_DESCRIPTOR UC_cot_descriptor;

/*********************************************************************
**    E_FUNCTION :  int uc_init_unibase_class()
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

int uc_init_unibase_class()
{
	int status = UU_SUCCESS;
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();

	uu_denter(UU_MTRC,(us,"uc_init_unibase()"));

	class_rec = ucu_init_class_rec(UC_UNIBASE_CLASS, &UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, um_delete_all);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE_UNDELETED, um_cannot_undelete);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_DATA, ur_retrieve_data);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_TRANSF, uc_retrieve_mtuple_transf);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_ATTR, uc_retrieve_mtuple_attr);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SETUP_DATA, ur_setup_data);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, um_query);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_unibase_class);

	uu_dexit;
	return(status);
}
