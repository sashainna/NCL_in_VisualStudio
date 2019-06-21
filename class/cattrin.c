/*********************************************************************
**
**    NAME         :  cattrin.c
**
**       CONTAINS:
**    		int uc_init_attribute_class()
**    		int uc_init_txtatt()
**    		int uc_init_tran()
**    		int uc_init_modatt()
**    		int uc_init_attsym()
**    		int uc_init_attbun()
**    		int uc_init_splatt()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**     MODULE NAME AND RELEASE LEVEL
**       cattrin.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:04:58
**
*********************************************************************/

#include "class.h"
#include "udebug.h"

extern UC_TABLE_DESCRIPTOR UC_cot_descriptor;

/*********************************************************************
**    E_FUNCTION :  int uc_init_attribute_class()
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

int uc_init_attribute_class()
{
	int status = UU_SUCCESS;
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();

	uu_denter(UU_MTRC,(us,"uc_init_attribute()"));

	class_rec = ucu_init_class_rec(UC_ATTRIBUTE_CLASS, &UC_cot_descriptor);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_attribute_class);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_txtatt()
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

int uc_init_txtatt()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_txtatt()"));

	uc_uni_init_txtatt();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UA_TEXTATTR_REL), 
							&UC_cot_descriptor);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_txtatt);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_tran()
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

int uc_init_tran()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_tran()"));

	uc_uni_init_tran();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_TRANSFORM_REL), 
							&UC_cot_descriptor);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_tran);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_modatt()
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

int uc_init_modatt()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_modatt()"));

	uc_uni_init_modatt();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_ATTR_REL), 
							&UC_cot_descriptor);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_modatt);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_attsym()
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

int uc_init_attsym()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;
	int i;

	uu_denter(UU_MTRC,(us,"ub_init_attsym()"));

	uc_uni_init_attsym();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UB_SYMATTR_REL), 
							&UC_cot_descriptor);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_attsym);

	uu_dexit;
	return(status);
}

