/*********************************************************************
**
**    NAME         :  cannoin.c
**
**       CONTAINS:
**				int uc_init_txt_uni()
**				int uc_init_txt_geo()
**				int uc_init_txt_crv()
**				int uc_init_txt2()
**				int uc_init_txt3()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       ctextin.c , 25.1 
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:05:00
**
*********************************************************************/

#include "class.h"
#include "udebug.h"

extern UC_TABLE_DESCRIPTOR UC_txt_descriptor;

/*********************************************************************
**    E_FUNCTION :  int uc_init_txt_uni();
**       init drafting associative class - unibase superclass
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_txt_uni()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_txt_uni()"));

	class_rec = ucu_init_class_rec(UC_UNIBASE_CLASS, &UC_txt_descriptor);
	ucu_init_class(class_rec, &UC_txt_descriptor);

	UC_ADD_METHOD_NAME(&UC_txt_descriptor, uc_init_txt_uni);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_txt_geo()
**       initialize routine
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_txt_geo()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_txt_geo()"));

	class_rec = ucu_init_class_rec(UC_GEOMETRY_CLASS, &UC_txt_descriptor);

	UC_ASSIGN_METHOD(&UC_txt_descriptor, UC_DELETE, ua_asc_txt_delete);
	UC_ASSIGN_METHOD(&UC_txt_descriptor, UC_TRANSLATE, ua_asc_txt_translate);
	UC_ASSIGN_METHOD(&UC_txt_descriptor, UC_ROTATE, ua_asc_txt_rotate);
	UC_ASSIGN_METHOD(&UC_txt_descriptor, UC_TRANSFORM, ua_asc_txt_transform);
	UC_ASSIGN_METHOD(&UC_txt_descriptor, UC_MIRROR, ua_asc_txt_mirror);
	UC_ASSIGN_METHOD(&UC_txt_descriptor, UC_SCALE, ua_asc_txt_scale);

	ucu_init_class(class_rec, &UC_txt_descriptor);

	UC_ADD_METHOD_NAME(&UC_txt_descriptor, uc_init_txt_geo);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_txt_crv()
**       initialize routine
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_txt_crv()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_txt_crv()"));

	class_rec = ucu_init_class_rec(UC_CURVE_CLASS, &UC_txt_descriptor);

	ucu_init_class(class_rec, &UC_txt_descriptor);

	UC_ADD_METHOD_NAME(&UC_txt_descriptor, uc_init_txt_crv);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_txt2()
**       initialize routine
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_txt2()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_txt2()"));

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_LINE_REL),
							&UC_txt_descriptor);

	ucu_init_class(class_rec, &UC_txt_descriptor);

	UC_ADD_METHOD_NAME(&UC_txt_descriptor, uc_init_txt2);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_txt3()
**       initialize routine
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_txt3()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_txt3()"));

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_CIRCLE_REL),
							&UC_txt_descriptor);

	ucu_init_class(class_rec, &UC_txt_descriptor);

	UC_ADD_METHOD_NAME(&UC_txt_descriptor, uc_init_txt3);

	uu_dexit;
	return(status);
}
