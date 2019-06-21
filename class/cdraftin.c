/*********************************************************************
**
**    NAME         :  cannoin.c
**
**       CONTAINS:
**				int uc_init_drf_uni()
**				int uc_init_drf_geo()
**				int uc_init_drf_crv()
**				int uc_init_drf_sur()
**				int uc_init_drf_sol()
**				int uc_init_drf_rel()
**				int uc_init_drf_sym()
**				int uc_init_drf_app()
**				int uc_init_drf1()
**				int uc_init_drf2()
**				int uc_init_drf3()
**				int uc_init_drf4()
**				int uc_init_drf5()
**				int uc_init_drf7()
**				int uc_init_drf8()
**				int uc_init_drf10()
**				int uc_init_drf20()
**				int uc_init_drf31()
**				int uc_init_drf40()
**				int uc_init_drf42()
**				int uc_init_drf44()
**				int uc_init_drf62()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**     MODULE NAME AND RELEASE LEVEL
**       cdraftin.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:04:58
*********************************************************************/

#include "class.h"
#include "udebug.h"

extern UC_TABLE_DESCRIPTOR UC_drf_descriptor;

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf_uni();
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

int uc_init_drf_uni()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf_uni()"));

	class_rec = ucu_init_class_rec(UC_UNIBASE_CLASS, &UC_drf_descriptor);
	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf_uni);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf_geo()
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

int uc_init_drf_geo()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf_geo()"));

	class_rec = ucu_init_class_rec(UC_GEOMETRY_CLASS, &UC_drf_descriptor);

	UC_ASSIGN_METHOD(&UC_drf_descriptor, UC_DELETE, ua_asc_drf_delete);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf_geo);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf_crv()
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

int uc_init_drf_crv()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf_crv()"));

	class_rec = ucu_init_class_rec(UC_CURVE_CLASS, &UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf_crv);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf_sur()
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

int uc_init_drf_sur()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf_sur()"));

	class_rec = ucu_init_class_rec(UC_SURFACE_CLASS, &UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf_sur);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf_sol()
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

int uc_init_drf_sol()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf_sol()"));

	class_rec = ucu_init_class_rec(UC_SOLID_CLASS, &UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf_sol);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf_sym()
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

int uc_init_drf_sym()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf_sym()"));

	class_rec = ucu_init_class_rec(UC_SYMBOL_CLASS, &UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf_sym);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf_rel()
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

int uc_init_drf_rel()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf_rel()"));

	class_rec = ucu_init_class_rec(UC_RELATED_CLASS, &UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf_rel);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf_app()
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

int uc_init_drf_app()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf_app()"));

	class_rec = ucu_init_class_rec(UC_APPLICATION_CLASS, &UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf_app);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf1()
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

int uc_init_drf1()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf1()"));

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_POINT_REL),
							&UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf1);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf2()
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

int uc_init_drf2()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf2()"));

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_LINE_REL),
							&UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf2);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf3()
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

int uc_init_drf3()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf3()"));

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_CIRCLE_REL),
							&UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf3);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf4()
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

int uc_init_drf4()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf4()"));

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_CONIC_REL),
							&UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf4);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf5()
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

int uc_init_drf5()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf5()"));

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_COMPCRV_REL),
							&UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf5);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf7()
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

int uc_init_drf7()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf7()"));

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_RBSPLCRV_REL),
							&UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf7);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf8()
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

int uc_init_drf8()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf8()"));

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_AGCRV_REL),
							&UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf8);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf10()
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

int uc_init_drf10()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf10()"));

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_AGSRF_REL),
							&UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf10);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf20()
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

int uc_init_drf20()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf20()"));

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_AGSHELL_REL),
							&UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf20);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf31()
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

int uc_init_drf31()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf31()"));

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_POLY_REL),
							&UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf31);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf40()
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

int uc_init_drf40()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf40()"));

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_POLY_REL),
							&UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf40);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf42()
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

int uc_init_drf42()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf42()"));

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_POLYLINE_REL),
							&UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf42);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf44()
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

int uc_init_drf44()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf44()"));

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_GROUP_REL),
							&UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf44);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_drf62()
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

int uc_init_drf62()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_drf62)"));

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UB_INSTANCE_REL),
							&UC_drf_descriptor);

	ucu_init_class(class_rec, &UC_drf_descriptor);

	UC_ADD_METHOD_NAME(&UC_drf_descriptor, uc_init_drf62);

	uu_dexit;
	return(status);
}
