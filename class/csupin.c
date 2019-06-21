/*********************************************************************
**
**    NAME         :  clinit.c
**
**       CONTAINS:
**    		int uc_init_support_class()
**    		int uc_init_viewing_class()
**    		int uc_init_corsys()
**    		int uc_init_draw()
**    		int uc_init_layer()
**    		int uc_init_view73()
**    		int uc_init_view74()
**    		int uc_init_view75()
**    		int uc_init_calc()
**				int uc_init_color()
**				int uc_init_unistat()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       csupin.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION 
**       04/29/15 , 15:04:59
**
*********************************************************************/

#include "class.h"
#include "udebug.h"

extern UC_TABLE_DESCRIPTOR UC_cot_descriptor;

/*********************************************************************
**    E_FUNCTION :  int uc_init_support_class()
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

int uc_init_support_class()
{
	int status = UU_SUCCESS;
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();

	uu_denter(UU_MTRC,(us,"uc_init_support_class()"));

/*	-- Viewing Class has no default methods -- */

	class_rec = ucu_init_class_rec(UC_SUPPORT_CLASS, &UC_cot_descriptor);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_support_class);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_viewing_class()
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

int uc_init_viewing_class()
{
	int status = UU_SUCCESS;
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();

	uu_denter(UU_MTRC,(us,"uc_init_viewing()"));

/*	-- Viewing Class has no default methods -- */

	class_rec = ucu_init_class_rec(UC_VIEWING_CLASS, &UC_cot_descriptor);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_viewing_class);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_corsys()
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

int uc_init_corsys()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_corsys()"));

	uc_uni_init_corsys();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_COORDSYS_REL),
							&UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, um_drw43_coordsys);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DISPLAY, uv_disp_entity);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, um_p43_coordsys);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_DATA, um_get_all_geom);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_TRANSF, uc_retrieve_mtuple_transf);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_ATTR, uc_retrieve_mtuple_attr);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_corsys);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_light()
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

int uc_init_light()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_light()"));

	uc_uni_init_light();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_LIGHT_REL),
							&UC_cot_descriptor);

	UC_ASSIGN_METHOD(
		&UC_cot_descriptor, UC_PLOC_TO_COORD, um_light_ploc_to_coord);
	UC_ASSIGN_METHOD(
		&UC_cot_descriptor, UC_PLOC_TO_VECTOR, um_light_ploc_to_vector);
	UC_ASSIGN_METHOD(
		&UC_cot_descriptor, UC_DRAW, um_drw38_light);
	UC_ASSIGN_METHOD(
		&UC_cot_descriptor, UC_DISPLAY, uv_disp_entity);
	UC_ASSIGN_METHOD(
		&UC_cot_descriptor, UC_PRINT, um_p38_light);
	UC_ASSIGN_METHOD(
		&UC_cot_descriptor, UC_TRANSLATE, um_translate_using_matrix);
	UC_ASSIGN_METHOD(
		&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
	UC_ASSIGN_METHOD(
		&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
	UC_ASSIGN_METHOD(
		&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
	UC_ASSIGN_METHOD(
		&UC_cot_descriptor, UC_TRANSFORM, um_tf38_tranflight);
	UC_ASSIGN_METHOD(
		&UC_cot_descriptor, UC_CREATE_DATA, um_create_geom);
	UC_ASSIGN_METHOD(
		&UC_cot_descriptor, UC_RETRIEVE_DATA, uc_retrieve_mtuple_data);
	UC_ASSIGN_METHOD(
		&UC_cot_descriptor, UC_RETRIEVE_TRANSF, uc_retrieve_mtuple_transf);
	UC_ASSIGN_METHOD(
		&UC_cot_descriptor, UC_RETRIEVE_ATTR, uc_retrieve_mtuple_attr);
	UC_ASSIGN_METHOD(
		&UC_cot_descriptor, UC_DELETE, um_dl38_light);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_corsys);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_draw()
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

int uc_init_draw()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_draw()"));

	uc_uni_init_draw();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_DRAWING_REL),
							&UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_DATA, um_get_all_geom);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_TRANSF, uc_retrieve_mtuple_transf);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_ATTR, uc_retrieve_mtuple_attr);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CREATE_DATA, um_create_geom);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DISPLAY, uv_disp_entity);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, um_drw46_drawing);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, um_p46_drawing);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, um_dl46_deldrawing);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_draw);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_layer()
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

int uc_init_layer()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_layer()"));

	uc_uni_init_layer();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_LAYER_REL),
							&UC_cot_descriptor);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_layer);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_view73()
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

int uc_init_view73()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();

	uu_denter(UU_MTRC,(us,"uv_init_viewing_rel73()"));

/* -- init view relation -- */

	uc_uni_init_view73();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UV_VIEW_REL),
							&UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, uv_print_view);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_view73);

	uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_view74()
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

int uc_init_view74()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();

	uu_denter(UU_MTRC,(us,"uv_init_viewing_rel74()"));

/* init viewport relation */

	uc_uni_init_view74();

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UV_VPORT_REL),
							&UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, uv_print_viewport);
	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_view74);

	uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_view75()
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

int uc_init_view75()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();

	uu_denter(UU_MTRC,(us,"uv_init_viewing_rel75()"));

/* init screen relation */
	uc_uni_init_view75();

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UV_SCREEN_REL),
							&UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, uv_print_screen);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_view75);

	uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_calc()
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

int uc_init_calc()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_calc()"));

	uc_uni_init_calc();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UQ_CALC_REL),
							&UC_cot_descriptor);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_calc);

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_color()
**       initialize color class
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_init_color()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_color()"));

	uc_uni_init_color();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_COLOR_REL),
							&UC_cot_descriptor);
	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_color);
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_unistat()
**       initialize Unibase statistics class
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_init_unistat()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;


	uc_uni_init_unistat();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UR_UNISTAT_REL),
							&UC_cot_descriptor);
	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_unistat);
	return(status);
}
