/*********************************************************************
**
**    NAME         :  cannoin.c
**
**       CONTAINS:
**    		int uc_init_anno_class()
**    		int uc_init_text()
**    		int uc_init_lindim()
**    		int uc_init_xhatch()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**     MODULE NAME AND RELEASE LEVEL
**       cannoin.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:04:58
*********************************************************************/

#include "class.h"
#include "udebug.h"

extern UC_TABLE_DESCRIPTOR UC_cot_descriptor;
extern UC_TABLE_DESCRIPTOR UC_drf_descriptor;

/*********************************************************************
**    E_FUNCTION :  int uc_init_anno_class()
**       init annotation class
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_anno_class()
{
	int status = UU_SUCCESS;
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();

	uu_denter(UU_MTRC,(us,"uc_init_annotation()"));

	class_rec = ucu_init_class_rec(UC_ANNOTATION_CLASS, &UC_cot_descriptor);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_anno_class);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_text()
**       init text class
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_text()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_text()"));

	uc_uni_init_txt();

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UA_TEXT_REL), &UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DISPLAY, uv_disp_entity);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, ua_draw_text);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, ua_p33_text);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, ua_delete_text);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, ua_class_copy_ctext);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, ua_tr_ctext);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, ua_scal_ctext);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, ua_mirror_ctext);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, ua_tf_ctext);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_DATA, ua_get_text1);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_TRANSF, uc_retrieve_mtuple_transf);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_ATTR, uc_retrieve_mtuple_attr);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CREATE_DATA, ua_create_text);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING, ua_txt_to_drawing);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ASSOC_UPDATE, ua_txt_autoupdate);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_group);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, ua_text_query);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_text);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_lindim()
**       init dimension class
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_lindim()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_lindim()"));

	uc_uni_init_lindim();

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UA_LINEAR_DIMS_REL),
							&UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DISPLAY, ua_display_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, ua_draw_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, ua_print_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, ua_delete_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, ua_copy_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, ua_translate_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, ua_rotate_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, ua_scale_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, ua_transform_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_DATA, ua_retrieve_data);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_TRANSF, ua_retrieve_transf);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_ATTR, ua_retrieve_attr);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CREATE_DATA, ua_create_entity);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING, ua_proj_to_drawing);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, ua_query);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, ua_mirror_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_group);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_lindim);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_xhatch()
**       init cross hatch class
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_xhatch()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_xhatch()"));

	uc_uni_init_xhatch();

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UA_HATCHING_REL),
							&UC_cot_descriptor);
	
/*-----------------------------------

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DISPLAY, ua_display_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, ua_draw_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, ua_print_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, ua_delete_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, ua_copy_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, ua_translate_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, ua_rotate_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, ua_scale_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, ua_transform_drafting);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_DATA, ua_retrieve_data);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_TRANSF, ua_retrieve_transf);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_ATTR, ua_retrieve_attr);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CREATE_DATA, ua_create_entity);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING, ua_proj_to_drawing);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, ua_query);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_group);

----------------------*/

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_xhatch);

	uu_dexit;
	return(status);
}
