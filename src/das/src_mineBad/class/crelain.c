/*********************************************************************
**
**    NAME         :  clinit.c
**
**       CONTAINS:
**    		int uc_init_related_class()
**    		int uc_init_symbol_class()
**    		int uc_init_group()
**    		int uc_init_massym()
**    		int uc_init_inssym()
**    		int uc_init_connector()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**     MODULE NAME AND RELEASE LEVEL
**       crelain.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:04:59
**
*********************************************************************/

#include "class.h"
#include "udebug.h"

extern UC_TABLE_DESCRIPTOR UC_cot_descriptor;

/*********************************************************************
**    E_FUNCTION :  int uc_init_related_class()
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

int uc_init_related_class()
{
	int status = UU_SUCCESS;
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();

	uu_denter(UU_MTRC,(us,"uc_init_related()"));

	class_rec = ucu_init_class_rec(UC_RELATED_CLASS, &UC_cot_descriptor);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_related_class);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_symbol_class()
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

int uc_init_symbol_class()
{
	int status = UU_SUCCESS;
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();

	uu_denter(UU_MTRC,(us,"uc_init_symbols()"));

	class_rec = ucu_init_class_rec(UC_SYMBOL_CLASS, &UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE_UNDELETED, um_cannot_undelete);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_symbol_class);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_group()
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

int uc_init_group()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_group()"));

	uc_uni_init_group();

/*	-- turn off associativity -- */

	ur_ignore_associative(UM_GROUP_REL);

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_GROUP_REL),
							&UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DISPLAY, uv_disp_entity);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, um_drw44_group);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, um_p44_group);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, um_dl44_group);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, um_cp44_group);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, um_tr44_group);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rot44_group);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, um_sc44_group);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, um_mir44_group);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, um_tf44_group);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, um_group_query);
 	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING, um_proj_to_drawing);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE_UNDELETED, um_undl5_group);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_group);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_massym()
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

int uc_init_massym()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"ub_init_massym()"));

	uc_uni_init_msym();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UB_SYMBOL_REL),
							&UC_cot_descriptor);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, ub_print_sym);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ASSOC_UPDATE, ub_msymAutoUpdate);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_DATA, ub_retrieve_sym);
	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_massym);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_inssym()
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

int uc_init_inssym()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"ub_init_inssym()"));

	uc_uni_init_instance();

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UB_INSTANCE_REL),
							&UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DISPLAY, ub_display_sym);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, ub_draw_sym);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, ub_print_sym);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, ub_delete_symbol);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, ub_copy_syminstance);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, ub_transl_syminstance);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, ub_rot_syminstance);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, ub_scale_syminstance);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, ub_transform_sym);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_DATA, ub_retrieve_sym);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_TRANSF, ub_get_sym_transf);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_ATTR, ub_get_sym_attr);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PLOC_TO_COORD, um_d_nrendpt);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING, ub_proj_to_drawing);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, ub_query);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ASSOC_UPDATE, ub_instAutoUpdate);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_inssym);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  int uc_init_connector()
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

int uc_init_connector()
{
	int status = UU_SUCCESS;
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	uu_denter(UU_MTRC,(us,"uc_init_connector()"));

	if (ub_con_init_relations() != UU_SUCCESS) goto failed;
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UB_CONECTOR_REL),
							&UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CREATE_DATA, ub_cr64_connect);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DISPLAY, ub_disp64_connect);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, ub_drw64_connect);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, ub_p64_connect);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, ub_dl64_connect);
/* UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, ub_tran64_connect);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, ub_rot64_connect);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, ub_scale64_connect);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, ub_mir64_connect);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, ub_transf64_connect);
*/
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ASSOC_UPDATE, ub_conAutoUpdate);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_DATA, ub_retr64_connect);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_TRANSF, uc_retrieve_mtuple_transf);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_ATTR, uc_retrieve_mtuple_attr);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_connector);
	goto done;
failed: status = UU_FAILURE;
done:
	uu_dexitstatus("uc_init_connector", status);
	return(status);
}
