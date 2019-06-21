/*********************************************************************
**    NAME         :  csolin.c
**       CONTAINS: routine to initialize standard MPE solids
**			int uc_init_solid_class()
**			int uc_init_agshell()
**			int uc_init_body()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       csolin.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:04:59
*********************************************************************/

#include "class.h"
#include "udebug.h"

extern UC_TABLE_DESCRIPTOR UC_cot_descriptor;

/*********************************************************************
**    E_FUNCTION :  int uc_init_solid_class()
**       Initialize UNIBASE relation and methods for solids super class.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_solid_class()
	
	{
	int status = UU_SUCCESS;
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();

	uu_denter(UU_MTRC,(us,"uc_init_solid_class()"));

	class_rec = ucu_init_class_rec(UC_SOLID_CLASS, &UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_group);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE_UNDELETED, um_cannot_undelete);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_solid_class);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_init_agshell()
**	     Initialize AG shell relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_init_agshell()

	{
	UC_CLASS_REC *class_rec;
	UC_CLASS_REC *ucu_init_class_rec();
	int class;
	int j;
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_agshell()"));

	uc_uni_init_agshell();

	class = CONVERT_REL_TO_CLASS(UM_AGSHELL_REL);
	class_rec = ucu_init_class_rec(class, &UC_cot_descriptor);

	/* DO NOT INHERIT ANY METHODS!!!! */
	for (j=0; j<UC_NUM_METHODS; j++) class_rec->methods[j] = UC_UNDEFINED_METHOD;

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_DATA, uc_retrieve_mtuple_data);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_TRANSF, uc_retrieve_mtuple_transf);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_ATTR, uc_retrieve_mtuple_attr);
/*
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CREATE_DATA, ncl_create_aggeo);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SETUP_DATA, um_agshell_setup_data);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, um_agshell_delete);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE_UNDELETED, um_cannot_undelete);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DISPLAY, uv_disp_entity);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, um_agshell_draw);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, um_agshell_print);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, um_agshell_copy);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, um_translate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, um_agshell_transform);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PLOC_TO_COORD, um_agshell_ploc_to_coord);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PLOC_TO_VECTOR, um_agshell_ploc_to_vector);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING, um_agshell_proj_to_drawing);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SRF_TESSELLATE, um_agshell_tessellate);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_TYPE, um_agshell_draft_type);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_ENDPTS, um_agshell_draft);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, um_agshell_query);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_FEATURE, um_agshell_feature);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_compsymgroup);
*/

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_agshell);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_init_body()
**       Initialize UNIBASE relation and methods for ROMULUS solid
**			bodies.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_body()
	
	{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_body()"));

	uc_uni_init_body();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_BODY_REL), 
							&UC_cot_descriptor);

/*
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, um_dl31_delbody);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, um_drw31_body);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, um_p31_body);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, um_tr31_tranbody);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, um_cp31_copybody);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rt31_rotbody);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, um_sc31_scalbody);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, um_mr31_mirrbody);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING, um_proj_to_drawing);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PLOC_TO_COORD, um_curve_ploc_to_coord);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PLOC_TO_VECTOR, um_curve_ploc_to_vector);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_FEATURE, um_f31_body);
*/
	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_body);

	uu_dexit;
	return(status);
	}
