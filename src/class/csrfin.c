
/*********************************************************************
**    NAME         :  csrfin.c
**       CONTAINS: routines to initialize standard MPE surfaces
**			int uc_init_surface_class()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       csrfin.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:04:59
*********************************************************************/

#include "class.h"
#include "udebug.h"

extern UC_TABLE_DESCRIPTOR UC_cot_descriptor;

/*********************************************************************
**    E_FUNCTION :  int uc_init_surface_class()
**       Initialize UNIBASE relation and methods for surface super class.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_surface_class()
	{
	int status = UU_SUCCESS;
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();

	uu_denter(UU_MTRC,(us,"uc_init_surface_class()"));

	class_rec = ucu_init_class_rec(UC_SURFACE_CLASS, &UC_cot_descriptor);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_group);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PLOC_TO_COORD, um_surface_ploc_to_coord);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_TYPE, um_draft_type);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_ENDPTS, um_d_endpts);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_INIT_EVSRFOUT, um_init_evsrfout);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_surface_class);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int uc_init_agsrf()
**	     Initialize AG surface relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_init_agsrf()

	{
	UC_CLASS_REC *class_rec;
	UC_CLASS_REC *ucu_init_class_rec();
	int class;
	int j;
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_agsrf()"));

	uc_uni_init_agsrf();

	class = CONVERT_REL_TO_CLASS(UM_AGSRF_REL);
	class_rec = ucu_init_class_rec(class, &UC_cot_descriptor);

	/* DO NOT INHERIT ANY METHODS!!!! */
	for (j=0; j<UC_NUM_METHODS; j++) class_rec->methods[j] = UC_UNDEFINED_METHOD;

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_DATA, uc_retrieve_mtuple_data);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_TRANSF, uc_retrieve_mtuple_transf);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_ATTR, uc_retrieve_mtuple_attr);
/*	
 UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CREATE_DATA, ncl_create_aggeo);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SETUP_DATA, um_agsrf_setup_data);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, um_agsrf_delete);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE_UNDELETED, um_cannot_undelete);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DISPLAY, uv_disp_entity);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, um_agsrf_draw);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, um_agsrf_print);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, um_agsrf_copy);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, um_translate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, um_agsrf_transform);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_INIT_EVSRFOUT, um_init_evsrfout);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_EVSRF, um_agsrf_evaluate);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PLOC_TO_COORD, um_agsrf_ploc_to_coord);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_FEATURE, um_agsrf_feature);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_compsymgroup);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_TYPE, um_agsrf_draft_type);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_ENDPTS, um_d_endpts);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SPAN_ENTITY, um_agsrf_span);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SRF_TESSELLATE, um_agsrf_tessellate);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING, um_agsrf_proj_to_drawing);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, um_agsrf_query);
*/

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_agsrf);

	uu_dexit;
	return(status);
	}

