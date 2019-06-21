/*********************************************************************
**    NAME         :  cgeoin.c
**       CONTAINS: routines to initialize standard MPE geometry
**			int uc_init_geom_class()
**			int uc_init_point()
**			int uc_init_polygon()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       cgeoin.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:04:58
*********************************************************************/

#include "class.h"
#include "udebug.h"

extern UC_TABLE_DESCRIPTOR UC_cot_descriptor;

/*********************************************************************
**    E_FUNCTION :  int uc_init_geom_class()
**       Initialize UNIBASE relation and methods for geometry super class.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_geom_class()
	{
	int status = UU_SUCCESS;
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();

	uu_denter(UU_MTRC,(us,"uc_init_geometry()"));
	um_init_labels();							/* initialize relation label server */

	class_rec = ucu_init_class_rec(UC_GEOMETRY_CLASS, &UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DISPLAY, uv_disp_entity);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, um_delete_all);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE_UNDELETED, um_can_undelete);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_DATA, um_get_all_geom);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_TRANSF, um_get_transformation);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_ATTR, uc_retrieve_mtuple_attr);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CREATE_DATA, um_create_geom);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, um_query);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_nothing);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_geom_class);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_init_point()
**       Initialize UNIBASE relation and methods for points.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_point()
	{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_point()"));

	uc_uni_init_point();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_POINT_REL), 
							&UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, um_drw1_pt);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, um_p1_pt);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, um_cp1_copypt);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, um_tr1_tranpt);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, um_tf1_tranfpt);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CREATE_DATA, um_create_pt1);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PLOC_TO_COORD, um_d_nrendpt);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PLOC_TO_VECTOR, um_d_vec);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJECT_TO_PLANE, um_proj_geom_to_plane);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_TYPE, um_draft_type);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_ENDPTS, um_d_endpts);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_symgroup);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, um_point_query);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SPAN_ENTITY, um_span_entity);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_NEAR_ON_ENTITY, um_near_on_point);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING, um_proj_to_drawing);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_point);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_init_polygon()
**       Initialize UNIBASE relation and methods for polygons.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_polygon()
	{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_polygon()"));

	uc_uni_init_polygon();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_POLY_REL), 
							&UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, um_drw40_poly);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, um_p40_poly);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, um_cp40_copypoly);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, um_tr40_tranpoly);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, um_tf40_tranfpoly);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING, um_proj_to_drawing);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MODIFY_ENTITY, umu_modify_polygon);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_polygon);

	uu_dexit;
	return(status);
	}

