/*********************************************************************
**
**    NAME         :  ccrvin.c
**
**       CONTAINS: routines to initialize standard MPE curve class
**			int uc_init_curve_class()
**			int uc_init_line()
**			int uc_init_circle()
**			int uc_init_conic()
**			int uc_init_comp()
**			int uc_init_rspl()
**			int uc_init_cvsf()
**			int uc_init_agcrv()
**			int uc_init_polyline()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ccrvin.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:04:58
*********************************************************************/

#include "class.h"
#include "udebug.h"

extern UC_TABLE_DESCRIPTOR UC_cot_descriptor;

/*********************************************************************
**    E_FUNCTION :  int uc_init_curve_class()
**       Initialize UNIBASE relation and methods for curve super class.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_curve_class()
	{
	int status = UU_SUCCESS;
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();

	uu_denter(UU_MTRC,(us,"uc_init_curve()"));

	class_rec = ucu_init_class_rec(UC_CURVE_CLASS, &UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PLOC_TO_COORD, um_curve_ploc_to_coord);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PLOC_TO_VECTOR, um_curve_ploc_to_vector);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING, um_proj_to_drawing);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_TYPE, um_draft_type);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_LINE, um_get_line_data);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_ARC, um_get_circle_data);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_CONIC, um_get_conic_data);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_ENDPTS, um_d_endpts);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_NEAR_ON_ENTITY, um_near_on_curve);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_INIT_EVCRVOUT, um_init_evcrvout);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CRV_INTERSECT, um_isect);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CRV_INTERSECT_SP, um_isect_sp);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CCTOU, um_cctou);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_UTOT, um_utot);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ALTOU, um_u_equals_pal);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PT_ON_CRV_AT_PAL, umu_pt_along_crv_at_u);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SPAN_ENTITY, um_span_entity);
/*	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TAN_TAN_LINE, um_tan_tan_line);*/
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_FILLET_CURVE, um_crv_fillet);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJECT_TO_PLANE, um_proj_geom_to_plane);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_symgroup);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_curve_class);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_init_line()
**       Initialize UNIBASE relation and methods for lines.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_line()
	{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_line()"));

	uc_uni_init_line();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_LINE_REL), 
							&UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, um_drw2_line);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, um_p2_line);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, um_cp2_copyline);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, um_tr2_tranline);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, um_tf2_tranfline);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_FEATURE, um_f2line);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_NEAR_ON_ENTITY, um_near_on_line);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_EVCRV, um_ev2_line);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CRV_TO_UNIRBSC, um_c7_frmline);
/*	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CRV_TO_AGRBSC, um_agcrv_frmline);*/
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRIM_EXTEND_CURVE, um_trim_extend_curve);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIDTRIM_CURVE, um_midtrim_curve);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SPLIT_CURVE, um_c2_splitline);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MODIFY_ENTITY, umu_modify_line);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_compsymgroup);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, um_line_query);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_REVERSE_CRV, um_reverse_line);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_line);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_init_circle()
**       Initialize UNIBASE relation and methods for circles.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_circle()
	{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_circle()"));

	uc_uni_init_circle();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_CIRCLE_REL), 
							&UC_cot_descriptor);

/*
.....Change 'ncl_draw' back to 'um_drw3_circle'
.....Also changed 'um_drw3_circle' to accept
.....*SET/ADISPL value for drawing circles
.....Bobby  -  8/7/91
*/
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, um_drw3_circle);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, um_p3_circle);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, um_cp3_copycirc);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, um_tr3_trancirc);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, um_mir3_mircirc);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, um_tf3_tranfcirc);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_FEATURE, um_f3circle);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_NEAR_ON_ENTITY, um_near_on_circle);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_EVCRV, um_ev3_circle);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CRV_TO_UNIRBSC, um_c7_frmcirc);
/*	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CRV_TO_AGRBSC, um_agcrv_frmcirc);*/
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TAN_LINE_THRU_PT, um_c2_pt_cir);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SPLIT_CURVE, um_c3_splitcircle);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRIM_EXTEND_CURVE, um_trim_extend_curve);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIDTRIM_CURVE, um_midtrim_curve);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MODIFY_ENTITY, umu_modify_circle);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_compsymgroup);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, um_circle_query);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_REVERSE_CRV, um_reverse_circle);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_circle);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_init_conic()
**       Initialize UNIBASE relation and methods for conics.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_conic()
	{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_conic()"));

	uc_uni_init_conic();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_CONIC_REL), 
							&UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, um_drw4_conic);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, um_p4_conic);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, um_cp4_copyconic);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, um_tr4_tranconic);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, um_tf4_tranfconic);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_FEATURE, um_f4_conic);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_EVCRV, um_ev4_conic);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CRV_TO_UNIRBSC, um_c7_frmconic);
/*	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CRV_TO_AGRBSC, um_agcrv_frmconic);*/
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRIM_EXTEND_CURVE, um_trim_extend_curve);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIDTRIM_CURVE, um_midtrim_curve);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MODIFY_ENTITY, umu_modify_conic);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SPLIT_CURVE, um_c4_splitconic);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, um_conic_query);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_REVERSE_CRV, um_reverse_conic);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_conic);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_init_compcrv()
**       Initialize UNIBASE relation and methods for oomposite curves.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_compcrv()
	{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;


	uu_denter(UU_MTRC,(us,"uc_init_compcrv()"));

	uc_uni_init_compcrv();

/*	-- turn off associativity -- */

	ur_ignore_associative(UM_COMPCRV_REL);

	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_COMPCRV_REL), 
							&UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DISPLAY, um_disp5_compcrv);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, um_drw5_compcrv);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, um_p5_compcrv);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, um_dl5_compcrv);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE_UNDELETED, um_undl5_compcrv);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, um_cp5_copycompcrv);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, um_tr5_trancompcrv);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, um_tf5_tranfcompcrv);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_EVCRV, um_ev5_compcrv);
/*	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CRV_TO_AGRBSC, um_agcrv_frmcompcrv);*/
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DISSOLVE, um_dis5_compcrv);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, um_compcrv_query);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_FEATURE, um_f5compcrv);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_compsymgroup);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_REVERSE_CRV, um_reverse_compcrv);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CCTOU, um_cctou_compcrv);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_compcrv);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_init_rspl()
**       Initialize UNIBASE relation and methods for rational bsplines.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_rspl()
	{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_rspl()"));

	uc_uni_init_rspl();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_RBSPLCRV_REL), 
							&UC_cot_descriptor);

/*  UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, um_drw7_rbsplcrv); */
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_DATA, ncl_retrieve_data);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, ncl_disp_curve);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, um_p7_rbsplcrv);
/* 	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, um_cp7_copyrbsplcrv); */
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, ncl_class_copy_rbsp);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, um_tr7_tranrbsplcrv);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, um_tf7_tranfrbsplcrv);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_FEATURE, ncl_rbsp_feat);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_EVCRV, um_ev7_rbsplcrv); 
/*   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_EVCRV, ncl_eval_rbsp); */
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CRV_TO_UNIRBSC, um_c7_frmrbsplcrv);
/*	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CRV_TO_AGRBSC, ncl_agcrv_frmrbsplcrv);*/
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRIM_EXTEND_CURVE, um_trim_extend_curve);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIDTRIM_CURVE, um_midtrim_curve);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SPLIT_CURVE, um_c7_splitrbsplcrv);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_compsymgroup);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_REVERSE_CRV, ncl_rbsp_reverse);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, ncl_rbcv_query);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_rspl);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_init_cvsf()
**       Initialize UNIBASE relation and methods for bsplines on surf.
**
**       NOTE: some methods are from splines but they are not used yet
**             and will be replaced in future. 
**
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_cvsf()
{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_rspl()"));


	uc_uni_init_cvsf();
	ur_ignore_associative(UM_UVCVONSF_REL);
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_UVCVONSF_REL), 
							&UC_cot_descriptor);

/*  UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, um_drw7_rbsplcrv); */
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_DATA, ncl_retrieve_data);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, ncl_disp_cvonsf);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, um_p7_rbsplcrv);
 	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, um_cp7_copyrbsplcrv); 
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, um_c13_delete);
/*	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, ncl_class_copy_rbsp);*/
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, um_tr7_tranrbsplcrv);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, ncl_trans_cvsf);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_FEATURE, ncl_rbsp_feat);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_EVCRV, um_ev13_uvcvonsf); 
/*   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_EVCRV, ncl_eval_rbsp); */
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CRV_TO_UNIRBSC, um_c7_frmrbsplcrv);
/*	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CRV_TO_AGRBSC, ncl_agcrv_frmrbsplcrv);*/
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRIM_EXTEND_CURVE, um_trim_extend_curve);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIDTRIM_CURVE, um_midtrim_curve);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SPLIT_CURVE, um_c7_splitrbsplcrv);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_compsymgroup);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_REVERSE_CRV, um_c13_reverse);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, ncl_cvsf_query);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_cvsf);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int uc_init_agcrv()
**	     Initialize AG curve relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_init_agcrv()

	{
	UC_CLASS_REC *class_rec;
	UC_CLASS_REC *ucu_init_class_rec();
	int class;
	int j;
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_agcrv()"));

	uc_uni_init_agcrv();

	class = CONVERT_REL_TO_CLASS(UM_AGCRV_REL);
	class_rec = ucu_init_class_rec(class, &UC_cot_descriptor);

	/* DO NOT INHERIT ANY METHODS!!!! */
	for (j=0; j<UC_NUM_METHODS; j++) class_rec->methods[j] = UC_UNDEFINED_METHOD;

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_DATA, uc_retrieve_mtuple_data);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_TRANSF, uc_retrieve_mtuple_transf);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_ATTR, uc_retrieve_mtuple_attr);
/*
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CREATE_DATA, ncl_create_aggeo);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SETUP_DATA, um_agcrv_setup_data);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, um_agcrv_delete);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE_UNDELETED, um_cannot_undelete);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DISPLAY, uv_disp_entity);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, um_agcrv_draw);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, um_agcrv_print);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, um_agcrv_copy);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, um_translate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, um_agcrv_transform);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_NEAR_ON_ENTITY, um_near_on_curve);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_INIT_EVCRVOUT, um_init_evcrvout);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_EVCRV, um_agcrv_evaluate);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CCTOU, um_agcrv_cctou);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CRV_INTERSECT, um_agcrv_isect);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CRV_INTERSECT_SP, um_agcrv_isect_sp);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CRV_TO_AGRBSC, um_crv_to_agrbsc);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PLOC_TO_COORD, um_agcrv_ploc_to_coord);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PLOC_TO_VECTOR, um_agcrv_ploc_to_vector);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_FEATURE, um_agcrv_feature);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_TYPE, um_agcrv_draft_type);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_ENDPTS, um_d_endpts);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SPAN_ENTITY, um_agcrv_span);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING, um_agcrv_proj_to_drawing);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TAN_LINE_THRU_PT, um_tan_line_thru_pt);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TAN_TAN_LINE, um_tan_tan_line);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRIM_EXTEND_CURVE, um_agcrv_trim_extend);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIDTRIM_CURVE, um_agcrv_midtrim);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SPLIT_CURVE, um_agcrv_split);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_FILLET_CURVE, um_agcrv_fillet);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DISSOLVE, um_agcrv_dissolve);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJECT_TO_PLANE, um_agcrv_proj_to_plane);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PT_ON_CRV_AT_PAL, umu_pt_on_curve_at_pal);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_REVERSE_CRV, um_agcrv_reverse);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, um_agcrv_query);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_compsymgroup);
*/
	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_agcrv);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_init_polyline()
**       Initialize UNIBASE relation and methods for polylines.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_init_polyline()
	{
	UC_CLASS_REC *class_rec, *ucu_init_class_rec();
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_init_polyline()"));

	uc_uni_init_polyline();
	class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_POLYLINE_REL), 
							&UC_cot_descriptor);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, um_drw42_polyline);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, um_p42_polyline);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, um_cp42_copypolyline);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE, um_tr42_tranpolyline);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, um_tf42_tranfpolyline);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, ncl_polyln_query);
/*	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CRV_TO_AGRBSC, um_agcrv_frmpolyline);*/
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_compsymgroup);
/*
.....vp 1.24.97 add evaluator to support trimmed surfaces trimmed by
.....polyline.  This should be checked out on 12.10.96.
*/
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_EVCRV, um_ev42_polyline);

	ucu_init_class(class_rec, &UC_cot_descriptor);
	UC_ADD_METHOD_NAME(&UC_cot_descriptor, uc_init_polyline);

	uu_dexit;
	return(status);
	}

