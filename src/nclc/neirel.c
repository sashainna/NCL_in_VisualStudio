/*********************************************************************
**    NAME         :  neirel.c
**       CONTAINS: routines to initialize NCL specific relations
**
**       ncl_init_attr()
**       ncl_init_surfattr()
**       ncl_init_vector()
**       ncl_init_point()
**       ncl_init_line()
**       ncl_init_circle()
**       ncl_init_plane()
**       ncl_init_matrix()
**       ncl_init_scalar()
**       ncl_init_curve()
**       ncl_init_surf()
**       ncl_init_revsurf()
**       ncl_init_panel()
**       ncl_init_meshsf()
**       ncl_init_quiltsf()
**       ncl_init_patern()
**       ncl_init_netsf()
**       ncl_init_shape()
**       ncl_init_evalcv()
**       ncl_init_evalsf()
**       ncl_init_rbsf()
**       ncl_init_pntvec()
**       ncl_init_trimsf()
**       ncl_init_datast()
**       ncl_init_textvar()
**       ncl_init_solid()
**       ncl_assign_methods()
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       neirel.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:36
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "mdrel.h"
#include "class.h"
#include "nccs.h"

extern UC_TABLE_DESCRIPTOR UC_cot_descriptor;

/*********************************************************************
**    E_FUNCTION     : ncl_init_attr()
**      Initializes NCL attribute UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_attr()
   {
   uu_denter(UU_MTRC,(us,"ncl_init_attr()"));

   ncl_uni_init_attr();

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_surfattr()
**      Initializes Surface attribute UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_init_surfattr()
{
   ncl_uni_init_surfattr();
   return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_init_vector()
**      Initializes NCL vector UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_vector()

   {
   int j;
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_vector()"));

   ncl_uni_init_vector();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_VECTOR_REL), &UC_cot_descriptor);
/*
....DO NOT INHERIT ANY METHODS!!!!  - vectors now fall in the class UC_CURVE_CLASS...
*/
   for (j=0; j<UC_NUM_METHODS; j++) class_rec->methods[j] = UC_UNDEFINED_METHOD;

/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_VECTOR_REL, class_rec);

/*
....Set up so we can get a directional vector from the entity when
....picked while expecting vector style input in 'by text' mode.
...vp 5-may-93 added projection method routine.
*/
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING,
                                          ncl_proj_vec_to_drawing);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_EVCRV, ncl_ev_vector);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_QUERY, um_vector_query);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_INIT_EVCRVOUT, um_init_evcrvout);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_PLOC_TO_VECTOR, um_curve_ploc_to_vector);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_TRANSFORM, um_tf2_tranfvec);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_point()
**      Initializes NCL point UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_point()

   {
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_point()"));

   ncl_uni_init_point();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_POINT_REL), &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_POINT_REL, class_rec);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PLOC_TO_COORD, um_d_nrendpt);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PLOC_TO_VECTOR, um_d_vec);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_line()
**      Initializes NCL line UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_line()

   {
   int j;
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_line()"));

   ncl_uni_init_line();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_LINE_REL), &UC_cot_descriptor);
/*
....DO NOT INHERIT ANY METHODS!!!!  - lines now fall in the class UC_CURVE_CLASS...
*/
   for (j=0; j<UC_NUM_METHODS; j++) class_rec->methods[j] = UC_UNDEFINED_METHOD;

/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_LINE_REL, class_rec);

   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_EVCRV, ncl_ev_line);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_INIT_EVCRVOUT, um_init_evcrvout);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_PLOC_TO_VECTOR, um_curve_ploc_to_vector);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_PLOC_TO_COORD, um_curve_ploc_to_coord);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_circle()
**      Initializes NCL circle UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_circle()

   {
   int j;
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_circle()"));

   ncl_uni_init_circle();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_CIRCLE_REL), &UC_cot_descriptor);
/*
....DO NOT INHERIT ANY METHODS!!!!  - circles now fall in the class UC_CURVE_CLASS...
*/
   for (j=0; j<UC_NUM_METHODS; j++) class_rec->methods[j] = UC_UNDEFINED_METHOD;

/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_CIRCLE_REL, class_rec);

   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_EVCRV, ncl_ev_circle);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_INIT_EVCRVOUT, um_init_evcrvout);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_PLOC_TO_VECTOR, um_curve_ploc_to_vector);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_PLOC_TO_COORD, um_curve_ploc_to_coord);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_plane()
**      Initializes NCL plane UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_plane()

   {
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_plane()"));

   ncl_uni_init_plane();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_PLN_REL), &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_PLN_REL, class_rec);

   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_PLOC_TO_VECTOR, ncl_plane_to_vector);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_PLOC_TO_COORD, ncl_plane_to_coord);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_TRANSFORM, um_tf2_tranfpln);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_QUERY, um_plane_query);
	UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_matrix()
**      Initializes NCL matrix UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_matrix()

   {
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_matrix()"));

   ncl_uni_init_matrix();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_MATRIX_REL), &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_MATRIX_REL, class_rec);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, um_matrix_query);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_scalar()
**      Initializes NCL scalar UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_scalar()

   {
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_scalar()"));

   ncl_uni_init_scalar();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_SCALAR_REL), &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_SCALAR_REL, class_rec);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_labloc()
**      Initializes NCL labloc UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_labloc()

   {
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_labloc()"));

   ncl_uni_init_labloc();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_LABTBL_REL), &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_LABTBL_REL, class_rec);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_curve()
**      Initializes NCL curve UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_curve()

   {
   int j;
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_curve()"));

   ncl_uni_init_curve();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_CURVE_REL), &UC_cot_descriptor);
/*
....DO NOT INHERIT ANY METHODS!!!!  - curves now fall in the class UC_CURVE_CLASS...
*/
   for (j=0; j<UC_NUM_METHODS; j++) class_rec->methods[j] = UC_UNDEFINED_METHOD;

/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_CURVE_REL, class_rec);

   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_INIT_EVCRVOUT, um_init_evcrvout);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_TRANSFORM, ncl_transform_nclcv);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_EVCRV, ncl_ev_curve);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_COPY, ncl_copy_nclcrv);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_PLOC_TO_VECTOR, um_curve_ploc_to_vector);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_PLOC_TO_COORD, um_curve_ploc_to_coord);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_QUERY, um_curve_query);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_NEAR_ON_ENTITY, um_near_on_curve);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_TRIM_EXTEND_CURVE, um_trim_extend_curve);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_MIDTRIM_CURVE, um_midtrim_curve);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_DRAW, ncl_disp_curve);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_CCTOU, um_cctou);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_CRV_INTERSECT, um_isect);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_CRV_INTERSECT_SP, um_isect_sp);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_FEATURE, ncl_nclcv_feat);
 

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_surf()
**      Initializes NCL surf UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_surf()

   {
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_surf()"));

   ncl_uni_init_surf();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_SURF_REL), &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_SURF_REL, class_rec);

   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_TRANSFORM, ncl_transform_nsf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_DRAW, ncl_disp_surf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_TRANSLATE, um_translate_using_matrix);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_COPY, ncl_class_copy_nsf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_QUERY, um_surf_query);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_EVSRF, ncl_eval_surf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_SRF_TESSELLATE, ncl_tessellate_surf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_DRAFT_ENDPTS, ncl_sf_d_endpts);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_FEATURE, ncl_surf_feat);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_CANBE, um_canbe_symbol);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_revsurf()
**      Initializes NCL surface of revolution UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_revsurf()

   {
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_revsurf()"));

   ncl_uni_init_revsurf();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_REVSURF_REL), &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_REVSURF_REL, class_rec);

   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_EVSRF, ncl_eval_revsf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_DRAW, ncl_disp_surf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_QUERY, um_revsurf_query);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_COPY, ncl_class_copy_revsf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_TRANSFORM, ncl_transform_revsf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_TRANSLATE, um_translate_using_matrix);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_SRF_TESSELLATE, ncl_tessellate_surf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_FEATURE, ncl_surf_feat);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_DRAFT_ENDPTS, ncl_sf_d_endpts);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_CANBE, um_canbe_symbol);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_panel()
**      Initializes NCL panel UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_panel()

   {
   uu_denter(UU_MTRC,(us,"ncl_init_panel()"));

   ncl_uni_init_panel();

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_meshsf()
**      Initializes NCL meshsf UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_meshsf()

   {
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_meshsf()"));

   ncl_uni_init_meshsf();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_MESHSURF_REL), &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_MESHSURF_REL, class_rec);

   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_TRANSFORM, ncl_transform_msh);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_DRAW, ncl_disp_surf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_TRANSLATE, um_translate_using_matrix);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_COPY, ncl_class_copy_msh);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, um_meshsf_query);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_EVSRF, ncl_eval_surf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_SRF_TESSELLATE, ncl_tessellate_surf);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_ENDPTS, ncl_sf_d_endpts);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_FEATURE, ncl_surf_feat);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_quiltsf()
**      Initializes NCL quiltsf UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_quiltsf()

   {
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_quiltsf()"));

   ncl_uni_init_quiltsf();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_QUILTSURF_REL), &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_QUILTSURF_REL, class_rec);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_patern()
**      Initializes NCL patern UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_patern()

   {
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_patern()"));

   ncl_uni_init_patern();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_PATERN_REL), &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_PATERN_REL, class_rec);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, ncl_transform_pn);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, um_patern_query);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_netsf()
**      Initializes NCL netsf UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_init_netsf()

   {
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_netsf()"));

   ncl_uni_init_netsf();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_NETSF_REL), &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_NETSF_REL, class_rec);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_DRAW, ncl_draw_netsf);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, ncl_transform_netsf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_COPY, ncl_class_copy_netsf);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, um_netsf_query);
/*
... aak 20-apr-1998: added this method
*/
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING, ncl_proj_netsf_to_drawing);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_CANBE, um_canbe_symbol);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_shape()
**      Initializes NCL shape UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_shape()

   {
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_shape()"));

   ncl_uni_init_shape();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_SHAPE_REL), &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_SHAPE_REL, class_rec);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, um_shape_query);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_evalcv()
**      Initializes NCL evalcv UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_evalcv()

   {
   int j;
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_evalcv()"));

   ncl_uni_init_evalcv();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_EVALCV_REL), &UC_cot_descriptor);
/*
....DO NOT INHERIT ANY METHODS!!!!  - evcrvs now fall in the class UC_CURVE_CLASS...
*/
   for (j=0; j<UC_NUM_METHODS; j++) class_rec->methods[j] = UC_UNDEFINED_METHOD;

/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_EVALCV_REL, class_rec);

   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_EVCRV, ncl_eval_evalcv);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_INIT_EVCRVOUT, um_init_evcrvout);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_evalsf()
**      Initializes NCL evalsf UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_evalsf()

   {
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_evalsf()"));

   ncl_uni_init_evalsf();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_EVALSF_REL), &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_EVALSF_REL, class_rec);

   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_EVSRF, ncl_eval_evalsf);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, ncl_evalsf_query);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_FEATURE, ncl_surf_feat);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_rbsf()
**      Initializes NCL rbsf UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_init_rbsf()

   {
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_rbsf()"));

   ncl_uni_init_rbsf();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_RBSPLSRF_REL),
	 &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(UM_RBSPLSRF_REL, class_rec);

   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_EVSRF, um_ev9_rbsplsrf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_DRAW, ncl_disp_surf);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, ncl_rbsf_query);

   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SRF_TESSELLATE, ncl_tessellate_surf);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, ncl_class_copy_rbsf);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE,um_translate_using_matrix);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, ncl_transform_rbsf);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_TYPE, um_agsrf_draft_type);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_ENDPTS, um_d_endpts);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING,
                                          ncl_proj_rbsf_to_drawing);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_FEATURE, ncl_surf_feat);
/*  UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, um_delete_all); */
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_CANBE, um_canbe_symbol);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_pntvec()
**      Initializes NCL pntvec UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_pntvec()

   {
   int j;
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_pntvec()"));

   ncl_uni_init_pntvec();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_POINTVEC_REL),
	                        &UC_cot_descriptor);
   for (j=0; j<UC_NUM_METHODS; j++) class_rec->methods[j] = UC_UNDEFINED_METHOD;

/*
....Assign those methods common to all NCL geometry
...vp 5-may-93 added projection method routine.
*/
   ncl_assign_methods(NCL_POINTVEC_REL, class_rec);

   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING,
                                          ncl_proj_pntvec_to_drawing);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_EVCRV, ncl_ev_pntvec); 
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_QUERY, um_pntvec_query);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_INIT_EVCRVOUT, um_init_evcrvout);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_PLOC_TO_VECTOR, um_curve_ploc_to_vector);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_PLOC_TO_COORD, um_curve_ploc_to_coord);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_TRANSFORM, um_tf2_tranfpvec);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_trimsf()
**      Initializes NCL trimsf UNIBASE relation.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_trimsf()

   {
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   uu_denter(UU_MTRC,(us,"ncl_init_trimsf()"));

   ncl_uni_init_trimsf();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_TRIMSF_REL),
                                    &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_TRIMSF_REL, class_rec);

   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_EVSRF, ncl_eval_trimsf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_DRAW, ncl_disp_trimsf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_TRANSFORM, ncl_trans_trimsf);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE,um_translate_using_matrix);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, ncl_class_copy_trimsf);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SRF_TESSELLATE, ncl_tessellate_surf);
/*    UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_FEATURE, ncl_surf_feat); */
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, ncl_trimsf_query);
/*    UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_TYPE, um_agsrf_draft_type); */
/*    UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_ENDPTS, um_d_endpts); */
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING, ncl_proj_trimsf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_CANBE, um_canbe_symbol);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   uu_dexit;
   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_datast()
**      Initializes NCL datast UNIBASE relation.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_datast()

   {
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   ncl_uni_init_datast();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_DATAST_REL),
                 &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_DATAST_REL, class_rec);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_textvar()
**      Initializes NCL textvar UNIBASE relation.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_init_textvar()

   {
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   ncl_uni_init_textvar();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(NCL_TEXTVAR_REL),
                 &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(NCL_TEXTVAR_REL, class_rec);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   return(UU_SUCCESS);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_init_solid()
**      Initializes Visual Solid UNIBASE relation.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_init_solid()
{
   UC_CLASS_REC *class_rec, *ucu_init_class_rec();

   ncl_uni_init_solid();

   class_rec = ucu_init_class_rec(CONVERT_REL_TO_CLASS(UM_SOLID_REL),
                 &UC_cot_descriptor);
/*
....Assign those methods common to all NCL geometry
*/
   ncl_assign_methods(UM_SOLID_REL, class_rec);
/*
.....Assign methods
*/
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAW, ncl_draw_solid);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSFORM, ncl_transform_solid);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_TRANSLATE,um_translate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_ROTATE, um_rotate_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_SCALE, um_scale_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_MIRROR, um_mirror_using_matrix);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_COPY, ncl_copy_solid);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CREATE_DATA, ncl_create_solid);
   UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, ncl_delete);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PLOC_TO_COORD, ncl_solid_to_coord);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PLOC_TO_VECTOR, ncl_solid_to_vector);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_NEAR_ON_ENTITY,
		ncl_solid_near_point);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PRINT, ncl_print_solid);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, ncl_solid_query);

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_TYPE, um_draft_type);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DRAFT_ENDPTS, ncl_solid_d_endpts);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_symgroup);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_PROJ_TO_DRAWING,
		ncl_solid_proj_to_drawing);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE_UNDELETED, um_cannot_undelete);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_FEATURE, um_f8solid);

   ucu_init_class(class_rec, &UC_cot_descriptor);

   return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_assign_methods()
**      Assign ncl methods to  ncl geometry.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_assign_methods(rel_num, class_rec)
int rel_num;
UC_CLASS_REC *class_rec;
   {
   uu_denter(UU_MTRC,(us,"ncl_assign_methods()"));

/*
.....Methods inherited from UC_GEOMETRY class.  Since not all entities
.....Are in UC_GEOMETRY now, lets set these up explicitly so we don't 
.....break anything.
.....NCL lines, circles, vectors, evaluated curves and NCL curves are
.....Now in the UC_CURVE class.
*/
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DISPLAY, uv_disp_entity);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_DELETE, um_delete_all);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE_UNDELETED, um_can_undelete);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_DATA, um_get_all_geom);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_TRANSF, um_get_transformation);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_RETRIEVE_ATTR, uc_retrieve_mtuple_attr);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CREATE_DATA, um_create_geom);
	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_CANBE, um_canbe_nothing);
/*
.....End of (formerly) inherited methods - RAZ
*/

	UC_ASSIGN_METHOD(&UC_cot_descriptor, UC_QUERY, ncl_query);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_DRAW, ncl_draw);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_DISPLAY, ncl_display);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_PRINT, ncl_print);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_DELETE, ncl_delete);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_COPY, ncl_copy);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_RETRIEVE_DATA, ncl_retrieve_data);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_RETRIEVE_TRANSF, ncl_retrieve_transf);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_RETRIEVE_ATTR, ncl_retrieve_attr);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_CREATE_DATA, ncl_create);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_TRANSLATE, ncl_translate);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_ROTATE, ncl_rotate);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_SCALE, ncl_scale);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_MIRROR, ncl_mirror);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_TRANSFORM, ncl_transform);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_PROJ_TO_DRAWING, ncl_proj_to_drawing);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_DRAFT_TYPE, ncl_draft_type);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_DRAFT_LINE, ncl_get_line_data);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_DRAFT_ARC, ncl_get_circle_data);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_DRAFT_ENDPTS, ncl_d_endpts);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_NEAR_ON_ENTITY, ncl_near_on_entity);
   UC_ASSIGN_METHOD (&UC_cot_descriptor, UC_CRV_TO_AGRBSC, ncl_evcrv_to_agrbsc);

   uu_dexit;
   return(UU_SUCCESS);
   }
