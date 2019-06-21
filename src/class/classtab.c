/*********************************************************************
**    NAME         :  classtab.c
**       CONTAINS:
**       	class table definitions
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       classtab.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:04:58
*********************************************************************/

#include "class.h"

/*	-- Method Description Table for this class system -- */

	char *UC_meth_name[] = 
	{
		"method UC_DISPLAY           (0)",
		"method UC_DRAW              (1)",
		"method UC_PRINT             (2)",
		"method UC_DELETE            (3)",
		"method UC_COPY              (4)",
		"method UC_TRANSLATE         (5)",
		"method UC_ROTATE            (6)",
		"method UC_SCALE             (7)",
		"method UC_MIRROR            (8)",
		"method UC_TRANSFORM         (9)",
		"method UC_RETRIEVE_DATA    (10)",
		"method UC_RETRIEVE_TRANSF  (11)",
		"method UC_RETRIEVE_ATTR    (12)",
		"method UC_CREATE_DATA      (13)",
		"method UC_PLOC_TO_COORD    (14)",
		"method UC_PLOC_TO_VECTOR   (15)",
		"method UC_NEAR_ON_ENTITY   (16)",
		"method UC_FEATURE          (17)",
		"method UC_PROJ_TO_DRAWING  (18)",
		"method UC_DRAFT_TYPE       (19)",
		"method UC_DRAFT_LINE       (20)",
		"method UC_DRAFT_ARC        (21)",
		"method UC_DRAFT_CONIC      (22)",
		"method UC_DRAFT_ENDPTS     (23)",
		"method UC_QUERY            (24)",
		"method UC_CANBE            (25)",
		"method UC_INIT_EVCRVOUT    (26)",
		"method UC_EVCRV            (27)",
		"method UC_CCTOU            (28)",
		"method UC_CRV_INTERSECT    (29)",
		"method UC_CRV_TO_UNIRBSC   (30)",
		"method UC_CRV_TO_AGRBSC    (31)",
		"method UC_INIT_EVSRFOUT    (32)",
		"method UC_EVSRF            (33)",
		"method UC_SPAN_ENTITY      (34)",
		"method UC_TAN_LINE_THRU_PT (35)",
		"method UC_TAN_TAN_LINE     (36)",
		"method UC_FILLET           (37)",
		"method UC_TRIM_EXTEND      (38)",
		"method UC_SPLIT_CURVE      (39)",
		"method UC_EXTEND_CURVE     (40)",
		"method UC_MIDTRIM_CURVE    (41)",
  		"method UC_SRF_TESSALATE    (42)",
  		"method UC_CRV_INTERSECT_SP (43)",
		"method UC_ASSOC_UPDATE     (44)",
		"method UC_DISSOLVE         (45)",
		"method UC_UTOT             (46)",
		"method UC_PROJECT_TO_PLANE (47)",
		"method UC_MODIFY_ENTITY    (48)",
		"method UC_ALTOU            (49)",
		"method UC_ALTOUV           (50)",
		"method UC_PT_ON_CRV_AT_PAL (51)",
		"method UC_PT_ON_SRF_AT_PAL (52)",
		"method UC_SETUP_DATA       (53)",
		"method UC_CANBE_UNDELETED  (54)",
		"method UC_REVERSE_CRV      (55)"
	};

static UC_METHOD_TABLE UC_method_descriptor =
	{1024, 0, UU_NULL, UC_meth_name};

#define UC_UM UC_UNDEFINED_METHOD
#define UC_ROOT UC_ROOT_CLASS
#define UC_UNIBASE UC_UNIBASE_CLASS
#define UC_GEOMETRY UC_GEOMETRY_CLASS
#define UC_CURVE UC_CURVE_CLASS
#define UC_SURFACE UC_SURFACE_CLASS
#define UC_SOLID UC_SOLID_CLASS
#define UC_ANNOTATION UC_ANNOTATION_CLASS
#define UC_SUPPORT UC_SUPPORT_CLASS
#define UC_VIEW UC_VIEWING_CLASS
#define UC_RELATED UC_RELATED_CLASS
#define UC_SYMBOL UC_SYMBOL_CLASS
#define UC_ATTRIBUTE UC_ATTRIBUTE_CLASS
#define UC_APPLICATION UC_APPLICATION_CLASS

	extern int uc_init_root();
	extern int uc_init_unibase_class();
	extern int uc_init_geom_class();
	extern int uc_init_curve_class();
	extern int uc_init_surface_class();
	extern int uc_init_solid_class();
	extern int uc_init_anno_class();
	extern int uc_init_support_class();
	extern int uc_init_viewing_class();
	extern int uc_init_related_class();
	extern int uc_init_symbol_class();
	extern int uc_init_attribute_class();
	extern int uc_init_application_class();

extern int uc_init_point(), uc_init_line(), uc_init_circle(), uc_init_conic();
extern int uc_init_compcrv(), uc_init_rspl(), uc_init_body();
extern int uc_init_group(), uc_init_connector(), uc_init_text();
extern int uc_init_txtatt(), uc_init_tran(), uc_init_modatt();
extern int uc_init_polygon(), uc_init_polyline(), uc_init_corsys(),uc_init_light();
extern int uc_init_draw(), uc_init_layer(), uc_init_lindim(), uc_init_xhatch();
extern int uc_init_massym(), uc_init_inssym(), uc_init_attsym();
extern int uc_init_view73(), uc_init_view74();
extern int uc_init_view75(), uc_init_calc(), uc_init_cvsf(); 
extern int uc_init_agcrv(), uc_init_agsrf(), uc_init_agshell();

/*  NCL initialization routines  */

extern int ncl_init_attr(), ncl_init_vector(), ncl_init_matrix();
extern int ncl_init_curve(), ncl_init_surf(), ncl_init_revsurf(); 
extern int ncl_init_panel(), ncl_init_meshsf(), ncl_init_quiltsf();
extern int ncl_init_shape(), ncl_init_point(), ncl_init_line();
extern int ncl_init_circle(), ncl_init_plane(), ncl_init_patern();
extern int ncl_init_netsf(), ncl_init_scalar(), ncl_init_labloc();
extern int ncl_init_evalcv(), ncl_init_evalsf();
extern int ncl_init_rbsf(), ncl_init_pntvec();
extern int ncl_init_trimsf(), ncl_init_datast();
extern int ncl_init_textvar();
extern int ncl_init_surfattr(), ncl_init_solid();
extern int uc_init_color(), uc_init_unistat();

/*	-- Define ROOT Class Table -- */

static UC_CLASS_REC UC_root0 = 
	UC_SUPER_IN(UC_ROOT_CLASS, "ROOT", UC_UNDEFINED_CLASS_STAT, uc_init_root);

static UC_CLASS_REC *UC_root[] = {&UC_root0};

UC_TABLE_DESCRIPTOR UC_root_descriptor = 
				{"UC_root", 1, 0, UC_root, UC_root,
						&UC_method_descriptor};

/*	-- Define "User" Class Tables -- */

/*	-- Super Classes-- */

static UC_CLASS_REC UC_cot_uni = 
	UC_SUPER_IN(UC_UNIBASE_CLASS, "UNIBASE",UC_ROOT_CLASS,
		uc_init_unibase_class);

static UC_CLASS_REC UC_cot_geo = 
	UC_SUPER_IN(UC_GEOMETRY_CLASS, "GEOMETRY", UC_UNIBASE_CLASS,
		uc_init_geom_class);

static UC_CLASS_REC UC_cot_cur = 
	UC_SUPER_IN(UC_CURVE_CLASS, "CURVE", UC_GEOMETRY_CLASS, uc_init_curve_class);

static UC_CLASS_REC UC_cot_sur = 
	UC_SUPER_IN(UC_SURFACE_CLASS, "SURFACE", UC_GEOMETRY_CLASS,
		uc_init_surface_class);

static UC_CLASS_REC UC_cot_sol = 
	UC_SUPER_IN(UC_SOLID_CLASS, "SOLID", UC_GEOMETRY_CLASS, uc_init_solid_class);

static UC_CLASS_REC UC_cot_ann = 
	UC_SUPER_IN(UC_ANNOTATION_CLASS, "ANNOTATION", UC_UNIBASE_CLASS,
		uc_init_anno_class);

static UC_CLASS_REC UC_cot_sup = 
	UC_SUPER_IN(UC_SUPPORT_CLASS, "SUPPORT", UC_UNIBASE_CLASS,
		uc_init_support_class);

static UC_CLASS_REC UC_cot_vw = 
	UC_SUPER_IN(UC_VIEWING_CLASS, "VIEWING", UC_SUPPORT_CLASS,
		uc_init_viewing_class);

static UC_CLASS_REC UC_cot_rel = 
	UC_SUPER_IN(UC_RELATED_CLASS, "RELATED", UC_UNIBASE_CLASS,
		uc_init_related_class);

static UC_CLASS_REC UC_cot_sym = 
	UC_SUPER_IN(UC_SYMBOL_CLASS, "SYMBOL", UC_RELATED_CLASS,
		uc_init_symbol_class);

static UC_CLASS_REC UC_cot_att = 
	UC_SUPER_IN(UC_ATTRIBUTE_CLASS, "ATTRIBUTE", UC_UNIBASE_CLASS,
		uc_init_attribute_class);

static UC_CLASS_REC UC_cot_app = 
	UC_SUPER_IN(UC_APPLICATION_CLASS, "APPLICATION", UC_UNIBASE_CLASS,
		uc_init_application_class);

/*	-- Point Class -- */

static UC_CLASS_REC UC_cot1 = 		 				/* 1 - point */
	UC_CREL_IN(UM_POINT_REL, "point", UC_GEOMETRY, uc_init_point);

/*	-- Curve Class -- */

static UC_CLASS_REC UC_cot2 = 		 				/* 2 - line */
	UC_CREL_IN(UM_LINE_REL, "line", UC_CURVE, uc_init_line);

static UC_CLASS_REC UC_cot3 = 		 				/* 3 - circle */
	UC_CREL_IN(UM_CIRCLE_REL, "circle", UC_CURVE, uc_init_circle);

static UC_CLASS_REC UC_cot4 = 		 				/* 4 - conic curve*/
	UC_CREL_IN(UM_CONIC_REL, "conic", UC_CURVE, uc_init_conic);

static UC_CLASS_REC UC_cot5 = 						/* 5 - composite curve */
	UC_CREL_IN(UM_COMPCRV_REL, "composite", UC_CURVE, uc_init_compcrv);

/* 6 - bspline curve */

static UC_CLASS_REC UC_cot7 = 						/* 7 - rational bspline */
	UC_CREL_IN(UM_RBSPLCRV_REL, "rat bspline", UC_CURVE, uc_init_rspl);

static UC_CLASS_REC UC_cot8 = 						/* 8 - ag rational bspline */
	UC_CREL_IN(UM_AGCRV_REL, "curve", UC_CURVE, uc_init_agcrv);

/* 9 - */


static UC_CLASS_REC UC_cot10 = 						/* 10 - ag surfaces */
	UC_CREL_IN(UM_AGSRF_REL, "surface", UC_SURFACE, uc_init_agsrf);

/* 11 - */

static UC_CLASS_REC UC_cot11 = 						/* 11 - rb surfaces */
	UC_CREL_IN(UM_RBSPLSRF_REL, "rb surface", UC_SURFACE, ncl_init_rbsf);


/* 12 - */
/* 13 - */

static UC_CLASS_REC UC_cot13 = 						/* 13 - curve on sf */
	UC_CREL_IN(UM_UVCVONSF_REL, "uvcv on sf", UC_CURVE, uc_init_cvsf);

/* 14 - */

static UC_CLASS_REC UC_cot14 = 				/* 14 - surface attribute */
	UC_CREL_IN(UM_SURFATTR_REL, "surface attribute", UC_ATTRIBUTE,
		ncl_init_surfattr);

/* 15 - */
/* 16 - */
/* 17 - */
/* 18 - */
/* 19 - */

/* 20 - */

static UC_CLASS_REC UC_cot20 = 						/* 20 - ag shell */
	UC_CREL_IN(UM_AGSHELL_REL, "shell", UC_SOLID, uc_init_agshell);

/* 21 - */

static UC_CLASS_REC UC_cot21 = 						/* 21 - visual solid */
	UC_CREL_IN(UM_SOLID_REL, "solid", UC_SOLID, ncl_init_solid);

/* 22 - */
/* 23 - */
/* 24 - */
/* 25 - */
/* 26 - */
/* 27 - */
/* 28 - */

/* 29 - */
/* 30 - */

/*	-- Solids Class -- */

static UC_CLASS_REC UC_cot31 = 		 		/* 31 - Romulus Body */
	UC_CREL_IN(UM_BODY_REL, "Romulus Body", UC_SOLID_CLASS, uc_init_body);

/* -- Miscellaneous Geometry -- */

/* 32 - features "relation" (not UNIBASE) */

/* 33 */
/* 34 */
/* 35 */

static UC_CLASS_REC UC_cot36 = 		 		/* 36 - transformation */
	UC_CREL_IN(UM_TRANSFORM_REL, "transformation", UC_ATTRIBUTE, uc_init_tran);

static UC_CLASS_REC UC_cot37 = 				/* 37 - model attribute */
	UC_CREL_IN(UM_ATTR_REL, "model attribute", UC_ATTRIBUTE, uc_init_modatt);

static UC_CLASS_REC UC_cot38 = 				/* 38 - light */
	UC_CREL_IN(UM_LIGHT_REL, "light", UC_SUPPORT, uc_init_light);

/* 39 */

static UC_CLASS_REC UC_cot40 = 				/* 40 - polyfill region	*/
	UC_CREL_IN(UM_POLY_REL, "polyfill surf", UC_SURFACE, uc_init_polygon);

/* UNUSED 41 */

static UC_CLASS_REC UC_cot42 = 		 		/* 42 - polyline */
	UC_CREL_IN(UM_POLYLINE_REL, "polyline", UC_CURVE, uc_init_polyline);

static UC_CLASS_REC UC_cot43 = 		 		/* 43 - coord system */
	UC_CREL_IN(UM_COORDSYS_REL, "coord system", UC_SUPPORT, uc_init_corsys);

static UC_CLASS_REC UC_cot44 = 				/* 44 - group */
	UC_CREL_IN(UM_GROUP_REL, "group", UC_RELATED, uc_init_group);

/* UNUSED 45 */

static UC_CLASS_REC UC_cot46 = 				/* 46 - drawing */
	UC_CREL_IN(UM_DRAWING_REL, "drawing entity", UC_SUPPORT, uc_init_draw);

static UC_CLASS_REC UC_cot47 = 				/* 47 - layer */
	UC_CREL_IN(UM_LAYER_REL, "layer entity", UC_SUPPORT, uc_init_layer);

/* -- Drafting Subsystem -- */

static UC_CLASS_REC UC_cot48 = 		 		/* 48 - linear dim */
	UC_CREL_IN(UA_LINEAR_DIMS_REL, "drafting", UC_ANNOTATION, uc_init_lindim);

/* 49 - */
/* 50 - */
/* 51 - */
/* 52 - */
/* 53 - */
/* 54 - */

static UC_CLASS_REC UC_cot55 = 				/* 55 - hatching */
	UC_CREL_IN(UA_HATCHING_REL, "crosshatch", UC_ANNOTATION, uc_init_xhatch);

/* 56 - */
/* 57 - */
/* 58 - */
/* 59 - */
/* 60 - */

/* -- Symbol Subsystem -- */

static UC_CLASS_REC UC_cot61 = 					/* 61 - master symbol */
	UC_CREL_IN(UB_SYMBOL_REL, "master symbol", UC_SYMBOL, uc_init_massym);

static UC_CLASS_REC UC_cot62 = 		 			/* 62 - symbol instance */
	UC_CREL_IN(UB_INSTANCE_REL, "symbol instance", UC_SYMBOL, uc_init_inssym);

static UC_CLASS_REC UC_cot63 = 					/* 63 - symbol attr */
	UC_CREL_IN(UB_SYMATTR_REL, "symbol attribute", UC_ATTRIBUTE, uc_init_attsym);

static UC_CLASS_REC UC_cot64 = 					/* 64 - connector */
	UC_CREL_IN(UB_CONECTOR_REL, "connector", UC_SYMBOL, uc_init_connector);

/* UNUSED 65 */
/* UNUSED 66 */
/* UNUSED 67 */
/* UNUSED 68 */
/* UNUSED 69 */
/* UNUSED 70 */
/* UNUSED 71 */
/* UNUSED 72 */

/* -- Viewing attribute relations -- */

static UC_CLASS_REC UC_cot73 = 					/* 73 - */
	UC_CREL_IN(UV_VIEW_REL, "view definition", UC_VIEW, uc_init_view73);

static UC_CLASS_REC UC_cot74 = 					/* 74 - */
	UC_CREL_IN(UV_VPORT_REL, "viewport", UC_VIEW, uc_init_view74);

static UC_CLASS_REC UC_cot75 = 					/* 75 - */
	UC_CREL_IN(UV_SCREEN_REL, "view screen", UC_VIEW, uc_init_view75);
 
/* -- Calculator Subsystem -- */

static UC_CLASS_REC UC_cot76 = 					/* 76 - */
	UC_CREL_IN(UQ_CALC_REL, "symbol table", UC_SUPPORT, uc_init_calc);

/* -- TEXT -- */

static UC_CLASS_REC UC_cot77 = 					/* 77 - text primitive */
	UC_CREL_IN(UA_TEXT_REL,"text",UC_ANNOTATION, uc_init_text);

static UC_CLASS_REC UC_cot78 = 					/* 78 - text attribute */
	UC_CREL_IN(UA_TEXTATTR_REL, "text attribute", UC_ATTRIBUTE, uc_init_txtatt);

/* -- NCL RELATIONS -- */

static UC_CLASS_REC UC_cot79 = 					/* 79 - NCL attribute */
	UC_CREL_IN(NCL_ATTR_REL, "NCL attribute", UC_ATTRIBUTE, ncl_init_attr);

static UC_CLASS_REC UC_cot80 = 					/* 80 - NCL vector */
	UC_CREL_IN(NCL_VECTOR_REL, "NCL vector", UC_CURVE, ncl_init_vector);

static UC_CLASS_REC UC_cot81 = 					/* 81 - NCL matrix */
	UC_CREL_IN(NCL_MATRIX_REL, "NCL matrix", UC_GEOMETRY, ncl_init_matrix);

static UC_CLASS_REC UC_cot82 = 					/* 82 - NCL curve */
	UC_CREL_IN(NCL_CURVE_REL, "NCL curve", UC_CURVE, ncl_init_curve);

/*
.....Changed from GEOMETRY to SURFACE class
.....Bobby  -  3/18/94
*/
static UC_CLASS_REC UC_cot83 = 					/* 83 - NCL surf */
	UC_CREL_IN(NCL_SURF_REL, "NCL surf", UC_SURFACE, ncl_init_surf);

static UC_CLASS_REC UC_cot84 = 					/* 84 - NCL panel */
	UC_CREL_IN(NCL_PANEL_REL, "NCL panel", UC_GEOMETRY, ncl_init_panel);

/*
.....Changed from GEOMETRY to SURFACE class
.....Bobby  -  3/18/94
*/
static UC_CLASS_REC UC_cot85 = 					/* 85 - NCL meshsf */
	UC_CREL_IN(NCL_MESHSURF_REL,"NCL meshsf",UC_SURFACE,ncl_init_meshsf);

static UC_CLASS_REC UC_cot86 = 					/* 86 - NCL quiltsf */
	UC_CREL_IN(NCL_QUILTSURF_REL,"NCL quiltsf", UC_GEOMETRY,
				  ncl_init_quiltsf);

static UC_CLASS_REC UC_cot87 = 					/* 87 - NCL shape */
	UC_CREL_IN(NCL_SHAPE_REL, "NCL shape", UC_GEOMETRY, ncl_init_shape);

static UC_CLASS_REC UC_cot88 = 					/* 88 - NCL point */
	UC_CREL_IN(NCL_POINT_REL, "NCL point", UC_GEOMETRY, ncl_init_point);

static UC_CLASS_REC UC_cot89 = 					/* 89 - NCL line */
	UC_CREL_IN(NCL_LINE_REL, "NCL line", UC_CURVE, ncl_init_line);

static UC_CLASS_REC UC_cot90 = 					/* 90 - NCL circle */
	UC_CREL_IN(NCL_CIRCLE_REL, "NCL circle", UC_CURVE, ncl_init_circle);

static UC_CLASS_REC UC_cot91 = 					/* 91 - NCL plane */
	UC_CREL_IN(NCL_PLN_REL, "NCL plane", UC_GEOMETRY, ncl_init_plane);

static UC_CLASS_REC UC_cot92 = 					/* 92 - NCL patern */
	UC_CREL_IN(NCL_PATERN_REL, "NCL patern", UC_GEOMETRY, ncl_init_patern);

static UC_CLASS_REC UC_cot93 = 					/* 93 - NCL netsf */
	UC_CREL_IN(NCL_NETSF_REL, "NCL netsf", UC_GEOMETRY, ncl_init_netsf);

static UC_CLASS_REC UC_cot94 = 					/* 94 - NCL scalar */
	UC_CREL_IN(NCL_SCALAR_REL, "NCL scalar", UC_GEOMETRY, ncl_init_scalar);

static UC_CLASS_REC UC_cot95 = 					/* 95 - NCL labtbl */
	UC_CREL_IN(NCL_LABTBL_REL, "NCL labtbl", UC_GEOMETRY, ncl_init_labloc);

static UC_CLASS_REC UC_cot96 = 				/* 96 - NCL evalcv */
	UC_CREL_IN(NCL_EVALCV_REL, "NCL evalcv", UC_CURVE, ncl_init_evalcv);

static UC_CLASS_REC UC_cot97 = 				/* 97 - NCL evalsf */
	UC_CREL_IN(NCL_EVALSF_REL, "NCL evalsf", UC_GEOMETRY, ncl_init_evalsf);

static UC_CLASS_REC UC_cot98 = 				/* 98 - NCL pntvec */
	UC_CREL_IN(NCL_POINTVEC_REL, "NCL pntvec", UC_CURVE, ncl_init_pntvec);

static UC_CLASS_REC UC_cot99 = 				/* 99 - NCL trimsf */
	UC_CREL_IN(NCL_TRIMSF_REL, "NCL trimsf", UC_SURFACE, ncl_init_trimsf);

static UC_CLASS_REC UC_cot100 = 				/* 100 - NCL surface of revolution */
	UC_CREL_IN(NCL_REVSURF_REL, "surf of revolution", UC_SURFACE, ncl_init_revsurf);

/* UNUSED 101 */
/* UNUSED 102 */
/* UNUSED 103 */
/* UNUSED 104 */
static UC_CLASS_REC UC_cot105 = 				/* 105 - NCL datast */
	UC_CREL_IN(NCL_DATAST_REL, "NCL datast", UC_GEOMETRY, ncl_init_datast);

static UC_CLASS_REC UC_cot106 = 				/* 106 - NCL text variable */
	UC_CREL_IN(NCL_TEXTVAR_REL, "NCL textvar", UC_GEOMETRY, ncl_init_textvar);

static UC_CLASS_REC UC_cot107 = 				/* 107 - NCL Custom color */
	UC_CREL_IN(NCL_COLOR_REL, "NCL Custom color", UC_SUPPORT_CLASS, uc_init_color);
/* UNUSED 108 */
/* UNUSED 109 */

/* UNUSED 110 */

static UC_CLASS_REC UC_cot111 = 				/* 111 - Unibase Statistics */
	UC_CREL_IN(UR_UNISTAT_REL, "Unibase Statistics", UC_UNIBASE_CLASS, uc_init_unistat);
/* UNUSED 112 */
/* UNUSED 113 */
/* UNUSED 114 */
/* UNUSED 115 */
/* UNUSED 116 */
/* UNUSED 117 */
/* UNUSED 118 */
/* UNUSED 119 */

/* UNUSED 120 */
/* UNUSED 121 */
/* UNUSED 122 */
/* UNUSED 123 */
/* UNUSED 124 */
/* UNUSED 125 */
/* UNUSED 126 */
/* UNUSED 127 */

static UC_CLASS_REC *UC_cot[] = 
{

/*	-- 0 - 11 are the super classes -- */

	&UC_cot_uni, &UC_cot_geo, &UC_cot_cur, &UC_cot_sur, &UC_cot_sol,
	&UC_cot_ann, &UC_cot_sup, &UC_cot_vw, &UC_cot_rel, &UC_cot_sym,
	&UC_cot_att, &UC_cot_app,

/*	-- Relations 1-9 -- */

	&UC_cot1, &UC_cot2, &UC_cot3, &UC_cot4, &UC_cot5, UU_NULL,
	&UC_cot7, &UC_cot8, UU_NULL,

/*	-- Relations 10-19 -- */

	&UC_cot10, &UC_cot11, UU_NULL, &UC_cot13, &UC_cot14,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 20-29 -- */

	&UC_cot20, &UC_cot21, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 30-39 -- */

	UU_NULL, &UC_cot31, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, &UC_cot36, &UC_cot37, &UC_cot38, UU_NULL,

/*	-- Relations 40-49 -- */

	&UC_cot40, UU_NULL, &UC_cot42, &UC_cot43, &UC_cot44,
	UU_NULL, &UC_cot46, &UC_cot47, &UC_cot48, UU_NULL,

/*	-- Relations 50-59 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	&UC_cot55, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 60-69 -- */

	UU_NULL, &UC_cot61, &UC_cot62, &UC_cot63, &UC_cot64,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 70-79 -- */

	UU_NULL, UU_NULL, UU_NULL, &UC_cot73, &UC_cot74,
	&UC_cot75, &UC_cot76, &UC_cot77, &UC_cot78, &UC_cot79,

/*	-- Relations 80-89 -- */

	&UC_cot80, &UC_cot81, &UC_cot82, &UC_cot83, &UC_cot84,
	&UC_cot85, &UC_cot86, &UC_cot87, &UC_cot88, &UC_cot89,

/*	-- Relations 90-99 -- */

	&UC_cot90, &UC_cot91, &UC_cot92, &UC_cot93, &UC_cot94,
	&UC_cot95, &UC_cot96, &UC_cot97, &UC_cot98, &UC_cot99,

/*	-- Relations 100-109 -- */

	&UC_cot100, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	&UC_cot105, &UC_cot106, &UC_cot107, UU_NULL, UU_NULL,

/*	-- Relations 110-119 -- */

	UU_NULL, &UC_cot111, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 120-129 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 130-139 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL,UU_NULL,

/*	-- Relations 140-149 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 150-159 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 160-169 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 170-179 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 180-189 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 190-199 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 200-209 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 210-219 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 220-229 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 230-239 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 240-249 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 250-256 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL
};

UC_TABLE_DESCRIPTOR UC_cot_descriptor = 
				{"UC_cot", UC_NUM_CLASS, UC_NUM_METHODS, UC_cot, UC_root,
						&UC_method_descriptor};

/*	-- Define Drafting Associativity Class Tables -- */

extern int uc_init_drf_uni();
extern int uc_init_drf_geo();
extern int uc_init_drf_crv();
extern int uc_init_drf_sur();
extern int uc_init_drf_sol();
extern int uc_init_drf_rel();
extern int uc_init_drf_sym();
extern int uc_init_drf_sym();
extern int uc_init_drf_app();
extern int uc_init_drf1();
extern int uc_init_drf2();
extern int uc_init_drf3();
extern int uc_init_drf4();
extern int uc_init_drf5();
extern int uc_init_drf7();
extern int uc_init_drf8();
extern int uc_init_drf10();
extern int uc_init_drf20();
extern int uc_init_drf31();
extern int uc_init_drf40();
extern int uc_init_drf42();
extern int uc_init_drf44();
extern int uc_init_drf62();

/*	-- Super Classes-- */

static UC_CLASS_REC UC_drf_uni = 
	UC_SUPER_IN(UC_UNIBASE_CLASS, "UNIBASE",UC_ROOT_CLASS, uc_init_drf_uni);

static UC_CLASS_REC UC_drf_geo = 
	UC_SUPER_IN(UC_GEOMETRY_CLASS, "GEOMETRY", UC_UNIBASE_CLASS,
		uc_init_drf_geo);

static UC_CLASS_REC UC_drf_cur = 
	UC_SUPER_IN(UC_CURVE_CLASS, "CURVE", UC_GEOMETRY_CLASS, uc_init_drf_crv);

static UC_CLASS_REC UC_drf_sur = 
	UC_SUPER_IN(UC_SURFACE_CLASS, "SURFACE", UC_GEOMETRY_CLASS, uc_init_drf_sur);

static UC_CLASS_REC UC_drf_sol = 
	UC_SUPER_IN(UC_SOLID_CLASS, "SOLID", UC_GEOMETRY_CLASS, uc_init_drf_sol);


/*	-- SUPPORT is undefined -- */
/*	-- VIEWING is undefined -- */

static UC_CLASS_REC UC_drf_rel = 
	UC_SUPER_IN(UC_RELATED_CLASS, "RELATED", UC_UNIBASE_CLASS, uc_init_drf_rel);

static UC_CLASS_REC UC_drf_sym = 
	UC_SUPER_IN(UC_SYMBOL_CLASS, "SYMBOL", UC_RELATED_CLASS, uc_init_drf_sym);

/*	-- ATTRIBUTE is undefined -- */

static UC_CLASS_REC UC_drf_app = 
UC_SUPER_IN(UC_APPLICATION_CLASS, "APPLICATION", UC_UNIBASE_CLASS,
	uc_init_drf_app);

/*	-- Point Class -- */

static UC_CLASS_REC UC_drf1 = 		 				/* 1 - point */
	UC_CREL_IN(UM_POINT_REL, "point", UC_GEOMETRY, uc_init_drf1);

/*	-- Curve Class -- */

static UC_CLASS_REC UC_drf2 = 		 				/* 2 - line */
	UC_CREL_IN(UM_LINE_REL, "line", UC_CURVE, uc_init_drf2);

static UC_CLASS_REC UC_drf3 = 		 				/* 3 - circle */
	UC_CREL_IN(UM_CIRCLE_REL, "circle", UC_CURVE, uc_init_drf3);

static UC_CLASS_REC UC_drf4 = 		 				/* 4 - conic curve*/
	UC_CREL_IN(UM_CONIC_REL, "conic", UC_CURVE, uc_init_drf4);

static UC_CLASS_REC UC_drf5 = 						/* 5 - composite curve */
	UC_CREL_IN(UM_COMPCRV_REL, "composite", UC_CURVE, uc_init_drf5);

/* 6 - bspline curve */

static UC_CLASS_REC UC_drf7 = 						/* 7 - rational bspline */
	UC_CREL_IN(UM_RBSPLCRV_REL, "rat bspline", UC_CURVE, uc_init_drf7);

static UC_CLASS_REC UC_drf8 = 						/* 8 - ag rational bspline */
	UC_CREL_IN(UM_AGCRV_REL, "curve", UC_CURVE, uc_init_drf8);

/* 9 - cardinal spline */

static UC_CLASS_REC UC_drf10 = 						/* 10 - ag surfaces */
	UC_CREL_IN(UM_AGSRF_REL, "surface", UC_SURFACE, uc_init_drf10);

/* 11 - */
/* 12 - */
/* 13 - */
/* 14 - */
/* 15 - */
/* 16 - */
/* 17 - */
/* 18 - */
/* 19 - */

static UC_CLASS_REC UC_drf20 = 						/* 20 - ag shell */
	UC_CREL_IN(UM_AGSHELL_REL, "shell", UC_SOLID, uc_init_drf20);

/* 21 - */
/* 22 - */
/* 23 - */
/* 24 - */
/* 25 - */
/* 26 - */
/* 27 - */
/* 28 - */

/* UNUSED 29 */
/* UNUSED 30 */

/*	-- Solids Class -- */

static UC_CLASS_REC UC_drf31 = 				/* 31 - Romulus Body	*/
	UC_CREL_IN(UM_BODY_REL, "Romulus Body", UC_SOLID, uc_init_drf31);

/* -- Miscellaneous Geometry -- */

/* 32 - features "relation" (not UNIBASE) */
/* UNUSED 33 */
/* UNUSED 34 */
/* UNUSED 35 */
/* 36 - */
/* 37 - */
/* 38 - */
/* UNUSED 39 */

static UC_CLASS_REC UC_drf40 = 				/* 40 - polyfill region	*/
	UC_CREL_IN(UM_POLY_REL, "polyfill surf", UC_SURFACE, uc_init_drf40);

/* UNUSED 41 */

static UC_CLASS_REC UC_drf42 = 		 		/* 42 - polyline */
	UC_CREL_IN(UM_POLYLINE_REL, "polyline", UC_CURVE, uc_init_drf42);

/* UNUSED 43 */

static UC_CLASS_REC UC_drf44 = 				/* 44 - group */
	UC_CREL_IN(UM_GROUP_REL, "group", UC_RELATED, uc_init_drf44);

/* UNUSED 45 */

/* 46 - */
/* 47 - */

/* -- Drafting Subsystem -- */

/* 48 - */
/* 49 - */
/* 50 - */
/* 51 - */
/* 52 - */
/* 53 - */
/* 54 - */
/* 55 - */

/* 56 - */
/* 57 - */
/* 58 - */
/* 59 - */
/* 60 - */

/* -- Symbol Subsystem -- */

/* 61 - master symbol */

static UC_CLASS_REC UC_drf62 = 		 			/* 62 - symbol instance */
	UC_CREL_IN(UB_INSTANCE_REL, "symbol instance", UC_SYMBOL, uc_init_drf62);

/* 63 - instance */
/* 64 - connector */

/* UNUSED 65 */
/* UNUSED 66 */
/* UNUSED 67 */
/* UNUSED 68 */
/* UNUSED 69 */


/* 70 */
/* 71 */
/* 72 */

/* -- Viewing attribute relations -- */

/* 73 - */
/* 74 - */
/* 75 - */

/* -- Calculator Subsystem -- */

/* 76 - */

/* -- TEXT -- */

/* 77 - text primitive */
/* 78 - text attribute */

/* UNUSED 79 */
/* UNUSED 80 */
/* UNUSED 81 */
/* UNUSED 82 */
/* UNUSED 83 */
/* UNUSED 84 */
/* UNUSED 85 */
/* UNUSED 86 */
/* UNUSED 87 */
/* UNUSED 88 */
/* UNUSED 89 */

/* UNUSED 90 */
/* UNUSED 91 */
/* UNUSED 92 */
/* UNUSED 93 */
/* UNUSED 94 */
/* UNUSED 95 */
/* UNUSED 96 */
/* UNUSED 97 */
/* UNUSED 98 */
/* UNUSED 99 */

/* UNUSED 100 */
/* UNUSED 101 */
/* UNUSED 102 */
/* UNUSED 103 */
/* UNUSED 104 */
/* UNUSED 105 */
/* UNUSED 106 */
/* UNUSED 107 */
/* UNUSED 108 */
/* UNUSED 109 */

/* UNUSED 110 */
/* UNUSED 111 */
/* UNUSED 112 */
/* UNUSED 113 */
/* UNUSED 114 */
/* UNUSED 115 */
/* UNUSED 116 */
/* UNUSED 117 */
/* UNUSED 118 */
/* UNUSED 119 */

/* UNUSED 120 */
/* UNUSED 121 */
/* UNUSED 122 */

/* 123 - */
/* 124 - */
/* 125 - */
/* 126 - */
/* 127 - */

static UC_CLASS_REC *UC_drf[] = 
{

/*	-- 0 - 11 are the super classes -- */

	&UC_drf_uni, &UC_drf_geo, &UC_drf_cur, &UC_drf_sur, &UC_drf_sol,
	UU_NULL, UU_NULL, UU_NULL, &UC_drf_rel, &UC_drf_sym,
	UU_NULL, &UC_drf_app,

/*	-- Relations 1-9 -- */

	&UC_drf1, &UC_drf2, &UC_drf3, &UC_drf4, &UC_drf5, UU_NULL,
	&UC_drf7, &UC_drf8, UU_NULL,

/*	-- Relations 10-19 -- */

	&UC_drf10, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 20-29 -- */

	&UC_drf20, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 30-39 -- */

	UU_NULL, &UC_drf31, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 40-49 -- */

	&UC_drf40, UU_NULL, &UC_drf42, UU_NULL, &UC_drf44,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 50-59 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 60-69 -- */

	UU_NULL, UU_NULL, &UC_drf62, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 70-79 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 80-89 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 90-99 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 100-109 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 110-119 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 120-129 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 130-139 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL,UU_NULL,

/*	-- Relations 140-149 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 150-159 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 160-169 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 170-179 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 180-189 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 190-199 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 200-209 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 210-219 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 220-229 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 230-239 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 240-249 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 250-256 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL
};

UC_TABLE_DESCRIPTOR UC_drf_descriptor = 
	{"UC_drafting_association", UC_NUM_CLASS, UC_NUM_METHODS, UC_drf, UC_root,
		&UC_method_descriptor};

/*	-- Define Text Along a Curve Associativity Class Tables -- */

extern int uc_init_txt_uni();
extern int uc_init_txt_geo();
extern int uc_init_txt_crv();
extern int uc_init_txt2();
extern int uc_init_txt3();

/*	-- Super Classes-- */

static UC_CLASS_REC UC_txt_uni = 
	UC_SUPER_IN(UC_UNIBASE_CLASS, "UNIBASE",UC_ROOT_CLASS, uc_init_txt_uni);

static UC_CLASS_REC UC_txt_geo = 
UC_SUPER_IN(UC_GEOMETRY_CLASS, "GEOMETRY", UC_UNIBASE_CLASS, uc_init_txt_geo);

static UC_CLASS_REC UC_txt_cur = 
	UC_SUPER_IN(UC_CURVE_CLASS, "CURVE", UC_GEOMETRY_CLASS, uc_init_txt_crv);

/*	-- SURFACE is undefined -- */
/*	-- SOLID is undefined -- */
/*	-- SUPPORT is undefined -- */
/*	-- VIEWING is undefined -- */
/*	-- RELATED is undefined -- */
/*	-- SYMBOL is undefined -- */
/*	-- ATTRIBUTE is undefined -- */
/*	-- APPLICATION is undefined -- */

/*	-- Point Class -- */

/* 1 - point is undefined */

/*	-- Curve Class -- */

static UC_CLASS_REC UC_txt2 = 		 				/* 2 - line */
	UC_CREL_IN(UM_LINE_REL, "line", UC_CURVE, uc_init_txt2);

static UC_CLASS_REC UC_txt3 = 		 				/* 3 - circle */
	UC_CREL_IN(UM_CIRCLE_REL, "circle", UC_CURVE, uc_init_txt3);

/* 4 - conic curve is undefined */
/* 5 - composite curve is undefined */
/* 6 - bspline curve is undefined */
/* 7 - rational bspline is undefined */

static UC_CLASS_REC *UC_txt[] = 
{

/*	-- 0 - 11 are the super classes -- */

	&UC_txt_uni, &UC_txt_geo, &UC_txt_cur, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL,

/*	-- Relations 1-9 -- */

	UU_NULL, &UC_txt2, &UC_txt3, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 10-19 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 20-29 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 30-39 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 40-49 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 50-59 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 60-69 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 70-79 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 80-89 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 90-99 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 100-109 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 110-119 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 120-129 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 130-139 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL,UU_NULL,

/*	-- Relations 140-149 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 150-159 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 160-169 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 170-179 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 180-189 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 190-199 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 200-209 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 210-219 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 220-229 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 230-239 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 240-249 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- Relations 250-256 -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL
};

UC_TABLE_DESCRIPTOR UC_txt_descriptor = 
	{"UC_text_association", UC_NUM_CLASS, UC_NUM_METHODS, UC_txt, UC_root,
		&UC_method_descriptor};

/*	-- Define the 3-D Dispatch Table. The third dimension is the relation
		number of the associate.  Entry 0 is the "user" associative class.
		Entry 1 to 127 are the relation numbers of the association
		entity. -- */

UC_TABLE_DESCRIPTOR *UC_3d_descriptor[] = 
{

/*	-- 0-9 Pointers -- */

	&UC_cot_descriptor, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- 10-19 Pointers -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- 20-29 Pointers -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- 30-39 Pointers -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- 40-49 Pointers -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, &UC_drf_descriptor, UU_NULL,

/*	-- 50-59 Pointers -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- 60-69 Pointers -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- 70-79 Pointers -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, &UC_txt_descriptor, UU_NULL, UU_NULL,

/*	-- 80-89 Pointers -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- 90-99 Pointers -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- 100-109 Pointers -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- 110-119 Pointers -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,

/*	-- 120-127 Pointers -- */

	UU_NULL, UU_NULL, UU_NULL, UU_NULL, UU_NULL,
	UU_NULL, UU_NULL, UU_NULL
};
