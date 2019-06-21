/*********************************************************************
**    NAME         :  openvx_mdl.h
**       CONTAINS: VX structures.
**    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**        openvx_mdl.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**        04/29/15 , 15:06:40
*********************************************************************/
/* ID:obi * DATE::970514091137 * PROJECT: OPENVX fixes */
/* ID:obi * DATE::970402140833 * PROJECT: Major OVX Enhancements */
/* ID:obi * DATE::961213103959 * PROJECT: OVX fixes and enhancements */
/* ID:obi * DATE::960829161659 * PROJECT: OPENVX Fixes */
/* ID:obi * DATE::960502171854 * PROJECT: Openvx fixes/enhancements */
/* ID:obi * DATE::960401175207 * PROJECT: Fixed srf,crv type mapping */
/* ID:obi * DATE::960219102839 * PROJECT: OPENVX enhancements */
/* ID:geg * DATE::951211140850 * PROJECT: New NURB mem flag */
/* ID:obi * DATE::951024093843 * PROJECT: Added new funcs to openvx */
/* ID:obi * DATE::950926105308 * PROJECT: OPENVX Fixes */
/* ID:obi * DATE::950811173728 * PROJECT: New openvx func vx_mdl_inq_solid */
/* ID:obi * DATE::950608125247 * PROJECT: OPENVX fixes and enhancements */
/* ID:obi * DATE::950608123335 * PROJECT: Added create/mod/inq rec funcs */
/* ID:obi * DATE::950601150510 * PROJECT: OPENVX Fixes to line/point types */
/* ID:obi * DATE::950522160608 * PROJECT: OPENVX fixes and enhancements */
/* ID:obi * DATE::950428082525 * PROJECT: Fixes to OPENVX */
/* ID:obi * DATE::950405172850 * PROJECT: Fixed line type mapping to VX */
/* ID:obi * DATE::950105181954 * PROJECT: Add grid surface entity */
/*		Copyright Varimetrix Corporation
*/

#ifndef __openvx_mdl_h
#define __openvx_mdl_h
#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include <stdio.h>
#include <math.h>
#include <malloc.h>
#include <memory.h>
#include <string.h>
#include "openvx_tim.h"

extern int TASK_NAME;
extern int TASK_FROM;
extern int MDL_REGEN_FLAG;
extern int MDL_PEDIT_FLAG;

/********** define ************************************************************/

#define DEL_INDEX 		0
#define DEL_PICK_ID 		1

/* file types */
#define FILE_TYPE_MDL	1
#define FILE_TYPE_ASM	2
#define FILE_TYPE_DWG	3
#define FILE_TYPE_MAC	4
#define FILE_TYPE_ACL	6
#define FILE_TYPE_TL		7
#define FILE_TYPE_OP		8
#define FILE_TYPE_PRM	9
#define FILE_TYPE_CSV	10
#define FILE_TYPE_IGS	100
#define FILE_TYPE_DXF	101
#define FILE_TYPE_EPS	102
#define FILE_TYPE_PGL	103
#define FILE_TYPE_STL	107
#define FILE_TYPE_FUN	200

/* line types  */
#define D_LIN_SOLID       	0
#define D_LIN_DOT         	1
#define D_LIN_DASH        	2
#define D_LIN_DASHDOT     	3
#define D_LIN_DASHDOTDOT	4
#define D_LIN_LONGDASH		5

#define MDL_NAME_LEN		32
#define MDL_DATA_LEN		20
#define MDL_PATH_LEN 	512

#define NULL 				0

/* point marker types */
#define D_PNT_POINT       0
#define D_PNT_PLUS        1
#define D_PNT_STAR        2
#define D_PNT_O           3
#define D_PNT_X           4

/********** enum **************************************************************/

typedef enum _mdl_struct_type
	{
	MDL_UNKNOWN,
	MDL_ADD_CV_ISO,
	MDL_ADD_CV_NURB,
	MDL_ADD_CV_OFF,
	MDL_ADD_CV_PRJ,
	MDL_ADD_CV_SPIRAL,
	MDL_APP_DAT,
	MDL_BLOCK,
	MDL_CIRCLE,
	MDL_CONE,
	MDL_CONIC,
	MDL_CRV_AT,
	MDL_CTRLPNT_DAT,
	MDL_CYLINDER,
	MDL_DATA,
	MDL_DIM_AT,
	MDL_DWG_DATA,
	MDL_ELLIPSE,
	MDL_ELLIPSOID,
	MDL_ENT_LOC,
	MDL_EPICK,
	MDL_ETY_AT,
	MDL_ETY_DATA,
	MDL_EVAL_CRV,
	MDL_EVAL_SRF,
	MDL_EVAL_UVCV,
	MDL_EXTRUSION,
	MDL_FIL_EDG,
	MDL_FIL_EDG_LIST,
	MDL_FILLET,
	MDL_FRUSTRUM,
	MDL_GEOM,
	MDL_INFI_LN,
	MDL_INFI_PLN,
	MDL_INP_ENT,
	MDL_INPUT_DATA,
	MDL_INT_CRV,
	MDL_INT_ENT,
	MDL_INT_LIST,
	MDL_INT_PNT,
	MDL_LIM1,
	MDL_LIM3,
	MDL_LINE,
	MDL_LIST,
	MDL_LN_BUNDLE,
	MDL_LOC3D,
	MDL_LOFT,
	MDL_LOOP,
	MDL_MESH,
	MDL_MOVE,
	MDL_NRB_CRV,
	MDL_NRB_SRF,
	MDL_OFFSET,
	MDL_PARM,
	MDL_PARM_DAT,
	MDL_PLANE,
	MDL_PLN_AT,
	MDL_PLN_REL_ENT,
	MDL_PNT,
	MDL_PNT_AT,
	MDL_PNT_BUNDLE,
	MDL_PNT_LIST,
	MDL_PROFILE,
	MDL_PROP,
	MDL_REVOLUTION,
	MDL_RULED,
	MDL_SPHERE,
	MDL_SRF_AT,
	MDL_SRF_CRV,
	MDL_STL_DATA,
	MDL_SWEEP,
	MDL_TORUS,
	MDL_TUBE,
	MDL_TXT_AT,
	MDL_UV_PNT,
	MDL_WEDGE,
	MDL_WIRE_DFLT
	} mdl_struct_type;

typedef enum _mdl_lin_unit
	{
	MDL_MIC,
	MDL_MM,
	MDL_CM,
	MDL_M,
	MDL_KM,
	MDL_MILS,
	MDL_IN,
	MDL_FT,
	MDL_YD,
	MDL_MI
	} mdl_lin_unit;

typedef enum _mdl_ang_unit
	{
	MDL_RAD,
	MDL_DEG
	} mdl_ang_unit;

typedef enum _mdl_data_type
	{
	MDL_DATA_UNKNOWN,
	MDL_DATA_T_PARAM,
	MDL_DATA_UV_PNT,
	MDL_DATA_3D_PNT,
	MDL_DATA_BND_BOX,
	MDL_DATA_UV_CRV,
	MDL_DATA_NRB_CRV,
	MDL_DATA_INT_PNT,
	MDL_DATA_INT_CRV,
	MDL_DATA_SRF_CRV,
	MDL_DATA_NRB_SRF,
	MDL_DATA_EVAL_UVCV,
	MDL_DATA_EVAL_CRV,
	MDL_DATA_EVAL_SRF,
	MDL_DATA_PLANE,
	MDL_DATA_SPHERE,
	MDL_DATA_CYLINDER,
	MDL_DATA_CONE,
	MDL_DATA_TORUS,
	MDL_DATA_ELLIPSOID,
	MDL_DATA_EXTRUSION,
	MDL_DATA_SRF_REV,
	MDL_DATA_LINE,
	MDL_DATA_CIRCLE,
	MDL_DATA_ELLIPSE,
	MDL_DATA_CONIC,
	MDL_DATA_INDEX,
	MDL_DATA_TEXT
	} mdl_data_type;

typedef enum _mdl_prop_type
	{
	MDL_RADIUS,
	MDL_CENTER,
	MDL_CENTROID,
	MDL_LENGTH,
	MDL_PERIMETER,
	MDL_AREA,
	MDL_VOLUME,
	MDL_MASS,
	MDL_MOMENTS,
	MDL_RAD_GYR,
	MDL_GBL_MOMENTS
	} mdl_prop_type;

typedef enum _mdl_trim_opt
	{
	MDL_UNTRIMMED,
	MDL_TRIMMED
	} mdl_trim_opt;

typedef enum _mdl_ety_type
	{
	MDL_ETY_ASSOC,
	MDL_ETY_ATEXT,
	MDL_ETY_CIR3,
	MDL_ETY_CONE,
	MDL_ETY_CONIC3,
	MDL_ETY_CURV,
	MDL_ETY_CURVSET,
	MDL_ETY_CYLINDER,
	MDL_ETY_DIM,
	MDL_ETY_EDGE,
	MDL_ETY_ELL3,
	MDL_ETY_ELLIPSOID,
	MDL_ETY_EXTRUSION,
	MDL_ETY_FCURV,
	MDL_ETY_FNCURV,
	MDL_ETY_FRAME,
	MDL_ETY_LAYER,
	MDL_ETY_LIGHT,
	MDL_ETY_LINE3,
	MDL_ETY_LOOP,
	MDL_ETY_PARAM,
	MDL_ETY_PLANE,
	MDL_ETY_PLINE3,
	MDL_ETY_POINT,
	MDL_ETY_SPHERE,
	MDL_ETY_SURF,
	MDL_ETY_SURFREV,
	MDL_ETY_SURFSET,
	MDL_ETY_TEXT3,
	MDL_ETY_TORUS,
	MDL_ETY_UDATA,
	MDL_ETY_UNKNOWN,
	MDL_ETY_UVCURV,
	MDL_ETY_VARIABLE,
	MDL_ETY_VIEW
	} mdl_ety_type;

typedef enum _mdl_ety_vis
	{
	MDL_INVISIBLE,
	MDL_VISIBLE,
	MDL_NOT_APPLICABLE
	} mdl_ety_vis;

typedef enum _mdl_pnt_class
	{
	MDL_INSIDE,
	MDL_OUTSIDE,
	MDL_BOUNDARY
	} mdl_pnt_class;

typedef enum _mdl_pnt_class_method
	{
	MDL_PTLOC_LIMITS,
	MDL_PTLOC_PARM,
	MDL_PTLOC_BNDRY
	} mdl_pnt_class_method;

typedef enum _mdl_int_class
	{
	MDL_INTERSECT,
	MDL_NO_INTERSECT,
	MDL_TOUCH
	} mdl_int_class;

typedef enum _mdl_int_type
   {
   MDL_ISECT_CURV_CURV,
   MDL_ISECT_CURV_SURF,
   MDL_ISECT_CURV_UVSF,
   MDL_ISECT_SURF_SURF,
   MDL_ISECT_SURF_UVSF,
   MDL_ISECT_UVSF_UVSF,
   MDL_ISECT_UVCV_UVCV
   } mdl_int_type;

typedef enum _mdl_response
	{
	MDL_NO,
	MDL_YES,
	MDL_NULL
	} mdl_response;

typedef enum _mdl_sset_method
	{
	MDL_SSET_SRF,
	MDL_SSET_SPHERE,
	MDL_SSET_HEMIS,
	MDL_SSET_CONE,
	MDL_SSET_CYLINDER,
	MDL_SSET_TORUS,
	MDL_SSET_BLOCK,
	MDL_SSET_WEDGE,
	MDL_SSET_ELLIPSOID,
	MDL_SSET_FRUSTRUM,
	MDL_SSET_EXTRUDE,
	MDL_SSET_REVOLVE,
	MDL_SSET_SWEEP,
	MDL_SSET_OFFSET,
	MDL_SSET_LOFT,
	MDL_SSET_TUBE
	} mdl_sset_method;

typedef enum _mdl_srf_method
	{
	MDL_SRF_PLANE,
	MDL_SRF_SPHERE,
	MDL_SRF_CONE,
	MDL_SRF_GRID,
	MDL_SRF_CYLINDER,
	MDL_SRF_EXTRUDE,
	MDL_SRF_REVOLVE,
	MDL_SRF_SWEEP,
	MDL_SRF_NURB,
	MDL_SRF_OFFSET,
	MDL_SRF_MESH,
	MDL_SRF_RULED,
	MDL_SRF_HEMIS,
	MDL_SRF_FRUSTRUM,
	MDL_SRF_TORUS,
	MDL_SRF_ELLIPSOID,
	MDL_SRF_LOFT,
	MDL_SRF_TUBE,
	MDL_SRF_PATCH,
	MDL_SRF_POLYGON,
	MDL_SRF_TRIM_PLANE,
	MDL_SRF_CORNER_FILLET,
	MDL_SRF_LIN_FILLET,
	MDL_SRF_CIR_FILLET,
	MDL_SRF_CON_FILLET,
	MDL_SRF_ROLL_FILLET,
	MDL_SRF_BLEND
	} mdl_srf_method;

typedef enum _mdl_ftr_method
	{
	MDL_FTR_FILLET
	} mdl_ftr_method;

typedef enum _mdl_crv_method
	{
	MDL_CRV_DEGEN,
	MDL_CRV_LINE,
	MDL_ARC_CSA,
	MDL_CRV_ELLIPSE,
	MDL_CRV_PTS,
	MDL_CON_ELL,
	MDL_CRV_CPTS,
	MDL_CON_HYP,
	MDL_CON_PAR,
	MDL_CRV_CONCAT,
	MDL_CRV_ISO,
	MDL_CRV_CIRCLE,
	MDL_CRV_COMPOSITE,
	MDL_CRV_NRB_CRV,
	MDL_CRV_OFFSET,
	MDL_ARC_CSE,
	MDL_CRV_FILLET,
	MDL_CRV_BLEND,
	MDL_CRV_CHAMFER,
	MDL_CRV_RAT_CLUB,
	MDL_CIR_3D,
	MDL_ARC_3D,
	MDL_CIR_2D,
	MDL_ARC_2D,
	MDL_CRV_PLINE,
	MDL_CRV_CONIC,
	MDL_LIN_TAN_PC,
	MDL_LIN_TAN_CC,
	MDL_ARC_3PT,
	MDL_ARC_2A,
	MDL_CIR_3PT,
	MDL_CIR_2PT,
	MDL_CIR_CTR_RAD,
	MDL_CIR_CTR_BND,
	MDL_CIR_TAN_3LIN,
	MDL_CIR_TAN_PC,
	MDL_CIR_TAN_CC,
	MDL_PLY_REG,
	MDL_CON_TAN_PNT,
	MDL_CON_SHLD_PNT,
	MDL_CON_4PNT,
	MDL_CRV_PTS_TAN,
	MDL_CRV_PROJ,
	MDL_CRV_SPIRAL,
	MDL_CRV_EDGE,
	MDL_CRV_SS_INT,
	MDL_CRV_UV
	} mdl_crv_method;

typedef enum _mdl_pnt_method
	{
	MDL_ABSOLUTE,
	MDL_RELATIVE,
	MDL_END_PNT,
	MDL_REF_PNT,
	MDL_ON_CURVE,
	MDL_MID_CURVE,
	MDL_DIST_CURVE,
	MDL_FRAC_CURVE,
	MDL_CURVE_CENTER,
	MDL_ON_SURFACE,
	MDL_SURFACE_CENTER,
	MDL_CRV_CRV_ISECT,
	MDL_CRV_SRF_ISECT
	} mdl_pnt_method;

typedef enum _mdl_pln_method
	{
	MDL_3_PNT,
	MDL_PERPENDICULAR,
	MDL_PLN_OFFSET,
	MDL_ANGLE,
	MDL_THRU_PNT,
	MDL_TAN_SURFACE,
	MDL_PNT_DIR,
	MDL_NORMAL_CURVE,
	MDL_NORMAL_X,
	MDL_NORMAL_Y,
	MDL_NORMAL_Z,
	MDL_WORLD
	} mdl_pln_method;

typedef enum _mdl_cset_method
	{
	MDL_CSET_PROFILE
	} mdl_cset_method;

typedef enum _mdl_bool_method
	{
	MDL_BOOL_ADD,
	MDL_BOOL_SUBTRACT,
	MDL_BOOL_INTERSECT
	} mdl_bool_method;

typedef enum _mdl_move_method
	{
	MDL_ALIGN_3PNT,
	MDL_PNT_TO_PNT,
	MDL_ALONG_CRV,
	MDL_TRANSLATE,
	MDL_ROTATE,
	MDL_MIRROR,
	MDL_SCALE
	} mdl_move_method;

typedef enum _mdl_move_modal
	{
	MDL_MOVE_MODE,
	MDL_COPY_MODE
	} mdl_move_modal;

typedef enum _mdl_del_method
	{
	MDL_ETY,
	MDL_ETY_LIST,
	MDL_ASSOC
	} mdl_del_method;

typedef enum _mdl_frame_method
	{
	MDL_FRM_3PNT,
	MDL_FRM_PNT_2DIR,
	MDL_FRM_PNT_XDIR,
	MDL_FRM_PNT_ZDIR
	} mdl_frame_method;

typedef enum _mdl_app_data
	{
	MDL_STRING,
	MDL_DOUBLE,
	MDL_INTEGER
	} mdl_app_data;

typedef enum _mdl_fct_opt
	{
	MDL_VTX,
	MDL_VTX_NORM
	} mdl_fct_opt;

typedef enum _mdl_default
	{
	MDL_DEF_LIN_UNIT,
	MDL_DEF_ANG_UNIT,
	MDL_DEF_RESOLUTION,
	MDL_DEF_LAYER,
	MDL_DEF_SRF_AT,
	MDL_DEF_SOL_AT,
	MDL_DEF_CRV_AT,
	MDL_DEF_PNT_AT,
	MDL_DEF_ATXT_AT,
	MDL_DEF_TXT3_AT,
	MDL_DEF_DIM_AT,
	MDL_DEF_PLN_AT,
	MDL_DEF_WIRE_DFLT,
	MDL_DEF_NUM_ISO,
	MDL_DEF_MODEL_NAME,
	MDL_DEF_SSI_METHOD
	} mdl_default;

typedef enum _mdl_ln_type
	{
	MDL_LIN_SOLID = 0,
	MDL_LIN_DOT = 1,
	MDL_LIN_DASH = 2,
	MDL_LIN_DASHDOT = 3,
	MDL_LIN_DASHDOTDOT = 4,
	MDL_LIN_LONGDASH = 5
	} mdl_ln_type;

typedef enum _mdl_pnt_type
	{
	MDL_PNT_POINT = 0,
	MDL_PNT_PLUS = 1,
	MDL_PNT_STAR = 2,
	MDL_PNT_O = 3,
	MDL_PNT_X = 4
	} mdl_pnt_type;

typedef enum _mdl_dsp_mode
	{
	MDL_DSP_WIRE,
	MDL_DSP_SHADE,
	MDL_DSP_HIDE
	} mdl_dsp_mode;

typedef enum _mdl_fct_res
	{
	MDL_FCT_FINE = 1,
	MDL_FCT_MEDIUM = 2,
	MDL_FCT_COARSE = 3
	} mdl_fct_res;

typedef enum _mdl_txt_halign
	{
	MDL_H_NORMAL,
	MDL_H_LEFT,
	MDL_H_CENTER,
	MDL_H_RIGHT
	} mdl_txt_halign;

typedef enum _mdl_txt_valign
	{
	MDL_V_NORMAL,
	MDL_V_TOP,
	MDL_V_CAP,
	MDL_V_HALF,
	MDL_V_BASE,
	MDL_V_BOTTOM
	} mdl_txt_valign;

typedef enum _mdl_txt_path
	{
	MDL_RIGHT,
	MDL_LEFT,
	MDL_UP,
	MDL_DOWN
	} mdl_txt_path;

typedef enum _mdl_txt_prec
	{
	MDL_TXT_STRING,
	MDL_TXT_CHAR,
	MDL_TXT_STROKE
	} mdl_txt_prec;

typedef enum _mdl_txt_leader
	{
	MDL_LEADER,
	MDL_NO_LEADER
	} mdl_txt_leader;

typedef enum _mdl_line_len
	{
	MDL_FIXED,
	MDL_LONG,
	MDL_VARIABLE
	} mdl_line_len;

typedef enum _mdl_attr_type
	{
	MDL_ETY_NAME,
	MDL_LAYER_NAME,
	MDL_LAYER_ID,
	MDL_BLANK_STATUS,
	MDL_DSP_AT,
	MDL_ALL_ATTR
	} mdl_attr_type;

typedef enum _mdl_geom_type
	{
	MDL_GT_POINT,
	MDL_GT_LINE,
	MDL_GT_PLANE,
	MDL_GT_BOX
	} mdl_geom_type;

typedef enum _mdl_xform_type
	{
	MDL_LOC_TO_WLD,
	MDL_WLD_TO_LOC
	} mdl_xform_type;

typedef enum _mdl_merge_opt
	{
	MDL_STATIC,
	MDL_HISTORY
	} mdl_merge_opt;

typedef enum _mdl_file_type
	{
	MDL_MODEL=1,
	MDL_ASSEMBLY=2,
	MDL_DRAWING=3,
	MDL_MACRO=4,
	MDL_ACL=6,
	MDL_TL=7,
	MDL_OP=8,
	MDL_IGES=100,
	MDL_DXF=101,
	MDL_EPS=102,
	MDL_PGL=103,
	MDL_STL=107
	} mdl_file_type;

typedef enum _mdl_rec_type
	{
	MDL_SURFSET_REC,
	MDL_CURVSET_REC,
	MDL_SURF_REC,
	MDL_LOOP_REC,
	MDL_EDGE_REC,
	MDL_UVCURV_REC,
	MDL_FCURV_REC,
	MDL_FRAME_REC,
	MDL_DATA_REC,
	MDL_PARAM_REC,
	MDL_RPOINT_REC,
	MDL_LAYER_REC,
	MDL_GEN_AT_REC,
	MDL_ASSOC_REC,
	MDL_ATEXT_REC,
	MDL_TEXT3_REC,
	MDL_FNCURV_REC,
	MDL_NDATA_REC
	} mdl_rec_type;

/********** misc **************************************************************/

typedef char mdl_name[MDL_NAME_LEN+1];	/* name */

typedef char mdl_path[MDL_PATH_LEN+1];	/* path */

typedef double mdl_matrix[4][4];			/* matrix */


/********** struct ************************************************************/

typedef struct _mdl_list
	{
	int num;						/* number of entities on list */
	int *list;					/* list of entity ids */
	} mdl_list;

typedef struct _mdl_uv_pnt
	{
	double u;					/* u value */
	double v;					/* v value */
	} mdl_uv_pnt;

typedef struct _mdl_pnt
	{
	double x;					/* x */
	double y;					/* y */
	double z;					/* z */
	} mdl_pnt;

typedef struct _mdl_pnt_list
	{
	int num;						/* number of points */
	mdl_pnt *list;				/* list of points */
	} mdl_pnt_list;

typedef struct _mdl_lim1
	{
	double min;					/* minimum value */
	double max;					/* maximum value */
	} mdl_lim1;

typedef struct _mdl_lim3
	{
	mdl_lim1 x;					/* x limits */
	mdl_lim1 y;					/* y limits */
	mdl_lim1 z;					/* z limits */
	} mdl_lim3;

typedef struct _mdl_eval_uvcv
	{
	int level;					/* level of evaluation								*/
	mdl_uv_pnt	pnt;			/* level 0 - x,y,z (or u,v)						*/
	mdl_uv_pnt	deriv_1;		/* level 1 - x',y' (or u',v')						*/
	mdl_uv_pnt	deriv_2;		/* level 2 - x'',y'' (or u'',v'')	 			*/
	mdl_uv_pnt	deriv_3;		/* level 3 - x''',y''',z''' (or u''',v''') 	*/
	} mdl_eval_uvcv;

typedef struct _mdl_eval_crv
	{
	int level;					/* level of evaluation								*/
	mdl_pnt	pnt;				/* level 0 - x,y,z (or u,v)						*/
	mdl_pnt	deriv_1;			/* level 1 - x',y' (or u',v')						*/
	mdl_pnt	deriv_2;			/* level 2 - x'',y'' (or u'',v'')	 			*/
	mdl_pnt	deriv_3;			/* level 3 - x''',y''',z''' (or u''',v''') 	*/
	} mdl_eval_crv;

typedef struct _mdl_eval_srf
	{
	int level;					/* level of evaluation 								*/
	mdl_pnt	pnt;				/* level 0 - x,y,z on surface 					*/
	mdl_pnt	normal;			/* level 1 - unit normal vector					*/
	mdl_pnt	partial_u;		/* level 1 - tangential component w/r u		*/
	mdl_pnt 	partial_v;		/* level 1 - tangential component w/r v		*/
	mdl_pnt	partial_uu;		/* level 2 - 2nd partial w/r u					*/
	mdl_pnt	partial_uv;		/* level 2 - 2nd partial w/r uv					*/
	mdl_pnt	partial_vv;		/* level 2 - 2nd partial w/r v					*/
	} mdl_eval_srf;

typedef struct _mdl_ety_data
	{
	int type;									/* entity type */
	int index;									/* entity index */
	int num_str;								/* number of string data elements */
	char str_data[MDL_DATA_LEN][80];		/* entity string data */
	int num_dbl;								/* number of double data elements */
	double dbl_data[MDL_DATA_LEN];		/* entity double data */
	} mdl_ety_data;

typedef struct _mdl_parm_dat
	{
	int closed;          	/* TRUE-closed curve, FALSE-open curve */
	int degree;          	/* degree (order = deg + 1) */
	int num_kt;          	/* number of knots */
	mdl_lim1 bnd;         	/* boundaries of parameter space */
	double *knots;       	/* num_kt knot values */
	} mdl_parm_dat;

typedef struct _mdl_ctrlpnt_dat
	{
	int rat;             	/* Always set to TRUE (==1) */
	int dim;             	/* number of coordinates per control point (3-4) */
	int plane;           	/* cp hyper plane type: 1:pnt, 2:line, 3:plane  */
	int num_cp;          	/* number of control points in list */
	mdl_lim3 box;         	/* bounding box of control points */
	double *list;        	/* num_cp weighted points of form (wx,wy,wz,w) */
	} mdl_ctrlpnt_dat;

typedef struct _mdl_app_dat
	{
	int type;           		/* Type of data stored in data. */
	void *data;         		/* Optional application specific data. */
	} mdl_app_dat;

typedef struct _mdl_nrb_crv
	{
	int type;            	/* curve type */
	int mem;            	   /* mem lock flag, currently not used (= 0). */
	double offset;          /* curve offset, currently not used (= 0.0). */
	mdl_parm_dat t;       	/* parameter space data for curve */
	mdl_ctrlpnt_dat cp;   	/* control point data for curve */
	mdl_app_dat app;      	/* optional struct for attaching application data */
	} mdl_nrb_crv;

typedef struct _mdl_loop
	{
	int num_cv;            	/* number of mdl_nurb_curv structs in list_cv */
	int nxt_in[2];				/* not used */
	mdl_nrb_crv *list_cv;  	/* adjacent curves which form single loop */
	} mdl_loop;

typedef struct _mdl_nrb_srf
	{
	int type;       			/* surface type */
	int mem;            	   /* mem lock flag, currently not used (= 0). */
	int out_norm;           /* 1-evaluated normal outward pointing
										0-evaluated normal inward pointing */
	double offset;          /* surface offset, currently not used (= 0.0). */
	mdl_parm_dat u;         /* u parameter space data */
	mdl_parm_dat v;         /* v parameter space data */
	mdl_ctrlpnt_dat cp;     /* control point data for surface */
	int num_loop;           /* number of loop structs in list_loop */
									/* num_loop == 0 implies full surface */
	mdl_loop *list_loop;  	/* surface boundary loops */
									/* list_loop[0] is outer loop */
	mdl_app_dat app;     	/* optional struct for attaching application data */
	} mdl_nrb_srf;

typedef union _mdl_parm
	{
	double			t;			/* parameter */
	mdl_uv_pnt		uv;		/* uv point */
	} mdl_parm;

/* 3D curve defined by uv-curve on a surface. */
typedef struct _mdl_srf_crv
	{
	int srf_idx;         	/* surface index, or -1 if not defined */
	mdl_nrb_srf srf;	   	/* NURBS definition if srf_idx == -1 */
	int uvcv_idx;        	/* surface index, or -1 if not defined */
	mdl_nrb_crv uvcv;     	/* uv NURBS definition if uvcv_idx == -1 */
	} mdl_srf_crv;

typedef struct _mdl_int_pnt
	{
	mdl_parm parm[2];    	/* parameter value at point of intersection */
	int geom[2];         	/* database indicies of intersected geometry */
	mdl_pnt pnt;      		/* world space coordinates of intersection point */
	} mdl_int_pnt;

typedef struct _mdl_int_crv
	{
	int srf[2];             /* surface index, or -1 if not defined */
	mdl_nrb_crv uvcv[2];    /* uv NURBS definition */
	mdl_nrb_crv crv;	      /* NURBS definition in world space */
	} mdl_int_crv;

typedef union _mdl_int_ent
	{
	mdl_int_pnt pt;         /* intersection results in a point. */
	mdl_int_crv cv;         /* intersection results in a curve. */
	} mdl_int_ent;

typedef struct _mdl_int_list
	{
	int type;            	/* type of intersection returned. */
	int num;             	/* number of intersections in list */
	mdl_int_ent *list;   	/* list of intersections          */
	} mdl_int_list;

typedef struct _mdl_ent_loc
	{
	int type;          		/* type of entity referenced */
	int index;         		/* index to geometry.   */
	mdl_parm parm;     		/* location parameter on geometry. */
	mdl_pnt pnt;       		/* model space point.   */
	} mdl_ent_loc;

typedef struct _mdl_profile
	{
	mdl_list	crv;				/* list of curves */
	mdl_matrix xform;			/* transformation matrix */
	} mdl_profile;

typedef struct _mdl_fil_edg
	{
	int edg_idx;				/* edge index */
	double radius_1;			/* start radius */
	double radius_2;			/* end radius */
	int func_crv;				/* optional function curve index */
	} mdl_fil_edg;

typedef struct _mdl_fil_edg_list
	{
	int num;						/* number of edges on list */
	mdl_fil_edg *list;		/* list of edge with fillet spec. */
	} mdl_fil_edg_list;

typedef struct _mdl_offset
	{
	int ety_idx;				/* reference entity */
	double offset;				/* offset distance */
	} mdl_offset;

typedef struct _mdl_loft
	{
	int loft_one;				/* NOT YET IMPLEMENTED */
	} mdl_loft;

typedef struct _mdl_tube
	{
	int drive_crv;				/* drive curve index*/
	double radius;				/* radius */
	int func_crv;				/* optional function curve index */
	} mdl_tube;

typedef struct _mdl_infi_pln
	{
	mdl_pnt	pnt;				/* point on infinite plane */
	mdl_pnt	norm;				/* unit normal vector of plane */
	} mdl_infi_pln;

typedef struct _mdl_pln_rel_ent
	{
	int ety;						/* reference entity */
                     		/* or flag: 1=use xform, 0=use active frame */
	mdl_matrix xform; 		/* base frame */
	mdl_pnt	pt[3];			/* axis pnts & dir vecs */
	double off;       		/* relative offset */
	} mdl_pln_rel_ent;

typedef struct _mdl_infi_ln
	{
	mdl_pnt pnt;				/* point on infinite line */
	mdl_pnt dir;				/* unit direction vector of inifinite line */
	} mdl_infi_ln;

typedef struct _mdl_plane
	{
	double x_dim;				/* x dimension */
	double y_dim;				/* y dimension */
	mdl_matrix xform;			/* transformation matrix */
	} mdl_plane;

typedef struct _mdl_sphere
	{
	double radius;				/* sphere radius */
	mdl_matrix xform;			/* transformation matrix */
	} mdl_sphere;

typedef struct _mdl_cylinder
	{
	double radius;				/* cylinder radius */
	double length;				/* cylinder length */
	mdl_matrix xform;			/* transformation matrix */
	} mdl_cylinder;

typedef struct _mdl_cone
	{
	double radius;				/* cone radius */
	double length;				/* cone length */
	mdl_matrix xform;			/* trandformation matrix */
	} mdl_cone;

typedef struct _mdl_frustrum
	{
	double radius_1;			/* first radius */
	double radius_2;			/* second radius */
	double length;				/* length */
	mdl_matrix xform;			/* transformation matrix */
	} mdl_frustrum;

typedef struct _mdl_torus
	{
	double inner_radius;		/* inner radius */
	double outer_radius;		/* outer radius */
	mdl_matrix xform;			/* transformation matrix */
	} mdl_torus;

typedef struct _mdl_ellipsoid
	{
	double x_dim;				/* x dimension */
	double y_dim;				/* y dimension */
	double z_dim;				/* z dimension */
	mdl_matrix xform;			/* transformation matrix */
	} mdl_ellipsoid;

typedef struct _mdl_extrusion
	{
	int shape_crv;				/* index of shape curve/curvset */
	mdl_pnt dir;				/* extrusion direction components */
	double length;				/* length of extrusion */
	double taper_ang;			/* taper angle */
	} mdl_extrusion;

typedef struct _mdl_revolution
	{
	int shape_crv;				/* index of shape curve/curvset */
	mdl_pnt pnt;				/* axis ref. point */
	mdl_pnt dir;				/* axis direction components */
	double s_ang;				/* start angle (radians) */
	double e_ang;				/* end angle (radians) */
	} mdl_revolution;

typedef struct _mdl_sweep
	{
	int shape_crv;				/* index of shape curve/curvset */
	int drive_crv;				/* index of drive curve */
	} mdl_sweep;

typedef struct _mdl_ruled
	{
	int crv_1;					/* index of first boundary curve */
	int crv_2;					/* index of second boundary curve */
	} mdl_ruled;

typedef struct _mdl_mesh
	{
	mdl_list	crv_1;			/* first set of curves */
	mdl_list crv_2;			/* second set of curves */
	} mdl_mesh;

typedef struct _mdl_fillet
	{
	int bndry_ent[2];			/* index of boundary entities */
	int defn_ent[2];			/* index of definition entities */
	mdl_parm parm[2];    	/* parameter value at point of intersection */
	int drive_crv;				/* index of drive curve	*/
	double radius;				/* radius */
	double ratio;				/* conic ratio */
	} mdl_fillet;

typedef struct _mdl_block
	{
	double x_dim;				/* x dimension */
	double y_dim;				/* y dimension */
	double z_dim;				/* z dimension */
	mdl_matrix xform;			/* transformation matrix of block */
	} mdl_block;

typedef struct _mdl_wedge
	{
	double x_dim;				/* x dimension */
	double y_dim;				/* y dimension */
	double z_dim_1;			/* first z dimension */
	double z_dim_2;			/* second z dimension */
	mdl_matrix xform;			/* transformation matrix */
	} mdl_wedge;

/*
**  Wireframe structures.
*/

typedef struct _mdl_line
	{
	mdl_pnt p1;					/* start point */
	mdl_pnt p2;					/* end point */
	} mdl_line;

typedef struct _mdl_circle
	{
	mdl_pnt c_pnt;				/* center point */
	mdl_pnt s_pnt;				/* start point	*/
	double angle;				/* angle (radians) */
	mdl_pnt norm;				/* normal vector components */
	} mdl_circle;

typedef struct _mdl_ellipse
	{
	double maj_axis;			/* length of major axis */
	double min_axis;			/* length of minor axis */
	mdl_pnt c_pnt;				/* center point */
	mdl_pnt norm;				/* components of normal direction */
	mdl_pnt major_axis;		/* direction components of major axis */
	} mdl_ellipse;

typedef struct _mdl_conic
	{
	mdl_pnt s_pnt;				/* start point */
	mdl_pnt e_pnt;				/* end point */
	mdl_pnt t_pt;				/* tangent line intersection point */
	double ratio;				/* conic ratio */
	} mdl_conic;

typedef struct _mdl_add_cv_nurb
	{
	int deg;                /* degree */
	mdl_pnt_list pnt_lst;   /* points to interpolate/approx */
   mdl_pnt tan1;           /* tan vec at curve beg */
   mdl_pnt tan2;           /* tan vec at curve end */
	} mdl_add_cv_nurb;

typedef struct _mdl_add_cv_off
	{
	double dist;           	/* offset distance  */
	int crv_idx;           	/* curve index */
	mdl_pnt pln_pt;        	/* point to determine plane if curve is line */
	} mdl_add_cv_off;

typedef struct _mdl_add_cv_prj
	{
	int crv_idx;           	/* curve index */
	int srf_idx;           	/* surface index */
	mdl_pnt prj_dir;       	/* vector indicating direction of projection  */
	} mdl_add_cv_prj;

typedef struct _mdl_add_cv_spiral
	{
	mdl_pnt s_pnt;         	/* start point   */
	mdl_pnt c_pnt;         	/* center point   */
	mdl_pnt axis_dir;      	/* vector indicating direction of axis   */
	double len;             /* length of center line  */
	int num_turns;          /* number of turns */
	double taper_ang;       /* taper angle */
	int rot_dir;            /* direction of rotation 0=cw,1=ccw */
	} mdl_add_cv_spiral;

typedef struct _mdl_add_cv_iso
	{
	int srf_idx;           	/* surface index */
	int iso_dir;           	/* direction of isoline 0=u,1=v */
	double parm;           	/* parameter value for isoline  */
	} mdl_add_cv_iso;

typedef struct _mdl_add_srf_grid
	{
	int u_deg;              /* degree in u direction. */
	int v_deg;              /* degree in v direction. */
	int npt_udir;           /* number of points in the u direction. */
	int npt_vdir;           /* number of points in the v direction. */
	mdl_pnt_list pnt_lst;   /* points to interpolate/approx */
	} mdl_add_srf_grid;

typedef struct _mdl_srf_at
   {
   mdl_ln_type ln_type; 	/* line type for wire display */
   mdl_dsp_mode dsp_mode;	/* display mode (wire or shaded) */
   mdl_fct_res fct_res;		/* facet resolution */
	int num_u_iso;				/* no. isolines in u direction */
	int num_v_iso;				/* no. isolines in v direction */
   int color;       			/* surface color ramp index, 0-2  */
   double ln_width;  		/* line width scale factor for wire display */
   double amb_refl;    		/* 0-1.0 */
   double diff_refl;   		/* 0-1.0 */
   double spec_refl;   		/* 0-1.0 */
   double spec_exp;    		/* 0-20  */
   double trans;       		/* transparency coeff */
   } mdl_srf_at;

typedef struct _mdl_crv_at
   {
   mdl_ln_type ln_type;		/* line type */
   double ln_width;    		/* line width scale factor */
   int color;       			/* color index, 0-15  */
   } mdl_crv_at;

typedef struct _mdl_dim_at
   {
   int style;       			/* dimension style */
   int color;       			/* color index, 0-15 */
   int font;        			/* text font number (1-21) */
   int places;      			/* no. decimal places displayed */
   int extend;      			/* 1 if leader should automatically be extended */
   double gap;       		/* break length */
   double over;      		/* over run */
   double height;    		/* text height */
   } mdl_dim_at;

typedef struct _mdl_pnt_at
   {
   mdl_pnt_type pnt_type;	/* point marker type */
   int color;       			/* color index, 0-15   */
   double size;        		/* point marker scale factor */
   } mdl_pnt_at;
 
typedef struct _mdl_txt_at
   {
   mdl_txt_halign hor_align; 	/* horizontal alignment */
   mdl_txt_valign ver_align;	/* vertical alignment  */
	mdl_txt_path path;			/* text path */
	mdl_txt_leader leader;		/* annotation leader switch */ 
	mdl_txt_prec prec;			/* text precision */ 
   int font;       				/* text font index (1-21) */
   int color;       				/* text color index (0-15) */
   double exp_fac;    			/* character expansion factor */
   double height;     			/* character height */
   double ch_space;    			/* character spacing */
   double ln_space; 				/* line spacing */
   double up_x, up_y;       	/* character up vector */
   double x_offs, y_offs; 		/* x, y offset for annotation text */
   } mdl_txt_at;

typedef struct _mdl_pln_at
	{
	int format;						/* format (1-3) */
	int grid_switch;				/* 1-ON, 0-OFF */
	int tic_switch;				/* 1-ON, 0-OFF */
	int tic_space;					/* tic spacing (0-7) */
	double grid_space;			/* grid spacing */
	double x_extent;				/* extent in x direction */
	double y_extent;				/* extent in y direction */
	} mdl_pln_at;

typedef struct _mdl_wire_dflt
	{
	int curve_degree;				/* default curve degree */
	mdl_line_len	line_len;	/* default line length */
	double fixed_line_len;		/* fixed line length */
	double conic_ratio;			/* default conic ratio */
	double fillet_radius;		/* default fillet radius */
	double fillet_con_rat;		/* default surface fillet conic ratio */
	} mdl_wire_dflt;

typedef struct _mdl_prop
	{
	mdl_prop_type type;			/* property data type */
	double data[3];				/* data */
	} mdl_prop;

/* 3D/Annotation text */
typedef struct _mdl_text
   {
	int 			anno_text;		/* 1 = annotation text, 0 = 3D text */
   mdl_txt_at 	at;				/* text display attributes, automatically filled
											in from defaults when adding text */  
	int 			num_char;		/* number of characters in text string */
   char 			*text;			/* null terminated text string */  
   mdl_pnt 		pos;				/* text position in world coordinates */  
   mdl_matrix 	xform;			/* position and orientation of 3D text plane */
   } mdl_text;

typedef struct _mdl_data
	{
	int index;						/* database entity index (-1 if undefined)	*/
	mdl_data_type type;			/*	type of data stored in this structure  	*/
	union
		{
		double			t;
		mdl_uv_pnt		uv;
		mdl_pnt			pnt;
		mdl_lim3			box;
		mdl_nrb_crv		nrb_crv;
		mdl_int_pnt		int_pnt;
		mdl_int_crv		int_crv;
		mdl_srf_crv		srf_crv;
		mdl_nrb_srf 	nrb_srf;
		mdl_eval_uvcv	ev_uvcv;
		mdl_eval_crv	ev_crv;
		mdl_eval_srf	ev_srf;
		mdl_plane		pln;
		mdl_sphere		sph;
		mdl_cylinder	cyl;
		mdl_cone			cone;
		mdl_torus		torus;
		mdl_ellipsoid	ellsd;
		mdl_extrusion	extrus;
		mdl_revolution	rev;
		mdl_line			line;
		mdl_circle		cir;
		mdl_ellipse		ell;
		mdl_conic		conic;
		mdl_text			text;
		} data;						/* the different data types */
	} mdl_data;

typedef struct _mdl_ety_at
	{
	mdl_name ety_name;			/* entity name */
	mdl_name layer_name;			/* layer name */
	int layer_id;					/* layer id number */
	mdl_ety_vis blank_status;	/* blank status */
	union
		{
		mdl_srf_at srf_at;
		mdl_crv_at crv_at;
		mdl_dim_at dim_at;
		mdl_pnt_at pnt_at;
		mdl_txt_at txt_at;
		mdl_pln_at pln_at;
		} data;						/* attributes */
	} mdl_ety_at;

typedef struct _mdl_geom
	{
	mdl_geom_type	type;			/* geometry type */
	union
		{
		mdl_pnt pnt;
		mdl_infi_ln ln;
		mdl_infi_pln pln;
		mdl_lim3 box;
		} data;						/* geometry */
	} mdl_geom;

typedef struct _mdl_inp_ent
	{
	int type;						/* entity type */
	int index;						/* entity index */
	mdl_pnt pickpnt;				/* pick point */
	} mdl_inp_ent;

typedef struct _mdl_move
	{
	double		dist;				/* distance */
	double		angle;			/* angle */
	mdl_pnt		scale;			/* scale */
	mdl_line		line;				/* move vector */
	mdl_pnt		pnts[6];			/* alignment points */
	mdl_matrix	plane;			/* reference plane */
	} mdl_move;

/* entity pick data */
typedef struct _mdl_epick
   {
   short type;        			/* entity type */
   int index;           		/* entity index */
   double p1,p2,p3;     		/* entity pickpt parameters */
   } mdl_epick;

/* line attribute bundle */
typedef struct _mdl_ln_bundle
   {
   char ln_type;     			/* LIN_SOLID, LIN_DASH, LIN_DOT, LIN_DOTDASH */
   char color;       			/* color index, 0-15  */
   char width;       			/* 1-100 */
	char use_rgb;					/* use rgb instead of color */
	char r,g,b;						/* rgb values */
   } mdl_ln_bundle;

/* point attribute bundle */
typedef struct _mdl_pnt_bundle
   {
   char pnt_type;    			/* PNT_POINT, PNT_PLUS, PNT_STAR, PNT_O, PNT_X */
   char color;       			/* 0-15   */
   char size;        			/* 1-100 tenths (.1 to 10) */
	char use_rgb;					/* use rgb instead of color */
	char r,g,b;						/* rgb values */
   } mdl_pnt_bundle;

/* stl file data */
typedef struct _mdl_stl_data
	{
	int			type;			 	/* file format (0 - ASCII or 1 - BINARY) */
	int			move_to_pos; 	/* 1 to move facets to 1st quadrant, else 0 */
	int			num_ents;	 	/* number of surface/surfset entities */
	mdl_epick	*ents;		 	/* list of surface/surfset entities */
	double		tol;			 	/* chord-height tolerance for faceting */
	} mdl_stl_data;

/* dwg file data */
typedef struct _mdl_dwg_data
	{
   int 		opt;         		/* 0-no special processing, 1-save by layer
								    		2-save associativities */
   int 		hide;        		/* 0-no hidden line removal,1-hidden line removal. */
   int 		view_opt;    		/* 0-save active view, 1-save 3 standard views, */
                    		 		/* 2-save 4 standard views. */
   int 		show_invis;  		/* 1-show invisible lines, else 0. */
   int 		invis_type;  		/* Invisible line type. */
   double 	view_ar;     		/* Aspect ratio of viewport (height/width). */
   double 	scale;    	 		/* Scale factor. */
   char 		*lyr_name; 	 		/* Layer name for active view (view_opt=0). */
	} mdl_dwg_data;
 
/* 3D position/orientation (i.e. location) */
typedef struct _mdl_loc3d
	{
	double xt;           	/* x translation */
	double yt;           	/* y translation */
	double zt;           	/* z translation */
	double xr;           	/* x rotation    */
	double yr;           	/* y rotation    */
	double zr;           	/* z rotation    */
	short  orientation;  	/* = -1  implies  left hand system,
									 	=  1  implies right hand system  */
	} mdl_loc3d;

/* surfset record */
typedef struct _mdl_surfset_rec
	{
   char 			top_flag;		/* 1-if solid topology, 0-if open set   	*/
   char 			solid_type;		/* NORMAL, KEEPOUT, UNOCCUP   				*/
   int 			id;				/* record id                              */
   mdl_name 	name;				/* record name                            */
   int 			assoc;			/* index to first association or NULL_IDX */
   int 			num_surf;		/* no. of surfaces in surfset             */  
   int 			tsurf;			/* index to first tsurf in surfset        */  
   mdl_loc3d 	frame;			/* local coordinate reference frame       */  
   mdl_lim3 	bnd_xyz;			/* 3d bounding box in world coordinates   */
	} mdl_surfset_rec;

/* curvset record */
typedef struct _mdl_curvset_rec
	{
	char			profile;			/* 1-if curvset is a valid profile			*/
   int 			id;				/* record id                              */
   mdl_name 	name;				/* record name                            */
   int 			assoc;			/* index to first association or NULL_IDX */
   int 			num_curv;		/* no. of curves in curvset					*/  
   int 			fcurv;			/* index to first tsurf in surfset        */  
   mdl_loc3d 	frame;			/* local coordinate reference frame       */  
	} mdl_curvset_rec;

/* surface record */
typedef struct _mdl_surf_rec
	{
   char 			type;				/* surface type                        */
   char 			dir;				/* surface direction flag, 1 or -1     */  
   char 			top_flag;		/* =1 if default topology, else 0      */  
   char 			blank;			/* blanking flag 								*/  
   char 			fillet;			/* fillet flag                         */
   int			id;				/* record id                           */
   mdl_name 	name;				/* record name                         */
   mdl_srf_at 	at;				/* surface display attributes          */  
   int 			layer;			/* layer reference number              */  
   short 		num_u_iso;		/* no. isolines displayed in u dir.    */
   short 		num_v_iso;		/* no. isolines displayed in v dir.    */
   int 			assoc;			/* index to first assoc. or NULL_IDX   */
   int 			surfset;			/* index to surfset record             */  
   int 			next;				/* index to next tsurf in surfset      */  
   int 			srf;				/* index to surface primitive record   */
   int 			num_loop;		/* no. of trimming loops               */  
   int 			loop;				/* index to first=outer loop record    */
   mdl_loc3d 	loc;				/* surface position and orientation    */
   mdl_lim3 	bnd_xyz;			/* 3D bounding box - world coord's     */  
   int 			num_pline;		/* no. of plines for isoline display   */
   int 			p_vtx;			/* float vertex data for pline's       */  
   int 			num_facet;		/* no. of facets for shaded display    */
   int 			f_vtx;			/* float vertex data for facets        */  
   double 		d_tol;			/* tolerance for properties calc.      */  
   double 		area;				/* surface area                        */
   double 		volume;			/* volume component for solids         */  
   double 		x,y,z;			/* centroid                            */
	} mdl_surf_rec;

/* loop record */
typedef struct _mdl_loop_rec
	{
	int			next_loop;		/* index to next loop on surface       */  
   int			edge;				/* index to first edge in loop         */  
	} mdl_loop_rec;

/* edge record */
typedef struct _mdl_edge_rec
	{
	char			edge_type;		/* EDGE_STD, EDGE_DEGEN, EDGE_SEAM     */  
	int			uv[2];			/* indices to uv curve records         */  
	int			next[2];			/* indices to next edge records        */  
	int			loop[2];			/* indices to parent loop records      */  
	} mdl_edge_rec;

/* uvcurv record */
typedef struct _mdl_uvcurv_rec
	{
	char			prim_type;		/* uv primitive type                   */  
	int			uv;				/* index to uv primitive record        */  
	int			surf;				/* index to tsurf record               */  
	int			edge;				/* index to edge record or NULL_IDX    */
	} mdl_uvcurv_rec;

/* fcurv record */
typedef struct _mdl_fcurv_rec
	{
	char			prim_type;		/* primitive curve type                */  
	char			blank;			/* blanking flag 								*/  
	char			fn_crv;			/* TRUE if fcurv belongs to func curv  */
	int			id;				/* record id                           */
	mdl_name		name;				/* record name                         */
	mdl_crv_at	at;				/* curve display attributes            */  
	int			layer;			/* layer reference number              */  
	int			assoc;			/* index to first assoc. or NULL_IDX   */
	int			curvset;			/* index to curvset record             */  
	int			next;				/* index to next fcurv in curvset      */  
	int			crv;				/* index to 3D curve primitive record  */
	int			srf[16];			/* indices to tsurf's (or NULL_IDX)    */
	} mdl_fcurv_rec;

/* frame record */
typedef struct _mdl_frame_rec
	{
   int 			time;           	/* time stamp                                */
   char 			disp_std_grid;  	/* display standard grid in xy plane (T/F)   */
   char 			disp_sub_grid;  	/* display sub grid in xy plane (TRUE/FALSE) */
   char 			blank;          	/* blanking flag (see README)                */
   char 			layout;         	/* 1=quad 1, 2=quad 1&2, 3=quad 1-4          */
   int 			id;        			/* record id                                 */
   mdl_name 	name;       		/* record name                               */
   int 			assoc;           	/* index to first assoc. or NULL_IDX         */
   int 			layer;           	/* layer reference number                    */
   double 		x_extent;     		/* extent of plane in x direction            */
   double 		y_extent;     		/* extent of plane in y direction            */
   double 		std_spacing;  		/* spacing between standard grid points      */
   double 		sub_spacing;  		/* spacing between sub grid points           */
   mdl_loc3d 	loc;           	/* frame location                            */
	} mdl_frame_rec;

/* param record */
typedef struct _mdl_param_rec
	{
	int			time;				/* time stamp                             */
	char			type;				/* 0-distance, 1-angle, 2-dimensionless   */
	int			id;				/* record id                              */
	mdl_name		name;				/* record name                            */
	int			assoc;			/* index to first assoc. or NULL_IDX      */
	double		value;			/* evaluated parameter                    */
	} mdl_param_rec;

/* rpoint record */
typedef struct _mdl_rpoint_rec
	{
   int 			time;          /* time stamp                             */
   char 			blank;         /* blanking flag (see README)             */  
   int 			id;           	/* record id                              */
   mdl_name 	name;          /* record name                            */
   mdl_pnt_at 	at;          	/* point display attributes               */  
   int 			layer;         /* layer reference number                 */  
   int 			assoc;         /* index to first assoc. or NULL_IDX      */  
   mdl_pnt 		pos;           /* point position in world space          */  
	} mdl_rpoint_rec;

/* layer record */
typedef struct _mdl_layer_rec
   {
   mdl_name 	name;     		/* record name                               */
   int 			assoc;         /* index to first assoc. or NULL_IDX         */
   int 			num;           /* layer reference number (0 to 1024)  */
   } mdl_layer_rec;

/* Variable Length Data */
typedef struct _mdl_vl_data
	{  
	char 			modified;      /* TRUE or FALSE (1/0)							*/
	void 			*ptr;          /* Pointer to the vl data array           */
	short 		elem_type;     /* Index into elem_type_table             */
	int 			no_elem;       /* Number of elements in the array        */
                              /* if -1, vl_data is not a homogeneous    */
                              /* array of one elem_type but an interpre-*/
                              /* ted data. 										*/
	} mdl_vl_data;    

/* data record */
typedef struct _mdl_data_rec
   {
   mdl_vl_data	v;             /* variable length data element        */  
   } mdl_data_rec;                                                          
 
/* general attribute record  */
typedef struct _mdl_gen_at_rec
   {
   char			tpl_flag;    	/* = 1 if command template, else 0           */
   char 			app_flag;    	/* 0-string, 1-double, 2-integer             */
   short 		appl_id;    	/* application id (0-host)                   */
   int 			tpl;          	/* index of template used to create attrib   */
   int 			id;     			/* record id                                 */
   mdl_name 	name;   			/* record name                               */
   int 			data;         	/* index to ascii data record                */
   } mdl_gen_at_rec;

/* associativity record  */
typedef struct _mdl_assoc_rec
   {
   char 			attrib[4];   	/* associativity attributes                  */
        			             	/* [0] - AUTO_DEL/NO_AUTO_DEL flag           */
   int 			file_proj_id; 	/* project id of referenced file             */
   int 			file_file_id; 	/* file id of referenced file                */
   mdl_name 	name;    		/* name of referenced record                 */
   int 			next;         	/* index to next assoc_rec or NULL_IDX       */
   } mdl_assoc_rec;                                                      

/* atext record */
typedef struct _mdl_atext_rec
	{
   char 				blank;       /* blanking flag (see README)             */  
   mdl_txt_at 		at;          /* text display attributes                */  
   int 				layer;       /* layer reference number                 */  
   int 				id;          /* unique id                              */
   int 				text;        /* index to ascii data record             */  
   mdl_name 		name;        /* record name                            */
   mdl_pnt 			pos;         /* refpt position in world space          */  
	} mdl_atext_rec;

/* text3 record */
typedef struct _mdl_text3_rec
	{
   char 				blank;        	/* blanking flag (see README)             */  
   mdl_txt_at 		at;          	/* text display attributes                */  
   int 				layer;        	/* layer reference number                 */  
   int 				id;           	/* unique id                              */
   int 				text;         	/* index to ascii data record             */  
   mdl_name 		name;         	/* record name                            */
   mdl_pnt 			pos;          	/* text position in world coordinates     */  
   mdl_loc3d 		loc;          	/* position and orientation of text plane */
	} mdl_text3_rec;

 /* function curve record */
typedef struct _mdl_fncurv_rec
   {
   int 			id;        		/* record id                                 */
   mdl_name 	name;       	/* record name                               */
   int 			assoc;         /* index to first assoc. or NULL_IDX         */  
   int 			fcurv;         /* index to fcurv                            */
   int 			line;          /* index to linear fcurv                     */  
   } mdl_fncurv_rec;
 
/* variable length data record - unaffected by undo/redo */
typedef struct _mdl_ndata_rec
   {
   mdl_vl_data v;             /* variable length data element        */  
   } mdl_ndata_rec;                                                         
 
/********** input_data ********************************************************/

/* max length of ascii strings (char) */
#define MDL_ISLEN  255

/* structure containing stacks of input data */
typedef struct _mdl_input_data
	{
	int flg_cnt;             				/* no. flags on flag list */
	int *flg;                 				/* flags - TRUE or FALSE */

	int str_cnt;              				/* no. strings on string list */
	char **str;									/* strings, each one allocated to 
														((2*MDL_ISLEN + 1) * sizeof(char)) */

	int ety_cnt;              				/* no. entities on entity list */
	mdl_epick *ety;           				/* database entities (direct addresses)*/

	int grp_cnt;              				/* no. members currently in group */
	mdl_epick *grp;           				/* group of database entities */

	int grp2_cnt;             				/* no. members currently in group */
	mdl_epick *grp2;          				/* 2nd group of database entities */

	int num_cnt;              				/* no. numbers on number list */
   double *num;              				/* floating point numbers */

	int pnt_cnt;              				/* no. points on point list */
   mdl_pnt *pnt;             				/* points */

	int vtx_cnt;              				/* no. vertices on stroke list */
	mdl_pnt *vtx;             				/* stroke - vertex list */

	int vec_cnt;				  				/* no. of lines on line list */
	mdl_line *vec;				  				/* lines */

	int pln_cnt;				  				/* no. of planes on list */
	mdl_matrix *pln;			  				/* planes */

	} mdl_input_data;

/* function prototypes */
#ifdef __STDC__
int vx_mdl_add_crv(mdl_crv_method method, void *data, mdl_list *crv_idx_list);
int vx_mdl_add_cset(mdl_cset_method method, void *data, int *cset_idx);
int vx_mdl_add_ftr(mdl_ftr_method method, void *data, int *ftr_idx);
int vx_mdl_add_layer(char *name, int disp_layer, int def_layer, int *index);
int vx_mdl_add_pln(mdl_pln_method method, void *data, int *pln_idx);
int vx_mdl_add_pnt(mdl_pnt_method method, void *data, int *pnt_idx);
int vx_mdl_add_srf(mdl_srf_method method, void *data, int *srf_idx);
int vx_mdl_add_sset(mdl_sset_method method, void *data, int *sset_idx);
int vx_mdl_add_text(mdl_text *text, int *text_idx);
int vx_mdl_add_to_sset(int sset_idx, int srf_idx, int option, mdl_list *sset_idx_list);
int vx_mdl_alloc_nrb(mdl_data_type type, void *geom);
int vx_mdl_app_command(char *command,mdl_input_data *inp_data);
int vx_mdl_classify_pnt(mdl_pnt_class_method method, mdl_pnt *pnt, int num_ety, mdl_data *ety_list, mdl_pnt_class *class);
int vx_mdl_clear_input_data(void);
int vx_mdl_combine(mdl_bool_method method, int ety_idx_1, int ety_idx_2, mdl_list *ety_list);
int vx_mdl_command(char *string, int *num_cnt, double **num_list, int *str_cnt, sys_strg **str_list);
int vx_mdl_db_get_first(mdl_rec_type rec,int *index);
int vx_mdl_db_get_last(mdl_rec_type rec,int *index);
int vx_mdl_db_get_next(mdl_rec_type rec,int *index);
int vx_mdl_db_get_prev(mdl_rec_type rec,int ref_index,int *index);
int vx_mdl_db_init(void);
int vx_mdl_db_load(mdl_file_type type, mdl_path name, void *data);
int vx_mdl_db_merge(mdl_file_type type, mdl_path name, mdl_merge_opt option, mdl_name prefix, void *data);
int vx_mdl_db_regen(void);
int vx_mdl_db_save(mdl_file_type type, mdl_path name, void *data);
int vx_mdl_dealloc_nrb(mdl_data_type type, void *geom);
int vx_mdl_debug_on(int pause, int prt_arg, mdl_path file);
int vx_mdl_delete_entity(int ety_idx);
int vx_mdl_display_add_set(char *set);
int vx_mdl_display_anno(char *set, int ent_id, char *buf, mdl_pnt *pos, double x, double y, double height, int leader, int color);
int vx_mdl_display_arrow(char *set, int ent_id, int color, mdl_line *line, double length);
int vx_mdl_display_blank(char *set, int ent_id, int blank);
int vx_mdl_display_clear(char *set);
int vx_mdl_display_del_ent(char *set, int id_type, int index);
int vx_mdl_display_del_set(char *set);
int vx_mdl_display_edge(char *set, int ent_id, int color, int edge_idx);
int vx_mdl_display_entity(int index);
int vx_mdl_display_hilite(int index, int hilite);
int vx_mdl_display_lines(char *set, int lkey, mdl_ln_bundle *line_at, int num_l, mdl_line *llist);
int vx_mdl_display_ncurv(char *set, int ent_id, int color, mdl_nrb_crv *crv, double width, int type);
int vx_mdl_display_nsurf(char *set, int ent_id, int color, mdl_nrb_srf *srf, double width, int type, mdl_loc3d *loc);
int vx_mdl_display_plane(char *set, int ent_id, int color, mdl_loc3d *loc, double extent);
int vx_mdl_display_pnts(char *set, int pkey, mdl_pnt_bundle *pnt_at, int num_p, mdl_pnt *plist);
int vx_mdl_display_redraw(void);
int vx_mdl_display_regen(void);
int vx_mdl_display_update(int flag);
int vx_mdl_display_vector(char *set, int ent_id, int color, int num_vec, mdl_line *list);
int vx_mdl_display_win_out(mdl_pnt *p1, mdl_pnt *p2);
int vx_mdl_display_win_view(mdl_pnt *p1, mdl_pnt *p2);
int vx_mdl_err_clear(void);
int vx_mdl_err_get_count(int *count);
int vx_mdl_err_get_data(int err_num, task_err_struc *error);
int vx_mdl_eval_dist(mdl_data *ety_1, mdl_data *ety_2, mdl_trim_opt option, double *dist, mdl_ent_loc *loc_1, mdl_ent_loc *loc_2);
int vx_mdl_eval_geom(mdl_data *geom, mdl_data *pnt, int level, mdl_data *result);
int vx_mdl_eval_geom2(mdl_data *geom, mdl_data *pnt, int level, mdl_data *result);
int vx_mdl_eval_isect(mdl_data *ety_1, mdl_data *ety_2, mdl_trim_opt option, mdl_int_list *isect_list);
int vx_mdl_exp_from_set(int ety_idx);
int vx_mdl_exp_set(int set_idx);
int vx_mdl_ext_crv(int crv_idx, int option, double dist);
int vx_mdl_ext_srf(int srf_idx, int option, double dist);
int vx_mdl_free_input_data(mdl_input_data *inp);
int vx_mdl_get_color_rgb(int color, int *r, int *g, int *b);
int vx_mdl_get_input_data(mdl_input_data *inp);
int vx_mdl_inp_angle(mdl_ang_unit unit, double *angle);
int vx_mdl_inp_curve(mdl_inp_ent *ent);
int vx_mdl_inp_disp_ent(mdl_inp_ent *ent);
int vx_mdl_inp_distance(double *dist);
int vx_mdl_inp_edge(mdl_inp_ent *edge, mdl_inp_ent *surf1, mdl_inp_ent *surf2);
int vx_mdl_inp_entities(int *num_ents, mdl_inp_ent **ents);
int vx_mdl_inp_entity(mdl_inp_ent *ent);
int vx_mdl_inp_number(double *number);
int vx_mdl_inp_plane(mdl_matrix plane);
int vx_mdl_inp_point(mdl_pnt *point);
int vx_mdl_inp_string(char *string);
int vx_mdl_inp_surface(mdl_inp_ent *surf);
int vx_mdl_inp_vector(mdl_line *vector);
int vx_mdl_inp_yes(int *yes);
int vx_mdl_inq_assoc_list(int ety_idx, mdl_list *assoc_list);
int vx_mdl_inq_default(mdl_default option, void *data);
int vx_mdl_inq_edg_by_vtx(int edg_idx, mdl_pnt *vtx, mdl_list *edg_list);
int vx_mdl_inq_edg_geom(int edg_idx, int srf_idx, int option, mdl_int_crv *edg);
int vx_mdl_inq_edg_list(int ety_idx, mdl_list *edg_list);
int vx_mdl_inq_edg_srf(int edg_idx, mdl_list *srf_list);
int vx_mdl_inq_endpt(int crv_idx, mdl_pnt *s_pnt, mdl_pnt *e_pnt);
int vx_mdl_inq_ety_at(int ety_idx, mdl_ety_at *at);
int vx_mdl_inq_ety_data(int ety_idx, mdl_ety_data *ety_data);
int vx_mdl_inq_ety_list(mdl_ety_type ety_type, mdl_list *ety_list);
int vx_mdl_inq_facet(int ety_idx, double tol, mdl_fct_opt option, int *num_fct, mdl_pnt **fct_list);
int vx_mdl_inq_index(char *ety_name, int *ety_idx);
int vx_mdl_inq_iso(mdl_add_cv_iso *isocv_dat, int *num, mdl_data **iso_curves);
int vx_mdl_inq_name(int ety_idx, char *ety_name);
int vx_mdl_inq_nrb(int ety_idx, mdl_trim_opt option, mdl_data *nrb);
int vx_mdl_inq_parent(int ety_idx, int *parent_idx);
int vx_mdl_inq_prim(int ety_idx, mdl_data *prim);
int vx_mdl_inq_prop(int ety_idx, mdl_list *type, mdl_parm *parm, double tol, int *num_prop, mdl_prop **prop);
int vx_mdl_inq_rec(int ety_idx, mdl_rec_type type, void *rec);
int vx_mdl_inq_set(int set_idx, mdl_list *ety_list);
int vx_mdl_inq_sil(int ety_idx, mdl_pnt *view_vec, mdl_list *crv_list);
int vx_mdl_inq_solid(int ety_idx, int *solid);
int vx_mdl_inq_text(int text_idx, mdl_text *text);
int vx_mdl_inq_type(int ety_idx, mdl_ety_type *ety_type);
int vx_mdl_inq_var(int index, double *value);
int vx_mdl_inq_version(int *version);
int vx_mdl_inq_vis(int ety_idx, mdl_ety_vis *vis_flag);
int vx_mdl_loc3_to_tmat(mdl_loc3d *l, mdl_matrix t);
int vx_mdl_log_angle(int clear, double angle);
int vx_mdl_log_distance(int clear, double distance);
int vx_mdl_log_entity(int clear, int type, int entity);
int vx_mdl_log_file_name(int clear, char *file_name);
int vx_mdl_log_flag(int clear, int flag);
int vx_mdl_log_number(int clear, double number);
int vx_mdl_log_point(int clear, mdl_pnt *point);
int vx_mdl_log_string(int clear, char *string);
int vx_mdl_log_vertex(int clear, mdl_pnt *vertex);
int vx_mdl_math_ang_2vec(mdl_pnt *vec_1, mdl_pnt *vec_2, double *angle);
int vx_mdl_math_box_find(mdl_pnt_list *pnt_list, mdl_lim3 *box);
int vx_mdl_math_box_merge(mdl_lim3 *b1, mdl_lim3 *b2, mdl_lim3 *box);
int vx_mdl_math_chk_pnt_lst(mdl_pnt_list *pnt_list, double tol, mdl_struct_type *set_type, mdl_pnt *vec);
int vx_mdl_math_crosprd(mdl_pnt *v1, mdl_pnt *v2, mdl_pnt *vec);
int vx_mdl_math_dist(mdl_geom *e1, mdl_geom *e2, double *dist);
int vx_mdl_math_dotprd(mdl_pnt *v1, mdl_pnt *v2, double *dot);
int vx_mdl_math_isect(mdl_geom *e1, mdl_geom *e2, double tol, mdl_geom *isect, mdl_int_class *class);
int vx_mdl_math_mk_frame(mdl_frame_method method, mdl_pnt lst_pt_vec[3], mdl_matrix frm);
int vx_mdl_math_pnt_3pln(mdl_infi_pln *p1, mdl_infi_pln *p2, mdl_infi_pln *p3, mdl_pnt *pnt);
int vx_mdl_math_prj_pnt(mdl_pnt *pnt, mdl_geom *geom, mdl_pnt *prj_pnt, double *dist);
int vx_mdl_math_xform_pnt(mdl_xform_type type, mdl_pnt *pnt, mdl_matrix xform);
int vx_mdl_mod_rec(int *ety_idx, mdl_rec_type type, void *rec);
int vx_mdl_mod_var(int index, double value);
int vx_mdl_move(mdl_move_method method, mdl_move_modal modal, mdl_list *ety_list, mdl_move *mdata, mdl_list *new_ety_list);
int vx_mdl_order_loops(mdl_data *surface);
int vx_mdl_prompt(int indent, char *text);
int vx_mdl_prt_cb_list(int type);
int vx_mdl_prt_cb_rec(int rec_idx);
int vx_mdl_prt_db_status(void);
int vx_mdl_prt_struct(mdl_struct_type type, void *data);
int vx_mdl_prt_topo(int ety_idx);
int vx_mdl_refine_nrb_crv(mdl_data *curve, double tol, mdl_pnt_list *pnt_list);
int vx_mdl_reverse(mdl_list *ent_list);
int vx_mdl_set_default(mdl_default option, void *data);
int vx_mdl_set_ety_at(int ety_idx, mdl_attr_type type, mdl_ety_at *at);
int vx_mdl_split(int ety_idx, int bnd_idx, mdl_list *ety_list);
int vx_mdl_tmat_to_loc3(mdl_matrix t, mdl_loc3d *l);
int vx_mdl_wait_for_command(int flag);
#endif

#ifdef __cplusplus
}
#endif /* __cplusplus */
#endif /* __openvx_mdl_h */
