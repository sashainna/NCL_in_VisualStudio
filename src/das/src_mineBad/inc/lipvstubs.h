/*********************************************************************
**    NAME         :  lipvstubs.h
**
**       CONTAINS:
**				Stub definitions for platforms that do not support NCLIPV.
**
**    COPYRIGHT 2001 (c) NCCS, Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       lipvstubs.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:28
**
*********************************************************************/

typedef int LtAssembly;
typedef int LtAttributeClass;
typedef int LtBody;
typedef int LtBodyQuery;
typedef int LtBoolean;
typedef double LtBounds[6];
typedef char LtChar;
typedef int LtColour[3];
typedef int LtComparison;
typedef int LtData;
typedef int LtDiagnostic;
typedef double LtDouble;
typedef double LtDoubleBounds[6];
typedef double LtDoublePoint[3];
typedef double LtDoubleVector[3];
typedef int LtDrawable;
typedef int LtDriver;
typedef int LtEntity;
typedef int LtEntityType;
typedef int LtExecute;
typedef int LtFace;
typedef double LtFloat;
typedef int LtFont;
typedef int LtFunc;
typedef char* LtGenericPtr;
typedef int LtImage;
typedef int LtInt32;
typedef int LtInteractType;
typedef int LtLineStyle;
typedef int LtLoop;
typedef int LtLoopEdge;
typedef double LtMatrix[4][4];
typedef int LtMWProgress;
typedef int LtNat32;
typedef int LtPatch; 
typedef int LtPickedEntity;
typedef int LtPickedEntityList;
typedef int LtPref;
typedef int LtPrim;
typedef int LtProfile;
typedef int LtProgCoordSys;
typedef int LtProgress;
typedef int LtProgressProperty;
typedef int LtProj;
typedef int LtMachiningSession;
typedef int LtMaterial;
typedef double LtPoint[3];
typedef int LtRvPick;
typedef int LtSavedMWEnvironment;
typedef int LtSEdge; 
typedef int LtSelfIntersectionList;
typedef int LtSelfIntersection;
typedef int LtSession;
typedef int LtSessionPrim;
typedef int LtShader;
typedef int LtSinglePrecisionFormat;
typedef int LtSolidStyle;
typedef int LtSolidType;
typedef int LtSref;
typedef int LtStatus;
typedef int LtSurfaceType;
typedef int LtTextStyle;
typedef int LtTransform;
typedef double LtVector[3];
typedef int LtVertex;
typedef int LtView;
typedef int LtViewport;

typedef struct
{
	int num_loops;
	int *vertices_per_loop;
	int *indices;
	double *plane_equation;
} LtMeshPolygon;

typedef struct
{
	LtDoublePoint p;
	LtDoubleVector v;
	LtDouble k;
} LtCone;

typedef struct
{
	LtDoublePoint p;
	LtDoubleVector v;
	LtDouble r;
} LtCylinder;

typedef struct
{
	LtDouble a;
	LtDouble b;
	LtDouble c;
	LtDouble d;
} LtPlane;

typedef struct
{
	LtDoublePoint p;
	LtDouble r;
} LtSphere;

typedef struct
{
	LtDoublePoint p;
	LtDoubleVector vaxis;
	LtDoubleVector vperp;
	LtDouble r_major;
	LtDouble r_minor;
} LtSweptTorus;

typedef struct
{
	LtDoublePoint p;
	LtDoubleVector v;
	LtDouble r_major;
	LtDouble r_minor;
} LtTorus;

typedef struct
{
	LtDoublePoint point;
	LtDoubleVector normal;
	LtPrim cut_prim;
	LtNat32 cut_number;
	union
	{
		LtPlane plane;
		LtCylinder cylinder;
		LtSphere sphere;
		LtTorus torus;
		LtSweptTorus swept_torus;
		LtCone cone;
	} surface;
	LtSurfaceType type;
} LtPickedSurface;


#define ANSI_CHARSET 1
#define DEFAULT_PITCH 1
#define DEFAULT_QUALITY 1
#define OUT_DEFAULT_PRECIS 1

#define LI_ANGLE_DEG_0 1
#define LI_ANGLE_DEG_90 1
#define LI_ANGLE_DEG_180 1
#define LI_ANGLE_DEG_MINUS_90 1
#define LI_CALL_DIAGNOSTIC 1
#define LI_CALL_DRIVER_CHECK_INTERRUPT 1
#define LI_CALL_MW_MESH_ADD_POLYGON 1
#define LI_CALL_MW_MESH_BEGIN 1
#define LI_CALL_MW_PROGRESS 1
#define LI_CALL_PROGRESS 1
#define LI_CE_CLOSE_DEFAULT 1
#define LI_CONTROL_SO_CONSISTENT_3_AXIS 1
#define LI_CONTROL_DRIVER_WINDOW 1
#define LI_CONTROL_FX_MAX_INTERP_ANGLE 1
#define LI_CONTROL_JOURNAL_FILE 1
#define LI_CONTROL_JOURNALING 1
#define LI_CONTROL_KI_SYNC_MOVES 1
#define LI_CONTROL_MW_ADJUST_VIEW_FROM 1
#define LI_CONTROL_MW_TOUCH_IS_CLASH 1
#define LI_CONTROL_MW_USE_OPENGL_DISP_LISTS 1
#define LI_CONTROL_MWUTIL_STL_MERGE_FACES 1
#define LI_CONTROL_NU_STL_MERGE_FACES 1
#define LI_CONTROL_OGLDRV_WINDOW_MODE 1
#define LI_CONTROL_RN_RENDERER_TYPE 1
#define LI_CONTROL_RV_CONVEXIFY_TOOLS 1
#define LI_CONTROL_RV_EXTRA_GL_POLYS 1
#define LI_CONTROL_RV_GRAPHICS_MODE 1
#define LI_CONTROL_RV_MESH_FACET_TOL 1
#define LI_CONTROL_RV_MESH_FACET_TYPE 1
#define LI_CONTROL_RV_REN_TARGET_DIFF 1
#define LI_CONTROL_RV_RESOLUTION 1
#define LI_CONTROL_VI_CUT_DATA_STORAGE 1
#define LI_CONTROL_VI_FACET_TOLERANCE 1
#define LI_CONTROL_VI_INIT_MERGE_TYPE 1
#define LI_CONTROL_VI_LATEST_CUT_NUMBER 1
#define LI_CONTROL_VI_LATEST_MATERIAL 1
#define LI_CONTROL_VI_PATH_FIT_TOL 1
#define LI_CONTROL_VI_PATH_TOLERANCE 1
#define LI_CONTROL_VI_QUERY_COORD_SYS 1
#define LI_CONTROL_VI_STOP_AT_ILLEGAL 1
#define LI_CONTROL_VI_XFER_DIFF_ATTRIBS 1
#define LI_DIAG_ERROR_CLASH_CONTINUE 1
#define LI_DIAG_ERROR_CLASH_STOP 2
#define LI_DRAWABLE_PROP_BACKGROUND 2
#define LI_ENTITY_TYPE_FACE 1
#define LI_ENTITY_TYPE_PATCH 2
#define LI_ENTITY_TYPE_SOLID 2
#define LI_EXECUTE_REN_MW_GOURAUD 1
#define LI_EXECUTE_REN_RV_POLYGONS 2
#define LI_EXECUTE_REN_VI_HIDDEN_LINE 3
#define LI_EXECUTE_REN_VI_HYBRID 4
#define LI_EXECUTE_REN_VI_OPENGL_WIRE 5
#define LI_EXECUTE_REN_VI_RT_TARGET_DIFF 6
#define LI_FILE_READ 1
#define LI_FILE_WRITE 2
#define LI_FILE_APPEND 3
#define LI_JOURNAL_TEXT 1
#define LI_MINX 0
#define LI_MINY 1
#define LI_MINZ 2
#define LI_MAXX 3
#define LI_MAXY 4
#define LI_MTL_SUBTYPE_DEFAULT 1
#define LI_MTL_TYPE_PLAIN 1
#define LI_MTL_TYPE_SHADER 1
#define LI_MAXZ 5
#define LI_MW_COORD_SYSTEM_LOCAL 0
#define LI_MW_COORD_SYSTEM_VIEW 1
#define LI_MW_COORD_SYSTEM_WORLD 1
#define LI_MW_FACET_OUTSIDE 1
#define LI_MW_INTERACT_CLASH_CONTINUE 1
#define LI_MW_INTERACT_CLASH_CUT 1
#define LI_MW_INTERACT_CUT 2
#define LI_MW_INTERACT_IGNORE 3
#define LI_MW_LINE_COLOUR 0
#define LI_MW_LINE_PATTERN 0
#define LI_MW_LINE_THICKNESS 0
#define LI_MW_LINE_VISIBLE 0
#define LI_MW_MESH_OUTPUT_PLANES 2
#define LI_MW_MESH_TRIANGLES 2
#define LI_MW_PATTERN_SOLID 0
#define LI_MW_PICK_SOLIDS 0
#define LI_MW_POLYCURVE_COPY_GEOMETRY 0
#define LI_MW_POSITION_MODE_3D_FRONT 0
#define LI_MW_POSITION_MODE_NORM_WINDOW 0
#define LI_MW_SESSION_RAPIDCUT 1
#define LI_MW_SESSION_SOLID_OPS 2
#define LI_MW_SESSION_VISICUT 2
#define LI_MW_SIMPLIFY_FIT_ARCS 1
#define LI_MW_SIMPLIFY_FULL 1
#define LI_MW_SIMPLIFY_NOTHING 1
#define LI_MW_SIMPLIFY_STANDARD_TOOL 1
#define LI_MW_SOLID_TYPE_FIXTURE 1
#define LI_MW_SOLID_TYPE_GENERAL 2
#define LI_MW_SOLID_TYPE_STOCK 3
#define LI_MW_SOLID_TYPE_TARGET 4
#define LI_MW_SURFACE_CONE 0
#define LI_MW_SURFACE_CYLINDER 1
#define LI_MW_SURFACE_PLANE 2
#define LI_MW_SURFACE_SPHERE 3
#define LI_MW_SURFACE_SWEPT_TORUS 4
#define LI_MW_SURFACE_TORUS 5
#define LI_MW_TURN_SMOOTH_3AXIS 0
#define LI_MW_VIS_PROP_EDGE_COLOUR 1
#define LI_MW_VIS_PROP_MATERIAL 1
#define LI_MW_VIS_PROP_REN_INTERN_EDGES 1
#define LI_MW_VIS_PROP_RENDER_EDGES 2
#define LI_MW_VIS_PROP_TRANSPARENCY 3
#define LI_NU_SOLID_SAVE_NORMALS 1
#define LI_NU_SOLID_SAVE_CUT_NUMBERS 2
#define LI_OGLDRV_WINDOW_MODE_LW 1
#define LI_OGLDRV_WINDOW_MODE_APP 1
#define LI_PROG_PROP_MW_PERCENT 1
#define LI_RENDER_MODE_MW_HIDDEN_LINE 1
#define LI_RENDER_MODE_MW_HYBRID 1
#define LI_RENDER_MODE_MW_POLYGONS 1
#define LI_RENDER_MODE_MW_WIREFRAME 1
#define LI_RENDER_MODE_VI_RT_TARGET_DIFF 1
#define LI_RN_RENDERER_OPENGL 1
#define LI_ROBJ_PROP_MW_POLYC_DEF_STYLE 1
#define LI_ROBJ_PROP_MW_TEXT_STYLE 1
#define LI_RV_PICK_DISTANCE 1
#define LI_RV_PICK_CUT_NUMBER 1
#define LI_RV_PICK_NORMAL 1
#define LI_RV_VERIFY_SMOOTHED 1
#define LI_SESS_PROP_FX_MAX_INTERP_ANG 1
#define LI_SESS_PROP_KI_SYNC_MOVES 1
#define LI_SESS_PROP_MW_CUT_NUMBER 1
#define LI_SESS_PROP_MW_REGION_INTEREST 1
#define LI_SESS_PROP_RV_REN_TARGET_DIFF 1
#define LI_SESS_PROP_VI_CREATE_ISCT_SOL 1
#define LI_SESS_PROP_VI_FACET_TOLERANCE 1
#define LI_SESS_PROP_VI_PATH_FIT_TOL 1
#define LI_SESS_PROP_VI_PATH_TOLERANCE 1
#define LI_SESS_PROP_VI_QUERY_COORD_SYS 1
#define LI_SHADER_CLASS_BACKGROUND 1
#define LI_SHADER_CLASS_COLOUR 1
#define LI_SHADER_CLASS_LIGHT 2
#define LI_SHADER_CLASS_REFLECTANCE 3
#define LI_SO_BODY_PROP_CLOSED 1
#define LI_SO_BODY_PROP_INSIDE_OUT 1
#define LI_SO_EDGE_PROP_CLOSED 1
#define LI_SO_PATCH_PROP_CLOSED 1
#define LI_SO_SINGLE_PRECISION_ASCII 1
#define LI_SO_SINGLE_PRECISION_BINARY 2
#define LI_SOL_STYLE_PROP_TRANSPARENCY 1
#define LI_SOL_STYLE_PROP_RENDER_EDGES 1
#define LI_SPRIM_PROP_BOUNDS 1
#define LI_SPRIM_PROP_CLASH_MATERIAL 1
#define LI_SPRIM_PROP_CUT_MATERIAL 1
#define LI_SPRIM_PROP_VI_SHANK_EXPAND 1
#define LI_SOLID_PROP_MW_BOUNDS 1
#define LI_SOLID_PROP_MW_SESSION_BOUNDS 2
#define LI_SOLID_PROP_VI_SHANK_EXPAND 1
#define LI_SPRIM_PROP_PRIM 1
#define LI_SPRIM_PROP_SOLID_TYPE 1
#define LI_SPRIM_PROP_VI_PASSIVE 1
#define LI_STATUS_ERROR 2
#define LI_STATUS_INTERRUPT 0
#define LI_STATUS_OK 0
#define LI_STATUS_WARNING 2
#define LI_TEXT_ENCODE_ANSII 2
#define LI_TEXT_ENCODE_ASCII 2
#define LI_TEXT_STYLE_PROP_FONT 2
#define LI_TEXT_STYLE_PROP_TYPE 2
#define LI_TEXT_TYPE_3D 2
#define LI_VI_ATTRIB_FACE_MATERIAL 1
#define LI_VI_CUT_DATA_STORE_FULL 1
#define LI_VI_CUT_DATA_STORAGE 1
#define LI_VI_MERGE_TYPE_SAME_MATERIAL 1
#define LI_VI_PROG_COORD_SYS_SOLID 1
#define LI_VI_PROG_COORD_SYS_WORLD 1
#define LI_VIEW_CURRENT 1
#define LI_VPORT_PROP_MW_IMPORTNT_PRIMS 1
#define LI_VPORT_PROP_MW_OBSTRUCT_STYLE 1
#define LI_VPORT_PROP_MW_ROBJ_POSN_MODE 1
#define LI_VPSP_PROP_MW_IMPORTANT 1
#define LI_VPSP_PROP_MW_ROBJ_POSN_MODE 1
#define LI_VPSP_PROP_MW_ROBJ_POSN_X 1
#define LI_VPSP_PROP_MW_ROBJ_POSN_Y 1
#define LI_VPSP_PROP_MW_SOL_COMPARISON 1
#define LI_VPSP_PROP_MW_VISIBLE 1
#define LI_VPSP_PROP_VI_SOL_CMPAR_SOLID 1

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

FILE *LiFileOpen();

