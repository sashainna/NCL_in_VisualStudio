/*********************************************************************
**    NAME         :  lipv.h
**
**       CONTAINS:
**				NCLIPV definitions
**
**    COPYRIGHT 2001 (c) NCCS, Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       lipv.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:27
**
*********************************************************************/

#ifndef LIPV
#define LIPV

#include "view.h"
#include "xenv1.h"
#ifdef UU_IPV
#include "lisys/listdio.h"
#include "lisys/listring.h"
#include "li/li.h"
#include "li/lidriver.h"
#include "li/lishader.h"
#include "li/liview.h"
#include "li/mwbase.h"
#include "li/mwrapid.h"
#include "li/mwsldops.h"
#include "li/mwvisi.h"
#include "li/mwutil.h"
#include "li/mwv5axis.h"
#include "li/mwvturn.h"
#include "li/mwvkinem.h"
#include "li/mwclose.h"
#else
#include "lipvstubs.h"
#endif

#include "mdcoord.h"
#include "mpocket.h"
#include "nclmplay.h"
#include "nclvx.h"
#include "ulist.h"

#undef EXT
#ifdef LPGM
#define EXT
#else
#ifdef __cplusplus 
#define EXT extern "C"
#else
#define EXT extern
#endif
#endif

#define LW_VISICUT 0
#define LW_RAPIDCUT 1
#define LW_ACTIVE 0
#define LW_PASSIVE 1
#define LW_MILL 0
#define LW_LATHE 1
#define LW_MILLTURN 2
#define LW_STRINGER 3
#define LW_DIAG_CLASH 0
#define LW_DIAG_ERROR 1
#define LW_MAXCLASH 20
#define LW_MAX_LIGHT 4
#define LW_MAX_HOLDER 24
#define LW_MAX_AXES 30
#define LW_MAX_SPINDLE 10

#define LW_INPROCESS 0
#define LW_STANDALONE 1

LtMaterial ul_ipv_create_material();
void ul_ipv_stock_dir();

typedef enum
{
	LW_NCVERIFY,
	LW_MWORKS
} LW_version_type;

typedef enum
{
	LW_STOCK_BOX,
	LW_STOCK_REVOLVE,
	LW_STOCK_SWEEP,
	LW_STOCK_SESSION,
	LW_STOCK_FILE,
	LW_STOCK_CONE,
	LW_STOCK_CYLINDER,
	LW_STOCK_SPHERE,
	LW_STOCK_TORUS,
	LW_STOCK_COMPOS,
	LW_STOCK_NONE
} LW_stock_type;

typedef enum
{
	LW_CLASH_NONE,
	LW_CLASH_STOCK,
	LW_CLASH_FIXTURE,
	LW_CLASH_TOOL,
	LW_CLASH_HOLDER,
	LW_CLASH_SHANK,
	LW_CLASH_HEAD,
	LW_CLASH_AXIS
} LW_clash_entity;

typedef enum
{
	LW_LIGHT_AMBIENT,
	LW_LIGHT_DISTANT,
	LW_LIGHT_EYE,
	LW_LIGHT_POINT
} LW_light_type;

typedef struct
{
	LW_stock_type type;
	LtSessionPrim stock;
	LtPrim prim;
	int id;
	int color;
	int translucency;
	UU_LOGICAL edge;
	int edge_color;
	UU_LOGICAL important;
	int units;
	UU_LOGICAL mxflag;
	UU_LOGICAL mxchg;
	UU_LOGICAL invflag;
	UU_LOGICAL axes;
	int axes_color;
	LtTransform xform;
	char mxname[NCL_MAX_LABEL+1];
	UU_LOGICAL visible;
	UU_LOGICAL active;
	UU_LOGICAL bin;
	UU_LOGICAL placed;
	UU_REAL toler;
	UM_transf matrix;
	UM_transf invmx;
	UU_REAL *data;
	int axis_seg;
	int tpin;
} LW_stock_struc;

typedef struct
{
	int color;
	int cut_color;
	int hold_color;
	int shank_color;
	int shank_clash;
	UU_LOGICAL edge;
	int edge_color;
	int translucency;
	int use_stock[2];
	int initial;
	UU_REAL toler;
	UU_REAL maxang;
	UU_REAL rapid;
} LW_tool_struc;

typedef struct
{
	UU_REAL auto_toler;
	UU_REAL toler[8];
	UU_REAL sftol;
	UU_REAL maxang;
	UU_REAL maxdis;
	UU_REAL minvol;
	int type;
	int hilite;
	int units;
	int color[9];
	UU_LOGICAL vis;
	int grid;
	UU_LOGICAL grid_flag;
	UU_LOGICAL analyze;
	int translucency;
	UX_pathname file;
} LW_compare_struc;

typedef struct
{
	int fixture;
	int holder;
	int rapid;
} LW_clash_struc;

typedef struct
{
	LtRvPick epick;
	UU_LOGICAL picked;
	LtDouble dis;
	LtNat32 cutn;
	LtDouble norm[3];
} LW_rv_picked_entity;

typedef struct
{
	LW_clash_entity ent[2];
	int axis[2];
	LtDouble loc[3];
	int color[2];
	int isn;
	int type;
	int id[2];
	int errnc[2];
	char errmsg1[80];
	char errmsg2[80];
} LW_clash_rec_struc;

typedef struct
{
	int mode;
	int buf;
	UU_LOGICAL axes;
	UU_LOGICAL swap;
	UU_LOGICAL minimize;
	int shader;
	int bgcolor;
	int grad[2];
	int fcolor[4];
	UM_vector bgrgb;
	UM_vector grgb[2];
	UM_vector frgb[4];
	UX_pathname bgfile;
	int rotate;
	UU_LOGICAL stretch;
	UU_LOGICAL hide_auto;
	UU_LOGICAL hide_edge;
	int hide_lucency;
} LW_display_prop_struc;

typedef struct
{
	UU_LOGICAL active;
	LW_light_type type;
	int color;
	int intensity;
	UM_vector rgb;
	UM_coord position;
	UM_vector direction;
} LW_light_struc;

EXT LtMaterial LW_material[UM_POCKET_COLORS];
EXT int LW_stock_material,LW_fixture_material,LW_tool_material,LW_cut_material;
EXT int LW_nclash,LW_print_num;
EXT UU_LOGICAL LW_clash_flag,LW_delete_stocks,LW_reset_attr[2];
EXT LW_clash_struc LW_clash_material;
EXT LW_clash_rec_struc LW_clash_record[LW_MAXCLASH];
EXT LW_display_prop_struc LW_display_prop;
EXT LW_version_type LW_version;
EXT LW_stock_struc *LW_stock_data[2],*LW_stock_first[2];
EXT LW_stock_struc LW_stock_default[2];
EXT int LW_nstock[2],LW_stock_flag,LW_mach_mode,LW_mach_type,LW_mach_type_flag;
EXT int LW_step,LW_mach_type_main;
EXT UU_LOGICAL LW_active,LW_is_lathe;
EXT int LW_display,LW_stl_format,LW_stl_units;
EXT int LW_rv_accy,LW_translucency[3],LW_stock_idn[2];
EXT UU_REAL LW_toler,LW_maxang,LW_box_expansion,LW_geom_toler,LW_stl_toler;
EXT UU_REAL LW_region[6],LW_tool_zhgt[LW_MAX_SPINDLE];
EXT LtAttributeClass LW_attrib;
EXT LtSessionPrim LW_tool[LW_MAX_SPINDLE],LW_shank[LW_MAX_SPINDLE];
EXT LtSessionPrim LW_holder[LW_MAX_SPINDLE][LW_MAX_HOLDER];
EXT UU_LIST LW_tool_list,LW_tool_sess;
EXT int LW_ntool,LW_cut_colormap[64],LW_n_cut_colormap,LW_cutcolor_index;
EXT int LW_act_tool[LW_MAX_SPINDLE],LW_ntool_sess,LW_tool_sess_act;
EXT int LW_num_holder[LW_MAX_SPINDLE];
EXT LW_tool_struc LW_default_tool;
EXT UN_mot_data *LW_mot_data,*LW_mot_data_first;
EXT UN_mot_attr *LW_mot_attr,*LW_mot_attr_first;
EXT int LW_mot_ndata,LW_mot_nattr;
EXT int LW_highlight_color;
EXT LtColour LW_highlight_value;
EXT UU_REAL LW_tool_pos[6],LW_last_tool_pos[6],LW_tool_limit[5];
EXT UU_LOGICAL LW_auto_reset,LW_tool_from;
EXT UV_vport LW_vport;
EXT LW_compare_struc LW_compare,LW_default_compare;
EXT LtTransform LW_xform_mminch,LW_xform_inchmm;
EXT UU_LOGICAL LW_diff_solid_view;
EXT LtDoubleBounds LW_diff_bounds;
EXT UU_LOGICAL LW_clash_stop[16],LW_clash_log[16],LW_reset_log;
EXT UU_LOGICAL LW_default_stop[16],LW_default_log[16],LW_stl_flag[3];
EXT UU_LOGICAL LW_region_defined;
EXT int LW_errors,LW_interrupt,LW_clash_color;
EXT UX_pathname LW_diag_file;
EXT LtAssembly LW_assemb;
EXT LtSessionPrim LW_part_stock;
EXT LtDoublePoint LW_assemb_pos;
EXT LW_light_struc LW_lights[LW_MAX_LIGHT];
EXT UU_LOGICAL LW_dntcut;
EXT LtDrawable LW_drawable;
EXT LtView LW_view;
EXT LtViewport LW_viewport,LW_so_viewport;

EXT char LW_ipv_title[256];
EXT int LW_ipv_pos[2],LW_ipv_size[2];
EXT int LW_nclipv,LW_dummy;

#ifdef LPGM
int LW_initialized = UU_FALSE;
LtSavedMWEnvironment LW_env=0,LW_session_save=0;
LtSession LW_session[2]={(LtSession)0,(LtSession)0};
LtAssembly LW_lathe=0;
#else
extern int LW_initialized;
extern LtSavedMWEnvironment LW_env,LW_session_save;
extern LtSession LW_session[2];
extern LtAssembly LW_lathe;
#endif

#endif
