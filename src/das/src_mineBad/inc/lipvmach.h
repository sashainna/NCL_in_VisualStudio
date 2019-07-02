/*********************************************************************
**    NAME         :  lipvmach.h
**
**       CONTAINS:
**				NCLIPV Machine Simulation definitions.
**
**    COPYRIGHT 2001 (c) NCCS, Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       lipvmach.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:27
**
*********************************************************************/

#ifndef LIPVMACH
#define LIPVMACH

#include "lipv.h"
#include "mdcoord.h"
#include "nsimulate.h"
#include "xenv1.h"

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

#define NMACHDAT 37
#define NMACHMDL 31
#define UNIT 13
#define MTBF 17
#define MACHINEAGE 33
#define LW_MAX_TURRET 32

typedef enum
{
	LW_MACH_NOTYPE,
	LW_MACH_LINEAR,
	LW_MACH_ROTARY
} LW_mach_axis_type;

typedef enum
{
	LW_MACH_TOOL,
	LW_MACH_HOLDER,
	LW_MACH_HEAD,
	LW_MACH_AXIS,
	LW_MACH_STOCK,
	LW_MACH_FIXTURE
} LW_mach_axis_style;

typedef struct
{
	UM_vector vec[3];
	UU_REAL rary[16];
	char desc[18][80];
} LW_mach_data_struc;

typedef struct
{
	char name[80];
	LW_stock_struc stock;
} LW_mach_solid_struc;

typedef struct
{
	UU_REAL parms[8];
	UU_REAL axis[LW_MAX_TURRET][6];
	int iparm[2];
	int naxis;
	int toolno[LW_MAX_TURRET];
	int nholder[LW_MAX_TURRET];
	LtSessionPrim tool_prim[LW_MAX_TURRET],shank_prim[LW_MAX_TURRET];
	LtSessionPrim holder_prim[LW_MAX_TURRET][LW_MAX_HOLDER];
} LW_mach_turret_struc;

typedef struct
{
	int units;
	char name[80];
	char axisname[20];
	char desc[3][80];
	UU_REAL axis[6];
	UU_REAL modaxis[6];
	UU_REAL position;
	UU_REAL last_pos;
	UU_REAL home;
	UU_REAL offset;
	UU_REAL baseofs;
	UU_REAL scale;
	UU_REAL parms[8];
	int color;
	int translucency;
	int visible;
	UU_LOGICAL edge;
	int edge_color;
	int beg_solid;
	int end_solid;
	int parent;
	LW_mach_axis_type type;
	LW_mach_axis_style style;
	int reverse;
/*	LtPref solid;*/
	LtAssembly assembly;
	LW_mach_turret_struc *turret;
} LW_mach_model_struc;

typedef struct
{
	UM_coord mpin[3];
	UM_coord ppin[3];
	UM_transf matrix;
	UM_transf invmx;
	int axis;
	char label[20];
} LW_mach_toolpin_struc;

typedef struct
{
	UU_REAL offset[LW_MAX_AXES];
	int reg;
	int nofs;
	char label[LW_MAX_AXES][20];
} LW_offsets_struc;

EXT UU_LOGICAL LW_mach_simul,LW_mach_defined;
EXT UU_LOGICAL LW_mach_clash_stop[16],LW_mach_clash_log[16];
EXT UX_pathname LW_mach_name,LW_mach_dir;
EXT LW_mach_data_struc LW_mach_data;
EXT LW_stock_struc LW_mach_stock;
EXT UU_LIST LW_mach_model;
EXT UU_LIST LW_mach_solid;
EXT int LW_mach_nmodel,LW_mach_nsolid,LW_mach_tpin_ix,LW_mach_num_tpin;
EXT UU_LIST LW_mach_toolpin,LW_register_offsets[2];
EXT UN_sim_machine LW_mach_desc;
EXT int LW_mach_axes[LW_MAX_AXES],LW_mach_naxes,LW_mach_max_axes;
EXT int LW_spindle[LW_MAX_SPINDLE],LW_spindle_ix[LW_MAX_SPINDLE];
EXT int LW_spindle_load[LW_MAX_SPINDLE],LW_spindle_nload,LW_spindle_num;
EXT int LW_offset_mode[2],LW_offset_reg[2];
EXT UU_LOGICAL LW_spindle_vis[LW_MAX_SPINDLE];

#ifdef LPGM
char LW_mach_desc_opt[NMACHDAT][20] = {
	"MachineName", "Family", "Type", "SubType", "Number", "Description",
	"Code", "WorkCell", "Site", "Plant", "Manufacturer", "ControllerName",
	"ControllerConfig", "Unit", "AxisNames", "ToolChangeType", "ToolLibraryName",
	"MTBF", "TotalLength", "TotalWidth", "TotalHeight", "EnvelopeLength",
	"EnvelopeWidth", "EnvelopeHeight","Weight", "NumberOfTools", "MaxToolDia",
	"MaxAdjToolDia", "MaxToolLength", "ToolChangeTime", "MaxToolWeight",
	"HourlyCost", "OperatorCost", "MachineAge", "ToolChangeAxis",
	"ViewFrontVector", "ViewUpVector"};

char LW_mach_model_opt[NMACHMDL][20] = {
	"MachineName", "Unit", "AxisName", "Color", "Rotate", "Translate", "Join",
	"AxisLine", "AxisPosition", "ToolingPin", "ToolSpindle", "AxisOffset",
	"Visible", "Translucency", "Reverse", "AxisType", "MaxTravel", "MinTravel",
	"MaxFeed", "MinFeed", "Accuracy", "Repeatability", "MaxAcceleration",
	"MaxDeceleration", "Home", "Description", "FeedbackSystem", "ControlSystem",
	"EdgeDisplay", "ToolTurret", "AxisScale"};

#else
extern char LW_mach_desc_opt[NMACHDAT][20];
extern char LW_mach_model_opt[NMACHMDL][20];
#endif

#endif
