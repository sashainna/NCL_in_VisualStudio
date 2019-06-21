/*********************************************************************
**    NAME         :  nclxmot.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nclxmot.h , 25.4
**    DATE AND TIME OF LAST  MODIFICATION
**       07/13/15 , 09:21:08
*********************************************************************/
#ifndef NCLXMOTDEF

#define NCLX_GOFWD 704
#define NCLX_GOLFT 705
#define NCLX_GORGT 706
#define NCLX_GOBCK 707
#define NCLX_GOUP  708
#define NCLX_GODWN 709

#define NCLX_TLRGT  1
#define NCLX_TLLFT -1
#define NCLX_TLON   0
#define NCLX_TO    714
#define NCLX_PAST  715
#define NCLX_ON    71
#define NCLX_TANTO 646
#define NCLX_PSTAN 729
#define NCLX_AUTO 0

#define NCLX_SCRUB 780
#define NCLX_LACE 781
#define NCLX_FIXED 782
#define NCLX_SCALLOP 783

typedef enum
{
	NCLX_MOT_TLAXIS_SAME,
	NCLX_MOT_TLAXIS_FIXED,
	NCLX_MOT_TLAXIS_NORMAL,
	NCLX_MOT_TLAXIS_ATANGL,
	NCLX_MOT_TLAXIS_TANTO,
	NCLX_MOT_TLAXIS_FAN,
	NCLX_MOT_TLAXIS_COMBIN,
	NCLX_MOT_TLAXIS_POINT,
	NCLX_MOT_TLAXIS_CURVE,
	NCLX_MOT_TLAXIS_INTERP
} NCLX_mot_tlaxis_type;

typedef enum
{
	NCLX_TOOL_CENTER_OFF,
	NCLX_TOOL_CENTER_ON,
	NCLX_TOOL_CENTER_AUTO
} NCLX_mot_tlaxis_center;

typedef enum
{
	NCLX_CLR_PLANE,
	NCLX_CLR_DISTANCE,
	NCLX_CLR_INCR
} NCLX_mot_clpl_type;

typedef enum
{
	NCLX_RAMP,
	NCLX_PLUNGE,
	NCLX_HELIX,
	NCLX_OMIT
} NCLX_mot_advpock_entry;

typedef enum
{
	NCLX_CLW,
	NCLX_CCLW
} NCLX_mot_advpock_dir;

typedef enum
{
	NCLX_ARC,
	NCLX_SHARP
} NCLX_mot_advpock_corner;

typedef enum
{
	NCLX_WARN,
	NCLX_NOWARN,
	NCLX_AVOID
} NCLX_mot_advpock_warn;

typedef enum
{
	NCLX_TLOFPS,
	NCLX_TLONPS
} NCLX_mot_pscond;

typedef enum
{
	NCLX_P_XAXIS,
	NCLX_P_YAXIS,
	NCLX_P_OFF,
	NCLX_P_POINT,
	NCLX_P_START
} NCLX_mot_return_type;

typedef enum
{
	NCLX_P_VOCAB,
	NCLX_P_VALUE
} NCLX_mot_ppword_type;

typedef enum
{
	NCLX_BOF,
	NCLX_EOF
} NCLX_mot_clrec_pos;

typedef enum
{
	NCLX_LOCK_OFF,
	NCLX_LOCK_OMIT,
	NCLX_LOCK_ON,
	NCLX_LOCK_END
} NCLX_mot_lock_mode;

typedef enum
{
	NCLX_MAXDP_WARN,
	NCLX_MAXDP_NOWARN
} NCLX_mot_maxdp_warn;

typedef struct
{
	double diameter;
	double radius;
	double height;
	double side_angle;
	double zheight;
	double flat_angle;
} NCLX_mot_cutter;

typedef struct
{
	char *start;
	char *end;
	char *current;
	int isn[2];
	int clrecno;
	int type;
	int subtype;
	int mxcl;
	double cldata[420];
} NCLX_mot_clrec;

typedef struct
{
	double ps;
	double ds;
	double cs;
	double cs2;
	double cs3;
	double cs4;
	double cs5;
} NCLX_mot_thick;

typedef struct
{
	double chordal;
	double dsps;
	double cs;
	double start_pt;
	double start_cos;
} NCLX_mot_toler;

typedef struct
{
	double min;
	double max;
	int mauto;
	int step;
	int attempts;
	NCLX_mot_maxdp_warn warn;
} NCLX_mot_maxdp;

typedef struct
{
	int angle_flag;
	double right_angle;
	double fwd_angle;
	int guide_flag;
	NCLX_mdl_data *guide;
	int guide_contact;
	int guide_cond;
	double guide_offset;
	int secps_flag;
	NCLX_mdl_struct *secps;
	int gouge;
	NCLX_mot_lock_mode lock_mode;
	int lock_transition;
	int lock_radius;
	double lock_dist;
	double lock_interp_dist;
} NCLX_mot_tlaxis_modify;

typedef struct
{
	double right_offset;
	double fwd_offset;
	double up_offset;
	double right_tilt;
	double fwd_tilt;
} NCLX_mot_tlaxis_adjust;

typedef struct
{
	NCLX_mot_tlaxis_type mode;
	int normal;
	double vector[3];
	int perpto_flag;
	double perpto[3];
	double angle;
	int contact;
	double heel;
	double height;
	double cmb_depart;
	double cmb_approach;
	int parelm;
	double point[3];
 	NCLX_mdl_curve *curve;
	double curve_dist;
	int adjust_flag;
	NCLX_mot_tlaxis_center center;
	NCLX_mot_tlaxis_modify modify;
	NCLX_mot_tlaxis_adjust adjust;
} NCLX_mot_tlaxis;

typedef struct
{
	int nrpt_flag;
	NCLX_mdl_point *nrpt;
	int nintof;
	int csatt;
	int avoid;
	NCLX_mdl_data *cs;
} NCLX_mot_cs_rec;

typedef struct
{
	int numchk;
	int wchk;
	NCLX_mot_cs_rec *csrec;
} NCLX_mot_cs;

typedef struct
{
	double base_feedrate;
	int mode;
	int accel_flag;
	int accel_type;
	double accel_dist;
	double accel_feedrate;
	int slowdown_flag;
	int slowdown_type;
	double slowdown_dist;
	double slowdown_feedrate;
	double height;
} NCLX_mot_feedrate;

typedef struct
{
	int dsflag;
	double dsvec[3];
	int csflag;
	double csvec[3];
} NCLX_mot_srfvct;

typedef struct
{
	char databag[512];
} NCLX_mot_data;

typedef struct
{
	NCLX_mot_clrec clrange;
	double rad;
	double tol;
	int same;
	double maxang;
	int combine;
	int fedctl;
	double fedrt;
	double fmax;
	int direction;
	double cdia;
	int warn;
} NCLX_mot_fillet;
/*
.....Scrub structure
*/
typedef struct
{
	int numpass;
	int numpts;
	int bounded;
	NCLX_mdl_point boundary[4];
} NCLX_mot_scrub;
/*
.....Rmill structure
*/
typedef struct
{
	int motion_type;
	int profile;
	NCLX_mot_clpl_type clpl_type;
	NCLX_mdl_plane clpl;
	double cldis;
	double pldis;
	int step_type;
	double step_dis;
	double fed;
	double pfed;
	double plfed;
	int ret_type;
	NCLX_mdl_plane retpl;
	double retdis;
	double rough_thick[4];
	double finish_thick[4];
} NCLX_mot_rmill;
/*
.....Advanced pocket structures
*/
typedef struct
{
	int secondary;
	double angle;
	double general;
	double retract;
	double position;
	double entry;
	double transition;
	double finish;
} NCLX_mot_advpocket_fr;

typedef struct
{
	NCLX_mdl_struct *ps;
	NCLX_mot_clpl_type top_type;
	NCLX_mdl_plane top;
	double topdis;
	NCLX_mot_clpl_type clpl_type;
	NCLX_mdl_plane clpl;
	double cldis;
} NCLX_mot_advpocket_pl;

typedef struct
{
	NCLX_tlcond peratt;
	NCLX_mdl_struct *perimeter;
	int num_islands;
	NCLX_tlcond islatt[100];
	NCLX_mdl_struct *island[100];
} NCLX_mot_advpocket_perim;

typedef struct
{
	NCLX_mot_advpock_entry entry;
	int nramp;
	int nrev;
	double ramp_dis;
	double helix_rad;
	NCLX_mot_advpock_warn warn;
	int retract;
	double level_dis;
	double retdis;
	NCLX_arc_dir pocket_dir;
	NCLX_tlcond spiral_dir;
	int section_ret;
	NCLX_mot_advpock_corner corner;
	double step_max;
	double step_min;
	NCLX_mot_advpocket_fr fedrat;
} NCLX_mot_advpocket;
/*
.....Lathe Rough structure
*/
typedef struct
{
	int major;
	int nwds;
	NCLX_mot_ppword_type type[50];
	int ppwrd[50];
	double ppval[50];
} NCLX_mot_post_cmd;

typedef struct
{
	NCLX_mot_return_type type;
	NCLX_mdl_point *pt;
} NCLX_mot_lathe_ret;

typedef struct
{
	double cldist;
	double stock_x;
	double stock_y;
	NCLX_mot_post_cmd *pcmd_depth;
	double depth;
	NCLX_mot_post_cmd *pcmd_cutang;
	double cutang;
	NCLX_mot_post_cmd *pcmd_retrct;
	double retang;
	double retdis;
	NCLX_mot_post_cmd *pcmd_return;
	NCLX_mot_lathe_ret rettyp;
	NCLX_mot_post_cmd *pcmd_final;
} NCLX_mot_lathe_rough;

typedef struct
{
	double stock_x;
	double stock_y;
	int inverse;
	NCLX_mot_post_cmd *pcmd_engage;
	double engang;
	double engdis;
	NCLX_mot_post_cmd *pcmd_retrct;
	double retang;
	double retdis;
	NCLX_mot_post_cmd *pcmd_return;
	NCLX_mot_lathe_ret rettyp;
	NCLX_mot_post_cmd *pcmd_final;
} NCLX_mot_lathe_finish;

typedef struct
{
        int numds;
        int look;
        NCLX_mot_cs_rec *dsrec;
} NCLX_mot_ds;
	
#define NclxMotClRewind(clrec) {(clrec).current = (clrec).start;}
#define NCLXMOTDEF
#endif
