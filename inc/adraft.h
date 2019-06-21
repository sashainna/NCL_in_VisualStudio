/*********************************************************************
**    NAME         :  adraft.h
**       CONTAINS:
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       adraft.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:10
*********************************************************************/

#ifndef ADRAFTH

#include "usysdef.h"

/*************** enumerated type definitions **************/

enum UA_text_blocks
	{
	main_txt1,main_txt2,tol_txt1,tol_txt2,dia_rad_sym,
	app_cre_txt,app_edit_txt, polar_arc_data
	} ;

enum UA_dvalue_types
	{
	shortest,horiz_dist,vert_dist
	} ;

enum UALORIENT
	{
	LO_HORIZ,LO_HORIZX,LO_LEADER,LO_LEADERX
	} ;

enum UA_dim_spaces
	{
	two_dim,three_dim
	} ;

enum UA_arc_blocks
	{
	ext_arc,dim_arc,geom_arc,misc_arc
	} ;

enum UA_line_blocks
	{
	ext_line,dim_line,geom_line,misc_line
	} ;

/***************** internal record definitions **************/

struct UA_line_data			/* pen data record */
	{
	int		line_font;	/* line_font */
	UU_REAL	line_density;	/* line_density */
	int		color;	/* color */
	} ;

struct UA_txt_blk				/* text block */
	{
	enum UA_text_blocks	subtype;	/* subtype */
	struct UA_line_data	text;	/* text */
	int		char_cnt;	/* char_cnt */
	int		txt_just;	/* txt_just */
	char		fontname[17];	/* fontname */
	char		tstring[1025];	/* tstring */
	UU_REAL	origin[3];	/* origin */
	UU_REAL	dx;	/* dx */
	UU_REAL	dy;	/* dy */
	UU_REAL	slant;	/* slant */
	UU_REAL	tangle;	/* tangle */
	UU_REAL	txt_size;	/* txt_size */
	UU_REAL	sub_super;	/* sub_super */
	UU_REAL	char_expansion;	/* char_expansion */
	UU_REAL	char_space;	/* char_space */
	UU_REAL	line_spacing;	/* line_spacing */
	} ;

struct UA_cpln				/* text plane data */
	{
	UU_REAL	cpln_origin[3];	/* cpln_origin */
	UU_REAL	xaxis[3];	/* xaxis */
	UU_REAL	yaxis[3];	/* yaxis */
	UU_REAL	zaxis[3];	/* zaxis */
	} ;

struct UA_arc_blk				/* arc block */
	{
	enum UA_arc_blocks	subtype;	/* subtype */
	struct UA_line_data	arc;	/* arc */
	int		num_pts;	/* num_pts */
	UU_REAL	center_pt[3];	/* center_pt */
	UU_REAL	radius;	/* radius */
	UU_REAL	angles[50];	/* angles */
	} ;

struct UA_line_blk				/* line block */
	{
	enum UA_line_blocks	subtype;	/* subtype */
	struct UA_line_data	line;	/* line */
	int		num_pts;	/* num_pts */
	UU_REAL	line_seg[50][3];	/* line_seg */
	} ;

struct UA_arrow_blk				/* arrow-head block */
	{
	int		arrow_type;	/* arrow_type */
	struct UA_line_data	arrow;	/* arrow */
	UU_REAL	location[3];	/* location */
	UU_REAL	aangle;	/* aangle */
	UU_REAL	size;	/* size */
	} ;

struct UA_asso_blk				/* association block */
	{
	int		asso_type;	/* asso_type */
	int		modifier;	/* modifier */
	int		key;	/* key */
	UU_REAL	location[3];	/* location */
	} ;

/* internal structure used in the drafting
** subsystem. This is not the same as the UNIBASE representation *******/

struct UA_generic_draft
	{
	int		key;	/* key */
	int		rel_num;	/* rel_num */
	int		etype;	/* etype */
	int		subtype;	/* subtype */
	int		draft_stand;	/* draft_stand */
	int		dims_display;	/* dims_display */
	int		txt_place;	/* txt_place */
	int		entity_site;	/* entity_site */
	int		txt_entry;	/* txt_entry */
	int		appn_text;	/* appn_text */
	int		txt_orent;	/* txt_orent */
	int		txt_just;	/* txt_just */
	int		stack_grid;	/* stack_grid */
	int		linear_units;	/* linear_units */
	int		fract_units;	/* fract_units */
	int		ang_units;	/* ang_units */
	int		units_sym;	/* units_sym */
	int		dim_type;	/* dim_type */
	int		dim_places;	/* dim_places */
	int		dim_zero_sup;	/* dim_zero_sup */
	int		dim_roundoff;	/* dim_roundoff */
	int		tol_places;	/* tol_places */
	int		tol_roundoff;	/* tol_roundoff */
	int		tol_method;	/* tol_method */
	int		tol_site;	/* tol_site */
	int		tol_zero_sup;	/* tol_zero_sup */
	int		dual_format;	/* dual_format */
	int		dual_l_units;	/* dual_l_units */
	int		dual_f_units;	/* dual_f_units */
	int		d_units_sym;	/* d_units_sym */
	int		dual_place;	/* dual_place */
	int		du_tol_pl;	/* du_tol_pl */
	int		dual_a_units;	/* dual_a_units */
	int		d_tol_z_sup;	/* d_tol_z_sup */
	int		d_dim_z_sup;	/* d_dim_z_sup */
	int		d_dim_roundoff;	/* d_dim_roundoff */
	int		d_tol_roff_meth;	/* d_tol_roff_meth */
	int		dia_place;	/* dia_place */
	int		diam_symbol;	/* diam_symbol */
	int		rad_place;	/* rad_place */
	int		rad_symb;	/* rad_symb */
	int		ext_line_sup;	/* ext_line_sup */
	int		lead_orient;	/* lead_orient */
	int		leader_loc;	/* leader_loc */
	int		arrow_place;	/* arrow_place */
	UU_REAL	txt_gap;	/* txt_gap */
	UU_REAL	grid_dist;	/* grid_dist */
	UU_REAL	char_slant;	/* char_slant */
	UU_REAL	char_size;	/* char_size */
	UU_REAL	sub_sup_ratio;	/* sub_sup_ratio */
	UU_REAL	dim_rnd_fact;	/* dim_rnd_fact */
	UU_REAL	d_dim_rnd_fact;	/* d_dim_rnd_fact */
	UU_REAL	tol_rnd_fact;	/* tol_rnd_fact */
	UU_REAL	d_tol_rnd_fact;	/* d_tol_rnd_fact */
	UU_REAL	upper_tol;	/* upper_tol */
	UU_REAL	lower_tol;	/* lower_tol */
	UU_REAL	d_lin_up_tol;	/* d_lin_up_tol */
	UU_REAL	d_lin_lo_tol;	/* d_lin_lo_tol */
	UU_REAL	gap_to_geo;	/* gap_to_geo */
	UU_REAL	ext_past_line;	/* ext_past_line */
	UU_REAL	oblique_angle;	/* oblique_angle */
	UU_REAL	stub_length;	/* stub_length */
	UU_REAL	arrow_size;	/* arrow_size */
	UU_REAL	dim_value;	/* dim_value */
	UU_REAL	dim2_value;	/* dim2_value */
	UU_REAL	dim_origin[3];	/* dim_origin */
	int		xh_pattern;	/* xh_pattern */
	UU_REAL	xh_angle;	/* xh_angle */
	UU_REAL	xh_spacing;	/* xh_spacing */
	int		txt_blk_use;	/* txt_blk_use */
	int		arc_blk_use;	/* arc_blk_use */
	int		line_blk_use;	/* line_blk_use */
	int		arrow_blk_use;	/* arrow_blk_use */
	int		asso_blk_use;	/* asso_blk_use */
	struct UA_cpln	cpln;	/* cpln */
	struct UA_txt_blk	txt_blk[10];	/* txt_blk */
	struct UA_arc_blk	arc_blk[5];	/* arc_blk */
	struct UA_line_blk	line_blk[5];	/* line_blk */
	struct UA_arrow_blk	arrow_blk[10];	/* arrow_blk */
	struct UA_asso_blk	asso_blk[50];	/* asso_blk */
	} ;

/******** predefined constants used in the DRAFTING subsystem **********/

/* associative type */
#define UA_ASSO_TANGPT 1
#define UA_ASSO_PARAM 3
#define UA_ASSO_ENDPT 0
#define UA_ASSO_POINT 5
#define UA_ASSO_BOUNDARY 6
#define UA_ASSO_ISLAND 7
#define UA_ASSO_CENTER 2

/* dimension units */
#define UA_METERS 5
#define UA_M 3
#define UA_ARCH_UNITS 2
#define UA_DEC_DEGREES 0
#define UA_THIN (UU_REAL) 5.000000E-001
#define UA_RADIANS 3
#define UA_ENG_UNITS 1
#define UA_DEG_MIN_SECS 2
#define UA_CMS 2
#define UA_DEG_MIN 1
#define UA_MILLIMETERS 3
#define UA_INCHES 0
#define UA_MM 1
#define UA_CENTIMETERS 4

/* colors */
#define UA_BACKGROUND 0
#define UA_LIGHT_BLUE 15
#define UA_GREEN 10
#define UA_YELLOW 6
#define UA_DARK_BLUE 5
#define UA_WHITE 1
#define UA_BLUE 11
#define UA_CYAN 7
#define UA_DARK_RED 3
#define UA_PINK 13
#define UA_MAGENTA 8
#define UA_RED 9
#define UA_LIGHT_GREEN 14
#define UA_ORANGE 12
#define UA_BLACK 2
#define UA_DARK_GREEN 4

/* positioning flags */
#define UA_TXT_OVER 2
#define UA_ABOVE 0
#define UA_BOTTOM 2
#define UA_MIDDLE_CENTER 4
#define UA_AFTER 3
#define UA_BELOW 1
#define UA_RIGHT 1
#define UA_TOP_CENTER 3
#define UA_LEFT 0
#define UA_MIDDLE_RIGHT 7
#define UA_BOTTOM_CENTER 5
#define UA_TOL_CENTERED 2
#define UA_TOP_RIGHT 6
#define UA_MIDDLE_LEFT 1
#define UA_TXT_UNDER 3
#define UA_MIDDLE 1
#define UA_BEFORE 2
#define UA_TOP_LEFT 0
#define UA_BOTTOM_RIGHT 8
#define UA_TOP 0
#define UA_MEDIUM 1
#define UA_CENTER 2
#define UA_BOTTOM_LEFT 2

/* numeric display flags */
#define UA_FRACT_FULL 1
#define UA_FRACT_3_4 2
#define UA_NO_FRACT 0

/* extension line flags */
#define UA_SUPPRESS_BEFORE 1
#define UA_SUPPRESS_SECOND 2
#define UA_SUPPRESS_BOTH 3
#define UA_SUPPRESS_FIRST 1
#define UA_SUPPRESS_AFTER 2
#define UA_SUPPRESS_NONE 0

/* diameter - radius symbol types */
#define UA_DIA_SYMBOL 1
#define UA_RAD_SYMBOL 2
#define UA_USER_SYMBOL 3
#define UA_PHI_SYMBOL 2
#define UA_R_SYMBOL 1
#define UA_NO_SYMBOL 0
#define UA_USER_DEF 5

/* data block types */
#define UA_ARROWBLOCK_TYPE 4
#define UA_LINEBLOCK_TYPE 3
#define UA_ARCBLOCK_TYPE 2

/* tolerance types */
#define UA_BILAT_ONE_LINE 5
#define UA_DUAL_MAIN 4
#define UA_MAIN_DUAL 3
#define UA_LIMIT_ONE_LINE 1
#define UA_NO_TOL 0
#define UA_BILAT_TWO_LINE 6
#define UA_TOL_ABOVE 0
#define UA_TOL_BELOW 1
#define UA_LIMIT_TWO_LINE 2
#define UA_UNILAT_ABOVE 7
#define UA_UNILAT_BELOW 8
#define UA_LIMIT_LARG_BELOW 4
#define UA_TOL_ALIG_BOT 4
#define UA_LIMIT_LARG_FIRST 3
#define UA_TOL_ALIG_TOP 3

/* dual dimension types */
#define UA_BDUAL_O_MAIN 6
#define UA_MAIN_O_BDUAL 5
#define UA_BDUAL_MAIN 8
#define UA_MAIN_BDUAL 7
#define UA_NO_DUAL 0
#define UA_DUAL_O_MAIN 2
#define UA_MAIN_O_DUAL 1

/* arrow-head styles */
#define UA_TILDE 4
#define UA_ARCH_CROSS 7
#define UA_SINGLE_FILLED 5
#define UA_DOUBLE_CLOSED 6
#define UA_SINGLE_CLOSED 0
#define UA_SINGLE_OPEN 1
#define UA_POINT 3
#define UA_NODE 2
#define UA_ARROWS_OUT 1
#define UA_ARROWS_IN 0
#define UA_REVERSED_CLOSED 8
#define UA_ARC_BACK_FILLED 9

/* dimension type and sub-types */
#define UA_THICK 2
#define UA_BASELINE 1
#define UA_BASIC 1
#define UA_NORMAL 0
#define UA_CHAINED 2
#define UA_BASIC 1
#define UA_ISO_BASE_TRI 8
#define UA_REFERENCE 2
#define UA_LINEAR_DIM		48
#define UA_HORIZ_DIM  1
#define UA_VERT_DIM  2
#define UA_PARAL_DIM  3
#define UA_PERP_DIM  4
#define UA_THICK_DIM  5
#define UA_CYL_DIM			48
#define UA_ARC_LEN_DIM		49
#define UA_ANGULAR_DIM		50
#define UA_ANGULAR_INTERIOR_DIM  1
#define UA_ANGULAR_SUPPLEMENT_DIM  2
#define UA_ANGULAR_COMPLEMENT_DIM  3
#define UA_ANGULAR_REP_INT_DIM     4
#define UA_ANGULAR_REP_COM_DIM     5
#define UA_DRF_ANG_DIM		4
#define UA_X_COORD_DIM		51
#define UA_Y_COORD_DIM		51
#define UA_XY_COORD_DIM  51
#define UA_LABEL_DIM				52
#define UA_NOTE_DIM				53
#define UA_CENTERLINE		54
#define UA_CROSSHATCH		55
#define UA_FANDP_TOL		56
#define UA_SECT_ARROW		57
#define UA_SRF_FIN_SYM		58
#define UA_TITLE_BLOCK		59
#define UA_DRW_BORDERS		60
#define UA_DIA_IN_DIM		61
#define UA_DIA_OUT_DIM		62
#define UA_DIA_ONE_DIM		63
#define UA_RAD_CEN_DIM		64
#define UA_RAD_OUT_DIM		65
#define UA_RAD_LRG_DIM		66
#define UA_CCIRC_DIM		67
#define UA_FLANG_ANG_DIM  68
#define UA_SYM_DIM			69
#define UA_ID_SYMBOL		70
#define UA_CHAIN_DIM		71
#define UA_BASELN_DIM		72
#define UA_POLAR_DIM		73
#define UA_PL_DIM		74
#define UA_BALLOON_DIM		75
#define UA_REPETITIVE_DIM       76

/* text attributes */
#define UA_SYS_TEXT 0
#define UA_AUTOMATIC 0

/* block sizes */
#define UA_NUM_TXTBLKS 10
#define UA_NUM_ARROWBLKS 10
#define UA_MAXCHARS 1024

/* sub-system number */
#define UA_DRAFTING 13

/* drafting standards */
#define UA_ANSI 0
#define UA_ISO 1
#define UA_DIN 2
#define UA_BSI 3
#define UA_JIS 4

/* drafting types */
#define UA_DRAFT_POINT 0
#define UA_DRAFT_LINE  1
#define UA_DRAFT_ARC   2
#define UA_DRAFT_CONIC 3
#define UA_DRAFT_CURVE 4
#define UA_DRAFT_UNKNOWN -1

/* misc. flags */
#define UA_ROUND_DOWN 2
#define UA_GRID_OFF 0
#define UA_ALL_VIEWS 1
#define UA_INDEPENDENT 1
#define UA_ROUND_NEAREST 3
#define UA_RELATED 0
#define UA_TRUNCATE 0
#define UA_POINT_REL 1
#define UA_LINE_REL 2
#define UA_NO_APP_TXT 4
#define UA_OVERIDE 1
#define UA_NUM_LINEBLKS 5
#define UA_TXTBLOCK_TYPE 1
#define UA_DISPLAY 0
#define UA_USER_TEXT 1
#define UA_NUM_ASSOBLKS 50
#define UA_CONTROL 3
#define UA_TXT_HORIZ 0
#define UA_GRID_ON 1
#define UA_NUM_ARCBLKS 5
#define UA_MANUAL 1
#define UA_MEDIUM 1
#define UA_ROUND_UP 1
#define UA_REFERENCE 2
#define UA_CIRCLE_REL 3
#define UA_TXT_IN_DLINE 1
#define UA_VIEW_OF_DEF 0
#define UA_FUZZ (UU_REAL) 1.000000e-004
#define UA_PI (UU_REAL) 3.14159265359
#define UA_HALFPI (UU_REAL) 1.570796326795
#define UA_TWOPI  (UU_REAL) 6.28318530718
#define UA_FALSE 0
#define UA_TRUE 1
#define UA_ALTACTION 2
#define UA_REJECT 0
#define UA_OPCOMPLETE 1
#define UA_ALT_ACTION 2
#define UA_DASUNITLESS 2
#define UA_XH_PATTERNXMAX 12
#define UA_XH_LINEFAMMAX 61
#define UA_XH_DASHMAX 12

#define ADRAFTH
#endif
