
/*********************************************************************
**    NAME         :  nclmplay.h
**     MODULE NAME AND RELEASE LEVEL 
**       nclmplay.h , 25.5
**    DATE AND TIME OF LAST MODIFICATION
**       12/01/15 , 08:24:02
**
**    ** Recompiile 'numot.c' when this file changes **
**
*********************************************************************/

#ifndef NMPLAY
#define NMPLAY

#include "gsegop.h"
#include "mdattr.h"
#include "mfort.h"
#include "modef.h"
#include "mdcoord.h"
#include "nconst.h"
#include "ulist.h"
#include "view.h"
#include "bsym.h"

#ifdef EXT
#undef EXT
#endif
#ifdef NCLPLAYBACK
#define EXT
#else
#define EXT extern
#endif

#define UN_MOTSEG_ACTIVE -1

/*
.....Cutter types
*/
typedef enum
{
	NCL_CUTTER_MILL,
	NCL_CUTTER_LATHE,
	NCL_CUTTER_BLADE
} NCL_cutter_type;
/*
.....Motion playback parameter structure
*/
typedef struct
{
	UU_REAL spt[6];
	UU_REAL cutr[20];
	UU_REAL fedrat[2];
	UU_REAL rpm;
	UU_REAL rcyc[10];
	UU_REAL tracut[12];
	UU_REAL axis[4];
	UU_REAL tlen;
	int cfl[3];
	int ftyp;
	int rapid;
	int npt;
	int icyc[10];
	int cycret;
	int ccmode;
	int ccdir;
	int coolnt;
	int tlno;
	int spdir;
	char sym[3][MAXSYMLEN];
} UN_playparm_struc;
/*
.....Motion extrema zoom box structure
*/
typedef struct
{
	UU_REAL ll[3];
	UU_REAL ur[3];
} UN_mot_vpbox_struc;
/*
.....Cutter definition structure
*/
typedef struct
{
	UM_vector vpn;
	UM_vector vup;
	UU_REAL scale;
	UN_mot_vpbox_struc box;
	int ngeo;
	UU_LIST npt;
	UU_LIST gpt;
	UU_LIST gnorm;
} UN_cutdef_view_struc;

typedef struct
{
	UU_REAL cutr[20];
	UM_vector vfd;
	UU_REAL tlax[3];
	UU_LOGICAL seguse;
	int cutfl[3];
	int color[3];
	int shaded[3];
	int trans[3];
	int segfl;
	int mov;
	char symbol[3][MAXSYMLEN];
	UN_cutdef_view_struc view;
} UN_cutdef_struc;

/* Start of New */

typedef struct
{
	int color;
	int lnstyle;
	int pen;
} UN_motseg_attr;

typedef struct
{
	UU_KEY_ID view[UV_NVPORTS];
	int vinc[UV_NVPORTS];
	int nview;
} UN_motseg_view;
	
typedef struct
{
	UU_REAL parms[10];
} UN_motseg_cutparm;

typedef struct
{
	UU_KEY_ID key;
	int segno;
	int type;
	UU_LOGICAL shaded;
	UU_REAL axis[3];
	UU_LIST curve;
	UU_LIST cnorm;
	char symbol[MAXSYMLEN];
} UN_motseg_symgeo;

typedef struct
{
	UU_REAL parms[4];
	UU_REAL zlim[2];
	int geoptr;
	UN_motseg_symgeo *geo;
} UN_motseg_symbol;

typedef struct
{
	int mov;
	int segfl;
	int shaded[3];
	int color[3];
	int pen[3];
	int trans[3];
} UN_motseg_cutattr;

typedef struct
{
	int cutter;
	int cattr;
	int cutsym;
	int shank;
	int holder;
} UN_motseg_cutter_struc;

typedef struct
{
	UN_motseg_cutparm *cutter;
	UN_motseg_cutattr *cattr;
	UN_motseg_symbol *cutsym;
	UN_motseg_symbol *shank;
	UN_motseg_symbol *holder;
} UN_motseg_cutter;

typedef struct
{
	UM_vector tfwd;
} UN_motseg_blade;

typedef struct
{
	Gwpoint3 tend;
	Gwpoint3 taxis;
	UU_REAL fr_val;
	int fr_mode;
	int attr;
	int cutter;
	int view;
	int blade;
	int isn;
	int mattr;
	int hattr;
} UN_motseg;

typedef struct
{
	int nent;
	int *line;
} UN_motseg_isn;

/* End of New */

/*
.....Cutter definitions structure
*/
typedef struct
{
	NCL_cutter_type type;
	int isn;
	int clrec;
	int color[3];
	int cut_color;
	int edge[3];
	int edge_color[3];
	int trans[3];
	int ncparm;
	int shank_clash;
	int tlno;
	int ctype[3];
	UU_LOGICAL used;
	UU_KEY_ID symkey[3];
/*	char symlib[24]; */
	char symlib[256];
	char symbol[3][MAXSYMLEN];
	UU_REAL cutter[7];
	UU_REAL toler;
	UU_REAL maxang;
	UU_REAL rapid;
	UU_REAL mchtim;
	UU_REAL parms[3][4];
	UU_REAL tlen;
	UU_REAL tlofs;
} UN_cutter_list;
/*
.....Motion attribute list structure
*/
typedef struct
{
	int tlno;
	int loadtl;
	UU_REAL tlen;
	int sp_mode;
	UU_REAL sp_val;
	int coolnt;
	int cc_mode;
	int cc_dir;
} UN_mot_attr;

typedef struct
{
	int isn;
	int seqno;
	int isnptr;
	int cut[2];
	int clrec[2];
	int fr_mode;
	UU_REAL fr_val;
	UN_mot_attr *mattr;
} UN_mot_data;
/*
.....Motion block structure
*/
typedef struct
{
	UU_REAL spt[6];
	UU_REAL ept[6];
	UU_REAL axis[13];
	UM_vector vfwd;
	UU_REAL time;
	int type;
	UU_LOGICAL opskip;
} UN_motion_block;
	
/*
.....Motion display variables
*/
	EXT UN_cutdef_struc cutdef[UV_NVPORTS];
	EXT UN_motseg last_motatt;
	EXT char **UN_mcd_fptr;
#ifdef NCLPLAYBACK
	UN_mot_vpbox_struc mot_vpbox[UV_NVPORTS] =
		{1,1,1,-1,-1,-1, 1,1,1,-1,-1,-1, 1,1,1,-1,-1,-1, 1,1,1,-1,-1,-1,
		1,1,1,-1,-1,-1, 1,1,1,-1,-1,-1, 1,1,1,-1,-1,-1, 1,1,1,-1,-1,-1,
		1,1,1,-1,-1,-1, 1,1,1,-1,-1,-1, 1,1,1,-1,-1,-1, 1,1,1,-1,-1,-1,
		1,1,1,-1,-1,-1, 1,1,1,-1,-1,-1, 1,1,1,-1,-1,-1, 1,1,1,-1,-1,-1,
		1,1,1,-1,-1,-1, 1,1,1,-1,-1,-1, 1,1,1,-1,-1,-1, 1,1,1,-1,-1,-1};
	UU_REAL UN_anlz_feed[10] = {0.,0.,0.,0.,0.,0.,0.,0.,0.,0.};
	int UN_anlz_fcolor[10] = {1,1,1,1,1,1,1,1,1,1};
	int UN_anlz_icolor[5] = {6,7,3,5,4};
	int UN_anlz_istyle[5] = {0,0,0,0,0};
	int UN_anlz_geo[2] = {-1,0};
	int UN_override_geo_attr[3] = {-1,-1,-1};
	int UN_override_geo_mask;
	int UN_override_geo_nkeys=0;
	UU_LIST UN_override_geo_keys;
	int UN_clip_enable = 0;
	int UN_clip_npl = 0;
	int UN_clip_side[5] = {0,0,0,0,0};
	UU_LOGICAL UN_playback_active = UU_FALSE;
	UU_REAL UN_clip_plane[5][4];
	int UN_playfile_start=0,UN_playfile_src=0,UN_playfile_end=0;
	int UN_mcd_nlines=0;
	int UN_unused_geo_flag=0,UN_unused_geo_color=UM_BLUE,UN_keep_stock=0;
	int UN_mot_single_sel=1,UN_mot_auto_preview=1;
#else
	extern UN_mot_vpbox_struc mot_vpbox[UV_NVPORTS];
	extern UU_REAL UN_anlz_feed[10];
	extern int UN_anlz_fcolor[10],UN_anlz_geo[2],UN_anlz_icolor[5];
	extern int UN_anlz_istyle[5];
	extern int UN_override_geo_attr[3],UN_override_geo_mask;
	extern int UN_override_geo_nkeys;
	extern UU_LIST UN_override_geo_keys;
	extern int UN_override_geo_nkeys;
	extern int UN_clip_npl,UN_clip_side[5],UN_clip_enable;
	extern UU_LOGICAL UN_playback_active;
	extern UU_REAL UN_clip_plane[5][4];
	extern int UN_playfile_start,UN_playfile_src,UN_playfile_end;
	extern int UN_mcd_nlines;
	extern int UN_unused_geo_flag,UN_unused_geo_color,UN_keep_stock;
	extern int UN_mot_single_sel,UN_mot_auto_preview;
#endif
/*
.....Motion playback variables
*/

#ifndef CLSTRUC
typedef struct
{
   int isn[2];
   int clrec;
   int type;
   int subt;
   int mxcl;
} UN_clstruc;
#define CLSTRUC
#endif

	EXT UN_playparm_struc UN_playparm;
	EXT UN_clstruc *UN_clfile_start,*UN_clfile_current;
	EXT int UN_clfile_curpt;
	EXT UN_clstruc *UN_last_clpt[2];
	EXT UN_clstruc *UN_clfile_end;
	EXT UM_int2 UN_clfile;
	EXT UU_REAL UN_playvpn[UV_NVPORTS][3],UN_playvup[UV_NVPORTS][3];
	EXT UU_REAL UN_playtax[UV_NVPORTS][3];
	EXT UU_REAL UN_playtax_save[3];
	EXT UN_motseg *UN_step_ptr;
	EXT int UN_motsav_color,UN_motsav_line,UN_rapsav_color,UN_rapsav_line;
	EXT int UN_cutsav_color;
#endif
#undef EXT
