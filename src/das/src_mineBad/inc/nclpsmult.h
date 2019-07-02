/***********************************************
** NAME: nclpsmult.h
** CONTAINS: definitions related to multiple PS
**
**    COPYRIGHT 1998 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nclpsmult.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:36
***********************************************/

#ifndef NCLPSMULTH
#define NCLPSMULTH

typedef struct _NCL_ps_data
{
   UU_REAL t1[6];             /* Tool table (13-18,1) */
   UU_REAL t2[6];             /* Tool table (13-18,2) */
   UU_REAL t3[6];             /* Tool table (13-18,3) */
   UU_REAL s1[10];             /* Surf table s(1-10,1)  */
   UU_REAL cuv[10];           /* Part surface gouge check uv values */
   UU_REAL ad2;               /* reverse normal flag */
} NCL_ps_data;

typedef struct _NCL_psmult_rec
{
   UU_KEY_ID  key;            /* surface key */
   NCL_ps_data ps;
   UU_LOGICAL linit;          /* =1 if initialized, =0 if not initialized */
   int extflg;                /* =1 if point in on extension of surface */
   int iuse;                  /* =1 iff this surface should be used in current 
                                                                       motion */
   int iclose;                /* =1 iff this surface intersects tool cylinder */
   int ierr;                  /* >0 if an error occured for this surface */
   UU_REAL select;            /* Parameter for selecting current PS; */
   UU_KEY_ID  bskey;          /* base surface key if it's a CVonSF */
   UU_REAL cvlen;             /* curve length if it's a CVonSF */
   UM_3D_box box;             /* surface bounding box */
   UM_srf_boundary bound;     /* surface boundary structure */
} NCL_psmult_rec;

typedef struct _NCL_tool
{
	UM_coord end;               /* t(1-3,ia) */
	UM_vector axis;             /* t(4-5,ia) */
   UU_REAL diameter;
   UU_REAL corner_radius;
   UU_REAL height;
   UU_REAL cut_edge;          /* tool(6) */
	UM_coord look_pt;          /* s(8-10,1) table */
	UM_vector look_vec;         /* t(16-18,ia) table */
	UM_vector forward;         /* forward vector perpto taxis */
	UM_vector end_forward;     /* forward vector of tend t(7-9,ia) */
	UM_circle ring;            /* circle of tool look points */
} NCL_tool;

typedef struct _NCL_surf_struct
{
	UM_vector normal;
	UU_REAL distance;
	UM_coord pt;
	UM_coord uv;
	UU_REAL thick;             /* surface thick */
} NCL_surf_struct;

typedef enum
{
	NCL_ON_SURFACE,
	NCL_ON_CORNER,
	NCL_ON_CLOSE_EXTENSION,
	NCL_ON_SIDE_EXTENSION,
	NCL_ON_FAR_EXTENSION,
	NCL_DISCARD_SURFACE,
	NCL_DISCARD_CORNER,
	NCL_DISCARD
} NCL_proj_type;

typedef enum
{
	NCL_TA_SAME,
	NCL_TA_NORMAL_PS,
	NCL_TA_ATANGL_PS,
	NCL_TA_TANTO_DS,
	NCL_TA_FAN,
	NCL_TA_TANTO_DS_PERPTO,
	NCL_TA_TANTO_DS_PARELM,
	NCL_TA_NORMAL_PS_PERPTO,
	NCL_TA_COMBINE,
	NCL_TA_COMBINE_PARELM,
	NCL_TA_ATANGL_PS_PERPTO,
	NCL_TA_ATANGL_PS_DIST,
	NCL_TA_ATANGL_PS_DIST_PERPTO,
	NCL_TA_THRU_PT,
	NCL_TA_THRU_CV,
	NCL_TA_INTERPOL
} NCL_taxis_mode;

/* 
... angle between tool frwd vector and direction to tool look 
... that separates NCL_CLOSE_EXTENSION from NCL_SIDE_EXTENSION
*/
#define NCL_CLOSE_EXT_TILT (UU_REAL) 0.8660254  /* 30 degrees */

/* 
... max angle between a common boundary tangent vector at the 
... end closest to the tool and the plane thru taxis, tool frwd vector:
... if < this angle, extension of the bndry will be included;
... will be not if > this angle ( to avoid extensions which can get
... inside the tool
*/
#define NCL_MAX_BNDRY_TILT (UU_REAL) 0.70710678   /* 45 degrees */
#define NCL_GOUGE 1

#endif
