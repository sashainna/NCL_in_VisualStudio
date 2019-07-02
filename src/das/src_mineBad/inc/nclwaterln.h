/*********************************************************************
**    NAME         :  nclwaterln.h
**       CONTAINS: waterline structures
**    COPYRIGHT 2002 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nclwaterln.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:37
*********************************************************************/

#ifndef NCLWATERLN

#include "mcrv.h"
#include "ncl.h"
#include "mdrel.h"
#include "nclfc.h"
#include "mdeval.h"
#include "modef.h"
#include "nccs.h"
#include "mgeom.h"

typedef enum
{
	GEN, HORZPL, TILTPL, VERTPL, VERT, BADPROJ, ABOVE
} ncl_waterline_sftype;

#define MANY_HOLES 1
#define SHADOW_BOX 2
#define VSOLID 3

#define LIST_A 0
#define LIST_B 1
#define LIST_C 2

#define STK_CONTOUR 1
#define STK_BOX 2
#define STK_IGNORE 3
#define STK_BOUNDSF 4

typedef struct
{
	int nb;                    /* number of trim boundaries */
	int npo;                   /* number of outer boundary points */
	int *np;                   /* number of boundary points */
	UU_LIST *cvpts;            /* xyz-boundary points */
} UM_srf_bound;

typedef struct
{
	UU_KEY_ID key;
	ncl_waterline_sftype sf_flag;
	UM_2Dcoord nvec;
	int nlist0;
	int plist0;
	int ncvs;
	int flag1;
	int slist;
	UM_2box box;
	UU_REAL zmin;
	UU_REAL zmax;
	UU_REAL size;
	UU_LIST *trianlist;
	UM_srf_bound bound;
} NCL_waterline_surf;

typedef struct
{
	int inside;
	int depth;
	UU_REAL xrange[2];
	UU_REAL yrange[2];
	int np;
	UM_coord *pt;
} NCL_w_geo;

typedef struct
{
	UM_coord lpt;
	UM_coord cpt;
	int lnj;
	int cnj;
	UU_REAL dsec;
} NCL_w_gap;

typedef struct _NCL_w_arc
{
	int c;
	int j0;
	int j1;
	UU_REAL rad;
	int ccw;
	UM_2Dcoord center;
	struct _NCL_w_arc *next;
} NCL_w_arc;

typedef struct _NCL_dpnode
{
	int itime;
	int cnum;
	int cstk;
	UU_LOGICAL levbetween;
	UU_REAL htop;
	struct _NCL_dpnode *lnext;
	struct _NCL_dpnode *next;
} NCL_dpnode;

typedef struct
{
	UU_KEY_ID key0;
	struct NCL_fixed_databag *srf;
	UM_transf tfmat;
	UU_LOGICAL lrev;
	UM_real8 asw;
	UM_real4 u;
	UM_real4 v;
} NCL_w_base;

typedef struct
{
	int steptype;
	UU_REAL zstep;
	int bottype;
	UU_REAL zbot;
	UU_KEY_ID botkey;
	int toptype;
	UU_REAL ztop;
	UU_KEY_ID topkey;
	UU_KEY_ID frmkey;
	UU_REAL offdis;
} NCL_w_setup;

typedef struct
{
	UU_LOGICAL havestk;
	int finish;
	int conpoc;
	int poctype;
	int frmtype;
	int method;
	UU_REAL z0;
	UU_REAL z1;
	UU_REAL dz;
	UU_REAL dup;
	UU_REAL ddn;
	UU_REAL trad;
	UU_REAL rsq;
	UM_2Dcoord sortpt;
} NCL_w_param;

#define NCLWATERLN

#endif
