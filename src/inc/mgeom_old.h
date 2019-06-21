/********************************************************************
**
** NAME: mgeom.h
** CONTAINS: geometric definitions
**
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       mgeom.h , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:30
*********************************************************************/

#ifndef MGEOMH
#define MGEOMH

#include "umath.h"
#include "mdcoord.h"
#include "modef.h"
#include "ulist.h"

typedef struct _UM_line
{
	UM_vector n;          /* unit vector along the line */
	UM_coord  p0;         /* arbitrary point on the line */
} UM_line;

#define UM_3DBOX_NUM_VERT   8       /* number of vertices of a 3D-box */
#define UM_3DBOX_NUM_EDGES  24      /* number of edges of a 3D-box */
#define UM_3DBOX_NUM_FACES  18      /* number of faces of a 3D-box */

/* pairs of vertices defining 12 edges of the box */

static int UM_3DBOX_EDGES[UM_3DBOX_NUM_EDGES] =
{ 0,1,  1,2,  2,3,  3,0,  4,5,  5,6,  6,7,  7,4,  0,4,  1,5,  2,6,  3,7 };

/* triplets of vertices defining 6 faces of the box */

static int UM_3DBOX_FACES[UM_3DBOX_NUM_FACES] =
	{ 0,1,3,  0,1,4,  1,2,5,  3,2,7,  0,3,4,  4,5,7 };

typedef struct _UM_3D_box 				/* parallelepiped */
{
	UM_coord ver[UM_3DBOX_NUM_VERT];	/* 8 vertices of a box */
} UM_3D_box;

typedef struct _UM_2D_box     /* parallelogram */
{
	UM_coord p0, p1, p2, p3;   /* vertices; p0 is lower left vertex */
	UU_REAL width, height;     /* lengths of (p0,p1) and (p0,p2) */
	UU_REAL	cosa, sina;       /* cos and sin of angle between (p0,p1) and (p0,p2) */
} UM_2D_box;

typedef struct _UM_cylinder 	/* infinite cylinder  */
{
	UM_line axis ;					/* cylinder axis */
	UU_REAL radius;
} UM_cylinder;

typedef struct _UM_plane
{
	UM_vector n;              /* unit vector normal to plane */
	UM_coord  p0;             /* arbitrary point on the plane */
} UM_plane;

typedef struct _UM_circle
{
	UU_REAL radius;
	UM_coord center;
	UM_vector n;              /* unit vector normal to circle plane */
} UM_circle;

typedef struct _UM_segment
{
	UM_coord p1;			/*the first point*/
	UM_coord p2;			/*the second point*/
} UM_segment;

/*                      BOUNDARY DEFINITIONS                           */

typedef struct _UM_srf_boundary
{
	UU_REAL toler;             /* tolerance for evaluating surface boundaries */
	int nb;                    /* Number of trim boundaries */
	int *np;                   /* number of boundary points */
	UM_2Dcoord *ummx;          /* u-v bounding  */
	UM_2Dcoord *vmmx;          /* boxes for each trim boundary  */

	UU_LIST *uvpts;             /* uv-boundary points */
	UU_LIST *cvpts;             /* xyz-boundary points */
} UM_srf_boundary;

typedef struct _UM_netsf_common_bndry
{
	int key;                   /* Unibase key of net surface */
	int num;                   /* number of common boundaries of net surface */
	UU_LIST *surfaces;         /* list of pairs of intersecting surfaces */
	                           /* (one pair for each common boundary */
	UU_LIST *np;               /* np[i] = # of points of i-th common boundary */
	UU_LIST *lengths;          /* list of lengths of all common boundaries */
	UU_LIST *pts;              /* list of points of all common boundaries */
} UM_netsf_common_bndry;

UU_REAL ncl_get_boundary_toler();

/*                      TESSELLATION DEFINITIONS                           */

typedef struct _UM_triangle
{
/*
... 3 vertices of triangle
*/
	UM_coord  p1;
	UM_coord  p2;
	UM_coord  p3;
/*
... unit vectors normal to surf. at p1,p2,p3
*/
	UM_vector norm1;
	UM_vector norm2;
	UM_vector norm3;
} UM_triangle;

typedef struct _UM_trian
{
/*
... 3 vertices of triangle
*/
	UM_coord  p1;
	UM_coord  p2;
	UM_coord  p3;
} UM_trian;

typedef struct _UM_tript
{
	UU_REAL  n1;
	UU_REAL  n2;
	UU_REAL  n3;
} UM_tript;

/*
... aak 30-sep-1998: sizeof(UM_edge) MUST be an integer of sizeof(UU_REAL);
... this is used in tess. storing/retrieving to/from Unibase
*/
/*
......changed from all integers to all UU_REAL
......because this structure will be saved use sizeof(UU_REAL)
......when save in *.ud, it will have problems to read back
......so changed to use all UU_REAL
......Yurong  3/4/99
*/
typedef struct _UM_edge
{
	UU_REAL  south;              /* # of starting (south) point of an edge */
	UU_REAL  north;              /* # of end (north) point of an edge */
/*
...4 edges adjacent to this edge in CCW order
...starting from start point
*/
	UU_REAL  s_e;                /* south-east */
	UU_REAL  n_e;                /* north-east */
	UU_REAL  n_w;                /* north-west */
	UU_REAL  s_w;                /* south-west */
} UM_edge;

typedef struct _UM_tess1
{
	UU_REAL toler;                /* tessellation tolerance */
	int key;                      /* surface key */
	int np;                       /* num. of tess. vertices */
	int nedges;                   /* num. of tess. edges */
	UU_LIST vertices;             /* list of tess. vertices */
	UU_LIST normals;              /* list of surface normals at vertices */
	UU_LIST uv;                   /* list of uv-params at vertices */
	UU_LIST edges;                /* list of tess. edges  */
} UM_tess1;

typedef struct _UM_tessellation
{
	UU_REAL toler;                /* tessellation tolerance */
	int np;                       /* num. of tess. vertices */
	int ntri;                     /* num. of tess. triangles */
	UU_LIST vertices;             /* list of tess. vertices */
	UU_LIST normals;              /* list of surface normals at vertices */
	UU_LIST tri;                  /* list of triangles */
} UM_tessellation;

typedef enum
{
	UM_2D_TESS,
	UM_3D_TESS
} UM_polygon_tess_type;

typedef enum
{
	UM_TESS_TOLER,
	UM_TESS_GRID,
	UM_TESS_BOTH,
	UM_TESS_WATRLN
} UM_tess_settype;

typedef struct _UM_tess_setting
{
	UM_tess_settype typ;
	UU_REAL tol;
	int kupts;
	int kvpts;
} UM_tess_setting;

typedef struct _UM_polyln_pt
{
	UM_2Dcoord p; /* point on polyline */
	int iloop;    /* # of closed loop (if polyline has several closed loops) */
	int ind;      /* # of polyline knot closest to the point; */
} UM_polyln_pt;

/*
... tolerance for a srf. boundary evaluation
*/
#define UM_BOUNDARY_TOLER (UU_REAL) 1.e-4
/*
... max number of edges a tessellation can have
*/
#define UM_MAX_TESS_EDGES 15000

typedef UU_REAL NCL_xplane[4];
/*
.....Triangle structure used in calculation of minimum distance/intersection
.....of triangles.
*/
typedef struct _NCL_xtriangle
{
	UM_coord p1;
	UM_vector v1;
	NCL_xplane pl1;
	UM_coord p2;
	UM_vector v2;
	NCL_xplane pl2;
	UM_coord p3;
	UM_vector v3;
	NCL_xplane pl3;
	NCL_xplane pln;
	UU_LOGICAL valid;
} NCL_xtriangle;

void ncl_setup_xtriangles();

UU_REAL um_get_tess_toler();

#define PREV_INDEX(i, n) ((i - 1 + n) % n)
#define NEXT_INDEX(i, n) ((i + 1) % n)

#define UU_FREE(p) {if (p) {uu_free(p); (p)= UU_NULL;}}
#define UU_LIST_FREE(p) {if (p) {uu_list_free(p); UU_FREE(p);}}

typedef struct _ncl_ioseg
{
	struct _ncl_ioseg *next;
	UM_coord pt1;
	UM_coord pt2;
} ncl_ioseg;

typedef struct {
	UM_pointd uv;   /*intersection uv point*/
	UM_coord pt;    /*intersection pt point*/
} UM_cvio;

typedef struct
{
	UM_coord pte;  /*cl point*/
	UM_vector vta; /*cl vector*/
} UM_clpt;

#endif
