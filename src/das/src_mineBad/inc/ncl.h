/*********************************************************************
**    NAME         :  ncl.h
**       CONTAINS: This file contains C structure definitions for
**                   mapping data between NCL and UNIBASE representations.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       ncl.h , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:34
*********************************************************************/

#include "mfort.h"

struct NCLI_vector_rec
   {
   UM_real8 vec[3];              /* coordinates of vector */
   };

struct NCLI_point_rec
   {
   UM_real8 pt[3];               /* coordinates of point */
   };

struct NCLI_line_rec
   {
   UM_real8 spt[3];              /* coordinates of start point of line */
   UM_real8 ijk[3];              /* vector defining direction and length */
   };

struct NCLI_circle_rec
   {
   UM_real8 center[3];           /* center of circle */
   UM_real8 normal[3];           /* unit normal to plane of circle; direction
                                    of travel around circle (arc) is defined
                                    by right hand rule */
   UM_real8 radius;              /* radius of circle */
   UM_real8 ijk[3];              /* unit normal to plane perpendicular to plane
                                    of circle and going thru start and end
                                    points of circular arc; points towards
                                    interior of arc */
   UM_real8 dist;                /* distance of origin to plane defining
                                    end points of arc */
   };

struct NCLI_plane_rec
   {
   UM_real8 ijk[3];              /* unit normal to plane */
   UM_real8 dist;                /* distance of origin to plane */
   };

struct NCLI_matrix_rec
   {
   UM_real8 mat[3][4];        /* */
   UM_real8 dalen;
   UM_real8 dbox[3];
   };

struct NCLI_mxdisp_rec
   {
   UM_real8 dalen;
   UM_real8 dbox[3];
   };

struct NCLI_crvhead_rec {
   UM_real4 nosegs;              /* number of segments in curve */
   UM_real4 param[19];           /* parameter values at start of each 
                                    segment */
   };

struct NCLI_crvseg_rec {
   UM_real8 point[3];            /* start point of segment */
   UM_real4 delta[3];            /* vector from start point to next point */
   UM_real4 duds0;               /* slope at beginning of segment */
   UM_real4 duds1;               /* slope at end of segment */
   UM_real4 rho;                 /* ratio of length of 3rd leg of Bezier segment
                                    to the 1st leg of next segment */
   };

struct NCLI_srfhead_rec {
   UM_int2 surftype;             /* type: 25 => quilt
                                          26 => mesh
                                          91 => regular */
   UM_int2 numpanels;            /* number of panels */
   UM_int2 notused[2];           /* not used */
   UM_int2 offset;               /* 0 => not offset; 7 => offset */
   UM_int2 notused1;             /* not used */
   UM_real4 offdist;             /* offset distance */
   UM_int2 numupaths;            /* number of u paths to draw */
   UM_int2 numvpaths;            /* number of v paths to draw */
   UM_int2 ptsperucrv;           /* number of points per u path */
   UM_int2 ptspervcrv;           /* number of points per v path */
   };

struct NCLI_srfpanel_rec {
   UM_int2  paneltype;           /* type: 0 => 28 point patch
                                          1 => 14 point patch */
   UM_int2  numpatch;            /* number of patches in panel */
   UM_int2  notused[6];          /* not used */
   UM_real4 param[10];           /* parameter values */
   };

struct NCLI_srfpatch_rec {
   UM_real8 point[3];            /* start point */
   UM_real4 delta[28][3];        /* delta values */
   };

struct NCLI_meshhead_rec {
   UM_int2 surftype;             /* type of mesh surface = 26 */
   UM_int2 numpatches;           /* number of patches = m*n */
   UM_int2 m;                    /* number of patches per row */
   UM_int2 n;                    /* number of rows*/
   UM_int2 offset;               /* 0 => not offset; 7 => offset */
   UM_int2 notused1;             /* not used */
   UM_real4 offdist;             /* offset distance */
   UM_int2 numupaths;            /* number of u paths to draw */
   UM_int2 numvpaths;            /* number of v paths to draw */
   UM_int2 ptsperucrv;           /* number of points per u path */
   UM_int2 ptspervcrv;           /* number of points per v path */
   };

struct NCLI_meshpatch_rec {
   UM_real8 point[3];            /* */
   UM_real4 delta[15][3];        /* */
   UM_real4 notused;             /* */
   };

struct NCLI_quilthead_rec {
   UM_int2  surftype;            /* type of quilt surface = 25 */
   UM_int2  numpatches;          /* number of patches */
   UM_int2  notused[2];          /* not used */
   UM_int2 offset;               /* 0 => not offset; 7 => offset */
   UM_int2 notused1;             /* not used */
   UM_real4 offdist;             /* offset distance */
   UM_real4 midpt[12][3];        /* */
   };

struct NCLI_quiltpatch_rec {
   UM_int2  bpatchnum[4];        /* boundary patch numbers */
   UM_real4 origin[3];           /* */
   UM_real4 xvalues[25];         /* */
   UM_real4 yvalues[25];         /* */
   UM_real4 zvalues[25];         /* */
   };

struct NCLI_pnhead_rec
   {
   UM_int2 numpts;               /* Number of points in patern */
   UM_int2 notused1[3];          /* Not used */
   };

struct NCLI_pnpoint_rec
   {
   UM_real8 pt[3];               /* coordinates of point */
   };

struct NCLI_netsf_rec
   {
   UM_int2  surftype;            /* type of net surface = 27 */
   UM_int2  numsfs;              /* number of surfs */
   UM_int2  notused[2];          /* not used */
   UM_int4  sfkeys[40];          /* unibase keys of component sfs */
   UM_int2  bndsfs[40][4];       /* index of boundary surfs */
   };

struct NCLI_scalar_rec
   {
   UM_real8 scalar_value;        /* scalar value */
   };

struct NCLI_pointvec_rec
   {
   UM_real8 pt[3];              /* coordinates of start point of pointvec */
   UM_real8 ve[3];              /* vector of pointvec */
   };

/* internal NCL type identifiers */

#define NCLI_SCALAR        2
#define NCLI_POINT         3
#define NCLI_VECTOR        4
#define NCLI_LINE          5
#define NCLI_PLANE         6
#define NCLI_CIRCLE        7
#define NCLI_CURVE         8
#define NCLI_SURF          9
#define NCLI_MATRIX        10
#define NCLI_LABEL         13
#define NCLI_RESERVED_ID   14
#define NCLI_SHAPE         18
#define NCLI_PATERN        20
#define NCLI_POINTVEC      21
#define NCLI_POCKET        22
#define NCLI_MESHSURF      26
#define NCLI_QUILTSURF     25
#define NCLI_NETSF         27
#define NCLI_EVALSF        28
#define NCLI_RBSF          29
#define NCLI_REGSURF       91
#define NCLI_REVSURF       100
#define NCLI_DATAST        23
#define NCLI_UVCVONSF      22
#define NCLI_TEXTVAR       24
#define NCLI_NOTE 	      30
#define NCLI_SYMBOL 	      31
#define NCLI_INSTANCE      32
#define NCLI_SOLID	      33

/* special primitive types */
typedef enum _nclsf_prim_type
{
NCLSF_UNKNOWN,
NCLSF_FREEFORM,
NCLSF_RULED,
NCLSF_PLANE,
NCLSF_SPHERE,
NCLSF_CYLINDER,
NCLSF_CONE,
NCLSF_REVOLV,
NCLSF_TORUS
} nclsf_prim_type;
