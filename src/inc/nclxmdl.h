/*********************************************************************
**    NAME         :  nclxmdl.h
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nclxmdl.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:38
*********************************************************************/

#ifndef NCLGEODEF

#define NCLX_int    int
#define NCLX_KEY_ID unsigned int
#define NCLX_PANEL_BUFSZ 7000

typedef void NCLX_mdl_ptr;

typedef enum
{
	NCLX_MDL_UNDEF,
	NCLX_MDL_REAL,
	NCLX_MDL_CIRCLE,
	NCLX_MDL_CONIC,
	NCLX_MDL_COMPOSITE,
	NCLX_MDL_CURVE,
	NCLX_MDL_DATA,
	NCLX_MDL_LINE,
	NCLX_MDL_MATRIX,
	NCLX_MDL_PATERN,
	NCLX_MDL_PLANE,
	NCLX_MDL_POINT,
	NCLX_MDL_PNTVEC,
	NCLX_MDL_BSPLINE,
	NCLX_MDL_NSURF,
	NCLX_MDL_SCALAR,
	NCLX_MDL_SURF,
	NCLX_MDL_TRIMSF,
	NCLX_MDL_VECTOR,
	NCLX_MDL_NETSF,
	NCLX_MDL_POLYLINE,
	NCLX_MDL_SHAPE,
	NCLX_MDL_REVSURF
} NCLX_mdl_type;

typedef enum
{
	NCLX_BASE,
	NCLX_FACE
} NCLX_mdl_trim_type;

typedef enum
{
	NCLX_S_ENDPT,
	NCLX_S_ARC,
	NCLX_S_DIRECTION,
	NCLX_S_VOCAB,
	NCLX_S_VALUE
} NCLX_shape_type;

typedef enum
{
	NCLX_P_LEFT,
	NCLX_P_RIGHT
} NCLX_tldir;

typedef enum
{
	NCLX_P_CLW,
	NCLX_P_CCLW
} NCLX_arc_dir;

typedef enum
{
	NCLX_P_IN,
	NCLX_P_OUT,
	NCLX_P_ON
} NCLX_tlcond;

typedef struct
{
	int color;
	int layer;
	int pen;
	int line_style;
	double line_weight;
	int visible;
	int label_on;
	int displayable;
} NCLX_mdl_attr;

typedef struct
{
	NCLX_KEY_ID key;
	NCLX_mdl_type relnum;
	NCLX_mdl_attr attrib;
	char label[64];
	int subscript;
	double label_pos[3];
} NCLX_mdl_struct;

typedef struct
{
	NCLX_mdl_struct header;
	double spt[3];
	double ept[3];
} NCLX_mdl_line;

typedef struct
{
   NCLX_mdl_struct header;
	int npts;
   double *pts;
} NCLX_mdl_polyline;

typedef struct
{
	NCLX_mdl_struct header;
	double pt[3];
	double vec[3];
	double dist;
} NCLX_mdl_plane;

typedef struct
{
	NCLX_mdl_struct header;
	double pt[3];
	double vec[3];
} NCLX_mdl_pntvec;

typedef struct
{
	NCLX_KEY_ID key;
	double pt[3];
	double normal[3];
	double udrv1[3];
	double vdrv1[3];
	double udrv2[3];
	double vdrv2[3];
	double ucrv;
	double vcrv;
} NCLX_mdl_surf_eval;

typedef struct
{
	double pt[3];
	float delta[7][3];
	float rho;
} NCLX_mdl_patch;

typedef struct
{
	NCLX_KEY_ID key;
	int idum1;
	char cdum[8];
	int idum2;
	int type;
	int nparm;
	float param[50];
	int npatch;
	NCLX_mdl_patch *patch;
	char varlist[NCLX_PANEL_BUFSZ];
} NCLX_mdl_panel;

typedef struct
{
	int material;
	int upaths;
	int vpaths;
	int upts;
	int vpts;
	int urld;
	int vrld;
	int reverse;
	int uclosed;
	int vclosed;
	double eval[2];
	int offset;
	float offdist;
	int type;
	int udegree;
	int vdegree;
	int udegseg;
	int vdegseg;
	int shaded;
	int lucency;
} NCLX_mdl_sfhead;

typedef enum
{
	NCLX_UNKNOWN,
	NCLX_FREEFORM,
	NCLX_RULED,
	NCLX_PLANE,
	NCLX_SPHERE,
	NCLX_CYLINDER,
	NCLX_CONE
} NCLX_prim_type;

typedef struct
{
	NCLX_mdl_struct header;
	NCLX_mdl_sfhead sfhead;
	void *evaluator;
	int npanel;
	NCLX_mdl_panel *panel;
	NCLX_prim_type  primitive;
	double prim_param[16];
	int ntu;
	double *tu;
	int ntv;
	double *tv;
	int npt;
	double *pt;
	int nwgt;
	double *wgt;
   int   no_boxlst;
   double *boxlst;
   int   no_bndrylst;
   double *bndrylst;
} NCLX_mdl_surf;

typedef struct
{
   NCLX_mdl_struct header;
   NCLX_mdl_sfhead sfhead;
   void *evaluator;
   NCLX_mdl_ptr *curve;
   double pta[3];
   double vca[3];
   double sa;
   double ta;
   NCLX_prim_type  primitive;
   double prim_param[16];
   int   no_boxlst;
   double *boxlst;
} NCLX_mdl_revsurf;

typedef struct
{
   NCLX_mdl_struct header;
	NCLX_mdl_surf *surf;
	NCLX_mdl_ptr *uv_cv;
	NCLX_mdl_ptr *xyz_cv;
	int ncurve;
	NCLX_mdl_ptr **inner;
	double offdist;
	double u_min;
	double u_max;
	double v_min;
	double v_max;
	NCLX_mdl_trim_type trim_type;
   int   no_boxlst;
   double *boxlst;
   int   no_bndrylst;
   double *bndrylst;
} NCLX_mdl_trimsf;

typedef struct
{
   double pt[3];
   double udrv1[3];
   double udrv2[3];
	double ucrv;
} NCLX_mdl_curve_eval;

typedef struct
{
	double point[3];
	float delta[3];
	float duds0;
	float duds1;
	float rho;
} NCLX_mdl_curve_segment;

typedef struct
{
   int closed;
	int degree;
	int degseg;
   double eval;
	double t0;
	double t1;
	double tlen;
} NCLX_mdl_cvhead;

typedef struct
{
   NCLX_mdl_struct header;
	NCLX_mdl_cvhead cvhead;
   void *evaluator;
	int nparm;
	float *parms;
	int ntparm;
	double *tparms;
	int npt;
	double *pt;
	int nwgt;
	double *wgt;
	int nsegment;
	NCLX_mdl_curve_segment *segment;
} NCLX_mdl_curve;

typedef struct
{
   int closed;
	double length;
   double eval;
	int planar;
	int continuity;
	int fcolor;
} NCLX_mdl_cmphead;

typedef struct
{
	NCLX_mdl_ptr *curve;
	int reverse;
	double endparam;
} NCLX_mdl_cmp_entity;

typedef struct
{
   NCLX_mdl_struct header;
	NCLX_mdl_cmphead cvhead;
   void *evaluator;
	int ncurve;
	NCLX_mdl_cmp_entity *cvid;
} NCLX_mdl_composite;

typedef struct
{
   NCLX_mdl_struct header;
   double pt[3];
} NCLX_mdl_point;

typedef struct
{
   NCLX_mdl_struct header;
	NCLX_mdl_type pntype;
	int npts;
   double *pts;
} NCLX_mdl_patern;

typedef struct
{
   NCLX_mdl_struct header;
   double  nvec[3];
   double  center[3];
   double  radius;
   double  svec[3];
   double  dang;
} NCLX_mdl_circle;

typedef struct
{
	double cen[2];
	double rad;
	double sang;
	double eang;
	NCLX_arc_dir dir;
} NCLX_mdl_shp_arc;

typedef struct
{
	NCLX_shape_type type;
	NCLX_arc_dir dir;
	int vocab;
	double value;
	double pt[2];
	NCLX_mdl_shp_arc arc;
} NCLX_mdl_shp_entity;

typedef struct
{
   NCLX_mdl_struct header;
	NCLX_tlcond side;
	NCLX_tldir dir;
	int nents;
	NCLX_mdl_shp_entity *shid;
} NCLX_mdl_shape;

typedef struct
{
   NCLX_mdl_struct header;
   int nsf;
   NCLX_mdl_ptr **sfptr;
} NCLX_mdl_netsf;

typedef struct
{
   union
   {
      NCLX_mdl_struct header;
      NCLX_mdl_point  pt;
      NCLX_mdl_patern  pn;
      NCLX_mdl_line   ln;
      NCLX_mdl_plane  pl;
      NCLX_mdl_pntvec pv;
      NCLX_mdl_circle ci;
      NCLX_mdl_curve  cv;
      NCLX_mdl_composite  cp;
      NCLX_mdl_shape  sh;
      NCLX_mdl_surf   sf;
      NCLX_mdl_trimsf tf;
      NCLX_mdl_netsf  nsf;
      NCLX_mdl_polyline  py;
   } data;
} NCLX_mdl_data;

typedef enum
{
	NCLX_BOX_LIST,
	NCLX_BOUNDARY_LIST
} NCLX_mdl_srflist_type;


#define NclxMdlGetColor(rec,color) {color = (rec).header.attrib.color;}
#define NclxMdlGetLayer(rec,layer) {layer = (rec).header.attrib.layer;}
#define NclxMdlGetPen(rec,pen) {pen = (rec).header.attrib.pen;}
#define NclxMdlGetStyle(rec,style) {style = (rec).header.attrib.line_style;}
#define NclxMdlGetWeight(rec,weight) {weight = (rec).header.attrib.line_weight;}
#define NclxMdlGetInvis(rec,vis) {vis = (rec).header.attrib.visible;}

#define NclxMdlSetColor(rec,icolor) {(rec).header.attrib.color = icolor;}
#define NclxMdlSetLayer(rec,ilayer) {(rec).header.attrib.layer = ilayer;}
#define NclxMdlSetPen(rec,ipen) {(rec).header.attrib.pen = ipen;}
#define NclxMdlSetStyle(rec,istyle) {(rec).header.attrib.line_style = istyle;}
#define NclxMdlSetWeight(rec,iweight) {(rec).header.attrib.line_weight = iweight;}
#define NclxMdlSetInvis(rec,vis) {(rec).header.attrib.visible = vis;}

#define NCLGEODEF

int NclxMdlEvalSetup();
int NclxMdlGetClosed();
int NclxMdlGetRuled();
int NclxMdlFindGeo();

#endif
