/*********************************************************************
**    NAME         :  ver8400.h
**       CONTAINS:
**       definitions used before the 8.301 version of Unibase. 
**    DATE AND TIME OF LAST MODIFICATION
**       rver8400.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:49
*********************************************************************/

struct id82
{
   UU_KEY_ID key;
   int rel_num;
   char label[8];
   int subscr;
};

struct vp82
{
	UU_KEY_ID   key;
	int   rel_num;
	char  name[15];
	int   xform;
	UU_REAL  llf[3];
	UU_REAL  urb[3];
	UU_KEY_ID   cur_view;
	int   disp_prio;
	int   input_prio;
	UU_LOGICAL  disp_all;
	int   bord_seg;
	UU_LOGICAL  aperture_on;
	UU_LOGICAL  v_axis_on;
	UU_LOGICAL  name_on;
	UU_LOGICAL  bord_on;
	int   nverts;
	UU_REAL  vertices[60];
	int   grid_seg;
	UU_LOGICAL  grid_on;
	UU_LOGICAL  motion;
};

struct curve82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   closdinu;
   int   no_param;
   float *param;
   int   no_segment;
   struct NCL_segment_rec  *segment;
   char  varlistbuf[NCL_CURVE_BUFSZ];
};

struct bcv82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   UU_LOGICAL  inverted;
   UU_LOGICAL  planar;
   UU_LOGICAL  open;
   int   k;
   int   n;
   UU_REAL  t0;
   UU_REAL  t1;
   int   no_pt;
   UU_REAL  *pt;
   char  varlistbuf[UM_BSPLCRV_BUFSZ];
};

struct rbcv82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   UU_LOGICAL  planar;
   UU_LOGICAL  open;
   int   closdinu;
   int   k;
   int   n;
   UU_REAL  t0;
   UU_REAL  t1;
   int   no_t;
   UU_REAL  *t;
   int   no_pt;
   UU_REAL  *pt;
   int   no_wt;
   UU_REAL  *wt;
   char  varlistbuf[UM_RBSPLCRV_BUFSZ];
};

struct ccv82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   closdinu;
   UU_REAL  arclen;
   UU_LOGICAL  planar;
   UU_LOGICAL  open;
   int   continuity;
   int   fcolor;
   int   no_cid;
   struct UM_cid_rec *cid;
   char  varlistbuf[UM_COMPCRV_BUFSZ];
};
 
struct mx82
{
   UU_KEY_ID key;
   int rel_num;
   char label[8];
   int subscr;
   UU_REAL mat[3][4];
};

struct pt82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   markertype;
   UU_LOGICAL  snap_node;
   UU_REAL  pt[3];
};

struct ln82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   UU_REAL  spt[3];
   UU_REAL  ept[3];
};

struct polgon82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   fcolor;
   int   numvtx;
   UU_REAL  vertex[200][3];
};

struct polln82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   no_pt;
   UU_REAL  *pt;
   char  varlistbuf[UM_POLYLINE_BUFSZ];
};

struct ci82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   UU_REAL  radius;
   UU_REAL  dang;
   UU_REAL  center[3];
   UU_REAL  svec[3];
   UU_REAL  nvec[3];
};

struct ve82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   UU_REAL  vec[3];
};

struct pln82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   UU_REAL  radius;
   UU_REAL  pt[3];
   UU_REAL  nvec[3];
};

struct surf82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   material;
   int   numupaths;
   int   numvpaths;
   int   ptsperucrv;
   int   ptspervcrv;
   int   rldnu;
   int   rldnv;
   UU_LOGICAL  rev_normal;
   int   closdinu;
   int   closdinv;
   int   offset;
   float offdist;
   int   surf_type;
   int   no_panelkey;
   UU_KEY_ID   *panelkey;
   char  varlistbuf[NCL_SURFACE_BUFSZ];
};

struct rbsf82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   material;
   int   numupaths;
   int   numvpaths;
   int   ptsperucrv;
   int   ptspervcrv;
   int   rldnu;
   int   rldnv;
   UU_LOGICAL  rev_normal;
   int   closdinu;
   int   closdinv;
   int   offset;
   UU_REAL  offdist;
   int   dumm1;
   int   ku;
   int   kv;
   int   nu;
   int   nv;
   int   no_tu;
   UU_REAL  *tu;
   int   no_tv;
   UU_REAL  *tv;
   int   no_pt;
   UU_REAL  *pt;
   int   no_wt;
   UU_REAL  *wt;
   char  varlistbuf[UM_RBSPLSRF_BUFSZ];
};

struct msf82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   material;
   int   numupaths;
   int   numvpaths;
   int   ptsperucrv;
   int   ptspervcrv;
   int   rldnu;
   int   rldnv;
   UU_LOGICAL  rev_normal;
   int   closdinu;
   int   closdinv;
   int   offset;
   float offdist;
   int   surf_type;
   int   m;
   int   n;
   int   no_mpatch;
   struct NCL_mpatch_rec   *mpatch;
   char  varlistbuf[NCL_MESHSF_BUFSZ];
};

struct qsf82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   surf_type;
   int   numpatches;
   int   rldnu;
   int   rldnv;
   int   offset;
   float offdist;
   float midpt[12][3];
   int   no_qpatch;
   struct NCL_qpatch_rec   *qpatch;
   char  varlistbuf[NCL_QUILTSF_BUFSZ];
};

struct pn82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   markertype;
   int   pntype;
   int   no_patpnt;
   UU_REAL  *patpnt;
   char  varlistbuf[NCL_PATERN_BUFSZ];
};

struct nsf82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   surf_type;
   int   rldnu;
   int   rldnv;
   int   bndsfs[40][4];
   int   no_netkey;
   UU_KEY_ID   *netkey;
   char  varlistbuf[NCL_NETSF_BUFSZ];
};

struct sh82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   no_shapwd;
   UU_REAL  *shapwd;
   char  varlistbuf[NCL_SHAPE_BUFSZ];
};

struct sc82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   UU_REAL  scalar_value;
};

struct evcv82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   curve_type;
   int   no_evwd;
   UU_REAL  *evwd;
   char  varlistbuf[NCL_EVALCV_BUFSZ];
};

struct evsf82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   numupaths;
   int   numvpaths;
   int   ptsperucrv;
   int   ptspervcrv;
   int   rldnu;
   int   rldnv;
   int   closdinu;
   int   closdinv;
   int   surf_type;
   int   offset;
   float offdist;
   int   no_evwd;
   UU_REAL  *evwd;
   char  varlistbuf[NCL_EVALSF_BUFSZ];
};

struct pv82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   UU_REAL  pt[3];
   UU_REAL  ve[3];
};

struct tsf82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   material;
   int   numupaths;
   int   numvpaths;
   int   ptsperucrv;
   int   ptspervcrv;
   UU_LOGICAL  rev_normal;
   int   closdinu;
   int   closdinv;
   float offdist;
   UU_KEY_ID   uv_key;
   UU_KEY_ID   cv_key;
   UU_KEY_ID   bs_key;
   float ub_min;
   float ub_max;
   float vb_min;
   float vb_max;
   float u_min;
   float u_max;
   float v_min;
   float v_max;
   int   no_ibndykey;
   UU_KEY_ID   *ibndykey;
   char  varlistbuf[NCL_TRIMSF_BUFSZ];
};

struct cn82
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   type;
   UU_REAL  invariants[2];
   UU_REAL  tfmat[4][3];
   UU_REAL  t0;
   UU_REAL  t1;
   char  varlistbuf[UM_CONIC_BUFSZ];
};
