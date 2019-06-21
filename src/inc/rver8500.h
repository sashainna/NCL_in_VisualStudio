/*********************************************************************
**    NAME         :  ver8500.h
**       CONTAINS:
**       definitions used before the 8.500 version of Unibase. 
**    DATE AND TIME OF LAST MODIFICATION
**       rver8500.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:49
*********************************************************************/

struct id84 /* A structure holding identity data of Unibase entity*/
{
	UU_KEY_ID key;
	int rel_num;
	char label[64];
	UU_REAL labloc[3];
	int subscr;
};

struct mx84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
	int   subscr;
	UU_REAL  dalen;
	UU_REAL  dbox[3];
	UU_REAL  mat[3][4];
	int   no_displst;
	UU_REAL  *displst;
	char  varlistbuf[NCL_MATRIX_BUFSZ];
};

struct pt84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
	int   subscr;
	int   markertype;
	UU_LOGICAL  snap_node;
	UU_REAL  pt[3];
	int   no_displst;
	UU_REAL  *displst;
	char  varlistbuf[NCL_NCLPT_BUFSZ];
};

struct pv84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
	int   subscr;
	UU_REAL  pt[3];
	UU_REAL  ve[3];
	int   no_displst;
	UU_REAL  *displst;
	char  varlistbuf[NCL_NCLPV_BUFSZ];
};

struct ln84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
	int   subscr;
	UU_REAL  spt[3];
	UU_REAL  ept[3];
	int   no_displst;
	UU_REAL  *displst;
	char  varlistbuf[NCL_NCLLN_BUFSZ];
};

struct ci84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
	int   subscr;
	UU_REAL  radius;
	UU_REAL  dang;
	UU_REAL  center[3];
	UU_REAL  svec[3];
	UU_REAL  nvec[3];
	int   no_displst;
	UU_REAL  *displst;
	char  varlistbuf[NCL_NCLCI_BUFSZ];
};

struct ve84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
	int   subscr;
	UU_REAL  vec[3];
	int   no_displst;
	UU_REAL  *displst;
	char  varlistbuf[NCL_VECTOR_BUFSZ];
};

struct pln84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
	int   subscr;
	UU_REAL  radius;
	UU_REAL  pt[3];
	UU_REAL  nvec[3];
	int   no_displst;
	UU_REAL  *displst;
	char  varlistbuf[NCL_NCLPL_BUFSZ];
};

struct sc84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	int   subscr;
	UU_REAL  scalar_value;
};

struct pn84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
	int   subscr;
	int   markertype;
	int   pntype;
	int   dummy;
	int   no_patpnt;
	UU_REAL  *patpnt;
	int   no_displst;
	UU_REAL  *displst;
	char  varlistbuf[NCL_PATERN_BUFSZ];
};

struct curve84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
	int   subscr;
	int   closdinu;
	UU_REAL  t0;
	UU_REAL  t1;
	UU_REAL  t_end;
	int   no_param;
	float *param;
	int   no_segment;
	struct NCL_segment_rec  *segment;
	int   no_displst;
	UU_REAL  *displst;
	char  varlistbuf[NCL_CURVE_BUFSZ];
};

struct bcv84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
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
	int   no_displst;
	UU_REAL  *displst;
	char  varlistbuf[UM_BSPLCRV_BUFSZ];
};

struct rbcv84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
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
	int   no_displst;
	UU_REAL  *displst;
	char  varlistbuf[UM_RBSPLCRV_BUFSZ];
};

struct ccv84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
	int   subscr;
	int   closdinu;
	UU_REAL  arclen;
	UU_LOGICAL  planar;
	UU_LOGICAL  open;
	int   continuity;
	int   fcolor;
	int   no_cid;
	struct UM_cid_rec *cid;
	int   no_displst;
	UU_REAL  *displst;
	char  varlistbuf[UM_COMPCRV_BUFSZ];
};

struct evcv84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
	int   subscr;
	int   curve_type;
	int   no_evwd;
	UU_REAL  *evwd;
	int   no_displst;
	UU_REAL  *displst;
	char  varlistbuf[NCL_EVALCV_BUFSZ];
};

struct surf84
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[64];
	UU_REAL lbaloc[3];
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
   int   no_displst;
   UU_REAL  *displst;
   int   no_tesslst;
   UU_REAL  *tesslst;
   char  varlistbuf[NCL_SURFACE_BUFSZ];
};

struct rbsf84
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[64];
	UU_REAL lbaloc[3];
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
	int   dummy;
   int   no_tu;
   UU_REAL  *tu;
   int   no_tv;
   UU_REAL  *tv;
   int   no_pt;
   UU_REAL  *pt;
   int   no_wt;
   UU_REAL  *wt;
   int   no_displst;
   UU_REAL  *displst;
   int   no_tesslst;
   UU_REAL  *tesslst;
   char  varlistbuf[UM_RBSPLSRF_BUFSZ];
};

struct nsf84
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[64];
	UU_REAL lbaloc[3];
   int   subscr;
   int   surf_type;
   int   rldnu;
   int   rldnv;
   int   bndsfs[40][4];
   int   no_netkey;
   UU_KEY_ID   *netkey;
   int   no_displst;
   UU_REAL  *displst;
   int   no_tesslst;
   UU_REAL  *tesslst;
   char  varlistbuf[NCL_NETSF_BUFSZ];
};

struct evsf84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
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
	int   no_displst;
	UU_REAL  *displst;
	int   no_tesslst;
	UU_REAL  *tesslst;
	char  varlistbuf[NCL_EVALSF_BUFSZ];
};

struct tsf84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
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
	int   drive_type;
	int   no_ibndykey;
	UU_KEY_ID   *ibndykey;
	int   no_displst;
	UU_REAL  *displst;
	int   no_tesslst;
	UU_REAL  *tesslst;
	char  varlistbuf[NCL_TRIMSF_BUFSZ];
};

struct msf84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
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
	int   no_displst;
	UU_REAL  *displst;
	int   no_tesslst;
	UU_REAL  *tesslst;
	char  varlistbuf[NCL_MESHSF_BUFSZ];
};

struct qsf84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
	int   subscr;
	int   surf_type;
	int   numpatches;
	int   rldnu;
	int   rldnv;
	int   offset;
	float offdist;
	float midpt[12][3];
	int   dummy;
	int   no_qpatch;
	struct NCL_qpatch_rec   *qpatch;
	int   no_displst;
	UU_REAL  *displst;
	int   no_tesslst;
	UU_REAL  *tesslst;
	char  varlistbuf[NCL_QUILTSF_BUFSZ];
};

struct polgon84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
	int   subscr;
	int   fcolor;
	int   numvtx;
	UU_REAL  vertex[200][3];
	int   no_displst;
	UU_REAL  *displst;
	char  varlistbuf[UM_POLY_BUFSZ];
};

struct polln84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
	int   subscr;
	int   dummy;
	int   no_pt;
	UU_REAL  *pt;
	int   no_displst;
	UU_REAL  *displst;
	char  varlistbuf[UM_POLYLINE_BUFSZ];
};

struct cn84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
	int   subscr;
	int   type;
	UU_REAL  invariants[2];
	UU_REAL  tfmat[4][3];
	UU_REAL  t0;
	UU_REAL  t1;
	int   no_displst;
	UU_REAL  *displst;
	char  varlistbuf[UM_CONIC_BUFSZ];
};

struct sh84
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[64];
	UU_REAL  labloc[3];
	int   subscr;
	int   dummy;
	int   no_shapwd;
	UU_REAL  *shapwd;
	int   no_displst;
	UU_REAL  *displst;
	char  varlistbuf[NCL_SHAPE_BUFSZ];
};
