/*********************************************************************
**    NAME         :  rver9100.h
**       CONTAINS:
**       definitions used before the 9.100 version of Unibase. 
**    MODULE NAME AND RELEASE LEVEL
**       rver9100.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:49
*********************************************************************/

struct surf90
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
	int   no_sskey;
	UU_KEY_ID   *sskey;
   int   no_displst;
   UU_REAL  *displst;
   int   no_tesslst;
   UU_REAL  *tesslst;
   char  varlistbuf[NCL_SURFACE_BUFSZ];
};

struct rbsf90
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
	int   no_sskey;
	UU_KEY_ID   *sskey;
   int   no_displst;
   UU_REAL  *displst;
   int   no_tesslst;
   UU_REAL  *tesslst;
   char  varlistbuf[UM_RBSPLSRF_BUFSZ];
};

struct nsf90
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
	int   no_sskey;
	UU_KEY_ID   *sskey;
   int   no_displst;
   UU_REAL  *displst;
   int   no_tesslst;
   UU_REAL  *tesslst;
   char  varlistbuf[NCL_NETSF_BUFSZ];
};

struct tsf90
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

struct msf90
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	material;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	int	rldnu;
	int	rldnv;
	UU_LOGICAL	rev_normal;
	int	closdinu;
	int	closdinv;
	int	offset;
	float	offdist;
	int	surf_type;
	int	m;
	int	n;
	int	no_mpatch;
	struct NCL_mpatch_rec	*mpatch;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	char	varlistbuf[NCL_MESHSF_BUFSZ];
};

struct qsf90
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	surf_type;
	int	numpatches;
	int	rldnu;
	int	rldnv;
	int	offset;
	float	offdist;
	float	midpt[12][3];
	int	dummy;
	int	no_qpatch;
	struct NCL_qpatch_rec	*qpatch;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	char	varlistbuf[NCL_QUILTSF_BUFSZ];
};

struct evsf90
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	int	rldnu;
	int	rldnv;
	int	closdinu;
	int	closdinv;
	int	surf_type;
	int	offset;
	float	offdist;
	int	no_evwd;
	UU_REAL	*evwd;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	char	varlistbuf[NCL_EVALSF_BUFSZ];
};

struct sh90
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

struct vp90
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

struct draw90
{
	UU_KEY_ID   key;
	int   rel_num;
	char  name[16];
	int   drwsize;
	UU_REAL  drwscale;
	int   drwunits;
	UU_REAL  modscale;
	int   modunits;
	UU_REAL  plotprec;
	int   no_member;
	UU_KEY_ID   *member;
	char  varlistbuf[UM_DRAWING_BUFSZ];
};

struct msf91
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	material;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	int	rldnu;
	int	rldnv;
	UU_LOGICAL	rev_normal;
	int	closdinu;
	int	closdinv;
	int	offset;
	float	offdist;
	int	surf_type;
	int	m;
	int	n;
	UU_LOGICAL	shaded;
	int	lucency;
	int	no_mpatch;
	struct NCL_mpatch_rec	*mpatch;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	int	no_boxlst;
	UU_REAL	*boxlst;
	int	no_xyzbylst;
	UU_REAL	*xyzbylst;
	char	varlistbuf[NCL_MESHSF_BUFSZ];
};

struct qsf91
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	surf_type;
	int	numpatches;
	int	rldnu;
	int	rldnv;
	int	offset;
	float	offdist;
	float	midpt[12][3];
	UU_LOGICAL	shaded;
	int	lucency;
	int	dummy;
	int	no_qpatch;
	struct NCL_qpatch_rec	*qpatch;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	int	no_boxlst;
	UU_REAL	*boxlst;
	int	no_xyzbylst;
	UU_REAL	*xyzbylst;
	char	varlistbuf[NCL_QUILTSF_BUFSZ];
};

struct nsf91
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	surf_type;
	int	rldnu;
	int	rldnv;
	int	bndsfs[40][4];
	UU_LOGICAL	shaded;
	int	lucency;
	int	no_netkey;
	UU_KEY_ID	*netkey;
	int	no_sskey;
	UU_KEY_ID	*sskey;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	char	varlistbuf[NCL_NETSF_BUFSZ];
};

struct sh91
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	f2d3d;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	UU_LOGICAL	shaded;
	int	lucency;
	int	dummy;
	int	no_shapwd;
	UU_REAL	*shapwd;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	char	varlistbuf[NCL_SHAPE_BUFSZ];
};

struct evsf91
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	int	rldnu;
	int	rldnv;
	int	closdinu;
	int	closdinv;
	int	surf_type;
	int	offset;
	UU_LOGICAL	shaded;
	int	lucency;
	float	offdist;
	int	no_evwd;
	UU_REAL	*evwd;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	char	varlistbuf[NCL_EVALSF_BUFSZ];
};

struct tsf91
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	material;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	UU_LOGICAL	rev_normal;
	int	closdinu;
	int	closdinv;
	float	offdist;
	UU_KEY_ID	uv_key;
	UU_KEY_ID	cv_key;
	UU_KEY_ID	bs_key;
	float	ub_min;
	float	ub_max;
	float	vb_min;
	float	vb_max;
	float	u_min;
	float	u_max;
	float	v_min;
	float	v_max;
	int	drive_type;
	UU_LOGICAL	shaded;
	int	lucency;
	int	no_ibndykey;
	UU_KEY_ID	*ibndykey;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	int	no_boxlst;
	UU_REAL	*boxlst;
	int	no_xyzbylst;
	UU_REAL	*xyzbylst;
	int	no_uvbylst;
	UU_REAL	*uvbylst;
	int	no_uvboxlst;
	UU_REAL	*uvboxlst;
	char	varlistbuf[NCL_TRIMSF_BUFSZ];
};


struct vp91
{
	UU_KEY_ID	key;
	int	rel_num;
	char	name[15];
	int	xform;
	UU_REAL	llf[3];
	UU_REAL	urb[3];
	UU_KEY_ID	cur_view;
	int	disp_prio;
	int	input_prio;
	UU_LOGICAL	disp_all;
	int	bord_seg;
	UU_LOGICAL	aperture_on;
	UU_LOGICAL	v_axis_on;
	UU_LOGICAL	name_on;
	UU_LOGICAL	bord_on;
	int	nverts;
	UU_REAL	vertices[60];
	int	grid_seg;
	UU_LOGICAL	grid_on;
	UU_LOGICAL	motion;
	int	disp_mode;
	UU_LOGICAL	wireframe;
};
