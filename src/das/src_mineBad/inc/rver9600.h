/*********************************************************************
**    NAME         :  rver9600.h
**       CONTAINS:
**       definitions used before the 9.600 version of Unibase. 
**    MODULE NAME AND RELEASE LEVEL
**       rver9600.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:49
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"
#include "nccs.h"
#include "mxxx.h"

struct NCL_scalar_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	int	subscr;
	UU_REAL	scalar_value;
};

struct id95
{
   UU_KEY_ID key;
   int rel_num;
   char label[64];
   UU_REAL labloc[3];
   int subscr;
};

struct UM_point_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	markertype;
	UU_LOGICAL	snap_node;
	UU_REAL	pt[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_POINT_BUFSZ];
};

struct UM_line_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	UU_REAL	spt[3];
	UU_REAL	ept[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_LINE_BUFSZ];
};

struct UM_circle_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	UU_REAL	radius;
	UU_REAL	dang;
	UU_REAL	center[3];
	UU_REAL	svec[3];
	UU_REAL	nvec[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_CIRCLE_BUFSZ];
};

struct UM_conic_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	type;
	UU_REAL	invariants[2];
	UU_REAL	tfmat[4][3];
	UU_REAL	t0;
	UU_REAL	t1;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_CONIC_BUFSZ];
};


struct UM_compcrv_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	closdinu;
	UU_REAL	arclen;
	UU_LOGICAL	planar;
	UU_LOGICAL	open;
	int	continuity;
	int	fcolor;
	int	no_cid;
	struct UM_cid_rec	*cid;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_COMPCRV_BUFSZ];
};

struct UM_bsplcrv_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	UU_LOGICAL	inverted;
	UU_LOGICAL	planar;
	UU_LOGICAL	open;
	int	k;
	int	n;
	UU_REAL	t0;
	UU_REAL	t1;
	int	no_pt;
	UU_REAL	*pt;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_BSPLCRV_BUFSZ];
};

struct UM_rbsplcrv_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	UU_LOGICAL	planar;
	UU_LOGICAL	open;
	int	closdinu;
	int	k;
	int	n;
	UU_REAL	t0;
	UU_REAL	t1;
	int	no_t;
	UU_REAL	*t;
	int	no_pt;
	UU_REAL	*pt;
	int	no_wt;
	UU_REAL	*wt;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_RBSPLCRV_BUFSZ];
};

struct UM_uvcvonsf_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	UU_KEY_ID	bskey;
	int	dummy;
	UU_LOGICAL	planar;
	UU_LOGICAL	open;
	int	closdinu;
	int	k;
	int	n;
	UU_REAL	t0;
	UU_REAL	t1;
	int	no_t;
	UU_REAL	*t;
	int	no_pt;
	UU_REAL	*pt;
	int	no_wt;
	UU_REAL	*wt;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_UVCVONSF_BUFSZ];
};

struct UM_poly_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	fcolor;
	int	numvtx;
	UU_REAL	vertex[200][3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_POLY_BUFSZ];
};

struct UM_polyline_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	dummy;
	int	no_pt;
	UU_REAL	*pt;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_POLYLINE_BUFSZ];
};

struct UM_attrdata_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	int	color;
	int	layer;
	int	pen;
	int	line_style;
	UU_REAL	line_weight;
	UU_REAL	line_width;
	int	displayable;
	UU_LOGICAL	selectable;
	UU_LOGICAL	label_on;
};

struct UM_attrmdl_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	int	color;
	int	layer;
	int	pen;
	int	line_style;
	UU_REAL	line_weight;
	UU_REAL	line_width;
	int	displayable;
	UU_LOGICAL	selectable;
	UU_LOGICAL	label_on;
};

struct UM_rbsplsrf_rec95
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
	UU_REAL	offdist;
	int	ku;
	int	kv;
	int	nu;
	int	nv;
	int	dum1;
	int	primitive;
	UU_REAL	prim_param[16];
	UU_LOGICAL	shaded;
	int	lucency;
	int	no_tu;
	UU_REAL	*tu;
	int	no_tv;
	UU_REAL	*tv;
	int	no_pt;
	UU_REAL	*pt;
	int	no_wt;
	UU_REAL	*wt;
	int	no_sskey;
	UU_KEY_ID	*sskey;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	int	no_boxlst;
	UU_REAL	*boxlst;
	int	no_xyzbylst;
	UU_REAL	*xyzbylst;
	char	varlistbuf[UM_RBSPLSRF_BUFSZ];
};

struct NCL_nclattr_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	int	color;
	int	layer;
	int	pen;
	int	line_style;
	UU_REAL	line_weight;
	UU_REAL	line_width;
	int	displayable;
	UU_LOGICAL	selectable;
	UU_LOGICAL	label_on;
};

struct NCL_nclpt_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	markertype;
	UU_LOGICAL	snap_node;
	UU_REAL	pt[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_NCLPT_BUFSZ];
};

struct NCL_nclln_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	UU_REAL	spt[3];
	UU_REAL	ept[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_NCLLN_BUFSZ];
};

struct NCL_nclci_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	UU_REAL	radius;
	UU_REAL	dang;
	UU_REAL	center[3];
	UU_REAL	svec[3];
	UU_REAL	nvec[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_NCLCI_BUFSZ];
};

struct NCL_vector_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	UU_REAL	vec[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_VECTOR_BUFSZ];
};

struct NCL_nclpl_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	UU_REAL	radius;
	UU_REAL	pt[3];
	UU_REAL	nvec[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_NCLPL_BUFSZ];
};

struct NCL_matrix_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	UU_REAL	dalen;
	UU_REAL	dbox[3];
	UU_REAL	mat[3][4];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_MATRIX_BUFSZ];
};

struct NCL_curve_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	closdinu;
	UU_REAL	t0;
	UU_REAL	t1;
	UU_REAL	t_end;
	int	no_param;
	float	*param;
	int	no_segment;
	struct NCL_segment_rec	*segment;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_CURVE_BUFSZ];
};

struct NCL_surface_rec95
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
	int	dum1;
	int	primitive;
	UU_REAL	prim_param[16];
	UU_LOGICAL	shaded;
	int	lucency;
	int	no_panelkey;
	UU_KEY_ID	*panelkey;
	int	no_sskey;
	UU_KEY_ID	*sskey;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	int	no_boxlst;
	UU_REAL	*boxlst;
	int	no_xyzbylst;
	UU_REAL	*xyzbylst;
	char	varlistbuf[NCL_SURFACE_BUFSZ];
};

struct NCL_revsurf_rec95
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
	int	closdinu;
	int	closdinv;
	UU_REAL	offdist;
	int	surf_type;
	int	primitive;
	UU_REAL	prim_param[16];
	UU_LOGICAL	shaded;
	int	lucency;
	UU_LOGICAL	rev_normal;
	UU_KEY_ID	cvkey;
	UU_REAL	pta[3];
	UU_REAL	vca[3];
	UU_REAL	sa;
	UU_REAL	ta;
	int	no_sskey;
	UU_KEY_ID	*sskey;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	int	no_boxlst;
	UU_REAL	*boxlst;
	int	no_xyzbylst;
	UU_REAL	*xyzbylst;
	char	varlistbuf[NCL_REVSURF_BUFSZ];
};

struct NCL_meshsf_rec95
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

struct NCL_quiltsf_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	material;
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

struct NCL_patern_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	markertype;
	int	pntype;
	int	dummy;
	int	no_patpnt;
	UU_REAL	*patpnt;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_PATERN_BUFSZ];
};

struct NCL_netsf_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	material;
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

struct NCL_shape_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	material;
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

struct NCL_evalcv_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	curve_type;
	int	no_evwd;
	UU_REAL	*evwd;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_EVALCV_BUFSZ];
};

struct NCL_evalsf_rec95
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

struct NCL_nclpv_rec95
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	UU_REAL	pt[3];
	UU_REAL	ve[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_NCLPV_BUFSZ];
};

struct NCL_trimsf_rec95
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

struct NCL_scalar_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	int	subscr;
	UU_REAL	scalar_value;
	char	classnm[21];
	char	descript[64];
};

struct id96
{
   UU_KEY_ID key;
   int rel_num;
   char label[64];
   UU_REAL labloc[3];
   UU_REAL ldrloc[3];
   int subscr;
};

struct UM_point_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	markertype;
	UU_LOGICAL	snap_node;
	UU_REAL	pt[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_POINT_BUFSZ];
};

struct UM_line_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	UU_REAL	spt[3];
	UU_REAL	ept[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_LINE_BUFSZ];
};

struct UM_circle_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	UU_REAL	radius;
	UU_REAL	dang;
	UU_REAL	center[3];
	UU_REAL	svec[3];
	UU_REAL	nvec[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_CIRCLE_BUFSZ];
};

struct UM_conic_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	type;
	UU_REAL	invariants[2];
	UU_REAL	tfmat[4][3];
	UU_REAL	t0;
	UU_REAL	t1;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_CONIC_BUFSZ];
};

struct UM_compcrv_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	closdinu;
	UU_REAL	arclen;
	UU_LOGICAL	planar;
	UU_LOGICAL	open;
	int	continuity;
	int	fcolor;
	int	no_cid;
	struct UM_cid_rec	*cid;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_COMPCRV_BUFSZ];
};

struct UM_bsplcrv_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	UU_LOGICAL	inverted;
	UU_LOGICAL	planar;
	UU_LOGICAL	open;
	int	k;
	int	n;
	UU_REAL	t0;
	UU_REAL	t1;
	int	no_pt;
	UU_REAL	*pt;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_BSPLCRV_BUFSZ];
};

struct UM_rbsplcrv_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	UU_LOGICAL	planar;
	UU_LOGICAL	open;
	int	closdinu;
	int	k;
	int	n;
	UU_REAL	t0;
	UU_REAL	t1;
	int	no_t;
	UU_REAL	*t;
	int	no_pt;
	UU_REAL	*pt;
	int	no_wt;
	UU_REAL	*wt;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_RBSPLCRV_BUFSZ];
};

struct UM_uvcvonsf_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	UU_KEY_ID	bskey;
	int	dummy;
	UU_LOGICAL	planar;
	UU_LOGICAL	open;
	int	closdinu;
	int	k;
	int	n;
	UU_REAL	t0;
	UU_REAL	t1;
	int	no_t;
	UU_REAL	*t;
	int	no_pt;
	UU_REAL	*pt;
	int	no_wt;
	UU_REAL	*wt;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_UVCVONSF_BUFSZ];
};

struct UM_poly_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	fcolor;
	int	numvtx;
	UU_REAL	vertex[200][3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_POLY_BUFSZ];
};

struct UM_polyline_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	dummy;
	int	no_pt;
	UU_REAL	*pt;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[UM_POLYLINE_BUFSZ];
};

struct UM_attrdata_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	int	color;
	int	layer;
	int	pen;
	int	line_style;
	UU_REAL	line_weight;
	UU_REAL	line_width;
	int	displayable;
	UU_LOGICAL	selectable;
	int	label_on;
};

struct UM_attrmdl_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	int	color;
	int	layer;
	int	pen;
	int	line_style;
	UU_REAL	line_weight;
	UU_REAL	line_width;
	int	displayable;
	UU_LOGICAL	selectable;
	int	label_on;
};

struct UM_rbsplsrf_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
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
	UU_REAL	offdist;
	int	ku;
	int	kv;
	int	nu;
	int	nv;
	int	dum1;
	int	primitive;
	UU_REAL	prim_param[16];
	UU_LOGICAL	shaded;
	int	lucency;
	int	no_tu;
	UU_REAL	*tu;
	int	no_tv;
	UU_REAL	*tv;
	int	no_pt;
	UU_REAL	*pt;
	int	no_wt;
	UU_REAL	*wt;
	int	no_sskey;
	UU_KEY_ID	*sskey;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	int	no_boxlst;
	UU_REAL	*boxlst;
	int	no_xyzbylst;
	UU_REAL	*xyzbylst;
	char	varlistbuf[UM_RBSPLSRF_BUFSZ];
};

struct NCL_nclattr_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	int	color;
	int	layer;
	int	pen;
	int	line_style;
	UU_REAL	line_weight;
	UU_REAL	line_width;
	int	displayable;
	UU_LOGICAL	selectable;
	int	label_on;
};

struct NCL_nclpt_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	markertype;
	UU_LOGICAL	snap_node;
	UU_REAL	pt[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_NCLPT_BUFSZ];
};

struct NCL_nclln_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	UU_REAL	spt[3];
	UU_REAL	ept[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_NCLLN_BUFSZ];
};

struct NCL_nclci_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	UU_REAL	radius;
	UU_REAL	dang;
	UU_REAL	center[3];
	UU_REAL	svec[3];
	UU_REAL	nvec[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_NCLCI_BUFSZ];
};

struct NCL_vector_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	UU_REAL	vec[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_VECTOR_BUFSZ];
};

struct NCL_nclpl_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	UU_REAL	radius;
	UU_REAL	pt[3];
	UU_REAL	nvec[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_NCLPL_BUFSZ];
};

struct NCL_matrix_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	UU_REAL	dalen;
	UU_REAL	dbox[3];
	UU_REAL	mat[3][4];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_MATRIX_BUFSZ];
};

struct NCL_curve_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	closdinu;
	UU_REAL	t0;
	UU_REAL	t1;
	UU_REAL	t_end;
	int	no_param;
	float	*param;
	int	no_segment;
	struct NCL_segment_rec	*segment;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_CURVE_BUFSZ];
};

struct NCL_surface_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
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
	int	dum1;
	int	primitive;
	UU_REAL	prim_param[16];
	UU_LOGICAL	shaded;
	int	lucency;
	int	no_panelkey;
	UU_KEY_ID	*panelkey;
	int	no_sskey;
	UU_KEY_ID	*sskey;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	int	no_boxlst;
	UU_REAL	*boxlst;
	int	no_xyzbylst;
	UU_REAL	*xyzbylst;
	char	varlistbuf[NCL_SURFACE_BUFSZ];
};

struct NCL_revsurf_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	material;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	int	closdinu;
	int	closdinv;
	UU_REAL	offdist;
	int	surf_type;
	int	primitive;
	UU_REAL	prim_param[16];
	UU_LOGICAL	shaded;
	int	lucency;
	UU_LOGICAL	rev_normal;
	UU_KEY_ID	cvkey;
	UU_REAL	pta[3];
	UU_REAL	vca[3];
	UU_REAL	sa;
	UU_REAL	ta;
	int	no_sskey;
	UU_KEY_ID	*sskey;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	int	no_boxlst;
	UU_REAL	*boxlst;
	int	no_xyzbylst;
	UU_REAL	*xyzbylst;
	char	varlistbuf[NCL_REVSURF_BUFSZ];
};

struct NCL_meshsf_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
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

struct NCL_quiltsf_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	material;
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

struct NCL_patern_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	markertype;
	int	pntype;
	int	dummy;
	int	no_patpnt;
	UU_REAL	*patpnt;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_PATERN_BUFSZ];
};

struct NCL_netsf_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	material;
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

struct NCL_shape_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	material;
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

struct NCL_evalcv_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	curve_type;
	int	no_evwd;
	UU_REAL	*evwd;
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_EVALCV_BUFSZ];
};

struct NCL_evalsf_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	material;
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

struct NCL_nclpv_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	UU_REAL	pt[3];
	UU_REAL	ve[3];
	int	no_displst;
	UU_REAL	*displst;
	char	varlistbuf[NCL_NCLPV_BUFSZ];
};

struct NCL_trimsf_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
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

#ifdef V96MAIN
struct UM_mtrlmdl_oldrec UM_mtrlmdl_old;
struct UM_mtrlmdl_oldrec UM_new_mtrlmdl_old;
#else
extern struct UM_mtrlmdl_oldrec UM_mtrlmdl_old;
extern struct UM_mtrlmdl_oldrec UM_new_mtrlmdl_old;
#endif
