/*********************************************************************
**    NAME         :  nccsddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct NCL_nclattr_rec
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

struct NCL_nclpt_rec
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
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[NCL_NCLPT_BUFSZ];
};

struct NCL_nclln_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	UU_REAL	spt[3];
	UU_REAL	ept[3];
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[NCL_NCLLN_BUFSZ];
};

struct NCL_nclci_rec
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
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[NCL_NCLCI_BUFSZ];
};

struct NCL_vector_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	UU_REAL	vec[3];
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[NCL_VECTOR_BUFSZ];
};

struct NCL_nclpl_rec
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
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[NCL_NCLPL_BUFSZ];
};

struct NCL_matrix_rec
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
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[NCL_MATRIX_BUFSZ];
};

struct NCL_segment_rec
{
	UU_REAL	point[3];
	float	delta[3];
	float	duds0;
	float	duds1;
	float	rho;
};

struct NCL_curve_rec
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
	float	*param;
	int	no_param;
	int	pad_param;
	struct NCL_segment_rec	*segment;
	int	no_segment;
	int	pad_segment;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[NCL_CURVE_BUFSZ];
};

struct NCL_patch_rec
{
	UU_REAL	pt[3];
	float	delta[7][3];
	float	rho;
};

struct NCL_panel_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	int	type;
	int	no_param;
	float	param[50];
	struct NCL_patch_rec	*patch;
	int	no_patch;
	int	pad_patch;
	char	varlistbuf[NCL_PANEL_BUFSZ];
};

struct NCL_surface_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	rldnu;
	int	swapuv;
	UU_LOGICAL	rev_normal;
	int	closdinu;
	int	closdinv;
	int	offset;
	float	offdist;
	int	surf_type;
	int	primitive;
	UU_REAL	prim_param[16];
	UU_KEY_ID	*panelkey;
	int	no_panelkey;
	int	pad_panelkey;
	UU_KEY_ID	*sskey;
	int	no_sskey;
	int	pad_sskey;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	UU_REAL	*tesslst;
	int	no_tesslst;
	int	pad_tesslst;
	UU_REAL	*boxlst;
	int	no_boxlst;
	int	pad_boxlst;
	UU_REAL	*xyzbylst;
	int	no_xyzbylst;
	int	pad_xyzbylst;
	char	varlistbuf[NCL_SURFACE_BUFSZ];
};

struct NCL_revsurf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	dummy;
	int	rldnu;
	int	swapuv;
	UU_LOGICAL	rev_normal;
	int	closdinu;
	int	closdinv;
	float	offdist;
	UU_KEY_ID	cvkey;
	int	primitive;
	UU_REAL	prim_param[16];
	UU_REAL	pta[3];
	UU_REAL	vca[3];
	UU_REAL	sa;
	UU_REAL	ta;
	UU_KEY_ID	*sskey;
	int	no_sskey;
	int	pad_sskey;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	UU_REAL	*tesslst;
	int	no_tesslst;
	int	pad_tesslst;
	UU_REAL	*boxlst;
	int	no_boxlst;
	int	pad_boxlst;
	UU_REAL	*xyzbylst;
	int	no_xyzbylst;
	int	pad_xyzbylst;
	char	varlistbuf[NCL_REVSURF_BUFSZ];
};

struct NCL_mpatch_rec
{
	UU_REAL	pt[3];
	UU_REAL	delta[15][3];
};

struct NCL_meshsf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	dummy;
	int	rldnu;
	int	swapuv;
	UU_LOGICAL	rev_normal;
	int	closdinu;
	int	closdinv;
	int	offset;
	float	offdist;
	int	surf_type;
	int	m;
	int	n;
	struct NCL_mpatch_rec	*mpatch;
	int	no_mpatch;
	int	pad_mpatch;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	UU_REAL	*tesslst;
	int	no_tesslst;
	int	pad_tesslst;
	UU_REAL	*boxlst;
	int	no_boxlst;
	int	pad_boxlst;
	UU_REAL	*xyzbylst;
	int	no_xyzbylst;
	int	pad_xyzbylst;
	char	varlistbuf[NCL_MESHSF_BUFSZ];
};

struct NCL_qpatch_rec
{
	int	bpatchnum[4];
	float	origin[3];
	float	xvalues[25];
	float	yvalues[25];
	float	zvalues[25];
};

struct NCL_quiltsf_rec
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
	struct NCL_qpatch_rec	*qpatch;
	int	no_qpatch;
	int	pad_qpatch;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	UU_REAL	*tesslst;
	int	no_tesslst;
	int	pad_tesslst;
	UU_REAL	*boxlst;
	int	no_boxlst;
	int	pad_boxlst;
	UU_REAL	*xyzbylst;
	int	no_xyzbylst;
	int	pad_xyzbylst;
	char	varlistbuf[NCL_QUILTSF_BUFSZ];
};

struct NCL_patern_rec
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
	UU_REAL	*patpnt;
	int	no_patpnt;
	int	pad_patpnt;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[NCL_PATERN_BUFSZ];
};

struct NCL_netsf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	surf_type;
	int	bndsfs[40][4];
	UU_KEY_ID	*netkey;
	int	no_netkey;
	int	pad_netkey;
	UU_KEY_ID	*sskey;
	int	no_sskey;
	int	pad_sskey;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	UU_REAL	*tesslst;
	int	no_tesslst;
	int	pad_tesslst;
	char	varlistbuf[NCL_NETSF_BUFSZ];
};

struct NCL_shape_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	f2d3d;
	int	dummy;
	UU_REAL	*shapwd;
	int	no_shapwd;
	int	pad_shapwd;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	UU_REAL	*tesslst;
	int	no_tesslst;
	int	pad_tesslst;
	char	varlistbuf[NCL_SHAPE_BUFSZ];
};

struct NCL_scalar_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	int	subscr;
	UU_REAL	scalar_value;
	char	classnm[21];
	char	descript[64];
	UU_REAL	modified;
};

struct NCL_labloc_rec
{
	UU_REAL	del[3];
	int	index;
};

struct NCL_labtbl_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	struct NCL_labloc_rec	*labloc;
	int	no_labloc;
	int	pad_labloc;
	char	varlistbuf[NCL_LABTBL_BUFSZ];
};

struct NCL_evalcv_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	curve_type;
	UU_REAL	*evwd;
	int	no_evwd;
	int	pad_evwd;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[NCL_EVALCV_BUFSZ];
};

struct NCL_evalsf_rec
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
	UU_REAL	*evwd;
	int	no_evwd;
	int	pad_evwd;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	UU_REAL	*tesslst;
	int	no_tesslst;
	int	pad_tesslst;
	char	varlistbuf[NCL_EVALSF_BUFSZ];
};

struct NCL_nclpv_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	UU_REAL	pt[3];
	UU_REAL	ve[3];
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[NCL_NCLPV_BUFSZ];
};

struct NCL_trimsf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
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
	UU_KEY_ID	*ibndykey;
	int	no_ibndykey;
	int	pad_ibndykey;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	UU_REAL	*tesslst;
	int	no_tesslst;
	int	pad_tesslst;
	UU_REAL	*boxlst;
	int	no_boxlst;
	int	pad_boxlst;
	UU_REAL	*xyzbylst;
	int	no_xyzbylst;
	int	pad_xyzbylst;
	UU_REAL	*uvbylst;
	int	no_uvbylst;
	int	pad_uvbylst;
	UU_REAL	*uvboxlst;
	int	no_uvboxlst;
	int	pad_uvboxlst;
	char	varlistbuf[NCL_TRIMSF_BUFSZ];
};

struct NCL_datael_rec
{
	UU_REAL	value;
	char	label[64];
	int	isub;
	int	type;
	int	delim;
	int	dum;
};

struct NCL_datast_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	int	subscr;
	int	nargs;
	struct NCL_datael_rec	*datael;
	int	no_datael;
	int	pad_datael;
	char	varlistbuf[NCL_DATAST_BUFSZ];
};

struct NCL_textvar_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	int	subscr;
	char	text[256];
};
