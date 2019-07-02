/*********************************************************************
**    NAME         :  rver9700.h
**       CONTAINS:
**       definitions used before the 9.700 version of Unibase. 
**    MODULE NAME AND RELEASE LEVEL
**       rver9700.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:49
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"
#include "nccs.h"
#include "mxxx.h"
#include "atext.h"
#include "bsym.h"

struct UA_txt_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	int	subtype;
	UU_KEY_ID	arckey;
	UU_REAL	dx;
	UU_REAL	dy;
	UU_REAL	tangle;
	UU_REAL	position[3];
	int	no_tchar;
	char	*tchar;
	char	varlistbuf[UA_TEXT_BUFSZ];
};

struct UA_txtattr_rec96
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
	int	font;
	int	prec;
	UU_REAL	expn;
	UU_REAL	spacing;
	UU_REAL	height;
	UU_REAL	up[3];
	UU_REAL	plane[3];
	int	path;
	int	align_hor;
	int	align_ver;
	int	txt_dens;
	UU_REAL	slant;
	UU_REAL	sub_sup;
	UU_REAL	line_spacing;
	int	entity_site;
};

struct UB_symattr_rec96
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
	UU_LOGICAL	see_snod;
	int	see_tnod;
};

struct UB_symbol_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	name[11];
	int	version;
	char	path[200];
	int	no_masters;
	struct UB_masters_rec	*masters;
	int	no_inst;
	struct UB_inst_rec	*inst;
	int	no_geom;
	UU_KEY_ID	*geom;
	int	no_text_nod;
	struct UB_text_nod_rec	*text_nod;
	int	no_snap_nod;
	struct UB_snap_nod_rec	*snap_nod;
	char	varlistbuf[UB_SYMBOL_BUFSZ];
};

struct UB_instance_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	int	no_geom;
	UU_KEY_ID	*geom;
	int	no_text_nod;
	struct UB_text_nod_rec	*text_nod;
	int	no_snap_nod;
	struct UB_snap_nod_rec	*snap_nod;
	char	varlistbuf[UB_INSTANCE_BUFSZ];
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

struct NCL_sfhead96
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	material;
};

struct UM_labelmdl_rec96
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	int	max;
	int	num;
	char	pf[20][3];
	int	next[20];
	char	pfs[20][7];
	int	subscr[20];
	int	issub[20];
	int	rel[256];
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

#ifdef V97MAIN
struct UM_labelmdl_rec96 UM_labelmdl96;
struct UM_labelmdl_rec96 UM_new_labelmdl96;
#else
extern struct UM_labelmdl_rec96 UM_labelmdl96;
extern struct UM_labelmdl_rec96 UM_new_labelmdl96;
#endif
