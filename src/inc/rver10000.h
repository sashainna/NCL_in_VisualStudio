
/*********************************************************************
**    NAME         :  rver10000.h
**       CONTAINS:
**       definitions used before the 10.000 version of Unibase. 
**    MODULE NAME AND RELEASE LEVEL
**       rver10000.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:48
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"
#include "nccs.h"
#include "msrf.h"

struct UM_rbsplsrf_rec99
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
	float	offdist;
	int	ku;
	int	kv;
	int	nu;
	int	nv;
	int	primitive;
	UU_REAL	prim_param[16];
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

struct NCL_surface_rec99
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

struct NCL_revsurf_rec99
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

struct NCL_meshsf_rec99
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

struct NCL_netsf_rec99
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	surf_type;
	int	bndsfs[40][4];
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

struct NCL_shape_rec99
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	f2d3d;
	int	dummy;
	int	no_shapwd;
	UU_REAL	*shapwd;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	char	varlistbuf[NCL_SHAPE_BUFSZ];
};

struct NCL_trimsf_rec99
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
