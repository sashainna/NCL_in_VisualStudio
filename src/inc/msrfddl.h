/*********************************************************************
**    NAME         :  msrfddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UM_surfattr_rec
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
	int	material;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	int	ecolor;
	UU_LOGICAL	shaded;
	int	lucency;
};

struct UM_rbsplsrf_rec
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
	UU_REAL	*tu;
	int	no_tu;
	int	pad_tu;
	UU_REAL	*tv;
	int	no_tv;
	int	pad_tv;
	UU_REAL	*pt;
	int	no_pt;
	int	pad_pt;
	UU_REAL	*wt;
	int	no_wt;
	int	pad_wt;
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
	char	varlistbuf[UM_RBSPLSRF_BUFSZ];
};

struct UM_agsrf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	int	material;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	int	rldnu;
	int	rldnv;
	UU_LOGICAL	rev_normal;
	int	srfaddr;
	int	closdinu;
	int	closdinv;
};
