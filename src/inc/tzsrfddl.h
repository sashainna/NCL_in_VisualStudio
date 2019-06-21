/*********************************************************************
**    NAME         :  msrfddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    MODULE NAME AND RELEASE LEVEL
**       tzsrfddl.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:02
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UM_pln_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	material;
	UU_REAL	radius;
	UU_REAL	pt[3];
	UU_REAL	nvec[3];
};

struct UM_revsrf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	material;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	UU_REAL	axispt[3];
	UU_REAL	axisdir[3];
	UU_KEY_ID	genid;
	UU_REAL	sang;
	UU_REAL	eang;
};

struct UM_rulsrf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	material;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	UU_KEY_ID	crv1id;
	UU_LOGICAL	crv1reversed;
	UU_KEY_ID	crv2id;
	UU_LOGICAL	crv2reversed;
};

struct UM_tabcyl_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	material;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	UU_KEY_ID	crvid;
	UU_REAL	liftdir[3];
	UU_REAL	dist;
};

struct UM_bsplsrf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	material;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	UU_LOGICAL	inverted;
	int	ku;
	int	kv;
	int	nu;
	int	nv;
	UU_REAL	u0;
	UU_REAL	u1;
	UU_REAL	v0;
	UU_REAL	v1;
	int	no_pt;
	UU_REAL	*pt;
	char	varlistbuf[UM_BSPLSRF_BUFSZ];
};

struct UM_rbsplsrf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	material;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	UU_LOGICAL	planar;
	int	ku;
	int	kv;
	int	nu;
	int	nv;
	UU_REAL	u0;
	UU_REAL	u1;
	UU_REAL	v0;
	UU_REAL	v1;
	int	no_u;
	UU_REAL	*u;
	int	no_v;
	UU_REAL	*v;
	int	no_pt;
	UU_REAL	*pt;
	int	no_wt;
	UU_REAL	*wt;
	char	varlistbuf[UM_RBSPLSRF_BUFSZ];
};

struct UM_surfrend_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	no_surfids;
	UU_KEY_ID	*surfids;
	char	varlistbuf[UM_SURFREND_BUFSZ];
};
