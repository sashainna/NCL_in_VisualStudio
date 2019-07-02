/*********************************************************************
**    NAME         :  mcrvddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UM_point_rec
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
	char	varlistbuf[UM_POINT_BUFSZ];
};

struct UM_line_rec
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
	char	varlistbuf[UM_LINE_BUFSZ];
};

struct UM_circle_rec
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
	char	varlistbuf[UM_CIRCLE_BUFSZ];
};

struct UM_conic_rec
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
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[UM_CONIC_BUFSZ];
};

struct UM_cid_rec
{
	UU_KEY_ID	crvid;
	UU_LOGICAL	reverse;
	UU_REAL	endparam;
};

struct UM_compcrv_rec
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
	UU_REAL	t0;
	UU_REAL	t1;
	int	addflg;
	struct UM_cid_rec	*cid;
	int	no_cid;
	int	pad_cid;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[UM_COMPCRV_BUFSZ];
};

struct UM_bsplcrv_rec
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
	UU_REAL	*pt;
	int	no_pt;
	int	pad_pt;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[UM_BSPLCRV_BUFSZ];
};

struct UM_rbsplcrv_rec
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
	UU_REAL	*t;
	int	no_t;
	int	pad_t;
	UU_REAL	*pt;
	int	no_pt;
	int	pad_pt;
	UU_REAL	*wt;
	int	no_wt;
	int	pad_wt;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[UM_RBSPLCRV_BUFSZ];
};

struct UM_uvcvonsf_rec
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
	UU_REAL	*t;
	int	no_t;
	int	pad_t;
	UU_REAL	*pt;
	int	no_pt;
	int	pad_pt;
	UU_REAL	*wt;
	int	no_wt;
	int	pad_wt;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[UM_UVCVONSF_BUFSZ];
};

struct UM_agcrv_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	int	crvaddr;
	int	closdinu;
};

struct UM_poly_rec
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
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[UM_POLY_BUFSZ];
};

struct UM_polyline_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	dummy;
	UU_REAL	*pt;
	int	no_pt;
	int	pad_pt;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[UM_POLYLINE_BUFSZ];
};
