/*********************************************************************
**    NAME         :  mcrvddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    MODULE NAME AND RELEASE LEVEL
**         tzcrvddl.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**         04/29/15 , 15:06:59
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct TZ_point_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	markertype;
	UU_LOGICAL	snap_node;
	UU_REAL	pt[3];
};

struct TZ_line_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	UU_REAL	spt[3];
	UU_REAL	ept[3];
};

struct TZ_circle_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	UU_REAL	radius;
	UU_REAL	dang;
	UU_REAL	center[3];
	UU_REAL	svec[3];
	UU_REAL	nvec[3];
};

struct TZ_conic_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	type;
	UU_REAL	invariants[2];
	UU_REAL	tfmat[4][3];
	UU_REAL	t0;
	UU_REAL	t1;
};

struct TZ_cid_rec
{
	UU_KEY_ID	crvid;
	UU_LOGICAL	reverse;
	UU_REAL	endparam;
};

struct TZ_compcrv_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	UU_REAL	arclen;
	UU_LOGICAL	planar;
	UU_LOGICAL	open;
	int	continuity;
	int	fcolor;
	int	no_cid;
	struct TZ_cid_rec	*cid;
	char	varlistbuf[TZ_COMPCRV_BUFSZ];
};

struct TZ_bsplcrv_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	UU_LOGICAL	inverted;
	UU_LOGICAL	planar;
	UU_LOGICAL	open;
	int	k;
	int	n;
	UU_REAL	t0;
	UU_REAL	t1;
	int	no_pt;
	UU_REAL	*pt;
	char	varlistbuf[TZ_BSPLCRV_BUFSZ];
};

struct TZ_rbsplcrv_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	UU_LOGICAL	planar;
	UU_LOGICAL	open;
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
	char	varlistbuf[TZ_RBSPLCRV_BUFSZ];
};

struct TZ_poly_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	fcolor;
	int	numvtx;
	UU_REAL	vertex[200][3];
};

struct TZ_polyline_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	no_pt;
	UU_REAL	*pt;
	char	varlistbuf[TZ_POLYLINE_BUFSZ];
};

struct TZ_text_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	UU_REAL	position[3];
	int	no_tchar;
	char	*tchar;
	char	varlistbuf[TZ_TEXT_BUFSZ];
};
