/*********************************************************************
**    NAME         :  tznccsddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Fri Dec 8 06:26:02  1989
**     MODULE NAME AND RELEASE LEVEL 
**       tznccsddl.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:01
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct TZNCL_nclattr_rec
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
	int	selectable;
	int	blanked;
	int	label_on;
};

struct TZNCL_nclpt_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	int	markertype;
	UU_LOGICAL	snap_node;
	UU_REAL	pt[3];
};

struct TZNCL_nclln_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	UU_REAL	spt[3];
	UU_REAL	ept[3];
};

struct TZNCL_nclci_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	UU_REAL	radius;
	UU_REAL	dang;
	UU_REAL	center[3];
	UU_REAL	svec[3];
	UU_REAL	nvec[3];
};

struct TZNCL_vector_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	UU_REAL	vec[3];
};

struct TZNCL_nclpl_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	UU_REAL	radius;
	UU_REAL	pt[3];
	UU_REAL	nvec[3];
};

struct TZNCL_matrix_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	UU_REAL	mat[3][4];
};

struct TZNCL_segment_rec
{
	UU_REAL	point[3];
	float	delta[3];
	float	duds0;
	float	duds1;
	float	rho;
};

struct TZNCL_curve_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	int	no_param;
	float	*param;
	int	no_segment;
	struct TZNCL_segment_rec	*segment;
	char	varlistbuf[TZNCL_CURVE_BUFSZ];
};

struct TZNCL_patch_rec
{
	UU_REAL	pt[3];
	float	delta[7][3];
	float	rho;
};

struct TZNCL_panel_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	int	type;
	int	no_param;
	float	param[20];
	int	no_patch;
	struct TZNCL_patch_rec	*patch;
	char	varlistbuf[TZNCL_PANEL_BUFSZ];
};

struct TZNCL_surface_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	int	surf_type;
	int	offset;
	float	offdist;
	int	no_panelkey;
	UU_KEY_ID	*panelkey;
	char	varlistbuf[TZNCL_SURFACE_BUFSZ];
};

struct TZNCL_mpatch_rec
{
	UU_REAL	pt[3];
	UU_REAL	delta[15][3];
};

struct TZNCL_meshsf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	int	surf_type;
	int	m;
	int	n;
	int	offset;
	float	offdist;
	int	no_mpatch;
	struct TZNCL_mpatch_rec	*mpatch;
	char	varlistbuf[TZNCL_MESHSF_BUFSZ];
};

struct TZNCL_qpatch_rec
{
	int	bpatchnum[4];
	float	origin[3];
	float	xvalues[25];
	float	yvalues[25];
	float	zvalues[25];
};

struct TZNCL_quiltsf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	int	surf_type;
	int	numpatches;
	int	offset;
	float	offdist;
	float	midpt[12][3];
	int	no_qpatch;
	struct TZNCL_qpatch_rec	*qpatch;
	char	varlistbuf[TZNCL_QUILTSF_BUFSZ];
};

struct TZNCL_patpnt_rec
{
	UU_REAL	pt[3];
	int	notused1;
};

struct TZNCL_patern_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	int	markertype;
	int	no_patpnt;
	struct TZNCL_patpnt_rec	*patpnt;
	char	varlistbuf[TZNCL_PATERN_BUFSZ];
};

struct TZNCL_netsf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	int	surf_type;
	int	bndsfs[40][4];
	int	no_netkey;
	UU_KEY_ID	*netkey;
	char	varlistbuf[TZNCL_NETSF_BUFSZ];
};

struct TZNCL_shape_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	int	no_shapwd;
	UU_REAL	*shapwd;
	char	varlistbuf[TZNCL_SHAPE_BUFSZ];
};

struct TZNCL_scalar_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	UU_REAL	scalar_value;
};

struct TZNCL_labloc_rec
{
	UU_REAL	del[3];
	int	index;
};

struct TZNCL_labtbl_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	no_labloc;
	struct TZNCL_labloc_rec	*labloc;
	char	varlistbuf[TZNCL_LABTBL_BUFSZ];
};
