/*********************************************************************
**    NAME         :  rmattrdd.c
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"

struct UM_attrmdl_rec
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

struct UM_attrmdl_rec UM_attrmdl;

struct UM_attrmdl_rec UM_new_attrmdl;

struct UM_mtrlmdl_rec
{
	int	index;
	char	name[64][20];
	UU_REAL	ka[64];
	UU_REAL	kd[64];
	UU_REAL	ks[64];
	UU_REAL	ks_r[64];
	UU_REAL	ks_g[64];
	UU_REAL	ks_b[64];
	UU_REAL	spec_exp[64];
};

struct UM_mtrlmdl_rec UM_mtrlmdl;

struct UM_mtrlmdl_rec UM_new_mtrlmdl;

struct UM_dispattr_rec
{
	int	consclr;
	int	conslnstyl;
	UU_REAL	conswght;
	int	featclr;
	int	fea_decpl;
	int	fillclr;
	UU_LOGICAL	hidn_lines;
	UU_KEY_ID	view_in;
	UU_KEY_ID	cpln_key;
};

struct UM_dispattr_rec UM_dispattr;

struct UM_dispattr_rec UM_new_dispattr;
