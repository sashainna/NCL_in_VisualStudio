/*********************************************************************
**    NAME         :  riallmod.h
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

extern struct UM_attrmdl_rec UM_attrmdl;

extern struct UM_attrmdl_rec UM_new_attrmdl;


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

extern struct UM_mtrlmdl_rec UM_mtrlmdl;

extern struct UM_mtrlmdl_rec UM_new_mtrlmdl;


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

extern struct UM_dispattr_rec UM_dispattr;

extern struct UM_dispattr_rec UM_new_dispattr;


struct UM_drwmdl_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	UU_KEY_ID	drwsn;
	UU_KEY_ID	drwvp;
	UU_KEY_ID	drwvw;
	UU_KEY_ID	curdrw;
	char	modsname[16];
	UU_REAL	aspect;
	char	unitsstr[100];
	int	drwsize;
	UU_REAL	drwscale;
	int	drwunits;
	UU_REAL	modscale;
	int	modunits;
	UU_REAL	plotprec;
};

extern struct UM_drwmdl_rec UM_drwmdl;

extern struct UM_drwmdl_rec UM_new_drwmdl;


struct UM_labelmdl_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	int	max;
	int	num;
	char	pf[40][21];
	int	next[40];
	char	pfs[40][21];
	int	subscr[40];
	int	issub[40];
	int	rel[256];
};

extern struct UM_labelmdl_rec UM_labelmdl;

extern struct UM_labelmdl_rec UM_new_labelmdl;


struct UR_unimod_rec
{
	UU_LOGICAL	dflt_editable;
	UU_LOGICAL	dflt_blanked;
};

extern struct UR_unimod_rec UR_unimod;

extern struct UR_unimod_rec UR_new_unimod;

