/*********************************************************************
**    NAME         :  riallmod.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    MODULE NAME AND RELEASE LEVEL
**         tzallmod.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**         04/29/15 , 15:06:58
*********************************************************************/
#include "usysdef.h"

struct TZ_attrmdl_rec
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
	UU_LOGICAL	blanked;
};

extern struct TZ_attrmdl_rec TZ_attrmdl;

extern struct TZ_attrmdl_rec TZ_new_attrmdl;


struct TZ_mtrlmdl_rec
{
	int	index;
	UU_REAL	kd[64];
	UU_REAL	ks[64];
	UU_REAL	spec_exp[64];
};

extern struct TZ_mtrlmdl_rec TZ_mtrlmdl;

extern struct TZ_mtrlmdl_rec TZ_new_mtrlmdl;


struct TZ_dispattr_rec
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

extern struct TZ_dispattr_rec TZ_dispattr;

extern struct TZ_dispattr_rec TZ_new_dispattr;


struct TZ_drwmdl_rec
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

extern struct TZ_drwmdl_rec TZ_drwmdl;

extern struct TZ_drwmdl_rec TZ_new_drwmdl;


struct TZ_labelmdl_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	int	max;
	int	num;
	char	pf[20][3];
	int	next[20];
	int	rel[256];
};

extern struct TZ_labelmdl_rec TZ_labelmdl;

extern struct TZ_labelmdl_rec TZ_new_labelmdl;


struct TZ_unimod_rec
{
	int	unused;
};

extern struct TZ_unimod_rec TZ_unimod;

extern struct TZ_unimod_rec TZ_new_unimod;

struct OTZ_labelmdl_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	int	max;
	int	num;
	char	pf[10][3];
	int	next[10];
	int	rel[256];
};

extern struct OTZ_labelmdl_rec OTZ_labelmdl;

extern struct OTZ_labelmdl_rec OTZ_new_labelmdl;
