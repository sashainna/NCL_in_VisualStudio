/*********************************************************************
**    NAME         :  bsymddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UB_text_nod_rec
{
	int	prompt;
	int	visibility;
	int	masterindx;
	UU_KEY_ID	text_key;
};

struct UB_snap_nod_rec
{
	UU_KEY_ID	snap_key;
	int	nbr;
};

struct UB_masters_rec
{
	UU_KEY_ID	mastr_key;
	UU_REAL	tfmat[4][3];
};

struct UB_inst_rec
{
	UU_KEY_ID	inst_key;
	UU_REAL	tfmat[4][3];
};

struct UB_acon_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_KEY_ID	con_key;
};

struct UB_symbol_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	version;
	char	path[200];
	struct UB_masters_rec	*masters;
	int	no_masters;
	int	pad_masters;
	struct UB_inst_rec	*inst;
	int	no_inst;
	int	pad_inst;
	UU_KEY_ID	*geom;
	int	no_geom;
	int	pad_geom;
	struct UB_text_nod_rec	*text_nod;
	int	no_text_nod;
	int	pad_text_nod;
	struct UB_snap_nod_rec	*snap_nod;
	int	no_snap_nod;
	int	pad_snap_nod;
	char	varlistbuf[UB_SYMBOL_BUFSZ];
};

struct UB_instance_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	UU_KEY_ID	*geom;
	int	no_geom;
	int	pad_geom;
	struct UB_text_nod_rec	*text_nod;
	int	no_text_nod;
	int	pad_text_nod;
	struct UB_snap_nod_rec	*snap_nod;
	int	no_snap_nod;
	int	pad_snap_nod;
	char	varlistbuf[UB_INSTANCE_BUFSZ];
};

struct UB_symattr_rec
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
	UU_LOGICAL	see_snod;
	int	see_tnod;
};

struct UB_conector_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_KEY_ID	pline;
	UU_KEY_ID	*ainst;
	int	no_ainst;
	int	pad_ainst;
	char	varlistbuf[UB_CONECTOR_BUFSZ];
};
