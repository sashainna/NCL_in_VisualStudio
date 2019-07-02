/*********************************************************************
**    NAME         :  bsymddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    MODULE NAME AND RELEASE LEVEL
**       tzsymddl.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:02
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct TZ_text_nod_rec
{
	int	prompt;
	int	visibility;
	int	masterindx;
	UU_KEY_ID	text_key;
};

struct TZ_snap_nod_rec
{
	UU_KEY_ID	snap_key;
	int	nbr;
};

struct TZ_masters_rec
{
	UU_KEY_ID	mastr_key;
	UU_REAL	tfmat[4][3];
};

struct TZ_inst_rec
{
	UU_KEY_ID	inst_key;
	UU_REAL	tfmat[4][3];
};

struct TZ_symbol_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	name[11];
	int	version;
	char	path[200];
	int	no_masters;
	struct TZ_masters_rec	*masters;
	int	no_inst;
	struct TZ_inst_rec	*inst;
	int	no_geom;
	UU_KEY_ID	*geom;
	int	no_text_nod;
	struct TZ_text_nod_rec	*text_nod;
	int	no_snap_nod;
	struct TZ_snap_nod_rec	*snap_nod;
	char	varlistbuf[TZ_SYMBOL_BUFSZ];
};

struct TZ_instance_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_KEY_ID	master_key;
	int	no_geom;
	UU_KEY_ID	*geom;
	int	no_text_nod;
	struct TZ_text_nod_rec	*text_nod;
	int	no_snap_nod;
	struct TZ_snap_nod_rec	*snap_nod;
	int	no_acon;
	UU_KEY_ID	*acon;
	char	varlistbuf[TZ_INSTANCE_BUFSZ];
};

struct TZ_symattr_rec
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
	UU_LOGICAL	see_snod;
	int	see_tnod;
};

struct TZ_conector_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_KEY_ID	pline;
	int	no_ainst;
	UU_KEY_ID	*ainst;
	char	varlistbuf[TZ_CONECTOR_BUFSZ];
};
