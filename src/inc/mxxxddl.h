/*********************************************************************
**    NAME         :  mxxxddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UM_coordsys_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	type;
	UU_REAL	xaxis[3];
	UU_REAL	yaxis[3];
	UU_REAL	zaxis[3];
	UU_REAL	origin[3];
	UU_REAL	z_depth;
	char	name[16];
};

struct UM_grouper_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	name[16];
	UU_KEY_ID	*member;
	int	no_member;
	int	pad_member;
	char	varlistbuf[UM_GROUPER_BUFSZ];
};

struct UM_drawing_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	name[16];
	int	drwsize;
	UU_REAL	drwscale;
	int	drwunits;
	UU_REAL	modscale;
	int	modunits;
	UU_REAL	plotprec;
	UU_KEY_ID	*member;
	int	no_member;
	int	pad_member;
	char	varlistbuf[UM_DRAWING_BUFSZ];
};

struct UM_layer_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	name[80];
	int	num;
	UU_LOGICAL	displayable;
	UU_LOGICAL	selectable;
	int	*layers;
	int	no_layers;
	int	pad_layers;
	char	varlistbuf[UM_LAYER_BUFSZ];
};
