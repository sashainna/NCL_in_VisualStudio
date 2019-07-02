/*********************************************************************
**    NAME         :  msolddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UM_solid_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	type;
	int	closed;
	UU_REAL	box[6];
	UU_REAL	*sdata;
	int	no_sdata;
	int	pad_sdata;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	UU_REAL	*tesslst;
	int	no_tesslst;
	int	pad_tesslst;
	UU_KEY_ID	*netkey;
	int	no_netkey;
	int	pad_netkey;
	char	varlistbuf[UM_SOLID_BUFSZ];
};

struct UM_agshell_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	int	material;
	int	shelladdr;
};

struct UM_body_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	subscr;
	UU_REAL	pitch;
	char	name[16];
	int	id;
	int	body_number;
	int	color;
	int	*edge;
	int	no_edge;
	int	pad_edge;
	char	varlistbuf[UM_BODY_BUFSZ];
};
