/*********************************************************************
**    NAME         :  mxxxddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    MODULE NAME AND RELEASE LEVEL
**       tzxxxddl.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:02
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
	int	no_member;
	UU_KEY_ID	*member;
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
	int	no_member;
	UU_KEY_ID	*member;
	char	varlistbuf[UM_DRAWING_BUFSZ];
};

struct UM_layer_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	name[16];
	int	num;
};
