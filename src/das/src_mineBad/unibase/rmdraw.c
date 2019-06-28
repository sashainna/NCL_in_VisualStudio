/*********************************************************************
**    NAME         :  rmdraw.c
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"

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

struct UM_drwmdl_rec UM_drwmdl;

struct UM_drwmdl_rec UM_new_drwmdl;
