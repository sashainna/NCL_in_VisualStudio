/*********************************************************************
**    NAME         :  rmlabddl.c
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"

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

struct UM_labelmdl_rec UM_labelmdl;

struct UM_labelmdl_rec UM_new_labelmdl;
