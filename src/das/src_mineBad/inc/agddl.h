/*********************************************************************
**    NAME         :  agddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       09/11/13 , 13:00:41
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UAG_crv_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	crvaddr;
	char	filename[32];
};

struct UAG_srf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	material;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	UU_LOGICAL	rev_normal;
	int	srfaddr;
	char	filename[32];
};

struct UAG_shell_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	int	material;
	int	shelladdr;
	char	filename[32];
};
