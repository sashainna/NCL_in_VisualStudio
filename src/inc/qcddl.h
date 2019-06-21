/*********************************************************************
**    NAME         :  qcddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UQ_func_rec
{
	int	arg[5];
	char	funcbuf[200];
};

struct UQ_qstb_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	symbol[12];
	int	ttype;
	UU_REAL	val[3];
	struct UQ_func_rec	*func;
	int	no_func;
	int	pad_func;
	char	varlistbuf[UQ_QSTB_BUFSZ];
};
