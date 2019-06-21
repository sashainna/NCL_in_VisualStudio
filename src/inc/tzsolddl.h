/*********************************************************************
**    NAME         :  msolddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    MODULE NAME AND RELEASE LEVEL
**       tzsolddl.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:01
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UM_body_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[8];
	UU_REAL	pitch;
	char	name[16];
	int	id;
	int	age;
	int	color;
	int	no_edge;
	int	*edge;
	char	varlistbuf[UM_BODY_BUFSZ];
};
