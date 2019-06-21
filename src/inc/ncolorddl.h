/*********************************************************************
**    NAME         :  ncolorddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct NCL_color_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	color_name[64][96];
	int	color_value[64][3];
};
