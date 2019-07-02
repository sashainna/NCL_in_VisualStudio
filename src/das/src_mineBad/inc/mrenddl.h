/*********************************************************************
**    NAME         :  mrenddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UM_light_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	index;
	int	type;
	int	intens;
	UU_REAL	position[3];
	UU_REAL	direction[3];
	UU_REAL	attenuation[3];
	UU_REAL	cone_angle;
	UU_REAL	scale;
	int	space;
	UU_REAL	exp;
	UU_REAL	ambient[4];
};
