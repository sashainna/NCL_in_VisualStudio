/*********************************************************************
**    NAME         :  ysplanddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    MODULE NAME AND RELEASE LEVEL
**       tzspland.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:02
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UY_room_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	no_walls;
	UU_KEY_ID	*walls;
	char	varlistbuf[UY_ROOM_BUFSZ];
};

struct UY_partn_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_REAL	startpt[3];
	UU_REAL	endpt[3];
	UU_REAL	support_len;
	UU_REAL	partition_ang;
	UU_REAL	partition_len;
};

struct UY_desk_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_REAL	corners[4][3];
	UU_REAL	desk_ang;
	UU_REAL	desk_len;
	UU_REAL	desk_wid;
};

struct UY_chair_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_REAL	chair_radius;
	int	no_chbase;
	UU_KEY_ID	*chbase;
	char	varlistbuf[UY_CHAIR_BUFSZ];
};

struct UY_splanatt_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	int	color;
	int	layer;
	int	pen;
	int	line_style;
	UU_REAL	line_weight;
	UU_REAL	line_width;
	int	displayable;
	UU_LOGICAL	selectable;
	UU_LOGICAL	blanked;
	int	partition_color;
	UU_LOGICAL	metal_desk;
	UU_LOGICAL	swivel_chair;
};
