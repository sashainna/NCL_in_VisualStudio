/*********************************************************************
**    NAME         :  atxtddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UA_txt_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	subtype;
	UU_KEY_ID	arckey;
	UU_REAL	dx;
	UU_REAL	dy;
	UU_REAL	tangle;
	UU_REAL	position[3];
	char	*tchar;
	int	no_tchar;
	int	pad_tchar;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	char	varlistbuf[UA_TXT_BUFSZ];
};

struct UA_txtattr_rec
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
	int	label_on;
	int	font;
	int	prec;
	UU_REAL	expn;
	UU_REAL	spacing;
	UU_REAL	height;
	UU_REAL	up[3];
	UU_REAL	plane[3];
	int	path;
	int	align_hor;
	int	align_ver;
	int	txt_dens;
	UU_REAL	slant;
	UU_REAL	sub_sup;
	UU_REAL	line_spacing;
	int	entity_site;
};
