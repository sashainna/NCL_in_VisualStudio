/*********************************************************************
**    NAME         :  adrfddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UA_txtblk_rec
{
	int	subtype;
	int	txt_font;
	UU_REAL	txt_dens;
	int	color;
	int	char_cnt;
	int	txt_just;
	char	fontname[17];
	char	tstring[1025];
	UU_REAL	origin[3];
	UU_REAL	dx;
	UU_REAL	dy;
	UU_REAL	slant;
	UU_REAL	tangle;
	UU_REAL	txt_size;
	UU_REAL	sub_sup;
	UU_REAL	char_exp;
	UU_REAL	char_spa;
	UU_REAL	line_spa;
};

struct UA_arcblk_rec
{
	int	subtype;
	int	arc_font;
	UU_REAL	arc_dens;
	int	color;
	int	num_pts;
	UU_REAL	cent_pt[3];
	UU_REAL	radius;
	UU_REAL	angles[50];
};

struct UA_lineblk_rec
{
	int	subtype;
	int	line_fon;
	UU_REAL	line_den;
	int	color;
	int	num_pts;
	UU_REAL	line_seg[50][3];
};

struct UA_arrowblk_rec
{
	int	arr_type;
	int	arr_font;
	UU_REAL	arr_dens;
	int	color;
	UU_REAL	location[3];
	UU_REAL	aangle;
	UU_REAL	asize;
};

struct UA_assoblk_rec
{
	int	asso_typ;
	int	modifier;
	UU_KEY_ID	asso_key;
	UU_REAL	location[3];
};

struct UA_draft_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	draf_ati[44];
	UU_REAL	draf_atr[25];
	UU_REAL	cpln[12];
	struct UA_txtblk_rec	*txtblk;
	int	no_txtblk;
	int	pad_txtblk;
	struct UA_arcblk_rec	*arcblk;
	int	no_arcblk;
	int	pad_arcblk;
	struct UA_lineblk_rec	*lineblk;
	int	no_lineblk;
	int	pad_lineblk;
	struct UA_arrowblk_rec	*arrowblk;
	int	no_arrowblk;
	int	pad_arrowblk;
	struct UA_assoblk_rec	*assoblk;
	int	no_assoblk;
	int	pad_assoblk;
	char	varlistbuf[UA_DRAFT_BUFSZ];
};

struct UA_hatchlin_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	UU_REAL	*wt;
	int	no_wt;
	int	pad_wt;
	char	varlistbuf[UA_HATCHLIN_BUFSZ];
};
