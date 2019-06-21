/*********************************************************************
**    NAME         :  adrfddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    MODULE NAME AND RELEASE LEVEL
**       tzdrfddl.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:59
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct TZ_txtblk_rec
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

struct TZ_arcblk_rec
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

struct TZ_lineblk_rec
{
	int	subtype;
	int	line_fon;
	UU_REAL	line_den;
	int	color;
	int	num_pts;
	UU_REAL	line_seg[50][3];
};

struct TZ_arrowblk_rec
{
	int	arr_type;
	int	arr_font;
	UU_REAL	arr_dens;
	int	color;
	UU_REAL	location[3];
	UU_REAL	aangle;
	UU_REAL	asize;
};

struct TZ_assoblk_rec
{
	int	asso_typ;
	int	modifier;
	UU_KEY_ID	asso_key;
	UU_REAL	location[3];
};

struct TZ_draft_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	draf_ati[44];
	UU_REAL	draf_atr[25];
	UU_REAL	cpln[12];
	int	no_txtblk;
	struct TZ_txtblk_rec	*txtblk;
	int	no_arcblk;
	struct TZ_arcblk_rec	*arcblk;
	int	no_lineblk;
	struct TZ_lineblk_rec	*lineblk;
	int	no_arrowblk;
	struct TZ_arrowblk_rec	*arrowblk;
	int	no_assoblk;
	struct TZ_assoblk_rec	*assoblk;
	char	varlistbuf[TZ_DRAFT_BUFSZ];
};

struct TZ_hatchlin_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	no_pt;
	UU_REAL	*pt;
	char	varlistbuf[TZ_HATCHLIN_BUFSZ];
};
