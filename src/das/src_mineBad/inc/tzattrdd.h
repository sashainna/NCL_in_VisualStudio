/*********************************************************************
**    NAME         :  mattrddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    MODULE NAME AND RELEASE LEVEL
**         tzattrdd.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**         04/29/15 , 15:06:59
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct TZ_attrdata_rec
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
};

struct UM_transf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	UU_REAL	tfmat[4][3];
};

struct TZ_textattr_rec
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
};

struct TZ_attrmdl_rec
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
};

extern struct TZ_attrmdl_rec TZ_attrmdl;

extern struct TZ_attrmdl_rec TZ_new_attrmdl;

#define tz_get_attrmdl(where) uu_move_byte(&TZ_attrmdl, where, sizeof(struct TZ_attrmdl_rec))
#define tz_get_new_attrmdl(where) uu_move_byte(&TZ_new_attrmdl, where, sizeof(struct TZ_attrmdl_rec))
#define tz_put_attrmdl(where) uu_move_byte(where, &TZ_attrmdl, sizeof(struct TZ_attrmdl_rec))
#define tz_put_new_attrmdl(where) uu_move_byte(where, &TZ_new_attrmdl, sizeof(struct TZ_attrmdl_rec))
#define tz_get_attrmdl_key() TZ_attrmdl.key
#define tz_get_new_attrmdl_key() TZ_new_attrmdl.key
#define tz_put_attrmdl_key(where) TZ_attrmdl.key = where
#define tz_put_new_attrmdl_key(where) TZ_new_attrmdl.key = where
#define tz_get_attrmdl_rel_num() TZ_attrmdl.rel_num
#define tz_get_new_attrmdl_rel_num() TZ_new_attrmdl.rel_num
#define tz_get_attrmdl_use_count() TZ_attrmdl.use_count
#define tz_get_new_attrmdl_use_count() TZ_new_attrmdl.use_count
#define tz_put_attrmdl_rel_num(where) TZ_attrmdl.rel_num=where
#define tz_put_new_attrmdl_rel_num(where) TZ_new_attrmdl.rel_num=where
#define tz_put_attrmdl_use_count(where) TZ_attrmdl.use_count = where
#define tz_put_new_attrmdl_use_count(where) TZ_new_attrmdl.use_count = where
#define tz_get_attrmdl_color() TZ_attrmdl.color
#define tz_get_new_attrmdl_color() TZ_new_attrmdl.color
#define tz_put_attrmdl_color(where) TZ_attrmdl.color = where
#define tz_put_new_attrmdl_color(where) TZ_new_attrmdl.color = where
#define tz_get_attrmdl_layer() TZ_attrmdl.layer
#define tz_get_new_attrmdl_layer() TZ_new_attrmdl.layer
#define tz_put_attrmdl_layer(where) TZ_attrmdl.layer = where
#define tz_put_new_attrmdl_layer(where) TZ_new_attrmdl.layer = where
#define tz_get_attrmdl_pen() TZ_attrmdl.pen
#define tz_get_new_attrmdl_pen() TZ_new_attrmdl.pen
#define tz_put_attrmdl_pen(where) TZ_attrmdl.pen = where
#define tz_put_new_attrmdl_pen(where) TZ_new_attrmdl.pen = where
#define tz_get_attrmdl_line_style() TZ_attrmdl.line_style
#define tz_get_new_attrmdl_line_style() TZ_new_attrmdl.line_style
#define tz_put_attrmdl_line_style(where) TZ_attrmdl.line_style = where
#define tz_put_new_attrmdl_line_style(where) TZ_new_attrmdl.line_style = where
#define tz_get_attrmdl_line_weight() TZ_attrmdl.line_weight
#define tz_get_new_attrmdl_line_weight() TZ_new_attrmdl.line_weight
#define tz_put_attrmdl_line_weight(where) TZ_attrmdl.line_weight = where
#define tz_put_new_attrmdl_line_weight(where) TZ_new_attrmdl.line_weight = where
#define tz_get_attrmdl_line_width() TZ_attrmdl.line_width
#define tz_get_new_attrmdl_line_width() TZ_new_attrmdl.line_width
#define tz_put_attrmdl_line_width(where) TZ_attrmdl.line_width = where
#define tz_put_new_attrmdl_line_width(where) TZ_new_attrmdl.line_width = where
#define tz_get_attrmdl_displayable() TZ_attrmdl.displayable
#define tz_get_new_attrmdl_displayable() TZ_new_attrmdl.displayable
#define tz_put_attrmdl_displayable(where) TZ_attrmdl.displayable = where
#define tz_put_new_attrmdl_displayable(where) TZ_new_attrmdl.displayable = where
#define tz_get_attrmdl_selectable() TZ_attrmdl.selectable
#define tz_get_new_attrmdl_selectable() TZ_new_attrmdl.selectable
#define tz_put_attrmdl_selectable(where) TZ_attrmdl.selectable = where
#define tz_put_new_attrmdl_selectable(where) TZ_new_attrmdl.selectable = where
#define tz_get_attrmdl_blanked() TZ_attrmdl.blanked
#define tz_get_new_attrmdl_blanked() TZ_new_attrmdl.blanked
#define tz_put_attrmdl_blanked(where) TZ_attrmdl.blanked = where
#define tz_put_new_attrmdl_blanked(where) TZ_new_attrmdl.blanked = where

struct TZ_mtrlmdl_rec
{
	int	index;
	UU_REAL	kd[64];
	UU_REAL	ks[64];
	UU_REAL	spec_exp[64];
};

extern struct TZ_mtrlmdl_rec TZ_mtrlmdl;

extern struct TZ_mtrlmdl_rec TZ_new_mtrlmdl;

#define tz_get_mtrlmdl(where) uu_move_byte(&TZ_mtrlmdl, where, sizeof(struct TZ_mtrlmdl_rec))
#define tz_get_new_mtrlmdl(where) uu_move_byte(&TZ_new_mtrlmdl, where, sizeof(struct TZ_mtrlmdl_rec))
#define tz_put_mtrlmdl(where) uu_move_byte(where, &TZ_mtrlmdl, sizeof(struct TZ_mtrlmdl_rec))
#define tz_put_new_mtrlmdl(where) uu_move_byte(where, &TZ_new_mtrlmdl, sizeof(struct TZ_mtrlmdl_rec))
#define tz_get_mtrlmdl_index() TZ_mtrlmdl.index
#define tz_get_new_mtrlmdl_index() TZ_new_mtrlmdl.index
#define tz_put_mtrlmdl_index(where) TZ_mtrlmdl.index = where
#define tz_put_new_mtrlmdl_index(where) TZ_new_mtrlmdl.index = where
#define tz_get_mtrlmdl_rel_num() TZ_mtrlmdl.rel_num
#define tz_get_new_mtrlmdl_rel_num() TZ_new_mtrlmdl.rel_num
#define tz_get_mtrlmdl_kd(where) uu_move_byte(TZ_mtrlmdl.kd, where, 512)
#define tz_get_new_mtrlmdl_kd(where) uu_move_byte(TZ_new_mtrlmdl.kd, where, 512)
#define tz_put_mtrlmdl_rel_num(where) TZ_mtrlmdl.rel_num=where
#define tz_put_new_mtrlmdl_rel_num(where) TZ_new_mtrlmdl.rel_num=where
#define tz_put_mtrlmdl_kd(where) uu_move_byte(where, TZ_mtrlmdl.kd, 512)
#define tz_put_new_mtrlmdl_kd(where) uu_move_byte(where, TZ_new_mtrlmdl.kd, 512)
#define tz_get_mtrlmdl_ks(where) uu_move_byte(TZ_mtrlmdl.ks, where, 512)
#define tz_get_new_mtrlmdl_ks(where) uu_move_byte(TZ_new_mtrlmdl.ks, where, 512)
#define tz_put_mtrlmdl_ks(where) uu_move_byte(where, TZ_mtrlmdl.ks, 512)
#define tz_put_new_mtrlmdl_ks(where) uu_move_byte(where, TZ_new_mtrlmdl.ks, 512)
#define tz_get_mtrlmdl_spec_exp(where) uu_move_byte(TZ_mtrlmdl.spec_exp, where, 512)
#define tz_get_new_mtrlmdl_spec_exp(where) uu_move_byte(TZ_new_mtrlmdl.spec_exp, where, 512)
#define tz_put_mtrlmdl_spec_exp(where) uu_move_byte(where, TZ_mtrlmdl.spec_exp, 512)
#define tz_put_new_mtrlmdl_spec_exp(where) uu_move_byte(where, TZ_new_mtrlmdl.spec_exp, 512)

struct TZ_dispattr_rec
{
	int	consclr;
	int	conslnstyl;
	UU_REAL	conswght;
	int	featclr;
	int	fea_decpl;
	int	fillclr;
	UU_LOGICAL	hidn_lines;
	UU_KEY_ID	view_in;
	UU_KEY_ID	cpln_key;
};

extern struct TZ_dispattr_rec TZ_dispattr;

extern struct TZ_dispattr_rec TZ_new_dispattr;

#define tz_get_dispattr(where) uu_move_byte(&TZ_dispattr, where, sizeof(struct TZ_dispattr_rec))
#define tz_get_new_dispattr(where) uu_move_byte(&TZ_new_dispattr, where, sizeof(struct TZ_dispattr_rec))
#define tz_put_dispattr(where) uu_move_byte(where, &TZ_dispattr, sizeof(struct TZ_dispattr_rec))
#define tz_put_new_dispattr(where) uu_move_byte(where, &TZ_new_dispattr, sizeof(struct TZ_dispattr_rec))
#define tz_get_dispattr_consclr() TZ_dispattr.consclr
#define tz_get_new_dispattr_consclr() TZ_new_dispattr.consclr
#define tz_put_dispattr_consclr(where) TZ_dispattr.consclr = where
#define tz_put_new_dispattr_consclr(where) TZ_new_dispattr.consclr = where
#define tz_get_dispattr_rel_num() TZ_dispattr.rel_num
#define tz_get_new_dispattr_rel_num() TZ_new_dispattr.rel_num
#define tz_get_dispattr_conslnstyl() TZ_dispattr.conslnstyl
#define tz_get_new_dispattr_conslnstyl() TZ_new_dispattr.conslnstyl
#define tz_put_dispattr_rel_num(where) TZ_dispattr.rel_num=where
#define tz_put_new_dispattr_rel_num(where) TZ_new_dispattr.rel_num=where
#define tz_put_dispattr_conslnstyl(where) TZ_dispattr.conslnstyl = where
#define tz_put_new_dispattr_conslnstyl(where) TZ_new_dispattr.conslnstyl = where
#define tz_get_dispattr_conswght() TZ_dispattr.conswght
#define tz_get_new_dispattr_conswght() TZ_new_dispattr.conswght
#define tz_put_dispattr_conswght(where) TZ_dispattr.conswght = where
#define tz_put_new_dispattr_conswght(where) TZ_new_dispattr.conswght = where
#define tz_get_dispattr_featclr() TZ_dispattr.featclr
#define tz_get_new_dispattr_featclr() TZ_new_dispattr.featclr
#define tz_put_dispattr_featclr(where) TZ_dispattr.featclr = where
#define tz_put_new_dispattr_featclr(where) TZ_new_dispattr.featclr = where
#define tz_get_dispattr_fea_decpl() TZ_dispattr.fea_decpl
#define tz_get_new_dispattr_fea_decpl() TZ_new_dispattr.fea_decpl
#define tz_put_dispattr_fea_decpl(where) TZ_dispattr.fea_decpl = where
#define tz_put_new_dispattr_fea_decpl(where) TZ_new_dispattr.fea_decpl = where
#define tz_get_dispattr_fillclr() TZ_dispattr.fillclr
#define tz_get_new_dispattr_fillclr() TZ_new_dispattr.fillclr
#define tz_put_dispattr_fillclr(where) TZ_dispattr.fillclr = where
#define tz_put_new_dispattr_fillclr(where) TZ_new_dispattr.fillclr = where
#define tz_get_dispattr_hidn_lines() TZ_dispattr.hidn_lines
#define tz_get_new_dispattr_hidn_lines() TZ_new_dispattr.hidn_lines
#define tz_put_dispattr_hidn_lines(where) TZ_dispattr.hidn_lines = where
#define tz_put_new_dispattr_hidn_lines(where) TZ_new_dispattr.hidn_lines = where
#define tz_get_dispattr_view_in() TZ_dispattr.view_in
#define tz_get_new_dispattr_view_in() TZ_new_dispattr.view_in
#define tz_put_dispattr_view_in(where) TZ_dispattr.view_in = where
#define tz_put_new_dispattr_view_in(where) TZ_new_dispattr.view_in = where
#define tz_get_dispattr_cpln_key() TZ_dispattr.cpln_key
#define tz_get_new_dispattr_cpln_key() TZ_new_dispattr.cpln_key
#define tz_put_dispattr_cpln_key(where) TZ_dispattr.cpln_key = where
#define tz_put_new_dispattr_cpln_key(where) TZ_new_dispattr.cpln_key = where
