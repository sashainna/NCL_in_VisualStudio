/*********************************************************************
**    NAME         :  mattrddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UM_attrdata_rec
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
};

struct UM_transf_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	UU_REAL	tfmat[4][3];
};

struct UM_attrmdl_rec
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
};

extern struct UM_attrmdl_rec UM_attrmdl;

extern struct UM_attrmdl_rec UM_new_attrmdl;

#define ur_get_attrmdl(where) uu_move_byte(&UM_attrmdl, where, sizeof(struct UM_attrmdl_rec))
#define ur_get_new_attrmdl(where) uu_move_byte(&UM_new_attrmdl, where, sizeof(struct UM_attrmdl_rec))
#define ur_put_attrmdl(where) uu_move_byte(where, &UM_attrmdl, sizeof(struct UM_attrmdl_rec))
#define ur_put_new_attrmdl(where) uu_move_byte(where, &UM_new_attrmdl, sizeof(struct UM_attrmdl_rec))
#define ur_get_attrmdl_key() UM_attrmdl.key
#define ur_get_new_attrmdl_key() UM_new_attrmdl.key
#define ur_put_attrmdl_key(where) UM_attrmdl.key = where
#define ur_put_new_attrmdl_key(where) UM_new_attrmdl.key = where
#define ur_get_attrmdl_rel_num() UM_attrmdl.rel_num
#define ur_get_new_attrmdl_rel_num() UM_new_attrmdl.rel_num
#define ur_get_attrmdl_use_count() UM_attrmdl.use_count
#define ur_get_new_attrmdl_use_count() UM_new_attrmdl.use_count
#define ur_put_attrmdl_rel_num(where) UM_attrmdl.rel_num=where
#define ur_put_new_attrmdl_rel_num(where) UM_new_attrmdl.rel_num=where
#define ur_put_attrmdl_use_count(where) UM_attrmdl.use_count = where
#define ur_put_new_attrmdl_use_count(where) UM_new_attrmdl.use_count = where
#define ur_get_attrmdl_color() UM_attrmdl.color
#define ur_get_new_attrmdl_color() UM_new_attrmdl.color
#define ur_put_attrmdl_color(where) UM_attrmdl.color = where
#define ur_put_new_attrmdl_color(where) UM_new_attrmdl.color = where
#define ur_get_attrmdl_layer() UM_attrmdl.layer
#define ur_get_new_attrmdl_layer() UM_new_attrmdl.layer
#define ur_put_attrmdl_layer(where) UM_attrmdl.layer = where
#define ur_put_new_attrmdl_layer(where) UM_new_attrmdl.layer = where
#define ur_get_attrmdl_pen() UM_attrmdl.pen
#define ur_get_new_attrmdl_pen() UM_new_attrmdl.pen
#define ur_put_attrmdl_pen(where) UM_attrmdl.pen = where
#define ur_put_new_attrmdl_pen(where) UM_new_attrmdl.pen = where
#define ur_get_attrmdl_line_style() UM_attrmdl.line_style
#define ur_get_new_attrmdl_line_style() UM_new_attrmdl.line_style
#define ur_put_attrmdl_line_style(where) UM_attrmdl.line_style = where
#define ur_put_new_attrmdl_line_style(where) UM_new_attrmdl.line_style = where
#define ur_get_attrmdl_line_weight() UM_attrmdl.line_weight
#define ur_get_new_attrmdl_line_weight() UM_new_attrmdl.line_weight
#define ur_put_attrmdl_line_weight(where) UM_attrmdl.line_weight = where
#define ur_put_new_attrmdl_line_weight(where) UM_new_attrmdl.line_weight = where
#define ur_get_attrmdl_line_width() UM_attrmdl.line_width
#define ur_get_new_attrmdl_line_width() UM_new_attrmdl.line_width
#define ur_put_attrmdl_line_width(where) UM_attrmdl.line_width = where
#define ur_put_new_attrmdl_line_width(where) UM_new_attrmdl.line_width = where
#define ur_get_attrmdl_displayable() UM_attrmdl.displayable
#define ur_get_new_attrmdl_displayable() UM_new_attrmdl.displayable
#define ur_put_attrmdl_displayable(where) UM_attrmdl.displayable = where
#define ur_put_new_attrmdl_displayable(where) UM_new_attrmdl.displayable = where
#define ur_get_attrmdl_selectable() UM_attrmdl.selectable
#define ur_get_new_attrmdl_selectable() UM_new_attrmdl.selectable
#define ur_put_attrmdl_selectable(where) UM_attrmdl.selectable = where
#define ur_put_new_attrmdl_selectable(where) UM_new_attrmdl.selectable = where
#define ur_get_attrmdl_label_on() UM_attrmdl.label_on
#define ur_get_new_attrmdl_label_on() UM_new_attrmdl.label_on
#define ur_put_attrmdl_label_on(where) UM_attrmdl.label_on = where
#define ur_put_new_attrmdl_label_on(where) UM_new_attrmdl.label_on = where

struct UM_mtrlmdl_rec
{
	int	index;
	char	name[64][20];
	UU_REAL	ka[64];
	UU_REAL	kd[64];
	UU_REAL	ks[64];
	UU_REAL	ks_r[64];
	UU_REAL	ks_g[64];
	UU_REAL	ks_b[64];
	UU_REAL	spec_exp[64];
};

extern struct UM_mtrlmdl_rec UM_mtrlmdl;

extern struct UM_mtrlmdl_rec UM_new_mtrlmdl;

#define ur_get_mtrlmdl(where) uu_move_byte(&UM_mtrlmdl, where, sizeof(struct UM_mtrlmdl_rec))
#define ur_get_new_mtrlmdl(where) uu_move_byte(&UM_new_mtrlmdl, where, sizeof(struct UM_mtrlmdl_rec))
#define ur_put_mtrlmdl(where) uu_move_byte(where, &UM_mtrlmdl, sizeof(struct UM_mtrlmdl_rec))
#define ur_put_new_mtrlmdl(where) uu_move_byte(where, &UM_new_mtrlmdl, sizeof(struct UM_mtrlmdl_rec))
#define ur_get_mtrlmdl_index() UM_mtrlmdl.index
#define ur_get_new_mtrlmdl_index() UM_new_mtrlmdl.index
#define ur_put_mtrlmdl_index(where) UM_mtrlmdl.index = where
#define ur_put_new_mtrlmdl_index(where) UM_new_mtrlmdl.index = where
#define ur_get_mtrlmdl_rel_num() UM_mtrlmdl.rel_num
#define ur_get_new_mtrlmdl_rel_num() UM_new_mtrlmdl.rel_num
#define ur_get_mtrlmdl_name(where) uu_move_byte(UM_mtrlmdl.name, where, 1284)
#define ur_get_new_mtrlmdl_name(where) uu_move_byte(UM_new_mtrlmdl.name, where, 1284)
#define ur_put_mtrlmdl_rel_num(where) UM_mtrlmdl.rel_num=where
#define ur_put_new_mtrlmdl_rel_num(where) UM_new_mtrlmdl.rel_num=where
#define ur_put_mtrlmdl_name(where) uu_move_byte(where, UM_mtrlmdl.name, 1284)
#define ur_put_new_mtrlmdl_name(where) uu_move_byte(where, UM_new_mtrlmdl.name, 1284)
#define ur_get_mtrlmdl_ka(where) uu_move_byte(UM_mtrlmdl.ka, where, 512)
#define ur_get_new_mtrlmdl_ka(where) uu_move_byte(UM_new_mtrlmdl.ka, where, 512)
#define ur_put_mtrlmdl_ka(where) uu_move_byte(where, UM_mtrlmdl.ka, 512)
#define ur_put_new_mtrlmdl_ka(where) uu_move_byte(where, UM_new_mtrlmdl.ka, 512)
#define ur_get_mtrlmdl_kd(where) uu_move_byte(UM_mtrlmdl.kd, where, 512)
#define ur_get_new_mtrlmdl_kd(where) uu_move_byte(UM_new_mtrlmdl.kd, where, 512)
#define ur_put_mtrlmdl_kd(where) uu_move_byte(where, UM_mtrlmdl.kd, 512)
#define ur_put_new_mtrlmdl_kd(where) uu_move_byte(where, UM_new_mtrlmdl.kd, 512)
#define ur_get_mtrlmdl_ks(where) uu_move_byte(UM_mtrlmdl.ks, where, 512)
#define ur_get_new_mtrlmdl_ks(where) uu_move_byte(UM_new_mtrlmdl.ks, where, 512)
#define ur_put_mtrlmdl_ks(where) uu_move_byte(where, UM_mtrlmdl.ks, 512)
#define ur_put_new_mtrlmdl_ks(where) uu_move_byte(where, UM_new_mtrlmdl.ks, 512)
#define ur_get_mtrlmdl_ks_r(where) uu_move_byte(UM_mtrlmdl.ks_r, where, 512)
#define ur_get_new_mtrlmdl_ks_r(where) uu_move_byte(UM_new_mtrlmdl.ks_r, where, 512)
#define ur_put_mtrlmdl_ks_r(where) uu_move_byte(where, UM_mtrlmdl.ks_r, 512)
#define ur_put_new_mtrlmdl_ks_r(where) uu_move_byte(where, UM_new_mtrlmdl.ks_r, 512)
#define ur_get_mtrlmdl_ks_g(where) uu_move_byte(UM_mtrlmdl.ks_g, where, 512)
#define ur_get_new_mtrlmdl_ks_g(where) uu_move_byte(UM_new_mtrlmdl.ks_g, where, 512)
#define ur_put_mtrlmdl_ks_g(where) uu_move_byte(where, UM_mtrlmdl.ks_g, 512)
#define ur_put_new_mtrlmdl_ks_g(where) uu_move_byte(where, UM_new_mtrlmdl.ks_g, 512)
#define ur_get_mtrlmdl_ks_b(where) uu_move_byte(UM_mtrlmdl.ks_b, where, 512)
#define ur_get_new_mtrlmdl_ks_b(where) uu_move_byte(UM_new_mtrlmdl.ks_b, where, 512)
#define ur_put_mtrlmdl_ks_b(where) uu_move_byte(where, UM_mtrlmdl.ks_b, 512)
#define ur_put_new_mtrlmdl_ks_b(where) uu_move_byte(where, UM_new_mtrlmdl.ks_b, 512)
#define ur_get_mtrlmdl_spec_exp(where) uu_move_byte(UM_mtrlmdl.spec_exp, where, 512)
#define ur_get_new_mtrlmdl_spec_exp(where) uu_move_byte(UM_new_mtrlmdl.spec_exp, where, 512)
#define ur_put_mtrlmdl_spec_exp(where) uu_move_byte(where, UM_mtrlmdl.spec_exp, 512)
#define ur_put_new_mtrlmdl_spec_exp(where) uu_move_byte(where, UM_new_mtrlmdl.spec_exp, 512)

struct UM_dispattr_rec
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

extern struct UM_dispattr_rec UM_dispattr;

extern struct UM_dispattr_rec UM_new_dispattr;

#define ur_get_dispattr(where) uu_move_byte(&UM_dispattr, where, sizeof(struct UM_dispattr_rec))
#define ur_get_new_dispattr(where) uu_move_byte(&UM_new_dispattr, where, sizeof(struct UM_dispattr_rec))
#define ur_put_dispattr(where) uu_move_byte(where, &UM_dispattr, sizeof(struct UM_dispattr_rec))
#define ur_put_new_dispattr(where) uu_move_byte(where, &UM_new_dispattr, sizeof(struct UM_dispattr_rec))
#define ur_get_dispattr_consclr() UM_dispattr.consclr
#define ur_get_new_dispattr_consclr() UM_new_dispattr.consclr
#define ur_put_dispattr_consclr(where) UM_dispattr.consclr = where
#define ur_put_new_dispattr_consclr(where) UM_new_dispattr.consclr = where
#define ur_get_dispattr_rel_num() UM_dispattr.rel_num
#define ur_get_new_dispattr_rel_num() UM_new_dispattr.rel_num
#define ur_get_dispattr_conslnstyl() UM_dispattr.conslnstyl
#define ur_get_new_dispattr_conslnstyl() UM_new_dispattr.conslnstyl
#define ur_put_dispattr_rel_num(where) UM_dispattr.rel_num=where
#define ur_put_new_dispattr_rel_num(where) UM_new_dispattr.rel_num=where
#define ur_put_dispattr_conslnstyl(where) UM_dispattr.conslnstyl = where
#define ur_put_new_dispattr_conslnstyl(where) UM_new_dispattr.conslnstyl = where
#define ur_get_dispattr_conswght() UM_dispattr.conswght
#define ur_get_new_dispattr_conswght() UM_new_dispattr.conswght
#define ur_put_dispattr_conswght(where) UM_dispattr.conswght = where
#define ur_put_new_dispattr_conswght(where) UM_new_dispattr.conswght = where
#define ur_get_dispattr_featclr() UM_dispattr.featclr
#define ur_get_new_dispattr_featclr() UM_new_dispattr.featclr
#define ur_put_dispattr_featclr(where) UM_dispattr.featclr = where
#define ur_put_new_dispattr_featclr(where) UM_new_dispattr.featclr = where
#define ur_get_dispattr_fea_decpl() UM_dispattr.fea_decpl
#define ur_get_new_dispattr_fea_decpl() UM_new_dispattr.fea_decpl
#define ur_put_dispattr_fea_decpl(where) UM_dispattr.fea_decpl = where
#define ur_put_new_dispattr_fea_decpl(where) UM_new_dispattr.fea_decpl = where
#define ur_get_dispattr_fillclr() UM_dispattr.fillclr
#define ur_get_new_dispattr_fillclr() UM_new_dispattr.fillclr
#define ur_put_dispattr_fillclr(where) UM_dispattr.fillclr = where
#define ur_put_new_dispattr_fillclr(where) UM_new_dispattr.fillclr = where
#define ur_get_dispattr_hidn_lines() UM_dispattr.hidn_lines
#define ur_get_new_dispattr_hidn_lines() UM_new_dispattr.hidn_lines
#define ur_put_dispattr_hidn_lines(where) UM_dispattr.hidn_lines = where
#define ur_put_new_dispattr_hidn_lines(where) UM_new_dispattr.hidn_lines = where
#define ur_get_dispattr_view_in() UM_dispattr.view_in
#define ur_get_new_dispattr_view_in() UM_new_dispattr.view_in
#define ur_put_dispattr_view_in(where) UM_dispattr.view_in = where
#define ur_put_new_dispattr_view_in(where) UM_new_dispattr.view_in = where
#define ur_get_dispattr_cpln_key() UM_dispattr.cpln_key
#define ur_get_new_dispattr_cpln_key() UM_new_dispattr.cpln_key
#define ur_put_dispattr_cpln_key(where) UM_dispattr.cpln_key = where
#define ur_put_new_dispattr_cpln_key(where) UM_new_dispattr.cpln_key = where
