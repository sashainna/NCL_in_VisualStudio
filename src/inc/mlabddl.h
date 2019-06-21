/*********************************************************************
**    NAME         :  mlabddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UM_labelmdl_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	int	max;
	int	num;
	char	pf[40][21];
	int	next[40];
	char	pfs[40][21];
	int	subscr[40];
	int	issub[40];
	int	rel[256];
};

extern struct UM_labelmdl_rec UM_labelmdl;

extern struct UM_labelmdl_rec UM_new_labelmdl;

#define ur_get_labelmdl(where) uu_move_byte(&UM_labelmdl, where, sizeof(struct UM_labelmdl_rec))
#define ur_get_new_labelmdl(where) uu_move_byte(&UM_new_labelmdl, where, sizeof(struct UM_labelmdl_rec))
#define ur_put_labelmdl(where) uu_move_byte(where, &UM_labelmdl, sizeof(struct UM_labelmdl_rec))
#define ur_put_new_labelmdl(where) uu_move_byte(where, &UM_new_labelmdl, sizeof(struct UM_labelmdl_rec))
#define ur_get_labelmdl_key() UM_labelmdl.key
#define ur_get_new_labelmdl_key() UM_new_labelmdl.key
#define ur_put_labelmdl_key(where) UM_labelmdl.key = where
#define ur_put_new_labelmdl_key(where) UM_new_labelmdl.key = where
#define ur_get_labelmdl_rel_num() UM_labelmdl.rel_num
#define ur_get_new_labelmdl_rel_num() UM_new_labelmdl.rel_num
#define ur_get_labelmdl_use_count() UM_labelmdl.use_count
#define ur_get_new_labelmdl_use_count() UM_new_labelmdl.use_count
#define ur_put_labelmdl_rel_num(where) UM_labelmdl.rel_num=where
#define ur_put_new_labelmdl_rel_num(where) UM_new_labelmdl.rel_num=where
#define ur_put_labelmdl_use_count(where) UM_labelmdl.use_count = where
#define ur_put_new_labelmdl_use_count(where) UM_new_labelmdl.use_count = where
#define ur_get_labelmdl_max() UM_labelmdl.max
#define ur_get_new_labelmdl_max() UM_new_labelmdl.max
#define ur_put_labelmdl_max(where) UM_labelmdl.max = where
#define ur_put_new_labelmdl_max(where) UM_new_labelmdl.max = where
#define ur_get_labelmdl_num() UM_labelmdl.num
#define ur_get_new_labelmdl_num() UM_new_labelmdl.num
#define ur_put_labelmdl_num(where) UM_labelmdl.num = where
#define ur_put_new_labelmdl_num(where) UM_new_labelmdl.num = where
#define ur_get_labelmdl_pf(where) uu_move_byte(UM_labelmdl.pf, where, 840)
#define ur_get_new_labelmdl_pf(where) uu_move_byte(UM_new_labelmdl.pf, where, 840)
#define ur_put_labelmdl_pf(where) uu_move_byte(where, UM_labelmdl.pf, 840)
#define ur_put_new_labelmdl_pf(where) uu_move_byte(where, UM_new_labelmdl.pf, 840)
#define ur_get_labelmdl_next(where) uu_move_byte(UM_labelmdl.next, where, 160)
#define ur_get_new_labelmdl_next(where) uu_move_byte(UM_new_labelmdl.next, where, 160)
#define ur_put_labelmdl_next(where) uu_move_byte(where, UM_labelmdl.next, 160)
#define ur_put_new_labelmdl_next(where) uu_move_byte(where, UM_new_labelmdl.next, 160)
#define ur_get_labelmdl_pfs(where) uu_move_byte(UM_labelmdl.pfs, where, 840)
#define ur_get_new_labelmdl_pfs(where) uu_move_byte(UM_new_labelmdl.pfs, where, 840)
#define ur_put_labelmdl_pfs(where) uu_move_byte(where, UM_labelmdl.pfs, 840)
#define ur_put_new_labelmdl_pfs(where) uu_move_byte(where, UM_new_labelmdl.pfs, 840)
#define ur_get_labelmdl_subscr(where) uu_move_byte(UM_labelmdl.subscr, where, 160)
#define ur_get_new_labelmdl_subscr(where) uu_move_byte(UM_new_labelmdl.subscr, where, 160)
#define ur_put_labelmdl_subscr(where) uu_move_byte(where, UM_labelmdl.subscr, 160)
#define ur_put_new_labelmdl_subscr(where) uu_move_byte(where, UM_new_labelmdl.subscr, 160)
#define ur_get_labelmdl_issub(where) uu_move_byte(UM_labelmdl.issub, where, 160)
#define ur_get_new_labelmdl_issub(where) uu_move_byte(UM_new_labelmdl.issub, where, 160)
#define ur_put_labelmdl_issub(where) uu_move_byte(where, UM_labelmdl.issub, 160)
#define ur_put_new_labelmdl_issub(where) uu_move_byte(where, UM_new_labelmdl.issub, 160)
#define ur_get_labelmdl_rel(where) uu_move_byte(UM_labelmdl.rel, where, 1024)
#define ur_get_new_labelmdl_rel(where) uu_move_byte(UM_new_labelmdl.rel, where, 1024)
#define ur_put_labelmdl_rel(where) uu_move_byte(where, UM_labelmdl.rel, 1024)
#define ur_put_new_labelmdl_rel(where) uu_move_byte(where, UM_new_labelmdl.rel, 1024)
