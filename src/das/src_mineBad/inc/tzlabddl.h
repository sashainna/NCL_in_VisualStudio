/*********************************************************************
**    NAME         :  mlabddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    MODULE NAME AND RELEASE LEVEL
**         tzlabddl.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:00
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct TZ_labelmdl_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	int	max;
	int	num;
	char	pf[20][3];
	int	next[20];
	int	rel[256];
};

extern struct TZ_labelmdl_rec TZ_labelmdl;

extern struct TZ_labelmdl_rec TZ_new_labelmdl;
/*MILLS: OLD LABEL STRUCTURE */
struct OTZ_labelmdl_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	int	max;
	int	num;
	char	pf[10][3];
	int	next[10];
	int	rel[256];
};

extern struct OTZ_labelmdl_rec OTZ_labelmdl;

extern struct OTZ_labelmdl_rec OTZ_new_labelmdl;

#define ur_get_labelmdl(where) uu_move_byte(&TZ_labelmdl, where, sizeof(struct TZ_labelmdl_rec))
#define ur_get_new_labelmdl(where) uu_move_byte(&TZ_new_labelmdl, where, sizeof(struct TZ_labelmdl_rec))
#define ur_put_labelmdl(where) uu_move_byte(where, &TZ_labelmdl, sizeof(struct TZ_labelmdl_rec))
#define ur_put_new_labelmdl(where) uu_move_byte(where, &TZ_new_labelmdl, sizeof(struct TZ_labelmdl_rec))
#define ur_get_labelmdl_key() TZ_labelmdl.key
#define ur_get_new_labelmdl_key() TZ_new_labelmdl.key
#define ur_put_labelmdl_key(where) TZ_labelmdl.key = where
#define ur_put_new_labelmdl_key(where) TZ_new_labelmdl.key = where
#define ur_get_labelmdl_rel_num() TZ_labelmdl.rel_num
#define ur_get_new_labelmdl_rel_num() TZ_new_labelmdl.rel_num
#define ur_get_labelmdl_use_count() TZ_labelmdl.use_count
#define ur_get_new_labelmdl_use_count() TZ_new_labelmdl.use_count
#define ur_put_labelmdl_rel_num(where) TZ_labelmdl.rel_num=where
#define ur_put_new_labelmdl_rel_num(where) TZ_new_labelmdl.rel_num=where
#define ur_put_labelmdl_use_count(where) TZ_labelmdl.use_count = where
#define ur_put_new_labelmdl_use_count(where) TZ_new_labelmdl.use_count = where
#define ur_get_labelmdl_max() TZ_labelmdl.max
#define ur_get_new_labelmdl_max() TZ_new_labelmdl.max
#define ur_put_labelmdl_max(where) TZ_labelmdl.max = where
#define ur_put_new_labelmdl_max(where) TZ_new_labelmdl.max = where
#define ur_get_labelmdl_num() TZ_labelmdl.num
#define ur_get_new_labelmdl_num() TZ_new_labelmdl.num
#define ur_put_labelmdl_num(where) TZ_labelmdl.num = where
#define ur_put_new_labelmdl_num(where) TZ_new_labelmdl.num = where
#define ur_get_labelmdl_pf(where) uu_move_byte(TZ_labelmdl.pf, where, 32)
#define ur_get_new_labelmdl_pf(where) uu_move_byte(TZ_new_labelmdl.pf, where, 32)
#define ur_put_labelmdl_pf(where) uu_move_byte(where, TZ_labelmdl.pf, 32)
#define ur_put_new_labelmdl_pf(where) uu_move_byte(where, TZ_new_labelmdl.pf, 32)
#define ur_get_labelmdl_next(where) uu_move_byte(TZ_labelmdl.next, where, 40)
#define ur_get_new_labelmdl_next(where) uu_move_byte(TZ_new_labelmdl.next, where, 40)
#define ur_put_labelmdl_next(where) uu_move_byte(where, TZ_labelmdl.next, 40)
#define ur_put_new_labelmdl_next(where) uu_move_byte(where, TZ_new_labelmdl.next, 40)
#define ur_get_labelmdl_rel(where) uu_move_byte(TZ_labelmdl.rel, where, 1024)
#define ur_get_new_labelmdl_rel(where) uu_move_byte(TZ_new_labelmdl.rel, where, 1024)
#define ur_put_labelmdl_rel(where) uu_move_byte(where, TZ_labelmdl.rel, 1024)
#define ur_put_new_labelmdl_rel(where) uu_move_byte(where, TZ_new_labelmdl.rel, 1024)
