/*********************************************************************
**    NAME         :  mdraw.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UM_drwmdl_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	int	use_count;
	UU_KEY_ID	drwsn;
	UU_KEY_ID	drwvp;
	UU_KEY_ID	drwvw;
	UU_KEY_ID	curdrw;
	char	modsname[16];
	UU_REAL	aspect;
	char	unitsstr[100];
	int	drwsize;
	UU_REAL	drwscale;
	int	drwunits;
	UU_REAL	modscale;
	int	modunits;
	UU_REAL	plotprec;
};

extern struct UM_drwmdl_rec UM_drwmdl;

extern struct UM_drwmdl_rec UM_new_drwmdl;

#define ur_get_drwmdl(where) uu_move_byte(&UM_drwmdl, where, sizeof(struct UM_drwmdl_rec))
#define ur_get_new_drwmdl(where) uu_move_byte(&UM_new_drwmdl, where, sizeof(struct UM_drwmdl_rec))
#define ur_put_drwmdl(where) uu_move_byte(where, &UM_drwmdl, sizeof(struct UM_drwmdl_rec))
#define ur_put_new_drwmdl(where) uu_move_byte(where, &UM_new_drwmdl, sizeof(struct UM_drwmdl_rec))
#define ur_get_drwmdl_key() UM_drwmdl.key
#define ur_get_new_drwmdl_key() UM_new_drwmdl.key
#define ur_put_drwmdl_key(where) UM_drwmdl.key = where
#define ur_put_new_drwmdl_key(where) UM_new_drwmdl.key = where
#define ur_get_drwmdl_rel_num() UM_drwmdl.rel_num
#define ur_get_new_drwmdl_rel_num() UM_new_drwmdl.rel_num
#define ur_get_drwmdl_use_count() UM_drwmdl.use_count
#define ur_get_new_drwmdl_use_count() UM_new_drwmdl.use_count
#define ur_put_drwmdl_rel_num(where) UM_drwmdl.rel_num=where
#define ur_put_new_drwmdl_rel_num(where) UM_new_drwmdl.rel_num=where
#define ur_put_drwmdl_use_count(where) UM_drwmdl.use_count = where
#define ur_put_new_drwmdl_use_count(where) UM_new_drwmdl.use_count = where
#define ur_get_drwmdl_drwsn() UM_drwmdl.drwsn
#define ur_get_new_drwmdl_drwsn() UM_new_drwmdl.drwsn
#define ur_put_drwmdl_drwsn(where) UM_drwmdl.drwsn = where
#define ur_put_new_drwmdl_drwsn(where) UM_new_drwmdl.drwsn = where
#define ur_get_drwmdl_drwvp() UM_drwmdl.drwvp
#define ur_get_new_drwmdl_drwvp() UM_new_drwmdl.drwvp
#define ur_put_drwmdl_drwvp(where) UM_drwmdl.drwvp = where
#define ur_put_new_drwmdl_drwvp(where) UM_new_drwmdl.drwvp = where
#define ur_get_drwmdl_drwvw() UM_drwmdl.drwvw
#define ur_get_new_drwmdl_drwvw() UM_new_drwmdl.drwvw
#define ur_put_drwmdl_drwvw(where) UM_drwmdl.drwvw = where
#define ur_put_new_drwmdl_drwvw(where) UM_new_drwmdl.drwvw = where
#define ur_get_drwmdl_curdrw() UM_drwmdl.curdrw
#define ur_get_new_drwmdl_curdrw() UM_new_drwmdl.curdrw
#define ur_put_drwmdl_curdrw(where) UM_drwmdl.curdrw = where
#define ur_put_new_drwmdl_curdrw(where) UM_new_drwmdl.curdrw = where
#define ur_get_drwmdl_modsname(where) uu_move_byte(UM_drwmdl.modsname, where, 20)
#define ur_get_new_drwmdl_modsname(where) uu_move_byte(UM_new_drwmdl.modsname, where, 20)
#define ur_put_drwmdl_modsname(where) uu_move_byte(where, UM_drwmdl.modsname, 20)
#define ur_put_new_drwmdl_modsname(where) uu_move_byte(where, UM_new_drwmdl.modsname, 20)
#define ur_get_drwmdl_aspect() UM_drwmdl.aspect
#define ur_get_new_drwmdl_aspect() UM_new_drwmdl.aspect
#define ur_put_drwmdl_aspect(where) UM_drwmdl.aspect = where
#define ur_put_new_drwmdl_aspect(where) UM_new_drwmdl.aspect = where
#define ur_get_drwmdl_unitsstr(where) uu_move_byte(UM_drwmdl.unitsstr, where, 100)
#define ur_get_new_drwmdl_unitsstr(where) uu_move_byte(UM_new_drwmdl.unitsstr, where, 100)
#define ur_put_drwmdl_unitsstr(where) uu_move_byte(where, UM_drwmdl.unitsstr, 100)
#define ur_put_new_drwmdl_unitsstr(where) uu_move_byte(where, UM_new_drwmdl.unitsstr, 100)
#define ur_get_drwmdl_drwsize() UM_drwmdl.drwsize
#define ur_get_new_drwmdl_drwsize() UM_new_drwmdl.drwsize
#define ur_put_drwmdl_drwsize(where) UM_drwmdl.drwsize = where
#define ur_put_new_drwmdl_drwsize(where) UM_new_drwmdl.drwsize = where
#define ur_get_drwmdl_drwscale() UM_drwmdl.drwscale
#define ur_get_new_drwmdl_drwscale() UM_new_drwmdl.drwscale
#define ur_put_drwmdl_drwscale(where) UM_drwmdl.drwscale = where
#define ur_put_new_drwmdl_drwscale(where) UM_new_drwmdl.drwscale = where
#define ur_get_drwmdl_drwunits() UM_drwmdl.drwunits
#define ur_get_new_drwmdl_drwunits() UM_new_drwmdl.drwunits
#define ur_put_drwmdl_drwunits(where) UM_drwmdl.drwunits = where
#define ur_put_new_drwmdl_drwunits(where) UM_new_drwmdl.drwunits = where
#define ur_get_drwmdl_modscale() UM_drwmdl.modscale
#define ur_get_new_drwmdl_modscale() UM_new_drwmdl.modscale
#define ur_put_drwmdl_modscale(where) UM_drwmdl.modscale = where
#define ur_put_new_drwmdl_modscale(where) UM_new_drwmdl.modscale = where
#define ur_get_drwmdl_modunits() UM_drwmdl.modunits
#define ur_get_new_drwmdl_modunits() UM_new_drwmdl.modunits
#define ur_put_drwmdl_modunits(where) UM_drwmdl.modunits = where
#define ur_put_new_drwmdl_modunits(where) UM_new_drwmdl.modunits = where
#define ur_get_drwmdl_plotprec() UM_drwmdl.plotprec
#define ur_get_new_drwmdl_plotprec() UM_new_drwmdl.plotprec
#define ur_put_drwmdl_plotprec(where) UM_drwmdl.plotprec = where
#define ur_put_new_drwmdl_plotprec(where) UM_new_drwmdl.plotprec = where
