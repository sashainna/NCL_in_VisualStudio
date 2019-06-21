/*********************************************************************
**    NAME         :  riddl.h
**       CONTAINS:
**       definitions generated from UniDDL file
**    DATE AND TIME OF LAST MODIFICATION
**       Mon May 04 14:29:47  2015
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"

struct UR_part_lis_rec
{
	char	part_name[16];
	char	part_date[16];
};

struct UR_geometry_rec
{
	char	geom_type[16];
	int	geom_id;
	int	num_varl;
};

struct UR_MTID_rec
{
	int	dsegid;
	int	save1;
	UU_KEY_ID	view_key;
	int	bit_tbl;
	UU_KEY_ID	*assocs;
	int	no_assocs;
	int	pad_assocs;
	char	varlistbuf[UR_MTID_BUFSZ];
};

struct UR_unimod_rec
{
	UU_LOGICAL	dflt_editable;
	UU_LOGICAL	dflt_blanked;
};

extern struct UR_unimod_rec UR_unimod;

extern struct UR_unimod_rec UR_new_unimod;

#define ur_get_unimod(where) uu_move_byte(&UR_unimod, where, sizeof(struct UR_unimod_rec))
#define ur_get_new_unimod(where) uu_move_byte(&UR_new_unimod, where, sizeof(struct UR_unimod_rec))
#define ur_put_unimod(where) uu_move_byte(where, &UR_unimod, sizeof(struct UR_unimod_rec))
#define ur_put_new_unimod(where) uu_move_byte(where, &UR_new_unimod, sizeof(struct UR_unimod_rec))
#define ur_get_unimod_dflt_editable() UR_unimod.dflt_editable
#define ur_get_new_unimod_dflt_editable() UR_new_unimod.dflt_editable
#define ur_put_unimod_dflt_editable(where) UR_unimod.dflt_editable = where
#define ur_put_new_unimod_dflt_editable(where) UR_new_unimod.dflt_editable = where
#define ur_get_unimod_rel_num() UR_unimod.rel_num
#define ur_get_new_unimod_rel_num() UR_new_unimod.rel_num
#define ur_get_unimod_dflt_blanked() UR_unimod.dflt_blanked
#define ur_get_new_unimod_dflt_blanked() UR_new_unimod.dflt_blanked
#define ur_put_unimod_rel_num(where) UR_unimod.rel_num=where
#define ur_put_new_unimod_rel_num(where) UR_new_unimod.rel_num=where
#define ur_put_unimod_dflt_blanked(where) UR_unimod.dflt_blanked = where
#define ur_put_new_unimod_dflt_blanked(where) UR_new_unimod.dflt_blanked = where

struct UR_unistat_rec
{
	UU_KEY_ID	key;
	int	rel_num;
	char	system[80];
	char	processor[80];
	char	author[80];
	char	company[80];
	char	fname[256];
	char	date[40];
	char	translator[80];
	char	mod_system[80];
	char	mod_author[80];
	char	mod_company[80];
	char	mod_date[40];
	char	*notes;
	int	no_notes;
	int	pad_notes;
	char	varlistbuf[UR_UNISTAT_BUFSZ];
};
