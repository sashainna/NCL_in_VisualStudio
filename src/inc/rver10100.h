
/*********************************************************************
**    NAME         :  rver10100.h
**       CONTAINS:
**       definitions used before the 10.100 version of Unibase. 
**    MODULE NAME AND RELEASE LEVEL
**       rver10100.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:48
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"
#include "nccs.h"
#include "mcrv.h"
#include "msol.h"

struct UM_compcrv_rec100
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[64];
   UU_REAL  labloc[3];
   UU_REAL  ldrloc[3];
   int   subscr;
   int   closdinu;
   UU_REAL  arclen;
   UU_LOGICAL  planar;
   UU_LOGICAL  open;
   int   continuity;
   int   fcolor;
   struct UM_cid_rec *cid;
   int   no_cid;
   int   pad_cid;
   UU_REAL  *displst;
   int   no_displst;
   int   pad_displst;
   char  varlistbuf[UM_COMPCRV_BUFSZ];
};


struct NCL_trimsf_rec100
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	closdinu;
	int	closdinv;
	float	offdist;
	UU_KEY_ID	uv_key;
	UU_KEY_ID	cv_key;
	UU_KEY_ID	bs_key;
	float	ub_min;
	float	ub_max;
	float	vb_min;
	float	vb_max;
	float	u_min;
	float	u_max;
	float	v_min;
	float	v_max;
	int	drive_type;
	UU_KEY_ID	*ibndykey;
	int	no_ibndykey;
	int	pad_ibndykey;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	UU_REAL	*tesslst;
	int	no_tesslst;
	int	pad_tesslst;
	UU_REAL	*boxlst;
	int	no_boxlst;
	int	pad_boxlst;
	UU_REAL	*xyzbylst;
	int	no_xyzbylst;
	int	pad_xyzbylst;
	UU_REAL	*uvbylst;
	int	no_uvbylst;
	int	pad_uvbylst;
	UU_REAL	*uvboxlst;
	int	no_uvboxlst;
	int	pad_uvboxlst;
	char	varlistbuf[NCL_TRIMSF_BUFSZ];
};

struct UM_solid_rec100
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	UU_REAL	ldrloc[3];
	int	subscr;
	int	type;
	UU_REAL	box[6];
	UU_REAL	*sdata;
	int	no_sdata;
	int	pad_sdata;
	UU_REAL	*displst;
	int	no_displst;
	int	pad_displst;
	UU_REAL	*tesslst;
	int	no_tesslst;
	int	pad_tesslst;
	char	varlistbuf[UM_SOLID_BUFSZ];
};
