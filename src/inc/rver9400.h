/*********************************************************************
**    NAME         :  rver9400.h
**       CONTAINS:
**       definitions used before the 9.400 version of Unibase. 
**    MODULE NAME AND RELEASE LEVEL
**       rver9400.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:49
*********************************************************************/
#include "usysdef.h"
#include "umoveb.h"
#include "nccs.h"
#include "mxxx.h"

struct layer93
{
	UU_KEY_ID	key;
	int	rel_num;
	char	name[16];
	int	num;
};

struct UM_light_rec93
{
	UU_KEY_ID	key;
	int	rel_num;
	int	index;
	int	type;
	int	intens;
	UU_REAL	position[3];
	UU_REAL	direction[3];
	UU_REAL	attenuation[2]; 
	UU_REAL	cone_angle;
	UU_REAL	scale;  
};

struct NCL_evalsf_rec93
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	int	rldnu;
	int	rldnv;
	int	closdinu;
	int	closdinv;
	int	surf_type;
	int	offset;
	UU_LOGICAL	shaded;
	int	lucency;
	float	offdist;
	int	no_evwd;
	UU_REAL	*evwd;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	char	varlistbuf[4000];
};

struct NCL_quiltsf_rec93
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	surf_type;
	int	numpatches;
	int	rldnu;
	int	rldnv;
	int	offset;
	float	offdist;
	float	midpt[12][3];
	UU_LOGICAL	shaded;
	int	lucency;
	int	dummy;
	int	no_qpatch;
	struct NCL_qpatch_rec	*qpatch;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	int	no_boxlst;
	UU_REAL	*boxlst;
	int	no_xyzbylst;
	UU_REAL	*xyzbylst;
	char	varlistbuf[4000];
};


struct NCL_netsf_rec93
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	surf_type;
	int	rldnu;
	int	rldnv;
	int	bndsfs[40][4];
	UU_LOGICAL	shaded;
	int	lucency;
	int	no_netkey;
	UU_KEY_ID	*netkey;
	int	no_sskey;
	UU_KEY_ID	*sskey;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	char	varlistbuf[4000];
};

struct NCL_shape_rec93
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	f2d3d;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	UU_LOGICAL	shaded;
	int	lucency;
	int	dummy;
	int	no_shapwd;
	UU_REAL	*shapwd;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	char	varlistbuf[3200];
};

struct NCL_quiltsf_rec94
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	material;
	int	surf_type;
	int	numpatches;
	int	rldnu;
	int	rldnv;
	int	offset;
	float	offdist;
	float	midpt[12][3];
	UU_LOGICAL	shaded;
	int	lucency;
	int	dummy;
	int	no_qpatch;
	struct NCL_qpatch_rec	*qpatch;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	int	no_boxlst;
	UU_REAL	*boxlst;
	int	no_xyzbylst;
	UU_REAL	*xyzbylst;
	char	varlistbuf[NCL_QUILTSF_BUFSZ];
};

struct NCL_netsf_rec94
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	material;
	int	surf_type;
	int	rldnu;
	int	rldnv;
	int	bndsfs[40][4];
	UU_LOGICAL	shaded;
	int	lucency;
	int	no_netkey;
	UU_KEY_ID	*netkey;
	int	no_sskey;
	UU_KEY_ID	*sskey;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	char	varlistbuf[NCL_NETSF_BUFSZ];
};

struct NCL_shape_rec94
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	material;
	int	f2d3d;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	UU_LOGICAL	shaded;
	int	lucency;
	int	dummy;
	int	no_shapwd;
	UU_REAL	*shapwd;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	char	varlistbuf[NCL_SHAPE_BUFSZ];
};

struct NCL_evalsf_rec94
{
	UU_KEY_ID	key;
	int	rel_num;
	char	label[64];
	UU_REAL	labloc[3];
	int	subscr;
	int	material;
	int	numupaths;
	int	numvpaths;
	int	ptsperucrv;
	int	ptspervcrv;
	int	rldnu;
	int	rldnv;
	int	closdinu;
	int	closdinv;
	int	surf_type;
	int	offset;
	UU_LOGICAL	shaded;
	int	lucency;
	float	offdist;
	int	no_evwd;
	UU_REAL	*evwd;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	char	varlistbuf[NCL_EVALSF_BUFSZ];
};

struct UM_light_rec94
{
	UU_KEY_ID	key;
	int	rel_num;
	int	index;
	int	type;
	int	intens;
	UU_REAL	position[3];
	UU_REAL	direction[3];
	UU_REAL	attenuation[3];
	UU_REAL	cone_angle;
	UU_REAL	scale;
	int	space;
	UU_REAL	exp;
	UU_REAL	ambient[4];
};

struct layer94
{
	UU_KEY_ID	key;
	int	rel_num;
	char	name[80];
	int	num;
	UU_LOGICAL	displayable;
	UU_LOGICAL	selectable;
	int	no_layers;
	int	*layers;
	char	varlistbuf[UM_LAYER_BUFSZ];
};

struct UM_mtrlmdl_oldrec
{
	int	index;
	UU_REAL	kd[64];
	UU_REAL	ks[64];
	UU_REAL	spec_exp[64];
};

#ifdef V94MAIN
struct UM_mtrlmdl_oldrec UM_mtrlmdl_old;
struct UM_mtrlmdl_oldrec UM_new_mtrlmdl_old;
#else
extern struct UM_mtrlmdl_oldrec UM_mtrlmdl_old;
extern struct UM_mtrlmdl_oldrec UM_new_mtrlmdl_old;
#endif
