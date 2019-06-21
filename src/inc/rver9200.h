/*********************************************************************
**    NAME         :  rver9200.h
**       CONTAINS:
**       definitions used before the 9.200 version of Unibase. 
**    MODULE NAME AND RELEASE LEVEL
**       rver9200.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:49
*********************************************************************/
struct surf91
{
        UU_KEY_ID       key;
        int     rel_num;
        char    label[64];
        UU_REAL labloc[3];
        int     subscr;
        int     material;
        int     numupaths;
        int     numvpaths;
        int     ptsperucrv;
        int     ptspervcrv;
        int     rldnu;
        int     rldnv;
        UU_LOGICAL      rev_normal;
        int     closdinu;
        int     closdinv;
        int     offset;
        float   offdist;
        int     surf_type;
        UU_LOGICAL      shaded;
        int     lucency;
        int     dummy;
        int     no_panelkey;
        UU_KEY_ID       *panelkey;
        int     no_sskey;
        UU_KEY_ID       *sskey;
        int     no_displst;
        UU_REAL *displst;
        int     no_tesslst;
        UU_REAL *tesslst;
        int     no_boxlst;
        UU_REAL *boxlst;
        int     no_xyzbylst;
        UU_REAL *xyzbylst;
        char    varlistbuf[NCL_SURFACE_BUFSZ];
};

struct rbsf91
{
        UU_KEY_ID       key;
        int     rel_num;
        char    label[64];
        UU_REAL labloc[3];
        int     subscr;
        int     material;
        int     numupaths;
        int     numvpaths;
        int     ptsperucrv;
        int     ptspervcrv;
        int     rldnu;
        int     rldnv;
        UU_LOGICAL      rev_normal;
        int     closdinu;
        int     closdinv;
        int     offset;
        UU_REAL offdist;
        int     dumm1;
        int     ku;
        int     kv;
        int     nu;
        int     nv;
        UU_LOGICAL      shaded;
        int     lucency;
        int     dummy;
        int     no_tu;
        UU_REAL *tu;
        int     no_tv;
        UU_REAL *tv;
        int     no_pt;
        UU_REAL *pt;
        int     no_wt;
        UU_REAL *wt;
        int     no_sskey;
        UU_KEY_ID       *sskey;
        int     no_displst;
        UU_REAL *displst;
        int     no_tesslst;
        UU_REAL *tesslst;
        int     no_boxlst;
        UU_REAL *boxlst;
        int     no_xyzbylst;
        UU_REAL *xyzbylst;
        char    varlistbuf[UM_RBSPLSRF_BUFSZ];
};


struct surf92
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
	UU_LOGICAL	rev_normal;
	int	closdinu;
	int	closdinv;
	int	offset;
	float	offdist;
	int	surf_type;
	int	dum1;
	int	primitive;
	UU_REAL	prim_param[16];
	UU_LOGICAL	shaded;
	int	lucency;
	int	no_panelkey;
	UU_KEY_ID	*panelkey;
	int	no_sskey;
	UU_KEY_ID	*sskey;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	int	no_boxlst;
	UU_REAL	*boxlst;
	int	no_xyzbylst;
	UU_REAL	*xyzbylst;
	char	varlistbuf[NCL_SURFACE_BUFSZ];
};

struct rbsf92
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
	UU_LOGICAL	rev_normal;
	int	closdinu;
	int	closdinv;
	int	offset;
	UU_REAL	offdist;
	int	ku;
	int	kv;
	int	nu;
	int	nv;
	int	dum1;
	int	primitive;
	UU_REAL	prim_param[16];
	UU_LOGICAL	shaded;
	int	lucency;
	int	no_tu;
	UU_REAL	*tu;
	int	no_tv;
	UU_REAL	*tv;
	int	no_pt;
	UU_REAL	*pt;
	int	no_wt;
	UU_REAL	*wt;
	int	no_sskey;
	UU_KEY_ID	*sskey;
	int	no_displst;
	UU_REAL	*displst;
	int	no_tesslst;
	UU_REAL	*tesslst;
	int	no_boxlst;
	UU_REAL	*boxlst;
	int	no_xyzbylst;
	UU_REAL	*xyzbylst;
	char	varlistbuf[UM_RBSPLSRF_BUFSZ];
};
