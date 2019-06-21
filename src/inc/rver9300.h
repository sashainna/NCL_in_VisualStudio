/*********************************************************************
**    NAME         :  rver9300.h
**       CONTAINS:
**       definitions used before the 9.300 version of Unibase. 
**    MODULE NAME AND RELEASE LEVEL
**       rver9300.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:49
*********************************************************************/
struct revsurf92
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[64];
   UU_REAL  labloc[3];
   int   subscr;
   int   material;
   int   numupaths;
   int   numvpaths;
   int   ptsperucrv;
   int   ptspervcrv;
   int   closdinu;
   int   closdinv;
   UU_REAL  offdist;
   int   surf_type;
   int   primitive;
   UU_REAL  prim_param[16];
   UU_LOGICAL  shaded;
   int   lucency;
   UU_LOGICAL  rev_normal;
   UU_KEY_ID   cvkey;
   UU_REAL  pta[3];
   UU_REAL  vca[3];
   UU_REAL  sa;
   UU_REAL  ta;
   int   no_sskey;
   UU_KEY_ID   *sskey;
   int   no_displst;
   UU_REAL  *displst;
   int   no_tesslst;
   UU_REAL  *tesslst;
   int   no_boxlst;
   UU_REAL  *boxlst;
   char    varlistbuf[NCL_SURFACE_BUFSZ];
};

struct revsurf93
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
	int	closdinu;
	int	closdinv;
	UU_REAL	offdist;
	int	surf_type;
	int	primitive;
	UU_REAL	prim_param[16];
	UU_LOGICAL	shaded;
	int	lucency;
	UU_LOGICAL	rev_normal;
	UU_KEY_ID	cvkey;
	UU_REAL	pta[3];
	UU_REAL	vca[3];
	UU_REAL	sa;
	UU_REAL	ta;
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
	char	varlistbuf[NCL_REVSURF_BUFSZ];
};
