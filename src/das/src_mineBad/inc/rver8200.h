/*********************************************************************
**    NAME         :  ver8101.h
**       CONTAINS:
**       definitions used before the 8.301 version of Unibase. 
**    DATE AND TIME OF LAST MODIFICATION
**       rver8200.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:48
*********************************************************************/

static int REL_92_LSIZE[1] = { 32 };

struct NCL_patpnt_rec
{
 UU_REAL pt[3];
 int notused1;
 int notused2;
};

struct pn8105
{
 UU_KEY_ID key;
 int rel_num;
 char label[8];
 int subscr;
 int markertype;
 int no_patpnt;
 struct NCL_patpnt_rec *patpnt;
 char varlistbuf[NCL_PATERN_BUFSZ];
};

struct vp8105
{
   UU_KEY_ID   key;
   int   rel_num;
   char  name[15];
   int   xform;
   UU_REAL  llf[3];
   UU_REAL  urb[3];
   UU_KEY_ID   cur_view;
   int   disp_prio;
   int   input_prio;
   UU_LOGICAL  disp_all;
   int   bord_seg;
   UU_LOGICAL  aperture_on;
   UU_LOGICAL  v_axis_on;
   UU_LOGICAL  name_on;
   UU_LOGICAL  bord_on;
   int   nverts;
   UU_REAL  vertices[60];
   int   grid_seg;
   UU_LOGICAL  grid_on;
};

struct rbcv8105
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[8];
	int   subscr;
	UU_LOGICAL  planar;
	UU_LOGICAL  open;
	int   closdinu;
	int   k;
	int   n;
	UU_REAL  t0;
	UU_REAL  t1;
	int   no_t;
	UU_REAL  *t;
	int   no_pt;
	UU_REAL  *pt;
	int   no_wt;
	UU_REAL  *wt;
	char  varlistbuf[UM_RBSPLCRV_BUFSZ];
};

struct surf8105
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[8];
	int   subscr;
	int   material;
	int   numupaths;
	int   numvpaths;
	int   ptsperucrv;
	int   ptspervcrv;
	int   rldnu;
	int   rldnv;
	UU_LOGICAL  rev_normal;
	int   closdinu;
	int   closdinv;
	int   offset;
	float offdist;
	int   surf_type;
	int   no_panelkey;
	UU_KEY_ID   *panelkey;
	char  varlistbuf[NCL_SURFACE_BUFSZ];
};

struct msf8105
{
	UU_KEY_ID   key;
	int   rel_num;
	char  label[8];
	int   subscr;
	int   material;
	int   numupaths;
	int   numvpaths;
	int   ptsperucrv;
	int   ptspervcrv;
	int   rldnu;
	int   rldnv;
	UU_LOGICAL  rev_normal;
	int   closdinu;
	int   closdinv;
	int   offset;
	float offdist;
	int   surf_type;
	int   m;
	int   n;
	int   no_mpatch;
	struct NCL_mpatch_rec   *mpatch;
	char  varlistbuf[NCL_MESHSF_BUFSZ];
};
