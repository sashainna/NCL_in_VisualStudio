/*********************************************************************
**    NAME         :  ver8103.h
**       CONTAINS:
**       definitions used before the 8.103 version of Unibase. 
**    DATE AND TIME OF LAST MODIFICATION
**       rver8103.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:48
*********************************************************************/

struct curve80
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   no_param;
   float *param;
   int   no_segment;
   struct NCL_segment_rec  *segment;
   char  varlistbuf[NCL_CURVE_BUFSZ];
};

struct ccv80
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   UU_REAL  arclen;
   UU_LOGICAL  planar;
   UU_LOGICAL  open;
   int   continuity;
   int   fcolor;
   int   no_cid;
   struct UM_cid_rec *cid;
   char  varlistbuf[UM_COMPCRV_BUFSZ];
};

struct agcv80
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   crvaddr;
};

struct surf80
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   surf_type;
   int   numupaths;
   int   numvpaths;
   int   ptsperucrv;
   int   ptspervcrv;
   int   rldnu;
   int   rldnv;
   int   offset;
   float offdist;
   int   no_panelkey;
   UU_KEY_ID   *panelkey;
   char  varlistbuf[NCL_SURFACE_BUFSZ];
};

struct msf80
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   surf_type;
   int   m;
   int   n;
   int   numupaths;
   int   numvpaths;
   int   ptsperucrv;
   int   ptspervcrv;
   int   rldnu;
   int   rldnv;
   int   offset;
   float offdist;
   int   no_mpatch;
   struct NCL_mpatch_rec   *mpatch;
   char  varlistbuf[NCL_MESHSF_BUFSZ];
};

struct evsf80
{
   UU_KEY_ID   key;
   int   rel_num;
   char  label[8];
   int   subscr;
   int   numupaths;
   int   numvpaths;
   int   ptsperucrv;
   int   ptspervcrv;
   int   rldnu;
   int   rldnv;
   int   surf_type;
   int   offset;
   float offdist;
   int   no_evwd;
   UU_REAL  *evwd;
   char  varlistbuf[NCL_EVALSF_BUFSZ];
};

struct agsf80
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
   int   srfaddr;
};
