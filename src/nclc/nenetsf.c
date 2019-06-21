/*********************************************************************
**    NAME         :  nenetsf.c
**       CONTAINS: routines to handle NCL net surfaces.
**
**       int ncl_net_nclsf
**       int ncl_net_mshsf
**       int ncl_net_agsrf
**       int ncl_net_evalsf(keyin, keyout)
**       int ncl_net_trimsf
**       int ncl_net_netsf
**       int ncl_get_netsf
**       int ssfcre
**       int ssfupd
**       int dspnwf
**       int isnswf
**       int ncl_transform_netsf (e, tfmat, store)
**       int ncl_class_copy_netsf (e1, e2, bagsize)
**       int ncl_copy_netsf (e1, e2)
**       int ncl_proj_netsf_to_drawing
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nenetsf.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:39
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "mdrel.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdebug.h"
#include "mdeval.h"
#include "class.h"
#include "ag_surfaces.h"
#include "mdattr.h"
#include "mattr.h"

#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclvx.h"

extern UU_LOGICAL UM_set_constituent_pickids;

/*********************************************************************
**    E_FUNCTION     : int ncl_net_nclsf(keyin, keyout)
**       Store an NCL surface into a net surface.
**    PARAMETERS   
**       INPUT  : 
**          keyin             Key of NCL sf to put in net sf
**       OUTPUT :  
**          keyout            Key of sf in net sf
**          none
**    RETURNS      : UU_SUCCESS on success, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_net_nclsf(keyin, keyout)
UU_KEY_ID keyin, *keyout;
{
   struct NCL_surface_rec nsf;
   struct NCL_surface_rec nnsf;
   int status;

   uu_denter(UU_MTRC,(us,"ncl_net_nclsf(keyin=%x, keyout=%x)",
      keyin, keyout));
   
   *keyout = 0;
   nsf.key = keyin;
   status = ncl_retrieve_data_fixed (&nsf);
   nnsf.subscr = 0;
   strcpy (nnsf.label,"");
   if (status == UU_SUCCESS) 
     status = ncl_copy_nsf (&nsf, &nnsf);
   if (status == UU_SUCCESS) 
   {
     *keyout = nnsf.key;
     strncpy (nnsf.label,"@UN    ",7);
     status = ur_update_data_fixed(&nnsf);
     ur_update_displayable(nnsf.key, UM_NEVERDISPLAYABLE);
   }

   uu_dexit;
   return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_net_mshsf(keyin, keyout)
**       Store a mesh surface into a net surface.
**    PARAMETERS   
**       INPUT  : 
**          keyin             Key of mesh sf to put in net sf
**       OUTPUT :  
**          keyout            Key of sf in net sf
**    RETURNS      : UU_SUCCESS on success, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_net_mshsf(keyin, keyout)
   UU_KEY_ID keyin, *keyout;
   {

   struct NCL_meshsf_rec msh1,msh2;
   struct NCL_mpatch_rec mpat;
   int ipat,status;
   struct UC_attributedatabag attr;

   uu_denter(UU_MTRC,(us,"ncl_net_mshsf(keyin=%x, keyout=%x)",
      keyin, keyout));

   status = UU_SUCCESS;

   msh1.key = keyin;
   if (ur_retrieve_data_relnum(msh1.key, &msh1.rel_num) != 0)
      status = UU_FAILURE;
   else if (msh1.rel_num != NCL_MESHSURF_REL)
      status = UU_FAILURE;
   else
      {
      if (ur_retrieve_data_fixed(&msh1) != 0)
         status = UU_FAILURE;
      else
         {
         ur_setup_data(NCL_MESHSURF_REL, &msh2, sizeof(msh2));

		 /* initialize key to zero. this tells ncl_create_entity() to create
			instead of update entity */
		 msh2.key = 0;
	     /* RAH: initialize label to null - corrects loadu/ */
/*	     strcpy(msh2.label,"");  */
      strncpy(msh2.label,"@UN    ",7);
	     msh2.subscr = 0;
         msh2.surf_type = NCLI_MESHSURF;
         msh2.no_mpatch = 0;
         msh2.m = msh1.m;
         msh2.n = msh1.n;
         msh2.offset = msh1.offset;
         msh2.offdist = msh1.offdist;

		 /* Initialize ruled-in-u/v variables */
		 msh2.rldnu = msh1.rldnu;
		 msh2.swapuv = 0;

   /* RAH: use uc_create_mtuple_data() to update attributes, etc */
   /* also prevents label from being generated ... */
         uc_retrieve_attr(msh1.key, &attr);
         uc_create_mtuple_data(&msh2, UM_DEFAULT_TF, &attr); 
/*
.....Mark the sub-surfaces of a net surface as never displayable.
.....This prevents the user from picking or seeing the sub-surfaces
.....even if the net surface is invisible.  Fixes PSB#242, rel 8.1.
*/
	     ur_update_displayable(msh2.key, UM_NEVERDISPLAYABLE);

         *keyout = msh2.key;
         ipat = 0;
         while (ipat<msh1.no_mpatch && status == UU_SUCCESS)
            {
            if (ur_retrieve_data_varlist(msh1.key, 1, &mpat, ipat+1, 1)
                 != 0) status = UU_FAILURE;
            else
               {
               if (ur_update_data_varlist(msh2.key, 1, &mpat,
                        ipat+1, 1) != 0)
                  status = UU_FAILURE;
               ipat++;
               }
            }       /* end while */
         }
      }

   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_net_agsrf(keyin, keyout)
**       Store an AG surface into a net surface.
**    PARAMETERS   
**       INPUT  : 
**          keyin             Key of AG sf to put in net sf
**       OUTPUT :  
**          keyout            Key of sf in net sf
**    RETURNS      : UU_SUCCESS on success, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_net_agsrf(keyin, keyout)
   UU_KEY_ID keyin, *keyout;
   {

   struct UM_agsrf_rec sf1,sf2;
   int status;
   AG_SURFACEP sf;
   AG_SURFACEP ag_srf_copy();
   struct UC_attributedatabag attr;

   uu_denter(UU_MTRC,(us,"ncl_net_agsrf(keyin=%x, keyout=%x)",
      keyin, keyout));

   status = UU_SUCCESS;

   sf1.key = keyin;
   if (ur_retrieve_data_relnum(sf1.key, &sf1.rel_num) != 0)
      status = UU_FAILURE;
   else if (sf1.rel_num != UM_AGSRF_REL)
      status = UU_FAILURE;
   else if (ur_retrieve_data_fixed(&sf1) != 0)
      status = UU_FAILURE;
   else
      {
/*      status = um_agsrf_copy (&sf1, &sf2, sizeof(struct UM_agsrf_rec)); */
      uc_setup_data(UM_AGSRF_REL, &sf2, sizeof(sf2));
      sf = NULL;
      sf = ag_srf_copy(sf1.srfaddr, sf);

      if (sf != NULL)
         {
         status = UU_SUCCESS;
         sf2.srfaddr = (int) sf;

         uc_retrieve_attr(sf1.key, &attr);
         uc_create_mtuple_data(&sf2, UM_DEFAULT_TF, &attr); 
/*
.....Mark the sub-surfaces of a net surface as never displayable.
.....This prevents the user from picking or seeing the sub-surfaces
.....even if the net surface is invisible.  Fixes PSB#242, rel 8.1.
*/
	     ur_update_displayable(sf2.key, UM_NEVERDISPLAYABLE);
         }
      *keyout = sf2.key;
      }

   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_net_evalsf(keyin, keyout)
**       Store a evaluted surface into a net surface.
**    PARAMETERS   
**       INPUT  : 
**          keyin             Key of evaluted sf to put in net sf
**       OUTPUT :  
**          keyout            Key of sf in net sf
**    RETURNS      : UU_SUCCESS on success, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_net_evalsf(keyin, keyout)
  UU_KEY_ID keyin, *keyout;
  {

   struct NCL_evalsf_rec e2;
   int status;
   
   *keyout = 0;
   status = ncl_copy_evalent (keyin,keyout);
   if (status == UU_SUCCESS) 
     {
      e2.key = *keyout;
      if (ur_retrieve_data_fixed(&e2) == UU_SUCCESS)
        {
         strncpy (e2.label,"@UN    ",7); 
         status = ur_update_data_fixed(&e2);
        }
     }
   uu_dexit;
   return (status);
  }

/*********************************************************************
**    E_FUNCTION     : int ncl_net_trimsf(keyin, keyout)
**       Store a trimmed surface into a net surface.
**    PARAMETERS
**       INPUT  :
**          keyin             Key of trimmed sf to put in net sf
**       OUTPUT :
**          keyout            Key of sf in net sf
**    RETURNS      : UU_SUCCESS on success, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_net_trimsf(keyin, keyout)
  UU_KEY_ID keyin, *keyout;
  {

   int status;
   struct NCL_trimsf_rec e1, e2;
   
   *keyout = 0;
   e1.key = keyin;
   status = ncl_retrieve_data_fixed (&e1);
   e2.subscr = 0;
   strcpy (e2.label,"");
   if (status == UU_SUCCESS) 
     status = ncl_copy_trimsf (&e1, &e2);
   if (status == UU_SUCCESS) 
   {
     *keyout = e2.key;
     strncpy (e2.label,"@UN    ",7);
     status = ur_update_data_fixed(&e2);
     ur_update_displayable(e2.key, UM_NEVERDISPLAYABLE);
   }

   return (status);
  }

  /*********************************************************************
**    E_FUNCTION     : int ncl_net_netsf(keyin, keyout)
**       Store an net surface into a net surface.
**    PARAMETERS   
**       INPUT  : 
**          keyin             Key of net sf to put in net sf
**       OUTPUT :  
**          keyout            Key of sf in net sf
**          none
**    RETURNS      : UU_SUCCESS on success, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_net_netsf(keyin, keyout)
UU_KEY_ID keyin, *keyout;
{
   struct NCL_netsf_rec nsf;
   struct NCL_netsf_rec nnsf;
   int status;

   uu_denter(UU_MTRC,(us,"ncl_net_netsf(keyin=%x, keyout=%x)",
      keyin, keyout));
   
   *keyout = 0;
   nsf.key = keyin;
   status = ncl_retrieve_data_fixed (&nsf);
   nnsf.subscr = 0;
   strcpy (nnsf.label,"");
   if (status == UU_SUCCESS) 
     status = ncl_copy_netsf (&nsf, &nnsf);
   if (status == UU_SUCCESS) 
   {
     *keyout = nnsf.key;
     strncpy (nnsf.label,"@UN    ",7);
     status = ur_update_data_fixed(&nnsf);
     ur_update_displayable(nnsf.key, UM_NEVERDISPLAYABLE);
   }

   uu_dexit;
   return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get_netsf(surf, buf)
**       Return a net surface header in NCLI format.
**    PARAMETERS   
**       INPUT  : 
**          surf              Net surface record
**       OUTPUT :  
**          buf               NCL surface record
**    RETURNS      : UU_SUCCESS on success, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get_netsf(surf, buf)
   struct NCL_netsf_rec *surf;
   struct NCLI_netsf_rec *buf;

   {
   int status;
   int nsfs;
   UU_KEY_ID key[40];
   int i,j;

   uu_denter(UU_MTRC,(us,"ncl_get_netsf(surf=%x, buf=%x)",
      surf, buf));

   status = UU_SUCCESS;

   buf->surftype = NCLI_NETSF;
   nsfs = surf->no_netkey;
   buf->numsfs = nsfs;

   for (i=0; i<nsfs; i++)
      for (j=0; j<4; j++)
         buf->bndsfs[i][j] = surf->bndsfs[i][j];

   if (ur_retrieve_data_varlist(surf->key, 1, key, 1, nsfs) != 0)
      status = UU_FAILURE;
   else
      for (i=0; i<nsfs; i++)
         buf->sfkeys[i] = key[i];

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ssfcre(buf, nclkey)
**       Set up a net surface in unibase.
**    PARAMETERS   
**       INPUT  : 
**          buf               NCL surface record
**       OUTPUT :  
**          nclkey            Unibase key of surface
**    RETURNS      : UU_SUCCESS on success, UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ssfcre(buf, nclkey)
UM_real8 buf[];
UM_int4 *nclkey;

   {
   int status;
   int i,j;
   struct NCL_netsf_rec surf;
   struct NCLI_netsf_rec *srfhead;

   uu_denter(UU_MTRC,(us,"ssfcre(buf=%x, nclkey=%x)",buf, nclkey));

   ur_setup_data(NCL_NETSF_REL, &surf, sizeof(surf));

   /* initialize key to zero. this tells ncl_create_entity() to create
      instead of update entity */
   surf.key = 0;
   surf.surf_type = NCLI_NETSF;
   surf.no_netkey = 0;

   srfhead = (struct NCLI_netsf_rec *) buf;

   for (i=0;i<srfhead->numsfs;i++)
      for (j=0;j<4;j++)
         surf.bndsfs[i][j]=srfhead->bndsfs[i][j];
/*
.....added shaded and lucency 
.....Yurong 1/29/99
*/
/*	surf.shaded = UU_TRUE;
	surf.lucency = 100;*/

   status = ncl_create_entity(&surf, 9); /* pass geo type 9. kathy */
   *nclkey = surf.key;

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ssfupd(netkey,nsf,sfkey)
**       Update the key list of a net surface.
**    PARAMETERS
**       INPUT  :
**          netkey               UNIBASE key of net surface
**          isf                  number of key to update
**          sfkey                surface key to put in list
**       OUTPUT :
**          none
**    RETURNS      :
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ssfupd(netkey, isf, sfkey)
   UM_int4 *netkey, *sfkey;
   UM_int2 *isf;

   {
   int status;
   int sfnum;
   UU_KEY_ID key;
   struct NCL_netsf_rec surf;

   status = UU_SUCCESS;

   surf.key = *netkey;
   sfnum = *isf;
   key = *sfkey;
   if (ur_retrieve_data_fixed(&surf) != 0)
      status = UU_FAILURE;
   else if (ur_update_data_varlist(surf.key, 1, &key, sfnum, 1) != 0)
      status = UU_FAILURE;

   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int dspnwf (nclkey dbuf)
**       Display a wf surface as part of a net surface.
**    PARAMETERS   
**       INPUT  : 
**          nclkey               key of surface to display
**       OUTPUT :  
**          dbuf                 Last point displayed (used for labelling)
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
dspnwf (nclkey, dbuf)
   UM_int4 *nclkey;
   UM_real8 dbuf[3];

   {
   int status, isize, rel_num;
	UU_LOGICAL savpik;
   UU_KEY_ID key;
   struct UM_agsrf_rec agsrf;
   struct UM_rbsplsrf_rec rbsrf;
   struct NCL_evalsf_rec evsrf;
   struct UC_entitydatabag *eptr;
   struct UC_attributedatabag attr;
   UM_transf tfmat;
   UM_param u, v;
#if UU_COMP == UU_CIM
   static struct UM_evsrfout evout;
#endif
#if UU_COMP != UU_CIM
   struct UM_evsrfout evout;
#endif

   uu_denter(UU_MTRC, (us,"dspnwf(nclkey=%x)", *nclkey));

   key = *nclkey;
	savpik = UM_set_constituent_pickids;
	UM_set_constituent_pickids = UU_FALSE;
   status = UU_FAILURE;
   if (ur_retrieve_data_relnum(key, &rel_num) == 0)
      {
      if (rel_num == UM_AGSRF_REL)
         {
         eptr = (struct UC_entitydatabag *)&agsrf;
         isize = sizeof(struct UM_agsrf_rec);
         }
      else if (rel_num == NCL_EVALSF_REL)
         {
         eptr = (struct UC_entitydatabag *)&evsrf;
         isize = sizeof(struct NCL_evalsf_rec);
         }
      else if (rel_num == UM_RBSPLSRF_REL)
         {
         eptr = (struct UC_entitydatabag *)&rbsrf;
         isize = sizeof(struct UM_rbsplsrf_rec);
         }
      eptr->key = key;
      status = uc_retrieve_data (eptr, isize);
      if (status == UU_SUCCESS) status = uc_retrieve_attr (key, &attr);
      if (status == UU_SUCCESS) status = uc_retrieve_transf (key, tfmat);
		if (status == UU_SUCCESS && savpik) gspickid(key);
      if (status == UU_SUCCESS) status = uc_draw (eptr, tfmat, &attr);
      if (status == UU_SUCCESS)
         {
         u = 1.0;
         v = 1.0;
         status = uc_evsrf (UM_POINT, u, v, eptr, tfmat, &evout);
         dbuf[0] = evout.sp[0];
         dbuf[1] = evout.sp[1];
         dbuf[2] = evout.sp[2];
         }
      }
	UM_set_constituent_pickids = savpik;
   uu_dexit;
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : int isnswf (nclkey, iret)
**       Determine if a net surface contains a wf surface.
**    PARAMETERS   
**       INPUT  : 
**          nclkey               key of net surface
**       OUTPUT :  
**          iret                 1 iff net surf contains wf surface
**                               0 otherwise
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
isnswf (nclkey, iret)
   UM_int4 *nclkey;
   UM_int2 *iret;

   {
   int status, i, nsfs;
   UU_KEY_ID key[40];
   struct NCL_netsf_rec netsf;

   uu_denter(UU_MTRC, (us,"isnswf(nclkey=%x)", *nclkey));

   netsf.key = *nclkey;
   *iret = 0;
   status = UU_FAILURE;
   if (ur_retrieve_data_fixed(&netsf) == 0)
      {
      nsfs = netsf.no_netkey;
      if (ur_retrieve_data_varlist (netsf.key, 1, key, 1, nsfs) == 0)
         {
         status = UU_SUCCESS;
         for (i=0; i<nsfs && *iret==0; i++) isitwf (&key[i], iret);
         }
      }

   uu_dexit;
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_transform_netsf (e, tfmat, store)
**       Transform a net surface.
**    PARAMETERS   
**       INPUT  : 
**          e1         - pointer to net surface.
**          tfmat      - Transformation matrix.
**          store      - =1 store in unibase, else don't store
**       OUTPUT :  
**          e1         - Pointer to transformed net surface.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_transform_netsf (e1, tfmat, store)
  struct NCL_netsf_rec *e1; 
  UM_transf *tfmat;
  UU_LOGICAL store;
   {
    int status, isize, i;
    struct NCL_fixed_databag e2;
    status = UU_SUCCESS;
/*
...Do the transformation for all component surfaces
*/
    isize = e1->no_netkey;
    for (i=0; i<isize && status ==  UU_SUCCESS; i++)
      {
       e2.key = e1->netkey[i];
       status = ncl_retrieve_data_fixed(&e2);
       if (status == UU_SUCCESS) status = uc_transform (&e2,tfmat,store); 
      }
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_class_copy_netsf (eptr1, eptr2, bagsize)
**       Copy a net surface.
**    PARAMETERS   
**       INPUT  : 
**          eptr1      - Net surface to copy.
**       OUTPUT :  
**          eptr2      - Copied net surface.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_class_copy_netsf (e1, e2, bagsize)
struct NCL_netsf_rec *e1, *e2; 
int bagsize;
{
   int status;
   UM_int2 nclstatus;

   status = ncl_label_wf(NCL_SURF_REL, e2->label, &e2->subscr, 0, &nclstatus);
   if (status == UU_SUCCESS) status = ncl_copy_netsf(e1, e2);
   if (status == UU_SUCCESS) status = ncl_store_wf1(e2->key);

   return(status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_copy_netsf (eptr1, eptr2)
**       Copy a net surface.
**    PARAMETERS   
**       INPUT  : 
**          eptr1      - Net surface to copy.
**       OUTPUT :  
**          eptr2      - Copied net surface.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_copy_netsf (eptr1, eptr2)
struct NCL_netsf_rec *eptr1, *eptr2; 
{
   int status, nsf, i, isz;
   int isub;
   char labl[NCL_MAX_LABEL];
   struct NCL_fixed_databag e1,e2;
   UM_int2 lfl_77;
   UU_KEY_ID *keyp;
   struct UM_transf_rec tran;
   struct UC_attributedatabag attr1;

   status = UU_SUCCESS;
   strncpy(labl,eptr2->label,NCL_MAX_LABEL);
   isub = eptr2->subscr;
   *eptr2 = *eptr1;
   eptr2->key = 0;
   strncpy(eptr2->label,labl,NCL_MAX_LABEL);
   eptr2->subscr     = isub;
   eptr2->no_netkey  = 0;
   eptr2->netkey     = UU_NULL;
   eptr2->no_sskey   = 0;
   eptr2->sskey      = UU_NULL;
   eptr2->no_displst = 0;
   eptr2->displst    = UU_NULL;
   eptr2->no_tesslst = 0;
   eptr2->tesslst    = UU_NULL;

   isz = sizeof (e2);
   lfl_77 = 1;
   stunlb (&lfl_77);
/*
... Copy all component surfaces
*/
   nsf = eptr1->no_netkey;
   keyp = UU_NULL;
   keyp = (UU_KEY_ID *)uu_malloc (nsf*sizeof(UU_KEY_ID)); 
   if (keyp == UU_NULL) status = UU_FAILURE;

   for (i=0; i<nsf && status ==  UU_SUCCESS; i++)
   {
      e1.key = eptr1->netkey[i];
      status = ncl_retrieve_data_fixed(&e1);
      if (status == UU_SUCCESS)
         status = uc_copy (&e1, &e2, isz); 
      if (status == UU_SUCCESS)
      {
         keyp[i] = e2.key;
         ur_update_displayable(e2.key, UM_NEVERDISPLAYABLE);
      }
   }

   stunlb (&lfl_77);
   if (status == UU_SUCCESS)
   {
     tran.key = eptr1->key;
     tran.rel_num = UM_TRANSFORM_REL;
     status = ur_retrieve_transf(&tran);
   }
   if (status == UU_SUCCESS)
     status = ncl_create_entity(eptr2, 9);
   if (status == UU_SUCCESS)
   {
     tran.key = eptr2->key;
     status = ur_update_transf(&tran);
   }
   if (status == UU_SUCCESS)
     status = ur_update_data_varlist (eptr2->key, 1, keyp, 1, nsf);
   if (status == UU_SUCCESS)
   {
     uc_retrieve_attr(eptr1->key, &attr1);
     attr1.key = eptr2->key;
     ur_update_attr(&attr1);
     status = ncl_retrieve_data_fixed(eptr2);
   }

   if (keyp) uu_free (keyp);

   return(status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_proj_netsf_to_drawing (eptr, tfmat, attrptr,
**                             drwmat, vrefpt, vpnorm, keylist, render_solids)
**      Projects a net surface on a drawing 
**    PARAMETERS   
**       INPUT  : 
**          eptr                 pointer to entity data
**          tfmat                matrix to position entity in MCS
**          attrptr              pointer to attribute bundle
**          drwmat               transformation to convert MCS to DCS
**          vrefpt               reference point of viewing plane
**          vpnorm               normal of viewing plane
**          render_solids        dummy
**       OUTPUT :
**          keylist              keys of projected entities are pushed
**                               onto this list of keys
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_proj_netsf_to_drawing (eptr, tfmat, attrptr, drwmat, vrefpt, vpnorm,
											keylist, render_solids)
struct NCL_netsf_rec *eptr;
UM_transf tfmat;
struct UM_attrdata_rec *attrptr;
UM_transf drwmat;
UM_coord vrefpt;
UM_vector vpnorm;
UU_LIST *keylist;
UU_LOGICAL render_solids;
{
	struct NCL_fixed_databag e;
	int status,savelabel,i;
	struct NCL_surface_rec *ss;

	savelabel = attrptr->label_on;

	for (i=0; i<eptr->no_netkey; i++)
	{
		e.key = eptr->netkey[i];
		status = ur_retrieve_data_relnum(e.key, &e.rel_num);
		if (status == UU_SUCCESS) status = wf_geom_type (e.rel_num);
		if (status == UU_SUCCESS) status = ncl_retrieve_data(&e, sizeof(e));
		if (status != UU_SUCCESS) return (status);

		if (i == 0)
		{
			ncl_get_label (eptr,e.label);
/*
.....check if label is on and altered
*/
			if (ncl_get_label_on(attrptr->label_on) && 
				ncl_get_label_alter(attrptr->label_on))
				ncl_retrieve_labloc (eptr, attrptr->label_on, e.labloc);
			else
				ncl_default_labloc (eptr,tfmat,e.labloc);
/*
.....set the label display to on and altered
*/
			ncl_set_label_on(&attrptr->label_on,1);
			ncl_set_label_alter(&attrptr->label_on,1);
			/*attrptr->label_on = 2;*/
		}

/*
.....There is a problem with pre-existing Unibases
.....having standalone corrupted surfaces with
.....the label @UN, so we no longer display these
.....surfaces, so ...
.....We need to locally give the sub-surfaces
.....different labels, so that they are displayed.
.....Bobby  -  12/8/99
*/
		ss = (struct NCL_surface_rec *)&e;
		strcpy(ss->label,"@NSF");

		status = uc_proj_to_drawing (&e, tfmat,attrptr,
						drwmat, vrefpt, vpnorm, keylist,render_solids);

      attrptr->label_on = 0;
	}

	attrptr->label_on = savelabel;

	return (status);
}

