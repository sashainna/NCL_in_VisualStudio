/*********************************************************************
**    NAME         :  trimsup.c
**       CONTAINS:  Trim surface rutines for pods. 
**    !!!!!!!!!!!!!!!!!!!!!  Note  !!!!!!!!!!!!!!!!!!!!!
**    This file needs deep changes to use new stuff from
**    ncl_d_trims to use in ncl_trim_bndcrv, and replace
**    intrim by um_cshape_inschk.
**    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
**           int tbncrv (key)
**           int ncl_trim_bndcrv (eptr, tfmat)
**           int resmem ()
**           int intrim (u,v,ifl)
**    COPYRIGHT 1993 (c) InterSoft.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       trimsup.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:09:18
*********************************************************************/
#include "udebug.h"
#include "mdrel.h"
#include "mcrv.h"
#include "msrf.h"
#include "ncl.h"
#include "nclfc.h"
#include "mdeval.h"
#include "nccs.h"
#include "ag_constant.h"
#include "ag_curves.h"
#include "ag_global.h"
#include "mattr.h"
#include "mdattr.h"
#include "modef.h"
#include "umath.h"
#include "ulist.h"
/*#include "class.h"   for UC_attributedatabag */

UM_real8 *bptr;
UM_int2 nb, *np;
UU_LIST blist;

/*********************************************************************
**    E_FUNCTION     : int  tbncrv (key)
**       Display a trimmed surface or project it to a drawing.
**    PARAMETERS   
**       INPUT  : 
**          key        - trim surface key.
**       OUTPUT :  
**          none 
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
   tbncrv (nclkey)
   UU_KEY_ID *nclkey;
  {
   UM_transf tfmat;
   struct NCL_trimsf_rec eptr;
   UU_KEY_ID key;
   int status;
   uu_denter(UU_MTRC,(us,"tbncrv(key=%x)", *nclkey));

   key = *nclkey;
   eptr.key = key;
   status = UU_FAILURE;
   if (ur_retrieve_data_fixed(&eptr) == 0)
      if (um_get_transformation(eptr.key, tfmat) == 0)
         status = UU_SUCCESS;
   if (status == UU_SUCCESS)
       status = ncl_trim_bndcrv (&eptr,tfmat);
   uu_dexit;
   return (0);
  }

/*********************************************************************
**    E_FUNCTION     : int  ncl_trim_bndcrv (eptr,tfmat)
**       Create UV boundry points for trimmed SF to support PODs 
**       command.
**    PARAMETERS   
**       INPUT  : 
**          key        - trim surface key.
**       OUTPUT :  
**          none 
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_trim_bndcrv (eptr, tfmat)
   struct NCL_trimsf_rec *eptr;
   UM_transf tfmat;
   {
# define EPS2 1.0e-10
   UU_REAL u, v, du, ust, dup, dv, vst, dvp, umax, umin, vmax, vmin, a1;
   UU_REAL ut, vt, sec;
   UU_REAL si, co1, alf;
   int status, i, j, k, nu, nup, nv, nvp, rel_num;
   int ix, n1, n2;
   struct NCL_fixed_databag bs, cv, c2;
   struct UM_surfattr_rec attr;
   struct UM_evcrvout evout;
   struct UM_evsrfout *evsrf;
   UM_real8 bndy[2], *b0, *b1, u0, v0;
   UM_int2 n;
   struct UM_cid_rec cid;
/*    struct UM_line_rec l1; */

   uu_denter(UU_MTRC,(us,"ncl_trim_bndcrv (key=%x, tfmat=%x)",
      eptr->key, tfmat));

   evsrf = 0;
   np = 0;
   status = UU_FAILURE;
   if (ncl_retrieve_data_fixed (eptr) == 0)
      {
		attr.key = eptr->key;
		uc_retrieve_attr(&attr);
      status = UU_SUCCESS;
      nu = attr.numupaths-2;
      if (nu<0) nu=0;
      nv = attr.numvpaths-2;
      if (nv<0) nv=0;
      nup = attr.ptsperucrv;
      if (nup<2) nup = 2;
      nvp = attr.ptspervcrv;
      if (nvp<2) nvp = 2;
/*      for (i=0; i<eptr->no_ibndykey && status == UU_SUCCESS; i+=2)
/*        {
/*        cv.key = eptr->ibndykey[i];
/*        status = ncl_retrieve_data_fixed (&cv);
/*        } */
      uu_list_init (&blist, 2*sizeof(UM_real8), 100, 100);
      nb = eptr->no_ibndykey/2+1;
      np = (UM_int2 *) uu_malloc(nb*sizeof(UM_int2));
      bs.key = eptr->bs_key;
      status = ncl_retrieve_data_fixed (&bs);
      if (status == UU_SUCCESS)
         {
         cv.key = eptr->uv_key;
         status = ncl_retrieve_data_fixed (&cv);
         }
      for (i=0, n1=0; i<nb && status == UU_SUCCESS; i++)
         {
         status = uc_init_evcrvout (&cv, &evout);
         ust = eptr->u_min;
         vst = eptr->v_min;
         dup = eptr->u_max-ust;
         dvp = eptr->v_max-vst;
         u = 0.;
         b0 = &bndy[0];
         b1 = &bndy[1];
         status = uc_evcrv (UM_POINT, u, &cv, UM_DEFAULT_TF, &evout);
         *b0 = (evout.cp[0]-ust)/dup;
         *b1 = (evout.cp[1]-vst)/dvp;
         uu_list_push (&blist, bndy);
         ut = *b0;
         vt = *b1;

         switch (cv.rel_num)
           {
         case UM_COMPCRV_REL:
           {
           struct UM_compcrv_rec *ccv;
           ccv = (struct UM_compcrv_rec *)&cv;
           for (j=1; j<=ccv->no_cid && status == UU_SUCCESS; j++)
             {
             if (ur_retrieve_data_varlist (cv.key, 1, &cid, j, 1) != 0)
               status = UU_FAILURE;
             else if (ur_retrieve_data_relnum (cid.crvid, &rel_num) != 0)
               status = UU_FAILURE;
/*              else if (rel_num == UM_LINE_REL) */
/*                { */
/*                l1.key = cid.crvid; */
/*                if (ur_retrieve_data_fixed (&l1) != 0) */
/*                  status = UU_FAILURE; */
/*                else */
/*                  *b0 = (l1.ept[0]-ust)/dup; */
/*                  *b1 = (l1.ept[1]-vst)/dvp; */
/*                  uu_list_push (&blist, bndy); */
/*                } */
/*              else if (rel_num == UM_CIRCLE_REL) */
/*              else if (rel_num == UM_RBSPLCRV_REL) */
             else
               {
               c2.key = cid.crvid;
               status = ncl_retrieve_data_fixed (&c2);
               du = nup-1;
               du = 1./du;
               u = du;
               for (k=1; k<nup && status == UU_SUCCESS; k++)
                 {
                 if (u>1.) u = 1.;
                 status = uc_evcrv (UM_POINT, u, &c2, UM_DEFAULT_TF, &evout);
                 *b0 = (evout.cp[0]-ust)/dup;
                 *b1 = (evout.cp[1]-vst)/dvp;
                 ut = *b0-ut;
                 vt = *b1-vt;
                 if (ut*ut+vt*vt > EPS2) uu_list_push (&blist, bndy);
                 ut = *b0;
                 vt = *b1;
                 u = u+du;
                 }
               }
             }
           }
           break;
         case UM_RBSPLCRV_REL:
         default:
           n = nup*4;
           du = n-1;
           du = 1./du;
           u = du;
           for (j=1; j<n && status == UU_SUCCESS; j++)
             {
             if (u>1.) u = 1.;
             status = uc_evcrv (UM_POINT, u, &cv, UM_DEFAULT_TF, &evout);
             *b0 = (evout.cp[0]-ust)/dup;
             *b1 = (evout.cp[1]-vst)/dvp;
             ut = *b0-ut;
             vt = *b1-vt;
             if (ut*ut+vt*vt > EPS2) uu_list_push (&blist, bndy);
             ut = *b0;
             vt = *b1;
             u = u+du;
             }
           }
         n2 = UU_LIST_LENGTH(&blist);
         np[i] = n2-n1;
         n1 = n2;
         if (i<nb-1)
           {
           cv.key = eptr->ibndykey[i*2+1];
           status = ncl_retrieve_data_fixed (&cv);
           }
         }
      }

      bptr = (UM_real8 *) UU_LIST_ARRAY (&blist);
      evsrf = (struct UM_evsrfout *) uu_malloc(sizeof(struct UM_evsrfout));
      uc_init_evsrfout(&bs, evsrf);
      umax = vmax = -1.0e9;
      umin = vmin = 1.0e9;
      for (k=0,ix=0; k<nb; k++)
        {
        n = ix+np[k];
        u0 = bptr[ix*2];
        v0 = bptr[ix*2+1];
        dup = u0 - bptr[n*2-4];
        dvp = v0 - bptr[n*2-3];
        sec = sqrt(dup*dup+dvp*dvp);
        if (sec > 0.0)
          {
          dup = dup/sec;
          dvp = dvp/sec;
          }
        a1 = 0.0;
        for (i=ix; i<n && status == UU_SUCCESS; i++)
          {
          u = u0;
          v = v0;
          if (i<n-1)
            {
            u0 = bptr[i*2+2];
            v0 = bptr[i*2+3];
            du = u0 - u;
            dv = v0 - v;
            sec = sqrt(du*du+dv*dv);
            if (sec > 0.0)
              {
              du = du/sec;
              dv = dv/sec;
              }
/*            a1 = a1 + asin(dup*dv-du*dvp); */
            si  = dup*dv-du*dvp;
            alf = asin(si);
            co1  = dup*du+dvp*dv;
            if (co1 < 0.0)
              {
              if (si < 0.0) alf = -alf-UM_PI;
              else alf = UM_PI-alf;
              }
            a1 = a1 + alf;
            dup = du;
            dvp = dv;
            }
          if (umax < u ) umax = u;
          if (vmax < v ) vmax = v;
          if (vmin > v ) vmin = v;
          if (umin > u ) umin = u;
          if (u<0.0) u = 0.0;
          if (u>1.0) u = 1.0;
          if (v<0.0) v = 0.0;
          if (v>1.0) v = 1.0;
          status = ncl_evsrf_tf(UM_POINT, u, v, &bs, tfmat, evsrf);
          }


        if (a1<0.0 && k==0 || a1>0.0 && k>0)
          {
          for (i=ix*2,j=n*2-2;i<ix+n;i+=2,j-=2)
            {
            u0 = bptr[i];
            v0 = bptr[i+1];
            bptr[i] = bptr[j];
            bptr[i+1] = bptr[j+1];
            bptr[j] = u0;
            bptr[j+1] = v0;
            }
          }
        ix += np[k];
        }

   if ((unsigned long)evsrf>0) uu_free(evsrf); 
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int  resmem ()
**       Release memory allocated for trimed surface processing.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none 
**    RETURNS      : 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
   resmem ()
  {
   uu_denter(UU_MTRC,(us,"resmem"));

   if ((unsigned long)np>0) uu_free(np);
   uu_list_free (&blist);  
   uu_dexit;
   return (0);
  }

/*********************************************************************
**    E_FUNCTION     : int  intrim (u,v,ifl)
**       Check if u,v point is inside the trimmed SF boundries.
**    PARAMETERS   
**       INPUT  : 
**          key        - trim surface key.
**       OUTPUT :  
**          none 
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
   intrim (u,v,ifl)
   UU_REAL *u, *v;
   int *ifl;
  {
   UM_int2 insf;
   uu_denter(UU_MTRC,(us,"intrim(u=%xi,v=%x, ifl=%xi)", *u, *v, *ifl));

   ptinsf (u,v,&nb,np,bptr,&insf);
   if (insf >= 0) 
       *ifl = 0;
   else
       *ifl = 1;

   uu_dexit;
   return (0);
  }

