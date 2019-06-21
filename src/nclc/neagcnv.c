/*********************************************************************
**    NAME         :  neagcnv.c
**       CONTAINS:
**         int ncl_agcrv_conv (fp, agcrv, bsp)
**         int ncl_agsrf_conv (fp, agcrv, bsp)
**    COPYRIGHT 1993 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       neagcnv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:23
*********************************************************************/

#include "udebug.h"
#include "mdrel.h"
#include "mcrv.h"
#include "msrf.h"
#include "class.h"
#include "xenv1.h"
#include "ulist.h"
#include "ag_incl.h"

/*********************************************************************
**    E_FUNCTION     : int ncl_agcrv_conv (fp, agcrv, bsp)
**       Convert an AG curve to an NCL B-spline. If the AG curve contains
**       multiple splines, convert it to a composite curve containing
**       B-splines.
**    PARAMETERS   
**       INPUT  : 
**          fp         - Pointer to AG file.
**          agcrvp     - ptr to AG curve.
**       OUTPUT :  
**          bsp        - ptr to B_spline.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_agcrv_conv (fp, agcrvp, bsp)
FILE *fp;
struct UM_agcrv_rec *agcrvp;
struct UM_rbsplcrv_rec *bsp;

   {
   int status, i, j, k, npts;
   int idim, irat, ns, nbs, itype, m, n, mult, iform;
   char s1[8];
   UU_REAL t, pt[3], wt;
   UU_REAL *sp, *ptp, *wtp;
   UU_LIST ptlist, slist, wtlist;
   struct UC_attributedatabag attr;
   struct UM_compcrv_rec comp;
   UM_transf tfmat;
   UU_KEY_ID *kptr;  /* keys of B-splines if AG curve contains multiple bs */
   char *uu_malloc();

   uu_denter(UU_MTRC,(us,"ncl_agcrv_conv (agcrvp=%x, bsp=%x)", agcrvp, bsp));

   status = UU_SUCCESS;
   uc_retrieve_transf (agcrvp->key, tfmat);
   uc_retrieve_attr (agcrvp->key, &attr);

   fscanf(fp, "%*s%*s%d%*s%d%*s%d", &idim,&nbs,&iform);

   kptr = UU_NULL;
   if (nbs>1) kptr = (UU_KEY_ID *)uu_malloc(nbs*sizeof(*kptr));

   for (i=0;i<nbs;i++)
     {
     fscanf(fp, "%*s%*s%*s%d%*s%d%*s%d%*s%d%*s%d%*s%d%*s%d",
       &itype,&idim,&m,&n,&irat,&mult,&iform);
     npts = m+n;
     ns = npts+m+1;
     if (i==0)
       {
       uu_list_init (&ptlist, 3*sizeof(UU_REAL), npts, 20);
       uu_list_init (&slist, sizeof(UU_REAL), ns, 20);
       uu_list_init (&wtlist, sizeof(UU_REAL), npts, 20);
       }
/*
...  Get points & weights & push onto list
*/
     wt = 1.0;
     pt[2] = 0.0;
     for (j=0;j<npts;j++)
       {
       fscanf (fp, "%*s%lf%lf", &pt[0], &pt[1]);
       if (idim == 3) fscanf (fp, "%lf", &pt[2]);
       uu_list_push (&ptlist, pt);
       if (irat == 1) fscanf (fp, "%lf", &wt);
       uu_list_push (&wtlist, &wt);
       }
/*
...  Get knot values & push onto list
*/
     for (j=0;j<n+1;j++)
       {
       if (j%3 == 0) fscanf (fp, "%s", s1);
       fscanf (fp, "%lf", &t);
       uu_list_push (&slist, &t);
       if (j == 0)
         {
         for (k=0; k<m; k++) uu_list_push (&slist, &t);
         }
       }

     for (k=0; k<m; k++)
       {
       uu_list_push (&slist, &t);
       }

     sp = (UU_REAL *) UU_LIST_ARRAY (&slist);
     ptp = (UU_REAL *) UU_LIST_ARRAY (&ptlist);
     wtp = (UU_REAL *) UU_LIST_ARRAY (&wtlist);
/*
...  Create B-spline
*/
     if (status == UU_SUCCESS)
       {
       ur_setup_data (UM_RBSPLCRV_REL, bsp, sizeof(*bsp));
       strncpy (bsp->label, agcrvp->label, 64);
       bsp->subscr = agcrvp->subscr;
       bsp->planar = UU_FALSE;
       bsp->open = UU_TRUE;
       bsp->closdinu = agcrvp->closdinu;
       bsp->k = m+1;
       bsp->n = n;
       bsp->t0 = sp[0];
       bsp->t1 = sp[ns-1];
       if (nbs == 1)
         {
/*
...  This is crude. If we delete the old curve immediately before creating the
...  new curve, the new curve will get the same key and a composite curve
...  containing it will work correctly.
*/
         ur_delete_all (agcrvp->key);
         }
       status = uc_create_mtuple_data (bsp, tfmat, &attr);
       }

     if (status == UU_SUCCESS)
       status = ur_update_data_varlist (bsp->key, 1, sp, 1, ns);

     if (status == UU_SUCCESS)
       status = ur_update_data_varlist (bsp->key, 2, ptp, 1, npts);

     if (status == UU_SUCCESS)
       status = ur_update_data_varlist (bsp->key, 3, wtp, 1, npts);

     if (status == UU_SUCCESS && nbs > 1) kptr[i] = bsp->key;
     slist.cur_cnt = 0;
     ptlist.cur_cnt = 0;
     wtlist.cur_cnt = 0;
     }

   if (status == UU_SUCCESS)
     if (nbs > 1)
       {
/*
...  If multiple splines, create a composite curve and display it.
*/
       ur_setup_data (UM_COMPCRV_REL, &comp, sizeof(comp));
       comp.planar = UU_FALSE;
       comp.closdinu = agcrvp->closdinu;
       comp.key = 0;
       status = um_c5_mergecrv (nbs, kptr, &comp);
       if (status == UU_SUCCESS)
         {
         strncpy (comp.label, agcrvp->label, 64);
         comp.subscr = agcrvp->subscr;
/*
...  Delete old curve here for same reasons as above
*/
         ur_delete_all (agcrvp->key);
         status = uc_create_mtuple_data (&comp, tfmat, &attr);
         }
       if (status == UU_SUCCESS) status = ncl_retrieve_data_fixed (&comp);
       if (status == UU_SUCCESS) status = uc_display(&comp);
       }
     else
       {
       status = ncl_retrieve_data_fixed (bsp);
       if (status == UU_SUCCESS) status = uc_display(bsp);
       }

   uu_list_free (&slist);
   uu_list_free (&ptlist);
   uu_list_free (&wtlist);
   if (kptr != UU_NULL) uu_free (kptr);

   uu_dexit;
   return (status);

   }

/*********************************************************************
**    E_FUNCTION     : int ncl_agsrf_conv (fp, agsrf, bsfp)
**       Convert an AG surface to an NCL B-spline surface.
**    PARAMETERS   
**       INPUT  : 
**          fp         - Pointer to AG file.
**          agsrfp     - ptr to AG surface.
**       OUTPUT :  
**          bsfp       - ptr to B_spline surface.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_agsrf_conv (fp, agsrfp, bsfp)
FILE *fp;
struct UM_agsrf_rec *agsrfp;
struct UM_rbsplsrf_rec *bsfp;

   {
   int status, i, j, npts;
   int idim, itype, nsu, nsv;
   int udeg, uspans, urat, umult, uform, upole;
   int vdeg, vspans, vrat, vmult, vform, vpole;
   char s1[8];
   UU_REAL t, pt[3], wt;
   UU_REAL *spu, *spv, *ptp, *wtp;
   UU_LIST ptlist, sulist, svlist, wtlist;
   struct UC_attributedatabag attr;
	struct UM_surfattr_rec *srfattr;
   UM_transf tfmat;

   uu_denter(UU_MTRC,(us,"ncl_agsrf_conv (agsrfp=%x, bsfp=%x)", agsrfp, bsfp));

   status = UU_SUCCESS;
   uc_retrieve_transf (agsrfp->key, tfmat);
   uc_retrieve_attr (agsrfp->key, &attr);

   fscanf(fp, "%*s%*s%d%*s%d", &idim,&itype);
   fscanf(fp, "%*s%d%*s%d%*s%d%*s%d", &udeg,&uspans,&urat,&umult);
   fscanf(fp, "%*s%d%*s%d%*s%d%*s%d", &vdeg,&vspans,&vrat,&vmult);
   fscanf(fp, "%*s%d%*s%d%*s%d%*s%d", &uform,&vform,&upole,&vpole);

   npts = (udeg+uspans)*(vdeg+vspans);
   nsu = uspans+2*udeg;
   nsv = vspans+2*vdeg;
   uu_list_init (&ptlist, 3*sizeof(UU_REAL), npts, 20);
   uu_list_init (&sulist, sizeof(UU_REAL), nsu, 20);
   uu_list_init (&svlist, sizeof(UU_REAL), nsv, 20);
   uu_list_init (&wtlist, sizeof(UU_REAL), npts, 20);
/*
...  Get points & weights & push onto list
*/
   wt = 1.0;
   pt[2] = 0.0;
   for (i=0;i<vdeg+vspans;i++)
     {
     for (j=0;j<udeg+uspans;j++)
       {
       fscanf (fp, "%*s%lf%lf", &pt[0], &pt[1]);
       if (idim == 3) fscanf (fp, "%lf", &pt[2]);
       uu_list_push (&ptlist, pt);
       if (urat == 1 || vrat == 1) fscanf (fp, "%lf", &wt);
       uu_list_push (&wtlist, &wt);
       }
     }
/*
...  Get u knot values & push onto list
*/
   for (i=0;i<=uspans;i++)
     {
     if (i%3 == 0) fscanf (fp, "%s", s1);
     fscanf (fp, "%lf", &t);
     uu_list_push (&sulist, &t);
     if (i == 0)
       {
       for (j=0;j<udeg; j++) uu_list_push (&sulist, &t);
       }
     }

   for (j=0;j<udeg; j++) uu_list_push (&sulist, &t);
/*
...  Get v knot values & push onto list
*/
   for (i=0;i<=vspans;i++)
     {
     if (i%3 == 0) fscanf (fp, "%s", s1);
     fscanf (fp, "%lf", &t);
     uu_list_push (&svlist, &t);
     if (i == 0)
       {
       for (j=0;j<vdeg; j++) uu_list_push (&svlist, &t);
       }
     }

   for (j=0;j<vdeg; j++) uu_list_push (&svlist, &t);

   spu = (UU_REAL *) UU_LIST_ARRAY (&sulist);
   spv = (UU_REAL *) UU_LIST_ARRAY (&svlist);
   ptp = (UU_REAL *) UU_LIST_ARRAY (&ptlist);
   wtp = (UU_REAL *) UU_LIST_ARRAY (&wtlist);
   npts = ptlist.cur_cnt;
/*
... Create B-spline surface.
*/
   if (status == UU_SUCCESS)
     {
     ur_setup_data (UM_RBSPLSRF_REL, bsfp, sizeof(*bsfp));
     strncpy (bsfp->label, agsrfp->label, 64);
     bsfp->subscr = agsrfp->subscr;
		srfattr = (struct UM_surfattr_rec *)&attr;
     srfattr->material = agsrfp->material;
     srfattr->numupaths = agsrfp->numupaths*uspans;
     srfattr->numvpaths = agsrfp->numvpaths*vspans;
     srfattr->ptsperucrv = agsrfp->ptsperucrv;
     srfattr->ptspervcrv = agsrfp->ptspervcrv;
     bsfp->rldnu = agsrfp->rldnu;
     bsfp->swapuv = 0;
     bsfp->rev_normal = agsrfp->rev_normal;
     bsfp->closdinu = agsrfp->closdinu;
     bsfp->closdinv = agsrfp->closdinv;
     bsfp->offdist = 0.0;
     bsfp->ku = udeg+1;
     bsfp->kv = vdeg+1;
     bsfp->nu = uspans;
     bsfp->nv = vspans;
     status = uc_create_mtuple_data (bsfp, tfmat, &attr);
     }

   if (status == UU_SUCCESS)
     status = ur_update_data_varlist (bsfp->key, 1, spu, 1, nsu);

   if (status == UU_SUCCESS)
     status = ur_update_data_varlist (bsfp->key, 2, spv, 1, nsv);

   if (status == UU_SUCCESS)
     status = ur_update_data_varlist (bsfp->key, 3, ptp, 1, npts);

   if (status == UU_SUCCESS)
     status = ur_update_data_varlist (bsfp->key, 4, wtp, 1, npts);

   if (status == UU_SUCCESS)
     {
     status = ncl_retrieve_data_fixed (bsfp);
     uc_display(bsfp);
     }

   uu_list_free (&sulist);
   uu_list_free (&svlist);
   uu_list_free (&ptlist);
   uu_list_free (&wtlist);

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_set_agcnv (lagcnv)
**       Set a flag to cause conversion of ag entities based on environment
**       variable UU_CONVERT_AG.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          lagcnv     - Flag set true iff AG entites are to be converted.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_set_agcnv (lagcnv)
UU_LOGICAL *lagcnv;
  {
  int status = UU_SUCCESS;
  char *str, *ux_getenv();

  str = ux_getenv("UU_CONVERT_AG", UX_NPRTERRS);
  if (str == UU_NULL) str = "YES";
  *lagcnv = strcmp(str, "NO");
  return(status);
  }
