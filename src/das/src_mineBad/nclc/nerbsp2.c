/*********************************************************************
**    NAME         :  nerbsp2.c
**   NOTE - This file contains routines which are used by both NCL & IGES.
**       CONTAINS:
**           int ncl_eval_rbsp (evflag, u, v, eptr, tfmat, evsrf)
**           int ncl_rbsp_reverse (eptr)
**    COPYRIGHT 1990 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nerbsp2.c , 25.1 
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:45 
*********************************************************************/

#include "udebug.h"
#include "mdeval.h"
#include "gobas.h"
#include "dasnog.h"
#include "mfort.h"
#include "mdrel.h"
#include "mattrddl.h"
#include "mcrv.h"
#include "nccs.h"
#include "msrf.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     : int ncl_eval_rbsp (evflag, u, v, eptr, tfmat, evcrv)
**       Evaluate a rational B-Spline curve.
**    PARAMETERS   
**       INPUT  : 
**          evflag     - evaluation flag
**          u          - evaluation parameter
**          eptr       - ptr to entity.
**          tfmat      - tranformation matrix
**       OUTPUT :  
**          evcrv      - evaluation record
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_eval_rbsp (evflag, u, e, tfmat, evcrv)
   int evflag;
   UM_param u;
   struct UM_rbsplcrv_rec *e;
   UM_transf tfmat;
   struct UM_evcrvout *evcrv;

   {
   int i, status;
   UM_int2 m1, nk1, nd;
   UM_real8 u8, t0, t1, wt, sv[9];
   
   uu_denter(UU_MTRC,(us,"ncl_eval_rbsp(key=%x, tfmat=%x)", eptr->key, tfmat));

   status = UU_SUCCESS;
   u8 = u;
   m1 = e->k-1; nk1 = e->n+2*m1;
   t0 = e->t0; t1 = e->t1;
   if (evflag != UM_POINT) nd = 1; else nd = 0;
   evrbsp (&m1, &nk1, e->t, e->wt, e->pt, &u8, &nd, &t0, &t1, sv, &wt);

   evcrv->cp[0] = sv[0];
   evcrv->cp[1] = sv[1];
   evcrv->cp[2] = sv[2];
   if (tfmat != UM_DEFAULT_TF) um_cctmtf (evcrv->cp, tfmat, evcrv->cp);

   if (nd>0)
      {
      for (i=0; i<3; i++) evcrv->dcdu[i] = sv[i+3];
      if (tfmat != UM_DEFAULT_TF) um_vctmtf (evcrv->dcdu, tfmat, evcrv->dcdu);
      }

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_rbsp_reverse (eptr)
**       Reverse a rational bspline curve.
**    PARAMETERS   
**       INPUT  : 
**          eptr              pointer to rational bspline curve
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff on error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ncl_rbsp_reverse (eptr)
   struct UM_rbsplcrv_rec *eptr;
   {
   int status;
   UM_coord pt;
   UU_REAL t1, ts, *ptr0, *ptr1;
	UU_REAL new_t1, new_t0;
   int i,n;
	UU_REAL high, low;

   uu_denter(UU_MTRC,(us,"ncl_rbsp_reverse(%d)", eptr->key));

   status = UU_SUCCESS;
/*
.....There is a problem here if the curve that is being reversed is a 
.....trimmed curve.  t0 and t1 were staying the same even though the
.....direction of the curve was being changed.  If the curve was trimmed
.....it would move the location of the curve.     JLS 4/23/99
   eptr->t0 = 0.0;;
   eptr->t1 = t1 - *ptr0;
*/
   n = eptr->no_t;
   ptr0 = eptr->t;
   ptr1 = &eptr->t[n-1];
   low = eptr->t[0];
	high = eptr->t[n-1];
	new_t0 = high-(eptr->t1)+ low;
	new_t1 = high-(eptr->t0)+ low;
   t1 = *ptr1;
   eptr->t0 = new_t0;
   eptr->t1 = new_t1;
   for (i=1; i<=n/2; i++,ptr0++,ptr1--)
     {
     ts = t1 - *ptr0;
     *ptr0 = low + t1 - *ptr1;
     *ptr1 = low + ts;
     }
   if (n%2 == 1) *ptr0 = low + t1 - *ptr0;

   n = eptr->no_pt;
   ptr0 = eptr->pt;
   ptr1 = &eptr->pt[(n-1)*3];
   for (i=1; i<=n/2; i++, ptr0 += 3, ptr1 -= 3)
     {
     um_vctovc(ptr0, pt);
     um_vctovc(ptr1, ptr0);
     um_vctovc(pt, ptr1);
     }

   n = eptr->no_wt;
   ptr0 = eptr->wt;
   ptr1 = &eptr->wt[n-1];
   for (i=1; i<=n/2; i++,ptr0++,ptr1--)
     {
     ts = *ptr0;
     *ptr0 = *ptr1;
     *ptr1 = ts;
     }

   uu_dexit;
   return(status);
   }
