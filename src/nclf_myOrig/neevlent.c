/*********************************************************************
**    NAME         :  neevlent.c
**       CONTAINS:  Routines for evaluated entities.
**           int ncl_disp_evalcv (eptr, tfmat, attptr)
**           int ncl_disp_evalsf (eptr, tfmat, attptr)
**           int ncl_eval_evalcv (evflag, u, eptr, tfmat, evcrv)
**           int ncl_eval_evalsf (evflag, u, eptr, tfmat, evcrv)
**           int ncl_trans_evalent (nclkey, tfmat)
**           int ncl_copy_evalent (key1, key2)
**           int clnent (nclkey, rmx, entkey)
**           int ofevsf (keyin, mod, dist, irs, rmx, keyout)
**           int isitev (nclkey, iflag)
**           int ncl_evcrv_to_agrbsc (evptr, agptr)
**    COPYRIGHT 1990 (c) Mills Data Systems Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       neevlent.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:32
*********************************************************************/

#include "udebug.h"
#include "mdeval.h"
#include "gobas.h"
#include "mfort.h"
#include "mdrel.h"
#include "mattrddl.h"
#include "mcrv.h"
#include "nccs.h"
#include "nclfc.h"
#include "ag_incl.h"
#include "ag_global.h"

/*********************************************************************
**    E_FUNCTION     : int ncl_disp_evalcv (eptr, tfmat, attptr)
**       Display an evaluated curve.
**    PARAMETERS   
**       INPUT  : 
**          eptr       - ptr to curve
**          tfmat      - transformation
**          attptr     - ptr to attribute bundle
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_disp_evalcv (eptr, tfmat, attrptr)
   struct NCL_evalcv_rec *eptr;
   UM_transf tfmat;
   struct UC_attributedatabag *attrptr;

   {
   
   UU_REAL u, du;
   UM_int2 idx, ival;
   int status, i, n;
   Gwpoint3 gpt[90];
   struct UM_evcrvout evout;

   uu_denter(UU_MTRC,(us,"ncl_disp_evalcv(key=%x, tfmat=%x, attrptr=%x)",
      eptr->key, tfmat, attrptr));

   status = UU_FAILURE;
   if (ur_retrieve_data(eptr, sizeof(struct UM_entitydatabag)) == 0)
      {
      status = UU_SUCCESS;
      idx = 136;           /* use ifl(136) for # of points to display */
      getifl (&idx, &ival);
      n = ival;
      if (n > 90) n = 90;
      du = n-1;
      du = 1./du;
      u = 0.;

      for (i=0; i<n; i++)
         {
         if (u>1.) u = 1.;
         ncl_eval_evalcv (UM_POINT, u, eptr, tfmat, &evout);
         gpt[i].x = evout.cp[0];
         gpt[i].y = evout.cp[1];
         gpt[i].z = evout.cp[2];
         u = u+du;
         }

      gpolyline3 (n, gpt);

      cvlbl(&ival);   /* draw label if necessary  */
      if (ival == 1)
         {
         u = .5;
         ncl_eval_evalcv (UM_POINT, u, eptr, tfmat, &evout);
         drwlab(&evout.cp[0], &evout.cp[1], &evout.cp[2], &eptr->key);
         }
      }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_disp_evalsf (eptr, tfmat, attptr)
**       Display an evaluated surface.
**    PARAMETERS   
**       INPUT  : 
**          eptr       - ptr to surface
**          tfmat      - transformation
**          attptr     - ptr to attribute bundle
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_disp_evalsf (eptr, tfmat, attrptr)
   struct NCL_evalsf_rec *eptr;
   UM_transf tfmat;
   struct UC_attributedatabag *attrptr;

   {
   
   UU_REAL u, v, du, dup, dv, dvp;
   int status, i, j, nu, nup, nv, nvp;
#if UU_COMP == UU_CIM
   static struct UM_evsrfout evout;
#endif
#if UU_COMP != UU_CIM
   struct UM_evsrfout evout;
#endif
   UM_int2 ival;

   uu_denter(UU_MTRC,(us,"ncl_disp_evalcv(key=%x, tfmat=%x, attrptr=%x)",
      eptr->key, tfmat, attrptr));

   status = UU_FAILURE;
   if (ur_retrieve_data(eptr, sizeof(struct UM_entitydatabag)) == 0)
      {
      status = UU_SUCCESS;
      nu = eptr->numupaths;
      nv = eptr->numvpaths;
      nup = eptr->ptsperucrv;
      nvp = eptr->ptspervcrv;
      du = 1./(nu-1);
      dv = 1./(nv-1);
      dup = 1./(nup-1);
      dvp = 1./(nvp-1);

      for (i=0, u=0.; i<nu; i++, u=u+du)
         {
         if (u>1.) u = 1.;
         for (j=0, v=0.; j<nvp; j++, v=v+dvp)
            {
            if (v>1.) v = 1.;
            ncl_eval_evalsf (UM_POINT, u, v, eptr, tfmat, &evout);
            glina3 (&evout.sp[0], &evout.sp[1], &evout.sp[2]);  /* draw to pt */
            }
         gdraw();
         }

      for (i=0, v=0.; i<nv; i++, v=v+dv)
         {
         if (v>1.) v = 1.;
         for (j=0, u=0.; j<nup; j++, u=u+dup)
            {
            if (u>1.) u = 1.;
            ncl_eval_evalsf (UM_POINT, u, v, eptr, tfmat, &evout);
            glina3 (&evout.sp[0], &evout.sp[1], &evout.sp[2]);  /* draw to pt */
            }
         gdraw();
         }

      sflbl(&ival);   /* draw label if necessary  */
      if (ival == 1)
         {
         u = 1.0;
         v = 1.0;
         ncl_eval_evalsf (UM_POINT, u, v, eptr, tfmat, &evout);
         drwlab(&evout.sp[0], &evout.sp[1], &evout.sp[2], &eptr->key);
         }
      }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_eval_evalcv (evflag, u, eptr, tfmat, evcrv)
**       Evaluate an evaluated curve.
**    PARAMETERS   
**       INPUT  : 
**          evflag     - evaluation flag
**          u          - evaluation parameter
**          eptr       - ptr to curve
**          tfmat      - transformation
**       OUTPUT :  
**          evcrv      - curve evaluator record
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_eval_evalcv (evflag, u, eptr, tfmat, evcrv)
   int evflag;
   UM_param u;
   struct NCL_evalcv_rec *eptr;
   UM_transf tfmat;
   struct UM_evcrvout *evcrv;

   {
   
   double pt[2][3];
   int status, crvcode, nd, irc;

   uu_denter(UU_MTRC,(us,"ncl_eval_evalcv(key=%x, tfmat=%x)",
      eptr->key, tfmat));

   status = UU_SUCCESS;

   if (evflag == UM_POINT) nd = 0; else nd = 1;

   if (eptr->curve_type == 1)
      {
      crvevl (eptr->evwd, &u, &nd, &pt, &irc);
      }
   else
      {
      crvcode = eptr->curve_type;
      gencev (eptr->evwd, &crvcode, &u, &nd, &pt, &irc);
      }

   evcrv->cp[0] = pt[0][0];
   evcrv->cp[1] = pt[0][1];
   evcrv->cp[2] = pt[0][2];
   if (tfmat != UM_DEFAULT_TF) um_cctmtf (evcrv->cp, tfmat, evcrv->cp);

   if (nd>0)
      {
      evcrv->dcdu[0] = pt[1][0];
      evcrv->dcdu[1] = pt[1][1];
      evcrv->dcdu[2] = pt[1][2];
      if (tfmat != UM_DEFAULT_TF) um_vctmtf (evcrv->dcdu, tfmat, evcrv->dcdu);
      }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_eval_evalsf (evflag, u, v, eptr, tfmat, evsrf)
**       Evaluate an evaluated surface.
**    PARAMETERS   
**       INPUT  : 
**          evflag     - evaluation flag
**          u          - evluation parameter
**          v          - evluation parameter
**          eptr       - ptr to surface
**          tfmat      - transformation
**       OUTPUT :  
**          evsrf      - ptr to surface evaluation record
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_eval_evalsf (evflag, u, v, eptr, tfmat, evsrf)
   int evflag;
   UM_param u, v;
   struct NCL_evalsf_rec *eptr;
   UM_transf tfmat;
   struct UM_evsrfout *evsrf;

   {
   
   double pt[7][3];
   int i, status, mfs, nd, irc, ixdu, ixdv;

   uu_denter(UU_MTRC,(us,"ncl_eval_evalsf(key=%x, tfmat=%x)",
      eptr->key, tfmat));

   status = UU_SUCCESS;

   if (evflag != UM_POINT || eptr->offset) nd = 1; else nd = 0;

   if (eptr->surf_type == 28)
      {
      parevl (eptr->evwd, &u, &v, &nd, &mfs, &pt, &irc);
      ixdu = 1;
      ixdv = 2;
      }
   else if (eptr->surf_type == 29)
      {
      usfevl (eptr->evwd, &u, &v, &nd, &pt, &irc);
      ixdu = 2;
      ixdv = 3;
      }

   evsrf->sp[0] = pt[0][0];
   evsrf->sp[1] = pt[0][1];
   evsrf->sp[2] = pt[0][2];
   if (tfmat != UM_DEFAULT_TF) um_cctmtf (evsrf->sp, tfmat, evsrf->sp);

   if (nd>0)
      {
      for (i=0; i<3; i++)
         {
         evsrf->dsdu[i] = pt[ixdu][i];
         evsrf->dsdv[i] = pt[ixdv][i];
         }
      if (tfmat != UM_DEFAULT_TF)
         {
         um_vctmtf (evsrf->dsdu, tfmat, evsrf->dsdu);
         um_vctmtf (evsrf->dsdv, tfmat, evsrf->dsdv);
         }
      if (eptr->offset)
         {
         um_cross (evsrf->dsdu, evsrf->dsdv, evsrf->snorm);
         um_unitvc (evsrf->snorm, evsrf->snorm);
         evsrf->sp[0] += evsrf->snorm[0] * eptr->offdist;
         evsrf->sp[1] += evsrf->snorm[1] * eptr->offdist;
         evsrf->sp[2] += evsrf->snorm[2] * eptr->offdist;
         }
      }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_trans_evalent (nclkey, tfmat)
**       Transform an evaluated entity.
**    PARAMETERS   
**       INPUT  : 
**          nclkey     - key of entity
**          tfmat      - transformation
**       OUTPUT :  
**          none
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : Updates transformation matrix of entity.
**    WARNINGS     : none
*********************************************************************/
int
ncl_trans_evalent (nclkey, tfmat)
   UU_KEY_ID nclkey;
   UM_transf tfmat;

   {
   
   int status;
   struct UM_transf_rec tran;

   uu_denter(UU_MTRC,(us,"ncl_transf_evalent(key=%x, tfmat=%x)",
      nclkey, tfmat));

   status = UU_FAILURE;

   tran.key = nclkey;
   tran.rel_num = UM_TRANSFORM_REL;
   if (ur_retrieve_transf(&tran) == 0)
      {
      um_tftmtf(tran.tfmat, tfmat, tran.tfmat);
      if (ur_update_transf (&tran) == 0) status = UU_SUCCESS;
      }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_copy_evalent (key1, key2)
**       Copy an evaluated entity.
**    PARAMETERS   
**       INPUT  : 
**          key1       - key of entity to copy.
**       OUTPUT :
**          key2       - key of copied entity.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_copy_evalent(key1, key2)
   UU_KEY_ID key1, *key2;

   {
   int status, ncltyp;
   struct UM_entitydatabag e;
   struct UM_transf_rec tran;

   uu_denter(UU_MTRC,(us,"ncl_copy_evalent (key=%x)",
      key1));

   status = UU_FAILURE;

   e.key = key1;
   if (ur_retrieve_data(&e, sizeof(struct UM_entitydatabag)) == 0)
      {
      tran.key = key1;
      tran.rel_num = UM_TRANSFORM_REL;
      if (ur_retrieve_transf(&tran) == 0)
         {
         ncltyp = 9;
         if (e.rel_num == NCL_EVALCV_REL) ncltyp = 8;
         ncl_create_entity (&e, ncltyp);
         *key2 = e.key;
         tran.key = e.key;
         if (ur_update_transf(&tran) == 0) status = UU_SUCCESS;
         }
      }
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int clnent (nclkey, rmx, entkey)
**       Clone an evaluated entity
**    PARAMETERS   
**       INPUT  : 
**          keyin      - key of entity
**          rmx        - NCL matrix
**       OUTPUT :
**          keyout     - key of copied & transformed entity
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
clnent (keyin, rmx, keyout)
   UM_int4 *keyin, *keyout;
   UM_real8 rmx[12];

   {
   int status, i, j, irmx;
   UM_transf tfmat;
   UU_KEY_ID key1, key2;

   uu_denter(UU_MTRC,(us,"clnent (key=%x, rmx=%x)",
      keyin, rmx));

   status = UU_FAILURE;

   for (i=0, irmx=0; i<3; i++) 
      {
      for (j=0; j<4; j++) tfmat[j][i] = rmx[irmx++];
/*      tfmat[3][i] = rmx[irmx++];    */
      }
   
   key1 = *keyin;
   if (ncl_copy_evalent(key1, &key2)== UU_SUCCESS)
      {
      status = ncl_trans_evalent (key2, tfmat);
      *keyout = key2;
      }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_offset_evalsf (bsf, cdis)
**       Offset an evaluated surface.
**    PARAMETERS   
**       INPUT  : 
**          bsf        - pointer to surface
**          cdis       - offset distance
**       OUTPUT :
**          bsf        - pointer to surface with offset field updated
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_offset_evalsf (bsf, cdis)
   struct NCL_evalsf_rec *bsf;
   UU_REAL cdis;

   {
   int status;

   uu_denter(UU_MTRC,(us,"ncl_offset_evalsf (key=%x)",bsf->key));

   status = UU_SUCCESS;

   bsf->offset = 1;
   bsf->offdist = bsf->offdist + cdis;

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int isitev (nclkey, iflag)
**       Determine if an entity is an evaluated entity.
**    PARAMETERS   
**       INPUT  : 
**          nclkey     - key of entity
**       OUTPUT :
**          iflag      - 1 of entity is evaluated, 0 otherwise.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
isitev (nclkey, iflag)
   UM_int4 *nclkey;
   UM_int2 *iflag;

   {
   int status, rel_num;
   UU_KEY_ID key;

   uu_denter(UU_MTRC,(us,"isitev (key=%x)", nclkey));

   *iflag = 0;
   key = *nclkey;
   if (ur_retrieve_data_relnum (key, &rel_num) != 0)
      status = UU_FAILURE;
   else
      {
      status = UU_SUCCESS;
      if (rel_num == NCL_EVALCV_REL || rel_num == NCL_EVALSF_REL) 
         *iflag = 1;
      }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_evcrv_to_agrbsc (evptr, agptr)
**       Convert an evaluated curve to an equivalent AG non_uniform
**       rational B-Spline.
**    PARAMETERS   
**       INPUT  : 
**          evptr      - pointer to evaluated curve
**       OUTPUT :
**          agptr      - pointer to equivalent AG rational B-spline
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_evcrv_to_agrbsc (evptr, agptr)
   struct NCL_evalcv_rec *evptr;
   struct UM_agcrv_rec *agptr;

   {
   int status, i, j;
   int dim;
   int npts;
   UU_REAL u, du;
   UM_vector svec, evec;
   UU_REAL *svecptr, *evecptr;
   UM_transf tfmat;
   struct UM_evcrvout evout;
   AG_CP_LISTP cplist, ag_bld_cp_l();
   AG_CPOINTP prevcp, cptr, ag_bld_cpt();
   AG_CURVEP crv, ag_crv_cub_fit_pts();

   uu_denter(UU_MTRC,(us,"ncl_evcrv_to_agcrv (key=%x)",
      evptr->key));

   status = UU_SUCCESS;
   dim = 3;
   if (uc_retrieve_transf(evptr->key, tfmat) != 0) status = UU_FAILURE;
   if (ur_retrieve_data(evptr, sizeof(struct UM_entitydatabag)) != 0)
      status = UU_FAILURE;

   cplist = ag_bld_cp_l(NULL, NULL, 0, dim);
   prevcp = NULL;
   npts = 100;
   u = 0.;
   du = npts-1;
   du = 1./du;
   for (i=0; i<npts && status==UU_SUCCESS; i++,u=u+du)
      {
      status = ncl_eval_evalcv(UM_POINT, u, evptr, tfmat, &evout);
      if (i==0 || !um_cceqcc(prevcp->P, evout.cp))
         {
         cptr = ag_bld_cpt(NULL, prevcp, dim);
         if (prevcp == NULL) cplist->cp1 = cptr;
         cplist->cpn = cptr;
         cplist->n++;
         prevcp = cptr;
         for (j=0; j<dim; j++) cptr->P[j] = evout.cp[j];
         }
      }

   if (status==UU_SUCCESS)
      {
      u=0.;
      status = ncl_eval_evalcv(UM_FRSTDERIV, u, evptr, tfmat, &evout);
      um_vctovc(evout.dcdu, svec);
      svecptr = svec;
      }
   if (status==UU_SUCCESS)
      {
      u=1.;
      status = ncl_eval_evalcv(UM_FRSTDERIV, u, evptr, tfmat, &evout);
      um_vctovc(evout.dcdu, evec);
      evecptr = evec;
      }

   if (status==UU_SUCCESS)
      {
      crv = ag_crv_cub_fit_pts(cplist, AG_tol_cfit, svecptr, 1, evecptr, 1);
      if (crv==0) status = UU_FAILURE;
      agptr->rel_num = UM_AGCRV_REL;
      agptr->crvaddr = (int) crv;
      }

   ag_db_cp_l(cplist);

   uu_dexit;
   return (status);
   }
