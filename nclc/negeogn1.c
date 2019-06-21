/*********************************************************************
**    NAME         :  negeogn1.c
**       CONTAINS:
**     int ncl_rldgen (bp1, bp2, sfp,ktwist)
**     int ncl_sbsolv (iphase, sa, sb, bp, tfmat)
**     int ncl_errpol (pt1, ve1, pt2, ve2, pt3, edis)
**     int ncl_errcal (vq, vr, vj, vpe, edis)
**     int ncl_rldpat (npat, sp, bp, tfmat, pts, ktwist)
**     int ncl_srfdef (fit, ncrvs, crvp, keyslp, sfp, ktwist)
**     int ncl_tslope(u, slpent, tfmat, evout_slp, pj)
**     int ncl_srfpre (npats, npans, csegp, tsegp, sp, tp, sfp)
**     int ncl_setup_rbsf (rel_num, sfp, isz)
**     int ncl_srffit (ncrvs, csegp, ns, tsegp, cvp, tfp)
**     int ncl_echk (pi, lastk, nextk, nexk)
**     int ncl_patpre (npat, cp1, cp2, cp3, cp4, pts)
**     int ncl_polfin (segp, pts);
**     int ncl_polbld (p1,p2,p3,p4);
**     int ncl_4crvsrf (ibp, sfp)
**     int ncl_eval_4crvs(it,u,v,bp1,tf1,bp2,tf2,bp3,tf3,bp4,tf4,
**                                 twist13,twist24,sfpt,sfvu,sfvv)
**     int bsfdef (nents, keys, slpkys, nclkey, ifit)
**     int sdedef (keys, iflg, nclkey)
**     int ncl_revsrf (pta, vca, sa, ea, crvp, sfp)
**     int ncl_tabcyl (crvp, vecd, sfp)
**     int ncl_rldfin (npat, sp, cpts, sfp)
**     int revdef (keycv, ptbuf, vebuf, sa, ea, nclkey)
**    COPYRIGHT 1991 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       negeogn1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:34
*********************************************************************/

#include "udebug.h"
#include "modef.h"
#include "mdeval.h"
#include "gobas.h"
#include "dasnog.h"
#include "dselmask.h"
#include "uhep.h"
#include "ulist.h"
#include "mfort.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mattr.h"
#include "mcrv.h"
#include "nccs.h"
#include "msrf.h"
#include "nclfc.h"
#include "ncl.h"
#include "uminmax.h"

char *uu_malloc();

#ifdef DEBUG
FILE *fdbg = 0;
#endif

/*********************************************************************
**    E_FUNCTION     : ncl_rldgen (bp1, bp2, sfp,ktwist)
**       Create a B-spline ruled surface thru 2 boundaries.
**    PARAMETERS   
**       INPUT  : 
**          bp1       - pointer to first boundary.
**          bp2       - pointer to second boundary.
**          ktwist    - flag; if ktwist=1 the second boundary should
**                      be reversed.
**       OUTPUT :  
**          sfp       - pointer to ruled surface created.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_rldgen (bp1, bp2, sfp, ktwist)
   struct NCL_fixed_databag *bp1, *bp2;
   struct UM_rbsplsrf_rec *sfp;
   int ktwist;
   {
   int ns, npts, npat, status;
   UU_REAL sa, sb, sb1, *sp, *cpts, *pts;
   UM_transf tfmat1, tfmat2;
   UU_LIST slist;
   UM_int2 typ = 0;
   UM_real8 param[16];

   uu_denter(UU_MTRC,(us,"ncl_rldgen ()"));

   status = UU_SUCCESS;
   cpts = 0;

   if (bp1->key > 0) status = uc_retrieve_transf (bp1->key, tfmat1);
   else um_identtf (tfmat1);
   if (status == UU_SUCCESS)
     if (bp2->key > 0) status = uc_retrieve_transf (bp2->key, tfmat2);
     else um_identtf (tfmat2);
   uu_list_init (&slist, sizeof(UU_REAL), 20, 20);
   sa = sb = 0.0;
   uu_list_push (&slist, &sb);
   ns = 1;
   while (sb < .99999 && status == UU_SUCCESS)
     {
     status = ncl_sbsolv (1, sa, &sb, bp1, tfmat1);
     if (status == UU_SUCCESS)
       {
       sb1 = sb;
       status = ncl_sbsolv (2, sa, &sb, bp2, tfmat2);
       if (sb>sb1) sb = sb1;
       if (ns<4 && sb > sa+.25) sb = sa+.25;
       uu_list_push (&slist, &sb);
       sa = sb;
       ns++;
       }
     }

   if (status == UU_SUCCESS)
     {
     npat = ns-1;
     sp = (UU_REAL *) UU_LIST_ARRAY (&slist);
     sp[npat] = 1.0;
     npts = npat*3+1;
     ns = npts+4;
     cpts = (UU_REAL *) uu_malloc (6*npts*sizeof(UU_REAL));
     if (cpts == 0) status = UU_FAILURE;
     pts = cpts;
     }

   if (status == UU_SUCCESS)
     status = ncl_rldpat (npat, sp, bp1, tfmat1, pts, 0);

   if (status == UU_SUCCESS)
     {
     pts += 3*npts;
     status = ncl_rldpat (npat, sp, bp2, tfmat2, pts, ktwist);
     }

   if (status == UU_SUCCESS)
   {
     ncl_rld_primdat (bp1,bp2,&typ,param);
     status = ncl_rldfin (npat,sp,cpts,typ,param, sfp);
   }

   uu_list_free (&slist);
   if ((unsigned long)cpts > 0) uu_free(cpts);

   uu_dexitstatus("ncl_rldgen ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_sbsolv (iphase, sa, sb, bp, tfmat)
**       Calculate the ending S value for a surface patch.
**    PARAMETERS   
**       INPUT  : 
**          icrv      - 1 = first boundary, 2 = second or greater boundary.
**          sa        - starting s value.
**          bp        - pointer to surface boundary curve.
**          tfmat     - transformation
**          sb        - last found ending S value.
**       OUTPUT :  
**          sb        - ending S value.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_sbsolv (icrv, sa, sb, bp, tfmat)
   int icrv;
   UU_REAL sa, *sb;
   struct NCL_fixed_databag *bp;
   UM_transf tfmat;
   {
   int i, iphase, status;
   UU_LOGICAL done;
   UU_REAL xs, dxs, emax, etol, dx, ddx;
   UM_coord pt1, pt2, pt3;
   UM_vector ve1, ve2;
   UM_param u;
   struct UM_evcrvout evout;
   UM_real8 tol8;

   uu_denter(UU_MTRC,(us,"ncl_sbsolv ()"));

   gettol (&tol8);
   etol = tol8;
   status = uc_init_evcrvout (bp, &evout);
   if (status == UU_SUCCESS)
     {
     u = sa;
     status = uc_evcrv (UM_FRSTDERIV, u, bp, tfmat, &evout);
     um_vctovc (evout.cp, pt1);
     um_vctovc (evout.dcdu, ve1);
     um_unitvc (ve1, ve1);
     }
/*  A. Fuchs 6/5/12
.....Modfied xs value so that it would take surface length into
	consideration.  Small changes in large surfaces were missed.
*/
	ddx = um_mag(evout.dcdu);
	dx = (!ncl_setver(99) && ddx > 1.)? MIN2(0.05,1./ddx): 0.05;
   if (icrv == 1 && status == UU_SUCCESS)
     {
     xs = dx;
     if (sa+xs > 1.0) xs = 1.0-sa;
/*
.....1.12*xs gives the same ratio as in previous versions of NCL
*/
     dxs = 1.12*xs;
     done = UU_FALSE;
     for (i=0; i<100 && status == UU_SUCCESS && !done; i++)
       {
       u = sa + xs;
       status = uc_evcrv (UM_FRSTDERIV, u, bp, tfmat, &evout);
       um_vctovc (evout.cp, pt2);
       um_vctovc (evout.dcdu, ve2);
       um_unitvc (ve2, ve2);
       u = sa + xs/2.0;
       status = uc_evcrv (UM_POINT, u, bp, tfmat, &evout);
       um_vctovc (evout.cp, pt3);
       status = ncl_errpol (pt1,ve1,pt2,ve2,pt3,&emax);
       done = emax > etol || sa+xs > .9999;
       if (!done)
         {
/*
.....dxs *= 1.357 gives the same ratio as in previous versions of NCL
*/
			dxs *= 1.357;
         xs = xs+dxs;
         if (sa+xs > 1.0) xs = 1.0-sa;
         }
       }
     if (emax>etol) xs *= 0.9;
     *sb = sa+xs;
     if (!done) status = UU_FAILURE;
     }

   iphase = 2;
   xs = *sb-sa;
   for (i=0; i<100 && status == UU_SUCCESS && iphase<5; i++)
     {
     iphase = 2;
     u = sa + xs;
     status = uc_evcrv (UM_FRSTDERIV, u, bp, tfmat, &evout);
     um_vctovc (evout.cp, pt2);
     um_vctovc (evout.dcdu, ve2);
     um_unitvc (ve2, ve2);
     u = sa + xs/2.0;
     do
       {
       status = uc_evcrv (UM_POINT, u, bp, tfmat, &evout);
       um_vctovc (evout.cp, pt3);
       status = ncl_errpol (pt1,ve1,pt2,ve2,pt3,&emax);
       if (emax < etol)
         {
         iphase++;
         if (iphase == 3) u = sa+xs*.25;
         if (iphase == 4) u = sa+xs*.75;
         }
       } while (iphase<5 && emax<etol);
     if (iphase < 5) xs *= .9;
     }

   *sb = sa+xs;

   uu_dexitstatus("ncl_sbsolv ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_errpol (pt1, ve1, pt2, ve2, pt3, edis)
**       Calculate distance between bezier segment & point.
**    PARAMETERS   
**       INPUT  : 
**          pt1       - start point of bezier segment.
**          ve1       - start vector of bezier segment.
**          pt2       - end point of bezier segment.
**          pt1       - end vector of bezier segment.
**          pt3       - external point.
**       OUTPUT :  
**          edis      - distance between segment & point.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_errpol (pt1, ve1, pt2, ve2, pt3, edis)
   UM_coord pt1, pt2, pt3;
   UM_vector ve1, ve2;
   UU_REAL *edis;
   {
   int status;
   UU_REAL chd, adis, perr;
   UM_vector vj, vq, vr, vpe;

   uu_denter(UU_MTRC,(us,"ncl_errpol ()"));

   um_vcmnvc (pt2,pt1,vj);
   chd = um_mag(vj);
   if (chd == 0.0) chd = 1.0;
   adis = .3345*chd;

   um_vctmsc (ve1,adis,vq);
   um_vctmsc (ve2,adis,vr);
   um_vcmnvc (vj,vr,vr);
   um_vcmnvc (pt3, pt1, vpe);

   status = ncl_errcal (vq, vr, vj, vpe, &perr);

   *edis = fabs(perr);

   uu_dexitstatus("ncl_errpol ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_errcal (vq, vr, vj, vpe, edis)
**       Calculate distance between bezier segment & point.
**    PARAMETERS   
**       INPUT  : 
**          vq        - vector from 1st to 2nd of bezier segment.
**          vq        - vector from 1st to 3rd of bezier segment.
**          vq        - vector from 1st to 4th of bezier segment.
**          vpe       - vector from 1st point to external point.
**       OUTPUT :  
**          edis      - distance between segment & point.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_errcal (vq, vr, vj, vpe, edis)
   UM_vector vq, vr, vj, vpe;
   UU_REAL *edis;
   {
   int i, status;
   UU_LOGICAL done;
   UU_REAL u, du, ctan, um, ca,cb,cc, x1,y1,z1, dx,dy,dz, uerr,oerr;

   uu_denter(UU_MTRC,(us,"ncl_errcal ()"));

   status = UU_SUCCESS;
   u = .5;
   du =.01;
   ctan = .5;
   done = UU_FALSE;
	oerr = 0.;
   for (i=0; i<100 && !done; i++)
     {
     um = 1.0-u;
     ca = um*um;
     cb = 2.0*u*um;
     cc = u*u;
     x1 = cb*vq[0]+cc*vr[0];
     y1 = cb*vq[1]+cc*vr[1];
     z1 = cb*vq[2]+cc*vr[2];
     dx = ca*vq[0]+cb*vr[0]+cc*vj[0] - x1;
     dy = ca*vq[1]+cb*vr[1]+cc*vj[1] - y1;
     dz = ca*vq[2]+cb*vr[2]+cc*vj[2] - z1;
     uerr = (dx*(vpe[0]-x1)+dy*(vpe[1]-y1)+dz*(vpe[2]-z1))/(dx*dx+dy*dy+dz*dz) - u;
     done  = fabs(uerr) < 1.0e-5;
     if (!done)
       {
       if (i>0)
         {
         if (fabs(oerr-uerr)>1.0e-5) ctan = du/(oerr-uerr);
         du = uerr*ctan;
         if (u+du>1.0) du = 1.0-u;
         if (u+du<0.0) du = -u;
         }
       u = u+du;
       oerr = uerr;
       }
     }

   if (done)
     {
     dx = x1+u*dx-vpe[0];
     dy = y1+u*dy-vpe[1];
     dz = z1+u*dz-vpe[2];
     *edis = sqrt (dx*dx+dy*dy+dz*dz);
     }
   else
     {
     *edis = 1.0e6;
     }

   uu_dexitstatus("ncl_errcal ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_rldpat (npat, sp, bp, tfmat, pts, ktwist)
**       Calculate a row of control points along a boundary of a ruled patch.
**    PARAMETERS   
**       INPUT  : 
**          npat      - number of patches
**          sp        - pointer to S values.
**          bp        - pointer to boundary curve.
**          tfmat     - transformation for boundary curve.
**          ktwist    - flag; if ktwist=1 the boundary should
**                      be reversed.
**       OUTPUT :  
**          pts       - control points of ruled patch.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_rldpat (npat, sp, bp, tfmat, pts, ktwist)
   int npat, ktwist;
   UU_REAL *sp;
   struct NCL_fixed_databag *bp;
   UM_transf tfmat;
   UU_REAL *pts;
   {
   int i, status;
   UU_REAL dx,dy,dz, chd,ad;
   UM_coord pt1, pt2;
   UM_vector ve1, ve2;
   UM_param u;
   struct UM_evcrvout evout;

   uu_denter(UU_MTRC,(us,"ncl_rldpat ()"));

   status = uc_init_evcrvout (bp, &evout);
   if (status == UU_SUCCESS)
     {
     if (ktwist == 0) 
        u = sp[0];
     else
        u = 1. - sp[0];
     status = uc_evcrv (UM_FRSTDERIV, u, bp, tfmat, &evout);
     um_vctovc (evout.cp, pt1);
     um_vctovc (evout.dcdu, ve1);
     if (ktwist == 1) 
        um_vctmsc (ve1,-1.0,ve1);
     um_unitvc (ve1, ve1);
     }

   for (i=0; i<npat && status == UU_SUCCESS; i++)
     {
     if (ktwist == 0) 
        u = sp[i+1];
     else
        u =  1. - sp[i+1];
     status = uc_evcrv (UM_FRSTDERIV, u, bp, tfmat, &evout);
     um_vctovc (evout.cp, pt2);
     um_vctovc (evout.dcdu, ve2);
     if (ktwist == 1) 
        um_vctmsc (ve2,-1.0,ve2);
     um_unitvc (ve2, ve2);
     dx = pt2[0]-pt1[0];
     dy = pt2[1]-pt1[1];
     dz = pt2[2]-pt1[2];
     chd = sqrt (dx*dx+dy*dy+dz*dz);
     ad = .3345*chd;
     um_vctovc(pt1,pts);
     pts += 3;
     pts[0] = pt1[0]+ad*ve1[0];
     pts[1] = pt1[1]+ad*ve1[1];
     pts[2] = pt1[2]+ad*ve1[2];
     pts += 3;
     pts[0] = pt2[0]-ad*ve2[0];
     pts[1] = pt2[1]-ad*ve2[1];
     pts[2] = pt2[2]-ad*ve2[2];
     pts += 3;
     um_vctovc (pt2, pt1);
     um_vctovc (ve2, ve1);
     }

   if (status == UU_SUCCESS)
     {
     um_vctovc(pt1,pts);
     pts += 3;
     }

   uu_dexitstatus("ncl_rldpat ()", status);
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_srfdef (fit,ncrvs,crvp,keyslp,sfp,ktwist)
**       Create a B-spline surface through a set of curves.
**    PARAMETERS   
**       INPUT  : 
**          fit        - =1 fit curves else nofit
**          ncrvs      - Number of curves.
**          crvp       - Array of pointers to curves.
**          keyslp     - Array of keys of slope control geometry.
**       OUTPUT :  
**          sfp        - Pointer to created surface.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_srfdef (fit, ncrvs, crvp, keyslp, sfp, ktwist)
int fit, ncrvs, *ktwist;
struct NCL_fixed_databag *crvp;
UM_int4 *keyslp;
struct UM_rbsplsrf_rec *sfp;
   {
   int i, status;
   int ns, npat, icv;
   UU_REAL sa;
   UU_REAL *sp, *tp;
   UM_param u;
   struct NCL_crvgen_rec *csegp, *tsegp, *pi, *pj;
   struct NCL_fixed_databag slpent, *cvp;
   UM_transf slptf, *tfa, *tfp;
   struct UM_evcrvout evout, evout_slp;
   UU_LIST slist;
   UM_int2 typ = 0;
   UM_real8 param[16];
   UU_LOGICAL ltwi,ltwi1;

   uu_denter(UU_MTRC,(us,"ncl_srfdef ()"));

   status = UU_SUCCESS;
   csegp = tsegp = 0;
   tp = 0;

   tfa = (UM_transf *) uu_malloc (ncrvs*sizeof(UM_transf));
   if (tfa == 0) status = UU_FAILURE;

   if (status == UU_SUCCESS)
     {
     status = ncl_presrf (ncrvs,crvp,tfa,&slist);
     ns = UU_LIST_LENGTH(&slist);
     sp = (UU_REAL *) UU_LIST_ARRAY (&slist);
     npat = ns-1;
     sp[npat] = 1.0;
     csegp = (struct NCL_crvgen_rec *) uu_malloc (ns*ncrvs*sizeof(*csegp));
     tsegp = (struct NCL_crvgen_rec *) uu_malloc (ns*ncrvs*sizeof(*tsegp));
     if (csegp == 0 || tsegp == 0)
        status = UU_FAILURE;
     }

   cvp = crvp;
   tfp = tfa;
   pi = csegp;
   for (icv=0; icv<ncrvs && status == UU_SUCCESS; icv++)
     {
     status = uc_init_evcrvout (cvp, &evout);
     slpent.key = 0;
     if (status == UU_SUCCESS && keyslp && keyslp[icv]>0)
       {
       slpent.key = keyslp[icv];
       status = ncl_retrieve_data_fixed (&slpent);
/* 
......vp 13-apr-93 PV added as a slope  
*/
       if (status == UU_SUCCESS && slpent.rel_num != UM_POINT_REL &&
           slpent.rel_num != NCL_VECTOR_REL &&
           slpent.rel_num != NCL_POINTVEC_REL)
         {
         uc_init_evcrvout(&slpent, &evout_slp);
         status = uc_retrieve_transf (slpent.key, slptf);
         }
       }
     pj = tsegp+icv;
     for (i=0; i<ns && status == UU_SUCCESS; i++)
       {

       ltwi = ltwi1 = UU_FALSE;
       if (ktwist != UU_NULL)
       {
         ltwi = (ktwist[icv] == 1);
         ltwi1 = (ktwist[ncrvs+icv] == 1); 
       }

       u = sp[i];
       if (ltwi)
         u = 1.0-sp[i];
       status = uc_evcrv (UM_FRSTDERIV, u, cvp, tfp, &evout);
       pi->x = pj->x = evout.cp[0];
       pi->y = pj->y = evout.cp[1];
       pi->z = pj->z = evout.cp[2];
       if (ltwi)
         um_vctmsc (evout.dcdu,-1.0,evout.dcdu);
       um_unitvc (evout.dcdu, &pi->a);
       pi->inv = 1;
       pj->inv = 0;
       if (status == UU_SUCCESS && slpent.key>(UU_KEY_ID)0)
         {
         if (ltwi1)
           u = 1.0-sp[i];
         status = ncl_tslope(u, &slpent, slptf, &evout_slp, pj);
         }
       pi++;
       pj += ncrvs;
       }
     cvp++;
     tfp++;
     }

   pi = tsegp;
   for (i=0; i<ns && status == UU_SUCCESS; i++)
     {
     status = ncl_slpset (ncrvs, pi, 0);
     pi += ncrvs;
     }

   if (fit && status == UU_SUCCESS)
     status = ncl_srffit (&ncrvs, csegp, ns, tsegp);

   if (status == UU_SUCCESS)
     {
     tp = (UU_REAL *) uu_malloc (ncrvs*sizeof(UU_REAL));
     if (tp == 0) status = UU_FAILURE;
     }

   if (status == UU_SUCCESS)
     {
     sa = ncrvs-1;
     sa = 1.0/sa;
     tp[0] = 0.0;
     for (i=0; i<ncrvs-1; i++) tp[i] = i*sa;
     tp[ncrvs-1] = 1.0;
     status = ncl_srfpre (npat,ncrvs-1,csegp,tsegp,sp,tp,typ,param, sfp);
     }

   uu_list_free (&slist);
   if ((unsigned long)tfa > 0) uu_free(tfa);
   if ((unsigned long)csegp > 0) uu_free(csegp);
   if ((unsigned long)tsegp > 0) uu_free(tsegp);
   if ((unsigned long)tp > 0) uu_free(tp);

   uu_dexitstatus("ncl_srfdef ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_tslope(u, slpent, tfmat, evout_slp, pj)
**       Calculate transverse slope at a surface defining curve.
**    PARAMETERS   
**       INPUT  : 
**          u          - U parameter of curve.
**          slpent     - Pointer to slope control entity.
**          tfmat      - Transformation of slope entity.
**          evout_slp  - Pointer to evaluation record.
**       OUTPUT :  
**          pj         - Pointer to slope control struct with slope filled in.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_tslope(u, slpent, tfmat, evout_slp, pj)
UM_param u;
struct NCL_fixed_databag *slpent;
UM_transf tfmat;
struct UM_evcrvout *evout_slp;
struct NCL_crvgen_rec *pj;
   {
   int status;
   UM_vector spt, svc;
   struct UM_point_rec *pt_ptr;
   struct NCL_vector_rec *vec_ptr;
   struct NCL_nclpv_rec *ptvec_ptr;
   UU_REAL sec;

   uu_denter(UU_MTRC,(us,"ncl_tslope ()"));

   status = UU_SUCCESS;

   if (slpent->rel_num == NCL_VECTOR_REL)
     {
     vec_ptr = (struct NCL_vector_rec *)slpent;
     pj->a = vec_ptr->vec[0];
     pj->b = vec_ptr->vec[1];
     pj->c = vec_ptr->vec[2];
     pj->inv = 1;
     }
/* 
......vp 13-apr-93 PV added as a slope, get vector part only  
*/
   else if (slpent->rel_num == NCL_POINTVEC_REL)
     {
     ptvec_ptr = (struct NCL_nclpv_rec *)slpent;
     pj->a = ptvec_ptr->ve[0];
     pj->b = ptvec_ptr->ve[1];
     pj->c = ptvec_ptr->ve[2];
     pj->inv = 1;
     }
   else
     {
     if (slpent->rel_num == UM_POINT_REL)
       {
       pt_ptr = (struct UM_point_rec *)slpent;
       um_vctovc(pt_ptr->pt, spt);
       }
     else
       {
       status = uc_evcrv(UM_POINT, u, slpent, tfmat, evout_slp);
       um_vctovc(evout_slp->cp, spt);
       }
     if (status == UU_SUCCESS)
       {
       svc[0] = spt[0]-pj->x;
       svc[1] = spt[1]-pj->y;
       svc[2] = spt[2]-pj->z;
       sec = um_mag(svc);
       if (sec < UM_FUZZ)
         status = UU_FAILURE;
       else
         {
         pj->a = svc[0]/sec;
         pj->b = svc[1]/sec;
         pj->c = svc[2]/sec;
         pj->inv = 1;
         }
       }
     }

   uu_dexitstatus("ncl_tslope ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_srfpre (npats, npans, csegp, tsegp, sp, tp, sfp)
**       Create a B-spline surface through a set slope control arrays.
**    PARAMETERS   
**       INPUT  : 
**          npats      - Number of patches per panel.
**          npans      - Number of panels (i.e. rows of patches).
**          csegp      - Slope control array.
**          tsegp      - Transverse slope control array.
**          sp         - Surface s values.
**          tp         - Surface transverse s values.
**       OUTPUT :  
**          sfp        - Pointer to created surface.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_srfpre (npats,npans,csegp,tsegp,sp,tp,typ,param, sfp)
int npats, npans;
struct NCL_crvgen_rec *csegp, *tsegp;
UU_REAL *sp, *tp;
UM_int2 typ;
UM_real8 param[];
struct UM_rbsplsrf_rec *sfp;
   {
   int i, j, ix, status;
   int ns, npts, nrows, ipt, ncrvs, isize;
   UU_KEY_ID key;
   UU_REAL *s;
   UM_coord *cpts, *pts;
   struct NCL_crvgen_rec *hpi, *hpj, *pi, *pj, *pk;
   struct NCL_crvgen_rec *cp1, *cp2;
   UM_int2 nclstatus;

   uu_denter(UU_MTRC,(us,"ncl_srfpre ()"));

#ifdef DEBUG
   if (fdbg == 0) fdbg = fopen("dbg.pp","w");
#endif
   status = UU_SUCCESS;
   key = 0;
   s = 0;
   cpts = 0;
   hpi = 0;
   ns = npats+1;
   ncrvs = npans+1;
   npts = npats*3+1;
   nrows = npans*3+1;
   isize = npts+4;
   if (isize < nrows+4) isize = nrows+4;
   s = (UU_REAL *) uu_malloc ((isize)*sizeof(UU_REAL));
   cpts = (UM_coord *) uu_malloc (12*npts*sizeof(UU_REAL));
   hpi = (struct NCL_crvgen_rec *) uu_malloc (2*ns*sizeof(*csegp));
   if (s == 0 || cpts == 0 || hpi == 0) status = UU_FAILURE;

   if (status == UU_SUCCESS)
     {
     ncl_setup_rbsf (UM_RBSPLSRF_REL, sfp, sizeof(struct UM_rbsplsrf_rec));
     sfp->ku = 4;
     sfp->kv = 4;
     sfp->nu = npts-3;
     sfp->nv = 3*npans-2;
     status = ncl_label_wf(sfp->rel_num,sfp->label,&sfp->subscr,sfp->key,&nclstatus);
     if (status == UU_SUCCESS)
       status = ncl_create_surf_data (sfp);
	 if (status == UU_SUCCESS)
       {
       key = sfp->key;
       status = ncl_store_wf1(key);
       }
     }

   if (status == UU_SUCCESS)
     {
     for (i=npats; i>=0; i--)
       {
       ix = 3*i+1;
       s[ix+2] = s[ix+1] = s[ix] = sp[i];
       }
     s[0] = s[1];
     s[npts+3] = s[npts+2];
#ifdef DEBUG
       for (j=0; j<=npats; j++) fprintf (fdbg,"$$ S(%d)=%f\n",j,sp[j]);
#endif
     status = ur_update_data_varlist (key, 1, s, 1, npts+4);
     }

   if (status == UU_SUCCESS)
     {
     for (i=npans; i>=0; i--)
       {
       ix = 3*i+1;
       s[ix+2] = s[ix+1] = s[ix] = tp[i];
       }
     s[0] = s[1];
     s[nrows+3] = s[nrows+2];
#ifdef DEBUG
       for (j=0; j<=npans; j++) fprintf (fdbg,"$$ T(%d)=%f\n",j,tp[j]);
#endif
     status = ur_update_data_varlist (key, 2, s, 1, nrows+4);
     }

	if (status == UU_SUCCESS)
	{
		UM_real8 ver;
		UM_int2 idx;

		idx = 169;
		getsc (&idx, &ver);
		if (ver > 9.2)
		{
			sfp->no_wt = 0;
			sfp->wt = UU_NULL;
		}
		else
		{
			for (i=0;i<npts;i++) s[i] = 1.0;
			for (i=0, ix=npts*(nrows-1)+1;i<nrows && status==UU_SUCCESS; i++, 
					ix -= npts)
				status = ur_update_data_varlist (key, 4, s, ix, npts);
		}
	}

   ipt = 1;
   hpj = hpi+ns;
   status = ur_update_data_varlist (key, 3, cpts, npts*nrows, 1);
   for (i=0; i<ncrvs-1 && status == UU_SUCCESS; i++)
     {
     pi = hpi;
     pj = hpj;
     pk = tsegp+i;
     for (j=0; j<ns; j++)
       {
       pi->x = pk->x+pk->a;
       pi->y = pk->y+pk->b;
       pi->z = pk->z+pk->c;
       pi->inv = 0;
       pk++;
       pj->x = pk->x-pk->a;
       pj->y = pk->y-pk->b;
       pj->z = pk->z-pk->c;
       pj->inv = 0;
       pi++;
       pj++;
       pk += ncrvs-1;
       }
     status = ncl_slpset (ns, hpi, 0);
     if (status == UU_SUCCESS) status = ncl_slpset (ns, hpj, 0);
     cp1 = csegp+i*ns;
     cp2 = cp1+ns;
     status = ncl_patpre (npats, cp1, hpi, hpj, cp2, cpts);
     if (status == UU_SUCCESS)
       {
#ifdef DEBUG
       for (j=0; j<3*npts; j++) fprintf (fdbg,"PX(%d)=PT/%f,%f,%f\n",
        ipt+j,cpts[j][0],cpts[j][1],cpts[j][2]);
#endif
       status = ur_update_data_varlist (key, 3, cpts, ipt, 3*npts);
       ipt += 3*npts;
       }
     }

   if (status == UU_SUCCESS)
     {
     pts = cpts+3*npts; 
#ifdef DEBUG
       for (j=0; j<npts; j++) fprintf (fdbg,"PX(%d)=PT/%f,%f,%f\n",
        ipt+j,pts[j][0],pts[j][1],pts[j][2]);
#endif
     status = ur_update_data_varlist (key, 3, pts, ipt, npts);
     }

   if (status == UU_SUCCESS)
     status = ncl_retrieve_data_fixed (sfp);

	if (status == UU_SUCCESS)
	{
		if (typ <= 0)
			status = ncl_sf_prim_analyz(&sfp->key,&typ,param);
		else
			status = ncl_put_sf_primdat(&sfp->key,&typ,param);
	}

   if ((unsigned long)s > 0) uu_free(s);
   if ((unsigned long)cpts > 0) uu_free(cpts);
   if ((unsigned long)hpi > 0) uu_free(hpi);
   if ((unsigned long)key > 0 && status != UU_SUCCESS)
     {
     uc_delete (key);
     }
#ifdef DEBUG
       if (fdbg) 
         {
         fclose(fdbg);
         fdbg = 0;
         }
#endif

   uu_dexitstatus("ncl_srfpre ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_setup_rbsf (rel_num, sfp, isz)
**       Initialize a rational B-spline surface structure.
**    PARAMETERS   
**       INPUT  : 
**          rel_num    - Rbsf relation number.
**          sfp        - Pointer to rbsf.
**          isx        - size of rbsf structure.
**       OUTPUT :  
**          sfp        - Pointer to initialized surface structure.
**    RETURNS      : UU_SUCCESS 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_setup_rbsf (rel_num, sfp, isz)
int rel_num, isz;
struct UM_rbsplsrf_rec *sfp;
   {
   int status, i;
   struct NCL_fixed_databag osf;
   struct UM_rbsplsrf_rec *osfp;

   uu_denter(UU_MTRC,(us,"ncl_setup_rbsf ()"));

   status = UU_SUCCESS;

   uc_setup_data (UM_RBSPLSRF_REL, sfp, sizeof(struct UM_rbsplsrf_rec));
   sfp->rel_num = rel_num;
   for (i=0;i<3;i++)
     {
     sfp->labloc[i] = 0.0;
/*
.....initialize default label location
*/
     sfp->ldrloc[i] = 0.0;
     }
   sfp->rldnu = -1;
   sfp->swapuv = 0;
   sfp->rev_normal = 0;
   sfp->closdinu = 0;
   sfp->closdinv = 0;
   sfp->offdist = 0.0;
   sfp->ku = 0;
   sfp->kv = 0;
   sfp->nu = 0;
   sfp->nv = 0;
	sfp->primitive = NCLSF_UNKNOWN;

   uu_dexitstatus("ncl_setup_rbsf ()", status);
   return(status);
   }

/*********************************************************************
**    E_FUNCTION :  int ncl_create_surf_data (eptr)
**			Create a UNIBASE master tuple for a surface, with a default surface attribute.
**    PARAMETERS   
**       INPUT  : 
**          eptr        - Pointer to rbsf.
**       OUTPUT :  none
**
**			RETURNS      : UU_SUCCESS / UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_create_surf_data (eptr)
struct UM_rbsplsrf_rec *eptr;
{
	struct UM_surfattr_rec sfattr;
	int status;

	uu_denter(UU_MTRC,(us,"ncl_create_surf_data(rel_num=%d)", 
			eptr->rel_num));

/* create master tuple in UNIBASE */
	status = ncl_create_data(eptr);
	if (status == UU_SUCCESS)
	{
/* initialize attributes */
		ncl_init_surf_attr(eptr->key,&sfattr,NCLI_SURF);

/* set the view that this entity is in */
		ur_update_view_key(eptr->key, ur_get_dispattr_view_in());
	}

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_srffit (ncrvs, csegp, ns, tsegp)
**       Handle surface fit.
**    PARAMETERS   
**       INPUT  : 
**          ncrvs      - number of curves.
**          csegp      - Array of curve segments.
**          ns         - number of transverse curves.
**          tsegp      - Array of transverse curve segments.
**       OUTPUT :  
**          ncrvs      - Updated
**          csegp      - Updated
**          tsegp      - Updated
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_srffit (ncrvs, csegp, ns, tsegp)
int *ncrvs, ns;
struct NCL_crvgen_rec *csegp, *tsegp;
   {
   int i, j, k, status;
   int ncvs, lastk, nextk, nexk, done;
   int *knv;
   struct NCL_crvgen_rec *pi, *pj;

   uu_denter(UU_MTRC,(us,"ncl_srffit ()"));

   status = UU_SUCCESS;
   knv = 0;
   ncvs = *ncrvs;
   knv = (int *) uu_malloc (ncvs*sizeof(int));
   for (i=0; i<ncvs; i++) knv[i] = 1;
   lastk = 1;

   while (lastk+1 < ncvs && status == UU_SUCCESS)
     {
     nextk = ncvs;
     pi = tsegp+lastk-1;
     for (i=0, done=0; i<ns && !done && status == UU_SUCCESS; i++)
       {
       status = ncl_echk (pi, lastk, nextk, &nexk);
       if (status == UU_SUCCESS)
         {
         if (nexk<nextk) nextk = nexk;
         done = nextk <= lastk+1;
         pi += ncvs;
         }
       }
     if (!done) for (i=lastk; i<nextk-1; i++) knv[i] = 0;
     lastk = nextk;
     }

   if (status == UU_SUCCESS)
     {
     for (i=0; i<ncvs && knv[i]==1; i++) ; 

     j = i;
     while (i<ncvs)
       {
       while (knv[i] == 0) i++;
       while (knv[i] == 1 && i<ncvs)
         {
         for (k=0,pi = csegp+i*ns,pj=csegp+j*ns; k<ns; k++, pi++, pj++)
             ncl_copy_seg(pi,pj);
         for (k=0,pi=tsegp+i,pj=tsegp+j; k<ns; k++, pi += ncvs, pj += ncvs)
             ncl_copy_seg(pi,pj);
         i++;
         j++;
         }
       }
     *ncrvs = j;
     pj = tsegp + *ncrvs;
     for (i=1; i<ns; i++)
       {
       pi = tsegp+i*ncvs;
       for (j=0; j<*ncrvs; j++)
         {
         ncl_copy_seg(pi,pj);
         pi++;
         pj++;
         }
       }
     }

   if ((unsigned long)knv > 0) uu_free (knv);

   uu_dexitstatus("ncl_srffit ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_echk (pi, lastk, nextk, nexk)
**       Determine the next curve required for surface fit.
**    PARAMETERS   
**       INPUT  : 
**          pi         - Array of curve segments.
**          lastk      - starting point.
**          nextk      - current ending point.
**       OUTPUT :  
**          nexk       - Calculated end point for this transverse curve.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_echk (pi, lastk, nextk, nexk)
struct NCL_crvgen_rec *pi;
int lastk, nextk, *nexk;
   {
   int i, j, m, n, npts, status;
   int done, done2;
   UM_real8 tol;
   UU_REAL dx,dy,dz,chd,cal,cbe,adis,bdis,xj,yj,zj,xk,yk,zk;
   UU_REAL u,um,du,ctan,ca,cb,cc,uerr,oerr,utol,x1,y1,z1;
   UU_REAL perr;
   struct NCL_crvgen_rec *pj, *pn;

   uu_denter(UU_MTRC,(us,"ncl_echk ()"));

   utol = 1.0e-5;
   gettol (&tol);
   npts = nextk-lastk+1;
   *nexk = nextk;
   pn = pi+nextk-lastk;
   status = UU_SUCCESS;
	oerr = 0.;
   for (n=npts, done=0; n>2 && !done && status==UU_SUCCESS; n--, pn--)
     {
     dx = pn->x - pi->x;
     dy = pn->y - pi->y;
     dz = pn->z - pi->z;
     chd = sqrt(dx*dx+dy*dy+dz*dz);
     if (chd == 0.0) status = UU_FAILURE;
     if (status == UU_SUCCESS)
       {
       cal = (dx*pi->a+dy*pi->b+dz*pi->c)/chd;
       cbe = (dx*pn->a+dy*pn->b+dz*pn->c)/chd;
       adis = .666667*chd/(1.0+cal);
       bdis = .666667*chd/(1.0+cbe);
       if (adis>bdis) adis = bdis*(2.0-bdis/adis);
       if (bdis>adis) bdis = adis*(2.0-adis/bdis);
       xj = pi->x+adis*pi->a;
       yj = pi->y+adis*pi->b;
       zj = pi->z+adis*pi->c;
       xk = pn->x-bdis*pn->a;
       yk = pn->y-bdis*pn->b;
       zk = pn->z-bdis*pn->c;
       m = n-1;
       pj = pi+1;
       for (i=2, done2=0; i<=m && !done2 && status==UU_SUCCESS; i++, pj++)
         {
         u = .5;
         du = .01;
         ctan = .5;
         for (j=0, uerr = utol*2.0; j<100 && fabs(uerr)>utol; j++)
           {
           um = 1-u;
           ca = um*um;
           cb = 2.0*um*u;
           cc = u*u;
           x1 = ca*pi->x+cb*xj+cc*xk;
           y1 = ca*pi->y+cb*yj+cc*yk;
           z1 = ca*pi->z+cb*zj+cc*zk;
           dx = ca*xj+cb*xk+cc*pn->x - x1;
           dy = ca*yj+cb*yk+cc*pn->y - y1;
           dz = ca*zj+cb*zk+cc*pn->z - z1;
           uerr = (dx*(pj->x-x1)+dy*(pj->y-y1)+dz*(pj->z-z1))
                   /(dx*dx+dy*dy+dz*dz);
           uerr = (uerr-u)/3.0;
           if (fabs(uerr)>utol)
             {
             if (j>0)
               {
               if (fabs(oerr-uerr)>utol) ctan = du/(oerr-uerr);
               du = uerr*ctan;
               if (u+du>1.0) du = 1.0-u;
               if (u+du<0.0) du = -u;
               }
             u += du;
             oerr = uerr;
             }
           }
/* Label 60 */
/*          if (fabs(uerr)>utol) status = UU_FAILURE; */
         if (fabs(uerr)>utol)
           {
           perr = 999.;
           done2 = 1;
           }
         else
           {
           dx = x1+u*dx-pj->x;
           dy = y1+u*dy-pj->y;
           dz = z1+u*dz-pj->z;
           perr = sqrt (dx*dx+dy*dy+dz*dz);
           if (fabs(perr)>tol) done2 = 1;
           }
         }  /* Label 70 */
       if (fabs(perr)>tol) (*nexk)--;
       else done = 1;
       }
     }

   uu_dexitstatus("ncl_echk ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_patpre (npat, cp1, cp2, cp3, cp4, pts);
**       Create a row of bezier patches.
**    PARAMETERS   
**       INPUT  : 
**          npat      - number of patches
**          cp1-4     - pointers to slope control arrays.
**       OUTPUT :  
**          pts       - polygon points.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_patpre (npat, cp1, cp2, cp3, cp4, pts)
   int npat;
   struct NCL_crvgen_rec *cp1, *cp2, *cp3, *cp4;
   UM_coord *pts;
   {
   int status;
   int i, j, k, ix, npts;
   UM_coord hpts[16];
   struct NCL_crvgen_rec *p1, *p2, *p3, *p4;

   uu_denter(UU_MTRC,(us,"ncl_patpre ()"));

   status = UU_SUCCESS;
   npts = npat*3+1;
   p1 = cp1;
   p2 = cp2;
   p3 = cp3;
   p4 = cp4;

   for (i=0; i<npat && status == UU_SUCCESS; i++,p1++,p2++,p3++,p4++)
     {
     status = ncl_polfin (p1, hpts);

     if (status == UU_SUCCESS)
       status = ncl_polfin (p2, &hpts[4][0]);

     if (status == UU_SUCCESS)
       status = ncl_polfin (p3, &hpts[8][0]);

     if (status == UU_SUCCESS)
       status = ncl_polfin (p4, &hpts[12][0]);

     for (j=0; j<4 && status == UU_SUCCESS; j++)
       status = ncl_polbld
          (&hpts[j][0],&hpts[j+4][0],&hpts[j+8][0],&hpts[j+12][0]);

     if (status == UU_SUCCESS)
       for (j=0, ix=0; j<4; j++)
         for (k=0; k<4; k++, ix++)
           um_vctovc (&hpts[ix][0], &pts[3*i+j*npts+k][0]);
      }

   uu_dexitstatus("ncl_patpre ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_polfin (segp, pts);
**       Finish a polygon for one row of a bezier patch.
**    PARAMETERS   
**       INPUT  : 
**          segp      - Two points & vectors.
**       OUTPUT :  
**          pts       - Four polygon points.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_polfin (segp, pts)
   struct NCL_crvgen_rec *segp;
   UM_coord *pts;
   {
   int status;
   UU_REAL dx,dy,dz,ad;
   UU_REAL *ppt;
   struct NCL_crvgen_rec *p2;

   uu_denter(UU_MTRC,(us,"ncl_polfin ()"));

   status = UU_SUCCESS;
   p2 = segp+1;
   dx = p2->x-segp->x;
   dy = p2->y-segp->y;
   dz = p2->z-segp->z;
   ad = .3345*sqrt(dx*dx+dy*dy+dz*dz);
   ppt = (UU_REAL *)pts;
   ppt[0] = segp->x;
   ppt[1] = segp->y;
   ppt[2] = segp->z;
   ppt[3] = segp->x+ad*segp->a;
   ppt[4] = segp->y+ad*segp->b;
   ppt[5] = segp->z+ad*segp->c;
   ppt[6] = p2->x-ad*p2->a;
   ppt[7] = p2->y-ad*p2->b;
   ppt[8] = p2->z-ad*p2->c;
   ppt[9] = p2->x;
   ppt[10] = p2->y;
   ppt[11] = p2->z;

   uu_dexitstatus("ncl_polfin ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_polbld (p1,p2,p3,p4);
**       Complete transverse polygon for a bezier patch.
**    PARAMETERS   
**       INPUT  : 
**          p1-4      - polygon points.
**       OUTPUT :  
**          p1-4      - completed polygon points.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_polbld (p1,p2,p3,p4)
   UM_coord p1,p2,p3,p4;
   {
   int status;
   UU_REAL dx,dy,dz,chd,vx1,vy1,vz1,vx2,vy2,vz2,sec1,sec2;
   UU_REAL cal,cbe,adis,bdis;

   uu_denter(UU_MTRC,(us,"ncl_polbld ()"));

   status = UU_SUCCESS;
   dx = p4[0]-p1[0];
   dy = p4[1]-p1[1];
   dz = p4[2]-p1[2];
   chd = sqrt(dx*dx+dy*dy+dz*dz);
   if (chd == 0.0) status = UU_FAILURE;
   if (status == UU_SUCCESS)
     {
     vx1 = p2[0]-p1[0];
     vy1 = p2[1]-p1[1];
     vz1 = p2[2]-p1[2];
     sec1 = sqrt(vx1*vx1+vy1*vy1+vz1*vz1);

     vx2 = p4[0]-p3[0];
     vy2 = p4[1]-p3[1];
     vz2 = p4[2]-p3[2];
     sec2 = sqrt(vx2*vx2+vy2*vy2+vz2*vz2);

     if (sec1 == 0.0 || sec2 == 0.0) status = UU_FAILURE;
     }

     if (status == UU_SUCCESS)
       {
       cal = (vx1*dx+vy1*dy+vz1*dz)/sec1/chd;
       cbe = (vx2*dx+vy2*dy+vz2*dz)/sec2/chd;
       adis = .666667*chd/(1.0+cal);
       bdis = .666667*chd/(1.0+cbe);
       if (adis>bdis) adis = bdis*(2.0-bdis/adis);
       if (bdis>adis) bdis = adis*(2.0-adis/bdis);
       p2[0] = p1[0]+adis*vx1/sec1;
       p2[1] = p1[1]+adis*vy1/sec1;
       p2[2] = p1[2]+adis*vz1/sec1;
       p3[0] = p4[0]-bdis*vx2/sec2;
       p3[1] = p4[1]-bdis*vy2/sec2;
       p3[2] = p4[2]-bdis*vz2/sec2;
       }

   uu_dexitstatus("ncl_polbld ()", status);
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_4crvsrf (itsk, bp, sfp)
**       Create a B-spline surface thru 4 boundaries.
**    PARAMETERS   
**       INPUT  : 
**          itsk      - = 0, linear interpolation
**                      = 1, cubic interpolation
**          bp        - pointers to boundaries.
**       OUTPUT :  
**          sfp       - pointer to surface created.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_4crvsrf (itsk, bp, sfp)
   int itsk;
   struct NCL_fixed_databag *bp;
   struct UM_rbsplsrf_rec *sfp;
   {
   int i, j, ns1, ns2, npat, npan, status;
   int twist13, twist24;
   UU_REAL sa, sb, sb1, *sp1, *sp2;
   UM_coord sfpt;
   UM_vector sfvu, sfvv;
   UM_param u, v;
   struct NCL_crvgen_rec *csegp, *tsegp, *pi, *pj;
   UM_transf tf1, tf2, tf3, tf4;
   UU_LIST slist1, slist2;
   UM_int2 idx,ival,typ = 0;
   UM_real8 param[16];

   uu_denter(UU_MTRC,(us,"ncl_4crvsrf ()"));

   csegp = tsegp = 0;
   uu_list_init (&slist1, sizeof(UU_REAL), 20, 20);
   uu_list_init (&slist2, sizeof(UU_REAL), 20, 20);
   status = uc_retrieve_transf (bp[0].key, tf1);
   if (status == UU_SUCCESS) status = uc_retrieve_transf (bp[1].key, tf2);
   if (status == UU_SUCCESS) status = uc_retrieve_transf (bp[2].key, tf3);
   if (status == UU_SUCCESS) status = uc_retrieve_transf (bp[3].key, tf4);
   sa = sb = 0.0;
   uu_list_push (&slist1, &sb);
   ns1 = 1;
   while (sb < .99999 && status == UU_SUCCESS)
     {
     status = ncl_sbsolv (1, sa, &sb, bp, tf1);
     if (status == UU_SUCCESS)
       {
       sb1 = sb;
       status = ncl_sbsolv (2, sa, &sb, &bp[2], tf2);
       if (sb>sb1) sb = sb1;
       if (ns1<4 && sb > sa+.25) sb = sa+.25;
       uu_list_push (&slist1, &sb);
       sa = sb;
       ns1++;
       }
     }

   sa = sb = 0.0;
   uu_list_push (&slist2, &sb);
   ns2 = 1;
   while (sb < .99999 && status == UU_SUCCESS)
     {
     status = ncl_sbsolv (1, sa, &sb, &bp[1], tf3);
     if (status == UU_SUCCESS)
       {
       sb1 = sb;
       status = ncl_sbsolv (2, sa, &sb, &bp[3], tf4);
       if (sb>sb1) sb = sb1;
       if (ns2<4 && sb > sa+.25) sb = sa+.25;
       uu_list_push (&slist2, &sb);
       sa = sb;
       ns2++;
       }
     }
   if (status != UU_SUCCESS) goto done;

   idx=346;
   getifl(&idx, &ival);
   twist13=0;
   twist24=0;
   if (ival==0)
     {
     status = chk_crv(&bp[0],&bp[2],&twist13);
     if (status != UU_SUCCESS) goto done;

     status = chk_crv(&bp[1],&bp[3],&twist24);
     }

   if (status == UU_SUCCESS)
     {
     npat = ns1-1;
     sp1 = (UU_REAL *) UU_LIST_ARRAY (&slist1);
     sp1[npat] = 1.0;
     npan = ns2-1;
     sp2 = (UU_REAL *) UU_LIST_ARRAY (&slist2);
     sp2[npan] = 1.0;
     csegp = (struct NCL_crvgen_rec *) uu_malloc (ns1*ns2*sizeof(*csegp));
     tsegp = (struct NCL_crvgen_rec *) uu_malloc (ns1*ns2*sizeof(*tsegp));
     if (csegp == 0 || tsegp == 0)
        status = UU_FAILURE;
     }

   pi = csegp;
   for (i=0; i<ns2 && status == UU_SUCCESS; i++)
     {
     v = sp2[i];
     pj = tsegp+i;
     for (j=0; j<ns1 && status == UU_SUCCESS; j++)
       {
       u = sp1[j];
       status = ncl_eval_4crvs (itsk,u,v,&bp[0],tf1,&bp[1],tf2,
           &bp[2],tf3,&bp[3],tf4,twist13,twist24,sfpt,sfvu,sfvv);
       pi->x = pj->x = sfpt[0];
       pi->y = pj->y = sfpt[1];
       pi->z = pj->z = sfpt[2];
/* --- NOTE - slopes from ncl_eval_4crvs() are valid only at boundaries --- */
       if (i == 0 || i == npan || j == 0 || j == npat)
         {
         pi->a = sfvu[0];
         pi->b = sfvu[1];
         pi->c = sfvu[2];
         pj->a = sfvv[0];
         pj->b = sfvv[1];
         pj->c = sfvv[2];
         pi->inv = 1;
         pj->inv = 1;
         }
       else
         {
         pi->inv = 0;
         pj->inv = 0;
         }
/* -----  END NOTE  ----- */
       pi++;
       pj += ns2;
       }
     }

   pi = csegp;
   for (i=0; i<ns2 && status == UU_SUCCESS; i++)
     {
     status = ncl_slpset (ns1, pi, 0);
     pi += ns1;
     }

   pi = tsegp;
   for (i=0; i<ns1 && status == UU_SUCCESS; i++)
     {
     status = ncl_slpset (ns2, pi, 0);
	  pi += ns2;
     }

   if (status == UU_SUCCESS)
     status = ncl_srfpre (npat,npan,csegp,tsegp,sp1,sp2,typ,param, sfp);

   if (status == UU_SUCCESS)
     status = ncl_retrieve_data_fixed (sfp);

done:;

   uu_list_free (&slist1);
   uu_list_free (&slist2);
   if ((unsigned long)csegp > 0) uu_free(csegp);
   if ((unsigned long)tsegp > 0) uu_free(tsegp);

   uu_dexitstatus("ncl_4crvsrf ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_eval_4crvs (it, u, v, bp1,tf1,bp2,tf2,bp3,tf3,
**                                   bp4,tf4,twist13,twist24,sfpt,sfvu,sfvv);
**       Evaluate a surface formed by 4 boundary curves.
**    PARAMETERS   
**       INPUT  : 
**          it        - = 0, linear interpolation
**                      = 1, cubic interpolation
**          u         - u parameter
**          v         - v parameter
**          bp1-4     - pointers to boundaries.
**          tf1-4     - transformations for boundary curves.
**          twist13   - flag: if twist13=1 the curve bp3 should be reversed.
**          twist24   - flag: if twist24=1 the curve bp4 should be reversed.
**       OUTPUT :  
**          sfpt      - point on surface.
**          sfvu      - Slope in u direction.
**          sfvv      - Slope in v direction.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : Slopes are valid only at the edge curves.
*********************************************************************/
int ncl_eval_4crvs (it,u,v,bp1,tf1,bp2,tf2,bp3,tf3,bp4,tf4,
                                       twist13,twist24,sfpt,sfvu,sfvv)
   int it,twist13,twist24;
   UM_param u, v;
   struct NCL_fixed_databag *bp1, *bp2, *bp3, *bp4;
   UM_transf tf1,tf2,tf3,tf4;
   UM_coord sfpt;
   UM_vector sfvu, sfvv;
   {
   int status;
   UU_REAL um, vm;
   UM_param u1;
   UM_coord p00, p01, p10, p11, pu0, pu1, p0v, p1v;
   UM_vector vu0, vu1, v0v, v1v;
   struct UM_evcrvout ev1;
   UU_REAL u2,u3,v2,v3,fu,fv,fmu,fmv;

   uu_denter(UU_MTRC,(us,"ncl_eval_4crvs ()"));

   status = uc_init_evcrvout (bp1, &ev1);

   u1 = 0.0;
   if (status == UU_SUCCESS) status = uc_evcrv (UM_POINT, u1, bp1, tf1, &ev1);
   um_vctovc (ev1.cp, p00);
   u1 = 1.0;
   if (status == UU_SUCCESS) status = uc_evcrv (UM_POINT, u1, bp1, tf1, &ev1);
   um_vctovc (ev1.cp, p10);
   if (status == UU_SUCCESS) 
      status = uc_evcrv (UM_FRSTDERIV, u, bp1, tf1, &ev1);
   um_vctovc (ev1.cp, pu0);
   um_vctovc (ev1.dcdu, vu0);

   if (status == UU_SUCCESS) status = uc_init_evcrvout (bp3, &ev1);
   u1 = 0.0;
   if (twist13) u1=1.0;
   if (status == UU_SUCCESS) status = uc_evcrv (UM_POINT, u1, bp3, tf3, &ev1);
   um_vctovc (ev1.cp, p01);
   u1 = 1.0;
   if (twist13) u1=0.0;
   if (status == UU_SUCCESS) status = uc_evcrv (UM_POINT, u1, bp3, tf3, &ev1);
   um_vctovc (ev1.cp, p11);
   if (status != UU_SUCCESS) goto done; 

   if (twist13==0) 
     {
     status = uc_evcrv (UM_FRSTDERIV, u, bp3, tf3, &ev1);
     um_vctovc (ev1.cp, pu1);
     um_vctovc (ev1.dcdu, vu1);
     }
   else
     {
     status = uc_evcrv (UM_FRSTDERIV, 1.0-u, bp3, tf3, &ev1);
     um_vctovc (ev1.cp, pu1);
     um_vctmsc (ev1.dcdu,-1.0,vu1);
     }

   if (status == UU_SUCCESS) status = uc_init_evcrvout (bp4, &ev1);
   if (status == UU_SUCCESS) status = uc_evcrv (UM_FRSTDERIV, v, bp4, tf4, &ev1);
   um_vctovc (ev1.cp, p0v);
   um_vctovc (ev1.dcdu, v0v);

   if (status == UU_SUCCESS) status = uc_init_evcrvout (bp2, &ev1);
   if (status != UU_SUCCESS) goto done; 

   if (twist24==0) 
     {
     status = uc_evcrv (UM_FRSTDERIV, v, bp2, tf2, &ev1);
     um_vctovc (ev1.cp, p1v);
     um_vctovc (ev1.dcdu, v1v);
     }
   else
     {
     status = uc_evcrv (UM_FRSTDERIV, 1.0-v, bp2, tf2, &ev1);
     um_vctovc (ev1.cp, p1v);
     um_vctmsc (ev1.dcdu,-1.0,v1v);
     }


   um = 1.0-u;
   vm = 1.0-v;

   if (it == 0)
     {
     sfpt[0] = vm*pu0[0]+v*pu1[0]+um*p0v[0]+u*p1v[0]
               -um*vm*p00[0]-um*v*p01[0]-u*vm*p10[0]-u*v*p11[0];
     sfpt[1] = vm*pu0[1]+v*pu1[1]+um*p0v[1]+u*p1v[1]
               -um*vm*p00[1]-um*v*p01[1]-u*vm*p10[1]-u*v*p11[1];
     sfpt[2] = vm*pu0[2]+v*pu1[2]+um*p0v[2]+u*p1v[2]
               -um*vm*p00[2]-um*v*p01[2]-u*vm*p10[2]-u*v*p11[2];
     }
   else
     {
     u2 = u*u;
     u3 = u*u2;
     v2 = v*v;
     v3 = v*v2;
     fu = 3*u2-2*u3;
     fv = 3*v2-2*v3;
     fmu = 1.0-fu;
     fmv = 1.0-fv;
     sfpt[0] = fmv*pu0[0]+fv*pu1[0]+fmu*p0v[0]+fu*p1v[0]
               -fmu*fmv*p00[0]-fmu*fv*p01[0]-fu*fmv*p10[0]-fu*fv*p11[0];
     sfpt[1] = fmv*pu0[1]+fv*pu1[1]+fmu*p0v[1]+fu*p1v[1]
               -fmu*fmv*p00[1]-fmu*fv*p01[1]-fu*fmv*p10[1]-fu*fv*p11[1];
     sfpt[2] = fmv*pu0[2]+fv*pu1[2]+fmu*p0v[2]+fu*p1v[2]
               -fmu*fmv*p00[2]-fmu*fv*p01[2]-fu*fmv*p10[2]-fu*fv*p11[2];
     }

     sfvu[0] = vm*vu0[0] + v*vu1[0];
     sfvu[1] = vm*vu0[1] + v*vu1[1];
     sfvu[2] = vm*vu0[2] + v*vu1[2];
     sfvv[0] = um*v0v[0] + u*v1v[0];
     sfvv[1] = um*v0v[1] + u*v1v[1];
     sfvv[2] = um*v0v[2] + u*v1v[2];

   um_unitvc (sfvu, sfvu);
   um_unitvc (sfvv, sfvv);

done:;

   uu_dexitstatus("ncl_eval_4crvs ()", status);
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : bsfdef (nents, keys, slpkys, nclkey, ifit)
**       Create B-spline surface.
**    PARAMETERS   
**       INPUT  : 
**          nents   - nents = 1 means create ruled sf, else  
**                    nents = number of boundary curves
**          keys    - keys of entities
**          slpkys  - keys of slope control entities.
**          ifit    - = 0 interpolate
**                    = 1 fit
**       OUTPUT :  
**          nclkey  - key of created B-spline surface.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int bsfdef (nents, keys, slpkys, nclkey, ifit)
UM_int2 *nents;
UM_int4 keys[500], slpkys[500];
UM_int4 *nclkey;
UM_int2 *ifit;
   {
   UM_int2 idx, ival;
   int i, status;
   int ncrvs, itsk;
   int *ktwist;   
   struct NCL_fixed_databag *crvp;
   struct UM_rbsplsrf_rec sfp;
   UU_LOGICAL lv97;
   struct UM_circle_rec *ci0,*ci1;
   UM_vector svec;

   uu_denter(UU_MTRC,(us,"bsfdef ()"));

   status = UU_SUCCESS;
   sfp.key = *nclkey;
   *nclkey = 0;
   ncrvs = *nents;
   itsk = *ifit;

   idx=346;
   getifl(&idx, &ival);

   if (ncrvs == 1) 
     {
     ncrvs = 2;
     itsk=1;
     }
   crvp = 0;
   crvp = (struct NCL_fixed_databag *)uu_malloc(2*ncrvs*sizeof(*crvp));
   if (crvp == 0) status = UU_FAILURE;

   for (i=0; i<ncrvs && status == UU_SUCCESS; i++)
     {
     crvp[i].key = keys[i];
     if (ncl_retrieve_data_fixed (&crvp[i]) != 0)
         status = UU_FAILURE;
     }

   ktwist = UU_NULL;
   ktwist = (int *)uu_malloc(2*ncrvs*sizeof(*ktwist));
   if (ktwist == UU_NULL) status = UU_FAILURE;

   for (i=0; i<ncrvs && itsk==0 && ival==0; i++)
     {
     crvp[i+ncrvs].key = slpkys[i];
     if (slpkys[i])
        ncl_retrieve_data_fixed (&crvp[i+ncrvs]);
     }
/*
.....for ruled surafce, Check if the two circle start vector the same,
.....if not, change to the same start vector fsr61081
*/
   if (*nents == 1 && ncrvs == 2 && 
	   crvp[0].rel_num == 3 && crvp[1].rel_num == 3)
   {
	   lv97 = UU_FALSE;
	   lv97 = ncl_setver(97);
	   if (!lv97)
	   {
		   ci0 = (struct UM_circle_rec *) &crvp[0];	
		   if (fabs(UM_TWOPI - fabs(ci0->dang)) < UM_FUZZ)
		   {
			   um_vctovc (ci0->svec,svec);		
			   ci1 = (struct UM_circle_rec *) &crvp[1];		
		       if (fabs(UM_TWOPI - fabs(ci1->dang)) < UM_FUZZ)
			   {
				   if (!um_vcparall(svec,ci1->svec) )			
					   um_vctovc (svec, ci1->svec);
			   }
		   }
	   }
   }

   if (ival==0)
     status = ncl_check_bnd (itsk, ncrvs,crvp,ktwist);
   else
     for (i=0;i<2*ncrvs;i++) ktwist[i]=0;
   if (status != UU_SUCCESS) goto done;

   if (status == UU_SUCCESS)
     if (*nents == 1)
       {
       status = ncl_rldgen (&crvp[0], &crvp[1], &sfp, ktwist[1]);
       }
     else
       {
       status =  ncl_srfdef (itsk, ncrvs, crvp, slpkys, &sfp, ktwist);
       }

   if (status == UU_SUCCESS)
     {
     *nclkey = sfp.key;
     ncl_def_color (sfp.key);
     }

done:;

   if ((unsigned long)crvp > 0) uu_free (crvp);
   if ((unsigned long)ktwist > 0) uu_free (ktwist);

   uu_dexitstatus("bsfdef ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : sdedef (keys, iflg, nclkey)
**       Create four curve surface.
**    PARAMETERS   
**       INPUT  : 
**          keys    - keys of entities
**          iflg    - = 0 linear interpolation
**                    = 1 cubic  interpolation
**       OUTPUT :  
**          nclkey  - key of created B-spline surface.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int sdedef (keys, iflg, nclkey)
UM_int4 keys[4];
UM_int2 *iflg;
UM_int4 *nclkey;
   {
   int i, status;
   int itsk;
   struct NCL_fixed_databag *crvp;
   struct UM_rbsplsrf_rec sfp;

   uu_denter(UU_MTRC,(us,"sdedef ()"));

   status = UU_SUCCESS;
   sfp.key = *nclkey;
   *nclkey = 0;
   crvp = 0;
   crvp = (struct NCL_fixed_databag *)uu_malloc(4*sizeof(*crvp));
   if (crvp == 0) status = UU_FAILURE;

   for (i=0; i<4 && status == UU_SUCCESS; i++)
     {
     crvp[i].key = keys[i];
     if (ncl_retrieve_data_fixed (&crvp[i]) != 0)
         status = UU_FAILURE;
     }
   if (status == UU_SUCCESS)
     {
     itsk = *iflg;
     status =  ncl_4crvsrf (itsk, crvp, &sfp);
     }

   if (status == UU_SUCCESS)
     {
     *nclkey = sfp.key;
     ncl_def_color (sfp.key);
     }

   if ((unsigned long)crvp > 0) uu_free (crvp);

   uu_dexitstatus("sdedef ()", status);
   return(status);
   }

/*********************************************************************
**    E_FUNCTION     : ncl_revsrf (pta, vca, sta, ea, crvp, sfp)
**       Create a B-spline surface of revolution.
**    PARAMETERS   
**       INPUT  : 
**          pta        - Axis point.
**          vca        - Axis vector.
**          sta        - Start angle.
**          ea         - End angle.
**          crvp       - Pointer to curve to revolve.
**       OUTPUT :  
**          sfp        - Pointer to created surface.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_revsrf (pta, vca, sta, ea, crvp, sfp)
UM_coord pta;
UM_vector vca;
UM_angle sta, ea;
struct NCL_fixed_databag *crvp;
struct UM_rbsplsrf_rec *sfp;
{
	int i, status;
	int ns, npat, ncrvs, icv;
	UU_REAL sa,sb, ang, da, etol, mag;
	UU_REAL *sp, *tp;
	UM_param u;
	UM_vector vrgt, svc, vt, vt2;
	struct NCL_crvgen_rec *csegp, *tsegp, *pi, *pii, *pj, *pjj;
	UM_transf rottf, tfa;
	struct UM_evcrvout evout;
	UU_LIST slist;
	UU_REAL sc, shld;
	UU_LOGICAL ireduc, jreduc = UU_FALSE;
	UM_int2 typ = 0;
	UM_real8 param[16];

	uu_denter(UU_MTRC,(us,"ncl_revsrf ()"));
	
	status = UU_SUCCESS;
	while (ea <= sta) ea += UM_TWOPI;
	ncrvs = (ea-sta)*12.0/UM_PI + 1;
	if (ncrvs < 3) ncrvs = 3;
	if (ncrvs > 24) ncrvs = 24;
	da = (ea-sta)/(ncrvs-1);
	csegp = tsegp = 0;
	tp = 0;
/* 
.....Since increase the number of points to be created
.....down below, need to increase the size of slist  JLS 1/26/99
*/
	uu_list_init (&slist, sizeof(UU_REAL), 100, 100);
	sa = sb = 0.0;
	ns = 1;
	if (crvp->key > 0) status = uc_retrieve_transf (crvp->key, tfa);
	else um_identtf (tfa);
	status = uc_init_evcrvout (crvp, &evout);
	u = .5;
	status = uc_evcrv (UM_FRSTDERIV, u, crvp, tfa, &evout);
	um_vcmnvc (evout.cp, pta, svc);
	um_cross (vca,svc,vrgt);
	if (um_mag(vrgt) < UM_FUZZ)
	{
		u=0.0;
		do
		{
			status = uc_evcrv (UM_FRSTDERIV, u, crvp, tfa, &evout);
			um_vcmnvc (evout.cp, pta, svc);
			um_cross (vca,svc,vrgt);
			u += .1;
			mag = um_mag(vrgt);
		} while (mag < UM_FUZZ && u <= 1.0);
		if (mag < UM_FUZZ) status = UU_FAILURE;
	}
	um_unitvc (vrgt, vrgt);
	um_cross (vrgt,vca,svc);
	
	uu_list_push (&slist, &sb);
	while (sb < .99999 && status == UU_SUCCESS)
	{
		status = ncl_sbsolv (1, sa, &sb, crvp, tfa);
/*
.....The numbers put into slist will be the tu values
.....of the surface.
.....Don't limit this check to see if sb jumped too
.....far, this will allow more values to be returned
.....when neccessary, but will keep the number of values
.....to a minimum. JLS 12/3/99
     if (ns<4 && sb > sa+.25) sb = sa+.25;
*/
		if (sb > sa+.25) sb = sa+.25;
/*
.....Out of tolerace surfaces were being created, unless
.....the .25 increment was lowered all the way down to
.....(.01) which is ridiculous. (That would create 101
.....different tu values for each surface which is way too
.....many) More values are needed when the returned sb value
.....is not that far away from sa.  So add to the list values
.....1/3 and 2/3 of the difference between sa and sb. JLS
.....Also add extra values at the end of the region where the
.....difference between sa and sb is small. IJD 25-APR-2000
*/
		ireduc = (sb-sa)<0.10;
		if(ireduc || jreduc)
		{
			if (ireduc) shld = ((sb -sa)/3.0);
			sc = shld + sa;
			uu_list_push (&slist, &sc);
			ns++;
			sc = 2*shld + sa;
			uu_list_push (&slist, &sc);
			ns++;
			jreduc = ireduc;
		}
		uu_list_push (&slist, &sb);
		sa = sb;
		ns++;
	}

	if (status == UU_SUCCESS)
	{
		npat = ns-1;
		sp = (UU_REAL *) UU_LIST_ARRAY (&slist);
		sp[npat] = 1.0;
		csegp = (struct NCL_crvgen_rec *) uu_malloc (ns*ncrvs*sizeof(*csegp));
		tsegp = (struct NCL_crvgen_rec *) uu_malloc (ns*ncrvs*sizeof(*tsegp));
		if (csegp == 0 || tsegp == 0)
			status = UU_FAILURE;
	}

	if (status == UU_SUCCESS)
	{
		um_rotlntf (pta, vca, sta, rottf);                   
		um_vctmtf(svc, rottf, svc);
		um_unitvc (svc, svc);
		etol = .001;
		um_vctmsc(svc, etol, svc);
		pi = csegp;
		pj = tsegp;
	}

	for (i=0; i<ns && status == UU_SUCCESS; i++)
	{
		u = sp[i];
		status = uc_evcrv (UM_FRSTDERIV, u, crvp, tfa, &evout);
/*
..... aak 20-OCT-97:
..... added calculation of the "vrgt" vector for every curve point.
..... Before the same "vrgt" (defined above) was used for all points.
..... This is incorrect when the rotation axis and the whole curve
..... do not lie on a plane
..... But if revolving curve crosses axis, vrgt goes to zero. Use last vrgt
..... in this case. IJD 26-MAY-1998.
*/
		um_vcmnvc (evout.cp, pta, vt);
		um_cross (vca,vt,vt2);
		if (um_mag(vt2) > UM_FUZZ) um_unitvc(vt2,vrgt);

		um_unitvc (evout.dcdu, evout.dcdu);
		if (i==0 || i==ns-1)
		{
			um_unitvc (vt,vt);
			if (um_mag(vt) < UM_FUZZ || 1.0-fabs(um_dot (vca, vt)) < UM_FUZZ)
			um_vcplvc (evout.cp, svc, evout.cp);
		}
		pii = pi;
		pjj = pj;
		ang = sta;
		for (icv=0; icv<ncrvs; icv++)
		{
			um_rotlntf (pta, vca, ang, rottf);                   
			um_cctmtf (evout.cp, rottf, &pii->x);
			pjj->x = pii->x;
			pjj->y = pii->y;
			pjj->z = pii->z;
			um_vctmtf(evout.dcdu, rottf, &pii->a);
			um_vctmtf(vrgt, rottf, &pjj->a);
			pii->inv = 1;
			pjj->inv = 1;
			ang += da;
			pii += ns;
			pjj++;
		}
 		pi++;
		pj += ncrvs;
	}

	if (status == UU_SUCCESS)
	{
		tp = (UU_REAL *) uu_malloc (ncrvs*sizeof(UU_REAL));
		if (tp == 0) status = UU_FAILURE;
	}

	if (status == UU_SUCCESS)
	{
/*
.....tp will contain the tv values.
*/
		sa = ncrvs-1;
		sa = 1.0/sa;
		tp[0] = 0.0;
		for (i=0; i<ncrvs-1; i++) tp[i] = i*sa;
		tp[ncrvs-1] = 1.0;

		status = ncl_revsf_primdat (pta,vca,crvp,&typ,param);

		status = ncl_srfpre (npat,ncrvs-1,csegp,tsegp,sp,tp,typ,param, sfp);
	}
	
	uu_list_free (&slist);
	if ((unsigned long)csegp > 0) uu_free(csegp);
	if ((unsigned long)tsegp > 0) uu_free(tsegp);
	if ((unsigned long)tp > 0) uu_free(tp);

	uu_dexitstatus("ncl_revsrf ()", status);
	return(status);
}
/*********************************************************************
**    E_FUNCTION     : ncl_tabcyl (crvp, vecd, sfp)
**       Create a B-spline tabcyl surface.
**    PARAMETERS   
**       INPUT  : 
**          crvp       - Pointer to curve for first edge.
**          vecd       - Displacement vector for second edge.
**       OUTPUT :  
**          sfp        - Pointer to created surface.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_tabcyl (crvp, vecd, sfp)
struct NCL_fixed_databag *crvp;
UM_vector vecd;
struct UM_rbsplsrf_rec *sfp;
   {
   int i, status;
   int ns, npts, npat;
   UU_REAL sa,sb; 
   UU_REAL *sp;
   UU_REAL *cpts;
   UM_coord *p1, *p2;
   UM_transf tfa;
   UU_LIST slist;
   UM_int2 typ = 0;
   UM_real8 param[16];

   uu_denter(UU_MTRC,(us,"ncl_tabcyl ()"));

   cpts = 0;
   uu_list_init (&slist, sizeof(UU_REAL), 20, 20);
   sa = sb = 0.0;
   ns = 1;
   status = uc_retrieve_transf (crvp->key, tfa);

   uu_list_push (&slist, &sb);

   while (sb < .99999 && status == UU_SUCCESS)
     {
     status = ncl_sbsolv (1, sa, &sb, crvp, tfa);
     if (ns<4 && sb > sa+.25) sb = sa+.25;
     uu_list_push (&slist, &sb);
     sa = sb;
     ns++;
     }

   if (status == UU_SUCCESS)
     {
     npat = ns-1;
     sp = (UU_REAL *) UU_LIST_ARRAY (&slist);
     sp[npat] = 1.0;
     npts = npat*3+1;
     cpts = (UU_REAL *) uu_malloc (6*npts*sizeof(UU_REAL));
     if ( cpts == 0) status = UU_FAILURE;
     }
/*
.....A new parameter was added and needs to be 
.....passed in here also.  JLS 5/13/99
*/

   if (status == UU_SUCCESS)
     status = ncl_rldpat (npat, sp, crvp, tfa, cpts,0);

   if (status == UU_SUCCESS)
     {
     p1 = (UM_coord *)cpts;
     p2 = p1 + npts;
     for (i=0; i<npts; i++, p1++, p2++) um_vcplvc (p1, vecd, p2);
     ncl_tabcyl_primdat (crvp,vecd,&typ,param);
     status = ncl_rldfin (npat,sp,cpts,typ,param, sfp);
     }

   uu_list_free (&slist);
   if ((unsigned long)cpts > 0) uu_free(cpts);

   uu_dexitstatus("ncl_tabcyl ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_rldfin (npat, sp, cpts, sfp)
**       Finish Ruled surface creation.
**    PARAMETERS   
**       INPUT  : 
**          npat       - Number of patches.
**          sp         - Pointer to knot values.
**          cpts       - Pointer to control points (2 rows).
**       OUTPUT :
**          sfp        - Pointer to created surface.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_rldfin (npat,sp,cpts,typ,param, sfp)
int npat;
UU_REAL *sp;
UU_REAL *cpts;
UM_int2 typ;
UM_real8 param[];
struct UM_rbsplsrf_rec *sfp;
   {
   int i, ix, status;
   int ns, npts;
   UM_int2 nstat;
   UU_REAL *s, min_max [2]; 

   uu_denter(UU_MTRC,(us,"ncl_rldfin ()"));

   s = 0;
   npts = npat*3+1;
   ncl_setup_rbsf (UM_RBSPLSRF_REL, sfp, sizeof(struct UM_rbsplsrf_rec));
   sfp->ku = 4;
   sfp->kv = 2;
   sfp->nu = npts-3;
   sfp->nv = 1;
   status = ncl_label_wf(sfp->rel_num,sfp->label,&sfp->subscr,sfp->key,&nstat);
   if (status == UU_SUCCESS)
     status = ncl_create_surf_data (sfp);
   if (status == UU_SUCCESS)
     status = ncl_store_wf1(sfp->key);

   if (status == UU_SUCCESS)
     {
     ns = npts+4;
     s = (UU_REAL *) uu_malloc (ns*sizeof(UU_REAL));
     if (s == 0) status = UU_FAILURE;
     }
/*
..... Set the knot values for the u direction (cubic curve).
..... It's the first pointer in the UM_rbsplsrf_rec structure,
..... so it when calling ur_update (), use 1 as list number to
..... update.  Be sure to tack on the min and max knot values at
..... end of knot vector.
*/
   if (status == UU_SUCCESS)
     {
     for (i=npat; i>=0; i--)
       {
       ix = 3*i+1;
       s[ix+2] = s[ix+1] = s[ix] = sp[i];
       }
     s[0] = s[1];
     s[3*npat+4] = s[3*npat+3];
     status = ur_update_data_varlist (sfp->key, 1, s, 1, ns);
     min_max[0] = sp[0];  min_max[1] = sp[npat];
     status = ur_update_data_varlist (sfp->key, 1, min_max, (ns+1), 2);
     }
/*
..... Set the knot values for the v direction (line).  It's the
..... second pointer in the data structure, and so you use 2 for
..... the list number in the ur_update call.  Be sure to tack on
..... the min and max knot values at end of knot vector.
*/
   if (status == UU_SUCCESS)
     {
     s[0] = s[1] = 0.0;
     s[2] = s[3] = 1.0;
     status = ur_update_data_varlist (sfp->key, 2, s, 1, 4);
     min_max[0] = 0.0;  min_max[1] = 1.0;
     status = ur_update_data_varlist (sfp->key, 2, min_max, 5, 2);
     }
/*
..... Store the control points.  There are 2 rows (one at v = 0
..... and the other at v = 1, lines join corresponding point in
..... each row -> ruled surface).  It's the third pointer in the
..... data structure, and so you use 3 for the list number in the
..... ur_update call.
*/
   if (status == UU_SUCCESS)
     {
     npts *= 2;
     status = ur_update_data_varlist (sfp->key, 3, cpts, 1, npts);
     }
/*
..... Store the weights.  All the weights are = 1.0, for now.
..... It's the fourth pointer in the data structure, so you use 4
..... for the list number in the ur_update call.
*/
	if (status == UU_SUCCESS)
	{
		UM_real8 ver;
		UM_int2 idx;

		idx = 169;
		getsc (&idx, &ver);
		if (ver > 9.2)
		{
			sfp->no_wt = 0;
			sfp->wt = UU_NULL;
		}
		else
		{
			for (i=0;i<npts;i++) cpts[i] = 1.0;
			status = ur_update_data_varlist (sfp->key, 4, cpts, 1, npts);
		}
	}

   if (status == UU_SUCCESS)
     status = ncl_retrieve_data_fixed (sfp);

	if (status == UU_SUCCESS)
	{
		if (typ <= 0)
			status = ncl_sf_prim_analyz(&sfp->key,&typ,param);
		else
			status = ncl_put_sf_primdat(&sfp->key,&typ,param);
	}

   if ((unsigned long)s > 0) uu_free(s);

   uu_dexitstatus("ncl_rldfin ()", status);
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : int revdef (keycv, ptbuf, vebuf, sa, ea, nclkey)
**       Create surface of revolution.
**    PARAMETERS   
**       INPUT  : 
**          keycv   - key of curve to revolve
**          ptbuf   - Coordinates of point on axis of revolution.
**          vebuf   - Vector defining axis of revolution.
**          sa      - start angle.
**          ea      - end angle.
**       OUTPUT :  
**          nclkey  - key of created B-spline surface or 0 if error.
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int revdef (keycv, ptbuf, vebuf, sa, ea, nclkey)
UM_int4 *keycv;
UM_real8 ptbuf[3], vebuf[3], *sa, *ea;
UM_int4 *nclkey;
   {
   int status;
   UM_angle stang, endang;
   struct NCL_fixed_databag crv;
   struct UM_rbsplsrf_rec sf;

   uu_denter(UU_MTRC,(us,"revdef ()"));

   status = UU_FAILURE;
   sf.key = *nclkey;
   *nclkey = 0;
   stang = *sa;
   endang = *ea;

   crv.key = *keycv;
   if (ncl_retrieve_data_fixed (&crv) == 0)
     status = ncl_revsrf (ptbuf, vebuf, stang, endang, &crv, &sf);

   if (status == UU_SUCCESS)
     {
     *nclkey = sf.key;
     ncl_def_color (sf.key);
     }

   uu_dexitstatus("revdef ()", status);
   return(status);
}
