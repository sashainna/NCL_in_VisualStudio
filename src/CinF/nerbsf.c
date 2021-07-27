/*********************************************************************
**    NAME         :  nerbsf.c
**       CONTAINS:
**
**           int ncl_eval_rbsf
**           int ncl_offset_rbsf
**           int ncl_net_rbsf
**           int ncl_copy_rbsf
**           int ncl_class_copy_rbsf
**           int ncl_transform_rbsf
**           int ncl_proj_rbsf_to_drawing
**
**    COPYRIGHT 1991 (c) Numerical Control Computer Sciences Inc.  
**                       All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nerbsf.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:44
*********************************************************************/

#include "udebug.h"
#include "uhep.h"
#include "mdeval.h"
#include "gobas.h"
#include "dselmask.h"
#include "nccs.h"
#include "ncl.h"
#include "mdrel.h"
#include "mdpick.h"
#include "mdattr.h"
#include "mattrddl.h"
#include "mcrv.h"
#include "msrf.h"
#include "msol.h"
#include "mdclass.h"
#include "nclfc.h"

UU_LOGICAL NCL_copy_assoc;

/*********************************************************************
**    E_FUNCTION     : int ncl_eval_rbsf (evflag, u, v, eptr, tfmat, evsrf)
**       Evaluate a rational B-Spline surface.
**    PARAMETERS   
**       INPUT  : 
**          evflag     - evaluation flag.
**          u          - U parameter.
**          v          - V parameter.
**          e          - pointer to surface.
**          tfmat      - transformation matrix.
**       OUTPUT :  
**          evsrf      - evaluation record.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_eval_rbsf (evflag, u, v, e, tfmat, evsrf)
   int evflag;
   UM_param u, v;
   struct UM_rbsplsrf_rec *e;
   UM_transf tfmat;
   struct UM_evsrfout *evsrf;

   {
   int status;
   
   uu_denter(UU_MTRC,(us,"ncl_eval_rbsf(key=%x, tfmat=%x)", eptr->key, tfmat));

   status = um_ev9_rbsplsrf(evflag, u, v, e, tfmat, evsrf);

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_offset_rbsf (bsf, cdis)
**       Offset a Rational Bspline surface.
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
ncl_offset_rbsf (bsf, cdis)
   struct UM_rbsplsrf_rec *bsf;
   UU_REAL cdis;

   {
   int status;

   uu_denter(UU_MTRC,(us,"ncl_offset_rbsf (key=%x)",bsf->key));

   status = UU_SUCCESS;

   bsf->offdist = bsf->offdist + cdis;
	status = ur_update_data_fixed (bsf);
	if (status == UU_SUCCESS && 
		bsf->primitive >= NCLSF_PLANE && fabs(cdis) > 0.0001)
	{
		ncl_offset_primdat (bsf->key,bsf->primitive,bsf->prim_param,cdis);
	}

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_net_rbsf (key1, key2)
**       Copy a rational bspline surface for a net surface.
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
ncl_net_rbsf(key1, key2)
   UU_KEY_ID key1, *key2;

   {
   int status;
   struct UM_rbsplsrf_rec e1, e2;

   uu_denter(UU_MTRC,(us,"ncl_net_rbsf (key=%x)", key1));

   status = UU_FAILURE;
   *key2 = 0;
   e1.key = key1;
   if (ncl_retrieve_data_fixed (&e1) == UU_SUCCESS)
     if (ncl_copy_rbsf (&e1, &e2) == UU_SUCCESS)
     {
       *key2 = e2.key;
       status = UU_SUCCESS;
/*
.....vp 11-aug-97 do not need label, set in ptntsf to force @UN
.....when in ncl_copy_rbsf
       strncpy (e2.label,"@UN    ",7);
       status = ur_update_data_fixed(&e2); */
/*
..... aak 21-apr-1998: Mark the sub-surfaces of a net surface as never displayable.
..... This prevents drawing of sub-surfaces when the net survace is made invisible.
*/
		 ur_update_displayable(e2.key, UM_NEVERDISPLAYABLE);
     }

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_copy_rbsf (e1, e2)
**       Copy a rational bspline surface.
**    PARAMETERS   
**       INPUT  : 
**          e1       - entity to copy.
**       OUTPUT :
**          e2       - copied entity.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_copy_rbsf(e1, e2)
   struct UM_rbsplsrf_rec *e1, *e2;

   {
   int status, i, isize;
	struct NCL_fixed_databag ss1, ss2;
	UU_KEY_ID *keys = UU_NULL;

   uu_denter(UU_MTRC,(us,"ncl_copy_rbsf (key=%x)", e1->key));

   status = UU_FAILURE;
   isize = sizeof(*e1);
	NCL_copy_assoc = UU_FALSE;
   if (ncl_copy_geom (e1, e2, isize) == UU_SUCCESS)
   {
     status = ur_update_data_varlist (e2->key, 1, e1->tu, 1, e1->no_tu);
     status = ur_update_data_varlist (e2->key, 2, e1->tv, 1, e1->no_tv);
     status = ur_update_data_varlist (e2->key, 3, e1->pt, 1, e1->no_pt);
     status = ur_update_data_varlist (e2->key, 4, e1->wt, 1, e1->no_wt);
     status = ncl_retrieve_data_fixed (e2);
   }
	NCL_copy_assoc = UU_TRUE;

   uu_dexit;
   return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_class_copy_rbsf (e1, e2, bagsize)
**       Copy a rational bspline surface.
**    PARAMETERS   
**       INPUT  : 
**          e1       - key of entity to copy.
**       OUTPUT :
**          e2       - key of copied entity.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_class_copy_rbsf(e1, e2, bagsize)
   struct UM_rbsplsrf_rec *e1, *e2;
   int bagsize;
   {
   int status;
   UM_int2 nclstatus;

   uu_denter(UU_MTRC,(us,"ncl_class_copy_rbsf (key=%x)", e1->key));

   status = ncl_label_wf(UM_RBSPLSRF_REL, e2->label, &e2->subscr, 0, &nclstatus);
   if (status == UU_SUCCESS) status = ncl_copy_rbsf(e1, e2);
   if (status == UU_SUCCESS) status = ncl_store_wf1(e2->key);

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_transform_rbsf (e1, tfmat, store)
**       Transform a rational bspline surface.
**    PARAMETERS   
**       INPUT  : 
**          e1         - pointer to rational B-spline surface
**          tfmat      - transformation matrix
**          store      - TRUE if transformed entity is to be stored
**       OUTPUT :
**          e1         - transformed entity.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_transform_rbsf(e1, tfmat, store)
   struct UM_rbsplsrf_rec *e1;
   UM_transf tfmat;
   UU_LOGICAL store;
   {
   int status;
   int i, n;
   UU_KEY_ID key;
   UM_coord *p;
   UU_REAL dist, det;

   uu_denter(UU_MTRC,(us,"ncl_transform_rbsf (key=%x)", e1->key));

   status = UU_SUCCESS;

/*
...fix surface offset: the previous version did it incorrectly, we put a change 
... with the version flag. Eduard 062100.
*/
	dist = e1->offdist;
	if (dist != 0.0)
	{
		UM_real8 ver;
		UM_int2 idx;

		um_determinant (tfmat,&det);
		idx = 169;
		getsc (&idx, &ver);
		if (ver < 9.149)
			e1->offdist = (det > 0.0)? dist: -dist;
		else
		{
			int neg = 0;
			if (det < 0.0)
			{
				neg = 1;
				det = -det;
			}
			if (det != 1.0) det = pow (det,1./3.);
			if (neg) det = -det;
			e1->offdist = dist * det;
		}
		if (store) ur_update_data_fixed (e1);
	}

	if (e1->primitive >= NCLSF_PLANE)
		ncl_transform_primdat (&e1->primitive,e1->prim_param,tfmat);

   n = e1->no_pt;
   p = (UM_coord *)e1->pt;
   for (i=0; i<n; i++,p++) um_cctmtf(p,tfmat,p);

   if (store) 
   {
      status = ur_update_data_varlist (e1->key, 3, e1->pt, 1, e1->no_pt);
      if (status == 0)
        status = ur_update_data_fixed (e1);
/*
.....changed because we may have tesslst that need delete too
.....Yurong 1/28/99
*/
/*   if (status == UU_SUCCESS) ncl_displst_delete(&e1->key);   */
		if (status == UU_SUCCESS)
		{
			if (e1->no_tesslst!=0)
				ncl_lst_delete(TESSELLATION_LIST,&e1->key);
			if (e1->no_displst!=0)
				ncl_lst_delete(DISPLAY_LIST,&e1->key);
			if (e1->no_xyzbylst!=0)
				ncl_lst_delete(WHOLE_BOUNDARY_LIST, &e1->key);
		}
   }

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_proj_rbsf_to_drawing(eptr, tfmat, attrptr, 
**                         drwmat, vrefpt, vpnorm,
**                         keylist, render_solids)
**       Create a list of keys (KEYLIST) of entities to be included
**       in a drawing for the given NCL curve (EPTR, TFMAT, ATTRPTR).
**       The transformation (DRWMAT) will position the viewplane
**       (VREFPT, VPNORM) on the XY plane of the drawing appropriately 
**       scaled to map the MCS to the DCS.
**    PARAMETERS   
**       INPUT  : 
**          eptr                 pointer to entity data (master tuple
**                               data only)
**          tfmat                matrix to position entity in MCS
**          attrptr              pointer to attribute bundle 
**          drwmat               transformation to convert MCS to DCS
**          vrefpt               reference point of viewing plane
**          vpnorm               normal of viewing plane
**          render_solids        UU_TRUE => render solid entities
**       OUTPUT :  
**          keylist              keys of projected entities are pushed
**                               onto this list of keys
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_proj_rbsf_to_drawing(eptr, tfmat, attrptr, drwmat, vrefpt, vpnorm,
                             keylist, render_solids)
struct UM_rbsplsrf_rec *eptr;
UM_transf tfmat;
struct UM_surfattr_rec *attrptr;
UM_transf drwmat;
UM_coord vrefpt;
UM_vector vpnorm;
UU_LIST *keylist;
UU_LOGICAL render_solids;
{
   struct UM_polyline_rec pline;
	UU_LIST cvpts;
   UM_coord proj_point, *pts, drw_point;
   int status, i, j, nu, nv, savelabel,np,nup,nvp;
   UU_REAL u, v, du, dv,ur[2],tol;
	UM_int2 idx;

   uu_denter(UU_MTRC,(us,"ncl_proj_rbsf_to_drawing(key=%x)", eptr->key));

   status = UU_SUCCESS;
/*
.....Don't project @UN surfaces
*/
	if (ncl_tst_un_label(eptr) == 1) return(UU_FAILURE);
/*
... aak 05-may-1998: changed this routine a lot (see previous version)
... Now it works in the same way as ncl_disp_surf does.
*/
   ur[0] = 0.;
   ur[1] = 1.;
   idx = 175;
   getsc (&idx,&tol);

   nu = attrptr->numupaths;
   nv = attrptr->numvpaths;
   nup = attrptr->ptsperucrv;
   nvp = attrptr->ptspervcrv;

   du = 1./(nu-1);
   dv = 1./(nv-1);
    
	uu_list_init (&cvpts, sizeof(UM_coord), 100, 100);
/*
.....Save label display status
.....Bobby  -  2/24/94
*/
	savelabel = attrptr->label_on;

   for (i=0, u=0.; i<nu; i++, u=u+du)
   {
      if (u>1.) u = 1.;
      ur_setup_data(UM_POLYLINE_REL, &pline, sizeof(pline));
/*
.....Project label onto drawing
.....Bobby  -  2/24/94
*/
		ncl_proj_label_to_drw(eptr,&pline,attrptr,drwmat,vrefpt,vpnorm);

      cvpts.cur_cnt = 0;

      if (nup > 0)
         ncl_evolve1_crv_on_srf (eptr,tfmat,u,2,nup,&cvpts);
		else
      	ncl_evolve_crv_on_srf (eptr,tfmat,u,ur,2,tol,&cvpts,UU_NULL,UU_NULL);

      np = UU_LIST_LENGTH (&cvpts);
		pts = (UM_coord *) UU_LIST_ARRAY (&cvpts);

		for (j=0; j<np; j++)
		{
			um_nptpln(pts[j], vrefpt, vpnorm, proj_point);
         um_cctmtf(proj_point, drwmat, drw_point);
         um_vctovc(drw_point, pts[j]);
		}

      pline.no_pt = np;
		pline.pt = (UU_REAL *) pts;

      ncl_create_geom_drw(&pline, UM_DEFAULT_TF, attrptr);
      ur_update_displayable(pline.key, UM_DISPLAYABLE);
      uc_display(&pline);
/*
.....Turn off label for subsequent polylines
.....Bobby  -  2/24/94
*/
		attrptr->label_on = 0;
   }
    
   for (i=0, v=0.; i<nv; i++, v=v+dv)
   {
      if (v>1.) v = 1.;
      ur_setup_data(UM_POLYLINE_REL, &pline, sizeof(pline));
      pline.subscr = 0;

		cvpts.cur_cnt = 0;

      if (nvp > 0)
         ncl_evolve1_crv_on_srf (eptr,tfmat,v,1,nvp,&cvpts);
		else
			ncl_evolve_crv_on_srf (eptr,tfmat,v,ur,1,tol,&cvpts,UU_NULL,UU_NULL);

      np = UU_LIST_LENGTH (&cvpts);
      pts = (UM_coord *) UU_LIST_ARRAY (&cvpts);

      for (j=0; j<np; j++)
      {
         um_nptpln(pts[j], vrefpt, vpnorm, proj_point);
         um_cctmtf(proj_point, drwmat, drw_point);
         um_vctovc(drw_point, pts[j]);
      }

      pline.no_pt = np;
		pline.pt = (UU_REAL *) pts;

      ncl_create_geom_drw(&pline, UM_DEFAULT_TF, attrptr);
      ur_update_displayable(pline.key, UM_DISPLAYABLE);
      uc_display(&pline);
   }

   uu_dexitstatus("ncl_proj_rbsf_to_drawing",status);
/*
.....Restore label display status
.....Bobby
*/
	attrptr->label_on = savelabel;

	uu_list_free (&cvpts);
   return (status);
}
