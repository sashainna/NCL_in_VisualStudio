/*********************************************************************
**    NAME         :  nerbsp.c
**       CONTAINS: 
**           int ncl_disp_rbsp (eptr, tfmat, attptr)
**           int ncl_rbsp_feat (eptr, tfmat, feature_order, dploc)
**           int ncl_agcrv_frmrbsplcrv (eptr, agcrv)
**           int ncl_copy_rbsp (e1, e2)
**           int ncl_class_copy_rbsp (e1, e2, bagsize)
**           void ncl_copy_rbsp_fixed (eptr, rptr)
**           int ncl_copy_rbsp2 (e1, e2)
**           int ncl_copy_rbsp1 (e1, e2)
**
**    COPYRIGHT 1990 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       nerbsp.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:45
*********************************************************************/

#include "udebug.h"
#include "mdeval.h"
#include "gobas.h"
#include "dasnog.h"
#include "mfort.h"
#include "class.h"
#include "mattrddl.h"
#include "mcrv.h"
#include "nccs.h"
#include "msrf.h"
#include "nclfc.h"

extern int NCL_multi_ent;

/*********************************************************************
**    E_FUNCTION     : int ncl_disp_rbsp (eptr, tfmat, attptr)
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
ncl_disp_rbsp (eptr, tfmat, attrptr)
   struct UM_rbsplcrv_rec *eptr;
   UM_transf tfmat;
   struct UC_attributedatabag *attrptr;

   {
   UU_REAL u, du;
   int status, i, nu;
#if UU_COMP == UU_CIM
   static struct UM_evcrvout evout;
#endif
#if UU_COMP != UU_CIM
   struct UM_evcrvout evout;
#endif
   UM_int2 idx, ival;

   uu_denter(UU_MTRC,(us,"ncl_disp_rbsp(key=%x, tfmat=%x, attrptr=%x)",
      eptr->key, tfmat, attrptr));

   status = UU_FAILURE;
   um_set_disp_attr(attrptr);

   if (ncl_retrieve_data_fixed (eptr) == 0)
      {
      status = UU_SUCCESS;
      idx = 136;
      getifl(&idx, &ival);
      nu = ival;
      du = 1./(nu-1);

      for (i=0, u=0.; i<nu; i++, u=u+du)
         {
         if (u>1.) u = 1.;
/*         ncl_eval_rbsp (UM_POINT, u, eptr, tfmat, &evout); */
         um_ev7_rbsplcrv (UM_POINT, u, eptr, tfmat, &evout);
         glina3 (&evout.cp[0], &evout.cp[1], &evout.cp[2]);  /* draw to pt */
         }
      gdraw();


      cvlbl(&ival);   /* draw label if necessary  */
      if (ival == 1)
         {
         u = .5;
/*         ncl_eval_rbsp (UM_POINT, u, eptr, tfmat, &evout); */
         um_ev7_rbsplcrv (UM_POINT, u, eptr, tfmat, &evout);
         drwlab(&evout.cp[0], &evout.cp[1], &evout.cp[2], &eptr->key);
         }
      }

   uu_dexit;
   return (status);

   }
/*********************************************************************
**    E_FUNCTION     : ncl_rbsp_feat (eptr, tfmat, feature_order, dploc)
**       Calculate features for rational bspline curve.
**    PARAMETERS   
**       INPUT  : 
**          eptr              pointer to rational bspline curve
**          tfmat             transformation matrix
**          feature_order     order of feature to display
**          dploc             picked location on bspline
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff on error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
**    NOTE         : Created from feature/f7slpine.c um_f7_spline()
*********************************************************************/
ncl_rbsp_feat (eptr, tfmat, feature_order, dploc)
   struct UM_rbsplcrv_rec *eptr;
   UM_transf tfmat;
   int feature_order;
   UD_PLOCREC *dploc;

   {
   int status;
   UU_REAL *cntlpt;
   UM_vector vec;
   UU_REAL len;
   struct UM_evcrvout evout;
   int i,j;

   uu_denter(UU_MTRC,(us,"ncl_rbsp_feat(%d,tfmat:%x,%d)",
               eptr->key,tfmat,feature_order));

   /* transform geometry to model space */
	um_tf7_tranfrbsplcrv(eptr, tfmat, UU_FALSE);

   /* calculate desired features */
   status = UU_SUCCESS;
	switch (feature_order)
	{
		case 1:
         /* show control points */
		cntlpt = eptr->pt;
		for (i=0, j=0; i<eptr->no_pt; i++, j=j+3)
		{
			status = um_feacoord(&(cntlpt[j]));
			if (status!=0)
				return UU_FAILURE;
		}
         /* show derivatives */
		uc_init_evcrvout(eptr, &evout);

         /* scale tangent vector at end points */
		uc_evcrv(UM_FRSTDERIV, (UU_REAL) 0.0, eptr, UM_idmat, &evout);

		um_vcmnvc(cntlpt, &(cntlpt[3]), vec);
		len = um_mag(vec);
		um_unitvc(evout.dcdu, vec);
		um_vctmsc(vec, 2.0*len, vec);
		status = um_feavect(evout.cp, vec);
		if (status!=0)
			return UU_FAILURE;

         /* scale tangent vector at end points */
		uc_evcrv(UM_FRSTDERIV, (UU_REAL) 1.0, eptr, UM_idmat, &evout);

		um_vcmnvc(&(cntlpt[3*eptr->no_pt-6]), &(cntlpt[3*eptr->no_pt-3]), vec);
		len = um_mag(vec);
		um_unitvc(evout.dcdu, vec);
		um_vctmsc(vec, 2.0*len, vec);
		status = um_feavect(evout.cp, vec);
		if (status!=0)
			return UU_FAILURE;
		break;

      case 2:
         /* tangents at knot values */
         /* length of spline */
         break;

      case 3:
         break;

      default:
         status = UU_FAILURE;

      }
   uu_dexit;
   return(status);
   }
/*********************************************************************
**    E_FUNCTION     : ncl_agcrv_frmrbsplcrv (eptr, agcrv)
**       Convert a rational bspline curve into an AG curve if degree <= 3.
**    PARAMETERS
**       INPUT  :
**          eptr              pointer to rational bspline curve
**       OUTPUT :
**          agcrv             Pointer to AG curve.
**    RETURNS      :
**       UU_SUCCESS iff on error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/*int ncl_agcrv_frmrbsplcrv (eptr, agcrv)
   struct UM_rbsplcrv_rec *eptr;
   struct UM_agcrv_rec *agcrv;
   {
   int status;

   uu_denter(UU_MTRC,(us,"ncl_agcrv_frmrbsplcrv(%d)", eptr->key));

   status = UU_FAILURE;

   if (eptr->k < 5) status = um_agcrv_frmrbsplcrv (eptr, agcrv);

   uu_dexitstatus("ncl_agcrv_frmrbsplcrv", status);
   return(status);
   } */
/*********************************************************************
**    E_FUNCTION     : int ncl_copy_rbsp (e1, e2)
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
ncl_copy_rbsp(e1, e2)
   struct UM_rbsplcrv_rec *e1, *e2;

   {
   int status, isize;

   uu_denter(UU_MTRC,(us,"ncl_copy_rbsp (key=%x)", e1->key));

   status = UU_FAILURE;
   isize = sizeof(*e1);
   if (ncl_copy_geom (e1, e2, isize) == UU_SUCCESS)
     {
     status = ur_update_data_varlist (e2->key, 1, e1->t, 1, e1->no_t);
     status = ur_update_data_varlist (e2->key, 2, e1->pt, 1, e1->no_pt);
     status = ur_update_data_varlist (e2->key, 3, e1->wt, 1, e1->no_wt);
     status = ncl_retrieve_data (e2, isize);
     }

   uu_dexit;
   return (status);
   }
/*********************************************************************
**    E_FUNCTION     : int ncl_class_copy_rbsp (e1, e2, bagsize)
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
ncl_class_copy_rbsp(e1, e2, bagsize)
   struct UM_rbsplcrv_rec *e1, *e2;
   int bagsize;
   {
   int status;
   UM_int2 nclstatus;

   uu_denter(UU_MTRC,(us,"ncl_class_copy_rbsp (key=%x)", e1->key));

   status = ncl_label_wf(UM_RBSPLCRV_REL, e2->label, &e2->subscr, 0, &nclstatus);
   if (status == UU_SUCCESS) status = ncl_copy_rbsp(e1, e2);
   if (status == UU_SUCCESS)
   {
	   	if (NCL_multi_ent == 1)
			status = ncl_store_wf2(e2->key,e2->rel_num,e2->label,e2->subscr);
		else
			status = ncl_store_wf1(e2->key);
   }

   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : void ncl_copy_rbsp_fixed (eptr, rptr)
**       Copy a rational bspline curve fixed data, initialize varlists.
**    PARAMETERS   
**       INPUT  : 
**          eptr       - pointer to the entity to copy.
**       OUTPUT :
**          rptr       - pointer to the copied entity.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_copy_rbsp_fixed (eptr, rptr)
struct UM_rbsplcrv_rec *eptr, *rptr;
{
	rptr->key = -1;
	rptr->rel_num = UM_RBSPLCRV_REL;

	rptr->planar = eptr->planar;
	rptr->open = eptr->open;
	rptr->closdinu = eptr->closdinu;

	rptr->n = eptr->n;
	rptr->k = eptr->k;
	rptr->t0 = eptr->t0;
	rptr->t1 = eptr->t1;

	rptr->no_t = rptr->no_pt = rptr->no_wt = rptr->no_displst = 0;
	rptr->t = rptr->pt = rptr->wt = rptr->displst = UU_NULL;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_copy_rbsp2 (e1, e2)
**       Copy a rational bspline curve - copy fixed data from e1 to e2,
**       create spline e2.
**    PARAMETERS   
**       INPUT  : 
**          e1       - pointer to the entity to copy.
**       OUTPUT :
**          e2       - pointer to the copied entity.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_copy_rbsp2 (e1, e2)
struct UM_rbsplcrv_rec *e1, *e2;
{
	int status;

	ncl_copy_rbsp_fixed (e1,e2);
	status = ncl_copy_geom1 (e1,e2);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_copy_rbsp1 (e1, e2)
**       Copy a rational bspline curve. Reason for this separate routine:
**       It can be used when e1 holds the spline data, but the data cannot be
**       directly retrieved from Unibase (since it is a something-to-spline
**       conversion).
**    PARAMETERS   
**       INPUT  : 
**          e1       - pointer to the entity to copy.
**       OUTPUT :
**          e2       - pointer to the copied entity.
**    RETURNS      : 
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_copy_rbsp1 (e1, e2)
struct UM_rbsplcrv_rec *e1, *e2;
{
	int status;
	UM_int2 nclstatus;

	status = ncl_label_wf(UM_RBSPLCRV_REL, e2->label, &e2->subscr, 0, &nclstatus);

	if (status == UU_SUCCESS)
	{
		status = ncl_copy_rbsp2 (e1,e2);
		if (status == UU_SUCCESS)
		{
			ur_update_data_varlist (e2->key, 1, e1->t, 1, e1->no_t);
			ur_update_data_varlist (e2->key, 2, e1->pt, 1, e1->no_pt);
			ur_update_data_varlist (e2->key, 3, e1->wt, 1, e1->no_wt);

			status = ncl_retrieve_data_fixed (e2);
		}
	}

	if (status == UU_SUCCESS)
		status = ncl_store_wf1(e2->key);

	return (status);
}
