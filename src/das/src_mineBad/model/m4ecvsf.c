/*********************************************************************
**    NAME         :  m4ecvsf.c
**       CONTAINS: support to convert and manipulate curve on surface.
**          Some of functions are not used yet and are designed
**          to support trimm feature (may need some changes).
**
**			int um_c13_delete (key)
**			int um_c7_frmcvonsf (eptr, r2)
**			int um_c13_frmcvonsf (eptr, r2)
**			int um_c13_frmrbspl (eptr, r2)
**			    um_m13_addknot (r1ptr,mul,tnew,r2ptr)
**       int m_c13_trimrbsplcrv (rptr,tsplit,udel,ptr1,ptr2)
**       int um_redef_cvonsf (eptr)
**       int um_endpoints (eptr, u, udel)
**       int um_ev13_uvcvonsf (evflag, u, eptr, tfmat, evout)
**       int um_c13_reverse (eptr)
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m4ecvsf.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:02
*********************************************************************/

#include "nccs.h"
#include "umath.h"
#include "udebug.h"
#include "mdebug.h"
#include "uhep.h"
#include "go.h"
#include "mdcoord.h"
#include "mattr.h"
#include "mcrv.h"
#include "mcvh.h"
#include "modef.h"
#include	"mdrel.h"
#include "mdeval.h"
#include "nclmodals.h"

/*********************************************************************
**    E_FUNCTION     : 	int um_c7_frmcvonsf(bptr,rptr)
**       Convert a rational bspline curve into a rational bspline 
**			curve (i.e. copy original).
**    PARAMETERS   
**       INPUT  : 
**				eptr    			rational bspline curve
**       OUTPUT :  
**				rptr				copy of original rational bspline curve
**    RETURNS      : 
**				0					if no error in conversion
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c7_frmcvonsf(eptr,rptr)
	struct  UM_uvcvonsf_rec  *eptr;
	struct  UM_rbsplcrv_rec  *rptr;

	{
	int status;

	uu_denter( UU_MTRC,(us,"um_c7_frmcvonsf(%x,%x)",eptr,rptr));
	status = UU_SUCCESS;
	ur_setup_data(UM_RBSPLCRV_REL, rptr, um_curve_size(eptr));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (rptr->label, "");
	rptr->subscr = 0;

	rptr->key = -1;
	rptr->rel_num = UM_UVCVONSF_REL;

	rptr->planar = eptr->planar;
	rptr->open = eptr->open;
	rptr->closdinu = eptr->closdinu;

	rptr->n = eptr->n;
	rptr->k = eptr->k;
	rptr->t0 = eptr->t0;
	rptr->t1 = eptr->t1;
	rptr->no_t = eptr->no_t;
	um_cparray(eptr->no_t, eptr->t, rptr->t);

	rptr->no_pt = eptr->no_pt;
	um_cparray(3*eptr->no_pt, eptr->pt, rptr->pt);

	rptr->no_wt = eptr->no_wt;
	um_cparray(eptr->no_wt, eptr->wt, rptr->wt);

	uu_dexitstatus("um_c13_frmcvonsf",status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : 	int um_c13_frmcvonsf(eptr,rptr)
**       Convert a rational bspline curve into a uv curve on surface 
**    PARAMETERS   
**       INPUT  : 
**				eptr    			rational bspline curve
**       OUTPUT :  
**				rptr				copy of original rational bspline curve
**    RETURNS      : 
**				0					if no error in conversion
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c13_frmcvonsf(eptr,rptr)
	struct  UM_uvcvonsf_rec  *eptr;
	struct  UM_uvcvonsf_rec  *rptr;

	{
	int status;

	uu_denter( UU_MTRC,(us,"um_c13_frmcvonsf(%x,%x)",eptr,rptr));
	status = UU_SUCCESS;
	ur_setup_data(UM_RBSPLCRV_REL, rptr, um_curve_size(eptr));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (rptr->label, "");
	rptr->subscr = 0;

	rptr->key = -1;
	rptr->rel_num = UM_UVCVONSF_REL;

	rptr->planar = eptr->planar;
	rptr->open = eptr->open;
	rptr->closdinu = eptr->closdinu;

	rptr->n = eptr->n;
	rptr->k = eptr->k;
	rptr->bskey = -1;
	rptr->t0 = eptr->t0;
	rptr->t1 = eptr->t1;
	rptr->no_t = eptr->no_t;
	um_cparray(eptr->no_t, eptr->t, rptr->t);

	rptr->no_pt = eptr->no_pt;
	um_cparray(3*eptr->no_pt, eptr->pt, rptr->pt);

	rptr->no_wt = eptr->no_wt;
	um_cparray(eptr->no_wt, eptr->wt, rptr->wt);

	uu_dexitstatus("um_c13_frmrbspl",status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : 	int um_c13_frmrbspl(eptr,rptr)
**       Convert a rational bspline curve into a uv curve on surface 
**    PARAMETERS   
**       INPUT  : 
**				eptr    			rational bspline curve
**       OUTPUT :  
**				rptr				copy of original rational bspline curve
**    RETURNS      : 
**				0					if no error in conversion
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c13_frmrbspl(eptr,rptr)
	struct  UM_rbsplcrv_rec  *eptr;
	struct  UM_uvcvonsf_rec  *rptr;

	{
	int status;

	uu_denter( UU_MTRC,(us,"um_c13_frmrbspl(%x,%x)",eptr,rptr));
	status = UU_SUCCESS;
	ur_setup_data(UM_RBSPLCRV_REL, rptr, um_curve_size(eptr));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (rptr->label, "");
	rptr->subscr = 0;

	rptr->key = -1;
	rptr->rel_num = UM_UVCVONSF_REL;

	rptr->planar = eptr->planar;
	rptr->open = eptr->open;
	rptr->closdinu = eptr->closdinu;

	rptr->n = eptr->n;
	rptr->k = eptr->k;
	rptr->bskey = -1;
	rptr->t0 = eptr->t0;
	rptr->t1 = eptr->t1;
	rptr->no_t = eptr->no_t;
	um_cparray(eptr->no_t, eptr->t, rptr->t);

	rptr->no_pt = eptr->no_pt;
	um_cparray(3*eptr->no_pt, eptr->pt, rptr->pt);

	rptr->no_wt = eptr->no_wt;
	um_cparray(eptr->no_wt, eptr->wt, rptr->wt);

	uu_dexitstatus("um_c13_frmrbspl",status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : um_m13_addknot(r1ptr,mul,tnew,r2ptr)
**			Create a rational bspline curve (R2PTR) which represents
**			the same curve as the given rational bspline (R1PTR) but which
**			has a knot vector (and associated control polygon and
**			weight vector) with the given knot value (TNEW) included
**			the specified number of times (MUL).
**    PARAMETERS   
**       INPUT  : 
**          r1ptr							pointer to rational bspline
**				mul							desired multiplicity
**				tnew							new knot value t
**													0.0 <= tnew <= 1.0
**       OUTPUT :  
**          r2ptr							new rational bspline
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_m13_addknot(r1ptr,mul,tnew,r2ptr)
struct UM_uvcvonsf_rec *r1ptr;
int mul;
UU_REAL tnew;
struct UM_uvcvonsf_rec *r2ptr;

{
	struct UM_rbsplcrv_rec *c1ptr, *c2ptr;

	uu_denter(UU_MTRC,(us,"um_m13_addknot(%8x,%d,%f,%8x)",r1ptr,mul,
				tnew,r2ptr));
/*
.....need copy uv curves to rbspl first then call um_m7_addknot
.....and copy result to r2ptr.
*/	
	c1ptr = (struct UM_rbsplcrv_rec *) uu_malloc (um_curve_size(r1ptr));
	c2ptr = (struct UM_rbsplcrv_rec *) uu_malloc (um_curve_size(r1ptr));
	um_c7_frmcvonsf (r1ptr,c1ptr);
	um_m7_addknot (c1ptr,mul,tnew,c2ptr);
	um_c13_frmrbspl (c2ptr,r2ptr);

	uu_free (c1ptr);
	uu_free (c2ptr);

	uu_dexit;
	return 0;
}

/*********************************************************************
**    E_FUNCTION: int um_c13_trimrbsplcrv(rptr,tsplit,udel,ptr1,ptr2)
**       Split a rational bspline curve (RPTR) at parameter value
**			TSPLIT into two rational bspline curves (R1PTR and R2PTR)
**			where R1PTR goes from the start to TSPLIT and R2PTR goes
**			from TSPLIT to the end of the curve.
**    PARAMETERS   
**       INPUT  : 
**				rptr					original rational bspline curve
**				tsplit				parameter value to split at
**										0.0 < tsplit < 1.0
**       OUTPUT :  
**				ptr1					left part of curve rptr (u < tsplit)
**				ptr2					right part of curve rptr (u > tsplit)
**    RETURNS      : 
**			0 iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c13_trimrbsplcrv(rptr,tsplit,udel,ptr1,ptr2)
	struct UM_uvcvonsf_rec *rptr,*ptr1,*ptr2;
	UM_param *tsplit, *udel;

  {
   UM_param urng,t;

   uu_denter(UU_MTRC,(us,"um_c13_trimrbsplcrv(%8x,%f,%f)",rptr,
              *tsplit,*udel));

   urng = rptr->t1 - rptr->t0;
   t    = rptr->t0 + *tsplit * urng; 

   um_c13_frmcvonsf (rptr, ptr1);
   ptr1->t1 = t;
   um_c13_frmcvonsf (rptr, ptr2);
   ptr2->t0 = t;
   
   return (UU_SUCCESS);
  }

/*********************************************************************
**    E_FUNCTION     : int um_redef_cvonsf(eptr)
**       Restore original bspline curve if it was trimmed. 
**    PARAMETERS   
**       INPUT  : 
**				eptr								trimmed rational bspline curve
**       OUTPUT :  
**				eptr								original curve.
**    RETURNS      : 
**			0 iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_redef_cvonsf (eptr)
	struct UM_uvcvonsf_rec *eptr;
  {
/*
...restore original curve length
*/
   eptr->t0 = eptr->t[0];
   eptr->t1 = eptr->t[eptr->no_t - 1];

   return (UU_SUCCESS);
  }

/*********************************************************************
**    E_FUNCTION     : int um_c13_endpoints (eptr, u, udel)
**       Extend trimmed RBspl curve to new parameter limits. 
**       ? It can be outside the orginal curve definition ? 
**    PARAMETERS   
**       INPUT  : 
**				eptr   - trimmed rational bspline curve
**          u      - parameter value outside the input RBspl limits
**          udel   - not used
**       OUTPUT :  
**				eptr   - extended RBspl curve.
**    RETURNS      : 
**			0 iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c13_endpoints(eptr, u, udel)
	struct UM_rbsplcrv_rec *eptr;
   UM_param u, *udel;
  {
   UM_param urng, t;

   urng = eptr->t1 - eptr->t0;
   t = eptr->t0 + u * urng;

   if (u < 0.0) 
     {
      eptr->t0 = t;
     }
   else if (u > 1.0)
     {
      eptr->t1 = t;
     }

   return(UU_SUCCESS);
  }

/*********************************************************************
**    E_FUNCTION     : um_c13_delete(ptr)
**       Print the contents of a rational bspline record.
**    PARAMETERS   
**       INPUT  : 
**				ptr							pointer to rational bspline 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_c13_delete(key)
UU_KEY_ID key;

{
	struct UM_uvcvonsf_rec crv;
	struct NCL_nclattr_rec attr;
	int dsegid, status;

	uu_denter(UU_MTRC,(us,"um_c13_delete(%d)",key));

	status = ur_retrieve_disp_segid(key, &dsegid);
   if (status == UU_SUCCESS)
	{
		if (dsegid >= 0) uv_delsegs(dsegid);
		ur_update_blanked(key, UU_TRUE);
		attr.key = key;
		ur_retrieve_attr(&attr);
/*
.....check if label is altered
*/
		if (ncl_get_label_alter(attr.label_on))
			ncl_delete_labloc(&attr.label_on);
		crv.key = key;
		status = ncl_retrieve_data_fixed(&crv);
		ur_delete_all(key);
		if (status == UU_SUCCESS)
		{
			status = ncl_delete_sskey (crv.bskey,key);
		} 
	}
	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : um_ev13_uvcvonsf(evflag, u, eptr, tfmat, evout)
**       Evaluate rational bspline curve on surface at u.
**    PARAMETERS   
**       INPUT  : 
**          evflag - evaluation flag
**          u      - parameter value
**				eptr   - pointer to rational bspline on surface
**          tfmat  - identity matrix
**       OUTPUT :  
**          evout  - curve evaluation structure
**    RETURNS      : UU_SUCCESS or UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_ev13_uvcvonsf (evflag, u, eptr, tfmat, evout)
int evflag;
UU_REAL u;
struct UM_uvcvonsf_rec *eptr;
UM_transf tfmat;
struct UM_evcrvout *evout;
{
	struct UM_rbsplcrv_rec rbcv, *rbptr;
	struct NCL_fixed_databag bsrf, *bsptr;
	struct UM_evcrvout uvcv;
	UU_REAL bplm[4];
	int status;
	UM_transf tfm,*tfp;


	tfp = (UM_transf *)tfm;
	if (uc_retrieve_transf(eptr->key, tfm) != UU_SUCCESS)
		tfp = UU_NULL;

	bsptr = &bsrf;
	bsrf.key = eptr->bskey;
	status = ncl_retrieve_data_fixed (&bsrf);
	if (status == UU_SUCCESS)
	{
		if (ncl_itsa_trimsrf (&bsrf))
			status = ncl_trimsrf_get_bs (&bsrf,&bsptr);
		rbptr = &rbcv;
		ncl_cp_struct_uvcv_rbcv (eptr,&rbptr);
		bplm[0] = bplm[2] = 0;
		bplm[1] = bplm[3] = 1;
		status = um_ev7_crv_on_surf (evflag,&bsrf,rbptr,bplm,u,tfp,evout,&uvcv);
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : um_c13_reverse(eptr)
**       Reverse a rational bspline curve on surface.
**    PARAMETERS   
**       INPUT  : 
**				eptr  - pointer to rational bspline on surface 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c13_reverse(eptr)
struct UM_uvcvonsf_rec *eptr;
{
	struct UM_rbsplcrv_rec rbcv, *rbptr;
	int status;

	rbptr = &rbcv;
	ncl_cp_struct_uvcv_rbcv (eptr,&rbptr);
	status = ncl_rbsp_reverse (rbptr);
	
	return(status);
}
