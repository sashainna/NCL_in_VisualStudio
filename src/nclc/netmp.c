/*********************************************************************
**    NAME         :  netmp.c
**       CONTAINS:  Routines to get nearest pt on boundary of trimmed sf.
**
**           ncl_tsf_get_trimmed
**           ncl_compcrv_getnents
**           ncl_tsf_get_boundary
**           ncl_itsa_trimsrf
**           ncl_trimsrf_get_tf
**           ncl_trimsrf_get_fixed
**           ncl_trimsrf_get_bs
**           nclf_trimsrf_ckuv
**           ncl_itsa_compcrv
**           ncl_itsa_polyline
**           ncl_itsa_line
**           ncl_itsa_circle
**           ncl_compcrv_getelm
**           ncl_compcrv_getendparam
**           ncl_maxim_crv
**           ncl_get1_tpar
**           ncl_put1_tpar
**           ncl_cp_struct_uvcv_rbcv
**           ncl_itsa_uvcv_onsf
**           ncl_netsf_getnents
**			 ncl_itsa_compcrv_onsf
**
**    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       netmp.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:54
*********************************************************************/

#include "mgeom.h"
#include "modef.h"
#include "ncldef.h"
#include "nccs.h"
#include "nclx.h"
#include "nclxmdl.h"

/*int NCLX_internal_geom = 0;...jingrong 08/03/99*/
extern int NCLX_internal_geom;

/*********************************************************************
**    FUNCTION     : int ncl_tsf_get_trimmed (eptr1, irslt)
**      Determine if an entity is a trimmed surface that should be
**      driven as trimmed.
**    PARAMETERS
**       INPUT  :
**          eptr1  - Pointer to surface.
**          irslt  - =0 use flag in trimmed surf.
**                   =1 drive any trimmed surface as trimmed.
**       OUTPUT :
**          irslt  - =1 iff eptr1 is a trimmed surf that should be
**                      driven as trimmed
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_tsf_get_trimmed (eptr1, irslt)
struct NCL_fixed_databag *eptr1;
int *irslt;
{
	int status = UU_SUCCESS;
	struct NCL_trimsf_rec *tsf1;
	NCLX_mdl_trimsf *tsf2;

	if (NCLX_internal_geom)
	{
		tsf2 = (NCLX_mdl_trimsf *)eptr1;
		if (tsf2->header.relnum != NCLX_MDL_TRIMSF)
			*irslt = 0;
		else if (tsf2->trim_type == NCLX_FACE)
			*irslt = 1;
	}
	else
	{
		tsf1 = (struct NCL_trimsf_rec *)eptr1;
		if (tsf1->rel_num != NCL_TRIMSF_REL)
			*irslt = 0;
		else if (tsf1->drive_type == NCLX_FACE)
			*irslt = 1;
	}

	return (status);

}
/*********************************************************************
**    E_FUNCTION     : int ncl_compcrv_getnents (eptr, ncv)
**       Return number of elements in composite curve.
**    PARAMETERS
**       INPUT  :
**          eptr   - Pointer to composite curve.
**       OUTPUT :
**          ncv    - Number of elements in composite curve.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_compcrv_getnents (eptr, ncv)
struct NCL_fixed_databag *eptr;
int *ncv;
{
	int status = UU_SUCCESS;
	struct UM_compcrv_rec *ccvp1;
	NCLX_mdl_composite *ccvp2;

	if (NCLX_internal_geom)
	{
		ccvp2 = (NCLX_mdl_composite *) eptr;
		*ncv = ccvp2->ncurve;
	}
	else
	{
		ccvp1 = (struct UM_compcrv_rec *) eptr;
		*ncv = ccvp1->no_cid;
	}

	return (status);

}
/*********************************************************************
**    E_FUNCTION     : int ncl_tsf_get_boundary (eptr1, inx,eptr2)
**       Return a boundary uv curve of a trimmed surface.
**    PARAMETERS
**       INPUT  :
**          eptr1  - Pointer to trimmed surface.
**          inx    - index of boundary curve
**                   inx = 0 for outer; inx > 0 for inner
**       OUTPUT :
**          eptr2  - Pointer to curve.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_tsf_get_boundary (eptr1,inx,eptr2)
struct NCL_fixed_databag *eptr1, *eptr2;
int inx;
{
	int status = NCLX_SUCCESS;
	struct NCL_trimsf_rec *tsf1;

	if (NCLX_internal_geom)
	{
		NCLX_mdl_trimsf *tsf2;
		NCLX_mdl_data *cvptr;
		NCLX_mdl_composite *cmpptr1, *cmpptr2;
		NCLX_mdl_curve *bspptr1, *bspptr2;
		NCLX_mdl_polyline *poly1,*poly2;

		tsf2 = (NCLX_mdl_trimsf *) eptr1;
		if (inx == 0)
			cvptr = (NCLX_mdl_data *) tsf2->uv_cv;
		else
			cvptr = (NCLX_mdl_data *) tsf2->inner[inx*2-1];
		switch (cvptr->data.header.relnum)
		{
      case NCLX_MDL_COMPOSITE:
         cmpptr1 = (NCLX_mdl_composite *)cvptr;
         cmpptr2 = (NCLX_mdl_composite *)eptr2;
         *cmpptr2 = *cmpptr1;
         break;
      case NCLX_MDL_CURVE:
         bspptr1 = (NCLX_mdl_curve *)cvptr;
         bspptr2 = (NCLX_mdl_curve *)eptr2;
         *bspptr2 = *bspptr1;
/*
.....Polylines can be used as trimming curves JLS 9/21/99
*/
		case NCLX_MDL_POLYLINE:
			poly1 = (NCLX_mdl_polyline *)cvptr;
			poly2 = (NCLX_mdl_polyline *)eptr2;
			*poly2 = *poly1;
         break;
      default:
         status = NCLX_FAILURE;
         break;
		}
	}
	else
	{
		tsf1 = (struct NCL_trimsf_rec *)eptr1;
		eptr2->key = 0;
		if (inx == 0)
			eptr2->key = tsf1->uv_key;
		else
			eptr2->key = tsf1->ibndykey[inx*2-1];
		if (eptr2->key > 0)
			status = ncl_retrieve_data_fixed (eptr2);
		else
			status = UU_FAILURE;
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_itsa_trimsrf (eptr)
**       Determine if an entity is a trimmed surface.
**    PARAMETERS
**       INPUT  :
**          eptr   - Pointer to entity
**       OUTPUT :
**          none
**    RETURNS      :
**         true if trimmed sf; else false
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_itsa_trimsrf (eptr)
struct NCL_fixed_databag *eptr;
{

	if (NCLX_internal_geom)
	{
		return ((eptr->rel_num == NCLX_MDL_TRIMSF));
	}
	else
	{
		return ((eptr->rel_num == NCL_TRIMSF_REL));
	}
}
/*********************************************************************
**    E_FUNCTION     : int ncl_trimsrf_get_tf (eptr,tfmat)
**       Retrieve transformation matrix (identity) of the surface.
**    PARAMETERS
**       INPUT  :
**          eptr   - Pointer to entity
**       OUTPUT :
**          tfmat  - identity matrix
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_trimsrf_get_tf (eptr,tfmat)
struct NCL_fixed_databag *eptr;
UU_REAL **tfmat;
{
	int status;
	status = UU_SUCCESS;
	if (NCLX_internal_geom)
	{
		*tfmat = UM_DEFAULT_TF;
	}
	else
	{
		status = uc_retrieve_transf (eptr->key,*tfmat);
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_trimsrf_get_fixed (eptr,nb,bplm)
**       Return number of boundary curves of trimmed surface.
**    PARAMETERS
**       INPUT  :
**          eptr   - Pointer to the trimmed srf.
**       OUTPUT :
**          nb     - Number of boundary curves.
**          bplm   - boundary box limit (u,v).
**    RETURNS      : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_trimsrf_get_fixed (eptr,nb,bplm)
struct NCL_trimsf_rec *eptr;
UU_REAL *bplm;
int *nb;
{
	if (NCLX_internal_geom)
	{
		NCLX_mdl_trimsf *tmp;

		tmp = (NCLX_mdl_trimsf *) eptr;
		*nb = (tmp->ncurve/2+1);
		bplm[0] = tmp->u_min;
		bplm[1] = tmp->u_max;
		bplm[2] = tmp->v_min;
		bplm[3] = tmp->v_max;
	}
	else
	{
		*nb = (eptr->no_ibndykey/2+1);
		bplm[0] = eptr->u_min;
		bplm[1] = eptr->u_max;
		bplm[2] = eptr->v_min;
		bplm[3] = eptr->v_max;
	}

	if (bplm[1] - bplm[0] < UM_DFUZZ)
	{
		bplm[0] = 0.; bplm[1] = 1.;
	}

	if (bplm[3] - bplm[2] < UM_DFUZZ)
	{
		bplm[2] = 0.; bplm[3] = 1.;
	}

	return(UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_trimsrf_get_bs (eptr,bsrf)
**       Get the base of a trimmed surface.
**    PARAMETERS
**       INPUT  :
**          eptr   - Pointer to the trimmed srf.
**       OUTPUT :
**          bsrf   - (Pointer to the pointer to) the base surface
**    RETURNS      : UU_SUCCESS / UU_FAILURE.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_trimsrf_get_bs (eptr,bsrf)
struct NCL_trimsf_rec *eptr;
struct NCL_fixed_databag **bsrf;
{
	int status;
	status = UU_SUCCESS;

	if (NCLX_internal_geom)
	{
		NCLX_mdl_trimsf *tmp;
		tmp = (NCLX_mdl_trimsf *) eptr;
		*bsrf = (struct NCL_fixed_databag *) tmp->surf;
/*
		*bsrf = (struct NCL_fixed_databag *) eptr;
*/
	}
	else
	{
		(*bsrf)->key = eptr->bs_key;
		status = ncl_retrieve_data_fixed (*bsrf);
	}
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int nclf_trimsrf_ckuv (nclkey,u,v)
**       Make sure that the input UV is within the bounds of the trimmed
**       surface.
**    PARAMETERS
**       INPUT  :
**          nclkey - Key of trimmed surface.
**          u      - Current U-parameter.
**          v      - Current V-parameter.
**       OUTPUT :
**          u      - Updated U-parameter.
**          v      - Updated V-parameter.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclf_trimsrf_ckuv (nclkey,u,v)
UU_KEY_ID *nclkey;
UM_real4 *u,*v;
{
	int rel_num,status;
	UU_REAL uv[3],tol8;
	UM_coord *pts;
	struct NCL_trimsf_rec tsf;
	UM_srf_boundary bnd;
/*
.....Make sure it is a trimmed surface
*/
	ur_retrieve_data_relnum(*nclkey,&rel_num);
	if (rel_num != NCL_TRIMSF_REL) return;
/*
.....Get the trimmed surface entity
*/
	tsf.key = *nclkey;
	status = ncl_retrieve_data_fixed (&tsf);
/*
.....If U or V is outside the trimmed boundaries
.....Then move U and V to middle of surface
*/
	if (status == UU_SUCCESS)
	{
		ncl_get_boundary(WHOLE_BOUNDARY_LIST,&tsf,&bnd);
		if (*u <= bnd.ummx[0][0] || *u >= bnd.ummx[0][1] ||
			*v <= bnd.vmmx[0][0] || *v >= bnd.vmmx[0][1])
		{
			pts = (UM_coord *)UU_LIST_ARRAY(bnd.uvpts);
			gettol(&tol8);
			um_centroid(pts,bnd.np[0],uv,tol8);
			*u = uv[0];
			*v = uv[1];
		}
		um_free_boundary(&bnd);
	}
	return;
}

/*********************************************************************
**    E_FUNCTION     : int ncl_itsa_compcrv (eptr)
**       Determine if an entity is a composite curve.
**    PARAMETERS
**       INPUT  :
**          eptr   - Pointer to entity
**       OUTPUT :
**          none
**    RETURNS      :
**         true iff composite curve; else false
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_itsa_compcrv (eptr)
struct NCL_fixed_databag *eptr;
{

	if (NCLX_internal_geom)
	{
		return ((eptr->rel_num == NCLX_MDL_COMPOSITE));
	}
	else
	{
		return ((eptr->rel_num == UM_COMPCRV_REL));
	}

}
/*********************************************************************
**    E_FUNCTION     : int ncl_itsa_polyline (eptr)
**       Determine if an entity is a polyline.
**    PARAMETERS
**       INPUT  :
**          eptr   - Pointer to entity
**       OUTPUT :
**          none
**    RETURNS      :
**         true if polyline curve; else false
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_itsa_polyline (eptr)
struct NCL_fixed_databag *eptr;
{

	if (NCLX_internal_geom)
	{
		return ((eptr->rel_num == NCLX_MDL_POLYLINE));
	}
	else
	{
		return ((eptr->rel_num == UM_POLYLINE_REL));
	}

}
/*********************************************************************
**    E_FUNCTION     : int ncl_itsa_line (eptr)
**       Determine if an entity is a line.
**    PARAMETERS
**       INPUT  :
**          eptr   - Pointer to entity
**       OUTPUT :
**          none
**    RETURNS      :
**         true if line ; else false
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_itsa_line (eptr)
struct NCL_fixed_databag *eptr;
{
	int rel = eptr->rel_num;

	if (NCLX_internal_geom)
	{
		return ((rel == NCLX_MDL_LINE));
	}
	else
	{
		return ((rel==UM_LINE_REL)||(rel==NCL_LINE_REL));
	}

}
/*********************************************************************
**    E_FUNCTION     : int ncl_itsa_circle (eptr)
**       Determine if an entity is a circle.
**    PARAMETERS
**       INPUT  :
**          eptr   - Pointer to entity
**       OUTPUT :
**          none
**    RETURNS      :
**         true if circle ; else false
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_itsa_circle (eptr)
struct NCL_fixed_databag *eptr;
{
        int rel = eptr->rel_num;

        if (NCLX_internal_geom)
        {
                return ((rel == NCLX_MDL_CIRCLE));
        }
        else
        {
                return ((rel==UM_CIRCLE_REL)||(rel==NCL_CIRCLE_REL));
        }
}
/*********************************************************************
**    E_FUNCTION     : int ncl_compcrv_getelm (eptr, ix, key, rev)
**       Return the key and reverse flag of a composite curve element.
**    PARAMETERS
**       INPUT  :
**          eptr   - Pointer to composite curve.
**          ix     - Index to element.
**       OUTPUT :
**          key    - Key of this element.
**          rev    - Reverse flag of this element.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_compcrv_getelm (eptr1, ix, eptr2, rev)
struct NCL_fixed_databag *eptr1, *eptr2;
int ix;
UU_LOGICAL *rev;
{
	int status = UU_SUCCESS;

	if (NCLX_internal_geom)
	{
		NCLX_mdl_composite *ccvp2;
		NCLX_mdl_cmp_entity *cidp2;
		NCLX_mdl_data *cvptr;
		NCLX_mdl_curve *cvp1, *cvp2;
		NCLX_mdl_line  *lnp1, *lnp2;
		NCLX_mdl_circle *cip1,*cip2;
		NCLX_mdl_polyline *poly1, *poly2;

      ccvp2 = (NCLX_mdl_composite *) eptr1;
      cidp2 = &ccvp2->cvid[ix];
      *rev  = cidp2->reverse;
      cvptr = (NCLX_mdl_data *)cidp2->curve;
      switch (cvptr->data.header.relnum)
      {
      case NCLX_MDL_CURVE:
      case NCLX_MDL_BSPLINE:
         cvp1 = (NCLX_mdl_curve *)cvptr;
         cvp2 = (NCLX_mdl_curve *)eptr2;
         *cvp2 = *cvp1;
         break;
      case NCLX_MDL_LINE:
         lnp1 = (NCLX_mdl_line *)cvptr;
         lnp2 = (NCLX_mdl_line *)eptr2;
         *lnp2 = *lnp1;
         break;
      case NCLX_MDL_CIRCLE:
         cip1 = (NCLX_mdl_circle *)cvptr;
         cip2 = (NCLX_mdl_circle *)eptr2;
         *cip2 = *cip1;
         break;
/*
.....Polylines may be a part of a composite curve. JLS 9/21/99
*/
		case NCLX_MDL_POLYLINE:
			poly1 = (NCLX_mdl_polyline *)cvptr;
			poly2 = (NCLX_mdl_polyline *)eptr2;
			*poly2 = *poly1;
			break;
      default:
         status = UU_FAILURE;
         break;
      }
	}
	else
	{
		struct UM_compcrv_rec *ccvp1;
		struct UM_cid_rec *cidp1;
		ccvp1 = (struct UM_compcrv_rec *) eptr1;
		cidp1 = &ccvp1->cid[ix];
		eptr2->key = cidp1->crvid;
		status = ncl_retrieve_data_fixed (eptr2);
		*rev = cidp1->reverse;
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_compcrv_getendparam (eptr, ix, t)
**       Get the ending parameter of a composite curve element.
**    PARAMETERS
**       INPUT  :
**          eptr   - Pointer to composite curve.
**          ix     - Index to element.
**       OUTPUT :
**          t      - end parameter
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_compcrv_getendparam (eptr, ix, t)
struct NCL_fixed_databag *eptr;
int ix;
UU_REAL *t;
{
	struct UM_compcrv_rec *comptr;
	NCLX_mdl_composite *nclxcomptr;

	if (NCLX_internal_geom)
	{
		nclxcomptr = (NCLX_mdl_composite *) eptr;
		if (ix >= 0 && ix < nclxcomptr->ncurve)
			*t = nclxcomptr->cvid[ix].endparam;
		else
			return (UU_FAILURE);
	}
	else
	{
		comptr = (struct UM_compcrv_rec *) eptr;
		if (ix >= 0 && ix < comptr->no_cid)
			*t = comptr->cid[ix].endparam;
		else
			return (UU_FAILURE);
	}

	return (UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_maxim_crv (key,tpar)
**       Dispatching routine to maximize curve if it is trimmed.
**
**    PARAMETERS
**       INPUT  :
**        key      curve nclkey
**       OUTPUT :
**        tpar     buffer for 3 trimm t (t0,t1,t[n])
**    RETURNS      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_maxim_crv (key,tpar)
int *key;
UU_REAL *tpar;
{
	int status;

	if (NCLX_internal_geom)
		status = UU_SUCCESS;
	else
		status = ncl_maxim_crv1 (key,tpar);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_get1_tpar (key,tpar)
**       Dispatching routine to get trim parameters from the
**       general curve 'eptr'.
**    PARAMETERS
**       INPUT  :
**        key      curve nclkey
**       OUTPUT :
**        tpar     buffer for 3 trimm t (t0,t1,t[n])
**    RETURNS      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_get1_tpar (key,tpar)
int *key;
UU_REAL *tpar;
{
	int status;

	if (NCLX_internal_geom)
		status = UU_SUCCESS;
	else
		status = ncl_get1_tpar1 (key,tpar);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ncl_put1_tpar (key,tpar)
**       Dispatching routine to get end trimm parameters from a
**       general curve 'eptr'.
**    PARAMETERS
**       INPUT  :
**        key      curve nclkey
**       OUTPUT :
**        tpar      curve end parameters (3 double)
**    RETURNS      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_put1_tpar (key,tpar)
int *key;
UU_REAL *tpar;
{
	int status;

	if (NCLX_internal_geom)
		status = UU_SUCCESS;
	else
		status = ncl_put1_tpar1 (key,tpar);

	return (status);
}

/*********************************************************************
**    E_FUNCTION     :  int ncl_cp_struct_uvcv_rbcv  (eptr,rbcv)
**       Convert a uv curve on surface into a rational bspline curve
**       The new structure uses same pointers to t, pt and wt arrays
**       as old (eptr) so it CAN NOT be used where these data is
**       affected (split, trim etc).
**    PARAMETERS
**       INPUT  :
**          eptr           uv on surface curve structure
**       OUTPUT :
**          rptr           rational bspline curve struct of uvonsf
**    RETURNS      :
**          0              no error in conversion
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_cp_struct_uvcv_rbcv (eptr,rbcv)
struct UM_uvcvonsf_rec *eptr;
struct UM_rbsplcrv_rec **rbcv;
{
	if (NCLX_internal_geom)
		*rbcv = (struct UM_rbsplcrv_rec *) eptr;
	else
		ncl_cp_struct_uvcv_rbcv1 (eptr,(*rbcv));

	return(0);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_netsf_getnents (eptr, nsf)
**       Return number of sub-surfaces of net surface
**    PARAMETERS
**       INPUT  :
**          eptr   - Pointer to net surface
**       OUTPUT :
**          nsf    - Number of elements in composite curve.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_netsf_getnents (eptr, nsf)
struct NCL_fixed_databag *eptr;
int *nsf;
{
	if (NCLX_internal_geom)
	{
		NCLX_mdl_netsf *netsf;
		netsf = (NCLX_mdl_netsf *) eptr;
		*nsf = netsf->nsf;
	}
	else
	{
		struct NCL_netsf_rec *netsf;
		netsf = (struct NCL_netsf_rec*) eptr;
		*nsf = netsf->no_netkey;
	}

	return (UU_SUCCESS);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_netsf_getelm (eptr1, ix, eptr2)
**       Return subsurface of a net surface.
**    PARAMETERS
**       INPUT  :
**          eptr   - Pointer to netsf.
**          ix     - Index to element.
**       OUTPUT :
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_netsf_getelm (eptr1, ix, eptr2)
struct NCL_fixed_databag *eptr1, **eptr2;
int ix;
{
	if (NCLX_internal_geom)
	{
		NCLX_mdl_netsf *netsf;
		netsf = (NCLX_mdl_netsf *) eptr1;
		*eptr2 = netsf->sfptr[ix];
	}
	else
	{
		struct NCL_fixed_databag *s;
		struct NCL_netsf_rec *netsf;
		netsf = (struct NCL_netsf_rec *) eptr1;
		s = (struct NCL_fixed_databag *) *eptr2;
		s->key = netsf->netkey[ix];
		ncl_retrieve_data_fixed (s);
	}
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     :  UU_LOGICAL ncl_itsa_compcrv_onsf(eptr)
**    Tells if a curve is a uv-curve on surface
**    PARAMETERS
**       INPUT  :
**          eptr  -  a curve
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_TRUE/UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_itsa_compcrv_onsf(eptr)
struct NCL_fixed_databag *eptr;
{
	int status,j,ncv,rev;
	struct NCL_fixed_databag crv;

	if (!ncl_itsa_compcrv(eptr)) return (UU_FALSE);

	status = ncl_compcrv_getnents (eptr, &ncv);

	for (j=0; j<ncv && status == UU_SUCCESS; j++)
	{
		status = ncl_compcrv_getelm (eptr, j, &crv, &rev);
		if (status == UU_SUCCESS && !ncl_itsa_uvcv_onsf(&crv))
			return UU_FALSE;
	}

	return UU_TRUE;
}

/*********************************************************************
**    E_FUNCTION     :  UU_LOGICAL ncl_itsa_cv_onsf(eptr)
**    Tells if a curve is a uv-curve on surface
**    PARAMETERS
**       INPUT  :
**          cv  -  a curve
**       OUTPUT :
**          none
**    RETURNS      :
**         UU_TRUE/UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_itsa_cv_onsf(eptr)
struct NCL_fixed_databag *eptr;
{
	if (ncl_itsa_compcrv(eptr))
		return ncl_itsa_compcrv_onsf(eptr);
	else
		return ncl_itsa_uvcv_onsf(eptr);
}
