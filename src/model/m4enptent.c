/*********************************************************************
**    NAME         :  m2nptent.c
**       CONTAINS:
**			int um_new_nptent(key, ploc, pt)
**			int um_new_nptcrv(crv, tfmat, ploc, pt)
**			int um_near_on_point(eptr, tfmat, ploc, pt)
**			int um_near_on_line(eptr, tfmat, ploc, pt)
**			int um_near_on_circle(eptr, tfmat, ploc, pt)
**			int um_near_on_curve(crv, tfmat, ploc, pt)
**			int um_near_on_curve1(crv, ploc, u,pt,ve)
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m4enptent.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:03
*********************************************************************/
#include "umath.h"
#include "usysdef.h"
#include "modef.h"
#include "mcrv.h"
#include "dasnog.h"
#include "mdgenent.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mdeval.h"
#include "mdebug.h"

/*********************************************************************
**    E_FUNCTION     :  int um_nptent(key, ploc, pt)
**			Find point (PT) on entity key nearest the cursor location 
**			in PLOC.
**    PARAMETERS   
**       INPUT  : 
**          key							entity key
**          ploc							pick location (ndc)
**       OUTPUT :  
**          pt								returned point
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_new_nptent(key, ploc, pt)
	UU_KEY_ID key;								/* entity key to find point on */
	UD_NDCLOCREC *ploc;						/* ndc portion of plocrec */
	UM_coord pt;								/* returned model coord point */

	{
	struct UM_entitydatabag e;
	UM_transf tfmat;
	UM_vector v1,v2,vpnorm;
	UM_coord pv_cc, pv_spt, pv_ept;
	UM_coord cc, ptc[2];
	UU_REAL cdot;
	int nint;
	int status;

	uu_denter(UU_MTRC, (us,"um_new_nptent(key=%x)", key));

	/* get entity data and transformation */
	e.key = key;
	status = uc_retrieve_data(&e, sizeof(e));
	if (status != UU_SUCCESS) goto done;
	status = uc_retrieve_transf(key, tfmat);
	if (status != UU_SUCCESS) goto done;

	/* switch on type */
	switch(e.rel_num)
		{
		case UM_POINT_REL:
			status = um_near_on_point(&e, tfmat, ploc, pt);
			break;
		case UM_LINE_REL:
			status = um_near_on_line(&e, tfmat, ploc, pt);
			break;
		case UM_CIRCLE_REL:
			status = um_near_on_circle(&e, tfmat, ploc, pt);
			break;
		case UM_CONIC_REL:
		case UM_POLYLINE_REL:
		case UM_RBSPLCRV_REL:
			status = um_near_on_curve(&e, tfmat, ploc, pt);
			break;
		default:
			status = UU_FAILURE;
			break;
		}
done:;
	uu_dexitstatus("um_new_nptent",status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_new_nptcrv(crv, ploc, pt)
**       Determine the point (PT) on the curve (CRV) which is closest
**			to the picked location (PLOC).
**    PARAMETERS   
**       INPUT  : 
**          crv						pointer to curve entity
**          tfmat						transformation matrix
**				ploc						picked location
**       OUTPUT :  
**          pt							nearest point on curve to picked loc
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_new_nptcrv(crv, tfmat, ploc, pt)
	struct UM_crvdatabag *crv;			/* curve to find point on */
	UD_NDCLOCREC *ploc;					/* ndc portion of plocrec */
	UM_coord pt;							/* returned model coord point */

	{
	struct UM_evcrvout evcrv;
	UM_param u, umin, delta;
	UU_REAL dist, dist_umin;
	UM_coord cc, vp_ploc, vp_crvpt;
	UM_coord vp_crvpts[20];
	int npts, i, closest;
	int status;

	uu_denter(UU_MTRC,(us,"um_new_nptcrv(key=%x)", crv->key));

	/* project the picked location onto the view plane where all tests 
		will be made */
	um_ploctocc(ploc, cc);
	uv_projvpln(cc, vp_ploc, ploc->transform);

	/* find the closest segment on the curve (as viewed on the view plane) */
	npts = 10;
	delta = 1.0/npts;
	status = uc_init_evcrvout(crv, &evcrv);
	if (status != UU_SUCCESS) goto done;
	for (i=0, u=0.0; i<=npts; i++, u=u+delta)
		{
		status = uc_evcrv(UM_POINT, u, crv, tfmat, &evcrv);
		if (status != UU_SUCCESS) goto done;
		uv_projvpln(evcrv.cp, vp_crvpts[i], ploc->transform); 
		}
	closest = um_ccnearcc(vp_ploc, npts+1, vp_crvpts);

	/* iterate over this segment to find the closest point */
	if (closest == 0) u = 0.0; else u = (closest - 1) * delta;
	delta = delta / 2.0;
	umin = u;
	dist_umin = 10000000.0;
	status = uc_init_evcrvout(crv, &evcrv);
	if (status != UU_SUCCESS) goto done;
	while (fabs(delta) > UM_FUZZ)
		{
		status = uc_evcrv(UM_POINT, u, crv, tfmat, &evcrv);
		if (status != UU_SUCCESS) goto done;
		uv_projvpln(evcrv.cp, vp_crvpt, ploc->transform); 
		dist = um_dcccc(vp_ploc, vp_crvpt);
		if (dist > dist_umin)
			{
			if ((u < umin) && (delta < 0.0))
				delta = -delta / 2.0;
			else if (delta > 0.0)
				delta = -delta / 2.0;
			}
		else
			{
			umin = u;
			dist_umin = dist;
			}
		u = u + delta;
		if (u < 0.0)
			{
			u = 0.0;
			delta = fabs(delta) / 2.0;
			}
		else if (u > 1.0)
			{
			u = 1.0;
			delta = -fabs(delta) / 2.0;
			}
		}
	uc_evcrv(UM_POINT, umin, crv, tfmat, &evcrv);
	um_vctovc(evcrv.cp, pt);
	status = UU_SUCCESS;

done:;
	uu_dexitstatus("um_new_nptcrv",status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_near_on_point(eptr, tfmat, ploc, pt)
**       Calculate the nearest point (PT) on a point (EPTR, TFMAT)
**			to the picked location (PLOC).
**    PARAMETERS   
**       INPUT  : 
**          eptr					pointer to point entity
**				tfmat					transformation matrix to position
**										point in MCS (may be UM_DEFAULT_TF)
**				ploc					picked location on point
**       OUTPUT :  
**          pt						point on point nearest to pick
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_near_on_point(eptr, tfmat, ploc, pt)
	struct UM_point_rec *eptr;
	UM_transf tfmat;
	UD_NDCLOCREC *ploc;
	UM_coord pt;

	{

	uu_denter(UU_MTRC,(us,"um_near_on_point(key=%x)", eptr->key));

	um_cctmtf(eptr->pt, tfmat, pt);

	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_near_on_line(eptr, tfmat, ploc, pt)
**       Calculate the nearest point (PT) on a line (EPTR, TFMAT)
**			to the picked location (PLOC).
**    PARAMETERS   
**       INPUT  : 
**          eptr					pointer to line entity
**				tfmat					transformation matrix to position
**										line in MCS (may be UM_DEFAULT_TF)
**				ploc					picked location on line
**       OUTPUT :  
**          pt						point on line nearest to pick
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_near_on_line(eptr, tfmat, ploc, pt)
	struct UM_line_rec *eptr;
	UM_transf tfmat;
	UD_NDCLOCREC *ploc;
	UM_coord pt;

	{
	UM_vector v1,v2,vpnorm;
	UM_coord pv_cc, pv_spt, pv_ept;
	UM_coord cc, ptc[2];
	UU_REAL cdot;
	int nint;
	int status;

	uu_denter(UU_MTRC,(us,"um_near_on_line(key=%x)", eptr->key));

	/* transform line to MCS */
	uc_transform(eptr, tfmat, UU_FALSE);

	/* convert location from ndc to MCS */
	uv_ndctocc(ploc->cord, cc, ploc->transform);

	/* project loc and line endpoints onto view plane */
	uv_projvpln(cc, pv_cc, ploc->transform);
	uv_projvpln(eptr->spt, pv_spt, ploc->transform);
	uv_projvpln(eptr->ept, pv_ept, ploc->transform);

	/* create vectors between loc and line */
	um_vcmnvc(pv_cc, pv_spt, v1);
	um_vcmnvc(pv_ept, pv_spt, v2);
	um_unitvc(v2, v2);

	/* calculate closest point on projected line */
	cdot = um_dot(v1, v2);
	um_vctmsc(v2, cdot, v2);
	um_vcplvc(eptr->spt, v2, pt);

	/* now project back onto original line */
	um_vcmnvc(eptr->ept, eptr->spt, v2);
	um_unitvc(v2, v2);
	um_vpnorm(ploc->transform, vpnorm);
	um_ilnln(eptr->spt, v2, pt, vpnorm, &nint, cc);

	/* return nearest point */
	if (nint == 1) um_vctovc(cc, pt); else status = UU_FAILURE;

	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_near_on_circle(eptr, tfmat, ploc, pt)
**       Calculate the nearest point (PT) on a circle (EPTR, TFMAT)
**			to the picked location (PLOC).
**    PARAMETERS   
**       INPUT  : 
**          eptr					pointer to circle entity
**				tfmat					transformation matrix to position
**										circle in MCS (may be UM_DEFAULT_TF)
**				ploc					picked location on circle
**       OUTPUT :  
**          pt						point on circle nearest to pick
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_near_on_circle(eptr, tfmat, ploc, pt)
	struct UM_circle_rec *eptr;
	UM_transf tfmat;
	UD_NDCLOCREC *ploc;
	UM_coord pt;

	{
	UM_vector v1,v2,vpnorm;
	UM_coord pv_cc, pv_spt, pv_ept;
	UM_coord cc, ptc[2];
	UU_REAL cdot;
	int nint;
	int status;

	uu_denter(UU_MTRC,(us,"um_near_on_circle(key=%x)", eptr->key));

	/* convert location from ndc to world */
	uv_ndctocc(ploc->cord, cc, ploc->transform);

	/* project point onto the plane of the circle */
	um_vpnorm(ploc->transform, vpnorm);
	um_ilnpln(cc,vpnorm, eptr->center, eptr->nvec, &nint, pv_cc);

	/* form line with projected point and circle center */
	um_vcmnvc(pv_cc, eptr->center, v1);
	um_unitvc(v1, v1);

	/* intersect line and circle */
	um_ilncir(eptr->center, v1, eptr->center, eptr->nvec, eptr->radius,
				&nint, ptc);

	/* return intersection point */
	um_vctovc(ptc[0], pt);

	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : um_near_on_curve(crv, tfmat, ploc, pt)
**       Determine the point (PT) on the curve (CRV, TFMAT) which is
**			closest to the picked location (PLOC). 
**    PARAMETERS   
**       INPUT  : 
**          crv						pointer to curve entity
**				tfmat						transformation matrix to position curve
**											in MCS
**				ploc						picked location on curve
**       OUTPUT :  
**          pt							nearest point on curve to picked loc
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : 
**			This routine may be used for any curve for which an evaluator
**			(UC_EVCRV) is available.
*********************************************************************/
um_near_on_curve(crv, tfmat, ploc, pt)
	struct UM_crvdatabag *crv;
	UM_transf tfmat;
	UD_NDCLOCREC *ploc;
	UM_coord pt;

	{
	struct UM_evcrvout evcrv;
	UM_param u, umin, delta;
	UU_REAL dist, dist_umin;
	UM_coord cc, vp_ploc, vp_crvpt;
	UM_coord vp_crvpts[20];
	int npts, i, closest;
	int status;

	uu_denter(UU_MTRC,(us,"um_near_on_curve(key=%x)", crv->key));

	/* project the picked location onto the view plane where all tests 
		will be made */
	um_ploctocc(ploc, cc);
	uv_projvpln(cc, vp_ploc, ploc->transform);

	/* find the closest segment on the curve (as viewed on the view plane) */
	npts = 10;
	delta = 1.0/npts;
	status = uc_init_evcrvout(crv, &evcrv);
	if (status != UU_SUCCESS) goto done;
	for (i=0, u=0.0; i<=npts; i++, u=u+delta)
		{
		status = uc_evcrv(UM_POINT, u, crv, tfmat, &evcrv);
		if (status != UU_SUCCESS) goto done;
		uv_projvpln(evcrv.cp, vp_crvpts[i], ploc->transform); 
		}
	closest = um_ccnearcc(vp_ploc, npts+1, vp_crvpts);

	/* iterate over this segment to find the closest point */
	if (closest == 0) u = 0.0; else u = (closest - 1) * delta;
	delta = delta / 2.0;
	umin = u;
	dist_umin = 10000000.0;
	uc_init_evcrvout(crv, &evcrv);
	while (fabs(delta) > UM_FUZZ)
		{
		uc_evcrv(UM_POINT, u, crv, tfmat, &evcrv);
		uv_projvpln(evcrv.cp, vp_crvpt, ploc->transform); 
		dist = um_dcccc(vp_ploc, vp_crvpt);
		if (dist > dist_umin)
			{
			if ((u < umin) && (delta < 0.0))
				delta = -delta / 2.0;
			else if (delta > 0.0)
				delta = -delta / 2.0;
			}
		else
			{
			umin = u;
			dist_umin = dist;
			}
		u = u + delta;
		if (u < 0.0)
			{
			u = 0.0;
			delta = fabs(delta) / 2.0;
			}
		else if (u > 1.0)
			{
			u = 1.0;
			delta = -fabs(delta) / 2.0;
			}
		}
	uc_evcrv(UM_POINT, umin, crv, tfmat, &evcrv);
	um_vctovc(evcrv.cp, pt);
	status = UU_SUCCESS;
done:;

	uu_dexitstatus("um_near_on_curve",status);
	return (status);
	}

/*******************************************************************
**    E_FUNCTION     : um_near_on_curve1(crv,tfmat ploc, uu,pt,ve)
**       Determine the extended point data (u-value, PT, tangent) for
**       the point on the curve CRV, which is closest to the picked 
**       location (PLOC). 
**    PARAMETERS   
**       INPUT  : 
**          crv						pointer to curve entity
**				tfmat						transformation matrix to position curve
**											in MCS
**				ploc						picked location on curve
**       OUTPUT :  
**          pt							nearest point on curve to picked loc
**          uu 						corresponding u-value 
**          ve							tangent vector at pt  
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : 
**			This routine may be used for any curve for which an evaluator
**			(UC_EVCRV) is available.
*********************************************************************/
um_near_on_curve1(crv,tfmat,ploc, uu,pt,ve)
	struct UM_crvdatabag *crv;
	UM_transf tfmat;
	UD_NDCLOCREC *ploc;
	UU_REAL *uu;
	UM_coord pt;
	UM_vector ve;

	{
	struct UM_evcrvout evcrv;
	UM_param u, umin, delta;
	UU_REAL dist, dist_umin;
	UM_coord cc, vp_ploc, vp_crvpt;
	UM_coord vp_crvpts[20];
	int npts, i, closest;
	int status;

	uu_denter(UU_MTRC,(us,"um_near_on_curve(key=%x)", crv->key));

	/* project the picked location onto the view plane where all tests 
		will be made */
	um_ploctocc(ploc, cc);
	uv_projvpln(cc, vp_ploc, ploc->transform);

	/* find the closest segment on the curve (as viewed on the view plane) */
	npts = 10;
	delta = 1.0/npts;
	status = uc_init_evcrvout(crv, &evcrv);
	if (status != UU_SUCCESS) goto done;
	for (i=0, u=0.0; i<=npts; i++, u=u+delta)
		{
		status = uc_evcrv(UM_POINT, u, crv, tfmat, &evcrv);
		if (status != UU_SUCCESS) goto done;
		uv_projvpln(evcrv.cp, vp_crvpts[i], ploc->transform); 
		}
	closest = um_ccnearcc(vp_ploc, npts+1, vp_crvpts);

	/* iterate over this segment to find the closest point */
	if (closest == 0) u = 0.0; else u = (closest - 1) * delta;
	delta = delta / 2.0;
	umin = u;
	dist_umin = 10000000.0;
	uc_init_evcrvout(crv, &evcrv);
	while (fabs(delta) > UM_FUZZ)
		{
		uc_evcrv(UM_POINT, u, crv, tfmat, &evcrv);
		uv_projvpln(evcrv.cp, vp_crvpt, ploc->transform); 
		dist = um_dcccc(vp_ploc, vp_crvpt);
		if (dist > dist_umin)
			{
			if ((u < umin) && (delta < 0.0))
				delta = -delta / 2.0;
			else if (delta > 0.0)
				delta = -delta / 2.0;
			}
		else
			{
			umin = u;
			dist_umin = dist;
			}
		u = u + delta;
		if (u < 0.0)
			{
			u = 0.0;
			delta = fabs(delta) / 2.0;
			}
		else if (u > 1.0)
			{
			u = 1.0;
			delta = -fabs(delta) / 2.0;
			}
		}
	uc_evcrv(UM_FRSTDERIV, umin, crv, tfmat, &evcrv);
	um_vctovc(evcrv.cp, pt);
	um_vctovc(evcrv.dcdu, ve);
	*uu = umin;
	status = UU_SUCCESS;
done:;

	uu_dexitstatus("um_near_on_curve",status);
	return (status);
	}

