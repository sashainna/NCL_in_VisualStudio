/*********************************************************************
**    NAME         :  nevolvegn1.c
**    CONTAINS: Routines converting curves to array of points with
**              given chordal tolerance in respect of the CV curvature.
**              Applies to generic SF/CV defined thru evaluator only.
**
**       ncl_evolve_curve_gen
**       ncl_weed_list_atol
**       ncl_srf_nextpt_at_tol
**       ncl_crv_nextpt_at_tol
**       um_ev9_crv
**       ncl_h_cnpts
**       ncl_h_atan
**       ncl_h_atan1
**       ncl_revers_list
**       ncl_revers1_list
**       ncl_own_geometry
**       ncl_evolve_composite_curve
**       ncl_mover_weed
**       ncl_del_max
**       ncl_evolve_crv_on_srf
**       ncl_evolve_crv_on_srf_gen
**       ncl_evolve_bn_crv_on_srf
**       ncl_evolve_bn_crv_on_srf_gen
**       ncl_bn_crv_nextpt_at_tol
**       ncl_evolve_bn_nclx_polyln
**       nclx_polyln_on_sf
**
**    COPYRIGHT 1996 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nevolvegn1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:57
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "mdrel.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdebug.h"
#include "mdattr.h"
#include "mdeval.h"
#include "modef.h"
#include "uminmax.h"
#include "ulist.h"
#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclxmdl.h"

extern int NCLX_internal_geom;

/********************************************************************
**    E_FUNCTION: ncl_own_geometry ()
**       Returns 1 if this is NCL geometry and its data definition
**       is available.
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**          0 or 1
**    SIDE EFFECTS : none
**    WARNINGS     : reversed order in the list.
*********************************************************************/
UU_LOGICAL ncl_own_geometry ()
{
	return (NCLX_internal_geom != 1);
}
/*********************************************************************
**    E_FUNCTION: ncl_evolve_curve_gen (eptr,tfmat,told,pptr,vptr,uptr)
**       Evolve generic curve into set of points with given
**       chordal tolerance.  Curve is defined by evaluater only.
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to curve record
**          tfmat  - ID matrix
**          told   - tolerance (chord height)
**       OUTPUT :
**          pptr  - pointer to points list
**          vptr  - pointer to slope vectors list
**          uptr  - pointer to u-parameters list
**    RETURNS      :
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_evolve_curve_gen (eptr, tfmat, told, pptr, vptr,uptr)
struct UM_crvdatabag *eptr;
UM_transf tfmat;
UU_REAL told;
UU_LIST *pptr, *vptr, *uptr;
{
	UU_REAL u, un, umax;
	int nu, j, status;
	struct UM_evcrvout evout;
	UU_LIST tmpt, tmvc, tmuu;
	UM_coord ptsv, uvp;

	uc_init_evcrvout (eptr,&evout);
	uu_list_init (&tmpt, sizeof(UM_coord), 200, 200);
	if (vptr != UU_NULL) uu_list_init (&tmvc, sizeof(UM_vector), 200, 200);
	if (uptr != UU_NULL) uu_list_init (&tmuu, sizeof(UM_coord), 200, 200);
	umax = 1.;
	u    = 0.;
	status = uc_evcrv (UM_ALL,u,eptr,tfmat,&evout);
/*
.....If uc_evcrv was not a success, exit
*/
	if (status!= UU_SUCCESS) goto done;
	nu = 1;
	uu_list_push (pptr,evout.cp);
	if (vptr != UU_NULL) uu_list_push (vptr,evout.dcdu);
	if (uptr != UU_NULL)
	{
		uvp[1] = uvp[2] = 0.;
		uvp[0] = u;
		uu_list_push (uptr,uvp);
	}
	um_vctovc (evout.cp,ptsv);
	j = 0;
/*
.....scan curve with given tolerance
*/
	while (u < umax && status == UU_SUCCESS)
	{
		status = ncl_crv_nextpt_at_tol (eptr,tfmat,told,u,&un,umax,&evout,0);
		if (status == UU_SUCCESS)
		{
			uu_list_push (&tmpt,evout.cp);
			if (vptr != UU_NULL) uu_list_push (&tmvc,evout.dcdu);
			if (uptr != UU_NULL)
			{
				uvp[0] = un;
				uu_list_push (&tmuu,uvp);
			}
			j++;
			u = un;
		}
	}
/*
.....weed out points
*/
done:;
/* RLS - 03/27 */
	if (status == UU_SUCCESS)
		nu = nu + ncl_weed_list_atol
			(&tmpt,&tmvc,&tmuu,pptr,vptr,uptr,told);
	else
		nu = 0;
/* RLS - END */

	uu_list_free (&tmpt);
	if (vptr != UU_NULL) uu_list_free (&tmvc);
	if (uptr != UU_NULL) uu_list_free (&tmuu);
	return(nu);
}

/**************************************************************************
**    E_FUNCTION: ncl_weed_list_atol (tmpt,tmvc,tmuu,pptr,vptr,uptr,told)
**       Process array of points simulating curve by removing unnecessary
**       points if chordal tolerance is satisfied.
**    PARAMETERS
**       INPUT  :
**          tmpt   - list containing points
**          tmvc   - list containing I-st derivative at a point
**          tmuu   - list containing uv parameters at a point (for points
**                   evaluated on surface thru the UV curve.
**          told   - chordal tolerance
**       OUTPUT :
**          pptr  - pointer to points list
**          vptr  - pointer to slope vectors list (optional)
**          uptr  - pointer to uv points list (optional)
**    RETURNS      :
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
int ncl_weed_list_atol (tmpt,tmvc,tmuu,pptr,vptr,uptr,told)
UU_LIST *tmpt, *tmvc, *pptr, *vptr, *tmuu, *uptr;
UU_REAL told;
{
	UM_coord *pts, *ptr, *uvp, ptsv;
	UM_vector *d1s;
	int n0, is, ix, j, k, kmax, last, nu;
	UU_REAL ncl_h_cnpts();
/*
.....get pointer to data arrays
*/
	n0 = pptr->cur_cnt;
	j = tmpt->cur_cnt;
	ptr = (UM_coord *) UU_LIST_ARRAY (pptr);
	pts = (UM_coord *) UU_LIST_ARRAY (tmpt);
	if (vptr != UU_NULL) d1s = (UM_vector *) UU_LIST_ARRAY (tmvc);
	if (uptr != UU_NULL) uvp = (UM_coord *) UU_LIST_ARRAY (tmuu);
	um_vctovc (ptr[n0-1],ptsv);
/*
.....weed out points
*/
	nu = 0;
	last = 1;
	kmax = j;
	while (last < j)
	{
		is = last - 1;
		for (k = 1; k < kmax; k++)
		{
			if (ncl_h_cnpts (ptsv,&pts[last++],&pts[is],k,&ix) > told)
			{
				ix = ix + is;
				um_vctovc (&pts[ix],ptsv);
				uu_list_push (pptr,ptsv);
				if (vptr != UU_NULL) uu_list_push (vptr,&d1s[ix]);
				if (uptr != UU_NULL) uu_list_push (uptr,&uvp[ix]);
				nu++;
				goto next;
			}
		}
/*
.....this (commented out) logic checks all points after break point is
.....created. The used logic checks following points only.

		ix   = ix + is;
next:
		kmax = j - ix - 1;
		last = ix + 2;
*/
next:
		kmax = j - last + 1;
	}
/*
.....store last point
*/
	uu_list_push (pptr,&pts[j-1]);
	if (vptr != UU_NULL) uu_list_push (vptr,&d1s[j-1]);
	if (uptr != UU_NULL) uu_list_push (uptr,&uvp[j-1]);
	nu++;

	return (nu);
}

/********************************************************************
**    E_FUNCTION: ncl_h_atan (pts,dcdu,evcrv)
**       Estimate chord value for 2 close points on curve with
**			associate tangent vectors (1st derivatives).
**    PARAMETERS
**       INPUT  :
**          pts    - start point of curve segment
**          dcdu   - 1st derivative at the point
**          evcrv  - evaluation record for the end point
**       OUTPUT :
**          none
**    RETURNS      :
**          estimated chord value.
**    SIDE EFFECTS : none
**    WARNINGS     : none
********************************************************************/
UU_REAL ncl_h_atan (pts,dcdu,evcrv)
UM_coord pts;
UM_vector dcdu;
struct UM_evcrvout *evcrv;
{
	UU_REAL h, d, a1, a2;
	UM_vector vec;

	um_vcmnvc (evcrv->cp,pts,vec);
	d  = um_mag (vec);
	a1 = um_angle (dcdu,vec);
	a2 = um_angle (evcrv->dcdu,vec);
/*
.....Curve segment is flat enough to consider chord calculation only
.....if segment and derivatives are oriented in the same direction.
.....estimate chord assuming the worst case:
.....1. segment is on plane,
.....2. a1 and a2 are in opposite direction from segment direction (vec)
*/
	if (a1+UM_FUZZ < .5*UM_PI && a2+UM_FUZZ < .5*UM_PI)
	{
		if (a1 < UM_FUZZ && a2 < UM_FUZZ)
			h = 0.;
		else
/*
..... eduard 09/14/00. I replaced two previous formulas: now h is calculated
..... assuming the curve is a circular arc.
..... the older one was as below, calculating the height of the
..... triangle based on the segment
			h = (a1+a2 == 0.)? 0.: d * sin(a1) * sin(a2) / sin(UM_PI-a1-a2);
..... the newer (Jingrong's) formula was geometrically meaningless, and
..... gave wrong results for small si (sine of the angle between tangent
..... vectors)
			h = d*si/8.;
*/
			h = (d/2.)*tan((a1 + a2)/4.);
	}
	else
		h = d;

	return (h);
}

/********************************************************************
**    E_FUNCTION: ncl_h_atan1 (vec,b,v0,dv0,v1,dv1)
**       Estimate chord value for 2 close points on curve with
**			associate tangent vectors (1st derivatives).
**    PARAMETERS
**       INPUT  :
**          vec    - the segment vector (unitized if b>0.0001)
**          b      - the segment length
**          v0     - the start point tangent (unitized if dv0>0.0001)
**          dv0    - length of the the start point tangent
**          v1     - the end point tangent (unitized if dv1>0.0001)
**          dv1    - length of the the end point tangent
**       OUTPUT :
**          none
**    RETURNS      :
**          estimated chord value.
**    SIDE EFFECTS : none
**    WARNINGS     : none
********************************************************************/
UU_REAL ncl_h_atan1 (vec,b,v0,dv0,v1,dv1)
UM_vector vec,v0,v1;
UU_REAL b,dv0,dv1;
{
	UU_REAL h, a,c,dd,si,x,d0,d1;
	UM_vector vn;
	int useleft = 0, useright = 0;

	h = 0;

	if (dv0 > UM_FUZZ) useleft = 1;
	if (dv1 > UM_FUZZ) useright = 1;
	if (useleft == 0 && useright == 0)
		return (h);
/*
..... estimate deviation by modelling the curve segment as a planar
..... cubic; defined by the segment length and the 2 derivatives at
..... the endpoints. the deviation is calculated as the max deviation
..... of the cubic from the straight segment connecting the endpoints
*/
	if (useleft == 1)
	{
		d0 = um_dot (v0,vec);
		if (d0 < 0.001)
		{
			h = 0.5 * b;
			return (h);
		}
		si = 1. - d0*d0;
		if (si > 0.)
		{
			si = sqrt(si);
			d0 = si/d0;
		}
		else
			d0 = 0.;
/*
..... if the right derivative is small, we calculate the deviation as
..... 4/9 * b * d0
*/
		if (useright == 0)
		{
			h = 0.14815 * b * d0;
			return (h);
		}
	}
	if (useright == 1)
	{
		d1 = um_dot (v1,vec);
		if (d1 < 0.001)
		{
			h = 0.5 * b;
			return (h);
		}
		si = 1. - d1*d1;
		if (si > 0.)
		{
			si = sqrt(si);
			d1 = si/d1;
		}
		else
			d1 = 0.;

		if (useleft == 0)
		{
			h = 0.14815 * b * d1;
			return (h);
		}
	}
	um_cross (v0,vec,vn);
	um_cross (v1,vec,v1);
	if (um_dot(vn,v1) < 0.) d1 = -d1;
	dd = d0 + d1;
	if (fabs (dd) < UM_FUZZ)
	{
		h = 0.25 * d0 * b;
	}
	else
	{
		a = dd / (b*b);
		c = (b * d0)/dd;
		si = b*b + c*c - b*c;
		if (si > 0.)
			si = sqrt (si);
		else
			si = 0.;
		x = 0.33333333 * (b + c + si);
		h = 0.;
		if (x > 0. && x < b)
			h = fabs (a * x * (x - b) * (x - c));
		x = 0.33333333 * (b + c - si);
		if (x > 0. && x < b)
		{
			dd = fabs (a * x * (x - b) * (x - c));
			if (dd > h) h = dd;
		}
	}

	return (h);
}

/*********************************************************************
**    I_FUNCTION: S_eval_tol (cvtyp,eptr,tfmat,told,uv,us,un,du,ptsv,d1sv,evout)
**       Tries to find an optimal bigger step knowing that current step
**       is within tolerance and twice the current step is out of tolerance.
**    PARAMETERS
**       INPUT  :
**          cvtyp  - 1 = u_curve, 2 = v_curve.
**          eptr   - pointer to curve record
**          tfmat  - ID matrix
**          told   - tolerance (chord height)
**          uv     - constant parameter value
**          us     - last point parameter value
**          du     - last increment
**          ptsv   - point at us
**          d1sv   - derivative at us
**          evout  - evaluator struct to use
**       OUTPUT :
**          un     - new point parameter value
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void S_eval_tol (cvtyp,eptr,tfmat,told,uv,us,un,du,ptsv,d1sv,evout)
struct UM_srfdatabag *eptr;
UM_transf tfmat;
UU_REAL uv,us,du,*un,told;
int cvtyp;
UM_coord ptsv;
UM_vector d1sv;
struct UM_evcrvout *evout;
{
	int istat,evflg;
	UU_REAL del,uu,h;

	evflg = UM_FRSTDERIV;

	del = 0.01*du;
	for (uu = us + 199*del; uu > us + du; uu -= del)
	{
		istat = um_ev9_crv (evflg, uv, uu, cvtyp, eptr, tfmat, evout);
		if (istat == UU_FAILURE) return;
		h = ncl_h_atan (ptsv,d1sv,evout);
		if (h < told)
		{
			*un = uu; return;
		}
	}
}

/*********************************************************************
**    E_FUNCTION: ncl_srf_nextpt_at_tol (eptr, tfmat, told, uv, us,
**                    cvtyp, un, umax, evout, itsk)
**       Finds next point on the 'parametric curve_on_surface' which is
**       close enough to the previous point (evaluated at 'us' parameter),
**        so that the chord height is better than required tolerance 'told'.
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to curve record
**          tfmat  - ID matrix
**          told   - tolerance (chord height)
**          uv     - constant parameter value
**          us     - last point parameter value
**          cvtyp  - 1 = u_curve, 2 = v_curve.
**          evout  - last point evaluation record
**          itsk  - 0 - min step is used , 1 - no min step
**       OUTPUT :
**          un     - new point parameter value
**          evout  - new point evalution record
**    RETURNS      :
**          UU_FAILURE if failed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_srf_nextpt_at_tol (eptr,tfmat,told,uv,us,cvtyp,un,umax,evout,itsk)
struct UM_srfdatabag *eptr;
UM_transf tfmat;
UU_REAL uv, us, *un, umax, told;
int cvtyp,itsk;
struct UM_evcrvout *evout;
{
	int status,evflg,i;
	UU_REAL dlim,dcumsv,r,du,uu,h,delt,eps,R,dumax,ddu,step;
	UM_coord ptsv;
	UM_vector d1sv;
	UU_LOGICAL larc = UU_FALSE;
	UU_LOGICAL lhalved = UU_FALSE;
	UU_LOGICAL lstretched = UU_FALSE;

	status = UU_SUCCESS;
	eps = told*told;
	R = 50.*told;

	um_vctovc (evout->cp,ptsv);
	um_vctovc (evout->dcdu,d1sv);
	dcumsv = um_mag (d1sv);
/*
	dlim   =  10. * told;
.....this step limit is too conservative, instead use
.....step based on curve speed (dc/du) magnitude.
*/
	dlim = .15 * dcumsv;
	if (evout->curv > UM_DFUZZ)
	{
		r = 1./evout->curv;
		larc = (!ncl_setver(95) && r > R && evout->curv > R);
		delt = (told < r-UM_FUZZ)? 2*sqrt(told*(2*r-told)): 2*r;
		if (itsk == 0 && delt > dlim) delt = dlim;
	}
	else if (itsk == 0)
		delt = dlim;
	else
		delt = dcumsv;
/*
..... avoid unnecessary sqrt calls
	r      = (evout->curv > UM_DFUZZ)? 1./evout->curv: 1.e16;
	delt = (told < r-UM_FUZZ)? 2*sqrt(told*(2*r-told)): 2*r;
	if (itsk == 0 && delt > dlim) delt = dlim;
*/
	du = (dcumsv > 0.)? delt / dcumsv: 1.0;
	uu = us + du;
	if (uu > umax)
	{
		du = umax - us;
		uu = umax;
	}
	else if (itsk == 1 && !ncl_setver(96) && uu < umax - UM_FUZZ)
	{
		dumax = umax - us;
/*
..... The version 20.4 had the modified 'if' statement below - to improve
..... tessellation. For now we return to the previous 'if' condition, as the
..... new version uncovered problems while doing qatest. It should probably
..... be reintroduced later.
		if (du > 0.5*dumax)
*/
		if (du > 0.75*dumax)
		{
			du = dumax;
			uu = umax;
			lstretched = UU_TRUE;
		}
	}
/*
.....estimate chordal tolerance and try to get it better,
.....loop until du increment is useable.
*/
stp1:
	evflg = (itsk == 0)? UM_ALL: UM_FRSTDERIV;
	status = um_ev9_crv (evflg, uv, uu, cvtyp, eptr, tfmat, evout);
	if (status == UU_FAILURE) goto Done;
	delt = UM_SQDIS(evout->cp,ptsv);
	if (du > UM_FUZZ && (delt >= UM_DFUZZ || dcumsv >= UM_FUZZ))
	{
		if (delt > eps)
			h = ncl_h_atan (ptsv,d1sv,evout);
		else
			h = 0.;
		if (h > told)
		{
			lhalved = UU_TRUE;
			du = .5 * du;
			uu = us + du;
			goto stp1;
		}
		else if (itsk == 1)
		{
			if (du >= 0.2 && dcumsv >= UM_FUZZ)
			{
				UU_REAL uui;
				struct UM_evcrvout evouti;

				for (uui = us + 0.15; uui <= uu - 0.05; uui += 0.15)
				{
					status = um_ev9_crv (evflg,uv,uui,cvtyp,eptr,tfmat,&evouti);
					if (status == UU_FAILURE) goto Done;
					delt = UM_SQDIS(evouti.cp,ptsv);
					if (delt >= eps)
					{
						h = ncl_h_atan (ptsv,d1sv,&evouti);
						if (h > told)
						{
							itsk = 0;
							du = 0.5*(uui - us);
							if (du < 0.1) du = 0.1;
							uu = us + du; goto stp1;
						}
					}
				}
			}
			else if (larc && lhalved && !lstretched && du < 0.15 && h > 0.1*told)
			{
				S_eval_tol (cvtyp,eptr,tfmat,told,uv,us,&uu,du,ptsv,d1sv,evout);
			}
			else if (uu <= umax - UM_FUZZ && du < 0.05 && h <= 0.25*told && dcumsv > 1000.*told)
			{
				du = MIN2(0.125,umax-us);
				uu = us + du;
				itsk = 2;
				goto stp1;
			}
		}
	}
/*
.....Check to make sure that the current step size doesn't allow
.....for nonlinear sections to be skipped over, which ensures no
.....points passed over are out of tolerance.  If they are, then 
.....the step size is reduced and the search starts over at the
.....first point that is not in tolerance.
*/
	step = um_dcccc(evout->cp,ptsv);
	if (step > 1. && step < .15*dcumsv && !ncl_setver(99) && itsk != 3)
	{
		UU_REAL tu;
		struct UM_evcrvout evoutt;
		ddu = du/6.;
		for (i=1;i<=5;i++)
		{
			tu = us + i*ddu;
			status = um_ev9_crv (evflg,uv,tu,cvtyp,eptr,tfmat,&evoutt);
			if (status == UU_FAILURE) goto Done;
			h = ncl_h_atan (ptsv,d1sv,&evoutt);
			if (h > told)
			{
				itsk = 3;
				du = i*ddu;
				uu = us + du; goto stp1;
			}
		}
	}
	*un = uu;

Done:
	return (status);
}

/*********************************************************************
**    E_FUNCTION: ncl_crv_nextpt_at_tol (eptr, tfmat, told, us, un,
**                    umax, evout, nomin)
**       Finds next point on the curve which is close enough to the
**       previous point (evaluated at 'us' parameter) so the chord
**       height is better than required tolerance 'told'.
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to curve record
**          tfmat  - ID matrix
**          told   - tolerance (chord height)
**          us     - last point parameter value
**          umax   - maximum parameter value
**          evout  - last point evaluation record
**          nomin  - 0 - min step is used , 1 - no min step
**       OUTPUT :
**          un     - new point parameter value
**          evout  - new point evalution record
**    RETURNS      :
**          1 if failed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_crv_nextpt_at_tol (eptr, tfmat, told, us, un, umax, evout, nomin)
struct UM_crvdatabag *eptr;
UM_transf tfmat;
UU_REAL us, *un, told, umax;
struct UM_evcrvout *evout;
int nomin;
{
	int status;
	UU_REAL dcumsv, r, du, uu, h, delt;
	UM_coord ptsv;
	UM_vector d1sv;

	status = UU_SUCCESS;
	um_vctovc (evout->cp,ptsv);
	um_vctovc (evout->dcdu,d1sv);
	dcumsv = um_mag (d1sv);
	r = (evout->curv > UM_DFUZZ)? 1./evout->curv: 1.e16;
	delt = (told < r-UM_FUZZ)? 2*sqrt(told*(2*r-told)): 2*r;

	if (nomin == 0)
	{
		UU_REAL dlim = .04 * dcumsv;
/*
	dlim   =  10. * told;
.....this step limit is too conservative, instead use
.....step based on curve speed (dc/du) magnitude.
*/
		if (delt > dlim) delt = dlim;
	}

	du = (dcumsv > 0.)? delt / dcumsv: 1.0;
	uu   = us + du;
	if (uu > umax)
	{
		du = umax - us;
		uu = umax;
	}
/*
.....estimate chordal tolerance and try to get it better,
.....loop until du increment is useable.
*/
stp1:
	status = uc_evcrv (UM_ALL,uu,eptr,tfmat,evout);
	if (status == UU_FAILURE) goto Done;
	if (um_dcccc(evout->cp,ptsv) < UM_FUZZ && dcumsv < UM_FUZZ)
		h = 0.;
	else
		h = ncl_h_atan (ptsv,d1sv,evout);
	if (h > told)
	{
		if (du > UM_FUZZ)
		{
			du = .5 * du;
			uu   = us + du;
			goto stp1;
		}
	}
	*un = uu;

Done:
	return (status);
}

/********************************************************************
**    E_FUNCTION: ncl_evsrf_to_evcrv (cvtyp,evsrf,evcrv)
**       Fill a (u- or v-) curve on surface evaluator from the surface
**       evaluator record.
**    PARAMETERS
**       INPUT  :
**          cvtyp  - 1 = u_curve at v=uv; 2 = v_curve at u=uv.
**          evout  - curve evaluation record.
**       OUTPUT :
**          evout  - curve evaluation record.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_evsrf_to_evcrv (cvtyp,evsrf,evout)
int cvtyp;
struct UM_evsrfout *evsrf;
struct UM_evcrvout *evout;
{
	if (cvtyp == 1)
	{
		um_vctovc (evsrf->dsdu,evout->dcdu);
		um_vctovc (evsrf->d2sdu2,evout->d2cdu2);
		evout->curv = evsrf->ucurv;
	}
	else
	{
		um_vctovc (evsrf->dsdv,evout->dcdu);
		um_vctovc (evsrf->d2sdv2,evout->d2cdu2);
		evout->curv = evsrf->vcurv;
	}
	um_vctovc (evsrf->sp,evout->cp);
}

/********************************************************************
**    E_FUNCTION: um_ev9_crv (evflg,uv,u,cvtyp,eptr,tfmat,evout)
**       Evaluate u (or v) curve on surface using general surface
**       evaluator.
**    PARAMETERS
**       INPUT  :
**          evflg  - evaluation flag
**          uv     - constant parameter (u or v)
**          u      - variable parameter (v or u)
**          cvtyp  - 1 = u_curve at v=uv; 2 = v_curve at u=uv.
**          eptr   - pointer to surface record
**          tfmat  - ID matrix of input surface
**       OUTPUT :
**          evout  - curve evaluation record.
**    RETURNS      :
**          UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_ev9_crv (evflg, uv, u, cvtyp, eptr, tfmat, evout)
struct UM_rbsplsrf_rec *eptr;
UM_transf tfmat;
UU_REAL uv, u;
int evflg, cvtyp;
struct UM_evcrvout *evout;
{
	int status;
	struct UM_evsrfout evsrf;

	if (ncl_fmill_past())
	{
		UU_REAL uu,vv,delu,delv;
		int i;

		if (cvtyp == 1)
		{
			uu = 2*u - 0.5;
			vv = 2*uv - 0.5;
		}
		else
		{
			uu = 2*uv - 0.5;
			vv = 2*u - 0.5;
		}
		delu = delv = 0;
		if (uu > 1)
		{
			delu = uu - 1; uu = 1;
		}
        else if (uu < 0)
		{
			delu = uu; uu = 0;
		}
		if (vv > 1)
		{
			delv = vv - 1; vv = 1;
		}
        else if (vv < 0)
		{
			delv = vv; vv = 0;
		}

		status = ncl_evsrf_tf (evflg, uu, vv, eptr, tfmat, &evsrf);
		for (i = 0; i < 3; i++)
			(evout->cp)[i] = evsrf.sp[i] + delu*evsrf.dsdu[i] + delv*evsrf.dsdv[i];
		if (cvtyp == 1)
		{
			um_vctovc (evsrf.dsdu,evout->dcdu);
			if (delu == 0)
			{
				um_vctovc (evsrf.d2sdu2,evout->d2cdu2);
				evout->curv = evsrf.ucurv;
			}
			else
			{
				um_nullvc (evout->d2cdu2);
				evout->curv = 0;
			}
		}
		else
		{
			um_vctovc (evsrf.dsdv,evout->dcdu);
			if (delv == 0)
			{
				um_vctovc (evsrf.d2sdv2,evout->d2cdu2);
				evout->curv = evsrf.vcurv;
			}
			else
			{
				um_nullvc (evout->d2cdu2);
				evout->curv = 0;
			}
		}
	}
	else
	{
		if (cvtyp == 1)
			status = ncl_evsrf_tf (evflg, u, uv, eptr, tfmat, &evsrf);
		else
			status = ncl_evsrf_tf (evflg, uv, u, eptr, tfmat, &evsrf);
		ncl_evsrf_to_evcrv (cvtyp,&evsrf,evout);
	}

	return (status);
}

/********************************************************************
**    E_FUNCTION: ncl_revers_list (num,ist,ptr1,ptr2,ptr3)
**       Reverse order of (UM_coord) type items in the list data
**       structure.  Up to 3 lists can be reversed synchronously.
**    PARAMETERS
**       INPUT  :
**          num    - number of items to reverse in list(s)
**          ist    - index of the first item in list to start
**          ptr1   - pointer to the list structure or NULL
**          ptr2   - pointer to the list structure or NULL
**          ptr3   - pointer to the list structure or NULL
**       OUTPUT :
**          none
**    RETURNS      :
**          UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : reversed order in the lists.
*********************************************************************/
int ncl_revers_list (num,ist,ptr1,ptr2,ptr3)
int num, ist;
UU_LIST *ptr1, *ptr2, *ptr3;
{
	UM_coord *arr1, *arr2, *arr3;

	if (ptr1 != UU_NULL)
	{
		arr1 = (UM_coord *) UU_LIST_ARRAY (ptr1);
		ncl_revers1_list (num,ist,arr1,1);
	}
	if (ptr2 != UU_NULL)
	{
		arr2 = (UM_coord *) UU_LIST_ARRAY (ptr2);
		ncl_revers1_list (num,ist,arr2,1);
	}
	if (ptr3 != UU_NULL)
	{
		arr3 = (UM_coord *) UU_LIST_ARRAY (ptr3);
		ncl_revers1_list (num,ist,arr3,1);
	}

	return (0);
}

/********************************************************************
**    E_FUNCTION: ncl_revers1_list (num,ist,ptr,itsk)
**       Reverse order of (UM_coord) type items in the list data
**       structure.
**    PARAMETERS
**       INPUT  :
**          num    - number of items to reverse in list
**          ist    - index of the first item in list to start
**          ptr    - pointer to the list structure
**          itsk   - 1 = Point list, 2 = Vector list (directions will
**                   be reversed).
**       OUTPUT :
**          ptr    - Updated list structure
**    RETURNS      :
**          UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : reversed order in the list.
*********************************************************************/
int ncl_revers1_list (num,ist,ptr,itsk)
int num, ist, itsk;
UM_coord ptr[];
{
	int i, j, n;
	UM_coord tmp;

	n  = ist + num / 2;
	for (i=ist, j=ist+num-1; i<n; i++, j--)
	{
		um_vctovc (ptr[i],tmp);
		um_vctovc (ptr[j],ptr[i]);
		um_vctovc (tmp,ptr[j]);
	}
/*
.....Reverse vector directions
*/
	if (itsk == 2)
	{
		for (i=0;i<num;i++) um_vctmsc(ptr[ist+i],-1.,ptr[ist+i]);
	}
	return (0);
}

/*********************************************************************
**    E_FUNCTION: ncl_evolve_composite_curve (eptr, tol, pptr, vptr,uptr)
**       Evolve composite curve into set of points with given
**       chordal tolerance.  Curve is defined by evaluator only.
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to curve record
**          tol    - tolerance (chord height)
**       OUTPUT :
**          pptr  - pointer to points list
**          vptr  - pointer to slope vectors list
**          uptr  - pointer to u-parameters list
**    RETURNS      :
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_evolve_composite_curve (eptr, tol, pptr, vptr,uptr,itsk)
struct UM_crvdatabag *eptr;
UU_REAL tol;
UU_LIST *pptr, *vptr, *uptr;
int itsk;
{
	int status,j,ncv,n,np,n0,rev,i,k;
/*
.....use NCL_fixed_databag which has bigger space
.....because of memory problems.
.....Yurong 11/18/98
*/
/*
	struct UM_crvdatabag crv;
*/
	struct NCL_fixed_databag crv;
	struct UM_crvdatabag *crv1,tptr;
	UM_transf tfmat, *tf;
	UM_coord *vc,*uvp,*pp;
	struct UM_compcrv_rec *comptr;
	NCLX_mdl_composite *nclxcomptr;
	UM_param cu0,cu1;
	UU_REAL t0,t1,u,dist;
	struct UM_evcrvout evout;

/*
... follows logic in function um_pre_srf_bndr()
*/
	if (!ncl_itsa_compcrv(eptr)) return (0);
	if (pptr == UU_NULL) return (0);

	status = ncl_compcrv_getnents (eptr, &ncv);
	np = 0;
	n0 = pptr->cur_cnt;

	if (uptr != UU_NULL)
	{
		if (NCLX_internal_geom)
			nclxcomptr = (NCLX_mdl_composite *) eptr;
		else
			comptr = (struct UM_compcrv_rec *) eptr;
		cu0 = 0.0;
	}

	for (j=0; j<ncv && status == UU_SUCCESS; j++)
	{
		status = ncl_compcrv_getelm (eptr, j, &crv, &rev);
		crv1 = (struct UM_crvdatabag *) &crv;
		if(status == UU_SUCCESS)
		{
			tf = &tfmat;
			status = ncl_trimsrf_get_tf (&crv,&tf);
			if (!status)
			{
				n = ncl_evolve_curve(crv1,tf[0],tol, pptr,vptr,uptr,itsk);
				if (uptr != UU_NULL)
				{
					cu1 = (NCLX_internal_geom)? nclxcomptr->cvid[j].endparam :
							comptr->cid[j].endparam;
					uvp = (UM_coord *) UU_LIST_ARRAY(uptr);
					for (i=np;i<np+n;i++)
					{
						k = n0 + i;
						if (rev)
							uvp[k][0] = cu1 - ((cu1 - cu0) * uvp[k][0]);
						else
							uvp[k][0] = cu0 + ((cu1 - cu0) * uvp[k][0]);
					}
					cu0 = cu1;
				}
/*
.....Change direction of tangent vectors
.....when reversing list
.....Required when generating PT/cv,dis command.
.....Bobby  -  11/2/98
*/
				if (rev)
				{
					ncl_revers_list(n,n0+np,pptr,vptr,uptr);
					if (vptr != UU_NULL)
					{
						vc = (UM_coord *) UU_LIST_ARRAY(vptr);
						for (i=np;i<np+n;i++)
						{
							k = n0 + i;
							vc[k][0] = vc[k][0] * -1.;
							vc[k][1] = vc[k][1] * -1.;
							vc[k][2] = vc[k][2] * -1.;
						}
					}
				}

				np += n;
/*
... drop last point of segment if more segments follows
*/
				if(j < ncv-1)
				{
					np--;
					pptr->cur_cnt--;
					if (vptr != UU_NULL) vptr->cur_cnt--;
					if (uptr != UU_NULL) uptr->cur_cnt--;
				}
			}
		}
	}
/*
.....Trim curve to t0 anf t1
*/
	if (!NCLX_internal_geom)
	{
		comptr = (struct UM_compcrv_rec *) eptr;
		t0 = comptr->t0;
		t1 = comptr->t1;
		if (fabs (t0) <= UM_DFUZZ && fabs (t1-1.) <= UM_DFUZZ)
			return(np);
		if (uptr != UU_NULL)
			uvp = (UM_coord *) UU_LIST_ARRAY(uptr);
		else
			pp = (UM_coord *) UU_LIST_ARRAY(pptr);
		tf = &tfmat;
		status = ncl_trimsrf_get_tf (eptr,&tf);
/*
.....Make sure no points outside t0 and t1 are kept
*/
		for (j=0;j<np;j++)
		{
			if (uptr != UU_NULL)
			{
				if (uvp[j][0]-t0 < 0. || t1-uvp[j][0] < 0.)
				{
					uu_list_delete(uptr,j,1);
					if (pptr != UU_NULL) uu_list_delete(pptr,j,1);
					if (vptr != UU_NULL) uu_list_delete(vptr,j,1);
					uvp = (UM_coord *) UU_LIST_ARRAY(uptr);
					j--; np--;
				}
				else
				{
					uvp[j][0] = (uvp[j][0] - t0) / (t1 - t0);
				}
			}
			else
			{
				um_cctou_compcrv(eptr,tfmat,pp[j],&u,&dist);
				if (u < 0. || u > 1.)
				{
					uu_list_delete(pptr,j,1);
					if (vptr != UU_NULL) uu_list_delete(vptr,j,1);
					pp = (UM_coord *) UU_LIST_ARRAY(pptr);
					j--; np--;
				}
			}
		}
/*
.....Make sure t0 and t1 are exact ends
*/
		t0 = 0.;
		t1 = 1.;
		tf = &tfmat;
		status = ncl_trimsrf_get_tf (comptr,&tf);
		uc_init_evcrvout(comptr, &evout);
		uc_evcrv(UM_FRSTDERIV,t0,comptr,tfmat,&evout);
		if (uptr != UU_NULL) uvp = (UM_coord *) UU_LIST_ARRAY(uptr);
		pp = (UM_coord *) UU_LIST_ARRAY(pptr);
		if (um_dcccc(pp[0],evout.cp) > UM_DFUZZ)
		{
			if (uptr != UU_NULL)
				uu_list_insert(uptr,0,&t0);
			uu_list_insert(pptr,0,evout.cp);
			if (vptr != UU_NULL) uu_list_insert(vptr,0,evout.dcdu);
			if (uptr != UU_NULL) uvp = (UM_coord *) UU_LIST_ARRAY(uptr);
			pp = (UM_coord *) UU_LIST_ARRAY(pptr);
			np++;
		}
		uc_evcrv(UM_FRSTDERIV,t1,comptr,tfmat,&evout);
		if (um_dcccc(pp[np-1],evout.cp) > UM_DFUZZ)
		{
			uu_list_insert(pptr,np-1,evout.cp);
			if (uptr != UU_NULL) uu_list_insert(uptr,np-1,&t1);
			if (vptr != UU_NULL) uu_list_insert(vptr,np-1,evout.dcdu);
			np++;
		}
	}

	return (np);
}

/*********************************************************************
**    E_FUNCTION: ncl_evolve_crv_on_srf (eptr, tfmat, uv, vpr, cvtyp,
**                                       told, pptr, vptr,  uptr)
**       Evolve isoparametric curve on surface (generic or NCL) into set
**       of points with given chordal tolerance.
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to surface record
**          tfmat  - ID matrix of input surface
**          uv     - constant parameter
**          vpr    - variable parameter limits
**          cvtyp  - 1 = u_curve, 2 = v_curve.
**          told   - tolerance (chord height)
**       OUTPUT :
**          pptr   - pointer to points list
**          vptr   - pointer to slope vectors list
**          uptr   - pointer to u/v parameter of V/U line
**    RETURNS      :
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_evolve_crv_on_srf (eptr,tfmat,uv,vpr,cvtyp,told,pptr,vptr,uptr)
struct UM_srfdatabag *eptr;
UM_transf tfmat;
UU_REAL uv, told, *vpr;
int cvtyp;
UU_LIST *pptr, *vptr, *uptr;
{
	int status;
	struct NCL_fixed_databag bsf, *sfptr;
	struct NCL_trimsf_rec *tsfptr;

	if (ncl_own_geometry() && !ncl_fmill_past())
	{
		status = UU_SUCCESS;
		sfptr = (struct NCL_fixed_databag *) eptr;
		if (eptr->rel_num == NCL_TRIMSF_REL)
		{
			sfptr = &bsf;
			tsfptr = (struct NCL_trimsf_rec *)eptr;
			bsf.key = tsfptr->bs_key;
			status = ncl_retrieve_data_fixed(&bsf);
		}
		if (status == UU_SUCCESS)
			status = ncl_evolve_crv_on_srf_own (sfptr,tfmat,uv,vpr,cvtyp,told,
					pptr,vptr,uptr);
	}
	else
		status = ncl_evolve_crv_on_srf_gen (eptr,tfmat,uv,vpr,cvtyp,told,
					pptr,vptr,uptr);
	return (status);
}

/*********************************************************************
**    E_FUNCTION: ncl_evolve_crv_on_srf_gen (eptr, tfmat, uv, vpr, cvtyp,
**                                       told, pptr, vptr,  uptr)
**       Evolve isoparametric curve on surface into set of points
**       with given chordal tolerance.  Supports unknown type of SF/CV
**       defined by evaluater only.
**       NOTE: 1. Based on logic in ncl_evolve_curve_gen.
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to surface record
**          tfmat  - ID matrix of input surface
**          uv     - constant parameter
**          vpr    - variable parameter limits
**          cvtyp  - 1 = u_curve, 2 = v_curve.
**          told   - tolerance (chord height)
**       OUTPUT :
**          pptr   - pointer to points list
**          vptr   - pointer to slope vectors list
**          uptr   - pointer to u/v parameter of V/U line
**    RETURNS      :
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_evolve_crv_on_srf_gen (eptr,tfmat,uv,vpr,cvtyp,told,pptr,vptr,uptr)
struct UM_srfdatabag *eptr;
UM_transf tfmat;
UU_REAL uv, told, *vpr;
int cvtyp;
UU_LIST *pptr, *vptr, *uptr;
{
	struct UM_evcrvout evout;
	UU_LIST tmpt, tmvc, tmuu;
	int j, nu, status;
	UU_REAL u, un, ulst;
	UM_coord uvp, ptsv;

	uu_list_init (&tmpt, sizeof(UM_coord), 200, 200);
	if (vptr != UU_NULL) uu_list_init (&tmvc, sizeof(UM_vector), 200, 200);
	if (uptr != UU_NULL) uu_list_init (&tmuu, sizeof(UM_coord), 200, 200);

	uvp[2-cvtyp] = uv;
	uvp[2] = uv;
	ulst = vpr[1];
	u = uvp[cvtyp-1] = vpr[0];

	status = um_ev9_crv (UM_ALL, uv, u, cvtyp, (struct UM_rbsplsrf_rec *)eptr,
		tfmat, &evout);
	nu = 1;
	uu_list_push (pptr,evout.cp);
	if (vptr != UU_NULL) uu_list_push (vptr,evout.dcdu);
	if (uptr != UU_NULL) uu_list_push (uptr,uvp);
	um_vctovc (evout.cp,ptsv);
	j = 0;
/*
.....scan curve with given tolerance
*/
	while (u < ulst && status == UU_SUCCESS)
	{
		status =
			ncl_srf_nextpt_at_tol (eptr,tfmat,told,uv,u,cvtyp,&un,ulst,&evout,0);
		if (status == UU_SUCCESS)
		{
			if (um_dcccc (ptsv,evout.cp) > told || un > ulst-UM_DFUZZ)
			{
				uu_list_push (&tmpt,evout.cp);
				if (vptr != UU_NULL) uu_list_push (&tmvc,evout.dcdu);
				if (uptr != UU_NULL)
				{
					uvp[cvtyp-1] = un;
					uu_list_push (&tmuu,uvp);
				}
				j++;
				um_vctovc (evout.cp,ptsv);
			}
			u = un;
		}
	}
/*
...weed out points
*/
/* RLS - 03/27 */
	if (status == UU_SUCCESS)
		nu = nu + ncl_weed_list_atol (&tmpt,&tmvc,&tmuu,pptr,vptr,uptr,told);
	else
		nu = 0;
/* RLS - END */

	uu_list_free (&tmpt);
	if (vptr != UU_NULL) uu_list_free (&tmvc);
	if (uptr != UU_NULL) uu_list_free (&tmuu);

	return (nu);
}

/********************************************************************
**    E_FUNCTION: ncl_evolve_bn_crv_on_srf (eptr,tfmat,cptr,bplm,pptr,
**                   vptr,uvptr)
**       Evolve any surface curve defined as: (u,v) = CVf(t) &
**       (x,y,z) = SFf(u,v) to 'nu' points polyline with given chordal
**       tolerance for specified curve.
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to trimmed surface base SF record
**          tfmat  - ID matrix of input surface
**          cptr   - pointer to curve on surface
**          bplm   - u,v limits defined by curve
**       OUTPUT :
**          pptr   - pointer to points list
**          vptr   - pointer to slope vector list
**          uvptr  - pointer to u,v parameters list
**    RETURNS      :
**          Number of points stored in list.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_evolve_bn_crv_on_srf (eptr,tfmat,cptr,bplm,told,pptr,vptr,uvptr)
struct UM_rbsplsrf_rec *eptr;
struct UM_crvdatabag *cptr;
UU_REAL *bplm, told;
UM_transf tfmat;
UU_LIST *pptr, *vptr, *uvptr;
{
	int npt;
	UU_LOGICAL gogen = UU_TRUE;

	if (!ncl_own_geometry() && vptr == UU_NULL && ncl_itsa_polyline (cptr))
	{
		gogen = UU_FALSE;
		npt = ncl_evolve_bn_nclx_polyln (eptr,tfmat,cptr,bplm,told,pptr,
			uvptr);
	}

	if (ncl_own_geometry() && eptr->rel_num != NCL_REVSURF_REL &&
		!ncl_itsa_fml_base (eptr->key))
	{
		UU_LIST plst,vlst,ulst;
		uu_list_init (&plst, sizeof(UM_coord), 200, 200);
		uu_list_init (&vlst, sizeof(UM_coord), 200, 200);
		uu_list_init (&ulst, sizeof(UM_coord), 200, 200);

		npt = ncl_evolve_bn_crv_on_srf_own (eptr,tfmat,cptr,bplm,told,&plst,
			&vlst,&ulst);

		if (npt > 0)
		{
			if (pptr)
				uu_list_push_list (pptr,&plst);
			if (vptr)
				uu_list_push_list (vptr,&vlst);
			if (uvptr)
				uu_list_push_list (uvptr,&ulst);
		}
		uu_list_free (&plst);
		uu_list_free (&vlst);
		uu_list_free (&ulst);

		gogen = (npt == 0);
	}

	if (gogen)
		npt = ncl_evolve_bn_crv_on_srf_gen (eptr,tfmat,cptr,bplm,told,pptr,
		vptr,uvptr);

	return (npt);
}

/*********************************************************************
**    E_FUNCTION: ncl_evolve_bn_nclx_polyln (eptr,tfmat,cptr,bplm,
**                   told,pptr,uvptr)
**       Evolve a polyline curve  on surface
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to trimmed surface base SF record
**          tfmat  - ID matrix of input surface
**          cptr   - pointer to curve on surface
**          bplm   - u,v limits defined by curve
**          told   - tolerance (chord height)
**       OUTPUT :
**          pptr   - pointer to points list
**          uvptr  - pointer to u,v parameters list
**    RETURNS      :
**          Number of points stored in list if all O.K.
**          -466 if evaluator fails
**          0    if something else is wrong
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_evolve_bn_nclx_polyln (eptr,tfmat,cptr,bplm,told,pptr,uvptr)
struct UM_srfdatabag *eptr;
struct UM_crvdatabag *cptr;
UM_transf tfmat;
UU_REAL told, *bplm;
UU_LIST *pptr, *uvptr;
{
	struct UM_evsrfout evsrf;
	UU_LIST tmpt,tmuu;
	int i,j,nu,status,numpts;
	UM_coord uvp,ptsv,uvsv,*pt;
	NCLX_mdl_polyline *poly;
	UU_REAL dup[2];
	UU_REAL toluvsq = 0.0625;
	UU_REAL tolsq = told*told;

	poly = (NCLX_mdl_polyline *)cptr;
	pt = (UM_coord *)poly->pts;
	numpts = poly->npts;
	if (numpts <= 0) return (UU_FAILURE);

	dup[0] = bplm[1] - bplm[0];
	dup[1] = bplm[3] - bplm[2];

	uu_list_init (&tmpt, sizeof(UM_coord), numpts, numpts);
	if (uvptr != UU_NULL) uu_list_init (&tmuu, sizeof(UM_vector),numpts, numpts);

	uc_init_evsrfout (eptr, &evsrf);

	nu = 0;
	uvsv[2] = 0;

	um_vctovc (pt[0],uvp);
	for (i=0; i<2; i++)
		uvp[i] = (uvp[i] - bplm[2*i])/dup[i];
	status = uc_evsrf (UM_POINT, uvp[0], uvp[1], eptr, tfmat, &evsrf);
	if (status != UU_SUCCESS) goto Done;

	nu = 1;
	uu_list_push (pptr,evsrf.sp);
	if (uvptr != UU_NULL) uu_list_push (uvptr,uvp);
	um_vctovc (evsrf.sp,ptsv);
	for (i=0; i<2; i++) uvsv[i] = uvp[i];

	for (j = 1; j < numpts && status == UU_SUCCESS; j++)
	{
		um_vctovc (pt[j],uvp);
		for (i=0; i<2; i++)
			uvp[i] = (uvp[i] - bplm[2*i])/dup[i];
		status = uc_evsrf (UM_POINT, uvp[0], uvp[1], eptr, tfmat, &evsrf);
		if (status != UU_SUCCESS) goto Done;
		if (numpts <= 5)
		{
			uu_list_push (pptr,evsrf.sp);
			if (uvptr != UU_NULL) uu_list_push (uvptr,uvp);
			nu++;
		}
		else if (UM_SQDIS_2D (uvsv,uvp) > toluvsq || UM_SQDIS (ptsv,evsrf.sp) > tolsq)
		{
			uu_list_push (&tmpt,evsrf.sp);
			if (uvptr != UU_NULL) uu_list_push (&tmuu,uvp);
			um_vctovc (evsrf.sp,ptsv);
			for (i=0; i<2; i++) uvsv[i] = uvp[i];
		}
	}
/*
...weed out points
*/
Done:;
	if (status == UU_SUCCESS)
	{
		if (numpts > 5)
		nu = nu + ncl_weed_list_atol(&tmpt,NULLST,&tmuu,pptr,NULLST,uvptr,told);
	}
	else if (status == 466)
		nu = -466;
	else
		nu = 0;

	uu_list_free (&tmpt);
	if (uvptr != UU_NULL) uu_list_free (&tmuu);

	return (nu);
}

/*********************************************************************
**    E_FUNCTION: nclx_polyln_on_sf (eptr,tfmat,cptr,told,npt,uvsls,xyzls)
**
**       Evolve a polyline curve on surface for driving as PS (called from
**       cvonsf_init).
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to trimmed surface base SF record
**          tfmat  - ID matrix of input surface
**          crv     - pointer to uv curve on surface
**          tol    - tolerance (chord height)
**       OUTPUT :
**          npt     - number of points in uvptr & cvpts lists.
**          uvptr   - pointer to list with UV points of curve.
**          cvpts   - pointer to list with XYZ points of curve.
**
**    RETURNS      :   UU_SUCCESS / UU_FAILURE,
**                     or 466 (error in surface evaluator)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int nclx_polyln_on_sf (eptr,tfmat,crv,tol,npt,uvptr,cvpts)
struct NCL_fixed_databag *eptr,*crv;
UM_transf tfmat;
UU_REAL tol;
int *npt;
UU_LIST *uvptr, *cvpts;
{
	int j,status,nb,n2;
	UU_REAL bplm[4];
	struct NCL_fixed_databag bs,*bsptr;
	UU_REAL told = tol/2.;
	struct UM_evcrvout evcv, uvcv;
	UU_REAL u,du,un,ulst,du0;

	*npt = 0;
	u = 0.;
	du = 1.;
	ulst = 1;
	if (!cvpts || !uvptr) return (UU_FAILURE);
	status = UU_SUCCESS;

	bsptr = (struct NCL_fixed_databag *) &bs;
	bplm[0] = bplm[2] = 0;
	bplm[1] = bplm[3] = 1;
/*
...get surface data
*/
	if (ncl_retrieve_data_fixed (eptr) != 0)
		return (UU_FAILURE);
	if (ncl_itsa_trimsrf (eptr))
	{
		ncl_trimsrf_get_fixed (eptr,&nb,bplm);
		status = ncl_trimsrf_get_bs (eptr,&bsptr);
		if (status != UU_SUCCESS) return (UU_FAILURE);
	}
	else
		bsptr = eptr;

	uc_init_evcrvout (crv, &evcv);
	ulst = 1;

	u = 0.;
	du = du0 = 1.;

	status = um_ev7_crv_on_surf (UM_ALL,bsptr,crv,bplm,u,tfmat,&evcv,&uvcv);
	if (status != UU_SUCCESS) goto Done;

	n2 = 1;
	uu_list_push (cvpts,evcv.cp);
	uu_list_push (uvptr,uvcv.cp);
/*
.....scan curve with given tolerance
*/
	while (u < ulst && status == UU_SUCCESS)
	{
		status = ncl_bn_crv_nextpt_at_tol (bsptr,crv,tfmat,bplm,told,u,&un,du,
			&evcv,&uvcv);
		if (status == UU_SUCCESS)
		{
			du = un - u;
			if (du < du0) du0 = du;
			u = un;
		}
	}
	if (status == UU_SUCCESS)
	{
		if (du0 < 0.001) du0 = 0.001;
		n2 = ceil(1./du0) + 1;
		u = du = 1./(n2-1);
		for (j = 0; j < n2-1 && status == UU_SUCCESS; j++, u+=du)
		{
			status =
				um_ev7_crv_on_surf (UM_POINT,bsptr,crv,bplm,u,tfmat,&evcv,&uvcv);
			if (status != UU_SUCCESS) goto Done;
			uu_list_push (cvpts,evcv.cp);
			uu_list_push (uvptr,uvcv.cp);
		}
	}

Done:;
	if (status == 466)
		n2 = -466;
	else if (status != UU_SUCCESS)
		n2 = 0;

	*npt = n2;

	return (status);
}

/*********************************************************************
**    E_FUNCTION: ncl_evolve_bn_crv_on_srf_gen (eptr,tfmat,cptr,bplm,
**                   told,pptr,vptr,uvptr)
**       Evolve any surface curve defined as: (u,v) = CVf(t) &
**       (x,y,z) = SFf(u,v) to 'nu' points polyline with given chordal
**       tolerance for specified curve. Supports unknown type of SF/CV
**       defined by evaluator only.
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to trimmed surface base SF record
**          tfmat  - ID matrix of input surface
**          cptr   - pointer to curve on surface
**          bplm   - u,v limits defined by curve
**          told   - tolerance (chord height)
**       OUTPUT :
**          pptr   - pointer to points list
**          vptr   - pointer to slope vector list
**          uvptr  - pointer to u,v parameters list
**    RETURNS      :
**          Number of points stored in list if all O.K.
**          -466 if evaluator fails
**          0    if something else is wrong
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_evolve_bn_crv_on_srf_gen (eptr,tfmat,cptr,bplm,told,pptr,vptr,uvptr)
struct UM_srfdatabag *eptr;
struct UM_crvdatabag *cptr;
UM_transf tfmat;
UU_REAL told, *bplm;
UU_LIST *pptr, *vptr, *uvptr;
{
	struct UM_evcrvout evcv, uvcv;
	UU_LIST tmpt, tmvc, tmuu;
	int j, nu, status;
	UU_REAL u, du, un, ulst, delt, tolsq;
	UM_coord ptsv,uvp;

	if (pptr != UU_NULL)  uu_list_init (&tmpt, sizeof(UM_coord), 500, 500);
	if (vptr != UU_NULL)  uu_list_init (&tmvc, sizeof(UM_vector),500, 500);
	if (uvptr != UU_NULL) uu_list_init (&tmuu, sizeof(UM_vector),500, 500);

	uc_init_evcrvout (cptr, &evcv);
	tolsq = told*told;
	ulst = 1;

	nu = 0;
	u = 0.;
	du = 1.;

	status = um_ev7_crv_on_surf (UM_ALL,eptr,cptr,bplm,u,tfmat,&evcv,&uvcv);
	if (status != UU_SUCCESS) goto Done;

	nu = 1;
	if (pptr != UU_NULL) uu_list_push (pptr,evcv.cp);
	if (vptr != UU_NULL) uu_list_push (vptr,evcv.dcdu);
	if (uvptr != UU_NULL)
	{
		um_vctovc_2d (uvcv.cp,uvp);
		uvp[2] = u;
		uu_list_push (uvptr,uvp);
	}
	um_vctovc (evcv.cp,ptsv);
	j = 0;
/*
.....scan curve with given tolerance
*/
	while (u < ulst && status == UU_SUCCESS)
	{
		status = ncl_bn_crv_nextpt_at_tol (eptr,cptr,tfmat,bplm,told,u,&un,du,
			&evcv,&uvcv);
		if (status == UU_SUCCESS)
		{
			delt = UM_SQDIS(evcv.cp,ptsv);
			if (delt > tolsq || un > ulst-UM_DFUZZ)
			{
				if (pptr != UU_NULL) uu_list_push (&tmpt,evcv.cp);
				if (vptr != UU_NULL) uu_list_push (&tmvc,evcv.dcdu);
				if (uvptr != UU_NULL)
				{
					um_vctovc_2d (uvcv.cp,uvp);
					uvp[2] = un;
					uu_list_push (&tmuu,uvp);
				}
				j++;
				um_vctovc (evcv.cp,ptsv);
			}
			du = un - u;
			u = un;
		}
	}
/*
...weed out points
*/
Done:;
	if (status == UU_SUCCESS)
		nu = nu + ncl_weed_list_atol (&tmpt,&tmvc,&tmuu,pptr,vptr,uvptr,told);
	else if (status == 466)
		nu = -466;
	else
		nu = 0;

	if (pptr != UU_NULL) uu_list_free (&tmpt);
	if (vptr != UU_NULL) uu_list_free (&tmvc);
	if (uvptr != UU_NULL) uu_list_free (&tmuu);

	return (nu);
}

/*********************************************************************
**    E_FUNCTION: ncl_bn_crv_nextpt_at_tol (eptr,cptr,tfmat,bplm,
**                     told,us,un,du0,evout,uvcv)
**       Finds next point on the 'curve_on_surface' which is close
**       enough to the previous point (evaluated at 'us' parameter) so
**       the chord height is better than required tolerance 'told'.
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to surface record
**          cptr   - pointer to uv curve record
**          tfmat  - surface ID for surface
**          bplm   - UV box of the outer boundary curve
**          told   - tolerance (chord height)
**          us     - last point parameter value
**          du0    - previously accepted parameter increment
**          evout  - last point evaluation record
**       OUTPUT :
**          un     - new point parameter value
**          evout  - surface evalution record with new point
**          uvcv   - curve evalution record with uv coordinates at new
**                   point
**    RETURNS      :
**          1 if failed.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_bn_crv_nextpt_at_tol (eptr,cptr,tfmat,bplm,told,us,un,du0,evout,uvcv)
struct UM_srfdatabag *eptr;
struct UM_crvdatabag *cptr;
UM_transf tfmat;
UU_REAL us, *bplm, *un, told, du0;
struct UM_evcrvout *evout, *uvcv;
{
	int status;
	UU_REAL dlim, dcumsv,r,du,uu,h,delt,umax;
	UM_coord ptsv;
	UM_vector d1sv,vec,d2sv;

	status = UU_SUCCESS;
	umax = 1.0;
	um_vctovc (evout->cp,ptsv);
	um_vctovc (evout->dcdu,d1sv);
	dcumsv = um_mag (d1sv);
	if (dcumsv > UM_FUZZ)
	{
		d1sv[0] /= dcumsv; d1sv[1] /= dcumsv; d1sv[2] /= dcumsv;
	}
	dlim = .04 * dcumsv;
	delt = dlim;
	if (evout->curv > UM_DFUZZ)
	{
		r = 1./evout->curv;
		if (told < r-UM_FUZZ)
		{
			r = told*(2*r-told);
			if (r > 0)
				delt = 2*sqrt(r);
			else
				delt = 0;
		}
		else
		{
			if (r < told) r = told/2.;
			delt = 2*r;
		}
	}
	if (delt > dlim) delt = dlim;
	du = (dcumsv > 0.)? delt / dcumsv: 1.0;
/*
..... do not exceed 3/2 of the previous increment - to make the routine faster
..... since the du calculated above may have to be subdivided several times
..... at each function call - and the previously accepted du gives a more
..... accurate estimate of what will be accepted now
*/
	if (du > 1.5*du0) du = 1.5*du0;
/*
.....estimate chordal tolerance and try to get it better,
.....loop until du increment is useable.
*/
	uu = us + du;
stp1:
	if (uu > umax) uu = umax;
	status = um_ev7_crv_on_surf (UM_ALL,eptr,cptr,bplm,uu,tfmat,evout,uvcv);
	if (status != UU_SUCCESS) goto Done;
	if (du > UM_FUZZ)
	{
		um_vcmnvc (evout->cp,ptsv,vec);
		delt = um_mag (vec);
		if (delt > UM_FUZZ)
		{
			vec[0] /= delt; vec[1] /= delt; vec[2] /= delt;
			r = um_mag (evout->dcdu);
			if (r > UM_FUZZ)
			{
				d2sv[0] = evout->dcdu[0]/r;
				d2sv[1] = evout->dcdu[1]/r;
				d2sv[2] = evout->dcdu[2]/r;
			}
			else
				d2sv[0] = d2sv[1] = d2sv[2] = 0.;
			h = ncl_h_atan1 (vec,delt,d1sv,dcumsv,d2sv,r);
			if (h > told)
			{
				int j,kk,nn;
				struct UM_evcrvout evcrv;
				struct UM_evsrfout evsrf;
				UU_REAL um_vcdir();
				UU_REAL u,v,ddu;
				UM_coord ppt;

/*
..... take a fraction of the current segment 1/4,1/3, or 1/2, depending on how
..... much the segment is longer that UM_FUZZ
*/
				nn = 4;
				kk = 10000. * du; /* that is du/UM_FUZZ */
				if (kk < nn) nn = kk; /* so that ddu > UM_FUZZ */
				if (nn < 2) nn = 2;
				ddu = du/nn;
				uu = us;
/*
..... do not insert points if ncl_weed_list_atol will weed them out
*/
				for (j = 1; j < nn; j++)
				{
					uu += ddu;
					status = um_ev7_uvcrv (UM_POINT, cptr, bplm, uu, &evcrv);
					if (status != UU_SUCCESS) goto Done;
					u = evcrv.cp[0];
					if (u < 0.) u = 0.;
					else if (u > 1.) u = 1.;
					v = evcrv.cp[1];
					if (v < 0.) v = 0.;
					else if (v > 1.) v = 1.;
					status = uc_evsrf (UM_POINT,u,v,eptr,UM_DEFAULT_TF,&evsrf);
					if (status == UU_SUCCESS)
					{
						um_vctovc (evsrf.sp, evcrv.cp);
						status = ncl_transform_evcrvout (UM_POINT, tfmat, &evcrv);
					}
					if (status != UU_SUCCESS) goto Done;
					um_nptln (evcrv.cp,ptsv,vec,ppt);
					r = um_vcdir (ppt,ptsv,vec);
					if (r >= 0. && r <= delt)
						h = um_dcccc (ppt,evcrv.cp);
					else if (r < 0.)
						h = um_dcccc (ptsv,evcrv.cp);
					else
						h = um_dcccc (evout->cp,evcrv.cp);
					if (h >= told)
					{
						if (j <= nn/2 && nn >= 3) uu += ddu;
						du = uu - us;
						goto stp1;
					}
				}
				uu = us + du;
			}
		}
	}
	*un = uu;

Done:
	return (status);
}

/********************************************************************
**    E_FUNCTION: ncl_del_max (ptsv,pt,pts,tasv,ta,tas,nmp,snm,told)
**       Checks list of nmp points for maximum distance from the
**       line defined by points 'ptsv' & 'pt'. Also check intermediate
**       vectors for the maximum deviation from the starting vector.
**    PARAMETERS
**       INPUT  :
**          ptsv   - start point
**          pt     - end point
**          pts    - array of intermediate points
**          tasv   - start vector
**          ta     - end vector
**          tas    - array of intermediate vectors
**          nmp    - number of points in arrays
**       OUTPUT : none
**
**    RETURNS      :
**          indx   - index of the point in 'pts' array with maximum deviation,
**                   if more than told;
**                   else 0.
**    SIDE EFFECTS : none
**    WARNINGS     : none
********************************************************************/
int ncl_del_max (ptsv,pt,pts,tasv,ta,tas,nmp,snm,told)
UM_coord ptsv,pt,*pts;
UM_vector tasv,ta,*tas;
int nmp;
UU_REAL told,snm;
{
	int i,indx;
	UU_REAL dm,dis,c,d,p,dd;
	UM_vector vec,uvc;
	UU_REAL eps = told/40.;

	indx = -1;
	um_vcmnvc (pt,ptsv,vec);
	dis = um_mag (vec);
	if (dis < told) return (indx);
	for (i = 0; i < 3; i++) uvc[i] = vec[i]/dis;

	dm = 0.;
	for (i=0; i<nmp; i++)
	{
		um_vcmnvc (pts[i],ptsv,vec);
		dd = UM_DOT(vec,vec);

		p = UM_DOT (vec,uvc);
		if (p < 0.) p = 0.;
		if (p > dis)
			d = um_dcccc (pt,pts[i]);
		else
		{
			d = dd - p*p;
			if (d > 0) d = sqrt(d);
		}

		p = UM_DOT (tas[i],tasv);
		if (p < 1.)
		{
			dd = sqrt (dd);
			c = snm * (1 - eps/dd)*sqrt(1 - p*p);
			if (c > d) d = c;
		}

		if (d > told && d > dm)
		{
			dm = d;
			indx = i;
		}
	}

	return (indx);
}

/**************************************************************************
**    E_FUNCTION: ncl_mover_weed (nc,tmpt,tmvc,pptr,vptr,told)
**       Weed an array of motion points by applying chordal tolerance and
**       vector angle checks, similar to the logic in mover.
**    PARAMETERS
**       INPUT  :
**          tmpt   - list containing points
**          tmvc   - list containing I-st derivative at a point
**          told   - chordal tolerance
**       OUTPUT :
**          pptr  - pointer to points list
**          vptr  - pointer to slope vectors list
**    RETURNS      :
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
**************************************************************************/
int ncl_mover_weed (nc,tmpt,tmvc,pptr,vptr,told)
int nc;
UU_LIST *tmpt, *tmvc, *pptr, *vptr;
UU_REAL told;
{
	UM_coord *pts, ptsv;
	UM_vector *vta,tasv;
	int is, ix, k, kmax, last, nu;
	UM_int2 idx = 80;
	UM_real8 sc80;
	UU_REAL sinm;

	getsc(&idx, &sc80); /* maximal angle parameter */
	if (sc80 < 0.01) sc80 = 0.01;

	sinm = told / (0.9*sin(sc80/UM_RADIAN));
/*
.....get pointer to data arrays
*/
	pts = (UM_coord *) UU_LIST_ARRAY (tmpt);
	vta = (UM_vector *) UU_LIST_ARRAY (tmvc);
	um_vctovc (pts[0],ptsv);
	um_vctovc (vta[0],tasv);

	uu_list_push (pptr,ptsv);
	uu_list_push (vptr,tasv);

/*
.....weed out points
*/
	nu = 1;
	last = 1;
	kmax = nc;
	while (last < nc)
	{
		is = last - 1;
		for (k = 1; k < kmax; k++)
		{
			ix = ncl_del_max (ptsv,pts[last],&pts[is],tasv,vta[last],&vta[is],k,
				sinm,told);
			last++;
			if (ix >= 0)
			{
				ix = ix + is;
				um_vctovc (pts[ix],ptsv);
				um_vctovc (vta[ix],tasv);

				uu_list_push (pptr,ptsv);
				uu_list_push (vptr,tasv);
				nu++;
				break;
			}
		}

		kmax = nc - last + 1;
	}
/*
.....store last point
*/
	uu_list_push (pptr,&pts[nc-1]);
	uu_list_push (vptr,&vta[nc-1]);
	nu++;

	return (nu);
}
