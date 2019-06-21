/*********************************************************************
**    NAME         :  nevolve1.c
**    CONTAINS: Routines converting curves to array of points with
**              given chordal tolerance in respect of the CV curvature.
**              These routines are called by the OpenNCL Geometry Library.
**
**          ncl_evolve_curve_own
**          ncl_chordh_cnpts
**          ncl_h
**          ncl_list_push
**          ncl_evolve_uvcv
**          ncl_evolve_line
**          ncl_evolve_rbsp
**          ncl_evolve_crv_on_revsf
**          ncl_make_parm_tab_cv
**          ncl_crv_on_srf_parm
**          ncl_evolve_crv_on_srf_own
**          ncl_make_parm_tab
**          ncl_evolve_bn_crv_on_srf_own
**          ncl_ver_uvsf
**
**    COPYRIGHT 2004 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       nevolve1.c , 25.1
**     DATE AND TIME OF LAST MODIFICATION
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
#include "ulist.h"

#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"

UU_REAL ncl_chordh_cnpts();
static int ncl_make_parm_tab_cv();
static int ncl_make_parm_tab ();
static void ncl_ver_uvsf ();
/*********************************************************************
**    E_FUNCTION: ncl_evolve_uvcv (eptr,tfmat,told,pptr,vptr,uptr)
**       Evolve a CVONSF curve into set of points with given
**       chordal tolerance.
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to curve record
**          tfmat  - ID matrix
**          told   - tolerance (chord higth)
**       OUTPUT :
**          pptr  - pointer to points list
**          vptr  - pointer to slope vectors list
**          uptr  - pointer to u-parameters list
**    RETURNS      :
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_evolve_uvcv (uvcv,tfmat,told,pptr,vptr,uptr)
struct UM_uvcvonsf_rec *uvcv;
UM_transf tfmat;
UU_REAL told;
UU_LIST *pptr, *vptr, *uptr;
{
	struct UM_rbsplcrv_rec *rcrv, rbcv;
	struct NCL_fixed_databag bs;
	UU_REAL bplm[4];
	int i;

	bplm[0] = bplm[2] = 0; bplm[1] = bplm[3] = 1;
	bs.key = uvcv->bskey;
	if (ncl_retrieve_data_fixed (&bs) != UU_SUCCESS) return (0);

	rcrv = &rbcv;
	ncl_cp_struct_uvcv_rbcv (uvcv,&rcrv);
	i = ncl_evolve_bn_crv_on_srf (&bs,tfmat,rcrv,bplm,told,pptr,vptr,uptr);

	return (i);
}

/*********************************************************************
**    E_FUNCTION: ncl_evolve_line (eptr,tfmat,told,pptr,vptr,uptr)
**       Evolve a line into set of two points
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to curve record
**          tfmat  - ID matrix
**          told   - tolerance (chord higth)
**       OUTPUT :
**          pptr  - pointer to points list
**          vptr  - pointer to slope vectors list
**          uptr  - pointer to u-parameters list
**    RETURNS      :
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_evolve_line (lptr,tfmat,pptr,vptr,uptr)
struct  UM_line_rec *lptr;
UM_transf tfmat;
UU_LIST *pptr, *vptr, *uptr;
{
	struct UM_evcrvout evout;
	int evflag;
	int j;
	UU_REAL u, du;
	UM_coord uvp;

	uvp[1] = uvp[2] = 0.;
	evflag = (vptr == UU_NULL)? UM_POINT: UM_FRSTDERIV;

	uc_init_evcrvout (lptr,&evout);
	for (j = 0,u = 0.,du = 1.; j < 2; j++,u+=du)
	{
		um_ev2_line(evflag,u,lptr,tfmat,&evout);
		uu_list_push (pptr,evout.cp);
		if (vptr != UU_NULL) uu_list_push (vptr,evout.dcdu);
		if (uptr != UU_NULL)
		{
			uvp[0] = u;
			uu_list_push (uptr,uvp);
		}
	}
	return (2);
}
/*********************************************************************
**    E_FUNCTION: ncl_evolve_rbsp (rcrv,relnum,tfmat,told,pptr,vptr,uptr))
**       Evolve a spline curve into set of points with given
**       chordal tolerance.
**       NOTE: routine has same logic as ncl_evolve_crv_on_srf.
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to curve record
**          tfmat  - ID matrix
**          told   - tolerance (chord higth)
**       OUTPUT :
**          pptr  - pointer to points list
**          vptr  - pointer to slope vectors list
**          uptr  - pointer to u-parameters list
**    RETURNS      :
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_evolve_rbsp (rcrv,relnum,tfmat,told,pptr,vptr,uptr)
struct UM_rbsplcrv_rec *rcrv;
int relnum;
UM_transf tfmat;
UU_REAL told;
UU_LIST *pptr, *vptr, *uptr;
{
	struct UM_evcrvout evout;
	int i, ip, j, k, nmt, itsz;
	int istat, nt, noinc, first, last;
	UU_REAL a1, *t, du1, uprv, r, du, uu1, h1, delt, urng, r1, ulst;
	UU_REAL *utb, dcumsv, rr, t0, t1, tn, ulim, h0, h, uu, rdus;
	UU_LIST lpts,lutb;
	UM_coord ptsv, *pts, uvp;
	UM_vector d1sv;
	UM_int2 idx = 169;
	UM_real8 ver;

/*
...Map the curve to RB spline
*/
	uvp[1] = uvp[2] = 0.;
	uc_init_evcrvout (rcrv,&evout);
/*
...Set some useful pointers & variables
*/
	t   = rcrv->t;
	nmt = rcrv->no_t;
	k   = rcrv->k;
	t0  = rcrv->t0;
	t1  = rcrv->t1;
	tn  = t[nmt-1];
	nt  = nmt - k;
/*
.....Changed the size of allocation from 2*nt + 3 to
.....2*nmt. This allows for enough space in utb to
.....create an additional parameter for each t parameter.
.....JLS 10/22/99
..... Increased memory allocation by 1 to allow for addition
..... of final u paramter if necessary. IJD 9-AUG-2000
*/
	itsz  = 2*nmt+1;
	uu_list_init (&lutb,sizeof (UU_REAL),itsz,itsz);
	uu_list_init (&lpts, sizeof(UM_coord), itsz, itsz);

	urng = rcrv->t1 - rcrv->t0;
	ulst = (t1 > tn)? (tn-t0)/urng: 1.0;
/*
...Get first point on curve
*/
	uu1  = uu = uprv = .0;
	um_ev7_rbsplcrv(UM_ALL, uu, rcrv, tfmat, &evout);
	um_vctovc (evout.cp,ptsv);
	um_vctovc (evout.dcdu,d1sv);
	uu_list_push (pptr,ptsv);
	if (vptr != UU_NULL) uu_list_push (vptr,d1sv);
	if (uptr != UU_NULL)
	{
		uvp[0] = uu;
		uu_list_push (uptr,uvp);
	}
	i   = 1;
	dcumsv = um_mag (evout.dcdu);
	r      = rdus = (evout.curv > UM_DFUZZ)? 1./evout.curv: 1.e16;
/*
...Set expected u increment for desired tolerance
*/
	first = (t[0] > t0+UM_FUZZ)? 0: 1;
	getsc(&idx,&ver);
	if (first == 1)
	{
		delt = (told < r-UM_FUZZ)? 2*sqrt(told*(2*r-told)): 2*r;
		if (r > told && delt > dcumsv && ver > 9.449)
			du = 1.;
		else
			du   = .92 * delt / dcumsv;
	}
	else
		du   = (t1 > t[0])? -t0/urng+UM_DFUZZ: 1.0;
	du1 = du;
/*
...Create u table at control points & between them
*/
	nt   = ncl_make_parm_tab_cv (relnum,t,&lutb,nmt,t0,t1,ulst);
	utb = (UU_REAL *) UU_LIST_ARRAY (&lutb);

	ip      = 0;
	j       = 1;
	last    = 0;
	noinc   = 0;
	h0      = 0;
	pts = UU_NULL;
/*
...Check all utb values if they are necessary to keep tolerance
...or we need even more points
*/
	while (j<=nt)
	{
		ulim = utb[j];
Nexti:
		if (last == 1) goto Outpt;
		uu = uprv + du;
		if (fabs(uu-ulst) < UM_DFUZZ) last = 1;
		if (first == 1)
		{
/*
...When next point is farther than control point check tolerance
...at this point and save it for future checks when
...tolerance < 80% of required
*/
			if (uu > ulim)
			{
				last = 0;
				uu   = ulim;
				du1  = uu - uu1;
				if (fabs(uu-ulst) < UM_DFUZZ) last = 1;
				um_ev7_rbsplcrv(UM_ALL, uu, rcrv, tfmat, &evout);
				r1   = (evout.curv > UM_DFUZZ)? 1./evout.curv: 1.e16;
				rr   = sqrt(rdus*r1);
				rdus = r1;
				if (ip == 1) ncl_h (ptsv,evout.cp,rr,&h0);
				if (ip > 0) istat = ncl_h (pts[ip-1],evout.cp,rr,&h);
				else istat  = ncl_h (ptsv,evout.cp,rr,&h);
				h1   = ncl_chordh_cnpts (ptsv,evout.cp,pts,ip);
				if (h1 > h) h = h1;
				if (h0 > h) h = h0;
				if (istat == 1)
				{
					du1   = .8 * du1;
					du    = uu1 - uprv + du1;
					last  = 0;
					noinc = 1;
					goto Nexti;
				}
				if (h < .8*told)
				{
					if (last == 1) goto Outpt;
					uu1   = uu;
					ip   = ncl_list_push (&lpts,evout.cp,&pts);
					um_vctovc (evout.dcdu,d1sv);
					goto Nextj;
				}
				else
				{
					if (h < told || ip > 0) goto Outpt;
					last  = 0;
					noinc = 1;
					du1   = .8 * du1;
					du    = uu1 - uprv + du1;
					goto Nexti;
				}
			}
		}
/*
...Between control points, make sure that new point is still
...in tolerance and output it if above 80% of required tol.
*/
		um_ev7_rbsplcrv(UM_ALL, uu, rcrv, tfmat, &evout);
		rdus = (evout.curv > UM_DFUZZ)? 1./evout.curv: 1.e16;
		rr   = (ip == 0)? sqrt(rdus*r): sqrt(rdus*r1);
		if (first == 1)
		{
			if (ip > 0)
			{
				if (ip == 1) ncl_h (ptsv,evout.cp,rr,&h0);
				istat = ncl_h (pts[ip-1],evout.cp,rr,&h);
			}
			else istat  = ncl_h (ptsv,evout.cp,rr,&h);
			if (h0 > h) h = h0;
			h1 = ncl_chordh_cnpts (ptsv,evout.cp,pts,ip);
			if (h1 > h) h = h1;
			if (h < told && noinc == 1) goto Outpt;
			if (h > told && istat == 1)
			{
				du1   = .8 * du1;
				du    = uu1 - uprv + du1;
				last  = 0;
				noinc = 1;
				goto Nexti;
			}
			if (last == 1) goto Outpt;
			last  = 0;
/*
...below 80% of tolerance, increase distance 15% or extrapolate
...from the last control point
*/
			if (h < .8*told)
			{
				if (noinc == 1) goto Outpt;
				if (ip > 0)
				{
					delt = (told < rdus-UM_FUZZ)?
							  2*sqrt(told*(2*rdus-told)): 2*rdus;
					a1   = uu1 - uprv + .92 * delt / um_mag(evout.dcdu);
					du   = du1 = (a1 <= du)? 1.15*du: a1;
				}
				else
					du   = du1 = 1.15 * du;
/*
...since point is calculated store it to check later
*/
				uu1  = uu;
				ip   = ncl_list_push (&lpts,evout.cp,&pts);
				um_vctovc (evout.dcdu,d1sv);
				r1   = rdus;
				goto Nexti;
			}
			else
			{
				if (h < told || ip > 0) goto Outpt;
				du1   = .8 * du1;
				du    = uu1 - uprv + du1;
				noinc = 1;
				goto Nexti;
			}
		}
/*
...Before point is output make sure if knot points
...on long span are still in tolerace
*/
Outpt:
		first = 1;
		if (ip > 0)
		{
			h = ncl_chordh_cnpts (ptsv,evout.cp,pts,ip);
			if (ip == 1 && h0 > h) h = h0;
			if (h > told)
			{
				um_vctovc (pts[--ip],evout.cp);
				um_vctovc (d1sv,evout.dcdu);
				rdus = r1;
				uu   = uu1;
				last = 0;
			}
			ip = lpts.cur_cnt = 0;
		}
/*
...Output point
*/
		um_vctovc (evout.cp,ptsv);
		uu_list_push (pptr,ptsv);
		if (vptr != UU_NULL) uu_list_push (vptr,evout.dcdu);
		if (uptr != UU_NULL)
		{
			uvp[0] = uu;
			uu_list_push (uptr,uvp);
		}
		i++;
		uprv = uu1 = uu;
		if (last == 1) break;
		h0   = 0;
/*
...estimate next step, fix limit if necessary &
...go for next point
*/
		dcumsv = um_mag (evout.dcdu);
		r    = rdus;
		delt = (told < r-UM_FUZZ)? 2*sqrt(told*(2*r-told)): 2*r;
		du   = du1 = .92 * delt / dcumsv;
		noinc = 0;
		if (uprv < utb[j-1]) ulim = utb[--j];
		if (uprv < ulim - UM_FUZZ) goto Nexti;

Nextj:
		j++;
	}
/*
...Output last point if curve is extended
*/
	if (uprv < 1 - .1*UM_FUZZ)
	{
		uu = 1.0;
		um_ev7_rbsplcrv(UM_FRSTDERIV, uu, rcrv, tfmat, &evout);
		uu_list_push (pptr,evout.cp);
		if (vptr != UU_NULL) uu_list_push (vptr,evout.dcdu);
		if (uptr != UU_NULL)
		{
			uvp[0] = uu;
			uu_list_push (uptr,uvp);
		}
		i++;
	}

	uu_list_free (&lutb);
	uu_list_free (&lpts);
	return (i);
}

/********************************************************************
**    E_FUNCTION: ncl_chordh_cnpts (ptsv, pt, pts, nmp)
**       Get maximum chord height on arc from point 'ptsv' thru
**       list of (nmp) points 'pts' to point 'pt'.
**    PARAMETERS
**       INPUT  :
**          ptsv   - start point of CV arc
**          pt     - end point of CV arc
**          pts    - array of intermediate points on arc
**          nmp    - number of points in array
**       OUTPUT :
**          none.
**    RETURNS      :
**          calculated (maximum) chordal height
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL ncl_chordh_cnpts (ptsv, pt, pts, nmp)
UM_coord ptsv, pt, *pts;
int nmp;
{
	int i;
	UU_REAL d, dm;
	UM_coord ppt;
	UM_vector vec;

	um_vcmnvc (pt,ptsv,vec);
	um_unitvc (vec,vec);

	dm = 0.;
	for (i=0; i<nmp; i++)
	{
		um_nptln (pts[i],ptsv,vec,ppt);
		d = um_dcccc (ppt,pts[i]);
		if (d > dm) dm = d;
	}

	return (dm);
}

/********************************************************************
**    E_FUNCTION: ncl_h (ptsv, pt, r, h)
**       Estimate maximum chord height of arc.
**    PARAMETERS
**       INPUT  :
**          ptsv   - start point of CV arc
**          pt     - end point of CV arc
**          r      - arc radius
**       OUTPUT :
**          h      - chord height.
**    RETURNS      :
**          0 - if arc is << 180 degree, otherwise 1 & no h.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_h (ptsv,pt,r,h)
UM_coord ptsv, pt;
UU_REAL r,*h;
{
	UU_REAL d;
	int stat;

	d  = .5 * um_dcccc (pt,ptsv);
	stat = (d < r)? 0: 1;
	if (stat == 0) *h = r - sqrt(r*r-d*d);
	else *h = r;
	return(stat);
}

/********************************************************************
**    E_FUNCTION: ncl_make_parm_tab_cv (relnum,t,utb,nmt,t0,t1)
**       Create parameter array for RB spline at knots & at between
**       them (in 1/2) using original parameter array.
**    PARAMETERS
**       INPUT  :
**          relnum - relation number of parametric entity for which
**                   array of parameters is used
**          t      - pointer to original parameters
**          nmt    - number of values in t table
**          t0,t1  - min & max value of parameter
**       OUTPUT :
**          utb    - array of parameters
**    RETURNS      :
**          number of parameters in utb
**    SIDE EFFECTS : none
**    WARNINGS     : none
********************************************************************/
static int ncl_make_parm_tab_cv (relnum,t,lutb,nmt,t0,t1,ulst)
UU_REAL *t,t0,t1,ulst;
UU_LIST *lutb;
int relnum, nmt;
{
	int m,j,jj,nt,nt0,m2,lchk;
	UU_REAL a1,urng,u,u0,u1,uu,uu0;
	UU_REAL amid,amax;
	UM_int2 idx = 169;
	UM_real8 ver;

	switch (relnum)
	{
		case NCL_EVALCV_REL:
		{
			urng = u1 = 1.;
			u0   = 0.;
			break;
		}
		default:
		{
			urng = t1 - t0;
			u0   = t0;
			u1   = t1;
		}
	}
/*
...Create u table at control points & between them
*/
	amax = amid = 0;

	u = t[0];
	uu0 = (u - u0) / urng;

	if (uu0 <= -UM_DFUZZ) uu0 = 0.;

	uu = uu0;
	uu_list_push (lutb,&uu);
	nt0 = lutb->cur_cnt;

	getsc(&idx,&ver);
	lchk = (ver < 9.449)? 0: 1;

Redo:

	for (j = 0, m = 1; j < nmt-m && u<t[nmt-1]; j += m)
	{
		m  = 1;
		while (t[j+m]<=t[j] && j+m < nmt-1) m++;
		m2 = 2*m;
		a1 = (t[j+m] - t[j])/m2;

		if (lchk > 0 && a1 > amax)
		{
			if (lchk == 1)
				amax = a1;
			else
			{
				u = t[j] + amid;
				if (u > u0 && u < u1)
				{
					uu = (u - u0) / urng;
					uu_list_push (lutb,&uu);
				}
			}
		}

		for (jj=1; jj<=m2; jj++)
		{
			u = t[j]+a1*jj;
			if (u > u0 && u < u1)
			{
				if (lchk == 1) amid = amid + a1;
				uu = (u - u0) / urng;
				uu_list_push (lutb,&uu);
			}
		}
	}
/*
..... qar 95226: if some a1 intervals are too big, redo the loop with a
..... maximal interval
*/
	if (lchk == 1)
	{
		nt = lutb->cur_cnt - 1;
		if (uu < ulst)
		{
			amid = amid + ulst - uu;
			nt++;
		}

		if (nt > 0) amid = amid/nt;

		if (amax > 4*amid)
		{
			amax = 2*amid;
			lutb->cur_cnt = nt0;
			u = t[0];
			lchk = 2;
			goto Redo;
		}
	}

	if (uu < ulst) uu_list_push (lutb,&ulst);

	nt = lutb->cur_cnt - 1;
	return (nt);
}

/********************************************************************
**    E_FUNCTION: ncl_crv_on_srf_parm (eptr,cvtyp,v,t,nmt,k,size)
**       Get parameters at control points for the isoparametric curve
**       on surface (NCL, MESH & RB surfaces supported).
**    PARAMETERS
**       INPUT  :
**          eptr   - pointer to surface structure data
**          crvtyp - 1 = u_curve at v=uv; 2 = v_curve at u=uv.
**          v      - v parameter for u curve (NCL surface only).
**       OUTPUT :
**          t      - Pointer to pointer of the parameters array.
**          nmt    - number of parameters in t array.
**          k      - order of the curve.
**          size   - size of allocated space for t array (0 for RB sf)
**    RETURNS      :
**          0      - if success.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int ncl_crv_on_srf_parm (eptr,cvtyp,v,t,nmt,kn,size)
struct UM_srfdatabag *eptr;
int cvtyp, *nmt, *kn, *size;
UU_REAL v, **t;
{
	int relnum, status, nt, i, j, k;
	UU_REAL *ttb;

	status = UU_SUCCESS;
	relnum = eptr->rel_num;
	*size  = 0;
	switch (relnum)
	{
/*
...RB surface, point to required array
*/
		case UM_RBSPLSRF_REL:
		{
			struct UM_rbsplsrf_rec *rsfr;
			rsfr = (struct UM_rbsplsrf_rec *) eptr;
			if (cvtyp == 1)
			{
				ttb  = rsfr->tu;
/*
..... tu and tv have 2 extra points: min and max t-values, which are not used
..... here. So use the number of knots from spline identity.
				*nmt = rsfr->no_tu;
*/
				*nmt = 2 * rsfr->ku - 1 + rsfr->nu;
				*kn  = rsfr->ku;
			}
			else
			{
				ttb  = rsfr->tv;
/*
				*nmt = rsfr->no_tv;
*/
				*nmt = 2 * rsfr->kv - 1 + rsfr->nv;
				*kn  = rsfr->kv;
			}
			*t = ttb;
			break;
		}
/*
...NCL surface,
*/
		case NCL_SURF_REL:
		{
			struct NCL_surface_rec *nsfr;
			struct NCL_panel_rec panel;
			int ordr, n;
			nsfr  = (struct NCL_surface_rec *) eptr;
/*
...u curve, get v panel & copy parameters like for RB surf
*/
			if (cvtyp == 1)
			{
				int ixv;
				ixv = (int)(v * nsfr->no_panelkey);
				if (v >= 1.) ixv--;
				panel.key = nsfr->panelkey[ixv];
				status = ncl_retrieve_data_fixed (&panel);
				if (status == UU_FAILURE) return (status);
				nt     = panel.no_param;
				n      = nt + 1;
				ttb    = (UU_REAL *) uu_toolmalloc ((3*n+2) * sizeof (UU_REAL));
				for (i=0, j=1; i<n; i++, j+=3)
					ttb[j]=ttb[j+1]=ttb[j+2]=panel.param[i];
				*nmt   = 3 * n + 2;
				*kn    = 4;
			}
/*
...v curve, check if not ruled surface & spread parameters evenly
...according to curve order (1 = ruled, 3 = regular).
*/
			else
			{
				UU_REAL dt;
				nt     = nsfr->no_panelkey;
				n      = nt + 1;
				ordr   = 3;
				if (nt == 1)
				{
					panel.key = nsfr->panelkey[0];
					status = ncl_retrieve_data_fixed (&panel);
					if (status == UU_FAILURE) return (status);
					if (panel.type == 1) ordr = 1;
				}
				dt     = 1.0/nt;
				ttb    = (UU_REAL *) uu_toolmalloc ((ordr*n+2) * sizeof (UU_REAL));
				for (i=0, j=1; i<n; i++, j+=ordr)
					for (k=0; k<ordr; k++) ttb[j+k]=i*dt;
				*nmt   = n * ordr + 2;
				*kn    = ordr + 1;
			}
			ttb[0] = 0.0;
			ttb[j] = ttb[j-1];
			*t     = ttb;
			*size  = *nmt;
			break;
		}
/*
...Mesh surface, same as NCL v curve (curve order = 3).
*/
		case NCL_MESHSURF_REL:
		{
			struct NCL_meshsf_rec *msfr;
			UU_REAL dt;
			int n;

			msfr  = (struct NCL_meshsf_rec *) eptr;
			nt    = (cvtyp == 1)? msfr->m: msfr->n;
			n     = nt + 1;
			dt    = 1.0/nt;
			ttb   = (UU_REAL *) uu_toolmalloc ((3*n+2) * sizeof (UU_REAL));
			for (i=0, j=1; i<n; i++, j+=3)
				for (k=0; k<3; k++) ttb[j+k]=i*dt;
			ttb[0] = 0.0;
			ttb[j] = ttb[j-1];
			*t     = ttb;
			*nmt   = n * 3 + 2;
			*kn    = 4;
			*size  = *nmt;
			break;
		}
		default:
			status = UU_FAILURE;
	}

	return(status);
}

/*********************************************************************
**    E_FUNCTION: S_trim_evolve (crv,tfmat,ulim,told,pptr,vptr,uptr)
**       Evolve a curve into a set of points with given chordal tolerance;
**       trim the result by the given parameter range.
**    PARAMETERS
**       INPUT  :
**          crv    - pointer to curve
**          tfmat  - ID matrix of input surface
**          ulim   - parameter limits
**          told   - tolerance (chord height)
**       OUTPUT :
**          pptr   - pointer to points list
**          vptr   - pointer to slope vectors list
**          uptr   - pointer to u parameter list
**    RETURNS      :
**          number of points stored (or 0 if failed).
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_trim_evolve (crv,tfmat,ulim,told,pptr,vptr,uptr)
struct NCL_fixed_databag *crv;
UM_transf tfmat;
UU_REAL told,ulim[];
UU_LIST *pptr,*vptr,*uptr;
{
	int i,idel,is,k,n,npt;
	UU_REAL delta,U0,U1,a,b;
	UM_coord *uvs,*pts,*vcs;
	UU_LIST plst,vlst,ulst;

	U0 = ulim[0]; U1 = ulim[1];
	delta = U1 - U0;
	if (delta < 0) return (0);

	uu_list_init (&plst, sizeof(UM_coord), 100, 100);
	uu_list_init (&vlst, sizeof(UM_vector), 100, 100);
	uu_list_init (&ulst, sizeof(UM_coord), 100, 100);

	npt = 0;
	n = ncl_evolve_curve (crv,tfmat,told,&plst,&vlst,&ulst,0);
	if (n < 1) goto Done;

	uvs = (UM_coord *) UU_LIST_ARRAY (&ulst);

	if (U0 < UM_FUZZ)
		is = 0;
	else
	{
		for (i = 0, is = -1; i < n-1 && is < 0; i++)
		{
			if (uvs[i+1][0] >= U0)
				is = i;
		}
		if (is < 0) goto Done;

		if (uvs[is+1][0] < U0 + UM_FUZZ)
		{
			idel = is+1;
		}
		else
		{
			a = (U0 - uvs[is][0])/(uvs[is+1][0] - uvs[is][0]);
			if (a > 1) a = 1;
			b = 1 - a;

			uvs[is][0] = b*uvs[is][0] + a*uvs[is+1][0];
			uvs[is][0] = U0;

			pts = (UM_coord *) UU_LIST_ARRAY (&plst);
			for (k = 0; k < 3; k++)
			{
				pts[is][k] = b*pts[is][k] + a*pts[is+1][k];
			}

			idel = is;
		}
		if (idel > 0)
		{
			uu_list_delete (&ulst,0,idel);
			uu_list_delete (&vlst,0,idel);
			uu_list_delete (&plst,0,idel);
			uvs = (UM_coord *) UU_LIST_ARRAY (&ulst);
			n = n - idel;
		}
	}

	if (U1 - U0 < UM_FUZZ)
		n = 1;
	else if (U1 < 1 - UM_FUZZ)
	{
		for (i = 0, is = -1; i < n-1 && is < 0; i++)
		{
			if (uvs[i+1][0] >= U1)
				is = i;
		}
		if (is < 0) goto Done;

		if (uvs[is+1][0] > U1 + UM_FUZZ)
		{
			a = (U1 - uvs[is][0])/(uvs[is+1][0] - uvs[is][0]);
			if (a > 1) a = 1;
			b = 1 - a;

			uvs[is+1][0] = b*uvs[is][0] + a*uvs[is+1][0];
			uvs[is+1][0] = U1;

			pts = (UM_coord *) UU_LIST_ARRAY (&plst);
			for (k = 0; k < 3; k++)
			{
				pts[is+1][k] = b*pts[is][k] + a*pts[is+1][k];
			}
		}

		n = is + 2;
	}

	npt = n;
	if (pptr != NULLST)
	{
		pts = (UM_coord *) UU_LIST_ARRAY (&plst);
		uu_list_push_multiple (pptr,n,pts);
	}
	if (vptr != NULLST)
	{
		vcs = (UM_vector *) UU_LIST_ARRAY (&vlst);
		uu_list_push_multiple (vptr,n,vcs);
	}
	if (uptr != NULLST)
	{
		uvs = (UM_coord *) UU_LIST_ARRAY (&ulst);
		uu_list_push_multiple (uptr,n,uvs);
	}

Done:
	uu_list_free (&plst);
	uu_list_free (&vlst);
	uu_list_free (&ulst);

	return (npt);
}

/*********************************************************************
**    E_FUNCTION: S_cv_revsf1 (rptr,crv,tfmat,uv,vpr,told,pptr,vptr,uptr)
**       Evolve v = const curve on a surface of revolution into set
**       of points with given chordal tolerance.
**    PARAMETERS
**       INPUT  :
**          rptr   - pointer to surface record
**          crv    - pointer to generatrix curve record
**          tfmat  - ID matrix of input surface
**          uv     - constant parameter
**          vpr    - variable parameter limits
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
static int S_cv_revsf1 (rptr,crv,tfmat,v,ulim,told,pptr,vptr,uptr)
struct NCL_revsurf_rec *rptr;
struct NCL_fixed_databag *crv;
UM_transf tfmat;
UU_REAL v,told,ulim[];
UU_LIST *pptr,*vptr,*uptr;
{
	int i,j,k,i0,memsz0;
	UU_REAL sa,ta,theta,offd;
	UM_coord ptsv,pta,uvp;
	UM_vector vca,svc,vrgt,snorm;
	UM_coord *uvs,*pts,*tmpts;
	UM_vector *tvc,*tmpvc;
	UM_transf rottf;
	UU_LOGICAL offs,tfcurve;
	UU_LIST *points,pptrloc,*derivs,vptrloc;

	memsz0 = pptr->exp_cnt;
	if (memsz0 <= 0) return (0);

	offd = rptr->offdist;
	offs = (offd != 0.);

	um_vctovc (rptr->pta,pta);
	um_unitvc (rptr->vca,vca);
	sa = rptr->sa / UM_RADIAN;
	ta = rptr->ta / UM_RADIAN;

	uvp[1] = uvp[2] = v;
	i = 0;
	i0 = pptr->cur_cnt;

	if (offs)
	{
		uu_list_init (&pptrloc, sizeof(UM_coord), memsz0, memsz0);
		points = &pptrloc;
		uu_list_init (&vptrloc, sizeof(UM_vector), memsz0, memsz0);
		derivs = &vptrloc;
	}
	else
	{
		points = pptr;
		derivs = vptr;
	}

	if (ulim[0] > UM_FUZZ || ulim[1] < 1. - UM_FUZZ)
		i = S_trim_evolve (crv,tfmat,ulim,told,points,derivs,uptr);
	else
		i = ncl_evolve_curve (crv, tfmat, told, points, derivs, uptr, 0);
	if (i <= 0) goto Done1;

	tfcurve = ncl_itsa_compcrv(crv);

	theta = sa + v*(ta - sa);
	um_rotlntf (pta, vca, theta, rottf);

	if (offs)
	{
		tmpts = (UM_coord *) UU_LIST_ARRAY (points);
		tmpvc = (UM_vector *) UU_LIST_ARRAY (derivs);
		if (tfcurve)
		{
			for (j=0; j<i; j++)
			{
				um_cctmtf(tmpts[j],tfmat,tmpts[j]);
				um_vctmtf(tmpvc[j],tfmat,tmpvc[j]);
			}
		}
	}
	else
	{
		pts = (UM_coord *) UU_LIST_ARRAY (pptr); pts += i0;
		if (vptr != UU_NULL)
		{
			tvc = (UM_vector *) UU_LIST_ARRAY (vptr); tvc += i0;
		}

		if (tfcurve)
		{
			for (j=0; j<i; j++)
			{
				um_cctmtf(pts[j],tfmat,pts[j]);
				if (vptr != UU_NULL)
					um_vctmtf(tvc[j],tfmat,tvc[j]);
			}
		}
	}

	if (uptr != UU_NULL)
	{
		uvs = (UM_coord *) UU_LIST_ARRAY (uptr); uvs += i0;
	}

	for (j=0; j<i; j++)
	{
		if (offs)
		{
			um_vcmnvc (tmpts[j], pta, svc);
			um_cross (vca,svc,vrgt);
/*
..... If the surface vector (svc) is collinear with the axis of
..... revolution, use the axis as the surface normal; set the direction
..... by the normal at a near point. QAR 92158
*/
			if (um_mag(vrgt) < 1.e-6)
			{
				k = (j == 0)? 1: j-1;
				um_vcmnvc (tmpts[k], pta, svc);
				um_cross (vca,svc,vrgt);
				if (um_mag(vrgt) < 1.e-6)
				{
					i = 0;
					goto Done1;
				}
				if (rptr->rev_normal == 1)
					um_cross (vrgt,tmpvc[k],snorm);
				else
					um_cross (tmpvc[k],vrgt,snorm);
				if (um_dot (snorm,vca) < 0.)
					um_vctmsc (vca,-1.,snorm);
				else
					um_vctovc (vca,snorm);
			}
			else
			{
				if (rptr->rev_normal == 1)
					um_cross (vrgt,tmpvc[j],snorm);
				else
					um_cross (tmpvc[j],vrgt,snorm);
				um_unitvc (snorm,snorm);
			}
			um_vctmsc (snorm,offd,snorm);
			um_vcplvc (tmpts[j],snorm,ptsv);
			uu_list_push (pptr,ptsv);
			pts = (UM_coord *) UU_LIST_ARRAY (pptr); pts += i0;
			if (vptr != UU_NULL)
			{
				um_vctovc (tmpvc[j],ptsv);
				uu_list_push (vptr,ptsv);
				tvc = (UM_coord *) UU_LIST_ARRAY (vptr); tvc += i0;
			}
		}
		um_cctmtf(pts[j],rottf,pts[j]);
		if (vptr != UU_NULL)
			um_vctmtf(tvc[j],rottf,tvc[j]);
		if (uptr != UU_NULL)
			uvs[j][1] = v;
	}

Done1:
	if (offs)
	{
 		uu_list_free (&pptrloc);
 		uu_list_free (&vptrloc);
	}

	return (i);
}

/*********************************************************************
**    E_FUNCTION: S_cv_revsf2 (rptr,crv,tfmat,uv,vpr,told,pptr,vptr,uptr)
**       Evolve u = const curve on a surface of revolution into set
**       of points with given chordal tolerance.
**    PARAMETERS
**       INPUT  :
**          rptr   - pointer to surface record
**          crv    - pointer to generatrix curve record
**          tfmat  - ID matrix of input surface
**          uv     - constant parameter
**          vpr    - variable parameter limits
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
static int S_cv_revsf2 (rptr,crv,tfmat,u,vlim,told,pptr,vptr,uptr)
struct NCL_revsurf_rec *rptr;
struct NCL_fixed_databag *crv;
UM_transf tfmat;
UU_REAL u,told,vlim[];
UU_LIST *pptr,*vptr,*uptr;
{
	int i,j;
	int status;
	struct UM_evcrvout evout,evout1;
	UU_REAL dv,sa,ta,theta,offd,del,delta,rad,u1,V0,V1;
	UM_coord pta,spt,uvp;
	UM_vector vca,svc,vrgt,snorm;
	UM_transf rottf;
	UU_LOGICAL offs;


	offd = rptr->offdist;
	offs = (offd != 0.);

	um_vctovc (rptr->pta,pta);
	um_unitvc (rptr->vca,vca);
	sa = rptr->sa / UM_RADIAN;
	ta = rptr->ta / UM_RADIAN;

	uvp[0] = uvp[2] = u;
	i = 0;

	V0 = vlim[0]; V1 = vlim[1];

	delta = ta - sa;
	sa = sa + V0*delta;
	delta = delta*(V1-V0);
	ta = sa + delta;

	del = fabs(delta);
	if (del <= UM_FUZZ) return (0);

	uc_init_evcrvout (crv,&evout);
	status = uc_evcrv(UM_FRSTDERIV, u, crv, tfmat, &evout);
	if (status != UU_SUCCESS) return (0);

	um_vcmnvc (evout.cp, pta, svc);
	um_cross (vca,svc,vrgt);
	rad = um_mag (vrgt);
	if (offs)
	{
/*
..... special case - point on the axis - see QAR 92158
*/
		if (rad < 1.e-6)
		{
			uc_init_evcrvout (crv,&evout1);
			u1 = (u < 0.5)? u + 0.0005: u - 0.0005;
			status = uc_evcrv(UM_FRSTDERIV, u1, crv, tfmat, &evout1);
			if (status != UU_SUCCESS) return (0);
			um_vcmnvc (evout1.cp,pta,svc);
			um_cross (vca,svc,vrgt);
			if (um_mag (vrgt) < 1.e-6) return (0);
			if (rptr->rev_normal == 1)
				um_cross (vrgt,evout1.dcdu,snorm);
			else
				um_cross (evout1.dcdu,vrgt,snorm);
			if (um_dot (snorm,vca) < 0.)
				um_vctmsc (vca,-1.,snorm);
			else
				um_vctovc (vca,snorm);
		}
		else
		{
			if (rptr->rev_normal == 1)
				um_cross (vrgt,evout.dcdu,snorm);
			else
				um_cross (evout.dcdu,vrgt,snorm);
			um_unitvc (snorm,snorm);
		}
		um_vctmsc (snorm,offd,snorm);
		um_vcplvc (evout.cp,snorm,evout.cp);
	}

	if (rad < 1.e-6)
	{
		um_vctovc(evout.cp,spt);
		uu_list_push (pptr,spt);
		if (vptr != UU_NULL)
		{
			um_nullvc (vrgt);
			uu_list_push (vptr,vrgt);
		}
		if (uptr != UU_NULL)
		{
			uvp[1] = V0;
			uu_list_push (uptr,uvp);
		}
		return (1);
	}
	else if (rad < told)
		i = (del < UM_PI)? 2: 3;
	else
	{
		theta = 2. * acos (1. - 0.9 * told / rad);
		if (rad*theta < UM_FUZZ) theta = UM_FUZZ/rad;
		i = (int) ceil (del / theta);
	}

	um_rotlntf (pta, vca, sa, rottf);
	um_cctmtf(evout.cp,rottf,spt);
	uu_list_push (pptr,spt);
	if (vptr != UU_NULL)
	{
		um_vctmtf(vrgt,rottf,svc);
		uu_list_push (vptr,svc);
	}
	if (uptr != UU_NULL)
	{
		uvp[1] = V0;
		dv = (V1 - V0)/i;
		uu_list_push (uptr,uvp);
	}

	delta = delta/i;
	um_rotlntf (pta, vca, delta, rottf);
	i++;
	for (j=1; j<i; j++)
	{
		um_cctmtf(spt,rottf,spt);
		uu_list_push (pptr,spt);
		if (vptr != UU_NULL)
		{
			um_vctmtf(svc,rottf,svc);
			uu_list_push (vptr,svc);
		}
		if (uptr != UU_NULL)
		{
			uvp[1] = V0 + j*dv;
			uu_list_push (uptr,uvp);
		}
	}

	return (i);
}

/*********************************************************************
**    E_FUNCTION: ncl_evolve_crv_on_revsf
**                (eptr, tfmat, uv, vpr, cvtyp, told, pptr, vptr,  uptr)
**       Evolve isoparametric curve on a surface of revolution into set
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
int ncl_evolve_crv_on_revsf (eptr,tfmat,uv,vpr,cvtyp,told,pptr,vptr,uptr)
struct UM_srfdatabag *eptr;
UM_transf tfmat;
UU_REAL uv, told, vpr[];
int cvtyp;
UU_LIST *pptr, *vptr, *uptr;
{
	int n,status;
	struct NCL_revsurf_rec rptr;
	struct NCL_fixed_databag crv;

	rptr.key = eptr->key;
	status = ncl_retrieve_data_fixed (&rptr);
	if (status != UU_SUCCESS) return (0);

	crv.key = rptr.cvkey;
	status = ncl_retrieve_data_fixed (&crv);
	if (status == UU_SUCCESS)
		status = ncl_transform_revsf (&rptr,tfmat,-1);
	if (status != UU_SUCCESS) return (0);

	if (cvtyp == 1)
		n = S_cv_revsf1 (&rptr,&crv,tfmat,uv,vpr,told,pptr,vptr,uptr);
	else
		n = S_cv_revsf2 (&rptr,&crv,tfmat,uv,vpr,told,pptr,vptr,uptr);

	return (n);
}

/*********************************************************************
**    E_FUNCTION: ncl_evolve_crv_on_srf_own
**                (eptr, tfmat, uv, vpr, cvtyp, told, pptr, vptr,  uptr)
**       Evolve isoparametric curve on surface (RB or NCL) into set
**       of points with given chordal tolerance.
**       NOTE: routine has same logic as ncl_evolve_curve.
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
int ncl_evolve_crv_on_srf_own (eptr,tfmat,uv,vpr,cvtyp,told,pptr,vptr,uptr)
struct UM_srfdatabag *eptr;
UM_transf tfmat;
UU_REAL uv, told, *vpr;
int cvtyp;
UU_LIST *pptr, *vptr, *uptr;
{
	struct UM_evcrvout evout;
	int i, ip, j, k, size, nmt, itsz;
	int istat, nt, noinc, last, lstj, ntry;
	UU_REAL a1, *t, du1, uprv, u, r, du, h1, h0, delt, r1, ulst;
	UU_REAL *utb, u1, dcumsv, rr, ulim, h, uu, rdus;
	UU_LIST lpts;
	UM_coord ptsv, *pts, uvp;
	UM_vector d1sv;
	int status;

	if (eptr->rel_num == NCL_REVSURF_REL)
	{
		i = ncl_evolve_crv_on_revsf (eptr,tfmat,uv,vpr,cvtyp,told,pptr,vptr,uptr);
		return (i);
	}

	uvp[2-cvtyp] = uv;
	uvp[2] = uv;
/*
...Get parametrization knots for specified curve orientation
*/
	size = 0;
	status = ncl_crv_on_srf_parm (eptr,cvtyp,uv,&t,&nmt,&k,&size);
	if (status != UU_SUCCESS) return (0);

	nt  = nmt;
/*
..... Increased memory allocation by 1 to allow for addition
..... of final u parameter if necessary. IJD 9-AUG-2000
*/
	itsz  = 2*nt+1;
	utb   = (UU_REAL *) uu_toolmalloc (itsz * sizeof (UU_REAL));

	uu_list_init (&lpts, sizeof(UM_coord), 3*nt, 3*nt);

	ulst  = vpr[1];
/*
...Get first point on curve
*/
	u    = uvp[cvtyp-1] = vpr[0];
	um_ev9_crv (UM_ALL, uv, u, cvtyp, eptr, tfmat, &evout);
	um_vctovc (evout.cp,ptsv);
	um_vctovc (evout.dcdu,d1sv);
	uu_list_push (pptr,ptsv);
	if (vptr != UU_NULL) uu_list_push (vptr,d1sv);
	if (uptr != UU_NULL) uu_list_push (uptr,uvp);
	i   = 1;
	dcumsv = um_mag (evout.dcdu);
	r      = rdus = (evout.curv > UM_DFUZZ)? 1./evout.curv: 1.e16;
/*
...Set expected u increment for desired tolerance
*/
	delt = (told < r-UM_FUZZ)? 2*sqrt(told*(2*r-told)): 2*r;
	du   = .92 * delt / dcumsv;
/*
...Create u table at control points & betwen them
*/
	nt   = ncl_make_parm_tab (utb,nmt,k);
	if (utb[nt] < ulst)
	{
		if (nt <= itsz-2) nt++;
		utb[nt] = ulst;
	}

	u1      = uprv = u;
	ip      = last = noinc = 0;
	h0      = 0.;
	for (j=0; j<nt && utb[j]<=u; j++);
/*
...Check all utb values if they are necessary to keep tolerance
...or we need even more points
*/
	lstj = j;
	ntry = 0;
	while (j<=nt && last == 0)
	{
		ulim = utb[j];
Nexti:
		if (last == 1) goto Outpt;
/*
.....Attempt only 1000 iterations between points
.....To avoid infinite loop with bad UV-curve
*/
		if (j == lstj)
		{
			if (ntry == 1000)
			{
				i = 0;
				goto done;
			}
			else
				ntry++;
		}
		else
		{
			lstj = j;
			ntry = 0;
		}

		uu  = uprv + du;
		if (uu > ulst-UM_DFUZZ) { uu = ulst; last = 1; }
/*
...When next point is farther than control point check tolerance
...at this point and save it for future checks when
...tolerance < 80% of required
*/
			if (uu > ulim)
			{
				last = 0;
				uu   = ulim;
				du1  = uu - u1;
				if (uu > ulst-UM_DFUZZ) { uu = ulst; last = 1; }
				um_ev9_crv (UM_ALL, uv, uu, cvtyp, eptr, tfmat, &evout);
				r1   = (evout.curv > UM_DFUZZ)? 1./evout.curv: 1.e16;
				rr   = sqrt(rdus*r1);
				rdus = r1;
				if (ip == 1) ncl_h (ptsv,evout.cp,rr,&h0);
				if (ip > 0) istat = ncl_h (pts[ip-1],evout.cp,rr,&h);
				else istat  = ncl_h (ptsv,evout.cp,rr,&h);
				h1   = ncl_chordh_cnpts (ptsv,evout.cp,pts,ip);
				if (h1 > h) h = h1;
				if (h0 > h) h = h0;
				if (istat == 1)
				{
					last = 0;
					noinc = 1;
					du1 = .8 * du1;
					du  = u1 - uprv + du1;
					goto Nexti;
				}
				if (h < .8*told)
				{
					if (last == 1) goto Outpt;
					u1   = uu;
/*					um_vctovc (evout.cp,pts[ip++]);  */
					ip   = ncl_list_push (&lpts,evout.cp,&pts);
					um_vctovc (evout.dcdu,d1sv);
					if (ip > nmt*3-1) goto Outpt;
					goto Nextj;
				}
				else
				{
					if (h < told || ip > 0) goto Outpt;
					last = 0;
					noinc = 1;
					du1 = .8 * du1;
					du  = u1 - uprv + du1;
					goto Nexti;
				}
			}
/*
...Between control points, make sure that new point is still
...in tolerance and output it if above 80% of required tol.
*/
		um_ev9_crv (UM_ALL, uv, uu, cvtyp, eptr, tfmat, &evout);
		rdus = (evout.curv > UM_DFUZZ)? 1./evout.curv: 1.e16;
		rr   = (ip == 0)? sqrt(rdus*r): sqrt(rdus*r1);
			if (ip > 0)
			{
				if (ip == 1) ncl_h (ptsv,evout.cp,rr,&h0);
				istat = ncl_h (pts[ip-1],evout.cp,rr,&h);
			}
			else istat  = ncl_h (ptsv,evout.cp,rr,&h);
			if (h0 > h) h = h0;
			h1 = ncl_chordh_cnpts (ptsv,evout.cp,pts,ip);
			if (h1 > h) h = h1;
			if (h > told && istat == 1)
			{
				du    = du1 = .8 * du1;
				last  = 0;
				noinc = 1;
				goto Nexti;
			}
			if (last == 1) goto Outpt;
			last  = 0;
/*
...below 80% of tolerance, increase distance 15% or extrapolate
...from the last control point
*/
			if (h < .8*told)
			{
				if (noinc == 1) goto Outpt;
				if (ip > 0)
				{
					delt = (told < rdus-UM_FUZZ)?
							  2*sqrt(told*(2*rdus-told)): 2*rdus;
					a1   = u1 - uprv + .92 * delt / um_mag(evout.dcdu);
					du   = du1 = (a1 <= du)? 1.15*du: a1;
				}
				else
					du   = du1 = 1.15 * du;
/*
...since point is calculated store it to check later
				 um_vctovc (evout.cp,pts[ip++]);
*/
				u1   = uu;
				ip   = ncl_list_push (&lpts,evout.cp,&pts);
				um_vctovc (evout.dcdu,d1sv);
				r1   = rdus;
				if (ip > nmt*3-1) goto Outpt;
				goto Nexti;
			}
			else
			{
				if (h < told || ip > 0) goto Outpt;
				du    = du1 = .8 * du;
				noinc = 1;
				goto Nexti;
			}
/*
...Before point is output make sure if accumulted points
...on long span are still in tolerace
*/
Outpt:
		if (ip > 0)
		{
			h = ncl_chordh_cnpts (ptsv,evout.cp,pts,ip);
			if (ip == 1 && h0 > h) h = h0;
			if (h > told)
			{
				um_vctovc (pts[--ip],evout.cp);
				um_vctovc (d1sv,evout.dcdu);
				rdus = r1;
				uu   = u1;
				last = 0;
			}
			ip = lpts.cur_cnt = 0;
		}
/*
...Output point
*/
		um_vctovc (evout.cp,ptsv);
		uu_list_push (pptr,ptsv);
		uprv = u1 = uvp[cvtyp-1] = uu;
		if (vptr != UU_NULL) uu_list_push (vptr,evout.dcdu);
		if (uptr != UU_NULL) uu_list_push (uptr,uvp);
		i++;
		h0   = 0.;
		if (last == 1) break;
/*
...estimate next step, fix limit if necessary &
...go for next point
*/
		dcumsv = um_mag (evout.dcdu);
		r    = rdus;
		delt = (told < r-UM_FUZZ)? 2*sqrt(told*(2*r-told)): 2*r;
		du   = du1 = .92 * delt / dcumsv;
		noinc = 0;
		if (uprv < utb[j-1]) ulim = utb[--j];
		if (uprv < ulim - UM_DFUZZ) goto Nexti;

Nextj:
		j++;
	}
/*
...Output last point if curve is extended
*/
	if (uprv < ulst - .1*UM_FUZZ)
	{
		u = uvp[cvtyp-1] = ulst;
		um_ev9_crv (UM_FRSTDERIV, uv, u, cvtyp, eptr, tfmat, &evout);
		uu_list_push (pptr,evout.cp);
		if (vptr != UU_NULL) uu_list_push (vptr,evout.dcdu);
		if (uptr != UU_NULL) uu_list_push (uptr,uvp);
		i++;
	}
done:;
	if (size > 0) uu_toolfree (t);
	uu_toolfree (utb);
	uu_list_free (&lpts);
	return (i);
}

/********************************************************************
**    E_FUNCTION: ncl_make_parm_tab (utb,nmt,k)
**       Create parameter array for RB spline at knots & at between
**       them (in 1/2) using original parameter array.
**    PARAMETERS
**       INPUT  :
**          nmt    - number of values in t table
**          k      - curve order
**       OUTPUT :
**          utb    - array of parameters
**    RETURNS      :
**          number of parameters in utb
**    SIDE EFFECTS : none
**    WARNINGS     : none
********************************************************************/
static int ncl_make_parm_tab (utb,nmt,k)
UU_REAL *utb;
int nmt, k;
{
	int m, j, jj, nt;
	UU_REAL a1;

	m = k - 1;
/*
...Create u table at control points & between them
*/
	nt = 0;
	jj = nmt - m - 2;
	a1 = .5 / jj;
	for (j = 0; j < 2*jj; j++)
	{
		 utb[nt++] = j * a1;
	}
	nt--;
	return (nt);
}

/********************************************************************
**    E_FUNCTION: ncl_evolve_bn_crv_on_srf_own (eptr,tfmat,cptr,bplm,pptr,
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
int ncl_evolve_bn_crv_on_srf_own (eptr,tfmat,cptr,bplm,told,pptr,vptr,uvptr)
struct UM_srfdatabag *eptr;
struct UM_crvdatabag *cptr;
UU_REAL *bplm, told;
UM_transf tfmat;
UU_LIST *pptr, *vptr, *uvptr;
{
	struct UM_evcrvout evcv, uvcv, uvsav;
	struct UM_rbsplcrv_rec *rcrv;
	int i, ip, j, k, ku, kv, size, nmt, nmu, nmv, nu, nv,
	    istat, nt, noinc, first, last, lims, cflg, uvf, ntry, lstj;
	UU_REAL a1, *t, *ut, *vt, r, h0, h1, delt, r1,u0,v0,um,vm,
	        *ttb, dcumsv, rr, t0, t1, tn, dt, h, dt1, rdus, dt2,
	        *utb, *vtb, tt, tt1, tprv, tlim, tlst, trng, cco;
	UU_LIST lpts;
	UM_coord ptsv,uvp,*pts;
	UM_vector d1sv,vv0,vv1;
	struct UM_evsrfout evout;
	UM_int2 idx = 169;
	UM_real8 ver;
	int IMAX;

	IMAX = 1600;
	if (told < UM_FUZZ)
		IMAX *= 2; 

	size = cflg = uvf = 0;
	getsc(&idx,&ver);
	istat = ncl_crv_on_srf_parm (eptr,1,(UU_REAL).0,&ut,&nmu,&ku,&size);
	if (istat == UU_SUCCESS)
		istat = ncl_crv_on_srf_parm (eptr,2,(UU_REAL).0,&vt,&nmv,&kv,&size);
	if (istat != UU_SUCCESS) return (0);

	if (cptr->rel_num != UM_RBSPLCRV_REL)
	{
		um_alloc_eq_curve (cptr, &rcrv);
		um_rbcrv_frmnclcrv (cptr, rcrv);
		cflg = 1;
	}
	else
		rcrv = (struct UM_rbsplcrv_rec *) cptr;

	t     = rcrv->t;
	uc_init_evcrvout (rcrv, &evcv);
	nmt   = rcrv->no_t;
	k     = rcrv->k;
	t0    = rcrv->t0;
	t1    = rcrv->t1;
	nt    = nmt - k;
	tn    = t[nmt-1];

	ttb   = (UU_REAL *) uu_toolmalloc (2*nt * sizeof (UU_REAL));
	utb   = (UU_REAL *) uu_toolmalloc (2*(nmu-ku) * sizeof (UU_REAL));
	vtb   = (UU_REAL *) uu_toolmalloc (2*(nmv-kv) * sizeof (UU_REAL));

	uu_list_init (&lpts, sizeof(UM_coord), 100, 100);

	trng  = t1 - t0;
	tlst  = (t1 > tn)? (tn-t0)/trng: 1.0;
/*
...Get first point on curve
*/
	tt    = .0;
	um_ev7_crv_on_surf (UM_CURVATURE,eptr,cptr,bplm,tt,tfmat,&evcv,&uvcv);
	um_vctovc (evcv.cp,ptsv);
	um_vctovc (evcv.dcdu,d1sv);
	um_vctovc (uvcv.cp,uvsav.cp);
	um_vctovc (uvcv.dcdu,uvsav.dcdu);
	if (pptr != UU_NULL) uu_list_push (pptr,ptsv);
	if (vptr != UU_NULL) uu_list_push (vptr,d1sv);
	if (uvptr != UU_NULL)
	{
		um_vctovc_2d (uvcv.cp,uvp);
		uvp[2] = tt;
		uu_list_push (uvptr,uvp);

		uc_init_evsrfout (eptr,&evout);
		u0 = um = uvcv.cp[0]; v0 = vm = uvcv.cp[1];
	}
	i      = 1;
	dcumsv = um_mag (evcv.dcdu);
	r      = rdus = (evcv.curv > UM_DFUZZ)? 1./evcv.curv: 1.e16;
/*
...Set expected u increment for desired tolerance
*/
	first = (t[0] > t0+UM_FUZZ)? 0: 1;
	if (first == 1)
	{
		delt = (told < r-UM_FUZZ)? 2*sqrt(told*(2*r-told)): 2*r;
		dt   = .92 * delt / dcumsv;
	}
	else
		dt   = (t1 > t[0])? -t0/trng: 1.0;
/*
...Create t, u & v table at knot points & between them
*/
	nt   = ncl_make_parm_tab (ttb,nmt,k);
	if (ttb[nt] < tlst) ttb[++nt] = tlst;

	nu   = ncl_make_parm_tab (utb,nmu,ku);
	if (utb[nu] < 1) utb[++nu] = 1;

	nv   = ncl_make_parm_tab (vtb,nmv,kv);
	if (vtb[nv] < 1) vtb[++nv] = 1;

	tt1    = tprv   = .0;
	ip     = last = noinc = lims = 0;
	j      = lstj   = 1;
	h0     = 0.;
	pts = UU_NULL;
/*
...Loop thru all ttb values
*/
	ntry = 0;
	while (j <= nt && last == 0)
	{
		tlim = ttb[j];
Nexti:
		if (last == 1) goto Outpt;
/*
.....Attempt only 1000 iterations between points
.....To avoid infinite loop with bad UV-curve
.....
..... QAR 92358 - increased to 1600.
*/
		if (j == lstj)
		{
			if (ntry == 1600)
			{
				i = 0;
				goto done;
			}
			else
				ntry++;
		}
		else
		{
			lstj = j;
			ntry = 0;
		}
		tt = tprv + dt;
		if (fabs(tt-tlst) < UM_DFUZZ) last = 1;
		if (first == 1)
		{
			lims = 0;
/*
...check for passing t knot
*/
			if (tt > tlim) { tt = tlim; lims = 1; }
/*
...check for passing u or v knot
*/
			dt1 = tt - tt1;
			um_ev7_uvcrv (UM_FRSTDERIV, cptr, bplm, tt, &uvcv);
			ncl_ver_uvsf (nu,utb,nv,vtb,&uvsav,dt1,&dt2);
			if (dt2 < dt1)
			{
				tt   = tt - dt1 + dt2;
				dt1  = dt2;
				lims = 2;
			}
/*
...When next point is farther than any knot point check tolerance
...at this point and save it for future checks when
...tolerance < 80% of required
*/
			if (lims)
			{
				last = 0;
				if (fabs(tt-tlst) < UM_FUZZ) last = 1;
				um_ev7_crv_on_surf (UM_CURVATURE,eptr,cptr,bplm,tt,tfmat,&evcv,
					&uvcv);
				r1   = (evcv.curv > UM_DFUZZ)? 1./evcv.curv: 1.e16;
/*				//rr   = sqrt(rdus*r1);*/
				if (r1 > rdus)
					r1 = rdus;
				rr = r1;
				rdus = r1;
				if (ip == 1) ncl_h (ptsv,evcv.cp,rr,&h0);
				if (ip > 0) istat = ncl_h (pts[ip-1],evcv.cp,rr,&h);
				else istat  = ncl_h (ptsv,evcv.cp,rr,&h);
				h1   = ncl_chordh_cnpts (ptsv,evcv.cp,pts,ip);
				if (h1 > h) h = h1;
				if (h0 > h) h = h0;
				if (istat == 1)
				{
					dt1   = .8 * dt1;
					dt    = tt1 - tprv + dt1;
					last  = 0;
					noinc = 1;
					goto Nexti;
				}
				if (h < .8*told)
				{
					if (last == 1) goto Outpt;

					if (uvptr != UU_NULL && ip > 1)
/*
..... eduard 10/09/2000. Also check the surf. point evaluated at
..... the middle of current chain. The reason: if a surf. is curved,
..... a straight xyz segment may not correspond to a straight uv-segment
*/
					{
						um = (u0 + uvcv.cp[0])/2.; vm = (v0 + uvcv.cp[1])/2.;
						uc_evsrf (UM_POINT, um, vm, eptr, UM_DEFAULT_TF, &evout);
						um_cctmtf(evout.sp, tfmat, evout.sp);
						h = ncl_chordh_cnpts (ptsv,evcv.cp,&evout.sp,1);
						if (h >= .8*told)
						{
							uvf = 1;
							goto Outpt;
						}
					}
					tt1  = tt;
					if (ip == 0 || um_dcccc (evcv.cp,pts[ip-1]) > 2.*told)
					{
						ip   = ncl_list_push (&lpts,evcv.cp,&pts);
						um_vctovc (evcv.dcdu,d1sv);
						um_vctovc (uvcv.cp,uvsav.cp);
						um_vctovc (uvcv.dcdu,uvsav.dcdu);
					}
					goto Nextj;
				}
				else
				{
					um_unitvc (d1sv,vv0);
					um_unitvc (evcv.dcdu,vv1);
					cco = UM_DOT(vv0,vv1);
					if (cco > 0.01 && (h < told || ip > 0)) goto Outpt;

					last  = 0;
					if (ntry >= 1)
						noinc = 1;
					else
						noinc = 0;
					dt1   = .8 * dt1;
					dt    = tt1 - tprv + dt1;
					goto Nexti;
				}
			}
		}
/*
...Between knots, make sure that new point is still
...in tolerance and output it if above 80% of required tol.
*/
		um_ev7_crv_on_surf (UM_CURVATURE,eptr,cptr,bplm,tt,tfmat,&evcv,&uvcv);
		rdus = (evcv.curv > UM_DFUZZ)? 1./evcv.curv: 1.e16;
		rr   = (ip == 0)? sqrt(rdus*r): sqrt(rdus*r1);
		if (first == 1)
		{
			if (ip > 0)
			{
				if (ip == 1) ncl_h (ptsv,evcv.cp,rr,&h0);
				istat = ncl_h (pts[ip-1],evcv.cp,rr,&h);
			}
			else istat  = ncl_h (ptsv,evcv.cp,rr,&h);
			if (h0 > h) h = h0;
			h1 = ncl_chordh_cnpts (ptsv,evcv.cp,pts,ip);
			if (h1 > h) h = h1;
			if ((h < told || dt < UM_DFUZZ) && noinc == 1) goto Outpt;
			if (h > told && istat == 1)
			{
				dt1   = .8 * dt1;
				dt    = tt1 - tprv + dt1;
				last  = 0;
				noinc = 1;
				goto Nexti;
			}
			if (last == 1) goto Outpt;
			last  = 0;
/*
...below 80% of tolerance, increase distance 15% or extrapolate
...from the last control point
*/
			if (h < .8*told)
			{
				if (ip > 0 && (ntry <= 250 || ver < 9.25))
				{
					delt = (told < rdus-UM_FUZZ)?
							  2*sqrt(told*(2*rdus-told)): 2*rdus;
					a1   = tt1 - tprv + .92 * delt / um_mag(evcv.dcdu);
					dt = dt1 = (a1 <= dt)? 1.15*dt: a1;
				}
				else
					dt   = dt1 = 1.15 * dt;
/*
...since point is calculated store it to check later
*/
				tt1  = tt;
				if (ip == 0 || um_dcccc (evcv.cp,pts[ip-1]) > 2.*told)
				{
					ip   = ncl_list_push (&lpts,evcv.cp,&pts);
					um_vctovc (evcv.dcdu,d1sv);
					um_vctovc (uvcv.cp,uvsav.cp);
					um_vctovc (uvcv.dcdu,uvsav.dcdu);
				}
				r1   = rdus;
				goto Nexti;
			}
			else
			{
				um_unitvc (d1sv,vv0);
				um_unitvc (evcv.dcdu,vv1);
				cco = UM_DOT(vv0,vv1);
				if (cco > 0.01 && (h < told || ip > 0)) goto Outpt;

				dt1   = .8 * dt1;
				dt    = tt1 - tprv + dt1;
				noinc = 1;
				goto Nexti;
			}
		}
/*
...Before point is output make sure if accumulted points
...on long span are still in tolerace
*/
Outpt:
		first = 1;
		if (ip > 0)
		{
			if (uvf == 0)
			{
				h = ncl_chordh_cnpts (ptsv,evcv.cp,pts,ip);
				if (ip == 1 && h0 > h) h = h0;
			}
			if (h > told)
			{
				um_vctovc (pts[--ip],evcv.cp);
				um_vctovc (d1sv,evcv.dcdu);
				um_vctovc (uvsav.cp,uvcv.cp);
				um_vctovc (uvsav.dcdu,uvcv.dcdu);
				rdus = r1;
				tt   = tt1;
				last = 0;
			}
			ip = lpts.cur_cnt = 0;
		}
/*
...Output point
*/
		um_vctovc (evcv.cp,ptsv);
		if (pptr != UU_NULL) uu_list_push (pptr,ptsv);
		if (vptr != UU_NULL) uu_list_push (vptr,evcv.dcdu);
		if (uvptr != UU_NULL)
		{
			um_vctovc_2d (uvcv.cp,uvp);
			uvp[2] = tt;
			uu_list_push (uvptr,uvp);
			u0 = uvcv.cp[0]; v0 = uvcv.cp[1];
			uvf = 0;
		}
		i++;
		if (last == 1) break;
		tprv = tt1 = tt;
		h0   = 0.;
/*
...estimate next step, fix limit if necessary &
...go for next point
*/
		dcumsv = um_mag (evcv.dcdu);
		r    = rdus;
		delt = (told < r-UM_FUZZ)? 2*sqrt(told*(2*r-told)): 2*r;
		dt   = dt1 = .92 * delt / dcumsv;
		noinc = 0;
		if (tprv < ttb[j-1]) tlim = ttb[--j];
		if (tprv < tlim - UM_DFUZZ) goto Nexti;

Nextj:
		if (lims == 1) j++;
	}
/*
...End of ... deallocate spaces
*/
done:;
	if (size != 0) { uu_toolfree (ut); uu_toolfree (vt); }
	if (cflg != 0) uu_toolfree (rcrv);
	uu_toolfree (ttb);
	uu_toolfree (utb);
	uu_toolfree (vtb);
	uu_list_free (&lpts);
	return(i);
}

/********************************************************************
**    E_FUNCTION: ncl_ver_uvsf (nu,utb,nv,vtb,uvsav,din,dt)
**       Check curve on surf parameter t if the evaluated u,v point
**       (surf parameters) is crossing any (u or v) knot value of
**       the surface parametrization when moving along the curve
**       from previous point. Curve on surf is UV curve.
**    PARAMETERS
**       INPUT  :
**          nu     - number of u knots (* 2)
**          utb    - u knots array
**          nv     - number of v knots (* 2)
**          vtb    - v knots array
**          uvsav  - evalution record at previous u,v point of CV
**          din    - input parameter increment (not used)
**       OUTPUT :
**          dt     - allowed increment for curve parameter t.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static void ncl_ver_uvsf (nu, utb, nv, vtb, uvsav, din, dt)
int nu, nv;
UU_REAL *utb, *vtb, din, *dt;
struct UM_evcrvout *uvsav;
{
	int i, ix;
	UU_REAL du, dv, us, vs, c1, c2, r, dd;

	c1 = c2 = 1.0e10;

	du = uvsav->dcdu[0];
	dv = uvsav->dcdu[1];
	us = uvsav->cp[0];
	vs = uvsav->cp[1];
/*
...find u knot in direction of du
*/
	if (fabs(du) > UM_DFUZZ)
	{
		dd  = .01*(utb[1] - utb[0]);
		dd  = (du > 0.)? dd: -dd;
		for (i=0; i<nu; i++)
			{ if (us+dd < utb[i]) break; }

		ix  = (du > 0.)? i: i - 1;
		if (ix > 0 && ix < nu)
		{
			r   = (utb[ix] - us)/du;
			c1 = r;
		}
	}
/*
...find v knot in direction of dv
*/
	if (fabs(dv) > UM_DFUZZ)
	{
		dd  = .01*(vtb[1] - vtb[0]);
		dd  = (dv > 0.)? dd: -dd;
		for (i=0; i<nv; i++)
			{ if (vs+dd < vtb[i]) break; }

		ix  = (dv > 0.)? i: i - 1;
		if (ix > 0 && ix < nv)
		{
			r   = (vtb[ix] - vs)/dv;
			c2 = r;
		}
	}
/*
...select the closest point in u,v space
*/
	c1  = (c1 < c2)? c1: c2;
	r   = c1;
	*dt = (r < din)? r: din;

	return;
}

/********************************************************************
**    E_FUNCTION: ncl_list_push (list,gent,ptr)
**       Push (UM_coord) type on data list, update pointer to array.
**    PARAMETERS
**       INPUT  :
**          list   - pointer to list
**          gent   - pointer to entity to push on list
**       OUTPUT :
**          ptr    - pointer to the array
**    RETURNS      : number of items in list
**    SIDE EFFECTS : none
*********************************************************************/
int ncl_list_push (list,gent,ptr)
UU_LIST *list;
UM_coord gent;
char **ptr;
{
	uu_list_push (list,gent);
	*ptr  = (char *) UU_LIST_ARRAY (list);
	return(UU_LIST_LENGTH(list));
}
