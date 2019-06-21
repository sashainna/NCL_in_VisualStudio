/*********************************************************************
**    NAME         :  m5ioslo.c
**       CONTAINS:  Evaluation routines for RB surface and curve
**                  on RB surface (UV curve).
**         um_ev9_rbsplsrf (evflag, u, v, eptr, tfmat, evout)
**         um_ev9_rbsplsrf1 (lflg,ku,nu,kv,nv,u,v,tu,tv,wt,pt,evout);
**         um_ev9_rbspl_segmnt (flag,ku,nku,tu,wt,pt,u,evout,wl)
**         
**    COPYRIGHT 1995 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       m5ioslo.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:06
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdeval.h"
#include "modef.h"
#include "msol.h"
#include "nccs.h"
#include "uminmax.h"
#include "nclxmdl.h"

#include "mdebug.h"

UU_REAL q[UM_MAXORDER*3];
UU_REAL q1d[UM_MAXORDER*3];
UU_REAL q2d[UM_MAXORDER*3];
UU_REAL wl[UM_MAXORDER];

extern int NCLX_internal_geom;
int NCL_need_normal = 0;

/*********************************************************************
**    E_FUNCTION: um_ev9_rbsplsrf (evflag, u, v, eptr, tfmat, evout)
**       Evaluate a rational bspline surface at a specified parameters.
**    PARAMETERS   
**       INPUT  : 
**          evflag               flag specifying data to evaluate
**          u, v                 parameter values to evaluate
**          eptr                 pointer to rational bspline SF record
**          tfmat                transformation matrix.
**       OUTPUT :  
**          evout                pointer to evaluator record
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_ev9_rbsplsrf(evflag, u, v, eptr, tfmat, evout)
int evflag;
UU_REAL u, v;
struct UM_rbsplsrf_rec *eptr;
UM_transf tfmat;
struct UM_evsrfout *evout;

{
	UU_REAL *tu, *tv, *pt, *wt;
	UU_REAL offd, ruv[2];
	int iuv[4];
	NCLX_mdl_surf *s;
/*
...unpack data from rbspline surface bundle
*/
	if (NCLX_internal_geom)
	{
		s = (NCLX_mdl_surf *)eptr;
		tu = s->tu;
		tv = s->tv;
		pt = s->pt;
		wt = s->wgt;
		offd = (*s).sfhead.offdist;
		iuv[0] = (*s).sfhead.udegree - 1;
		iuv[1] = (*s).sfhead.vdegree - 1;
		iuv[2] = (*s).sfhead.udegseg;
		iuv[3] = (*s).sfhead.vdegseg;
	}
	else
	{
		tu = eptr->tu;
		tv = eptr->tv;
		pt = eptr->pt;
		wt = eptr->wt;
		offd = eptr->offdist;

		iuv[0] = eptr->ku - 1;
		iuv[1] = eptr->kv - 1;
		iuv[2] = eptr->nu;
		iuv[3] = eptr->nv;
	}
	ruv[0] = u;
	ruv[1] = v;
	um_ev9_rbsplsrf1 (evflag,iuv,ruv,tu,tv,wt,pt,offd,evout); 
	ncl_transform_evsfout (evflag, tfmat, evout);
	uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION: um_ev9_rbsplsrf1 (lflg,ku,nu,kv,nv,u,v,
**                                  tu,tv,wt,pt,evout); 
**       Evaluate a rational bspline surface at a specified parameters
**       using data extracted from unibase structure.
**    PARAMETERS   
**       INPUT  : 
**          evflag               flag specifying data to evaluate
**          u, v                 parameter values to evaluate
**          eptr                 pointer to rational bspline SF record
**       OUTPUT :  
**          evout                pointer to evaluator record
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_ev9_rbsplsrf1 (evflag,iuv,ruv,tu,tv,wt,pt,offd,evout)
int evflag;
int *iuv;
UU_REAL *ruv, offd, *tu, *tv, *wt, *pt;
struct UM_evsrfout *evout;

{
	struct UM_evcrvout evcv;
	UU_REAL u, v, vv, *weight;
	UU_REAL u1;
	UU_REAL del,d0,d1,d;
	UM_vector voff;
	int ku, kv, nu, nv, mv, i, j, lflg;
	int nku, nkv, ikv, ixwh, ixch, ixw, ixc;
	int lcorner;
	UU_LOGICAL lfixperp;

	ku = iuv[0];
	kv = iuv[1];
	nu = iuv[2];
	nv = iuv[3];

	nku = nu + 2*ku;    
	nkv = nv + 2*kv; 

	u   = ruv[0];
	v   = ruv[1];

	lflg = (evflag == UM_NORM || evflag < UM_FRSTDERIV && offd != 0.)? 
				UM_FRSTDERIV: evflag;
/*
...Set u=const & get control points along v segment of curve
*/
	vv   = tv[kv] + (tv[nkv-kv] - tv[kv]) * v; 

	lcorner = (lflg >= UM_FRSTDERIV && u*(1-u) == 0)? 1: 0;
	lfixperp = (NCL_need_normal == 1) && (evflag == UM_FRSTDERIV);

	ixwh = nku - ku;
	ixch = 3*ixwh;
	um_knotindex(nv+1, &tv[kv], vv, &mv);
/*
.....vp 6/2/97
.....make sure that multiple knots are pointed at its first vertice.
*/
	while (tv[kv+mv+1] - tv[kv+mv] < UM_DFUZZ && mv > 0) mv--;
	ikv = mv;                            

	j = 0;
	weight = UU_NULL;
	for (i=ikv; i<=ikv+kv; i++)
	{
		ixw = i * ixwh;
		ixc = i * ixch;
		if (wt) 
		{
			weight = &wt[ixw]; 
			um_ev9_rbspl_segmnt(lflg,ku,nku,tu,weight,&pt[ixc],u,&evcv,&wl[i-ikv]);
		}
		else
			um_ev9_rbspl_segmnt(lflg,ku,nku,tu,weight,&pt[ixc],u,&evcv,UU_NULL);
		um_vctovc (evcv.cp,&q[j]); 
/*
...To get I and II derivative save them at control points
*/ 
		if (lflg >= UM_FRSTDERIV) um_vctovc (evcv.dcdu,&q1d[j]);
		if (lflg >= UM_SECDERIV) um_vctovc (evcv.d2cdu2,&q2d[j]);
		j +=3;
	}        
/*
...Evaluate segment at v to get point.
*/
	if (wt)
		um_evrbsplcrv(lflg, kv, vv, &tv[ikv+kv], q, wl, &evcv, UU_NULL, lcorner);
	else
		um_evrbsplcrv(lflg, kv, vv, &tv[ikv+kv], q, UU_NULL, &evcv, UU_NULL, lcorner);
	um_vctovc (evcv.cp,evout->sp);

	if (lcorner == 1 && UM_DOT (evcv.dcdu,evcv.dcdu) < 1.e-8)
	{
/*
..... If at u=0 or u=1 the v-derivative is zero, get the derivative at a nearby
..... different u. This should work better than just substituting zero derivatiive
..... with the hard-coded (0.001,0,0) vector (as um_evrbsplcrv does when lcorner=0).
*/
		u1 = (u <= 0.5)? u+0.001: u-0.001;
		um_ev9_rbsplsrf2 (lflg,ikv,kv,ku,nku,ixwh,ixch,tu,tv,wt,pt,u1,vv,&evcv);
	}
	
	if (lfixperp && UM_DOT (evcv.dcdu,evcv.dcdu) < 1.e-8 &&	(u < 0.001 || u > 0.999))
	{
		d0 = UM_DOT (evcv.dcdu,evcv.dcdu);

		if (u < 0.5)
			del = 0.001;
		else
			del = -0.001;
		for (i = 0, u1 = u; i < 50; i++)
		{
			u1 += del;
			um_ev9_rbsplsrf2 (lflg,ikv,kv,ku,nku,ixwh,ixch,tu,tv,wt,pt,u1,vv,&evcv);
			d = UM_DOT (evcv.dcdu,evcv.dcdu);
			if (d > 1.e-8 || d < d0) break;
		}
	}

/*
...Evaluate segment (of I derivatives) at v to get first derivative.
...Copy v features if required, before evcv is reused.
*/
	if (lflg >= UM_FRSTDERIV)
	{
		um_vctovc (evcv.dcdu,evout->dsdv);
		if (lflg >= UM_SECDERIV) um_vctovc (evcv.d2cdu2,evout->d2sdv2);
		if (lflg >= UM_CURVATURE) evout->vcurv = evcv.curv;
		if (wt)
			um_evrbsplcrv(lflg, kv, vv, &tv[ikv+kv], q1d, wl, &evcv, UU_NULL, 0);
		else
			um_evrbsplcrv(lflg, kv, vv, &tv[ikv+kv], q1d, UU_NULL, &evcv, UU_NULL, 0);
		um_vctovc (evcv.cp,evout->dsdu);
		um_cross (evout->dsdu,evout->dsdv,evout->snorm);
		if (lfixperp)
		{
			del = UM_DOT (evout->snorm,evout->snorm);
			if (del < 1.e-12 && del > 1.e-16)
			{
				d0 = UM_DOT (evout->dsdu,evout->dsdu);
				d1 = UM_DOT (evout->dsdv,evout->dsdv);
				if (d0 > 1.e-12 && d1 > 1.e-12)
				{
					d = sqrt (1.2e-12 / del);
					um_vctmsc (evout->dsdu,d,evout->dsdu);
					um_vctmsc (evout->dsdv,d,evout->dsdv);
				}
			}
		}
		um_unitvc (evout->snorm,evout->snorm);
		if (offd != 0.) 
		{
			um_vctmsc (evout->snorm,offd,voff);
			um_vcplvc (evout->sp,voff,evout->sp);
		}
	} 
/*
...Evaluate segment (of II derivatives) at v to get second derivative      
*/
	if (lflg >= UM_SECDERIV)
	{
		if (wt)
			um_evrbsplcrv(lflg, kv, vv, &tv[ikv+kv], q2d, wl, &evcv, UU_NULL, 0);
		else
			um_evrbsplcrv(lflg, kv, vv, &tv[ikv+kv], q2d, UU_NULL, &evcv, UU_NULL, 0);
		um_vctovc (evcv.cp,evout->d2sdu2);
	} 
/*
...Calculate u curvature 
*/
	if (lflg >= UM_CURVATURE)
	{
		um_get_curvature (evout->dsdu, evout->d2sdu2, &evout->ucurv);
	}
	uu_dexit;
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION: um_ev9_rbsplsrf2 (lflg,ikv,kv,ku,nku,ixwh,ixch,
**                                         tu,tv,wt,pt,u,vv,evcv)
**       Evaluate a u=const curve on a rational bspline surface, at a
**       given v - this routine does not change global data (q,q1d,q2d,wl)
**    PARAMETERS   
**       INPUT  : 
**          lflg                 flag specifying data to evaluate
**          u, vv                 parameter values to evaluate
**       OUTPUT :  
**          evcv                 pointer to evaluator record
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_ev9_rbsplsrf2 (lflg,ikv,kv,ku,nku,ixwh,ixch,tu,tv,wt,pt,u,vv,evcv)
int lflg,ikv,kv,ku,nku,ixwh,ixch;
UU_REAL *tu, *tv, *wt, *pt;
struct UM_evcrvout *evcv;
UU_REAL u,vv;
{
	int i,j;
	int ixw, ixc;
	UU_REAL *weight;
		
	j = 0;
	weight = UU_NULL;
	for (i = ikv; i <= ikv+kv; i++)
	{
		ixw = i * ixwh;
		ixc = i * ixch;
		if (wt != UU_NULL) 
		{
			weight = &wt[ixw]; 
			um_ev9_rbspl_segmnt(lflg,ku,nku,tu,weight,&pt[ixc],u,evcv,&wl[i-ikv]);
		}
		else
			um_ev9_rbspl_segmnt(lflg,ku,nku,tu,weight,&pt[ixc],u,evcv,UU_NULL);
		um_vctovc (evcv->cp,&q[j]); 
		j +=3;
	}
		
	um_evrbsplcrv(lflg, kv, vv, &tv[ikv+kv], q, UU_NULL, evcv, UU_NULL, 0);
		
	return(UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION: um_ev9_rbspl_segmnt (flag,ku,nku,tu,wt,pt,u,evout,wl)
**       Evaluate a rational bspline surface v segment at specified 
**       u parameter.
**    PARAMETERS   
**       INPUT  : 
**          flag                 flag specifying data to evaluate
**          ku                   u degree of (v) segment
**          nku                  number of knots in u direction
**          tu                   pointer to u knot values
**          wt                   pointer to weght values
**          pt                   pointer to control points of segment 
**          u                    parameter value to evaluate
**       OUTPUT :  
**          evout                pointer to evaluator record
**          wl                   weight at evaluated point
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_ev9_rbspl_segmnt (flag,ku,nku,tu,wt,pt,u,evout,wl)
int flag, ku, nku;
UU_REAL *tu, *wt, *pt, u, *wl;
struct UM_evcrvout *evout;
{
	int mu, ix;
	UU_REAL uu, t0, t1, *weight;
 
	t0 = tu[ku];
	t1 = tu[nku-ku];

	uu = u*(t1 - t0) + t0;
	 
	um_knotindex (nku-2*ku+1, &tu[ku], uu, &mu);
	while (tu[ku+mu+1] - tu[ku+mu] < UM_DFUZZ && mu > 0) mu--;

	ix = 3 * mu; 
	weight = (wt == UU_NULL)? UU_NULL: &wt[mu];

	um_evrbsplcrv(flag, ku, uu, &tu[mu+ku], &pt[ix], weight, evout, wl, 0);

	uu_dexit;
	return (UU_SUCCESS);
}

void um_evlset ()
{
	NCL_need_normal = 1;
}

void um_evlrst ()
{
	NCL_need_normal = 0;
}
