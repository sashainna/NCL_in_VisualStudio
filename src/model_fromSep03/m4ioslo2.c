/*********************************************************************
**    NAME         :  m4ioslo2.c
**       CONTAINS:
**       um_knotindex
**       um_evrbsplcrv
**       um_ev7_rbsplcrv
**       These were removed from m4ioslo.c so that they may be
**       included in the OML.  JLS 6/15/99
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m4ioslo2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:04
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "mcrv.h"
#include "mdeval.h"
#include "modef.h"
#include "mdebug.h"
#include "nclxmdl.h"

UU_REAL a[UM_MAXORDER];
UU_REAL b[UM_MAXORDER];
UU_REAL qw[UM_MAXORDER][4];

extern int NCLX_internal_geom;

/*********************************************************************
**    E_FUNCTION     : um_knotindex(kn,tau,t,mu)
**      Given a knot vector TAU and a knot value T, find the index
**      MU (for TAU) such that TAU(MU) <= T < TAU(MU+1).
**    PARAMETERS   
**       INPUT  : 
**          kn                number of values in knot vector tau
**          tau               knot vector
**          t                 knot value
**
**       OUTPUT :  
**          mu             index into tau satisfying inequality
**                         tau(mu) <= t < tau(mu+1)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_knotindex(kn,tau,t,mu)
	int kn;
	UU_REAL tau[];
	UU_REAL t;
	int *mu;

	{
	int i;

	uu_denter(UU_MTRC,(us,"um_knotindex(%d,%8x,%8x,?)",kn,tau,t));

	*mu = 0;
	for (i=0; i<(kn-1) && (tau[i] <= (t+UM_DFUZZ)); i++) *mu = i;

	uu_dexit;
	return (0);
	}
/*********************************************************************
**    E_FUNCTION     : um_evrbsplcrv(evflag, m, x, t, pt, wt, evout, w)
**       Evaluate a rational bspline curve at a specified parameter.
**    PARAMETERS   
**       INPUT  : 
**          evflag                  data to evaluate for curve
**          m                       degree of curve
**          x                       parameter value to evaluate at
**          t                       knot vector for curve
**          pt                      control polygon coefficients
**          wt                      weight vector (UU_NULL => 1.0)
**       OUTPUT :  
**          evout                   curve evaluation output record
**          w                       
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_evrbsplcrv(evflag, m, x, t, pt, wt, evout, w, lnofix)
	int evflag;
	int m;
	UU_REAL x;
	UU_REAL t[];
	UU_REAL pt[][3];
	UU_REAL wt[];
	struct UM_evcrvout *evout;
	UU_REAL *w;
	int lnofix;

	{
	UU_REAL a1;
	UU_REAL b1;
	UU_REAL c1;
	UU_REAL c2;
	UU_REAL c3;
	UU_REAL v;
	UU_REAL weight;
	UU_REAL del1,del2;
	UU_LOGICAL fromright;
	int i,j,k,jj;
	int KMAX;
	UU_LOGICAL lmult;
	UU_REAL d;

	for (i=1; i<=m; i++) a[i] = x - t[i-m];
	for (i=1; i<=m; i++) b[i] = t[i] - x;
	fromright = ((t[1] - x) > (x - t[0]));
	lmult = UU_FALSE;

	weight = 1.;
	KMAX = (wt == UU_NULL)? 3: 4;
	if (fromright)
	{
		if (m > 1 && evflag >= UM_FRSTDERIV && wt == UU_NULL)
		{
			d = UM_SQDIS (pt[0],pt[1]);
			lmult  = (d < 1.e-16);
		}

		for (i=0; i<=m; i++)
		{
			if (wt != UU_NULL) weight = wt[i];
			for (k=0; k<3; k++) qw[i][k] = pt[i][k] * weight;
			qw[i][3] = weight;
		}

		if (lmult)
		{
			for (k=0; k<3; k++) qw[1][k] = pt[2][k];
		}

		for (j=1; j<=m; j++)
		{
			for (i=0; i<=(m-j); i++)
			{
				a1 = a[i+j];
				b1 = b[i+1];
				for (k=0; k<KMAX; k++) 
					qw[i][k] = (a1*qw[i+1][k] + b1*qw[i][k]) / (a1+b1);
			}
		}
		del1 = m/b[1];
		if (m > 1) del2 = (m-1)/b[2];
	}
	else
	{
		if (m > 1 && evflag >= UM_FRSTDERIV && wt == UU_NULL)
		{
			d = UM_SQDIS (pt[m],pt[m-1]);
			lmult  = (d < 1.e-16);
		}

		for (i=0; i<=m; i++)
		{
			if (wt != UU_NULL) weight = wt[m-i];
			for (k=0; k<3; k++) qw[i][k] = pt[m-i][k] * weight;
			qw[i][3] = weight;
		}

		if (lmult)
		{
			for (k=0; k<3; k++) qw[1][k] = pt[m-2][k];
		}

		for (j=1; j<=m; j++)
		{
			jj = m-j+1;
			for (i=0; i<=(m-j); i++)
			{
				a1 = a[m-i];
				b1 = b[jj-i];
				for (k=0; k<KMAX; k++) 
					qw[i][k] = (a1*qw[i][k] + b1*qw[i+1][k]) / (a1+b1);
			}
		}
		del1 = -m/a[m];
		if (m > 1) del2 = -(m-1)/a[m-1];
	}
	for (k=0; k<3; k++) evout->cp[k] = qw[0][k] / qw[0][3];
	if (w) *w = qw[0][3];

	if (evflag >= UM_FRSTDERIV)
	{
		c1 = (del1*qw[1][3])/qw[0][3];
		for (k=0; k<3; k++)
			evout->dcdu[k] = c1*(qw[1][k]/qw[1][3] - evout->cp[k]);
/*
.....Protect againts zero length derivatives
.....Bobby  -  11/24/99
*/
		if (lnofix == 0 &&
			UM_DOT(evout->dcdu,evout->dcdu) == 0.) evout->dcdu[0] = .001;
	}

	if (evflag >= UM_SECDERIV)
	{
		if (m <= 1)
			for (k=0; k<3; k++)
				evout->d2cdu2[k] = 2.0 * (del1 - c1) * evout->dcdu[k];
		else
		{
			c2 = del2 * qw[2][3] / qw[0][3];
			c3 = del2 + 2.0 * (c1-del1) + (m-1)*del1/m;
			for (k=0; k<3; k++)
			{
				v = qw[2][k]/qw[2][3] - evout->cp[k];
				evout->d2cdu2[k] = (c2 * v * del1) - (c3 * evout->dcdu[k]);
			}
		}
	}
	if (evflag >= UM_CURVATURE)
		 um_get_curvature (evout->dcdu, evout->d2cdu2, &evout->curv);

	return (0);
}

/*********************************************************************
**    E_FUNCTION: um_ev7_rbsplcrv(evflag, u, eptr, tfmat, evout)
**       Evaluate a rational bspline curve at a specified parameter.
**    PARAMETERS   
**       INPUT  : 
**          evflag               flag specifying data to evaluate
**          u                    parameter value to evaluate
**          eptr                 pointer to rational bspline record
**          tfmat                transformation matrix.
**       OUTPUT :  
**          evout                pointer to evaluator record
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_ev7_rbsplcrv(evflag, u, eptr, tfmat, evout)
int evflag;
UU_REAL u;
struct UM_rbsplcrv_rec *eptr;
UM_transf tfmat;
struct UM_evcrvout *evout;

{
	UU_REAL *t;
	UM_coord *pt;
	UU_REAL *wt,*weight;
	UU_REAL uu, u1, dlt, dt, t0, t1;
	UU_REAL w;
	UM_vector vec;
	int k, n, mu, flag, last;

	uu_denter(UU_MTRC,(us,"um_ev7_rbsplcrv(%d,u:%g,%8x,tfmat:%x,%8x)",
					evflag,u,eptr,tfmat,evout));

	if (NCLX_internal_geom)
	{
		NCLX_mdl_curve *s;
		s  = (NCLX_mdl_curve *)eptr;
		t  = s->tparms;
		pt = (UM_coord *)s->pt;
		wt = s->wgt;
		k  = (*s).cvhead.degree;
		n  = (*s).cvhead.degseg;
		last = (*s).ntparm - 1;
		t0 = (*s).cvhead.t0;
		t1 = (*s).cvhead.t1;
   }
   else
   {
		t  = eptr->t;
		pt = (UM_coord *)eptr->pt;
		wt = eptr->wt;
		k  = eptr->k;
		n  = eptr->n;
		last = eptr->no_t - 1;
		t0 = eptr->t0;
		t1 = eptr->t1;
	}
	u1 = uu = t0 + (u * (t1 - t0));
	if (uu < t[0]) uu = t[0];
	if (uu > t[last]) uu = t[last];
	dlt = u1 - uu;
	if (u1 < t[0] || u1 > t[last])
	{
		flag = UM_FRSTDERIV;
		if (evflag > UM_FRSTDERIV) um_vctovc (UM_zerovec,evout->d2cdu2);
		if (evflag > UM_SECDERIV) evout->curv = .0;
	}
	else flag = evflag;

	um_knotindex(n+1, &t[k-1], uu, &mu);
/*
.....vp 6/27/97
.....make sure that multiple knots are pointed at its first vertice.
.....This may be necessary also in other evaluators where um_knotindex is
.....called to find span to evaluate. For now I leave this open.
*/
	while (t[k+mu] - t[k+mu-1] < UM_DFUZZ && mu>0) mu--;
	weight = (wt == UU_NULL)? UU_NULL: &wt[mu];
	um_evrbsplcrv(flag, k-1, uu, &t[mu+k-1], &pt[mu], weight, evout, &w, 0);
		
	if (flag >= UM_FRSTDERIV)
	{
		dt  = t1 - t0;
		um_vctmsc(evout->dcdu, dt, evout->dcdu); 
		if (dlt != 0.)
		{
			um_unitvc(evout->dcdu,vec);
			um_vctmsc(vec, dlt, vec);
			um_vcplvc(evout->cp, vec, evout->cp);
		}
	} 
	if (flag >= UM_SECDERIV)
		um_vctmsc(evout->d2cdu2, dt*dt, evout->d2cdu2); 

	um_transform_evcrvout(flag, eptr, tfmat, evout);
	uu_dexit;
	return(UU_SUCCESS);
}
