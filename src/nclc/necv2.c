/*********************************************************************
**    NAME         :  necv2.c
**       CONTAINS: routines to handle NCL Bezier curves
**                 These were removed from necv.c so that
**                 they may also be included in the OML
**                 JLS 6/15/99
**
**       ncl_ev_curve
**       int ncl_evnclcrv
**       ncl_ev7_nclcrv
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       necv2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:28
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "mfort.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mdebug.h"
#include "mdattr.h"
#include "mdeval.h"
#include "modef.h"
#include "ulist.h"
#include "uminmax.h"

#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclxmdl.h"


extern int NCLX_internal_geom;

static int NCL_COMPCRV = 0;
/*********************************************************************
**    E_FUNCTION     : int ncl_ev_curve(evflag,u,eptr,tfmat,evout)
**       Evaluate an NCL curve at a parameter
**      PARAMETERS:
**          INPUT:
**              evflag        UM_POINT= calculate point on line only;
**                        UM_FRSTDERIV= calculate point and 1st
**                                  derivative;
**                        UM_SECDERIV= calculate point, 1st and
**                                  2nd derivative;
**                        UM_CURVATURE= calc point, 1st, 2nd deriv,
**                                 and curvature;
**              u                   the parameter value
**              eptr                pointer to the entity data
**              tfmat               transformation matrix.
**          OUTPUT:
**              evout               pointer to a curve evaluator
**                         record containing both the requested
**                         information, and (ultimately) a
**                         status value indicating the validity
**                         of the requested information.
**  RETURNS : nothing currently, ultimately will return one of the
**              following:
**            UM_VALID: all requested fields are valid;
**            UM_BADFIELDS: at least one requested fields is invalid;
**            UM_BADRECORDS: at least one entire record is invalid;
**            UM_INVALID: all output is suspect.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int
ncl_ev_curve(evflag,u,eptr,tfmat, evout)
int evflag;
UM_param u;
struct NCL_curve_rec *eptr;
UM_transf tfmat;
struct UM_evcrvout *evout;
{
	UM_real8 fu, ver;
	UM_real8 pt[3];
	UM_real8 ve[3];
	UM_int4 key;
	int i;
	UM_int2 idx = 169;

	getsc (&idx,&ver);

	if (ver >= 8.34999) 
	{
		ncl_ev7_nclcrv (evflag,u,eptr,tfmat, evout);
		goto Done;
	}
/*
.....Init variables for FORTRAN call
*/
	fu = eptr->t0 + (u * (eptr->t1 - eptr->t0));
	key = eptr->key;

	switch (evflag)
	{
		case UM_CURVATURE:
		case UM_SECDERIV:
/*
.............Just set second derivative to zero for now
*/
			evout->curv = 0.0;
			for (i=0 ; i < 3 ; i++) evout->d2cdu2[i] = 0.0;

		case UM_FRSTDERIV:
		case UM_POINT:
/*
.............Get point, vector from NCL
*/
			ncvevl(&key, &fu, pt, ve);
/*
.............Store pt and ve into evout
*/
			for (i = 0; i < 3; i++)
			{
				evout->cp[i] = pt[i];
				evout->dcdu[i] = ve[i];
			}

			break;

		default:
			uu_uerror0(/*error - illegal evaluation request*/UM_MODEL,50);
			break;
	}
	/* position results in evaluator record according to the transform, tfmat */
	um_transform_evcrvout(evflag, eptr, tfmat, evout);

Done:
	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : ncl_evnclcrv(evflag, m, x, t, pt, wt, evout, w)
**       Evaluate NCL curve at a specified parameter.
**       (Based on um_evrbsplcrv).
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
int ncl_evnclcrv(evflag, x, t, pt, evout)
int evflag;
UU_REAL x;
UU_REAL t[];
UU_REAL pt[][3];
struct UM_evcrvout *evout;

{
	UU_REAL a[4];
	UU_REAL b[4];
	UU_REAL q[4][4];
	UU_REAL a1, b1, c1, c2, c3, d2, s2, v;
	UU_REAL del1,del2;
	UU_LOGICAL fromright;
	int i,j,k,jj;

	for (i=1; i<=3; i++) 
	{
		a[i] = x - t[0];
		b[i] = t[1] - x;
	}
	fromright = ((t[1] - x) > (x - t[0]));

	if (fromright)
	{
		for (i=0; i<=3; i++)
		{
			for (k=0; k<3; k++) q[i][k] = pt[i][k];
			q[i][3] = 1;
		}
		for (j=1; j<=3; j++)
		{
			for (i=0; i<=(3-j); i++)
			{
				a1 = a[i+j];
				b1 = b[i+1];
				for (k=0; k<4; k++) q[i][k] = (a1*q[i+1][k] + b1*q[i][k]) / (a1+b1);
			}
		}
		del1 = 3/b[1];
		del2 = 2/b[2];
	}
	else
	{
		for (i=0; i<=3; i++)
		{
			for (k=0; k<3; k++) q[i][k] = pt[3-i][k];
			q[i][3] = 1;
		}
		for (j=1; j<=3; j++)
		{
			jj = 4-j;
			for (i=0; i<=(3-j); i++)
			{
				a1 = a[3-i];
				b1 = b[jj-i];
				for (k=0; k<4; k++) q[i][k] = (a1*q[i][k] + b1*q[i+1][k]) / (a1+b1);
			}
		}
		del1 = -3/a[3];
		del2 = -2/a[2];
	}
	for (k=0; k<3; k++) evout->cp[k] = q[0][k] / q[0][3];

	if (evflag >= UM_FRSTDERIV)
	{
		c1 = (del1*q[1][3])/q[0][3];
		for (k=0; k<3; k++)
			evout->dcdu[k] = c1*(q[1][k]/q[1][3] - evout->cp[k]);
	}

	if (evflag >= UM_SECDERIV)
	{
		c2 = del2 * q[2][3] / q[0][3];
		c3 = del2 + 2.0 * (c1-2.0*del1/3);
		for (k=0; k<3; k++)
		{
			v = q[2][k]/q[2][3] - evout->cp[k];
			evout->d2cdu2[k] = (c2 * v * del1) - (c3 * evout->dcdu[k]);
		}
	}

	if (evflag >= UM_CURVATURE)
	{
		d2 = um_dot (evout->dcdu,evout->dcdu);
		s2 = um_dot (evout->d2cdu2,evout->d2cdu2);
		c1 = um_dot (evout->dcdu,evout->d2cdu2);
		c1 = d2*s2 - c1*c1;
		c1 = (c1 > 0.0)? c1: 0.0;
		evout->curv = sqrt(c1 / (d2*d2*d2));  
	}
	return (0);
}

/*********************************************************************
**    E_FUNCTION: ncl_ev7_nclcrv(evflag, u, eptr, tfmat, evout)
**       Evaluate NCL curve at a specified parameter.
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
ncl_ev7_nclcrv(evflag, u, eptr, tfmat, evout)
int evflag;
UU_REAL u;
struct NCL_curve_rec *eptr;
UM_transf tfmat;
struct UM_evcrvout *evout;

{
	struct NCL_segment_rec *segment;
	NCLX_mdl_curve *s;
	UM_real4 *t;
	UU_REAL tt[2], pt[12];
	UU_REAL uu, u1, rho, dlt;
	UU_REAL t0,t1,tend;
	UM_vector vec;
	int i, n, mu, flag, last;

	if (NCLX_internal_geom)
	{
		s = (NCLX_mdl_curve *)eptr;
		n = s->nparm;
		t = s->parms;
		t0 = (*s).cvhead.t0;
		t1 = (*s).cvhead.t1;
		tend = (*s).cvhead.tlen;
	}
	else
	{
		n  = eptr->no_param;
		t  = eptr->param;
		t0 = eptr->t0;
		t1 = eptr->t1;
		tend = eptr->t_end;
	}

	last  = n - 1;
	u1    = uu = t0 + (u * (t1 - t0));
	if (uu < 0.)       uu = 0.;        
	if (uu > t[last])  uu = t[last];  
	dlt = tend * (u1 - uu);
	if (u1 < 0. || u1 > t[last])
	{
		flag = UM_FRSTDERIV;
		if (evflag > UM_FRSTDERIV) um_vctovc (UM_zerovec,evout->d2cdu2);
		if (evflag > UM_SECDERIV) evout->curv = 0.;
	}
	else flag = evflag;
/*
...find segment on the curve
*/
	for (mu=0; mu<n-1 && (t[mu] <= (uu+UM_EQPARM)); mu++);
  
	tt[0] = (mu > 0)? t[mu-1] : .0;
	tt[1] = t[mu];
/*
...recover control points from record
*/
	if (NCLX_internal_geom) segment = (struct NCL_segment_rec *) &s->segment[mu];
	else segment = (struct NCL_segment_rec *) &eptr->segment[mu]; 
	rho     = segment->rho;
	for (i=0; i<3; i++)
	{
		pt[i] = segment->point[i];
		pt[i+3] = pt[i] + segment->delta[i];
	}
	segment++;
	for (i=0; i<3; i++)
	{
		pt[i+6] = segment->point[i] - rho * segment->delta[i];
		pt[i+9] = segment->point[i];
	}

	ncl_evnclcrv(flag, uu, tt, pt, evout);
		
	if (flag >= UM_FRSTDERIV)
	{
		um_vctmsc(evout->dcdu, t1 - t0, evout->dcdu);
		if (dlt != 0.)
		{
			um_unitvc(evout->dcdu,vec);
			um_vctmsc(vec, dlt, vec);
			um_vcplvc(evout->cp, vec, evout->cp);
		}
	} 
	um_transform_evcrvout(flag, eptr, tfmat, evout);
	uu_dexit;
	return(UU_SUCCESS);
}
