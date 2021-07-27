/*********************************************************************
**    NAME         :  m4ioslo.c
**       CONTAINS:
**       um_addknot
**       um_subdiv
**       um_crvoslo
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m4ioslo.c , 25.1
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

extern int NCLX_internal_geom;

/*********************************************************************
**    E_FUNCTION     : um_addknot(mul,tnew,n,t)
**       Add a knot value to a knot array making sure that it occurs
**       exactly mul times.
**    PARAMETERS   
**       INPUT  : 
**          mul                  desired multiplicity of new knot
**          tnew                 new knot value
**          n                    number of knot values in t
**          t                    array of knot values
**       OUTPUT :  
**          n                    number of knot values in t 
**          t                    new knot value added mul times
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_addknot(mul,tnew,n,t)
	int mul;
	UU_REAL tnew;
	int *n;
	UU_REAL t[];

	{
	int i,j;
	int mu;
	int no_t;
	int no_new;
	int equalparam, skip, move;

	uu_denter(UU_MTRC,(us,"um_addknot(%d,%f,%d,%8x)",mul,tnew,*n,t));
	um_knotindex(*n,t,tnew,&mu);
	no_t = *n;
	if (mu < no_t)
		for (i=no_t-1, j=(no_t+mul-1); i>mu; i--, j--) t[j] = t[i];
	for (j=mu+1; j<=(mu+mul); j++) t[j] = tnew;
	no_t = no_t + mul;
	no_new = 0;
	for (i=0,j=0; i<no_t; i++)
		{
		equalparam = (fabs(t[i] - tnew) < UM_EQPARM);
		if (equalparam) no_new = no_new + 1;
		skip = (equalparam) && (no_new > mul);
		move = (!equalparam) && (no_new > mul);
		if (move) t[j] = t[i];
		if (!skip) j++;
		}
	no_t = j;
	*n = j;
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : um_subdiv(k,tau,p,w,t,mu,j,pp,ww)
**      Find the next control polygon vertex.
**    PARAMETERS   
**       INPUT  : 
**          k                    order of bspline
**          tau                  knot vector for orginal polygon
**          p                    original control polygon
**          w                    weights
**          t                    knot vector for new polygon
**          mu                   tau[mu] <= t[j] < tau[mu+1]
**          j
**       OUTPUT :  
**          pp                   new vertex of new control polygon
**          ww                   new weight
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_subdiv(k,tau,p,w,t,mu,j,pp,ww)
	int k;
	UU_REAL tau[];
	UU_REAL p[][3];
	UU_REAL w[];
	UU_REAL t[];
	int mu;
	int j;
	UU_REAL pp[3];
	UU_REAL *ww;

	{
	UU_REAL p1, p2;
	UU_REAL q[10][4];
	UU_REAL a1,a2,a3;
	UU_REAL tval;
	int i,ii;
	int ikr;
	int jkr;
	int kk;
	int r;

/*---------------------------------------------------------------
**  Start of Executable Code
**-------------------------------------------------------------*/
/*
	sprintf(UM_sbuf,"SUBDIV:  mu=%d j=%d",mu,j);
	um_pscroll(UM_sbuf);
*/
	for (kk=(mu-k+1),i=0; kk<=mu; kk++,i++)
		{
		um_vctovc(p[kk],q[i]);
		q[i][3] = 1.0;
		if (w) q[i][3] = w[kk];
		}
	for (r=1,jkr=(j+k-1); r<=(k-1); r++,jkr--)
		{
		tval = t[jkr];
		for (ii=0,i=(mu-k+1+r),ikr=(mu+1); i<=mu; i++,ii++,ikr++)
			{
/*
			p1 = t[j+k-r] - tau[i];
			p2 = tau[i+k-r] - t[j+k-r];
*/
			p1 = tval - tau[i];
			p2 = tau[ikr] - tval;
/*
			sprintf(UM_sbuf,"        p1=%f, p2=%f",p1,p2);
			um_pscroll(UM_sbuf);
*/
			a1 = p1*q[ii+1][3];
			a2 = p2*q[ii][3];
			a3 = 1.0/(a1+a2);
			for (kk=0; kk<3; kk++)  
				q[ii][kk] = (q[ii+1][kk]*a1 + q[ii][kk]*a2) * a3;
			q[ii][3] = (a1+a2) / (p1+p2);
			}
		}
	for (kk=0; kk<3; kk++) pp[kk] = q[0][kk];
	*ww = q[0][3];
/*
	sprintf(UM_sbuf,"SUBDIV: pp=(%f,%f,%f) ww=%f",pp[0],pp[1],pp[2],*ww);
	um_pscroll(UM_sbuf);
*/
	}
/*********************************************************************
**    E_FUNCTION     : um_crvoslo(r1ptr,r2ptr)
**       Given a rational bspline curve (R1PTR) with its order, knot
**       vector, control polygon, and weight values completely
**       specified and another rational bspline curve (R2PTR) with
**       the same order but having a knot vector with more values,
**       compute a control polygon and weight values which specify
**       the same curve.
**    PARAMETERS   
**       INPUT  : 
**          r1ptr                original rational bspline
**          r2ptr                new rational bspline with
**                               all data specified except 
**                               control polygon and weights
**       OUTPUT :  
**          r2ptr                new rational bspline with
**                               control polygon and weights
**                               specified
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_crvoslo(r1ptr,r2ptr)
	struct UM_rbsplcrv_rec *r1ptr;
	struct UM_rbsplcrv_rec *r2ptr;

	{
	int j;                           /* index */
	int mu;                          /* index of knot value in tau */
	int k;
	int n;
	UU_REAL *tau;
	UU_REAL *pt;
	UU_REAL *wt;
	UU_REAL *t;
	UU_REAL *d;
	UU_REAL *w;
	UU_REAL wtmp;

/*---------------------------------------------------------------
**  Start of Executable Code
**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_crvoslo(%8x,%8x)",r1ptr,r2ptr));
	k = r1ptr->k;
	n = r1ptr->no_pt;
	tau = r1ptr->t;
	pt = r1ptr->pt;
	wt = r1ptr->wt;
	t =  r2ptr->t;
	d =  r2ptr->pt;
	w = UU_NULL;
	if (wt) w = r2ptr->wt;
	r2ptr->no_pt = r2ptr->no_t - r2ptr->k;
	r2ptr->no_wt = 0;
	if (wt) r2ptr->no_wt = r2ptr->no_pt;
	for (j=0; j<r2ptr->no_pt; j++)
		{
/*    um_knotindex(k+n,tau,t[j],&mu);*/
		um_knotindex(r1ptr->no_t,tau,t[j],&mu);
		um_subdiv(k,tau,pt,wt,t,mu,j,&d[3*j],&wtmp);
		if (w) w[j] = wtmp;
		}
	uu_dexit;
	}
