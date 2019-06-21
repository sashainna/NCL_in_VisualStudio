/*********************************************************************
**    NAME         :  ubrent.c
**
**	 Local version of the brent minimum search algorithm.
**
**    COPYRIGHT 2006 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       ubrent.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:52
*********************************************************************/

#include <math.h>
#include "usysdef.h"

#define SHFT(a,b,c,d) (a)=(b);(b)=(c);(c)=(d);
#define SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a))

#define ITMAX 100
#define CGOLD 0.3819660
#define ZEPS 1.0e-10

/*********************************************************************
**    E_FUNCTION     :  UU_REAL uu_brent (ax,bx,cx,xmin,fmin,f,fdata,eps)
**
**       Find the minimum of a one-parameter function. The code is
**       copied from the 'Numerical recipes in C'
**    PARAMETERS
**       INPUT  :
**          (ax,cx)       Parameter range for the function
**          bx            First approximation of the minimum:
**                        MUST HAVE f(b) < f(a) and f(b) < f(c)
**          f             function pointer
**          fdata         function data pointer
**          eps           epsilon value for success: good enough if found
**                        x with f(x) < eps
**       OUTPUT :
**          xmin          the parameter at minimum
**          fmin          the function minimum value
**    RETURNS      :  UU_FAILURE iff the function call fails, else UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uu_brent (ax,bx,cx,xmin,fmin,f,fdata,eps)
UU_REAL ax,bx,cx,*xmin,*fmin,eps;
void *fdata;
int	(*f)();
{
	int iter,status;
	UU_REAL a,b,d,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm;
	UU_REAL e = 0.0;
	UU_REAL tol = 0.0001;

/*
..... a,b must be in ascending order
*/
	a = (ax < cx ? ax : cx);
	b = (ax > cx ? ax : cx);

	x = w = v = bx;
	status = (*f)(x,fdata,&fx);
	if (status != UU_SUCCESS) return (status);
	fw = fv = fx;

	for (iter = 1; iter <= ITMAX; iter++)
	{
		xm = 0.5*(a + b);
		tol2=2.0*(tol1=tol*fabs(x)+ZEPS);
		if (fabs(x-xm) <= (tol2-0.5*(b-a)))
		{
			*xmin = x;
			*fmin = fx;
			return (status);
		}
		if (fabs(e) > tol1)
		{
			r = (x-w)*(fx-fv);
			q = (x-v)*(fx-fw);
			p = (x-v)*q-(x-w)*r;
			q = 2.0*(q-r);
			if (q > 0.0) p = -p;
			q = fabs(q);
			etemp = e;
			e = d;
			if (fabs(p) >= fabs(0.5*q*etemp) || p <= q*(a-x) || p >= q*(b-x))
				d=CGOLD*(e=(x >= xm ? a-x : b-x));
			else
			{
				d=p/q;
				u=x+d;
				if (u-a < tol2 || b-u < tol2)
				d=SIGN(tol1,xm-x);
			}
		}
		else
		{
			d = CGOLD*(e=(x >= xm ? a-x : b-x));
		}
		u = (fabs(d) >= tol1 ? x+d : x+SIGN(tol1,d));


		status = (*f)(u,fdata,&fu);
		if (status != UU_SUCCESS) return (status);

		if (fu <= fx)
		{
/*
..... exit if the current minimum is small enough
*/
			if (fu < eps)
			{
				*xmin = u;
				*fmin = fu;
				return (status);
			}
			if (u >= x) a=x; else b=x;
			SHFT(v,w,x,u);
			SHFT(fv,fw,fx,fu);
		}
		else
		{
			if (u < x) a=u; else b=u;
			if (fu <= fw || w == x)
			{
				v=w;
				w=u;
				fv=fw;
				fw=fu;
			}
			else if (fu <= fv || v == x || v == w)
			{
				v=u;
				fv=fu;
			}
		}
	}

	*xmin = x;
	*fmin = fx;
	return (status);
}
