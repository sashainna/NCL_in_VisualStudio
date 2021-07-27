/*********************************************************************
**    NAME         : mibsc 
**       CONTAINS:
**			   um_knot
**			   um_d2basis
**			   um_dbasis
**			   um_basis
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m4ibsc.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:04
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "uhep.h"
#include "udebug.h"
#include "modef.h"

static UU_LOGICAL trace = UU_FALSE;
#define UM_MAX_CNTL_PTS 200
#define BYTES_IN_MAX_CNTL_PTS 3*sizeof(UU_REAL)*MAX_CNTL_PTS

/*********************************************************************
**	E_FUNCTION: um_knot(i,max)
**
**		DESCRIPTION: Calculate bspline knot value.
**		
**		PARAMETERS	
**			INPUT: 
**				i			  knot index
**				max			maximum knot index (=npts-order+2)
**			
**			OUTPUT : none. 
**			
**		RETURNS		: calculated knot value.
**
**		SIDE EFFECTS : none
**
**		WARNINGS	  : none
*********************************************************************/
UU_REAL
um_knot(i,max)
	int	i;
	int	max;
{
	UU_REAL knot;					 /* knot value to return */
	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	if (i < 0)
		knot = 0;
	else if (i <= max)
		knot = i;
	else
		knot = max;
	return(knot);
}
/*********************************************************************
**	E_FUNCTION: um_d2basis(u,i,order,max) 
**
**		DESCRIPTION: Calculates the second derivative of a bspline
**				basis function. Not yet implemented.
**		
**		PARAMETERS	
**			INPUT: 
**			
**			OUTPUT :  
**			
**		RETURNS		: none
**
**		SIDE EFFECTS : none
**
**		WARNINGS	  : none
*********************************************************************/
UU_REAL
um_d2basis(u,i,order,max)
	UU_REAL u;
	int i;
	int order;
	int max;
{
/*---------------------------------------------------------------
**  Start of Executable Code
**-------------------------------------------------------------*/
}
/*********************************************************************
**	E_FUNCTION: um_dbasis(u,i,order,max)
**
**		DESCRIPTION: Calculates the first derivative of a bspline basis
**				function.
**		
**		PARAMETERS	
**			INPUT: 
**				u			   parameter value for evaluating the current
**								basis function; note, u is in the range
**								[0, maxknot], see "um_ev6_bsplcrv".
**
**				i			   max knot value of the support of the current
**								basis function.
**
**				order		   order of the basis function.
**
**				max			max knot value.
**			
**			OUTPUT :  none.
**			
**		RETURNS	: first derivative value of the basis function at u.
**
**		SIDE EFFECTS : none
**
**		WARNINGS	  : none
*********************************************************************/
UU_REAL
um_dbasis(u,i,order,max)
	UU_REAL u;
	int i;
	int order;
	int max;	/* max knot value */
{
	UU_REAL rbasis;
	UU_REAL um_knot();
	int icubic = 4;
	int iquadratic = 3;
	int ilinear = 2;
	UU_REAL bfuzz = UM_FUZZ;
	UU_REAL t0;
	UU_REAL t1;
	UU_REAL t2;
	UU_REAL t3;
	UU_REAL t4;
	UU_REAL a;
	UU_REAL b;
	UU_REAL a1;
	UU_REAL a2;
	UU_REAL a3;
	UU_REAL b1;
	UU_REAL b2;
	UU_REAL b3;

	/*  uu_denter(UU_MTRC,(us,"um_dbasis(%g,%d,%d,%d)",u, i, order, max)); */
	if (u > max) u = max;
	if (order == icubic)  /* fourth order b-spline basis functions */
	{
		t0 = um_knot(i-4, max);
		t1 = um_knot(i-3, max);
		t2 = um_knot(i-2, max);
		t3 = um_knot(i-1, max);
		t4 = um_knot(i, max);

		if ((u >= t0) && (u <= t1) && (t0 != t1))
		{
			a = 3.0 * (u - t0) * (u - t0);
			b = (t3 - t0) * (t2 - t0) * (t1 - t0);
			if (fabs(a) >= bfuzz)
				rbasis = a / b;
			else 
				rbasis = 0.0;
		}
		else if ((u >= t1) && (u <= t2) && (t1 != t2))
		{
			a1 = 2.0 * (u - t0) * (t2 - u) - (u - t0) * (u - t0);
			b1 = (t3 - t0) * (t2 - t0) * (t2 - t1);
			a2 = (u - t0) * (t3 - u) - (u - t1) * (u - t0) + (t3 - u) * (u - t1);
			b2 = (t3 - t0) * (t3 - t1) * (t2 - t1);
			a3 = 2.0 * (t4 - u) * (u - t1) - (u - t1) * (u - t1);
			b3 = (t4 - t1) * (t3 - t1) * (t2 - t1);
			if (fabs(a1) <= bfuzz)
					a1 = 0.0;
			else a1 = a1 / b1;
			if (fabs(a2) <= bfuzz)
					a2 = 0.0;
			else a2 = a2 / b2;
			if (fabs(a3) <= bfuzz)
					a3 = 0.0;
			else a3 = a3 / b3;
			rbasis = a1 + a2 + a3;
		}
		else if ((u >= t2) && (u <= t3) && (t2 != t3))
		{
			a1 = (t3 - u) * (t3 - u) - 2.0 * (u - t0) * (t3 - u);
			b1 = (t3 - t0) * (t3 - t1) * (t3 - t2);
			a2 = (t4 - u) * (t3 - u) - (t4 - u) * (u - t1) - (u - t1) * (t3 - u);
			b2 = (t4 - t1) * (t3 - t1) * (t3 - t2);
			a3 = (t4 - u) * (t4 - u) - 2.0 * (t4 - u) * (u - t2);
			b3 = (t4 - t1) * (t4 - t2) * (t3 - t2);
			if (fabs(a1) <= bfuzz)
					a1 = 0.0;
			else a1 = a1 / b1;
			if (fabs(a2) <= bfuzz)
					a2 = 0.0;
			else a2 = a2 / b2;
			if (fabs(a3) <= bfuzz)
					a3 = 0.0;
			else a3 = a3 / b3;
			rbasis = a1 + a2 + a3;
		}
		else if ((u >= t3) && (u <= t4) && (t3 != t4))
		{
			a1 = -3.0 * (t4 - u) * (t4 - u);
			b1 = (t4 - t1) * (t4 - t2) * (t4 - t3);
			if (fabs(a1) <= bfuzz)
					a1 = 0.0;
			else a1 = a1 / b1;
			rbasis = a1;
		}
		else rbasis = 0.0;
	}
	else if (order == iquadratic)  /* third order b-spline basis functions */
	{
		t0 = um_knot(i-3, max);
		t1 = um_knot(i-2, max);
		t2 = um_knot(i-1, max);
		t3 = um_knot(i, max);

		if ((u >= t0) && (u <= t1) && (t0 != t1))
		{
			a = 2.0 * (u - t0);
			b = (t2 - t0) * (t1 - t0);
			if (fabs(a) >= bfuzz)
					rbasis = a / b;
			else rbasis = 0.0;
		}
		else if ((u >= t1) && (u <= t2) && (t1 != t2))
		{
			a1 = (t2 - u) - (u - t0);
			a2 = (t3 - u) - (u - t1);
			b1 = (t2 - t0) * (t2 - t1);
			b2 = (t3 - t1) * (t2 - t1);
			if (fabs(a1) <= bfuzz)
				a1 = 0.0;
			else a1 = a1 / b1;
			if (fabs(a2) <= bfuzz)
				a2 = 0.0;
			else a2 = a2 / b2;
			rbasis = a1 + a2;
		}
		else if ((u >= t2) && (u <= t3) && (t2 != t3))
		{
			a = -2.0 * (t3 - u);
			b = (t3 - t1) * (t3 - t2);
			if (fabs(a) <= bfuzz)
					a = 0.0;
			else a = a / b;
			rbasis = a;
		}
		else rbasis = 0.0;
	}		
	else if (order == ilinear)	 /* second order b-spline basis functions */
	{
		t0 = um_knot(i - 2, max);
		t1 = um_knot(i - 1, max);
		t2 = um_knot(i, max);

		if ((u >= t0) && (u <= t1) && (t0 != t1))
		{
			a = 1.0;
			b = (t1 - t0);
			a = a / b;
			rbasis = a;
		}
		else if ((u >= t1) && (u <= t2) && (t1 != t2))
		{
			a = -1.0;
			b = (t2 - t1);
			a = a / b;
			rbasis = a;
		}
		else rbasis = 0.0;
	}
	else rbasis = 0.0;
	/* uu_dexit; */
	return(rbasis);
}

/*********************************************************************
**	E_FUNCTION: um_basis(u,i,order,max)
**
**		DESCRIPTION: Calculates the value of a normalized bspline basis 
**				function.
**		
**		PARAMETERS	
**			INPUT: 
**				u			   parameter value for evaluating the current
**								basis function; note, u is in the range
**								[0, maxknot], see "um_ev6_bsplcrv".
**
**				i			   max knot value of the support of the current
**								basis function.
**
**				order		   order of the basis function.
**
**				max		   max knot value.
**			
**			OUTPUT :  none.
**			
**		RETURNS	: value of the basis function at u.
**
**		SIDE EFFECTS : none
**
**		WARNINGS	  : none
*********************************************************************/
UU_REAL
um_basis(u,i,order,max)
	UU_REAL u;
	int i;	  /* a sort of index for the largest knot value to be*/
				  /* used in the um_basis function */
	int order;
	int max;	/* max knot value */
{
	UU_REAL t0, t1, t2, t3, t4;	 /* knot values */
	UU_REAL a1, a2, a3, b1, b2, b3;/* temporary variables */
	UU_REAL rbasis;					 /* evaluated basis function */
	UU_REAL um_knot();			  /* knot value */

	/*	uu_denter(UU_MTRC,(us,"um_basis(%g,%d,%d,%d)",u,i,order,npts));*/
	if (u > max) u = max;
	switch (order)
	{
		case 4:
			t0 = um_knot(i - 4, max);
			t1 = um_knot(i - 3, max);
			t2 = um_knot(i - 2, max);
			t3 = um_knot(i - 1, max);
			t4 = um_knot(i	, max);
			if ( (u >= t0) && (u <= t1) && (t0 != t1) )
			{
				a1 = (u  - t0) * (u  - t0) * (u  - t0);
				b1 = (t3 - t0) * (t2 - t0) * (t1 - t0);
				if (a1 < UM_FUZZ)
					rbasis = 0;
				else
					rbasis = a1 / b1;
			}
			else if ( (u >= t1) && (u <= t2) && (t1 != t2) )
			{
				a1 = (u  - t0) * (u  - t0) * (t2 - u );
				b1 = (t3 - t0) * (t2 - t0) * (t2 - t1);
				a2 = (u  - t0) * (t3 - u ) * (u  - t1);
				b2 = (t3 - t0) * (t3 - t1) * (t2 - t1);
				a3 = (t4 - u ) * (u  - t1) * (u  - t1);
				b3 = (t4 - t1) * (t3 - t1) * (t2 - t1);
				if (a1 < UM_FUZZ)
					a1 = 0;
				else
					a1 = a1 / b1;
				if (a2 < UM_FUZZ)
					a2 = 0;
				else
					a2 = a2 / b2;
				if (a3 < UM_FUZZ)
					a3 = 0;
				else
					a3 = a3 / b3;
				rbasis = a1  +  a2  +  a3;
			}
			else if ( (u >= t2) && (u  <=  t3) && (t2 != t3) )
			{
				a1 = (u  - t0) * (t3 - u ) * (t3 - u );
				b1 = (t3 - t0) * (t3 - t1) * (t3 - t2);
				a2 = (t4 - u ) * (u  - t1) * (t3 - u );
				b2 = (t4 - t1) * (t3 - t1) * (t3 - t2);
				a3 = (t4 - u ) * (t4 - u ) * (u  - t2);
				b3 = (t4 - t1) * (t4 - t2) * (t3 - t2);
				if (a1 < UM_FUZZ)
					a1 = 0;
				else
					a1 = a1 / b1;
				if (a2 < UM_FUZZ)
					a2 = 0;
				else
					a2 = a2 / b2;
				if (a3 < UM_FUZZ)
					a3 = 0;
				else
					a3 = a3 / b3;
				rbasis = a1  +  a2  +  a3;
			}
			else if ( (u >= t3) && (u <= t4) && (t3 != t4) )
			{
				a1 = (t4 - u ) * (t4 - u ) * (t4 - u );
				b1 = (t4 - t1) * (t4 - t2) * (t4 - t3);
				if (a1 < UM_FUZZ)
					rbasis = 0;
				else
					rbasis = a1 / b1;
			}
			else
				rbasis = 0;
			break;
		case 3:
			t0 = um_knot(i - 3, max);
			t1 = um_knot(i - 2, max);
			t2 = um_knot(i - 1, max);
			t3 = um_knot(i	, max);
			if ( (u >= t0) && (u <= t1) && (t0 != t1) )
			{
				a1 = (u  - t0) * (u  - t0);
				b1 = (t2 - t0) * (t1 - t0);
				if (a1 < UM_FUZZ)
					rbasis = 0;
				else
					rbasis = a1 / b1;
			}
			else if ( (u >= t1) && (u <= t2) && (t1 != t2) )
			{
				a1 = (u  - t0) * (t2 - u );
				a2 = (t3 - u ) * (u  - t1);
				b1 = (t2 - t0) * (t2 - t1);
				b2 = (t3 - t1) * (t2 - t1);
				if (a1 < UM_FUZZ)
					a1 = 0;
				else
					a1 = a1 / b1;
				if (a2 < UM_FUZZ)
					a2 = 0;
				else
					a2 = a2 / b2;
				rbasis = a1  +  a2;
			}
			else if ( (u  >= t2) && (u <= t3) && (t2 != t3) )
			{
				a1 = (t3 - u ) * (t3 - u );
				b1 = (t3 - t1) * (t3 - u );
				if (a1 < UM_FUZZ)
					rbasis = 0;
				else
					rbasis = a1 / b1;
			}
			else
				rbasis = 0;
			break;
		case 2:
			t0 = um_knot(i - 2, max);
			t1 = um_knot(i - 1, max);
			t2 = um_knot(i	, max);
			if ( (u >= t0) && (u <= t1) && (t0 != t1) )
			{
				a1 = (u  - t0);
				b1 = (t1 - t0);
				if (a1 < UM_FUZZ)
					rbasis = 0;
				else
					rbasis = a1 / b1;
			}
			else if ( (u >= t1) && (u <= t2) && (t1 != t2) )
			{
				a1 = (t2 - u );
				b1 = (t2 - t1);
				if (a1 < UM_FUZZ)
					rbasis = 0;
				else
					rbasis = a1 / b1;
			}
			else
				rbasis = 0;
			break;
		default:
			rbasis = 0;
			break;
	}
	/*	uu_dexit;*/
	return (rbasis);
}
