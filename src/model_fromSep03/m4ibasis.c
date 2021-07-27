
/*********************************************************************
**    NAME         :  m4ibasis.c
**       CONTAINS:
**			um_mergeknots
**			um_bsplbasis
**			um_rbsplcrvorder
**			int um_rbsplc_reparm(r1ptr,u0,u1,r2ptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m4ibasis.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:04
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "uhep.h"
#include "udebug.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mcrv.h"
#include "mdmatrix.h"
#include "modef.h"

#define UM_MAXORDER 8

/*********************************************************************
**    E_FUNCTION     : um_mergeknots(mul,no_t1,t1,no_t2,t2,no_t,t);
**			Merge two knot vectors T1 and T2 making sure that no knot
**			appears with multiplicity greater than MUL.
**    PARAMETERS   
**       INPUT  : 
**          mul							desired multiplicity
**				no_t1							number of knots in t1 
**				t1								knot vector 1
**				no_t2							number of knots in t2
**				t2								knot vector 2
**       OUTPUT :  
**          no_t							number of knots in result
**				t								merged knot vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_mergeknots(mul,no_t1,t1,no_t2,t2,no_t,t)
	int mul;
	int no_t1;
	UU_REAL t1[];
	int no_t2;
	UU_REAL t2[];
	int *no_t;
	UU_REAL t[];

	{
	int i,j,k;
	int i1;
	int i2;
	int add;
	UU_REAL diff;
	int no_new;
	UU_LOGICAL equalparam;
	UU_LOGICAL skip;
	UU_LOGICAL move;

	uu_denter(UU_MTRC,(us,"um_mergeknots(%d,%d,%x,%d,%x,,)",k,no_t1,t1,
		no_t2,t2));
	i = 0; i1 = 0; i2 = 0;
	while ((i1<no_t1) && (i2<no_t2))
		{
		if (i1 == no_t1) add = 2;
		else if (i2 == no_t2) add = 1;
		else
			{
			diff = t1[i1] - t2[i2];
			if (fabs(diff) < UM_EQPARM) add = 3;
			else if (diff < 0.0) add = 1;
			else add = 2;
			}
		if ((add == 1) || (add == 3))
			{
			t[i] = t1[i1];
			i++;
			i1++;
			}
		if ((add == 2) || (add == 3))
			{
			t[i] = t2[i2];
			i++;
			i2++;
			}
		}
	no_new = 1;
	for (j=1,k=1; j<i; j++)
		{
		equalparam = (fabs(t[j] - t[k-1]) < UM_EQPARM);
		if (equalparam) no_new = no_new + 1; else no_new = 1;
		skip = (equalparam) && (no_new > mul);
		if (!skip)
			{
			t[k] = t[j];
			k++;
			}
		}
	*no_t = k;
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int um_bsplbasis(uu,k,n,t,i,b)
**			Evaluate the K (order) (possibly) non-zero bspline
**			basis functions for the specified knot vector T of
**			N values at a value X. Return the index of the lowest
**			basis function and the K values
**
**					 B[I,K,X] ... B[I+K-1,K,X]
**
**    PARAMETERS   
**       INPUT  : 
**				x								parameter value
**				k								order of bspline
**				n								number of knot values
**				t								knot vector
**       OUTPUT :  
**          i								lowest index of (possibly) non-zero
**												bspline basis for given x and knots t
**				b								B[I,K,X]...B[I+K-1,K,X]
**    RETURNS      : 
**				um_bsplbasis				zero iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
**		NOTES:
**
**			Reference:	Carl de Boor, "A Practical Guide to Splines",
**								pp 132-135
*********************************************************************/
int
um_bsplbasis(x,k,n,t,i,b)
	UU_REAL x;
	int k;
	int n;
	UU_REAL t[];
	int *i;
	UU_REAL b[];

	{
	UU_REAL dr[UM_MAXORDER];
	UU_REAL dl[UM_MAXORDER];
	UU_REAL saved;
	UU_REAL term;
	UU_REAL basis;
	int mu;
	int j,r;
	int status;
	UU_LOGICAL fromright;

	uu_denter(UU_MTRC,(us,"um_basis(%d,%f,%x)",k,x,t));
	status = 0;
	um_knotindex(n-k+1, t, x, &mu);
	if ((t[mu+1] - t[mu]) < UM_FUZZ) mu--;
	b[0] = 1.0;
	for (j=0; j<(k-1); j++)
		{
		dr[j] = t[mu+j+1] - x;
		dl[j] = x - t[mu-j];
		saved = 0.0;
		for (r=0; r<=j; r++)
			{
			term = b[r] / (dr[r] + dl[j-r]);
			b[r] = saved + (dr[r]*term);
			saved = dl[j-r]*term;
			}
		b[j+1] = saved;
		}
	*i = mu - k + 1;
/*
	um_p_ary(UM_PFLOAT,"  dr",k-1,dr);
	um_p_ary(UM_PFLOAT,"  dl",k-1,dl);
	um_p_ary(UM_PFLOAT,"  b",k,b);
	um_p_ary(UM_PINT,"  i",1,i);
*/
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_rbsplcrvorder(r1ptr,k2,r2ptr)
**       Determine a rational bspline curve (R2PTR) which represents
**			the same curve as R1PTR but which if of a greater order (K2).
**    PARAMETERS   
**       INPUT  : 
**          r1ptr						original rational bspline
**				k2							order for new rational bspline
**       OUTPUT :  
**				r2ptr						new rational bspline
**    RETURNS      : 
**				0							zero if no error
**				-1							if K2 <= R1PTR.K
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_rbsplcrvorder(r1ptr,k2,r2ptr)
	struct UM_rbsplcrv_rec *r1ptr;
	int k2;
	struct UM_rbsplcrv_rec *r2ptr;

	{
	struct UM_matrix bmat;
	UU_REAL b1[UM_MAXORDER];
	UU_REAL b2[UM_MAXORDER];
	UU_REAL b[50][50];
	UU_REAL mat[300];
	UU_REAL rhs[50][4];
	UU_REAL coeff[50][4];
	UU_REAL temp[3];
	UU_REAL x,xinc;
	UU_REAL eps = 1.0e-4;
	UU_REAL determinant;
	UU_REAL diff;
	int status;
	int i,j,k,lasti,index;
	int no_t2;
	int n2;
	int mu;
	int inc;

	uu_denter(UU_MTRC,(us,"um_rbsplcrvorder(%x,%x)",r1ptr,r2ptr));
	inc = k2 - r1ptr->k;
	if (inc <= 0)
		status = -1;
	else
		{
		status = 0;

		/* calculate new knot values by increasing the multiplicity of
			each knot by (k2 - r1ptr->k) but keeping the multiplicity
			less than k2 */

		no_t2 = 0;
		lasti = r1ptr->no_t-1;
		r2ptr->t0 = r1ptr->t0;
		r2ptr->t1 = r1ptr->t1;
		for (i=1; i<=lasti; i++)
			{
			diff = fabs(r1ptr->t[i] - r1ptr->t[i-1]);
			if (diff < UM_FUZZ)
				{/* copy old knot value */
				r2ptr->t[no_t2] = r1ptr->t[i-1];
				no_t2++;
				}
			if ((diff >= UM_FUZZ) || (i==lasti))
				{/* increase all old values by inc multiplicity */
				for (j=0; j<=inc; j++,no_t2++) r2ptr->t[no_t2] = r1ptr->t[i-1];
				}
			}
		r2ptr->k = k2;
		r2ptr->no_t = no_t2;
		r2ptr->n = no_t2 - (2*k2) + 1;

		/* calculate new control polygon by solving the following system
			of equations:

				 n2									n1
				sum2 (b2[i,k2,t] * pt2[i]) = sum1 (b1[i,k1,t] * pt1[i])
				 0										0
			or
				B*COEFF = RHS

			where b1 and b2 are the respective bspline basis functions
			for n2 values of t.
		*/

		n2 = r2ptr->n + r2ptr->k - 1;
		x = r2ptr->t0;
		xinc = (r2ptr->t1 - r2ptr->t0) / (n2-1);
		for (i=0; i<n2; i++)
			{/* calculate RHS */
			um_bsplbasis(x,r1ptr->k,r1ptr->no_t,r1ptr->t,&mu,b1);
			um_vctovc(UM_zerovec,rhs[i]);
			rhs[i][3] = 0;
			for (j=0,index=3*mu; j<r1ptr->k; j++,index=index+3)
				{
				um_vctmsc(&r1ptr->pt[index],b1[j],temp);
				um_vctmsc(temp,r1ptr->wt[mu+j],temp);
				um_vcplvc(temp,rhs[i],rhs[i]);
				rhs[i][3] = rhs[i][3] + (r1ptr->wt[mu+j] * b1[j]);
				}
			x = x + xinc;
			}

		x = r1ptr->t0;
		for (i=0; i<n2; i++)
			{/* calculate the B matrix */
			um_bsplbasis(x,r2ptr->k,r2ptr->no_t,r2ptr->t,&mu,b2);
			for (j=0; j<=mu; j++) b[i][j] = 0.0;
			for (j=0; j<k2; j++) b[i][mu+j] = b2[j];
			for (j=mu+k2; j<n2; j++) b[i][j] = 0.0;
			x = x + xinc;
			}

		bmat.nrow = n2;
		bmat.ncol = n2+4;
		bmat.ary = mat;
		r2ptr->no_pt = r2ptr->n + r2ptr->k - 1;
		r2ptr->no_wt = r2ptr->n + r2ptr->k - 1;
		for (i=0,index=0; i<n2; i++)
			{/* use temporay matrix (destroyed by um_eval_matrix) */
			for (k=0; k<n2; k++,index++)
				bmat.ary[index] = b[i][k];
			for (k=0; k<4; k++,index++)
				bmat.ary[index] = rhs[i][k];
			}
		status = um_eval_matrix(UM_SOLVEQNS,eps,&bmat,coeff,&determinant);

		for (j=0; j<3; j++)
			{
			for (i=0,index=j; i<n2; i++,index=index+3)
				{
				r2ptr->pt[index] = coeff[i][j]/coeff[i][3];
				r2ptr->wt[i] = coeff[i][3];
				}
			}
		}
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_rbsplc_reparm(r1ptr,u0,u1,r2ptr)
**       Determine a rational bspline curve (R2PTR) which represents
**			the same curve as R1PTR but which is defined over a new 
**			parameter range [u0,u1].
**    PARAMETERS   
**       INPUT  : 
**          r1ptr						original rational bspline
**				u0							new starting parameter
**				u1							new ending parameter
**       OUTPUT :  
**				r2ptr						new rational bspline
**    RETURNS      : 
**				0							zero if no error
**				-1							otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_rbsplc_reparam(r1ptr,u0,u1,r2ptr)
	struct UM_rbsplcrv_rec *r1ptr;
	UU_REAL u0;
	UU_REAL u1;
	struct UM_rbsplcrv_rec *r2ptr;

	{
	struct UM_matrix bmat;
	UU_REAL b1[UM_MAXORDER];
	UU_REAL b2[UM_MAXORDER];
	UU_REAL b[50][50];
	UU_REAL mat[300];
	UU_REAL rhs[50][4];
	UU_REAL coeff[50][4];
	UU_REAL temp[3];
	UU_REAL x,xinc;
	UU_REAL eps = 1.0e-4;
	UU_REAL determinant;
	UU_REAL a1,a2;
	int status;
	int i,j,k,lasti,index;
	int n2;
	int k2;
	int mu;
	int inc;

	uu_denter(UU_MTRC,(us,"um_rbsplc_reparam(%x,%f,%f,%x)",r1ptr,
			u0,u1,r2ptr));
	if (u0 >= u1)
		status = -1;
	else
		{
		status = 0;
		r2ptr->k = r1ptr->k;
		r2ptr->n = r1ptr->n;
		r2ptr->t0 = u0;
		r2ptr->t1 = u1;
		r2ptr->no_t = r1ptr->no_t;
		a1 = (u0 - u1) / (r1ptr->t0 - r1ptr->t1);
		a2 = u1 - (a1*r1ptr->t1);
		for (i=0; i<r2ptr->no_t; i++) r2ptr->t[i] = (a1*r1ptr->t[i]) + a2;

		/* calculate new control polygon by solving the following system
			of equations:

				 n2									n1
				sum2 (b2[i,k2,t] * pt2[i]) = sum1 (b1[i,k1,t] * pt1[i])
				 0										0
			or
				B*COEFF = RHS

			where b1 and b2 are the respective bspline basis functions
			for n2 values of t.
		*/

		n2 = r2ptr->n + r2ptr->k - 1;
		k2 = r2ptr->k;
		x = r1ptr->t0;
		xinc = (r1ptr->t1 - r1ptr->t0) / (n2 - 1);
		for (i=0; i<n2; i++)
			{/* calculate RHS */
			um_bsplbasis(x,r1ptr->k,r1ptr->no_t,r1ptr->t,&mu,b1);
			um_vctovc(UM_zerovec,rhs[i]);
			rhs[i][3] = 0;
			for (j=0,index=3*mu; j<r1ptr->k; j++,index=index+3)
				{
				um_vctmsc(&r1ptr->pt[index],b1[j],temp);
				um_vctmsc(temp,r1ptr->wt[mu+j],temp);
				um_vcplvc(temp,rhs[i],rhs[i]);
				rhs[i][3] = rhs[i][3] + (r1ptr->wt[mu+j] * b1[j]);
				}
			x = x + xinc;
			}

		x = r2ptr->t0;
		xinc = (r2ptr->t1 - r2ptr->t0) / (n2 - 1);
		for (i=0; i<n2; i++)
			{/* calculate the B matrix */
			um_bsplbasis(x,r2ptr->k,r2ptr->no_t,r2ptr->t,&mu,b2);
			for (j=0; j<=mu; j++) b[i][j] = 0.0;
			for (j=0; j<k2; j++) b[i][mu+j] = b2[j];
			for (j=mu+k2; j<n2; j++) b[i][j] = 0.0;
			x = x + xinc;
			}

		bmat.nrow = n2;
		bmat.ncol = n2+4;
		bmat.ary = mat;
		r2ptr->no_pt = r2ptr->n + r2ptr->k - 1;
		r2ptr->no_wt = r2ptr->n + r2ptr->k - 1;
		for (i=0,index=0; i<n2; i++)
			{/* use temporay matrix (destroyed by um_eval_matrix) */
			for (k=0; k<n2; k++,index++)
				bmat.ary[index] = b[i][k];
			for (k=0; k<4; k++,index++)
				bmat.ary[index] = rhs[i][k];
			}
		status = um_eval_matrix(UM_SOLVEQNS,eps,&bmat,coeff,&determinant);

		for (j=0; j<3; j++)
			{
			for (i=0,index=j; i<n2; i++,index=index+3)
				{
				r2ptr->pt[index] = coeff[i][j]/coeff[i][3];
				r2ptr->wt[i] = coeff[i][3];
				}
			}
		}
	uu_dexit;
	return (status);
	}
