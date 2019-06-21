/*********************************************************************
**    NAME         :  m3icn2.c
**				Support for creating conics sections from quadratic
**				equations arising from geometric constraints
**       CONTAINS:
**			um_normquad()	-	turns six coefficients into conic entity
**			um_rotquad()	-	effects rotation on cooeficients
**			um_tranquad()	-	effects translation on cooeficients
**			um_copyquad()	-	copies moves an array of six UU_REALs
**			um_quadtype()	-	returns the type of curve given by coefficients
**			--	NEW	--
**			um_lntmln()		-	multiplies eqns. for lines to get a quadratic
**			um_quadev()		-	evaluates a quadratic equation at (x,y)
**			um_quadtmsc()	-	quadratic times scalar
**			um_quadplquad()-	sum of quadratics
**			um_ptpttoln()	-	line coefficient array from two points
**			um_tgtoln()		-	line coeffs. from point/tangent
**			um_cnstrconic()-	construct conic from five constraints
**			UU_LOGICAL umi_cn4_closed(eptr)
**
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m3icn2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:56
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mcrv.h"
#include "modef.h"
#include "mdebug.h"

#define	UU_MAX(A,B) ((A)>(B)?(A):(B))
#define	UU_MIN(A,B) ((A)<(B)?(A):(B))

#define	UM_MTRACE	1

/*********************************************************************
**    I_FUNCTION :  um_normquad(S, eptr, tfmat)
**       determines conic entity defined by six coefficients
**    PARAMETERS   
**       INPUT  : 
**          S[6]	-	coefficients of two-variable quadratic
**				S[0]XX + S[1]XY + S[2]YY + S[3]X + S[4]Y + S[5] = 0
**			
**       OUTPUT :  
**          eptr	-	entity record is filled in with geometry,
**							including type (if error, type = UM_CN_UNKNOWN)
**							NOTE: endpoints are not handled here.
**				tfmat	-	this transformation will be COMPOSED on the left
**							by (the inverse of) the transformation used to
**							normalize the quadratic.
**    RETURNS      : 0 only if OK, if not, eptr->type will be UM_CN_UNKNOWN
**    SIDE EFFECTS : none
**    WARNINGS     : NOTE WELL that the tranformation tfmat must have
**							meaning before calling, since the result is a
**							*composition* with the existing matrix.
**							FIX: there will need to be more handling for the
**							determination of the branch of the hyperbola
*********************************************************************/
int
um_normquad(S, eptr, tfmat)
	UU_REAL	S[6];
	struct	UM_conic_rec	*eptr;
	UM_transf	tfmat;
	{
	UU_REAL	dx,	dy;
	UU_REAL	diff;
	UU_REAL	theta;

	/* STRATEGY:
	 * Using standard congruences, we first rotate
	 * to eliminate the cross term, S[1], Then translate to
	 * remove the linear term(s).
	 *
	 * Special handling is necessary in some cases because:
	 *		- You can't eliminate both linear terms from
	 *		  a parabola, but the constant term must go
	 *		- another quarter or half circle rotation might
	 *		  be needed to make hyperbolas and parabolas open
	 *		  to the right, and ellipse major axis must align
	 *		  with x-axis
	 */

	um_pscroll("um_normquad: original quadric");
	um_p_ary(UM_PFLOAT, "  quad=", 6, S);
	um_p_ary(UM_PFLOAT, "  tfmat[0]", 3, tfmat[0]);
	um_p_ary(UM_PFLOAT, "  tfmat[1]", 3, tfmat[1]);
	um_p_ary(UM_PFLOAT, "  tfmat[2]", 3, tfmat[2]);
	um_p_ary(UM_PFLOAT, "  tfmat[3]", 3, tfmat[3]);

	if ((eptr->type = um_quadtype(S)) == UM_CN_UNKNOWN)
		{
		return(-1);
		}

	/* Rotate to remove cross term */
	diff = S[2]-S[0];
	if (!UM_ZILCH(diff))
		theta = 0.5 * atan( S[1]/diff);
	else
		theta = -0.5 * UM_HALFPI;
	um_rotquad(S, theta, tfmat);
	um_pscroll("cross term removed");
	um_p_ary(UM_PFLOAT, "  quad=", 6, S);
	um_p_ary(UM_PFLOAT, "  tfmat[0]", 3, tfmat[0]);
	um_p_ary(UM_PFLOAT, "  tfmat[1]", 3, tfmat[1]);
	um_p_ary(UM_PFLOAT, "  tfmat[2]", 3, tfmat[2]);
	um_p_ary(UM_PFLOAT, "  tfmat[3]", 3, tfmat[3]);

	/* Translate, and align canonically	*/

	if (eptr->type == UM_PARABOLA)
		{
		UU_REAL	m;

		if (UM_ZILCH(S[0]))	/* opens left or right	*/
			{	/* S[2], S[3] should be non-zero	*/
/*
			dy = S[4]/(2*S[2]);
			dx = (S[5] + S[4]*dy)/S[3];
*/
			dy = S[4]/(2*S[2]);
			dx = (S[5] - S[4]*dy/2)/S[3];
			um_tranquad(S, dx, dy, tfmat);
			um_pscroll("opens left/right; translated");
			um_p_ary(UM_PFLOAT, "  quad=", 6, S);
			um_p_ary(UM_PFLOAT, "  tfmat[0]", 3, tfmat[0]);
			um_p_ary(UM_PFLOAT, "  tfmat[1]", 3, tfmat[1]);
			um_p_ary(UM_PFLOAT, "  tfmat[2]", 3, tfmat[2]);
			um_p_ary(UM_PFLOAT, "  tfmat[3]", 3, tfmat[3]);

			/* Rotate if necessary	*/
			m = S[2]/S[3];
			if (m > 0)	/* opens to the left	*/
				{
				um_rotquad(S, UM_PI, tfmat);
				um_pscroll("opens left; rotated pi");
				um_p_ary(UM_PFLOAT, "  quad=", 6, S);
				um_p_ary(UM_PFLOAT, "  tfmat[0]", 3, tfmat[0]);
				um_p_ary(UM_PFLOAT, "  tfmat[1]", 3, tfmat[1]);
				um_p_ary(UM_PFLOAT, "  tfmat[2]", 3, tfmat[2]);
				um_p_ary(UM_PFLOAT, "  tfmat[3]", 3, tfmat[3]);
				}
			}
		else	/* opens up or down	*/
			{ 			/* S[4] should also not be zero	*/
/*
			dx = S[3]/(2*S[0]);
			dy = (S[5] + S[3]*dx)/S[4];
*/
			dx = S[3]/(2*S[0]);
			dy = (S[5] - S[3]*dx/2)/S[4];
			um_tranquad(S, dx, dy, tfmat);
			um_pscroll("opens up/down; translated");
			um_p_ary(UM_PFLOAT, "  quad=", 6, S);
			um_p_ary(UM_PFLOAT, "  tfmat[0]", 3, tfmat[0]);
			um_p_ary(UM_PFLOAT, "  tfmat[1]", 3, tfmat[1]);
			um_p_ary(UM_PFLOAT, "  tfmat[2]", 3, tfmat[2]);
			um_p_ary(UM_PFLOAT, "  tfmat[3]", 3, tfmat[3]);

			/* Rotate if necessary	*/
			m = S[0]/S[4];
			if (m > 0)	/* opens to the bottom	*/
				{
				um_rotquad(S, UM_HALFPI, tfmat);
				um_pscroll("opens down; rotated pi/2");
				um_p_ary(UM_PFLOAT, "  quad=", 6, S);
				um_p_ary(UM_PFLOAT, "  tfmat[0]", 3, tfmat[0]);
				um_p_ary(UM_PFLOAT, "  tfmat[1]", 3, tfmat[1]);
				um_p_ary(UM_PFLOAT, "  tfmat[2]", 3, tfmat[2]);
				um_p_ary(UM_PFLOAT, "  tfmat[3]", 3, tfmat[3]);
				}
			else			/* opens to the top	*/
				{
				um_rotquad(S, -UM_HALFPI, tfmat);
				um_pscroll("opens up; rotated -pi/2");
				um_p_ary(UM_PFLOAT, "  quad=", 6, S);
				um_p_ary(UM_PFLOAT, "  tfmat[0]", 3, tfmat[0]);
				um_p_ary(UM_PFLOAT, "  tfmat[1]", 3, tfmat[1]);
				um_p_ary(UM_PFLOAT, "  tfmat[2]", 3, tfmat[2]);
				um_p_ary(UM_PFLOAT, "  tfmat[3]", 3, tfmat[3]);
				}
			}
		eptr->invariants[0] = fabs(m);
		eptr->invariants[1] = 0;

		}
	else	/* Hyperbola or Ellipse	*/
		{
		UU_REAL	d0, d2;		/* denominators of squared terms	*/

		dx = S[3]/(2*S[0]);	/* remove both linear terms	*/
		dy = S[4]/(2*S[2]);
		um_tranquad(S, dx, dy, tfmat);
		um_pscroll("hyp/ell:remove both linear terms");
		um_p_ary(UM_PFLOAT, "  quad=", 6, S);
		um_p_ary(UM_PFLOAT, "  tfmat[0]", 3, tfmat[0]);
		um_p_ary(UM_PFLOAT, "  tfmat[1]", 3, tfmat[1]);
		um_p_ary(UM_PFLOAT, "  tfmat[2]", 3, tfmat[2]);
		um_p_ary(UM_PFLOAT, "  tfmat[3]", 3, tfmat[3]);

		/* rotate if necessary	*/
		d0 = S[5]/S[0];
		d2 = S[5]/S[2];

		if ( eptr->type == UM_HYPERBOLA )
			{
			if (d2  < 0)	/* e.g. xx - yy + 1 = 0		*/
				{
				um_rotquad(S, UM_HALFPI, tfmat);	/* may be -pi/2, pi	*/
				um_pscroll("hyperbola: rotate pi/2");
				um_p_ary(UM_PFLOAT, "  quad=", 6, S);
				um_p_ary(UM_PFLOAT, "  tfmat[0]", 3, tfmat[0]);
				um_p_ary(UM_PFLOAT, "  tfmat[1]", 3, tfmat[1]);
				um_p_ary(UM_PFLOAT, "  tfmat[2]", 3, tfmat[2]);
				um_p_ary(UM_PFLOAT, "  tfmat[3]", 3, tfmat[3]);
				eptr->invariants[0] = sqrt(fabs(d2));
				eptr->invariants[1] = sqrt(fabs(d0));
				}
			else
				{
				eptr->invariants[0] = sqrt(fabs(d0));
				eptr->invariants[1] = sqrt(fabs(d2));
				}

			}
		else	/* eptr->type == UM_ELLIPSE	*/
			{
			if (S[0] < S[1])	/* off by pi/2	*/
				{
				um_rotquad(S, UM_HALFPI, tfmat);
				um_pscroll("ellipse: rotate pi/2");
				um_p_ary(UM_PFLOAT, "  quad=", 6, S);
				um_p_ary(UM_PFLOAT, "  tfmat[0]", 3, tfmat[0]);
				um_p_ary(UM_PFLOAT, "  tfmat[1]", 3, tfmat[1]);
				um_p_ary(UM_PFLOAT, "  tfmat[2]", 3, tfmat[2]);
				um_p_ary(UM_PFLOAT, "  tfmat[3]", 3, tfmat[3]);
				eptr->invariants[0] = sqrt(fabs(d2));
				eptr->invariants[1] = sqrt(fabs(d0));
				}
			else
				{
				eptr->invariants[0] = sqrt(fabs(d0));
				eptr->invariants[1] = sqrt(fabs(d2));
				}
			}
		}
	um_p_ary(UM_PFLOAT, "  final quad=", 6, S);
	return(0);
	}
/*********************************************************************
**    I_FUNCTION :  um_rotquad(A, theta, tfmat)
**       apply a normalizing rotation in x,y-plane to the coefficients
**			of a quadratic equation.
**    PARAMETERS   
**       INPUT  : 
**          A		-	coefficient matrix (in variable X)	
**				theta	-	will get coords in U = X*Rot(+theta)
**       OUTPUT :  
**          A		-	coefficient matrix (after rotation)
**          tfmat	-	transformation is composed to == Rot(-theta) * tfmat
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

um_rotquad(A, theta, tfmat)
	UU_REAL	A[6];
	UU_REAL	theta;
	UM_transf	tfmat;
	{
	UU_REAL	temp[6];
	UU_REAL	c, s;
	UU_REAL	diff;
	UU_REAL	a00, a01, a02, a10, a11, a12;	/* temps for ULC of tfmat	*/

	theta = -theta;

	if (UM_ZILCH(theta))
		return;
	c = cos(theta);
	s = sin(theta);

	/* coefficient array	*/
	temp[0] = A[0]*c*c + A[1]*s*c + A[2]*s*s;
	temp[1] = 2*(A[2]-A[0])*s*c + A[1]*(c*c - s*s);
	temp[2] = A[0]*s*s - A[1]*s*c + A[2]*c*c;
	temp[3] = A[3]*c + A[4]*s;
	temp[4] = -A[3]*s + A[4]*c;
	temp[5] = A[5];
	um_copyquad(temp, A);

	/* transformation	(watch those minus signs)	*/
	a00 = c * tfmat[0][0] + s * tfmat[1][0];
	a01 = c * tfmat[0][1] + s * tfmat[1][1];
	a02 = c * tfmat[0][2] + s * tfmat[1][2];
	a10 = c * tfmat[1][0] - s * tfmat[0][0];
	a11 = c * tfmat[1][1] - s * tfmat[0][1];
	a12 = c * tfmat[1][2] - s * tfmat[0][2];

	tfmat[0][0] = a00;
	tfmat[0][1] = a01;
	tfmat[0][2] = a02;
	tfmat[1][0] = a10;
	tfmat[1][1] = a11;
	tfmat[1][2] = a12;

	}

/*********************************************************************
**    I_FUNCTION :  um_tranquad(A, dx, dy, tfmat)
**			Given a quadratic form A(x,y), determine the equivalent
**			quadratic form AP(xp,yp) where 
**				1. xp = x - dx
**				2. yp = y - dy
**    PARAMETERS   
**       INPUT  : 
**          A		-	coefficient array
**				dx, dy-	translation offset
**				tfmat	-	transformation to be composed with translation
**       OUTPUT :  
**				A		-	the equivalent quadratic
**          tfmat	-	composed on left by inverse of translation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

um_tranquad(A, dx, dy, tfmat)
	UU_REAL	A[6];
	UU_REAL	dx,	dy;
	UM_transf	tfmat;

	{
	uu_denter(UU_MTRC,(us,"um_tranquad(dx=%g, dy=%g)", dx, dy));

	dx= -dx;
	dy= -dy;

/*
	A[5] += A[3]*dx + A[4]*dy + A[0]*dx*dx + A[2]*dy*dy;
	A[4] += 2*A[2]*dy;
	A[3] += 2*A[0]*dx;
*/
	A[3] = A[3] + 2*A[0]*dx + A[1]*dy; 
	A[4] = A[4] + 2*A[2]*dy + A[1]*dx;
	A[5] = A[0]*dx*dx + A[1]*dx*dy + A[2]*dy*dy + A[3]*dx + A[4]*dy + A[5];

	/* must multiply on left by translation matrix	*/
	tfmat[3][0] +=	dx*tfmat[0][0] + dy*tfmat[1][0];
	tfmat[3][1] +=	dx*tfmat[0][1] + dy*tfmat[1][1];
	tfmat[3][2] +=	dx*tfmat[0][2] + dy*tfmat[1][2];

	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION :  um_copyquad(a, b)
**       copy coefficient array
**    PARAMETERS   
**       INPUT  : 
**          a		-	array
**       OUTPUT :  
**          b		- copy of a
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

um_copyquad(a, b)
UU_REAL	*a, *b;
	{
	register	int	i = 5;
	do 	{
		*(b+i) = *(a+i);
		}	while (i--);
	}

/*********************************************************************
**    I_FUNCTION :  int	um_quadtype(A)
**       returns type of conic section defined by coefficient array A
**    PARAMETERS   
**       INPUT  : 
**          A		-	array of coefficients
**    RETURNS      : type, or UM_CN_UNKNOWN, if the equation
**							does not define a real (and robust) conic
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

	int
um_quadtype(A)
	UU_REAL	A[6];
	{
	int	type;
	UU_REAL	Q1,	Q2,	Q3;	/* (sub-)discriminant	*/

	/*
	 * Reference: IGES standards document
	 */
	Q1 = .25 * (4*A[0]*A[2]*A[5] - A[0]*A[4]*A[4] - A[1]*A[1]*A[5]
					- A[2]*A[3]*A[3] + A[1]*A[3]*A[4]);
	Q2 = A[0]*A[2] - (A[1]*A[1]/4);
	Q3 = A[0] + A[2];

	if (Q2 > UM_FUZZ)
		type =  (Q1*Q3 < -UM_FUZZ)? UM_ELLIPSE: UM_CN_UNKNOWN;
	else if (Q2 < -UM_FUZZ)
		type = (!UM_ZILCH(Q1))? UM_HYPERBOLA: UM_CN_UNKNOWN;
	else		/* Q1 approx == 0	*/
		type = (!UM_ZILCH(Q1))? UM_PARABOLA: UM_CN_UNKNOWN;
	
	return(type);
	}
/*********************************************************************
**    I_FUNCTION :  um_lntmln(l1, l2, S)
**			multiplies eqns. for lines to get a quadratic
**    PARAMETERS   
**       INPUT  : 
**          l1, l2	coefficient arrays of lines
**       OUTPUT :  
**				S 			coefficient array of resulting quadratic
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_lntmln(l1, l2, S)
	UU_REAL	l1[3];
	UU_REAL	l2[3];
	UU_REAL	S[6];
	{
	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_lntmln()"));

	S[0] = l1[0] * l2[0];
	S[1] = l1[0] * l2[1] + l1[1] * l2[0];
	S[2] = l1[1] * l2[1];
	S[3] = l1[0] * l2[2] + l1[2] * l2[0];
	S[4] = l1[1] * l2[2] + l1[2] * l2[1];
	S[5] = l1[2] * l2[2];

	uu_dexit;
	}
/*********************************************************************
**    I_FUNCTION :  um_quadev(p, S)
**			evaluates a quadratic equation at (x,y)
**    PARAMETERS   
**       INPUT  : 
**          p		-	point at which to evaluate (z-coefficient ignored)
**          S		-	coefficient array for quadratic
**       OUTPUT :  
**    RETURNS      : UU_REAL value of quadratic evaluated at (x,y) of p.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
	UU_REAL
um_quadev(p, S)
	UM_coord	p;
	UU_REAL	S[6];
	{
	UU_REAL	value = 0.0;
	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_quadev()"));

	value  = S[0] * p[0] * p[0];
	value += S[1] * p[0] * p[1];
	value += S[2] * p[1] * p[1];
	value += S[3] * p[0];
	value += S[4] * p[1];
	value += S[5];

	uu_dexit;
	return(value);
	}
/*********************************************************************
**    I_FUNCTION :  um_quadtmsc(S, k, T)
**			quadratic times scalar
**    PARAMETERS   
**       INPUT  : 
**          S		-	coefficient array of quadratic
**				k		-	UU_REAL scalar
**       OUTPUT :  
**          T		-	resultant quadratic after multiplication (may == S)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_quadtmsc(S, k, T)
	UU_REAL	S[6];
	UU_REAL	k;
	UU_REAL	T[6];
	{
	int	i;

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_quadtmsc()"));

	for( i = 0; i < 6; i++)
		{
		T[i] = k * S[i];
		}
	

Done:
	uu_dexit;
	}
/*********************************************************************
**    I_FUNCTION :  um_quadplquad(S1, S2, T)
**			sum of quadratics
**    PARAMETERS   
**       INPUT  : 
**          S1, S2	-	two quadratic coefficient arrays to be added
**       OUTPUT :  
**          T			-	the sum of S1 and S2 (may == either S1 or S2)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_quadplquad(S1, S2, T)
	UU_REAL	S1[6];
	UU_REAL	S2[6];
	UU_REAL	T[6];
	{
	int i;

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_quadplquad()"));

	for (i = 0; i < 6; i++)
		{
		T[i] = S1[i] + S2[i];
		}

Done:
	uu_dexit;
	}
/*********************************************************************
**    I_FUNCTION :  um_ptpttoln(p1, p2, l)
**			line coefficient array from two points
**    PARAMETERS   
**       INPUT  : 
**          p1, p2	-	two points (3rd coord ignored)
**       OUTPUT :  
**          l			-	coefficient array for line through p1 and p2
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : It is assumed that the points are not too near
*********************************************************************/
um_ptpttoln(p1, p2, l)
	UM_coord	p1;
	UM_coord	p2;
	UU_REAL	l[3];
	{
	UU_REAL	dx;	/* delta x	*/
	UU_REAL	dy;

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_ptpttoln()"));
	
	dx = p1[0] - p2[0];
	dy = p1[1] - p2[1];
	l[0] = dy;
	l[1] = -dx;
	l[2] = -p2[0] * dy + p2[1] *  dx;

Done:
	uu_dexit;
	}
/*********************************************************************
**    I_FUNCTION :  um_tgtoln(tg, l)
**			line coefficient array from point/vector condition
**    PARAMETERS   
**       INPUT  : 
**          tg			-	point and tangent (UU_REAL	tg[2][3])
**							tg[0] is the point
**							tg[1] is the vector
**       OUTPUT :  
**          l			-	coefficient array for line through point/tgt
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : It is assumed that the vector is not too small
*********************************************************************/
um_tgtoln(tg, l)
	UU_REAL	tg[2][3];
	UU_REAL	l[3];
	{
	UU_REAL	dx;	/* delta x	*/
	UU_REAL	dy;

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_tgtoln()"));
	
	dx = tg[1][0];
	dy = tg[1][1];

	l[0] = dy;
	l[1] = -dx;
	l[2] = -tg[0][0] * dy + tg[0][1] *  dx;

Done:
	uu_dexit;
	}
/*********************************************************************
**    I_FUNCTION :  um_cnstrconic(eptr, numtgt, tgts, numpts, pts, errp)
**       creates conic entity from tangent and point constraints
**    PARAMETERS   
**       INPUT  : 
**          numtgt		-	number of tangent constraints supplied
**				tgts			-	list of tangent constraints
**				numpts		-	number of point constraints
**				pts			-	list of point constraints
**				eptr			-	pointer to conic record.  NOTE: eptr->tfmat
**									MUST be filled in with definition space to 
**									model space transformation before calling
**			NOTE:	constraints must be mapped into definition space
**			(by the inverse of eptr->tfmat, normally) before calling
**       OUTPUT :  
**          *eptr			-	conic entity will be filled in 
**    RETURNS      : 0 if conic OK, otherwise error from um_normquad
**							or -1 if conic can't be created.
**    SIDE EFFECTS : 'type' field of *eptr will be UM_CN_UNKNOWN if error
**    WARNINGS     : numtgt + numpts must equal 5
*********************************************************************/
um_cnstrconic(eptr, numtgt, tgts, numpts, pts)
	struct	UM_conic_rec	*eptr;
	int		numtgt;
	UU_REAL	tgts[2][2][3];
	int		numpts;
	UM_coord	pts[5];

	{
	UU_REAL	lines[4][3];
								/* each lines[i][] is a coefficient array for
								* a line in 2-space
								*/
	UU_REAL	quad1[6];	
	UU_REAL	quad2[6];
								/* two quadratic coefficient arrays.  Used as 
								 * basis for a pencil of conics, one of which
								 *	is the one with the fifth constraint
								 */
	UU_REAL	lambda;
								/* parameter of pencil of conics				*/
	UU_REAL	temp1;
	int		ret_val;
	UU_REAL um_quadev();
	UM_transf invtfmat;
	UM_coord testpt;

	/** STRATEGY	(Reference Faux and Pratt, pp 32ff.)
	 **	Five constraints must be gotten from the user.  They
	 **	are loaded into pconstr (point constraints) and
	 **	tconstr (tangent constraints).  The latter count for two.
	 **
	 **	Using four constraints (save a pconstr for later), two conics
	 **	consisting of line-pairs through the constraint points are
	 **	determined.  These conics form a basis for a pencil of conics:
	 **			(1-lambda)*quad1 + lambda*quad2
	 **	
	 **	Using the fifth constraint (a point), the value of lamba may
	 **	be determined, and the conic is found, in terms of six
	 **	coefficients.  This is normalized by um_normquad().
	 **
	 **	Endpoints are picked by the user and established in the conic
	 **	entity record.
	 **/

	/*---------------------------------------------------------------
	**  Start of Executable Code
	**-------------------------------------------------------------*/
	uu_denter(UU_MTRC,(us,"um_cnstrconic()"));

	/**	--	set up lines[0-3]	--		**/
	switch(numtgt)	/* number of tangent constraints provided	*/
		{
	 case	0:
		um_ptpttoln(pts[0], pts[1], lines[0]);
		um_ptpttoln(pts[2], pts[3], lines[1]);
		um_ptpttoln(pts[0], pts[2], lines[2]);
		um_ptpttoln(pts[1], pts[3], lines[3]);
		break;

	 case	1:
		um_tgtoln(tgts[0], lines[0]);
		um_ptpttoln(pts[0], pts[1], lines[1]);
		um_ptpttoln(tgts[0][0], pts[0], lines[2]);
		um_ptpttoln(tgts[0][0], pts[1], lines[3]);
		break;

	 case	2:
		um_tgtoln(tgts[0], lines[0]);
		um_tgtoln(tgts[1], lines[1]);
		um_ptpttoln(tgts[0][0], tgts[1][0], lines[2]);
		um_ptpttoln(tgts[0][0], tgts[1][0], lines[3]);
		break;

		}

	/** set up two basis quadratics	**/
	um_lntmln(lines[0], lines[1], quad1);
	um_lntmln(lines[2], lines[3], quad2);

	/** calculate lambda	**/
	temp1 = 	um_quadev(pts[numpts-1], quad1) - um_quadev(pts[numpts-1], quad2);
	if (UM_ZILCH(temp1))
		{
		/* this means that two degenerate conics quad1 and quad2 
		 * do not span a pencil.
		 */
		ret_val = -1;
		goto Done;
		}

	lambda = um_quadev(pts[numpts-1], quad1)/temp1;

#if	UM_MTRACE
		{
		UU_REAL	junk1;
		int		i;

		for ( i = 0; i < numpts; i++)
			{
			junk1 = um_quadev(pts[i], quad1);
			um_p_ary(UM_PFLOAT, "CHECKPOINT 1,1:", 1, &junk1);
			}
		for ( i = 0; i < numtgt; i++)
			{
			junk1 = um_quadev(tgts[i][0], quad1);
			um_p_ary(UM_PFLOAT, "CHECKPOINT 1,2:", 1, &junk1);
			}

		for ( i = 0; i < numpts; i++)
			{
			junk1 = um_quadev(pts[i], quad2);
			um_p_ary(UM_PFLOAT, "CHECKPOINT 2,1:", 1, &junk1);
			}
		for ( i = 0; i < numtgt; i++)
			{
			junk1 = um_quadev(tgts[i][0], quad2);
			um_p_ary(UM_PFLOAT, "CHECKPOINT 2,2:", 1, &junk1);
			}
		}
#endif

	/** execute linear combination for quadratic form of conic	**/
	um_quadtmsc(quad1, (1-lambda), quad1);
	um_quadtmsc(quad2, lambda, quad2);
	um_quadplquad(quad1, quad2, quad1);
#if	UM_MTRACE
		{
		UU_REAL	junk1;
		int		i;
		um_p_ary(UM_PFLOAT, "final equation", 6, quad1);
		for ( i = 0; i < numpts; i++)
			{
			junk1 = um_quadev(pts[i], quad1);
			um_p_ary(UM_PFLOAT, "CHECKPOINT 3,1:", 1, &junk1);
			}
		for ( i = 0; i < numtgt; i++)
			{
			junk1 = um_quadev(tgts[i][0], quad1);
			um_p_ary(UM_PFLOAT, "CHECKPOINT 3,2:", 1, &junk1);
			}
		}
#endif

	/** fill in conic entity from coefficient array	**/
	um_cctmtf(pts[numpts-1], eptr->tfmat, testpt);
	ret_val =  um_normquad(quad1, eptr, eptr->tfmat);
	if (eptr->type == UM_HYPERBOLA)
		{
		um_inverttf(eptr->tfmat, invtfmat);
		um_cctmtf(testpt, invtfmat, testpt);
		if (testpt[0] < 0.0)
			um_rotquad(quad1, UM_PI, eptr->tfmat);
		}

Done:
	uu_dexit;
	return(ret_val);
	}
/*********************************************************************
**    I_FUNCTION :  UU_LOGICAL umi_cn4_closed(eptr)
**       determines if conic is closed
**    PARAMETERS   
**       INPUT  : 
**          eptr	-	pointer to conic
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE if conic is a closed ellipse, else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL
umi_cn4_closed(eptr)
	struct	UM_conic_rec	*eptr;
	{
		UU_LOGICAL	ret_val;

		if (	eptr->type == UM_ELLIPSE
				&&
				(	(	UM_ZILCH(eptr->t0 - eptr->t1)
						&&
						(eptr->t1 < eptr->t0 )
					)
					||
					(	UM_ZILCH(eptr->t0 + 2.0)
						&&
						UM_ZILCH(eptr->t1 - 2.0)
					)
				)
			)
				{
				return (UU_TRUE);
				}
			else
				{
				return (UU_FALSE);
				}

	}
