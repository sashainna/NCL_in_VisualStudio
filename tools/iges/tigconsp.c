/*********************************************************************
**    NAME         :  tigconsp.c
**       CONTAINS:
**       uig_normquad(S, eptr, tfmat)
**       int	uig_quadtype(A)
**       uig_rotquad(A, theta, tfmat)
**       uig_tranquad(A, dx, dy, tfmat)
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tigconsp.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:44
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mattr.h"
#include "mcrv.h"
#include "modef.h"
#include "mdeval.h"
#include "mdebug.h"

#define	UM_ECN1TRC	0

#undef UM_ZILCH
#define	UM_ZILCH(A)	(fabs((UU_REAL)(A)) < UM_DFUZZ)

void uig_rotquad(),uig_tranquad();

/*********************************************************************
**    I_FUNCTION :  uig_normquad(S, eptr, tfmat)
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
uig_normquad(S, eptr, tfmat)
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

#ifdef UM_ECN1TRC
	um_pscroll("um_normquad: original quadric");
	um_p_ary(UM_PFLOAT, "  quad=", 6, S);
	um_p_ary(UM_PFLOAT, "  tfmat[0]", 3, tfmat[0]);
	um_p_ary(UM_PFLOAT, "  tfmat[1]", 3, tfmat[1]);
	um_p_ary(UM_PFLOAT, "  tfmat[2]", 3, tfmat[2]);
	um_p_ary(UM_PFLOAT, "  tfmat[3]", 3, tfmat[3]);
#endif

	if ((eptr->type = uig_quadtype(S)) == UM_CN_UNKNOWN)
		{
		return(-1);
		}

	/* Rotate to remove cross term */
	diff = S[2]-S[0];
	if (!UM_ZILCH(diff))
		theta = 0.5 * atan( S[1]/diff);
	else
		theta = -0.5 * UM_HALFPI;
	uig_rotquad(S, theta, tfmat);
#ifdef UM_ECN1TRC
	um_pscroll("cross term removed");
	um_p_ary(UM_PFLOAT, "  quad=", 6, S);
	um_p_ary(UM_PFLOAT, "  tfmat[0]", 3, tfmat[0]);
	um_p_ary(UM_PFLOAT, "  tfmat[1]", 3, tfmat[1]);
	um_p_ary(UM_PFLOAT, "  tfmat[2]", 3, tfmat[2]);
	um_p_ary(UM_PFLOAT, "  tfmat[3]", 3, tfmat[3]);
#endif

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
			uig_tranquad(S, dx, dy, tfmat);
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
				uig_rotquad(S, UM_PI, tfmat);
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
			uig_tranquad(S, dx, dy, tfmat);
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
				uig_rotquad(S, UM_HALFPI, tfmat);
				um_pscroll("opens down; rotated pi/2");
				um_p_ary(UM_PFLOAT, "  quad=", 6, S);
				um_p_ary(UM_PFLOAT, "  tfmat[0]", 3, tfmat[0]);
				um_p_ary(UM_PFLOAT, "  tfmat[1]", 3, tfmat[1]);
				um_p_ary(UM_PFLOAT, "  tfmat[2]", 3, tfmat[2]);
				um_p_ary(UM_PFLOAT, "  tfmat[3]", 3, tfmat[3]);
				}
			else			/* opens to the top	*/
				{
				uig_rotquad(S, -UM_HALFPI, tfmat);
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
		uig_tranquad(S, dx, dy, tfmat);
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
				uig_rotquad(S, UM_HALFPI, tfmat);	/* may be -pi/2, pi	*/
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
				uig_rotquad(S, UM_HALFPI, tfmat);
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
/*********************************************************************
**    I_FUNCTION :  int	uig_quadtype(A)
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
uig_quadtype(A)
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

	if (Q2 > UM_DFUZZ)
		type =  (Q1*Q3 < 0.)? UM_ELLIPSE: UM_CN_UNKNOWN;
	else if (Q2 < -UM_DFUZZ)
		type = (!UM_ZILCH(Q1))? UM_HYPERBOLA: UM_CN_UNKNOWN;
	else		/* Q1 approx == 0	*/
		type = (!UM_ZILCH(Q1))? UM_PARABOLA: UM_CN_UNKNOWN;

/*
...   The above test is numerically sensitive. Check If the conic is
...   normalized, and use the coefficients directly if it is.
*/
	if (type == UM_CN_UNKNOWN && A[1] == 0.0)
		if (A[5] == 0.0)
			{
			if (A[0]*A[2] == 0.0 &&
				(A[0] == 1.0 && A[3] > 0.0 || A[2] == 1.0 && A[4] > 0.0 ))
				type = UM_PARABOLA;
			}
		else if (A[3] == 0.0 && A[4] == 0.0)
			{
			if (A[5] < 0.0)
				{
				if (A[0]*A[2] > 0) type = UM_ELLIPSE;
				}
			else
				{
				if (A[0]*A[2] < 0) type = UM_HYPERBOLA;
				}
			}

	return(type);
	}
/*********************************************************************
**    I_FUNCTION :  uig_rotquad(A, theta, tfmat)
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

void uig_rotquad(A, theta, tfmat)
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

/*********************************************************************
**    I_FUNCTION :  uig_tranquad(A, dx, dy, tfmat)
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

void uig_tranquad(A, dx, dy, tfmat)
	UU_REAL	A[6];
	UU_REAL	dx,	dy;
	UM_transf	tfmat;

	{
	uu_denter(UU_MTRC,(us,"uig_tranquad(dx=%g, dy=%g)", dx, dy));

	dx= -dx;
	dy= -dy;

/*
	A[5] += A[3]*dx + A[4]*dy + A[0]*dx*dx + A[2]*dy*dy;
	A[4] += 2*A[2]*dy;
	A[3] += 2*A[0]*dx;

	A[3] = A[3] + 2*A[0]*dx + A[1]*dy; 
	A[4] = A[4] + 2*A[2]*dy + A[1]*dx;
	A[5] = A[0]*dx*dx + A[1]*dx*dy + A[2]*dy*dy + A[3]*dx + A[4]*dy + A[5];
*/
/*   Reverse order of asignment so A[5] uses orig A[3:4] - IJD 2-Nov-90 */
	A[5] += A[0]*dx*dx + A[1]*dx*dy + A[2]*dy*dy + A[3]*dx + A[4]*dy;
	A[4] += 2*A[2]*dy + A[1]*dx;
	A[3] += 2*A[0]*dx + A[1]*dy; 

	/* must multiply on left by translation matrix	*/
	tfmat[3][0] +=	dx*tfmat[0][0] + dy*tfmat[1][0];
	tfmat[3][1] +=	dx*tfmat[0][1] + dy*tfmat[1][1];
	tfmat[3][2] +=	dx*tfmat[0][2] + dy*tfmat[1][2];

	uu_dexit;
	}
