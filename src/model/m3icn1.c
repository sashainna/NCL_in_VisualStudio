/*********************************************************************
**    NAME         :  m3icn1.c
**       CONTAINS: routines to manipulate parameters (t or u) for
**				conics
**			um_conic_parameter(u, eptr, t, interval)
**			umi_cc4_ttou(t, eptr, u)
**			um_val4_conic(evflag, t, eptr, evout);
**			um_set4_conic(eptr, nsideptr)
**			um_slv4_hyperbola(point, eptr, t)
**			um_slv4_parabola(point, eptr, t)
**			um_slv4_ellipse(point, eptr, t)
**			umi_cc4_cctot(cc, eptr, tfmat, tvaluep, approxp)
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m3icn1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:56
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mcrv.h"
#include "mdcpln.h"
#include "mdeval.h"
#include "modef.h"
#include "mdebug.h"
#include "mfort.h"
#include "nclfc.h"

#define	UM_ICN1TRC	0

#define	UU_MAX(A,B) ((A)>(B)?(A):(B))
#define	UU_MIN(A,B) ((A)<(B)?(A):(B))

/*********************************************************************
**    I_FUNCTION :  um_conic_parameter(u, eptr, t, interval)
**       converts segment parameter [0,1] to natural parameter
**			for conic.
**    PARAMETERS   
**       INPUT  : 
**          u			user's segment parameter in [0,1]
**				eptr			pointer to entity rec
**       OUTPUT :  
**				t			value of internal parameterization
**				interval	total curve change in t (linear scale factor)
**						(note that interval may be negative for hyperbola
**						 and parabola which are traversed backwards. 
**						 Ellipse interval is always positive.)
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_conic_parameter(u, eptr, t_p, interval_p)
	UU_REAL	u;
	struct UM_conic_rec	*eptr;
	UU_REAL	*t_p;
	UU_REAL	*interval_p;
	{
	/* note: assumes parameter endpoints in conic_rec */
	*interval_p = eptr->t1 - eptr->t0;

	if  (eptr->type == UM_ELLIPSE && *interval_p < 0)
		{		/* curve wraps through -2 = 2 */

		*interval_p += 4;	/* gives correct, positive, interval size */

		/* affine transformation	*/
		*t_p = eptr->t0 + ( u * (*interval_p) );

		if ( *t_p > 2 )	/* in case point is on other side of split from t0	*/
			{
			*t_p -= 4;		/* brings it back into range [-2,2]	*/
			}
		}
	else
		/* affine transformation */
		*t_p = eptr->t0 + (u * (*interval_p));
	}

/*********************************************************************
**    I_FUNCTION :  umi_cc4_ttou(t, eptr, u)
**       converts conic internal parameter t to user parameter u;
**		SEE ALSO:
**			um_conic_parameter() for inverse operation
**    PARAMETERS   
**       INPUT  : 
**          t	-	internal parameter
**				eptr	pointer to conic entity data: t0, t1, and type fields are used
**				u	-	pointer to REAL for answer
**       OUTPUT :  
**				*u		user parameter: in [0,1] for point on segment
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : will provide values outside of [0,1] for values of
**				t outside of curve segment, but these answers are not unique,
**				nor predictable
*********************************************************************/
umi_cc4_ttou(t, eptr, u)
	UU_REAL	t;
	struct UM_conic_rec	*eptr;
	UU_REAL	*u;
	{
	UU_REAL	interval;	/* parameter difference of endpoints	*/
	UU_REAL	split;		/* for ellipse: is u value not on curve >1 or <0?	*/
	UU_REAL	deltat;		/* t - t0	*/

	interval = eptr->t1 - eptr->t0;
	deltat = t - eptr->t0;

	if (fabs(interval) < UM_FUZZ)
		/* interval too small: split difference	*/
		*u = .5;

	else if  (eptr->type == UM_ELLIPSE && interval < 0)
		{		/* curve wraps through t = -2 = 2 */

		/* get correct interval size */
		split = -interval/2;	/* a positive number, used to decide
									 * if point is outside of
									 * u:[0,1] is closer to t0 or t1
									 */
		interval += 4;

		if (deltat < -split)	/* on other side of seam from t0	*/
			deltat += 4;

		*u = deltat/interval;
		}

	else
		/* inverse affine transformation */
		*u = deltat/interval;
	}
/*********************************************************************
**    I_FUNCTION :  um_val4_conic(evflag, t, eptr, evout);
**		Definition space evaluator for all conics.  Uses def'n
**		parameter t, gives answers in definition plane.
**    PARAMETERS   
**       INPUT  : 
**				int	evflag		passed down from um_ev_crv()
**          real	t;				'natural' parameter in definition space
**						*eptr			entity record
**       OUTPUT :  
**				*evout				evaluator output record
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : FIX: Needs error handling for illegal values of t
*********************************************************************/

um_val4_conic(evflag, t, eptr, evout)
	int		evflag;
	UU_REAL	t;
	struct	UM_conic_rec	*eptr;
	register struct UM_evcrvout	*evout;
	{

	UU_REAL	s;		/* ellipse parameter			*/
	int	sign;		/* + or - 1, for ellipse	*/
	UU_REAL	A;		/* invariants	*/
	UU_REAL	B;

	UU_REAL	D;		/* numerators and denominator */
	UU_REAL	N0;
	UU_REAL	N1;

	/* get invariants */
	A = eptr->invariants[0];
	B = eptr->invariants[1];

	/* Set parameterization in definition space	*/
	switch(eptr->type)
		{
	 case	UM_HYPERBOLA:
		D = (1 - t*t);
		N0 = A * (1 + t*t);
		N1 = B * 2 * t;
		break;

	 case	UM_PARABOLA:
		D = 1;
		N0 = A * t * t;
		N1 = t;	/* B is not used */
		break;

	 case	UM_ELLIPSE:
		/*--------------------------------------------------------------
		 * Ellipses have special parameterization:
		 *		C(t) = sign * B(s)
		 * where 'sign' and s(t) differ for the
		 * top and bottom halves.
		 *
		 *	B(s) = (u(s), v(s)) where u(s) = -2*a*s/D
		 *										v(s) = b*(1-s*s)/D
		 *	where D = 1 + s*s.
		 *--------------------------------------------------------------*/

		if ( 0 <= t && t <= 2) /* bottom half */
			sign = -1;
		else if ( -2 <= t && t < 0) /* top half */
			sign = 1;
		else
			;	/** FIX: Error handling: illegl parameter **/

		/* change from t in [-2,2] to s in [-1,1] */
		s = t + sign;

		D = 1 + s*s;
		N0 = A * sign * -2 * s;
		N1 = B * sign * (1 - s*s);
		break;

	 default:
		;	/** FIX: ERROR HANDLING **/
		}

	/* set zero order */
	evout->cp[0] = N0/D;
	evout->cp[1] = N1/D;
	evout->cp[2] = 0;

	if (evflag != UM_POINT)	/* need higher order than zero */
		{
		UU_REAL	dD;			/* d/dt */
		UU_REAL	dN0;			
		UU_REAL	dN1;
		
		switch(eptr->type)
			{
		 case	UM_HYPERBOLA:
			dD = - 2 * t;
			dN0 = A * 2 * t;
			dN1 = B * 2;
			break;

		 case UM_PARABOLA:
			/** Will use special case for parabola **/
			break;

		 case UM_ELLIPSE:
			dD = 2 * s;
			dN0 = A * sign * -2;
			dN1 = B * sign * -2 * s;
			break;
			}

		if (eptr->type == UM_PARABOLA)
			{
			evout->dcdu[0] = 2 * A * t;
			evout->dcdu[1] = 1;
			evout->dcdu[2]	= 0;
			}
		else
			{
			evout->dcdu[0]	= (D * dN0 - dD * N0)/(D * D);
			evout->dcdu[1]	= (D * dN1 - dD * N1)/(D * D);
			evout->dcdu[2]	= 0;
			}

		if (evflag != UM_FRSTDERIV)	/* go for two */
			{
			UU_REAL	d2D;
			UU_REAL	d2N0;
			UU_REAL	d2N1;

			switch(eptr->type)
				{
			 case UM_HYPERBOLA:
				d2D = -2;
				d2N0 = A * 2;
				d2N1 = 0;
				break;

			 case UM_PARABOLA:
				/** will use special formula **/
				break;

			 case UM_ELLIPSE:
				d2D = 2;
				d2N0 = 0;
				d2N1 = B * sign * -2;
				
				}

			if (eptr->type == UM_PARABOLA)
				{
				evout->d2cdu2[0] = 2 * A;
				evout->d2cdu2[0] = 0;
				evout->d2cdu2[0] = 0;
				}
			else
				{
				evout->d2cdu2[0] =
						(D*D*d2N0 - D*N0*d2D - 2*dD*(D*dN0 * N0*dD))/(D*D*D);
				evout->d2cdu2[1] =
						(D*D*d2N1 - D*N1*d2D - 2*dD*(D*dN1 * N1*dD))/(D*D*D);
				evout->d2cdu2[2] = 0;
				}

			if (evflag != UM_SECDERIV)	/* need curvature, too */
				{
				extern UU_REAL	um_mag();
				UU_REAL dsdt;

				dsdt = um_mag(evout->dcdu);
				evout->curv = (evout->dcdu[0] * evout->d2cdu2[1] -
								evout->d2cdu2[0] * evout->dcdu[1])/(dsdt*dsdt*dsdt);
				}
			}

		}
	evout->status = UM_VALID;	/* FIX: error handling */
	}
/*********************************************************************
**    I_FUNCTION :  um_set4_conic(eptr, nsideptr)
**			Determines number of poly-segments for drawing curve
**    PARAMETERS   
**       INPUT  : 
**          UM_conic_rec	*eptr;	ptr to conic entity
**											(includes endpoints)
**       OUTPUT :  
**				int	*nsideptr		-number of sides of polyline 
**										== (number of points) - 1
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

/*
.....Treat ellipses as circles and all other conics
.....as NCL curves when determining the number of
.....points to draw.
.....Bobby  -  3/11/92
*/

um_set4_conic(eptr, tfmat, nsideptr)
struct UM_conic_rec	*eptr;
UU_REAL	tfmat[4][3];
int		*nsideptr;
{
	UM_angle ang,deltang;
	struct UM_circle_rec ce;
	UM_cn_defn cn_defn;
	UM_vector tv;
	UM_int2 ifl,ival;
/*
.....Treat ellipse's as circles
.....using the Major axis as the radius
.....when calculating the number of sides
.....to draw on the conic
*/
	if (eptr->type == UM_ELLIPSE)
	{
/*
........Evaluate the ellipse
*/
		um_cn_defn(eptr,tfmat,&cn_defn);
/*
........Convert the ellipse to a circle
*/
		ce.radius = cn_defn.major_length;
		ce.center[0] = cn_defn.center[0];
		ce.center[1] = cn_defn.center[1];
		ce.center[2] = cn_defn.center[2];
		tv[0] = cn_defn.spt[0] - cn_defn.center[0];
		tv[1] = cn_defn.spt[1] - cn_defn.center[1];
		tv[2] = cn_defn.spt[2] - cn_defn.center[2];
		um_unitvc(tv,ce.svec);
		ce.nvec[0] = cn_defn.pln_normal[0];
		ce.nvec[1] = cn_defn.pln_normal[1];
		ce.nvec[2] = cn_defn.pln_normal[2];
		tv[0] = cn_defn.ept[0] - cn_defn.center[0];
		tv[1] = cn_defn.ept[1] - cn_defn.center[1];
		tv[2] = cn_defn.ept[2] - cn_defn.center[2];
		ce.dang = um_angle2p(ce.svec,tv,ce.nvec);
		if (ce.dang <= 0.) ce.dang = ce.dang + UM_TWOPI;
/*
........Get number of points on ellipse to draw
*/
		um_set3_circle(&ce,nsideptr,&ang,&deltang);
	}
/*
.....For all other conics use the
.....*SET/ADISPL value
*/
	else
	{
		ifl = 136;
		getifl (&ifl,&ival);
		*nsideptr = ival;
	}
}

/*********************************************************************
**    I_FUNCTION :   um_slv4_hyperbola(point, eptr, t)
**       Find's the t-parameter value corresponding to point on
**			an hyperbola in definition space.
**    PARAMETERS   
**       INPUT  : 
**          real	eptr				- pointer to entity record
**				real	point[3]			- point allegedly on hyperbola
**       OUTPUT :  
**          real	*t					- parameter value, if point valid
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : coordinates are presumed valid: i.e., point is near
**							correct branch.
**							Does not verify conic 'type'
*********************************************************************/

um_slv4_hyperbola(point, eptr, t_p)
UU_REAL	point[3];		/* z coord is ignored */
struct	UM_conic_rec	*eptr;	/* Assumed to be hyperbola	*/
UU_REAL	*t_p;
{
	UU_REAL	b;	/* invariant	*/
	UU_REAL	y;

	b = eptr->invariants[1];
	y = point[1];
	/* Calculate endpoint (in [-1,1]) from X coordinate	*/
	/* quadratic formula	*/
/*
.....If y=0, then we have a problem when we divide by 0
.....So check to make sure y does not equal zero before
.....trying to divide.  JLS 11/22/99
*/
	if(y!=0)
	{
		*t_p = (-b + sqrt(b*b + y*y)) / y;
		if ( *t_p > 1.0 + UM_FUZZ || *t_p < -1.0 - UM_FUZZ)
			/* use other solution of equation	*/
			*t_p = (-b - sqrt(b*b + y*y)) / y;
	}
	else
		*t_p=0.0;
}
/*********************************************************************
**    I_FUNCTION :   um_slv4_parabola(point, eptr, t)
**       Find's the t-parameter value corresponding to point on
**			an parabola in definition space.
**    PARAMETERS   
**       INPUT  : 
**          real	eptr				- pointer to entity record
**				real	point[3]			- point allegedly on parabola
**       OUTPUT :  
**          real	*t					- parameter value, if point valid
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : coordinates are presumed valid: i.e., x is positive.
**							Does not verify conic 'type'
**					NOTE that this is a trivial function
*********************************************************************/

um_slv4_parabola(point, eptr, t_p)
	UU_REAL	point[3];		/* z coord is ignored */
	struct	UM_conic_rec	*eptr;	/* Assumed to be parabola	*/
	UU_REAL	*t_p;
	{
	*t_p = point[1];
	}
/*********************************************************************
**    I_FUNCTION :   um_slv4_ellipse(point, eptr, t)
**       Find's the t-parameter value corresponding to point on
**			an ellipse in definition space.
**    PARAMETERS   
**       INPUT  : 
**          real	eptr				- pointer to entity record
**				real	point[3]			- point allegedly on ellipse
**       OUTPUT :  
**          real	*t					- parameter value, if point valid
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : coordinates are presumed valid: i.e., bounded by 
**						axis lengths. Also, does not verify conic 'type'
*********************************************************************/

um_slv4_ellipse(point, eptr, t_p)
	UM_coord	point;		/* z coord is ignored */
	struct	UM_conic_rec	*eptr;	/* Assumed to be ellipse	*/
	UU_REAL	*t_p;
	{
	int	flip;
	UU_REAL	x0,	y0;
	UU_REAL	s0;	/* secret parameter for ellipse	*/
	UU_REAL	a;		/* conic invariant	*/

	a = eptr->invariants[0];

#if	UM_ICN1TRC
	sprintf(UM_sbuf, "um_slv4_ellipse: point <%f,%f,%f>, a invariant %f",
		point[0], point[1], point[2], a);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "address of entity: %x, address of t buffer: %x",
		eptr, t_p);
	um_pscroll(UM_sbuf);
#endif

	/* flip for bottom half */
	if (point[1] < 0)
		{
		x0 = -point[0];
		y0 = -point[1];
		flip = UU_TRUE;
		}
	else
		{
		x0 = point[0];
		y0 = point[1];
		flip = UU_FALSE;
		}

#if	UM_ICN1TRC
	sprintf(UM_sbuf, "flip: %d", flip);
	um_pscroll(UM_sbuf);
#endif

	/* Calculate endpoint (in [-1,1]) from X coordinate	*/
	/* quadratic formula	*/
	if (UM_ZILCH(x0))
		{
#if	UM_ICN1TRC
	sprintf(UM_sbuf, "slv4: x0 ZILCH %f", x0);
	um_pscroll(UM_sbuf);
#endif
		s0 =  0.0;	/* end of minor axis	*/
		}
	else
		{
		s0 = (-a + sqrt(fabs(a*a - x0*x0))) / x0;

#if	UM_ICN1TRC
	sprintf(UM_sbuf, "check1 s0 %f", s0);
	um_pscroll(UM_sbuf);
#endif

		if ( s0 > 1.0 + UM_FUZZ || s0 < -1.0 - UM_FUZZ)
			/* use other solution of equation	*/
			s0 = (-a - sqrt(fabs(a*a - x0*x0))) / x0;

#if	UM_ICN1TRC
	sprintf(UM_sbuf, "check2 s0 %f, check2 a %f", s0, a);
	um_pscroll(UM_sbuf);
#endif

		}

	/* adjust to [-2,2] */
	*t_p = flip? s0 + 1: s0 - 1;
	}
	
/*********************************************************************
**    I_FUNCTION :  umi_cc4_cctot(cc, eptr, tfmat, tvaluep, approxp)
**       converts cartesian coordinate of point on conic to internal
**			curve parameter, t.  (NOT user parameter, u.)
**    PARAMETERS   
**       INPUT  : 
**          cc				-	point coordinates
**				eptr			-	conic entity
**				tfmat			-	net transformation from conic def'n space (xy space)
**									to domain of cc
**       OUTPUT :  
**          *tvaluep		-	best guess for curve parameter
**				*approxp		-	distance between Curve(*tvaluep) and cc
**    RETURNS      : 0 if guess gives point within UM_FUZZ of cc
**							1 if the best guess is not close
**							-1 if point is too screwy to calculate
**								output is NOT VALID
**							-2 if other branch of hyperbola is closer
**								output is NOT VALID
**							-3	other catastrophe (e.g.invert matrix error)
**    SIDE EFFECTS : DOES NOT FLIP HYPERBOLA TO OPPOSITE BRANCH
**    WARNINGS     : YOU MUST CHECK FOR RETURNED 2 WHEN DEALING
**							WITH HYPERBOLAS
*********************************************************************/
umi_cc4_cctot(cc, eptr, tfmat, tvaluep, approxp)
	UM_coord	cc;
	struct UM_conic_rec	*eptr;
	UM_transf	tfmat;
	UU_REAL	*tvaluep;
	UU_REAL	*approxp;
	{
	UM_transf	invtfmat;	/* inverse of tfmat	*/
	UU_REAL		point[3];	/* definition space point	*/
	int			retval;		/* return value				*/

	struct 	UM_evcrvout		evout;
	UU_REAL	um_dcccc();

	uu_denter(UU_MTRC,(us,"umi_cc4_cctot()"));

	/** Invert point to definition space. **/
	if (!um_inverttf(tfmat, invtfmat))
		{
		retval = -3;
		goto Done;
		}

	um_cctmtf(cc, invtfmat, point);

#if	UM_ICN1TRC
		um_p_ary(UM_PFLOAT, "um_cctot: transformation passed:", 12, tfmat);
		um_p_ary(UM_PFLOAT, "um_cctot: inverse transformation :", 12, invtfmat);
		sprintf(UM_sbuf, "um_cctot: point in conic space = <%g,%g,%g>",
			point[0], point[1], point[2]);
		um_pscroll(UM_sbuf);
		um_p4_conic(eptr);
#endif

	/** first validity checks, and dispatch	**/
	switch (eptr->type)
		{
	 case UM_HYPERBOLA:
		if (point[0] < 0)		/* branch swap	*/
			{
			retval = -2;
			goto Done;
			}
		um_slv4_hyperbola(point, eptr, tvaluep);
		break;
	 case UM_PARABOLA:
		if (point[0] < -UM_FUZZ)
			{
			retval = -1;
			goto Done;
			}
		um_slv4_parabola(point, eptr, tvaluep);
		break;
	 case	UM_ELLIPSE:
		if ((point[0] < -(eptr->invariants[0]) - UM_FUZZ)
			|| (point[0] > eptr->invariants[0] + UM_FUZZ)
			|| (point[1] > eptr->invariants[1] + UM_FUZZ)
			|| (point[1] < -(eptr->invariants[1]) - UM_FUZZ) )
			{
			retval = -1;
			goto Done;
			}
		um_slv4_ellipse(point, eptr, tvaluep);

#if	UM_ICN1TRC
		sprintf(UM_sbuf, "um_cctot: ellipse case: t = %f", *tvaluep);
		um_pscroll(UM_sbuf);
#endif

		break;
		}

	um_val4_conic(UM_POINT, *tvaluep, eptr, &evout);

#if	UM_ICN1TRC
		sprintf(UM_sbuf, "um_cctot: point from val4 = <%g, %g, %g>",
			evout.cp[0], evout.cp[1], evout.cp[2]);
		um_pscroll(UM_sbuf);
#endif

	if ( (*approxp = um_dcccc(point, evout.cp) ) > UM_FUZZ)
		retval = 1;
	else
		retval = 0;

Done:
	uu_dexit;
	return(retval);
	}
