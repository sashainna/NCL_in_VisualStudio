/*********************************************************************
**    NAME         :  m7math1.c
**       CONTAINS:
**			um_nptln(pt,lpt,ulvc,npt)
**			um_nptpln(pt,ppt,unvc,npt)
*			UU_REAL um_ptplndis(pt,ppt,unvc)
**			um_isegseg (pt1,pt2,qt1,qt2,nint,pt,tol)
**			um_isegseg1 (pt1,pt2,qt1,qt2,nvec,nint,pt,fact,tol)
**			um_isegseg2d (pt1,pt2,qt1,qt2,nint,pt,tol)
**			um_ilnln(pt1,uvc1,pt2,uvc2,nint,pt)
**			um_ilnpln(lpt,ulvc,ppt,unvc,nint,pt)
**			um_iplnpln(p1,n1,p2,n2,lp,lv)
**			um_lnlndis (pt1,uvc1,pt2,uvc2,dis,pt)
**			um_getpolylen(npt, pts)
**			um_nptsg
**			um_nptsg1
**			um_pt_in_contour
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m7math1.c , 25.1
**     DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:08
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "mdcoord.h"
#include "modef.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     : um_nptln(pt,lpt,ulvc,npt)
**			Calculate the  coordinates of the nearest point (NPT) to the
**			given point (PT) which lies on the infinite line defined
**			by a point (LPT) and unit vector (ULVC).
**    PARAMETERS   
**       INPUT  : 
**				pt                 coordinates of given point
**          lpt                point on line
**          ulvc               unit vector along line
**       OUTPUT :  
**				npt                coordinates of nearest point on line
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_nptln(pt,lpt,ulvc,npt)
	UM_coord pt;
	UM_coord lpt;
	UM_vector ulvc;
	UM_coord npt;

	{
	UM_vector vc;				/* temporary vector */
	UM_length len;				/* length of projection of vector from
										line point to given point onto line */

	um_vcmnvc(pt, lpt, vc);
	len = um_dot(ulvc, vc);
	um_vctmsc(ulvc, len, vc);
	um_vcplvc(lpt, vc, npt);
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : um_nptpln(pt,ppt,unvc,npt)
**      Calculate the nearest point on the plane to the specified point.
**    PARAMETERS   
**       INPUT  : 
**				pt             coordinates of given point
**          ppt            point on plane
**          unvc           unit normal to plane
**       OUTPUT :  
**				npt            coordinates of nearest point on plane
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_nptpln(pt,ppt,unvc,npt)
	UM_coord pt;
	UM_coord ppt;
	UM_vector unvc;
	UM_coord npt;

	{
	UU_REAL proj;				/* projection along normal */
	UM_vector vc;				/* temporary vector */

	um_vcmnvc(pt, ppt, vc);
	proj = um_dot(vc, unvc);
	um_vctmsc(unvc,  - proj, vc);
	um_vcplvc(pt, vc, npt);
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : um_ptplndis(pt,ppt,unvc)
**     Calculate the distance from the specified point(pt) to plane(ppt,unvc).
**    PARAMETERS   
**       INPUT  : 
**			pt         coordinates of given point
**          ppt        point on plane
**          unvc       unit normal to plane
**       OUTPUT :  
**			dis        the distance between point and plane
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL 
um_ptplndis(pt,ppt,unvc)
	UM_coord pt;
	UM_coord ppt;
	UM_vector unvc;
	{
		UU_REAL proj;				/* projection along normal */
		UM_vector vc;				/* temporary vector */

		um_vcmnvc(pt, ppt, vc);
		proj = um_dot(vc, unvc);
		return (fabs(proj));
	}

/*********************************************************************
**    E_FUNCTION     : um_isegseg (pt1,pt2,qt1,qt2,nint,pt,tol)
**      Intersect two segments which are not known to lie in the same 
**      plane.
**    PARAMETERS   
**       INPUT  : 
**          [pt1,pt2]      first segment
**          [qt1,qt2]      second segment
**          tol            tolerance ( = sc(27) )
**       OUTPUT :  
**          nint           number of intersection points (0 or 1)
**          pt             intersection point (middle point if the segments
**                                                                  overlap)
**    RETURNS      : 1 in the case of a nonoverlapping and nondegenerate 
**                     intersection;
**                   0 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_isegseg (pt1,pt2,qt1,qt2,nint,pt,tol)
UM_coord pt1,pt2,qt1,qt2;
int  *nint;
UU_REAL tol;
UM_coord pt;
{
	UM_vector p,q,pqtmp,p1q1,p1q2,p2q1,p2q2;
	UU_REAL co,dp,dq,dpq11,dpq12,dpq21,dpq22,proj,u,v;
	UU_REAL tolsq = tol*tol;

	*nint = 0;

	um_vcmnvc (pt2, pt1, p);
	um_vcmnvc (qt2, qt1, q);
	dp = UM_DOT (p,p);
	dq = UM_DOT (q,q);

	um_vcmnvc (qt1,pt1,p1q1);
	um_vcmnvc (qt2,pt1,p1q2);
	um_vcmnvc (qt1,pt2,p2q1);
	um_vcmnvc (qt2,pt2,p2q2);
	dpq11 = UM_DOT (p1q1,p1q1);
	dpq12 = UM_DOT (p1q2,p1q2);
	dpq21 = UM_DOT (p2q1,p2q1);
	dpq22 = UM_DOT (p2q2,p2q2);

	if (dp < tolsq)
	{
		if (dpq11 < tolsq || dpq12 < tolsq)
		{
			um_vctovc(pt1,pt); *nint = 1;
		}
		else if (dpq21 < tolsq || dpq22 < tolsq)
		{
			um_vctovc(pt2,pt); *nint = 1;
		}
		return (0);
	}

	if (dq < tolsq)
	{
		if (dpq11 < tolsq || dpq12 < tolsq)
		{
			um_vctovc(pt1,pt); *nint = 1;
		}
		else if (dpq21 < tolsq || dpq22 < tolsq)
		{
			um_vctovc(pt2,pt); *nint = 1;
		}
		return (0);
	}

	dp = sqrt(dp); dq = sqrt(dq);
	p[0] /= dp; p[1] /= dp; p[2] /= dp;
	q[0] /= dq; q[1] /= dq; q[2] /= dq;

	co = UM_DOT(p, q);
	if (1. - fabs(co) < 1.e-8) 
	{
		UU_REAL umin,umax;

		um_cross (p1q1,p,pqtmp);
		if (UM_DOT(pqtmp,pqtmp) > tolsq) return (0);

		umin = UM_DOT (p1q1, p);
		umax = UM_DOT (p1q2, p);
		if (umin > umax) 
		{
			u = umin; umin = umax; umax = u;
		}
		if ((umax < tol) || (umin > dp - tol)) 
		{
			if (umax < tol && umax > - tol) 
			{
				*nint = 1;
				um_vctovc(pt1,pt);
			}
			else if ((umin > dp - tol) && (umin < dp + tol))
			{
				*nint = 1;
				um_vctovc(pt2,pt);
			}
			return (0);
		}
		if (umin < 0.) umin = 0.;
		if (umax > dp) umax = dp;
		um_translate_point (pt1, (umin + umax) / 2., p, pt);
		*nint = 1;
		return (0);
	}

	if (dpq11 < tolsq || dpq12 < tolsq)
	{
		um_vctovc(pt1,pt); *nint = 1; return (0);
	}
	if (dpq21 < tolsq || dpq22 < tolsq)
	{
		um_vctovc(pt2,pt); *nint = 1; return (0);
	}

	if (fabs(co) >= 0.999876)
	{
		int insid = 0;

		um_cross (p1q1,q, pqtmp);
		if ((UM_DOT(pqtmp,pqtmp) < tolsq) &&
			 (UM_DOT(p1q1,q) * UM_DOT(p1q2,q) < 0))
		{
			insid = 1; um_vctovc (pt1,pt);
		}
		else
		{
			um_cross (p2q1,q, pqtmp);
			if ((UM_DOT(pqtmp,pqtmp) < tolsq) &&
				 (UM_DOT(p2q1,q) * UM_DOT(p2q2,q) < 0))
			{
				insid = 1; um_vctovc (pt2,pt);
			}
		}
		if (insid == 1)
		{
			um_cross (p1q1,p, pqtmp);
			if ((UM_DOT(pqtmp,pqtmp) < tolsq) &&
				 (UM_DOT(p1q1,p) * UM_DOT(p2q1,p) < 0))
			{
				um_middlept (pt,qt1,pt);
				*nint = 1; return (0);
			}
			um_cross (p1q2,p, pqtmp);
			if ((UM_DOT(pqtmp,pqtmp) < tolsq) &&
				 (UM_DOT(p1q2,p) * UM_DOT(p2q2,p) < 0))
			{
				um_middlept (pt,qt2,pt);
				*nint = 1; return (0);
			}
		}
	}

	um_cross (p,q, pqtmp);
	proj = UM_DOT (pqtmp,p1q1);
	if (proj*proj > (1. - co*co)*tolsq) return (0);
/*
..... proj = dis*si; where dis is the distance between two lines
*/
	um_vctmsc(q, co, pqtmp);
	um_vcmnvc(pqtmp, p, pqtmp);
/* pqtmp is a vector from pt1 perp to q, its length is sin(p,q) */
	proj = UM_DOT(p1q1, pqtmp);
	u = - proj / (1 - co*co);
/*
..... u is the distance from pt1 to the intersection along p
*/
	if ((u < - tol) || (u - dp > tol)) return (0);
	um_vctmsc(p, u, pt);
	um_vcplvc(pt1, pt, pt);
	um_vcmnvc (pt,qt1,pqtmp);
	v = UM_DOT (pqtmp,q);
/*
..... v is the distance from qt1 to the intersection along q
*/
	if ((v < - tol) || (v - dq > tol)) return (0);

	*nint = 1;

	if ((u > tol) && (dp - u > tol) && (v > tol) && (dq - v > tol))
		return (1);
	else
		return (0);
}

/*********************************************************************
**    E_FUNCTION     : S_isegseg1 (pt1,pt2,qt1,qt2,nvec,nint,pt,qt,fact,tol)
**      Intersect two segments which are not known to lie in the same
**      plane.
**    PARAMETERS   
**       INPUT  : 
**          [pt1,pt2]      first segment
**          [qt1,qt2]      second segment
**          tol            tolerance
**          nvec           the vertical direction (or NULL)
**       OUTPUT :  
**          nint           number of intersection points (0 or 1)
**          pt             intersection point (middle point if the segments
**                                                                  overlap)
**    RETURNS      : 1 in the case of a nonoverlapping and nondegenerate 
**                     intersection;
**                   0 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int S_isegseg1 (pt1,pt2,qt1,qt2,nvec,nint,pt,qt,fact,tol)
UM_coord pt1,pt2,qt1,qt2;
UU_REAL *nvec;
int *nint;
UU_REAL fact,tol;
UM_coord pt,qt;
{
	UM_vector p,q,pqtmp,p1q1,p1q2,p2q1,p2q2;
	UU_REAL co,dp,dq,dpq11,dpq12,dpq21,dpq22,proj,u,v,dis;
	UU_REAL tolsq = tol*tol;
	UM_coord zpt;
	UM_vector zvec,pn,qn;
	int i;

	*nint = 0;

	um_vcmnvc (pt2, pt1, p);
	um_vcmnvc (qt2, qt1, q);
	dp = UM_DOT (p,p);
	dq = UM_DOT (q,q);

	um_vcmnvc (qt1,pt1,p1q1);
	um_vcmnvc (qt2,pt1,p1q2);
	um_vcmnvc (qt1,pt2,p2q1);
	um_vcmnvc (qt2,pt2,p2q2);
	dpq11 = UM_DOT (p1q1,p1q1);
	dpq12 = UM_DOT (p1q2,p1q2);
	dpq21 = UM_DOT (p2q1,p2q1);
	dpq22 = UM_DOT (p2q2,p2q2);

	if (dp < tolsq)
	{
		if (dpq11 < tolsq || dpq12 < tolsq)
		{
			um_vctovc(pt1,pt); *nint = 1;
		}
		else if (dpq21 < tolsq || dpq22 < tolsq)
		{
			um_vctovc(pt2,pt); *nint = 1;
		}
		return (0);
	}

	if (dq < tolsq)
	{
		if (dpq11 < tolsq || dpq12 < tolsq)
		{
			um_vctovc(pt1,pt); *nint = 1;
		}
		else if (dpq21 < tolsq || dpq22 < tolsq)
		{
			um_vctovc(pt2,pt); *nint = 1;
		}
		return (0);
	}

	dp = sqrt(dp); dq = sqrt(dq);
	p[0] /= dp; p[1] /= dp; p[2] /= dp;
	q[0] /= dq; q[1] /= dq; q[2] /= dq;

	co = UM_DOT(p, q);
	if (1. - fabs(co) < 1.e-8) 
	{
		UU_REAL umin,umax;

		um_cross (p1q1,p,pqtmp);
		if (UM_DOT(pqtmp,pqtmp) > tolsq) return (0);

		umin = UM_DOT (p1q1, p);
		umax = UM_DOT (p1q2, p);
		if (umin > umax) 
		{
			u = umin; umin = umax; umax = u;
		}
		if ((umax < tol) || (umin > dp - tol)) 
		{
			if (umax < tol && umax > - tol) 
			{
				*nint = 1;
				um_vctovc(pt1,pt);
			}
			else if ((umin > dp - tol) && (umin < dp + tol))
			{
				*nint = 1;
				um_vctovc(pt2,pt);
			}
			return (0);
		}
		if (umin < 0.) umin = 0.;
		if (umax > dp) umax = dp;
		um_translate_point (pt1, (umin + umax) / 2., p, pt);
		*nint = 1;
		return (0);
	}

	if (dpq11 < tolsq || dpq12 < tolsq)
	{
		um_vctovc(pt1,pt); *nint = 1; return (0);
	}
	if (dpq21 < tolsq || dpq22 < tolsq)
	{
		um_vctovc(pt2,pt); *nint = 1; return (0);
	}

	if (fabs(co) >= 0.999876)
	{
		int insid = 0;

		um_cross (p1q1,q, pqtmp);
		if ((UM_DOT(pqtmp,pqtmp) < tolsq) &&
			 (UM_DOT(p1q1,q) * UM_DOT(p1q2,q) < 0))
		{
			insid = 1; um_vctovc (pt1,pt);
		}
		else
		{
			um_cross (p2q1,q, pqtmp);
			if ((UM_DOT(pqtmp,pqtmp) < tolsq) &&
				 (UM_DOT(p2q1,q) * UM_DOT(p2q2,q) < 0))
			{
				insid = 1; um_vctovc (pt2,pt);
			}
		}
		if (insid == 1)
		{
			um_cross (p1q1,p, pqtmp);
			if ((UM_DOT(pqtmp,pqtmp) < tolsq) &&
				 (UM_DOT(p1q1,p) * UM_DOT(p2q1,p) < 0))
			{
				um_middlept (pt,qt1,pt);
				*nint = 1; return (0);
			}
			um_cross (p1q2,p, pqtmp);
			if ((UM_DOT(pqtmp,pqtmp) < tolsq) &&
				 (UM_DOT(p1q2,p) * UM_DOT(p2q2,p) < 0))
			{
				um_middlept (pt,qt2,pt);
				*nint = 1; return (0);
			}
		}
	}

	um_cross (p,q, pqtmp);
	proj = UM_DOT (pqtmp,p1q1);
	if (nvec != UU_NULL)
	{
		for (i = 0; i < 3; i++) zvec[i] = nvec[i];
/*
..... make a plane through each line parallel to nvec
..... intersect the two planes
*/
		um_cross (p,zvec, pn);
		um_unitvc (pn,pn);
		um_cross (q,zvec, qn);
		um_unitvc (qn,qn);

		if (um_iplnpln(pt1,pn,qt1,qn,zpt,zvec) != 0) return (0);
/*
..... intersect the plane intersection line with each line
*/
		um_ilnln(pt1,p,zpt,zvec,&i,pt);
		if (i == 0) return (0);
		um_vcmnvc (pt,pt1,pqtmp);
		u = UM_DOT (p,pqtmp);
		if ((u < - tol) || (u - dp > tol)) return (0);

		um_ilnln(qt1,q,zpt,zvec,&i,qt);
		if (i == 0) return (0);
		um_vcmnvc (qt,qt1,pqtmp);
		v = UM_DOT (q,pqtmp);
		if ((v < - tol) || (v - dq > tol)) return (0);
/*
..... return both intersections, the calling code will decide how to use them
*/
		*nint = 1;
		return (2);
	}
	else
		um_unitvc (pqtmp,zvec);

	dis = (proj*proj)/(1. - co*co);
	dis = sqrt (dis);
/*
..... proj = dis*si; where dis is the distance between two lines
*/
	um_vctmsc(q, co, pqtmp);
	um_vcmnvc(pqtmp, p, pqtmp);
/* pqtmp is a vector from pt1 perp to q, its length is sin(p,q) */
	proj = UM_DOT(p1q1, pqtmp);
	u = - proj / (1 - co*co);
/*
..... u is the distance from pt1 to the intersection along p
*/
	if ((u < - tol) || (u - dp > tol)) return (0);
	um_vctmsc(p, u, pt);
	um_vcplvc(pt1, pt, pt);
	um_vcmnvc (pt,qt1,pqtmp);
	v = UM_DOT (pqtmp,q);
/*
..... v is the distance from qt1 to the intersection along q
*/
	if ((v < - tol) || (v - dq > tol)) return (0);

	um_vctmsc(q, v, qt);
	um_vcplvc(qt1, qt, qt);

	if (dis > 10.*tol)
	{
		if (dis > 500.*tol)
			return (0);
		else
		{
			UU_REAL ftol1 = fact*tol;
			UU_REAL ftol2 = 0.16*ftol1*ftol1;

			um_vcmnvc (pt,qt,pqtmp);

			proj = UM_DOT (pqtmp,zvec);
			proj = dis*dis - proj*proj;
/*
..... proj is the distance between closest points in the current 'horizontal'
..... plane
*/
			if (proj > ftol2) return (0);
			if (fact > 5 && proj < tolsq && dis < 1.5*ftol1)
				dis = 0;
			if (dis > ftol1) return (0);
		}
	}

	um_middlept (pt,qt,pt);

	*nint = 1;

	if ((u > tol) && (dp - u > tol) && (v > tol) && (dq - v > tol))
		return (1);
	else
		return (0);
}

/*********************************************************************
**    E_FUNCTION     : um_isegseg1 (pt1,pt2,qt1,qt2,nint,pt,fact,tol)
**      Intersect two segments which are not known to lie in the same 
**      plane.
**    PARAMETERS   
**       INPUT  : 
**          [pt1,pt2]      first segment
**          [qt1,qt2]      second segment
**          tol            tolerance ( = sc(27) )
**       OUTPUT :  
**          nint           number of intersection points (0 or 1)
**          pt             intersection point (middle point if the segments
**                                                                  overlap)
**    RETURNS      : 1 in the case of a nonoverlapping and nondegenerate 
**                     intersection;
**                   0 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_isegseg1 (pt1,pt2,qt1,qt2,nvec,nint,pt,qt,fact,tol)
UM_coord pt1,pt2,qt1,qt2;
UU_REAL *nvec;
int *nint;
UU_REAL fact,tol;
UM_coord pt,qt;
{
	int iret;

	iret = S_isegseg1 (pt1,pt2,qt1,qt2,nvec,nint,pt,qt,fact,tol);
/*
..... if the vertical direction is specified we return two points: one
..... on each line
*/
	if (*nint > 0 && iret < 2)
		um_vctovc (pt,qt);
}

/*********************************************************************
**    E_FUNCTION     : um_isegseg2d (pt1,pt2,qt1,qt2,nint,pt,tol)
**      Intersect two 2D segments.
**    PARAMETERS   
**       INPUT  : 
**              [pt1,pt2]      first segment
**              [qt1,qt2]      second segment
**              tol            tolerance
**       OUTPUT :  
**          nint           number of intersection points (0 or 1)
**          pt             intersection point (middle point if the segments
**                                                                  overlap)
**    RETURNS      : 1 in the case of a nondegenerate intersection;
**                   0 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_isegseg2d (pt1,pt2,qt1,qt2,nint,pt,tol)
UM_2Dcoord pt1,pt2,qt1,qt2,pt;
int  *nint;
UU_REAL tol;
{
	UM_2Dcoord p,q,pqtmp,p1q1,p1q2,p2q1,p2q2;
	UU_REAL co,dp,dq,dpq11,dpq12,dpq21,dpq22,proj,u,v;
	UU_REAL tolsq = tol*tol;
	
	*nint = 0;

	um_vcmnvc_2d (pt2, pt1, p);
	um_vcmnvc_2d (qt2, qt1, q);
	dp = UM_DOT_2D (p,p);
	dq = UM_DOT_2D (q,q);

	um_vcmnvc_2d (qt1,pt1,p1q1);
	um_vcmnvc_2d (qt2,pt1,p1q2);
	um_vcmnvc_2d (qt1,pt2,p2q1);
	um_vcmnvc_2d (qt2,pt2,p2q2);
	dpq11 = UM_DOT_2D (p1q1,p1q1);
	dpq12 = UM_DOT_2D (p1q2,p1q2);
	dpq21 = UM_DOT_2D (p2q1,p2q1);
	dpq22 = UM_DOT_2D (p2q2,p2q2);

	if (dp < tolsq)
	{
		if (dpq11 < tolsq || dpq12 < tolsq)
		{
			um_vctovc_2d(pt1,pt); *nint = 1;
		}
		else if (dpq21 < tolsq || dpq22 < tolsq)
		{
			um_vctovc_2d(pt2,pt); *nint = 1;
		}
		return (0);
	}

	if (dq < tolsq)
	{
		if (dpq11 < tolsq || dpq12 < tolsq)
		{
			um_vctovc_2d(pt1,pt); *nint = 1;
		}
		else if (dpq21 < tolsq || dpq22 < tolsq)
		{
			um_vctovc_2d(pt2,pt); *nint = 1;
		}
		return (0);
	}

	dp = sqrt(dp); dq = sqrt(dq);
	p[0] /= dp; p[1] /= dp;
	q[0] /= dq; q[1] /= dq;

	co = UM_DOT_2D(p, q);
	if (1. - fabs(co) < 1.e-8) 
	{
		UU_REAL umin,umax;

		if (fabs(p1q1[0]*p[1] - p1q1[1]*p[0]) > tol) return (0);

		umin = UM_DOT_2D (p1q1, p);
		umax = UM_DOT_2D (p1q2, p);
		if (umin > umax) 
		{
			u = umin; umin = umax; umax = u;
		}
		if ((umax < tol) || (umin > dp - tol)) 
		{
			if (umax < tol && umax > - tol) 
			{
				*nint = 1;
				um_vctovc_2d(pt1,pt);
			}
			else if ((umin > dp - tol) && (umin < dp + tol))
			{
				*nint = 1;
				um_vctovc_2d(pt2,pt);
			}
			return (0);
		}
		if (umin < 0.) umin = 0.;
		if (umax > dp) umax = dp;
		u = (umin + umax)/2.;
		pt[0] = pt1[0] + u*p[0]; pt[1] = pt1[1] + u*p[1];
		*nint = 1;
		return (0);
	}

	if (dpq11 < tolsq || dpq12 < tolsq)
	{
		um_vctovc_2d(pt1,pt); *nint = 1; return (0);
	}
	if (dpq21 < tolsq || dpq22 < tolsq)
	{
		um_vctovc_2d(pt2,pt); *nint = 1; return (0);
	}

	if (fabs(co) >= 0.999876)
	{
		int insid = 0;

		if ((fabs(p1q1[0]*q[1] - p1q1[1]*q[0]) < tol) &&
			 (UM_DOT_2D(p1q1,q) * UM_DOT_2D(p1q2,q) < 0))
		{
			insid = 1; pt[0] = pt1[0]; pt[1] = pt1[1];
		}
		else if ((fabs(p2q1[0]*q[1] - p2q1[1]*q[0]) < tol) &&
			 (UM_DOT_2D(p2q1,q) * UM_DOT_2D(p2q2,q) < 0))
		{
			insid = 1; pt[0] = pt2[0]; pt[1] = pt2[1];
		}
		if (insid == 1)
		{
			if ((fabs(p1q1[0]*p[1] - p1q1[1]*p[0]) < tol) &&
				 (UM_DOT_2D(p1q1,p) * UM_DOT_2D(p2q1,p) < 0))
			{
				pt[0] = 0.5*(qt1[0]+pt[0]); pt[1] = 0.5*(qt1[1]+pt[1]);
				*nint = 1; return (0);
			}
			else if ((fabs(p1q2[0]*p[1] - p1q2[1]*p[0]) < tol) &&
				 (UM_DOT_2D(p1q2,p) * UM_DOT_2D(p2q2,p) < 0))
			{
				pt[0] = 0.5*(qt2[0]+pt[0]); pt[1] = 0.5*(qt2[1]+pt[1]);
				*nint = 1; return (0);
			}
		}
	}

	um_vctmsc_2d(q, co, pqtmp);
	um_vcmnvc_2d(pqtmp, p, pqtmp);
/* pqtmp is a vector from pt1 perp to q, its length is sin(p,q) */
	proj = UM_DOT_2D(p1q1, pqtmp);
	u = - proj / (1 - co*co);
/*
..... u is the distance from pt1 to the intersection along p
*/
	if ((u < - tol) || (u - dp > tol)) return (0);
	um_vctmsc_2d(p, u, pt);
	um_vcplvc_2d(pt1, pt, pt);
	um_vcmnvc_2d (pt,qt1,pqtmp);
	v = UM_DOT_2D (pqtmp,q);
/*
..... v is the distance from qt1 to the intersection along q
*/
	if ((v < - tol) || (v - dq > tol)) return (0);

	*nint = 1;

	if ((u > tol) && (dp - u > tol) && (v > tol) && (dq - v > tol))
		return (1);
	else
		return (0);
}

/*********************************************************************
**    E_FUNCTION     : um_ilnln(pt1,uvc1,pt2,uvc2,nint,pt)
**      Intersect two lines which are known to lie in the same plane.
**    PARAMETERS   
**       INPUT  : 
**          pt1            point on line 1
**          uvc1           unit vector along line 1
**          pt2            point on line 2
**          uvc2           vector along line 2
**       OUTPUT :  
**          nint           number of intersection points (=0,1)
**          pt             intersection points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_ilnln(pt1,uvc1,pt2,uvc2,nint,pt)
	UM_coord pt1;
	UM_vector uvc1;
	UM_coord pt2;
	UM_vector uvc2;
	int  *nint;
	UM_coord pt;

{
	UU_REAL proj;
	UM_vector t1, t2;
	UU_REAL t;

	if(um_vcparall(uvc1, uvc2)  ==  UU_TRUE) *nint = 0;
	else
	{
		proj = um_dot(uvc1, uvc2);
		um_vcmnvc(pt1, pt2, t1);
		um_vctmsc(uvc2, proj, t2);
		um_vcmnvc(t2, uvc1, t2);
		t = um_dot(t1, t2);
		t = t / (1 - proj *proj);
		*nint = 1;
		um_vctmsc(uvc1, t, pt);
		um_vcplvc(pt1, pt, pt);
	}
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : um_ilnln1(pt1,uvc1,pt2,uvc2,nint,pt)
**      Intersect two lines which are known to lie in the same plane,
**      and if line are same, the median point is returned.
**    PARAMETERS   
**       INPUT  : 
**				pt1            point on line 1
**          uvc1           unit vector along line 1
**          pt2            point on line 2
**          uvc2           vector along line 2
**       OUTPUT :  
**				nint           number of intersection points (=0,1)
**          pt             intersection points
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_ilnln1(pt1,uvc1,pt2,uvc2,nint,pt)
	UM_coord pt1;
	UM_vector uvc1;
	UM_coord pt2;
	UM_vector uvc2;
	int  *nint;
	UM_coord pt;

{
	UM_vector t1, t2;

	*nint = 0;
	if(um_vcparall(uvc1, uvc2)  ==  UU_TRUE)
	{
		um_vcmnvc(pt1, pt2, t1);
		if (um_mag(t1) < UM_DFUZZ)
		{
			um_vctovc(pt1,pt);
			*nint = 1;
		}
		else
		{
			um_unitvc (t1,t2);
			if (um_vcparall(uvc1, t2)  ==  UU_TRUE)
			{
				um_vcplvc(pt2,pt1,pt);		
				um_vctmsc(pt,(UU_REAL) .5, pt);
				*nint = 1;
			}
		}
	}
	else um_ilnln (pt1,uvc1,pt2,uvc2,nint,pt);

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : um_ilnpln(lpt,ulvc,ppt,unvc,nint,pt)
**      Intersect a line and a plane
**    PARAMETERS   
**       INPUT  : 
**				lpt            point on line
**          ulvc           unit vector along line
**          ppt            point on plane
**          unvc           unit vector normal to plane
**       OUTPUT :  
**				nint           number of intersection points
**          pt             intersection point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_ilnpln(lpt,ulvc,ppt,unvc,nint,pt)
	UM_coord lpt;
	UM_vector ulvc;
	UM_coord ppt;
	UM_vector unvc;
	int  *nint;
	UM_coord pt;

	{
	UM_vector vc;				/* vector from point on line to point on plane */
	UU_REAL numer, denom;	/* used to calculate t parameter on line */
	UU_REAL t;					/* parameter corresponding to point of intersection */

	if (um_vcperp(ulvc, unvc)  ==  UU_TRUE)
		{
		 *nint = 0;
		}
	else
		{
		um_vcmnvc(lpt, ppt, vc);
		numer = um_dot(unvc, vc);
		denom = um_dot(ulvc, unvc);
		t =  - (numer / denom);
		um_vctmsc(ulvc, t, vc);
		*nint = 1;
		um_vcplvc(lpt, vc, pt);
		}
	return (0);
	}

/*********************************************************************
**    E_FUNCTION     : um_iplnpln(p1,n1,p2,n2,lp,lv)
**       Intersect two planes, and return the line of intersection.
**    PARAMETERS   
**       INPUT  : 
**				p1							point defining plane 1
**				n1							normal to plane 1
**				p2							point defining plane 2
**				n2							normal to plane 2
**       OUTPUT :  
**				lp							point defining line of intersection
**				lv							vector defining line of intersection
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_iplnpln(p1,n1,p2,n2,lp,lv)
	UM_coord p1;
	UM_vector n1;
	UM_coord p2;
	UM_vector n2;
	UM_coord lp;
	UM_vector lv;

	{
	UU_REAL num, den;
	UU_REAL rs[3];

	if (!um_vcparall(n1,n2))
		{
		um_cross(n1,n2,lv);
		um_unitvc(lv,lv);
		rs[0] = um_dot(p1,lv);
		rs[1] = um_dot(p1,n1);
		rs[2] = um_dot(p2,n2);
		den =  (lv[0]*n1[1]*n2[2]) + (lv[1]*n1[2]*n2[0]) + (lv[2]*n1[0]*n2[1])
			  - (lv[2]*n1[1]*n2[0]) - (lv[0]*n1[2]*n2[1]) - (lv[1]*n1[0]*n2[2]);
		num =  (rs[0]*n1[1]*n2[2]) + (lv[1]*n1[2]*rs[2]) + (lv[2]*rs[1]*n2[1])
			  - (lv[2]*n1[1]*rs[2]) - (rs[0]*n1[2]*n2[1]) - (lv[1]*rs[1]*n2[2]);
		lp[0] = num/den;
		num =  (lv[0]*rs[1]*n2[2]) + (rs[0]*n1[2]*n2[0]) + (lv[2]*n1[0]*rs[2])
			  - (lv[2]*rs[1]*n2[0]) - (lv[0]*n1[2]*rs[2]) - (rs[0]*n1[0]*n2[2]);
		lp[1] = num/den;
		num =  (lv[0]*n1[1]*rs[2]) + (lv[1]*rs[1]*n2[0]) + (rs[0]*n1[0]*n2[1])
			  - (rs[0]*n1[1]*n2[0]) - (lv[0]*rs[1]*n2[1]) - (lv[1]*n1[0]*rs[2]);
		lp[2] = num/den;
		return (0);
		}
	else
		return (-1);
	}

/*********************************************************************
**    E_FUNCTION     : um_lnlndis (pt1,uvc1,pt2,uvc2,dis,pt)
**      Find distance between two lines.  If lines are || distance is
**      calculated from pt1 to line (pt2,unvc2)
**    PARAMETERS   
**       INPUT  : 
**          pt1            point on line 1
**          uvc1           unit vector along line 1
**          pt2            point on line 2
**          uvc2           vector along line 2
**       OUTPUT :  
**          dis            shortest distance between lines.
**          pt             nearest point on line 1 to line 2.
**    RETURNS      : 0 = distance defined, 1 = lines are parallel.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_lnlndis (pt1,uvc1,pt2,uvc2,dis,pt)
UM_coord pt1,pt2,pt;
UM_vector uvc1,uvc2;
UU_REAL *dis;
{
	UM_vector vn,vc;
	UM_coord pt2a;
	UU_REAL t,d;
	int ni,status;

	um_vcmnvc (pt1,pt2,vc);
	if (um_vcparall(uvc1, uvc2)  ==  UU_TRUE)
	{
		status = 1;
		t = UM_DOT(vc,uvc1);
		d = UM_DOT(vc,vc);
		*dis = sqrt (d - t*t);
		um_vctovc (pt1,pt);
	}
	else
	{
		status = 0;
		um_cross (uvc1, uvc2, vn);
		um_unitvc (vn,vn);
		d = UM_DOT (vn,vc); 
		*dis = fabs(d);
/*
...get intersection point on line 1
*/
		um_translate_point (pt2,d,vn, pt2a);
		um_ilnln (pt1,uvc1,pt2a,uvc2,&ni,pt);
	}
	return(status);
}

/****************************************************************************
**    E_FUNCTION: UU_REAL um_getpolylen (npt,pts)
**
**       DESCRIPTION: returns the length of a polyline.
**
**       PARAMETERS:
**          INPUT:
**          npt              number of points in polyline
**          pts              pointer to array of points in polyline
**       OUTPUT: none.
**
**       RETURNS: length of the polyline.
**
**       SIDE EFFECTS: none.
**
**       WARNINGS: none.
**
****************************************************************************/
UU_REAL um_getpolylen(npt, pts)
UM_coord pts[];
{
	UM_coord *ecc;
	UU_REAL len;
	int i;

	len = 0;
	ecc = &pts[0];
	for (i=1; i<npt; i++, ecc++)
		len += um_dcccc (ecc,pts[i]);

	return (len);
}

/*********************************************************************
**    E_FUNCTION     : um_nptsg(pt,spt,ulvc,dln,npt,d)
**			Project a point onto the line defined by a segment, and decide
**       if the projection is inside the segment.
**       This routine is um_nptln combined with um_ptinsg_check
**    PARAMETERS   
**       INPUT  : 
**				pt                 coordinates of given point
**          spt                start point of segment
**          ulvc               unit vector along segment
**          dln                length of segment
**       OUTPUT :  
**				npt                coordinates of nearest point on line
**				d                  distance from ept to npt
**    RETURNS      :   0 = projected point is outside segment, before spt
**                     1 = point is in segment
**                     2 = point is outside segment (>pt1 in front)
**    SIDE EFFECTS : none
**    WARNINGS     : the tolerance is hardcoded as UM_FUZZ
*********************************************************************/
int um_nptsg(pt,spt,ulvc,dln,npt,d)
UM_coord pt,spt,npt;
UM_vector ulvc;
UU_REAL dln,*d;
{
	UM_vector vc;	
	UM_length len;	

	um_vcmnvc(pt, spt, vc);
	len = um_dot(ulvc, vc);
	um_vctmsc(ulvc, len, vc);
	um_vcplvc(spt, vc, npt);

	*d = dln - len;

	if (len + UM_FUZZ < 0.)
		return (0);
	else if (len < dln + UM_FUZZ)
		return (1);
	else
		return (2);
}

/*********************************************************************
**    E_FUNCTION     : um_nptsg1 (pt,spt,ulvc,dln,npt,d)
**       Project a point onto the line defined by a segment, and decide
**       if the projection is inside the segment.
**       Returns the squared distance from the point to the segment
**    PARAMETERS   
**       INPUT  : 
**          pt                 coordinates of given point
**          spt                start point of segment
**          ulvc               unit vector along segment
**          dln                length of segment
**       OUTPUT :  
**				npt                coordinates of nearest point on line
**				d                  distance from the point to the segment
**    RETURNS      :   0 = projected point is outside segment, before spt
**                     1 = point is in segment
**                     2 = point is outside segment, after last point
**    SIDE EFFECTS : none
**    WARNINGS     : the tolerance is hardcoded as UM_FUZZ
*********************************************************************/
int um_nptsg1 (pt,spt,ulvc,dln,npt,d)
UM_coord pt,spt,npt;
UM_vector ulvc;
UU_REAL dln,*d;
{
	UM_vector vc;	
	UM_length len;	

	um_vcmnvc(pt, spt, vc);
	len = UM_DOT(ulvc, vc);

	if (len + UM_FUZZ < 0.)
	{
		*d = UM_DOT(vc,vc);
		um_vctovc (spt,npt);
		return (0);
	}
	else if (len < dln + UM_FUZZ)
	{
		*d = UM_DOT(vc,vc) - len*len;
		um_vctmsc(ulvc, len, vc);
		um_vcplvc(spt, vc, npt);
		return (1);
	}
	else
	{
		um_translate_point (vc,-dln,ulvc, vc);
		*d = UM_DOT (vc,vc);
		um_translate_point (spt,dln,ulvc, npt);
		return (2);
	}
}

/*********************************************************************
**    FUNCTION : int um_pt_in_contour (pp,np,vx,vy,ptt,tol)
**
**    Determines if a point is inside a polygon (more exactly, the polygon's
**    projection on the (vx,vy) plane). Borrowed from "Computational
**    geometry in C" by O'Rourke.
**
**    PARAMETERS
**       INPUT  :
**          np  - number of points in polygon
**          pp  - the polygon points
**          vx  - the 'X' direction
**          vx  - the 'Y' direction
**          ptt - point in question
**          tol - tolerance
**       OUTPUT :
**          none 
**
**    RETURNS  : 1 if point is in polygon; 
**               0 if point is on polygon boundary; 
**              -1 if point is outside polygon
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_pt_in_contour (pp,np,vx,vy,ptt,tol,tolsq)
UM_coord pp[];
int np;
UM_vector vx,vy;
UU_REAL tol,tolsq;
UM_coord ptt;
{
	int i,rcross,lcross;
	UU_LOGICAL rstrad,lstrad;
	UU_REAL ax,bx,ay,by,x;
	UM_vector vc;

/*
..... We determine the number of boundary intersections of the ray from ptt
..... in the positive X direction. Then we do the same for the 180 rotation
..... of the polygon around ptt. Theory says if the numbers are of different
..... parity, the point ptt is on edge.
*/
	rcross = lcross = 0;
	um_vcmnvc (pp[0],ptt,vc);
	ax = UM_DOT (vc,vx); ay = UM_DOT (vc,vy);
	if (ax*ax + ay*ay < tolsq) return (0);

	for (i = 1; i < np; i++)
	{
		um_vcmnvc (pp[i],ptt,vc);
		bx = UM_DOT (vc,vx); by = UM_DOT (vc,vy);
		if (bx*bx + by*by < tolsq) return (0);
/*
..... return zero if too close to a vertex.
*/
		rstrad = (by > tol != ay > tol);
		lstrad = (by < -tol != ay < -tol);
/*
..... decide if the edge crosses the ray going from
..... the point in positive x-direction
*/
		if (rstrad || lstrad)
		{
			x = (ax*by - ay*bx)/(by - ay);
			if (rstrad && x > 0.) rcross++;
			if (lstrad && x < 0.) lcross++;
		}
		ax = bx; ay = by;
	}
/*
..... odd number of crossings means "inside"; even means "outside"
*/
	if (rcross%2 != lcross%2)
		return (0);
	else if ((rcross%2) == 1) 
		return (1);
	else
		return (-1);
}
