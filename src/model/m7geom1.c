/********************************************************************
**
** NAME: m7geom1.c
** CONTAINS: geometric functions
**
**      um_is_triangle_CCW
**      um_triangle_make_CCW
**      um_is_triangle_valid
**      um_maxcoord
**      um_mincoord
**      um_calc_rect
**      um_on_cylinder
**      um_between
**      um_segments_isect
**      um_linear_curve
**      um_extreme_crv
**      um_farpt_crv
**      um_box_on_plane
**      um_4pts_plane
**      um_proj_pt_on_plane
**
**    COPYRIGHT 1998 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m7geom1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:08
*********************************************************************/

#include "mgeom.h"
#include "nccs.h"
#include "uminmax.h"
#include "mfort.h"
#include "nclfc.h"
#include "mcvh.h"
#include "ncl.h"

#define GO_DONE { status = UU_FAILURE; goto Done; }

/*********************************************************************
** FUNCTION: int um_is_triangle_CCW (tript,p,norm)
**
** Determine if a triangle is oriented counter clockwise (CCW).
**
** PARAMETERS
**    INPUT:
**            tript - triangle indices
**            p     - list of (all) points
**            norm  - list of (all) normals
**    OUTPUT:
**            none
**
** RETURNS:	UU_SUCCESS
**
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
int um_is_triangle_CCW (tript,p,norm)
UM_tript *tript;
UM_coord *p;
UM_vector *norm;
{
	int i1,i2,i3;
	UM_vector n1,n2,v;

	i1 = (int)tript->n1; i2 = (int)tript->n2; i3 = (int)tript->n3;
/*
... look onto the triangle in the opposite direction to its normal
*/
	um_vcmnvc (p[i2],p[i1],n1);
	um_cross (norm[i1],n1,n2);

	um_vcmnvc (p[i3],p[i1],v);

	if (UM_DOT (n2,v) < 0.)
		return (UU_FALSE);
	else
		return (UU_TRUE);
}

/*********************************************************************
** FUNCTION: int um_triangle_make_CCW (triangle)
**
** Re-orders triangle vertices such that the resulting
** triangle is oriented counter clockwise (CCW)
**
** PARAMETERS
**    INPUT:
**            triangle - triangle
**    OUTPUT:
**            none
**
** RETURNS:	UU_SUCCESS
**
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
int um_triangle_make_CCW (triangle, trian)
UM_triangle *triangle, *trian;
{
	UM_vector n1,n2,v;
/*
... look onto the triangle in the opposite direction to its normal
*/
	um_vcmnvc (triangle->p2,triangle->p1,n1);
	um_cross (triangle->norm1,n1,n2);

	um_vcmnvc (triangle->p3,triangle->p1,v);

	*trian = *triangle;

	if (um_dot (n2,v) < 0)
	{
		um_vctovc (triangle->p2,trian->p3);
		um_vctovc (triangle->p3,trian->p2);

		um_vctovc (triangle->norm2,trian->norm3);
		um_vctovc (triangle->norm3,trian->norm2);
	}

	return (UU_SUCCESS);
}

/*********************************************************************
** FUNCTION: UU_LOGICAL um_is_triangle_valid (tript,p,tolsq)
**
** Determine if a triangle is non-degenerate.
**
** PARAMETERS
**    INPUT:
**            i1,i2,i3 - triangle indices
**            p        - list of (all) points
**            tolsq    - squared tolerance
**    OUTPUT:
**            none
**
** RETURNS:	true if valid, else false
**
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
UU_LOGICAL um_is_triangle_valid (i1,i2,i3,p,tolsq)
int i1,i2,i3;
UM_coord *p;
UU_REAL tolsq;
{
	int i;
	UM_vector v[3];
	UU_REAL dd[3];

	um_vcmnvc (p[i3],p[i2],v[0]);
	um_vcmnvc (p[i1],p[i3],v[1]);
	um_vcmnvc (p[i2],p[i1],v[2]);

	for (i = 0; i < 3; i++)
	{
		dd[i] = UM_DOT (v[i],v[i]);
		if (dd[i] < tolsq) return (UU_FALSE);
	}

	return (UU_TRUE);
}

/*********************************************************************
**    FUNCTION : UU_REAL um_mincoord (coord, np, p, nmin)
**
**      Finds min coordinate of a set of points
**
**    PARAMETERS
**       INPUT  :
**                  coord - coordinate number: xyz = 012
**                  np -  # of points
**                  p  -  pointer to the beginning of the array of points
**       OUTPUT :
**                  nmin - number of the point with min coordinate
**
**    RETURNS      : min of the coordinates of the set
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_mincoord (coord, np, p, nmin)
int coord,np;
UM_2Dcoord *p;
int *nmin;
{
	UU_REAL xmin;
	int i;

	xmin = p[0][coord];
	*nmin = 0;
	for(i=1; i<np;i++)
	{
		if(p[i][coord] < xmin)
		{
			*nmin = i;
			xmin = p[i][coord];
		}
	}

	return (xmin);
}

/*********************************************************************
**    FUNCTION : UU_REAL um_maxcoord (coord, np, p, nmax)
**
**      Finds max coordinate of a set of points
**
**    PARAMETERS
**       INPUT  :
**                  coord - coordinate number: xyz = 012
**                  np -  # of points
**                  p  -  pointer to the beginning of the array of points
**       OUTPUT :
**                  nmax - number of the point with max(x)
**
**    RETURNS      : max of the coordinates of the set
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_maxcoord (coord, np, p, nmax)
int coord,np;
UM_2Dcoord *p;
int *nmax;
{
	UU_REAL xmax;
	int i;

	xmax = p[0][coord];
	*nmax = 0;
	for(i=1; i<np;i++)
	{
		if(p[i][coord] > xmax)
		{
			*nmax = i;
			xmax = p[i][coord];
		}
	}

	return (xmax);
}

/*********************************************************************
**    E_FUNCTION     : um_calc_rect (plane,pts,npts,center,xaxis,yaxis,x,y)
**       Calculate a minimum area rectangle around a planar polygon.
**    PARAMETERS
**       INPUT  :
**          plane  - plane of the polygon.
**          pts    - array of points representing closed curve, last
**                   point is same as the first point in array.
**          npt    - number of points in pts array.
**       OUTPUT :
**          center - center of rectangle.
**          xaxis  - vector along an axis of rectangle
**          yaxis  - vector along another axis of rectangle
**          x,y    - the dimensions of rectangle
**    RETURNS      :
**       UU_SUCCESS/UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : the routine does not check whether the polygon is on
**                   the plane.
*********************************************************************/
int um_calc_rect (plane,pts,npts,center,xaxis,yaxis,x,y)
UM_coord *pts,center,xaxis,yaxis;
UU_REAL *x,*y,plane[2][3];
int npts;
{
	int i,j,status,imin;
	C2vh  cvh;
	UU_REAL d,xmax,xmin,ymax,area,areamin,x0,x1,y1;
	UM_vector vc,xvec;

	status = UU_SUCCESS;

	*x = 0.; *y = 0;
	um_nullvc (xaxis); um_nullvc (yaxis);
	um_vctovc (pts[0],center);
	if (npts < 3) return (UU_FAILURE);

	cvh.no_pt = npts;
	cvh.pt  = (UU_REAL (*)[3]) pts;
	cvh.cv  = (int *) uu_toolmalloc ((cvh.no_pt+1)*sizeof(int));
	um_plncvh(&cvh, plane);

	if (cvh.shape & UM_CVH_SMALL) GO_DONE;

	areamin = 1.e+50;
	imin = -1;
	for (i = 0; i < cvh.no_cv - 1; i++)
	{
		um_vcmnvc (pts[cvh.cv[i+1]], pts[cvh.cv[i]],xvec);
		d = um_mag (xvec);
		if (d < UM_DFUZZ) continue;
/*
..... um_plncvh is supposed to eliminate double points, so d is not 0
*/
		for (j = 0; j < 3; j++) xvec[j] /= d;
		x0 = y1 = 0.; x1 = d;
		for (j = 0; j < cvh.no_cv - 1; j++)
		{
			if (i == cvh.no_cv - 2 && j == 0) continue;
			if (j == i || j == i+1) continue;
			um_vcmnvc(pts[cvh.cv[j]], pts[cvh.cv[i]], vc);
			d = um_dot (xvec,vc);
			if (d > x1) x1 = d;
			if (d < x0) x0 = d;
			d = sqrt (um_dot(vc,vc) - d*d);
			if (d > y1) y1 = d;
		}
		area = (x1 - x0)*y1;
		if (area < areamin)
		{
			areamin = area; imin = i;
			xmin = x0; xmax = x1; ymax = y1;
			um_vctovc (xvec,xaxis);
			if (areamin < UM_DFUZZ) break;
		}
	}
	if (imin < 0) GO_DONE;

	um_cross (plane[1],xaxis,yaxis);
	i = imin + 2;
	if (i >= cvh.no_cv) i -= cvh.no_cv;
	um_vcmnvc (pts[cvh.cv[i]],pts[cvh.cv[imin+1]],vc);
	if (um_dot (yaxis,vc) < 0.) um_vctmsc (yaxis,-1.,yaxis);
	um_unitvc (yaxis,yaxis);

	*x = xmax - xmin;
	*y = ymax;

	um_vctmsc(xaxis,0.5*(xmin+xmax),vc);
	um_vcplvc(pts[cvh.cv[imin]], vc, center);
	um_vctmsc(yaxis,0.5*ymax,vc);
	um_vcplvc(center, vc, center);

Done:;
	if (cvh.cv) uu_toolfree (cvh.cv);
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : um_on_cylinder (cyl,pt1,pt2,pt3,tpt,tol)
**       Determine if a point is on a cylinder and projects inside an arc.
**    PARAMETERS
**       INPUT  :
**          cyl         - cylinder given by 7 numbers: xyzijkr
**          pt1,pt2,pt3 - points defining an arc on cylinder (the points
**                        must be on the same level re cylinder axis).
**          tpt         - point to test.
**          tol         - tolerance.
**       OUTPUT : none
**    RETURNS      :
**       UU_TRUE/UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : the routine does not check whether pts1,pts2,pts3 do
**                   define an arc on the cylinder.
**                   the routine does not check whether the cylinder axis
**                   is a unit vector, and whether its radius is > tol.
*********************************************************************/
UU_LOGICAL um_on_cylinder (cyl,pt1,pt2,pt3,tpt,tol)
UU_REAL *cyl;
UM_coord pt1,pt2,pt3,tpt;
UU_REAL tol;
{
	int i;
	UU_REAL dt,told,a12,a23,a13,a1t,at3;
	UM_vector vc,v1,v2,v3,vt;
	UM_coord center;
	UU_LOGICAL ccw;

	um_vcmnvc (tpt,cyl,vc);
	dt = um_dot (vc,&cyl[3]);
	um_translate_point (vc,-dt,&cyl[3],vt);
	dt = um_mag(vt);
	if (fabs (dt - cyl[6]) > tol) return (UU_FALSE);
	for (i = 0; i < 3; i++) vt[i] /= dt;

	um_nptln (pt2,cyl,&cyl[3],center);
	um_vcmnvc (pt1,center,v1);
	um_vcmnvc (pt2,center,v2);
	um_vcmnvc (pt3,center,v3);
	for (i = 0; i < 3; i++) v1[i] /= cyl[6];
	for (i = 0; i < 3; i++) v2[i] /= cyl[6];
	for (i = 0; i < 3; i++) v3[i] /= cyl[6];

	told = (0.5*tol*tol)/(cyl[6]*cyl[6]);
/*
..... first check if the point is close to pts1 or pts3
*/
	a1t = um_dot(vt,v1);
	if (1. - a1t < told) return (UU_TRUE);
	at3 = um_dot(vt,v3);
	if (1. - at3 < told) return (UU_TRUE);
/*
..... check if the arc is degenerate
*/
	a12 = um_dot(v1,v2);
	a13 = um_dot(v1,v3);
	if (1. - a13 < told)
	{
		if (1. - a12 < told) return (UU_FALSE); /* empty arc */
		else return (UU_TRUE); /* full circle */
	}
	a23 = um_dot(v2,v3);
/*
..... check if pts2 really defines an arc
*/
	if (1. - a23 < told || 1. - a12 < told) return (UU_TRUE);

	a12 = acos (a12);
	um_cross(v1,v2,vc);
	dt = um_dot(vc,&cyl[3]);
	if (dt < 0.) a12 = UM_TWOPI - a12;

	a13 = acos (a13);
	um_cross(v1,v3,vc);
	dt = um_dot(vc,&cyl[3]);
	if (dt < 0.) a13 = UM_TWOPI - a13;

	a23 = acos (a23);
	um_cross(v2,v3,vc);
	dt = um_dot(vc,&cyl[3]);
	if (dt < 0.) a23 = UM_TWOPI - a23;

	ccw = (fabs(a13 - a12 - a23) < UM_FUZZ);

	a1t = acos (a1t);
	um_cross(v1,vt,vc);
	dt = um_dot(vc,&cyl[3]);
	if (dt < 0.) a1t = UM_TWOPI - a1t;

	at3 = acos (at3);
	um_cross(vt,v3,vc);
	dt = um_dot(vc,&cyl[3]);
	if (dt < 0.) at3 = UM_TWOPI - at3;


	return ((fabs(a13 - a1t - at3) < UM_FUZZ) == ccw);
}

/*********************************************************************
**    E_FUNCTION: um_between (a,b,c)
**     Check whether a 2D point c is between two 2D points a,b.
**    PARAMETERS
**       INPUT  :
**          a,b,c
**       OUTPUT : none
**    RETURNS      :   UU_TRUE / UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static UU_LOGICAL um_between (a,b,c)
UM_2Dcoord a,b,c;
{
	if (fabs(a[0] - b[0]) > UM_FUZZ)
	{
		return ((a[0] <= c[0]) && (c[0] <= b[0])) ||
				((a[0] >= c[0]) && (c[0] >= b[0]));
	}
	else
	{
		return ((a[1] <= c[1]) && (c[1] <= b[1])) ||
				((a[1] >= c[1]) && (c[1] >= b[1]));
	}
}

/*********************************************************************
**    E_FUNCTION: um_segments_isect (p1,p2,q1,q2)
**     Check whether two 2-D segments intersect.
**    PARAMETERS
**       INPUT  :
**          p1,p2 - start and end points of first segment
**          q1,q2 - start and end points of second segment
**       OUTPUT : none
**    RETURNS      :   UU_FALSE if no intersection, UU_TRUE else
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_segments_isect (p1,p2,q1,q2)
UM_2Dcoord p1,p2,q1,q2;
{
	UU_REAL pp1,pp2,qq1,qq2;

	pp1 = um_triangle_signed_area (p1,p2,q1);
	if (fabs(pp1) < UM_DFUZZ)
		return (um_between (p1,p2,q1));
	pp2 = um_triangle_signed_area (p1,p2,q2);
	if (fabs(pp2) < UM_DFUZZ)
		return (um_between (p1,p2,q2));

	qq1 = um_triangle_signed_area (q1,q2,p1);
	if (fabs(qq1) < UM_DFUZZ)
		return (um_between (q1,q2,p1));
	qq2 = um_triangle_signed_area (q1,q2,p2);
	if (fabs(qq2) < UM_DFUZZ)
		return (um_between (q1,q2,p2));

	return (pp1*pp2 < 0 && qq1*qq2 < 0.);
}

/*********************************************************************
**    E_FUNCTION: um_isect_segs (pt1,vln,dln,p,q,tol,ptd)
**     Intersect two 2-D segments.
**    PARAMETERS
**       INPUT  :
**          pt1   - start point of first segment
**          vln   - unit vector of first segment
**          dln   - length of first segment
**          p     - start point of second segment
**          q     - end point of second segment
**          tol   - tolerance
**       OUTPUT :
**          ptd   - intersection point; if the segments overlap,
**                  the point closest to pt1
**    RETURNS      :   0 if no intersection, 1 if there is
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_isect_segs (pt1,vln,dln,p,q,tol,ptd)
UM_coord pt1,p,q;
UM_2Dcoord vln;
UU_REAL dln, tol;
UM_pointd *ptd;
{
	UU_REAL dpq,t,det;
	UM_2Dcoord pq,v;
	int nn;

	nn = 0;

	pq[0] = q[0] - p[0]; pq[1] = q[1] - p[1];
	dpq = UM_MAG_2D (pq);
	if (dpq < tol)
	{
/*
..... if second segment is too small: return the middle if it is
..... inside the first segment
*/
		pq[0] = 0.5 * (q[0] + p[0]); pq[1] = 0.5 * (q[1] + p[1]);
		nn = um_ptinsg (pt1,vln,dln,pq,tol,&ptd->param);
		if (nn == 1)
		{
			ptd->point[2] = 0.;
			ptd->point[0] = pq[0]; ptd->point[1] = pq[1];
		}
	}
	else
	{
		pq[0] /= dpq; pq[1] /= dpq;
		det = vln[0]*pq[1] - vln[1]*pq[0];
		if (fabs(det) < UM_DFUZZ)
		{
/*
..... if lines are parallel: if pt1 is in [p,q] return pt1;
..... else if p,or q, or both found inside the first segment, return
..... the one closest to pt1
*/
			nn = um_ptinsg (p,pq,dpq,pt1,tol,&t);
			if (nn == 1)
			{
				ptd->param = 0;
				um_vctovc (pt1,ptd->point);
			}
			else
			{
				int nnp,nnq;

				nnp = um_ptinsg (pt1,vln,dln,p,tol,&ptd->param);
				nnq = um_ptinsg (pt1,vln,dln,q,tol,&t);
				if (nnp + nnq > 0)
				{
					nn = 1;
					ptd->point[2] = 0.;
					if (nnq == 1 && (nnp == 0 || t < ptd->param))
					{
						ptd->param = t;
						ptd->point[0] = q[0]; ptd->point[1] = q[1];
					}
					else
						ptd->point[0] = p[0]; ptd->point[1] = p[1];
				}
			}
		}
		else
		{
/*
..... generic case: calculate the intersection of 2 lines; return it if
..... inside each segment
*/
         v[0] = pt1[0] - p[0]; v[1] = pt1[1] - p[1];
			t = (pq[0]*v[1] - pq[1]*v[0])/det;
/*
..... t is the distance from pt1 to the intersection along vln
*/
			if (t > -tol && t < dln + tol)
			{
				if (t < 0.) t = 0.;
				else if (t > dln) t = dln;
				ptd->param = t;

				t = (vln[0]*v[1] - vln[1]*v[0])/det;
				if (t > -tol && t < dpq + tol)
				{
					nn = 1;
					ptd->point[2] = 0.;
					ptd->point[0] = pt1[0] + ptd->param * vln[0];
					ptd->point[1] = pt1[1] + ptd->param * vln[1];
				}
			}
		}
	}

	return (nn);
}

/*********************************************************************
**    E_FUNCTION: int um_ptinsg (pt1,vln,dln,pti,tol,t)
**       Checks if point lies on the segment of line within
**       specified tolerance. If tol>0, a point within tolerance to
**       an endpoint is counted "in segment". If tol<0, the endpoints
**       are excluded.
**    PARAMETERS
**       INPUT  :
**          pti   - input point in question
**          pt1   - start point of segment
**          vln   - unit vector of the segment
**          dln   - length of segment
**          tol   - tolerance
**       OUTPUT :
**          t     - distance from pt1 if in segment
**    RETURNS      :   0 = point is outside segment
**                     1 = point is in segment
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_ptinsg (pt1,vln,dln,pti,tol,t)
UM_2Dcoord pti,pt1,vln;
UU_REAL dln,tol,*t;
{
	UU_REAL dp;
	UM_2Dcoord vp;

	vp[0] = pti[0] - pt1[0]; vp[1] = pti[1] - pt1[1];
	dp = vp[0]*vln[0] + vp[1]*vln[1];
	*t = dp;

	if (dp > -tol && dp < dln + tol)
	{
		if (vp[0]*vp[0] + vp[1]*vp[1] - dp*dp < tol*tol)
		{
			if (dp < 0.) dp = 0.;
			else if (dp > dln) dp = dln;
			return (1);
		}
	}
	return (0);
}

/*********************************************************************
**    FUNCTION : UU_LOGICAL um_linear_curve (pts,npts,)
**
**      Determines if a set of points lie on a line
**
**    PARAMETERS
**       INPUT  :
**                  pts   - Set of points to check for planar.
**                  npts  - Number of points in 'pts'.
**       OUTPUT :
**                  none
**
**    RETURNS      : UU_TRUE if points lie on a line.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_linear_curve (pts,npts)
UM_coord *pts;
int npts;
{
	int status, indx;
	UM_real8 tol8;
	UU_REAL chord, tol;
	UM_coord pt1,pt2;

	gettol (&tol8);
	tol = tol8;
/*
.....get extrema points on the curve
*/
	status = um_extreme_crv (pts,npts,pt1,pt2);

/*
.....Checks list of points for maximum distance from
.....line defined by the extrema points .If checked point is
.....outside segment than distance from closest end point is used.
*/
	chord = ncl_h_cnpts (pt1,pt2,pts,npts,&indx);

/*
.....if chordal dist is less than tol then the points are collinear
*/
	if(indx == npts -1) return(UU_TRUE);
	else return(UU_FALSE) ;
}

/*********************************************************************
**    FUNCTION :  um_extreme_crv (pts,npts,pt1,pt2)
**
**      Calculates the extrema points on a curve
**
**    PARAMETERS
**       INPUT  :
**                  pts   - Set of points to check for planar.
**                  npts  - Number of points in 'pts'.
**       OUTPUT :
**                  pt1,pt2 - extrema points
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_extreme_crv (pts,npts,pt1,pt2)
UM_coord *pts;
int npts;
UM_coord pt1,pt2;
{
	int status;
/*
.....get the farthest point on the curve from the first point
.....on the curve to obtain one extrema point
*/
	status = um_farpt_crv (pts,npts,pts[0],pt1);

/*
.....get the farthest point on the curve from the above
.....extreme point, to obtain the other extreme point
*/
	status = um_farpt_crv (pts,npts,pt1,pt2);
	return 0;
}

/*********************************************************************
**    FUNCTION :  um_farpt_crv (pts,npts,pt1,pt2)
**
**      Calculates from a point on the curve the farthest point
**		on a curve .
**
**    PARAMETERS
**       INPUT  :
**                  pts   - Set of points to check for planar.
**                  npts  - Number of points in 'pts'.
**					pt1   - first point
**       OUTPUT :
**                  pt2	  - farthest point
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_farpt_crv (pts,npts,pt1,pt2)
UM_coord *pts;
int npts;
UM_coord pt1,pt2;
{
	int i,indx = 0;
	UU_REAL dist,dmax = 0.0;

	for (i=0; i<npts; i++)
	{
		dist = um_dcccc(pt1,pts[i]);
		if (dist > dmax)
		{
			dmax = dist;
			indx= i;
		}
	}
	um_vctovc(pts[indx],pt2);
	return 0;
}

/*********************************************************************
**    E_FUNCTION: void um_box_on_plane(pts,npts,ppt,pve,cpt,flag)
**       Create a 2d bounding box on a plain from a set of points.
**    PARAMETERS
**       INPUT  :
**          pts   - set of points
**          npts  - number of points
**          ppt   - point on the plane
**          pve   - plane normal
**          flag  - UU_TRUE = Box is used for creating planar surface
**                            (box is expanded by 1 and the returned
**                             points form 2 lines).
**                  UU_FALSE = Return bounding box without expansion
**                             and with points in CCLW order.
**       OUTPUT :
**          cpt   - coordinates of the bounding box on the plane
**    RETURNS      :   none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_box_on_plane(pts,npts,ppt,pve,cpt,flag)
UM_coord *pts;
int npts;
UM_coord ppt;
UM_vector pve;
UM_coord *cpt;
UU_LOGICAL flag;
{
	UM_vector v1,v2;
	int i;
	UM_real8 d1, d2, d1max, d1min, d2max, d2min;
	UM_coord pt1,pt2,pt3,pt4,*p1max,*p1min,*p2max,*p2min;
/*
.....Create 2 vectors perp to plane normal and each other and find max
.....and min evolved points in these directions.
*/
	um_perpvc(pve,v1);
	um_unitvc(v1,v1);
	um_cross (v1,pve,v2);

	for (i=0; i<npts; i++,pts++)
	{
		d1 = UM_DOT (pts[0],v1);
		if (i==0 || d1>d1max)
		{
			d1max = d1;
			p1max = pts;
		}
		if (i==0 || d1<d1min)
		{
			d1min = d1;
			p1min = pts;
		}
		d2 = UM_DOT (pts[0],v2);
		if (i==0 || d2>d2max)
		{
			d2max = d2;
			p2max = pts;
		}
		if (i==0 || d2<d2min)
		{
			d2min = d2;
			p2min = pts;
		}
	}
/*
.....Expand max and min points by 1 inch.
*/
	if (flag)
	{
		um_vcplvc(p1max,v1,pt1);
		um_vcmnvc(p1min,v1,pt2);
		um_vcplvc(p2max,v2,pt3);
		um_vcmnvc(p2min,v2,pt4);
	}
	else
	{
		um_vctovc(p1max,pt1);
		um_vctovc(p1min,pt2);
		um_vctovc(p2max,pt3);
		um_vctovc(p2min,pt4);
	}
/*
.....Project max & min points onto planes through adjacent points to
.....get corner (control) points of surface.
*/
	if (flag)
	{
		um_nptpln(pt4,pt2,v1,cpt[0]);
		um_nptpln(pt3,pt2,v1,cpt[1]);
		um_nptpln(pt4,pt1,v1,cpt[2]);
		um_nptpln(pt3,pt1,v1,cpt[3]);
	}
	else
	{
		um_nptpln(pt4,pt2,v1,cpt[0]);
		um_nptpln(pt3,pt2,v1,cpt[1]);
		um_nptpln(pt3,pt1,v1,cpt[2]);
		um_nptpln(pt4,pt1,v1,cpt[3]);
	}
/*
.....Project surface control points onto plane.
*/
	um_nptpln(cpt[0],ppt,pve,cpt[0]);
	um_nptpln(cpt[1],ppt,pve,cpt[1]);
	um_nptpln(cpt[2],ppt,pve,cpt[2]);
	um_nptpln(cpt[3],ppt,pve,cpt[3]);
	return (0);
}

/*********************************************************************
**    FUNCTION :  um_4pts_plane (pts,tol)
**      Checks if four points are coplanar.
**    PARAMETERS
**       INPUT  :
**                  pts   - Set of points to check for planar.
**                  tol   - tolerance
**       OUTPUT :  none
**
**    RETURNS      : UU_TRUE iff coplanar
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_4pts_plane (pts,tol)
UM_coord pts[];
UU_REAL tol;
{
	UU_REAL d;
	int istat,i,N;
	UM_coord plpt;
	UM_vector plvc,vec;

	N = 4;
	istat = um_ptstopln (N,pts,plpt,plvc,tol);
	if (istat == UU_SUCCESS)
	{
/*
.....Verify all points lie on plane
*/
		for (i = 0; i < N; i++)
		{
			um_vcmnvc(pts[i],plpt,vec);
			d = UM_DOT (vec,plvc);
			if (fabs(d) > tol) return (UU_FALSE);
		}
	}

	return (UU_TRUE);
}
/*********************************************************************
** FUNCTION: ind um_proj_pt_on_plane (np, p, plane, proj)
**                                               
**          projects a set of points in 3D onto a plane
** PARAMETERS
**    INPUT:	
**            np - number of points in the set
**            p   - pointer to array of points
**            plane - plane to project onto
**    OUTPUT: 
**            proj - pointer to array of projected points
**
** RETURNS:	none 
**
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
int um_proj_pt_on_plane (np,p,plane,proj)
int np;
UM_coord *p,*proj;
UM_plane *plane;
{
	int i;
	UM_vector vec;

	for(i=0; i<np; i++)
	{
		um_vcmnvc(p[i], plane->p0, vec);
		um_translate_point(p[i],-um_dot(vec,plane->n),plane->n,proj[i]);
	}
	return (0);
}

