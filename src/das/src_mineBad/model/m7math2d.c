/*********************************************************************
**    NAME  :  m7math2d.c
**       CONTAINS: routines for 2-D geometry
**
**  um_vctovc_2d
**  um_vcplvc_2d
**  um_middlept_2d
**  um_vcmnvc_2d
**  um_vctmsc_2d
**  um_perpvc_2d
**  um_mag_2d
**  um_dot_2d
**  um_unitvc_2d
**  um_angle_2d
**  um_cosang_2d
**  um_xytovc_2d
**  um_triangle_signed_area
**  um_point_isin_triangle
**  um_xmin_2d
**  um_xmax_2d
**  um_ymin_2d
**  um_ymax_2d
**  um_dist_2d
**  um_polygon_orientation
**  um_polygon_area
**  um_convert3D_2D
**  um_convert2D_3D
**  um_reverse_array_2d
**  um_mod
**  um_check_inside
**  um_sticky_contour
**  um_point_close
**	um_negvc_2d
**  um_sqdis_from_segment_2d
**	um_nptln_2d
**	um_point_in_segment_2d
**
** These functions are not used
**  int um_to_edge
**  um_ver_in_triangle
**  um_merge_polygons
**
**    COPYRIGHT 1997 (c) Numerical Control Computer Sciences Inc.
**                       All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       m7math2d.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:09
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "nclfc.h"
#include "udebug.h"
#include "uhep.h"
#include "mdcoord.h"
#include "modef.h"
#include "ulist.h"
#include "uminmax.h"
#include "mgeom.h"

/*********************************************************************
**    FUNCTION : um_vctovc_2d(UM_2Dcoord vi, UM_2Dcoord vo)
**
**      Copy one 2d-vector to another.
**    PARAMETERS
**       INPUT  :
**				vi        vector in
**       OUTPUT :
**				vo        vector out
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_vctovc_2d(vi,vo)
UM_2Dcoord vi, vo;
{
	vo[0]  =  vi[0];
	vo[1]  =  vi[1];

	return (0);
}

/*********************************************************************
**    FUNCTION : um_vcplvc_2d(UM_2Dcoord v1,UM_2Dcoord v2,UM_2Dcoord vr)
**
**      Add two vectors and return the result.
**    PARAMETERS
**       INPUT  :
**          v1            first vector
**          v2            second vector
**       OUTPUT :
**          vr            resultant vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_vcplvc_2d(v1,v2,vr)
UM_2Dcoord v1, v2, vr;
{
	vr[0]  =  v1[0] + v2[0];
	vr[1]  =  v1[1] + v2[1];

	return (0);
}

/*********************************************************************
**    FUNCTION : um_vcplbvc_2d(v1,b,v2,vr)
**
**      The result is v1 + sca*v2.
**    PARAMETERS
**       INPUT  :
**          v1            first vector
**          b             scalar
**          v2            second vector
**       OUTPUT :
**          vr            resultant vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_vcplbvc_2d(v1,b,v2,vr)
UM_2Dcoord v1, v2, vr;
UU_REAL b;
{
	vr[0] = v1[0] + b*v2[0];
	vr[1] = v1[1] + b*v2[1];

	return (0);
}

/*********************************************************************
**    FUNCTION : um_avcplbvc_2d(a,u,b,v,w)
**
**      The result is a*u + b*v.
**    PARAMETERS
**       INPUT  :
**          a            first scalar
**          u             first vector
**          b            second scalar
**          v            second vector
**       OUTPUT :
**          w             resultant vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_avcplbvc_2d(a,u,b,v,w)
UM_2Dcoord u, v, w;
UU_REAL a,b;
{
	w[0] = a*u[0] + b*v[0];
	w[1] = a*u[1] + b*v[1];

	return (0);
}

/*********************************************************************
**    FUNCTION : um_middlept_2d(UM_2Dcoord p1,UM_2Dcoord p2,UM_2Dcoord middle)
**
**      Get middle point on the line through 2 given points
**    PARAMETERS
**       INPUT  :
**				p1 -   first point
**          p2 -   second point
**       OUTPUT :
**			  	p -   middle point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_middlept_2d(p1,p2,middle)
UM_2Dcoord p1, p2, middle;
{
	middle[0]  =  0.5*(p1[0] + p2[0]);
	middle[1]  =  0.5*(p1[1] + p2[1]);
   return (0);
}

/*********************************************************************
**    FUNCTION : um_vcmnvc_2d(UM_2Dcoord v1,UM_2Dcoord v2,UM_2Dcoord vr)
**
**      Subtract second vector from the first vector and return the result.
**    PARAMETERS
**       INPUT  :
**				v1            first vector
**          v2            second vector
**       OUTPUT :
**				vr            resultant vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_vcmnvc_2d(v1,v2,vr)
UM_2Dcoord v1,v2,vr;
{
	vr[0]  =  v1[0] - v2[0];
	vr[1]  =  v1[1] - v2[1];
	return (0);
}

/*********************************************************************
**    FUNCTION : um_vctmsc_2d(UM_2Dcoord vi,UU_REAL sca,UM_2Dcoord vo)
**
**       Multiply a vector times a scalar.
**    PARAMETERS
**       INPUT  :
**          vi        vector in
**          sca       scalar
**       OUTPUT :
**          vo        vector out
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_vctmsc_2d(vi,sca,vo)
UM_2Dcoord vi;
UU_REAL sca;
UM_2Dcoord vo;
{
	vo[0]  =  sca * vi[0];
	vo[1]  =  sca * vi[1];
	return (0);
}

/*********************************************************************
**    FUNCTION : um_perpvc_2d(UM_2Dcoord vi,UM_2Dcoord vr)
**
**      Determine a vector perpendicular to a given vector.
**    PARAMETERS
**       INPUT  :
**				vi            given vector
**       OUTPUT :
**				vr            perpendicular vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_perpvc_2d(vi,vr)
UM_2Dcoord vi,vr;
{
		vr[0] =  - vi[1];
		vr[1] =  vi[0];

		return (0);
}

/*********************************************************************
**    FUNCTION     : um_negvc_2d(vci,vco)
**       Multiply a vector by -1.
**    PARAMETERS
**       INPUT  :
**          vci        vector in
**       OUTPUT :
**          vco        vector out
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_negvc_2d(vci,vco)
UM_2Dcoord vci,vco;
{
   vco[0]  =  -vci[0]; vco[1]  =  -vci[1];
   return (0);
}

/*********************************************************************
**    FUNCTION : UU_REAL um_mag_2d(UM_2Dcoord v1)
**      Calculate magnitude of a vector.
**    PARAMETERS
**       INPUT  :
**				v1         vector
**       OUTPUT :
**				none
**    RETURNS      :
**				mag		  magnitude of vector
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_mag_2d(v1)
UM_2Dcoord v1;
{
	UU_REAL vmag;

	vmag = v1[0]*v1[0] + v1[1]*v1[1];
	if (vmag > 0)
		vmag = sqrt (vmag);
	else
		vmag = 0;

	return (vmag);
}

/*********************************************************************
**    FUNCTION : UU_REAL um_dot_2d(UM_2Dcoord v1,UM_2Dcoord v2)
**      Calculate the dot product of two vectors.
**    PARAMETERS
**       INPUT  :
**				v1       first vector
**          v2       second vector
**       OUTPUT :
**    RETURNS   :
**				         dot product of vectors
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_dot_2d(v1, v2)
UM_2Dcoord v1, v2;
{
	return (UM_DOT_2D(v1,v2));
}

/*********************************************************************
**    FUNCTION: um_unitvc_2d(UM_2Dcoord v1,UM_2Dcoord vr)
**
**      Calculate a unit vector. If the given vector is "equal"
**      to the zero vector, return (0.0, 0.0).
**    PARAMETERS
**       INPUT  :
**				v1            vector
**       OUTPUT :
**				vr            resultant vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_unitvc_2d(v1, vr)
UM_2Dcoord v1, vr;
{
	UU_REAL vmag;

	vmag = um_mag_2d(v1);
	if (vmag < UM_DFUZZ)
	{
		vr[0] = vr[1] = 0.0;
	}
	else
	{
		vr[0] = v1[0]/vmag;
		vr[1] = v1[1]/vmag;
	}

	return (0);
}

/*********************************************************************
**    FUNCTION : UU_REAL um_cosang_2d (UM_2Dcoord vc1,UM_2Dcoord vc2)
**      Calculate the cosine of the angle between two vectors.
**    PARAMETERS
**       INPUT  :
**               vc1 - first vector
**               vc2 - second vector
**       OUTPUT :
**    RETURNS      :
**				cos - cosine of angle between vectors
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_cosang_2d (vc1, vc2)
UM_2Dcoord vc1, vc2;
{
	UM_2Dcoord uvc1;		/* unit vector along first vector */
	UM_2Dcoord uvc2;		/* unit vector along second vector */
	UU_REAL cos;

	um_unitvc_2d(vc1, uvc1);
	um_unitvc_2d(vc2, uvc2);
	cos = UM_DOT_2D(uvc1, uvc2);

	if (cos < -1.0) cos = -1.0;
	else if (cos > 1.0) cos = 1.0;

	return (cos);
}

/*********************************************************************
**    FUNCTION : UU_REAL um_angle_2d(UM_2Dcoord vc1,UM_2Dcoord vc2)
**      Calculate the angle between two vectors.
**    PARAMETERS
**       INPUT  :
**               vc1 - first vector
**               vc2 - second vector
**       OUTPUT :
**    RETURNS      :
**				ang - angle between vectors
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_angle_2d(vc1, vc2)
UM_2Dcoord vc1, vc2;
{
	UM_angle ang;
	UU_REAL cos;

	cos = um_cosang_2d (vc1, vc2);

	ang = acos(cos);
	return (ang);
}

/*********************************************************************
**    E_FUNCTION     : um_identtf_2d( tfo )
**			Return the identity transformation
**    PARAMETERS
**       INPUT  :
**				none
**       OUTPUT :
**          tfo			identity transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_identtf_2d( tfo )
UM_2Dtransf	tfo;
{
	int i,j;

	for (i=0; i<3; i++)
		for (j=0; j<3; j++) tfo[i][j] = UM_idmat[i][j];
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : um_tftotf_2d( tfi, tfo )
**			Copy transformation
**    PARAMETERS
**       INPUT  :
**				tfi			transformation
**       OUTPUT :
**          tfo			copied transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_tftotf_2d( tfi, tfo )
UM_2Dtransf	tfi;
UM_2Dtransf	tfo;
{
	int i,j;

	if (tfi == UM_DEFAULT_TF)
		um_identtf_2d(tfo);
	else
		for (i=0; i<3; i++)
			for (j=0; j<3; j++)
				tfo[i][j] = tfi[i][j];
}

/*********************************************************************
**    E_FUNCTION     :	um_tftmtf_2d( tf1, tf2, tfr )
**       Multiply two 3X3 transformations matrices and return the
**			result tfr = tf1 * tf2. The resultant matrix may be identical
**			to either of the input transformations.
**    PARAMETERS
**       INPUT  :
**          tf1					first transformation
**				tf2					second transformation
**       OUTPUT :
**          tfr					resultant transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_tftmtf_2d( tf1, tf2, tfr )
UM_2Dtransf	tf1;
UM_2Dtransf	tf2;
UM_2Dtransf	tfr;
{
	int i,j,k;
	UM_2Dtransf temp;

	if (tf1 == UM_DEFAULT_TF)
		{
		if (tf2 == UM_DEFAULT_TF)
			um_identtf_2d(tfr);
		else
			um_tftotf_2d(tf2, tfr);
		goto Done;
		}
	else if (tf2 == UM_DEFAULT_TF)
		{
		um_tftotf_2d(tf1, tfr);
		goto Done;
		}

	for (i=0; i<3; i++)
		for (j=0; j<3; j++)
			{
			temp[i][j] = 0.0;
			for (k=0; k<3; k++) temp[i][j] = temp[i][j] + (tf1[i][k]*tf2[k][j]);
			}

	um_tftotf_2d(temp, tfr);


Done:;
}

/*********************************************************************
**    E_FUNCTION     :	um_disptf_2d( delta, tfr )
**       Return the 3X3 translation transformation for a given
**			delta = (dx,dy).
**    PARAMETERS
**       INPUT  :
**				delta			delta vector
**       OUTPUT :
**          tfr			resulting transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
um_disptf_2d( delta, tfr )
UM_2Dcoord	delta;
UM_2Dtransf	tfr;
{
	int i,j;

	for (i=0; i<3; i++)
		for (j=0; j<3; j++) tfr[i][j] = UM_idmat[i][j];
	for (i=0; i<2; i++) tfr[i][2] = delta[i];
}

/*********************************************************************
**    E_FUNCTION     : UU_REAL um_sqdis(p1,p2)
**      Calculate the squared distance between two points.
**    PARAMETERS
**       INPUT  :
**          p1          first point
**          p2          second point
**       OUTPUT :
**    RETURNS      :
**          squared distance
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_sqdis_2d(p1,p2)
UM_2Dcoord p1,p2;
{
	UU_REAL d;
	UM_2Dcoord vc;

	um_vcmnvc_2d(p1,p2,vc);
	d = UM_DOT_2D(vc,vc);
	return (d);
}

/*********************************************************************
**    FUNCTION : UU_REAL um_dist_2d(p1,p2)
**
**      Finds distance between two 2D-points
**
**    PARAMETERS
**       INPUT  :
**                  p1, p2  -  points
**       OUTPUT :
**                  none
**
**    RETURNS :  distance between p1,p2
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_dist_2d(p1, p2)
UM_2Dcoord p1, p2;
{
	UU_REAL d;

	d = um_sqdis_2d(p1,p2);
	if (d > 0)
		d = sqrt (d);
	else
		d = 0;

	return (d);
}

/*********************************************************************
**    FUNCTION :  um_rottf_2d(UM_2Dcoord pt,UM_angle angle,UM_2Dtransf rot)
**       Return the 3X2 rotation transformation given an
**          centerfor rotation and a rotation angle.
**    PARAMETERS
**       INPUT  :
**          pt			center of rotation
**          angle       rotation angle
**       OUTPUT :
**          rot         resulting transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_rottf_2d(pt, angle,rot)
UM_2Dcoord pt;
UM_angle angle;
UM_2Dtransf rot;
{
	UU_REAL cosine, sine;
	UM_2Dtransf tran, invtran;
	UM_2Dcoord invpt;

	sine = sin(angle);
	cosine = cos(angle);

/*
.....calculate 2d rotation matrix about origin
*/
	rot[2][2] = 1.0;
	rot[2][0] = rot[2][1] =rot[0][2] = rot[1][2] = 0.0;
	rot[0][0] = rot[1][1] = cosine;
	rot[0][1] = -(rot[1][0] = sine);

/*
.....if the rotation is about a point other than the origin,
.....then translate this point to the origin rotate about
.....then do an inverse translation.
*/

	if (um_dist_2d(UM_origin,pt) > UM_FUZZ)
	{
		um_disptf_2d(pt,invtran);
		invpt[0] = -pt[0];
		invpt[1] = -pt[1];
		um_disptf_2d(invpt,tran);
		um_tftmtf_2d( rot, tran, rot);
		um_tftmtf_2d( invtran, rot, rot);

	}
}

/*********************************************************************
**    E_FUNCTION     :	um_cctmtf_2d( cci, tf, cco )
**       Multiply a cartesian coordinate by a transformation matrix
**			and return the result cco = cci * tf. The output coordinate
**			may be identical to the input.
**    PARAMETERS
**       INPUT  :
**				cci		coordinate
**				tf		transformation	-- if UM_DEFAULT_TF: identity
**       OUTPUT :
**          cco		coordinate * transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void
um_cctmtf_2d( cci, tf, cco )
UM_2Dcoord		cci;
UM_2Dtransf	tf;
UM_2Dcoord		cco;
{
	int i,j;
	UM_2Dcoord temp;
	UM_coord	tempco;

	if (tf == UM_DEFAULT_TF)
		{
			for (i=0; i<2; i++) cco[i] = cci[i];
		}
	else
		{
			for (i=0; i<2; i++) tempco[i] = cci[i];
			tempco[2] = 1;
			for (i=0; i<2; i++)
			{
				temp[i] = 0;
				for (j=0; j<3; j++) temp[i] = temp[i] + (tempco[j] * tf[i][j]);
			}

			for (i=0; i<2; i++) cco[i] = temp[i];
		}
}

/*********************************************************************
**    FUNCTION :  um_xytovc_2d(UU_REAL x, UU_REAL y, UM_2Dcoord vco)
**       Assign the X, Y  values to the vector VCO.
**    PARAMETERS
**       INPUT  :
**          x - x component
**          y - y component
**       OUTPUT :
**          vco - vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_xytovc_2d(x, y, vco)
UU_REAL x, y;
UM_2Dcoord vco;
{
	vco[0] = x;
	vco[1] = y;

	return (0);
}

/*********************************************************************
**    FUNCTION : UU_REAL um_triangle_signed_area(p1,p2,p3)
**
**      Finds signed area of a 2D-triangle (p1,p2,p3):
**      sign is + iff (p1,p2,p3) form a counterclockwise cycle
**
**    PARAMETERS
**       INPUT  :
**                  p1,p2,p3 - triangle vertices
**       OUTPUT :
**                   none
**    RETURNS      : signed area of triangle; sign is + iff (p1,p2,p3)
**                   form a counterclockwise cycle, - otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_triangle_signed_area(p1,p2,p3)
UM_2Dcoord p1, p2, p3;
{
	UU_REAL area;

	area = 0.5*( p1[0]*(p2[1] - p3[1]) -
	             p1[1]*(p2[0] - p3[0]) +
	             p2[0]*p3[1] - p3[0]*p2[1] );

	return (area);
}

/*********************************************************************
**    FUNCTION : UU_LOGICAL um_point_isin_triangle(p,p1,p2,p3)
**
**      Finds if a point is inside of a 2D-triangle
**
**    PARAMETERS
**       INPUT  :
**                  p        - point
**                  p1,p2,p3 - triangle vertices
**       OUTPUT :
**                   none
**    RETURNS      : UU_TRUE if p is inside triangle, UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : if p is on a triangle side, returns UU_FALSE
*********************************************************************/
UU_LOGICAL um_point_isin_triangle(p, p1,p2,p3,tol)
UM_2Dcoord p, p1,p2,p3;
UU_REAL tol;
{
	UM_2Dcoord v1,v2;
	UU_REAL a1,a2,a3;
	UU_REAL um_triangle_signed_area();
	UU_LOGICAL res;

	a1 = um_triangle_signed_area(p1,p2,p);
	if (fabs(a1) < tol)
	{
		um_vcmnvc_2d (p,p1,v1);
		um_vcmnvc_2d (p,p2,v2);
		if (UM_DOT_2D (v1,v2) <= 0.) return (UU_TRUE);
	}
	a2 = um_triangle_signed_area(p2,p3,p);
	if (fabs(a2) < tol)
	{
		um_vcmnvc_2d (p,p2,v1);
		um_vcmnvc_2d (p,p3,v2);
		if (UM_DOT_2D (v1,v2) <= 0.) return (UU_TRUE);
	}
	a3 = um_triangle_signed_area(p3,p1,p);	
	if (fabs(a3) < tol)
	{
		um_vcmnvc_2d (p,p1,v1);
		um_vcmnvc_2d (p,p3,v2);
		if (UM_DOT_2D (v1,v2) <= 0.) return (UU_TRUE);
	}

	res = (a1*a2 > 0.) && ( a2*a3 > 0.) ;

	return (res);
}

/*********************************************************************
**    FUNCTION : UU_REAL um_xmin_2d(int np, UM_2Dcoord *p, int *nxmin)
**
**      Finds left boundary of a set of 2D-points
**
**    PARAMETERS
**       INPUT  :
**                  np -  # of points
**                  p  -  pointer to the beginning of the array of points
**       OUTPUT :
**                  nxmin - number of the point with min(x)
**
**    RETURNS      : min of the x-coordinates of the set
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_xmin_2d(np, p, nxmin)
int np;
UM_2Dcoord *p;
int *nxmin;
{
	UU_REAL xmin;
	int i;

	xmin = p[0][0];
	*nxmin = 0;
	for(i=1; i<np; i++)
	{
		if(p[i][0] < xmin)
		{
			*nxmin = i;
			xmin = p[i][0];
		}
	}

	return (xmin);
}

/*********************************************************************
**    FUNCTION : UU_REAL um_xmax_2d(int np, UM_2Dcoord *p, int *nxmax)
**
**      Finds right boundary of a set of 2D-points
**
**    PARAMETERS
**       INPUT  :
**                  np -  # of points
**                  p  -  pointer to the beginning of the array of points
**       OUTPUT :
**                  nxmax - number of the point with max(x)
**
**    RETURNS      : max of the x-coordinates of the set
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_xmax_2d(np, p, nxmax)
int np;
UM_2Dcoord *p;
int *nxmax;
{
	UU_REAL xmax;
	int i;

	xmax = p[0][0];
	*nxmax = 0;
	for(i=1; i<np; i++)
	{
		if(p[i][0] > xmax)
		{
			*nxmax = i;
			xmax = p[i][0];
		}
	}

	return (xmax);
}

/*********************************************************************
**    FUNCTION : UU_REAL um_ymin_2d(int np, UM_2Dcoord *p, int *nymin)
**
**      Finds bottom boundary of a set of 2D-points
**
**    PARAMETERS
**       INPUT  :
**                  np -  # of points
**                  p  -  pointer to the beginning of the array of points
**       OUTPUT :
**                  nymin - number of the point with min(y)
**
**    RETURNS      : min of the y-coordinates of the set
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_ymin_2d(np, p, nymin)
int np;
UM_2Dcoord *p;
int *nymin;
{
	UU_REAL ymin;
	int i;

	ymin = p[0][1];
	*nymin = 0;
	for(i=1; i<np; i++)
	{
		if(p[i][1] < ymin)
		{
			*nymin = i;
			ymin = p[i][1];
		}
	}

	return (ymin);
}

/*********************************************************************
**    FUNCTION : UU_REAL um_ymax_2d(int np, UM_2Dcoord *p, int *nymax)
**
**      Finds top boundary of a set of 2D-points
**
**    PARAMETERS
**       INPUT  :
**                  np -  # of points
**                  p  -  pointer to the beginning of the array of points
**       OUTPUT :
**                  nymax - number of the point with max(y)
**
**    RETURNS      : max of the y-coordinates of the set
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_ymax_2d(np, p, nymax)
int np;
UM_2Dcoord *p;
int *nymax;
{
	UU_REAL ymax;
	int i;

	ymax = p[0][1];
	*nymax = 0;
	for(i=1; i<np; i++)
	{
		if(p[i][1] > ymax)
		{
			*nymax = i;
			ymax = p[i][1];
		}
	}

	return (ymax);
}

/*********************************************************************
**    FUNCTION : int um_polygon_orientation(int np, UM_2Dcoord *p)
**
**    Finds orientation of a simple polygon formed by an ordered set
**    of 2D-points
**
**    PARAMETERS
**       INPUT  :
**                  np -  # of points (vertices of polygon)
**                  p  -  pointer to the beginning of the array of
**                        points
**       OUTPUT :
**                  none
**
**    RETURNS      : +1, if the orientation is CCW;
**                   -1, if it is CW;
**                    0, if cannot be determined
**    SIDE EFFECTS : none
**    WARNINGS     : the data pointed by p is overwritten
*********************************************************************/
int um_polygon_orientation(np, p)
int np;
UM_2Dcoord *p;
{
	int i, click, nn, nxmin, next, prev;
	UU_REAL turn, xmin,yxmin,xi,yi;


	if (np <= 2) return (0);

	for (i = 0; i < np; i++)
	{
		p[i][0] = floor (10000. * p[i][0] + 0.5);
		p[i][1] = floor (10000. * p[i][1] + 0.5);
	}

	if (np == 3)
	{
		turn = um_triangle_signed_area(p[0],p[1],p[2]);
		if (turn > 0.)
			return (1);
		else if (turn < 0.)
			return (-1);
		else
			return (0);
	}
/*
... Get the number nxmin of the highest point having min x-coordinate.
... Next, find the next & previous points to nxmin along
... the polyline. Check that distances are not 0; otherwise, go further
... forwards/backwards.
*/
	xmin = p[0][0];
	yxmin = p[0][1];
	nxmin = 0;
	for (i=1; i<np;i++)
	{
		xi = p[i][0]; yi = p[i][1];
		if ((xi<xmin) || (xi==xmin && yi>yxmin))
		{
			nxmin = i;
			xmin = xi; yxmin = yi;
		}
	}

	click = 0;
	for (i=0, next = nxmin + 1; i< np; i++, next ++)
	{
		next = um_mod(next,np);
		if ((p[next][0]-xmin) + fabs(p[next][1]-yxmin) > 0.)
		{
			click = 1;
			break;
		}
	}
	if (!click) return (0);

	nn = um_mod (nxmin - next - 1, np);
	click = 0;
	for (prev = nxmin - 1, i = 0; i < nn; prev--, i++)
	{
		prev = um_mod(prev,np);
		if ((p[prev][0]-xmin) + fabs(p[prev][1]-yxmin) > 0.)
		{
			click = 1;
			break;
		}
	}

	if (!click) return (0);
/*
... orientation of the polyline is the same as that of the triangle
... (prev,nxmin,next)
*/
	turn = um_triangle_signed_area(p[prev],p[nxmin],p[next]);
	if (turn > 0.)
		return (1);
	else if (turn < -0.)
		return (-1);
	else
	{
		UU_REAL xmax,yxmax;
		int nxmax;

		xmax = p[0][0];
		yxmax = p[0][1];
		nxmax = 0;
		for (i=1; i<np;i++)
		{
			xi = p[i][0]; yi = p[i][1];
			if ((xi>xmax) || (xi==xmax && yi<yxmax))
			{
				nxmax = i;
				xmax = xi; yxmax = yi;
			}
		}

		click = 0;
		for (i=0, next = nxmax + 1; i< np; i++, next ++)
		{
			next = um_mod(next,np);
			if ((xmax -p[next][0]) + fabs(p[next][1]-yxmax) > 0.)
			{
				click = 1;
				break;
			}
		}
		if (!click) return (0);

		nn = um_mod (nxmax - next - 1, np);
		click = 0;
		for (prev = nxmax - 1, i = 0; i < nn; prev--, i++)
		{
			prev = um_mod(prev,np);
			if ((xmax-p[prev][0]) + fabs(p[prev][1]-yxmax) > 0.)
			{
				click = 1;
				break;
			}
		}

		if (!click) return (0);
/*
... orientation of the polyline is the same as that of the triangle
... (prev,nxmax,next)
*/
		turn = um_triangle_signed_area(p[prev],p[nxmax],p[next]);
		if (turn > 0.)
			return (1);
		else if (turn < -0.)
			return (-1);
		else
			return (0);
	}
}

/*********************************************************************
**    FUNCTION : void um_polygon_area (np,p,ar)
**
**    Finds area of a simple polygon formed by an ordered set of 2D-points
**
**    PARAMETERS
**       INPUT  :
**                  np -  # of points (vertices of polygon)
**                  p  -  pointer to the beginning of the array of
**                        points
**       OUTPUT :
**                  ar - area
**
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_polygon_area (nv,pp,ar)
int nv;
UM_2Dcoord *pp;
UU_REAL *ar; 
{
	int i;
	UU_REAL area; 

	for (i = 1, area = 0.; i < nv - 1; i++)
	{
		area += um_triangle_signed_area(pp[0],pp[i],pp[i+1]);
	}

	*ar = area;
}

/*********************************************************************
*********************************************************************/
static int S_area2 (ax,ay,bx,by,cx,cy)
int ax,ay,bx,by,cx,cy;
{
	return ((bx - ax)*(cy - ay) - (cx - ax)*(by - ay));
}

/*********************************************************************
**    FUNCTION : S_triangle_ccw (p1,p2,p3)
*********************************************************************/
static int S_triangle_ccw (p1,p2,p3)
UU_REAL *p1,*p2,*p3;
{
	int nx1,ny1,nx2,ny2,nx3,ny3,narea;

	nx1 = 10000 * p1[0] + 0.5;
	ny1 = 10000 * p1[1] + 0.5;

	nx2 = 10000 * p2[0] + 0.5;
	ny2 = 10000 * p2[1] + 0.5;

	nx3 = 10000 * p3[0] + 0.5;
	ny3 = 10000 * p3[1] + 0.5;

	narea = S_area2 (nx1,ny1,nx2,ny2,nx3,ny3);

	if (narea > 0)
		return (1);
	else if (narea < 0)
		return (-1);
	else
		return (0);
}

/*********************************************************************
**    FUNCTION : int um_polygon_orientation1 (np,p)
**
**    Finds orientation of a simple polygon formed by an ordered set
**    of 2D-points
**
**    PARAMETERS
**       INPUT  :
**                  np -  # of points (vertices of polygon)
**                  p  -  pointer to the beginning of the array of
**                        points
**       OUTPUT :  none
**
**    RETURNS      : +1, if the orientation is CCW;
**                   -1, if it is CW;
**                    0, if cannot be determined
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_polygon_orientation1 (np, p)
int np;
UM_2Dcoord *p;
{
	int nturn,imin,xmin,ymin,i,xi,yi,iprev,inext;
	UU_REAL area = 0.0;

	if (np <= 2)
		return (0);

	um_polygon_area(np, p, &area);
		
	if (area > 0.0)
		return (1);
	else if (area < 0.0)
		return (-1);
	else
		return (0);
}
			
/*********************************************************************
**    FUNCTION : um_convert3D_2D(int np, UM_coord *p3d, UM_2Dcoord *p2d)
**
**    Converts an array of 3D points {x_i,y_i,z_i} into an array of
**    2D points {x_i,y_i}, by "forgetting" the third coordinates z_i.
**
**    PARAMETERS
**       INPUT  :
**                  np -  # of points to be converted
**                  p3d  -  array of 3D points
**                  p2d  -  array of 2D points
**       OUTPUT :
**                  none
**
**    RETURNS  : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_convert3D_2D(np, p3d, p2d)
int np;
UM_coord *p3d;
UM_2Dcoord *p2d;
{
	int i,j;
	UU_REAL *p3,*p2;

	p3 = (UU_REAL *) p3d;
	p2 = (UU_REAL *) p2d;

	for(i=0,j=0; i<2*np; i+=2,j+=3)
	{
		p2[i] = p3[j];
		p2[i+1] = p3[j+1];
	}
	return (0);
}

/*********************************************************************
**    FUNCTION : um_convert2D_3D(int np, UM_2Dcoord *p2d, UM_coord *p3d)
**
**    Converts an array of 2D coordinates {xi,yi} into an array of
**    3D coordinates {xi,yi,0}
**
**    PARAMETERS
**       INPUT  :
**                  np -  # of points to be converted
**                  p2d  -  array of 2D points
**                  p3d  -  array of 3D points
**       OUTPUT :
**                  none
**
**    RETURNS  : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_convert2D_3D(np, p2d, p3d)
int np;
UM_2Dcoord *p2d;
UM_coord *p3d;
{
	int i,j;
	UU_REAL *p3,*p2;

	p3 = (UU_REAL *) p3d;
	p2 = (UU_REAL *) p2d;

	for(i=0,j=0; i<2*np; i+=2, j+=3)
	{
		p3[j] = p2[i];
		p3[j+1] = p2[i+1];
		p3[j+2] = 0.;
	}
	return (0);
}

/*********************************************************************
**    FUNCTION : um_reverse_array_2d(int np, UM_2Dcoord *p)
**
**    Reverses order of elements of an array of 3d coordinates: p[i] -> p[n-i];
**
**    PARAMETERS
**       INPUT  :
**                  np -  # of points in the array
**                  p  -  array of 2D points
**       OUTPUT :
**                 array p is reversed
**
**    RETURNS  : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_reverse_array_2d(np, p)
int np;
UM_2Dcoord *p;
{
	int i,j,n;
	UM_2Dcoord v;

	n = np/2;
	for (i=0, j=np-1; i<n; i++, j--)
	{
		um_vctovc_2d(p[i],v);
		um_vctovc_2d(p[j],p[i]);
		um_vctovc_2d(v,p[j]);
	}
	return (0);
}

/*********************************************************************
**    FUNCTION : int um_mod(int i, int np)
**
**   	Finds i(mod np) for cyclic enumeration of vertices of a polygon
**
**    PARAMETERS
**       INPUT  :
**                  i -  index to be reduced mod(np)
**                  np  -  number of vertices in cycle group
**       OUTPUT : none
**
**    RETURNS  : new value of index from 0 to np-1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_mod(i, np)
int i, np;
{
	int k,res;

	k = i/np;
	res = i - np*k;
	if (res < 0) res += np;

	return (res);
}

/*********************************************************************
**    FUNCTION : int um_to_edge (uv0,vec,uv)
**
**   	Given a point inside the square [0,1]*[0,1] and direction vector,
**    extrapolates the point to the closest square boundary along the direction
**    vector;
**
**    PARAMETERS
**       INPUT  :
**                  uv0  - the point inside the square [0,1]*[0,1]
**                  vec  - unit direction vector
**       OUTPUT :
**                  uv - resulting point on the square boundary
**
**    RETURNS  : UU_SUCCESS
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int um_to_edge(uv0,vec,uv)
UM_2Dcoord *uv0,*vec,*uv;
{
	UU_REAL u,v,a,b,d,dmin;
	UM_2Dcoord x[4];
	int i,imin;
/*
... intersect the line uv = uv0 + t*vec with the 4 boundaries
... of the square [0,1]*[0,1] and take the point closest to uv0
*/
	u = uv0[0][0]; v = uv0[0][1];
	a = vec[0][0]; b = vec[0][1];

	x[0][0] = x[2][0] = x[3][1] = x[1][1] = 1.e9;
/*
... intersection with v=0 axis
*/
	x[0][1] = 0.;
	if (b != 0.) x[0][0] = u - v*a/b;
	else if (v == 0.) x[0][0] = u;
/*
... intersection with u=1 axis
*/
	x[1][0] = 1.;
	if (a != 0) x[1][1] = v + (1.-u)*b/a;
	else if (u == 1.) x[1][1] = v;
/*
... intersection with v=1 axis
*/
	x[2][1] = 1.;
	if (b != 0) x[2][0] = u + (1.-v)*a/b;
	else if (v == 1.) x[2][0] = u;
/*
... intersection with u=0 axis
*/
	x[3][0] = 0.;
	if (a != 0.) x[3][1] = v - u*b/a;
	else if (u == 0.) x[3][1] = v;

	dmin = UM_DIST_2D(uv0[0], x[0]);
	imin = 0;

	for (i = 1; i < 4; i++)
	{
		d = UM_DIST_2D(uv0[0], x[i]);
		if ( d < dmin)
		{
			imin = i; dmin = d;
		}
	}

	um_vctovc_2d (x[imin],uv[0]);

	for (i = 0; i<2; i++) uv[0][i] = MIN2 (1., MAX2 (0.,uv[0][i]));

	return (UU_SUCCESS);
}

/*********************************************************************
**    E_FUNCTION     : um_check_inside (vertex,np,point,box,tol)
**       Checks if point is inside/on/outside the closed contour
**       represented by polyline
**    PARAMETERS
**       INPUT  :
**          vertex - array of points representing closed curve, last
**                   point is same as the first point in array.
**          np     - number of points in the array.
**          point  - point in question.
**          box    - the 2D-box of the contour
**          tol
**       OUTPUT :
**          none
**    RETURNS      :  1 - point is inside,
**                    0 = point on curve within tolerance,
**                   -1 - point is outside
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_check_inside (vertex,np,point,box,tol)
UM_2Dcoord *vertex,point;
int np;
UM_2box *box;
UU_REAL tol;
{
	int i,rcross,lcross;
	UU_LOGICAL rstrad,lstrad;
	UU_REAL ax,bx,ay,by,d1,d2,cx,cy;
	UU_REAL tolsq;

/*
...easy solution:
...check if point is outside box
*/
	if (box && (point[0] - box->xmax > tol ||
		 box->xmin - point[0] > tol)) return (-1);
	if (box && (point[1] - box->ymax > tol ||
		 box->ymin - point[1] > tol)) return (-1);
/*
...point is in the box
*/
	rcross = lcross = 0;
	tolsq = tol*tol;

	ax = vertex[0][0] - point[0];
	ay = vertex[0][1] - point[1];
	if (ax*ax + ay*ay < tolsq) return (0);

	for (i = 1; i <= np; i++)
	{
		if (i < np)
		{
			bx = vertex[i][0] - point[0];
			by = vertex[i][1] - point[1];
		}
		else
		{
			bx = vertex[0][0] - point[0];
			by = vertex[0][1] - point[1];
		}
		if (bx*bx + by*by < tolsq) return (0);
/*
..... if point is on the edge return 0
*/
		d1 = ax*bx + ay*by; d2 = ax*by - ay*bx;
		if (d1 < 0.)
		{
			if (i < np)
			{
				cx = vertex[i][0] - vertex[i-1][0];
				cy = vertex[i][1] - vertex[i-1][1];
			}
			else
			{
				cx = vertex[0][0] - vertex[i-1][0];
				cy = vertex[0][1] - vertex[i-1][1];
			}
			d1 = cx*cx + cy*cy;
			if (d1 < tolsq) continue;
			if (d2*d2 < 4.*tolsq*d1) return (0);
		}
/*
..... if not on the edge decide if the edge crosses the ray going from
..... the point in positive x-direction
*/
		rstrad = (by > tol != ay > tol);
		lstrad = (by < -tol != ay < -tol);

		if (rstrad || lstrad)
		{
			d2 = d2*(by - ay);
			if (rstrad && d2 > 0. && (ax > 0. || bx > 0.)) rcross++;
			if (lstrad && d2 < 0. && (ax < 0. || bx < 0.)) lcross++;
		}
		ax = bx; ay = by;
	}
/*
..... odd number of crossings means "inside"; odd means "outside"
*/
	if (rcross == 0 || lcross == 0)
		return (-1);
	else if (rcross%2 != lcross%2)
		return (0);
	else if ((rcross%2) == 1)
		return (1);
	else
		return (-1);
}

/*********************************************************************
**    E_FUNCTION     : um_check_box_ins (boxa,boxb,tol)
**       Checks if one of two boxes is inside another
**    PARAMETERS
**       INPUT  :
**          boxa, boxb - the 2D-boxes
**          tol
**       OUTPUT :
**          none
**    RETURNS      :  1 - A is inside B,
**                   -1 - B is inside A
**                    0 - neither
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_check_box_ins (boxa,boxb,tol)
UM_2box *boxa,*boxb;
UU_REAL tol;
{
	UU_REAL dx0,dx1,dy0,dy1;

	dx0 = boxa->xmin - boxb->xmin;
	dx1 = boxa->xmax - boxb->xmax;
	dy0 = boxa->ymin - boxb->ymin;
	dy1 = boxa->ymax - boxb->ymax;

	if (dx0 > -tol && dx1 < tol && dy0 > -tol && dy1 < tol) return (1);
	if (dx1 > -tol && dx0 < tol && dy1 > -tol && dy0 < tol) return (-1);
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : um_check_box_ins (boxa,boxb,tol)
**       Checks if one of two boxes intersect
**    PARAMETERS
**       INPUT  :
**          boxa, boxb - the 2D-boxes
**          tol
**       OUTPUT :
**          none
**    RETURNS      :  1 - A intersects B,
**                    0 - else
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_isect_boxes0 (boxa,boxb,tol)
UM_2box *boxa,*boxb;
UU_REAL tol;
{
	UU_REAL *a = (UU_REAL *) boxa;
	UU_REAL *b = (UU_REAL *) boxb;

	return (um_isect_boxes (a,a+2,b,b+2,tol));
}

/*********************************************************************
**    E_FUNCTION     : um_sticky_contour (vertex,np,point,tolsq)
**       Move a point onto a closed contour if already within tolerance
**    PARAMETERS
**       INPUT  :
**          vertex - array of points representing closed curve
**          np     - number of points in the array
**          point  - point in question
**          tolsd  - squared tolerance
**       OUTPUT :
**          vertex - possibly changed, by no more than tol
**    RETURNS      :  1 - point changed
**                    0 - point did not change
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_sticky_contour (vertex,np,point,tolsq)
UM_2Dcoord *vertex,point;
int np;
UU_REAL tolsq;
{
	int i;
	UU_REAL ax,bx,ay,by,d1,d2,cx,cy;

	for (i = 1; i <= np; i++)
	{
		ax = point[0] - vertex[i-1][0];
		ay = point[1] - vertex[i-1][1];
		if (ax*ax + ay*ay < tolsq)
		{
			point[0] = vertex[i-1][0];
			point[1] = vertex[i-1][1];
			return (1);
		}
		if (i < np)
		{
			bx = vertex[i][0] - point[0];
			by = vertex[i][1] - point[1];
		}
		else
		{
			bx = vertex[0][0] - point[0];
			by = vertex[0][1] - point[1];
		}
		if (bx*bx + by*by < tolsq)
		{
			point[0] = vertex[i][0];
			point[1] = vertex[i][1];
			return (1);
		}
/*
..... if point is within the circle built on current edge
*/
		d1 = ax*bx + ay*by; d2 = ax*by - ay*bx;
		if (d1 > 0.)
		{
			if (i < np)
			{
				cx = vertex[i][0] - vertex[i-1][0];
				cy = vertex[i][1] - vertex[i-1][1];
			}
			else
			{
				cx = vertex[0][0] - vertex[i-1][0];
				cy = vertex[0][1] - vertex[i-1][1];
			}
			d1 = cx*cx + cy*cy;
/*
..... (d2*d2)/d1 is the squared distance from point to current edge.
..... if less than tolerance, replace the point with its projection onto edge
*/
			if (d1 > tolsq && d2*d2 < tolsq*d1)
			{
				d2 = ax*cx + ay*cy;
				if (d2 > 0 && d2 < d1)
				{
					d2 = d2/d1;
					point[0] = vertex[i-1][0] + d2*cx;
					point[1] = vertex[i-1][1] + d2*cy;
					return (1);
				}
			}
		}
	}

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : um_point_close (vertex,np,point,tolsq)
**       Determine if a point is within tolerance to a closed contour.
**    PARAMETERS
**       INPUT  :
**          vertex - array of points representing closed curve
**          np     - number of points in the array
**          point  - point in question
**          tolsd  - squared tolerance
**       OUTPUT : none
**    RETURNS      :  UU_TRUE/UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_point_close (vertex,np,point,tolsq)
UM_2Dcoord *vertex,point;
int np;
UU_REAL tolsq;
{
	int i;
	UU_REAL ax,bx,ay,by,d1,d2,cx,cy;

	for (i = 1; i <= np; i++)
	{
		ax = point[0] - vertex[i-1][0];
		ay = point[1] - vertex[i-1][1];
		if (ax*ax + ay*ay < tolsq)
		{
			return (UU_TRUE);
		}
		if (i < np)
		{
			bx = vertex[i][0] - point[0];
			by = vertex[i][1] - point[1];
		}
		else
		{
			bx = vertex[0][0] - point[0];
			by = vertex[0][1] - point[1];
		}
		if (bx*bx + by*by < tolsq)
		{
			return (UU_TRUE);
		}
/*
..... if point is within the circle built on current edge
*/
		d1 = ax*bx + ay*by; d2 = ax*by - ay*bx;
		if (d1 > 0.)
		{
			if (i < np)
			{
				cx = vertex[i][0] - vertex[i-1][0];
				cy = vertex[i][1] - vertex[i-1][1];
			}
			else
			{
				cx = vertex[0][0] - vertex[i-1][0];
				cy = vertex[0][1] - vertex[i-1][1];
			}
			d1 = cx*cx + cy*cy;
/*
..... (d2*d2)/d1 is the squared distance from point to current edge.
..... if less than tolerance, replace the point with its projection onto edge
*/
			if (d1 > tolsq && d2*d2 < tolsq*d1)
			{
				d2 = ax*cx + ay*cy;
				if (d2 > 0 && d2 < d1)
				{
					return (UU_TRUE);
				}
			}
		}
	}

	return (UU_FALSE);
}

/*********************************************************************
** FUNCTION : UU_REAL um_sqdis_from_segment_2d (p,pt1,pt2)
**        returns minmum squared distance from a point to a line 
** PARAMETERS:
**    INPUT :
**       p  - point 
**       pt1 - segment start point
**       pt2 - segment end point
**    OUTPUT : none
** RETURNS:  
**        squared distance
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
UU_REAL um_sqdis_from_segment_2d (p,pt1,pt2,lSegment)
UM_2Dcoord p,pt1,pt2;
UU_LOGICAL lSegment;
{
	UU_REAL c1,c2,b;
	UM_2Dcoord v,w,pb; 
 
	um_vcmnvc_2d(pt2,pt1,v);
	um_vcmnvc_2d(p, pt1, w);

	c1 = UM_DOT_2D(w,v);
	if (lSegment && c1 <= 0.0)
		return UM_SQDIS_2D(p,pt1);

	c2 = UM_DOT_2D(v,v);
	if (lSegment && c2 <= c1)
		return UM_SQDIS_2D(p,pt2);

	b = c1/ c2;
	um_vcplbvc_2d(pt1, b, v, pb);

	return UM_SQDIS_2D(p,pb);
}

/*********************************************************************
**    E_FUNCTION     : um_nptln_2d(pt,pt1,pt2,npt)
**			Calculate the  coordinates of the nearest point (NPT) to the
**			given point (PT) which lies on the line.
**    PARAMETERS   
**       INPUT  : 
**			pt		-coordinates of given point
**			pt1		- segment start point
**			pt2		- segment end point
**       OUTPUT :  
**			npt     - coordinates of nearest point on line
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_nptln_2d(pt,pt1,pt2,npt)
UM_2Dcoord pt,pt1,pt2;
UM_2Dcoord npt;
{
	UU_REAL c1,c2,b;
	UM_2Dcoord v,w,pb; 
 
	um_vcmnvc_2d(pt2,pt1,v);
	um_vcmnvc_2d(pt, pt1, w);
	c1 = UM_DOT_2D(w,v);
	c2 = UM_DOT_2D(v,v);
	b = c1/ c2;
	um_vcplbvc_2d(pt1, b, v, npt);

	return(0);
}

/*********************************************************************
** FUNCTION : UU_LOGICAL um_point_in_segment (p0, p1, p2)
**            finds if point p0 is inside the segment of line(p1,p2)
** PARAMETERS:
**    INPUT :
**       p0		  - The point to check
**		 p1, p2   - The segment start and end point
**		 tol	  - The tolerance to check
**    OUTPUT : 
**       none
** RETURNS: 
**           UU_TRUE iff the point is inside the segment; 
**           UU_FALSE otherwise	
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
UU_LOGICAL um_point_in_segment_2d (p0,p1,p2,tol)
UM_2Dcoord p0,p1,p2;
UU_REAL tol;
{
	UM_vector v1;
	UU_REAL d1,d2,d,dsq;

	dsq = um_sqdis_from_segment_2d (p0,p1,p2,UU_TRUE);
	if (dsq < 0.01*tol*tol)
		return UU_TRUE;

	d = UM_SQDIS_2D(p1,p2);
	d1 = UM_SQDIS_2D(p0,p1);
	d2 = UM_SQDIS_2D(p0,p2);
	if (d < d1 || d < d2)
		return UU_FALSE;

	return UU_TRUE;
}
