/*********************************************************************
**    NAME         :  m7mathvc.c
**       CONTAINS: routines to manipulate vectors
**
**			um_cparray
**			um_cotovc
**			um_vctovc
**			um_vcplvc
**			um_middlept
**			um_avcplbvc
**			um_translate_point
**			um_vcmnvc
**			um_negvc
**			um_vctmsc
**			UU_LOGICAL um_vcperp
**			UU_LOGICAL um_vcparall
**			um_perpvc
**			um_2perpvc
**			um_cross
**			UU_REAL um_mag
**			UU_REAL um_dot
**			um_unitvc
**			um_nullvc
**			UU_REAL um_angle
**			UU_REAL um_angle2p
**			um_vctmtf
**			um_xyztovc
**			UU_REAL um_angle2p_acc
**			UU_REAL um_vcdir
**			um_uvctopln
**			um_vctopln
**			um_3pt_colinear
**			um_3pt_pln
**			um_vcortho
**			um_tanvc
**			um_swapvc
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m7mathvc.c , 25.1
**     DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:10
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mdcoord.h"
#include "modef.h"
#include "mfort.h"

/*********************************************************************
**    E_FUNCTION     : um_cparray(m,from,to)
**       Copy m values from one array to another.
**    PARAMETERS
**       INPUT  :
**          m                    number of values to copy
**          from                 array to copy from
**       OUTPUT :
**          to                   array to copy to
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_cparray(m,from,to)
   int m;
   UU_REAL from[];
   UU_REAL to[];

   {
   int i;

   uu_denter(UU_MTRC,(us,"um_cparray(%d,%8x,%8x)",m,from,to));
   for (i=0; i<m; i++) to[i] = from[i];
   uu_dexit;
   return (0);
   }

/*********************************************************************
**    I_FUNCTION     : um_cotovc(co, type, vc)
**      Convert a coordinate represented in either CARTESIAN,
**			CYLINDRICAL, or SPHERICAL coordinates into an internal
**			CARTESIAN  coordinate system.
**    PARAMETERS
**       INPUT  :
**				co                    a coordinate
**      	   type                  coordinate type
**       OUTPUT :
**				vc                    internal  CARTESIAN  vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_cotovc(co, type, vc)
	UM_coord co;
	int  type;
	UM_vector vc;

   {
   int i;

   switch (type)
      {
		case  UM_VCARTESIAN:
      case  UM_CARTESIAN:
         for (i = 0; i < 3; i++) vc[i]  =  co[i];
         break;
      case  UM_VCYLINDRICAL:
      case  UM_CYLINDRICAL:
         vc[0] = co[0]  * cos(co[1]);
         vc[1] = co[0]  * sin(co[1]);
         vc[2] = co[2];
         break;
      case  UM_VSPHERICAL:
      case  UM_SPHERICAL:
         vc[0] = co[0] * cos(co[1])  * sin(co[2]);
         vc[1] = co[0] * sin(co[1])  * sin(co[2]);
         vc[2] = co[0] * cos(co[2]);
         break;
      default:;
         break;
      }
   return (0);
   }

/*********************************************************************
**    E_FUNCTION     : um_vctovc(vci,vco)
**      Copy one vector to another.
**    PARAMETERS
**       INPUT  :
**				vci        vector in
**       OUTPUT :
**				vco        vector out
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_vctovc(vci,vco)
	UM_vector vci;
	UM_vector vco;

	{
	int i;				/* index */

	for (i = 0; i < 3; i++) vco[i]  =  vci[i];
	return (0);
	}

/*********************************************************************
**    FUNCTION     : int um_vcplvc(v1,v2,vr)
**      Add two vectors and return the result.
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
int um_vcplvc(v1,v2,vr)
UM_vector v1,v2,vr;
{
	int i;

	for (i = 0; i < 3; i++) vr[i]  =  v1[i] + v2[i];
	return (0);
}

/*********************************************************************
**    FUNCTION : um_middlept(UM_coord p1,UM_coord p2,UM_coord middle)
**
**      Get middle point on the line through 2 given points
**    PARAMETERS
**       INPUT  :
**          p1 -   first point
**          p2 -   second point
**       OUTPUT :
**          p -   middle point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_middlept(p1,p2,middle)
UM_coord p1, p2, middle;
{
	int i;

	for (i = 0; i < 3; i++) middle[i]  =  0.5*(p1[i] + p2[i]);
	return (0);
}

/*********************************************************************
**    FUNCTION     : int um_avcplbvc (a,v1,b,v2,v)
**      v = a*v1 +b*v2
**    PARAMETERS
**       INPUT  :
**          a             1st factor
**				v1            first vector
**          b             2nd factor
**          v2            second vector
**       OUTPUT :
**				v            resultant vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_avcplbvc (a,v1,b,v2,v)
UU_REAL a,b;
UM_vector v1,v2,v;
{
	int i;

	for (i = 0; i < 3; i++) v[i]  =  a*v1[i] + b*v2[i];
	return (0);
}

/*********************************************************************
** FUNCTION : int um_translate_point (p, t, v, pv)
**              translates point by a vector times scalar
** PARAMETERS:
**    INPUT :
**       p -  point to be translated
**       t - scalar
**       v - vector
**    OUTPUT :
**       pv - point: pv = p + t*v
** RETURNS:  none
** SIDE EFFECTS: none
** WARNINGS    : none
*********************************************************************/
int um_translate_point (p,t,v,pv)
UM_coord p,pv;
UU_REAL t;
UM_vector v;
{
   int i;
   for (i=0; i<3; i++) pv[i] = p[i] + t*v[i];
   return (0);
}

/*********************************************************************
**    E_FUNCTION     : um_vcmnvc(v1,v2,vr)
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
um_vcmnvc(v1,v2,vr)
UM_vector v1;
UM_vector v2;
UM_vector vr;
{
	int i;

	for (i = 0; i < 3; i++) vr[i]  =  v1[i] - v2[i];
	return (0);
}

/*********************************************************************
**    FUNCTION     : um_negvc(vci,vco)
**       Multiply a vector by -1.
**    PARAMETERS
**       INPUT  :
**				vci        vector in
**       OUTPUT :
**				vco        vector out
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_negvc(vci,vco)
UM_vector vci;
UM_vector vco;
{
	vco[0]  =  -vci[0]; vco[1]  =  -vci[1]; vco[2]  =  -vci[2];
	return (0);
}

/*********************************************************************
**    FUNCTION     : um_vctmsc(vci,sca,vco)
**       Multiply a vector times a scalar.
**    PARAMETERS
**       INPUT  :
**				vci        vector in
**          sca        scalar
**       OUTPUT :
**				vco        vector out
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_vctmsc(vci,sca,vco)
UM_vector vci;
UU_REAL sca;
UM_vector vco;
{
	int i;
	for (i = 0; i < 3; i++) vco[i]  =  sca * vci[i];
	return (0);
}

/*********************************************************************
**    FUNCTION     : UU_LOGICAL um_vcperp(v1,v2)
**      Determine if two unit vectors are perpendicular.
**    PARAMETERS
**       INPUT  :
**				v1            first vector
**          v2            second vector
**       OUTPUT :
**				none
**    RETURNS      : none
**			UU_TRUE iff vectors are perpendicular; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_vcperp(v1,v2)
UM_vector v1;
UM_vector v2;
{
	UU_REAL proj;			/* v1 projected on v2 */

	proj = UM_DOT(v1, v2);
	if (fabs(proj) < UM_DFUZZ) return (UU_TRUE); else return (UU_FALSE);
}

/*********************************************************************
**    FUNCTION     : UU_LOGICAL um_vcperp_tol(v1,v2,tol)
**      Determine if two unit vectors are perpendicular.
**    PARAMETERS
**       INPUT  :
**				v1            first vector
**          v2            second vector
**          tol           tolerance
**       OUTPUT :
**				none
**    RETURNS      : none
**			UU_TRUE iff vectors are perpendicular; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_vcperp_tol(v1,v2,tol)
UM_vector v1;
UM_vector v2;
UU_REAL tol;
{
	UU_REAL proj;			/* v1 projected on v2 */

	proj = UM_DOT(v1, v2);
	if (fabs(proj) < tol) return (UU_TRUE); else return (UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL um_vcparall(v1,v2)
**      Determine if two unit vectors are parallel.
**    PARAMETERS
**       INPUT  :
**          none
**      INPUT:  v1            first vector
**              v2            second vector
**       OUTPUT :
**          none
**    RETURNS      : none
**			UU_TRUE iff vectors are parallel; else false UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_vcparall(v1,v2)
UM_vector v1;
UM_vector v2;
{
	UU_REAL proj;			/* v1 projected on v2 */

	proj = UM_DOT(v1, v2);
	if ( (1.0 - fabs(proj)) < 0.01*UM_DFUZZ) return (UU_TRUE);
	else return (UU_FALSE);
}

/*********************************************************************
**    E_FUNCTION     : um_perpvc(vi,vr)
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
um_perpvc(vi,vr)
UM_vector vi;
UM_vector vr;
{
	if (fabs(vi[0])  <  UM_FUZZ)
		{
		vr[0] = 0.0;
		vr[1] =  - vi[2];
		vr[2] =  vi[1];
		}
	else
		{
		vr[0] =  - vi[1];
		vr[1] =  vi[0];
		vr[2] = 0.0;
		}
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : um_cross(v1,v2,vr)
**      Calculate the cross product of two vectors.
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
um_cross(v1,v2,vr)
UM_vector v1;
UM_vector v2;
UM_vector vr;
{
	int i;
	UM_vector rv;

	rv[0] = (v1[1] *v2[2]) - (v1[2] *v2[1]);
	rv[1] = (v1[2] *v2[0]) - (v1[0] *v2[2]);
	rv[2] = (v1[0] *v2[1]) - (v1[1] *v2[0]);
	for (i = 0; i < 3; i++) vr[i]  =  rv[i];
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : um_2perpvc(vz,vx,vy)
**      Determine two unit vectors perpendicular to a given unit vector.
**    PARAMETERS
**       INPUT  :
**				vz            given vector
**       OUTPUT :
**				vx, vy        perpendicular vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_2perpvc(vz,vx,vy)
UM_vector vz,vx,vy;
{
	um_nullvc (vx);

	if (vz[1]*vz[1] < 0.33334)
	{
		vx[0] = vz[2];
		vx[2] = -vz[0];
	}
	else if (vz[2]*vz[2] < 0.33334)
	{
		vx[0] = -vz[1];
		vx[1] = vz[0];
	}
	else
	{
		vx[1] = vz[2];
		vx[2] = -vz[1];
	}

	um_unitvc (vx,vx);
	um_cross (vz,vx,vy);
	um_unitvc (vy,vy);
}

/*********************************************************************
**    E_FUNCTION     : UU_REAL um_mag(v1)
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
UU_REAL um_mag(v1)
UM_vector v1;
{
	UU_REAL vmag;

	vmag = v1[0]*v1[0] + v1[1]*v1[1] + v1[2]*v1[2];
	if (vmag > 0)
		vmag = sqrt (vmag);
	else
		vmag = 0;

	return (vmag);
}

/*********************************************************************
**    E_FUNCTION     : UU_REAL um_dot(v1,v2)
**      Calculate the dot product of two vectors.
**    PARAMETERS
**       INPUT  :
**				v1          first vector
**          v2          second vector
**       OUTPUT :
**    RETURNS      :
**				dot			dot product of vectors
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_dot(v1,v2)
UM_vector v1;
UM_vector v2;
{
	return (UM_DOT(v1,v2));
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
UU_REAL um_sqdis(p1,p2)
UM_coord p1,p2;
{
	UU_REAL d;
	UM_vector vc;

	um_vcmnvc(p1,p2,vc);
	d = UM_DOT(vc,vc);
	return (d);
}

/*********************************************************************
**    E_FUNCTION     : um_unitvc(v1,vr)
**      Calculate a unit vector. If the given vector is "equal"
**      to the zero vector, return (0.0, 0.0, 0.0).
**    PARAMETERS
**       INPUT  :
**				v1            vector
**       OUTPUT :
**				vr            resultant vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_unitvc(v1,vr)
UM_vector v1;
UM_vector vr;
{
	int i;						/* index */
	UU_REAL vx,vy,vz,vmag;			/* magnitude */

/*
.....UM_MAG will sometimes return a bad value
.....on Windows NT when doing linear dimensioning
.....(on second dim of same entity QAR 92304)
.....and could not find the reason why.
.....So changed it to long hand which does not
.....calculate a bad value.
.....Bobby  -  6/3/02
*/
        vx = v1[0] * v1[0];
        vy = v1[1] * v1[1];
        vz = v1[2] * v1[2];
        vmag = vx + vy + vz;
	vmag = sqrt(vmag);
/*	vmag = UM_MAG(v1);*/
	if (vmag < UM_DFUZZ)
		{
		for (i=0; i<3; i++) vr[i] = v1[i];
		}
	else
		for (i=0; i<3; i++) vr[i] = v1[i]/vmag;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : um_nullvc(v)
**       return v = (0.0, 0.0, 0.0).
**    PARAMETERS
**       INPUT  :
**				v            vector
**       OUTPUT :
**				v            resultant zero vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_nullvc(v)
UM_vector v;
{
	int i;

	for (i=0; i<3; i++) v[i] = 0.;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : UU_REAL um_angle(vc1,vc2)
**      Calculate the angle between two vectors.
**    PARAMETERS
**       INPUT  :
**				vc1        first vector
**				vc2        second vector
**       OUTPUT :
**    RETURNS      :
**				ang		  angle between vectors
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL um_angle(vc1,vc2)
UM_vector vc1;
UM_vector vc2;
{
	UM_vector uvc1;		/* unit vector along first vector */
	UM_vector uvc2;		/* unit vector along second vector */
	UM_angle ang;

	um_unitvc(vc1, uvc1);
	um_unitvc(vc2, uvc2);
	ang = UM_DOT (uvc1, uvc2);
	if (ang < -1.0) ang = -1.0; else if (ang > 1.0) ang = 1.0;
	ang = acos(ang);
	return (ang);
}

/*********************************************************************
**    E_FUNCTION     : UU_REAL um_angle2p(vect1, vect2, nvect)
**      Finds the angle between vect1 and vect2.  Angle will be
**      between 0 and 2PI relative to nvect
**  PARAMETERS
**      INPUT  :
**				vect1				vector
**				vect2				vector
**				nvect				vector normal to vect1 and vect2 that angle will
**									be relative to
**      OUTPUT :
**				none
**  RETURNS      :
**				angle				angle between vectors
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
UU_REAL um_angle2p(vect1, vect2, nvect)
UM_vector vect1;				/* first vector */
UM_vector vect2;				/* second vector */
UM_vector nvect;				/* vector normal to vect1 and vect2 that angle
											will be relative to */
{
	UU_REAL temp;					/* temporary storage */
	UM_vector tempvec;			/* temporary storage */
	UM_angle angle;					/* angle between vect1 and vect2 */

	angle = um_angle(vect1, vect2);
	um_cross(vect1, vect2, tempvec);
	temp = UM_DOT(tempvec, nvect);
/*
.....Changed UM_FUZZ to UM_DFUZZ
.....Because circles with radii less than .01
.....were not being cut correctly
.....Vadim  -  1/31/95
*/
	if (fabs(temp) > UM_DFUZZ)
	  {
		if (temp < 0.0)
			angle = UM_TWOPI - angle;
	  }
	return (angle);
}

/*********************************************************************
**    E_FUNCTION     :	um_vctmtf( vci, tf, vco )
**       Multiply a cartesian vector by a transformation matrix
**			and return the result vco = vci * tf. The output vector
**			may be identical to the input.  This operation is identical to
**			um_cctmtf() except that the displacement of the transformation
**			is ignored.
**    PARAMETERS
**       INPUT  :
**				vci		vector
**				tf			transformation
**       OUTPUT :
**          vco		vector * transformation
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_vctmtf( vci, tf, vco )
UM_vector		vci;
UM_transf		tf;
UM_vector		vco;
{
	int i,j;
	UM_vector temp;

	if (tf == UM_DEFAULT_TF)
	{
		for (i=0; i<3; i++) vco[i] = vci[i];
	}
	else
	{
		for (i=0; i<3; i++) temp[i] = 0.0;

		for (i=0; i<3; i++)
		{
			for (j=0; j<3; j++) temp[i] = temp[i] + vci[j] * tf[j][i];
		}

		for (i=0; i<3; i++) vco[i] = temp[i];
	}
}

/*********************************************************************
**    E_FUNCTION     :	um_xyztovc(x, y, z, vco)
**       Assign the X, Y , and Z values to the vector VCO.
**    PARAMETERS
**       INPUT  :
**          x							x component
**          y							y component
**          z							z component
**       OUTPUT :
**          vco						vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_xyztovc(x, y, z, vco)
UU_REAL x;
UU_REAL y;
UU_REAL z;
UM_vector vco;
{
	vco[0] = x; vco[1] = y; vco[2] = z;
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : UU_REAL um_angle2p_acc(vect1, vect2, nvect)
**      Finds the angle between vect1 and vect2.  Angle will be
**      between 0 and 2PI relative to nvect
**  PARAMETERS
**      INPUT  :
**				vect1				vector
**				vect2				vector
**				nvect				vector normal to vect1 and vect2 that angle will
**									be relative to
**      OUTPUT :
**				none
**  RETURNS      :
**				angle				angle between vectors
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
UU_REAL um_angle2p_acc(vect1, vect2, nvect)
UM_vector vect1;				/* first vector */
UM_vector vect2;				/* second vector */
UM_vector nvect;				/* vector normal to vect1 and vect2 that angle
											will be relative to */
{
	UU_REAL temp;					/* temporary storage */
	UM_vector tempvec, vc1, vc2;	/* temporary storage */
	UM_angle angle;					/* angle between vect1 and vect2 */

   um_unitvc (vect1,vc1);
   um_unitvc (vect2,vc2);
/*
...get direct angle for very small angles
*/
   temp = UM_DOT (vc1,vc2);
   if (temp > .9999)
      angle = UM_DCCCC (vc1,vc2);
   else
      {
       if (temp <-.9999)
         {
          um_vcplvc (vc1,vc2,tempvec);
          angle = UM_PI - um_mag (tempvec);
         }
       else
          angle = acos (temp);
      }
	um_cross(vect1, vect2, tempvec);
	temp = UM_DOT(tempvec, nvect);
/*
...set angle direction
*/
   if (temp < 0.0) angle = UM_TWOPI - angle;
	return (angle);
}

/*********************************************************************
**    E_FUNCTION     : um_vcdir(pt1, pt0, vdir)
**      Defines position of the point on the virtual axis specified by
**      zero point pt0 and direction vector vdir (unit).
**  PARAMETERS
**      INPUT  :
**				pt1				input point on axis
**				pt0				zero point on axis
**				vdir				vector defining axis orientation

**      OUTPUT :
**				none
**  RETURNS      :
**				distance from zero point to input point with vdir orientation.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
UU_REAL um_vcdir (pt1,pt0,vdir)
UM_coord pt1, pt0;
UM_vector vdir;
{
   UU_REAL r;
   UM_vector pvc;

   um_vcmnvc (pt1,pt0,pvc);
   r = um_mag(pvc);
   if (UM_DOT(pvc,vdir) < 0.) r = -r;
   return (r);
}

/*********************************************************************
**    E_FUNCTION     : um_triple_cross(v1,v2,v3,vr)
**      Calculate the triple cross product of three vectors.
**    PARAMETERS
**       INPUT  :
**			v1            first vector
**			v2            second vector
**			v3            third vector
**       OUTPUT :
**				vr            resultant vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_triple_cross(v1,v2,v3,vr)
UM_vector v1,v2,v3,vr;
{
	UU_REAL a, b;

	a = UM_DOT(v1,v3);
	b = UM_DOT(v2,v3);

	um_avcplbvc (a,v2,-b,v1,vr);
	return (0);
}

/*********************************************************************
**    E_FUNCTION     : um_vctopln (v1,vn,v2)
**      Project a vector onto a plane.
**    PARAMETERS
**       INPUT  :
**			v1            vector
**			vn            unit vector defining the plane
**       OUTPUT :
**			v2            projected vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_vctopln (v1,vn,v2)
UM_vector v1,vn,v2;
{
	UU_REAL a;

	a = UM_DOT (v1,vn);
	um_translate_point (v1,-a,vn,v2);

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : um_uvctopln (v1,vn,v2)
**      Calculate the unit vector projection onto a plane.
**    PARAMETERS
**       INPUT  :
**			v1            unit vector
**			vn            unit vector defining the plane
**       OUTPUT :
**			v2            resultant unit vector
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_uvctopln (v1,vn,v2)
UM_vector v1,vn,v2;
{
	um_vctopln (v1,vn,v2);
	um_unitvc (v2,v2);

	return (0);
}

/*********************************************************************
**    E_FUNCTION     : um_3pt_colinear(pt1,pt2,pt3,nvec)
**      Determine whether the three points are on the same line;
**      if not, return a vector normal to the plane they define
**    PARAMETERS
**       INPUT  :
**          pt1            first point
**          pt2            second point
**          pt3            third point
**          tol            tolerance
**       OUTPUT :
**          nvec           if not collinear, a unit normal to the plane
**                         through the three points.
**                         if collinear, a unit vector along the line
**    RETURNS      :
**			1 		collinear
**			0		not collinear
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_3pt_colinear(pt1,pt2,pt3,nvec,tol)
UM_coord pt1,pt2,pt3,nvec;
UM_real8 tol;
{
	UM_vector a,b;
	UU_REAL da;

	um_vcmnvc(pt3,pt1,a);
	um_vcmnvc(pt2,pt1,b);
	da = um_mag (a);

	if (da < tol)
	{
		um_unitvc (b,nvec);
		return (1);
	}

	a[0] /= da; a[1] /= da; a[2] /= da;
	um_cross(a,b,nvec);

	if (um_mag (nvec) < tol)
	{
		um_vctovc (a,nvec);
		return (1);
	}
	else
	{
		um_unitvc (nvec,nvec);
		return (0);
	}
}

/*********************************************************************
**    E_FUNCTION     : um_3pt_pln (a,b,c,nvec,dpl)
**      Return a vector normal to the 3-point plane, or zero vector
**    PARAMETERS
**       INPUT  :
**          a            first point
**          b            second point
**          c            third point
**       OUTPUT :
**          nvec           a unit normal to the plane through the points.
**                         zero vector if plane is not defined
**          dpl            plane distance
**    RETURNS      : -1 iff a plane is not defined
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_3pt_pln (a,b,c,nvec,dpl)
UM_coord a,b,c;
UM_vector nvec;
UU_REAL *dpl;
{
	int k;
	UU_REAL d;

	nvec[0] = (b[1] - a[1]) * (c[2] - a[2]) -
		(b[2] - a[2]) * (c[1] - a[1]);
	nvec[1] = (b[2] - a[2]) * (c[0] - a[0]) -
		(b[0] - a[0]) * (c[2] - a[2]);
	nvec[2] = (b[0] - a[0]) * (c[1] - a[1]) -
		(b[1] - a[1]) * (c[0] - a[0]);

	d = UM_DOT (nvec,nvec);
	if (d < 1.e-13)
	{
		for (k = 0; k < 3; k++) nvec[k] = 0;
		*dpl = 0;
		return (-1);
	}
	else
	{
		d = sqrt(d);
		for (k = 0; k < 3; k++) nvec[k] = nvec[k]/d;

		*dpl = (UM_DOT(nvec,a) + UM_DOT(nvec,b) + UM_DOT(nvec,c))/3;

		return (0);
	}
}

/*********************************************************************
**    E_FUNCTION     : um_vcortho(vc1,vc2)
**      Adjusts the second input vector so that it is orthogonal to
**      the first input vector.
**    PARAMETERS
**       INPUT  :
**          vc1            Controlling vector.
**          vc2            Vector to adjust to be orthogonal.
**       OUTPUT :
**          vc2            Orthogonal vector.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_vcortho(vc1,vc2)
UM_vector vc1,vc2;
{
	UM_vector tmpvc,yaxis;
/*
.....Create orthogonal vector
*/
	um_unitvc(vc1,tmpvc);
	um_unitvc(vc2,vc2);
	um_cross(tmpvc,vc2,yaxis);
	um_unitvc(yaxis,yaxis);
	um_cross(yaxis,tmpvc,vc2);
	um_unitvc(vc2,vc2);
}

/*********************************************************************
**    E_FUNCTION     : um_tanvc(pt1,pt2,pt3,tvec)
**      Calculates a tangent vector to three points.  The tangent
**      vector will be the bisector of a vector between the first and
**      second point and a vector between the second and third point.
**    PARAMETERS
**       INPUT  :
**          pt1            First point.
**          pt2            Second point.
**          pt3            Third point.
**       OUTPUT :
**          tvec           Tangent vector.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_tanvc(pt1,pt2,pt3,tvec)
UM_coord pt1,pt2,pt3;
UM_vector tvec;
{
	UM_vector vc1,vc2;
/*
.....Calculate directional vectors
*/
	um_vcmnvc(pt2,pt1,vc1);
	um_vcmnvc(pt3,pt2,vc2);
	um_unitvc(vc1,vc1);
	um_unitvc(vc2,vc2);
/*
.....Calculate tangent vector
*/
	um_vcplvc(vc1,vc2,tvec);
	um_unitvc(tvec,tvec);
}

/*********************************************************************
**    E_FUNCTION     : um_swapvc (vc1,vc2)
**      Swap two vectors.
*********************************************************************/
void um_swapvc (vc1,vc2)
UM_vector vc1,vc2;
{
	UM_vector tvec;

	um_vctovc (vc1,tvec);
	um_vctovc (vc2,vc1);
	um_vctovc (tvec,vc2);
}
