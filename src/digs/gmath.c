/**********************************************************************
**    NAME        :  gmath.c
**       CONTAINS:
**		ug_vctovc --		copy vector to vector
**		ug_nptpln --		nearest point on plane (project)
**		ug_dot    --		vector dot product
**		ug_vcmnvc --		vector subtraction
**		ug_vctmsc --		vector times scalar
**		ug_vcplvc --		vector addition
**		ug_unitvc --		unitize vector
**		ug_mag    --		vector magnitude
**		ug_angle2p --		angle btwn vectors relative to normal
**		ug_angle  --		angle btwn vectors
**		ug_cross  --		vector cross product
**		ug_acos   --		acos with undefined checking
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       gmath.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:21
*********************************************************************/
#include "umath.h"
#include "usysdef.h"
#include "zsysdep.h"
#include "go.h"
#include "udebug.h"

#define UG_FUZZ (UU_REAL) 1.0e-10

/*************** ug_vctovc ******************************************/
ug_vctovc(vci,vco)				/* assign vector to vector */
	Gfloat vci[3];
	Gfloat vco[3];

	{
	int i;				/* index */

	uu_denter(UU_GITRC,(us,"ug_vctovc(<%g,%g,%g>,vco)",
		vci[0],vci[1],vci[2]));
	for (i = 0; i < 3; i++) vco[i]  =  vci[i];
	uu_dexit;
	}
/*************** ug_nptpln*******************************************/
ug_nptpln(pt,ppt,unvc,npt)		/* nearest point on plane */
	Gfloat pt[3];
	Gfloat ppt[3];
	Gfloat unvc[3];
	Gfloat npt[3];

	{
	Gfloat proj;				/* projection along normal */
	Gfloat vc[3];				/* temporary vector */
	Gfloat ug_dot();

	uu_denter(UU_GITRC,(us,"ug_nptpln(pt=<%g,%g,%g>,)",pt[0],pt[1],pt[2]));
	uu_dprint(UU_GITRC,(us,"ug_nptpln(ppt=<%g,%g,%g>,)",ppt[0],ppt[1],ppt[2]));
	uu_dprint(UU_GITRC,(us,"ug_nptpln(unvc=<%g,%g,%g>,)",unvc[0],unvc[1],unvc[2]));
	ug_vcmnvc(pt, ppt, vc);
	proj = ug_dot(vc, unvc);
	ug_vctmsc(unvc,  - proj, vc);
	ug_vcplvc(pt, vc, npt);
	uu_dprint(UU_GITRC,(us,"ug_nptpln ret npt=<%g,%g,%g>,)",npt[0],npt[1],npt[2]));
	uu_dexit;
	}
/**************** ug_dot ********************************************/
Gfloat ug_dot(v1,v2)			/* dot product */
	Gfloat v1[3];
	Gfloat v2[3];

	{
	int i;				/* index */
	Gfloat  rdot;

	rdot = 0;
	for (i = 0; i < 3; i++)  rdot = rdot + (v1[i] *v2[i]);
	return (rdot);
	}
/*************** ug_vcmnvc ******************************************/
ug_vcmnvc(v1,v2,vr)			/* vector subtraction */
	Gfloat v1[3];
	Gfloat v2[3];
	Gfloat vr[3];

	{
	int i;				/* index */

	for (i = 0; i < 3; i++) vr[i]  =  v1[i] - v2[i];
	}
/*************** ug_vctmsc ******************************************/
ug_vctmsc(vci,sca,vco)			/* vector time scalar */
	Gfloat vci[3];
	Gfloat sca;
	Gfloat vco[3];
	{
	int i;				/* index */
	for (i = 0; i < 3; i++) 
		vco[i]  =  sca * vci[i];
	}
/************* ug_vcplvc ********************************************/
ug_vcplvc(v1,v2,vr)				/* vector addition */
	Gfloat v1[3];
	Gfloat v2[3];
	Gfloat vr[3];

	{
	int i;				/* index */

	for (i = 0; i < 3; i++) vr[i]  =  v1[i] + v2[i];
	}
/************** ug_unitvc ********************************************/
ug_unitvc(v1,vr)
	Gfloat v1[3];
	Gfloat vr[3];

	{
	int i;						/* index */
	Gfloat vmag;			/* magnitude */
	Gfloat ug_mag();

	vmag = ug_mag(v1);
	if (vmag < UG_FUZZ)
		{
		for (i=0; i<3; i++) vr[i] = 0.0;
		}
	else
		for (i=0; i<3; i++) vr[i] = v1[i]/vmag;
	}
/************** ug_mag **********************************************/
Gfloat ug_mag(v1)
	Gfloat v1[3];
	{
	int i;				/* index */
	Gfloat  rmag;
	rmag =0;
	for (i = 0; i < 3; i++)  rmag = rmag + (v1[i] *v1[i]);
	rmag = sqrt(rmag);
	return (rmag);
	}
/************  ug_angle2p ************************************************/
Gfloat ug_angle2p(vect1, vect2, nvect)
	Gfloat vect1[3];				/* first vector */
	Gfloat vect2[3];				/* second vector */
	Gfloat nvect[3];				/* vector normal to vect1 and vect2 that angle
											will be relative to */
{
	Gfloat temp;					/* temporary storage */
	Gfloat tempvec[3];			/* temporary storage */
	Gfloat angle;				/* angle between vect1 and vect2 */
	Gfloat ug_angle();
	Gfloat ug_dot();

	uu_denter(UU_GITRC,(us,"ug_angle2p(vect1=<%g,%g,%g>,)",
		vect1[0],vect1[1],vect1[2]));
	uu_dprint(UU_GITRC,(us,"ug_angle2p(vect2=<%g,%g,%g>,)",
		vect2[0],vect2[1],vect2[2]));
	uu_dprint(UU_GITRC,(us,"ug_angle2p(nvect=<%g,%g,%g>,)",
		nvect[0],nvect[1],nvect[2]));
	angle = ug_angle(vect1, vect2);
	ug_cross(vect1, vect2, tempvec);
	temp = ug_dot(tempvec, nvect);

	if (temp < 0.0)
		angle =  - angle;
	uu_dprint(UU_GITRC,(us,"ug_angle2p ret angle=%g",angle));
	uu_dexit;
	return (angle);
}
/*********** ug_angle ***********************************************/
Gfloat ug_angle(vc1,vc2)
	Gfloat vc1[3];
	Gfloat vc2[3];

	{
	Gfloat uvc1[3];		/* unit vector along first vector */
	Gfloat uvc2[3];		/* unit vector along second vector */
	Gfloat ang;
	Gfloat ug_acos();
	Gfloat ug_dot();

	uu_denter(UU_GITRC,(us,"ug_angle(vc1=<%g,%g,%g>,)",
		vc1[0],vc1[1],vc1[2]));
	uu_dprint(UU_GITRC,(us,"ug_angle(vc2=<%g,%g,%g>,)",
		vc2[0],vc2[1],vc2[2]));
	ug_unitvc(vc1, uvc1);
	ug_unitvc(vc2, uvc2);
	ang = ug_dot (uvc1, uvc2);
	ang = ug_acos(ang);
	uu_dprint(UU_GITRC,(us,"ug_angle ret ang=%g",ang));
	uu_dexit;
	return (ang);
	}
/************** ug_cross ********************************************/
ug_cross(v1,v2,vr)
	Gfloat v1[3];
	Gfloat v2[3];
	Gfloat vr[3];

	{
	int i;				/* index */
	Gfloat rv[3];		/* temporary resultant vector */

	rv[0] = (v1[1] *v2[2]) - (v1[2] *v2[1]);
	rv[1] = (v1[2] *v2[0]) - (v1[0] *v2[2]); rv[2] = (v1[0] *v2[1]) - (v1[1] *v2[0]);
	for (i = 0; i < 3; i++) vr[i]  =  rv[i];
	}
/************** ug_acos ********************************************/
Gfloat ug_acos(acosval)
	Gfloat	acosval;								/* arccosine value */
{
	double x,z;
	double y,q;
	x = acosval;
	z = fabs(x);
	q = 1.0;
	if ( z>q )					/* undefined */
		return(0);									/* always return a zero */
	else
	{
		y = acos(x);
#if UU_COMP == UU_WINNT
		y = acos(x);
#endif
	}
		return( y );
}

