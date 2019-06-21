
/*********************************************************************
**    NAME         :  m3ecirc1.c
**       CONTAINS:
**			int um_c3_test_3pt(pt1, pt2, pt3)
**			int um_c3_3pt(pt1, pt2, pt3, cptr)
**			int um_c3_crn(center, radius, normal, cptr)
**			int um_c3_cpn(center, cpt, normal, cptr)
**			int um_c3_cr(center, radius, cptr)
**			int um_c3_cp(center, cpt, cptr)
**			int um_c3_ct_line(eptr, center, ploc, cptr)
**			int um_c3_ct_circle(eptr, center, ploc, cptr)
**         int um_c3_arc3pt(pt1, pt2, pt2, cptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ecirc1.c , 25.1
**     DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:50
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mdcpln.h"
#include "modef.h"


/*********************************************************************
**    E_FUNCTION     : int um_c3_test_3pt(pt1, pt2, pt3)
**			Verify that a circle can be built through three points.
**
**    PARAMETERS   
**       INPUT  : 
**				pt1					first point
**				pt2					second point
**				pt3					third point
**       OUTPUT :  
**				none
**    RETURNS      : 
**				UU_SUCCESS if circle can be built, UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_c3_test_3pt(pt1, pt2, pt3)
UM_coord		pt1, pt2, pt3;
{
	int	status;					/* error status */
	UM_vector	a;					/* vector from the first to the second
											point */
	UM_vector	b;					/* vector from the first to the third
											point */
	UM_vector	c;					/* vector from the second to the third
											point */
	UM_vector	ua, ub;			/* unit vectors of a and b */
	UM_vector	v1;
	UM_length	ma, mb, mv1;	/* vector magnitudes */
	UM_vector	v2;				/*  a  X b  */
	UU_REAL		mv2;				/* |a  X b| */
	UU_REAL radius;

/*--------------------------------------------------------------------
**
**	Calculate vectors from the first point to the second and from
** the first point to to the third to set up the equation in
** which the center is the intersection of the perpendicular
**	bisectors of the vectors.
*/
	status = UU_SUCCESS;
	um_vcmnvc(pt2, pt1, a );
	um_vcmnvc(pt3, pt1, b );
	um_vcmnvc(pt3, pt2, c );
	if (um_cceqcc(a,UM_zerovec) || um_cceqcc(b,UM_zerovec) || um_cceqcc(c,UM_zerovec))
		status = UU_FAILURE;
	else
	{
		um_unitvc(a,ua);
		um_unitvc(b,ub);
		if (um_vcparall(ua,ub))
			status = UU_FAILURE;
		else
		{
			um_vcmnvc( a, b, v1 );
			ma = um_mag(a);
			mb = um_mag(b);
			mv1 = um_mag(v1);
			um_cross( a, b, v2 );
			mv2 = um_mag(v2);
			radius = ma * mb * mv1 / ( 2.0 * mv2 );
			if (radius < UM_FUZZ)
				status = UU_FAILURE;
		}
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int um_c3_3pt1(vocal, pt1, pt2, pt3, cptr)
**			Create a circle through three points. The data defining the
**			circle is determined as follows:
**
**				1. the center and radius are calculated using the
**					three points;
**				2. the normal to the plane is defined by the cross
**					product of the vector from PT1 to PT2 and
**					the vector from PT1 to PT3;
**				3. the start vector is defined to be the vector from the
**					center to PT1;
**				4. the angle is 2 PI;
**    PARAMETERS   
**       INPUT  : 
**				vocal				output error if 1, else return status
**				pt1					first point
**				pt2					second point
**				pt3					third point
**       OUTPUT :  
**				cptr					pointer to circle entity
**    RETURNS      : 
**				0 iff circle data defined; else -1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c3_3pt1(vocal, pt1, pt2, pt3, cptr)
	int vocal;
	UM_coord		pt1, pt2, pt3;
	struct UM_circle_rec *cptr;

	{
	int	status;					/* error status */
	UM_vector	a;					/* vector from the first to the second
											point */
	UM_vector	b;					/* vector from the first to the third
											point */
	UM_vector	c;					/* vector from the second to the third
											point */
	UM_vector	ua, ub;			/* unit vectors of a and b */
	UM_vector	v1;				/* a - b */
	UM_length	ma, mb, mv1;	/* vector magnitudes */
	UM_vector	v2;				/*  a  X b  */
	UU_REAL		mv2;				/* |a  X b| */
	UU_REAL 		denom,
		  			afactor,
		  			bfactor,
		  			cfactor;
	UM_coord 	uc,
		  			center,
		  			temp,
		  			pta,
		  			ptb;

/*--------------------------------------------------------------------
**
**	Calculate vectors from the first point to the second and from
** the first point to to the third to set up the equation in
** which the center is the intersection of the perpendicular
**	bisectors of the vectors.
*/
	uu_denter(UU_MTRC,(us,"um_c3_3pt(?..?)"));
	status = -1;
	um_vcmnvc(pt2, pt1, a );
	um_vcmnvc(pt3, pt1, b );
	um_vcmnvc(pt3, pt2, c );
	if (um_cceqcc(a,UM_zerovec) || um_cceqcc(b,UM_zerovec) || um_cceqcc(c,UM_zerovec))
		{
		if (vocal == 1)
			uu_uerror0(/*points are collinear*/UM_MODEL,24);
		else
			goto Done;
		}
	else
		{
		um_unitvc(a,ua);
		um_unitvc(b,ub);
		if (um_vcparall(ua,ub))
			{
			if (vocal == 1)
				uu_uerror0(/*points are collinear*/UM_MODEL,24);
			else
				goto Done;
			}
		else
			{
			ur_setup_data(UM_CIRCLE_REL, cptr, sizeof(struct UM_circle_rec));
			/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
			strcpy (cptr->label, "");
			cptr->subscr = 0;
			um_vcmnvc( a, b, v1 );
			ma = um_mag(a);
			mb = um_mag(b);
			mv1 = um_mag(v1);
			um_cross( a, b, v2 );
			mv2 = um_mag(v2);
			cptr->radius = ma * mb * mv1 / ( 2.0 * mv2 );
			if (cptr->radius < UM_FUZZ)
				{
				if (vocal == 1)
					uu_uerror0(/*radius is too small*/UM_MODEL,19);
				else
					goto Done;
				}
			else
				{
				cptr->dang = UM_TWOPI;
			
				um_vcplvc(pt1,pt2,pta);
				um_vctmsc(pta,(UU_REAL) 0.5,pta);
				um_vcplvc(pt1,pt3,ptb);
				um_vctmsc(ptb,(UU_REAL) 0.5,ptb);
			
				um_cross(ua,ub,uc);
				um_unitvc(uc,uc);
				um_vctovc(uc,cptr->nvec);
			
				um_cross(ub,uc,temp);
				denom = um_dot(ua,temp);

				afactor = um_dot(ua,pta);
				um_vctmsc(temp,afactor,center);

				bfactor = um_dot(ub,ptb);
				um_cross(uc,ua,temp);
				um_vctmsc(temp,bfactor,temp);
				um_vcplvc(temp,center,center);

				cfactor = um_dot(uc,pt1);
				um_cross(ua,ub,temp);
				um_vctmsc(temp,cfactor,temp);
				um_vcplvc(temp,center,center);

				um_vctmsc(center,(1.0/denom),cptr->center);
				um_cir_svec(cptr);
				status = 0;
				}
			}
		}
Done:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_c3_3pt(pt1, pt2, pt3, cptr)
**			Create a circle through three points. The data defining the
**			circle is determined as follows:
**
**				1. the center and radius are calculated using the
**					three points;
**				2. the normal to the plane is defined by the cross
**					product of the vector from PT1 to PT2 and
**					the vector from PT1 to PT3;
**				3. the start vector is defined to be the vector from the
**					center to PT1;
**				4. the angle is 2 PI;
**    PARAMETERS   
**       INPUT  : 
**				pt1					first point
**				pt2					second point
**				pt3					third point
**       OUTPUT :  
**				cptr					pointer to circle entity
**    RETURNS      : 
**				0 iff circle data defined; else -1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c3_3pt(pt1, pt2, pt3, cptr)
	UM_coord		pt1, pt2, pt3;
	struct UM_circle_rec *cptr;

	{
	int	status;					/* error status */
	int vocal = 1;

	uu_denter(UU_MTRC,(us,"um_c3_3pt(?..?)"));

	status = um_c3_3pt1(vocal, pt1, pt2, pt3, cptr);

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_c3_crn(center, radius, normal, cptr)
**			Given the CENTER point, RADIUS, and NORMAL, determine the
**			remaining data defining a circle entity as follows:
**
**				1. the angle of the circle is 2 PI;
**				2. the start vector of the circle is the projection
**					of the modeling coordinate MX-axis onto the plane
**					of the circle (or the projection of the modeling
**					MZ-axis onto the plane if the MX-axis is parallel
**					to the NORMAL)
**    PARAMETERS   
**       INPUT  : 
**				center				center of circle
**				radius				radius of circle
**				normal				normal vector to plane of circle
**       OUTPUT :  
**				cptr					pointer to circle entity
**    RETURNS      : 
**				0 iff circle data defined; else -1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c3_crn(center, radius, normal, cptr)
	UM_coord center;
	UM_length radius;
	UM_vector normal;
	struct UM_circle_rec *cptr;

	{
	int status;

	uu_denter(UU_MTRC,(us,"um_c3_crn()"));
	ur_setup_data(UM_CIRCLE_REL, cptr, sizeof(struct UM_circle_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (cptr->label, "");
	cptr->subscr = 0;
	if (radius < UM_FUZZ)
		{
		uu_uerror0(/* radius is too small */UM_MODEL, 19);
		status = -1;
		}
	else
		{
		um_vctovc(center, cptr->center);
		um_unitvc(normal, cptr->nvec);
		cptr->radius = radius;
		cptr->dang = UM_TWOPI;
		um_cir_svec(cptr);
		status = 0;
		}
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_c3_cpn(center, cpt, normal, cptr)
**			Given the CENTER point of a circle, the NORMAL to the
**			plane of the circle, and a point (CPT) whose
**			projection onto the plane of the circle (pcpt) lies on the
**			circumference of the circle, determine the remaining data
**			defining the circle as follows:
**
**				1. the angle of the circle is 2 PI;
**				2. the start vector of the circle is the projection
**					of the modeling coordinate MX-axis onto the plane
**					of the circle (or the projection of the modeling
**					MZ-axis onto the plane if the MX-axis is parallel
**					to the CZ-axis)
**    PARAMETERS   
**       INPUT  : 
**				center				center of circle
**				cpt					point on circumference
**				normal				normal vector to plane of circle
**       OUTPUT :  
**				cptr					pointer to circle entity 
**    RETURNS      : 
**				0 iff circle entity defined; else -1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c3_cpn(center, cpt, normal, cptr)
	UM_coord center;
	UM_coord cpt;
	UM_vector normal;
	struct UM_circle_rec *cptr;

	{
	int status;
	UM_coord pcpt;				/* nearest point to given point on plane */

	uu_denter(UU_MTRC,(us,"um_c3_cpn()"));
	ur_setup_data(UM_CIRCLE_REL, cptr, sizeof(struct UM_circle_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (cptr->label, "");
	cptr->subscr = 0;
	if (um_cceqcc(center,cpt))
		{
		uu_uerror0(/*center and circumference point are identical*/UM_MODEL,20);
		status = -1;
		}
	else
		{
		um_unitvc(normal, cptr->nvec);
		um_nptpln(cpt, center, cptr->nvec, pcpt);
		if (!um_cceqcc(cpt, pcpt))
			{
			uu_uerror0(/*given point will be projected onto plane of circle*/
							UM_MODEL, 21);
			}
		cptr->radius = um_dcccc(center, pcpt);
		if (cptr->radius < UM_FUZZ)
			{
			uu_uerror0(/* radius is too small */UM_MODEL,19);
			status = -1;
			}
		else
			{
			um_vctovc(center, cptr->center);
			cptr->dang = UM_TWOPI;
			um_cir_svec(cptr);
			status = 0;
			}
		}
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_c3_cr(center, radius, cptr)
**			Given the CENTER point and RADIUS, determine the remaining
**			data defining a circle entity as follows:
**
**				1. the normal to the plane of the circle is defined
**					to be the construction plane CZ-axis;
**				2. the angle of the circle is 2 PI;
**				3. the start vector of the circle is the projection
**					of the modeling coordinate MX-axis onto the plane
**					of the circle (or the projection of the modeling
**					MZ-axis onto the plane if the MX-axis is parallel
**					to the CZ-axis)
**    PARAMETERS   
**       INPUT  : 
**				center				center of circle
**				radius				radius of circle
**       OUTPUT :  
**				cptr					pointer to circle entity
**    RETURNS      : 
**				0 iff circle data defined; else -1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c3_cr(center, radius, cptr)
	UM_coord center;
	UM_length radius;
	struct UM_circle_rec *cptr;

	{
	int status;

	uu_denter(UU_MTRC,(us,"um_c3_cr()"));
	ur_setup_data(UM_CIRCLE_REL, cptr, sizeof(struct UM_circle_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (cptr->label, "");
	cptr->subscr = 0;
	status = um_c3_crn(center, radius, UM_cpln.zaxis, cptr);
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_c3_cp(center, cpt, cptr)
**			Given the CENTER point of a circle and a point (CPT) whose
**			projection onto the plane of the circle (pcpt) lies on the
**			circumference of the circle, determine the remaining data
**			defining the circle as follows:
**
**				1. the normal to the plane of the circle is defined
**					to be the construction plane CZ-axis;
**				2. the angle of the circle is 2 PI;
**				3. the start vector of the circle is the projection
**					of the modeling coordinate MX-axis onto the plane
**					of the circle (or the projection of the modeling
**					MZ-axis onto the plane if the MX-axis is parallel
**					to the CZ-axis)
**    PARAMETERS   
**       INPUT  : 
**				center				center of circle
**				cpt					point on circumference
**       OUTPUT :  
**				cptr					pointer to circle entity 
**    RETURNS      : 
**				0 iff circle entity defined; else -1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c3_cp(center, cpt, cptr)
	UM_coord center;
	UM_coord cpt;
	struct UM_circle_rec *cptr;

	{
	int status;

	uu_denter(UU_MTRC,(us,"um_c3_cp()"));
	ur_setup_data(UM_CIRCLE_REL, cptr, sizeof(struct UM_circle_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (cptr->label, "");
	cptr->subscr = 0;
	status = um_c3_cpn(center, cpt, UM_cpln.zaxis, cptr);
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_c3_ct_line(eptr, center, ploc, cptr)
**       Create a circle tangent to a line (EPTR) and having the
**			given center point (CENTER).
**    PARAMETERS   
**       INPUT  : 
**          eptr						line circle is to be tangent to
**				center					center point of circle
**				ploc						picked location on line (not used
**											but must be present because objects
**											require identical argument lists)
**       OUTPUT :  
**          cptr						circle tangent to line
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c3_ct_line(eptr, center, ploc, cptr)
	struct UM_line_rec *eptr;
	UM_coord center;
	UD_NDCLOCREC *ploc;
	struct UM_circle_rec *cptr;

	{
	UU_REAL lvc[3];					/* unit vector along line */
	UU_REAL cvc[3];					/* vector from line start to center */
	UU_REAL npt[3];					/* nearest point on line to center */
	int status;

	uu_denter(UU_MTRC,(us,"um_c3_ct_line(key=%d,pt=%x,ploc=%x)",
			eptr->key,center,ploc));
	status = -1;
	ur_setup_data(UM_CIRCLE_REL, cptr, sizeof(struct UM_circle_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (cptr->label, "");
	cptr->subscr = 0;
	um_vcmnvc(eptr->ept, eptr->spt, lvc);
	um_unitvc(lvc, lvc);
	um_nptln(center, eptr->spt, lvc, npt);
	cptr->radius = um_dcccc(center, npt);
	if (cptr->radius < UM_FUZZ)
		uu_uerror0(/*radius is too small*/UM_MODEL,19);
	else
		{
		um_vctovc(center, cptr->center);
		um_vcmnvc(cptr->center, eptr->spt, cvc);
		um_cross(cvc, lvc, cptr->nvec);
		um_unitvc(cptr->nvec, cptr->nvec);
		cptr->dang = UM_TWOPI;
		um_cir_svec(cptr);
		status = 0;
		}
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_c3_ct_circle(eptr, center, ploc, cptr)
**       Create a circle tangent to a line (EPTR) and having the
**			given center point (CENTER).
**    PARAMETERS   
**       INPUT  : 
**          eptr						circle circle is to be tangent to
**				center					center point of circle
**				ploc						picked location on line (not used
**											but must be present because objects
**											require identical argument lists)
**       OUTPUT :  
**          cptr						circle tangent to line
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c3_ct_circle(eptr, center, ploc, cptr)
	struct UM_circle_rec *eptr;
	UM_coord center;
	UD_NDCLOCREC *ploc;
	struct UM_circle_rec *cptr;

	{
	UM_vector lvc;						/* unit vector along line */
	int nint;							/* number of intersection points */
	UM_coord ipt[2];					/* intersection of line / circle */
	int um_nearest_to_ploc();
	int closest;
	int status;

	uu_denter(UU_MTRC,(us,"um_c3_ct_circle(key=%d,center=%x,ploc=%x)",
		eptr->key,center,ploc));
	status = -1;
	if (um_cceqcc(center,eptr->center))
		uu_uerror0(/*new center is identical to circle center*/
						UM_MODEL,22);
	else
		{
		ur_setup_data(UM_CIRCLE_REL, cptr, sizeof(struct UM_circle_rec));
		/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
		strcpy (cptr->label, "");
		cptr->subscr = 0;
		um_vctovc(center, cptr->center);
		um_vcmnvc(cptr->center, eptr->center, lvc);
		um_unitvc(lvc, lvc);
		um_ilncir(cptr->center, lvc, eptr->center,
			   eptr->nvec, eptr->radius, &nint, ipt);
		closest = um_nearest_to_ploc(ploc, 2, ipt);
		cptr->radius = um_dcccc(ipt[closest], cptr->center);
		if (cptr->radius < UM_FUZZ)
			uu_uerror0(/*radius is too small*/UM_MODEL,19);
		else
			{
			cptr->dang = UM_TWOPI;
			um_vctovc(eptr->nvec, cptr->nvec);
			um_cir_svec(cptr);
			status = 0;
			}
		}
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_c3_arc3pt(pt1, pt2, pt2, cptr)
**         Create a circular arc through three points. The order
**         of the three points determines the arc created 
**         (i.e. from first point, through the second point,
**         to the third point).
**    PARAMETERS   
**       INPUT  : 
**            pt1               first point
**            pt2               second point
**            pt3               third point
**       OUTPUT :  
**          cptr               circle entity
**    RETURNS      : 
**            0 iff arc data defined; else -1
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c3_arc3pt(pt1, pt2, pt3, cptr)
   UM_coord pt1;
   UM_coord pt2;
   UM_coord pt3;
   struct UM_circle_rec *cptr;

   {
   int status;
   UM_vector   v1, v3;         /* vectors from the center of the
                                 circle to points 1 and 3 */
   UM_vector  v1xv3;            /* v1 cross v3 */
   UU_REAL sign;

   uu_denter( UU_MTRC,(us,"um_c3_arc3pt()"));
   ur_setup_data(UM_CIRCLE_REL, cptr, sizeof(struct UM_circle_rec));
   /* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
   strcpy (cptr->label, "");
   cptr->subscr = 0;
   status = um_c3_3pt(pt1, pt2, pt3, cptr);
   if (status == 0)
      {
      um_vcmnvc(pt1, cptr->center, v1 );
      um_unitvc(v1, cptr->svec);
      um_vcmnvc(pt3, cptr->center, v3 );
      um_cross(v1, v3, v1xv3);
      sign = um_dot(v1xv3, cptr->nvec);
      cptr->dang = um_angle(v1, v3);
      if (sign < 0)     cptr->dang = UM_TWOPI - cptr->dang;
      }
   uu_dexit;
   return (status);
   }
