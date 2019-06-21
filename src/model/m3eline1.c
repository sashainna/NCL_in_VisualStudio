/*********************************************************************
**    NAME         :  m3eline1.c
**       CONTAINS:
**			int um_c2_projtopln(lold, pt, normal, lnew)
**			int um_c2_pt_cir(cptr, tfmat, pt, ploc, lptr)
**			int um_c2_tt(c1ptr, p1loc, c2ptr, p2loc, lptr)
**			int um_c2_parto(lold, ploc, dist, lnew)
**			int um_c2_angle(lold, pt, zaxis, angle, length, lnew)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3eline1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:54
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "dasnog.h"
#include "mdrel.h"
#include "mattr.h"
#include "mcrv.h"
#include "mdeval.h"
#include "mdcpln.h"
#include "modef.h"

/*********************************************************************
**    E_FUNCTION     : int um_c2_projtopln(lold, pt, normal, lnew)
**       Create a new line (LNEW) by projecting a line (LOLD) onto
**			a plane defined by a point (PT) and normal (NORMAL).
**    PARAMETERS   
**       INPUT  : 
**          lold						line to project
**				pt							point defining plane
**				normal					normal of plane
**       OUTPUT :  
**          lnew						new line projected onto plane
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c2_projtopln(lold, pt, normal, lnew)
	struct UM_line_rec *lold;
	UM_coord pt;
	UM_vector normal;
	struct UM_line_rec *lnew;

	{
	int status;

	uu_denter(UU_MTRC,(us,"um_c2_projtopln(key=%d, pt=(%f,%f,%f), normal=(%f,%f,%f))",
		lold->key, pt[0],pt[1],pt[2], normal[0],normal[1],normal[2]));
	ur_setup_data(UM_LINE_REL, lnew, sizeof(struct UM_line_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (lnew->label, "");
	lnew->subscr = 0;
	um_nptpln(lold->spt, pt, normal, lnew->spt);
	um_nptpln(lold->ept, pt, normal, lnew->ept);
	if (!um_cceqcc(lnew->spt, lnew->ept))
		status = UU_SUCCESS;
	else
		{
		uu_uerror0(/*start and end points are identical*/UM_MODEL, 55);
		status = UU_FAILURE;
		}
	uu_dexitstatus("um_c2_projtopln", status);
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_c2_pt_cir(cptr, tfmat, pt, ploc, lptr)
**       Create a line through a given point (PT) and tangent to a 
**			circle (EPTR, TFMAT). If the point does not lie in the plane of
**			the circle, it will be projected onto the plane. The picked
**			location (PLOC) is used to discriminate the desired circle
**			from all possible circles.
**    PARAMETERS   
**       INPUT  : 
**          eptr				circle entity
**				tfmat				transformation matrix
**				pt					point line is to go through
**				ploc				pick location for discriminating between
**									multiple tangent points
**       OUTPUT :  
**          lptr				line entity
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : 
**			tfmat is not applied to the circle; it is assumed to be the
**			identity.
*********************************************************************/
int
um_c2_pt_cir(eptr, tfmat, pt, ploc, lptr)
	struct UM_circle_rec *eptr;
	UM_transf tfmat;
	UM_coord pt;
	UD_NDCLOCREC *ploc;
	struct UM_line_rec *lptr;

	{
	UM_length dist;				/* distance of line start point to circle 
											center */
	UM_coord ppt;					/* point projected onto plane of circle */
	int nint;						/* number of circle/circle intersections */
	UM_coord ipt[2];				/* circle/circle intersection points */
	int closest;					/* index of closest intersection point to 
											picked point */
	int status;

	uu_denter( UU_MTRC,(us,"um_c2_pt_cir(key:%d,?,?)",eptr->key));
	status = UU_FAILURE;
	if (eptr->rel_num  !=  UM_CIRCLE_REL)
		uu_uerror0(/*you must pick a circle*/UM_MODEL,56);
	else
		{
		ur_setup_data(UM_LINE_REL, lptr, sizeof(struct UM_line_rec));
		/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
		strcpy (lptr->label, "");
		lptr->subscr = 0;
		um_nptpln(pt, eptr->center, eptr->nvec, ppt);
		if (!um_cceqcc(pt,ppt))
			uu_uerror0(/*point is not in plane of circle*/UM_MODEL,57);
		dist = um_dcccc(ppt, eptr->center);
		if (dist < (eptr->radius + UM_FUZZ) )
			uu_uerror0(/*point is inside circle*/UM_MODEL,58);
		else
			{
			um_vctovc(ppt, lptr->spt);
			um_vcplvc(eptr->center, lptr->spt, ppt);
			um_vctmsc(ppt, (UU_REAL) 0.5, ppt);
			um_icircir(eptr->center, eptr->nvec, eptr->radius, 
			  ppt, eptr->nvec, dist / 2.0, &nint, ipt);
			closest = um_nearest_to_ploc(ploc,nint,ipt);
			um_vctovc(ipt[closest],lptr->ept);
			if (um_cceqcc(lptr->spt, lptr->ept))
				uu_uerror0(/*start and end point are identical*/UM_MODEL,55);
			else status = UU_SUCCESS;
			}
		}
	uu_dexitstatus("um_c2_pt_cir", status);
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_c2_tt(c1ptr, p1loc, c2ptr, p2loc, lptr)
**			Create a line tangent to two circles. The two circles must
**			lie in the same plane. The two pick locations (P1LOC and
**			P2LOC) are used to select the desired line from all possible
**			tangent lines.
**    PARAMETERS   
**       INPUT  : 
**          c1ptr					first circle
**				p1loc					pick location on first circle
**				c2ptr					second circle
**				p2ptr					pick location on second circle
**       OUTPUT :  
**          lptr					line entity
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c2_tt(c1ptr,p1loc,c2ptr,p2loc,lptr)
	struct UM_circle_rec *c1ptr;
	UD_NDCLOCREC *p1loc;
	struct UM_circle_rec *c2ptr;
	UD_NDCLOCREC *p2loc;
	struct UM_line_rec *lptr;

	{
	struct UM_circle_rec  *lrc;	/* pointer to largest radius circle */
	struct UM_circle_rec  *src;	/* pointer to smallest radius circle */
	UD_NDCLOCREC *pptlr;				/* pick location for largest circle */
	UD_NDCLOCREC *pptsr;				/* pick location for smallest circle */
	UM_coord ccvc;						/* vector from small circle center to
												large circle center */
	UM_length dist;					/* length of this vector */
	UM_vector lrvc;					/* vector from large radius circle to 
												point picked */
	UM_vector srvc;					/* vector from small radius circle to 
												point picked */
	UM_vector cclrvc;					/* cross product of ccvc and lrvc */
	UM_vector ccsrvc;					/* cross product of ccvc and srvc */
	UU_REAL signtest;					/* used to tell if external or internal
												tangents are to be calculated */
	UM_length rad;						/* radius of circle to calc tangents */
	UM_coord cent;						/* center of circle to calc tangents */
	int nint;							/* number of intersection points */
	UM_coord ipt[2];					/* intersection points */
	int closest;						/* index of closest intersection point to
												picked location */
	UM_vector lvc[3];					/* vector defining tangent line */
	UM_vector nvc[3];					/* vector normal to tangent line */
	UM_coord npt[3];					/* nearest point to given point on plane 
												of circle */
	UU_REAL temp[3];
	UU_REAL arclen;
	int status;

	uu_denter( UU_MTRC,(us,"um_c2_tt()"));
	status = UU_FAILURE;
	if ((c1ptr->rel_num != UM_CIRCLE_REL) || (c2ptr->rel_num != UM_CIRCLE_REL))
  	  uu_uerror0(/*you must pick circles*/UM_MODEL,61);					
	else
		{
		if (c1ptr->key == c2ptr->key)
	   	uu_uerror0(/*second circle picked is same as first*/
				UM_MODEL,60);
		else if (!um_vcparall(c1ptr->nvec,c2ptr->nvec))
	   	uu_uerror0(/*circles do not lie in same plane*/
				UM_MODEL,62);
		else
			{
			um_nptpln(c1ptr->center,c2ptr->center,c2ptr->nvec,npt);
			if (!um_cceqcc(c1ptr->center,npt))
				uu_uerror0(/*circles do not lie in same plane*/
					UM_MODEL,62);
			else
				{
				lrc = c1ptr;
				src = c2ptr;
				pptlr = p1loc;
				pptsr = p2loc;
				if (c1ptr->radius < c2ptr->radius)
					{
					lrc = c2ptr;
					src = c1ptr;
					pptlr = p2loc;
					pptsr = p1loc;
					}
				dist = um_dcccc(lrc->center, src->center);
				if ( (dist + src->radius) < lrc->radius)
					uu_uerror0(/*one circle is inside other*/
						UM_MODEL,64);
				else
					{
					ur_setup_data(UM_LINE_REL, lptr, sizeof(struct UM_line_rec));
					/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
					strcpy (lptr->label, "");
					lptr->subscr = 0;
					um_vcmnvc(lrc->center, src->center, ccvc);
					status = um_projploctopln(pptlr, lrc->center, lrc->nvec, temp);
					um_vcmnvc(temp, lrc->center, lrvc);
					status = um_projploctopln(pptsr, src->center, src->nvec, temp);
					um_vcmnvc(temp, src->center, srvc);
					um_cross(ccvc, lrvc, cclrvc);
					um_cross(ccvc, srvc, ccsrvc);
					signtest = um_dot(cclrvc, ccsrvc);
					if (signtest < 0)
						{
						rad = lrc->radius + src->radius;
						}
					else
						{
						rad = lrc->radius - src->radius;
						}
					um_vcmnvc(lrc->center, src->center, lrvc);
					if ((dist < (lrc->radius + src->radius))
						&& (signtest < 0.0))
							ud_wrerr
							("no solution: pick points closer to desired line");
					else
						{
						if (rad < UM_FUZZ)
							{
							um_cross(lrvc, lrc->nvec, nvc);
							um_unitvc(nvc, nvc);
							um_vctmsc(nvc, lrc->radius, nvc);
							um_vcplvc(lrc->center, nvc, ipt[0]);
							um_vcmnvc(lrc->center, nvc, ipt[1]);
							closest = um_nearest_to_ploc(pptlr, 2, ipt);
							um_vctovc(ipt[closest], lptr->ept);
							um_vcmnvc(lptr->ept, lrvc, lptr->spt);
							}
						else
							{
							um_vcplvc(lrc->center, src->center, cent);
							um_vctmsc(cent, (UU_REAL) 0.5, cent);
							um_icircir(lrc->center, lrc->nvec, rad, 
							  cent, lrc->nvec, dist / 2.0, &nint, ipt);
							closest = um_nearest_to_ploc(pptlr, nint, ipt);
							um_vcmnvc(ipt[closest], src->center, lvc);
							um_cross(lrvc, lvc, nvc);
							um_cross(nvc, lvc, nvc);
							um_unitvc(nvc, nvc);
							um_vctmsc(nvc, src->radius, nvc);
							if (signtest < 0) um_vctmsc(nvc, (UU_REAL)  - 1.0, nvc);
							um_vcplvc(src->center, nvc, lptr->spt);
							um_vcplvc(ipt[closest], nvc, lptr->ept);
							}
						arclen = um_dcccc(lptr->spt,lptr->ept);
						if (arclen < UM_FUZZ)
							uu_uerror0(/*start and end point are identical*/
								UM_MODEL,55);
						else status = UU_SUCCESS;
						}
					}
				}
			}
		}
	uu_dexitstatus("um_c2_tt", status);
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_c2_parto(lold, ploc, dist, lnew)
**			Given a line (LOLD) and a pick location (PLOC), create a
**			new line (LNEW) which is offset from the old line by the
**			specified distance (DIST). The direction of offset is
**			parallel to the cross product of a vector along the line
**			and the construction plane normal and on the same side of
**			the line as the picked location.
**    PARAMETERS   
**       INPUT  : 
**				lold						old line 
**				ploc						picked location on line
**				dist						distance to offset
**       OUTPUT :  
**          l							new offset line
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c2_parto(e, ploc, dist, lnew)
	struct UM_line_rec *e;
	UD_NDCLOCREC  *ploc;
	UM_length dist;
	struct UM_line_rec *lnew;

	{
	UM_vector lnvec;				/* unit vector from start to end point (x) */
	UM_vector dirvec;				/* unit direction vector (y) */
	UM_vector normvec;			/* unit normal vector (z) */
	UM_vector ptvec;				/* unit vector from start to picked point */
	UM_vector offset;				/* vector to translate picked line */
	UU_REAL signtest;				/* sign determines side to put new line */
	UM_coord ipt;					/* intersection point */
	UM_coord pt;					/* coordinate of picked location in model
										   coordinates */
	UM_vector vpnorm;				/* view normal*/
	int status;

	uu_denter( UU_MTRC,(us,"um_c2_parto()"));
	status = UU_FAILURE;
	if (e->rel_num  != UM_LINE_REL)
		uu_uerror0(/*you must pick a line*/UM_MODEL,67);
	else
		{
		if (fabs(dist) < UM_FUZZ)
			uu_uerror0(/*distance is too small*/UM_MODEL,66);
		else
			{
			um_vcmnvc(e->ept, e->spt, lnvec);
			um_unitvc(lnvec,lnvec);
			if (um_vcparall(UM_cpln.zaxis,lnvec))
				 uu_uerror0(
				 	/*line is parallel to construction plane normal*/
					UM_MODEL,68);
			else
				{
				um_cross(UM_cpln.zaxis, lnvec, dirvec);
				um_unitvc(dirvec,dirvec);
				um_cross(lnvec, dirvec, normvec);
				um_unitvc(normvec,normvec);
				status = um_projploctopln(ploc, e->spt, normvec, ipt);
				if (status != 0)
					um_pscroll("um_c2_parto: no intersection of line and plane");
				else
					{
					um_vcmnvc(ipt, e->spt, ptvec);
					um_unitvc(ptvec,ptvec);
					signtest = um_dot(dirvec, ptvec);
					if (signtest < 0)
						um_vctmsc(dirvec, -dist, offset);
					else
						um_vctmsc(dirvec, dist, offset);

					ur_setup_data(UM_LINE_REL, lnew, sizeof(struct UM_line_rec));
					/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
					strcpy (lnew->label, "");
					lnew->subscr = 0;
					um_vcplvc(e->spt, offset, lnew->spt);
					um_vcplvc(e->ept, offset, lnew->ept);
					status = UU_SUCCESS;
					}
				}
			}
		}
	uu_dexitstatus("um_c2_parto", status);
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_c2_angle(lold, pt, angle,
**													length, ploc, lnew)
**			Create a line (LNEW) which goes through the specified
**			point (PT) and makes the specified angle (ANGLE) with
**			the given line (LOLD). If the point does not lie on the old
**			line, the new line will lie in the plane defined by the point
**			and the old line. If the point does lie
**			on the old line, then the new line will lie in the 
**			plane defined by the construction plane normal and the
**			given point. The length of the line is then specified
**			by LENGTH. In either case, the desired line is chosen to be
**			that on the same side of the line as the PLOC.
**    PARAMETERS   
**       INPUT  : 
**				lold						old line
**				pt							start point of new line and point
**											defining plane
**				angle						angle new line makes with projection of
**											old line onto plane (-PI <= angle <= PI)
**				length					length of new line 
**				ploc						picked location on old line 
**       OUTPUT :  
**          lnew						new line
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c2_angle(lold, pt, angle, length, ploc, lnew)
	struct UM_line_rec *lold;
	UM_coord pt;
	UM_angle angle;
	UM_length length;
	UD_NDCLOCREC *ploc;
	struct UM_line_rec *lnew;

	{
	int			status;
	UM_length	distx;			/* distance  along the x axis */
	UM_length	disty;			/* distance  along the y axis */
	UM_coord		ptp;				/* the perpendicular projection of
											the chosen point onto the line */
	UM_vector	vx;				/* the vector along the x-axis */
	UM_vector	vy;				/* the vector along the y-axis */
	UM_vector	vz;				/* the vector along the z-axis */
	UM_coord		pts[4];			/* points to determine angle */
	int			closest;			/* index of closest point to picked point */
	UM_coord		temppt;			/* temporary point */
	UM_vector	vpnorm;			/* view plane normal */
	UU_REAL		signtest;		/* used to test direction of vectors */
	UM_length	arclen;

	uu_denter(UU_MTRC,(us,"um_c2_angle()"));
	ur_setup_data(UM_LINE_REL, lnew, sizeof(struct UM_line_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (lnew->label, "");
	lnew->subscr = 0;
	status = UU_FAILURE;
	if(lnew->rel_num != UM_LINE_REL)
		{
		uu_uerror0(/*you must pick a line*/UM_MODEL,67);
		}
	else
		{
		if ( angle < -UM_PI || angle > UM_PI )
			{
			uu_uerror0(/*angle out of range*/UM_MODEL,69);
			}
		else if (fabs(angle) < UM_FUZZ)
			{
			uu_uerror0(/*zero degree angle not allowed*/UM_MODEL,70);
			}
		else
			{
			um_vcmnvc(lold->ept, lold->spt, vx);
			um_unitvc(vx, vx);
			um_vctovc(pt, lnew->spt);
			um_nptln(lnew->spt, lold->spt, vx, ptp);
			if (um_cceqcc(lnew->spt, ptp))
				{
				um_cross(UM_cpln.zaxis, vx, vy);
				um_unitvc(vy, vy);
				signtest = um_dot(UM_cpln.yaxis, vy);
				if( signtest < 0.0 ) 
					{
					um_vctmsc(vx, (UU_REAL) -1.0, vx);
					um_vctmsc(vy, (UU_REAL) -1.0, vy);
					}
				disty = length * sin(angle);
				um_vctmsc(vy, disty, vy);
				distx = length * cos(angle);
				um_vctmsc(vx, distx, vx);
				um_vcplvc(lnew->spt, vx, lnew->ept);
				um_vcplvc(lnew->ept, vy, lnew->ept);
				/*
				disty = length * sin(angle);
				um_vctmsc(vy, disty, vy);
				distx = length * cos(angle);
				um_vctmsc(vx, distx, vx);
				um_vcplvc(lnew->spt, vx, temppt);
				um_vcplvc(temppt, vy, pts[0]);
				um_vcmnvc(temppt, vy, pts[1]);
				um_vcmnvc(lnew->spt, vx, temppt);
				um_vcplvc(temppt, vy, pts[2]);
				um_vcmnvc(temppt, vy, pts[3]);
				closest = um_nearest_to_ploc(ploc, 4, pts);
				um_vctovc(pts[closest], lnew->ept);
				*/
				}
			else	/* point is not on the line */
				{
				um_vcmnvc(lnew->spt, ptp, vy);
				um_unitvc(vy,vy);
				if (!um_cceqcc(lold->spt,ptp))
					um_vcmnvc(lold->spt, ptp, vx);
				else
					um_vcmnvc(lold->ept, ptp, vx);
				um_unitvc(vx,vx);
				um_cross(vx,vy,vz);
				um_vpnorm(ploc->transform, vpnorm);
				signtest = um_dot(vz,vpnorm);
				if (signtest < 0) um_vctmsc(vx,(UU_REAL) -1.0,vx);
				disty = um_dcccc( lnew->spt, ptp);
				if ( fabs(angle-UM_HALFPI) < UM_FUZZ )
					distx = 0.0;
				else
					distx = disty / tan(angle);
				um_vctmsc(vx, -distx, vx);
				um_vcplvc(ptp, vx, lnew->ept );
				}	
			arclen = um_dcccc(lnew->ept, lnew->spt);
			if (arclen < UM_FUZZ)
				uu_uerror0(/*start point and end point identical*/
					UM_MODEL,55);
			else
				{
				status = UU_SUCCESS;
				}
			}
		}
	uu_dexitstatus("um_c2_angle", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_redef_line(eptr)
**      Redefine line to a line of unit length.
**    PARAMETERS
**       INPUT  :
**          eptr        line entity
**       OUTPUT :
**          eptr        line entity
**    RETURNS      :
**      UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     :
**      Unpredictable results may occur if the curve is closed.
*********************************************************************/
int
um_redef_line(eptr)
  struct UM_line_rec  *eptr;

 {
  int status;
  UM_vector vec;
/*
...make unit length line
*/
  um_vcmnvc (eptr->ept,eptr->spt,vec);
  um_unitvc (vec,vec);
  um_vcplvc (eptr->spt,vec,eptr->ept);

  uu_dexit;
  return (UU_SUCCESS);
 }
