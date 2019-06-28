
/*********************************************************************
**    NAME         :  m3ecirc5.c
**       CONTAINS: 
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ecirc5.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:51
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mdrel.h"
#include "modef.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "mdpick.h"
#include "mdebug.h"

#define PROMPT_BASE 256
#define ERROR_BASE 216
/*********************************************************************
**    E_FUNCTION     : um_cir_3tan(option,e,ploc,circle)
**						Creates a cirlce tangent to the 3 lines given and
**						in the area specified by the points picked.
**    PARAMETERS   
**       INPUT  : 
**				option		UU_TRUE if creating circle
**				e[3]			3 lines that the circle is to be tangent to
**				ploc[3]		the picked locations picked near the vectors
**       OUTPUT :  
**				circle		circle entity with fields are filled in
**    RETURNS      : status		UU_TRUE if error occurred
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UU_LOGICAL um_cir_3tan(option, e, ploc, circle) 
	UU_LOGICAL option;
	struct UM_crvdatabag e[3];
	UU_REAL ploc[3][3];
	struct UM_circle_rec *circle;
	{
	UU_LOGICAL	um_cir_2v2p();
	UU_REAL v0[3],v1[3],v2[3];
	UU_REAL uv0[3],uv1[3],uv2[3];
	int i,n1,n2,n3;
	struct UM_line_rec *a,*b,*c, *line;
	UU_REAL i_ac[3],i_bc[3],i_ab[3];
	UU_REAL r[3],s[3];
	UU_REAL ploc_b_on_a[3];
	UU_REAL angle;
	UU_REAL temp[3];
	UU_LOGICAL error;
	UU_LOGICAL inseg[3];
	int insegs;
	UU_REAL sign;

	uu_denter( UU_MTRC,(us,"um_cir_3tan()"));

	error = UU_FALSE;

	a = (struct UM_line_rec *)&e[0];
	b = (struct UM_line_rec *)&e[1];
	c = (struct UM_line_rec *)&e[2];

	um_vcmnvc(a->ept,a->spt,v0); um_unitvc(v0,v0); um_vctovc(v0,uv0);
	um_vcmnvc(b->ept,b->spt,v1); um_unitvc(v1,v1); um_vctovc(v1,uv1);
	um_vcmnvc(c->ept,c->spt,v2); um_unitvc(v2,v2); um_vctovc(v2,uv2);

	/* get closest point on the vector to the picked location */
	um_nptln(ploc[0],a->spt,v0,ploc[0]);
	um_nptln(ploc[1],b->spt,v1,ploc[1]);
	um_nptln(ploc[2],c->spt,v2,ploc[2]);

	um_ilnln(a->spt, v0, c->spt, v2, &n1, i_ac);
	um_ilnln(b->spt, v1, c->spt, v2, &n2, i_bc);
	um_ilnln(a->spt, v0, b->spt, v1, &n3, i_ab);

	if (um_vcparall(v0,v2) && um_vcparall(v1,v2)) {
				/* all 3 lines cannot be parallel */
		uu_uerror0(UM_MODEL,ERROR_BASE+1);
		error = UU_TRUE;
		}
		/* if there are 2 parallel lines they must be line a and line b */
	else if (um_vcparall(v0,v2)) { /* if a is parallel to c then swap b and c */
		um_vctovc(v2,temp);
		um_vctovc(v1,v2);
		um_vctovc(temp,v1);
		b = (struct UM_line_rec *)&e[2];
		c = (struct UM_line_rec *)&e[1];
		um_vctovc(ploc[2],temp);
		um_vctovc(ploc[1],ploc[2]);
		um_vctovc(temp,ploc[1]);
		}
	else if (um_vcparall(v1,v2)) { /* if b is parallel to c then swap a and c */
		um_vctovc(v2,temp);
		um_vctovc(v0,v2);
		um_vctovc(temp,v0);
		a = (struct UM_line_rec *)&e[2];
		c = (struct UM_line_rec *)&e[0];
		um_vctovc(ploc[2],temp);
		um_vctovc(ploc[0],ploc[2]);
		um_vctovc(temp,ploc[0]);
		}
	else if (um_cceqcc(i_ac,i_bc)) { /* if 3 lines intersect at one point */
		uu_uerror0(UM_MODEL,ERROR_BASE+3);
		error = UU_TRUE;
		}
	else if (!um_vcparall(v0,v1)) {
			 		/* make sure that line c has picked location inbetween */
					/* the intersection points of the 3 lines */
		insegs = ((inseg[0]=um_ptinseg(i_ac, ploc[0], i_ab)) ? 1 : 0);
		insegs += ((inseg[1]=um_ptinseg(i_bc, ploc[1], i_ab)) ? 1 : 0);
		insegs += ((inseg[2]=um_ptinseg(i_ac, ploc[2], i_bc)) ? 1 : 0);

		if ((insegs == 2) || (insegs == 0)) {
				uu_uerror0(UM_MODEL, ERROR_BASE+4); /* ambigous picks */
				error = UU_TRUE;
				}
		else if (! inseg[2]) { /* switch so we have the one inbetween as line c */
				if (inseg[0]) { /* swap a and c */
					um_vctovc(v2,temp); um_vctovc(v0,v2); um_vctovc(temp,v0);
					a = (struct UM_line_rec *)&e[2];
					c = (struct UM_line_rec *)&e[0];
					um_vctovc(ploc[2],temp); 
					um_vctovc(ploc[0],ploc[2]);
					um_vctovc(temp,ploc[0]);
					}
				else { /* swap b and c */
					um_vctovc(v2,temp); um_vctovc(v1,v2); um_vctovc(temp,v1);
					b = (struct UM_line_rec *)&e[2];
					c = (struct UM_line_rec *)&e[1];
					um_vctovc(ploc[2],temp);
					um_vctovc(ploc[1],ploc[2]);
					um_vctovc(temp,ploc[1]);
					}
				}
		}

	if (! error) {
		um_ilnln(a->spt, v0, c->spt, v2, &n1, i_ac);
		um_ilnln(b->spt, v1, c->spt, v2, &n2, i_bc);

		}

	if (um_vcparall(v0,v1)) {  /* if a and b are parallel then */
										/*    check for positioning */
		um_nptln(ploc[1],a->spt,v0,ploc_b_on_a);
		if (um_ptinseg(a->spt,ploc[0],i_ac))
			error = ! um_ptinseg(a->spt, ploc_b_on_a, i_ac);
		else
			error = ! um_ptinseg(a->ept, ploc_b_on_a, i_ac);
		if (error)
			uu_uerror0(UM_MODEL, ERROR_BASE+4); /* ambigous picks */
		}


	if (! error) {
		if ((n1 == 0) || (n2 == 0)) {
			uu_uerror0(UM_MODEL,ERROR_BASE+4);
			error = UU_TRUE;
			}
		else {
			um_vcmnvc(ploc[0], i_ac, r); um_unitvc(r,r);
			um_vcmnvc(ploc[1], i_bc, s); um_unitvc(s,s);
	
			error = um_cir_2v2p(i_ac, r, i_bc, s, circle);
			}
		}
	
	if ((! error) && option) {
		line = (struct UM_line_rec *)&e[0];
		um_nptln(circle->center, line->spt, uv0, ploc[0]);
		line = (struct UM_line_rec *)&e[1];
		um_nptln(circle->center, line->spt, uv1, ploc[1]);
		line = (struct UM_line_rec *)&e[2];
		um_nptln(circle->center, line->spt, uv2, ploc[2]);

		/* define normal as cross of [p0,p1] and [p0,p2] */
		um_vcmnvc(ploc[1],ploc[0],r);
		um_vcmnvc(ploc[2],ploc[0],s);
		um_cross(r,s,circle->nvec);
		um_unitvc(circle->nvec,circle->nvec);

		um_vcmnvc(ploc[0],circle->center, v0);
		um_vcmnvc(ploc[2],circle->center, v2);
		um_unitvc(v0, circle->svec);
		um_cross(v0,v2,v1);
		sign = um_dot(v1,circle->nvec);
		circle->dang = um_angle(v0,v2);
		if (sign < 0) circle->dang = UM_TWOPI - circle->dang;
		}

	uu_dexit;

	return error;
	}


/**/
/*********************************************************************
**    E_FUNCTION     : um_cir_2v2p(p1,v1,p2,v2,circle)
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**				circle		circle entity with fields are filled in
**    RETURNS      : status		UU_TRUE if error occured
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL um_cir_2v2p(p1,v1,p2,v2,circle)
	UU_REAL p1[3];
	UU_REAL v1[3];
	UU_REAL p2[3];
	UU_REAL v2[3];
	struct UM_circle_rec *circle;
	{
	UU_REAL v12[3],v21[3];
	UU_REAL b1[3],b2[3];
	UU_REAL radius;
	int n;

	/* algorithm
	** implied vector from p1,p2 
	** take bisector vectors and intersect them for the center
	** from center drop perpendicular to a vector and get radius
	*/

	um_vcmnvc(p1,p2,v21); um_unitvc(v21,v21);
	um_vcmnvc(p2,p1,v12); um_unitvc(v12,v12);

	um_bisect(v12,v1,p1,b1); um_unitvc(b1,b1);
	um_bisect(v2,v21,p2,b2); um_unitvc(b2,b2);

	um_ilnln(p1, b1, p2, b2, &n, circle->center);
	vector("center",circle->center);

	circle->dang = UM_TWOPI;
	um_cross(b1,b2, circle->nvec);
	um_unitvc(circle->nvec, circle->nvec);

	um_nptln(circle->center, p1, v1, b1);
	circle->radius = um_dcccc(circle->center, b1);
	if (circle->radius < UM_FUZZ)
		uu_uerror0(UM_MODEL, 19);

	sprintf(UM_sbuf,"radius = %f",circle->radius);
	um_pscroll(UM_sbuf);

	um_cir_svec(circle);

	return (circle->radius < UM_FUZZ);
	}

/**/
/*********************************************************************
**    E_FUNCTION     : um_bisect(v1,v2,p,b)
**						Return a vector that bisects the 2 given vectors 
**						around the point given.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_bisect(v1,v2,p,b)
	UU_REAL v1[3];
	UU_REAL v2[3];
	UU_REAL p[3];
	UU_REAL b[3];
	{
	UU_REAL rotvec[3];
	UU_REAL to_org[3];
	UM_angle angle;
	UU_REAL rottf[4][3];
	UU_REAL temptf[4][3];
	UU_REAL u1[3],u2[3];

	uu_denter( UU_MTRC,(us,"um_bisect()"));
	vector("bisect v1 =",v1);
	vector("  with v2 =",v2);
	vector("  about p =",p);
	/* 
	** function is to rotate v1 half way to v2
	** get angle between v1 and v2
	** rotate v1 by angle / 2 towards v2 around p
	** new vector in b
	*/
	um_unitvc(v1, u1);
	um_unitvc(v2, u2);

	um_cross(u1,u2,rotvec);
	angle = um_angle2p(u1,u2,rotvec) / 2.0;

	um_rottf(rotvec,angle,rottf);
	um_vctmsc(p,(UU_REAL) -1.0,to_org);
	um_disptf(to_org,temptf);
	um_tftmtf(temptf, rottf,rottf);
	um_disptf(p, temptf);
	um_tftmtf(rottf, temptf, rottf);   

	um_vctmtf(v1, rottf, b);
	vector("bisect vector =",b);

	uu_dexit;
	}

/**/

static vector(s,v)
	char *s;
	UU_REAL v[3];
	{
	sprintf(UM_sbuf, "%s <%f,%f,%f>",s,v[0],v[1],v[2]);
	um_pscroll(UM_sbuf);
	}
