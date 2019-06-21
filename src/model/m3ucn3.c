

/*********************************************************************
**    NAME         :  m3ucn3.c
**       CONTAINS: user interface routines for creating parabolas
**			umu_c4_pbvxfc()
**			umu_c4_p4pts()
**			umu_c4_pvpt()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m3ucn3.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:58
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dselmask.h"
#include "class.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "modef.h"
#include "mcrv.h"
#include "mdpick.h"
#include "mdcpln.h"
#include "misect.h"
#include "mdebug.h"
#include "mattr.h"

/*********************************************************************
**    E_FUNCTION :  umu_c4_pbvxfc()
**			Prompt the user for the following information to define a
**			parabola:
**				1. the focus
**				2. the vertex
**				3. a curve to limit the extent
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c4_pbvxfc()

	{
	struct UM_conic_rec e;
	struct UC_entitydatabag c;
	int numint;
/**	UM_coord	focus;
	UM_coord	vertex; **/
    UD_NDCLOCREC focus, vertex;

	UM_vector vector;
	int i;
	int status;
	int nint;
	UM_PICKENT pick;
	UU_REAL distp;

	uu_denter(UU_MTRC,(us,"umu_c4_pbvxfc()"));

	ur_setup_data(UM_CONIC_REL, &e, sizeof(struct UM_conic_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /* enter vertex of parabola */
			UM_MODEL, 315, &vertex, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto done;

		ud_ldas(UD_DASCART, /* enter focal point of parabola */
			UM_MODEL, 314, &focus, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto repeat;

		if (um_cceqcc(&vertex, &focus))
			{
			uu_uerror0(/* focal length is too short */UM_MODEL, 184);
			goto repeat;
			}

		ud_lgeo(UU_TRUE, UD_vircurves);
		um_dl_pdas(UD_DASPICK, /* pick a curve to trim conic to */
			UM_MODEL, 316, &pick, 1, &numint, 2);
		if (numint < 1) goto repeat;

		c.key = um_get_pickkey(&pick, 2);
		status = uc_retrieve_data(&c, sizeof(c));
		if (status != UU_SUCCESS) goto repeat;

		um_vcmnvc(&focus, &vertex, vector);

		/* set fields within parabola entity	*/
		e.type = UM_PARABOLA;
		e.invariants[0] = 1.0/(um_mag(vector)*4);				/* focal length */
		e.invariants[1] = 0;											/* filler */
		um_unitvc(vector, e.tfmat[0]);							/* x axis */
		um_vctovc(UM_cpln.zaxis, e.tfmat[2]);					/* z axis */
		um_cross(UM_cpln.zaxis, e.tfmat[0], e.tfmat[1]);	/* y axis */
		um_vctovc(&vertex, e.tfmat[3]);							/* center */
		e.t0 = 50.0;
		e.t1 =  -50.0;

		/* create parabola UNIBASE */
		um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);

		/* limit conic to extend to the intersection of the curves */
		status = um_limit_conic(&e, &c);
		if (status != UU_SUCCESS) goto repeat;

		/* update trimmed entity and display */
		um_update_geom(&e, UM_DEFAULT_TF);
		uc_display(&e);

repeat:;
		}

done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  umu_c4_p4pts()
**			Create a parabola from 4 points. The points are entered 
**			sequentially as P1 to P4.  The parabola is defined
**				1. to go from P1 to P3
**				2. be tangent to line P1P2 at P1
**				3. be tangent to line P4P3 at P3
**				4. go through the mid point of the line between
**					the midpoints of the line segments from P1 and
**					P3 to the intersection of P1P2 and P3P4.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c4_p4pts()

	{
	int status;
	struct UM_conic_rec	e;

	int pindex = 1;
	UM_coord pconst[5];
	int tindex = 2;
	UU_REAL tconst[2][2][3];	
    UD_NDCLOCREC tmp;

	int numint;
	int nint;
	int i,j;

	int dim;
	UU_REAL space[2][3];
	UM_transf invtfmat;
	UM_coord spt;
	UM_coord ept;
	struct UM_line_rec line;
	struct UM_point_rec point;
	struct UM_attrdata_rec attr;

	uu_denter(UU_MTRC,(us,"umu_c4_p4pts()"));

	ur_setup_data(UM_CONIC_REL, &e, sizeof(e));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /* enter point1 of parabola */
			UM_MODEL, 310, &tmp, 1, &numint, UD_NODEFAULT);
        for(j=0; j<3; j++) tconst[0][0][j] = tmp.cord[j];
		if (numint < 1) goto done;
		um_vctovc(tconst[0][0], spt);

		ud_ldas(UD_DASCART, /* enter point2 of parabola */
			UM_MODEL, 311, &tmp, 1, &numint, UD_NODEFAULT);
        for(j=0; j<3; j++) tconst[0][1][j] = tmp.cord[j];
		if (numint < 1) goto repeat;

		ud_ldas(UD_DASCART, /* enter point3 of parabola */
			UM_MODEL, 312, &tmp, 1, &numint, UD_NODEFAULT);
        for(j=0; j<3; j++) tconst[1][0][j] = tmp.cord[j];
		if (numint < 1) goto repeat;
		um_vctovc(tconst[1][0], ept);

		ud_ldas(UD_DASCART, /* enter point4 of parabola */
			UM_MODEL, 313, &tmp, 1, &numint, UD_NODEFAULT);
        for(j=0; j<3; j++) tconst[1][1][j] = tmp.cord[j];
		if (numint < 1) goto repeat;

		/* calculate tangent vectors at point1 and point3 */
		um_vcmnvc(tconst[0][1], tconst[0][0], tconst[0][1]);
		um_unitvc(tconst[0][1], tconst[0][1]);
		um_vcmnvc(tconst[1][1], tconst[1][0], tconst[1][1]);
		um_unitvc(tconst[1][1], tconst[1][1]);

		/* check that constraints lie in plane	*/
		dim = -1;
		for (i=0; i<tindex; ++i)
			um_netspan(dim, space, 1, tconst[i], &dim, space );
	
		if (dim != 2)
			{
			um_pscroll("Conic constraints not coplanar");
			goto repeat;
			}
	
		/* find intersection of tangent lines */
		um_ilnln(tconst[0][0], tconst[0][1], tconst[1][0], tconst[1][1],
			&nint, pconst[0]);
		if (nint != 1)
			{
			um_pscroll("Conic constraints do not intersect");
			goto repeat;
			}

		/* calculate point on parabola */
		um_vcplvc(tconst[0][0], pconst[0], pconst[1]);
		um_vctmsc(pconst[1], (UU_REAL) .5, pconst[1]);
		um_vcplvc(tconst[1][0], pconst[0], pconst[2]);
		um_vctmsc(pconst[2], (UU_REAL) .5, pconst[2]);

		um_vcplvc(pconst[1], pconst[2], pconst[0]);
		um_vctmsc(pconst[0], (UU_REAL) .5, pconst[0]);
		
		/* set up conic internal transformation, using
		 	plane defined by input conditions */
		um_vctovc(space[1], e.tfmat[2]);						/* normal	*/
		um_vctovc(pconst[0], e.tfmat[3]);					/* center	*/
		um_perpvc(space[1], e.tfmat[0]);						/* x axis	*/
		um_unitvc(e.tfmat[0], e.tfmat[0]);
		um_cross(e.tfmat[2], e.tfmat[0], e.tfmat[1]);	/* y axis	*/
		um_unitvc(e.tfmat[1], e.tfmat[1]);


		um_pscroll("umu_c4_p4pts: transformation");
		um_p_ary(UM_PFLOAT, "  tfmat[0]", 3, e.tfmat[0]);
		um_p_ary(UM_PFLOAT, "  tfmat[1]", 3, e.tfmat[1]);
		um_p_ary(UM_PFLOAT, "  tfmat[2]", 3, e.tfmat[2]);
		um_p_ary(UM_PFLOAT, "  tfmat[3]", 3, e.tfmat[3]);
	
		/** map constraints from model space to definition space	**/
		um_inverttf(e.tfmat, invtfmat);
		for (i=0; i<tindex; ++i)
			{
			um_p_ary(UM_PFLOAT,"mcs point",3,tconst[i][0]);
			um_p_ary(UM_PFLOAT,"mcs tangent",3,tconst[i][1]);
			um_cctmtf(tconst[i][0], invtfmat, tconst[i][0]);
			um_vctmtf(tconst[i][1], invtfmat, tconst[i][1]);
			um_p_ary(UM_PFLOAT,"xy point",3,tconst[i][0]);
			um_p_ary(UM_PFLOAT,"xy tangent",3,tconst[i][1]);
			}
		for (i=0; i<pindex; ++i)
			{
			um_p_ary(UM_PFLOAT,"mcs point",3,pconst[i]);
			um_cctmtf(pconst[i], invtfmat, pconst[i]);
			um_p_ary(UM_PFLOAT,"xy point",3,pconst[i]);
			}
	
		/*	Build a conic from all of this	*/
		if (um_cnstrconic(&e, tindex, tconst, pindex, pconst))
			{ /* Can't create conic from given constraint */
			uu_uerror0(UM_MODEL, 191);
			goto repeat;
			}
		else if (e.type != UM_PARABOLA)
			{ /* conic is not a parabola */
			uu_uerror1(UM_MODEL, 279, "PARABOLA");
			goto repeat;
			}
		else
			{	/* get endpoints	*/
			e.t0 = 50;
			e.t1 = -50;
			um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
			status = um_cn4_endpoints(&e, spt, ept, UM_idmat);
			if (status != 0)
				{/* set defaults */
				e.t0 = 50;
				e.t1 = -50;
				}
			um_update_geom(&e, UM_DEFAULT_TF);
			uc_display(&e);
			};
repeat:;
		};

done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  umu_c4_pvpt()
**			Create a parabola from the following information:
**				1. the vertex of the parabola
**				2. a point on the parabola
**				3. the tangent vector at the point 
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c4_pvpt()

	{
	int status;
	struct UM_conic_rec	e;

	int numint;
	int nint;
	int i;

/**	UM_coord vertex;
	UM_coord tanpt; **/
    UD_NDCLOCREC vertex, tanpt;

	UM_vector tanvec;

	int dim;
	UU_REAL space[2][3];
	UM_transf invtfmat;
	UM_coord spt;
	UM_coord ept;

	UM_coord focus;
	UM_coord dirpt;
	UM_coord npt;
	UM_vector axisvec;
	UM_vector dirvec;
	UM_vector normal;
	UM_vector tempvec;
	UM_vector vertvec;
	UM_vector focalvec;
	UU_REAL d;
	UU_REAL dist_ln;
	UU_REAL dist_focus;
	UU_REAL diff;
	UM_angle beta;
	UM_angle alpha;
	UM_angle talpha;
	UM_angle delta;
	UM_transf rottf;

	uu_denter(UU_MTRC,(us,"umu_c4_pvpt()"));

	ur_setup_data(UM_CONIC_REL, &e, sizeof(e));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /* enter vertex of parabola */
			UM_MODEL, 315, &vertex, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto done;

		ud_ldas(UD_DASCART, /* enter point of parabola */
			UM_MODEL, 310, &tanpt, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto repeat;

		ud_ldas(UD_DASVEC, /* enter tangent at point */
			UM_MODEL, 317, tanvec, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto repeat;
		um_unitvc(tanvec, tanvec);

		/* calculate the vector from the tangent point to the vertex and
			make sure that the tangent vector points in the same direction */
		um_vcmnvc(&vertex, &tanpt, vertvec);
		um_unitvc(vertvec, vertvec);
		if (um_dot(tanvec, vertvec) < 0.0)
			um_vctmsc(tanvec, (UU_REAL) -1.0, tanvec);
		um_cross(tanvec, vertvec, normal);
		um_unitvc(normal, normal);
	
		sprintf(UM_sbuf,"vertex=(%g,%g,%g)\n", vertex.cord[0], vertex.cord[1], vertex.cord[2]);
		um_pscroll(UM_sbuf);
		sprintf(UM_sbuf,"tanpt=(%g,%g,%g)\n", tanpt.cord[0], tanpt.cord[1], tanpt.cord[2]);
		um_pscroll(UM_sbuf);
		sprintf(UM_sbuf,"tanvec=(%g,%g,%g)\n", tanvec[0], tanvec[1], tanvec[2]);
		um_pscroll(UM_sbuf);
		sprintf(UM_sbuf,"vertvec=(%g,%g,%g)\n", vertvec[0], vertvec[1], vertvec[2]);
		um_pscroll(UM_sbuf);
	
		/* calculate the angle between the tangent vector and vertex vector;
			the starting angle for the iteration is twice this */
		beta = um_angle2p(tanvec, vertvec, normal);
		alpha = 2.0 * beta;
		delta = beta / 2.0;
	
		while (UU_TRUE)
			{
			/* determine rotation matrices about the line through the
				tangent point in the direction of the plane normal to 
				calculate the trial axis vector and focal vector */
			um_rotlntf(&tanpt, normal, alpha, rottf);
			um_vctmtf(tanvec, rottf, focalvec);
			talpha = UM_PI - alpha;
			um_rotlntf(&tanpt, normal, talpha, rottf);
			um_vctmtf(tanvec, rottf, axisvec);
	
			sprintf(UM_sbuf,"\n\niteration\n");
			um_pscroll(UM_sbuf);
			sprintf(UM_sbuf,"alpha=%g, delta=%g, talpha=%g\n", alpha, delta, talpha);
			um_pscroll(UM_sbuf);
			sprintf(UM_sbuf,"focalvec=(%g,%g,%g)\n", focalvec[0], focalvec[1], focalvec[2]);
			um_pscroll(UM_sbuf);
			sprintf(UM_sbuf,"axisvec=(%g,%g,%g)\n", axisvec[0], axisvec[1], axisvec[2]);
			um_pscroll(UM_sbuf);
	
			/* the intersection of the line through the tangent point in
				the direction of the focal vector and the line through the
				vertex in the dirction of the axis vector is the trial
				focal point */
			um_ilnln(&tanpt, focalvec, &vertex, axisvec, &nint, focus);
			if (nint != 1)
				{
				uu_outputerr("unable to calculate focal point");
				sprintf(UM_sbuf,"unable to calculate focal point\n");
				um_pscroll(UM_sbuf);
				goto repeat;
				}
	
			sprintf(UM_sbuf,"focus=(%g,%g,%g)\n", focus[0], focus[1], focus[2]);
			um_pscroll(UM_sbuf);
	
			/* the directrix is the line perpendicular to the axis vector
				and at an equal distance from the vertex as the focal point
				(and in opposite direction)  */
			d = um_dcccc(&vertex, focus);
			um_vctmsc(axisvec, -d, tempvec);
			um_vcplvc(&vertex, tempvec, dirpt);
			um_cross(axisvec, normal, dirvec);
			um_unitvc(dirvec, dirvec);
	
			sprintf(UM_sbuf,"dirpt=(%g,%g,%g)\n", dirpt[0], dirpt[1], dirpt[2]);
			um_pscroll(UM_sbuf);
			sprintf(UM_sbuf,"dirvec=(%g,%g,%g)\n", dirvec[0], dirvec[1], dirvec[2]);
			um_pscroll(UM_sbuf);
	
			/* now, test to see if the given tangent point is an equal distance
				from the directrix and focal point */
			um_nptln(&tanpt, dirpt, dirvec, npt);
			dist_ln = um_dcccc(&tanpt, npt);
			dist_focus = um_dcccc(&tanpt, focus);
			diff = fabs(dist_ln - dist_focus);
	
			sprintf(UM_sbuf,"diff=%g, d=%g, d1=%g\n", diff, dist_ln, dist_focus);
			um_pscroll(UM_sbuf);
			/* if the distances are within tolerance, we are done */
			if (diff < 0.00001)
				{
				sprintf(UM_sbuf,"found parabola\n");
				um_pscroll(UM_sbuf);
				goto found;
				}
	
			/* otherwise, calculate a new angle and try again */
			if (dist_ln > dist_focus)
				{/* increment angle */
				if (delta < 0.0) delta = -(delta / 2.0);
				}
			else
				{/* decrement angle */
				if (delta > 0.0) delta = -(delta / 2.0);
				}
			alpha = alpha + delta;
			}
found:;
		sprintf(UM_sbuf,"focus=(%g,%g,%g)\n", focus[0], focus[1], focus[2]);
		um_pscroll(UM_sbuf);

		/* set fields within parabola entity	*/
		e.type = UM_PARABOLA;
		e.invariants[0] = 1.0/(d*4);								/* focal length */
		e.invariants[1] = 0;											/* filler */
		um_unitvc(axisvec, e.tfmat[0]);							/* x axis */
		um_unitvc(normal, e.tfmat[2]);							/* z axis */
		um_cross(normal, e.tfmat[0], e.tfmat[1]);				/* y axis */
		um_unitvc(e.tfmat[1], e.tfmat[1]);
		um_vctovc(&vertex, e.tfmat[3]);							/* center */
		e.t0 = 50.0;
		e.t1 =  -50.0;

		um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
		uc_display(&e);

repeat:;
		};

done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  um_limit_conic(conic, limit)
**			Limit the extent of the conic curve to lie between the
**			minimum and maximum intersections with the limiting curve.
**    PARAMETERS   
**       INPUT  : 
**          conic					conic curve
**       OUTPUT :  
**          limit					curve to limit conic
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_limit_conic(conic, limit)
	struct UM_conic_rec *conic;
	struct UC_entitydatabag *limit;
	
	{
	int status = UU_FAILURE;
	int err;
	int dim;
	int i;
	UU_REAL space[2][3];
	int elist[2];
	int tlist[2];
	int nint;
	UM_isect ibuff[10];
	UU_REAL tmin, tmax;
	UU_REAL distp;

	uu_denter(UU_MTRC,(us,"um_limit_conic(conic(r,k)=(%d,%x), limit(r,k)=(%d,%x))",
		conic->rel_num, conic->key, limit->rel_num, limit->key));

	/* check to see that the conic and curve picked lie in
		the same plane */
	dim = -1;
	elist[0] = (int) conic;
	elist[1] = (int) limit;
	tlist[0] = UM_DEFAULT_TF;
	tlist[1] = UM_DEFAULT_TF;
	status = um_span_elist(2, elist, tlist, &dim, space, &err);
	if (status != UU_SUCCESS)
		{
		uu_outputerr("can not calculate span");
		goto done;
		}
	if (dim != 2)
		{
		uu_outputerr("curve does not lie on plane of conic");
		goto done;
		}

	/* intersect conic with limit curve */
	status = uc_crv_intersect(limit, UM_idmat, conic, UM_idmat,
		&nint, 10, ibuff);
	if (status != UU_SUCCESS)
		{
		uu_outputerr("unable to intersectcurve with conic");
		goto done;
		}
	if (nint < 2)
		{
		uu_outputerr("curve does not intersect conic in at least two places");
		status = UU_FAILURE;
		goto done;
		}
	
	/* find min and max intersection points on conic and use to
		define extent of conic */
	for (i=0; i<nint; i++)
		{
		sprintf(UM_sbuf,"returned ibuff[%d].t0=%g",i,ibuff[i].t0);
		um_pscroll(UM_sbuf);
		umi_cc4_cctot(ibuff[i].pt, conic, conic->tfmat, &ibuff[i].t0, &distp);
		sprintf(UM_sbuf,"modified ibuff[%d].t0=%g",i,ibuff[i].t0);
		um_pscroll(UM_sbuf);
		}

	tmin = ibuff[0].t0;
	tmax = ibuff[0].t0;
	sprintf(UM_sbuf,"ibuff[0].t0=%g",ibuff[0].t0);
	um_pscroll(UM_sbuf);
	for (i=1; i<nint; i++)
		{
		sprintf(UM_sbuf,"ibuff[%d].t0=%g",i,ibuff[i].t0);
		um_pscroll(UM_sbuf);
		if (ibuff[i].t0 < tmin) tmin = ibuff[i].t0;
		if (ibuff[i].t0 > tmax) tmax = ibuff[i].t0;
		}
	conic->t0 = tmax;
	conic->t1 = tmin;
	status = UU_SUCCESS;

done:;

	uu_dexitstatus("um_limit_conic", status);
	return(status);
	}
