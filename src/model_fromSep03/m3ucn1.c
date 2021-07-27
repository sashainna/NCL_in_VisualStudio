/*********************************************************************
**    NAME         :  m3ucn1.c
**       CONTAINS: User interface routines to create/modify an ellipse.
**			umu_c4_ellipse_popupmenu()
**			umu_c4_elfcpt()
**			umu_c4_elptax()
**			umu_c4_projcirc()
**			umu_c4_elc2la()
**			umu_c4_elbox()
**			umu_c4_elpart()
**			umu_m4_ellaxes(key)
**			umu_m4_curv()
**			umu_m4_intpoint()
**			umu_m4_close(key)	
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m3ucn1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:57
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dselmask.h"
#include "mfort.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "modef.h"
#include "mcrv.h"
#include "mdpick.h"
#include "mdcpln.h"
#include "mdeval.h"
#include "mdebug.h"
#include "go.h"
#include "mdclass.h"
#include "mdgenent.h"
#include "mattr.h"
#include "mpopmenu.h"
#include "nclvx.h"
 		
void umu_c4_elc2la();
void umu_c4_elfcpt();
void umu_c4_elptax();
void umu_c4_elpart();
void umu_c4_projcirc();
void umu_c4_elbox();

/*********************************************************************
**    E_FUNCTION     : umu_c4_ellipse_popupmenu()
**       Put up a popup menu for all of the various ellipse
**			construction techniques, and let the user choose one.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_c4_ellipse_popupmenu()

	{
	int status;
	int option;

	uu_denter(UU_MTRC,(us,"umu_c4_ellipse_popupmenu()"));

	status = um_popupmenu(UM_CREATE_ELLIPSE, &option);
	if (option == 1)
 		umu_c4_elc2la();
	else if (option == 2)
		umu_c4_elfcpt();
	else if (option == 3)
		umu_c4_elptax();
	else if (option == 4)
		umu_c4_elpart();
	else if (option == 5)
		umu_c4_projcirc();
	else if (option == 6)
		umu_c4_elbox();

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  umu_c4_elfcpt()
**       Create ellipse by foci and point on curve
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : creates and displays new entity
**    WARNINGS     : none
*********************************************************************/

void umu_c4_elfcpt()
	{
	struct UM_conic_rec e;
	int numint;
/**	UM_coord	foci[2]; **/
    UD_NDCLOCREC foci0,foci1;

/**	UM_coord	point; **/				/* point on ellipse			*/
    UD_NDCLOCREC point;

	UU_REAL	focus_segment[3];	/* difference of foci		*/
	UU_REAL	focal_length;		/* distance between foci	*/
	UM_vector	vector;			/* used as holding place for temp vector.	*/
	UU_REAL	sum;					/* distance focus-point-focus	*/
	UU_REAL	semi_minor;			/* one of the invariants	*/
	int		ok = UU_TRUE;

	uu_denter(UU_MTRC,(us,"umu_c4_elfcpt()"));
	ur_setup_data(UM_CONIC_REL, &e, sizeof(struct UM_conic_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;

	do 	/* Get focal points	*/
		{
		ud_ldas(UD_DASCART, /* two focus points */UM_MODEL, 197, &foci0, 1,
			&numint, UD_NODEFAULT);
		if (numint < 1)
			goto Done;
		ud_ldas(UD_DASCART, /* two focus points */UM_MODEL, 197, &foci1, 1,
			&numint, UD_NODEFAULT);
		if (numint < 1)
			goto Done;
		
		if (um_cceqcc(&foci0, &foci1))
			{
			uu_uerror0(/* foci are too close	*/UM_MODEL, 182);
			ok = UU_FALSE;
			}
		else
			{

			/*
			 * Calculate focal length, center, and X-axis
			 */
			um_vcmnvc(&foci1, &foci0, focus_segment);
			focal_length = um_mag(focus_segment);
				/* x-axis	*/
			um_unitvc(focus_segment, e.tfmat[0]);
				/* center	*/
			um_vcplvc(&foci0, &foci1, vector);
			um_vctmsc(vector, (UU_REAL) 0.5, e.tfmat[3]);

			ok = UU_TRUE;
			}

		} while (!ok);

	do		/* get point on ellipse	*/
		{
		ud_ldas(UD_DASCART, /* Point on ellipse */UM_MODEL, 198, &point, 1,
					&numint, UD_NODEFAULT);
		if (numint < 1)
			goto Done;

		/*
		 * Calulate sum of distances from point to foci	
		 */
		um_vcmnvc(&point, &foci1, vector);
		sum = um_mag(vector);
		um_vcmnvc(&point, &foci0, vector);
		sum = sum +  um_mag(vector);
		/** Value in 'vector' is used below **/

		semi_minor = sqrt(sum*sum - focal_length*focal_length)/2.0;
		
		if ( semi_minor < UM_FUZZ)
			{
			uu_uerror0(/* ellipse would be too narrow	*/UM_MODEL, 177);
			ok = UU_FALSE;
			}
		else
			ok = UU_TRUE;

		}	while (!ok);

	/*
	 * Calculate Rest of transformation.
	 *
	 * recall that vector is (point - foci[0])
	 */
		/* normal	*/
	um_cross(e.tfmat[0], vector, vector);
	/*
	 * if point given on ellipse is very close to the major
	 * axis, normal is not determinable.
	 * Try, first, to use the construction plane normal,
	 * if that makes sense, otherwise, bag it.  Could
	 * ask the human for another point in the ellipse plane.
	 */
	 if (um_mag(vector) < UM_FUZZ)
	 	{
			/* is major axis in construction plane? */
		um_unitvc(e.tfmat[0], vector);
		if (um_vcperp(vector, UM_cpln.zaxis))
			{
			/* use constr. plane normal	*/
			um_vctovc(UM_cpln.zaxis, e.tfmat[2]);
			}
		else
			{
			uu_uerror0(/* point is too close to axis */UM_MODEL, 183);
			goto Done;
			}
	 	}
	 else
	 	{
		um_unitvc(vector, e.tfmat[2]);
	 	}

		/* y-axis	*/
	um_cross(e.tfmat[2], e.tfmat[0], e.tfmat[1]);

	/* full ellipse	*/
	e.t0 = -2;
	e.t1 = 2;

	e.invariants[0] = sum/2.0;	/* semi-major	*/
	e.invariants[1] = semi_minor;
	e.type = UM_ELLIPSE;

	um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
	uc_display(&e);

Done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  umu_c4_elptax()
**       creates ellipse by center point, major axis endpoint,
**			and length of semi-minor axis (user's choices projected
**			to construction plane.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : creates and displays new entity
**    WARNINGS     : none
*********************************************************************/
void umu_c4_elptax()

	{
	struct UM_conic_rec e;
	int	ok;
	int numint;
/**	UM_coord	center;
	UM_coord	vertex; **/
    UD_NDCLOCREC center, vertex;

	UM_vector	vector;		/* temp. space	*/
	UU_REAL	semi_minor;		/* one of the invariants	*/

	uu_denter(UU_MTRC,(us,"umu_c4_elptax()"));
	ur_setup_data(UM_CONIC_REL, &e, sizeof(struct UM_conic_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;

	ok = UU_FALSE;

	do 	/* Get center	*/
		{
		ud_ldas(UD_DASCART, /* center of ellipse */UM_MODEL, 199, &center, 1,
			&numint, UD_NODEFAULT);
		if (numint < 1)
			goto Done;
		/** WARNING: used to project to const. plane	**/
		/** WARNING um_projcpln(&center, &center); **/

		ud_ldas(UD_DASCART, /* vertex of conic */UM_MODEL, 202, &vertex, 1,
			&numint, UD_NODEFAULT);
		if (numint < 1)
			goto Done;
		/** WARNING um_projcpln(&vertex, &vertex); **/
		if (um_cceqcc(&center, &vertex))
			{
			uu_uerror0(/* center is too close to vertex */UM_MODEL, 180);
			continue;
			}

		/*
		 *calculate semi-major axis.  Don't let semi-minor exceed it.
		 */

		um_vcmnvc(&vertex, &center, vector);
		e.invariants[0] = um_mag(vector);			/* semi-major	*/
		um_unitvc(vector, e.tfmat[0]);	/* x-axis	*/

		ud_ldas(UD_DASDISTANCE, /* Length of semi-minor */UM_MODEL, 200,
			&semi_minor, 1, &numint, UD_NODEFAULT);
		if (numint < 1)
			goto Done;
		if (semi_minor < UM_FUZZ)
			{
			uu_uerror0(/* ellipse would be too narrow	*/UM_MODEL, 177);
			continue;
			}
		if (semi_minor > e.invariants[0])	/* exceeds semi-major	*/
			{
			uu_uerror0(/* axis length must be shorter */UM_MODEL, 178);
			continue;
			}

		ok = UU_TRUE;

		} while (!ok);

	/*
	 * fill up rest of entity values
	 */

	e.invariants[1] = semi_minor;
	um_vctovc(UM_cpln.zaxis, e.tfmat[2]);
	um_cross(UM_cpln.zaxis, e.tfmat[0], e.tfmat[1]);
	um_vctovc(&center, e.tfmat[3]);

	e.type = UM_ELLIPSE;
	e.t0 = -2;
	e.t1 =  2;

	um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
	uc_display(&e);

Done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  umu_c4_projcirc()
**       creates ellipse by projecting picked circle to specified plane
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : creates and displays new entity
**    WARNINGS     : none
*********************************************************************/
void umu_c4_projcirc()

	{
	UM_PLOCREC	pick;
	int	numint;
	int	rel_num;
	int	error;
	int	status;		/* returned from dl_pldas: UU_TRUE if OK	*/
	UM_vector plane_normal;
/**	UM_coord	plane_point; **/
    UD_NDCLOCREC plane_point;

	UM_transf	tfmat;

	struct UM_conic_rec ellipse;
	struct UM_circle_rec circle;

	uu_denter(UU_MTRC,(us,"umu_c4_projcirc()"));
	ur_setup_data(UM_CONIC_REL, &ellipse, sizeof(struct UM_conic_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (ellipse.label, "");
	ellipse.subscr = 0;

		/*	ask user to pick circle	*/
	status = um_dl_pldas(UD_DASPCKLOC,/* pick circle */UM_MODEL,159,&pick,
					1, &numint, 1);
	if (status || numint < 1)
		{
		um_p_ary(UM_PINT, "bad status?", 1, &status);
		um_p_ary(UM_PINT, "bad numint?", 1, &numint);
		goto Done;
		}

	circle.key = um_get_pickkey(&pick.pent, 1);
	ur_retrieve_data_relnum(circle.key, &rel_num);
	if (rel_num != UM_CIRCLE_REL)
		{
		uu_uerror0(/* you must pick a circle	*/UM_MODEL, 61);
		goto Done;
		}

	um_get_all_geom( &circle, sizeof(struct UM_circle_rec));
	um_get_transformation (circle.key, tfmat);
		/* FIX: no error checking can be done here	*/

	/* Get Plane	*/
	ud_ldas(UD_DASVEC, /* normal to (projection) plane */UM_MODEL, 182,
			plane_normal, 1, &numint, UD_NODEFAULT);
	if (numint < 1)
		goto Done;

	plane_point.cord[0] = 0;	/* default	*/
	plane_point.cord[1] = 0;
	plane_point.cord[2] = 0;
	ud_ldas(UD_DASCART, /* point on (projection) plane */UM_MODEL, 181,
		&plane_point, 1, &numint, UD_DEFAULT);
	if (numint < 1)
		goto Done;

	um_create_geom(&ellipse, UM_DEFAULT_TF, UM_CURRENT_ATTR);
	um_projcirc(&circle, tfmat, plane_normal, &plane_point,  &ellipse, &error);
	switch(error)
		{
	 case	0:
		um_update_geom(&ellipse, UM_DEFAULT_TF);
		uc_display(&ellipse);
		break;	/* no error	*/
	 case 1:
	ur_delete_all(ellipse.key);
		uu_uerror0(/* ellipse would be too narrow	*/UM_MODEL, 177);
		break;
	 default:
   	ur_delete_all(ellipse.key);
		uu_uerror1(/* error projecting circle: %d	*/UM_MODEL, 185, error);
		break;
		}

Done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  umu_c4_elc2la()
**			Prompt the user for the data defining an ellipse by its
**			center point, lengths for the major and minor axes, and the
**			angle that the major axis makes with the x-axis of the
**			construction plane. The ellipse will lie in a plane parallel
**			to the x-y construction plane and through the center.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : creates and displays new entity
**    WARNINGS     : none
*********************************************************************/
void umu_c4_elc2la()

	{
	struct UM_conic_rec e;
	int	ok;
	int numint;
/**	UM_coord center; **/
    UD_NDCLOCREC center;

	UM_length semi_minor;		/* one of the invariants	*/
	UM_length semi_major;		/* one of the invariants	*/
	UM_length temp;
	UM_angle rotangle;
	UM_transf tfmat;

	uu_denter(UU_MTRC,(us,"umu_c4_c2la()"));
	ur_setup_data(UM_CONIC_REL, &e, sizeof(struct UM_conic_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;

	ok = UU_FALSE;

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /* center of ellipse */UM_MODEL, 199, &center, 1,
			&numint, UD_NODEFAULT);
		if (numint < 1) goto done;

		ud_ldas(UD_DASDISTANCE, /* length of semi_major axis */UM_MODEL, 254,
			&semi_major, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto repeat;

		ud_ldas(UD_DASDISTANCE, /* length of semi_minor axis */UM_MODEL, 200,
			&semi_minor, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto repeat;

		ud_ldas(UD_DASANGLE, /* angle of major axis */UM_MODEL, 255,
			&rotangle, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto repeat;

		if ((semi_minor < UM_FUZZ) || (semi_major < UM_FUZZ))
			{
			uu_uerror0(/* ellipse would be too narrow	*/UM_MODEL, 177);
			goto repeat;
			}

		if (semi_minor > semi_major)	/* exceeds semi-major	*/
			{
			temp = semi_minor;
			semi_minor = semi_major;
			semi_major = temp;
			}

		e.invariants[0] = semi_major;
		e.invariants[1] = semi_minor;

		um_rottf(UM_cpln.zaxis, rotangle, tfmat);
		um_vctmtf(UM_cpln.xaxis, tfmat, e.tfmat[0]);
		um_cross(UM_cpln.zaxis, e.tfmat[0], e.tfmat[1]);
		um_vctovc(UM_cpln.zaxis, e.tfmat[2]);
		um_vctovc(&center, e.tfmat[3]);
	
		e.type = UM_ELLIPSE;
		e.t0 = -2;
		e.t1 =  2;
	
		um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
		uc_display(&e);

repeat:;
	}

done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  umu_c4_elbox()
**		Create a rectangle bound for an ellipse by the following method:
**				1. Prompt the user for two points defining a line
**				2. The next point will determine the direction
**					and distance of the next leg of the polygon which
**					starts at the end of the previous leg and is at a
**					right angle
**				3. Fit the ellipse into this rectangle by centerpoint,
**					axes length and axes endpoint
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_c4_elbox()

	{
/**	UM_coord pt1, pt2, pt3; **/
	UD_NDCLOCREC pt1, pt2, pt3;

	int numint;
	int	i;
	UM_coord	nvx;
	UM_vector lnvc;
	Gwpoint3	tempseg[2];
	struct UM_conic_rec ec;
   	UM_coord	center;

	UM_vector major_axis;
	UM_vector minor_axis;
	UU_REAL	semi_minor, semi_major;
	UU_REAL tempd1, tempd2;

	uu_denter(UU_MTRC,(us,"umu_c4_elbox()"));

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, UM_MODEL, 309, &pt1, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto done;

		ud_ldas(UD_DASCART, UM_MODEL, 309, &pt2, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto repeat;

		/** a little visual feedback, if you please	**/
		tempseg[0].x = pt1.cord[0];
		tempseg[0].y = pt1.cord[1];
		tempseg[0].z = pt1.cord[2];
		tempseg[1].x = pt2.cord[0];
		tempseg[1].y = pt2.cord[1];
		tempseg[1].z = pt2.cord[2];
		gpolyline3( 2, tempseg);	/* drawn in no segment	*/

		ud_ldas(UD_DASCART, UM_MODEL, 309, &pt3, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto repeat;

		/* modify point 3 to maintain the right angle constraint */
		/* third point forms right angle: */
		um_vcmnvc(&pt1, &pt2, lnvc);
		um_unitvc(lnvc, lnvc);
		um_nptln(&pt3, &pt2, lnvc, nvx);
		um_vcmnvc(&pt3, nvx, lnvc);
		um_vcplvc(&pt2, lnvc, &pt3);

		/** a little visual feedback, if you please	**/
		tempseg[0].x = pt2.cord[0];
		tempseg[0].y = pt2.cord[1];
		tempseg[0].z = pt2.cord[2];
		tempseg[1].x = pt3.cord[0];
		tempseg[1].y = pt3.cord[1];
		tempseg[1].z = pt3.cord[2];
		gpolyline3( 2, tempseg);	/* drawn in no segment	*/
	
		/* lines define ellipse; collect info for um_create_geom call */
		ur_setup_data(UM_CONIC_REL, &ec, sizeof(struct UM_conic_rec));
		/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
		strcpy (ec.label, "");
		ec.subscr = 0;
		
		/* there is a routine to compute line length: um_dcccc */
		tempd1 = um_dcccc(&pt1, &pt2);
		tempd2 = um_dcccc(&pt2, &pt3);
	
		/* for the greater length, use as "semi-major", for the lesser,
		use the midpoint of the shorter leg for "vertex" */
		if (tempd1>tempd2)
			{
			semi_minor = tempd2/2;
			semi_major = tempd1/2;
			for (i=0; i<3; i++) 
				minor_axis[i] = (pt3.cord[i]-pt2.cord[i])/2;
			for (i=0; i<3; i++) 
				major_axis[i] = (pt1.cord[i]-pt2.cord[i])/2;
			}
		else
			{
			semi_minor = tempd1/2;
			semi_major = tempd2/2;
			for (i=0; i<3; i++) 
				minor_axis[i] = (pt1.cord[i]-pt2.cord[i])/2;
			for (i=0; i<3; i++) 
				major_axis[i] = (pt3.cord[i]-pt2.cord[i])/2;
			}
	
		/* compute the ellipse center point, into "center": (p0+p2)/2 */
		for (i=0; i<3; i++) 
			center[i] = (pt1.cord[i]+pt3.cord[i])/2;
		
		ec.type = UM_ELLIPSE;
		ec.invariants[0] = semi_major;
		ec.invariants[1] = semi_minor;
		um_unitvc(major_axis, ec.tfmat[0]);
		um_unitvc(minor_axis, ec.tfmat[1]);
		um_cross(ec.tfmat[0], ec.tfmat[1], ec.tfmat[2]);
		um_unitvc(ec.tfmat[2], ec.tfmat[2]);
		um_vctovc(center, ec.tfmat[3]);
		ec.t0 = -2;
		ec.t1 =  2;

		um_create_geom(&ec, UM_DEFAULT_TF, UM_CURRENT_ATTR);
		uc_display(&ec);

repeat:;
		}

done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  umu_c4_elpart()
**			Create a partial ellipse.
**			Prompt the user for the data defining an ellipse by its
**			center point, lengths for the major and minor axes, and the
**			angle that the major axis makes with the x-axis of the
**			construction plane. The ellipse will lie in a plane parallel
**			to the x-y construction plane and through the center.
**			Prompt for the beginning and ending angles (from the
**			major axis) to define the part of the ellipse to
**			display
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : creates and displays new entity
**    WARNINGS     : none
*********************************************************************/
void umu_c4_elpart()

	{
	struct UM_conic_rec e;
	int	ok;
	int numint;
/**	UM_coord center; **/
    UD_NDCLOCREC center;

	UM_length semi_minor;		/* one of the invariants	*/
	UM_length semi_major;		/* one of the invariants	*/
	UM_length temp;
	UM_angle rotangle;
	UM_transf tfmat;
	UM_angle  sangle,eangle;	/* start and end angles */
	UM_vector svec,evec;		/* start and end vectors, conic space */
	UM_coord  spt[2],ept[2];	/* intersection of svec and evec
					   with ellipse, conic space */
	UM_vector svecm,evecm;		/* start and end vectors, model space */
	UM_coord  sptm[2],eptm[2];	/* intersection of svec and evec
					   with ellipse, model space */
	UM_transf cninv;
	int nint,mult;
	int si,ei;			/* indexes for start and end points */

	uu_denter(UU_MTRC,(us,"umu_c4_elpart()"));
	ur_setup_data(UM_CONIC_REL, &e, sizeof(struct UM_conic_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;

	ok = UU_FALSE;

	while (UU_TRUE)
		{
		ud_ldas(UD_DASCART, /* center of ellipse */UM_MODEL, 199, &center, 1,
			&numint, UD_NODEFAULT);
		if (numint < 1) goto done;

		ud_ldas(UD_DASDISTANCE, /* length of semi_major axis */UM_MODEL, 254,
			&semi_major, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto repeat;

		ud_ldas(UD_DASDISTANCE, /* length of semi_minor axis */UM_MODEL, 200,
			&semi_minor, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto repeat;

		rotangle = 0.0;
		ud_ldas(UD_DASANGLE, /* angle of major axis */UM_MODEL, 255,
			&rotangle, 1, &numint, UD_DEFAULT);
		if (numint < 1) goto repeat;

		if ((semi_minor < UM_FUZZ) || (semi_major < UM_FUZZ))
			{
			uu_uerror0(/* ellipse would be too narrow	*/UM_MODEL, 177);
			goto repeat;
			}

		if (semi_minor > semi_major)	/* exceeds semi-major	*/
			{
			temp = semi_minor;
			semi_minor = semi_major;
			semi_major = temp;
			}

		e.invariants[0] = semi_major;
		e.invariants[1] = semi_minor;

		um_rottf(UM_cpln.zaxis, rotangle, tfmat);
		um_vctmtf(UM_cpln.xaxis, tfmat, e.tfmat[0]);
		um_cross(UM_cpln.zaxis, e.tfmat[0], e.tfmat[1]);
		um_vctovc(UM_cpln.zaxis, e.tfmat[2]);
		um_vctovc(&center, e.tfmat[3]);
	
		/* create full ellipse */
		e.type = UM_ELLIPSE;
		e.t0 = -2;
		e.t1 =  2;

		/* get start and end angles for partial ellipse */
		sangle = 0;
		ud_ldas(UD_DASANGLE, UM_MODEL, 278, &sangle, 
			1, &numint, UD_DEFAULT);

		eangle = UM_TWOPI;
		ud_ldas(UD_DASANGLE, UM_MODEL, 279, &eangle, 
			1, &numint, UD_DEFAULT);

		/* find inverse of local to model space transformation */
		um_inverttf(e.tfmat,cninv);

		/* find intersection of start angle with ellipse */
		svec[0] = cos(sangle);
		svec[1] = sin(sangle);
		svec[2] = 0.0;
		um_vctmtf(svec,e.tfmat,svecm);
		um_ilnconic(&center,svecm,&e,&nint,sptm,&mult); 
		um_cctmtf(sptm[0],cninv,spt[0]);
		um_cctmtf(sptm[1],cninv,spt[1]);

		/* if more than one point of intersection, check quadrant
		   to select correct point */
		if (nint == 2) 
			if (fabs(svec[0]) < UM_FUZZ) 
				{
				if (svec[1] < 0)
					{
					if (spt[0][1] < 0)
						si = 0;
					else
						si = 1;
					}
				else if (svec[1] > 0)
					{
					if (spt[0][1] > 0)
						si = 0;
					else
						si = 1;
					}
				}
			else if (svec[0] < 0)
				{
				if (spt[0][0] < 0)
					si = 0;
				else
					si = 1;
				}
			else if (svec[0] > 0)
				{
				if (spt[0][0] > 0)
					si = 0;
				else
					si = 1;
				}

		/* find intersection of end angle with ellipse */
		evec[0] = cos(eangle);
		evec[1] = sin(eangle);
		evec[2] = 0.0;
		um_vctmtf(evec,e.tfmat,evecm);
		um_ilnconic(&center,evecm,&e,&nint,eptm,&mult); 
		um_cctmtf(eptm[0],cninv,ept[0]);
		um_cctmtf(eptm[1],cninv,ept[1]);

		/* if more than one point of intersection, check quadrant
		   to select correct point */
		if (nint == 2)
			if (fabs(evec[0]) < UM_FUZZ) 
				{
				if (evec[1] < 0)
					{
					if (ept[0][1] < 0)
						ei = 0;
					else
						ei = 1;
					}
				else 
					{
					if (ept[0][1] > 0)
						ei = 0;
					else
						ei = 1;
					}
				}
			else if (evec[0] < 0)
				{
				if (ept[0][0] < 0)
					ei = 0;
				else
					ei = 1;
				}
			else if (evec[0] > 0)
				{
				if (ept[0][0] > 0)
					ei = 0;
				else
					ei = 1;
				}

		um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
		/* set start and end parameters */
		um_cn4_endpoints(&e,sptm[si],eptm[ei],UM_idmat);
	
		/* display partial ellipse */
		um_update_geom(&e, UM_DEFAULT_TF);
		uc_display(&e);

repeat:;
	}

done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  umu_m4_ellaxes(key)
**       modify ellipse axis lengths
**    PARAMETERS   
**       INPUT  : 
**          key							UU_NULL => prompt user to pick and
**												ellipse; otherwise, this is key of
**												ellipse
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_m4_ellaxes(key)
	UU_KEY_ID key;

	{
	UM_PLOCREC	pick;						/* the pick id and location */
	int			numint;					/* the number user interactions	*/
	struct		UM_conic_rec	e;		/* ellipse	*/
	UU_REAL		new_axes[2];			/* input values for axis lengths	*/
	char label[NCL_MAX_LABEL];		/*store label to reset */
	int subscr;			/*store subscr */
	UU_KEY_ID okey;		/*store key */

	UU_LOGICAL	umi_cn4_closed();

	uu_denter(UU_MTRC,(us,"umu_m4_ellaxes(key=%x)",key));

	if (key != 0)
		{
		e.key = key;
		}
	else
		{
		ud_lgeo(UU_TRUE, UD_conics);
		um_dl_pldas( UD_DASPCKLOC, /* Pick an ellipse*/UM_MODEL,
				208, &pick, 1, &numint, 1);
		if (numint < 1) goto Done;
		e.key = um_get_pickkey(&pick.pent, 1);
		}

	um_get_all_geom(&e, sizeof(struct UM_conic_rec));

	if  (e.type != UM_ELLIPSE)
		{
		uu_uerror0(UM_MODEL, 186); /* Ellipse not picked	*/
		goto Done;
		}

	/* save off label, key, subscr for later */
	strncpy(label,e.label, NCL_MAX_LABEL);
	subscr = e.subscr;
	okey = e.key;

	/* -- semi-major --	*/
	new_axes[0] = 2 * e.invariants[0];
	ud_ldas(UD_DASDISTANCE, /* Total length of major axis */UM_MODEL, 209,
		new_axes + 0, 1, &numint, UD_DEFAULT);
	if (numint < 1) goto Done;

	if (new_axes[0] < 2 * UM_FUZZ)
		{
		uu_uerror0(/* axis must be longer	*/UM_MODEL, 177);
		goto Done;
		}

	/* -- semi-minor --	*/
	new_axes[1] = 2 * e.invariants[1];
	ud_ldas(UD_DASDISTANCE, /* Total length of minor axis */UM_MODEL, 210,
		new_axes + 1, 1, &numint, UD_DEFAULT);
	if (numint < 1) goto Done;

	if (new_axes[1] < 2 * UM_FUZZ)
		{
		uu_uerror0(/* axis must be longer	*/UM_MODEL, 177);
		goto Done;
		}
	if (new_axes[1] > new_axes[0])
		{
		UU_REAL	tempvc[3];

		e.invariants[0] = .5 * new_axes[1];	/* switch	*/
		e.invariants[1] = .5 * new_axes[0];
		um_vctmsc(e.tfmat[0], (UU_REAL) -1.0, tempvc);
		um_vctovc(e.tfmat[1], e.tfmat[0]);
		um_vctovc(tempvc, e.tfmat[1]);

		/* 	skip for complete (closed) ellipse	*/
		if (!umi_cn4_closed(&e))
			{	/* Slide parameter around	*/
			e.t0 -= 1.0;
			if (e.t0 < -2.0)
				e.t0 += 4.-0;
			e.t1 -= 1.0;
			if (e.t1 < -2.0)
				e.t1 += 4.-0;
			}
		}
	else
		{
		e.invariants[0] = .5 * new_axes[0];
		e.invariants[1] = .5 * new_axes[1];
		}

	/*reset label and subscr */
	strncpy(e.label,label, NCL_MAX_LABEL);
	e.subscr = subscr;
	e.key = okey;

	um_update_geom(&e, UM_DEFAULT_TF);
	uc_display(&e);

Done:
	ud_lgeo(UU_FALSE, UD_circle);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  umu_m4_curv()
**       prescribe curvature at vertices of conics
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

void umu_m4_curv()
	{
	UM_PLOCREC	pick;						/* the pick id and location */
	int			numint;					/* the number user interactions	*/
	struct		UM_conic_rec	e;	
	UU_REAL		curvature;				/* user prescribes				*/

	uu_denter(UU_MTRC,(us,"umu_m4_curv()"));

	do
		{
		um_dl_pldas( UD_DASPCKLOC, /* Pick a conic*/UM_MODEL,
								211, &pick, 1, &numint, 1);
		if (numint < 1)
			goto Done;

		e.key = um_get_pickkey(&pick.pent, 1);
		ur_retrieve_data_relnum(e.key, &e.rel_num);
		if (e.rel_num !=  UM_CONIC_REL)
			uu_uerror0(UM_MODEL, 189 /* conic not picked	*/);

		}	while (e.rel_num != UM_CONIC_REL);

	um_get_all_geom(&e, sizeof(struct UM_conic_rec));

	/*	--	set default (current) value	--	*/
	switch(e.type)
		{
	 case UM_HYPERBOLA:
		curvature = e.invariants[0]/e.invariants[1];
		break;
	 case UM_ELLIPSE:
		curvature = e.invariants[0]/(e.invariants[1] * e.invariants[1]);
		break;
	 case UM_PARABOLA:
		curvature = 2 * e.invariants[0];
		break;
	
	 default:
		goto Done;
		}

	ud_ldas(UD_DASUNITLESS, /* Curvature at vertex */UM_MODEL, 214,
		&curvature, 1, &numint, UD_DEFAULT);
	if (numint < 1)
		goto Done;

	/* don't mess with negative numbers	*/
	curvature = fabs(curvature);
	if (UM_ZILCH(curvature))
		{
		uu_uerror0(UM_MODEL,/* value too small	*/ 190);
		goto Done;
		}

	switch(e.type)
		{
	UU_REAL	tempb;

	 case	UM_HYPERBOLA:
		e.invariants[1] = e.invariants[0]/curvature;
		break;

	 case	UM_PARABOLA:
		e.invariants[0] = curvature / 2.0;
		break;

	 case UM_ELLIPSE:
		tempb = sqrt(e.invariants[0]/curvature);
		if (tempb > e.invariants[0])
			{
			uu_uerror1(UM_MODEL, /* curvature too small */ 190);
			goto Done;
			}

		e.invariants[1] = tempb;
		break;

	 default:
		goto Done;
		}

	um_update_geom(&e, UM_DEFAULT_TF);
	uc_display(&e);

Done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  umu_m4_intpoint()
**       choose a new interior point for conic arc, keeping endpoint
**			conditions fixed
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_m4_intpoint()

	{
	struct	UM_conic_rec	e;
	struct	UM_evcrvout	evout;		/* evaluator record for conic	*/
	UM_PLOCREC	pick;						/* the pick id and location	*/
	int		numint;
	int		dummy;

	UU_REAL	invtfmat[4][3];			/* inverse to internal tfmat	*/
	UU_REAL	tfmat[4][3];				/* from unibase				*/
	UU_REAL	endpoints[2][3];
	UU_REAL	tconst[2][2][3];			/*	tangent constraints		*/
/**	UU_REAL	point[3];	**/				/* point constraint			*/
    UD_NDCLOCREC point;

	char label[NCL_MAX_LABEL];		/*store label to reset */
	int subscr;			/*store subscr */
	UU_KEY_ID key;		/*store key */

	UU_LOGICAL	umi_cn4_closed();

	uu_denter(UU_MTRC,(us,"umu_m4_intpoint()"));

	do
		{
		um_dl_pldas( UD_DASPCKLOC, /* Pick a conic*/UM_MODEL,
								211, &pick, 1, &numint, 1);
		if (numint < 1)
			goto Done;

		e.key = um_get_pickkey(&pick.pent, 1);
		ur_retrieve_data_relnum(e.key, &e.rel_num);
		if (e.rel_num !=  UM_CONIC_REL)
			uu_uerror0(UM_MODEL, 189 /* conic not picked	*/);
		}	while (e.rel_num != UM_CONIC_REL);

	um_get_all_geom(&e, sizeof(struct UM_conic_rec));
	
	/* save off label, key, subscr for later */
	strncpy(label,e.label, NCL_MAX_LABEL);
	subscr = e.subscr;
	key = e.key;

	/* don't allow closed ellipses	*/
	if (umi_cn4_closed(&e))
		{
		uu_uerror0(UM_MODEL, 195 /* can't modify closed ellipse	*/);
		goto Done;
		}

	/* get new interior point	*/
	ud_ldas(UD_DASCART,  UM_MODEL, /* point on conic */ 215,
		&point, 1, &numint, UD_NODEFAULT);
	if (numint < 1)
		goto Done;

	/* project point to plane of endpoint conditions */
	um_ilnpln(&point, e.tfmat[2], e.tfmat[3], e.tfmat[2], &dummy, &point);

	/** get endpoint conditions via evaluator	**/
	/** save endpoints for later	**/
	/** map constraints from model space to definition space	**/

	um_inverttf(e.tfmat, invtfmat);
	um_cctmtf(&point, invtfmat, &point);	/* point already projected to plane
													 * of conic
												 	 */
	um_get_transformation(e.key, tfmat);
	uc_init_evcrvout(&e, &evout);

	uc_evcrv(UM_FRSTDERIV, (UU_REAL) 0.0, &e, tfmat, &evout);
	um_vctovc(evout.cp, endpoints[0]);
	um_cctmtf(evout.cp, invtfmat, tconst[0][0]);
	um_vctmtf(evout.dcdu, invtfmat, tconst[0][1]);

	uc_evcrv(UM_FRSTDERIV, (UU_REAL) 1.0, &e, tfmat, &evout);
	um_vctovc(evout.cp, endpoints[1]);
	um_cctmtf(evout.cp, invtfmat, tconst[1][0]);
	um_vctmtf(evout.dcdu, invtfmat, tconst[1][1]);

	if (um_cnstrconic(&e, 2, tconst, 1, &point))
		{
		uu_uerror0(UM_MODEL/* Can't create conic from given constraint*/, 191);
		goto Done;
		}
	/**	--	set endpoints	--	**/
	if (um_cn4_endpoints(&e, endpoints[0], endpoints[1]))
		{
		/* FIX: use uu_uerror here	*/
		um_pscroll("Error with endpoints:");
		goto Done;
		}

	/*reset label and subscr */
	strncpy(e.label,label, NCL_MAX_LABEL);
	e.subscr = subscr;
	e.key = key;

	um_update_geom(&e, UM_DEFAULT_TF);
	uc_display(&e);

Done:
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_m4_close()	
**       IF KEY is UU_NULL, prompt the user for an ellipse to close,
**			otherwise, KEY is the ellipse to close.
**    PARAMETERS   
**       INPUT  : 
**          key						UU_NULL => prompt user to pick an
**											ellipse, otherwise, this is the
**											key of an ellipse to close
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void umu_m4_close(key)	
	UU_KEY_ID key;

	{
	struct UM_conic_rec e;
	UM_PICKENT pent;
	int numint;

	uu_denter(UU_MTRC,(us,"umu_m4_close(key=%x)",key));

	if (key != 0)
		{
		e.key = key;
		}
	else
		{
		ud_lgeo(UU_TRUE, UD_conics);
		um_dl_pdas( UD_DASPICK, /* Pick a conic*/UM_MODEL,
				211, &pent, 1, &numint, 1);
		if (numint < 1) goto Done;
		e.key = um_get_pickkey(&pent, 1);
		}
	um_get_all_geom(&e, sizeof(struct UM_conic_rec));

	if (e.type != UM_ELLIPSE)
		{
		uu_uerror0(UM_MODEL, 186); /* Ellipse not picked	*/
		goto Done;
		}

	e.t0 = -2.0;
	e.t1 = 2.0;

	um_update_geom(&e, UM_DEFAULT_TF);
	uc_display(&e);

Done:
	ud_lgeo(UU_FALSE, UD_circle);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : um_redef_conic(e,tfmat)	
**			Redefines conics so it becomes a 1 inch lentgth segment for
**       open curves or full ellipse.
**    PARAMETERS   
**       INPUT  : 
**          e		- pointer to conic entity
**          tfmat	- pointer to entity transformation mx.
**       OUTPUT :  
**          e
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_redef_conic(e,tfmat)	
  struct UM_conic_rec *e;
  UM_transf *tfmat;
 {
  int typ, status;

  status = UU_SUCCESS;
  typ = e->type;
  switch (typ)
   {
    case UM_ELLIPSE:
      e->t0 = -2.0;
      e->t1 = 2.0;
      break;

    case UM_PARABOLA:
      status = um_len1_conic (e,tfmat);
      break;

    case UM_HYPERBOLA:
      status = um_len1_conic (e,tfmat);
      break;

    default:
      status = UU_FAILURE;
      break;
    }

	uu_dexit;
   return(status);
	}

/*********************************************************************
**    E_FUNCTION     : um_len1_conic(e,tfmat)	
**			Redefines conics so it becomes a 1 inch lentgth segment
**    PARAMETERS   
**       INPUT  : 
**          e		- pointer to conic entity
**          tfmat	- pointer to entity transformation mx.
**       OUTPUT :  
**          e
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_len1_conic (e,tfmat)
  struct UM_conic_rec *e;
  UM_transf *tfmat;
  {
   UM_param dp, t, s, d, r;
   UU_REAL len, slen;
/*
...estimate first step
*/
   len = um_getarclen (e,tfmat);  
   if (fabs(len-1.0) < .0005) goto done;
   d   = e->t1 - e->t0;
   t   = (d < 0.0)?  -1.: 1.0;
   d   = fabs (d);
   if (len > 5.0 || len < .2) 
     {
      d   = d / len;
      e->t1 = e->t0 + d * t;
      len = um_getarclen (e,tfmat);  
     }
/*
...Iterate end point
*/

   r   = 1.0;
   s   = (len > 1.0)? -0.5: 0.5;
   while (fabs(len-1.0) > .001)
     {
      slen  = len;
      if ((1.0-len) * s < 0.0)  { r = .5; s = -s; }
      s     = r * s;
      dp    = t * d * s;
      e->t1 = e->t1 + dp;
      len = um_getarclen (e,tfmat);  
     }
/*
...fix last iteration
*/
   s  = (1.0 - len) / (len - slen) * s;
   e->t1 = e->t1 + t*d*s;

done:
   uu_dexit;
   return(UU_SUCCESS);
  } 
   
