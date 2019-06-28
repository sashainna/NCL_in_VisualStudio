/*********************************************************************
**    NAME         :  f3circle.c 
**       CONTAINS: features for circle
**			int um_f3circle(ent, tfmat, feature_order, dploc)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       f3circle.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:44
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdcoord.h"
#include "mcrv.h"
#include "mdeval.h"
#include "modef.h"

/*********************************************************************
**    E_FUNCTION     :  int um_f3circle(ent, tfmat, feature_order, dploc)
**      calculate the desired features for a given order 
**       for the geometry type "circle" 
**    PARAMETERS   
**       INPUT  : 
**				ent				pointer to the geometric information
**				tfmat				transformation matrix.
**          feature_order	1 => center, radius, tangents
**									2 => arc length, angle, quarter points
**									3 => normal
**				dploc				picked location on circle
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_f3circle(ent, tfmat, feature_order, dploc)
	struct UM_circle_rec *ent;
	UM_transf tfmat;
	int feature_order;
	UD_PLOCREC *dploc;

	{
	struct UM_evcrvout evout; 
	UU_REAL mag_vect;
	UM_vector vect;
	UU_REAL char_height;
	UM_coord pos;
	UU_REAL arclen;
	UU_LOGICAL dflag;
	UU_LOGICAL status;

	UM_coord vporigin;
	UM_vector vpxaxis;
	UM_vector vpyaxis;
	UM_vector vpzaxis;
	UU_REAL aperture;
	
	uu_denter(UU_MTRC,(us,"um_f3circle(%d,tfmat:%x, %d)", 
						ent->key, tfmat, feature_order));
	
	status = UU_SUCCESS;

	/* transform geometry to model space */
	um_tf3_tranfcirc(ent, tfmat, UU_FALSE);

	/* initialize evaluator */
	uc_init_evcrvout(ent, &evout);
	
	/* calculate arclength */
	arclen = um_getarclen(ent, UU_NULL);

	/* get current view definition */
	um_get_current_viewplane(vporigin, vpxaxis, vpyaxis, vpzaxis, &aperture);

	mag_vect = arclen / 4.0;

	/* set feature character height */
	um_set_feature_char_height(&char_height);

	switch (feature_order)
		{
		case 1:

			/*	center point */
			status = um_feacoord (ent->center);
			if (status!=0)
				return UU_FAILURE;

			/*	start point tangent vector */
			um_ev3_circle(UM_FRSTDERIV,(UU_REAL) 0.,ent,UM_idmat,&evout);
			um_unitvc(evout.dcdu, vect);
			um_vctmsc(vect, 3.0 * char_height, vect);
			status = um_feavect (evout.cp,vect);
			if (status!=0)
				return UU_FAILURE;

			/*	if the entity is a full circle - skip endpoint features */
			if (fabs(fabs(ent->dang) - UM_TWOPI) > UM_FUZZ)
				{
				um_ev3_circle (UM_FRSTDERIV, (UU_REAL) 1., ent, UM_idmat, &evout);
				um_unitvc(evout.dcdu, vect);
				um_vctmsc(vect, 3.0 * char_height, vect);
				status = um_feavect (evout.cp, vect);
				if (status!=0)
					return UU_FAILURE;
				}

			/*	radius */
			um_vctovc (vpyaxis, pos);
			um_vctmsc (pos, 2.0 * char_height, pos);
			um_ev3_circle (UM_POINT, (UU_REAL) .33, ent, UM_idmat, &evout);
			um_vcplvc (evout.cp, pos, pos);
			dflag = UU_TRUE;
			status = um_fealength(" Radius = ", ent->radius, pos, evout.cp, dflag);
			if (status!=0)
				return UU_FAILURE;
			break;

		case 2:

			/*	arc length */
			um_vctovc (vpyaxis, pos);
			um_vctmsc (pos, 3.5 * char_height, pos);
			um_ev3_circle (UM_POINT, (UU_REAL) .33, ent, UM_idmat, &evout);
			um_vcplvc (evout.cp, pos, pos);
			dflag = UU_FALSE;
			status = um_fealength(" Arc length = ", arclen, pos, evout.cp, dflag);
			if (status!=0)
				return UU_FAILURE;

			/*	swept angle */
			if (fabs(fabs(ent->dang) - UM_TWOPI) > UM_FUZZ)
				{
				um_vctovc (vpyaxis, pos);
				um_vctmsc (pos, 5.0 * char_height, pos);
				um_vcplvc (evout.cp, pos, pos);
				dflag = UU_FALSE;
				status = um_feaangle(" Angle = ", ent->dang, pos, evout.cp, dflag);
				if (status!=0)
					return UU_FAILURE;
				}

			/*	quarter points */
			um_ev3_circle (UM_POINT, (UU_REAL) 0.0, ent, UM_idmat, &evout);
			status = um_feacoord (evout.cp);
			if (status!=0)
				return UU_FAILURE;
			um_ev3_circle (UM_POINT, (UU_REAL) 0.25, ent, UM_idmat, &evout);
			status = um_feacoord (evout.cp);
			if (status!=0)
				return UU_FAILURE;
			um_ev3_circle (UM_POINT, (UU_REAL) 0.5, ent, UM_idmat, &evout);
			status = um_feacoord (evout.cp);
			if (status!=0)
				return UU_FAILURE;
			um_ev3_circle (UM_POINT, (UU_REAL) 0.75, ent, UM_idmat, &evout);
			status = um_feacoord (evout.cp);
			if (status!=0)
				return UU_FAILURE;

			break;

		case 3:
			/*	normal vector */
			um_vctmsc (ent->nvec, 3.0 * char_height, vect);
			status = um_feavect (ent->center, vect);
			if (status!=0)
				return UU_FAILURE;

			break;

		default:
			/*	return error statusus */
			uu_uerror0(UU_FEATERROR,1);
			/*	message is: feature order out of bounds */
			status = UU_FAILURE;
		}

	uu_dexit;
	return(status);
	}
