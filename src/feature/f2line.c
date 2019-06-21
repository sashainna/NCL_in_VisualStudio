/*********************************************************************
**    NAME         :  f2line.c
**       CONTAINS: determine features for lines.
**			int um_f2line(ent, tfmat, feature_order, dploc)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       f2line.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:44
*********************************************************************/
#include "usysdef.h"
#include "umath.h" 
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "ginq.h"
#include "dasnog.h"
#include "mcrv.h"
#include "mdeval.h"
#include "mdebug.h"

/*********************************************************************
**    E_FUNCTION     : um_f2line (ent, tfmat, feature_order, dploc)
**       Calculate features of a specified order for the given
**			line entity (ENT, TFMAT).
**    PARAMETERS   
**       INPUT  : 
**          ent					pointer to line entity
**				tfmat					transformation matrix
**				feature_order		order of features to display
**				dploc					picked location on entity
**       OUTPUT :  
**          none
**    RETURNS      : UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_f2line(ent, tfmat, feature_order, dploc)
   struct UM_line_rec *ent;
	UM_transf tfmat;
   int feature_order;
	UD_PLOCREC *dploc;

	{
	struct UM_evcrvout evout;
   UU_LOGICAL stat;
	UU_LOGICAL dflag;
   UM_coord spt;
	UM_coord ept;
   UM_coord pos;
	UM_length mag_vect;
	UU_REAL mvec1;
	UU_REAL mvec2;
   UM_vector vect;
	UM_vector uvect;
	UM_vector unvect;
	UM_vector nvect;
	UM_vector dvec1;
	UM_vector dvec2;
	UM_vector ref_vect;
	UM_coord vprefpt;
	UM_vector vpup;
	UM_vector vpnorm;
	UM_vector vptan;
	UM_length aperture;
	UU_REAL char_height;
	void um_get_current_viewplane();
	void um_set_feature_char_height();

	uu_denter(UU_MTRC,(us,"um_f2line(%d, tfmat:%x, %d)", 
						ent->key, tfmat, feature_order));

	stat = UU_SUCCESS;

	/* get the view parameters for the currently active normtran
		(i.e. the view that the features will be displayed in) */
	um_get_current_viewplane(vprefpt, vptan, vpup, vpnorm, &aperture);

	/* set feature character height */
	um_set_feature_char_height(&char_height);

	switch (feature_order)
		{
		case 1: /* 1st order features: length and start vector */

   		/* calculate the start and end points of the line, its length,
				and a unit vector along the line */
			um_cctmtf(ent->spt, tfmat, spt);
			um_cctmtf(ent->ept, tfmat, ept);
			um_vcmnvc(ept, spt, uvect); 
			mag_vect = um_mag(uvect);
			um_unitvc (uvect, uvect);

			um_cross (uvect, vpup,  dvec1);
			um_cross (uvect, vptan, dvec2);
			mvec1 = um_mag (dvec1);
			mvec2 = um_mag (dvec2);
			if (mvec1 > mvec2)
				um_vctovc (vpup, ref_vect);
			else
				um_vctovc (vptan, ref_vect);

			um_cross (uvect, ref_vect, nvect);
			um_cross (nvect, uvect, unvect);
			um_vctmsc (unvect, char_height, nvect);
			um_vcplvc (spt, nvect, spt);
			um_vctmsc(uvect, 3.0 * char_height, vect);
			stat = um_feavect(spt, vect);
			if (stat!=0)
				return UU_FAILURE;

			/* define the length feature */
			um_ev2_line(UM_POINT, (UU_REAL) .3333, ent, tfmat, &evout);
			um_vctmsc(vpup, 2.0*char_height, pos);
			um_vcplvc(evout.cp, pos, pos);
			dflag = UU_TRUE;
			stat = um_fealength(" Length = ", mag_vect, pos, evout.cp, dflag);
			if (stat!=0)
				return UU_FAILURE;
			break;

		case 2: /* 2nd order features: mid-point */

			um_ev2_line(UM_POINT, (UU_REAL) .50, ent, tfmat, &evout);
			stat = um_feacoord (evout.cp);
			if (stat!=0)
				return UU_FAILURE;
			break;

		case 3: /* 3rd order features: quarter points */

			um_ev2_line(UM_POINT, (UU_REAL) .25, ent, tfmat, &evout);
			stat = um_feacoord (evout.cp);
			if (stat!=0)
				return UU_FAILURE;
			um_ev2_line(UM_POINT, (UU_REAL) .75, ent, tfmat, &evout);
			stat = um_feacoord (evout.cp);
			if (stat!=0)
				return UU_FAILURE;
			break;
 
		default:

			uu_uerror0 (UU_FEATERROR,1);
			/* message is: error - feature order out of bounds */
			stat = UU_FAILURE;
 
		}
	uu_dexit;
	return(stat);
	}

/*********************************************************************
**    E_FUNCTION     :
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_get_current_viewplane(vporigin, vpxaxis, vpyaxis, vpzaxis, aperture)
	UM_coord vporigin;
	UM_vector vpxaxis;
	UM_vector vpyaxis;
	UM_vector vpzaxis;
	UU_REAL *aperture;

	{
	Gindex xform;
	Gwpoint3 *vp;
	Gwrect3 *window;

	uu_denter(UU_MTRC,(us, "um_get_current_viewplane()"));

	xform = gqnormtran();

	vp = gqrefpt(xform);
	vporigin[0] = vp->x;
	vporigin[1] = vp->y;
	vporigin[2] = vp->z;

	vp = gqvup3(xform);
	vpyaxis[0] = vp->x;
	vpyaxis[1] = vp->y;
	vpyaxis[2] = vp->z;

	vp = gqvpn3(xform);
	vpzaxis[0] = vp->x;
	vpzaxis[1] = vp->y;
	vpzaxis[2] = vp->z;

	um_cross(vpyaxis, vpzaxis, vpxaxis);

	window = gqwindow3(xform);
	*aperture = window->urb.x - window->llf.x;

	uu_dexit;
}

/*********************************************************************
**    E_FUNCTION     :
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_set_feature_char_height(char_height)
	UM_length *char_height;

	{
	Gindex xform;
	UM_ndc ndc1, ndc2;
	UM_coord cc1, cc2;

	xform = gqnormtran();
	um_xyztovc((UU_REAL) .01, (UU_REAL) 0.0, (UU_REAL) 0.0, ndc1);
	uv_ndctocc(ndc1, cc1, xform);
	um_xyztovc((UU_REAL) 0.0, (UU_REAL) 0.0, (UU_REAL) 0.0, ndc2);
	uv_ndctocc(ndc2, cc2, xform);
	um_vcmnvc(cc1, cc2, cc1);
	*char_height = um_mag(cc1);
	gscharheight(*char_height);
}

