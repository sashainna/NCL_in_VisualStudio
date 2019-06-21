
/*********************************************************************
**    NAME         :  f6spline.c
**       CONTAINS: features for bspline
**				um_f6spline (ent, tfmat, feature_order, dploc)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       f6spline.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:44
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "umath.h"
#include "dasnog.h"
#include "mcrv.h"
#include "mdeval.h"

/*********************************************************************
**    E_FUNCTION     : um_f6spline (ent, tfmat, feature_order, dploc)
**       Calculate the features for a bspline curve.
**    PARAMETERS   
**       INPUT  : 
**          ent					pointer to bspline entity 
**				tfmat					transformation matrix
**				feature_order		order of feature to calculate
**				dploc					picked location on bspline
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
um_f6spline (ent, tfmat, feature_order, dploc)
   struct UM_bsplcrv_rec *ent;
	UM_transf tfmat;
   int feature_order;
	UD_PLOCREC *dploc;

	{
   int status;

	uu_denter(UU_MTRC,(us, "um_f6spline(key=%x,tfmat=%x,order=%d)",
		ent->key, tfmat, feature_order));

	status = UU_FAILURE;

	switch (feature_order)
		{
		case 1:
			/* start point */
			/* end point */
			/* control points */

			break;

		case 2:
			/* tangents at knot values */
			/* length of spline */

			break;

		case 3:

			break;
 
		default:
			status = UU_FAILURE;
 
		}

	uu_dexit;
	return(status);

	}
       
