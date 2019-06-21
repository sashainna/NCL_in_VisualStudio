/*********************************************************************
**    NAME         :  f5comp.c 
**       CONTAINS: determine features for composite curves.
**			int um_f5compcrv(ent, tfmat, feature_order, dploc)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       f5comp.c , 25.1
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
**    E_FUNCTION     : um_f5compcrv (eptr, tfmat, feature_order, dploc)
**       Calculate features of a specified order for the given
**			composite curve (ENT, TFMAT).
**    PARAMETERS   
**       INPUT  : 
**          eptr					pointer to composite curve
**				tfmat					transformation matrix
**				feature_order		order of features to display
**				dploc					picked location on entity
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE iff no error; else UU_FALSE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_f5compcrv(eptr, tfmat, feature_order, dploc)
   struct UM_compcrv_rec *eptr;
	UM_transf tfmat;
   int feature_order;
	UD_PLOCREC *dploc;

	{
	int status;
	struct UM_evcrvout evout;
	UM_vector vec;
	UM_coord vprefpt;
	UM_vector vpup;
	UM_vector vpnorm;
	UM_vector vptan;
	UM_length aperture;
	UU_REAL char_height;

	uu_denter(UU_MTRC,(us,"um_f5comp(%d, tfmat:%x, %d)", 
						eptr->key, tfmat, feature_order));

	status = UU_SUCCESS;

	/* get the view parameters for the currently active normtran
		(i.e. the view that the features will be displayed in) */
	um_get_current_viewplane(vprefpt, vptan, vpup, vpnorm, &aperture);

	/* set feature character height */
	um_set_feature_char_height(&char_height);

	switch (feature_order)
		{
		case 1: /* 1st order features: derivatives and length */

			/* show derivatives */
			uc_init_evcrvout(eptr, &evout);

			/* scale tangent vector at start point */
			um_ev5_compcrv(UM_FRSTDERIV, (UU_REAL) 0.0, eptr, tfmat, &evout);
			um_unitvc(evout.dcdu, vec);
			um_vctmsc(vec, 3.0 * char_height, vec);
			status = um_feavect(evout.cp, vec);
			if (status!=0)
				return UU_FAILURE;

			/* scale tangent vector at end point */
			um_ev5_compcrv(UM_FRSTDERIV, (UU_REAL) 1.0, eptr, tfmat, &evout);
			um_unitvc(evout.dcdu, vec);
			um_vctmsc(vec, 3.0 * char_height, vec);
			status = um_feavect(evout.cp, vec);
			if (status!=0)
				return UU_FAILURE;

			break;

		default:

			uu_uerror0 (UU_FEATERROR,1);
			/* message is: error - feature order out of bounds */
			status = UU_FAILURE;
 
		}
	uu_dexitstatus("um_f5compcrv", status);
	return(status);
	}

