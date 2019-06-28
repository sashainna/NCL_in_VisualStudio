/*********************************************************************
**    NAME         :  f7spline.c 
**       CONTAINS: features for bspline
**			int um_f7_spline(eptr, tfmat, feature_order, dploc)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       f7spline.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:44
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "dasnog.h"
#include "mcrv.h"
#include "mdeval.h"

/*********************************************************************
**    E_FUNCTION     : um_f7_spline (eptr, tfmat, feature_order, dploc)
**       Calculate features for rational bspline curve.
**    PARAMETERS   
**       INPUT  : 
**          eptr					pointer to rational bspline curve
**				tfmat					transformation matrix
**				feature_order		order of feature to display
**				dploc					picked location on bspline
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff on error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_f7_spline (eptr, tfmat, feature_order, dploc)
	struct UM_rbsplcrv_rec *eptr;
	UM_transf tfmat;
	int feature_order;
	UD_PLOCREC *dploc;

	{
	int status;
	UU_REAL *cntlpt;
	UM_vector vec;
	UU_REAL len;
	struct UM_evcrvout evout;
	int i,j;

	uu_denter(UU_MTRC,(us,"um_f7_spline(%d,tfmat:%x,%d)",
					eptr->key,tfmat,feature_order));

	/* transform geometry to model space */
	um_tf7_tranfrbsplcrv(eptr, tfmat, UU_FALSE);

	/* calculate desired features */
	status = UU_SUCCESS;
	switch (feature_order)
		{
		case 1:
			/* show control points */
			cntlpt = eptr->pt;
			for (i=0, j=0; i<eptr->no_pt; i++, j=j+3)
			{
				status = um_feacoord(&(cntlpt[j]));
				if (status!=0)
					return UU_FAILURE;
			}
			/* show derivatives */
			uc_init_evcrvout(eptr, &evout);

			/* scale tangent vector at end points */
			um_ev7_rbsplcrv(UM_FRSTDERIV, (UU_REAL) 0.0, eptr, UM_idmat, &evout);

			um_vcmnvc(cntlpt, &(cntlpt[3]), vec);
			len = um_mag(vec);
			um_unitvc(evout.dcdu, vec);
			um_vctmsc(vec, 2.0*len, vec);
			status = um_feavect(evout.cp, vec);
			if (status!=0)
				return UU_FAILURE;

			/* scale tangent vector at end points */
			um_ev7_rbsplcrv(UM_FRSTDERIV, (UU_REAL) 1.0, eptr, UM_idmat, &evout);

			um_vcmnvc(&(cntlpt[3*eptr->no_pt-6]), &(cntlpt[3*eptr->no_pt-3]), vec);
			len = um_mag(vec);
			um_unitvc(evout.dcdu, vec);
			um_vctmsc(vec, 2.0*len, vec);
			status = um_feavect(evout.cp, vec);
			if (status!=0)
				return UU_FAILURE;
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
       
