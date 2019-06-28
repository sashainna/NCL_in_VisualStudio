/*********************************************************************
**    NAME         :  m8ersol2.c
**       CONTAINS: determine features for ROMULUS solids.
**			int um_f31_body(ent, tfmat, feature_order, dploc)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m8ersol2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:10
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "class.h"
#include "mcrv.h"
#include "msol.h"
#include "mdebug.h"
#include "mdpick.h"

/*********************************************************************
**    E_FUNCTION     : um_f31_body (ent, tfmat, feature_order, dploc)
**       Calculate features of a specified order for the given
**			solid entity (ENT, TFMAT).
**    PARAMETERS   
**       INPUT  : 
**          ent					pointer to solid entity
**				tfmat					transformation matrix
**				feature_order		order of features to display
**				dploc					picked location on entity
**       OUTPUT :  
**          none
**    RETURNS      : UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_f31_body(ent, tfmat, feature_order, dploc)
   struct UM_body_rec *ent;
	UM_transf tfmat;
   int feature_order;
	UD_PLOCREC *dploc;

	{
   UU_LOGICAL status;
	UM_PICKENT pent;
	struct UC_entitydatabag crv;

	uu_denter(UU_MTRC,(us,"um_f31_body(%d, tfmat:%x, %d)", 
						ent->key, tfmat, feature_order));

	/* retrieve edge of body */
	um_d_pickresolve(&dploc->ppath, 2, &pent);
	crv.key = um_get_pickkey(&pent, 2);

	/* check to see if a "real" edge was picked */
	if (ent->key == crv.key)
		status = UU_FAILURE;
	else
		{ /* calculate feature for edge */
		um_get_all_geom(&crv, sizeof(crv));
		status = uc_feature(&crv, UM_idmat, feature_order, dploc);
		}

	uu_dexit;
	return(status);
	}

