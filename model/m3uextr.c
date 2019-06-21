
/*********************************************************************
**    NAME         :  m3uextr
**       CONTAINS:
**			umu_circular_curve_sweep()
**			umu_linear_curve_extrusion()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3uextr.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:58
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dselmask.h"
#include "class.h"
#include "mdclass.h"
#include "mdrel.h"
#include "modef.h"
#include "mcrv.h"
#include "mattr.h"
#include	"mdpick.h"
#include	"mdeval.h"

UU_LOGICAL	ud_gnxt();

/*********************************************************************
**    E_FUNCTION     : umu_circular_curve_sweep()
**       Prompt the user for a curve to sweep around a line.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_circular_curve_sweep()

	{
	UM_PICKENT pent;
	struct UC_entitydatabag e;
/**	UM_coord pt; **/
    UD_NDCLOCREC pt;

	UM_vector vec;
	UM_angle ang;
	UU_LOGICAL initialize;
	int status;
	int numint;

	uu_denter(UU_MTRC,(us,"umu_circular_curve_sweep()"));

	ud_lgeo(UU_TRUE, UD_allcurves);

	ud_ldas(UD_DASSELECT, /*Pick curve for circular extrusion */ 
			UM_MODEL, 251, UU_NULL, 0, &numint, UD_NODEFAULT);
	if (numint <= 0) goto done;

repeat:
	ud_ldas(UD_DASCART, /* point on rotation axis*/ UM_MODEL, 6,
		&pt, 1, &numint, UD_NODEFAULT);
	if (numint <= 0) goto repeat;

	ud_ldas(UD_DASVEC, /* direction vector of axis */ UM_MODEL, 7,
		vec, 1, &numint, UD_NODEFAULT);
	if (numint <= 0) goto repeat;

	ud_ldas(UD_DASANGLE, /* angle of rotation */ UM_MODEL, 77,
		&ang, 1, &numint, UD_NODEFAULT);
	if (numint <= 0) goto repeat;

	um_unitvc(vec, vec);
	initialize = UU_TRUE;
	while(ud_gnxt(initialize, UU_NULL, &e.key, 1) == UU_TRUE)
   	{                                              
		initialize = UU_FALSE;
		status = uc_retrieve_data(&e, sizeof(e));
		if (status == UU_SUCCESS) um_circular_curve_sweep(&e, &pt, vec, ang);
		}
done:
	ud_lgeo(UU_FALSE, UD_circle);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : umu_linear_curve_extrusion()
**       Prompt the user for a curve to linearly extrude.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_linear_curve_sweep()

	{
	UM_PICKENT pent;
	struct UC_entitydatabag e;
	UM_vector offset;
	UU_LOGICAL initialize;
	int status;
	int numint;

	uu_denter(UU_MTRC,(us,"umu_linear_curve_sweep()"));

	ud_lgeo(UU_TRUE, UD_allcurves);

	ud_ldas(UD_DASSELECT, /*Pick curve for circular extrusion */ 
			UM_MODEL, 251, UU_NULL, 0, &numint, UD_NODEFAULT);
	if (numint <= 0) goto done;

repeat:
	ud_ldas(UD_DASVEC, /* distance and direction to lift*/ UM_MODEL, 10,
		offset, 1, &numint, UD_NODEFAULT);
	if (numint <= 0) goto repeat;

	initialize = UU_TRUE;
	while(ud_gnxt(initialize, UU_NULL, &e.key, 1) == UU_TRUE)
	 	{                                              
		initialize = UU_FALSE;
		status = uc_retrieve_data(&e, sizeof(e));
		if (status == UU_SUCCESS) um_linear_curve_extrusion(&e, offset);
		}

done:
	ud_lgeo(UU_FALSE, UD_circle);
	uu_dexit;
	}

