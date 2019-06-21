/*********************************************************************
**    NAME         :  m2udislv.c
**       CONTAINS: user interface routines for dissolving entities
**			umu_dissolve()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2udislv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:49
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "dselmask.h"
#include "mdpick.h"

/*********************************************************************
**    E_FUNCTION     : umu_dissolve()
**			Prompt the user to pick entities to dissolve.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_dissolve()

{
	int status;
	int numint;
	struct UC_entitydatabag e;
	UU_LOGICAL initialize;

	uu_denter(UU_MTRC,(us,"umu_dissolve()"));

	/* limit pickable entities to those which can be dissolved */
	ud_lgeo(UU_TRUE, UD_dissolve);
	while (UU_TRUE)
	{
/*
.....Prompt user to select entities to dissolve.
*/
		ud_ldas(UD_DASSELECT,UM_MODEL,134,UU_NULL,1,&numint,UD_NODEFAULT);
		if (numint < 1) goto done;
		initialize = UU_TRUE;
/*
.....Loop through the entities, and if it should
.....be dissolved, disolve it.
*/
		while(ud_gnxt(initialize, UU_NULL, &e.key,1) == UU_TRUE)
		{
			initialize = UU_FALSE;
			uc_retrieve_data(&e, sizeof(e));			
/*
.....Only dissolve composite curve.
*/
			if (e.rel_num ==5)
				uc_dissolve(&e);
		}
	}
done:;
	uu_dexit;
}

