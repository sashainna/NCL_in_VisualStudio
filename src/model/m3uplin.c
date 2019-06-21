
/*********************************************************************
**    NAME         :  m3uplin.c
**       CONTAINS: test routine to create a polyline entity.
**			umu_c42_polyline()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3uplin.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:59
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdrel.h"
#include "mcrv.h"
#include "dmark.h"

/*********************************************************************
**    E_FUNCTION     : umu_c42_polyline()
**			Create polyline by prompting the user for a sequence of points.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c42_polyline()

	{
	struct UM_polyline_rec e;		/* polyline curve */
/**	UM_coord pt;						/* new point of line */
    UD_NDCLOCREC pt;

	int numint;							/* number of DAS entries */
	UD_RUBBER rubber;					/*  rubber band control block */
	int markval;						/*  MARK value return */

	uu_denter(UU_MTRC, (us, "umu_c42_polyline"));
	numint = 1;
	ud_strub(&rubber, 4);
	UD_MARK(markval, UU_FALSE);
	ur_setup_app_data(UM_POLYLINE_REL, &e, sizeof(e));
	if(markval == 0)
		{
		while (UU_TRUE)
			{
			ud_ldas(UD_DASCART, /*start point of line*/UM_MODEL, 29, &pt, 
				 1, &numint, UD_NODEFAULT);
			if (numint <= 0) goto done;
			e.no_pt = 0;
			ur_update_app_data_varlist(&e, 1, &pt, e.no_pt+1, 1);
			ud_onrub(&rubber);
			while (UU_TRUE)
				{
				ud_ldas(UD_DASCART, /*end point of line*/UM_MODEL, 30, &pt, 
					 1, &numint, UD_NODEFAULT);
				if (numint <= 0) goto repeat;
				ur_update_app_data_varlist(&e, 1, &pt, e.no_pt+1, 1);
				}
repeat:
			if (e.no_pt > 1)
				{
				um_create_geom(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
				uc_display(&e);
				}
			ud_endrub(&rubber);
			}
		}
done:
	ud_endrub(&rubber);
	uu_dexit;
	UD_UNMARK(markval);
	}
