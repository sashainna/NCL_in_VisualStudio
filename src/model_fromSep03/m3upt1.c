
/*********************************************************************
**    NAME         :  m3upt1.c
**       CONTAINS: user interface for point definition
**			umu_c1_nptent()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3upt1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:00
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dselmask.h"
#include "mdclass.h"
#include "mdrel.h"
#include "modef.h"
#include "mdgenent.h"
#include "mcrv.h"
#include "mdcpln.h"
#include	"mdpick.h"
#include	"misect.h"


/*********************************************************************
**    E_FUNCTION     : umu_c1_nptent()
**       Create a single point at the closest point on a picked entity.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_c1_nptent()

	{
	struct UM_point_rec e;
	UM_PLOCREC pick;
	int numint;
	int status;

	uu_denter(UU_MTRC,(us,"umu_c1_nptent()"));

	ud_lgeo(UU_TRUE, UD_vircurves);
	ur_setup_data(UM_POINT_REL, &e, sizeof(struct UM_point_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e.label, "");
	e.subscr = 0;
	while (UU_TRUE)
		{
		um_dl_pdas(UD_DASPICK,/*pick entity near point */UM_MODEL, 138, &pick,
			1, &numint, 1);
		if (numint <= 0) goto done;
		e.key = um_get_pickkey(&pick.pent, 2);
		status = uc_near_on_entity(e.key, &pick.ploc, e.pt);
		um_create_pt1(&e, UM_DEFAULT_TF, UM_CURRENT_ATTR);
		uc_display(&e);
		}
done:;
	uu_dexit;
	}
