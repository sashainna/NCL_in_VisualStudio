/*********************************************************************
**
**    NAME         :  zsdraft.s
**
**       CONTAINS:
**				uz_drafting()
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			zsdraft.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:37
**
*********************************************************************/

#include "usysdef.h"
#include "dmark.h"
#include "diconm.h"
#include "driver.h"
#include "lcom.h"
#include "nclicons.h"

UU_LOGICAL UA_draft = UU_FALSE;
static int markval;

/*********************************************************************
**    E_FUNCTION :  uz_zdrafting()
**       main drafting function
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_zdrafting()
	{
	int status;
	int pos[2], size[2];
	static int first = UU_TRUE;
	char us[100];

	UD_MARK(markval, UU_FALSE);
	if(markval == 0)
	{
		UD_ICMU_UP(ALTACT_ICONS);
		ALTACTION_UP = ALTACT_ICONS;
		if(first)
			{
			first = UU_FALSE;
			um_update_drwunits();
			status = umu_set_drwscale();
			if (status!=0)
			{
				first = UU_TRUE;
/*
......we need unmark it
......cause fatal error if not
*/				UD_UNMARK(markval);
				return -1;
			}
			uz_actscale();
			}
		ua_set_drafting_view(UU_TRUE);
		UA_draft = UU_TRUE;
/*
.....Bring up drafting menu
*/
		pos[0] = -1; pos[1] = -1;
		size[0] = -1; size[1] = -1;
		udm_read_menu("DRAFTING.menu",pos,size,1,1,-1);
	}
/*
......we need unmark it
......cause fatal error if not
*/
	UD_UNMARK(markval);
}

/*********************************************************************
**    E_FUNCTION :  uz_zdrafting_exit()
**       Exits main drafting function
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uz_zdrafting_exit()
{
	if (UA_draft == UU_TRUE)
	{
		UD_ICMU_DN(ALTACT_ICONS);
		ALTACTION_UP = 0;
/*
.....we don't got it why exit drafting unmark
.....here, does that mean we have to exit drafting 
.....before execute other function
.....it will cause error on WinNT, temp remove here
*/
/*
		UD_UNMARK(markval);
*/
		UA_draft = UU_FALSE;
	}
}
