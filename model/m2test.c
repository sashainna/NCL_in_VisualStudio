
/*********************************************************************
**    NAME         :  mtest
**       CONTAINS:
**			umu_delone
**			umu_print
**			um_display
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2test.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:48
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "usysg.h"
#include "dasnog.h"
#include "dselect.h"
#include "class.h"
#include "mdpick.h"

UU_LOGICAL ud_gnxt();

/*********************************************************************
**    E_FUNCTION     : umu_delone()
**       Delete entities.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_delone()
	{
	int numint;
	int key;
	UU_LOGICAL um_initialize;
	int i;

	ud_ldas(UD_DASSELECT, /*pick entities to delete*/UM_MODEL, 68, UU_NULL, 
			 1, &numint,UD_NODEFAULT);

	if (numint != 0)
		{
		um_initialize = UU_TRUE;
		while(ud_gnxt(um_initialize, UU_NULL, &key, 1) == UU_TRUE)
			{
			uc_delete(key);
			um_initialize = UU_FALSE;
			}
		}
	}
/*********************************************************************
**    E_FUNCTION     : umu_print(option)
**       Print an entity either by having the user pick from the
**			displayed entities or by entering a MTID.
**    PARAMETERS   
**       INPUT  : 
**          option				1 => pick from display
**										2 => enter MTID
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
umu_print(option)
	int option;

	{
	struct UC_entitydatabag e;
	UM_PICKENT pent;
	UU_KEY_ID um_get_pickkey();
	int numint;

	if (option  ==  1)
		{
		um_dl_pdas(UD_DASPICK, /*pick entity*/UM_MODEL, 153, &pent, 1, &numint, 1);
		e.key = um_get_pickkey(&pent, 1);
		}
	else if (option  ==  2)
		{
		ud_ldas(UD_DASINT, /*enter  UM_MTID*/UM_MODEL, 69, &e.key, 
					1, &numint,UD_NODEFAULT);
		}
	if (numint > 0) 
		{
		um_get_all_geom(&e, sizeof(struct UC_entitydatabag));
		uc_print(&e);
		}
	}
/*********************************************************************
**    E_FUNCTION     : um_display()
**       Display an entity by specifying its MTID.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_display()
	{
	struct UC_entitydatabag e;        /* entity data */
	int numint;
	int status;
	int ur_retrieve_data_fixed();

	ud_ldas(UD_DASINT,/*display key*/UM_MODEL, 70, &e.key, 1, 
				&numint,UD_NODEFAULT);
	if (numint > 0)
		{
		status = ur_retrieve_data_fixed(&e);
		if (status == 0) uc_draw(&e);
		}
	}
