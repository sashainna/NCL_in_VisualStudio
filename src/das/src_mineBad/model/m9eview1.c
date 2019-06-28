/*********************************************************************
**    NAME         :  m9eview
**       CONTAINS: routine to switching between subsystems
**			um_getallobjs(init, key)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m9eview1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:13
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "mdrel.h"
#include "view.h"

/**************************************************************************
**  E_FUNCTION:  um_getallobjs(init, key)
**      Use Unibase to get ALL geometry while filtering out viewing entities
**  PARAMETERS   
**      INPUT  :
**				init	:	set to UU_TRUE to reset to start of unibase
**      OUTPUT :
**				key	:	next key to be drawn
**  RETURNS      :
**			0 iff no error; else -1
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
um_getallobjs(init, key)
	UU_LOGICAL init;
	UU_KEY_ID  *key;

	{
	int status;
	static int next_tupleid;
	int relnum;

	uu_denter(UU_MTRC,(us,"um_getallobjs(init=%d,key=%d)",init,*key));

	if (init == UU_TRUE) next_tupleid = 1;
	status = UU_FAILURE;

	/* query Unibase for next key */
	while (ur_get_next_key(&next_tupleid, key) > -1)
		{
		next_tupleid++;

		/* find out what type of entity we found */
		ur_retrieve_data_relnum(*key, &relnum);

		switch (relnum)
			{
			/* filter out Viewing entities */
			case UV_VIEW_REL :
			case UV_VPORT_REL :
			case UV_SCREEN_REL :
			case UR_UNISTAT_REL:
				break;
			default:
				status = UU_SUCCESS;
				break;
			}
		if (status == UU_SUCCESS) break;
		}

/*
	sprintf(UM_sbuf,"um_getallobjs: key=%d, status=%d",*key,status);
	um_pscroll(UM_sbuf);
*/
	uu_dexit;
	return(status);
	}
