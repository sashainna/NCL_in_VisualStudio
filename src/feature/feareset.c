/*********************************************************************
**    NAME         :  feareset
**       CONTAINS:
**       feareset
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       feareset.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:45
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "mfeatcom.h"
#include "mdebug.h"

/*********************************************************************
**    E_FUNCTION     :  int um_feareset()
**      reset the features sub-system
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS :  initializes the features system
**    WARNINGS     : none
*********************************************************************/
um_feareset()

	{
	int i;

	uu_denter(UU_MTRC,(us,"um_feareset()"));

	for (i=0; i<UM_MAXFPICK; i++)
		{
		sprintf(UM_sbuf, "used=%d, dseg=%d",
			UM_Features[i].used, UM_Features[i].seg);
		um_pscroll(UM_sbuf);
		}

	for (i=0; i<UM_MAXFPICK; i++)
		if (UM_Features[i].used != 0)
			gdeleteseg(UM_Features[i].seg);

   um_feainit();

	uu_dexit;
	}
