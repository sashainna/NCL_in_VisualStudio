/*********************************************************************
**    NAME         :  feapick.c
**       CONTAINS:
**			int um_feapick()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       feapick.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:45
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dtypes.h"
#include "dselect.h"
#include "dselmask.h"
#include "mfeatcom.h"
#include "mdebug.h"
#include "mdpick.h"

static UU_LOGICAL trace = UU_TRUE;
/*********************************************************************
**    E_FUNCTION     :  int um_feapick()
**       get the pick id values for the geometry selected
**       by the user for feature definition 
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      :  stat - returned error status
**    SIDE EFFECTS :  fills pick_ids and feature_orders arrays
**    WARNINGS     : none
*********************************************************************/

um_feapick()

	{
   register int i,j;
   int numint;
	int dsegid;
	int pickid;
   UD_PLOCREC dploc;
   UM_PLOCREC mploc;
   UU_LOGICAL in;
	int unused;
	int viewid;

	uu_denter(UU_MTRC,(us,"um_feapick()"));

	for (i=0; i<UM_MAXFPICK; i++)
		UM_Features[i].order[0] = 0;  /* no current picks for any entry */

	/* limit DAS to pick only those entities for which features are
		available */
	ud_lgeo(UU_TRUE, UD_features);

	while (UU_TRUE)
		{
		/* pick an entity */
		ud_ldas(UD_DASPCKLOC, UU_FEATERROR, 1, &dploc, 1, &numint, UD_NODEFAULT);
		if (numint < 1) goto done;

		/* get the view, display segment, and pickid */
		viewid = dploc.pndc.transform;
		dsegid = dploc.ppath.pickpath[0];
		pickid = dploc.ppath.pickpath[1];

		/* check to see if the new entity picked has already been picked */
		in = UU_FALSE;
		for (j=0; (!in) && (j<UM_MAXFPICK); j++)
			{
			if (UM_Features[j].used != 0)
				in =((dsegid == UM_Features[j].ploc.ppath.pickpath[0]) &&
					  (pickid == UM_Features[j].ploc.ppath.pickpath[1]));
			if (in)
				{
				if ((UM_Features[j].order[0]+1) < UM_NORDER)
					UM_Features[j].order[0]++;
				break;
				}
			}

		/* if this is a new entity, add to the Feature tables */
      if (!in)
			{
			/* find an unused feature entry */
			unused = -1;
			for (i=0; (unused<0) && (i<UM_MAXFPICK); i++)
				if (UM_Features[i].used == 0)
					unused = i;

         if (unused>=0)
				{
				UM_Features[unused].ploc = dploc;
				UM_Features[unused].used = 1;
				UM_Features[unused].order[0] = 1;
				UM_Features[unused].order[1] = 0;
				UM_Features[unused].order[2] = 0;
				UM_Features[unused].order[3] = 0;
				UM_Features[unused].seg = -1;
				UM_Features[unused].view[0] = viewid;
         	}
			else
				{
			   uu_uerror0(UU_FEATERROR, 6); /* error - too many picks */
           	}
        	}
   	}

done:;
	uu_dexit;
   return(UU_SUCCESS);
	}
