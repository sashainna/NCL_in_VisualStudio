/*********************************************************************
**    NAME         :  feacoord
**       CONTAINS: routines to handle coordinate features
**       feacoord
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       feacoord.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:45
*********************************************************************/
#include "usysdef.h" 
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "usysg.h"
#include	"gtbl.h"
#include "g.h"
#include "mdcoord.h"
#include "mdcpln.h"
#include "mfeatcom.h"
#include "mdebug.h"

/*********************************************************************
**    E_FUNCTION     :  int um_feacoord(coord)
**      add a coordinate feature to the current features tables
**       and draw the feature on the display device
**    PARAMETERS   
**       INPUT  : 
**				coord 		internal representation of coordinate feature
**								(i.e. cartesian and UM_CM)
**       OUTPUT :  
**          output: none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_feacoord(coord)
   UM_coord coord; 		/* internal (i.e. cc) coordinate representation */

	{
   int cur_type;			/* the current GKS marker type */
   int pick_id;			/* the local offset GKS pick id number */

	uu_denter(UU_MTRC,(us,"um_feacoord(%x)", coord));

	if (UM_F_coord_count < UM_MAXCOORDF)
		{
		/* offset the GKS pick id for the coordinate range */
		pick_id = UM_F_coord_count + UM_COORDMIN;
		gspickid(pick_id);

		/* save the current GKS marker type */
		cur_type = gqmarktype();

		/* set the GKS marker type to 3 */
		gsmarktype(3);

		/* display a GKS marker at the coordinate */
		gpolymarker3(1,coord);

		/* restore the current GKS marker type */
		gsmarktype(cur_type);

		/* update number of coordinate features in feature table */
		sprintf(UM_sbuf,"feacoord: pick_id=%d count=%d coord=(%f,%f,%f)",
			pick_id,UM_F_coord_count,coord[0],coord[1],coord[2]);
		um_pscroll(UM_sbuf);
		um_vctovc (coord,UM_F_coord[UM_F_coord_count]);
		UM_F_coord_count++;
		}
	else
	{
		uu_uerror0(UU_FEATERROR, 13);
		return UU_FAILURE;
		/* message is: feacoord: no further coordinate features allowed */
	}
	return UU_SUCCESS;
	uu_dexit;
} /* end of main program body */
