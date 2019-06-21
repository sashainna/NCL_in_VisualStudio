/*********************************************************************
**    NAME         :  feavect
**       CONTAINS:
**       feavect
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       feavect.c , 25.1
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
**    E_FUNCTION     :  int um_feavect(point,vector)
**			Add a vector feature to the current features tables
**			and draw the feature on the display device
**    PARAMETERS   
**       INPUT  : 
**          point				point to start drawing vector
**          vector			vector to draw
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS :  adds the vector feature to the features
**                    tables which are included in featcom.h 
**                    and draws the feature on the screen
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

um_feavect (point,vector)
   UM_coord point;
   UM_vector vector;

	{
   int pick_id;
	UM_vector uvector;  /* unit external vector representation */

	uu_denter(UU_MTRC,(us,"um_feavect(%x,%x)", point, vector));

	if (UM_F_vect_count < UM_MAXVECTF) /* enough room in vector table */
		{
		/* offset the GKS pick id for the vector range */
		pick_id = UM_F_vect_count + UM_VECTMIN;
		gspickid(pick_id);

		/* display the vector at the correct position */
		um_arrow (point, vector);

		/* update number of vector features in feature table */
		um_unitvc(vector, uvector);
		sprintf(UM_sbuf,"feavect: pick_id=%d count=%d vect=(%f,%f,%f)",
			pick_id,UM_F_vect_count,uvector[0],uvector[1],uvector[2]);
		um_pscroll(UM_sbuf);
      um_vctovc (uvector,UM_F_vect[UM_F_vect_count]);
		UM_F_vect_count ++;
		}
	else
	{
		uu_uerror0(UU_FEATERROR, 14);
		return UU_FAILURE;
		/* message is: feavect: no further vector features allowed */
	}
	return UU_SUCCESS;
	uu_dexit;
}
