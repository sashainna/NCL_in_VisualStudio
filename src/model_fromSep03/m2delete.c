/*********************************************************************
**    NAME         :  m2delete.c
**       CONTAINS: Geometry methods
**				int um_delete_all(associate, association)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2delete.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:45
**
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"

/*********************************************************************
**    E_FUNCTION :  int um_delete_all(associate, association)
**       Delete method
**    PARAMETERS   
**       INPUT  : 
**				UU_KEY_ID associate = key of associate
**				UU_KEY_ID association = key of association
**          relation			relation number to query
**       OUTPUT :  
**				none
**    RETURNS      : ptr to record of what this relation can be
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int um_delete_all(associate, association)
UU_KEY_ID associate;						/* key of associate */
UU_KEY_ID association;					/* key of association */
{
   uu_denter(UU_MTRC,(us,"enter um_delete_all: ate=0x%x, tion=%x",
		associate, association));
	uu_dexit;
	return(ur_delete_all(associate));
}
