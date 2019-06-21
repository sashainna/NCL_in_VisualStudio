/*********************************************************************
**    NAME         :  rtpltst.c
**       CONTAINS:
**       ur_test_tuple()
**    COPYRIGHT 1989 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rtpltst.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:50
*********************************************************************/

#include "ribase.h"
#include	"udebug.h"

/*********************************************************************
**    I_FUNCTION     :  status = ur_test_tuple(key)
**
**			Determine if the given key is acceptable.
**			status = UU_SUCCESS if   relation <= maximum relation
**                               tuple index <= number of entities
**                               rcb bit map != NULL
**                               bit map tests o.k.
**                               tuple has not already been deleted
**      else return UU_FAILURE
**
**
**    PARAMETERS   
**       INPUT  :  key - data key 
**
**       OUTPUT :  none
**
**    RETURNS      :  UU_SUCCESS if function was successful and
**                    UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_test_tuple(key) 
UU_KEY_ID key;
{
int					status;		/* return status */
struct UR_rcb_rec	*the_rcb;	/* avoid indexing */
UR_REL_NUM rel;
UR_TUPLE_INDX tuple;

uu_denter(UU_RTRC,(us,"ur_test_tuple(key=%d)", key));

status	=	UU_FAILURE;
ur_k2rt(key, &rel, &tuple);
the_rcb = &UR_rcb[rel];	/* index to the proper rcb */

/* make sure relation and index numbers are reasonable, */
/* and that relation number is less than max and index is less than max */
/* and that the tuple is actually active and not a phantom tuple from del */
/* but only perform these safety tests for debug system */

if(rel <= UR_MAX_REL && tuple <= the_rcb->n_ent &&
	the_rcb->bmap_ptr != UU_NULL && uu_tst_bit(the_rcb->bmap_ptr, tuple-1) &&
	!ur_del_test(rel, tuple)) 
	{
	status = UU_SUCCESS;
	}
else
	{
	/* we have bad data base info - write the appropriate error message */
	if(rel > UR_MAX_REL)
		{
		uu_dprint(-1,(us,"ERROR:ur_test_tuple illegal relation %d", rel));
		}
	else if(tuple > the_rcb->n_ent)
		{
		uu_dprint(-1,(us,"ERROR:ur_test_tuple illegal tuple %d", tuple));
		}
	else if(the_rcb->bmap_ptr == UU_NULL ||
			!uu_tst_bit(the_rcb->bmap_ptr, tuple-1))
		{
		uu_dprint(-1,(us,"ERROR:ur_test_tuple inactive tuple %d", tuple));
		}
	else /* has to have been the delete test */
		{
		uu_dprint(-1,(us,"ERROR:ur_test_tuple deleted tuple, %d", tuple));
		}
	status = UU_FAILURE;
	}
uu_dexit;
return(status);
}
