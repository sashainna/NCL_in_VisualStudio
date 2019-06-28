#define MODULEDBG 0
/*********************************************************************
**    NAME         :  rigtp.c
**       CONTAINS:
**       ur_get_tuple_ptr()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rigtp.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:44
*********************************************************************/

#include "usysdef.h"
#include "ribase.h"
#include	"udebug.h"
#include "uhep.h"

/*********************************************************************
**    I_FUNCTION     :  status = ur_get_tuple_ptr(rel, tuple, ent_ptr)
**      get a pointer to an active(used) relation tuple
**    PARAMETERS   
**       INPUT  : 
**			rel,		a relation number
**			tuple	index of tuple within the relation
**       OUTPUT :  
**			ent_ptr		pointer to the specified entry
**    RETURNS      :  0 if function was successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_get_tuple_ptr(rel, tuple, tuple_ptr) 
UR_REL_NUM		rel;
UR_TUPLE_INDX	tuple;
char				**tuple_ptr;	/* pointer to an tuple	*/
{
	int					status;		/* return status */
	struct UR_rcb_rec	*the_rcb;	/* avoid indexing */
	UR_TUPLE_INDX		tuple_off;	/* tuple offset */
	int					segno;		/* which storage segment */
	int					pag_indx;	/* which entry in the segment */

	uu_denter(UU_RTRC,(us,"ur_get_tuple_ptr(rel=%d, tuple=%d)", rel,
							tuple));
	status	=	0;
	the_rcb = &UR_rcb[rel];	/* index to the proper rcb */
	tuple_off = tuple - 1;

	/* make sure relation and index numbers are reasonable, */
	/* and that relation number is less than max and index is less than max */
	/* and that the tuple is actually active and not a phantom tuple from del */
	/* but only perform these safety tests for debug system */
#ifndef UU_DEBUGOFF
	if(rel <= UR_MAX_REL &&
		tuple <= the_rcb->n_ent &&
		the_rcb->bmap_ptr != UU_NULL &&
		uu_tst_bit(the_rcb->bmap_ptr, tuple_off) &&
		!ur_del_test(rel, tuple))
	{
#endif
		segno = (tuple_off) >> the_rcb->seg_shift;
		pag_indx = (tuple_off) & (the_rcb->page_size - 1);
#if MODULEDBG != 0
		uu_dprint(UU_RITRC,(us,"page %d, index %d",segno,pag_indx));
		uu_dprint(UU_RITRC,(us,"rcb->ent_ptr = 0x%x",the_rcb->ent_ptr));
		uu_dprint(UU_RITRC,(us,"pg_ptr = 0x%x",the_rcb->ent_ptr[segno].pg_ptr));
#endif
		if (the_rcb->ent_ptr!=UU_NULL)
		{
			if (the_rcb->ent_ptr[segno].pg_ptr)
			{
			*tuple_ptr = the_rcb->ent_ptr[segno].pg_ptr +
							(pag_indx * the_rcb->tuple_size);
			}
			/*else
			{
				*tuple_ptr = 0;
				status = -1;
			}*/
			if (*tuple_ptr )
			{
				status = 0;
			}
		}
			
		else
		{
			*tuple_ptr = 0;
			status = -1;
		}
#ifndef UU_DEBUGOFF 
	}
	else
	{
		if(rel > UR_MAX_REL)
		{
			uu_dprint(-1,(us,"ERROR:ur_get_tuple_ptr illegal relation %d", rel));
		}
		else if(tuple > the_rcb->n_ent)
		{
			uu_dprint(-1,(us,"ERROR:ur_get_tuple_ptr illegal tuple %d", tuple));
		}
		else if(the_rcb->bmap_ptr == UU_NULL ||
				!uu_tst_bit(the_rcb->bmap_ptr, tuple_off))
		{
			uu_dprint(-1,(us,"ERROR:ur_get_tuple_ptr inactive tuple %d", tuple));
		}
		else /* has to have been the delete test */
		{
			uu_dprint(-1,(us,"ERROR:ur_get_tuple_ptr deleted tuple, %d", tuple));
		}
		*tuple_ptr = 0;
		status = -1;
	}
#endif
#if MODULEDBG != 0
	uu_dprint(UU_RITRC,(us,"ur_get_tuple_ptr, exit ptr 0x%x, status %d",
			*tuple_ptr,status));
#endif
	uu_dexit;
	return(status);
}
