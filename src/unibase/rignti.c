/*********************************************************************
**    NAME         :  rgnti.c
**       CONTAINS:
**       ur_get_next_tuple_index()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rignti.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:44
*********************************************************************/

#include	"usysdef.h"
#include	"ribase.h"
#include	"udebug.h"

/*********************************************************************
**    E_FUNCTION     :  status = ur_get_next_tuple_index(rel_num,&tuple_indx)
**      get the index for the next active tuple within a relation
**    PARAMETERS   
**       INPUT  : 
**			rel_num, relation in which next tuple index is desired
**			tuple_indx, the entry to start looking from
**       OUTPUT :  
**			tuple_indx, the next active tuple index
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : to scan all entries, start with tuple_indx = 1 and 
**						  increment tuple_indx before each subsequent call
*********************************************************************/

ur_get_next_tuple_index(rel_num,tuple_indx)
UR_REL_NUM 		rel_num;			/* rel to find last active entry in	*/
UR_TUPLE_INDX 	*tuple_indx;	/* the last active entry				*/
{
	int					status;		/* status, -1 if error, 0 otherwise		*/
	int					i,j;			/* indexs										*/
	struct UR_rcb_rec	*rcb_ptr;	/* ptr to relation control block		*/
	int 					last_ent;	/* last active entry in rel				*/
	unsigned long		mask;			/* generate mask for position in bitmap*/

	uu_denter(UU_RTRC,(us,"ur_get_next_tuple_index(rel=%d , start index=%d)",
							rel_num,*tuple_indx));
	status = 0;
	if(rel_num <= UR_MAX_REL && *tuple_indx > 0)
	{
		if(UR_rcb[rel_num].status >= 0)
		{
			rcb_ptr = &UR_rcb[rel_num];

			last_ent = UR_rcb[rel_num].last_active_index;	/* set last entry */

			/* make sure start entry is before or at last entry */
			if(*tuple_indx <= last_ent)
			{
				/* search from tuple_indx passed to us, until the last entry */
				i = *tuple_indx;
l10:
				while((uu_tst_bit(rcb_ptr->bmap_ptr,i-1) == 0) && !status)
				{
					if(i < last_ent)
					{
						i++;
					}
					else
					{
						i = 0 ;
						status = -1 ;/* ran off the end, no more active tuples*/
					}
				}
				/* i = active index, make sure not a phantom tuple from delete */
				*tuple_indx = i;
				if(ur_del_test(rel_num,i))
				{
					i++;
					goto l10;
				}
			}
			else	/* attempted to start search after the last active tuple */
			{
				*tuple_indx = 0;
				status  = -1;
			}
		}
		else
		{
			*tuple_indx = 0;	/* inactive relation, never initialized */
			status = -1;
		}
	}
	else
	{
		status = -1;	/* illegal relation, or illegal index */
	}
	uu_dexit;
	return(status);
}
