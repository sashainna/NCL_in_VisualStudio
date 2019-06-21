/*********************************************************************
**    NAME         :  ridrt.c
**       CONTAINS:
**       ur_deallocate_rel_tuple
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ridrt.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:42
*********************************************************************/

#include	"usysdef.h"
#include "rbase.h"
#include	"ribase.h"
#include	"udebug.h"

/*********************************************************************
**    I_FUNCTION     :  ur_deallocate_rel_tuple(rel_num,tuple_indx)
**      deallocate a tuple for a specified relation
**    PARAMETERS   
**       INPUT  : 
**			rel_num, relation in which the tuple should be released
**			tuple_indx, entry within relation to be release
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : resets all bit map settings(del & svld) for this tuple
**    WARNINGS     : none
*********************************************************************/

ur_deallocate_rel_tuple(rel_num,tuple_indx)
UR_REL_NUM 		rel_num;		/* rel to release entry in				*/
UR_TUPLE_INDX 	tuple_indx;	/* entry to be released					*/
{
	int	status;					/* status, -1 if error, 0 otherwise		*/
	int	i_word,j;				/* indexs										*/
	struct UR_rcb_rec *rcb_ptr;/* pointer to relation control block	*/
	unsigned	long 	temp;			/* holds word of bit map					*/
	unsigned	long	mask;			/* generate mask for position in bitmap*/
	long		temp_indx;			/* index to use into bitmap				*/

	uu_denter(UU_RITRC,(us,"ur_deallocate_rel_tuple(rel=%d, tuple=%d)",
						rel_num,tuple_indx));
	if(tuple_indx > 0)
	{
		rcb_ptr = &UR_rcb[rel_num];

		/* calculate the i'th word, j'th bit in the bit map */
		temp_indx = tuple_indx - 1;
		i_word =	temp_indx / UU_BITPW;
		j	= temp_indx - (i_word * UU_BITPW);
		if(temp_indx <= rcb_ptr->n_ent)			/* if legal ent num proceed */
		{
			temp = (rcb_ptr->bmap_ptr[i_word]);	/* get a bit map word*/
			mask = 1 << j;								/* generate mask bit */
			if(temp & mask)/* if bit set, clear and return good status	*/
			{
				/* clear the allocate bit in the allocation bit map, and */
				/* the save/load bit and the marked as delete map */
				rcb_ptr->bmap_ptr[i_word] &= ~mask;	/* clr the allocate bit */
				rcb_ptr->bmap_ptr[i_word+(UR_SVLD_MAP*rcb_ptr->bmap_size)] &= ~mask;
				rcb_ptr->bmap_ptr[i_word+(UR_DEL_MAP*rcb_ptr->bmap_size)] &= ~mask;
				rcb_ptr->active_tuple_cnt--;		/* one less active tuple*/
				status = 0;								/* send back good status*/

				/* if tuple index was the last active index, then we must go */
				/* back and find the new last active index */
				if(tuple_indx == UR_rcb[rel_num].last_active_index)
				{
					/* we know the i'th word in the bitmap that last_tuple_index */
					/* was in, so search back for last non-zero word in the map */
					while (i_word >= 0 && rcb_ptr->bmap_ptr[i_word] == 0)
					{
						i_word--;
					}
					/* if i >= 0, a non-zero word was found, */
					/* otherwise no entries for this relation */
					if( i_word >= 0)
					{
						/* now find the last bit set in the non-zero word */
						j	= 	UU_BITPW;
						mask = UR_HIGH_BIT;
						while(((rcb_ptr->bmap_ptr[i_word] & mask) == 0) && (j > 0))
						{
							j--;
							mask = mask >> 1;
						}
						UR_rcb[rel_num].last_active_index = (i_word * UU_BITPW) + j;
					}
					else	/* this rel no longer has any active tuples */
					{
						UR_rcb[rel_num].last_active_index = 0;
					}
				}
			}
			else
			{
				status	=	-1;	/* attempted to delete non-active ent	*/
			}
		}
		else
		{
			status = -1;		/* ent specified is to large, non-existent	*/
		}
	}
	else
	{
		status = -1;			/* illegal entry specified */
	}
	uu_dexit;
	return(status);
}
