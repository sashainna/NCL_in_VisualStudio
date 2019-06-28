#define MODULEDBG 0
/*********************************************************************
**    NAME         :  riart.c
**       CONTAINS:
**       ur_alloc_rel_tuple()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       riart.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:41
*********************************************************************/

#include	"usysdef.h"
#include	"rbase.h"
#include	"ribase.h"
#include	"udebug.h"
#include "umoveb.h"

/*********************************************************************
**    I_FUNCTION     :  status = ur_alloc_rel_tuple(rel,&tuple)
**      allocate an entry id for a specified relation
**    PARAMETERS   
**       INPUT  : 
**			rel, relation for which an entry id is to be allocated
**       OUTPUT :  
**         tuple, entry within the relation which has been 
**							allocated
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_alloc_rel_tuple(rel,tuple)
UR_REL_NUM		rel;			/* rel for which an entry is requested	*/
UR_TUPLE_INDX 	*tuple;	/* entry allocated for use					*/
{
	int				status;			/* status, -1 if error, 0 otherwise */
	int				i,					/* index of bitmap word */
						j;					/* index of bit in word */
	struct
	UR_rcb_rec		*rcb_ptr;		/* pointer to relation control block */
	int				no_hit;			/* signal if an entry has been found */
	unsigned	int 	temp;				/* holds word of bit map */
	unsigned int	mask;				/* locator for avail entity storage */
	int 				entry_ct;		/* count of entries used */

	uu_denter(UU_RITRC,(us,"ur_alloc_rel_tuple(rel=%d)",rel));
	status = 0;
	rcb_ptr = &UR_rcb[rel];
#if MODULEDBG != 0
	if(rcb_ptr->status < 0)					/* relation initialized? */
	{
		status = -1;
		uu_dprint(-1, (us,
			"ERROR ur_alloc_rel_tuple:attempt allocation from uninitialized rel."));
		uu_dexit;
		return(status);
	}
#endif

	entry_ct = 1;
	no_hit = UU_TRUE;
	i = 0;
	while (no_hit && (status == 0))
	{
		/* this algorithum uses the fact that the allocation bit map */
		/* is bit map 0, in other words that UR_ALLOC_MAP = 0 */
		for ( ; (no_hit && i < rcb_ptr->bmap_size); i++)
		{
			temp = ~ (rcb_ptr->bmap_ptr[i]);	/* complement of a bit map word */
			if(temp == 0)					/* all entries used, nothing avaiable */
			{
				entry_ct += UU_BITPW;
			}
			else	/* an open bit therefore an available entry */
			{
				/* walk a mask across the word til we find it */
				mask = 1;	/* define the mask */
				for (j = 0; (no_hit && j < UU_BITPW); j++)		
				{
					if ((mask & temp) == 0)
					{
						mask <<= 1	;	/* shift one to the left,	*/
						entry_ct++	;	/* and bump the count		*/
					}
					else	/* we found an entry */
					{
						no_hit = UU_FALSE;
						rcb_ptr->bmap_ptr[i] |= mask;	/* allocate it */
						/* set last active entry for this relation */
						if(UR_rcb[rel].last_active_index < entry_ct)
								UR_rcb[rel].last_active_index = entry_ct;
						UR_rcb[rel].active_tuple_cnt++; /* bump # of active tuples */
						*tuple = entry_ct;
					}
				}	/* for each bit in bitmap word */
			}	/* if - else one available */
		}	/* for each word in bitmap */
		if(no_hit)				/* did we get an entry? */
		{
			/* if not, then expand */
			status = ur_expand_rel(rel);
		}
	}	/* while no_hit & status 0 */
	if(no_hit)				/* did we get an entry? */
	{
		/* fell out with bad status */
		*tuple = 0;
		uu_dprint(-1,(us, "ERROR ur_alloc_rel_tuple:unable to expand relation"));
	}
	uu_dprint(UU_RITRC,(us,"ur_alloc_rel_tuple: entry %d allocated for rel %d",
						*tuple,rel));
	uu_dexit;
	return(status);
}

