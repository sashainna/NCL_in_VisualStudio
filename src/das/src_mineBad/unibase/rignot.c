/*********************************************************************
**    NAME         :  rignot.c
**       CONTAINS:
**       ur_get_next_old_tuple()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rignot.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:44
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) rignot.c 2.1 9/5/86 09:20:12 single"};
#else
static char uu_sccsident[]={"@(#) rignot.c 2.1 9/5/86 09:20:12 double"};
#endif
#endif

#include	"usysdef.h"
#include	"ribase.h"
#include	"udebug.h"

/*********************************************************************
**    E_FUNCTION     :  status = ur_get_next_old_tuple(rel_num,&tuple_indx)
**      get the index for the next new tuple within a relation
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

ur_get_next_old_tuple(rel_num,tuple_indx)
UR_REL_NUM 		rel_num;			/* rel to find last active entry in	*/
UR_TUPLE_INDX 	*tuple_indx;	/* where to start search */
{
	int				status;		/* status, -1 if error, 0 otherwise		*/
	int				i,j;			/* indexs										*/
	int 				last_ent;	/* last active entry in rel				*/
	unsigned	long	mask;			/* generate mask for position in bitmap*/
	long				svld_indx;	/* index to save/load map					*/

	uu_denter(UU_RTRC,(us,"ur_get_next_old_tuple(rel=%d, start index=%d)",
				rel_num, *tuple_indx));
	status = 0;
	if(rel_num <= UR_MAX_REL && *tuple_indx > 0)
	{
		if(UR_rcb[rel_num].status >= 0)
		{
			/* set last entry */
			last_ent = UR_rcb[rel_num].last_active_index;

			/* make sure start entry is before or at last entry */
			if(*tuple_indx <= last_ent)
			{
				/* start seach from the value of tuple_indx passed to us, */
				/* until the last entry */
				i = *tuple_indx;
				svld_indx = UR_rcb[rel_num].bmap_size * UR_SVLD_MAP;
				while((uu_tst_bit(&(UR_rcb[rel_num].bmap_ptr[svld_indx]),
										i-1) != 0) && !status)
				{
					if(i < last_ent)
					{
						i++;
					}
					else
					{
						i = 0;
						status = -1;/* ran off the end, no more active tuples*/
					}
				}
				*tuple_indx = i;
			}
			else	/* attempted to start search after the last active tuple */
			{
				*tuple_indx = 0;
				status  = -1;
			}
		}
		else
		{
			*tuple_indx = 0; /* inactive relation, never initialized */
			status = -1;
		}
	}
	uu_dexit;
	return(status);
}
