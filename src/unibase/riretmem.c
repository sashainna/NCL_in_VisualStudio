/*********************************************************************
**    NAME         :  riretmem.c
**       CONTAINS:
**       uri_return_mem()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       riretmem.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:47
*********************************************************************/

#include "udebug.h"
#include "ribase.h"

/*********************************************************************
**    E_FUNCTION     :  uri_return_mem()
**			return allocated memory in use by Unibase
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : deallocates memory for all relations and lists
**    WARNINGS     : none
*********************************************************************/

uri_return_mem()
{
	int		status;				/* holds status of unibase calls */
	long		rel;					/* relation counter */
	long		nxt_ent;				/* next entry in relation */
	int		i;						/* indexes */
	unsigned int	*w_ptr;		/* a pointer to an integer */
	int		lst_len;				/* length of a list in bytes */
	int		whole_pages;		/* # of whole pages of entries */

	uu_denter(UU_RTRC,(us,"uri_return_mem()"));
	status = 0;
	for (rel = 0; rel <= UR_MAX_REL; rel++)
	{
		/* first determine if initialized relation with active tuples */
		if(UR_rcb[rel].status >= 0 && UR_rcb[rel].active_tuple_cnt > 0)
		{
			/* first return any memory used by varlists */
			for(i = 1 ; i <= UR_rcb[rel].n_varl; i++)
			{
				/* step through the active entries and return each list */
				nxt_ent	= 1	;
				status = 0 ;
				while(!ur_get_next_tuple_index(rel,&nxt_ent))
				{
					status = ur_get_varlist_ptr(rel, nxt_ent,i, &w_ptr, &lst_len);
					if(lst_len > 0 && status == 0)
					{
						uu_toolfree(w_ptr);
					}	/* if list not 0 length */
					nxt_ent++ ;
				}	/* while got active tuple */
			} /* for each variable list */
			/* , but keep the list parameters */
			/* next trash the actual entries */
			whole_pages = (int)(UR_rcb[rel].n_ent / UR_rcb[rel].page_size);
			for (i = 0; i < whole_pages; i++)
			{
				uu_toolfree(UR_rcb[rel].ent_ptr[i].pg_ptr);
			}
			if (whole_pages * UR_rcb[rel].page_size == UR_rcb[rel].n_ent)
				uu_toolfree(UR_rcb[rel].ent_ptr[whole_pages].pg_ptr);
			uu_toolfree(UR_rcb[rel].ent_ptr);
			uu_toolfree(UR_rcb[rel].bmap_ptr); /* and finally the allocation bitmaps */
		}
	}
	uu_dprint(UU_RITRC,(us,"uri_return_mem: memory free'd."));
	uu_dexit;
	return(status);
}
