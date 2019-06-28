/*********************************************************************
**	 NAME			:  rinewpag.c
**		 CONTAINS:
**		 ur_new_page()
**	 COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**	 MODULE NAME AND RELEASE LEVEL
**       rinewpag.c , 25.1
**	 DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:45
*********************************************************************/

#include "usysdef.h"
#include "rbase.h"
#include "ribase.h"
#include "udebug.h"
#include "umoveb.h"

/*********************************************************************
**	 I_FUNCTION	  :  status = ur_new_page(rel)
**		allocate a new page to contain entries for the specified relation
**	 PARAMETERS
**		 INPUT  :
**				rel, relation for which a new page is desired
**		 OUTPUT :
**				none
**	 RETURNS		:  0 if function successful, -1 otherwise
**	 SIDE EFFECTS : memory allocated for this relation
**	 WARNINGS	  : Do NOT depend on any pointers having to do with
**							this relation!
*********************************************************************/

ur_new_page(rel)
UR_REL_NUM	rel;
{
   char           *uu_toolmalloc();    /* allocate memory function */
   int            status;              /* status, -1 if error, 0 otherwise */
   UR_rcb_rec		*rcb_ptr; 	       	/* pointer to relation control block */
	int				whole_pages;			/* # of whole pages in relation */
	int				cur_p_size;				/* current size of page table in bytes */
	char				*new_p_ptr;				/* new page (table) pointer */
   int            cur_b_size;          /* current bit map size in WORDS */
   int            expand_ent;          /* number of entries added in expand */
   int            new_d_size;          /* new data size in BYTES */
   int            new_b_size;          /* new bit map size in WORDS */
   char           *d_ptr;              /* pointer to new data area */
   unsigned int   *nbmap_ptr;          /* pointer to new bitmap area */
   int            i,j;                 /* indexs */

	uu_denter(UU_RITRC,(us,"ur_new_page(%d)",rel));
	status = 0;
	rcb_ptr = &UR_rcb[rel];

	whole_pages = (int)(rcb_ptr->n_ent / rcb_ptr->page_size);
	if (rcb_ptr->ent_ptr != UU_NULL)		/* check if page table exists */
	{
		/* get current page table size and bump by one UR_page entry */
		cur_p_size = whole_pages * sizeof(UR_page);
		new_p_ptr = uu_toolmalloc(cur_p_size + sizeof(UR_page));
		if (new_p_ptr != UU_NULL)
		{
			uu_dprint(UU_RITRC,(us,"page table reallocated at 0x%x",new_p_ptr));

			/* copy the old table info */
			uu_move_byte(rcb_ptr->ent_ptr, new_p_ptr, cur_p_size);
			uu_toolfree(rcb_ptr->ent_ptr);
			rcb_ptr->ent_ptr = (UR_page *)new_p_ptr;
		}
		else										/* can't allocate new table space */
		{
			uu_dprint(-1,(us,"ERROR:ur_new_page allocation failure."));
			uu_dexit;
			return(-1);
		}
	}
	else											/* no page table */
	{
		new_p_ptr = uu_toolmalloc(sizeof(UR_page));
		if (new_p_ptr != UU_NULL)
		{
			uu_dprint(UU_RITRC,(us,"new page table at 0x%x",new_p_ptr));
			rcb_ptr->ent_ptr = (UR_page *)new_p_ptr;
		}
		else										/* can't allocate new table space */
		{
			uu_dprint(-1,(us,"ERROR:ur_new_page allocation failure."));
			uu_dexit;
			return(-1);
		}
	}
	/* the new page table is setup. allocate entry space */
	cur_b_size = rcb_ptr->bmap_size;

	/* calc the expanded # of entries, and the mem required for them */
	expand_ent = rcb_ptr->init_ent;
	new_d_size = expand_ent * (rcb_ptr->tuple_size + rcb_ptr->var_info_size);
	new_b_size = cur_b_size + (expand_ent / UU_BITPW);

	/* first get all necessary space for expansion */
	d_ptr = uu_toolmalloc(new_d_size);
	if(d_ptr != 0)
	{
		/* get space for all bit maps & clear'em */
		nbmap_ptr = (unsigned int *) uu_toolmalloc(new_b_size *
														UR_NUM_BMAPS * sizeof(int));
		if(nbmap_ptr != 0)
		{  
			for(i = 0; i < UR_NUM_BMAPS; i++)
				for(j = 0; j < new_b_size; j++)
				{
					nbmap_ptr[(i*new_b_size)+j] = 0;
				}
		}
	}
	/* if we got all the required storage, then copy the bit map, */
	/* otherwise error */
	if(d_ptr != 0 && nbmap_ptr != 0)
	{
		uu_dprint(UU_RITRC,(us,"new data at 0x%x",d_ptr));
		rcb_ptr->ent_ptr[whole_pages].pg_ptr = d_ptr;
		if (rcb_ptr->bmap_ptr != UU_NULL)
		{
			for(i = 0; i < UR_NUM_BMAPS; i++)
			{
				uu_move_byte(&(rcb_ptr->bmap_ptr[i*cur_b_size]),
								(char *)&(nbmap_ptr[i*new_b_size]),
								cur_b_size*sizeof(int));
			}
			uu_toolfree(rcb_ptr->bmap_ptr);
		}
		rcb_ptr->bmap_ptr = nbmap_ptr;
		rcb_ptr->bmap_size = new_b_size;
		rcb_ptr->n_ent = rcb_ptr->n_ent + expand_ent;
		uu_dprint(UU_RITRC,(us," %d entries added",expand_ent));
	}
	uu_dexit;
	return(status);
}

