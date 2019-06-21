/*********************************************************************
**	 NAME			:  rixpand.c
**		 CONTAINS:
**		 ur_expand_rel()
**	 COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**	 MODULE NAME AND RELEASE LEVEL
**       rixpand.c , 25.1
**	 DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:49
*********************************************************************/

#include "usysdef.h"
#include "rbase.h"
#include "ribase.h"
#include "udebug.h"
#include "umoveb.h"

/*********************************************************************
**	 I_FUNCTION	  :  status = ur_expand_rel(rel)
**		allocate more space for entries in the specified relation
**	 PARAMETERS
**		 INPUT  :
**				rel, relation for which an entry id is to be allocated
**		 OUTPUT :
**				none
**	 RETURNS		:  0 if function successful, -1 otherwise
**	 SIDE EFFECTS : memory allocated for this relation and existing
**							entries may be moved.
**	 WARNINGS	  : Do NOT depend on any pointers having to do with
**							this relation!
*********************************************************************/

ur_expand_rel(rel)
UR_REL_NUM	rel;
{
   char           *uu_toolmalloc();    /* allocate memory function */
   int            status;              /* status, -1 if error, 0 otherwise */
   int            cur_d_size;          /* current data size in BYTES */
   int            cur_b_size;          /* current bit map size in WORDS */
   UR_rcb_rec		*rcb_ptr; 	       	/* pointer to relation control block */
   int            expand_ent;          /* number of entries added in expand */
   int            new_d_size;          /* new data size in BYTES */
   int            new_b_size;          /* new bit map size in WORDS */
   char           *d_ptr;              /* pointer to new data area */
   unsigned int   *nbmap_ptr;          /* pointer to new bitmap area */
   int            i,j;                 /* indexs */
	int				whole_pages;			/* # of whole pages in relation */

	uu_denter(UU_RITRC,(us,"ur_expand_rel(%d)",rel));
	status = 0;
	rcb_ptr = &UR_rcb[rel];

	/* see if this expansion should start a new page (# of entries is multiple */
	/* of the page size) If so then we should add a new page */
	whole_pages = (int)(rcb_ptr->n_ent / rcb_ptr->page_size);
	if (whole_pages * rcb_ptr->page_size == rcb_ptr->n_ent ||
			rcb_ptr->ent_ptr == UU_NULL)
	{
		status = ur_new_page(rel);
		uu_dexit;
		return(status);
	}

	/* get cur data size in bytes for the current page of this relation, */
	/* and cur bit map size in WORDS */
	cur_d_size = (rcb_ptr->n_ent - whole_pages * rcb_ptr->page_size) *
					(rcb_ptr->tuple_size + rcb_ptr->var_info_size);
	cur_b_size = rcb_ptr->bmap_size;

	/* calc the expanded # of entries, and the mem required for them */
	expand_ent = rcb_ptr->init_ent;
	new_d_size = cur_d_size + (expand_ent *
					(rcb_ptr->tuple_size + rcb_ptr->var_info_size));
	new_b_size = cur_b_size + (expand_ent / UU_BITPW);

	/* first get all necessary space for expansion */
	d_ptr = uu_toolmalloc(new_d_size);
	if(d_ptr != 0) /* get space for all bit maps & clear'em */
	{
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
	/* if we got all the required storage, then copy the data and */
	/* bit map, otherwise error */
	if(d_ptr != 0 && nbmap_ptr != 0)
	{
		uu_move_byte(rcb_ptr->ent_ptr[whole_pages].pg_ptr, d_ptr, cur_d_size);
		uu_toolfree(rcb_ptr->ent_ptr[whole_pages].pg_ptr);
		rcb_ptr->ent_ptr[whole_pages].pg_ptr = d_ptr;
		for(i = 0; i < UR_NUM_BMAPS; i++)
		{
			uu_move_byte(&(rcb_ptr->bmap_ptr[i*cur_b_size]),
							(char *)&(nbmap_ptr[i*new_b_size]),
							cur_b_size*sizeof(int));
		}
		uu_toolfree(rcb_ptr->bmap_ptr);
		rcb_ptr->bmap_ptr = nbmap_ptr;
		rcb_ptr->bmap_size = new_b_size;
		rcb_ptr->n_ent = rcb_ptr->n_ent + expand_ent;
		uu_dprint(UU_RITRC,(us," %d entries added",expand_ent));
	}
	else		/* didn't get needed space */
	{
		status = -1;
		uu_dprint(-1,(us,"ERROR:ur_expand_rel failed to allocate space"));
	}
	uu_dexit;
	return(status);
}
