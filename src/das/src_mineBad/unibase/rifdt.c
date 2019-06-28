/*********************************************************************
**    NAME         :  rifdt.c
**       CONTAINS:
**       ur_force_delete_tuple()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rifdt.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:43
*********************************************************************/

#include "usysdef.h"
#include "ribase.h"
#include "mattrddl.h"
#include	"udebug.h"

/*********************************************************************
**    I_FUNCTION     :  status = ur_force_delete_tuple(rel_num,tuple_indx)
**      delete a tuple in the specified relation
**    PARAMETERS   
**       INPUT  : 
**				rel_num, the number identifying the relation
**				tuple_indx,	index into the relation
**       OUTPUT :  
**          none
**    RETURNS      :  -1, if operation was unsuccessful, 0 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_force_delete_tuple(rel_num,tuple_indx)
UR_REL_NUM		rel_num;		/* the relation to delete the tuple in	*/
UR_TUPLE_INDX	tuple_indx;	/* the index of the tuple to delete		*/
{
	int					status;		/* return status						*/
	int					i, j;			/* indexs								*/
	struct UR_rcb_rec	*rcb_ptr;	/* ptr to relation control block	*/
	struct UR_attr		*attr_ptr;	/* pointer to an attribute bundle*/
	struct UM_transf_rec	*transf_ptr;/* ptr to a transformation tuple	*/
	UU_REL_ID			rel_key;		/* a relation,tuple index key		*/
	int					lst_len;

	uu_denter(UU_RITRC,(us,"ur_force_delete_tuple(rel=%d, tuple=%d",
				rel_num, tuple_indx));
	status = 0;								/* initialize return status */
	if(tuple_indx > 0)
	{
		rcb_ptr = &UR_rcb[rel_num];	/* get ptr to rel control blk	*/

		/* check for active and legal rel,ent to delete */
		if(tuple_indx <= rcb_ptr->n_ent &&
			uu_tst_bit(rcb_ptr->bmap_ptr, tuple_indx-1))
		{
			ur_rt2k(rel_num,tuple_indx,&rel_key) ;
			if(UR_last_mod_mkey == rel_key) UR_last_mod_mkey = 0;
			if(UR_last_mod_rkey == rel_key) UR_last_mod_rkey = 0;

			/* if this type of relation has variable lists, delete each one */
			if(rcb_ptr->n_varl > 0)
			{
				for ( i = 1; i <= rcb_ptr->n_varl; i++)
				{
					ur_delete_tuple_varlist(rel_num,tuple_indx,i);
				}
			}	
			if(uu_tst_bit(&(UR_rcb[rel_num].rel_flags),UR_DATA_REL))
			{
				/* deallocate the tuple if a data relation */
				ur_deallocate_rel_tuple(rel_num,tuple_indx) ;
				if(UR_rcb[rel_num].last_accessed_index == tuple_indx)
					UR_rcb[rel_num].last_accessed_index = 0	;
			}
			else if(uu_tst_bit(&(UR_rcb[rel_num].rel_flags),UR_ATTR_REL))
			{
				/* process delete an attribute bundle */
				status = ur_get_varlist_ptr(rel_num, tuple_indx, 0, &attr_ptr,
											&lst_len);
				if(status == 0)
				{
					ur_deallocate_rel_tuple(rel_num,tuple_indx);
					if(UR_rcb[rel_num].last_accessed_index == tuple_indx)
						UR_rcb[rel_num].last_accessed_index = 0;
				}
				else
				{
					status = -1;	/* set didn't delete status */
				}
			}
			else if(uu_tst_bit(&(UR_rcb[rel_num].rel_flags),UR_TRANSF_REL))
			{
				/* process delete a transformation tuple */
				status = ur_get_varlist_ptr(rel_num, tuple_indx,0,&transf_ptr,
						&lst_len) ;
				if(status == 0)
				{
					ur_deallocate_rel_tuple(rel_num,tuple_indx) ;
					if(UR_rcb[rel_num].last_accessed_index == tuple_indx)
						UR_rcb[rel_num].last_accessed_index = 0;
				}
				else
				{
					status = -1;	/* set didn't delete status	*/
				}
			}
		}
		else
		{
			status	=	-1;		/* attempted to delete non-existent entry	*/
		}
	}
	else			/* illegal entry specified	*/
	{
		status = -1;
	}
	uu_dexit;
	return(status);
}
