/*********************************************************************
**    NAME         :  ridt.c
**       CONTAINS:
**       ur_delete_tuple()
**       ur_delete_tuple_abs()
**       ur_pull_del_stack()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ridt.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:42
*********************************************************************/

#include	"usysdef.h"
#include	"udebug.h"
#include	"ribase.h"
#include	"rstack.h"
#include	"mattrddl.h"

/*********************************************************************
**    I_FUNCTION     :  status = ur_delete_tuple(rel,tuple)
**      delete a tuple in the specified relation
**    PARAMETERS   
**       INPUT  : 
**				rel,		the number identifying the relation
**				tuple,	index into the relation
**       OUTPUT :  
**          none
**    RETURNS      :  -1, if operation was unsuccessful, 0 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

extern	UR_STACK(UR_del_stack, UR_DEL_STK_DEPTH, dstack_element)	;

ur_delete_tuple(rel, tuple)
UR_REL_NUM		rel;		/* the relation to delete the tuple in	*/
UR_TUPLE_INDX	tuple;	/* the index of the tuple to delete		*/
{
	int					status;			/* return status */
	struct UR_rcb_rec	*rcb_ptr;		/* ptr to relation control block	*/
	UU_REL_ID			rel_key;			/* a relation, tuple index key */
	dstack_element		*dstk_ptr;		/* pointer to delete stack */
	struct UR_attr		*attr_ptr;		/* ptr to attribute bundle */
	int					lst_len;			/* length of tuple */

	uu_denter(UU_RTRC, (us, "ur_delete_tuple(rel=%d, tuple=%d)",
				rel, tuple));
	status = 0;		/* initialize return status */
	ur_unibase_change(rel);
	if(UR_del_stack_enabled) 
	{
		rcb_ptr = &UR_rcb[rel];	/* get ptr to rel control blk	*/
/*
.....vp 3/3/98 don't try delete when there is nothing
.....to delete in rcb tuple (gives error with 0 bitmap pointer)
		if(tuple > 0) 
*/
		if(tuple > 0 && rcb_ptr->n_ent > 0)
		{

			/* check for active and legal rel, ent to delete */
			if(uu_tst_bit(rcb_ptr->bmap_ptr, tuple-1) &&
				tuple <= rcb_ptr->n_ent)
			{
				ur_rt2k(rel, tuple, &rel_key);
				if(UR_last_mod_mkey == rel_key) UR_last_mod_mkey = 0;
				if(UR_last_mod_rkey == rel_key) UR_last_mod_rkey = 0;

				/* if stack full, clear out from the bottom of the stack */
				/* until the next marked element */
				if(ur_stack_space(UR_del_stack) < 1)
				{
					ur_pull_del_stack();
				}
				/* now put the newest item on the stack */
				dstk_ptr = ur_stack_top(UR_del_stack);
				dstk_ptr->del_key = rel_key;
				dstk_ptr->mark = UR_del_mark;
				UR_del_mark = UU_FALSE;		/* set mark false no matter what*/
				ur_push(UR_del_stack);

				/* if a data tuple, set as deleted if attribute tuple or */
				/* transformation tuple do nothing */
				if(uu_tst_bit(&(UR_rcb[rel].rel_flags), UR_DATA_REL))
				{
					status = ur_del_set(rel, tuple);
					if(UR_rcb[rel].last_accessed_index == tuple)
					{
						UR_rcb[rel].last_accessed_index = 0;
					}
				}
			} /* if active tuple	*/
			else
			{
				status = -1;		/* attempted to delete non-existent tuple	*/
			}
		}
		else
		{
			status = -1;			/* illegal entry specified	*/
		}
	} /* delete stack enabled		*/
	else
	{ /* delete stack not enabled	*/
		status = ur_delete_tuple_abs(rel, tuple);
	}
	UR_del_started = UU_TRUE;	/* mark delete series going */
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ur_delete_tuple_abs
**       absolutely, positively attempt the delete of a tuple
**    PARAMETERS   
**       INPUT  : 
**			rel, the number identifing the relation
**			tuple, index into the relation
**       OUTPUT :  
**          output
**    RETURNS      : 0 if successful, -1 otherwise
**    SIDE EFFECTS : all bit maps for this tuple will be reset
**    WARNINGS     : none
*********************************************************************/

ur_delete_tuple_abs(rel, tuple)
UR_REL_NUM		rel;		/* the relation to delete tuple in */
UR_TUPLE_INDX	tuple;	/* the index of the tuple to delete	*/
{
	int						status;		/* return status						*/
	int						i;				/* indexs								*/
	struct	UR_rcb_rec	*rcb_ptr;	/* ptr to relation control block	*/
	struct	UR_attr		*attr_ptr;	/* pointer to an attribute bundle*/
	struct	UM_transf_rec	*transf_ptr;/* ptr to a transformation tuple	*/
	int						lst_len;
	UU_KEY_ID key;

	uu_denter(UU_RTRC, (us, "ur_delete_tuple_abs(rel=%d, tuple=%d)",
				rel, tuple));
	status = 0;			/* initialize return status */
	rcb_ptr = &UR_rcb[rel] ;/* get ptr to rel control blk	*/
/*
.....vp 3/3/98 don't try delete when there is nothing
.....to delete in rcb tuple (gives error with 0 bitmap pointer)
	if(tuple >0) 
*/
	if(tuple > 0 && rcb_ptr->n_ent > 0)
	{

		/* check for active and legal rel, ent to delete */
		if(uu_tst_bit(rcb_ptr->bmap_ptr, tuple-1) &&
			tuple <= rcb_ptr->n_ent)
		{
			ur_del_reset(rel, tuple);	/* get deleted bit out of the way */

			/* reset last_mod if deleted by other means */
			ur_rt2k(rel,tuple,&key);
			if(UR_last_mod_mkey == key) UR_last_mod_mkey = 0;
			if(UR_last_mod_rkey == key) UR_last_mod_rkey = 0;

			/* if this type of relation has variable lists, delete each one */
			if(rcb_ptr->n_varl > 0)
			{
				for ( i = 1; i <= rcb_ptr->n_varl; i++)
				{
					ur_delete_tuple_varlist(rel, tuple, i);
				}
			}	
			/* deallocate the tuple if a data relation */
			if(uu_tst_bit(&(UR_rcb[rel].rel_flags), UR_DATA_REL))
			{
				ur_deallocate_rel_tuple(rel, tuple);
				if(UR_rcb[rel].last_accessed_index == tuple)
					UR_rcb[rel].last_accessed_index = 0;
			}
			else if(uu_tst_bit(&(UR_rcb[rel].rel_flags), UR_ATTR_REL))
			{
				/* process delete an attribute bundle */
				status = ur_get_varlist_ptr(rel, tuple, 0,
												&attr_ptr, &lst_len);
				if(status == 0)
				{
					attr_ptr->use_count--;
					if(attr_ptr->use_count <= 0)
					{
						ur_deallocate_rel_tuple(rel, tuple);
						if(UR_rcb[rel].last_accessed_index == tuple)
						{
							UR_rcb[rel].last_accessed_index = 0	;
						}
						status = 0;
					}
				}
				else
				{
					status = -1;	/* set didn't delete status */
				}
			}
			/* process delete a transformation tuple */
			else if(uu_tst_bit(&(UR_rcb[rel].rel_flags), UR_TRANSF_REL))
			{
				status = ur_get_varlist_ptr(rel, tuple, 0, &transf_ptr,
						&lst_len) ;
				if(status == 0)
				{
					if(transf_ptr->use_count > 0)
					{
						transf_ptr->use_count--	;
					}
					/* do the actual deallocation if the tuple is no longer */
					/* referenced, and it is not the reserved default tuple */
					if(transf_ptr->use_count <= 0 && tuple > 1)
					{
						ur_deallocate_rel_tuple(rel, tuple);
						if(UR_rcb[rel].last_accessed_index == tuple)
						{
							UR_rcb[rel].last_accessed_index = 0;
						}
						status = 0;
					}
				}
				else
				{
					status = -1 ;	/* set didn't delete status	*/
				}
			} /* if TRANSF_REL */
		} /* if active tuple	*/
		else
		{
			status	=	-1	;	/* attempted to delete non-existent entry	*/
		}
	} /* if legal tuple */
	else			/* illegal entry specified	*/
	{
		status = -1;
	}
	uu_dexit;
	return(status);
}
/*********************************************************************
**    I_FUNCTION     :  status = ur_pull_del_stack()
**		pull elements fromn the bottom of the stack until the next mark
**		or the top of the stack
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**          none
**    RETURNS      :  -1, if operation was unsuccessful, 0 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_pull_del_stack()
{
	long				stack_indx;			/* index into the delete stack		*/
	long				top_of_del;			/* index to the top element to del	*/
	dstack_element	*dstk_ptr;			/* pointer to delete stack		*/
	UU_KEY_ID		del_key;				/* key of actual tuple to be deleted*/
	UR_REL_NUM		del_rel;		/* rel number of tuple to be deleted*/
	UR_TUPLE_INDX	del_tuple;	/* tuple indx of tuple to be deleted*/

	uu_denter(UU_RTRC, (us, "ur_pull_del_stack()"));

	/* first determine the bounds to delete within, the bottom of the */
	/* stack should always be marked so start from element 1 */
	top_of_del = 1 ;
	dstk_ptr = &(UR_del_stack.data[1]);
		
	while(dstk_ptr->mark == UU_FALSE && top_of_del <= UR_del_stack.curpos)
	{
		dstk_ptr++ ;
		top_of_del++ ;
	}
	uu_dprint(UU_RITRC, (us, "del stack full, remove %d elements", top_of_del));
	dstk_ptr = &(UR_del_stack.data[0]) ;
	stack_indx = 0 ;
	while(stack_indx < top_of_del)
	{
		del_key = dstk_ptr->del_key ;
		ur_k2rt(del_key, &del_rel, &del_tuple) ;
		ur_delete_tuple_abs(del_rel, del_tuple) ;
		dstk_ptr++ ;
		stack_indx++ ;
	} /* stack_indx < top_of_del	*/

	/* if stack was completely flushed just reset the current position, */
	/* otherwise move the remainder of the stack down */
	if(top_of_del >= UR_del_stack.curpos)
	{
		uu_dprint(UU_RITRC, (us, "stack emptied"));
		UR_del_stack.curpos = -1 ;
		ur_set_del_mark()	;	/* make sure delete mark gets reset	*/

		/* turn off stacking if already under way */
		if (UR_del_started)
		{
			uu_dprint(UU_RITRC, (us, "stack emptied while underway - not saving"));
			ur_disable_del_stack();
		}
	}
	else
	{
		for(stack_indx = 0;
			stack_indx < (UR_del_stack.curpos-top_of_del);
			stack_indx++)
		{
			UR_del_stack.data[stack_indx] =
									UR_del_stack.data[stack_indx+top_of_del];
		}
		uu_dprint(UU_RITRC, (us, "curpos is %d, will be %d",
				UR_del_stack.curpos, UR_del_stack.curpos-top_of_del));
		UR_del_stack.curpos -= top_of_del;
	}
	uu_dexit;
	return 0;
}
