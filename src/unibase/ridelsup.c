#define MODULEDBG 0
/*********************************************************************
**    NAME         :  ridelsup.c
**       CONTAINS:
**       ur_del_clear()
**       ur_del_set()
**       ur_del_reset()
**			ur_del_test()
**			ur_enable_del_stack()
**			ur_disable_del_stack()
**			ur_get_del_stat()
**			ur_set_del_stat()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ridelsup.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:42
*********************************************************************/

#include  "usysdef.h"
#include  "udebug.h"
#include  "ribase.h"

/*********************************************************************
**    E_FUNCTION     :  ur_del_clear()
**       clear(reset) all deleted tuple bits
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_del_clear()
{
	int	status;	/* return status */
	int	i, j;		/* indexs */

	uu_denter(UU_RTRC,(us,"ur_del_clear()"));
	for(i = 0; i <= UR_MAX_REL; i++)
	{
		for(j = 0; j < UR_rcb[i].bmap_size; j++)
		{
			UR_rcb[i].bmap_ptr[UR_DEL_MAP*UR_rcb[i].bmap_size+j] = 0;
		}
	}
	uu_dexit;
	return(0);
}

/*********************************************************************
**    E_FUNCTION     :  ur_del_set(relation,tuple_indx)
**       set deleted tuple bit for this key
**    PARAMETERS   
**       INPUT  : 
**			relation, a relation to set deleted in
**			tuple_indx, a tuple within the relation to set deleted
**       OUTPUT :  
**          output
**    RETURNS      :	0 if able to set, -1 if specified tuples was
**							inactive(not used)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_del_set(relation,tuple_indx)
int	relation;	/* what relation to set deleted bit */
int	tuple_indx;	/* what tuple within relation to set deleted bit */
{
	UU_KEY_ID	key;		/* a master key, or relation key */
	int			status;	/* return status */
	int			i, j;		/* indexs */
	UU_KEY_ID	keys[512]; /* holder of imbedded keys */
	int			num_keys; /* number of keys extracted */
	long			a_rel;	/* a relation number */
	long			a_indx;	/* a tuple index */

	uu_denter(UU_RITRC,(us,"ur_del_set(rel=%d, tuple=%d",relation,tuple_indx));
	status = 0 ;
	if(UR_rcb[relation].status == 0)
	{
		if(uu_tst_bit(&(UR_rcb[relation].bmap_ptr[UR_ALLOC_MAP]),tuple_indx-1))
		{
			ur_rt2k(relation,tuple_indx,&key);
/*
 *	comment out extraction code for now 01/13/86. Impacts how modeling
 *	disolves composite curves and removes symbol instances. EJK
 */
/******			status = ur_extract_keys(key,keys,sizeof(keys)/sizeof(UU_KEY_ID),&num_keys) ;*********/
			status = 0 ; /* REMOVE this line if previous line enabled */
			if(status > 0) /* returned number of possible keys	*/
			{
				/* if we didn't get all keys, don't mark anything */
				if(status == num_keys)
				{
					uu_dprint(UU_RITRC,(us,"extracted %d keys",num_keys)) ;
					status = 0 ;
					i = 0	;
					while(status == 0 && i < num_keys)
					{
						if(keys[i] != 0)
						{
							ur_k2rt(keys[i],&a_rel,&a_indx) ;
							status = ur_del_set(a_rel,a_indx) ;
						}
						i++ ;
					}
				} /* if status != num_keys	*/
				else
				{
					uu_dprint(-1,(us,"ERROR, only got %d keys of %d",
							num_keys,status));
					status = -1 ;
				} /* else status != num_keys	*/
			} /* status > 0, keys existed to be extracted	*/

			/* now set as deleted */
			uu_set_bit(
				&(UR_rcb[relation].bmap_ptr[UR_DEL_MAP*UR_rcb[relation].bmap_size]),
				tuple_indx-1);
		} /* if uu_tst_bit true	*/
		else
		{
			uu_dprint(-1,(us,"attempted to set deleted for inactive tuple"));
			status = -1 ; /* attempted to set for deleted an inactive tuple	*/
		}
	}
	else
	{
		uu_dprint(-1,(us,"attempted to set deleted for inactive relation"));
		status = -1 ;	/* tried to set deleted for inactive relation		*/
	}
	uu_dexit ;
	return(status) ;
}

/*********************************************************************
**    E_FUNCTION     :  ur_del_reset(relation,tuple_indx)
**       reset deleted tuple bit for this key
**    PARAMETERS   
**       INPUT  : 
**          key,	can be either a master(primary) key, or an 
**						actual relation key
**       OUTPUT :  
**          output
**    RETURNS      :	0 if able to reset, -1 if specified relation
**							inactive(not used)
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_del_reset(relation,tuple_indx)
int	relation			;	/* what relation to set deleted bit				*/
int	tuple_indx		;	/* what tuple within relation to set deleted bit*/
{
	int	status			;	/* return status										*/

	uu_denter(UU_RTRC,(us,"ur_del_reset(rel=%d, tuple=%d",relation,tuple_indx));
	status = 0 ;
	if(UR_rcb[relation].status == 0)
	{
		/* clear the bit, even if the tuple is used - avoids deadlock */
		uu_clr_bit(
			&(UR_rcb[relation].bmap_ptr[UR_DEL_MAP*UR_rcb[relation].bmap_size]),
			tuple_indx-1);
	}
	else
	{
		uu_dprint(-1,(us,"attempted to reset deleted for inactive relation"));
		status = -1;	/* tried to reset deleted for inactive relation		*/
	}
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  ur_del_test(relation,tuple_indx) 
**       test tuples to see if it is to be deleted
**    PARAMETERS   
**       INPUT  : 
**				relation, relation to be tested	
**				tuple_indx, tuple within the relation to be tested
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE if tuple has been deleted, UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_del_test(relation,tuple_indx)
int	relation;	/* what relation to set deleted bit	*/
int	tuple_indx;	/* what tuple within relation to set deleted bit*/
{
	int	status;	/* return status */

#if MODULEDBG != 0
	uu_denter(UU_RTRC,(us,"ur_del_test(rel=%d, tuple=%d)",relation,tuple_indx));
#endif
	status = UU_FALSE ;
	if(UR_rcb[relation].status == 0)
	{
		if(uu_tst_bit(
			&(UR_rcb[relation].bmap_ptr[UR_DEL_MAP*UR_rcb[relation].bmap_size]),
			tuple_indx-1))
		{
			status = UU_TRUE;
		}
 	}
#if MODULEDBG != 0
 	uu_dprint(UU_RITRC,(us,"ur_del_test, return %d",status));
	uu_dexit;
#endif
	return(status);
}
/*********************************************************************
**    E_FUNCTION     :  ur_enable_del_stack()
**       enable the delete stack
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**          none
**    RETURNS      : 
**				none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_enable_del_stack()
{
	int	status;			/* return status */

	uu_denter(UU_RTRC,(us,"ur_enable_del_stack"));
	status = 0;
	UR_del_mark = UU_TRUE;
	UR_del_stack_enabled = UU_TRUE;
	UR_del_started = UU_FALSE;	/* a new delete trans has not yet started */
	uu_dexit;
	return(status);
}
/*********************************************************************
**    E_FUNCTION     :  ur_disable_del_stack()
**       disable the delete stack
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**          none
**    RETURNS      : 
**				none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_disable_del_stack()
{
	int	status;			/* return status */

	uu_denter(UU_RTRC,(us,"ur_disable_del_stack"));
	status = 0;
	UR_del_mark = UU_TRUE;	/* shouldn't hurt, might help, to reset mark	*/
	UR_del_stack_enabled = UU_FALSE;
	uu_dexit;
	return(status);
}



/*********************************************************************
**    E_FUNCTION     :  ur_get_del_stat()
**       return the current state of the delete stacking
**    PARAMETERS   
**       INPUT  : 
**				none
**       OUTPUT :  
**          none
**    RETURNS      : 
**				UU_TRUE if stacking enabled, UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL ur_get_del_stat()
{
	return(UR_del_stack_enabled);
}



/*********************************************************************
**    E_FUNCTION     :  ur_set_del_stat(value)
**       set the current state of the delete stacking without starting
**				a new delete operation
**    PARAMETERS   
**       INPUT  :
**				value	UU_LOGICAL	new state for the delete stacking
**       OUTPUT :  
**          none
**    RETURNS      : 
**				UU_TRUE if stacking enabled, UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_set_del_stat(value)
UU_LOGICAL	value;
{
	UR_del_stack_enabled = value;
}
