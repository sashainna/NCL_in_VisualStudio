/*********************************************************************
**    NAME         :  reascsup.c
**       CONTAINS:
**       miscellaneous associativity support routines:
**			ur_assoc_off()
**			ur_assoc_on()
**			ur_get_assoc_state()
**			ur_set_assoc_state()
**			ur_ignore_associative()
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reascsup.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:27
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) reascsup.c 3.2 3/15/88 10:16:07 single"};
#else
static char uu_sccsident[]={"@(#) reascsup.c 3.2 3/15/88 10:16:07 double"};
#endif
#endif

#include "usysdef.h"
#include "udebug.h"
#include "rbase.h"
#include "ribase.h"

/*********************************************************************
**    E_FUNCTION :  ur_assoc_off(key)
**		mark whether to schedule the given key in response to an operation
**		on an associate
**    PARAMETERS   
**		INPUT  : 
**			key		UU_KEY_ID	master tuple to mark
**		OUTPUT :		none
**    RETURNS:		0 if successful, -1 otherwise
**    SIDE EFFECTS:	mark association update disallowed
**    WARNINGS:		none
*********************************************************************/

ur_assoc_off(key)
UU_KEY_ID		key;		/* master tuple key */
{
	int				status;	/* return status */
	UR_REL_NUM		rel;		/* what relation to set assoc bit */
	UR_TUPLE_INDX	tuple;	/* what tuple within relation to set assoc bit */

	uu_denter(UU_RTRC,(us,"ur_assoc_off(key:0x%x)",key));
	status = 0;
	ur_k2rt(key,&rel,&tuple);
	if(UR_rcb[rel].status == 0)
	{
		if(uu_tst_bit(&(UR_rcb[rel].bmap_ptr[UR_ALLOC_MAP]),tuple-1))
		{
			uu_set_bit(&(UR_rcb[rel].bmap_ptr[UR_ASSOC_MAP *
							UR_rcb[rel].bmap_size]),tuple-1);
		}
		else
		{
			uu_dprint(-1,(us,"ERROR:ur_assoc_off for inactive tuple %d", tuple));
			status = -1;
		}
	}
	else
	{
		uu_dprint(-1,(us,"ERROR:ur_assoc_off for inactive relation %d", rel));
		status = -1;
	}
	uu_dexit;
	return(status);
}



/*********************************************************************
**    E_FUNCTION :  ur_assoc_on(key)
**		mark whether to schedule the given key in response to an operation
**		on an associate
**    PARAMETERS   
**		INPUT  : 
**			key		UU_KEY_ID	master tuple to mark
**		OUTPUT :		none
**    RETURNS:		0 if successful, -1 otherwise
**    SIDE EFFECTS:	mark association update allowed
**    WARNINGS:		none
*********************************************************************/

ur_assoc_on(key)
UU_KEY_ID		key;			/* master tuple key */
{
	int				status;		/* return status */
	UR_REL_NUM		rel;	/* what relation to set assoc bit */
	UR_TUPLE_INDX	tuple;	/* what tuple within relation to set assoc bit */

	uu_denter(UU_RTRC,(us,"ur_assoc_on(key:0x%x)",key));
	status = 0;
	ur_k2rt(key,&rel,&tuple);
	if(UR_rcb[rel].status == 0)
	{
		if(uu_tst_bit(&(UR_rcb[rel].bmap_ptr[UR_ALLOC_MAP]),tuple-1))
		{
			uu_clr_bit(&(UR_rcb[rel].bmap_ptr[UR_ASSOC_MAP *
							UR_rcb[rel].bmap_size]),tuple-1);
		}
		else
		{
			uu_dprint(-1,(us,"ERROR:ur_assoc_off for inactive tuple %d", tuple));
			status = -1;
		}
	}
	else
	{
		uu_dprint(-1,(us,"ERROR:ur_assoc_off for inactive relation %d", rel));
		status = -1;
	}
	uu_dexit;
	return(status);
}


/*********************************************************************
**    E_FUNCTION :  ur_get_assoc_state(key, &where)
**		inquire whether to schedule the given key in response to an operation
**		on an associate
**    PARAMETERS   
**		INPUT  : 
**			key		UU_KEY_ID	master tuple to mark
**			where		UU_LOGICAL*	where to put the state
**		OUTPUT :		none
**    RETURNS:		0 if successful, -1 otherwise
**    SIDE EFFECTS:	return whether association update allowed
**    WARNINGS:		none
*********************************************************************/

ur_get_assoc_state(key, where)
UU_KEY_ID	key;			/* master tuple key */
UU_LOGICAL	*where;
{
	int				status;		/* return status */
	UR_REL_NUM		rel;	/* what relation to set assoc bit */
	UR_TUPLE_INDX	tuple;	/* what tuple within relation to set assoc bit */

	uu_denter(UU_RTRC,(us,"ur_assoc_state(key:0x%x)",key));
	status = 0;
	ur_k2rt(key, &rel, &tuple);
	if(UR_rcb[rel].status == 0)
	{
		if(uu_tst_bit(&(UR_rcb[rel].bmap_ptr[UR_ALLOC_MAP]),tuple-1))
		{
			*where = uri_notify_allowedp(rel, tuple);
		}
		else
		{
			uu_dprint(-1,(us,"ERROR:ur_get_assoc_state for inactive tuple %d", tuple));
			status = -1;
		}
	}
	else
	{
		uu_dprint(-1,(us,"ERROR:ur_get_assoc_state for inactive relation %d", rel));
		status = -1;
	}
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  ur_set_assoc_state(key, value)
**		mark whether to schedule the given key in response to an operation
**		on an associate
**    PARAMETERS   
**		INPUT  : 
**			key		UU_KEY_ID	master tuple to mark
**		OUTPUT :		none
**    RETURNS:		0 if successful, -1 otherwise
**    SIDE EFFECTS:	mark whether association update allowed
**    WARNINGS:		none
*********************************************************************/

ur_set_assoc_state(key, value)
UU_KEY_ID		key;
UU_LOGICAL	value;
{
	if(value)
	{
		return(ur_assoc_on(key));
	}
	else
	{
		return(ur_assoc_off(key));
	}
}


/*********************************************************************
**    E_FUNCTION :  ur_ignore_associative(rel)
**       tells unibase to ignore the fact that a relation is an association
**    PARAMETERS   
**       INPUT  : 
**          rel	the subject relation
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_ignore_associative(rel)
UR_REL_NUM	rel;
{
	uu_denter(UU_RTRC,(us,"ur_ignore_associative(rel=%d)",rel));
	if (UR_rcb[rel].status >= 0 && UR_rcb[rel].active_tuple_cnt == 0)
	{
		/* only do it if this is a valid relation and no tuples yet */
		uu_clr_bit(&UR_rcb[rel].rel_flags, UR_ASSOC_REL);
	}
	uu_dexit;
}

