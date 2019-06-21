/*********************************************************************
**    NAME         :  rere.c
**       CONTAINS:
**       ur_retrieve_editability()
**    COPYRIGHT 1988 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rere.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:34
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) rere.c 3.1 2/5/88 16:51:56 single"};
#else
static char uu_sccsident[]={"@(#) rere.c 3.1 2/5/88 16:51:56 double"};
#endif
#endif

#include "usysdef.h"
#include "udebug.h"
#include "rbase.h"
#include "ribase.h"
#include "rmtuple.h"

/*********************************************************************
**    E_FUNCTION :  ur_retrieve_editability(key, &editState)
**       retrieve the editability of the entity identified by key
**    PARAMETERS   
**       INPUT  : 
**          key			UU_KEY_ID	identifier for the entity
**       OUTPUT :  
**          editState	UU_LOGICAL*	UU_TRUE if editable
**    RETURNS      : 0 if successful
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_editability(key, editState)
UU_KEY_ID	key;
UU_LOGICAL	*editState;
{
	int				status;	/* return status built here */
	UR_REL_NUM		rel;		/* relation number */
	UR_TUPLE_INDX	tuple;	/* which tuple in the relation */

	uu_denter(UU_RTRC,(us,"ur_retrieve_editability(0x%x, %d)",key,editState));
	status = -1;
	ur_k2rt(key, &rel, &tuple);
	if (rel == UR_MTUPLE_REL)
	{
		if (ur_allocatedp(rel, tuple))
		{
			status = 0;
			if (ur_tst_mtuple_flag(key, UR_EDITABLE))
			{
				*editState = UU_TRUE;
			}
			else
			{
				*editState = UU_FALSE;
			}
		}
	}
	uu_dexit;
	return(status);
}

