/*********************************************************************
**    NAME         :  reue.c
**       CONTAINS:
**       ur_update_editability()
**    COPYRIGHT 1988 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reue.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:38
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) reue.c 3.1 2/5/88 16:52:00 single"};
#else
static char uu_sccsident[]={"@(#) reue.c 3.1 2/5/88 16:52:00 double"};
#endif
#endif

#include "usysdef.h"
#include "udebug.h"
#include "rbase.h"
#include "ribase.h"
#include "rmtuple.h"

/*********************************************************************
**    E_FUNCTION :  ur_update_editability(key, editState)
**       set the editability for the entity identified by key to editState
**    PARAMETERS   
**       INPUT  : 
**          key			UU_KEY_ID	unibase key for the entity
**				editState	UU_LOGICAL	state to set editability to
**       OUTPUT :  
**          none
**    RETURNS      : 0 if successful
**    SIDE EFFECTS : records the new editability
**    WARNINGS     : none
*********************************************************************/

ur_update_editability(key, editState)
UU_KEY_ID	key;
UU_LOGICAL	editState;
{
	int				status;	/* return status built here */
	UR_REL_NUM		rel;		/* relation number */
	UR_TUPLE_INDX	tuple;	/* which tuple in the relation */

	uu_denter(UU_RTRC,(us,"ur_update_editability(0x%x, %d)",key,editState));
	status = -1;
	ur_k2rt(key, &rel, &tuple);
	if (rel == UR_MTUPLE_REL)
	{
		if (ur_allocatedp(rel, tuple))
		{
			status = 0;
			if (editState)
			{
				ur_set_mtuple_flag(key, UR_EDITABLE);
			}
			else
			{
				ur_clr_mtuple_flag(key, UR_EDITABLE);
			}
		}
	}
	uu_dexit;
	return(status);
}

