/*********************************************************************
**    NAME         :  reuvk.c
**       CONTAINS:
**       ur_update_view_key()
**       ur_update_view_key1()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reuvk.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:41
*********************************************************************/

#include "usysdef.h"
#include "umessages.h"
#include	"udebug.h"
#include	"ribase.h"
#include	"riddle.h"
#include "rmtuple.h"
#include "r1emsgpk.h"

/*********************************************************************
** E_FUNCTION : status = ur_update_view_key(key, view_key)
**      Update view key for a particular master key
**    PARAMETERS   
**       INPUT  : 
**				key, key of master tuple to update
**				view_key, key of view to update with
**       OUTPUT :  
**				none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_view_key(key, view_key)
UU_KEY_ID	key;
UU_KEY_ID	view_key;
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_update_view_key(key=0x%x)", key));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_UPDT_VKEY, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_update_view_key1(key, view_key, &theMessage, 1));
}


/*********************************************************************
** E_FUNCTION : status = ur_update_view_key1(key, view_key,
**										theMessage, messageCnt)
**      Update view key for a particular master key
**    PARAMETERS   
**       INPUT  : 
**				key, key of master tuple to update
**				view_key, key of view to update with
**				theMessage,	for scheduler
**       OUTPUT :  
**				none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_view_key1(key, view_key, theMessage, messageCnt)
UU_KEY_ID	key;
UU_KEY_ID	view_key;
UR_messagePacket	theMessage[];
int					messageCnt;
{
	struct UR_MTID_rec	*m_ptr;	/* pointer to a master tuple	*/
	int						status;
	int						rel;
	int						tuple;

	uu_denter(UU_RTRC, (us, "ur_update_view_key1(key=0x%x, view_key=0x%x messageCnt=%d)",
					key, view_key, messageCnt));
	status = 0;
	ur_k2rt(key, &rel, &tuple);
	ur_get_tuple_ptr(rel, tuple, &m_ptr);
	m_ptr->view_key = view_key;

	/* now notify associates */
	if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
	{
		uri_notify(m_ptr, key, theMessage, messageCnt);
	}
	ur_unibase_change(rel);
	uu_dprint(UU_RITRC, (us, "ur_update_view_key exit status=%d", status));
	uu_dexit;
	return(status);
}
