/*********************************************************************
**    NAME         :  retouch.c
**       CONTAINS:
**       ur_touch()
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       retouch.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:37
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) retouch.c 3.2 1/21/88 11:05:51 single"};
#else
static char uu_sccsident[]={"@(#) retouch.c 3.2 1/21/88 11:05:51 double"};
#endif
#endif

#include "usysdef.h"
#include "udebug.h"
#include "rbase.h"
#include	"rmtuple.h"
#include "riddle.h"
#include "r1emsgpk.h"

/*********************************************************************
**    E_FUNCTION :  ur_touch(key, theMessage, messageCnt)
**       announce to any associates that the operation described
**			by the message and parameters has been done to the entity
**			given by key.
**    PARAMETERS   
**       INPUT  : 
**				key		- entity changed
**				theMessage,	for scheduler
**       OUTPUT :  
**          none
**    RETURNS      : zero if all is OK
**    SIDE EFFECTS : perform the notification
**    WARNINGS     : none
*********************************************************************/

ur_touch(key, theMessage, messageCnt)
UU_KEY_ID			key;
UR_messagePacket	theMessage[];
int					messageCnt;
{
	UR_REL_NUM		rel;			/* the relation to update */
	UR_TUPLE_INDX	tuple;		/* index of the tuple to update */
	struct UR_MTID_rec	*m_ptr;

	uu_denter(UU_RTRC,(us,"ur_touch(key=0x%x)",key));
	ur_k2rt(key, &rel, &tuple);
	if(rel == UR_MTUPLE_REL)
	{
		ur_get_tuple_ptr(rel, tuple, &m_ptr);	

		/* now notify associates */
		if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
		{
			uri_notify(m_ptr, key, theMessage, messageCnt);
		}
	}
	uu_dexit;
	return(0);
}

