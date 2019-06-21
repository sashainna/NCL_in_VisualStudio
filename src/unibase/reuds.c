/*********************************************************************
**    NAME         :  reuds.c
**       CONTAINS:
**       ur_update_disp_segid()
**       ur_update_disp_segid1()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reuds.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:38
*********************************************************************/

#include	"usysdef.h"
#include "umessages.h"
#include	"rmtuple.h"
#include	"ribase.h"
#include	"riddle.h"
#include	"udebug.h"
#include "r1emsgpk.h"

/*********************************************************************
**    E_FUNCTION     :  status = ur_update_disp_segid(key, dsegid)
**      update display segment id
**    PARAMETERS   
**       INPUT  : 
**        key,	key to to use in updating the display seg id
**			dsegid,	the display segment to use in updating
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_disp_segid(key, dsegid)
UU_KEY_ID 	key;				/* the key to update to					*/
int			dsegid;			/* the display segment id					*/
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_update_disp_segid(key=0x%x)", key));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_UPDT_DSEG, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_update_disp_segid1(key, dsegid, &theMessage, 1));
}


/*********************************************************************
**    E_FUNCTION     :  status = ur_update_disp_segid1(key, dsegid,
**												theMessage, messageCnt)
**      update display segment id
**    PARAMETERS   
**       INPUT  : 
**        key,	key to to use in updating the display seg id
**			dsegid,	the display segment to use in updating
**				theMessage,	for scheduler
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_disp_segid1(key, dsegid, theMessage, messageCnt)
UU_KEY_ID 	key;				/* the key to update to					*/
int			dsegid;			/* the display segment id					*/
UR_messagePacket	theMessage[];
int					messageCnt;
{
	int						status;		/* status, -1 if error, 0 otherwise */
	int						rel;			/* relation id from key, better = 0 */
	int						tuple;		/* entry id from key, better = 0 */
	UR_rcb_rec				*rcb_ptr;	/* ptr to key rel cntrl blk */
	struct UR_MTID_rec	*m_ptr;		/* pointer to key entry */

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
	uu_denter(UU_RTRC, (us, "ur_update_disp_segid1(key= 0x%x disp_segid %d messageCnt=%d)",
					key, dsegid, messageCnt));
	status = 0;

	/* get key as rel, entry */	
	ur_k2rt(key, &rel, &tuple);
	rcb_ptr = &UR_rcb[rel];
	if((rel==UR_MTUPLE_REL) && (uu_tst_bit(rcb_ptr->bmap_ptr, tuple-1)))
	{
		/* got a valid key relation and an active entry, */
		/* now update the display segment id */
		status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
		if(status == 0)
		{
			m_ptr->dsegid = dsegid;
			ur_unibase_change(rel);
		}

/* this removed until someone wants to know! */
		/* now notify associates */
/*		if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
/*		{
/*			uri_notify(m_ptr, key, theMessage, messageCnt);
/*		}	*/

	}
	else
	{
		status = -1;
	}
	uu_dexit;
	return(status);
}
