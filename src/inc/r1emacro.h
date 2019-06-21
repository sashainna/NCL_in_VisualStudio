/*********************************************************************
**    NAME:  r1emacro.h
**       CONTAINS:
**				ur1_touchAssoc(key,msgPackets,nbrPackets,statusptr)
**				ur1_getSameTupleSchedHead(hashKey, dataptr)
**				ur1_nxtUpdateSameTuple(descrptr)	
**				ur1_getUpdateSchedEntry(descrptr)
**				ur1_setValidation(descrptr, value)
**				ur1_isValid(entryptr)
**				ur1_getDescrptr(entryptr) 
**				UR1_CURNT_MSG_NBR(descrptr) ((descrptr)->currentMsgPacket)+1
**				UR1_NXT_CURNT_MSG_PAK(descrptr, msgPacketptrptr)
**				UR1I_DO_UPDATE(McEptr,McMsgPacketptr,McNbrPackets,McChangeRec,
**									McFunct,McStatusptr,McAutoUpdate)	
**				UR1_UPDATE_ME(McEptr,McMsgPacketptr,McNbrPackets, McUpdateFunct, 
**									McPermsnOkptr,McStatusptr) 
**				UR1_AUTO_UPDATE(McEptr,McMsgPacketptr,McNbrPackets, McFixAssocFunct,
**									McStatusptr) 
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       r1emacro.h , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:06:41
*********************************************************************/
#include "usysdef.h"
#include "uhash.h"		/* for UU_HASH_TBL */
#include "r1isched.h"

#ifndef UR1_EMACRO

/*********************************************************************
**    E_MACRO: ur1_touchAssoc(key,msgPackets,nbrPackets,statusptr)
**			This macro causes Unibase to view the entity corresponding to, "key",
**			as if it had been updated.
**    PARAMETERS   
**       INPUT: 
**				key			Key of the entity to be viewed as if it had been 
**								updated.
**				msgPackets	Array of message packets.
**				nbrPackets	Number of valid message packets.
**       OUTPUT:  
**				statusptr	Pointer to UU_TRUE iff no problems; UU_FALSE otherwise.
**    RETURNS:  none.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
#define ur1_touchAssoc(key, msgPackets, nbrPackets, statusptr)	\
{ \
	uu_dprint(UU_R1TRC,(us,"ur1_touchAssoc(key:%d,1stmsg:%s,nbrPaks:%d.?),(macro)", key, uu_msg(UR1_GET_MSG(msgPackets)),nbrPackets)); \
	if (ur_touch(key, msgPackets, nbrPackets) != 0) *(statusptr) = UU_FAILURE;	\
	else *(statusptr) = UU_SUCCESS; \
} if (UU_TRUE)

/*********************************************************************
**    E_MACRO:  ur1_getSameTupleSchedHead(hashKey, dataptrptr)
**       This macro retrieves the highest entry in the schedule for
**			updating the association tuple whose key is "hashKey".  The
**			the retrieved entry is considered valid.
**    PARAMETERS   
**       INPUT: 
**          hashKey		The key id (UU_KEY_ID) of the association tuple whose
**								update list head is to be retrieved from the schedule.
**       OUTPUT:  
**          dataptrptr	If an update entry is found, then this is a pointer to 
**								the a pointer to the highest such entry in the 
**								schedule (UR_schedEntry).
**								If no entry is found, then UU_NULL is returned.
**    RETURNS: UU_NULL iff the hash key doesn't correspond to a hash table 
**					entry.
**    SIDE EFFECTS : none
**    WARNINGS: Must be used as the expression of an "if" statement;
**					 e.g. if (ur1_getSameTupleSchedHead(hashKey, &dataptr) 
**									== UU_NULL) <no hash entry found>
*********************************************************************/
#define ur1_getSameTupleSchedHead(hashKey, dataptrptr)				\
	(dataptrptr = (UR_schedEntry **)uu_hash_get(UR_schedHashTbl, hashKey))

/*********************************************************************
**    E_MACRO:ur1_nxtUpdateSameTuple(descrptr)				\
**			This macro retrieves the next lowest update descriptor from the
**			association update schedule.
**    PARAMETERS   
**       INPUT: 
**				descrptr		Update descriptor from which to start to get the next 
**								one.
**       OUTPUT:  
**				descrptr		Next update descriptor or UU_NULL if no next descriptor.
**		RETURNS: none.
**    SIDE EFFECTS: none
**    WARNINGS: none
*****************************************************************************/
#define ur1_nxtUpdateSameTuple(descrptr)				\
	descrptr = \
		(UR_assocUpdateDescriptor *)(((UR_schedEntry *)descrptr)->sameTupleLower)

#define ur1_getUpdateSchedEntry(descrptr) (UR_schedEntry *) (descrptr)

#define ur1_setValidation(descrptr, value)		\
	uu_dprint(UU_R1TRC,(us,"ur1_setValidation(dscrptr>key:%d,valu:%d)(macro)",(descrptr)->assocKey, value));	\
	((UR_schedEntry *)(descrptr))->valid = value

#define ur1_isValid(entryptr)	((entryptr)->valid) ? UU_TRUE : UU_FALSE

#define ur1_getDescrptr(entryptr) (UR_assocUpdateDescriptor *) entryptr

#define UR1_CURNT_MSG_NBR(descrptr) ((descrptr)->currentMsgPacket)+1

/* no change in the "current" message is done */
#define UR1_CURNT_MSG_PAK(descrptr, msgPacketptrptr) \
	ur1_getMsgPacket(descrptr,0,msgPacketptrptr) 
	
/****************************************************************
**		EMACRO: UR1_NXT_CURNT_MSG_PAK(descrptr, msgPacketptrptr)
**       This macro retrieves the current message packet from the
**       association update descriptor, "descrptr".  The message packet
**			retrieved is relative to the "current" message packet of the
**			descriptor, "*descrptr".  If a valid message packet is found,
**			then the "current" message packet indicator is incremented.
**			Note, if UU_FAILURE is returned, the "current" message
**			packet indicator will be set to UR1_BAD_MSGPK_NBR (an invalid
**			value).
**    PARAMETERS
**       INPUT:
**          descrptr          Pointer to the association update descriptor.
**       OUTPUT:
**				descrptr				Pointer to the descriptor with the "current"
**										message indicator likely changed.
**          msgPacketptrptr   Pointer to the pointer to the message packet to
**                            be returned.
**                            If there is no such packet, then UU_NULL
**                            is returned.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**		EXAMPLE: The following call retrieves the "current" message packet
**						and ...
**					if (UR1_NXT_CURNT_MSG_PAK(&descrpt,&msgPacketptr) != UU_SUCCESS)
**						goto failed;
**					if (msgPacketptr == UU_NULL) no more packets 
**							...
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
#define UR1_NXT_CURNT_MSG_PAK(descrptr, msgPacketptrptr) \
	((ur1_getMsgPacket(descrptr,0,msgPacketptrptr) != UU_SUCCESS) \
		? UU_FAILURE : \
		(ur1_setCurntMsg(descrptr,UR1_CURNT_MSG_NBR(descrptr)+1) != UU_SUCCESS)  \
		/* return UU_SUCCESS in either case */ \
		? UU_FAILURE : UU_SUCCESS)

/*********************************************************************
**    E_MACRO: UU_LOGICAL ur1_1stIsHigherPriority(firstEntryptr, secondEntryptr)
**       This function returns UU_TRUE iff the first parameter has a
**			higher auto-update priority than the second one.
**    PARAMETERS   
**       INPUT: 
**          firstEntryptr	Pointer to an auto-update schedule entry.
**				seconEntryptr	Pointer to an auto-update schedule entry.
**       OUTPUT : none. 
**    RETURNS: UU_TRUE iff the first entry has higher priority than the 
**					second.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
#define ur1_1stIsHigherPriority(firstEntryptr, secondEntryptr) \
	(firstEntryptr->priority > secondEntryptr->priority)

/*********************************************************************
**    I_MACRO:UR1I_DO_UPDATE(McEptr,McMsgPacketptr,McNbrPackets,McChangeRec,
**									McFunct,McStatusptr,McAutoUpdate)	
**			This macro does most of the update mechanism for UR1_UPDATE_ME and
**			UR1_AUTO_UPDATE.  The only added parameter that is not discussed in
**			the other 2 macros is "McAutoUpdate".  This is UU_TRUE iff we are
**			doing an auto-update.
**    PARAMETERS   
**       INPUT: 
**				input
**       OUTPUT:  
**				output
**    WARNINGS: none
*********************************************************************/
#define UR1I_DO_UPDATE(McEptr,McMsgPacketptr,McNbrPackets,McChangeRec,McFunct,McStatusptr,McAutoUpdate)	\
{	\
	UU_LOGICAL McNotify = UU_FALSE, McProceed = UU_TRUE; \
	int Mci; \
	/* Note, we should not need to ask for permission to do the updates \
	 * here because the entire association dependency graph should have  \
	 * been searched via the permission call in "UR1_UPDATE_ME". */ \
	if (!ur1_isAnAssoc((McEptr)->rel_num)) \
	{  /* if its not an association just fix association entity */ \
		if (McFunct != UU_SUCCESS) *(McStatusptr) = UU_FAILURE; \
		McProceed = UU_FALSE; /* don't do anything else */ \
	} \
	/*********** got an association **********/ \
	else if (ur1_notifyAssocOfUpdates((McEptr)->key, &McNotify) != UU_SUCCESS)  \
	{	\
			*(McStatusptr) = UU_FAILURE;   \
			McProceed = UU_FALSE;	\
	}	\
	else /* if UU_DELETE found as one of the new messages, then don't do final \
			* notify since the entity has been deleted */	\
		for (Mci=0; Mci<McNbrPackets; Mci++)	\
			if (UR1_GET_MSG( &((McMsgPacketptr)[Mci]) ) == UU_DELETE)	\
			{	/* since we are going to delete, don't notify McEptr of updates */ \
				uu_dprint(UU_R1TRC,(us,"UU_DELETE found; don't notify or touch"));\
				if (McAutoUpdate)	\
				{	\
					UR_assocUpdateDescriptor *topDescrptr; \
					/* mark all entries in schedule as invalid (they should already \
					 * be invalid for this association as invalid) */ \
					if (ur1_getTopDescrptr((McEptr)->key, &topDescrptr) \
						!= 0) *(McStatusptr) = UU_FAILURE; \
					if (topDescrptr != UU_NULL)  \
						/* then invalidate everything on sched for this tuple */ \
						if (ur1_invalidateAll(topDescrptr) != UU_SUCCESS) \
							*(McStatusptr) = UU_FAILURE; \
				}	\
				McProceed = UU_FALSE;	\
				if (McFunct != UU_SUCCESS)  *(McStatusptr) = UU_FAILURE;   \
				break; \
			}	\
	if (McProceed)	/* no UU_DELETE msg and "McEptr" is an association */	\
		if (McFunct != UU_SUCCESS)  *(McStatusptr) = UU_FAILURE;   \
		else if (ur1_notifyAssocOfUpdates((McEptr)->key,&McNotify) != UU_SUCCESS)\
			*(McStatusptr) = UU_FAILURE;   \
		else if ((McChangeRec) == UU_NO_REC_CHANGE)	\
		{	/* didn't change record; so must have changed referenced subents */ \
			uu_dprint(UU_R1TRC,(us,"GOING TO TOUCH ENTITY:%d, proceed:%d",(McEptr)->key,McProceed));	\
			if ((UU_TRANSLATE <= (McMsgPacketptr)[0].message) && \
				((McMsgPacketptr)[0].message <= UU_SCALE)) \
			{ \
				ur1_printTfmat(-1, (McMsgPacketptr)[0].dataPacket[0].dataptr, "transformation is:"); \
			} \
			ur1_touchAssoc((McEptr)->key,McMsgPacketptr,McNbrPackets,McStatusptr);\
		}	\
	else { uu_dprint(UU_R1TRC,(us,"no notify or touch done"));} \
} if (UU_TRUE)

/*********************************************************************
**		E_MACRO: UR1_UPDATE_ME(McEptr,McMsgPacketptr,McNbrPackets, 
**								McUpdateFunct, McStatusptr, McPermsnOkptr) 
**			This macro does the following:
**			1. asks permission to do the operation corresponding to the 
**				message(s) in "*McMsgPacketptr";
**			2. If "*McEptr" is an association entity the following steps are taken:
**				2.1. turns off notification of association entity;
**				2.2. calls the function, "McFixAssocFunct";
**				2.3.if association entity still exists:
**					2.3.1. resets notification of association entity; 
**					2.3.2. if the Unibase record for "*McEptr" has not changed (i.e.
**						"McChangeRec" is UU_NO_REC_CHANGE, then the Unibase record for
**						"*McEptr" is "touched" to initiate any association updates. 
**			3. If "*McEptr" is NOT an association, only the function, 
**			   "McFixAssocFunct" is called.
**    PARAMETERS
**       INPUT:
**				McEptr				Pointer to the entity (association tuple) to be
**										updated.
**				McMsgPacketptr		Messages to be passed to entities "*McEptr" is 
**										associated with; i.e. "McEptr->key" is referenced
**										by these other entities.	
**				McNbrPackets		Number messages in "McMsgPacketptr".  Note, if
**										this is less than or equal to zero, then this 
**										macro will fail, and no update will be done;
**										i.e. "McUpdateFunct" will not be executed. 
**				McChangeRec			UU_REC_CHANGE iff the entity Unibase data record 
**										for "McEptr" is to be changed; otherwise if 
**										UU_NO_REC_CHANGE only entities referenced by 
**										"McEptr" are to be changed.
**				McUpdateFunct		The function instantiation to be used to update
**										the Unibase association tuple corresponding to
**										"McEptr".  Note, this function ought to respond
**										appropriately to each one of the "McNbrPackets"
**										message packets.  Also, this must be the actual 
**										executable call (must be the 
**										function together with it's parameters. Finally, 
**										this function must return UU_SUCCESS or 
**										UU_FAILURE.
**			OUTPUT:
**				McPermsnOkptr		Pointer to UU_TRUE iff permission was given and
**										the update attempted.
**				McStatusptr			Pointer to UU_SUCCESS if no problems encountered,
**										UU_FAILURE otherwise.
**    SIDE EFFECTS: Note, if permission is not granted an error message will
**							be printed.
**    WARNINGS: none
*********************************************************************/
#define UR1_UPDATE_ME(McEptr,McMsgPacketptr,McNbrPackets, McChangeRec, McUpdateFunct, McPermsnOkptr,McStatusptr) \
{  \
	UU_LOGICAL McContinue; \
	uu_denter(UU_R1TRC,(us,"UR1_UPDATE_ME((eptr)>ky:%d,msg:%s,nbrpks:%d,changeRec:%d,funct,?,?)",(McEptr)->key,uu_msg(UR1_GET_MSG(McMsgPacketptr)),McNbrPackets,McChangeRec)); \
	*(McStatusptr) = UU_SUCCESS;	\
	if (uc_permission(McEptr,McMsgPacketptr,McNbrPackets,	\
					UU_TRUE/*printerrs*/, &McContinue) != UU_SUCCESS) 	\
	{	\
		McContinue = UU_FALSE;	\
		*(McStatusptr) = UU_FAILURE;		\
	}	\
	*(McPermsnOkptr) = McContinue;	\
	if (McContinue) /* continue; permission was granted */	\
		UR1I_DO_UPDATE(McEptr,McMsgPacketptr,McNbrPackets,McChangeRec,McUpdateFunct,McStatusptr,UU_FALSE);	\
	uu_dexitstatus("UR1_UPDATE_ME",*(McStatusptr));   \
} if (UU_TRUE)

/*********************************************************************
**		E_MACRO: UR1_AUTO_UPDATE(McEptr,McMsgPacketptr,McNbrPackets, 
**								McChangeRec, McFixAssocFunct, McStatusptr) 
**			If "*McEptr" is an association entity the following steps are taken:
**				1. turns off notification of association entity;
**				2. calls the function, "McFixAssocFunct";
**				3. if association entity still exists:
**					3.1 resets notification of association entity; 
**					3.2 if the Unibase record for "*McEptr" has not changed (i.e.
**						"McChangeRec" is UU_NO_REC_CHANGE, then the Unibase record for
**						"*McEptr" is "touched" to initiate any association updates. 
**			If "*McEptr" is NOT an association, only the function, 
**			"McFixAssocFunct" is called.
**    PARAMETERS
**       INPUT:
**				McEptr				Pointer to the entity (association tuple) to be
**										updated.
**				McMsgPacketptr		Messages to be passed on to other entities.	
**				McNbrPackets		Number messages in "McMsgPacketptr".  
**				McChangeRec			UU_REC_CHANGE iff the entity Unibase data record 
**										for "McEptr" is to be changed; otherwise if 
**										UU_NO_REC_CHANGE only entities referenced by 
**										"McEptr" are to be changed.
**				McFixAssocFunct	The function instantiation to be used to update
**										the Unibase association tuple corresponding to
**										"McEptr".  Note, this function ought to respond
**										appropriately to each one of the "McNbrPackets"
**										message packets.  Also, this must be the actual 
**										executable call (must be the function together 
**										with it's parameters). Finally, this function 
**										must return UU_SUCCESS or UU_FAILURE.
**			OUTPUT:
**				McStatusptr			Pointer to UU_SUCCESS if no problems encountered,
**										UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
#define UR1_AUTO_UPDATE(McEptr,McMsgPacketptr,McNbrPackets, McChangeRec, McFixAssocFunct, McStatusptr) \
{  \
	uu_denter(UU_R1TRC,(us,"UR1_AUTO_UPDATE(eptr>ky:%d,1stmsg:%s,nbrpks:%d,changrec:%s,funct,?)",(McEptr)->key,uu_msg(UR1_GET_MSG(McMsgPacketptr)),McNbrPackets,uu_msg(McChangeRec))); \
	*(McStatusptr) = UU_SUCCESS;	\
	UR1I_DO_UPDATE(McEptr,McMsgPacketptr,McNbrPackets,McChangeRec,McFixAssocFunct,McStatusptr,UU_TRUE);	\
	uu_dexitstatus("UR1_AUTO_UPDATE",*(McStatusptr));   \
} if (UU_TRUE)

#define UR1_EMACRO
#endif
