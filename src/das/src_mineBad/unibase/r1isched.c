/*********************************************************************
**    NAME:  r1isched.c
**       CONTAINS:
**    		ur1i_stopScheduler
**    		ur1i_isSchedEmpty 
**    		ur1i_deleteUpdateEntry(positionptr) 
**				ur1i_nxtTopOfSched()
**    		ur1i_beginSchedUpdates() 
**    		ur1i_schedUpdate(priority,assocKey,causeKey,field, 
**    		ur1i_commitSchedUpdates()
**    		ur1i_add2Sched(newUpdateptr,hiPositionptr,newPositionptr)
**    		ur1i_linkSameTupleUpdates(newUpdateptr)
**    		ur1i_startSched()
**    		ur1i_printSched(strg,schedptr) 
**				ur1i_printEntry(entryptr)
**				ur1i_msg(msgNbr)
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       r1isched.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:27
*********************************************************************/
#include "usysdef.h"	/* basic data types */
#include "uhep.h"		/* for error system */
#include "udebug.h"	/* for debugging trace facility */
#include "uhash.h"	/* for hash table definitions */
#include "umessages.h"/* for UU_REGENERATE, etc. */
#include "rbase.h"	/* for UR_REL_NAME */
#define UR_R1PGM		/* for allocation of storage */
#include "r1isched.h"/* for internal scheduler data types */
#include "r1esched.h"/* for external scheduler data types */
#include "r1emacro.h"/* scheduler macros */
#undef UR_R1PGM

#define TRACE UU_FALSE /* for debugging only */
/*********************************************************************
**    I_MACRO: ur1i_stopScheduler
**		This macro inserts a stop entry into the scheduler at the top of 
**		the schedule.
**    PARAMETERS   
**		INPUT:		none.
**		OUTPUT:		none.
**    RETURNS:	none.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
#define ur1i_stopScheduler  UR_schedOn = UU_FALSE

/*********************************************************************
**    I_MACRO: ur1i_isSchedEmpty 
**		This macro returns UU_TRUE iff the auto update schedule is empty.
**    PARAMETERS   
**		INPUT:			none.
**		OUTPUT:		none.
**    RETURNS:		UU_TRUE iff the auto update schedule is empty.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
#define ur1i_isSchedEmpty 		\
	(UR_topOfSched == UU_NULL) ? UU_TRUE : UU_FALSE

/*********************************************************************
**    I_FUNCTION:  ur1i_nxtTopOfSched()
**		This function replaces the top of the schedule with any next entry.
**		If no next entry, or, no current entry, then the global, 
**		"UR_topOfSched", will have the value UU_NULL.
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE
**				 otherwise.
**    SIDE EFFECTS: Changes the value of "UU_topOfSched", hash table contents,
**						  schedule entries.  Note, this function frees dynamic storage
**						  allocated for schedule entries that correspond to previous
**						  top of schedule.
**    WARNINGS    : none
*********************************************************************/
int ur1i_nxtTopOfSched()
{	
	UR_schedEntry **sameTupleHeadptrptr;
	UR_schedEntry *newTopOfSched;
	int status = UU_SUCCESS;
	uu_denter(UU_R1ITRC,(us,"ur1i_nxtTopOfSched()"));

	if (UR_topOfSched == UU_NULL) 
	{
		uu_dprint(UU_R1ITRC,(us,"schedule empty"));
		goto done;
	}
	if (UR_topOfSched->valid) 
	{
		if (UR_topOfSched->sameTupleLower != UU_NULL)	
			/* replace hash table entry for this tuple's updates */
			if (ur1_getSameTupleSchedHead(UR_topOfSched->descrpt.assocKey,
					sameTupleHeadptrptr) == UU_NULL) 
				/* then something's wrong; ptr to tuple entry not in hash table */
				goto failed;
			else /* found in hash table */
				*sameTupleHeadptrptr = UR_topOfSched->sameTupleLower;
		else /* there is no other update for this tuple */
		{
/* #if (TRACE)
			ur1i_hash_print();
#endif */
			uu_hash_dele(UR_schedHashTbl,UR_topOfSched->descrpt.assocKey);			
/* #if (TRACE)
			ur1i_hash_print();
#endif */
		}
	}
	newTopOfSched = UR_topOfSched->lower; /* get new top */
	uu_dprint(UU_R1ITRC,(us,"freeing UR_topOfSched:%x", UR_topOfSched));
	uu_toolfree(UR_topOfSched);	/* deallocate old top */
	UR_topOfSched = newTopOfSched;
	if (UR_topOfSched == UU_NULL) /* then schedule is empty */
	{	
		UR_schedOn = UU_FALSE;
		UR_endOfSched = UU_NULL;
	}
	else
	{
		UR_topOfSched->higher = UU_NULL;	
		UR_topOfSched->sameTupleHigher = UU_NULL;
	}
	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dexitstatus("ur1i_nxtTopOfSched",status);
	return(status);
}

/*********************************************************************
**    I_FUNCTION: int ur1i_beginSchedUpdates() 
**		This function sets up the necessary data structures to collect 
**		association updates from Unibase proper. 
**    PARAMETERS   
**		INPUT:			none.
**		OUTPUT:		none.
**    RETURNS:		UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
int ur1i_beginSchedUpdates()
{
	char *uu_lsnew();
	int status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us,"@@ur1i_beginSchedUpdates()"));
	/* if (ur1i_hash_print() != UU_SUCCESS) goto failed; */
	UR_updateListHead = uu_lsnew(); 
	UR_updateListEnd = (UR_schedEntry **) UR_updateListHead;
	if (UR_updateListHead == UU_NULL)
	{
		uu_uerror0(UU_UBASE, 1111); /* out of dynamic memory */
		goto failed;
	}
	UR_nbrUpdates = 0;
	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dexitstatus("ur1i_beginSchedUpdates",status);
	return(status);
}

/*********************************************************************
**    I_FUNCTION: int ur1i_schedUpdate(priority,assocKey,causeKey,field, 
**							msgPackets, nbrPackets)
**		This function provides the scheduler with the necessary data to create 
**		a schedule entry for an automatic update of the association tuple 
**		"assocKey".  However, the schedule entry is not put into the schedule 
**		here.
**    PARAMETERS   
**		INPUT: 
**			priority		Schedule priority of the association tuple to be updated.
**			assocKey		Key of the association tuple to be updated.
**			causeKey		Key of the entity causing the association tuple update.
**			field			Field (i.e. column) in the association tuple containing 
**							the key of the entity causing the update of the association
**							tuple.
**			msgPackets	Message packet indicating what kind of change happened to 
**							the entity represented by "causeKey".
**			nbrPackets	Number of message packets.
**		OUTPUT:		none.
**    RETURNS:		UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
int ur1i_schedUpdate(priority,assocKey,causeKey,field,msgPackets,nbrPackets)
	int			priority;
	UU_KEY_ID	assocKey;
	UU_KEY_ID	causeKey;
	int			field;
	UR_messagePacket msgPackets[];
	int nbrPackets;
{
	char *uu_msg(), *uu_toolmalloc(), *uu_lsinsrt();
	UR_schedEntry *newUpdateptr; /*, **newUpdateptrptr; */
	UR_messagePacket *newMsgPacketptr, *msgPacketptr;
	int i,j, status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us,
	"@@ur1i_schedUpdate(prty:%d,ascKy:%d,causKy:%d,ddlfld:%d,1stmsg:%s,paks:%d)",
	priority,assocKey,causeKey,field,uu_msg(UR1_GET_MSG(msgPackets)),nbrPackets));

	if (	((UU_TRANSLATE <= msgPackets[0].message) && 
			 (msgPackets[0].message <= UU_SCALE)) || 
			(msgPackets[0].message == UU_TRANSFORM)) 
	{
		ur1_printTfmat(-1, msgPackets[0].dataPacket[0].dataptr,
		"transformation is:");
	}

	UR_nbrUpdates++;
	UR_updateListEnd = 
		(UR_schedEntry **)uu_lsinsrt(UR_updateListEnd, sizeof(UR_schedEntry *));
	if (UR_updateListEnd == UU_NULL) 
	{
		uu_uerror0(UU_UBASE, 1111); /* out of dynamic memory */
		goto failed;
	}
	/* UR_updateListEnd = (char *)newUpdateptrptr; */
	newUpdateptr = *UR_updateListEnd = 
								(UR_schedEntry *)uu_toolmalloc(sizeof(UR_schedEntry));
#if (UU_FALSE)
	{	/* debugging stuff; print UR_updateListHead list contents */
		char *tmp;
		tmp = UR_updateListHead;
		while ((tmp = (char *)uu_lsnext(tmp)) != UU_NULL)
			uu_dprint(UU_R1ITRC,(us,"list addrs: %x, list contents:%x",
							tmp, *((char **)tmp) ));
	}
#endif
	if (newUpdateptr == UU_NULL)
	{
		uu_uerror0(UU_UBASE, 1111); /* out of dynamic memory */
		goto failed;
	}
	newUpdateptr->descrpt.assocKey = assocKey;
	newUpdateptr->descrpt.field = field; /* number of the field that changed */
	newUpdateptr->descrpt.causeKey = causeKey;

	/* which message packet is being looked at by user */
	newUpdateptr->descrpt.currentMsgPacket = 0;

	if (nbrPackets >= UR_MAX_MSGS)
	{
		uu_uerror2(UU_UBASE,1111, nbrPackets, assocKey);
		/* ERROR: too many message packets (%d) for update of assoc:%d */
		goto failed;
	}
 	newUpdateptr->descrpt.nbrPackets = nbrPackets;
	for (i=0; i<nbrPackets; i++)
	{
		newMsgPacketptr = &(newUpdateptr->descrpt.msgPacket[i]);
		msgPacketptr = &(msgPackets[i]);

		newMsgPacketptr->message = msgPacketptr->message;
		newMsgPacketptr->nbrDataPks = msgPacketptr->nbrDataPks;
		/* NOTE, WE ARE NOT COPYING THE DATA PACKETS */
		newMsgPacketptr->dataPacket = msgPacketptr->dataPacket;
	}

	ur1_setValidation(ur1_getDescrptr(newUpdateptr), UU_FALSE); 
	newUpdateptr->sameTupleHigher = UU_NULL;
	newUpdateptr->sameTupleLower = UU_NULL;
	newUpdateptr->priority = priority;

	/* links for doubly linking entries into scheduler */
	newUpdateptr->higher = UU_NULL; 
	newUpdateptr->lower = UU_NULL;
#if (TRACE)
	uu_dprint(UU_R1ITRC,(us,"address of new entry to put in sched:%x", 
						newUpdateptr));
#endif
	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dexitstatus("ur1i_schedUpdate",status);
	return(status);
}

/*********************************************************************
**    I_FUNCTION: int ur1i_commitSchedUpdates()
**		A call to this function commits the association update entries created
**		by "ur1i_schedUpdate" to be scheduled and starts the scheduler executing.
**    PARAMETERS   
**		INPUT: none.
**		OUTPUT: none.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
int ur1i_commitSchedUpdates()
{
	UR_schedPosition *currntPosition; /* sched positions, if currently sched'd */
	UR_schedEntry *updateptr;
	int status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us,"@@ur1i_commitSchedUpdates()"));

	if (UR_initSched)
	{
		UR_initSched = UU_FALSE;
		/* init hash tbl that will be used to indicate whether assoc tuple has
		 * already been scheduled. */
		uu_hash_init(UR_schedHashTbl, UR_schedHashTblSz);
	}

	/* get the current and new positions for the new association updates */
	if (ur1_putNewUpdatsInSched() != UU_SUCCESS) goto failed;

	/* start scheduler executing if it isn't already */
	if (!UR_schedOn)
		if (ur1i_startSched() != UU_SUCCESS) goto failed;

	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dprint(UU_R1TRC,(us,"AT END OF ur1i_commitSchedUpdates"));
	/* if (ur1i_hash_print() != UU_SUCCESS) goto failed; */
	uu_dexitstatus("ur1i_commitSchedUpdates", status);
	return(status);
}

/*********************************************************************
**    I_FUNCTION: int ur1i_add2Sched(newUpdateptr,positionForUpdate)
**		This function adds the association update descriptor pointed to by 
**		"newUpdateptr" to the auto update schedule. Note, the message field
**		of "positionForUpdate" can only have the following values:
**			UR_NXT_HIGHER_POSITION, UR_NXT_LOWER_POSITION, UR_TOEND, UR_TOTOP.
**    PARAMETERS   
**		INPUT: 
**			newUpdateptr		Pointer to the new entry to add to the schedule.
**			positionForUpdate	Pointer to the position where the new entry is 
**									to be put in the schedule.
**		OUTPUT: none.
**    RETURNS:	UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS:	none
**    WARNINGS: none
*********************************************************************/
int ur1i_add2Sched(newUpdateptr,positionForUpdate)
	UR_schedEntry *newUpdateptr;
	UR_schedPosition *positionForUpdate;
{
	UU_KEY_ID hashKey;
	UR_schedEntry *nxtEntryptr, *dataptr;
	UU_LOGICAL found;
	char *ur1i_msg();
	int status = UU_SUCCESS;
	uu_denter(UU_R1ITRC,(us,
		"ur1i_add2Sched(newUpdat>desc.assocKy:%d,newPstn:%s)",
		newUpdateptr->descrpt.assocKey, ur1i_msg(positionForUpdate->message)));

	/* determine whether the location is higher or lower than "where" field */
	switch(positionForUpdate->message)
	{
		case UR_TOTOP:
			newUpdateptr->lower = UR_topOfSched;
			newUpdateptr->higher = UU_NULL;
			if (UR_topOfSched == UU_NULL) /* then nothing on schedule */
			{
				UR_endOfSched = newUpdateptr;
			}
			else /* there are entries on the schedule */
			{
				UR_topOfSched->higher = newUpdateptr;
			}
			UR_topOfSched = newUpdateptr;
			break;
		case UR_NXT_HIGHER_POSITION:
			/* put update just higher than "positionForUpdate->where" */
			newUpdateptr->lower = positionForUpdate->where;
			newUpdateptr->higher = positionForUpdate->where->higher;
			/* now fill-in the "higher" position */
			positionForUpdate->where->higher = newUpdateptr;
			if (positionForUpdate->where == UR_topOfSched)
				/* then there is a new top */
				UR_topOfSched = newUpdateptr;
			break;
		case UR_NXT_LOWER_POSITION:
			/* put update just lower than "positionForUpdate->where" */
			newUpdateptr->higher = positionForUpdate->where;
			newUpdateptr->lower = positionForUpdate->where->lower;
			positionForUpdate->where->lower = newUpdateptr;
			if (positionForUpdate->where == UR_endOfSched)
				/* there is a new bottom */
				UR_endOfSched =  newUpdateptr;
			break;
		case UR_TOEND:
			newUpdateptr->lower = UU_NULL;
			newUpdateptr->higher = UR_endOfSched;
			if (UR_topOfSched == UU_NULL) /* then nothing on schedule */
			{
				UR_topOfSched = newUpdateptr;
			}
			else /* there are entries on the schedule */
			{
				UR_endOfSched->lower = newUpdateptr;
			}
			UR_endOfSched = newUpdateptr;
			break;
		default: goto failed;
	}
	if (ur1i_linkSameTupleUpdates(newUpdateptr) != UU_SUCCESS)
			goto failed;
	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dexitstatus("ur1i_add2Sched", status);
	return(status);
}

/*********************************************************************
**    I_FUNCTION: int ur1i_linkSameTupleUpdates(newUpdateptr)
**		This function adds the association update descriptor pointed to by 
**		"newUpdateptr" to a linked list of updates for the same assoc tuple,
**		and if no such list exists for "newUpdateptr" then one is created.
**    PARAMETERS   
**		INPUT: 
**			newUpdateptr	Pointer to the new entry to add to the schedule.
**		OUTPUT:	none.
**    RETURNS:	UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS:	links an update entry to a list, or, adds an entry to a
**							hash table.
**    WARNINGS:		none
*********************************************************************/
int ur1i_linkSameTupleUpdates(newUpdateptr)
	UR_schedEntry *newUpdateptr;
{
	UR_schedEntry **sameTupleHeadptrptr, *nxtEntryptr;
	UU_KEY_ID hashKey;
	UU_LOGICAL looking4Link;
	int status = UU_SUCCESS;
	uu_denter(UU_R1ITRC,(us,
		"ur1i_linkSameTupleUpdates(newUpdateptr>descrpt.assocKey:%d)",
				newUpdateptr->descrpt.assocKey));
#if (UU_FALSE)
	if (ur1i_printEntry(newUpdateptr) != UU_SUCCESS) goto failed;
#endif
	newUpdateptr->valid = UU_TRUE;
	if ((newUpdateptr->sameTupleLower != UU_NULL) 
			|| (newUpdateptr->sameTupleHigher != UU_NULL))
	{
		uu_dprint(UU_R1ITRC,(us,"'sameTuple' fields not UU_NULL, nothing done")); 
		goto failed;
	}
	/* must now link this new descriptor in with the others that are valid
	 * for the same tuple */
	hashKey = newUpdateptr->descrpt.assocKey;
	if (ur1_getSameTupleSchedHead(hashKey, sameTupleHeadptrptr) != UU_NULL)
	{	/* then there is already an update for this assoc tuple sched'd 
		 * find out where it is */
		looking4Link = UU_TRUE;
		/* look above "newUpdateptr" in sched for same tuple entry */
		nxtEntryptr = newUpdateptr->higher; 
		while (nxtEntryptr != UU_NULL) /* find any higher entry for this tuple */
			if ((nxtEntryptr->descrpt.assocKey == hashKey) 
				&& (ur1_isValid(nxtEntryptr)) )
			{	/* all entries on "sameTuple????" list are valid; invalid entries 
				 * are taken off this list because otherwise when we have a valid 
				 * entry at the top of the schedule, we must search thru the
				 * "sameTuple???" list to see if any other entries for the assoc.
				 * are also valid in order to determine if we should send a 
				 * "regenerate" message. */
				looking4Link = UU_FALSE;
				break;
			}
			else nxtEntryptr = nxtEntryptr->higher;

		if (!looking4Link) /* insert new entry in the list after the one found */
		{
			newUpdateptr->sameTupleHigher = nxtEntryptr;
			newUpdateptr->sameTupleLower = nxtEntryptr->sameTupleLower;
			/* fix up fields of adjacent entries for same tuple */
			if (nxtEntryptr->sameTupleLower != UU_NULL)
				nxtEntryptr->sameTupleLower->sameTupleHigher = newUpdateptr;
			nxtEntryptr->sameTupleLower = newUpdateptr;
		}
		else /* no higher update for this tuple; so look below for entries for
				* this tuple. */
		{
			newUpdateptr->sameTupleHigher = UU_NULL;
			newUpdateptr->sameTupleLower = *sameTupleHeadptrptr;
 			/* create new head for entries of same tuple in the hash table */
			*sameTupleHeadptrptr = newUpdateptr;
		}	
	}
	else /* there is no other updates for this tuple on the schedule */
	{
/* #if (TRACE)
		ur1i_hash_print();
#endif */
		uu_dprint(UU_R1ITRC,(us,
			"no other updates for assoc with key:%d, in sched; so add one",
			newUpdateptr->descrpt.assocKey));
		if ((sameTupleHeadptrptr = (UR_schedEntry **)uu_hash_add(UR_schedHashTbl,
					newUpdateptr->descrpt.assocKey, sizeof(UR_schedEntry *)) )
					== UU_NULL)
		{
			uu_uerror2(UU_UBASE, 1111, newUpdateptr->descrpt.assocKey,
				"ur1i_linkSameTupleUpdates");		
			/* Out of memory for scheduling a new assoc. update for key:%d, 
			 * (%s). */
			goto failed;
		}
		else 
		{	/* "sameTupleHeadptr" now points to new hash table entry;
			 * put pointer to schedule entry in hash table */
/* #if (TRACE)
			ur1i_hash_print();
#endif */
			*sameTupleHeadptrptr = newUpdateptr;
		}
	}
	goto done;
failed: status = UU_FAILURE;
done: 
#if (UU_FALSE)
	ur1i_printSched("WHOLE SCHED at end of ur1i_linkSameTupleUpdates",
						UR_topOfSched); 
#endif
	uu_dexitstatus("ur1i_linkSameTupleUpdates",status);
	return(status);
}

/*********************************************************************
**    I_FUNCTION: int ur1i_startSched()
**		This function starts the scheduler executing at the top of the schedule.  
**		Note, the schedule may or may not be empty when this function returns.
**    PARAMETERS   
**		INPUT:none.
**		OUTPUT:none.
**    RETURNS:		UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
int ur1i_startSched()
{
	UR_schedEntry *sameTupleLinkptr; 
	UR_schedEntry *toFree;
	UU_LOGICAL printSched = UU_FALSE;
	int status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us,"ur1i_startSched()"));

	if (UR_schedOn) 
	{
		uu_dprint(UU_R1TRC,(us,"scheduler already on"));
		goto done;
	}
	/* else try to turn scheduler on */
	if (UR_batchNumber > 0) /* can't start scheduler */
	{
		uu_dprint(UU_R1TRC,(us,"batching on, UR_batchNumber:%d",UR_batchNumber));
		goto done;
	}
	/* otherwise start scheduler */	
#if (TRACE)
	if (ur1i_printSched("WHOLE SCHED. from start of ur1i_startSched", 
				UR_topOfSched) != UU_SUCCESS) goto failed;
	printSched = UU_TRUE; /* print the schedule below; for debugging only */
#endif
	if (UR_topOfSched == UU_NULL) goto done; /* nothing on schedule */
	UR_schedOn = UU_TRUE; /* indicate that the scheduler is processing */
	while ((UR_topOfSched != UU_NULL) && (UR_schedOn) && (UR_batchNumber == 0))
	{
		if (!UR_topOfSched->valid)/* then top not valid; get next one */
		{
			if (ur1i_nxtTopOfSched() != UU_SUCCESS) goto failed;
			continue;
		}
		/* mark ALL entries for this assoc tuple as invalid */
		if (ur1_invalidateAll(ur1_getDescrptr(UR_topOfSched)) != UU_SUCCESS) 
				goto failed;

		if (UR_topOfSched->sameTupleLower != UU_NULL)
		{	/* there is more than one update for this tuple; note, we are
			 * assuming all entries on this list are valid here and must be made
			 * invalid. */
			UR_schedEntry regenEntry;

			regenEntry.descrpt.assocKey = UR_topOfSched->descrpt.assocKey;
			regenEntry.descrpt.field = UU_NOFIELD;
			regenEntry.descrpt.causeKey = UU_NOKEY;
			if (ur1_initMsgPacket(&(regenEntry.descrpt.msgPacket[0]),UU_REGENERATE,
						0 /* nbr data packets */) != UU_SUCCESS) goto failed;
			regenEntry.descrpt.currentMsgPacket = 0;
			regenEntry.descrpt.nbrPackets = 1;
			/* note, probably want all messages for updates */
			regenEntry.higher = UU_NULL;
			regenEntry.lower = UU_NULL;
			regenEntry.sameTupleHigher = UU_NULL;
			regenEntry.sameTupleLower = UR_topOfSched;

			/* now do update */
			if (uc_autoUpdate(ur1_getDescrptr(&regenEntry)) != UU_SUCCESS)
			{
#ifdef UU_DEBUGON
				int saveMask;
				uu_dprint(-1,(us,
					"ERROR in auto update but continuing (caused by)"));
				saveMask = UU_debmask;
				UU_debmask = UU_R1TRC;
				if (ur1i_printEntry(&regenEntry) != UU_SUCCESS) goto failed;
				UU_debmask = saveMask;
				/* if (ur1i_nxtTopOfSched() != UU_SUCCESS) goto failed;
				goto failed;
				*/
#endif
			}
		}
		else /* no other entry for this tuple than one at the top of sched */
		{
			UR_schedEntry nxtEntry;

			/* copy entry to give to application; this is a safety device
			 * so that if s/he screws up, the top of the schedule is still in
			 * tact. */
			uu_move_byte(UR_topOfSched, &nxtEntry, sizeof(UR_schedEntry));
			if (uc_autoUpdate(ur1_getDescrptr(&nxtEntry)) != UU_SUCCESS)
			{
#ifdef UU_DEBUGON
				int saveMask;
				uu_dprint(-1,(us,
					"ERROR in auto update but continuing (caused by)"));
				saveMask = UU_debmask;
				UU_debmask = UU_R1TRC;
				uu_dprint(-1,(us,
					"ERROR in auto update but continuing (caused by)"));
				if (ur1i_printEntry(&nxtEntry) != UU_SUCCESS) goto failed;
				UU_debmask = saveMask;
				/* if (ur1i_nxtTopOfSched() != UU_SUCCESS) goto failed;
				goto failed;
				*/
#endif
			}
		}
	}/* end while executing top */
	goto done;
failed: status = UU_FAILURE;
	if (UR_topOfSched != UU_NULL)
	{
		uu_dprint(-1,(us,
			"deleting ALL assoc sched entries due to FAILURE (ur1i_startSched)"));
		while (UR_topOfSched != UU_NULL)
			if (ur1i_nxtTopOfSched() != UU_SUCCESS)
				uu_dprint(-1,(us,"ERROR, in deleting schedule entries"));
	}
done: 
#if (TRACE)
	if (printSched)
		if (ur1i_printSched("from end of ur1i_startSched", UR_topOfSched) 
			!= UU_SUCCESS) goto failed;
#endif
	uu_dexitstatus("ur1i_startSched", status);
	return(status);
}

/*********************************************************************
**    I_FUNCTION: int ur1i_printSched(strg, schedptr) 
**		This function prints the schedule entries starting at "schedptr".
**    PARAMETERS   
**		INPUT: 
**			strg			String to print out with schedule.
**			schedptr		Pointer to the schedule entry to start printing from.
**		OUTPUT:		none.
**    RETURNS:		UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
#ifdef UU_DEBUGON
int ur1i_printSched(strg, schedptr) 
	char *strg;
	UR_schedEntry *schedptr;
{ 
	UR_schedEntry *nxtEntryptr;
	int saveMask, status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us,"ur1i_printSched(schedptr:%x)************",schedptr));

	uu_dprint(UU_R1TRC,(us,"%s", strg));
	nxtEntryptr = schedptr;
	if (nxtEntryptr == UU_NULL)
	{
		uu_dprint(UU_R1ITRC,(us,"	schedule is empty"));
		uu_hash_initscan(UR_schedHashTbl);
		if (uu_hash_scan(UR_schedHashTbl) != UU_NULL)
		{
			uu_dprint(-1,(us,"ERROR: HASH TABLE AND SCHEDULE OUT OF SYNC!!"));
			goto failed;
		}
	}
	while (nxtEntryptr != UU_NULL)
	{
		sprintf(UR_sbuf, "NXT SCHED ENTRY: addrs:%x",nxtEntryptr);
		uu_dprint(UU_R1TRC,(us,"%s",UR_sbuf));
		if (ur1i_printEntry(nxtEntryptr) != UU_SUCCESS) goto failed;
		nxtEntryptr = nxtEntryptr->lower;
	}
	uu_dprint(UU_R1TRC,(us,"***********************************"));
	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dexitstatus("ur1i_printSched", status);
	return(status);
}
#else
int ur1i_printSched(strg, schedptr) 
	char *strg;
	UR_schedEntry *schedptr;
{} 
#endif

/*********************************************************************
**    E_FUNCTION: ur1i_printEntry(entryptr)
**			This function prints a schedule entry record.
**    PARAMETERS   
**       INPUT: 
**				entryptr		Pointer to the schedule entry record to be printed.
**       OUTPUT:  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
#ifdef UU_DEBUGON
int ur1i_printEntry(entryptr)
	UR_schedEntry *entryptr;
{
	UR_REL_NAME	name1;
	UR_REL_NAME	name2;
	int saveMask, i, status = UU_SUCCESS;
	char us[150];

	saveMask = UU_debmask;
	UU_debmask = 0;
	if (ur_retrieve_rel_name(entryptr->descrpt.assocKey, name1) != 0)
		goto failed;
	if (ur_retrieve_rel_name(entryptr->descrpt.causeKey, name2) != 0)
		goto failed;
	UU_debmask = saveMask;

	sprintf(UR_sbuf,"	assocKey:%d, assocName:%s, causeKey:%d, causeName:%s",
				entryptr->descrpt.assocKey,name1,entryptr->descrpt.causeKey,name2);
	uu_dprint(UU_R1TRC,(us,"%s",UR_sbuf));

	sprintf(UR_sbuf,"	ddl fld causing update:%d, nbrPackets:%d", 
					entryptr->descrpt.field, entryptr->descrpt.nbrPackets);
	uu_dprint(UU_R1TRC,(us,"%s",UR_sbuf));

	for (i=0; i<entryptr->descrpt.nbrPackets; i++)
	{
		sprintf(UR_sbuf,"		msg:%s, nbr opt data:%d", 
			uu_msg(entryptr->descrpt.msgPacket[i].message), 
			entryptr->descrpt.msgPacket[i].nbrDataPks);
		uu_dprint(UU_R1TRC,(us,"%s",UR_sbuf));
		if ((UU_TRANSLATE <= entryptr->descrpt.msgPacket[i].message) && 
			(entryptr->descrpt.msgPacket[i].message <= UU_SCALE)) 
		{
			ur1_printTfmat(-1,
			entryptr->descrpt.msgPacket[i].dataPacket[0].dataptr,
			"transformation is:");
		}
	}

	sprintf(UR_sbuf,"	priorty:%d, valid:%d",entryptr->priority,entryptr->valid);
	uu_dprint(UU_R1TRC,(us,"%s",UR_sbuf));

	sprintf(UR_sbuf,"	sameTupleHigher:%x,sameTupleLower:%x",
			entryptr->sameTupleHigher, entryptr->sameTupleLower);
	uu_dprint(UU_R1TRC,(us,"%s", UR_sbuf));

	sprintf(UR_sbuf,"	higher:%x, lower:%x",
			entryptr->higher,entryptr->lower);
	uu_dprint(UU_R1TRC,(us,"%s", UR_sbuf));
	goto done;
failed: status = UU_FAILURE;
done: 
	return(status);
}
#else
int ur1i_printEntry(entryptr)
	UR_schedEntry *entryptr;
{}
#endif

/*********************************************************************
**    I_FUNCTION: char *ur1i_msg(msgNbr) 
**       This function returns the message string associated with the
**			message number, "msgNbr".
**    PARAMETERS   
**       INPUT: 
**          msgNbr	Message number to have it's message string returned.
**       OUTPUT: none.  
**    RETURNS: Pointer to the character string corresponding to "msgNbr".
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char *ur1i_msg(msgNbr)
	int msgNbr;
{
	return(UR_message[msgNbr]);
}

/*********************************************************************
**    I_FUNCTION: int ur1i_hash_print()
**			This function prints the schedule entry records that correspond
**			to the pointers contained in the hash table.
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : Prints to the "trc" file.
**    WARNINGS     : none
*********************************************************************/
#ifdef UU_DEBUGON
int ur1i_hash_print()
{
	char us[100];
	char *ptr;
	int status = UU_SUCCESS;

	uu_hash_initscan(UR_schedHashTbl);				
	while ((ptr = (char *)uu_hash_scan(UR_schedHashTbl)) != UU_NULL)
	{
		uu_dprint(UU_R1ITRC,(us,"NEXT HASH ENTRY ADD:%x, contents:%x",
				ptr,*(char**)ptr));	
 		/* if (ur1i_printEntry(*ptr) != UU_SUCCESS) goto failed; */
	}
	goto done;
failed: status = UU_FAILURE;
done: 
	return(status);
}
#else
int ur1i_hash_print()
{}
#endif
