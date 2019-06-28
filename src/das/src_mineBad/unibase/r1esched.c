/*********************************************************************
**    NAME:  r1esched.c
**       CONTAINS:
**    		ur1_isSchedExecuting()
**    		ur1i_nxtUpdate(updateList)
**    		ur1_foundInSched(updateptr,positionptr)
**    		ur1_putNewUpdatsInSched(newUpdateListHead,nbr,position,
**    		ur1_1stIsHigherPriority(firstEntryptr, secondEntryptr)
**    		ur1_1stHigherInSched(position1ptr, position2ptr) 
**    		ur1_getPosition(message,positionptr)
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       r1esched.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:27
*********************************************************************/
#include "usysdef.h"	/* basic data types */
#include "uhep.h"		/* for error system */
#include "udebug.h"	/* for debugging trace facility */
#include "uhash.h"	/* for hash table definitions */
#include "r1isched.h"/* internal scheduler data structures etc */
#include "r1esched.h"/* external scheduler data structures */
#include "r1emacro.h"/* for scheduler macros */

#define TRACE UU_TRUE	/* for debugging only */
/*********************************************************************
**    E_MACRO: ur1_isSchedExecuting()
**		This macro returns UU_TRUE iff the scheduler is executing.
**    PARAMETERS   
**		INPUT:	none.
**		OUTPUT: 	none.
**    RETURNS:	UU_TRUE iff the scheduler is executing.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
#define ur1_isSchedExecuting() 	\
	(UR_schedOn) ? UU_TRUE : UU_FALSE

/*********************************************************************
**    E_MACRO: ur1i_nxtUpdate(updateList)
**		This macro returns the first update descriptor on the list
**		"updateList".
**    PARAMETERS   
**		INPUT: 
**			updateList		List of pointers to update schedule entries.
**		OUTPUT:The first pointer on the list; UU_NULL if the list is empty.
**    RETURNS: none.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
#define ur1i_nxtUpdate(updateList)						\
	uu_dprint(UU_R1ITRC,(us,"ur1i_nxtUpdate(look after address:%x)",updateList));\
	updateList = (UR_schedEntry **) uu_lsnext(updateList);	\
	uu_dprint(UU_R1TRC,(us,"	next update ptr at:%x, update addrs:%x", (updateList), *(updateList)))

/*********************************************************************
**    E_MACRO: ur1_foundInSched(updateptr,positionptr)
**		This macro determines if an update to the association tuple
**		referenced by "updateptr" is currently scheduled.  If so, then a pointer
**		to the highest such entry is output.
**    PARAMETERS   
**		INPUT: 
**			updateptr		Pointer to a schedule entry record; the association 
**								tuple this parameter corresponds to is the one being 
**								sought.
**		OUTPUT:  
**			positionptr		Pointer to the position record that contains an entry 
**								in the schedule that requests an update to the same 
**								association tuple as the one referred to in 
**								"updateptr".  The message field, 
**								"positionptr->message", will be UR_FOUND if an update 
**								entry for the association tuple referred to by 
**								"updateptr" is found.
**								Otherwise, UR_NOTFOUND is returned in this field.
**    RETURNS: none.
**    SIDE EFFECTS:	none.
**    WARNINGS:		none.
*********************************************************************/
#ifdef UU_DEBUGON
#define ur1_foundInSched(updateptr,positionptr)		\
{	char *ptr;	\
	uu_dprint(UU_R1ITRC,(us,"ur1_foundInSched(updat_ptr:%x(1stmsg:%s),?)", (updateptr), uu_msg((updateptr)->descrpt.msgPacket[0].message)));			\
	uu_dprint(UU_R1ITRC,(us,"UR_schedHasTbl:%x",UR_schedHashTbl));	\
	if (ur1i_printEntry(updateptr) != UU_SUCCESS) goto failed;	\
	(positionptr)->where = (UR_schedEntry *) (((ptr = \
		(char *)uu_hash_get(UR_schedHashTbl, (updateptr)->descrpt.assocKey)) \
			== UU_NULL) ? UU_NULL: *((UR_schedEntry **)ptr)) ;				\
	ur1i_hash_print();		\
	if ((positionptr)->where != UU_NULL)				\
		(positionptr)->message = UR_FOUND;				\
	else (positionptr)->message = UR_NOTFOUND;		\
	uu_dprint(UU_R1ITRC,(us,"	returns:positionptr>[msg:%s,where:%x]",ur1i_msg((positionptr)->message), (positionptr)->where));			\
} if (UU_TRUE)
#else	/* debugging is off so don't call debugging functions */
#define ur1_foundInSched(updateptr,positionptr)		\
{	char *ptr;	\
	(positionptr)->where = (UR_schedEntry *) (((ptr = \
		(char *)uu_hash_get(UR_schedHashTbl, (updateptr)->descrpt.assocKey)) \
			== UU_NULL) ? UU_NULL: *((UR_schedEntry **)ptr)) ;				\
	if ((positionptr)->where != UU_NULL)				\
		(positionptr)->message = UR_FOUND;				\
	else (positionptr)->message = UR_NOTFOUND;		\
} if (UU_TRUE)
#endif

/*********************************************************************
**    E_FUNCTION: int ur1_putNewUpdatsInSched()
**			This is the function programmers can modify to change the 
**			algorithm for the way association updates are scheduled.  Currently,
**			new updates are scheduled by priority only. New updates are 
**			scheduled as low as possible and still be of greater priority than
**			updates scheduled below it.  
**    PARAMETERS   
**		INPUT: none.
**		OUTPUT:  none.
**    RETURNS:	UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS:	Schedule entries are added to the schedule; resets
**							UR_updateListHead and UR_updateListEnd to UU_NULL.
**    WARNINGS:		none
*********************************************************************/
int ur1_putNewUpdatsInSched()
{
	UR_schedEntry **updateptrptr; 
	UR_schedEntry *updateptr;
	UR_schedPosition newPosition;
	UR_schedPosition nxtPosition;
	UU_LOGICAL placed;
	int i, status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us,"ur1_putNewUpdatsInSched()"));
	uu_dprint(UU_R1TRC,(us,"	nbr of updates:%d, UR_updateListHead:%x", 
			UR_nbrUpdates, UR_updateListHead));

	updateptrptr = (UR_schedEntry **)UR_updateListHead;
	for (i=0; i<UR_nbrUpdates; i++)
	{
		ur1i_nxtUpdate(updateptrptr); /* get next schedule entry */
		updateptr = *updateptrptr;
		ur1_foundInSched(updateptr,&(nxtPosition));
		if (nxtPosition.message == UR_NOTFOUND) 
		{	/* not currently scheduled; get top of schedule to start looking */
			if (ur1_getPosition(UR_TOTOP, &nxtPosition) != UU_SUCCESS) goto failed;
		}
/*#if (TRACE)
		if (ur1i_printSched("WHOLE SCHED from ur1_putNewUpdatesInSched",
					UR_topOfSched) != UU_SUCCESS) goto failed;
#endif */
		/* now find the position for the new entry */
		placed = UU_FALSE; /* UU_TRUE iff place for updateptr is found */
		while (nxtPosition.message == UR_FOUND)
		{	/* get next update entry in schedule */

			/* which is higher priority? *updateptr or *nxtPosition.where */
			if (ur1_1stIsHigherPriority(updateptr, nxtPosition.where))
			{ /* *updateptr is higher; fill out it's position record */
				newPosition.where = nxtPosition.where;
				newPosition.message = UR_NXT_HIGHER_POSITION;
				placed = UU_TRUE;
				break;
			}					
			/* prepare to get next sched entry */
			nxtPosition.message = UR_NXT_LOWER_POSITION;
			if (ur1_getPosition(UR_NXT_LOWER_POSITION, &nxtPosition) 
					!= UU_SUCCESS) goto failed;
		} 
		if (!placed) 
		{ /* then new sched entry is lower priority than all else */
			newPosition.message = UR_TOEND;
		}
		if (ur1i_add2Sched(updateptr, &newPosition) != UU_SUCCESS) 
					goto failed;
	}
	goto done;
failed: status = UU_FAILURE;
done: 
	UR_nbrUpdates = 0; /* reset this for the next set of association updates */
	uu_lsdel(UR_updateListHead);
	UR_updateListHead = UU_NULL;
	UR_updateListEnd = UU_NULL;
	uu_dexitstatus("ur1_putNewUpdatsInSched",status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION:  int ur1_1stHigherInSched(position1ptr, position2ptr,retptr) 
**		This function returns UU_TRUE iff the first position record is
**		higher in the schedule than the second one. We assume both
**		parameters refer to schedule entries that in turn refer to
**		updates of the same association tuple; also, both must be valid.
**    PARAMETERS   
**		INPUT: 
**			position1ptr	Pointer to the first position record.
**			position2ptr	Pointer to the second position record.
**		OUTPUT:
**			retptr			Pointer to UU_TRUE iff the 1st position references
**								a higher entry in the schedule.
**    RETURNS:	UU_TRUE iff the first position record is higher in the 
**					schedule than the second one.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
int ur1_1stHigherInSched(position1ptr, position2ptr, retptr)
	UR_schedPosition *position1ptr;
	UR_schedPosition *position2ptr;
	UU_LOGICAL *retptr;
{
	UR_schedEntry **schedEntryptrptr, *schedEntryptr;
	UU_LOGICAL found = UU_FALSE;
	UU_KEY_ID assocKey;
	int status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us,
		"ur1_1stHigherInSched(pos1>where>assocKey:%d,pos2>where>assocKey:%d,?)",
		position1ptr->where->descrpt.assocKey,
		position2ptr->where->descrpt.assocKey));

	if (position1ptr->where == position2ptr->where)
		*retptr = UU_FALSE;
	else if ((assocKey = position1ptr->where->descrpt.assocKey) ==
				position2ptr->where->descrpt.assocKey)
	{	/* then search "sameTupleLower" */
		ur1_getSameTupleSchedHead(assocKey, schedEntryptrptr);
		schedEntryptr = *schedEntryptrptr;
		while ((schedEntryptr != UU_NULL) && (!found))
		{
			if (schedEntryptr->sameTupleLower == position1ptr->where)
			{
				*retptr = UU_TRUE;
				found = UU_TRUE;
			}
			else if (schedEntryptr->sameTupleLower == position2ptr->where)
			{
				*retptr = UU_FALSE;
				found =  UU_TRUE;
			}
			schedEntryptr = schedEntryptr->sameTupleLower;
		}
		if (!found) 
		{
			uu_dprint(UU_R1TRC,(us,"can't do test"));
			goto failed;
		}
	}
	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dexitstatus("ur1_1stHigherInSched", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int ur1_getPosition(message,positionptr)
**		This function finds the position record for the association update 
**		schedule entry according the value of "message" which can be any of the 
**		following: 
**			UR_HIGHER_POSITION, UR_LOWER_POSITION, UR_TOTOP, UR_TOEND.
**		The returned positions can be to valid or invalid entries.
**    PARAMETERS   
**		INPUT: 
**			message		The values of this identifier have the following meanings:
**							UR_NXT_HIGHER_POSITION: Get the position record for the 
**							schedule entry just higher than the entry referred to 
**							by "positionptr" (if any).
**							UR_NXT_LOWER_POSITION:  Get the position record for the 
**								schedule entry just lower than the entry referred to 
**								by "positionptr" (if any).
**							UR_TOTOP: Get the position record for the schedule entry 
**								at the top of the schedule (if any).
**							UR_TOEND: Get the position record for the schedule entry 
**								at the bottom of the schedule (if any).
**			positionptr	Pointer to a position record from which to find the 
**							position record to return if "message" is either 
**							UR_NXT_HIGHER_POSITION or UR_NXT_LOWER_POSITION.
**		OUTPUT:  
**			positionptr	Pointer to the position record for the schedule entry 
**							requested.
**							The message field, "positionptr->message", will be 
**							UR_FOUND if an update entry for the association tuple 
**							referred to by "updateptr" is found. Otherwise, 
**							UR_NOTFOUND is returned in this field.
**    RETURNS:	UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS:	none
**    WARNINGS:	none
*********************************************************************/
int ur1_getPosition(message,positionptr)
	int message;
	UR_schedPosition *positionptr;
{
	int status = UU_SUCCESS;
	uu_denter(UU_R1ITRC,(us,
		"ur_getPosition(message:%s,positionptr>[msg:%s,where:%x)",
		ur1i_msg(message), ur1i_msg(positionptr->message), positionptr->where));

	switch(message)
	{
		case UR_TOTOP:
			if (UR_topOfSched != UU_NULL)
			{
				positionptr->where = (UR_schedEntry *) UR_topOfSched;
				positionptr->message = UR_FOUND;
			}
			else positionptr->message = UR_NOTFOUND;
			break;
		case UR_NXT_HIGHER_POSITION:
			if (positionptr->where == UU_NULL) 
				positionptr->message = UR_NOTFOUND;
			else
			{
				positionptr->where = positionptr->where->higher; 
				if (positionptr->where == UU_NULL)
					positionptr->message = UR_NOTFOUND;
				else positionptr->message = UR_FOUND;
			}
			break;
		case UR_NXT_LOWER_POSITION:
			if (positionptr->where == UU_NULL) 
				positionptr->message = UR_NOTFOUND;
			else
			{
				positionptr->where = positionptr->where->lower; 
				if (positionptr->where == UU_NULL)
					positionptr->message = UR_NOTFOUND;
				else positionptr->message = UR_FOUND;
			}
			break;
		case UR_TOEND:
			if (UR_endOfSched != UU_NULL)
			{
				positionptr->where = (UR_schedEntry *) UR_endOfSched;
				positionptr->message = UR_FOUND;
			}
			else positionptr->message = UR_NOTFOUND;
			break;
		default:
			break;
	}
	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dexitstatus("ur1_getPosition", status);
	return(status);
}

