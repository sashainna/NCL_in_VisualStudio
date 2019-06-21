/*********************************************************************
**    NAME:  r1eassoc.c
**       CONTAINS:
**    		ur1_setSchedValidation(descrptr, mkValid) 
**    		ur1_beginBatch() 
**    		ur1_endBatch() 
**   			ur1_invalidateAll(descrptr)
**				ur1_notifyAssocOfUpdates(assocKey, notifyptr)
**    		ur1_getTopDescrptr(hashKey, dataptr)
**				UU_LOGICAL ur1_isAnAssoc(relnum)
**				void ur1_printTfmat(trc, dataptr, strg)
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       r1eassoc.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:26
*********************************************************************/
#include "usysdef.h"	/* basic data types */
#include "uhep.h"		/* for error system */
#include "udebug.h"	/* for debugging trace facility */
#include "uhash.h"	/* for hash table definitions */
#include "umessages.h"	/* for the messages strings */
#include "ribase.h"		/* for uri_assocp */
#include "r1isched.h"/* for internal scheduler data types etc */
#include "r1esched.h"/* for external scheduler data types */
#include "r1emacro.h"/* for scheduler macros */

#define TRACE UU_TRUE /* for debugging only */
/*********************************************************************
**    E_FUNCTION: int ur1_setSchedValidation(descrptr, mkValid) 
**		This function sets to (in)valid the schedule entry that refers to the 
**		auto-update descriptor pointed to by "descrptr" depending on the
**		value of "mkValid".
**    PARAMETERS   
**			INPUT: 
**				descrptr		Pointer to the auto-update descriptor whose schedule 
**								entry to mark valid.
**				mkValid		UU_TRUE iff the descriptor pointed to by "descrptr"
**								is to be made valid; otherwise it is going to be made
**								invalid.
**			OUTPUT:	none.
**    RETURNS:	UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
int ur1_setSchedValidation(descrptr, mkValid) 
	UR_assocUpdateDescriptor *descrptr;
	UU_LOGICAL mkValid;
{
	UR_schedEntry *entryptr;
	int status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us,
		"ur1_setSchedValidation(descrptr>assocKey:%d,mkValid:%d)",
		descrptr->assocKey, mkValid));
	
	ur1_setValidation(descrptr, mkValid); 
	entryptr = ur1_getUpdateSchedEntry(descrptr); 
	if (mkValid)
		if (!entryptr->valid)
		{
			if (ur1i_linkSameTupleUpdates(entryptr) != UU_SUCCESS)
				goto failed;
		}
		else; /* it's already valid */
	else /* make it invalid */
		if (entryptr->valid)
		{
			if (entryptr->sameTupleHigher != UU_NULL)
				entryptr->sameTupleHigher->sameTupleLower=entryptr->sameTupleLower;
			if (entryptr->sameTupleLower != UU_NULL)
				entryptr->sameTupleLower->sameTupleHigher=entryptr->sameTupleHigher;
			entryptr->sameTupleHigher = UU_NULL;
			entryptr->sameTupleLower = UU_NULL;
		}
	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dexitstatus("ur1_setSchedValidation",status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int ur1_beginBatch() 
**		A call to this function prohibits the association auto-update scheduler 
**		from processing its entries.  In order to allow the scheduler to continue
**		processing, calls to this function must be uniquely paired with calls 
**		to "ur1_endBatch".  This function thus allows a large number of entries 
**		to be put in the schedule without auto-updates happening as the entries 
**		are being put on the schedule.  Thus, multiple updates to a single 
**		association tuple can be handled with one "regenerate" update message 
**		if the association's update method so chooses.
**    PARAMETERS   
**			INPUT:		none.
**			OUTPUT:		none.
**    RETURNS:	UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
int ur1_beginBatch()
{
	int status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us,"ur1_beginBatch()"));

	UR_batchNumber++;
	if (UR_schedOn) /* then force scheduler to stop */
		UR_schedOn = UU_FALSE;

	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dexitstatus("ur1_beginBatch",status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int ur1_endBatch() 
**		This function restarts the association auto-update scheduler whenever each
**		"ur1_beginBatch" call executed has been paired up uniquely with a call 
**		to this function.
**    PARAMETERS   
**		INPUT:		none.
**		OUTPUT:		none.
**    RETURNS:		UU_SUCCESS if no problems encountered, UU_FAILURE 
**				 		otherwise.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
int ur1_endBatch()
{
	int status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us,"ur1_endBatch()"));

	UR_batchNumber--;
	if (UR_batchNumber > 0) /* continue */;
	else if (UR_batchNumber == 0)
	{
		if (ur1i_startSched() != UU_SUCCESS) goto failed;
	}
	else if (UR_batchNumber < 0)
	{
		uu_uerror0(UU_UBASE, 1111);
		/* error is: Association batch number is corrupted; it has been reset. */
		UR_batchNumber = 0;
	}
	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dexitstatus("ur1_endBatch",status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int ur1_invalidateAll(descrptr)
**			This function marks all schedule entries as invalid that:
**				1) reference the association tuple to be updated; and
**				2) are scheduled at or below the schedule entry for "descrptr".
**    PARAMETERS   
**       INPUT: 
**				descrptr		Pointer to the update descriptor used to indicate
**								the portion of the schedule to mark as invalid.
**       OUTPUT:  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ur1_invalidateAll(descrptr)
	UR_assocUpdateDescriptor *descrptr;
{
	UR_assocUpdateDescriptor *nxtDescrptr;
	UR_schedEntry *entryptr;
	int status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us,"ur1_invalidateAll(descrptr>assocKey:%d)",
					descrptr->assocKey));

	/* mark all entries for this assoc tuple as invalid */
	nxtDescrptr = descrptr; 
	do {
		if (ur1_setSchedValidation(ur1_getUpdateSchedEntry(nxtDescrptr), UU_FALSE)
				!= UU_SUCCESS) goto failed;
		ur1_nxtUpdateSameTuple(nxtDescrptr);
	} while (nxtDescrptr != UU_NULL);
/* #if (TRACE)
	ur1i_hash_print();
#endif */
	entryptr = ur1_getUpdateSchedEntry(descrptr);
	if (entryptr->sameTupleHigher == UU_NULL) 
		/* then we have invalidated all entries for this association tuple */
		uu_hash_dele(UR_schedHashTbl, descrptr->assocKey);
/* #if (TRACE)
	ur1i_hash_print();
#endif */
	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dexitstatus("ur1_invalidateAll",status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int ur1_notifyAssocOfUpdates(assocKey, notifyptr)
**			This function (re)sets the state of the entity, A, with key, 
**			"assocKey", so that the association notification mechanism for this 
**			entity is either turned on or turned off depending on what the 
**			value of "*notifyptr" is.
**			If "*notifyptr" is UU_TRUE, then A will be notified for possible
**			updating whenever A contains the key of an entity that is being
**			modified.  
**			If "*notifyptr" is UU_FALSE, then A will not be notified.
**    PARAMETERS   
**       INPUT: 
**				key			Key of the entity which is to have it's association 
**								state set.
**				notifyptr	Pointer to UU_TRUE iff the entity with "assocKey"
**								is to be notified.
**       OUTPUT:  
**				notifyptr	Pointer to the old notification setting; this is useful
**								in resetting the entity's notification state.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ur1_notifyAssocOfUpdates(assocKey, notifyptr)
	UU_KEY_ID assocKey;
	UU_LOGICAL *notifyptr;
{
	UU_LOGICAL oldNotifyState;
	int status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us,"ur1_notifyAssocOfUpdates(assocKey:%d, notify:%d)",
				assocKey, *notifyptr));

	if (ur_get_assoc_state(assocKey, &oldNotifyState) != 0) goto failed;
	if (*notifyptr)
	{
		uu_dprint(UU_R1TRC,(us, "TURN ON NOTIFICATION OF UPDATES TO ENTITY:%d", 
						assocKey));
	}
	else
		uu_dprint(UU_R1TRC,(us,"TURN OFF NOTIFICATION OF UPDATES TO ENTITY:%d",
						assocKey));
	if (oldNotifyState)
	{
		uu_dprint(UU_R1TRC,(us,"previous state: notify of updates"));
	}
	else uu_dprint(UU_R1TRC,(us,
						"previous state: DON'T notify of updates"));
	if (*notifyptr != oldNotifyState)
	{
		if (ur_set_assoc_state(assocKey, *notifyptr) != 0) goto failed;
		*notifyptr = oldNotifyState;
	}
	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dexitstatus("ur1_notifyAssocOfUpdates",status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION:  ur1_getTopDescrptr(hashKey, dataptr)
**       This macro retrieves the highest descriptor scheduled for
**			updating the association tuple whose key is "hashKey".  The
**			the retrieved entry is considered valid.
**    PARAMETERS   
**       INPUT: 
**          hashKey		The key id (UU_KEY_ID) of the association tuple whose
**								update list head is to be retrieved from the schedule.
**       OUTPUT:  
**          dataptr		If an update entry is found, then this is a pointer to 
**								the highest such descriptor in the schedule 
**								(UR_assocUpdateDescriptor).
**								If no entry is found, then UU_NULL is output.
**    RETURNS: UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS: none.
*********************************************************************/
int ur1_getTopDescrptr(hashKey, dataptrptr)
	UU_KEY_ID hashKey;
	UR_assocUpdateDescriptor **dataptrptr;
{
	UR_schedEntry **entryptrptr;
	int status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us,"ur1_getTopDescrptr(key:%d,?)",hashKey));

	if ((entryptrptr = (UR_schedEntry **)uu_hash_get(UR_schedHashTbl, hashKey)) 
				!= UU_NULL)
		*dataptrptr = ur1_getDescrptr(*entryptrptr);
	else *dataptrptr = UU_NULL;
	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dexitstatus("ur1_getTopDescrptr",status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: UU_LOGICAL ur1_isAnAssoc(relnum)
**			This function returns UU_TRUE iff "relnum" corresponds to an
**			association.
**    PARAMETERS   
**       INPUT: 
**				relnum		Relation number of the relation to be determined 
**								if it is an association.
**       OUTPUT: none. 
**    RETURNS: UU_TRUE iff "relnum" is an association.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
UU_LOGICAL ur1_isAnAssoc(relnum)
	int relnum;
{
	UU_LOGICAL status;
	uu_denter(UU_R1TRC,(us,"ur1_isAnAssoc(relnum:%d)",relnum));
	status = uri_assocp(relnum);
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION: void ur1_printTfmat(trc, dataptr, strg)
**			This function prints the contents of dataptr as if it
**			were a transformation. 	
**    PARAMETERS   
**       INPUT: 
**				trc		Trace value to use to print transformation; note, UU_R1TRC
**							always causes the transformation to print.
**				dataptr 	Pointer to the character array containing the 
**							transformation.
**				strg		String to be printed out before the transformation.
**       OUTPUT:  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
void ur1_printTfmat(trc, dataptr, strg)	
	int trc;
	char *dataptr;
	char *strg;
{	
	UU_REAL (*tfmat)[3]; 
	char us[150];
	tfmat = (UU_REAL (*)[3]) dataptr; 
	uu_dprint((trc|UU_R1TRC),(us, strg)); 
	uu_dprint((trc|UU_R1TRC),
		(us,"				  %g, %g, %g", tfmat[0][0],tfmat[0][1],tfmat[0][2]));	
	uu_dprint((trc|UU_R1TRC),
		(us,"              %g, %g, %g", tfmat[1][0],tfmat[1][1],tfmat[1][2]));
	uu_dprint((trc|UU_R1TRC),
		(us,"              %g, %g, %g", tfmat[2][0],tfmat[2][1],tfmat[2][2]));	
	uu_dprint((trc|UU_R1TRC),
		(us,"              %g, %g, %g", tfmat[3][0],tfmat[3][1],tfmat[3][2]));	
} 
