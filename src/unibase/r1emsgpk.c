
/*********************************************************************
**    NAME:  r1emsgpk.c
**       CONTAINS:
**				DON'T USE! ur1_nxtMessage(descrptr, msgptr, dataPacketptrs, 
**													nbrDataPakptr)
**				ur1_initMsgPacket(msgPacketptr, msg, nbrDataPaks)
**				ur1_addData2MsgPacket(msgPacketptr, dataptr, datasz)
**				ur1_getMsgData(msgPacketptr,dataNbr,dataptrptr,dataszptr)
**				ur1_getMsgPacket(descrptr, msgNbr, msgPacketptrptr)
**				ur1_setCurntMsg(descrptr, msgNbr)
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       r1emsgpk.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:27
*********************************************************************/
#include "usysdef.h"	/* basic data types */
#include "uhep.h"		/* for error system */
#include "udebug.h"	/* for debugging trace facility */
#include "uhash.h"	/* for hash table definitions */
#define UU_MSGPGM
#include "umessages.h"/* for messages and message storage, etc. */
#undef UU_MSGPGM
#include "ribase.h"		/* for uri_assocp */
#include "r1isched.h"/* for internal scheduler data types etc */
#include "r1esched.h"/* for external scheduler data types */
#include "r1emacro.h"/* for scheduler macros */
#include "r1emsgpk.h"/* for UR_messagePacket and UR_dataPacket */

#define TRACE UU_FALSE /* for debugging only */
/*********************************************************************
**    E_FUNCTION: int ur1_nxtMessage(descrptr, msgptr, dataPacketptrs, 
**												nbrDataPakptr)
**			Don't use!!!
**			This function retrieves the next message and it's corresponding
**			data from the association update descriptor pointed to by "descrptr".
**    PARAMETERS   
**       INPUT: 
**				descrptr			Pointer to the association update descriptor
**									from which the next message is to be retrieved.
**       OUTPUT:  
**				msgptr			Pointer to the retrieved message; note if no 
**									further messages can be retrieved, then UU_ENDOFMSGS
**									is output as the value pointed to here.
**				dataPacketptrs	Pointer to the list of message data packet pointers 
**									such that each data packet pointed to by each one 
**									of these pointers is pertinent to the message in 
**									*msgptr.
**				nbrDataPakptr	Pointer to the number of valid data packet pointers.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**		EXAMPLE: Calling example: 
**			UR_assocUpdateDescriptor *descrptr;
**			int msg;
**			UR_dataPacket *dataPacketptrs;
**			int nbrDataPaks;
**					...
**			if (ur1_nxtMessage(descrptr,&msg,&dataPacketptrs,&nbrDataPaks)
**					!= UU_SUCCESS) goto failed;
**    SIDE EFFECTS: none
**    WARNINGS: The data packets are not copied; so modify their contents
**					 at your own risk.
*********************************************************************/
/* Don't use */
int ur1_nxtMessage(descrptr, msgptr, dataPacketptrs, nbrDataPakptr)
	UR_assocUpdateDescriptor *descrptr;
	int *msgptr;
	UR_dataPacket **dataPacketptrs;
	int *nbrDataPakptr;
{
	int currentPak, status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us,
		"ur1_nxtMessage(descrptr>asocKy:%d)", descrptr->assocKey));

	currentPak = descrptr->currentMsgPacket;
	if (currentPak >= descrptr->nbrPackets)
	{
		*msgptr = UU_ENDOFMSGS;
		*dataPacketptrs = UU_NULL;
		*nbrDataPakptr = 0;
	}
	else
	{
		*msgptr = descrptr->msgPacket[currentPak].message;
		*dataPacketptrs = descrptr->msgPacket[currentPak].dataPacket;
		*nbrDataPakptr = descrptr->msgPacket[currentPak].nbrDataPks;
		descrptr->currentMsgPacket++;
	}
	uu_dprint(UU_R1TRC,(us,"returning msg:%s, nbrDataPaks:%d",
				uu_msg(*msgptr), *nbrDataPakptr));
	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dexitstatus("ur1_nxtMessage",status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int ur1_initMsgPacket(msgPacketptr, msg, nbrDataPaks)
**			This function initializes an assocication update message
**			packet with the message to be sent.
**    PARAMETERS   
**       INPUT: 
**				msgPacketptr	Pointer to the message packet to be initialized.
**				msg				Message to be put into the message packet.
**				nbrDataPaks		Max number of data packets to be put in this
**									message packet; starts with 1; i.e. 0 implies
**									no data packets.
**       OUTPUT:  
**				msgPacketptr	Pointer to the initialized message packet.	
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ur1_initMsgPacket(msgPacketptr, msg, nbrDataPaks)
	UR_messagePacket *msgPacketptr;
	int msg;
	int nbrDataPaks;
{
	int i, status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us,"ur1_initMsgPacket(msgPak:%x,msg:%s,nbrDataPaks:%d)",
						msgPacketptr,uu_msg(msg),nbrDataPaks));

	msgPacketptr->message = msg;
	/* get space for the number of data packet points needed */
	msgPacketptr->dataPacket = (UR_dataPacket *) 
									uu_malloc(nbrDataPaks * (sizeof(UR_dataPacket)));
	msgPacketptr->nbrDataPks = nbrDataPaks;
	/* initialize these data buffer pointer records */
	for (i=0; i<nbrDataPaks; i++)
	{
		msgPacketptr->dataPacket[i].dataptr = UU_NULL;
		msgPacketptr->dataPacket[i].datasz = 0;
	}
	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dexitstatus("ur1_initMsgPacket",status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int ur1_addData2MsgPacket(msgPacketptr, dataNbr, dataptr, 
**										datasz)
**			This function adds data to an association update message packet.
**			Note, we do dynamic storage allocation here and currently rely
**			on DAS to free the storage at the end of a semantic action.
**    PARAMETERS   
**       INPUT: 
**				msgPacketptr	Pointer to the message packet for which data
**									is to be added.
**				dataNbr			Number designating where the data buffer is to
**									added to the message packet; starts with 1.
**				dataptr			Pointer to the data to be added.
**				datasz			Size in bytes of the data to be added.
**       OUTPUT:  
**				msgPacketptr	Pointer to the message packet with the new
**									in it.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS:There is no check to see if you are overwriting 
**					another data buffer.
*********************************************************************/
int ur1_addData2MsgPacket(msgPacketptr, dataNbr, dataptr, datasz)
	UR_messagePacket *msgPacketptr;
	int dataNbr;
	char *dataptr;
	int datasz;
{
	char *uu_msg();
	int status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us,
	"ur1_addData2MsgPacket(msgPk:%s,datNbr:%d,datptr:%x,sz:%d)", 
			uu_msg(UR1_GET_MSG(msgPacketptr)), dataNbr, dataptr, datasz));

	if ((dataNbr < 1) || (dataNbr > msgPacketptr->nbrDataPks) || (datasz < 0))
		goto failed;
	if (datasz == 0) goto done;

	/* note, a copy of the data is necessary since the data in "*dataptr"
	 * may be in a local variable which could be freed before it could 
	 * be used. */
	if ((msgPacketptr->dataPacket[dataNbr-1].dataptr = (char *)uu_malloc(datasz))
			== UU_NULL) 
	{
		uu_dprint(-1,(us,"ERROR, no storage allocated for message data"));
		goto failed;
	}
	uu_move_byte(dataptr, msgPacketptr->dataPacket[dataNbr-1].dataptr, datasz);
	msgPacketptr->dataPacket[dataNbr-1].datasz = datasz;

	goto done;
failed: status = UU_FAILURE;
done: 
	uu_dexitstatus("ur1_addData2MsgPacket",status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int ur1_getMsgData(msgPacketptr,dataNbr,dataptrptr,
**													dataszptr)
**			This function returns a pointer to the data buffer in the
**			message packet that is designated by "dataNbr".
**    PARAMETERS   
**       INPUT: 
**				msgPacketptr	Pointer to the message packet from which
**									the data buffer is to be obtained.
**				dataNbr			The number of the data buffer to be obtained.
**									Starts with zero.
**       OUTPUT:  
**				dataptrptr		The pointer to the pointer to the data 
**									buffer, or a pointer to UU_NULL if no
**									such buffer found.
**				dataszptr		Pointer to the size of the data buffer,
**									"*dataptrptr", or zero if no buffer found.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ur1_getMsgData(msgPacketptr,dataNbr,dataptrptr,dataszptr)
	UR_messagePacket *msgPacketptr;
	int dataNbr;
	char **dataptrptr;
	int *dataszptr;
{
	char *uu_msg();
	int status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us, "ur1_getMsgData(msg:%s,dataNbr:%d,?,?)",
						uu_msg(UR1_GET_MSG(msgPacketptr)),dataNbr));

	if ((msgPacketptr == UU_NULL) || (dataNbr < 0) 
			|| (dataNbr >= msgPacketptr->nbrDataPks)
			|| (msgPacketptr->dataPacket[dataNbr].dataptr == UU_NULL))
	{
		*dataptrptr = UU_NULL;
		*dataszptr = 0;
	}
	else /* at least minimally ok */
	{
		*dataptrptr = msgPacketptr->dataPacket[dataNbr].dataptr;
		*dataszptr = msgPacketptr->dataPacket[dataNbr].datasz;
#if (TRACE)
		if (msgPacketptr->dataPacket[dataNbr].datasz <= 0)
		{
			uu_dprint(UU_R1TRC,(us,"data size, %d, is invalid in data packet",
						msgPacketptr->dataPacket[dataNbr].datasz));
			goto failed;
		}
#endif
	}
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ur1_getMsgData", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int ur1_getMsgPacket(descrptr, msgRelPos, msgPacketptrptr)
**			This function retrieves a message packet from the
**			association update descriptor, "descrptr".  The message retrieved
**			is relative to the "current" message packet of the descriptor,
**			"*descrptr" as follows: msgRelPos=0 implies the current message 
**			packet will be retrieved; msgRelPos=n (n>0) implies the nth message
**			forward of the current message will be retrieved; msgRelPos=n (n<0)
**			implies the nth message before the current message will be 
**			retrieved.
**			Note, NO reset of the current message packet is done.
**    PARAMETERS   
**       INPUT: 
**				descrptr				Pointer to the association update descriptor.
**				msgRelPos			Relative position from the current message 
**										packet desired of the message packet desired.
**       OUTPUT:  
**				msgPacketptrptr	Pointer to the pointer to the message packet to 
**										be returned.
**										If there is no such packet, then UU_NULL
**										is returned.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ur1_getMsgPacket(descrptr, msgRelPos, msgPacketptrptr)
	UR_assocUpdateDescriptor *descrptr;
	int msgRelPos;
	UR_messagePacket **msgPacketptrptr;
{
	int newPakNbr, status = UU_SUCCESS;
	uu_denter(UU_R1TRC,(us, "ur1_getMsgPacket(dscptr>ky:%d,msgPkRelPos:%d,?)",
		descrptr->assocKey, msgRelPos));

	if (descrptr->currentMsgPacket == UR1_BAD_MSGPK_NBR)
	{
		*msgPacketptrptr = UU_NULL;
		goto done;
	}
	newPakNbr = (descrptr->currentMsgPacket) + msgRelPos;
	if (newPakNbr < 0) 
	{
		*msgPacketptrptr = UU_NULL;
		goto failed;
	}
	if (newPakNbr >= descrptr->nbrPackets)
		*msgPacketptrptr = UU_NULL;
	else /* get the message */
		*msgPacketptrptr = &(descrptr->msgPacket[newPakNbr]);
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ur1_getMsgPacket", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int ur1_setCurntMsg(descrptr, msgNbr)
**			This function (re)sets the current message packet indicator
**			in "*descrptr" such that if 0<"msgNbr"<=nbr of message packets
**			for this descriptor, then the
**			"msgNbr"_th message packet becomes the current packet;
**			e.g. 1 is the first packet.
**			In all other cases, the current packet is set to an illegitimate
**			value (UR1_BAD_MSGPK_NBR).  If "msgNbr" == nbr of messages, then 
**			UU_SUCCESS is returned even though the current message packet is 
**			set to an illegitimate value (note, this is done so that there 
**			is no failure when trying to get the next message packet for the 
**			current one and run out of messages packets).  However, any other 
**			improper value for "msgNbr" results in a UU_FAILURE being returned.
**    PARAMETERS   
**       INPUT: 
**				descrptr		Pointer to the association update descriptor
**								to have its "current" message packet reset.
**				msgNbr		Number indicating what the new message packet
**								is to be. 1 is the lowest legitimate value.
**       OUTPUT:  
**				descrptr		Pointer to the association update decriptor
**								with a new "current" message packet.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ur1_setCurntMsg(descrptr, msgNbr)
	UR_assocUpdateDescriptor *descrptr;
	int msgNbr;
{
	int status = UU_SUCCESS;
	uu_denter(UU_R1TRC, (us, "ur1_setCurntMsg(descrptr,msgNbr:%d(lowest is 1))",
						msgNbr));
	if ((msgNbr <= 0) || (descrptr->nbrPackets < msgNbr))
	{
		if (descrptr->nbrPackets+1 == msgNbr) /* no next one */
		{
			descrptr->currentMsgPacket = UR1_BAD_MSGPK_NBR;
			uu_dprint(UU_R1TRC,(us,"trying to set current msg just beyond range"));
			goto done;
		}
		descrptr->currentMsgPacket = UR1_BAD_MSGPK_NBR;
		goto failed;
	}
	descrptr->currentMsgPacket = msgNbr-1;

	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ur1_setCurntMsg",status);
	return(status);
}	
