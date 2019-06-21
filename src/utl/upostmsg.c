/*********************************************************************
**    NAME:  umsg.c
**       CONTAINS:
**    		uu_postMessage(msgDescrptr)
**    		uu_retrieveMessage(msgDescrptr)
**    		uu_deleteMessages()
**    		uu_displayMessage()
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       upostmsg.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:55
*********************************************************************/
#include "usysdef.h"	/* basic data types */
#include "uhep.h"		/* for error system */
#include "udebug.h"	/* for debugging trace facility */
#include "umessages.h"/* for UU_messageDescriptor */

/*********************************************************************
**    E_FUNCTION: int uu_postMessage(msgDescrptr)
**		This function puts the filled-in message descriptor into a global 
**		message data structure.  If this function is compiled with debug then 
**		each posted message will be output to the "trc" file.
**    PARAMETERS   
**		INPUT: 
**			msgDescrptr	Pointer to the message descriptor to be put in the
**									message structure.
**		OUTPUT:		none.
**    RETURNS:		UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
int uu_postMessage(msgDescrptr)
	UU_messageDescriptor *msgDescrptr;
{
	int status = UU_SUCCESS;
	uu_denter(UU_UITRC,(us,"uu_postMessage(%s,%s)", 
		"not implemented, message is:", msgDescrptr));
	uu_dexitstatus("uu_postMessage", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int uu_retrieveMessage(msgDescrptr)
**		This function retrieves a filled-in message from the global message 
**		data structure.
**    PARAMETERS   
**		INPUT: none.
**		OUTPUT:
**			msgDescrptr	Pointer to a filled-in message descriptor to be 
**									put; if a message was found.
**    RETURNS:UU_SUCCESS if a message can be retrieved, UU_FAILURE otherwise.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
int uu_retrieveMessage(msgDescrptr)
	UU_messageDescriptor *msgDescrptr;
{
	int status = UU_SUCCESS;
	uu_denter(UU_UITRC,(us,"uu_retrieveMessage(%s,%s)", 
		"not implemented, message is:", msgDescrptr));
	uu_dexitstatus("uu_retrieveMessage", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int uu_deleteMessages()
**		This function deletes all messages that are in the messager.
**    PARAMETERS   
**		INPUT: none.
**		OUTPUT:none.
**    RETURNS:	UU_SUCCESS if a message can be retrieved, UU_FAILURE otherwise.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
int uu_deleteMessages()
{
	int status = UU_SUCCESS;
	uu_denter(UU_UITRC,(us,"uu_deleteMessages(%s)", "not implemented yet"));
	uu_dexitstatus("uu_deleteMessages", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int uu_displayMessage()
**		This function displays the most recent message and deletes it from 
**		the messager. If compiled with debug, this function also print the 
**		message in the "trc" file.
**    PARAMETERS   
**		INPUT: none.
**		OUTPUT:none.
**    RETURNS:	UU_SUCCESS if a message can be displayed, UU_FAILURE otherwise.
**    SIDE EFFECTS:	none
**    WARNINGS:		none
*********************************************************************/
int uu_displayMessage()
{
	int status = UU_SUCCESS;
	uu_denter(UU_UITRC,(us,"uu_retrieveMessage(%s)", 
		"not implemented yet"));
	uu_dexitstatus("uu_displayMessage", status);
	return(status);
}
