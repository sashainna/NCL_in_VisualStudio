/*********************************************************************
**    NAME:  umsg2stg.c
**       CONTAINS:
**				char *uu_msg(messageNbr)
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       umsg2stg.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:54
*********************************************************************/
#include "umessages.h"
/*********************************************************************
**    E_FUNCTION: char *uu_msg(messageNbr)
**       This function returns the character string associated with the
**			message, "message".  This function is mainly for printing
**			messages as character strings in the "trc" file when the trace is 
**			turned on.
**				e.g. instead of:
**				uu_dprint(UU_?TRC,(us,"someFunctName(message:%d,%d"),
**												message[0], message[1]));
**				you can have:
**				uu_dprint(UU_?TRC,(us,"someFunctName(message:%s)",
**												uu_msg(message)));
**    PARAMETERS   
**       INPUT: 
**          messageNbr		Numerical description of the message.
**       OUTPUT: none. 
**    RETURNS: The corresponding string equivalent of the message.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
char *uu_msg(messageNbr)
	int messageNbr;
{
	return(uu_message[messageNbr]);
}	

