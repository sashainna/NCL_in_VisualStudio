/*********************************************************************
**    NAME:  bpermsn.c
**       CONTAINS:
**				ub_instPermsn
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       bpermsn.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:04
*********************************************************************/
#include "usysdef.h"
#include "uhep.h"		/* error message identifiers */
#include "class.h"
#include "ulist.h"
#include "dasnog.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mdattr.h"
#include "udebug.h"
#include "umessages.h"	/* for UU_ADD2CONNECTOR */
#include "r1esched.h"
#include "rbase.h"		/* for UR_REL_NAME */
#include "bsym.h"			/* for instance record */

#define UB_PRINTERRORS	\
	if (printErrs)			\
	{		\
		if (ur_retrieve_rel_name(objptr->key, name) != 0)			\
		{	\
			uu_uerror1(UB_SYMBOL,1111,objptr->key);					\
			/* error is: Can't find name of entity with key:%d, (%s). */			\
			goto failed;	\
		}						\
		uu_uerror2(UB_SYMBOL,1111, uu_msg(msgPacket[0].message), objptr->key);		\
		/* error: Permission to %s (key %d) not granted, semantic conflict(s)	\
		 * exists. */		\
		goto failed;		\
	} if (UU_TRUE)

/*********************************************************************
**    E_FUNCTION: int ub_instPermsn(objptr,msgPacket,nbrMsgPaks,
**										printErrs,retPermptr)
**			This function determines if the instance pointed to by, "objptr",
**			can be modified according to the messages in "msgPacket".
**    PARAMETERS   
**       INPUT: 
**				objptr		Pointer to the object to which permission is sought
**								to be modified according to the semantics of the
**								messages in "msgPacket".
**				msgPacket	Message packets containing the messages for the
**								modifications of "objptr".
**				nbrMsgPaks	Number of valid message packets in "msgPackets".
**				printErrs	UU_TRUE iff any errors encountered should be printed.
**       OUTPUT:  
**				retPermptr	Pointer UU_TRUE iff permission is granted.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_instPermsn(objptr,msgPacket,nbrMsgPaks,printErrs,retPermptr)
	struct UB_instance_rec *objptr;	
	UR_messagePacket msgPacket[];
	int nbrMsgPaks;
	UU_LOGICAL printErrs;
	UU_LOGICAL *retPermptr;
{
	char *uu_msg();
	UR_REL_NAME		name;
	int status = UU_SUCCESS;
	uu_denter((UU_R1TRC|UU_BTRC),(us,
		"ub_instPermsn(objptr>ky:%d,1stmsg:%s,nbrMsgPaks:%d,prtErrs:%d,?)",
			objptr->key, uu_msg(UR1_GET_MSG(msgPacket)), nbrMsgPaks, printErrs));

	switch (msgPacket[0].message)
	{
		case UU_TRANSLATE: 
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_ROTATE: 
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_MIRROR:   
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_SCALE:   
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DELETE: 
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_VIS:   
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_INVIS:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_CHANGE_ATTR:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_REGENERATE:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_NEVERDISPLAYABLE:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DISPLAYABLE: 
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_UPDATE_TRANSF:   
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_NOMESSAGE: 
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_UPDATE:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_UPDT_FIXED:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_UPDT_DVARL:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_UPDT_TRANS:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_UPDT_ATTR:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_UPDT_DSEG:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_UPDT_COLOR:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_UPDT_LAYER:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_UPDT_PEN:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_UPDT_LNSTY:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_UPDT_LNWGT:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_UPDT_LNWID:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_UPDT_DISP:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_UPDT_SEL:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_UPDT_BLNKD:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_UPDT_VKEY:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_DELETE:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_DEL_ATTR:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_DEL_DVARL:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_DEL_TRANS:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		case UU_DEFAULT_DEL_VARLA:
			*retPermptr = UU_TRUE;
			if (!(*retPermptr)) UB_PRINTERRORS;
			break;
		default:
			*retPermptr = UU_FALSE;
			if (printErrs)			
			{		
				if (ur_retrieve_rel_name(objptr->key, name) != 0)			
				{	
					uu_uerror2(UB_SYMBOL,1111,objptr->key,"ub_instPermsn");	
					/* error is: Can't find name of entity with key:%d, (%s). */			
					goto failed;	
				}						
				uu_uerror3(UB_SYMBOL,1111, uu_msg(msgPacket[0].message), name, 
								objptr->key);		
				/* error: Unknown permission message: %s, entity: %s, key: %d. */
			}
			goto failed;
	}
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ub_instPermsn", status);
	return(status);
}
	
/*********************************************************************
**    E_FUNCTION: int ub_connPermsn(objptr,msgPacket,nbrMsgPaks,
**										printErrs,retPermptr)
**			This function determines if the connentor pointed to by, "objptr",
**			can be modified according to the messages in "msgPacket".
**    PARAMETERS   
**       INPUT: 
**				objptr		Pointer to the object to which permission is sought
**								to be modified according to the semantics of the
**								messages in "msgPacket".
**				msgPacket	Message packets containing the messages for the
**								modifications of "objptr".
**				nbrMsgPaks	Number of valid message packets in "msgPackets".
**				printErrs	UU_TRUE iff any errors encountered should be printed.
**       OUTPUT:  
**				retPermptr	Pointer UU_TRUE iff permission is granted.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_connPermsn(objptr,msgPacket,nbrMsgPaks,printErrs,retPermptr)
	struct UC_entitydatabag *objptr;	
	UR_messagePacket msgPacket[];
	int nbrMsgPaks;
	UU_LOGICAL printErrs;
	UU_LOGICAL *retPermptr;
{
	char *uu_msg();
	UR_REL_NAME		name;
	int status = UU_SUCCESS;
	uu_denter((UU_R1TRC|UU_BTRC),(us,
		"ub_connPermsn(objptr>ky:%d,1stmsg:%s,nbrMsgPaks:%d,prtErrs:%d,?)",
			objptr->key, uu_msg(msgPacket[0].message), nbrMsgPaks, printErrs));
	switch (msgPacket[0].message)
	{
		case UU_TRANSLATE: 
			*retPermptr = UU_TRUE;
			if (!*retPermptr) UB_PRINTERRORS;
			break;
		case UU_ROTATE: 
			*retPermptr = UU_TRUE;
			if (!*retPermptr) UB_PRINTERRORS;
			break;
		case UU_MIRROR:   
			*retPermptr = UU_TRUE;
			if (!*retPermptr) UB_PRINTERRORS;
			break;
		case UU_SCALE:   
			*retPermptr = UU_TRUE;
			if (!*retPermptr) UB_PRINTERRORS;
			break;
		case UU_DELETE: 
			*retPermptr = UU_TRUE;
			if (!*retPermptr) UB_PRINTERRORS;
			break;
		case UU_VIS:   
			*retPermptr = UU_TRUE;
			if (!*retPermptr) UB_PRINTERRORS;
			break;
		case UU_INVIS:
			*retPermptr = UU_TRUE;
			if (!*retPermptr) UB_PRINTERRORS;
			break;
		case UU_CHANGE_ATTR:
			*retPermptr = UU_TRUE;
			if (!*retPermptr) UB_PRINTERRORS;
			break;
		case UU_REGENERATE:
			*retPermptr = UU_TRUE;
			if (!*retPermptr) UB_PRINTERRORS;
			break;
		case UU_ADD2CONNECTOR:   
			*retPermptr = UU_TRUE;
			if (!*retPermptr) UB_PRINTERRORS;
			break;
		case UU_NEVERDISPLAYABLE:
			*retPermptr = UU_TRUE;
			if (!*retPermptr) UB_PRINTERRORS;
			break;
		case UU_DISPLAYABLE: 
			*retPermptr = UU_TRUE;
			if (!*retPermptr) UB_PRINTERRORS;
			break;
		case UU_UPDATE_TRANSF:   
			*retPermptr = UU_TRUE;
			if (!*retPermptr) UB_PRINTERRORS;
			break;
		case UU_NOMESSAGE: 
			*retPermptr = UU_TRUE;
			if (!*retPermptr) UB_PRINTERRORS;
			break;
		default:
			*retPermptr = UU_FALSE;
			if (printErrs)			
			{		
				if (ur_retrieve_rel_name(objptr->key, name) != 0)			
				{	
					uu_uerror2(UB_SYMBOL,1111,objptr->key,"ub_connPermsn");	
					/* error is: Can't find name of entity with key:%d, (%s). */			
					goto failed;	
				}						
				uu_uerror3(UB_SYMBOL,1111, uu_msg(msgPacket[0].message), name, 
									objptr->key);		
				/* error: Unknown permission message: %s, entity: %s, key: %d. */
			}
			goto failed;
	}
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("ub_connPermsn", status);
	return(status);
}
	

	
