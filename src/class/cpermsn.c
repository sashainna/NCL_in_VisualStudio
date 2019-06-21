/*********************************************************************
**    NAME:  cpermsn.c
**       CONTAINS:
**				uc_permission
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       cpermsn.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:04:59
*********************************************************************/
#include "usysdef.h"
#include "uhep.h"
#include "class.h"
#include "ulist.h"
#include "dasnog.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mdattr.h"
#include "udebug.h"
/*  #include "umessages.h"	/* for UU_ADD2CONNECTOR */
#include "rbase.h"		/* for UR_REL_NAME */
#include "r1esched.h"

#define UC_PRINTERRORS	\
	if (printErrs)			\
	{		\
		if (ur_retrieve_rel_name(objptr->key, &name) != 0)			\
		{	\
			uu_uerror2(UU_CLASS,3,objptr->key,"uc_permission");	\
			/* error is: Can't find name of entity with key:%d, (%s). */			\
			goto failed;	\
		}						\
		uu_uerror2(UU_CLASS,2, uu_msg(msgPacket[0].message), objptr->key);		\
		/* error: Permission to %s (key %d) not granted, semantic conflict(s)	\
		 * exists. */		\
		goto failed;		\
	}

/*********************************************************************
**    E_FUNCTION: int uc_permission(objptr,msgPacket,nbrMsgPaks,
**										printErrs,retPermptr)
**			This function determines if the object pointed to by, "objptr",
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
int uc_permission(objptr,msgPacket,nbrMsgPaks,printErrs,retPermptr)
	struct UC_entitydatabag *objptr;	
	UR_messagePacket msgPacket[];
	int nbrMsgPaks;
	UU_LOGICAL printErrs;
	UU_LOGICAL *retPermptr;
{
	char *uu_msg();
	UR_REL_NAME		name;
	int status = UU_SUCCESS;
	uu_denter((UU_R1TRC|UU_MTRC),(us,
		"uc_permission(objptr>ky:%d,1stmsg:%s,nbrMsgPaks:%d,prtErrs:%d,?)",
			objptr->key, uu_msg(msgPacket[0].message), nbrMsgPaks, printErrs));
	if (nbrMsgPaks <= 0) 
	{
		uu_dprint((UU_R1TRC|UU_MTRC),(us,"NO MESSAGE PACKETS"));
		goto failed;
	}
	switch (objptr->rel_num)
	{
		case UB_INSTANCE_REL: 
			if (ub_instPermsn(objptr,msgPacket,nbrMsgPaks,printErrs,retPermptr)
					!= UU_SUCCESS) goto failed;
			break;
		case UB_CONECTOR_REL: 
			if (ub_connPermsn(objptr,msgPacket,nbrMsgPaks,printErrs,retPermptr)
					!= UU_SUCCESS) goto failed;
			break;
		default:
			*retPermptr = UU_FALSE;
			if (printErrs)			
			{		
				if (ur_retrieve_rel_name(objptr->key, name) != 0)			
				{	
					uu_uerror2(UU_CLASS,3,objptr->key,"uc_permission");	
					/* error is: Can't find name of entity with key:%d, (%s). */			
					goto failed;	
				}						
				uu_uerror3(UU_CLASS,4, uu_msg(msgPacket[0].message), name, 
							objptr->key);		
				/* error: Unknown permission message: %s, entity: %s, key: %d. */
			}
			goto failed;
	}
	goto done;
failed: status = UU_FAILURE;
done:;
	uu_dexitstatus("uc_permission", status);
	return(status);
}
	

	
