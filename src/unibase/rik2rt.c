/*********************************************************************
**    NAME         :  rik2rt
**       CONTAINS:
**       ur_k2rt
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rik2rt.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:44
*********************************************************************/

#include "usysdef.h"
#include "ribase.h"
#include "udebug.h"

/*********************************************************************
**    I_FUNCTION     :  status = ur_k2rt(key_id,&rel_id,&tuple_indx)
**      convert a tuple id to a relation id, and an entry id
**    PARAMETERS   
**       INPUT  : 
**				key_id, key to be disassembled
**         	rel_num, ptr to a word in which to place the relation id
**				tuple_indx, ptr to a word in which to place the entry id
**       OUTPUT :  
**				the relation num
**				the tuple index
**    RETURNS      :  0, or -1 if function was not successful
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
ur_k2rt(key_id,rnum_ptr,tindx_ptr)
/* argument declarations		*/
UU_KEY_ID 		key_id;		/* tuple key to be converted				*/
UR_REL_NUM		*rnum_ptr;	/* ptr to where to put the relation num*/
UR_TUPLE_INDX	*tindx_ptr;	/* ptr to where to put the tuple index	*/
{
	uu_denter(UU_RTRC,(us,"ur_k2rt(key=0x%x)",key_id));
	*rnum_ptr	=	(key_id >> UR_RSHFT);	/* shift right to get rel */
	*tindx_ptr	=	(key_id & UR_EMASK);/* mask off the entry from the key_id */
	uu_dexit;
	return(0);
}

