/*********************************************************************
**    NAME         :  rirt2k
**       CONTAINS:
**       ur_rt2k
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rirt2k.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:47
*********************************************************************/

#include "usysdef.h"
#include "ribase.h"
#include	"udebug.h"

/*********************************************************************
**    I_FUNCTION     :  ur_rt2k(rel_num,tuple_indx,key)
**     definition:	Convert a relation numbe, tuple id 
**     				to a key identifier.
**    PARAMETERS   
**       INPUT  : 
**         rel_num, relation number
**			 tuple_indx, tuple identifier within the relation
**       OUTPUT :  
**			 key, a constructed key
**    RETURNS      : 0
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_rt2k(rel_num,tuple_indx,key)
long rel_num;
long tuple_indx;
UU_KEY_ID *key;
{
	int	status;	/* return status */

	uu_denter(UU_RTRC,(us,"ur_rt2k(rel=%d, tuple=%d, key adr= 0x%x)",
					rel_num,tuple_indx,key));
	status	=	0;
	*key = ((rel_num << UR_RSHFT) | tuple_indx);
	uu_dprint(UU_RITRC,(us,"ur_rt2k, key = 0x%x",*key));
	uu_dexit;
	return(status);
}
