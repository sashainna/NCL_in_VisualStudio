/*********************************************************************
**    NAME         :  regnk.c
**       CONTAINS:
**       ur_get_next_key()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       regnk.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:30
*********************************************************************/

#include "usysdef.h"
#include	"udebug.h"
#include "rbase.h"
#include	"rmtuple.h"

/*********************************************************************
**    E_FUNCTION     :  status = ur_get_next_key(&next_tupleid,&key_id)
**      get key_id for next active master tuple
**    PARAMETERS   
**       INPUT  : 
**				next_tupleid, entry to start search from
**       OUTPUT :  
**				next_tupleid, index of next active tuple
**				key_id,		next active key_id
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_get_next_key(nxt_tid,key_id)
/* argument declarations */
UR_TUPLE_INDX	*nxt_tid;	/* tuple to start looking from */
UU_KEY_ID		*key_id;		/* where to put next active key_id */
{
	/* local  parameter declarations */
	int		status;		/* status, -1 if error, 0 otherwise	*/

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
	uu_denter(UU_RTRC,(us,"ur_get_next_key from %d",*nxt_tid));

	/* go get next active key_id entry */
	status = ur_get_next_tuple_index(UR_MTUPLE_REL,nxt_tid);
	ur_rt2k(UR_MTUPLE_REL,*nxt_tid,key_id);
	uu_dexit;
	return(status);
}
