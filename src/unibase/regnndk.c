/*********************************************************************
**    NAME         :  regnndk.c
**       CONTAINS:
**       ur_get_next_new_data_key()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       regnndk.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:30
*********************************************************************/

#include	"udebug.h"
#include "ribase.h"

/*********************************************************************
** E_FUNCTION : status = ur_get_next_new_data_key(rel_num,&next_tupleid,&key_id)
**      get next new active key_id for rel = rel_num
**    PARAMETERS   
**       INPUT  : 
**			rel_num,			the type of data(relation) to find entry in 
**			next_tupleid,	tuple id to start search from
**       OUTPUT :  
**			next_tupleid,	index to where next active tuple was found
**			key_id,			the identifier for the master associated with this entry
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_get_next_new_data_key(rel_num,nxt_tid,key_id)
/* argument declarations         */
int			rel_num;		/* relation to find next entry in	*/
int			*nxt_tid;	/* entry to start looking from		*/
UU_KEY_ID	*key_id;		/* ptr to key_id to be returned			*/
{
	/* local  parameter declarations */
	int			status;	/* status, -1 if error, 0 otherwise	*/
	UU_REL_ID	rel_key;	/*  a relation,tuple_indx key			*/

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
	uu_denter(UU_RTRC,(us,"ur_get_next_new_data_key for rel_num %d, from nxt_tid = %d",rel_num,*nxt_tid));
	status 	=	0;
/*
	go get next active key_id entry for the specifed relation
*/
	status = ur_get_next_new_tuple_index(rel_num,nxt_tid);
	if(status == 0)
	{
		ur_rt2k(rel_num,*nxt_tid,&rel_key);
		if(uu_tst_bit(&UR_rcb[rel_num].rel_flags,UR_MTUPLE_REQD))
		{
			status = ur_retrieve_data_tuple_key(rel_key,key_id);
		}
		else
		{
			*key_id = rel_key;
		}
	}
	uu_dexit;
	return(status);
}
