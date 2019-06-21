/*********************************************************************
**    NAME         :  regnodk.c
**       CONTAINS:
**       ur_get_next_old_data_key()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       regnodk.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:30
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) regnodk.c 2.2 9/12/86 15:16:38 single"};
#else
static char uu_sccsident[]={"@(#) regnodk.c 2.2 9/12/86 15:16:38 double"};
#endif
#endif

#include	"udebug.h"
#include "rbase.h"
#include "ribase.h"

/*********************************************************************
** E_FUNCTION : status = ur_get_next_old_data_key(rel_num,&next_tid,&key_id)
**      get next old active key_id for rel = rel_num
**    PARAMETERS   
**       INPUT  : 
**			rel_num,			the type of data(relation) to find entry in 
**			next_tid,		tuple id to start search from
**       OUTPUT :  
**			next_tid,		index to where next active tuple was found
**			key_id,			the identifier for the master associated with this entry
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_get_next_old_data_key(rel_num,nxt_tid,key_id)
UR_REL_NUM		rel_num;		/* relation to find next entry in	*/
UR_TUPLE_INDX	*nxt_tid;	/* entry to start looking from		*/
UU_KEY_ID		*key_id;		/* ptr to key_id to be returned			*/
{
	int			status;	/* status, -1 if error, 0 otherwise	*/
	UU_REL_ID	rel_key;	/*  a relation,tuple_indx key			*/

	uu_denter(UU_RTRC,(us, "ur_get_next_old_data_key(rel=%d, nxt_tid=%d)",
					rel_num, *nxt_tid));
	status 	=	0;

	/* go get next active key_id entry for the specifed relation */
	status = ur_get_next_old_tuple(rel_num, nxt_tid);
	if (status == 0)
	{
		ur_rt2k(rel_num, *nxt_tid, &rel_key);
		if (uu_tst_bit(&UR_rcb[rel_num].rel_flags, UR_MTUPLE_REQD))
		{
			status = ur_retrieve_data_tuple_key(rel_key, key_id);
		}
		else
		{
			*key_id = rel_key;
		}
	}
	uu_dexit;
	return(status);
}
