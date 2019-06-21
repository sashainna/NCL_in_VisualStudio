/*********************************************************************
**    NAME         :  riut
**       CONTAINS:
**       ur_update_tuple
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       riut.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:48
*********************************************************************/

#include "udebug.h"
#include "umoveb.h"
#include "ribase.h"
#include "rmtuple.h"

/*********************************************************************
**    I_FUNCTION : status = ur_update_tuple(rel,tuple,data_ptr)
**		store a relation entry fixed data
**    PARAMETERS   
**       INPUT  : 
**				rel_key,	a relation,entry tuple
**				data_ptr, pointer of list to store
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function was successful, -1 otherwise
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_tuple(rel,tuple,data_ptr)
UR_REL_NUM		rel;		/* the relation number */
UR_TUPLE_INDX	tuple;	/* tuple index within the relation */
char 				*data_ptr;	/* ptr to a lst of data, where the data is */
{
 	UU_REL_ID	rel_key;		/* a relation,tuple index key				*/
	int			status;		/* return status, -1 if error				*/
	char			*lst_ptr;	/* ptr to a lst, where to put the data	*/
	int			lst_len;		/* the length of the fixed length data	*/

/*----------------------------------------------------------------------
** Start of Executable Code
**----------------------------------------------------------------------
**
*/
	uu_denter(UU_RTRC,(us,"update_tuple(rel=%d, tuple=%d)",rel,tuple));

	/* get list adrs - might not be one if not active, but try anyway */
	status = ur_get_varlist_ptr(rel, tuple,0,&lst_ptr,&lst_len);

	/* adjust length to not step on varlists */
	lst_len -= UR_rcb[rel].n_varl * sizeof(UR_lpacket);

	/* if active, move the data */
	if (status == 0)
	{
		UR_rcb[rel].last_accessed_index = tuple;
		uu_move_byte(data_ptr,lst_ptr,lst_len);
		ur_rt2k(rel,tuple,&rel_key);
		ur_unibase_change(rel);
		if(rel == UR_MTUPLE_REL)
		{
			UR_last_mod_mkey = rel_key;
		}
		else
		{
			UR_last_mod_rkey = rel_key;
		}
	}
	uu_dexit;
	return(status);
}
