/*********************************************************************
**    NAME         :  rirdtk
**       CONTAINS:
**       ur_retrieve_data_tuple_key
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rirdtk.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:47
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "rbase.h"

/*********************************************************************
** I_FUNCTION : status = ur_retrieve_data_tuple_key(rel_key,&key_id)
**		retrieve relation entry key_id
**    PARAMETERS   
**       INPUT  : 
**				rel_key,	rel,ent to retrieve key_id from
**       OUTPUT :  
**				key_id, 	the master tuple this entry is related to,
**							if any
**    RETURNS      :  0 if function was successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS    : the key_id of a relation,entry can only be retrieved
**						if that relation,entry is active, otherwise it is
**						treated as an error condition
*********************************************************************/

ur_retrieve_data_tuple_key(rel_key,key_id)
/* argument declarations         */
UU_REL_ID	rel_key;	/* a relation,entry key						*/
UU_KEY_ID	*key_id;	/* the key_id of this relation,if any			*/
{
	/* local  parameter declarations */
	int	status;						/* return status							*/
	struct UR_data	*pack_ptr;		/* pointer to fixed data packet		*/
	int	lst_len;						/* length of the fixed data			*/
	int	rel_num;
	int	tuple_indx;

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/

	uu_denter(UU_RITRC,(us,"ur_retrieve_data_tuple_key"));
/*
	go get list address -  if none, then error
*/
	ur_k2rt(rel_key, &rel_num, &tuple_indx);
	status = ur_get_varlist_ptr(rel_num, tuple_indx,0,&pack_ptr,&lst_len);
	if(status == 0)
	{
		*key_id =  pack_ptr->key_id;	/* get the assc key_id	*/
	}
	uu_dprint(UU_RITRC,(us," retrieved key_id = 0x%x, from tuple 0x%x",
					*key_id,rel_key));
	uu_dexit;
	return(status);
}
