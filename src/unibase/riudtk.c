/*********************************************************************
**    NAME         :  riudtk.c
**       CONTAINS:
**       ur_update_data_tuple_key()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       riudtk.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:48
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"

/*********************************************************************
**    I_FUNCTION  : status = ur_update_data_tuple_key(rel_key,key_id)
**		update relation entry key_id
**    PARAMETERS   
**       INPUT  : 
**				rel_key,	rel,ent to update key_id in
**				key_id, 	the master tuple this entry is to be related to,
**							if any
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function was successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     :  the key_id of a relation,entry can only be updated
**						if that relation,tuple is active, otherwise it is
**						treated as an error condition
*********************************************************************/

ur_update_data_tuple_key(rel_key,key_id)
UU_REL_ID	rel_key;	/* a relation,entry tuple */
UU_KEY_ID	key_id;	/* the key_id of this relation,if any */
{
	int				status;		/* return status */
	UU_KEY_ID		*lst_ptr;	/* pointer to fixed data */
	int				lst_len;		/* length of the fixed data */
	int				rel_num;
	int				tuple_indx;

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
	uu_denter(UU_RITRC,(us,"update_data_tuple_key(rel_key= 0x%x, key_id= 0x%x",
						rel_key,key_id));

	/* go get list address -  if none, then error */
	ur_k2rt(rel_key, &rel_num, &tuple_indx);
	status = ur_get_varlist_ptr(rel_num, tuple_indx,0,&lst_ptr,&lst_len);

	/* key_id is the first attribute of the tuple */
	if(status == 0)
	{
		*lst_ptr = key_id;	/* set the associated key_id	*/
		ur_unibase_change(rel_num);
	}
	uu_dexit;
	return(status);
}
