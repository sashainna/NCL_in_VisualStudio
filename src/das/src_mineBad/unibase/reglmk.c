/*********************************************************************
**    NAME         :  reglmk.c
**       CONTAINS:
**       ur_get_last_modified_mkey()
**       ur_get_last_modified_rkey()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reglmk.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:30
*********************************************************************/

#include  "usysdef.h"
#include  "udebug.h"
#include  "ribase.h"

/*********************************************************************
**    E_FUNCTION     :  status = ur_get_last_modified_mkey(&key_id) 
**       get key of last modified master tuple
**    PARAMETERS   
**       INPUT  : 
**          &key_id, address of where to put the key_id
**       OUTPUT :  
**          none
**    RETURNS      : -1, no last modified master key
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_get_last_modified_mkey(key_id)
UU_KEY_ID	*key_id	;	/* address of key to be returned		*/
{
	int	status	;	/* return status								*/

	uu_denter(UU_RTRC,(us,"ur_get_last_modified_mkey"));
	status = 0;
	*key_id = UR_last_mod_mkey;
	if(*key_id == 0)
	{
		status = -1 ;
	}
	uu_dprint(UU_RITRC,(us,"key = 0x%x", *key_id));
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  status = ur_get_last_modified_rkey(&rel_key) 
**       get key of last modified tuple
**    PARAMETERS   
**       INPUT  : 
**          &rel_key, address of where to put the rel_key
**       OUTPUT :  
**          none
**    RETURNS      : -1, no last modified tuple key
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_get_last_modified_rkey(rel_key)
UU_REL_ID	*rel_key;	/* address of key to be returned		*/
{
	int	status	;	/* return status								*/

	uu_denter(UU_RTRC,(us,"ur_get_last_modified_rkey"));
	status = 0;
	*rel_key  = UR_last_mod_rkey;
	if(*rel_key == 0)
	{
		status = -1 ;
	}
	uu_dprint(UU_RTRC,(us,"key = 0x%x", *rel_key));
	uu_dexit ;
	return(status) ;
}

