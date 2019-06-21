/*********************************************************************
**    NAME         :  rervk.c
**       CONTAINS:
**       ur_retrieve_view_key(master_key,&view_key)
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rervk.c , 25.1
**       rera.c , 1.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:35
*********************************************************************/

#include  "usysdef.h"
#include	"udebug.h"
#include	"ribase.h"
#include	"riddle.h"

/*********************************************************************
** E_FUNCTION : status = ur_retrieve_view_key(master_key,&view_key)
**      retrieve view key
**    PARAMETERS   
**       INPUT  : 
**			master_key, master key_id for which to retrieve the view key
**			&view_key, the address of where to put the view key
**       OUTPUT :  
**				none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_view_key(master_key,view_key)
UU_KEY_ID      master_key;
UU_KEY_ID      *view_key;
{
	struct UR_MTID_rec	*m_ptr;	/* pointer to a master tuple */
	int						status;
	int						rel_num;
	int						tuple_indx;

	uu_denter(UU_RTRC,(us,"ur_retrieve_view_key for key_id= 0x%x", master_key));
	*view_key = 0;
	ur_k2rt(master_key, &rel_num, &tuple_indx);
	status = ur_get_tuple_ptr(rel_num, tuple_indx, &m_ptr);
	if(status == 0)
		*view_key = m_ptr->view_key;
	uu_dprint(UU_RITRC,(us,"ur_retrieve_view_key, view_key = 0x%x, status=%d*",
			*view_key,status));
	uu_dexit;
	return(status);
}

