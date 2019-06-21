/*********************************************************************
**    NAME         :  rera
**       CONTAINS:
**       ur_retrieve_attr(&attr_packet)
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rera.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:33
*********************************************************************/

#include  "usysdef.h"
#include	"udebug.h"
#include	"ribase.h"
#include	"rmtuple.h"

/*********************************************************************
** E_FUNCTION : status = ur_retrieve_attr(&attr_packet)
**      get attribute tuple data for existing key_id (master tuple).
**    PARAMETERS   
**       INPUT  : 
**				&attr_packet, address of attribute packet to put data
**				attr_packet.key_id, must equal key_id of master to get
**       OUTPUT :  
**				none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_attr(attr_packet)
struct	UR_attr  *attr_packet	;	/* pointer to attr data packet */
{
	/*------------ local declares ---------------------------------------*/
	int				status;
	UU_REL_ID      rel_key;
	UU_KEY_ID      master_key;
	UR_REL_NUM     rel_num;
	UR_TUPLE_INDX  tuple_indx;
	int            i;
	/*------------ begin function code ----------------------------------*/

	uu_denter(UU_RTRC,(us,"ur_retrieve_attr for key_id= 0x%x",
					attr_packet->key_id));
	master_key = attr_packet->key_id;	/* save master key */

	/* get attr tuple key */
	status = ur_retrieve_master_attr(master_key, UR_ATTR_INDX, &rel_key);
	if (status!=0)
	{
		uu_dprint(-1,(us,"ERROR:ur_retrieve_attr can't retrieve attribute key from master 0x%x",
							master_key));
		goto exit;					/* cannot continue */
	}
	if(rel_key==0)					/* no attr data */
	{
		uu_dprint(-1,(us,"ERROR:ur_retrieve_attr no attribute key associated with master 0x%x",
							master_key));
		status = -1;
		goto exit;					/* cannot continue */
	}
	ur_k2rt(rel_key, &rel_num, &tuple_indx);	/* get relation and index*/

	/* and go get tuple data */
	status = ur_retrieve_tuple(rel_num, tuple_indx, attr_packet);
	attr_packet->key_id = master_key; /* reset in case diff(use>1)*/

	/*------------ function exit --------------------------------------*/
exit:                            /***** EXIT LABEL ******/
	uu_dexit;
	return(status);
}
