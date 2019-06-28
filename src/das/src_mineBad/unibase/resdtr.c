/*********************************************************************
**    NAME         :  resdtr.c
**       CONTAINS:
**       ur_set_default_transf()
**			ur_has_default_transf()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       resdtr.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:36
*********************************************************************/

#include  "usysdef.h"
#include	"udebug.h"
#include	"ribase.h"
#include "rmtuple.h"
#include "mattrddl.h"

/*********************************************************************
** E_FUNCTION: status = ur_set_default_transf(&transf_packet)
**      set a default matrix for transformation relation transf_rel
**    PARAMETERS   
**       INPUT  : 
**				&transf_packet, address of transformation packet to 
**							to use a deault
**				transf_packet.key, 0
**       OUTPUT :  
**				none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_set_default_transf(transf_packet)
struct	UM_transf_rec  *transf_packet	;	/* ptr to transformation data packet */
{
	struct UR_rcb_rec *rcb;         /* current relations rcb block */
/*	struct UM_transf_rec  transf;        generic transformation tuple */
	int				status;
	int            retcod;
	UU_REL_ID      rel_key;
	UR_REL_NUM     rel_num;
	UR_TUPLE_INDX  tuple_indx;

	uu_denter(UU_RTRC,(us,"ur_set_default_transf"));
	transf_packet->key = 0;

	/* define the default with use count 0 otherwise update the existing */
	transf_packet->rel_num = UR_transf_relnum;
	transf_packet->use_count = 0;
	status = ur_update_tuple(UR_transf_relnum,1,transf_packet);
	if(status == 0)
	{
		UR_default_transf = UU_TRUE;
	}
	uu_dprint(UU_RITRC,(us,"ur_set_default_transf exit status=%d",status));
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION :  UU_LOGICAL ur_has_default_transf(key)
**       determines whether the given entity has the default transformation
**    PARAMETERS   
**       INPUT  : 
**          key	UU_KEY_ID	subject of the enquiry
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE is the entity has the default, UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UU_LOGICAL ur_has_default_transf(key)
UU_KEY_ID	key;
{
	UU_KEY_ID	rel_key;
	int			status;

	status = ur_retrieve_master_attr(key, UR_TRANSF_INDX, &rel_key);
	if (rel_key)
		return(UU_FALSE);
	else
		return(UU_TRUE);
}

