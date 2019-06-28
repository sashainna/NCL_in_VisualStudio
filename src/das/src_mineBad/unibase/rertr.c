/*********************************************************************
**    NAME         :  rertr.c
**       CONTAINS:
**       ur_retrieve_transf()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rertr.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:35
*********************************************************************/

#include  "usysdef.h"
#include	"udebug.h"
#include	"rbase.h"
#include	"ribase.h"
#include	"mattrddl.h"
#include	"rmtuple.h"

/*********************************************************************
** E_FUNCTION : status = ur_retrieve_transf(&transf_packet)
**      get transformation tuple data for existing key_id (master tuple).
**    PARAMETERS   
**       INPUT  : 
**				&transf_packet, address of transformation packet to put data
**				transf_packet.key, must equal key_id of master to get
**       OUTPUT :  
**				none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_transf(transf_packet)
struct	UM_transf_rec  *transf_packet;	/* pointer to transf data packet */
{
	int				status;
	UU_REL_ID      rel_key;
	UU_KEY_ID      master_key;
	UR_REL_NUM     rel_num;
	UR_TUPLE_INDX  tuple_indx;
	int            i;

	uu_denter(UU_RTRC,(us,"ur_retrieve_transf(key=0x%x",transf_packet->key));
	master_key = transf_packet->key;   /* save master key */
	/* get transf tuple key */
	status = ur_retrieve_master_attr(master_key, UR_TRANSF_INDX, &rel_key);
	if (status == 0)
	{
		/* if no transformation matrix associated with this entity, */
		/* use the default if one has been defined */
		if (rel_key==0)                  /* no transf data */
		{
			if(UR_default_transf)
				ur_retrieve_tuple(UR_transf_relnum,1,transf_packet);
			else
				status = -1;
		}
		else			/* otherwise go get the transformation packet */
		{
			ur_k2rt(rel_key,&rel_num,&tuple_indx); /* get relation and index*/
			status = ur_retrieve_tuple( rel_num,tuple_indx,transf_packet);
		}

		transf_packet->key = master_key; /* reset in case diff(use>1)*/
	}
	uu_dprint(UU_RITRC,(us,"ur_retrieve_transf exit status=%d",status));
	uu_dexit;
	return(status);
}

