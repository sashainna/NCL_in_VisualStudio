/*********************************************************************
**    NAME         :  reudf.c
**       CONTAINS:
**				ur_update_data_fixed()
**				ur_update_data_fixed1()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reudf.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:37
*********************************************************************/

#include "usysdef.h"
#include	"udebug.h"
#include "umessages.h"
#include	"rbase.h"
#include "ribase.h"
#include	"rmtuple.h"
#include "riddle.h"
#include "r1emsgpk.h"

/*********************************************************************
**    E_FUNCTION   :   status = ur_update_data_fixed(&data_packet)
**      update fixed, geometric data
**    PARAMETERS   
**       INPUT  : 
**			&data_packet,			address of the data packet
**			data_packet.key_id,	key of master tuple to update data in
**			data_packet.rel_num,	the relation number of the data
**			data_packet.data,		the data
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_data_fixed(data_packet)
struct	UR_data	*data_packet;	/*  a ptr to a data packet			*/
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_update_dat_fixed(key=0x%x)", data_packet->key_id));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_UPDT_FIXED, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_update_data_fixed1(data_packet, &theMessage, 1));
}


/*********************************************************************
**    E_FUNCTION   :   status = ur_update_data_fixed1(&data_packet,
**												theMessage, messageCnt)
**      update fixed, geometric data
**    PARAMETERS   
**       INPUT  : 
**			&data_packet,			address of the data packet
**			data_packet.key_id,	key of master tuple to update data in
**			data_packet.rel_num,	the relation number of the data
**			data_packet.data,		the data
**				theMessage,	for scheduler
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_data_fixed1(data_packet, theMessage, messageCnt)
struct	UR_data	*data_packet;	/*  a ptr to a data packet			*/
UR_messagePacket	theMessage[];
int					messageCnt;
{
	UU_REL_ID		rel_key;		/* the geometric entity tuple from MTID*/
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;
	int				status;		/* status, -1 if error, 0 otherwise		*/
	struct UR_MTID_rec	*m_ptr;
	struct UR_data	*ent_ptr;	/* a ptr to a data packet */
	UR_lpacket 		*lpack_ptr;	/* pointer to a list packet */
	int				i;				/* loop index */

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
	uu_denter(UU_RTRC, (us, "ur_update_data_fixed1(key= 0x%x, rel= %d messageCnt=%d)",
					data_packet->key_id, data_packet->rel_num, messageCnt));

	/* get the geometry tuple */	
	ur_k2rt(data_packet->key_id, &rel, &tuple);
	if(rel == UR_MTUPLE_REL)
	{
		status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
		if (ur_editablep(m_ptr))
		{
			rel_key = m_ptr->assocs[UR_DATA_INDX];
		}
		else
		{
			status = -1;	/* entity is not editable */
		}
	}
	else
	{
		rel_key = data_packet->key_id;
	}
	if(status == 0)
	{
		ur_k2rt(rel_key, &rel, &tuple);
		
		if (uri_assocp(rel))
		{
			status = ur_get_tuple_ptr(rel, tuple, &ent_ptr);	
			uri_update_assoc_list_fixed(ent_ptr, data_packet);
		}

		/* fix the list packet info, if any, and update the tuple */
		for (i = 1; i <= UR_rcb[rel].n_varl; i++)
		{
			ur_get_list_packet_ptr(rel, i, data_packet, &lpack_ptr);
			ur_get_varlist_ptr(rel, tuple, i,
					&(lpack_ptr->list_ptr), &(lpack_ptr->atom_cnt));
		}
		status = ur_update_tuple(rel, tuple, data_packet);

		/* get master rel & tuple, and a pointer to the master fixed data */
		ur_k2rt(data_packet->key_id, &rel, &tuple);
		if(rel == UR_MTUPLE_REL)
		{
			status = ur_get_tuple_ptr(rel, tuple, &m_ptr);	

			/* now notify associates */
			if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
			{
				uri_notify(m_ptr, data_packet->key_id, theMessage, messageCnt);
			}
		}
	}
	uu_dexit;
	return(status);
}
