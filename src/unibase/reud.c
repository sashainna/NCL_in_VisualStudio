#define MODULEDBG 0
/*********************************************************************
**    NAME         :  reud.c
**       CONTAINS:
**       ur_update_data()
**       ur_update_data1()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reud.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:37
*********************************************************************/

#include "usysdef.h"
#include	"udebug.h"
#include "umessages.h"
#include "rbase.h"
#include	"ribase.h"
#include	"rmtuple.h"
#include "riddle.h"
#include "riddldef.h"
#include "r1emsgpk.h"

/*********************************************************************
** E_FUNCTION : status = ur_update_data(&data_packet)
**      update application data, both fixed & variable
**    PARAMETERS   
**       INPUT  : 
**				&data_packet, address of data packet to update with
**				data_packet.key_id, must equal key of data to update
**				data_packet.rel_num, must equal relation number of data
**       OUTPUT :  
**				none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     :	data can only be updated if it has been previous 
**							created
*********************************************************************/

ur_update_data(data_packet)
struct UR_data	*data_packet;	/* pointer to data packet */
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_update_data(key=0x%x)", data_packet->key_id));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_UPDATE, 0);
#if MODULEDBG != 0
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
#endif
	uu_dexit;
	return(ur_update_data1(data_packet, &theMessage, 1));
}


/*********************************************************************
** E_FUNCTION : status = ur_update_data1(&data_packet, theMessage, messageCnt)
**      update application data, both fixed & variable
**    PARAMETERS   
**       INPUT  : 
**				&data_packet, address of data packet to update with
**				data_packet.key_id, must equal key of data to update
**				data_packet.rel_num, must equal relation number of data
**				theMessage,	for scheduler
**       OUTPUT :  
**				none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     :	data can only be updated if it has been previous 
**							created
*********************************************************************/

ur_update_data1(data_packet, theMessage, messageCnt)
struct UR_data	*data_packet;	/* pointer to data packet */
UR_messagePacket	theMessage[];
int					messageCnt;
{
	int				status;		/* return status */
	UU_REL_ID		rel_key;		/* key to the relation to update	*/
	UR_REL_NUM		rel;			/* the relation to update */
	UR_TUPLE_INDX	tuple;		/* index of the tuple to update */
	UR_lpacket 		*lpack_ptr;	/* pointer to a list packet */
	UR_lpacket 		*old_list_ptr;	/* pointer to a list packet */
	UR_lpacket 		*new_list_ptr;	/* pointer to a list packet */
	int				atom_size;	/* size of an atom within a list */
	int				i;				/* for index */
	struct UR_MTID_rec	*m_ptr;
	struct UR_data	*ent_ptr;
	int				lst_len;
	int				list;
	int				num_attr;
	struct attr_def	*atdefs;	/* attribute definitions */
	struct attr_def	*v_adefs;/* attribute definitions - varlist */
	int		v_nattr;				/* number of attributes parsed	*/
	int		rel_typ;

	uu_denter(UU_RTRC,(us,"ur_update_data1(key 0x%x, rel %d messageCnt=%d)",
					data_packet->key_id, data_packet->rel_num, messageCnt));
	status = 0;
	ur_k2rt(data_packet->key_id,&rel,&tuple);
	if(rel == UR_MTUPLE_REL)
	{
		status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
		if (ur_editablep(m_ptr))
		{
			rel_key = m_ptr->assocs[UR_DATA_INDX];
			ur_k2rt(rel_key,&rel,&tuple);
		}
		else
		{
			status = -1;	/* entity is not editable */
		}
	}
	else if(rel > UR_MAX_REL)
	{
		status = -1;	/* illegal relation number		*/
	}
	else
	{
		rel_key = 0;
	}
	if(status == 0)
	{
		/* check if assoc w/keys changing if so must change back links */
		if (uri_assocp(rel))
		{
			ur_get_varlist_ptr(rel, tuple, 0, &ent_ptr, &lst_len);
			uri_update_assoc_list_fixed(ent_ptr, data_packet);
#if MODULEDBG != 0
			uu_dprint(UU_RITRC,(us,"assoc fixed done."));
#endif
			list = 1;			/* prime the list number for varlists */
			num_attr = UR_rcb[rel].rel_def->rellen;
			atdefs = UR_rcb[rel].relattr;

			for (i = 1; i < num_attr; i++)
			{
				if (atdefs[i].attr_type == JOIN)
				{
					ur_get_list_packet_ptr(rel, list, ent_ptr, &old_list_ptr);
					ur_get_list_packet_ptr(rel, list, data_packet, &new_list_ptr);
					v_nattr = UR_rcb[rel].lparms[list-1].atom_def->rellen;
					v_adefs = UR_rcb[rel].lparms[list-1].atomattr;
					ur_get_atom_size(rel, list, &atom_size);
					uri_update_assoc_list_varlist(old_list_ptr, new_list_ptr,
								v_adefs, v_nattr, atom_size, data_packet->key_id);
					list++;
				}
			}
#if MODULEDBG != 0
			uu_dprint(UU_RITRC,(us,"assoc varlists fixed."));
#endif
		}

		status = ur_update_tuple(rel,tuple,data_packet);
		if(status == 0)
		{
			/* update the variable length list if any */
			for (i=1; i<=UR_rcb[rel].n_varl; i++)
			{
				ur_get_list_packet_ptr(rel,i,data_packet,&lpack_ptr);
				/* for now, we must delete the existing list before updating */
				ur_delete_tuple_varlist(rel, tuple, i);
				if(lpack_ptr != 0 && lpack_ptr->atom_cnt > 0)
				{
					ur_update_tuple_varlist(rel, tuple, i, 1,
								lpack_ptr->atom_cnt, lpack_ptr->list_ptr);
				}
			}
		}
		/* get master rel & tuple, and a pointer to the master fixed data */
		ur_k2rt(data_packet->key_id, &rel, &tuple);
		if(rel == UR_MTUPLE_REL)
		{
			ur_get_tuple_ptr(rel, tuple, &m_ptr);	

			/* now notify associates */
			if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
			{
				uri_notify(m_ptr, data_packet->key_id, theMessage, messageCnt);
			}
		}
	} /* if status == 0 */
	if(rel_key == 0)		/* direct access with rel_key */
	{
		ur_update_data_tuple_key(data_packet->key_id, 0);	/* clear data key */
	}
#if MODULEDBG != 0
	uu_dprint(UU_RITRC,(us,"leaving update data status = %d", status));
	ur_dump_mtuple(data_packet->key_id);
	ur_dump_data(data_packet->key_id);
#endif
	uu_dexit;
	return(status);
}
