/*********************************************************************
**    NAME         :  reudv.c
**       CONTAINS:
**       ur_update_data_varlist()
**       ur_update_data_varlist1()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reudv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:38
*********************************************************************/

#include "usysdef.h"
#include "umessages.h"
#include	"udebug.h"
#include "rmtuple.h"
#include "rbase.h"
#include "ribase.h"
#include "riddle.h"
#include "riddldef.h"
#include "r1emsgpk.h"

/*********************************************************************
** E_FUNCTION : status = ur_update_data_varlist(key, list, &var_data,
**													start_atom, num_atom)
**	update varlist for a given entity
**    PARAMETERS   
**       INPUT  : 
**				key,		key to use in updating the data
**				list,			list number to update
**				var_data,	address of buff containing the update data
**				start_atom,	first atom to update, 1 = first
**				num_atom,	number of atoms to update
**       OUTPUT :  
**          none
**    RETURNS      :  0 if no error -1 if error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_data_varlist(key, list, data_ptr, start_atom, num_atom)
UU_KEY_ID	key;		/* a master tuple id								*/
int			list;			/* which list of data to update				*/
char 			*data_ptr;	/* ptr to where to get the data				*/
int			start_atom;	/* first atom to update							*/
int			num_atom;	/* number of atoms to update					*/
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_update_data_varlist(key=0x%x)", key));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_UPDT_DVARL, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_update_data_varlist1(key, list, data_ptr, start_atom, num_atom,
						&theMessage, 1));
}


/*********************************************************************
** E_FUNCTION : status = ur_update_data_varlist1(key, list, &var_data,
**											start_atom, num_atom, theMessage, messageCnt)
**	update varlist for a given entity
**    PARAMETERS   
**       INPUT  : 
**				key,		key to use in updating the data
**				list,			list number to update
**				var_data,	address of buff containing the update data
**				start_atom,	first atom to update, 1 = first
**				num_atom,	number of atoms to update
**				theMessage,	for scheduler
**       OUTPUT :  
**          none
**    RETURNS      :  0 if no error -1 if error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_data_varlist1(key, list, data_ptr, start_atom, num_atom,
						theMessage, messageCnt)
UU_KEY_ID	key;		/* a master tuple id								*/
int			list;			/* which list of data to update				*/
char 			*data_ptr;	/* ptr to where to get the data				*/
int			start_atom;	/* first atom to update							*/
int			num_atom;	/* number of atoms to update					*/
UR_messagePacket	theMessage[];
int					messageCnt;
{
	UU_REL_ID		rel_key;		/* a relation,entry tuple			*/
	UR_REL_NUM		rel;			/* relation id to update to		*/
	UR_TUPLE_INDX	tuple;		/* entry id to update to			*/
	int				status;		/* return status						*/
	int				atom_size;	/* size of the data atom			*/
	int				data_len;	/* length of data to move			*/
	int				data_displ;	/* displacement to the data		*/
	struct UR_MTID_rec	*m_ptr;
	UR_lpacket 		old_list_pkt;	/* constructed list packets to describe */
	UR_lpacket 		new_list_pkt;	/* only the atoms which are changing */
	UR_lpacket 		*old_list_ptr;	/* pointer to a list packet */
	struct UR_data	*ent_ptr;
	int				lst_len;
	int				num_attr;
	struct attr_def	*atdefs;	/* attribute definitions */
	struct attr_def	*v_adefs;/* attribute definitions - varlist */
	int		v_nattr;				/* number of attributes parsed	*/
	int		rel_typ;

	uu_denter(UU_RTRC,(us,"ur_update_data_varlist1(key= 0x%x, list= %d messageCnt=%d)",
					key,list, messageCnt));

	/* get the geometric tuple for this key */
	ur_k2rt(key, &rel, &tuple);
	status = 0;
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
		rel_key = key;
	}
	if(status == 0) 
	{
		ur_k2rt(rel_key,&rel,&tuple);

		/* check if assoc w/keys changing if so must change back links */
		if (uri_assocp(rel))
		{
			ur_get_varlist_ptr(rel, tuple, 0, &ent_ptr, &lst_len);
			num_attr = UR_rcb[rel].rel_def->rellen;
			atdefs = UR_rcb[rel].relattr;
			ur_get_list_packet_ptr(rel, list, ent_ptr, &old_list_ptr);
			v_nattr = UR_rcb[rel].lparms[list-1].atom_def->rellen;
			v_adefs = UR_rcb[rel].lparms[list-1].atomattr;
			ur_get_atom_size(rel, list, &atom_size);

			/* synthesize list packets */
			new_list_pkt.list_ptr = data_ptr;
			new_list_pkt.atom_cnt = num_atom;
			old_list_pkt.list_ptr = old_list_ptr->list_ptr
											+ atom_size * (start_atom - 1);
			old_list_pkt.atom_cnt = old_list_ptr->atom_cnt - start_atom + 1;
			if (old_list_pkt.atom_cnt < 0)
			{
				old_list_pkt.atom_cnt = 0;	/* can't be negative # atoms */
			}
			if (old_list_pkt.atom_cnt > num_atom)
			{
				old_list_pkt.atom_cnt = num_atom;	/* can't be too many atoms */
			}
			uri_update_assoc_list_varlist(&old_list_pkt, &new_list_pkt,
					v_adefs, v_nattr, atom_size, key);
			uu_dprint(UU_RITRC,(us,"assoc varlists fixed."));
		}

		status = ur_update_tuple_varlist(rel, tuple, list,
											start_atom, num_atom, data_ptr);

		/* get master rel & tuple, and a pointer to the master fixed data */
		ur_k2rt(key, &rel, &tuple);
		if(rel == UR_MTUPLE_REL)
		{
			status = ur_get_tuple_ptr(rel, tuple, &m_ptr);	

			/* now notify associates */
			if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
			{
				uri_notify(m_ptr, key, theMessage, messageCnt);
			}
		}
	}
	uu_dexit;
	return(status);
}

