#define MODULEDBG 0
/*********************************************************************
**    NAME         :  reda.c
**       CONTAINS:
**       ur_delete_all()
**       ur_delete_all1()
**       ur_force_delete_all()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reda.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:28
*********************************************************************/

#include	"usysdef.h"
#include "umessages.h"
#include	"udebug.h"
#include "riddle.h"
#include "rbase.h"
#include "ribase.h"
#include	"rmtuple.h"
#include	"rstack.h"
#include "riddldef.h"
#include "r1emsgpk.h"

/*********************************************************************
**    E_FUNCTION     :  status = ur_delete_all(key)
**      delete all data associated with a particular master key
**    PARAMETERS   
**       INPUT  : 
**				key, the identifier of the master to be deleted
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_delete_all(key)
UU_KEY_ID	key;				/* master tuple key						*/
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_delete_all(key=0x%x)", key));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_DELETE, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_delete_all1(key, &theMessage, 1));
}

/*********************************************************************
**    E_FUNCTION     :  status = ur_delete_all1(key, theMessage, messageCnt)
**      delete all data associated with a particular master key
**    PARAMETERS   
**       INPUT  : 
**				key, the identifier of the master to be deleted
**				theMessage,	for scheduler
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_delete_all1(key, theMessage, messageCnt)
UU_KEY_ID	key;		/* key to be deleted */
UR_messagePacket	theMessage[];
int					messageCnt;
{
	extern	UR_STACK(UR_del_stack,UR_DEL_STK_DEPTH,dstack_element);
	int				status;			/* status, -1 if error, 0 otherwise	*/
	struct UR_MTID_rec	*m_ptr;	/* a master tuple */
	UU_REL_ID		rel_key;			/* relation,tuple key from attr table	*/
	int				i, j, k;			/* indices */
	UR_REL_NUM		rel;
	UR_TUPLE_INDX	tuple;
	int				num_deletes;	/* number of tuples we will be deleting*/
	int				stk_space;
	struct attr_def	*atdefs;	/* attribute definitions */
	int				num_attr;		/* number of attributes parsed */
	struct attr_def	*v_adefs;	/* attribute definitions - varlist */
	int				v_nattr;			/* number of attributes parsed	*/
	int				rel_typ;
	struct UR_data	*ent_ptr;		/* pointer to data */
	UU_KEY_ID		back_key;		/* key to be deleted */
	int				n_atoms;			/* min atoms in the lists */
	int				list;
	UR_REL_NUM		temp_rel;
	UR_TUPLE_INDX	temp_tuple;
	int				atom_size;
	UR_lpacket		*list_ptr;

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
	uu_denter(UU_RTRC, (us, "ur_delete_all1(key=0x%x messageCnt=%d)", key, messageCnt));
	status = 0;
	ur_k2rt(key, &rel, &tuple);
	back_key = key;
	if(rel == UR_MTUPLE_REL)
	{
		/* get master rel & tuple, and a pointer to the master fixed data */
		status = ur_get_tuple_ptr(rel, tuple, &m_ptr);	
		if ((status == 0) && !ur_editablep(m_ptr))
		{
			status = -1;	/* entity is not editable */
		}

		/* if valid entry pointer to key, loop through the tuple */
		/* table deleting each rel, entry */
		if (status == 0)
		{
			/* first determine how many tuples we will be deleting, so that */
			/* we make sure there is room on the stack, if not go make room */
			/***** W A R N I N G : ******************************************/
			/* undelete function is broken with regard to gen associativity */
			/* only takes account of xfrm and attrs */
			num_deletes = 1 ;	/* always deleting a master	*/
			if (m_ptr->no_assocs > 0)
			for (i = 0; i <= UR_MAX_INDX; i++) 
			{
				if((*m_ptr).assocs[i] != 0) num_deletes++;
			}
			stk_space = ur_stack_space(UR_del_stack);
			uu_dprint(UU_RTRC, (us, "remaining stack space = %d, need %d",
								stk_space, num_deletes));
			if(ur_stack_space(UR_del_stack) < num_deletes)
			{
				ur_pull_del_stack();
			}
			if (m_ptr->no_assocs > 0)
			for(i = 1; i <= UR_MAX_INDX; i++)	/* del all but data tuple */
			{
				/* retrieve the tuple id at tuple index, and delete the
				/* rel, entry if used(non-zero) */
				rel_key = (*m_ptr).assocs[i];
				if(rel_key != 0)
				{
					ur_k2rt(rel_key, &rel, &tuple);
					status = ur_delete_tuple(rel, tuple);
				}
			}

			/* now notify associates and zero any keys to this entity */
			if ((*m_ptr).no_assocs > UR_MAX_INDX + 1)
			{
				uri_notify(m_ptr, key, theMessage, messageCnt);
				uri_zap_keys(m_ptr, key);
			}

			/* BANDAID: this allows the undelete function to work */
			/*          partially.  Pending a better fix to support undo */
			(*m_ptr).no_assocs = UR_MAX_INDX + 1;

			/* now delete the master */
			rel_key = 0;
			if ((unsigned long)m_ptr->assocs > 0)
				rel_key = (*m_ptr).assocs[UR_DATA_INDX];	/* save data key */
			ur_k2rt(key, &rel, &tuple);
			status = ur_delete_tuple(rel, tuple);
			ur_k2rt(rel_key, &rel, &tuple);
		}
	}
	if (status == 0)
	{
		/* delete with a rel_key, or delete data from master */
		/* if the relation is an association delete back pointers */
		if (uri_assocp(rel))
		{
			/* get definition and pointer to data */
			status = ur_get_tuple_ptr(rel, tuple, &ent_ptr);
#if MODULEDBG != 0
			ur_dump_tuple(rel, ent_ptr);
#endif
			if (status == 0)
			{
				num_attr = UR_rcb[rel].rel_def->rellen;
				atdefs = UR_rcb[rel].relattr;
				list = 1;			/* prime the list number for varlists */
				uu_dprint(UU_RITRC,(us,"removing back links to association"));

				/* for each key field */
				for (i = 1; (i < num_attr); i++)
				{
					/* if master key */
					if ((atdefs[i].attr_type == KEY_ID)
						|| (atdefs[i].attr_type == REL_ID))
					{
						ur_k2rt(*(UU_KEY_ID *)((char *)ent_ptr
								+ atdefs[i].attr_here), &temp_rel, &temp_tuple);
						if ((temp_rel == UR_MTUPLE_REL) && (temp_tuple != 0))
						{
							/* add assoc list back_key */
							uri_del_assoc_list(*(UU_KEY_ID *)((char *)ent_ptr
								+ atdefs[i].attr_here), back_key);
						}
					}
					else if (atdefs[i].attr_type == JOIN)
					{
						/* do the same thing with each joined field */
						v_nattr = UR_rcb[rel].lparms[list-1].atom_def->rellen;
						v_adefs = UR_rcb[rel].lparms[list-1].atomattr;
						ur_get_list_packet_ptr(rel, list, ent_ptr, &list_ptr);
						ur_get_atom_size(rel, list, &atom_size);
						for (j = 1; j < v_nattr; j++)
						{
							if ((v_adefs[j].attr_type == KEY_ID)
								|| (v_adefs[j].attr_type == REL_ID))
							{
								/* for each atom */
								n_atoms = list_ptr->atom_cnt;
								for (k = 0; k < n_atoms; k++)
								{
#if MODULEDBG != 0
									uu_dprint(UU_RITRC,(us,"handle key in atom %d", k));
									uu_dprint(UU_RITRC,(us,"list ptr = 0x%x",
												list_ptr->list_ptr));
									uu_dprint(UU_RITRC,(us,"& of key = 0x%x",
												(char *)list_ptr->list_ptr
												+ v_adefs[j].attr_here + k * atom_size));
									uu_dprint(UU_RITRC,(us,"key = 0x%x",
												*(UU_KEY_ID *)((char *)list_ptr->list_ptr
												+ v_adefs[j].attr_here + k * atom_size)));
#endif
									ur_k2rt(*(UU_KEY_ID *)((char *)list_ptr->list_ptr
											+ v_adefs[j].attr_here + k * atom_size),
											&temp_rel, &temp_tuple);
									if ((temp_rel == UR_MTUPLE_REL) && (temp_tuple != 0))
									{
										/* add assoc list back_key */
										uri_del_assoc_list(
											*(UU_KEY_ID *)((char *)list_ptr->list_ptr
											+ v_adefs[j].attr_here + k * atom_size),
											back_key);
									}
								}
							}
						}
						list++;
					}
				}
				uu_dprint(UU_RITRC,(us,"done removing back links to association"));
			}
		}
	}
	if (status == 0)
	{
		status = ur_delete_tuple(rel, tuple);
		UR_del_started = UU_TRUE;				/* set delete set started */
	}
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     :  status = ur_force_delete_all(key)
**      delete all data associated with a particular master key
**    PARAMETERS   
**       INPUT  : 
**				key, the identifier of the master to be deleted
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_force_delete_all(key)
UU_KEY_ID	key;				/* master tuple key						*/
{
	UU_LOGICAL	ur_get_del_stat();	/* routine to get delete state */
	UU_LOGICAL	old_delete_status;	/* place to save old status */
	int status; 

	uu_denter(UU_RTRC, (us, "ur_force_delete_all(0x%x)", key));
	old_delete_status = ur_get_del_stat();
	ur_set_del_stat(UU_FALSE);		/* turn off delete stacking */
	status = ur_delete_all(key);			/* delete the requested thingy */
	ur_set_del_stat(old_delete_status);	/* restore delete state */
	uu_dexit;
	return(status);
}
