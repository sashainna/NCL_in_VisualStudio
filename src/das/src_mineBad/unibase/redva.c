/*********************************************************************
**    NAME         :  redva.c
**       CONTAINS:
**       ur_delete_varlist_atoms()
**       ur_delete_varlist_atoms1()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       redva.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:29
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) redva.c 3.6 3/3/88 09:57:40 single"};
#else
static char uu_sccsident[]={"@(#) redva.c 3.6 3/3/88 09:57:40 double"};
#endif
#endif

#include "usysdef.h"
#include "udebug.h"
#include "umessages.h"
#include "umoveb.h"
#include "rbase.h"
#include "rmtuple.h"
#include "riddle.h"
#include "r1emsgpk.h"

/*********************************************************************
**    E_FUNCTION :  status = ur_delete_varlist_atoms(key, list, start, count)
**       delete one or more atoms from a varlist
**    PARAMETERS   
**       INPUT  : 
**          key	key for data to delete atoms from
**				list	list number to delete atoms from
**				start	starting atom to be deleted from the list
**				count	number of atoms to be deleted
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_delete_varlist_atoms(key, list, start, count)
UU_KEY_ID	key;		/* key for data to delete atoms from */
int			list;		/* list number to delete atoms from */
int			start;	/* starting atom to be deleted from the list */
int			count;	/* number of atoms to be deleted */
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_delete_varlist_atoms(key=0x%x)", key));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_DEL_VARLA, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_delete_varlist_atoms1(key, list, start, count, &theMessage, 1));
}


/*********************************************************************
**    E_FUNCTION :  status = ur_delete_varlist_atoms1(key, list, start, count,
**											theMessage, messageCnt)
**       delete one or more atoms from a varlist
**    PARAMETERS   
**       INPUT  : 
**          key	key for data to delete atoms from
**				list	list number to delete atoms from
**				start	starting atom to be deleted from the list
**				count	number of atoms to be deleted
**				theMessage,	for scheduler
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_delete_varlist_atoms1(key, list, start, count, theMessage, messageCnt)
UU_KEY_ID	key;		/* key for data to delete atoms from */
int			list;		/* list number to delete atoms from */
int			start;	/* starting atom to be deleted from the list */
int			count;	/* number of atoms to be deleted */
UR_messagePacket	theMessage[];
int					messageCnt;
{
	UU_REL_ID		rel_key;		/* a relation,entry tuple */
	UR_REL_NUM		rel;			/* relation id to update to */
	UR_TUPLE_INDX	tuple;		/* entry id to update to */
	int				status;		/* return status */
	char				*list_ptr;	/* pointer to the varlist data */
	int				list_len;	/* list length in atoms */
	int				atom_size;	/* size of the atoms in the list */
	int				data_len;	/* length of data to move */
	int				data_displ;	/* displacement to the data */
	struct UR_MTID_rec	*m_ptr;

	uu_denter(UU_RTRC,(us,"ur_delete_varlist_atoms1(key=0x%x, list=%d messageCnt=%d)",
					key, list, messageCnt));
	status = 0;
	if (count == 0)
		goto theExit;
	if ((start < 1) || (count < 1))
	{
		status = -1;
		uu_dprint(-1,(us,"ERROR: illegal parameters to ur_delete_varlist_atoms"));
		uu_dprint(-1,(us,"       start %d, count %d", start, count));
		goto theExit;
	}
	/* get the geometric tuple for this key */
	ur_k2rt(key, &rel, &tuple);
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
		ur_get_varlist_ptr(rel, tuple, list, &list_ptr, &list_len);
		if (list_len >= start + count -1)
		{
			if (list_len - count > 0)
			{
				ur_get_atom_size(rel, list, &atom_size);
				uu_move_byte(list_ptr + (start + count - 1) * atom_size,
							list_ptr + (start - 1) * atom_size,
							(list_len - start - count + 1)*atom_size);
			}
			else
			{
				uu_toolfree(list_ptr);
				list_ptr = UU_NULL;
			}
			ur_update_varlist_info(rel, tuple, list, list_ptr,
							list_len - count);

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
	}
theExit:
	uu_dexit;
	return(status);
}


/*********************************************************************
**    E_FUNCTION :  status = uri_delete_assoclist_atoms(key, list, start,
**											count, theMessage)
**       delete one or more atoms from a varlist
**    PARAMETERS   
**       INPUT  : 
**          key	key for data to delete atoms from
**				list	list number to delete atoms from
**				start	starting atom to be deleted from the list
**				count	number of atoms to be deleted
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uri_delete_assoclist_atoms(key, list, start, count)
UU_KEY_ID	key;		/* key for data to delete atoms from */
int			list;		/* list number to delete atoms from */
int			start;	/* starting atom to be deleted from the list */
int			count;	/* number of atoms to be deleted */
{
	UU_REL_ID		rel_key;		/* a relation,entry tuple */
	UR_REL_NUM		rel;			/* relation id to update to */
	UR_TUPLE_INDX	tuple;		/* entry id to update to */
	int				status;		/* return status */
	char				*list_ptr;	/* pointer to the varlist data */
	int				list_len;	/* list length in atoms */
	int				atom_size;	/* size of the atoms in the list */
	int				data_len;	/* length of data to move */
	int				data_displ;	/* displacement to the data */
	struct UR_MTID_rec	*m_ptr;

	uu_denter(UU_RTRC,(us,"uri_delete_assoclist_atoms(key=0x%x, list=%d)",
					key, list));
	status = 0;
	if (count == 0)
		goto theExit;
	if ((start < 1) || (count < 1))
	{
		status = -1;
		uu_dprint(-1,(us,
					"ERROR: illegal parameters to uri_delete_asssoclist_atoms"));
		uu_dprint(-1,(us,"       start %d, count %d", start, count));
		goto theExit;
	}
	/* get the tuple for this key must be master tuple */
	ur_k2rt(key, &rel, &tuple);
	status = 0;
	if(rel != UR_MTUPLE_REL)
	{
		status = -1;
		uu_dprint(-1,(us,
					"ERROR: illegal parameters to uri_delete_asssoclist_atoms"));
		uu_dprint(-1,(us,"       start %d, count %d", start, count));
		goto theExit;
	}
	if(status == 0) 
	{
		ur_get_varlist_ptr(rel, tuple, list, &list_ptr, &list_len);
		if (list_len >= start + count -1)
		{
			if (list_len - count > 0)
			{
				ur_get_atom_size(rel, list, &atom_size);
				uu_move_byte(list_ptr + (start + count - 1) * atom_size,
							list_ptr + (start - 1) * atom_size,
							(list_len - start - count + 1)*atom_size);
			}
			else
			{
				uu_toolfree(list_ptr);
				list_ptr = UU_NULL;
			}
			ur_update_varlist_info(rel, tuple, list, list_ptr,
							list_len - count);
		}
	}
theExit:
	uu_dexit;
	return(status);
}
