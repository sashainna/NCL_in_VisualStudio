/*********************************************************************
**    NAME         :  reddv.c
**       CONTAINS:
**        ur_delete_data_varlist()
**        ur_delete_data_varlist1()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reddv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:28
*********************************************************************/

#include "usysdef.h"
#include "umessages.h"
#include "rmtuple.h"
#include "riddle.h"
#include	"udebug.h"
#include "r1emsgpk.h"

/*********************************************************************
**    E_FUNCTION     :  ur_delete_data_varlist(key, lst_num)
**      delete a geometry entry variable list
**    PARAMETERS   
**       INPUT  : 
**				key,		a master tuple identifier
**				lst_num,		which variable list to delete, 1 to n_varl,
**       OUTPUT :  
**          none
**    RETURNS      :  -1, if operation was unsuccessful, 0 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     :  list 0, or the entire entry must be deleted using 
**							ur_delete_geometry
*********************************************************************/

ur_delete_data_varlist(key, lst_num)
UU_KEY_ID	key;		/* a relation, entry tuple					*/
int			lst_num;
{
	UR_messagePacket	theMessage;

	uu_denter(UU_RTRC, (us, "ur_delete_data_varlist(key=0x%x)", key));
	ur1_initMsgPacket(&theMessage, UU_DEFAULT_DEL_DVARL, 0);
	uu_dprint(UU_RTRC, (us, "warning I exit here without really exiting"));
	uu_dexit;
	return(ur_delete_data_varlist1(key, lst_num, &theMessage, 1));
}

/*********************************************************************
**    E_FUNCTION     :  ur_delete_data_varlist1(key, lst_num, theMessage, messageCnt)
**      delete a geometry entry variable list
**    PARAMETERS   
**       INPUT  : 
**				key,		a master tuple identifier
**				lst_num,		which variable list to delete, 1 to n_varl,
**				theMessage,	for scheduler
**       OUTPUT :  
**          none
**    RETURNS      :  -1, if operation was unsuccessful, 0 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     :  list 0, or the entire entry must be deleted using 
**							ur_delete_geometry
*********************************************************************/

ur_delete_data_varlist1(key, lst_num, theMessage, messageCnt)
UU_KEY_ID	key;		/* a relation, entry tuple					*/
int			lst_num;
UR_messagePacket	theMessage[];
int					messageCnt;
{
	UU_REL_ID	rel_key;	/* a relation num, tuple id key			*/
	int			status;	/* return status								*/
	char			*lst_ptr; /* pointer to list to be deleted			*/
	int			lst_len;	/* length of list to be deleted			*/
	int			rel;
	int			tuple;
	struct UR_MTID_rec	*m_ptr;

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
*/
	uu_denter(UU_RTRC, (us, "ur_delete_data_varlist1(%d, key= 0x%x messageCnt=%d)",
					lst_num, key, messageCnt));

	/* get the rel, tuple index for the data specified by key */
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
		/*  if list zero, do nothing */
		if(lst_num > 0)
		{
			/* get list pointer, it will for valid relation, entry, & list # */
			ur_k2rt(rel_key, &rel, &tuple);
			status = ur_get_varlist_ptr(rel, tuple, lst_num, &lst_ptr,
					&lst_len);

			/* if valid list & non-zero length, delete it by freeing the space */
			/* and set list atom count and length to 0 */
			if(status == 0 && lst_len > 0)								
			{
				uu_toolfree(lst_ptr);
				ur_update_varlist_info(rel, tuple, lst_num, 0, 0);

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
			else
			{
				status = -1;
		  	}
		}
	}
	uu_dexit;
	return(status);
}
