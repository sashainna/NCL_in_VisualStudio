#define MODULEDBG 1
/*********************************************************************
**    NAME         :  regnak.c
**       CONTAINS:
**       ur_get_next_assoc_key()
**       ur_get_app_assoc_list()
**			ur_retrieve_assoc_keys()
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       regnak.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:30
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) regnak.c 3.4 4/19/88 14:32:41 single"};
#else
static char uu_sccsident[]={"@(#) regnak.c 3.4 4/19/88 14:32:41 double"};
#endif
#endif

#include "usysdef.h"
#include "udebug.h"
#include "rbase.h"
#include "ribase.h"
#include "riddle.h"

/*********************************************************************
**	E_FUNCTION :  ur_get_next_assoc_key(key, nxt_assoc_indx, assoc_key)
**		Get the next key associated with the given key.  Search starts with
**		nxt_assoc_indx'th key and the next key, if any, is returned as assoc_key
**		or the function fails.
**    PARAMETERS   
**       INPUT  : 
**		key				UU_KEY_ID	master tuple key for which to retrieve
**											associations
**		nxt_assoc_indx	int*			start index into association list - 1 based -
**											3 'special' keys skipped automatically
**       OUTPUT :  
**		nxt_assoc_indx	int*			updated index to association returned
**										(necessary because some assoc's may be disabled)
**		assoc_key		UU_KEY_ID	returned association key
**    RETURNS:		0 if successful, -1 otherwise
**    SIDE EFFECTS:	none
**    WARNINGS:	none
*********************************************************************/

ur_get_next_assoc_key(key, nxt_assoc_indx, assoc_key)
UU_KEY_ID	key;					/* master tuple key */
int			*nxt_assoc_indx;	/* which associate to start with */
UU_KEY_ID	*assoc_key;			/* key of associated tuple */
{
	struct UR_MTID_rec	*m_ptr;
	UR_REL_NUM				rel;
	UR_TUPLE_INDX			tuple;
	UU_LOGICAL				found;
	int						status;

	uu_denter(UU_RTRC,(us,"ur_get_next_assoc_key(key=0x%x, indx=%d)",
					key, *nxt_assoc_indx));
	ur_k2rt(key, &rel, &tuple);
	status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
	*assoc_key = 0;
	if (status == 0)
	{
		found = UU_FALSE;
		status = -1;				/* prime status for not found */
		while ((m_ptr->no_assocs > (*nxt_assoc_indx + 2)) && !found)
		{
			if (uri_notify_allowedp(rel,tuple))
			{
				*assoc_key = m_ptr->assocs[*nxt_assoc_indx + 2];
#if MODULEDBG != 0
				uu_dprint(UU_RITRC,(us,"found key 0x%x at index %d",
								*assoc_key, *nxt_assoc_indx + 2));
#endif
				status = 0;
				found = UU_TRUE;
			}
			else
			{
				*nxt_assoc_indx++;
			}
		}
	}
	uu_dexit;
	return(status);
}


/*********************************************************************
**	E_FUNCTION :  ur_get_app_assoc_list(key, list_len, assoc_list)
**		Get the list of keys associated with the given key.
**    PARAMETERS   
**       INPUT  : 
**			key			UU_KEY_ID	master tuple key for which to retrieve
**											associations
**       OUTPUT :  
**			list_len		int*			number of keys returned in the list
**			assoc_list	UU_KEY_ID**	returned association list
**    RETURNS:		0 if successful, -1 otherwise
**    SIDE EFFECTS:	none
**    WARNINGS:	none
*********************************************************************/

ur_get_app_assoc_list(key, list_len, assoc_list)
UU_KEY_ID	key;				/* master tuple key */
int			*list_len;		/* which associate to start with */
UU_KEY_ID	**assoc_list;	/* key of associated tuple */
{
	struct UR_MTID_rec	*m_ptr;
	UR_REL_NUM				rel;
	UR_TUPLE_INDX			tuple;
	int						status;
	int						nxt_indx;	/* key index to start with */
	UU_KEY_ID				*list_ptr;	/* ptr to where next key goes */

	uu_denter(UU_RTRC,(us,"ur_get_app_assoc_list(key=0x%x)", key));
	*list_len = 0;
	ur_k2rt(key, &rel, &tuple);
	status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
	if (status == 0)
	{
		status = -1;				/* prime status for not found */

		/* allocate memory to hold the list */
		*assoc_list = (UU_KEY_ID *)
							uu_malloc(m_ptr->no_assocs * sizeof(UU_KEY_ID));
		if (assoc_list != 0)
		{
			nxt_indx = 1;
			list_ptr = *assoc_list;
			while (ur_get_next_assoc_key(key, &nxt_indx, list_ptr) == 0)
			{
				status = 0;
				nxt_indx++;
				(*list_len)++;
			}
		}
	}
	uu_dexit;
	return(status);
}


/*********************************************************************
**    E_FUNCTION :  *keys = ur_retrieve_assoc_keys(key, &buffer, &len, unique)
**       retrieve the list of association keys referencing the given key
**    PARAMETERS   
**       INPUT  : 
**          key		key_id of the entity to retrieve the associations of
**				buffer	array to put the keys of associations in
**				len		length of the buffer array
**				unique	UU_TRUE if a unique list is desired
**       OUTPUT :  
**          keys		contains keys, if any, of associations NOTE: *keys
**							usually = &buffer, but may point to space from uu_malloc()
**				len		contains number of keys returned
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

UU_KEY_ID *ur_retrieve_assoc_keys(key, buffer, len, unique)
UU_KEY_ID	key;		/* key_id of the entity to retrieve the associations of */
UU_KEY_ID	*buffer;	/* array to put the keys of associations in */
int			*len;		/* length of the buffer array */
UU_LOGICAL	unique;	/* UU_TRUE if a unique list is desired */
{
	struct UR_MTID_rec	*m_ptr;
	UR_REL_NUM				rel;
	UR_TUPLE_INDX			tuple;
	int						status;
	int						nxt_indx;	/* key index to start with */
	UU_KEY_ID				*list_ptr;	/* ptr to where next key goes */
	UU_KEY_ID				*assoc_list;	/* key of associated tuple */
	UU_LOGICAL				there;
	int						j;

	uu_denter(UU_RTRC,(us,"ur_retrieve_assoc_keys(key=0x%x)", key));
	ur_k2rt(key, &rel, &tuple);
	status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
	if (status == 0)
	{
		status = -1;				/* prime status for not found */
		if (m_ptr->no_assocs > *len)
		{
			/* allocate memory to hold the list */
			assoc_list = (UU_KEY_ID *)
								uu_malloc(m_ptr->no_assocs * sizeof(UU_KEY_ID));
		}
		else
		{
			assoc_list = buffer;
		}
		*len = 0;
		if (assoc_list != 0)
		{
			nxt_indx = 1;
			list_ptr = assoc_list;
			while (ur_get_next_assoc_key(key, &nxt_indx, list_ptr) == 0)
			{
				if (unique)
				{
					/* see if already there */
					there = UU_FALSE;
					for (j = 0; j < *len; j++)
					{
						if (*list_ptr == assoc_list[j])
						{
							there = UU_TRUE;
							break;
						}
					}
					if (!there)
					{
						status = 0;
						nxt_indx++;
						list_ptr++;
						(*len)++;
					}
					else
					{
						nxt_indx++;
					}
				}
				else	/* don't care if unique */
				{
					status = 0;
					nxt_indx++;
					list_ptr++;
					(*len)++;
				}
			}
		}
	}
	else
	{
		/* error but return len = 0 */
		*len = 0;
	}
#if MODULEDBG != 0
	uu_dprint(UU_RITRC,(us,"ur_retrieve_assoc_keys returns %d keys",*len));
#endif
	uu_dexit;
	return( (status == 0) ? assoc_list : UU_NULL);
}

