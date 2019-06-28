/*********************************************************************
**    NAME         :  reradv.c
**       CONTAINS:
**       ur_retrieve_app_data_varlist()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reradv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:33
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) reradv.c 3.3 3/15/88 10:16:30 single"};
#else
static char uu_sccsident[]={"@(#) reradv.c 3.3 3/15/88 10:16:30 double"};
#endif
#endif

#include "usysdef.h"
#include "rmtuple.h"
#include "rbase.h"
#include "ribase.h"
#include "udebug.h"

/*********************************************************************
** E_FUNCTION : status = ur_retrieve_app_data_varlist(data, list)
**		retrieve variable data for a geometric relation
**    PARAMETERS   
**       INPUT  : 
**				data,			pointer to fixed data buffer with valid key_id
**				list,			list number to retrieve
**       OUTPUT :  
**           pointer and atom count for the list are filled in
**    RETURNS      :  0 if function was successful, -1 error
**								NOTE: retrieving an empty list is NOT an error.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_app_data_varlist(data,list)
struct UR_data	*data;	/* pointer to fixed data buffer */
int		list;				/* which list of data to return */
{
	UU_KEY_ID		key;			/* key for data to retrieve */
	UU_REL_ID		rel_key;		/* a relation,entry tuple */
	UR_REL_NUM		rel;			/* relation id to retrieve from */
	UR_TUPLE_INDX	tuple;		/* entry id to retrieve from */
	int				status;		/* return status */
	int				atom_size;	/* size of the data atom */
	struct UR_data	*tuple_ptr;	/* pointer to the tuple in Unibase */
	struct UR_lpacket	*lpack_ptr;/* pointer to list info in Unibase */
	struct UR_lpacket	*data_lpack_ptr;/* pointer to list info in data buffer */
	char				*the_ptr;
	extern char		*uu_malloc();

	uu_denter(UU_RTRC,(us,"retrieve_app_data_varlist(key=0x%x, list=%d)",
				data->key_id, list));
	status = 0;
	key = data->key_id;

	/* first determine if a master tuple key or a relation key */
	ur_k2rt(key, &rel, &tuple);
	if(rel == UR_MTUPLE_REL)
	{
		/*if (tuple == 1378897)
				{
					sprintf (" ","Invalid value for DNC modal. /%s/ %s"," "," ");
				}*/
		/* master tuple, get the data key and break in to rel,index */
		status = uri_retrieve_master_attr(rel, tuple, UR_DATA_INDX, &rel_key);
	}
	else
	{
		rel_key = key;
	}
	ur_k2rt(rel_key, &rel, &tuple);
	if(status == 0) 
	{
		/* get address of list packet in Unibase */
		ur_get_tuple_ptr(rel, tuple, &tuple_ptr);
		ur_get_list_packet_ptr(rel, list, tuple_ptr, &lpack_ptr);

		/* fill in packet in fixed data buffer */
		ur_get_list_packet_ptr(rel, list, data, &data_lpack_ptr);
		data_lpack_ptr->atom_cnt = lpack_ptr->atom_cnt;
		if (data_lpack_ptr->list_ptr)
		{
			/* free any list space currently occupied */
			uu_free(data_lpack_ptr->list_ptr - sizeof(int));
		}
		ur_get_atom_size(rel, list, &atom_size);
		the_ptr = uu_malloc(lpack_ptr->atom_cnt * atom_size +
						sizeof(int));	/* allocate space */
		if(the_ptr == 0)
		{
			uu_dprint(-1,(us,
						"ERROR:uu_malloc failure in ur_retrieve_app_data_varlist"));
			uu_dexit;
			return(-1);
		}
		*((int *)the_ptr) = lpack_ptr->atom_cnt;		/* set max count */
		data_lpack_ptr->list_ptr = the_ptr + sizeof(int);

		if (data_lpack_ptr->list_ptr && data_lpack_ptr->atom_cnt > 0)
		{
			status = ur_retrieve_tuple_varlist(rel, tuple, list,
								1, data_lpack_ptr->atom_cnt, data_lpack_ptr->list_ptr);
		}
	}
	uu_dexit;
	return(status);
}
