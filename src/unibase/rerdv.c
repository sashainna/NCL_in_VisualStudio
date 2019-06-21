/*********************************************************************
**    NAME         :  rerdv.c
**       CONTAINS:
**       ur_retrieve_data_varlist()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rerdv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:34
*********************************************************************/

#include "usysdef.h"
#include "rmtuple.h"
#include "rbase.h"
#include "udebug.h"

/*********************************************************************
** E_FUNCTION : status = ur_retrieve_data_varlist(key,list,
**										&var_data,start_atom,num_atom)
**	retrieve variable data for a geometric relation
**    PARAMETERS   
**       INPUT  : 
**				key,		key_id or rel_key to use in retreiving the data
**				list,		list number to retrieve
**				var_data,	address of buff to place the retrieved data in
**				start_atom,	first atom to read, 1 = first
**				num_atom,	number of atoms to read
**       OUTPUT :  
**           var_data, the retrieved data
**    RETURNS      :  0 if function was successful, -1 error
**								atom count if partially filled
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_data_varlist(key,list,data_ptr,start_atom,num_atom)
UU_KEY_ID	key;			/* a master tuple id or relation key		*/
int			list;			/* which list of data to return				*/
char 			*data_ptr;	/* ptr to where to pout the data				*/
int			start_atom;	/* first atom to read							*/
int			num_atom;	/* number of atoms to read						*/
{
	UU_REL_ID		rel_key;		/* a relation,entry tuple			*/
	UR_REL_NUM		rel;			/* relation id to retrieve from	*/
	UR_TUPLE_INDX	tuple;		/* entry id to retrieve from		*/
	int				status;		/* return status						*/
	int				atom_size;	/* size of the data atom			*/
	int				data_len;	/* length of data to move			*/
	int				data_displ;	/* displacement to the data		*/

	uu_denter(UU_RTRC,(us,
				"retrieve_data_varlist, key= 0x%x list %d, start atom = %d",
				key,list,start_atom));
	status = 0;

	/* first determine if a master tuple key or a relation key */
	ur_k2rt(key,&rel,&tuple);
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
	ur_k2rt(rel_key,&rel,&tuple);
	if(status == 0) 
	{
		if(start_atom > 0)
		{
			status = ur_retrieve_tuple_varlist(rel, tuple, list,
									start_atom, num_atom, data_ptr);
		}
		else
		{
			status = -1;
		}
	}
	uu_dexit;
	return(status);
}

