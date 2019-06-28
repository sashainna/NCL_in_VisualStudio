/*********************************************************************
**    NAME         :  rerdf.c
**       CONTAINS:
**       ur_retrieve_data_fixed()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rerdf.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:34
*********************************************************************/

#include "usysdef.h"
#include "ribase.h"
#include "rmtuple.h"
#include "udebug.h"

/*********************************************************************
**   E_FUNCTION  :  status = ur_retrieve_data_fixed(&data_packet)
**      retrieve data, fixed
**    PARAMETERS   
**       INPUT  : 
**			data_packet,a pointer to a data packet where the fixed data
**							that is retrieved is to be put
**        data_packet.key_id,	key_id to to use in retreiving the data
**       OUTPUT :  
**		data_packet.key_id, still requested key_id
**		data_packet.rel_num, the relation number for the retrieved data
**		data_packet.buff, the retrieved fixed dta
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_data_fixed(data_packet)
struct	UR_data	*data_packet;	/* address of the data packet			*/
{
	int			status;	/* status, -1 if error, 0 otherwise		*/
	UU_KEY_ID 	key;		/* the key_id to retrieve from			*/
	UU_REL_ID	rel_key;	/* the geometric entity tuple from MTID*/
	int			rel;		/* the relation number						*/
	int			tuple;	/* the index into the relation			*/

	uu_denter(UU_RTRC,(us,"ur_retrieve_data_fixed, key = 0x%x",
				data_packet->key_id));

	status = 0 ;

	/* get the data tuple */
	key = data_packet->key_id;
	ur_k2rt(key, &rel, &tuple);
	if(rel == UR_MTUPLE_REL)
	{
		/*if (tuple == 1378897)
				{
					sprintf (" ","Invalid value for DNC modal. /%s/ %s"," "," ");
				}*/
		status = uri_retrieve_master_attr(rel, tuple, UR_DATA_INDX, &rel_key);
	}
	else
	{
		rel_key = key;
	}
	ur_k2rt(rel_key, &rel, &tuple);
	if(status == 0)
	{
		/* got a valid data key, get the fixed data and variable info for it */
		status = ur_retrieve_tuple_and_var_info(rel, tuple, data_packet);
	}
	if(data_packet->key_id == 0)
	{
		data_packet->key_id = rel_key;	/* make key valid in no mtuple */
	}
	uu_dexit;
	return(status);
}

