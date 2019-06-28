/*********************************************************************
**    NAME         :  rerdr.c
**       CONTAINS:
**       ur_retrieve_data_relnum()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rerdr.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:34
*********************************************************************/

#include	"usysdef.h"
#include	"ribase.h"
#include	"rmtuple.h"
#include	"udebug.h"

/*********************************************************************
**	E_FUNCTION	:	status = ur_retrieve_data_relnum(key,rel)
**		retrieve the data relation number for a particular key
**	PARAMETERS   
**		INPUT		: 
**			key,	key to to use in retrieving the relation number
**			rel,	a pointer to where the relation number is to be put
**		OUTPUT	:  
**			rel, the appropriate relation number
**    RETURNS	:  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS	:	if the associated key is inactive, an error
**					   status will be returned
*********************************************************************/

ur_retrieve_data_relnum(key, rel)
UU_KEY_ID 	key;	/* the key to update to */
UR_REL_NUM	*rel;	/* pointer to the type */
{
	int				status;			/* status, -1 if error, 0 otherwise */
	UU_REL_ID 		rel_key;			/* the relation,tuple key */
	UR_TUPLE_INDX	tuple;			/* tuple id from key */

	uu_denter(UU_RTRC,(us,"ur_retrieve_data_relnum(key=%d)", key));
	status = 0;
	if (key == 0)
	{
		*rel = 0;
		status = -1;
	}
	else
	{
		ur_k2rt(key, rel, &tuple);		/* get key as rel,entry */	
		if((*rel == UR_MTUPLE_REL)&&uu_tst_bit(UR_rcb[*rel].bmap_ptr,tuple-1))
		{
			/*if (tuple == 1378897)
				{
					sprintf (" ","Invalid value for DNC modal. /%s/ %s"," "," ");
				}*/
		/* valid key_id relation and active entry, now get the data type */
			status = uri_retrieve_master_attr(*rel,tuple,UR_DATA_INDX,&rel_key);
			if(status == 0)
			{
				ur_k2rt(rel_key,rel,&tuple);
			}
		}
	}
	/* else was a direct rel key so already set */
	uu_dprint(UU_RTRC,(us,"ur_retrieve_data_relnum rel %d",*rel));
	uu_dexit;
	return(status);
}
