/*********************************************************************
**    NAME         :  rerad.c
**       CONTAINS:
**       ur_retrieve_app_data()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rerad.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:33
*********************************************************************/

#include "rbase.h"
#include	"ribase.h"
#include	"rmtuple.h"
#include	"udebug.h"

/*********************************************************************
** E_FUNCTION : status = ur_retrieve_app_data(&data_packet)
**      retrieve application data, both fixed & variable
**			space for the variable lists, if any, is allocated to fit.
**    PARAMETERS   
**       INPUT  : 
**				&data_packet, address of where to put the data
**				data_packet.key_id = key id of data to retrieve
**       OUTPUT :  
**				data_packet, retrieved data 
**				data_packet.rel_num = set to relation number of data
**				data_packet.data = the data
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_app_data(data_packet)
struct	UR_data	*data_packet;	/* pointer to data packet */
{
	extern char	*uu_malloc();	/* allocation routine */

	int			status;
 	UU_REL_ID	rel_key;			/* key to relation tuple */
	int			rel;			/* number identifying the rel	*/
	int			tuple;		/* index into the relation */
	char			*lst_ptr;		/* ptr to a list, where to get the data */
	int			atom_cnt;		/* length of the variable length data */
	int			i, j;				/* index */
	struct UR_lpacket	*lpack_ptr;	/* pointer to a list packet */
	struct UR_lpacket	*folpack_ptr; /* pointer to a list packet */
	int			atom_size;		/* the atom size of the list */
	int			max_atoms;		/* max atoms in a list */
	int			space_used;
	int			space_left;
	int			space_each[UR_MAX_VARL];
	char			*the_ptr;
	
	uu_denter(UU_RTRC,(us,"ur_retrieve_app_data for key_id= 0x%x, rel= %d",
					data_packet->key_id,data_packet->rel_num));
	status = 0;
	ur_k2rt(data_packet->key_id,&rel,&tuple);

	/* if a master key, then get the data key from the MTUPLE relation, */
	/* otherwise check if legal relation number and assume it's a */
	/* direct relation key */
	if(rel == UR_MTUPLE_REL)
	{
		/*if (tuple == 1378897)
				{
					sprintf (" ","Invalid value for DNC modal. /%s/ %s"," "," ");
				}*/
		status = uri_retrieve_master_attr(rel, tuple, UR_DATA_INDX, &rel_key);
		ur_k2rt(rel_key,&rel,&tuple);
	} 
	else if(rel > UR_MAX_REL)
	{
		status = -1 ; /* illegal relation number	*/
	}
	else
	{
		rel_key = data_packet->key_id;
	}
	if(status == 0)
	{
		/* first get the fixed data and atom counts for any varlists */
		/* then determine how to allocate the space for varlists. */
		status = ur_retrieve_tuple_and_var_info(rel,tuple,data_packet);
		if(status == 0)
		{
			for(i = 1; i <= UR_rcb[rel].n_varl; i++)
			{
				ur_get_list_packet_ptr(rel,i,data_packet,&lpack_ptr);
				ur_get_atom_size(rel,i,&atom_size);
				space_used = lpack_ptr->atom_cnt * atom_size;
				the_ptr = uu_malloc(space_used + sizeof(int));/* allocate space */
				if(the_ptr == 0)
				{
					uu_dprint(-1,(us,
								"ERROR:uu_malloc failure in ur_retrieve_app_data"));
					uu_dexit;
					return(-1);
				}
				*((int *)the_ptr) = lpack_ptr->atom_cnt;		/* set max count */

				/* fill in the pointer and retrieve the varlist data */
				lpack_ptr->list_ptr = the_ptr + sizeof(int);
				ur_get_varlist_ptr(rel, tuple,i,&lst_ptr,&atom_cnt);
				if(atom_cnt)
				{
					ur_retrieve_tuple_varlist(rel,tuple,i,1,atom_cnt,
						lpack_ptr->list_ptr);
				}
			}/* for each varlist */
		}/* if status == 0 */
		if(data_packet->key_id == 0)
		{
			data_packet->key_id = rel_key;	/* make key valid in no mtuple */
		}
	}
	uu_dexit;
	return(status);
}
