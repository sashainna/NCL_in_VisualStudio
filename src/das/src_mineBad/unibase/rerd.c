#define MODULEDBG 1
/*********************************************************************
**    NAME         :  rerd.c
**       CONTAINS:
**       ur_retrieve_data()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rerd.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:34
*********************************************************************/

#include	"ribase.h"
#include	"rmtuple.h"
#include	"udebug.h"

/*********************************************************************
** E_FUNCTION : status = ur_retrieve_data(&data_packet,sizeof(data_packet))
**      retrieve application data, both fixed & variable
**    PARAMETERS   
**       INPUT  : 
**				&data_packet, address of where to put the data
**				data_packet.key_id = key id of data to retrieve
**				data_size,	size of data_packet in bytes
**       OUTPUT :  
**				data_packet, retrieved data 
**				data_packet.rel_num = set to relation number of data
**				data_packet.data = the data
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_data(data_packet,data_size)
struct UR_data	*data_packet;	/* pointer to data packet */
int					data_size;	/* size of data packet in bytes */
{
	int					status;
 	UU_REL_ID			rel_key;			/* key to relation tuple */
	UR_REL_NUM			rel;			/* number identifying the rel	*/
	UR_TUPLE_INDX		tuple;		/* index into the relation */
	char					*lst_ptr;		/* ptr to a list, where to get the data */
	int					atom_cnt;		/* length of the variable length data */
	int					i, j;				/* index */
	struct UR_lpacket	*lpack_ptr;		/* pointer to a list packet */
	struct UR_lpacket	*folpack_ptr;	/* pointer to a list packet */
	int					atom_size;		/* the atom size of the list */
	int					max_atoms;		/* max atoms in a list */
	int					space_used;
	int					space_left;
	int					space_each[UR_MAX_VARL];
	char					*the_ptr;
	
	uu_denter(UU_RTRC,(us,"ur_retrieve_data(key=0x%x, rel=%d)",
					data_packet->key_id,data_packet->rel_num));
	uu_dprint(UU_RITRC,(us,"ur_retrieve_data: data packet size = %d",data_size));
	status = 0;
	ur_k2rt(data_packet->key_id,&rel,&tuple);

	/* if a master key, then go get the data key, otherwise check */
	/* if legal relation number and assume it's a direct relation key */
	if(rel == UR_MTUPLE_REL)
	{
		/*if (tuple == 1378897)
				{
					sprintf (" ","Invalid value for DNC modal. /%s/ %s"," "," ");
				}*/
		status = uri_retrieve_master_attr(rel, tuple, UR_DATA_INDX, &rel_key);
#if MODULEDBG != 0
		uu_dprint(UU_RITRC,(us,"ur_retrieve_data: rel_key = 0x%x",rel_key));
#endif
		ur_k2rt(rel_key,&rel,&tuple);
	} 
	else if(rel > UR_MAX_REL)
	{
		uu_dprint(-1,(us,"ERROR:ur_retrieve_data - illegal relation number"));
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
			space_used = 0;		/* init sum of varlist lengths */
			for(i = 1; i <= UR_rcb[rel].n_varl; i++)
			{
				ur_get_list_packet_ptr(rel,i,data_packet,&lpack_ptr);
				ur_get_atom_size(rel,i,&atom_size);
				space_used += lpack_ptr->atom_cnt * atom_size;
			}
#if MODULEDBG != 0
			uu_dprint(UU_RITRC,(us,
					"ur_retrieve_data: tuple size = %d",UR_rcb[rel].tuple_size));
#endif
			space_left = data_size - UR_rcb[rel].tuple_size - space_used;
			if(space_left < 0)	/* need more than we have? */
			{
				/* the lists won't fit -- bail out! */
				uu_dprint(-1,(us,"ERROR:ur_retrieve_data insufficient space."));
				status = -1;
				uu_dexit;
				return(status);
			}
			/* distribute the remaining space a la setup_data */
			uri_calc_space(space_left, rel, space_each);

			/* fill in the pointers and retrieve the varlist data */
			the_ptr = ((char *)data_packet) + UR_rcb[rel].tuple_size;
			for(i = 1; i <= UR_rcb[rel].n_varl; i++)
			{
				ur_get_list_packet_ptr(rel,i,data_packet,&lpack_ptr);
				lpack_ptr->list_ptr = the_ptr;
				ur_get_varlist_ptr(rel, tuple,i,&lst_ptr,&atom_cnt);
				ur_retrieve_tuple_varlist(rel,tuple,i,1,atom_cnt,
					lpack_ptr->list_ptr)	;
				ur_get_atom_size(rel,i,&atom_size);
				the_ptr += lpack_ptr->atom_cnt * atom_size + space_each[i];
				lpack_ptr++ ;
			}/* for i			 */
		}/* if status == 0 */
		if(data_packet->key_id == 0)
		{
			data_packet->key_id = rel_key;	/* make key valid in no mtuple */
		}
	}
	uu_dexit;
	return(status);
}

