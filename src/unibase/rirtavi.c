#define MODULEDBG 0
/*********************************************************************
**    NAME         :  rirtavi.c
**       CONTAINS:
**       ur_retrieve_tuple_and_var_info()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rirtavi.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:47
*********************************************************************/

#include "usysdef.h"
#include "rbase.h"
#include "ribase.h"
#include "udebug.h"

/*********************************************************************
**   E_FUNCTION  :  status = ur_retrieve_tuple_and_var_info(rel_num, tuple_indx,
**																&data_packet)
**      retrieve fixed data and variable length list information
**    PARAMETERS   
**       INPUT  : 
**			rel_num,			relation number to retrieve from
**			tuple_indx,		index of tuple to retrieve
**			data_packet,	a pointer to a data packet where the fixed data
**								and var info that is retrieved is to be put
**       OUTPUT :  
**			data_packet.key_id,	still requested key_id
**			data_packet.rel_num,	the relation number for the retrieved data
**			data_packet.buff,		the retrieved fixed data and var info
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_tuple_and_var_info(rel_num, tuple_indx, data_packet)
UR_REL_NUM		rel_num;			/* relation number to retrieve from */
UR_TUPLE_INDX	tuple_indx;		/* index of tuple to retrieve */
struct UR_data	*data_packet;	/* address of the data packet */
{
	int		status;		/* status, -1 if error, 0 otherwise		*/
	int		i;				/* an index										*/
	int		lst_len;		/* length of a varlist in bytes			*/
	int		atom_size;	/*  size of an atom within the list		*/
	char		*lst_ptr;	/* pointer to a varlist, we don't use	*/
	struct UR_lpacket	*lpack_ptr;	/* pointer to a list_pack	*/

/*----------------------------------------------------------------------
** Start of Executable Code
**----------------------------------------------------------------------
**
*/
	uu_denter(UU_RTRC,(us,"ur_retrieve_tuple_and_var_info(rel=%d, tuple=%d)",
				rel_num, tuple_indx));

	/* get the data tuple */
	status = ur_retrieve_tuple(rel_num, tuple_indx, data_packet);
	if(!status)
	{
		/* now insert the variable length info after the fixed data */
		for(i = 1; i <= UR_rcb[rel_num].n_varl; i++)
		{
			/* get a pointer to the list packet data after the fixed data */
			ur_get_list_packet_ptr(rel_num, i, data_packet, &lpack_ptr);
			ur_get_varlist_ptr(rel_num, tuple_indx, i, &lst_ptr,
								&(lpack_ptr->atom_cnt));
			lpack_ptr->list_ptr = 0;
#if MODULEDBG != 0
			uu_dprint(UU_RITRC,(us,"list %d has %d atoms",
							i, lpack_ptr->atom_cnt));
#endif
			lpack_ptr++;
		}
	}
	uu_dexit;
	return(status);
}
