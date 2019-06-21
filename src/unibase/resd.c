/*********************************************************************
**    NAME         :  resd.c
**       CONTAINS:
**       ur_setup_data()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       resd.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:35
*********************************************************************/

#include	"usysdef.h"
#include	"udebug.h"
#include	"ribase.h"

/*********************************************************************
** E_FUNCTION : status = ur_setup_data(rel_num,&data_packet,sizeof(data_packet))
**		set up a data packet for an antcipated create data,
**		the rel_num is set in the data packet and pointers are 
**		established variable lists, if any, with an initial number of
**		entries 
**    PARAMETERS   
**       INPUT  : 
**				rel_num,	relation number for which to set up the data 
**							packet 
**				&data_packet, address of data-packet to setup
**				packet_size,	size of the supplied data packet
**       OUTPUT :  
**				none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_setup_data(rel_num,data_packet,packet_size)
/* argument declarations */
int				rel_num;			/* relation number to set-up for	*/
struct UR_data	*data_packet;	/* pointer to data packet			*/
int				packet_size;	/* size in bytes						*/
{
	int	status			;	/* return status		*/
	int	i					;	/* an index				*/
	int	n_varl			;	/* the number of varlists for this rel		*/
	struct 	UR_lpacket	*lpack_ptr	;
	int	max_var_size	;	/* max number of bytes available to vlists*/
	int	size_each[20];		/* space assigned each list */
	char	*the_ptr			;	

	uu_denter(UU_RTRC,(us,"ur_setup_data(rel=%d, adrs=0x%x, size=%d)",
					rel_num, data_packet, packet_size));
	status = 0;

	/* stuff the relation number in the data packet */
	data_packet->rel_num = rel_num;
	uri_clear_fixed_keys(data_packet);	/* init any assoc keys */

	/* if variable length lists, set up the pointers and clear atom counts */
	n_varl = UR_rcb[rel_num].n_varl	;
	if(n_varl > 0)
	{
		/* calculate the avaiable size for the variable lists */
		max_var_size = packet_size - UR_rcb[rel_num].tuple_size ;
		if(max_var_size < 0)	/* some room for lists? */
		{
			status = -1 ;
		}
		else
		{
			/* distribute the available space among the varlists */
			uri_calc_space(max_var_size, rel_num, size_each);
			the_ptr = ((char *) data_packet) + UR_rcb[rel_num].tuple_size;
			for(i = 1; i <= n_varl; i++)
			{
				ur_get_list_packet_ptr(rel_num,i,data_packet,&lpack_ptr) ;
				lpack_ptr->atom_cnt = 0 ;
				lpack_ptr->list_ptr = the_ptr	;
				uu_dprint(UU_RITRC,(us,"put ptr 0x%x at 0x%x",
							the_ptr,&(lpack_ptr->list_ptr))) ;
				the_ptr += size_each[i];
			}
		}
	}
	uu_dexit	;
	return(status) ;
}
