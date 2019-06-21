/*********************************************************************
**    NAME         :  resad.c
**       CONTAINS:
**       ur_setup_app_data()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       resad.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:35
*********************************************************************/

#include	"usysdef.h"
#include	"udebug.h"
#include	"ribase.h"

/*********************************************************************
** E_FUNCTION : status = ur_setup_app_data(rel_num, &data_packet,
**														sizeof(data_packet))
**		set up a data packet for an antcipated create data,
**		the rel_num is set in the data packet and pointers are 
**		established variable lists, if any, with an initial number of
**		entries, space for the lists is allocated.
**    PARAMETERS   
**       INPUT  : 
**				rel_num,	relation number for which to set up the data 
**							packet 
**				&data_packet, address of where to put the data
**       OUTPUT :  
**				none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_setup_app_data(rel_num,data_packet,packet_size)
/* argument declarations */
int				rel_num;			/* relation number to set-up for	*/
struct UR_data	*data_packet;	/* pointer to data packet			*/
int				packet_size;	/* size in bytes						*/
{
	char				*uu_malloc();	/* allocation routine */

	int				req_size;		/* required varlist size */
	int				status;			/* return status		*/
	int				i;					/* an index				*/
	int				n_varl;			/* the number of varlists for this rel		*/
	struct UR_lpacket	*lpack_ptr;
	int				max_var_size;	/* max number of bytes available to vlists*/
	int				size_each[8];	/* space assigned each list */
	char				*the_ptr;	

	uu_denter(UU_RTRC,(us,"ur_setup_app_data for rel= %d, size= %d at 0x%x",
					rel_num,packet_size,data_packet));
	status = 0;

	/* stuff the relation number in the data packet */
	data_packet->rel_num = rel_num;
	uri_clear_fixed_keys(data_packet);	/* init any assoc keys */

	/* if variable length lists, set up the pointers and clear atom counts */
	n_varl = UR_rcb[rel_num].n_varl;

	/* calculate the needed room for the variable lists */
	for(i = 1; i <= n_varl; i++)
	{
		ur_get_list_packet_ptr(rel_num,i,data_packet,&lpack_ptr);
		req_size = UR_rcb[rel_num].lparms[i-1].atom_size *
					  UR_rcb[rel_num].lparms[i-1].list_size;
		the_ptr = uu_malloc(req_size + sizeof(int));		/* allocate list space */
		*((int *)the_ptr) = UR_rcb[rel_num].lparms[i-1].list_size;	/* max atom count */
		lpack_ptr->atom_cnt = 0;								/* current atom count */
		lpack_ptr->list_ptr = the_ptr + sizeof(int);		/* data pointer */
		uu_dprint(UU_RITRC,(us,"put ptr 0x%x at 0x%x",
					the_ptr,&(lpack_ptr->list_ptr)));
	}
	uu_dexit;
	return(status);
}
