/*********************************************************************
**    NAME         :  refad.c
**       CONTAINS:
**       ur_free_app_data()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       refad.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:30
*********************************************************************/

#include	"usysdef.h"
#include	"udebug.h"
#include	"ribase.h"

/*********************************************************************
** E_FUNCTION : status = ur_free_app_data(&data_packet)
**		free the varlist space for an application data packet which
**		is no longer needed.
**    PARAMETERS   
**       INPUT  : 
**				&data_packet, address of the data packet to be freed
**       OUTPUT :  
**				none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_free_app_data(data_packet)
/* argument declarations */
struct UR_data	*data_packet;	/* pointer to data packet			*/
{
	int				status;			/* return status		*/
	int				i;					/* an index				*/
	struct UR_lpacket	*lpack_ptr;

	uu_denter(UU_RTRC,(us,"ur_free_app_data for rel= %d at 0x%x",
					data_packet->rel_num, data_packet));
	status = 0;

	/* if varlists, clear pointers and counts and recover the space */
	for(i = 1; i <= UR_rcb[data_packet->rel_num].n_varl; i++)
	{
		ur_get_list_packet_ptr(data_packet->rel_num,i,data_packet,&lpack_ptr);
		if (lpack_ptr && lpack_ptr->list_ptr)
		{
			uu_free(lpack_ptr->list_ptr - sizeof(int));	/* free any list space */
		}
		lpack_ptr->atom_cnt = 0;							/* current atom count */
		lpack_ptr->list_ptr = 0;							/* data pointer */
	}
	uu_dexit;
	return(status);
}
