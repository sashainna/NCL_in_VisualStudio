/*********************************************************************
**    NAME         :  riglpp.c
**       CONTAINS:
**       ur_get_list_packet_ptr()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       riglpp.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:43
*********************************************************************/

#include	"udebug.h"
#include	"ribase.h"

/*********************************************************************
** E_FUNCTION : status = ur_get_list_packet_ptr(rel_num,list_num,
**										&data_packet,&lpack_ptr) 
**		get a list packet pointer into a data packet
**    PARAMETERS   
**       INPUT  : 
**				rel_num,		relation number for which to get the list packet
**								pointer 
**				list_num,	list number for which the pointer is requested
**				&data_packet, address of data_packet from which to contruct
**								the pointer
**       OUTPUT :  
**				lpack_ptr	the requested pointer
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_get_list_packet_ptr(rel_num,list_num,data_packet,lpack_ptr)
UR_REL_NUM			rel_num;			/* rel number to set-up for	*/
int					list_num;		/* which list, 1-9				*/
struct UR_data		*data_packet;	/* pointer to data packet		*/
struct UR_lpacket	**lpack_ptr;	/* pointer to data packet		*/
{
	int	status;					/* return status		*/
	int	fixed_length;			/* size of fixed data, without list info	*/
	char	*a_ptr;					/* a byte pointer to do calculations with */
	struct attr_def   *atdefs; /* attribute definitions */
	int      num_attr;         /* number of attributes parsed */

	uu_denter(UU_RITRC,(us,"ur_get_list_packet_ptr(rel=%d,list#=%d,adrs=0x%x)",
					rel_num,list_num,data_packet));
	status = 0;

	/* if variable length lists, set up the pointers and clear atom counts */
	*lpack_ptr = 0;
	if(UR_rcb[rel_num].n_varl > 0)
	{
		/* get the data definition for the relation */
		num_attr = UR_rcb[rel_num].rel_def->rellen;
		atdefs = UR_rcb[rel_num].relattr;

		/* calculate the pointer into the data packet where the */
		/* atom count and list pointer need to be placed */
		a_ptr = ((char *)data_packet)
				+ atdefs[num_attr - UR_rcb[rel_num].n_varl + list_num - 1].attr_here;
		*lpack_ptr = (struct UR_lpacket *) a_ptr;
	}
	uu_dprint(UU_RITRC,(us,"list_packet_ptr = 0x%x",*lpack_ptr));
	uu_dexit;
	return(status);
}

