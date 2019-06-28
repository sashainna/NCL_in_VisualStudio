/*********************************************************************
**    NAME         :  regavm.c
**       CONTAINS:
**       ur_get_app_varlist_max()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       regavm.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:30
*********************************************************************/

#include "ribase.h"
#include "udebug.h"

/*********************************************************************
** E_FUNCTION : status = ur_get_app_varlist_max(&data_packet,
**											list_num, &list_max)
**		get the maximum number of atoms allowed in a variable length list
**		within a dynamic application bundle
**    PARAMETERS   
**       INPUT  : 
**				&data_packet,	address of application data packet to expand
**								the list in
**				list_num,	number of the list to get the maximum of
**				&list_max,	address of where to put the maximum
**       OUTPUT :  
**				list_max,	set to the maximum number of allowed atoms
**								in the list 
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS :	none
**    WARNINGS     :	the returned list maximum does include any atoms
**							already used, i.e. useable atoms = max - used
*********************************************************************/

ur_get_app_varlist_max(data_packet, list_num, list_max)
/* argument declarations         */
struct	UR_data	*data_packet;	/* address of appl data packet		*/
int					list_num		;	/* number of list to get the max for*/
int					*list_max	;	/* address of where to put the max	*/
{
	int					status;			/* return status			*/
	int					rel_num;			/* relation number of the list	*/
	struct UR_lpacket	*lpack_ptr;		/* ptr to list packet in data	*/

	uu_denter(UU_RTRC,(us,"ur_get_app_varlist_max, rel= %d, list= %d",
					data_packet->rel_num,list_num));
	*list_max = 0 ;	/* default to nothing	*/
	status = 0;
	rel_num = data_packet->rel_num;
	if(list_num > 0 && list_num <= UR_rcb[rel_num].n_varl)
	{
		status = ur_get_list_packet_ptr(rel_num,list_num,data_packet,&lpack_ptr);
		if(lpack_ptr->list_ptr)
		{
			/* get saved max */
			*list_max = *((int *)(lpack_ptr->list_ptr - sizeof(int)));
		}
	}
	uu_dprint(UU_RITRC,(us,"ur_get_app_varlist_max, max = %d", *list_max));
	uu_dexit;
	return(status);
}
