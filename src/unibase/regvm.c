#include "ribase.h"
#include "udebug.h"
/*********************************************************************
**    NAME         :  regvm
**       CONTAINS:
**       ur_get_list_max
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       regvm.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:31
*********************************************************************/

/*********************************************************************
** E_FUNCTION : status = ur_get_varlist_max(&data_packet,sizeof(data_packet),list_num,&list_max)
**		get the maximum number of atoms allowed in a variable length list
**		within a set-up or retreived application bundle
**    PARAMETERS   
**       INPUT  : 
**				&data_packet,	address of application data packet to expand
**								the list in
**				sizeof(data_packet), the size of the entire packet in bytes
**				list_num,	number of the list to get the maximum of
**				&list_max,	address of where to put hte maximum
**       OUTPUT :  
**				list_max,	set to the maximum number of allowed atoms
**								in the list 
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS :	none
**    WARNINGS     :	the returned list maximum does include any atoms
**							already used, i.e. useable atoms = max - used
*********************************************************************/

ur_get_varlist_max(data_packet,packet_size,list_num,list_max)

   /* argument declarations         */
	struct	UR_data	*data_packet;	/* address of appl data packet		*/
	int					packet_size	;	/* size of data packet in bytes		*/
	int					list_num		;	/* number of list to get the max for*/
	int					*list_max	;	/* address of where to put the max	*/

	{
		int	status	;	/* return status			*/
		int					rel_num		;	/* relation number of the list	*/
		struct	UR_lpacket	*lpack_ptr	;	/* ptr to list packet in data	*/
		char					*start_of_list	; /* the end of list_num's space	*/
		char					*end_of_list;	/* the end of list_num's space	*/
		int					atom_size	;	/* size of atom in list_num		*/

		uu_denter(UU_RTRC,(us,"ur_get_varlist_max, rel= %d, list= %d",
					data_packet->rel_num,list_num)) ;
		*list_max = 0 ;	/* default to nothing	*/
		rel_num = data_packet->rel_num	;

		if(list_num > 0 && list_num <= UR_rcb[rel_num].n_varl )
		{
/*
		end of list is either the start of the next list , or
		the end of the packet
*/
			if(list_num != UR_rcb[rel_num].n_varl)
			{
				ur_get_list_packet_ptr(rel_num,list_num+1,data_packet,&lpack_ptr);
				end_of_list = lpack_ptr->list_ptr ;
				end_of_list--	;
			}
			else
			{
				end_of_list = ((char *) data_packet) + packet_size	;
				end_of_list--	;
			}
			status = ur_get_list_packet_ptr(rel_num,list_num,data_packet,
							&lpack_ptr)  ;
			start_of_list = lpack_ptr->list_ptr ;
			ur_get_atom_size(rel_num,list_num,&atom_size) ;

			if(atom_size > 0)
			*list_max = (end_of_list - start_of_list +  1) / atom_size ;
		}


		uu_dexit	;
		return(status) ;
	}
