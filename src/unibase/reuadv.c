/*********************************************************************
**    NAME         :  reuadv.c
**       CONTAINS:
**       ur_update_app_data_varlist()
**    COPYRIGHT 1986 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reuadv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:37
*********************************************************************/

#include "usysdef.h"
#include	"udebug.h"
#include "umoveb.h"
#include "ribase.h"

/*********************************************************************
** E_FUNCTION : status = ur_update_app_data_varlist(&data_packet,lst_num,
**													&var_data, start_atom, num_atom)
**	update variable data for a geometric relation
**    PARAMETERS   
**       INPUT  : 
**				data_packet	address of the data packet whose list to update
**				lst_num,		list number to update
**				var_data,	address of buff to use in updateing list num
**				start_atom,	first atom to update, 1 = first
**				num_atom,	number of atoms to update
**       OUTPUT :  
**          none
**    RETURNS      :  -1 if error on update
**							>0, the number of atoms updated, should = num_atom
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_app_data_varlist(data_packet,lst_num,data_ptr,start_atom,num_atom)
struct UR_data	*data_packet;	/* ptr to the packet whose list to update */
int				lst_num;			/* which list of data to update */
char 				*data_ptr;		/* ptr to where to get the data */
int				start_atom;		/* first atom to update */
int				num_atom;		/* number of atoms to update */
{
	extern char	*uu_malloc();		/*allocation routine */

	UU_REL_ID			rel_key;		/* a relation,entry tuple */
	unsigned	int		rel_num;		/* relation id to update to */
	unsigned	int		tuple_indx;	/* entry id to update to */
	int					status;		/* return status */
	int					atom_size;	/* size of the data atom */
	int					data_len;	/* length of data to move */
	int					data_displ;	/* displacement to the data */
	char					*new_ptr;	/* new list pointer */
	int					max_atom;	/* max # of atoms the varlist can now hold */
	int					lst_len;		/* current list length in bytes */
	int					new_len;		/* new length of list in bytes */
	int					req_size;	/* min expansion size for the list */
	int					exp_size;	/* actual expansion size */
	struct UR_lpacket	*lpack_ptr;	/* pointer to data packet */

	uu_denter(UU_RTRC,(us,"ur_update_app_data_varlist(data adrs=0x%x,list_num=%d, start=%d, num=%d)",
					data_packet, lst_num, start_atom, num_atom));
	status = 0;
	rel_num = data_packet->rel_num;
	ur_get_atom_size(rel_num,lst_num,&atom_size);

	/* calculate diaplacement, byte count, and go get the data */
	if(start_atom > 0)
	{
		data_displ = (start_atom - 1) * atom_size;
		data_len = num_atom * atom_size;
		ur_get_list_packet_ptr(rel_num, lst_num, data_packet,
									&lpack_ptr);

		/* go get current list max length */
		status = ur_get_app_varlist_max(data_packet, lst_num, &max_atom);
		lst_len = max_atom * atom_size;			/* max len in bytes */
		uu_dprint(UU_RITRC,(us,"  :avail. len %d bytes",
						lst_len));

		/* if no room for the transfer, must get a bigger piece of */
		/* memory & copy over existing */
		if(status == 0)
		{
			if(lst_len < (data_displ + data_len))
			{
				req_size = atom_size*UR_rcb[rel_num].lparms[lst_num-1].list_size;
				uu_dprint(UU_RITRC,(us,"  :required length %d bytes",
								req_size));
				exp_size = (data_len > req_size) ? data_len : req_size;
				uu_dprint(UU_RITRC,(us,"  :expanding length by %d bytes",
								exp_size));
				new_len = data_displ + exp_size + sizeof(int);
				new_ptr = uu_malloc(new_len);			/* get a new piece of mem	*/
				if(!new_ptr)
				{
					uu_dprint(-1,(us,
								"ERROR:ur_update_app_data unable to allocate"));
					uu_dexit;
					return(-1);		/* bail out with failure */
				}
				*(int *)new_ptr = (data_displ+exp_size) / atom_size;/* new max */
				new_ptr += sizeof(int);					/* list ptr w/room for max */
				if(lst_len != 0) 							/* if existing list,			*/
				{
					/* copy existing data and, release the memory*/
					uu_dprint(UU_RITRC,(us,"ur_update_app_data:move old data"));
					uu_move_byte(lpack_ptr->list_ptr, new_ptr, lst_len);
					uu_free(lpack_ptr->list_ptr - sizeof(int));
				}
				/* update the pointer and length */
				lpack_ptr->list_ptr = new_ptr;
				lpack_ptr->atom_cnt = start_atom + num_atom -1;

				/* now copy in the new data */
				uu_dprint(UU_RITRC,(us,"ur_update_app_data:copying new data"));
				uu_move_byte(data_ptr, new_ptr+data_displ, data_len);
				status = data_len;
			}
			else
			{
				uu_dprint(UU_RITRC,(us,"ur_update_app_data:copying new data"));
				uu_move_byte(data_ptr, lpack_ptr->list_ptr+data_displ, data_len);
				if (start_atom + num_atom - 1 > lpack_ptr->atom_cnt)
				{
					/* update the length */
					lpack_ptr->atom_cnt = start_atom + num_atom -1;
				}
				status = data_len;
			}
			status = status/atom_size;
		}
	}
	else
	{
		status = -1;
	}
	uu_dexit;
	return(status);
}
