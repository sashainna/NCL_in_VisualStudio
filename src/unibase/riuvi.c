/*********************************************************************
**    NAME         :  riuvi
**       CONTAINS:
**       ur_update_varlist_info
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       riuvi.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:48
*********************************************************************/

#include "usysdef.h"
#include "ribase.h"
#include "udebug.h"

/*********************************************************************
** I_FUNCTION : status = ur_update_varlist_info(rel_num,tuple_indx,lst_num,
**																lst_ptr,lst_len)
**      create a relation entry list, define the pointer and length
**    PARAMETERS   
**       INPUT  : 
**		rel_num, a relation,
**		tuple_indx	entry tuple
**		lst_num, which list within the rel,entry for which to get the ptr
**		lst_ptr, pointer to the list data
**		lst_len,	length of the data in bytes
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function was successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_varlist_info(rel_num,tuple_indx,lst_num,lst_ptr,lst_len) 
/* argument declarations */
UR_REL_NUM		rel_num;		/* a  relation,entry tuple */
UR_TUPLE_INDX	tuple_indx;
int				lst_num;		/* number of the list */
char				*lst_ptr;	/* address of list */
int				lst_len;		/* length of list */
{
	/* local  parameter declarations */
	int					status;	/* return status */
	int					displ;	/* delat in words to var list info */
	struct UR_lpacket	*a_ptr;	/* a pointer to the lst ptr & length entry */
	char					*b_ptr;

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/

	uu_denter(UU_RTRC,(us,"ur_update_varlist_info(rel=%d, tuple=%d, lst#=%d, ptr=0x%x, len=%d)",
					rel_num,tuple_indx,lst_num,lst_ptr,lst_len));
	status	=	0;

	/* if legal list number, stuff the pointer and length */
	if(UR_rcb[rel_num].n_varl >= lst_num)
	{
		/* NOTE: if list number is 0, i.e. fixed data, there is nothing to do */
		if(lst_num > 0)	
		{
			/* get a pointer to the relation entry */
			status = ur_get_tuple_ptr(rel_num, tuple_indx, &b_ptr);
			if(status == 0)
			{
				ur_get_list_packet_ptr(rel_num,lst_num,b_ptr,&a_ptr);
				a_ptr->list_ptr = lst_ptr;
				a_ptr->atom_cnt = lst_len;
				ur_unibase_change(rel_num);
			}
		}
	}
	else
	{
		uu_dprint(-1,(us,"ERROR:ur_update_varlist_info illegal list num"));
		status = -1;	/* if still zero, illegal list number specified	*/
	}
	uu_dexit;
	return(status);
}
