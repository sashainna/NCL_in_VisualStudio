/*********************************************************************
**    NAME         :  ridtv
**       CONTAINS:
**       ur_delete_tuple_varlist
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ridtv.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:42
*********************************************************************/

#include "usysdef.h"
#include	"udebug.h"

/*********************************************************************
** I_FUNCTION : status = ur_delete_tuple_varlist(rel_num,tuple_indx,lst_num)
**      delete a relation entry list
**    PARAMETERS   
**       INPUT  : 
**				rel_num,		a relation number
**				tuple_indx,	the index of the tuple to delete
**				lst_num,		which list to delete, 1 to n_varl,
**       OUTPUT :  
**          none
**    RETURNS      :  -1, if operation was unsuccessful, 0 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_delete_tuple_varlist(rel_num,tuple_indx,lst_num)
long	rel_num;		/* the relation number */
long	tuple_indx;	/* tuple index within the relation */
int	lst_num;
{
	UU_REL_ID	rel_key;	/* a relation,entry key */
	int			status;	/* return status */
	char			*lst_ptr; /* pointer to list to be deleted */
	int			lst_len;	/* length of list to be deleted */

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
*/
	uu_denter(UU_RITRC,(us,"ur_delete_tuple_varlist(rel=%d, tuple=%d, list=%d",
						rel_num, tuple_indx, lst_num));
	status	=	0;		/* init return status		*/

	if(lst_num > 0)	/* if list zero, do nothing */
	{
		/* get list pointer */
		status =	ur_get_varlist_ptr(rel_num,tuple_indx,lst_num,&lst_ptr,&lst_len);

		/* if valid list & non-zero length, delete it */
		if(status == 0 && lst_len > 0)								
		{
			uu_toolfree(lst_ptr);				/* release the memory	*/
			ur_update_varlist_info(rel_num,tuple_indx,lst_num,0,0);
		}
		else
		{
			status = -1;
	  	}
	}
	uu_dexit;
	return(status);
}
