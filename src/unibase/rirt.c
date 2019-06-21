/*********************************************************************
**    NAME         :  rirt
**       CONTAINS:
**       ur_retrieve_tuple
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rirt.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:47
*********************************************************************/

#include "usysdef.h"
#include "ribase.h"
#include "udebug.h"
#include "umoveb.h"
#include "uhep.h"

/*********************************************************************
**    I_FUNCTION : status = ur_retrieve_tuple(rel_num,tuple_indx,&data)
**		retrieve a relation tuple data
**    PARAMETERS   
**       INPUT  : 
**				rel_num,	a relation number
**				tuple_indx, the tuple index within the relation
**				data, pointer to where to put the data
**       OUTPUT :  
**           data, the data retrieved
**    RETURNS      :  0 if function was successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_tuple(rel_num,tuple_indx,data_ptr)
/* argument declarations         */
UR_REL_NUM		rel_num;		/* the relation from the tuple						*/
UR_TUPLE_INDX	tuple_indx;	/* the entry from the tuple							*/
char 		*data_ptr;			/* ptr to a lst of data, where data goes	*/
{
	/* local  parameter declarations */
	UU_REL_ID	rel_key;		/* relation num, tuple index key			*/
	int			status;		/* return status								*/
	char			*lst_ptr;	/* pointer to a list, where to get the data		*/
	int			lst_len;		/* the length of the fixed length data				*/

/*----------------------------------------------------------------------
**  Start of Executable Code
**----------------------------------------------------------------------
**
*/
	uu_denter(UU_RTRC,(us,"ur_retrieve_tuple(rel=%d, tuple=%d)",
					rel_num,tuple_indx)) ;

	/* make sure the entry is active before moving any data */
	if(uu_tst_bit(UR_rcb[rel_num].bmap_ptr,tuple_indx-1))
	{
		/* get list pointer for list 0(fixed data), and move the data from it */
		status = ur_get_varlist_ptr(rel_num, tuple_indx,0,&lst_ptr,&lst_len)	;
		if(status == 0 && lst_len > 0)
		{
			uu_move_byte(lst_ptr,data_ptr,lst_len) ;
		}
	}
	else
	{
		uu_dprint(-1,(us,"ERROR:ur_retrieve_tuple for inactive tuple"));
		status = -1;	/* don't return data if no defined entry	*/
	}
	uu_dexit ;
	return(status) ;
}
