#include	"usysdef.h"
#include	"udebug.h"
#include	"rbase.h"
#include	"rstack.h"
/*********************************************************************
**    NAME         :  rifdstk.c
**       CONTAINS:
**       ur_flush_del_stack
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rifdstk.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:43
*********************************************************************/

/*********************************************************************
**    E_FUNCTION :  status = ur_flush_del_stack()
**       description
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : disables the delete stack
**    WARNINGS     : none
*********************************************************************/

int	ur_flush_del_stack()
{
	extern	UR_STACK(UR_del_stack,UR_DEL_STK_DEPTH,dstack_element) ;

	int	status					;	/* return status								*/
	dstack_element	*dstk_ptr	;	/* pointer to an element on del stack	*/
	UU_KEY_ID	del_key			;	/* the key to delete with					*/
	long		del_rel_num			;
	long		del_tuple_indx		;

	uu_denter(UU_RTRC,(us,"flush delete stack with %d elements",UR_del_stack.curpos)) ;

	status = 0 ;

	dstk_ptr = ur_pop(UR_del_stack) ;
	while(dstk_ptr != 0)
	{
		del_key = dstk_ptr->del_key ;
		ur_k2rt(del_key,&del_rel_num,&del_tuple_indx) ;
		ur_delete_tuple_abs(del_rel_num,del_tuple_indx) ;
		dstk_ptr = ur_pop(UR_del_stack) ;
	} /* while dstk_ptr != 0 */

	ur_del_clear() ;	/* make sure the bit map is clear	*/
	ur_disable_del_stack() ;

	uu_dexit ;
	return(status) ;

}
