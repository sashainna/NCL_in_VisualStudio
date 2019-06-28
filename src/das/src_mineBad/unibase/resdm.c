
/*********************************************************************
**    NAME         :  resdm
**       CONTAINS:
**       ur_set_del_mark
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       resdm.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:35
*********************************************************************/
/*********************************************************************
**    E_FUNCTION :  ur_set_del_mark
**			set the delete mark such that the next time somthing is put
**			on the delete stack mark will be set
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/


#include "usysdef.h"
#include "udebug.h"
#include "ribase.h"
#include "rstack.h"

ur_set_del_mark()
{
	extern UR_STACK(UR_del_stack,UR_DEL_STK_DEPTH,dstack_element) ;

	uu_denter(UU_RTRC,(us,"set mark with del stack pointer = %d",UR_del_stack.curpos)) ;
	UR_del_mark = UU_TRUE ;
	uu_dexit ;
}
