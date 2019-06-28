/*********************************************************************
**    NAME         :  resp01.c
**       CONTAINS:
**       ur_sp01()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       resp01.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:36
*********************************************************************/

#include "udebug.h"
#include "usysdef.h"

/*********************************************************************
**    E_FUNCTION     :  ur_sp01(fnames, fnamea, saveall)
**       pre-save of Solids database part for later load
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

/*--------------------------------------------------------------------
**    Start of Executable Code
**--------------------------------------------------------------------
**
*/
/*
		store(save) a part
*/

	ur_sp01(fnames, fnamea, saveall)
		char		*fnames[]	;	/* solid filename to use	*/
		char		*fnamea[]	;	/* applied geometry filename to use	*/
		UU_LOGICAL	saveall	;	/* flag for selective save */
	{
		int		status			;	/* holds status of unibase calls			*/

		uu_denter(UU_RTRC,(us,"ur_sp01(fnames=%s, fnamea=%s)",
			fnames, fnamea)) ;

		status = UU_SUCCESS;
/*
		flag all solid bodies, indicating that they were saved
*/
		um_pre_save(saveall);

		uu_dexit ;
		return(status)	;
	}
