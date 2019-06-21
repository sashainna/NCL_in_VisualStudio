#include "udebug.h"
#include "usysdef.h"

/*********************************************************************
**    NAME         :  resp03.c
**       CONTAINS:
**       ur_sp03
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       resp03.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:36
*********************************************************************/

/*********************************************************************
**    E_FUNCTION     :  ur_sp03(fnames, fnamea)
**       post-save of solids database part for later load
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
		post store(save) a Solids portion of a part
*/

	ur_sp03(fnames, fnamea)
		char		*fnames[]	;	/* solid filename to use	*/
		char		*fnamea[]	;	/* applied geometry filename to use	*/
	{
		int		status			;	/* holds status of unibase calls			*/

		uu_denter(UU_RTRC,(us,"ur_sp03(fnames=%s, fnamea=%s)",
			fnames, fnamea)) ;
		status = 0 ;
/*
		save the romulus portion of the database
*/
		um_sv_romulus(fnames) ;
/*
/*		save the applied geometry portion of the database
/*		um_save_appgeo(fnamea);
*/
/*
		perform any special processing
*/
		um_post_save();

		uu_dexit ;
		return(status)	;
	}
