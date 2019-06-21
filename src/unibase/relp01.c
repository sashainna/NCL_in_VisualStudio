/*********************************************************************
**    NAME         :  relp01
**       CONTAINS:
**			ur_lp01
**       
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       relp01.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:32
*********************************************************************/

#include "usysdef.h"
#include	"udebug.h"

/*********************************************************************
**    E_FUNCTION     :  status = ur_lp01(fnames, fnamea) 
**  Solids preload procedure, do before calling ur_lp02
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : 
**				0,		pre-load successful
**				<0,	Error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

/*--------------------------------------------------------------------
**    Start of Executable Code
**--------------------------------------------------------------------
**
*/

/*
		pre-load solids
*/

ur_lp01(fnames, fnamea, load)
char		*fnames[]		;	/* solids filename to use	*/
char		*fnamea[]		;	/* applied geometry filename to use	*/
UU_LOGICAL	load;			/* flag - UU_TRUE=load, UU_FALSE=merge */
{
	int		status				;	/* holds status of unibase calls		*/

	uu_denter(UU_RTRC,(us,"ur_lp01 with solids file %s and load = %d",fnames,load));
	status = 0 ;
/*
	preprocess all solid bodies
*/
	um_pre_load(load);
/*
	load the romulus portion of the database
*/
	status = um_ld_romulus(load) ;

/*
	load the applied geometry portion of the database
*/
	status = um_load_appgeo(fnamea,load) ;

	uu_dprint(UU_RTRC,(us,"ur_lp01 exit with status = %d",status)) ;
	uu_dexit ;
	return(status)  ;
}
