/*********************************************************************
**    NAME         :  relp03
**       CONTAINS:
**			ur_lp03
**       
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       relp03.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:32
*********************************************************************/

#include "usysdef.h"
#include	"udebug.h"

/*********************************************************************
**    E_FUNCTION     :  status = ur_lp03(fnames, fnamea,load) 
**  		Unibase post-load processing
**    PARAMETERS   
**       INPUT  : 
**				fnames, solids file being loaded
**				load, true if a load, false if a merge
**       OUTPUT :  
**          none
**    RETURNS      : 
**				0,		load successful
**				<0,	Error
**    SIDE EFFECTS : indirectly many effects on the database
**    WARNINGS     : none
*********************************************************************/

ur_lp03(fnames, fnamea, load)
char			*fnames[];	/* solids filename to use	*/
char			*fnamea[];	/* applied geometry filename to use	*/
UU_LOGICAL	load;			/* flag true for load false for merge */
{
	int	status;		/* return status	*/

/*--------------------------------------------------------------------
** Start of Executable Code
**--------------------------------------------------------------------
**
*/

	uu_denter(UU_RTRC,(us,"ur_lp03 %s, load = %d",fnames,load));
	if(load)
	{
		ur_install_new_modals();
	}
	/* post process the solid bodies (ROMULUS) before they are displayed */
	/* ... and many other interesting post load processes these days */
	status = um_post_load(fnames, fnamea, load);
	uu_dexit;
	return(status);
}
