/*********************************************************************
**    NAME         :  reasp.c
**       CONTAINS:
**       ur_auto_save_part()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reasp.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:27
*********************************************************************/

#include "udebug.h"
#include "ribase.h"
#include "xenv1.h"
#include "nccs.h"
#include "lcom.h"

/*********************************************************************
**    E_FUNCTION     :  ur_auto_save_part(aspfd)
**       save a unibase database part for later load
**			this means of saving the part sacrifices nice features for
**			speed in order to support automatically saving the database.
**			It is not intended for regular use, but for disaster prevention
**			and recovery.
**          Auto save was modified to save the unibase using the
**          standard SAVEU logic. ASF 2/25/14
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_auto_save_part()
{
	int sdisp,stess,status;
	UM_f77_str fname;
	UM_int4 nci;
	UM_int2 ierr;
	char name[15] = "autosave.u";

	uu_denter(UU_RTRC,(us,"ur_auto_save_part, save Unibase to %d",aspfd));
	status = 0;

	ierr = 0;
	UM_init_f77_str(fname, name, UX_MAX_PATH_LEN);
	nci = strlen(name);
/*
.....Save time and space by not saving the display and tesselation lists
*/
	sdisp = UR_save_display;
	stess = UR_save_tessel;
	UR_save_display = 0;
	UR_save_tessel = 0;
	status = saveu(UM_addr_of_f77_str(fname),&nci,&ierr);
	UR_save_display = sdisp;
	UR_save_tessel = stess;

	uu_dexit;
	return(status);
}
