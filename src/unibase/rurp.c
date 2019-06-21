/*********************************************************************
**    NAME         :  rurp.c
**       CONTAINS:
**       ur_recover_part()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rurp.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:50
*********************************************************************/

#include "udebug.h"
#include "usysdef.h"
#include "dasnog.h"
#include "rmtuple.h"
#include "xenv1.h"
#include "xfsys1.h"
#include "rbase.h"
#include "ribase.h"
#include "uhep.h"
#include "nccs.h"

UU_LOGICAL	ud_lyesno();

/*********************************************************************
**    E_FUNCTION     :  ur_recover_part()
**			recover a part saved by autosave
**          Auto save was modified to save the unibase using the
**          standard SAVEU logic so the unibase can now be recovered
**          using LOADU. ASF 2/25/14
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_recover_part()
{
	int status;
	UM_f77_str fname;
	UM_int4 nci;
	UM_int2 batch=0,ierr=0;
	char name[15] = "autosave.u";
/*--------------------------------------------------------------------
**    Start of Executable Code
**--------------------------------------------------------------------
**
*/
	uu_denter(UU_RTRC,(us,"ur_recover_part, recover Unibase"));
	status = 0;

	/* destructive -- better make sure. */
	if(!ud_lyesno(UU_UBASE,10))
	{
		uu_dexit;
		return 0;
	}
	UM_init_f77_str(fname, name, UX_MAX_PATH_LEN);
	nci = strlen(name);
	status = loadu(UM_addr_of_f77_str(fname),&nci,&batch,&ierr);
	uu_dexit;
	return(status)	;
}
