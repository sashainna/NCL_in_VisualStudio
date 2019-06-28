
/*********************************************************************
**    NAME         :  resavora.c
**       CONTAINS:
**       ur_save_oracle() (and stub version)
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reorasav.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:33
*********************************************************************/

#include "udebug.h"
#include "uhep.h"
#include "rerrdef.h"

#ifdef UR_ORACLE

#include "usysdef.h"
#include "dasnog.h"
UU_LOGICAL	ud_lyesno();
UU_LOGICAL	ur_unibase_used();

/*********************************************************************
**    E_FUNCTION     :  ur_save_oracle()
**       transcribe a part out to oracle database
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_save_oracle()
{
	extern   UU_LOGICAL  UR_sav_all; /* boolean, UU_TRUE if save all     */

	char			dbname[16];			/* database name for oracle */
	char			partnm[16];			/* part name to save as */
	UU_LOGICAL	keepold;				/* old or new priority flag */
	int			status;				/* status of transcription */
	int			length;				/* DAS communication */
/*--------------------------------------------------------------------
** Start of Executable Code
**--------------------------------------------------------------------
*/
	uu_denter(UU_RTRC,(us,"ur_save_oracle"));
	if(!ur_unibase_used())	/* if data base unused, tell'em and exit */
	{
		uu_uerror0(UU_UBASE, 1);
		uu_dexit;
		return(0);
	}
	/* ?should prompt for database name? */
	strcpy(dbname, "unicad/oracle");

si_1:
	/* prompt for part name */
	length = sizeof(partnm);
	uu_dprint(UU_RITRC,(us,"ur_save_oracle get part name"));
	ud_ldas(UD_DASSTRING, UU_UBASE, 1, partnm, length, &length, UD_NODEFAULT);
	uu_dprint(UU_RITRC,(us,"ur_save_oracle part name=%s", partnm));
	UR_sav_all = UU_TRUE;


	/* try to save with old priority */
	keepold = UU_TRUE;
	status = ur_trans_out_oracle(dbname, partnm, keepold);
	if (status != 0)
	{
		if (status != UR_PRTXISTS)
		{
			/* real error of some sort */
			ur_report_error(status);
			uu_dexit;
			return(status);
		}
		/* tell user part exists, Y to delete */
		if(ud_lyesno(UU_UBASE, 2))
		{
			keepold = UU_FALSE;
			status = ur_trans_out_oracle(dbname, partnm, keepold);
			/* if status still nfg then barf? */
		}
		else
			goto si_1;
	}
	uu_dexit;
	return(0);
}

#else
/* stub version for ORACLEless machines */
ur_save_oracle()
{
	uu_denter(UU_RTRC,(us,"ur_save_oracle-----(not available)"));
	uu_uerror0(UU_UBASE,URM_NO_RDBMS);
	uu_dexit;
	return(0);
}
#endif
