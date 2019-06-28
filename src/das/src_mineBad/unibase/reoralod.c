/*********************************************************************
**    NAME         :  reoralod.c
**       CONTAINS:
**       ur_load_oracle() (and stub version)
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       reoralod.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:32
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
**    E_FUNCTION     :  ur_load_oracle()
**       transcribe a part in from oracle database
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ur_load_oracle(merge)
int	merge;	/* 0-load, 1-merge, -1-ask */
{
	char			dbname[16];			/* database name for oracle */
	char			partnm[16];			/* part name to save as */
	int			status;				/* status of transcription */
	int			length;				/* DAS communication */
	UU_LOGICAL	del_existing;		/* delete existing Unibase geom? */
/*--------------------------------------------------------------------
**    Start of Executable Code
**--------------------------------------------------------------------
*/
	uu_denter(UU_RTRC,(us,"ur_load_oracle"));
	del_existing = UU_TRUE;
	if(merge)
	{
		del_existing = UU_FALSE;
	}
	if(ur_unibase_used() && merge == -1)
	{
		/* enter Y to remove existing geom */
		del_existing = ud_lyesno(UU_UBASE,3);
	}
	strcpy(dbname, "unicad/oracle");

	/* prompt for part name */
	length = sizeof(partnm);
	ud_ldas(UD_DASSTRING, UU_UBASE, 5, partnm, length, &length, UD_NODEFAULT);
	uu_dprint(UU_RITRC,(us,"ur_load_oracle part name=%s", partnm));

	/* delete existing first if we're supposed to */
	if (del_existing)
	{
		uv_set_defered_mode();	/* allow block erase if appropriate */
		uu_unireset();
		uv_set_immediate_mode();
	}

	/* load the part */
	um_pre_load(del_existing);
	status = ur_trans_in_oracle(dbname, partnm);
	if (status != 0)
	{
		/* real error of some sort */
		switch(status)
		{
		case UR_PRTXISTS:
			break;
		case URM_RELNTFND:
			uu_uerror0(UU_UBASE,18);
			break;
		case URM_NEXIST_VARL:
			uu_uerror0(UU_UBASE,14);
			break;
		case URM_MT_PART:
		case UR_UU:
			break;
		case URM_NO_PART:
			uu_uerror0(UU_UBASE,19);
			break;
		case URM_RDBMS_ERR:
		default:
			uu_uerror0(UU_UBASE,20);
			break;
		}
		uu_dexit;
		return(status);
	}
	/* do display of new data after load */
	um_post_load(del_existing);
	ur_lp04();
	uu_dexit;
	return(0);
}

#else
/* stub for ORACLEless machines */
ur_load_oracle()
{
	uu_denter(UU_RTRC,(us,"ur_load_oracle-----(not available)"));
	uu_uerror0(UU_UBASE,URM_NO_RDBMS);
	uu_dexit;
	return(0);
}
#endif
