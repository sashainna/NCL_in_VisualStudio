/*********************************************************************
**
**    NAME         :  d4pstat.c
**
**       CONTAINS:
**				int ud_pstat(stat, pname)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL
**       d4pstat.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:10
**
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "udfconst.h"

#define STATNO 29

/* -- status return type: must match typedef enum of 
		UD_FSTAT in udfconst.h -- */

char sfstat[STATNO][12] =
{
	"UD_FLDOK",							/* data entered and checked ok */
	"UD_FWD",							/* requested to go to next field */
	"UD_BAK",							/* requested to go to previous field */
	"UD_BADREQ",						/* requested type was invalid or undefined */
	"UD_ACTOK",							/* the form is successfully activated */
	"UD_BADACT",						/* init or activate error - data no good */
	"UD_VALOK"	,						/* data passed all checks */
	"UD_BADRNG",						/* data out of range */
	"UD_BADTYP",						/* data was not an allowed type */
	"UD_FDSYNC",						/* no of form fields != no of data fields */
	"UD_DFLTER",						/* the default display errored out */
	"UD_FILLOK",						/* form filled, all data present, chk'd */
	"UD_DONE"	,						/* "done" key hit, no oustanding mandatory */
	"UD_ALT"	,							/* the "user alternate action" key was hit */
	"UD_DFLTOK",						/* any default(s) properly displayed */
	"UD_PRSTOK",						/* all went well, the form is now displayed. */
	"UD_BADMSG",						/* there was a message diplay error */
	"UD_BADPRMT",						/* there was a prompt display error */
	"UD_BADMSK",						/* there was a mask display error */
	"UD_FRMOK"	,						/* form traversed ok, good data returned */
	"UD_BADFILL",						/* form data fill error, data no good */
	"UD_BADPRST",						/* form presentation error, data no good */
	"UD_XFROK",							/* data transfer was done successfully */
	"UD_BADXFR",						/* bad data type in data transfer call */
	"UD_OUTOK",							/* dummy output routine ok return */
	"UD_DATAOK",						/* data entered was not function key */
	"UD_PASSF",							/* no data was entered to field */
	"UD_TFWD",							/* toggle fwd was hit (choice fields only)*/
	"UD_TBAK"							/* toggle bck was hit (  "       "     " )*/
};

/*********************************************************************
**    I_FUNCTION :  int ud_pstat(stat, pname)
**			prints the character string translation of the
**			value of stat to the udebug trace file.
**
**    PARAMETERS   
**       INPUT  : 
**          stat = status to print
**				pname = procedure name callsed from
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ud_pstat(stat, pname)
UD_FSTAT stat;					/* status to print */
char *pname;					/* procedure name called from */
{
	uu_denter(UU_DTRC,(us,"ud_pstat: called from procedure %s", pname));

	if (((int)stat>STATNO) || ((int)stat<0))
	{
		uu_dprint(UU_DTRC,(us,"stat=unknown value:%x",
			stat));
	}
	else
	{
		uu_dprint(UU_DTRC,(us,"stat=%s",&(sfstat[(int)stat][0])));
	}

	uu_dexit;
	return;
}
