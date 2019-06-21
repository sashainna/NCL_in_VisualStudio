/*********************************************************************
**    NAME         :  nuwrap.c
**       CONTAINS:
**			nclu_wrapup()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nuwrap.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:09:18
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "nclfc.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "mfort.h"
#include "xenv1.h"
#include "lcom.h"


/*********************************************************************
**    E_FUNCTION     : nclu_wrapup()
**       Terminate current session.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void nclu_wrapup()
	{
	int numint, retstat;			/* number of das entries returned */
	int i;
	char str[UX_MAX_FILE_LEN];
	UM_f77_str name;
	UU_LOGICAL answer;
	UU_LOGICAL ncl_get_edit_option();
	UU_LOGICAL ud_yesno();

	uu_denter( UU_MTRC,(us,"nclu_wrapup()"));

/*	ncl_init_cmdbuf(&cmdbuf);
	ncl_add_token(&cmdbuf, NCL_quit, NCL_nocomma);
	ncl_add_cmdbuf(&cmdbuf);
	edit_mode = ncl_get_edit_option();
	if(edit_mode) ncl_edit_option();
	ncl_call(&cmdbuf);
	if(edit_mode) ncl_edit_option();
*/
	answer = ud_yesno(0, "Save part program file?", "Save part program file?");
	if(answer)
		{
		UM_init_f77_str(name, str, UX_MAX_FILE_LEN);
		gpgmnm(UM_addr_of_f77_str(name),&i);

		str[i] = '\0';
		if(strcmp(str, "none") != 0)
			{
			str[i] = '\0';
			}
		else
			{
			str[0] = '\0';
			}

		ud_string_def(UA_NCL, 427, str, UL_line_len, &numint, &retstat);
		if(numint > 0)
			{
      			i = strlen(str);
/*      			for (j=i; j<80; j++) str[j] = ' ';*/
			ptppnm(UM_addr_of_f77_str(name),&i);
			}
		}

/*	nclfin();   call later to allow reject op to work. */

	uu_dexit;
	}
