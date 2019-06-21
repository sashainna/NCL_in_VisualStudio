
/*********************************************************************
**    NAME         :  nuedits.c
**       CONTAINS:
**			nclu_cmd_edit
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nuedits.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:09:07
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "dmark.h"
#include "nccs.h"
#include "nclcmd.h"
#include "nclinp.h"
#include "nkeywd.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     : ncl_cmd_edit(form)
**       Process EDIT command from CONTROL menu.
**    PARAMETERS   
**       INPUT  : 
**          form						Menu choice
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
nclu_cmd_edit(form)
	int form;
	{
	int status;
	int choice;
	UU_LOGICAL wind, answer;
	NCL_cmdbuf cmdbuf;
	int markval;

	uu_denter( UU_MTRC,(us,"ncl_cmd_edit()"));

	status = NCL_OKINPUT;
	wind = UU_FALSE;
	UD_MARK(markval, UU_FALSE);
	ncl_init_cmdbuf(&cmdbuf);
	if(markval == 0)
		{
		switch(form)
			{
			case 1:   /* delete */
				status = ncl_add_token(&cmdbuf, NCL_src_delete, NCL_nocomma);
				status = ncl_add_str(&cmdbuf, 213, NCL_nocomma);
				break;
			case 2:   /* find */
				status = ncl_add_token(&cmdbuf, NCL_src_find, NCL_nocomma);
				status = ncl_add_str(&cmdbuf, 214, NCL_nocomma);
				wind = UU_TRUE;
				break;
			case 3:   /* findtk */
				status = ncl_add_token(&cmdbuf, NCL_src_findtk, NCL_nocomma);
				status = ncl_add_str(&cmdbuf, 215, NCL_nocomma);
				wind = UU_TRUE;
				break;
			case 4:   /* input */
				status = ncl_add_token(&cmdbuf, NCL_src_input, NCL_nocomma);
				ncl_add_cmdbuf(&cmdbuf);
				while(status != NCL_NOINPUT)
					{
					status = ncl_add_str(&cmdbuf, 401, NCL_nocomma);
					if(status == NCL_OKINPUT)
						{
						ncl_add_cmdbuf(&cmdbuf);
						}
					else
						{
						ncl_call(&cmdbuf);
						status = NCL_NOINPUT;
						}
					}
				break;
			case 5:		/* insert */
				status = ncl_add_token(&cmdbuf, NCL_src_insert, NCL_nocomma);
				break;
			case 6:		/* consol. kathy */
				status = ncl_add_token(&cmdbuf, NCL_src_consol, NCL_nocomma);
				break;
			case 7:   /* skip */
				status = ncl_popup(NCL_CMD_SKIP, &choice);
				if(status != NCL_OKINPUT)
					{
					status = NCL_NOINPUT;
					break;
					}
				else
					{
					status = ncl_add_token(&cmdbuf, NCL_src_skip, NCL_nocomma);
					switch(choice)
						{
						case 1:
							status = ncl_add_token(&cmdbuf, "-1", NCL_nocomma);
							break;

						case 2:
							status = ncl_add_token(&cmdbuf, "1", NCL_nocomma);
							break;
	
						case 3:
							status = ncl_add_str(&cmdbuf, 210, NCL_nocomma);
							break;

						case 4:
							status = ncl_add_token(&cmdbuf, NCL_to, NCL_comma);
							status = ncl_add_str(&cmdbuf, 203, NCL_nocomma);
							break;

						case 5:
							status = ncl_add_token(&cmdbuf, NCL_to, NCL_comma);
							status = ncl_add_token(&cmdbuf, NCL_end, NCL_nocomma);
							break;

						default:
							status = NCL_NOINPUT;
							break;
						}
					break;
					}
			case 8:   /* edit */
				status = ncl_add_token(&cmdbuf, NCL_src_edit, NCL_nocomma);
				status = ncl_add_str(&cmdbuf, 203, NCL_nocomma);
				break;

			default:
				status = NCL_NOINPUT;
			}

		if(status == NCL_OKINPUT || status == NCL_DONE )
			{
			ncl_add_cmdbuf(&cmdbuf);
			if(wind)	opnwin();
			ncl_call(&cmdbuf);
			if(wind)
				{
				answer = ud_hakt(10, 1);
				clswin();
				}
			}
		}
	if(wind)
		{
		clswin();
		wind = UU_FALSE;
		}
	UD_UNMARK(markval);
loop:;
	uu_dexit;
	}
