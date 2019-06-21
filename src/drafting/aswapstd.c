
/*********************************************************************
**    NAME         : aswapstd.c
**       CONTAINS:
**					ua_swap_standard(pathname, flag)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       aswapstd.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:40
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "dasnog.h"
#include "dasg.h"
#include "uhep.h"
#include "ustdio.h"
#include "mdrel.h"
#include "adrf.h"
#include "adraft.h"

/*********************************************************************
**		E_FUNCTION:	ua_swap_standard(pathname, flag)
**
**		PURPOSE:
**    PARAMETERS   
**       INPUT  : 
**				pathname			new standard file name
**				flag				UU_TRUE:	user defined standard (prompt)
**									UU_FALSE:	default standard
**       OUTPUT :  
**    RETURNS      :	none
**    SIDE EFFECTS : none
**    WARNINGS     : All the prompts are taken from the application
**							prompt message file.
**
*********************************************************************/

ua_swap_standard(pathname, flag)
char						*pathname;
UU_LOGICAL				flag;
{
	struct UA_draft_rec entity;
	int		key;
	int	  	char_cnt;
	int		status;
	int		entnum;
	UU_LOGICAL	swap;
	char     fname[40];

	uu_denter(UU_MTRC,(us,"ua_swap_standard(standard = %s,flag = %d)",
				 pathname, flag));

	/* if user defined standard, prompt for the standards file name */

	strcpy(fname, pathname);
	if (flag == UU_TRUE)
		{
		ud_ldas(UD_DASSTRING,UA_DRAFTING,112,fname,40,&char_cnt,UD_NODEFAULT);
		/* check if user entered any text */
		if(char_cnt == 0) 
			{
			uu_dexit;
			return;
			}
		}


	/* check to see if any current drafting entities were created with a 
		different standard	*/
	status = 1;
	entnum = 0;
	swap = UU_TRUE;

	while (status >= 0)
		{
		entnum++;

		/* get the first drafting entity */
		status = ur_get_next_data_key(UA_LINEAR_DIMS_REL, &entnum, &entity.key);
		if (status >= 0)
			{
			ur_retrieve_data(&entity, sizeof(struct UA_draft_rec));
			uu_denter2(UU_MTRC,(us,"ua_swap_standard(entity.draf_ati[2] = %d)",
				 entity.draf_ati[2] ));
			uu_dexit;

			/* check to see if the standards will "clash" */
			if ((entity.draf_ati[2] == UA_ANSI && 
				 strcmp(fname,"ANSISTD.DAT") != 0) ||
				 (entity.draf_ati[2] == UA_DIN && 
				 strcmp(fname,"DINSTD.DAT") != 0) ||
				 (entity.draf_ati[2] == UA_ISO && 
				 strcmp(fname,"ISOSTD.DAT") != 0) ||
				 (entity.draf_ati[2] == UA_BSI && 
				 strcmp(fname,"BSISTD.DAT") != 0))
				 {
				 /* dont allow the swap to new standards */
				 swap = UU_FALSE;
				 break;
				 }
			/* also string compare in case of user defined standards */
			if ((entity.draf_ati[2] == UA_USER_DEF) && 
				(strcmp(fname,"ANSISTD.DAT") == 0 ||
				 strcmp(fname,"DINSTD.DAT")  == 0 ||
				 strcmp(fname,"ISOSTD.DAT")  == 0 ||
				 strcmp(fname,"BSISTD.DAT")  == 0 ))
				{	
				/* dont allow the swap to new standards */
				swap = UU_FALSE;
				break;
				}
			}
		}

	if (swap)
		{
		if(flag) 
			{
			ua_init_standard(fname, UU_FALSE);
			}
		else
			{
			ua_init_standard(fname, UU_TRUE);
			}
		ua_init_text();
		um_update_drwunits();
		}
	else
		{
		uu_uerror0(UA_DRAFTING, 34);
		/* current drafting entities and the new standards are inconsistent */
		}
uu_dexit;
}
