
/*********************************************************************
**    NAME         :  m6esh1.c
**       CONTAINS: AG shell support routines
**			int um_agshell_query(key, list)
**			int umi_agshell_query(shell, list)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m6esh1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:07
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "ulist.h"
#include "go.h"
#include "dasnog.h"
#include "class.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdebug.h"
#include "mdeval.h"
#include "mcrv.h"
#include "msrf.h"
#include "msol.h"
#include "modef.h"
#include "misect.h"
#include "mderror.h"
#include "mdpick.h"

#include "ag_incl.h"

/*********************************************************************
**    I_FUNCTION : int um_agshell_query(key, list)
**			This function produces the query output for an agshell.
**    PARAMETERS   
**       INPUT  : 
**       OUTPUT :  
**    RETURNS:
**			UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agshell_query(key, list)
	UU_KEY_ID	key;
	UU_LIST 	*list;

	{
	int status;
	struct UM_agshell_rec shell;
	struct UM_attrdata_rec attr;
	char	 buf[256];

	uu_denter(UU_MTRC,(us, "um_agshell_query(key=%d)", key));

	/* get the entity data */
	shell.key = key;
	status = uc_retrieve_data(&shell, sizeof(struct UM_agshell_rec));
	if (status != UU_SUCCESS) goto done;
	status = uc_retrieve_attr(key, &attr);
	if (status != UU_SUCCESS) goto done;

	/* print surface information */
	umi_agshell_query(&shell, list);

done:;
	uu_dexitstatus("um_agshell_query", status);
	return(status);
	}	

/*********************************************************************
**    I_FUNCTION : int umi_agshell_query(shell, list)
**			This function produces the query output for a shell (shell).
**    PARAMETERS   
**       INPUT  : 
**				shell					shell entity
**				list					list to put data onto
**       OUTPUT :  
**    RETURNS:
**			UU_SUCCESS if no problems encountered; UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umi_agshell_query(shell, list)
	struct UM_agshell_rec *shell;
	UU_LIST 	*list;

	{
	int status;
	char	 buf[256];
	struct UM_agsrf_rec srf;
	AG_SHELLP sh;
	AG_FACEP face;

	uu_denter(UU_MTRC,(us, "umi_agshell_query(key=%d)", shell->key));

	sh = (AG_SHELLP) shell->shelladdr;

	/* print surface specific data */
	uc_setup_data(UM_AGSRF_REL, &srf, sizeof(srf));
	face = sh->f;
	if (face != NULL) do
		{
		srf.srfaddr = (int) face->srf;
		umi_agsrf_query(&srf, list);
		face = face->next;
		}
	while ((face != sh->f) && (face != NULL));



	status = UU_SUCCESS;

done:;
	uu_dexitstatus("umi_agshell_query", status);
	return(status);
	}	
