/*********************************************************************
**    NAME         :  m6edba.c
**       CONTAINS: routines to interface AG shells with UNIBASE
**			int um_agshell_setup_data(rel_num, shell, size)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m6edba.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:07
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "msol.h"
#include "mattr.h"

/*********************************************************************
**    E_FUNCTION     : int um_agshell_setup_data(rel_num, shell, size)
**       Initialize the data bag (SHELL) of the specified SIZE with
**			the format required by the given relation (REL_NUM). 
**			UNIBASE is called to setup the data bag, and appropriate
**			fields within the record are initialized to default values.
**    PARAMETERS   
**       INPUT  : 
**          rel_num						relation number 
**				size							size of data bag (bytes)
**       OUTPUT :  
**				shell							pointer to data bag to init
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agshell_setup_data(rel_num, shell, size)
	int rel_num;
	struct UM_agshell_rec *shell;
	int size;

	{
	int status;

	uu_denter(UU_MTRC, (us,"um_agshell_setup_data(rel_num=%d, size=%d)",
		rel_num, size));

	status = ur_setup_data(rel_num, shell, size);
	if (status != UU_SUCCESS) goto done;

	shell->key = -1;

	strcpy(shell->label, "");
	/* MILLS-  Initialize the SUBSCRIPT field. */
	shell->subscr = 0;
	shell->material = UM_mtrlmdl.index;
	shell->shelladdr = NULL;

done:;
	uu_dexitstatus("um_agshell_setup_data", status);
	return (status);
	}

