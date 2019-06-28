/*********************************************************************
**    NAME:  bxmanip.c
**       CONTAINS:
**    		ub_delete_symbol(key) 
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       bxmanip.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:07
*********************************************************************/
#include "usysdef.h"	/* for UU_REAL, etc. */
#include "uhep.h"    /* for error system */
#include "udebug.h"	/* for debugging trace facility */
#include "class.h"	/* for "UC_" data types */
#include "mdcoord.h" /* for UM_vector, etc */
#include "mdattr.h"	/* for definition of colors */
#include "mdrel.h"	/* for define relation numbers */
#include "bsym.h"

#define TRACE UU_FALSE	/* for debugging only */
/*********************************************************************
**    E_FUNCTION : int ub_delete_symbol(key) 
**       This function determines whether a master symbol or a symbol instance
**			is to be deleted and calls the appropriate function.
**    PARAMETERS   
**       INPUT  : 
**         key				UNIBASE key to the symbol entity to be deleted. 
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_delete_symbol(key)
	UU_KEY_ID key;
{
	int rel_num;
	int status;			/* return status; either UU_SUCCESS or UB_FAILURE */

	uu_denter(UU_BTRC, (us, "ub_delete_symbol(key:%d)", key));
	status = UU_SUCCESS; /* assume success */
	if (ur_retrieve_data_relnum(key, &rel_num) != 0)
		goto failed;
	switch(rel_num)
	{
		case UB_INSTANCE_REL:
			if (ub_del_syminstance(key) != UU_SUCCESS)
				goto failed;
			break;
		case UB_SYMBOL_REL:
			if (ubu_del_symmaster(key, UU_FALSE, UU_TRUE) != UU_SUCCESS)
				goto failed;
			break;
		default:
			uu_uerror3(UB_SYMBOL, 36, rel_num, key, "ub_delete_symbol");
			/* error message: Unknown relation type: %d, for key = %d (%s).  */
			goto failed;
			break;
	}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	
