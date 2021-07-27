/*********************************************************************
**    NAME         :  m6edraft.c
**       CONTAINS: routines to interface AG shell to drafting
**			int um_agshell_draft_type(rel_num, type)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m6edraft.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:07
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "class.h"
#include "mdrel.h"
#include "adrfcom.h"

/*********************************************************************
**    E_FUNCTION     : int um_agshell_draft_type(rel_num, type)
**       Determine the drafting class for an AG shell.
**    PARAMETERS   
**       INPUT  : 
**          rel_num						NCL entity relation number
**       OUTPUT :  
**          type                    drafting type
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : 
**			The only data which must be initialized in the entity 
**			are the UNIBASE key and relation number.
*********************************************************************/
int
um_agshell_draft_type(rel_num, type)
	int rel_num;
	int *type;

	{
	int status;

	uu_denter(UU_MTRC,(us,"um_agshell_draft_type(rel_num=%d)", rel_num));
	*type = UA_DRAFT_CURVE;
	uu_dexit;
	return(UU_SUCCESS);
	}
