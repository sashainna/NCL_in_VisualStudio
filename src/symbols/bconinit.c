/*********************************************************************
**    NAME:  bconinit.c
**       CONTAINS: 
**    	int ub_con_init_relations()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       bconinit.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:02
*********************************************************************/
#include "usysdef.h"
#include "uhep.h"
#include "udebug.h"
#include "mdrel.h"
#include "mdcoord.h"	/* for UM_DEFAULT_TF */
#include "mattr.h"	/* for UM_transf_rec */
#include "class.h"	/* for UC_attributedatabag */
#include "bsym.h"

/*********************************************************************
**    E_FUNCTION :  int ub_con_init_relations()
**       Initialize FRAMATOME system relations.
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS: UU_SUCCESS if (apparently) successful, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_con_init_relations()
{
	int atom_size[2];/* sizes of atoms for variable lists */
	int list_size[2];/* expansion factor for each list */
	int fixed_size;
	int i, status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ub_con_init_relations()"));

	list_size[0] = 50;
	atom_size[0] = sizeof(UU_KEY_ID);
	for (i=1; i<2; i++) 
	{ 
		list_size[i] = 0;
		atom_size[i] = 0;
	}
	fixed_size = sizeof(struct UB_conector_rec)-sizeof(char)*UB_CONECTOR_BUFSZ;
	/* in the next call, 10=expansion factor, 1= number of variable lists */
	if (um_init_rel(UB_CONECTOR_REL, "conector", 10, fixed_size, 1, atom_size, 
						list_size) != UU_SUCCESS) goto failed;
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}
