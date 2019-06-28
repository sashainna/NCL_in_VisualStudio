/*********************************************************************
**    NAME:  bxinit.c
**       CONTAINS:
**				int ub_init_msym_rel()
**				int ub_init_instance_rel()
**    COPYRIGHT 1988 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       bxinit.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:07
*********************************************************************/
#include "usysdef.h"		/* for UU_REAL, etc. */
#include "udebug.h"		/* for debugging trace facility */
#include "mdrel.h"		/* for relation numbers */
#include "xfsys1.h"
#include "bsym.h"			/* declares symbol storage and definitions */

#define TRACE UU_FALSE	/* for debugging only */
/*********************************************************************
**    E_FUNCTION :  int ub_init_msym_rel()
**       Initialize master symbol relations.
**    PARAMETERS   
**       INPUT  : none.
**       OUTPUT : none.
**    RETURNS: UU_SUCCESS if (apparently) successful, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_init_msym_rel()
{
	int atom_size[UB_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int list_size[UB_MAX_NBR_VAR_LISTS];/* expansion factor for each list */
	int fixed_size;
	int status = UU_SUCCESS, i;
	uu_denter(UU_BTRC,(us,"ub_init_msym_rel()"));

	list_size[0] = UB_MAX_MASTERS;
	atom_size[0] = sizeof(struct UB_masters_rec);
	list_size[1] = UB_MAX_INSTANCES;
	atom_size[1] = sizeof(struct UB_inst_rec);
	list_size[2] = UB_MAX_SYM_GEOM;
	atom_size[2] = sizeof(UU_KEY_ID);
	list_size[3] = UB_MAX_TEXT_NODES;
	atom_size[3] = sizeof(struct UB_text_nod_rec);
	list_size[4] = UB_MAX_SNAP_NODES;
	atom_size[4] = sizeof(struct UB_snap_nod_rec);
	for (i=5; i<UB_MAX_NBR_VAR_LISTS; i++) 
	{ 
		list_size[i] = 0;
		atom_size[i] = 0;
	}
	fixed_size = sizeof(struct UB_symbol_rec) - sizeof(char) * UB_SYMBOL_BUFSZ;
	/* in the next call, 10=expansion factor, 5= number of variable lists */
	um_init_rel(UB_SYMBOL_REL,"symbol",10,fixed_size, 5, atom_size, list_size);

	goto done;
failed: status = UB_FAILURE;	
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int ub_init_instance_rel()
**			This function initializes symbol instances.
**    PARAMETERS   
**       INPUT: none.
**       OUTPUT: none. 
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_init_instance_rel()
{
	int atom_size[UB_MAX_NBR_VAR_LISTS];/* sizes of atoms for variable lists */
	int list_size[UB_MAX_NBR_VAR_LISTS];/* expansion factor for each list */
	int fixed_size;
	int status = UU_SUCCESS, i;
	uu_denter(UU_BTRC,(us,"ub_init_instance_rel()"));

	list_size[0] = UB_MAX_SYM_GEOM;
	atom_size[0] = sizeof(UU_KEY_ID);
	list_size[1] = UB_MAX_TEXT_NODES;
	atom_size[1] = sizeof(struct UB_text_nod_rec);
	list_size[2] = UB_MAX_SNAP_NODES;
	atom_size[2] = sizeof(struct UB_snap_nod_rec);
	for (i=3; i<UB_MAX_NBR_VAR_LISTS; i++) 
	{ 
		list_size[i] = 0;
		atom_size[i] = 0;
	}
	fixed_size = sizeof(struct UB_instance_rec)-sizeof(char)*UB_INSTANCE_BUFSZ;
	/* in the next call, 10=expansion factor, 4= number of variable lists */
	um_init_rel(UB_INSTANCE_REL,"instance", 10, fixed_size, 3, atom_size, 
						list_size);

	goto done;
failed: status = UB_FAILURE;	
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}

