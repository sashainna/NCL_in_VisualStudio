
/*********************************************************************
**    NAME         :  qinit.c
**       CONTAINS:
**    		uq_init_calc_rel
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       qinitrel.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:04:54
*********************************************************************/
#include		"usysdef.h"
#include		"udebug.h"
#include		"calcom.h"
#include		"mdrel.h"


/*********************************************************************
**    I_FUNCTION :  uq_init_calc_rel()
**       initialize calculator's relation
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uq_init_calc_rel()

{
	int	atom_size[2], list_size[2];
	int	status;

	atom_size[0] = sizeof(UQ_func);
	list_size[0] = 1;
	status = ur_init_data_rel(UQ_CALC_REL, "qstb", 25,
			  sizeof(UQ_qstb)-UQ_QSTB_BUFSZ*sizeof(char), 1, atom_size,list_size); 
	/*
	status = ur_init_rel(UQ_CALC_REL, "qstb", 25,
			  sizeof(UQ_qstb)-UQ_QSTB_BUFSZ*sizeof(char), 1, atom_size,list_size); 
	*/
	return (status);
}	/* uq_init_calc_rel */


