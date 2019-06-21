
/*********************************************************************
**    NAME         :  m2eirel
**       CONTAINS: routines to initialize modeling relations
**			int um_init_rel (rel_num, rel_name,
**			um_init_transf(rel_num, rel_name, expansion_factor) 
**			um_init_attr(rel_num, rel_name, expansion_factor) 
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2eirel.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:46
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "mdrel.h"

/*********************************************************************
**    E_FUNCTION : int um_init_rel (rel_num, rel_name,
**												expansion_factor, nbr_varlists, 
**												atom_size, list_size)
**       Interface to UNIBASE initialization routine for initializing 
**			master tuple relations.
**    PARAMETERS   
**       INPUT  : 
**          rel_num				Number of the relation to be initialized.
**				rel_name				Name for the relation to be initialized.
**				expansion_factor	Initial number of entries expected in this 
**										relation = number of entries to expand table if
**										relation becomes full.
**				fixed_size			The size of each entry in the relation.
**				nbr_varlists		Number of variable lists associated with this
**										relation.
**				atom_size			Array containing the atom size for each variable
**										list.
**				list_size			array containing the number of atoms expected on 
**										each variable list.
**       OUTPUT :  
**          none
**    RETURNS      : UU_SUCCESS if successful, UU_FAILURE otherwise.
**    SIDE EFFECTS : prints error message if relation can not be initialized.
**    WARNINGS     : none
*********************************************************************/
int
um_init_rel(rel_num, rel_name, expansion_factor, fixed_size,
				nbr_varlists, atom_size, list_size)
	int rel_num;
	char *rel_name;
	int expansion_factor;	
	int fixed_size;
	int nbr_varlists;
	int atom_size[];
	int list_size[];

	{
	int status;

	uu_denter(UU_MTRC, 
		(us,"um_init_rel(rel_num:%d,%.15s,exp_factor:%d,fixed_sz:%d,%d,%x,%x)",
		rel_num,rel_name,expansion_factor,fixed_size,nbr_varlists,
		atom_size,list_size));
	status = UU_SUCCESS;	/* assume success */
	if ( ur_init_rel(rel_num, rel_name, expansion_factor, fixed_size, 
				nbr_varlists, atom_size, list_size) != 0)
		{
		uu_uerror1(UM_MODEL, 168, rel_num);
		/* message is: um_init_rel: UNIBASE relation creation unsuccessful 
		 * for entity relation %d 
		 */
		status = UU_FAILURE;
		}
	uu_dexit;
	return(status);
	}
/*********************************************************************
**    E_FUNCTION : um_init_transf(rel_num, rel_name, expansion_factor) 
**       Interface to UNIBASE initialization routine for initializing 
**			transformation relations.
**    PARAMETERS   
**       INPUT  : 
**          rel_num				Relation number for the relation to be 
**										initialized.
**				rel_name				Relation name for the relation to be initialized.
**				expansion_factor	Initial number of entries expected in this 
**										relation = number of entries to expand table if
**										relation becomes full.
**				fixed_size			The size of each entry within the relation.
**       OUTPUT :  
**          output
**    RETURNS      : none
**			RETURNS      : UU_SUCCESS if no problems encountered, UU_FAILURE
**								otherwise.
**    SIDE EFFECTS : prints error message if relation can not be initialized.
**    WARNINGS     : none
*********************************************************************/
um_init_transf(rel_num, rel_name, expansion_factor, fixed_size)
	int rel_num;
	char *rel_name;
	int expansion_factor;	
	int fixed_size;

	{
	uu_denter(UU_MTRC, 
		(us,"um_init_transf(rel_num:%d,%.15s,exp_factor:%d,fixed_sz:%d,%d)",
		rel_num,rel_name,expansion_factor,fixed_size));

	if ( ur_init_transf(rel_num, rel_name, expansion_factor, fixed_size) != 0)
		uu_uerror1(UM_MODEL, 168, rel_num);
		/* message is: um_init_rel: UNIBASE relation creation unsuccessful 
		 * for entity relation %d 
		 */

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION : um_init_attr(rel_num, rel_name, expansion_factor) 
**       Interface to UNIBASE initialization routine for initializing 
**			attribute relations.
**    PARAMETERS   
**       INPUT  : 
**          rel_num				Relation number for the relation to be 
**										initialized.
**				rel_name				Relation name for the relation to be initialized.
**				expansion_factor	Initial number of entries expected in this 
**										relation = number of entries to expand table if
**										relation becomes full.
**				fixed_size			the size of each entry within the relation.
**       OUTPUT :  
**          output
**    RETURNS      : none
**			RETURNS      : UU_SUCCESS if no problems encountered, UU_FAILURE
**								otherwise.
**    SIDE EFFECTS : prints error message if relation can not be initialized.
**    WARNINGS     : none
*********************************************************************/
um_init_attr(rel_num, rel_name, expansion_factor, fixed_size)
	int rel_num;
	char *rel_name;
	int expansion_factor;	
	int fixed_size;

	{
	uu_denter(UU_MTRC, 
		(us,"um_init_attr(rel_num:%d,%.15s,exp_factor:%d,fixed_sz:%d,%d)",
		rel_num,rel_name,expansion_factor,fixed_size));

	if ( ur_init_attr(rel_num, rel_name, expansion_factor, fixed_size) != 0)
		uu_uerror1(UM_MODEL, 168, rel_num);
		/* message is: um_init_rel: UNIBASE relation creation unsuccessful 
		 * for entity relation %d 
		 */

	uu_dexit;
	}
