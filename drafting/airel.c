/*********************************************************************
**    NAME         :  airel.c
**       CONTAINS:
**       Temp interface from DRAFTING to unibase.
**				 ua_init_relations ()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       airel.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:35
*********************************************************************/

#include "usysdef.h"
#include "udebug.h"
#include "mdrel.h"
#include "adraft.h"
#include "adrf.h"

#define VARLISTS 6

#define INITSIZE 31
#define UA_NE_HATCHLINE 20	/* initial # of entries in rel */
#define UA_NV_HATCHLINE 1	/* # of varlists in UA_hatchlin_rec */
#define UA_LS_HATCHLINE 200 /* initial varlist size */

/*********************************************************************
**    E_FUNCTION :  ua_init_relations ()
**       Initialize the DRAFTING subsystem relations.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ua_init_relations()

{
	int  status,fixed_size;

	static int list_sizes[VARLISTS];
	static int atom_sizes[VARLISTS];

	uu_denter(UU_MTRC,(us,"ua_init_relations()"));
	/*	initialize the DRAFTING entity relations
	 */

	atom_sizes[0] = sizeof(struct UA_txtblk_rec);
	list_sizes[0] = 10;
	atom_sizes[1] = sizeof(struct UA_arcblk_rec);
	list_sizes[1] = 5;
	atom_sizes[2] = sizeof(struct UA_lineblk_rec);
	list_sizes[2] = 5;
	atom_sizes[3] = sizeof(struct UA_arrowblk_rec);
	list_sizes[3] = 10;
	atom_sizes[4] = sizeof(struct UA_assoblk_rec);
	list_sizes[4] = 50;
	fixed_size = sizeof(struct UA_draft_rec) -
					sizeof(char) * UA_DRAFT_BUFSZ;

	/* drafting relation	*/
	status = ur_init_rel(UA_LINEAR_DIMS_REL, "draft", INITSIZE, 
				fixed_size , 5, atom_sizes, list_sizes);


	/* now initialize UA_HATCHING_REL */

	list_sizes[0] = UA_LS_HATCHLINE;
	atom_sizes[0] = sizeof(UU_REAL);
	fixed_size = sizeof(struct UA_hatchlin_rec) - sizeof(char) *
		UA_HATCHLIN_BUFSZ;
 status = ur_init_rel(UA_HATCHING_REL, "hatchline", UA_NE_HATCHLINE,
	fixed_size, UA_NV_HATCHLINE, atom_sizes, list_sizes);

	uu_dexit;

}

