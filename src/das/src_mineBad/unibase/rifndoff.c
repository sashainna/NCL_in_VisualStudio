#define MODULEDBG 0
/*********************************************************************
**    NAME         :  rifdnoff.c
**       CONTAINS:
**       ur_findoff()
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rifndoff.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:43
*********************************************************************/

#if MODULEDBG != 0
#include "udebug.h"
#endif
#include "riddldef.h"

/*********************************************************************
**    I_FUNCTION     :  int ur_findoff(def)
**       finds the offset to the next element of a structure.
**    PARAMETERS   
**       INPUT  : 
**          def	struct*	attribute definition for the element
**       OUTPUT :  
**          def	struct*	definition + offset field
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_findoff(def)
struct attr_def	*def;
{
	int	over;

#if MODULEDBG != 0
	uu_denter(UU_RTRC,(us,"ur_findoff"));
#endif
	/*  offset to the next structure element */
	/*  = size of atom * number of array elements rounded up to nearest */
	/*   multiple of ALIGNSZ. */
	def->attr_off = def->attr_size * def->num_rows * def->num_cols;
	over = def->attr_off % ALIGNSZ;	/* amount over the last align boundary */
	if (over)
		def->attr_off += ALIGNSZ - over;	/* bump to next boundary */
#if MODULEDBG != 0
	uu_dexit;
#endif
} /* ur_findoff() */
