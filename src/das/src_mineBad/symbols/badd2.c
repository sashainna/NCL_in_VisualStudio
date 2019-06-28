/*********************************************************************
**    NAME:  badd2.c
**       CONTAINS:
**				ub_add2GeomList
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       badd2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:02
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) badd2.c 3.4 2/1/88 18:01:52 single"};
#else
static char uu_sccsident[]={"@(#) badd2.c 3.4 2/1/88 18:01:52 double"};
#endif
#endif

#include "usysdef.h"	 /* for UU_REAL, etc. */
#include "uhep.h"		 /* for error system */
#include "udebug.h"	  /* for debugging trace facility */
#include "mdrel.h"		/* for relation numbers */
#include "class.h"		/* for "UC_" data types */
#include "mattr.h"		/* for text attributes */
#include "mcrv.h"		 /* for text definition */
#include "mdcpln.h"	  /* for construction plane definition */
#include "bsym.h"	 
#include "dselmask.h"	 

#define TRACE UU_FALSE /* for debugging only */
/*********************************************************************
**    E_FUNCTION: int ub_add2GeomList(key, masterptr, instList, nbrOnListptr) 
**			This function determines if the entity corresponding to "key" is
**			allowed to be added to the master symbol pointed to by "masterptr".
**			If allowed then "key" is added to the master.
**    PARAMETERS   
**       INPUT: 
**				key				Key of the entity to be put in the master symbol.
**				masterptr		Pointer to the master symbol to be added to.
**       OUTPUT:  
**				masterptr		Pointer to the master symbol with possibly a new 
**									enity added to it.
**				instList			List of instances to be deleted (not used yet).
**				nbrOnListptr	Pointer to the number of keys on "instList" (not 
**									used yet).
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_add2GeomList(key, masterptr, instList, nbrOnListptr) 
	UU_KEY_ID key;
	struct UB_symbol_rec *masterptr;
	UU_KEY_ID instList[];
	int *nbrOnListptr;
{
	struct UC_entitydatabag ent;
	char rel_name[20];
	int i, rel_num, status = UU_SUCCESS;
	uu_denter(UU_BTRC, (us, "ub_add2GeomList(key:%d,masterptr:%x,?,?)",
								key,masterptr));

	if (ur_retrieve_data_relnum(key, &rel_num) != 0)
	{
		uu_uerror2(UB_SYMBOL,36, key, "ub_add2GeomList");
		/* error is: Error in retrieving relation type for eintity with key: %d
		 * (%s). */
		goto failed;
	}
	/* put the geometry key into the variable list */
	if (ubi_update_app_varlist(masterptr,UB_MGEOM_LIST,&key,
				masterptr->no_geom+1, 1) != UU_SUCCESS) goto failed;
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	
