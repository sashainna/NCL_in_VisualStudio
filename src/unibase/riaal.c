#define MODULEDBG 1
/*********************************************************************
**    NAME         :  name
**       CONTAINS:
**       names of functions in file
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       riaal.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:41
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) riaal.c 3.2 8/25/88 13:42:16 single"};
#else
static char uu_sccsident[]={"@(#) riaal.c 3.2 8/25/88 13:42:16 double"};
#endif
#endif

#include "riddle.h"
#include "rbase.h"
#include "rmtuple.h"
#include "udebug.h"

/*********************************************************************
**    E_FUNCTION :  uri_add_assoc_list(key, assoc_key)
**       adds the key for the association to the list of associations for
**			the given key
**    PARAMETERS   
**       INPUT  : 
**          key			UU_KEY_ID	key whose association list to modify
**				assoc_key	UU_KEY_ID	key of association being added to list
**       OUTPUT :  
**          none
**    RETURNS      : 0 on success -1 otherwise(always succeeds for good parms)
**    SIDE EFFECTS : key added to list
**    WARNINGS     : none
*********************************************************************/
uri_add_assoc_list(key, assoc_key)
UU_KEY_ID		key;				/* key whose assoc list to modify */
UU_KEY_ID		assoc_key;		/* key of association being added */
{
	UR_REL_NUM				rel;				/* number identifying the rel	*/
	UR_TUPLE_INDX			tuple;			/* index into the relation */
	struct UR_MTID_rec	*mtuple_ptr;	/* pointer to a master tuple */
	int						status;			/* return status */

	uu_denter(UU_RTRC,(us,"uri_add_assoc_list(key=0x%x, assoc_key=0x%x)",
					key, assoc_key));
	status = 0;
	if (key)		/* only non zero keys */
	{
		ur_k2rt(key, &rel, &tuple);

		/* can only keep assoc list if key is for a master tuple */
		if(rel == UR_MTUPLE_REL)
		{
			/* find the master tuple involved */
			status = ur_get_tuple_ptr(rel, tuple, &mtuple_ptr);
	
			if (status == 0)
			{
				/* add assoc key as last atom in the assoc varlist */
				ur_update_tuple_varlist(rel, tuple, 1, mtuple_ptr->no_assocs + 1,
							1, &assoc_key);
			}
		} 
		else	/* not for master tuple -- error */
		{
			status = -1;
		}
	}
#if MODULEDBG != 0
	ur_dump_mtuple(key);
#endif
	uu_dexit;
	return(status);
}


