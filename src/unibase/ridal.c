#define MODULEDBG 1
/*********************************************************************
**    NAME         :  ridal.c
**       CONTAINS:
**       uri_del_assoc_list()
**    COPYRIGHT 1987 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ridal.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:42
*********************************************************************/
#ifdef UU_DEBUG
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) ridal.c 3.1 11/2/87 21:23:47 single"};
#else
static char uu_sccsident[]={"@(#) ridal.c 3.1 11/2/87 21:23:47 double"};
#endif
#endif

#include "usysdef.h"
#include "udebug.h"
#include "rbase.h"
#include "rmtuple.h"
#include "riddle.h"

/*********************************************************************
**    I_FUNCTION :  uri_del_assoc_list(key, assoc_key)
**       deletes the key for the association from the list of
**			associations for the given key
**    PARAMETERS   
**		INPUT  : 
**			key			UU_KEY_ID	key whose association list to modify
**			assoc_key	UU_KEY_ID	key of association being deleted from list
**		OUTPUT:		none
**    RETURNS:		0 on success -1 otherwise(always succeeds for good parms)
**    SIDE EFFECTS:	key added to list
**    WARNINGS:		none
*********************************************************************/

uri_del_assoc_list(key, assoc_key)
UU_KEY_ID		key;			/* key whose assoc list to modify */
UU_KEY_ID		assoc_key;	/* key of association being deleted */
{
	int						indx;				/* assoc list index */
	UR_REL_NUM				rel;				/* number identifying the rel	*/
	UR_TUPLE_INDX			tuple;			/* index into the relation */
	struct UR_MTID_rec	*mtuple_ptr;	/* pointer to a master tuple */
	int						status;			/* return status */

	uu_denter(UU_RTRC, (us, "uri_del_assoc_list(key=0x%x, assoc_key=0x%x)",
				key, assoc_key));
	status = 0;
	if (key)
	{
		ur_k2rt(key, &rel, &tuple);

		/* can only keep assoc list if key is for a master tuple */
		if(rel == UR_MTUPLE_REL)
		{
			/* find the master tuple involved */
			ur_get_tuple_ptr(rel, tuple, &mtuple_ptr);
	
			for (indx = 0; indx < mtuple_ptr->no_assocs; indx++) /* entire list: */
			{
				/* find an instance of the target key in the list */
				if (mtuple_ptr->assocs[indx] == assoc_key)
				{
					/* if atom found is greater than fixed portion of list */
					if (indx > UR_MAX_INDX)
					{
						/* delete atom found from the varlist */
						uri_delete_assoclist_atoms(key, 1, indx + 1, 1);
					}
					else
					{
						/* else set key to zero and decrement approp use count?? */
						mtuple_ptr->assocs[indx] = 0;
					}
				}
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

