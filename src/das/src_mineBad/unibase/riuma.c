#define MODULEDBG 0
/*********************************************************************
**    NAME         :  riuma.c
**       CONTAINS:
**       ur_update_mtuple_attr()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       riuma.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:48
*********************************************************************/

#include	"usysdef.h"
#include	"riddle.h"
#include	"udebug.h"

/*********************************************************************
** I_FUNCTION : status = ur_update_mtuple_attr(key_id,attr_indx,rel_key)
**      update the tuple index table of a master tuple
**    PARAMETERS   
**       INPUT  : 
**			key_id, the identifier for the master to be updated
**			attr_indx,	the index into the tuple table to update
**			rel_key, a relation,entry tuple to be used for the update
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_update_mtuple_attr(key_id,attr_indx,rel_key)
UU_KEY_ID	key_id;		/* master tuple id								*/
int			attr_indx;	/* index into master tuple table				*/
UU_REL_ID 	rel_key;		/* key to be placed in the attr table		*/
{
	int						status;		/* return -1 if error, 0 otherwise		*/
	struct UR_MTID_rec	*mtuple_ptr;/* pointer to master tuple	*/
	int		rel_num;
	int		tuple_indx;

	uu_denter(UU_RITRC,(us,
			"ur_update_mtuple_attr(key= 0x%x, index= %d, with key= 0x%x)",
			key_id,attr_indx,rel_key));
	status = 0;

	/* get a pointer to the fixed data of the key_id */
	ur_k2rt(key_id, &rel_num, &tuple_indx);
	ur_get_tuple_ptr(rel_num, tuple_indx, &mtuple_ptr);
#if MODULEDBG != 0
	ur_dump_mtuple(key_id);
	uu_dprint(UU_RITRC,(us,"assocs at 0x%x", (*mtuple_ptr).assocs));
#endif

	/* put in the tuble table for this MTID */
	(*mtuple_ptr).assocs[attr_indx] = rel_key;
#if MODULEDBG != 0
	ur_dump_mtuple(key_id);
#endif
	ur_unibase_change(rel_num);
	uu_dexit;
	return(status);
}
