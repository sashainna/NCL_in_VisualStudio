/*********************************************************************
**    NAME         :  ridma.c
**       CONTAINS:
**       ur_del_mtuple_attr()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       ridma.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:42
*********************************************************************/

#include	"usysdef.h"
#include	"riddle.h"
#include	"udebug.h"

/*********************************************************************
**    I_FUNCTION     :  status = ur_del_mtuple_attr(key_id,attr_indx)
**      delete an attribute from the master tuple 
**    PARAMETERS   
**       INPUT  : 
**				key_id, the identifier for the master to be updated
**				attr_indx,	the index into the tuple table of the entry to 
**								be deleted
**       OUTPUT :  
**          none
**    RETURNS      :  0 if function successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_del_mtuple_attr(key_id,attr_indx)
UU_KEY_ID	key_id;					/* master tuple index						*/
int			attr_indx;				/* index into master tuple table			*/
{
	int			status;				/* return status							*/
	struct UR_MTID_rec *m_ptr;	/* ptr to master tuple					*/
	UU_REL_ID	rel_key;				/* attribute of relation,tuple		*/
	int			rel_num;
	int			tuple_indx;

	uu_denter(UU_RTRC,(us,"ur_del_mtuple_attr(key= 0x%x, index= %d)",
					key_id,attr_indx));
	status 	=	0;

	/* get master relation & entry, and a pointer to the master fixed data */
	ur_k2rt(key_id, &rel_num, &tuple_indx);
	ur_get_tuple_ptr(rel_num, tuple_indx, &m_ptr);	

	/* retrieve the tuple id at tuple index, and convert to relation,entry */
	rel_key = (*m_ptr).assocs[attr_indx];
	if(rel_key != 0)
	{

		/* now go delete the relation,entry */
		status = ur_delete_tuple(rel_key);

		/* delete the entry the the key_id tuple table by setting = 0 */
		(*m_ptr).assocs[attr_indx] = 0;
	}
	uu_dexit;
	return(status);
}
