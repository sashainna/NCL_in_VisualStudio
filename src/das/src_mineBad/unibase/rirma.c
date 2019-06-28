/*********************************************************************
**    NAME         :  rirma.c
**       CONTAINS:
**       ur_retrieve_master_attr()
**       uri_retrieve_master_attr()
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       rirma.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:47
*********************************************************************/

#include	"usysdef.h"
#include	"riddle.h"
#include	"rbase.h"
#include	"udebug.h"

/*********************************************************************
**    I_FUNCTION : status = ur_retrieve_master_attr(key_id,attr_indx,&rel_key)
**      retrieve a rel,ent key given a key_id and an attr table index
**    PARAMETERS   
**       INPUT  : 
**	       key_id, the identifier for the master to be retrieved from
**	       attr_indx,	the index into the attribute table of the key to
**                    retrieve
**       OUTPUT :  
**	       rel_key, relation,entry key
**    RETURNS      : 0 if successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

ur_retrieve_master_attr(key_id,attr_indx,rel_key)
UU_KEY_ID	key_id	;	/* master tuple index						*/
int			attr_indx;	/* index into master attributes table		*/
UU_REL_ID	*rel_key	;	/* a relation,entry tuple						*/
{
	int						status;		/* return status						*/
	struct UR_MTID_rec	*m_ptr;		/* ptr to relation control block*/
	UR_REL_NUM				rel;
	UR_TUPLE_INDX			tuple;

	uu_denter(UU_RTRC,(us,"ur_retrieve_master_attr(key=0x%x, index=%d)",
					key_id,attr_indx));
	/* get master tuple as relation,entry, and a pointer to the fixed data */
	ur_k2rt(key_id, &rel, &tuple);
	status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
	if(status == 0)
	{
		/* retrieve key from the mtuple table */
		if (m_ptr->no_assocs > 0)
			*rel_key = m_ptr->assocs[attr_indx];
		else
			status = UU_FAILURE;
	}
	uu_dexit;
	return(status);
}


/*********************************************************************
**    I_FUNCTION : status = uri_retrieve_master_attr(rel, tuple,
**										attr_indx, &rel_key)
**      retrieve a rel,ent key given rel and tuple of master and assoc index
**    PARAMETERS   
**       INPUT  : 
**			rel,			master tuple relation
**			tuple			master tuple index
**			attr_indx,	the index into the attribute table of the key to
**                    retrieve
**       OUTPUT :  
**	       rel_key, relation,entry key
**    RETURNS      : 0 if successful, -1 otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uri_retrieve_master_attr(rel, tuple,attr_indx,rel_key)
UR_REL_NUM				rel;
UR_TUPLE_INDX			tuple;
int			attr_indx;	/* index into master attributes table		*/
UU_REL_ID	*rel_key	;	/* a relation,entry tuple						*/
{
	int						status;		/* return status						*/
	struct UR_MTID_rec	*m_ptr;		/* ptr to relation control block*/

	uu_denter(UU_RTRC,(us,"uri_retrieve_master_attr(rel=%d, tuple=%d, index=%d)",
					rel, tuple, attr_indx));
	/* get master tuple pointer to the fixed data */
	status = ur_get_tuple_ptr(rel, tuple, &m_ptr);
	if(status == 0)
	{
		/* retrieve key from the mtuple table */
		if (m_ptr->no_assocs > 0)
			*rel_key = m_ptr->assocs[attr_indx];
		else
			status = UU_FAILURE;
	}
	uu_dexit;
	return(status);
}
