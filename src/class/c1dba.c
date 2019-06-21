/******************************************************************************
**	NAME: c1dba.c
**		CONTAINS: general interface routines to retrieve/update data associated
**						with master tuples
**		   int uc_retrieve_mtuple_data(eptr, size)
**			int uc_retrieve_mtuple_transf(key, tfmat)
**    	int uc_retrieve_mtuple_attr(key, attrptr)
**			int uc_create_mtuple_data(eptr, tfmat, attrptr)
**	COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**	MODULE NAME AND RELEASE LEVEL
**       c1dba.c , 25.1    
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:04:57
*******************************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "class.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdebug.h"


/*********************************************************************
**    E_FUNCTION     : int uc_retrieve_mtuple_data(eptr, size)
**       Retrieve the data associated with a master tuple key
**			(EPTR->KEY). The fixed data and ALL  variable length list
**			data for the given master tuple will be retrieved.
**    PARAMETERS   
**       INPUT  : 
**          eptr					pointer to entity structure into
**										which the data associated with the
**										master tuple with key = eptr->key
**										will be retrieved
**				size					size of above structure 
**       OUTPUT :  
**          eptr					all fixed data and variable length list
**										data 
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : 
**			BOTH eptr->key AND eptr->rel_num MUST BE INITIALIZED
*********************************************************************/
int
uc_retrieve_mtuple_data(eptr, size)
	struct UC_entitydatabag *eptr;
	int size;

	{
	int status;

	uu_denter(UU_MTRC,(us,"uc_retrieve_mtuple_data(key=%d, size=%d)",
		eptr->key, size));

	if (ur_retrieve_data(eptr, size) != 0) goto failure;

success:
	status = UU_SUCCESS;
	uu_dexit;
	return (status);

failure:
	status = UU_FAILURE;
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int uc_retrieve_mtuple_transf(key, tfmat)
**       Retrieve the transformation associated with the specified
**			master tuple (KEY).
**    PARAMETERS   
**       INPUT  : 
**          key						key of master tuple to retrieve
**											transformation of
**       OUTPUT :  
**          tfmat						transformation matrix
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_retrieve_mtuple_transf(key, tfmat)
	UU_KEY_ID key;
	UM_transf tfmat;

	{
	int status;
	struct UM_transf_rec transfpacket;
	char ebuf[256];

	uu_denter(UU_MTRC,(us,"uc_retrieve_mtuple_transf(key=%d, tfmat=%x)",
		key, tfmat));

	transfpacket.key = key;
	transfpacket.rel_num = UM_TRANSFORM_REL;
	if (ur_retrieve_transf(&transfpacket) != 0)
		{
 		uu_ugerror1(UM_MODEL, 163, key, ebuf);
		ud_prmerr (ebuf);
		goto failure;
		}
	else
		um_tftotf(transfpacket.tfmat, tfmat);

success:
	status = UU_SUCCESS;
	uu_dexit;
	return (status);

failure:
	status = UU_FAILURE;
	uu_dexit;
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int uc_retrieve_mtuple_attr(key, attrptr)
**			Retrieve all of the attribute data associated with the master
**			tuple having the given key.
**    PARAMETERS   
**       INPUT  : 
**          key						key of the master tuple for which
**											the attribute data is desired
**       OUTPUT :  
**          attrptr					pointer to an attribute record
**											into which the atttribute data will
**											be placed
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_retrieve_mtuple_attr(key, attrptr)
	UU_KEY_ID key;
	struct UC_attributedatabag *attrptr;

	{
	int status;

	uu_denter(UU_MTRC,(us,"uc_retrieve_mtuple_attr(key=%d, attrptr=%x)",
		key, attrptr));

	attrptr->key = key;
	if (ur_retrieve_attr(attrptr) != UU_SUCCESS) goto failure;

success:
	status = UU_SUCCESS;
	uu_dexit;
	return (status);


failure:
	status = UU_FAILURE;
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION :  int uc_create_mtuple_data(eptr, tfmat, attrptr)
**			Create a UNIBASE master tuple for the specified entity (EPTR).
**			If the transformation matrix (TFMAT) is UU_NULL, associate the
**			current default transformation with the entity; otherwise, 
**			use the given transformation. If the attribute data (ATTRPTR)
**			is UU_NULL, associate the current default attributes with the
**			entity; otherwise associate the given attributes. Finally,
**			set the view(s) that the entity is to be displayed in.
**    PARAMETERS   
**       INPUT  : 
**          eptr				pointer to completed entity record to be stored in
**									UNIBASE.
**			 	tfmat				transformation matrix (if not NULL).
**				attrptr			pointer to the attribute bundle (if not NULL).
**       OUTPUT :  
**          output
**			RETURNS      : UU_SUCCESS if no problems encountered, UU_FAILURE
**								otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
uc_create_mtuple_data(eptr, tfmat, attrptr)
	struct UC_entitydatabag *eptr;
	UM_transf tfmat;
	struct UM_attrdata_rec *attrptr;

	{
	struct UM_transf_rec transpacket;
	int status;
	int geom_class;

	uu_denter(UU_MTRC,(us,"uc_create_mtuple_data(rel_num=%d, tfmat:%x, attrptr:%x)", 
							eptr->rel_num, tfmat, attrptr));

	/* create master tuple in UNIBASE */
	if (ur_create_data(eptr) != 0) goto failure;

	/* initialize transformation */
	if (tfmat != UM_DEFAULT_TF)
		{ /* only need to update transformation if not default */
		transpacket.key = eptr->key;
		transpacket.rel_num = UM_TRANSFORM_REL;
		um_tftotf(tfmat, transpacket.tfmat);
		if (ur_update_transf(&transpacket) != 0) goto failure;
		}

	/* initialize attributes if current attributes are not desired */
	if (attrptr != UM_CURRENT_ATTR)
		{
		attrptr->key = eptr->key;
		if (ur_update_attr(attrptr) != 0) goto failure;
		}
	else
		{
		ur_put_attrmdl_key(eptr->key);
		ur_put_attrmdl_rel_num(UM_ATTR_REL);
		if (ur_update_attr(&UM_attrmdl) != 0) goto failure;
		}

	/* set the view that this entity is in */
	ur_update_view_key(eptr->key, ur_get_dispattr_view_in());

success:
	status = UU_SUCCESS;
	uu_dexit;
	return(status);

failure:
	status = UU_FAILURE;
	uu_dexit;
	return(status);
	}

