/*********************************************************************
**    NAME:  bcondba.c
**       CONTAINS: 
**    	int ub_retr64_connect(eptr, bagsize)
**    	int ub_cr64_connect(eptr, tfmat, attrptr)
**    	int ub_dl64_connect(key)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       bcondba.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:02
*********************************************************************/
#include "usysdef.h"
#include "uhep.h"
#include "udebug.h"
#include "mdrel.h"
#include "mdcoord.h"	/* for UM_DEFAULT_TF */
#include "mattr.h"	/* for UM_transf_rec */
#include "class.h"	/* for UC_attributedatabag */
#include "bsym.h"

/*********************************************************************
**    E_FUNCTION: int ub_retr64_connect(eptr, bagsize)
**       Retrieve all of the data associated with a connector
**			from UNIBASE.
**    PARAMETERS   
**       INPUT  : 
**          eptr					pointer to connector record
**				bagsize				size of storage area to put data
**       OUTPUT :  none
**    RETURNS      : UU_SUCCESS iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ub_retr64_connect(eptr, bagsize)
	struct UB_conector_rec *eptr;
	int bagsize;
{
	int status;
	uu_denter(UU_BTRC, (us,"ub_retr64_connect(key=%d, bagsize=%d)",
		eptr->key, bagsize));
	status = ur_retrieve_app_data(eptr, bagsize);
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION :  int ub_cr64_connect(eptr, tfmat, attrptr)
**       Create a connector in UNIBASE.
**    PARAMETERS   
**       INPUT  : 
**          eptr				pointer to completed entity record to be stored in
**									UNIBASE.
**			 	tfmat				transformation matrix (if not NULL).
**				attrptr			pointer to the attribute bundle (if not NULL).
**       OUTPUT :  none.
**			RETURNS      : UU_SUCCESS if no problems encountered, UU_FAILURE
**								otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ub_cr64_connect(eptr, tfmat, attrptr)
	struct UB_conector_rec *eptr;
	UU_REAL tfmat[4][3];
	struct UC_attributedatabag *attrptr;
{
	struct UM_transf_rec transpacket;
	int status = UU_SUCCESS;

	uu_denter(UU_BTRC,(us,"ub_cr64_connect(rel_num=%d, tfmat:%x, attrptr:%x)", 
							eptr->rel_num, tfmat, attrptr));

	/* create master tuple in UNIBASE */
	if (ur_create_data(eptr) != 0)
	{
		uu_uerror1(UM_MODEL, 157, eptr->rel_num);
		/* message is: um_create_geom: entity not stored, rel_num = %d */
		goto failed;
	}

	/* initialize transformation */
	if (tfmat != UM_DEFAULT_TF)
	{ /* only need to update transformation if not default */
		transpacket.key = eptr->key;
		transpacket.rel_num = UM_TRANSFORM_REL;
		um_tftotf(tfmat, transpacket.tfmat);
		if (ur_update_transf(&transpacket) != 0)
		{
			uu_uerror1(UM_MODEL, 162, eptr->key);
			/* message is: um_create_geom: can't store transform for entity
			 * that has key = %d */
			goto failed;
		}
	}

	/* initialize attributes if current attributes are not desired */
	if (attrptr != UM_CURRENT_ATTR)
	{
		attrptr->key = eptr->key;
		if (ur_update_attr(attrptr) != 0) goto failed;
	}
	else
	{
		/* UM_attr.key = eptr->key; */
		ur_put_attrmdl_key(eptr->key);
		/* UM_attr.rel_num = UM_ATTR_REL; */
		ur_put_attrmdl_rel_num(UM_ATTR_REL);
		if (ur_update_attr(&UM_attrmdl) != 0) goto failed;
	}

	/* set the view that this piece of geometry is in */
	ur_update_view_key(eptr->key, ur_get_dispattr_view_in());

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

/*********************************************************************
**    E_FUNCTION     : int ub_dl64_connect(key)
**       Delete a connector:
**				1. remove the connector key from all instances 
**					associated with it;
**				2. delete the graphical representation from DIGS;
**				3. delete the connector from UNIBASE
**    PARAMETERS   
**       INPUT  : 
**          key					key of connector entity to delete
**       OUTPUT :  none
**    RETURNS: UU_SUCCESS if no problems; UB_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ub_dl64_connect(key)
	UU_KEY_ID key;
{
	struct UB_conector_rec con;
	int i, dsegid, status = UU_SUCCESS;
	uu_denter(UU_BTRC, (us,"ub_dl64_connect(key=%d)", key));

	/* retrieve the data for the connector */
	con.key = key;
	status = ur_retrieve_app_data(&con, sizeof(con));
	if (status != UU_SUCCESS) goto failed;

	/* delete DIGS segment */
	ur_retrieve_disp_segid(key, &dsegid);
	if (dsegid > 0) uv_delsegs(dsegid);

	/* delete the connector */
	ur_delete_all(key);
	/* delete the polyline inside connector */
	ur_delete_all(con.pline);
		
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexit;
	return(status);
}	

