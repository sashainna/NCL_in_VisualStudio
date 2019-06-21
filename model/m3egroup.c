/*********************************************************************
**    NAME         :  m3egroup.c
**       CONTAINS: group manipulation operations
**			int um_p44_group(eptr)
**			int um_drw44_group(eptr, tfmat, attrptr)
**			int um_dl44_group(key)
**			int um_tr44_group(eptr, offset)
**			int um_rot44_group(eptr, pt, dir, angle, rotmat)
**			int um_tf44_group(eptr, tfmat, store)
**			int um_cp44_group(e1ptr, e2ptr, bagsize)
**			int um_mir44_group(eptr, mirrpt, normal, mirrmat)
**			int um_sc44_group(eptr, scalpt, scalmat, scale)
**			int um_undl5_group(key, undeleteok)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3egroup.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:54
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "go.h"                          
#include "class.h"                          
#include "mdrel.h"                          
#include "mxxx.h"
#include "mattr.h"
#include "mdattr.h"
#include "mdcoord.h"
#include "mdebug.h"

extern UU_LOGICAL UM_set_constituent_pickids;

/*********************************************************************
**    E_FUNCTION     :
**       description
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
um_p44_group(eptr)
	struct UM_grouper_rec *eptr;

	{
	uu_denter(UU_MTRC,(us,"um_p44_group(key=%x)", eptr->key));

	sprintf(UM_sbuf, "GROUP %d", eptr->key);
	um_pscroll(UM_sbuf);
	um_p_ary(UM_PINT,"members", eptr->no_member, eptr->member);

	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_drw44_group(eptr, tfmat, attrptr)
**       Draw the entities in a group.
**    PARAMETERS   
**       INPUT  : 
**          eptr					pointer to a group entity
**				tfmat					transformation matrix
**				attrptr				pointer to an attribute bundle
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_drw44_group(eptr, tfmat, attrptr)
	struct UM_grouper_rec *eptr;
	UM_transf tfmat;
	struct UM_attrdata_rec *attrptr;

	{
	struct UC_entitydatabag subent;
	UM_transf subent_tfmat;
	struct UC_attributedatabag subent_attr;
	int status;
	int i;

	uu_denter(UU_MTRC, (us,"um_drw44_group(key=%x)", eptr->key));
	status = UU_SUCCESS;

	UM_set_constituent_pickids = UU_FALSE;

	/* set the current display attributes */
	um_set_disp_attr(attrptr);

	for (i=0; i<eptr->no_member; i++)
		{
		subent.key = eptr->member[i];
		uc_retrieve_data(&subent, sizeof(struct UC_entitydatabag));
		uc_retrieve_transf(subent.key, subent_tfmat);
		um_tftmtf(subent_tfmat, tfmat, subent_tfmat);
		subent_attr.key = subent.key;
		uc_retrieve_attr(subent.key, &subent_attr);
		gspickid(subent.key);
		uc_draw(&subent, subent_tfmat, &subent_attr);
		}

	UM_set_constituent_pickids = UU_TRUE;

	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_dl44_group(key)
**       Delete a group and all of the entities in the group.
**    PARAMETERS   
**       INPUT  : 
**          key				UNIBASE key of group
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_dl44_group(key)
	UU_KEY_ID key;

	{
	struct UM_grouper_rec group;
	int status;
	int i;

	uu_denter(UU_MTRC, (us,"um_dl44_group(key=%x)", key));

	status = UU_SUCCESS;

	group.key=key;

	/* Has the group already been deleted? */
	if ((status = ur_test_tuple(key)) == UU_FAILURE) 
		{
		uu_dprint(UU_MTRC, (us,"group already deleted"));
		goto done;
		}

	status = uc_retrieve_data(&group, sizeof(struct UM_grouper_rec));
	if (status == UU_FAILURE)
		{
		uu_dprint(UU_MTRC, (us,"cannot retrieve group data"));
		goto done;
		}

	for (i=0; i< group.no_member; i++)
		uc_delete(group.member[i]);

	ur_delete_all(key);

done:
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_tr44_group(eptr, offset)
**       Translate a group by translating all of the members of the
**			group.
**    PARAMETERS   
**       INPUT  : 
**          eptr						pointer to space planning entity
**				offset					vector to translate along
**       OUTPUT :  
**          none
**    RETURNS      : 
**				UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tr44_group(eptr, offset)
	struct UM_grouper_rec *eptr;
	UM_coord offset;

	{
	struct UC_entitydatabag e;
	int i;

	uu_denter(UU_MTRC,(us,"um_tr44_group(key=%x)",eptr->key));

	for (i=0; i<eptr->no_member; i++)
		{
		e.key = eptr->member[i];
		uc_retrieve_data(&e, sizeof(struct UC_entitydatabag));
		uc_translate(&e, offset);
		}

	uu_dexit;
	return(UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_rot44_group(eptr, pt, dir, angle, rotmat)
**			Rotate a group by rotating each of the members of the group.
**    PARAMETERS   
**       INPUT  : 
**				eptr	     	entity pointer
**          pt		      point on the rotation axis
**          dir			direction vector of axis
**          angle   		the angle through which to rotate
**          rotmat	  	4 x 3 rotation matrix
**       OUTPUT :  
**				none:
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_rot44_group(eptr, pt, dir, angle, rotmat)
   struct UM_grouper_rec *eptr;
   UM_coord    pt;
	UM_vector    dir;
	UM_angle    angle;
   UM_transf    rotmat;

	{
	struct UC_entitydatabag e;
	int i;
	int status;

   uu_denter(UU_MTRC, (us,"um_rot44_group(key=%x)", eptr->key));
	status = UU_SUCCESS;

	for (i=0; i<eptr->no_member; i++)
		{
		e.key = eptr->member[i];
		uc_retrieve_data(&e, sizeof(struct UC_entitydatabag));
		uc_rotate(&e, pt, dir, angle, rotmat);
		}

	uu_dexit;	
	return (status);
	}

/*********************************************************************
**    E_FUNCTION: int um_tf44_group(eptr, tfmat, store)
**      Apply a transformation matrix to a group by applying it to
**			each of the members of the group.
**    PARAMETERS   
**       INPUT  : 
**			 eptr	      	entity pointer
**        tfmat	   	4 x 3 transformation matrix to transform entity by.
**			 store			TRUE iff the transformed entity is to be stored
**								in UNIBASE. 
**       OUTPUT :  
**          eptr			pointer to the transformed entity record.
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int 
um_tf44_group(eptr, tfmat, store)
   struct UM_grouper_rec *eptr;
   UM_transf tfmat;
	UU_LOGICAL store;

	{
	struct UC_entitydatabag e;
	int i;
	int status;

   uu_denter(UU_MTRC, (us,"um_tf44_group(key=%x)", eptr->key));

	status = UU_SUCCESS;
	for (i=0; i<eptr->no_member; i++)
		{
		e.key = eptr->member[i];
		uc_retrieve_data(&e, sizeof(struct UC_entitydatabag));
		uc_transform(&e, tfmat, store);
		}

	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_cp44_group(e1ptr, e2ptr, bagsize)
**      Copy a group.
**    PARAMETERS   
**       INPUT  : 
**				e1ptr    pointer to group to be copied
**				bagsize	size of the data bags pointed to by e2.
**       OUTPUT :  
**				e2ptr    pointer to new group
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_cp44_group(e1ptr, e2ptr, bagsize)
	struct UM_grouper_rec *e1ptr;
	struct UM_grouper_rec *e2ptr;
	int bagsize;

	{
	UU_KEY_ID	tempkey;
	UM_transf	tfmat;
	struct UC_entitydatabag *e1;
	struct UC_entitydatabag *e2;
	struct UC_attributedatabag attrbag;
	int i;
	int status;

	uu_denter(UU_MTRC, (us,"um_cp44_group(key=%x, bagsize=%d)", 
		e1ptr->key, bagsize));

   status = UU_SUCCESS;

	/* get space for subentities */
	e1 = (struct UC_entitydatabag *) uu_malloc(sizeof(struct UC_entitydatabag));
	e2 = (struct UC_entitydatabag *) uu_malloc(sizeof(struct UC_entitydatabag));

	/* initialize e2 data storage area */
	ur_setup_data(UM_GROUP_REL, e2ptr, bagsize);
	
	/* for each subentity in group e1, make a copy and include in group e2 */
	e2ptr->no_member = e1ptr->no_member;
	for (i=0; i<e1ptr->no_member; i++)
		{
		e1->key = e1ptr->member[i];
		uc_retrieve_data(e1, sizeof(struct UC_entitydatabag));

		uc_copy(e1, e2, sizeof(struct UC_entitydatabag));

		e2ptr->member[i] = e2->key;
	   ur_update_displayable(e2ptr->member[i], UM_NEVERDISPLAYABLE);
		}

	/* create the copy in Unibase */
	uc_retrieve_transf(e1ptr->key, tfmat);
	uc_retrieve_attr(e1ptr->key, &attrbag);
	um_create_geom(e2ptr, tfmat, &attrbag);

	/* free storage */
	uu_free(e1);
	uu_free(e2);

	uu_dexit;
	return (status);

	}

/**************************************************************************
**  E_FUNCTION:   int um_mir44_group(eptr, mirrpt, normal, mirrmat)
**      Apply a mirror transformation to all of the members of a group.
**  PARAMETERS   
**      INPUT  : 
**          eptr :			Pointer to the entity to be rotated such that 
**									it is a mirror image of its current orientation.
**          mirrpt :			point on mirror plane
**          mirrnorm :		mirror plane normal
**				mirrmat:			mirroring matrix
**      OUTPUT :    
**				eptr				pointer to the mirrored entity.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int
um_mir44_group(eptr, mirrpt, mirrnorm, mirrmat)
	struct UM_grouper_rec *eptr;
	UM_coord mirrpt;
	UM_vector mirrnorm;
	UM_transf mirrmat;

	{                        
	struct UC_entitydatabag e;
	int i;
	int status;

	uu_denter(UU_MTRC,(us,"um_mir44_group(key=%d)", eptr->key));

	status = UU_SUCCESS;

	for (i=0; i<eptr->no_member; i++)
		{
		e.key = eptr->member[i];
		ur_retrieve_data_relnum(e.key, &e.rel_num);
		uc_retrieve_data(&e, sizeof(struct UC_entitydatabag));
		uc_mirror(&e, mirrpt, mirrnorm, mirrmat);
		}
	uu_dexit;
	return (status);
	}

/**************************************************************************
**  E_FUNCTION:   int um_sc44_group(eptr, scalpt, scalmat, scale)
**			Scale an group by scaling each of the members of the group.
**  PARAMETERS   
**      INPUT  : 
**          eptr :			Pointer to the  entity to be scaled.
**          scalpt :			point on scale plane
**      OUTPUT :    
**				eptr				pointer to the scaled entity.
**  RETURNS      :  
**			0 iff no error;
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
int
um_sc44_group(eptr, scalpt, scalmat, scale)
	struct UM_grouper_rec *eptr;
	UM_coord scalpt;
	UM_transf scalmat;
	UM_length scale;

	{
	struct UC_entitydatabag e;
	int i;
	int status;

   uu_denter(UU_MTRC,(us,"um_sc44_group(key=%x)", eptr->key));

	status = UU_SUCCESS;
	for (i=0; i<eptr->no_member; i++)
		{
		e.key = eptr->member[i];
		ur_retrieve_data_relnum(e.key, &e.rel_num);
		uc_retrieve_data(&e, sizeof(struct UC_entitydatabag));
		uc_scale(&e, scalpt, scalmat, scale);
		}

   uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_undl5_group(key, undeleteok)
**       Check to see if the group can be undeleted.
**    PARAMETERS   
**       INPUT  : 
**				key				key of group curve
**       OUTPUT :  
**          undeleteok		UU_TRUE => can be undeleted
**									UU_FALSE => can not be undeleted
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_undl5_group(key, undeleteok)
	UU_KEY_ID key;
	UU_LOGICAL *undeleteok;

	{
	int status;
	int i;
	struct UM_grouper_rec e;

	uu_denter(UU_MTRC,(us,"um_undl5_group(key:%d)",key));
	e.key = key;
	status = uc_retrieve_data(&e, sizeof(e));
	if (status != UU_SUCCESS) goto done;
	for (i=0; i<e.no_member; i++) 
		{
		status = uc_canbe_undeleted(e.member[i], undeleteok);
		if ((status != UU_SUCCESS) || (!*undeleteok)) goto done;
		}

done:;
	uu_dexitstatus("um_undl5_group", status);
	return (status);
	}
