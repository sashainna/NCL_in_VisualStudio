/*********************************************************************
**
**    NAME         :  masctxt.c
**
**       CONTAINS:
**				int ua_asc_txt_delete(associate, association)
**				int ua_asc_txt_translate(eptr, association, offset)
**				int ua_asc_txt_rotate(eptr, association, pt, dir, angle, rotmat)
**				int ua_asc_txt_transform(eptr, association, tfmat, store)
**				int ua_asc_txt_mirror(eptr, association, mirrpt, normal, mirrmat)
**				int ua_asc_txt_scale(eptr, association, scalpt, scalmat, scale)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**
**    MODULE NAME AND RELEASE LEVEL 
**       asctext.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:39
**
*********************************************************************/

#include "usysdef.h"
#include "class.h"
#include "mdcoord.h"
#include "mdebug.h"
#include "udebug.h"

/*********************************************************************
**    E_FUNCTION :  int ua_asc_txt_delete(associate, association)
**       text along a curve associativity back-end delete method
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ua_asc_txt_delete(associate, association)
UU_KEY_ID associate;					/* associate key */
UU_KEY_ID association;				/* association key */
{
	int status;

	uu_denter(UU_MTRC,(us,"enter ua_asc_txt_delete: ate=0x%x, tion=0x%x",
		associate, association));

/*	-- delete the unibase representation of the text -- */

	status = uc_delete(association);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ua_asc_txt_translate(eptr, association, offset)
**      Translate one entity (EPTR) by the specified OFFSET. 
**    PARAMETERS   
**       INPUT  : 
**				eptr      Pointer to the entity to be translated.
**				association = key id of association entity
**          offset    The vector by which to offset.
**       OUTPUT :  
**          none
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ua_asc_txt_translate(eptr, association, offset)
struct UC_entitydatabag *eptr;
UU_KEY_ID association;							/* key of association entity */
UM_vector offset;
{                        
	int status;
	struct UC_entitydatabag packet;

	uu_denter(UU_MTRC,(us,"ua_asc_txt_translate(key=%d, tion=%x, vector=%x)", 
		(*eptr).key, association, offset));

/*	-- set the key into the data bag -- */

	packet.key = association;
	uc_retrieve_data(&packet, sizeof(struct UC_entitydatabag));

/*	-- translate and display the text -- */

	status = ua_tr_text(&packet, offset);
	if(status == UU_SUCCESS)
		status = uc_display(&packet);

/*	-- invoke the back-end -- */

	uc_be_translate(&packet, UU_NULL, offset, UC_TOUCH);

	uu_dprint(UU_MTRC,(us,"leave ua_asc_txt_translate: status=%d", status));
	uu_dexitstatus("ua_asc_txt_translate", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ua_asc_txt_rotate(eptr, association, pt, dir, angle, rotmat)
**      Rotate one curve (EPTR) by the specified matrix.
**    PARAMETERS   
**       INPUT  : 
**			 eptr	      	entity pointer
**				association = key id of association entity
**          pt		      point on the rotation axis
**          dir				direction vector of axis
**          angle   		the angle through which to rotate
**          rotmat	   	4 x 3 rotation matrix
**       OUTPUT :  
**          eptr			pointer to rotated entity.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ua_asc_txt_rotate(eptr, association, pt, dir, angle, rotmat)
struct UC_entitydatabag *eptr;
UU_KEY_ID association;							/* key of association entity */
UM_coord pt;
UM_vector dir;
UM_angle angle;
UM_transf rotmat;
{
	int status;
	struct UC_entitydatabag packet;

   uu_denter(UU_MTRC,  (us, "ua_asc_txt_rotate(%d, %x, %x, %g, %x)", 
								(*eptr).key, pt, dir, angle, rotmat));

/*	-- set the key into the data bag -- */

	packet.key = association;
	uc_retrieve_data(&packet, sizeof(struct UC_entitydatabag));
	status = ua_tf_text(&packet, rotmat, UU_TRUE);
	if(status == UU_SUCCESS)
		status = uc_display(&packet);

/*	-- invoke the back-end -- */

	uc_be_rotate(&packet, UU_NULL, pt, dir, angle, rotmat, UC_TOUCH);

	uu_dexitstatus("ua_asc_txt_rotate", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int ua_asc_txt_transform(eptr, association, tfmat, store)
**      Tranform entity (EPTR) by the specified transform.
**    PARAMETERS   
**       INPUT  : 
**			 eptr	      	entity pointer
**				association = key id of association entity
**        tfmat	   	4 x 3 transformation matrix to transform entity by.
**			 store			TRUE iff the transformed entity is to be stored
**								in UNIBASE. 
**       OUTPUT :  
**          eptr			pointer to the transformed entity record.
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ua_asc_txt_transform(eptr, association, tfmat, store)
struct UC_entitydatabag *eptr;
UU_KEY_ID association;							/* key of association entity */
UM_transf tfmat;
UU_LOGICAL store;
{
	int status;
	struct UC_entitydatabag packet;

	uu_denter(UU_MTRC, (us,"ua_asc_txt_transform(key:%d,tfmat:%x,store:%d)",
		(*eptr).key, tfmat, store));

/*	-- set the key into the data bag -- */

	packet.key = association;
	uc_retrieve_data(&packet, sizeof(struct UC_entitydatabag));
	status = ua_tf_text(&packet, tfmat, store);
	if(status == UU_SUCCESS)
		status = uc_display(&packet);

/*	-- invoke the back-end -- */

	uc_be_transform(&packet, UU_NULL, tfmat, store, UC_TOUCH);

	uu_dexitstatus("ua_asc_txt_transform", status);
	return(status);
}

/**************************************************************************
**  E_FUNCTION:   int ua_asc_txt_mirror(eptr, association, mirrpt,
**												normal, mirrmat)
**      Mirror one curve (EPTR) around the specified mirror plane.
**  PARAMETERS   
**      INPUT  : 
**          eptr :			Pointer to the entity to be rotated such that 
**									it is a mirror image of its current orientation.
**				association = key id of association entity
**          mirrpt :			point on mirror plane
**          mirrnorm :		mirror plane normal
**				mirrmat:			mirroring matrix
**      OUTPUT :    
**				eptr				pointer to the mirrored entity.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/

int ua_asc_txt_mirror(eptr, association, mirrpt, mirrnorm, mirrmat)
struct UC_entitydatabag *eptr;
UU_KEY_ID association;							/* key of association entity */
UM_coord mirrpt;
UM_vector mirrnorm;
UM_transf mirrmat;
{                        
	int status;

	uu_denter(UU_MTRC, (us, "ua_asc_txt_mirror(%d, %x, %x, %x)", (*eptr).key, 
		mirrpt, mirrmat, mirrnorm));

/*	-- delete the unibase representation of the text -- */

	status = uc_delete(association);

	uu_dexitstatus("ua_asc_txt_mirror", status);
	return(status);
}

/**************************************************************************
**  E_FUNCTION:  int ua_asc_txt_scale(eptr, association, scalpt, scalmat, scale)
**			Scale an entity.
**  PARAMETERS   
**      INPUT  : 
**				association = key id of association entity
**          scalpt :			point on scale plane
**      OUTPUT :    
**				eptr				pointer to the scaled entity.
**  RETURNS      :  
**			0 iff no error;
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/

int ua_asc_txt_scale(eptr, association, scalpt, scalmat, scale)
struct UC_entitydatabag *eptr;
UU_KEY_ID association;							/* key of association entity */
UM_coord scalpt;
UM_transf scalmat;
UM_length scale;
{                        
	int status;
	struct UC_entitydatabag packet;

   uu_denter(UU_MTRC,(us,"ua_asc_txt_scale(%d,%x,%x,%g)",(*eptr).key,
						scalpt,scalmat,scale));

/*	-- set the key into the data bag -- */

	packet.key = association;
	uc_retrieve_data(&packet, sizeof(struct UC_entitydatabag));
	status = ua_scal_text(&packet, scalpt, scalmat, scale);
	if(status == UU_SUCCESS)
		status = uc_display(&packet);

/*	-- invoke the back-end -- */

	uc_be_scale(&packet, UU_NULL, scalpt, scalmat, scale, UC_TOUCH);

	uu_dexitstatus("ua_asc_txt_scale", status);
	return(status);
}
