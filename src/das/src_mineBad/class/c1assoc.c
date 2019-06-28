/*********************************************************************
**    NAME         :  c1assoc.c
**       CONTAINS: back-end class dispatchers
**				int uc_delete(key)
**				int uc_delete_noseg(key)
**				int uc_translate(eptr, offset)
**				int uc_rotate(eptr, pt, dir, angle, rotmat)
**				int uc_transform(eptr, tfmat, store)
**				int uc_mirror(eptr, mirrpt, normal, mirrmat)
**				int uc_scale(eptr, scalpt, scalmat, scale)
**				ucu_3d_validate(table_3d_desc, ate_class, tion_relnum, message)
**				int uc_be_delete(class, associate, association, action)
**				int uc_be_translate(eptr, association, offset, action)
**				int uc_be_rotate(eptr, pt, dir, angle, rotmat, action)
**				int uc_be_transform(eptr, tfmat, store, action)
**				int uc_be_mirror(eptr, mirrpt, normal, mirrmat, action)
**				int uc_be_scale(eptr, scalpt, scalmat, scale, action)
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       c1assoc.c , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       08/01/17 , 13:30:37
*********************************************************************/

#include "class.h"
#include "mdcoord.h"
#include "udebug.h"
#include "nclmodals.h"

extern UC_TABLE_DESCRIPTOR *UC_3d_descriptor[];	
extern int  UR_active;

#define ASSOC_BUF_SIZE 50

/*	-- This is the dispatched key mechanism so that we do not invoke the list
		of associations on an associate more than once when doing the
		back-end processing. This list will contain the keys of the entities 
		that will have or have already had back-end processing performed
		ont them. --*/

	static UU_KEY_ID dispatched[ASSOC_BUF_SIZE];	/* dispatched key list */
	static int dispatched_ptr = 0;

/*	-- This macro determines if he associate being processed is a new one
		in this sequence that needs to be considered for back-end 
		processing.  If it is, it is pushed onto the Q to be dispatched
		on the back-end. -- */

#define Q_PUSH(name, associate) \
processed = UU_TRUE; \
for(i=0; i<dispatched_ptr; i++) \
	if(associate == dispatched[i]) \
		processed = UU_FALSE; \
if(processed == UU_TRUE) \
{ \
	dispatched[dispatched_ptr] = associate; \
	dispatched_ptr++; \
}

/*	-- This macro removes a key from the Q after the back-end processing
		has been completed. -- */

#define Q_POP(name) \
if(processed == UU_TRUE) \
	dispatched_ptr--

/*	-- This is the macro to process the back-end list of associations. -- */

#define BACKEND(name, method_function) \
/*	-- only process associates that are first time entities to be dispatched \
		through the back-end -- */ \
if(processed == UU_TRUE) \
{ \
/*	-- only process successful disptaches or undefined methods --*/ \
	if((status==UU_SUCCESS) || (function==UC_UNDEFINED_METHOD)) \
	{ \
		uu_dprint(UU_MTRC,(us,"%s back end processing: num assocs=%d", \
			name, assoc_buf_len)); \
/*		-- dispatch to the list of assoaciations of this associate --*/ \
		for(i=0; i<assoc_buf_len; i++) \
		{ \
			uu_dprint(UU_MTRC,(us, \
				"%s back end: dispatch to entry=%d, key=%x", \
					name, i, assoc_buf[i])); \
			if (ur_get_del_stat() == UU_FALSE) method_function; \
			else \
				{ \
				UU_LOGICAL undeletable; \
				status = uc_canbe_undeleted(assoc_buf[i], &undeletable); \
				if (status == UU_FAILURE) break; \
				if (!undeletable) \
					{ \
					ur_set_del_stat(UU_FALSE); \
					method_function; \
					ur_set_del_stat(UU_TRUE); \
					} \
				else method_function; \
				} \
		} \
	} \
}
/*********************************************************************
**    E_FUNCTION     : int uc_delete(key)
**      Delete the entity specified by the given key from UNIBASE and
**		  DIGS.
**    PARAMETERS   
**       INPUT  : 
**				key = master tuple id of entity to be deleted
**       OUTPUT :  
**          none
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_delete(key)
UU_KEY_ID key;
{
	int class;
	int dsegid;										/* DIGS segment identifier */
	int rel_num;
	int flg=0,status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_delete(%d)",key));

	if(um_retrieve_data_relnum(key, &rel_num) != UU_SUCCESS)
		goto failed;

	if(rel_num == UM_COORDSYS_REL || rel_num == UM_LIGHT_REL || rel_num == NCL_COLOR_REL) 
		goto done;

	if(UR_active != 2) 
		ur_retrieve_disp_segid(key, &dsegid);

	class = CONVERT_REL_TO_CLASS(rel_num);
	
	status = uc_be_delete(class, key, UU_NULL, UC_PERFORM);
	if(status != UU_SUCCESS)
		goto failed;
/*
.....delete was a success; so delete entity form label list
*/
	ncl_delete_ldrlst(key);

/* -- delete was a success; so delete segment from display -- */

	if(UR_active != 2) 
	{
/*
.....Set flag to blank label boxes after remove.
*/
		if(!NCL_toggle_off)
		{
			NCL_labeldisp = UU_TRUE;
			NCL_toggle_off = UU_TRUE;
			flg =1;
		}
		if(dsegid >= 0) uv_delsegs(dsegid);
		ncl_randel(key, rel_num);
/*
.....ReSet flag to blank label boxes after remove.
*/
		if(flg)
		{
			flg =0;
			NCL_labeldisp = UU_FALSE;
			NCL_toggle_off = UU_FALSE;
		}
	}
	goto done;

failed: status = UU_FAILURE;
done:
	uu_dexitstatus("uc_delete", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int uc_delete_noseg(key)
**      Delete the entity specified by the given key from UNIBASE 
**    PARAMETERS   
**       INPUT  : 
**				key = master tuple id of entity to be deleted
**       OUTPUT :  
**          none
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_delete_noseg(key)
UU_KEY_ID key;
{
	int class;
	int rel_num;
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"uc_delete_noseg(%d)",key));

	if(um_retrieve_data_relnum(key, &rel_num) != UU_SUCCESS)
		goto failed;

	if(rel_num == UM_COORDSYS_REL || rel_num == UM_LIGHT_REL || rel_num == NCL_COLOR_REL) 
		goto done;

	class = CONVERT_REL_TO_CLASS(rel_num);

	status = uc_be_delete(class, key, UU_NULL, UC_PERFORM);
	if(status != UU_SUCCESS)
		goto failed;

	goto done;

failed: status = UU_FAILURE;
done:
	uu_dexitstatus("uc_delete", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int uc_translate(eptr, offset)
**      Translate one entity (EPTR) by the specified OFFSET. 
**    PARAMETERS   
**       INPUT  : 
**				eptr      Pointer to the entity to be translated.
**          offset    The vector by which to offset.
**       OUTPUT :  
**          none
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_translate(eptr, offset)
struct UC_entitydatabag *eptr;
UM_vector offset;
{                        
	int status;

	uu_denter(UU_MTRC,(us,"uc_translate(key:%d,%x)", (*eptr).key, offset));

	status = uc_be_translate(eptr, UU_NULL, offset, UC_PERFORM);

	uu_dprint(UU_MTRC,(us,"exit uc_translate(key=%d)", (*eptr).key));
	uu_dexitstatus("uc_translate", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int uc_rotate(eptr, pt, dir, angle, rotmat)
**      Rotate one curve (EPTR) by the specified matrix.
**    PARAMETERS   
**       INPUT  : 
**			 eptr	      	entity pointer
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

int uc_rotate(eptr, pt, dir, angle, rotmat)
struct UC_entitydatabag *eptr;
UM_coord    pt;
UM_vector    dir;
UM_angle    angle;
UM_transf    rotmat;
{
	int status;

   uu_denter(UU_MTRC, (us,"uc_rotate(%d,%x,%x,%g,%x)",
								(*eptr).key, pt, dir, angle, rotmat));

	status = uc_be_rotate(eptr, UU_NULL, pt, dir, angle, rotmat, UC_PERFORM);

	uu_dexitstatus("uc_rotate", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int uc_transform(eptr, tfmat, store)
**      Tranform entity (EPTR) by the specified transform.
**    PARAMETERS   
**       INPUT  : 
**			 eptr	      	entity pointer
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

int uc_transform(eptr, tfmat, store)
struct UC_entitydatabag *eptr;
UM_transf tfmat;
UU_LOGICAL store;
{
	UC_METHOD  ucu_validate();
	int status;

	uu_denter(UU_MTRC, (us,"uc_transform(key:%d,tfmat:%x,store:%d)",
		(*eptr).key, tfmat, store));

	status = uc_be_transform(eptr, UU_NULL, tfmat, store, UC_PERFORM);

	uu_dexitstatus("uc_transform", status);
	return(status);
}

/**************************************************************************
**  E_FUNCTION:   int uc_mirror(eptr, mirrpt, normal, mirrmat)
**      Mirror one curve (EPTR) around the specified mirror plane.
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

int uc_mirror(eptr, mirrpt, mirrnorm, mirrmat)
struct UC_entitydatabag *eptr;
UM_coord mirrpt;
UM_vector mirrnorm;
UM_transf mirrmat;
{                        
	int status;

	uu_denter(UU_MTRC,(us,"uc_mirror(%d,%x,%x,%x)",(*eptr).key,
		mirrpt,mirrmat,mirrnorm));

	status = uc_be_mirror(eptr, UU_NULL, mirrpt, mirrnorm, mirrmat, UC_PERFORM);

	uu_dexitstatus("uc_mirror", status);
	return(status);
}

/**************************************************************************
**  E_FUNCTION:   int uc_scale(eptr, scalpt, scalmat, scale)
**			Scale an entity.
**  PARAMETERS   
**      INPUT  : 
**          eptr :				Pointer to the  entity to be scaled.
**          scalpt :			point on scale plane
**      OUTPUT :    
**				eptr				pointer to the scaled entity.
**  RETURNS      :  
**			0 iff no error;
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/

int uc_scale(eptr, scalpt, scalmat, scale)
struct UC_entitydatabag *eptr;
UM_coord scalpt;
UM_transf scalmat;
UM_length scale;
{                        
   int status;

   uu_denter(UU_MTRC,(us,"uc_scale(%d,%x,%x,%g)",(*eptr).key,
						scalpt,scalmat,scale));

	status = uc_be_scale(eptr, UU_NULL, scalpt, scalmat, scale, UC_PERFORM);

	uu_dexitstatus("uc_scale", status);
	return(status);
}

/*********************************************************************
**
**    I_FUNCTION:  UC_METHOD ucu_3d_validate(table_3d_desc, ate_class,
**									tion_relnum, message)
**			This function validates a 3D class dispatch request and
**			returns the function address
**
**    PARAMETERS   
**       INPUT: 
**				UC_TABLE_DESCRIPTOR *table_3d_desc[] = 3D table descriptor
**				int ate_class = associate class number
**				int tion_relnum = assocation relation number
**				int message = message identifier
**       OUTPUT:  
**				none
**
**    RETURNS: function address if no problems encountered,
**					UU_NULL otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
**
*********************************************************************/

UC_METHOD ucu_3d_validate(table_3d_desc, ate_class, tion_relnum, message)
UC_TABLE_DESCRIPTOR *table_3d_desc[];	/* 3D table descriptor */
int ate_class;									/* assocate class */
int tion_relnum;								/* assocation relation number */
int message;									/* message identifier */
{

	UC_METHOD function;
	char *uc_method_lookup();
	UC_TABLE_DESCRIPTOR *table_desc;		/* table descriptor */
	
	uu_denter(UU_MTRC,(us, 
		"enter DISPATCHER, table=%x, ate_class=%d, tion_relnum=%d, message=%d",
			table_desc, ate_class, tion_relnum, message));

/*	-- get the domain -- */

	table_desc = table_3d_desc[tion_relnum];

	if(table_desc == UU_NULL)
		goto failed;

	if((*table_desc).table_ptr[ate_class] != UU_NULL)
	{
		uu_dprint(UU_MTRC,(us, "CLASS DISPATCHER, table=%s, class=%s",
			(*table_desc).table_name,
				(*(*table_desc).table_ptr[ate_class]).label));

		uu_dprint(UU_MTRC,(us,"dispatch to %s = %s", 
			(*(*table_desc).method_ptr).meth_name[message],
				uc_method_lookup(
					(*(*table_desc).table_ptr[ate_class]).methods[message],
						table_desc)));

/*		-- validate class -- */

		if(ate_class<0 || ate_class>=(*table_desc).num_classes)
			goto failed;

/*		-- validate message -- */

		if(message<0 || message>=(*table_desc).num_methods)
			goto failed;

		function = (*(*table_desc).table_ptr[ate_class]).methods[message];
		if(function == UC_UNDEFINED_METHOD)
			goto failed;
	}
	else
		goto failed;

/*		-- request is valid --*/

done:
	uu_dprint(UU_MITRC,(us,"leave uc_3d_validate, func=%x", function));
	uu_dexit;
	return(function);

failed:
	function = UC_UNDEFINED_METHOD;
	goto done;
}

/*********************************************************************
**    E_FUNCTION     : int uc_be_delete(class, associate, association, action)
**      The entity specified by the given key has been deleted from UNIBASE 
**			and DIGS. Fix yourself, association.
**    PARAMETERS   
**       INPUT  : 
**				class = class number of the associate
**				associate = key id of the associate deleted
**				association = key id of the association deleted
**				action = what to do flag
**       OUTPUT :  
**          none
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

uc_be_delete(class, associate, association, action)
int class;											/* class number of associate */
UU_KEY_ID associate;								/* associate key */
UU_KEY_ID association;							/* association key */
UC_CLASS_ACTION action;							/* what to do flag */
{
	int status;
	int i;
	UU_LOGICAL processed;						/* key processed flag */
	int rel_num;									/* relation number of association */
	int assoc_buf_len;							/* association buffer length */
	UU_KEY_ID assoc_buf[ASSOC_BUF_SIZE];	/* association key buffer */
	UC_METHOD function, ucu_3d_validate();

	uu_denter(UU_MTRC,(us,"uc_be_delete(class=%d, ate=%x, tion=%x action=%d)",
		class, associate, association, action));

/*	-- get the relation number of the association and set up the 
		back-end Q. -- */

	if(association == 0)
	{
		rel_num = 0;
		Q_PUSH("uc_be_delete", associate);
	}
	else
	{
		processed = UU_FALSE;
		status = um_retrieve_data_relnum(association, &rel_num);
		if(status != UU_SUCCESS)
			goto done;
	}

/*	-- Get the list of associations for this key.  This must be done
		here before the associate is deleted and the list is deleted
		by unibase.  This is not true for the edit situations. -- */

	if(processed == UU_TRUE)
	{
		assoc_buf_len = ASSOC_BUF_SIZE;
		ur_retrieve_assoc_keys(associate, assoc_buf, &assoc_buf_len, UU_TRUE);
	}
	else
		assoc_buf_len = 0;

/*	-- only dispatch for UC_PERFORM and UC_PERMISSION -- */

	if(action == UC_TOUCH)
	{
		status = UU_SUCCESS;
		function = UC_UNDEFINED_METHOD;
		action = UC_PERFORM;
	}
	else
	{

/*		-- get the method from the class table and dispatch to it --*/

		function = ucu_3d_validate(UC_3d_descriptor, class, rel_num, UC_DELETE);

		if(function == UC_UNDEFINED_METHOD)
			status = UU_FAILURE;
		else
			status = (*function)(associate, association);
	}

/*	-- go notify all associations -- */

	BACKEND("uc_be_delete", 
		(uc_be_delete(class, associate, assoc_buf[i], action)));

done:

/*	-- Remove the associate from the list of keys that are processed on the
		back-end. -- */

	Q_POP("uc_be_delete");
	uu_dexitstatus("uc_be_delete", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int uc_be_translate(eptr, association, offset)
**      Translate one entity (EPTR) by the specified OFFSET. 
**    PARAMETERS   
**       INPUT  : 
**				eptr      Pointer to the entity to be translated.
**				association = key id of association entity
**          offset    The vector by which to offset.
**				action = what to do flag
**       OUTPUT :  
**          none
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_be_translate(eptr, association, offset, action)
struct UC_entitydatabag *eptr;
UU_KEY_ID association;							/* key of association entity */
UM_vector offset;
UC_CLASS_ACTION action;							/* what to do flag */
{                        
	int i;
	UU_LOGICAL processed;						/* key processed flag */
	int status;
	int class;										/* class number of associate */
	int rel_num;									/* relation number of association */
	int assoc_buf_len;							/* association buffer length */
	UU_KEY_ID assoc_buf[ASSOC_BUF_SIZE];	/* association key buffer */
	UC_METHOD function, ucu_3d_validate();

	uu_denter(UU_MTRC,(us,
		"uc_be_translate:key=%d, tion=%x, vector=%x, action=%d", 
			(*eptr).key, association, offset, action));

/*	-- get the relation number of the association and set up the 
		back-end Q. -- */

	if(association == 0)
	{
		rel_num = 0;
		Q_PUSH("uc_be_translate", (*eptr).key);
	}
	else
	{
		processed = UU_FALSE;
		status = um_retrieve_data_relnum(association, &rel_num);
		if(status != UU_SUCCESS)
			goto done;
	}

	class = CONVERT_REL_TO_CLASS((*eptr).rel_num);

/*	-- Get the list of associations for this key.  This must be done
		here before the associate is deleted and the list is deleted
		by unibase.  This may or may not be true for the edit situations. -- */

	assoc_buf_len = ASSOC_BUF_SIZE;
	ur_retrieve_assoc_keys((*eptr).key, assoc_buf, &assoc_buf_len, UU_TRUE);

/*	-- only dispatch for UC_PERFORM and UC_PERMISSION -- */

	if(action == UC_TOUCH)
	{
		status = UU_SUCCESS;
		function = UC_UNDEFINED_METHOD;
		action = UC_PERFORM;
	}
	else
	{

/*		-- get the method from the class table and dispatch to it --*/

		function = ucu_3d_validate(UC_3d_descriptor, class, rel_num,
			UC_TRANSLATE);
		if(function == UC_UNDEFINED_METHOD)
			status = UU_FAILURE;
		else
		{
			if(association == 0)
				status = (*function)(eptr, offset);
			else
				status = (*function)(eptr, association, offset);
		}
	}

/*	-- go notify all associations -- */

	BACKEND("uc_be_translate",
		(uc_be_translate(eptr, assoc_buf[i], offset, action)));

done:

/*	-- Remove the associate from the list of keys that are processed on the
		back-end. -- */

	Q_POP("uc_be_translate");
	uu_dprint(UU_MTRC,(us,"leave uc_be_translate: status=%d", status));
	uu_dexitstatus("uc_be_translate", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int uc_be_rotate(eptr, pt, dir, angle, rotmat)
**      Rotate one curve (EPTR) by the specified matrix.
**    PARAMETERS   
**       INPUT  : 
**			 eptr	      	entity pointer
**				association = key id of association entity
**          pt		      point on the rotation axis
**          dir				direction vector of axis
**          angle   		the angle through which to be_rotate
**          rotmat	   	4 x 3 rotation matrix
**				action = what to do flag
**       OUTPUT :  
**          eptr			pointer to be_rotated entity.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_be_rotate(eptr, association, pt, dir, angle, rotmat, action)
struct UC_entitydatabag *eptr;
UU_KEY_ID association;							/* key of association entity */
UM_coord    pt;
UM_vector    dir;
UM_angle    angle;
UM_transf    rotmat;
UC_CLASS_ACTION action;							/* what to do flag */
{
	int i;
	UU_LOGICAL processed;						/* key processed flag */
	int status = UU_FAILURE;
	int class;										/* class number of associate */
	int rel_num;									/* relation number of association */
	int assoc_buf_len;							/* association buffer length */
	UU_KEY_ID assoc_buf[ASSOC_BUF_SIZE];	/* association key buffer */
	UC_METHOD function, ucu_3d_validate();

   uu_denter(UU_MTRC, (us,"uc_be_rotate(%d,%x,%x,%g,%x), action=%d",
								(*eptr).key, pt, dir, angle, rotmat, action));

/*	-- Get the relation number of the association and set up the 
		back-end Q. -- */

	if(association == 0)
	{
		rel_num = 0;
		Q_PUSH("uc_be_rotate", (*eptr).key);
	}
	else
	{
		processed = UU_FALSE;
		status = um_retrieve_data_relnum(association, &rel_num);
		if(status != UU_SUCCESS)
			goto done;
	}

	class = CONVERT_REL_TO_CLASS((*eptr).rel_num);


/*	-- get the list of associations for this key -- */

	assoc_buf_len = ASSOC_BUF_SIZE;
	ur_retrieve_assoc_keys((*eptr).key, assoc_buf, &assoc_buf_len, UU_TRUE);

/*	-- only dispatch for UC_PERFORM and UC_PERMISSION -- */

	if(action == UC_TOUCH)
	{
		status = UU_SUCCESS;
		function = UC_UNDEFINED_METHOD;
		action = UC_PERFORM;
	}
	else
	{

/*		-- get the method from the class table and dispatch to it --*/

		function = ucu_3d_validate(UC_3d_descriptor, class, rel_num, UC_ROTATE);

		if(function == UC_UNDEFINED_METHOD)
			status = UU_FAILURE;
		else
		{
			if(association == 0)
				status = (*function)(eptr, pt, dir, angle, rotmat);
			else
				status = (*function)(eptr, association, pt, dir, angle, rotmat);
		}
	}

/*	-- Go notify all associations -- */

	BACKEND("uc_be_rotate", 
		(uc_be_rotate(eptr, assoc_buf[i], pt, dir, angle, rotmat, action)));

done:

/*	-- Remove the associate from the list of keys that are processed on the
		back-end. -- */

	Q_POP("uc_be_rotate");
	uu_dexitstatus("uc_be_rotate", status);
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int uc_be_transform(eptr, tfmat, store)
**      Tranform entity (EPTR) by the specified transform.
**    PARAMETERS   
**       INPUT  : 
**			 eptr	      	entity pointer
**				association = key id of association entity
**        tfmat	   	4 x 3 transformation matrix to transform entity by.
**			 store			TRUE iff the transformed entity is to be stored
**								in UNIBASE. 
**				action = what to do flag
**       OUTPUT :  
**          eptr			pointer to the transformed entity record.
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int uc_be_transform(eptr, association, tfmat, store, action)
struct UC_entitydatabag *eptr;
UU_KEY_ID association;							/* key of association entity */
UM_transf tfmat;
UU_LOGICAL store;
UC_CLASS_ACTION action;							/* what to do flag */
{
	int i;
	UU_LOGICAL processed;						/* key processed flag */
	int status = UU_FAILURE;
	int class;										/* class number of associate */
	int rel_num;									/* relation number of association */
	int assoc_buf_len;							/* association buffer length */
	UU_KEY_ID assoc_buf[ASSOC_BUF_SIZE];	/* association key buffer */
	UC_METHOD function, ucu_3d_validate();

	uu_denter(UU_MTRC, (us,
		"uc_be_transform(key:%d,tfmat:%x,store:%d, action=%d)",
			(*eptr).key, tfmat, store, action));

/*	-- get the relation number of the association and set up the 
		back-end Q. -- */

	if(association == 0)
	{
		rel_num = 0;
		Q_PUSH("uc_be_transform", (*eptr).key);
	}
	else
	{
		processed = UU_FALSE;
		status = um_retrieve_data_relnum(association, &rel_num);
		if(status != UU_SUCCESS)
			goto done;
	}

	class = CONVERT_REL_TO_CLASS((*eptr).rel_num);

/*	-- get the list of associations for this key -- */

	assoc_buf_len = ASSOC_BUF_SIZE;
	ur_retrieve_assoc_keys((*eptr).key, assoc_buf, &assoc_buf_len, UU_TRUE);

/*	-- only dispatch for UC_PERFORM and UC_PERMISSION -- */

	if(action == UC_TOUCH)
	{
		status = UU_SUCCESS;
		function = UC_UNDEFINED_METHOD;
		action = UC_PERFORM;
	}
	else
	{

/*		-- get the method from the class table and dispatch to it --*/

		function = ucu_3d_validate(UC_3d_descriptor, class, rel_num, 
			UC_TRANSFORM);
	
		if(function == UC_UNDEFINED_METHOD)
			status = UU_FAILURE;
		else
		{
			if(association == 0)
				status = (*function)(eptr, tfmat, store);
			else
				status = (*function)(eptr, association, tfmat, store);
		}
	}

/*	-- go notify all associations -- */

	BACKEND("uc_be_transform", 
		(uc_be_transform(eptr, assoc_buf[i], tfmat, store, action)));

done:

/*	-- Remove the associate from the list of keys that are processed on the
		back-end. -- */

	Q_POP("uc_be_transform");
	uu_dexitstatus("uc_be_transform", status);
	return(status);
}

/**************************************************************************
**  E_FUNCTION:   int uc_be_mirror(eptr, mirrpt, normal, mirrmat)
**      Mirror one curve (EPTR) around the specified mirror plane.
**  PARAMETERS   
**      INPUT  : 
**          eptr :			Pointer to the entity to be rotated such that 
**									it is a mirror image of its current orientation.
**				association = key id of association entity
**          mirrpt :			point on mirror plane
**          mirrnorm :		mirror plane normal
**				mirrmat:			mirroring matrix
**				action = what to do flag
**      OUTPUT :    
**				eptr				pointer to the mirrored entity.
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/

int uc_be_mirror(eptr, association, mirrpt, mirrnorm, mirrmat, action)
struct UC_entitydatabag *eptr;
UU_KEY_ID association;							/* key of association entity */
UM_coord mirrpt;
UM_vector mirrnorm;
UM_transf mirrmat;
UC_CLASS_ACTION action;							/* what to do flag */
{                        
	int i;
	UU_LOGICAL processed;						/* key processed flag */
	int status = UU_FAILURE;
	int class;										/* class number of associate */
	int rel_num;									/* relation number of association */
	int assoc_buf_len;							/* association buffer length */
	UU_KEY_ID assoc_buf[ASSOC_BUF_SIZE];	/* association key buffer */
	UC_METHOD function, ucu_3d_validate();

	uu_denter(UU_MTRC,(us,"uc_be_mirror(%d,%x,%x,%x), action=%d",(*eptr).key,
		mirrpt, mirrmat, mirrnorm, action));

/*	-- get the relation number of the association and set up the 
		back-end Q. -- */

	if(association == 0)
	{
		rel_num = 0;
		Q_PUSH("uc_be_mirror", (*eptr).key);
	}
	else
	{
		processed = UU_FALSE;
		status = um_retrieve_data_relnum(association, &rel_num);
		if(status != UU_SUCCESS)
			goto done;
	}

	class = CONVERT_REL_TO_CLASS((*eptr).rel_num);

/*	-- get the list of associations for this key -- */

	assoc_buf_len = ASSOC_BUF_SIZE;
	ur_retrieve_assoc_keys((*eptr).key, assoc_buf, &assoc_buf_len, UU_TRUE);

/*	-- only dispatch for UC_PERFORM and UC_PERMISSION -- */

	if(action == UC_TOUCH)
	{
		status = UU_SUCCESS;
		function = UC_UNDEFINED_METHOD;
		action = UC_PERFORM;
	}
	else
	{

/*		-- get the method from the class table and dispatch to it --*/

		function = ucu_3d_validate(UC_3d_descriptor, class, rel_num, UC_MIRROR);

		if(function == UC_UNDEFINED_METHOD)
			status = UU_FAILURE;
		else
		{
			if(association == 0)
				status = (*function)(eptr, mirrpt, mirrnorm, mirrmat);
			else
				status = (*function)(eptr, association, mirrpt, mirrnorm, mirrmat);
		}
	}
	
/*	-- go notify all associations -- */

	BACKEND("uc_be_mirror", 
	(uc_be_mirror(eptr, assoc_buf[i], mirrpt, mirrnorm, mirrmat, action)));

done:

/*	-- Remove the associate from the list of keys that are processed on the
		back-end. -- */

	Q_POP("uc_be_mirror");
	uu_dexitstatus("uc_be_mirror", status);
	return(status);
}

/**************************************************************************
**  E_FUNCTION:   int uc_be_scale(eptr, scalpt, scalmat, scale)
**			Scale an entity.
**  PARAMETERS   
**      INPUT  : 
**				association = key id of association entity
**          scalpt :			point on scale plane
**				action = what to do flag
**      OUTPUT :    
**				eptr				pointer to the scaled entity.
**  RETURNS      :  
**			0 iff no error;
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/

int uc_be_scale(eptr, association, scalpt, scalmat, scale, action)
struct UC_entitydatabag *eptr;
UU_KEY_ID association;							/* key of association entity */
UM_coord scalpt;
UM_transf scalmat;
UM_length scale;
UC_CLASS_ACTION action;							/* what to do flag */
{                        
	int i;
	UU_LOGICAL processed;						/* key processed flag */
	int status = UU_FAILURE;
   int class;										/* class number of associate */
	int rel_num;									/* relation number of association */
	int assoc_buf_len;							/* association buffer length */
	UU_KEY_ID assoc_buf[ASSOC_BUF_SIZE];	/* association key buffer */
	UC_METHOD function, ucu_3d_validate();

   uu_denter(UU_MTRC,(us,"uc_be_scale(%d,%x,%x,%g), action=%d",
		(*eptr).key, scalpt, scalmat, scale, action));

/*	-- get the relation number of the association and set up the 
		back-end Q. -- */

	if(association == 0)
	{
		rel_num = 0;
		Q_PUSH("uc_be_scale", (*eptr).key);
	}
	else
	{
		processed = UU_FALSE;
		status = um_retrieve_data_relnum(association, &rel_num);
		if(status != UU_SUCCESS)
			goto done;
	}

	class = CONVERT_REL_TO_CLASS((*eptr).rel_num);

/*	-- get the list of associations for this key -- */

	assoc_buf_len = ASSOC_BUF_SIZE;
	ur_retrieve_assoc_keys((*eptr).key, assoc_buf, &assoc_buf_len, UU_TRUE);

/*	-- only dispatch for UC_PERFORM and UC_PERMISSION -- */

	if(action == UC_TOUCH)
	{
		status = UU_SUCCESS;
		function = UC_UNDEFINED_METHOD;
		action = UC_PERFORM;
	}
	else
	{

/*		-- get the method from the class table and dispatch to it --*/

		function = ucu_3d_validate(UC_3d_descriptor, class, rel_num, UC_SCALE);

		if(function == UC_UNDEFINED_METHOD)
			status = UU_FAILURE;
		else
		{
			if(association == 0)
				status = (*function)(eptr, scalpt, scalmat, scale);
			else
				status = (*function)(eptr, association, scalpt, scalmat, scale);
		}
	}

/*	-- go notify all associations -- */

	BACKEND("uc_be_scale", 
		(uc_be_scale(eptr, assoc_buf[i], scalpt, scalmat, scale, action)));

done:

/*	-- Remove the associate from the list of keys that are processed on the
		back-end. -- */

	Q_POP("uc_be_scale");
	uu_dexitstatus("uc_be_scale", status);
	return(status);
}
