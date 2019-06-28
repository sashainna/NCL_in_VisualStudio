/*********************************************************************
**    NAME:  beimanip.c
**       CONTAINS:
**				ub_del_syminstance
**				ub_transl_syminstance
**				ub_rot_syminstance
**				ub_scale_syminstance
**				ub_copy_syminstance
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       beimanip.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:11:03
*********************************************************************/
#include "usysdef.h"	/* for UU_REAL, etc. */
#include "uhep.h"    /* for error system */
#include "udebug.h"	/* for debugging trace facility */
#include "class.h"	/* for "UC_" data types */
#include "mdcoord.h" /* for UM_vector, etc */
#include "mdrel.h"	/* for define relation numbers */
#include "umessages.h"/* for association update messages */
#include "r1emacro.h"	/* for ur1_touchAssoc */
#include "r1emsgpk.h"	/* for UR_messagePacket */
#include "bsym.h"
#include "mfort.h"
#include "nclfc.h"

#define TRACE UU_FALSE	/* for debugging only */
/*********************************************************************
**    E_FUNCTION : int ub_del_syminstance(key) 
** 			Deletes a symbol instance from UNIBASE and all its subentities.
**				Also deletes from DIGS.
**    PARAMETERS   
**       INPUT  : 
**          key		UNIBASE key to the instance to be deleted.
**       OUTPUT :  	none.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_del_syminstance(key)
	UU_KEY_ID key;
{
	struct UB_instance_rec sym;
	UU_LOGICAL ok;
	UR_messagePacket msgPacket;
	int status = UU_SUCCESS;/* return status; either UU_SUCCESS or UB_FAILURE */
	uu_denter(UU_BTRC, (us, "ub_del_syminstance(key:%d)", key));
	
	/* fill-in message packet for any associations referencing the entities
	 * being deleted. */
	if (ur1_initMsgPacket(&msgPacket, UU_DELETE, 0 /* nbr data packets */)
			!= UU_SUCCESS) goto failed;

	sym.key = key;
	if (ub_retrieve_sym(&sym, sizeof(sym)) != UU_SUCCESS)
		goto failed;

	UR1_UPDATE_ME(&sym, &msgPacket, 1, UU_REC_CHANGE, 
			ubi_deleteInstance(&sym,&msgPacket), &ok, &status);
	if ((!ok) || (status != UU_SUCCESS)) goto failed;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexitstatus("ub_del_syminstance",status);
	return(status);
}	

/*********************************************************************
**    E_FUNCTION: int ubi_deleteInstance(symptr,msgPacketptr)
**			This function deletes the symbol instance pointed to by "symptr".	
**    PARAMETERS   
**       INPUT: 
**				symptr			Pointer to the symbol instance to be deleted.
**				msgPacketptr	Pointer to the message packet containing the
**									delete message.
**       OUTPUT: none. 
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubi_deleteInstance(symptr,msgPacketptr)
	struct UB_instance_rec *symptr;
	UR_messagePacket *msgPacketptr;
{
	int dsegid, dummy, i, status = UU_SUCCESS;
	uu_denter(UU_BTRC,(us,"ubi_deleteInstance(symptr>ky:%d,msg:%s)",
			symptr->key, uu_msg(UR1_GET_MSG(msgPacketptr))));

	if (ub_retrieve_disp_segid(symptr->key,&dsegid) != UU_SUCCESS)
		goto failed;
	if (dsegid >= 0) uv_delsegs(dsegid);

#if (TRACE)
	ubi_pscroll("INSTANCE TO BE DELETED");
	if (ub_print_sym(symptr) != UU_SUCCESS) goto failed;
#endif
	/* delete instance record from master's list */
	dummy = 0;
	if (ubi_update_inst_data_in_msym(symptr, UU_NULL, UB_UNKNOWN, dummy) 
				!= UU_SUCCESS) goto failed;

	/* delete instance */
	if (ur_delete_all1(symptr->key,msgPacketptr,1) != 0)
	{
		uu_uerror2(UB_SYMBOL, 35, symptr->key, "ub_del_syminstance");
		/* error message: unable to delete symbol instance with key=%d, (%s). */
		goto failed;
	}

	/* now delete instance geometry */
	for (i=0; i<symptr->no_geom; i++)
		if (symptr->geom[i] > 0)
		if (ur_delete_all1(symptr->geom[i],msgPacketptr,1) != 0)
		{
			uu_uerror2(UB_SYMBOL, 34, symptr->geom[i], "ub_del_syminstance");
			/* error message: unable to delete geometry with key=%d, (%s). */
			goto failed;
		}

	/* now delete snap nodes */
	for (i=0; i<symptr->no_snap_nod; i++)
		if (symptr->snap_nod[i].snap_key > 0)
		if (ur_delete_all1(symptr->snap_nod[i].snap_key,msgPacketptr,1) != 0)
		{
			uu_uerror2(UB_SYMBOL,51,symptr->snap_nod[i].snap_key,
						"ub_del_syminstance");
			/* error message: Unable to delete snap node with key=%d, (%s). */
			goto failed;
		}

	/* now delete text nodes */
	for (i=0; i<symptr->no_text_nod; i++)
		if (symptr->text_nod[i].text_key > 0)
		if (ur_delete_all1(symptr->text_nod[i].text_key,msgPacketptr,1) != 0)
		{
			uu_uerror2
				(UB_SYMBOL, 52, symptr->text_nod[i].text_key, "ub_del_syminstance");
			/* error message: Unable to delete text node with key=%d, (%s). */
			goto failed;
		}
	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexitstatus("ubi_deleteInstance",status);
	return(status);
}	

/*********************************************************************
**    E_FUNCTION :  int ub_transl_syminstance(symptr, offset)
**       This function translates a symbol instance.
**    PARAMETERS   
**       INPUT  : 
**				symptr			pointer to the symbol instance to be deleted.
**				offset			vector to translate instance by.
**       OUTPUT : none. 
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_transl_syminstance(symptr, offset)
	struct UB_instance_rec *symptr;
	UU_REAL offset[3];
{
	UM_transf temptfmat;
	UU_LOGICAL ok;
	UR_messagePacket msgPacket;
	int status = UU_SUCCESS;	
	uu_denter(UU_BTRC, (us, "ub_transl_syminstance(key:%d,%x)", symptr->key,
								offset));

	/* get transformation that will only translate */
	um_identtf(temptfmat);
	um_vcplvc(offset, temptfmat[3], temptfmat[3]);

	/* construct message packet(s) indicating what is to be done */
	if (ur1_initMsgPacket(&msgPacket, UU_TRANSLATE, 1/*nbr data packs*/) 
			!= UU_SUCCESS) goto failed;
	if (ur1_addData2MsgPacket(&msgPacket,1,temptfmat,sizeof(UM_transf)) 
			!= UU_SUCCESS) goto failed;

	UR1_UPDATE_ME(symptr, &msgPacket, 1, UU_NO_REC_CHANGE, 
					ubi_try2DoTransf(symptr,temptfmat), &ok, &status);
	if ((!ok) || (status != UU_SUCCESS)) goto failed;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexitstatus("ub_transl_syminstance",status);
	return(status);
}	

/*********************************************************************
**    E_FUNCTION :int ub_rot_syminstance(eptr, pt, dir, angle, rotmat)
**      This function rotates the symbol instance pointed to by "eptr";
**		  the transformation matrix corresponding to the rotation is 
**		  pointed to by "rotmat". We assume the symbol instance record
**		  has already been retrieved from UNIBASE.
**		  NOTE, WE ASSUME THE INSTANCE TRANSFORMATION IS THE IDENTITY.
**    PARAMETERS   
**       INPUT  : 
**      		eptr		pointer to the symbol instance.
**				pt 		point on rotation axis.
**				dir		direction vector of axis.
**				angle		the angle through which to rotate.
**				rotmat	pointer to the rotation transformation.	
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UU_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_rot_syminstance(eptr, pt, dir, angle, rotmat)
	struct UB_instance_rec *eptr;
	UM_coord pt;
	UM_vector dir;
	UM_angle angle;
	UM_transf rotmat;
{
	UR_messagePacket msgPacket;
	UU_LOGICAL ok;
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC,(us,"ub_rot_syminstance(eptr>key:%d,%x,%x,angle:%g,%x)",
								eptr->key, pt, dir, angle, rotmat));

	/* construct message packet(s) indicating what is to be done */
	if (ur1_initMsgPacket(&msgPacket, UU_ROTATE, 1) != UU_SUCCESS) 
			goto failed;
	if (ur1_addData2MsgPacket(&msgPacket, 1, rotmat, sizeof(UM_transf)) 
			!= UU_SUCCESS) goto failed;

	UR1_UPDATE_ME(eptr,&msgPacket,1,UU_NO_REC_CHANGE,
			ubi_try2DoTransf(eptr, rotmat), &ok, &status);
	if ((!ok) || (status != UU_SUCCESS)) goto failed;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexitstatus("ub_rot_syminstance",status);
	return(status);
}	

/*********************************************************************
**    E_FUNCTION :  int ub_scale_syminstance(eptr, scalpt, scalmat, scale)
**       This function scales a symbol instance.
**			NOTE, WE ASSUME INSTANCES HAVE IDENTITY TRANSFORMS.
**    PARAMETERS   
**       INPUT  : 
**          eptr		pointer to the symbol instance to have its record
**							modified to reflect the scaling.
**				scalpt	point on scale plane 
**				scalmat	scaling matrice 
**				scale;	scale ratio 
**       OUTPUT :  none.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS : 
**    WARNINGS     : none
*********************************************************************/
int ub_scale_syminstance(eptr, scalpt, scalmat, scale)
	struct UB_instance_rec *eptr;
			/* Entity to be scaled, can contain any modelling entity */
	UU_REAL	scalpt[3];				/* point on scale plane */
	UM_transf scalmat;				/* scaling matrice */
	UU_REAL	scale;					/* scale ratio */
{
	UR_messagePacket msgPacket;
	UU_LOGICAL ok;
	int status = UU_SUCCESS;
	uu_denter(UU_BTRC,(us,"ub_scale_syminstance(eptr->key:%d,%x,%x,scale:%g)",
							eptr->key, scalpt, scalmat, scale));

	/* construct message packet(s) indicating what is to be done */
	if (ur1_initMsgPacket(&msgPacket, UU_SCALE, 1) != UU_SUCCESS) 
			goto failed;
	if (ur1_addData2MsgPacket(&msgPacket, 1, scalmat, sizeof(UM_transf)) 
			!= UU_SUCCESS) goto failed;
#if (TRACE)
	ur1_printTfmat(-1,msgPacket.dataPacket->dataptr,"from beimanip");
#endif

	UR1_UPDATE_ME(eptr,&msgPacket,1,UU_NO_REC_CHANGE,
			ubi_try2DoTransf(eptr, scalmat), &ok, &status);
	if ((!ok) || (status != UU_SUCCESS)) goto failed;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexitstatus("ub_scale_syminstance",status);
	return(status);
}	

/*********************************************************************
**    E_FUNCTION :  int ub_copy_syminstance(inst2copyptr, newinstptr, bagsize)
**       This function copies the symbol instance pointed to by "inst2copyptr"
**			into the bag pointed to by "newinstptr".
**    PARAMETERS   
**       INPUT  : 
**			inst2copyptr	Pointer to the symbol instance to be copied.
**			newinstptr		Pointer to the storage in which the symbol instance
**								is to be copied into.
**			bagsize			Size of the data bags (both the same size).
**       OUTPUT :  
**          newinstptr			Pointer to the copy of the instance symbol.
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ub_copy_syminstance(inst2copyptr, newinstptr, bagsize)
	struct UB_instance_rec *inst2copyptr;
	struct UB_instance_rec *newinstptr;
	int bagsize;
{
	UU_KEY_ID masterkey;
	int indx;	/* index of instance data packet in instance list of master */
	struct UC_entitydatabag *subentptr;
	struct UC_entitydatabag *copyptr;
	struct UB_symattr_rec attr;
	UM_transf tfmat;
	struct UB_inst_rec instdata;
	int i, status = UU_SUCCESS;
	UM_int2 iunk;
	uu_denter(UU_BTRC,(us,
		"ub_copy_syminstance(inst2copyptr->key:%d,newinstptr:%x,size:%d)",
								inst2copyptr->key, newinstptr, bagsize));

	if (ubi_retrieve_inst_data_in_msym(inst2copyptr,&instdata,&masterkey,&indx)
			!= UU_SUCCESS) goto failed;

	/* get storage for subentities to copy; note, must malloc; they are too
	 * big to put on execution stack. */
	subentptr = (struct UC_entitydatabag *) 
						uu_malloc(sizeof(struct UC_entitydatabag));
	copyptr = (struct UC_entitydatabag *) 
						uu_malloc(sizeof(struct UC_entitydatabag));

	newinstptr->key = inst2copyptr->key;	/* put old key in "newinstptr" */

	/* retrieve new copy; note, this allocates enough storage */
	if (ub_retrieve_sym(newinstptr, bagsize) != UU_SUCCESS) goto failed;

	/* now copy the geometry for "newinstptr" */
	iunk = 1;
	stunlb(&iunk);
	for (i=0; i<newinstptr->no_geom; i++)
	{
		subentptr->key = newinstptr->geom[i];
		if (uc_retrieve_data(subentptr, sizeof(struct UC_entitydatabag)) 
				!= UU_SUCCESS) goto failed;	
		/* create new copy of the geometry */
		if (uc_copy(subentptr, copyptr, sizeof(struct UC_entitydatabag)) 
				!= UU_SUCCESS)	goto failed;
		newinstptr->geom[i] = copyptr->key;
	}
	stunlb(&iunk);
	for (i=0; i<newinstptr->no_snap_nod; i++) /* copy snap nodes */
	{
		subentptr->key = newinstptr->snap_nod[i].snap_key;
		if (uc_retrieve_data(subentptr, sizeof(struct UC_entitydatabag))
			!= UU_SUCCESS) goto failed;
		if (uc_copy(subentptr, copyptr, sizeof(struct UC_entitydatabag)) 
			!= UU_SUCCESS)	goto failed;
		newinstptr->snap_nod[i].snap_key = copyptr->key;
	}
	for (i=0; i<newinstptr->no_text_nod; i++) /* copy text nodes */
	{
		subentptr->key = newinstptr->text_nod[i].text_key;
		if (uc_retrieve_data(subentptr, sizeof(struct UC_entitydatabag))
			!= UU_SUCCESS) goto failed;
		if (uc_copy(subentptr, copyptr, sizeof(struct UC_entitydatabag)) 
			!= UU_SUCCESS)	goto failed;
		newinstptr->text_nod[i].text_key = copyptr->key;
	}

	/*** add copy to UNIBASE ***/
	if (ub_get_sym_attr(inst2copyptr->key, &attr) != UU_SUCCESS) goto failed;
	if (ub_get_sym_transf(inst2copyptr->key, tfmat) != UU_SUCCESS) goto failed;
	
	/* create a copy of instance */
	if (ub_create_instance_tuple(masterkey,newinstptr, tfmat, &attr, &instdata) 
			!= UU_SUCCESS)	goto failed;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_free(subentptr);
	uu_free(copyptr);
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION: int ubi_try2DoTransf(eptr, newtfmat)
**			This function attempts to transform a symbol instance.
**			Note, we assume the transform only modifies subentities, not
**			the instance record itself.
**    PARAMETERS   
**       INPUT: 
**				eptr			Instance to be transformed.
**				newtfmat		Transformation to be applied.
**       OUTPUT: none. 
**    RETURNS: UU_SUCCESS if no problems encountered, UB_FAILURE otherwise.
**    SIDE EFFECTS: none
**    WARNINGS: none
*********************************************************************/
int ubi_try2DoTransf(eptr, newtfmat)
	struct UB_instance_rec *eptr;
	UM_transf newtfmat;
{
	UU_KEY_ID masterKey;
	struct UB_inst_rec instdata;
	UM_transf tfmat;
	int indx, status = UU_SUCCESS;
	uu_denter(UU_BTRC,(us,"ubi_try2DoTransf(eptr>ky:%d,newtfmat)",
					eptr->key));

	if (ub_get_sym_transf(eptr->key, tfmat) != UU_SUCCESS) goto failed;

	um_tftmtf(tfmat, newtfmat, tfmat);
	if (ub_transform_sym(eptr, tfmat, UU_TRUE) != UU_SUCCESS) goto failed;

	/*** now update master symbol instance data packet ***/

	if (ubi_retrieve_inst_data_in_msym(eptr,&instdata,&masterKey,&indx)
			!= UU_SUCCESS) goto failed;
	um_tftmtf(instdata.tfmat, newtfmat, instdata.tfmat);

	if (ubi_update_inst_data_in_msym(eptr, &instdata, masterKey, indx)
			!= UU_SUCCESS) goto failed;

	goto done;
failed: status = UB_FAILURE;
	UB_IF_FAILURE_PRINT_IT
done:;
	uu_dexitstatus("ubi_try2DoTransf",status);
	return(status);
}
