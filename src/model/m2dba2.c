/******************************************************************************
**	NAME: m2dba2.c
**		CONTAINS: modeling interface routines to UNIBASE
**			int um_update_transformation(key, tfmat)
**			int um_get_disp_attr(key, attrptr)
**			int um_create_geom(eptr, tfmat, attrptr)
**			int um_update_geom(eptr, tfmat)
**			int um_current_default_attr(rel_num, attrptr)
**       int um_cannot_undelete(key, undeleteok)
**       int um_can_undelete(key, undeleteok)
**
**    vp 12.8.95 - This is the second part of the old version of m2dba.c
**                 file.  See m2dba.c.
**
**	COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m2dba2.c , 25.2
**    DATE AND TIME OF LAST MODIFICATION
**       08/17/15 , 17:40:50
*******************************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "umoveb.h"
#include "uhep.h"
#include "mdcoord.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mattr.h"
#include "mcrv.h"
#include "msrf.h"
#include "msol.h"
#include "mdebug.h"
#include "mfort.h"
#include "ncl.h"
#include "nccs.h"
#include "nclvx.h"

/*
.....Moved definitions from m3ecomp.c so they will be defined in iges.
*/ 
UU_LOGICAL NCL_create_compcrv = UU_FALSE;
UU_LOGICAL NCL_copy_compcrv = UU_FALSE;

extern int NCL_ubcopy;
extern int NCL_multi_ent;

/*********************************************************************
**    E_FUNCTION     : int um_update_transformation(key, tfmat)
**       Update the transformation (TFMAT) for the entity with
**			the given KEY.
**    PARAMETERS   
**       INPUT  : 
**				key					key of entity to have its transformation
**										updated.
**				tfmat					transformation matrix.
**       OUTPUT :  
**				none
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_update_transformation(key, tfmat)
	int			key;
	UM_transf	tfmat;
	{
	int urstatus;
	int status;
	struct UM_transf_rec transfpacket;

	uu_denter(UU_MTRC,(us,"um_update_transformation(key:%d, tfmat:%x)",
					key, tfmat));
	status = UU_SUCCESS; /* assume success */
	transfpacket.key = key;
	transfpacket.rel_num = UM_TRANSFORM_REL;
	um_tftotf(tfmat, transfpacket.tfmat);
	urstatus = ur_update_transf(&transfpacket);
	if (urstatus != 0)
		{
		uu_uerror1(UM_MODEL, 165, key);
		/* message is: um_update_transformation: transformation not updated
		 * for entity with key = %d
		 */
		status = UU_FAILURE;
		} 
	uu_dexit; 
	return(status);
	}

/*********************************************************************
**    E_FUNCTION: int um_get_disp_attr(key, attrptr)
**				Get the display attributes for the modelling entity with UNIBASE
**				id of "key".
**			PARAMETERS   
**				INPUT: 
**					key
**				OUTPUT :  
**					attrptr				pointer to the attributes
**			RETURNS      : UU_SUCCESS if no problems encountered, UU_FAILURE
**								otherwise.
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
int
um_get_disp_attr(key, attrptr)
	int		key;
	struct UM_attrdata_rec  *attrptr;
	{
	int status;

	uu_denter(UU_MTRC,(us,"um_get_disp_attr(%d, ?)",key));
	status = UU_SUCCESS; /* assume success */
	attrptr->key = key; /* store key to entity for which attributes wanted */
	if (ur_retrieve_attr(attrptr) != 0)
		status = UU_FAILURE; 
	uu_dexit;
	return(status);
	}


/*********************************************************************
**    E_FUNCTION :  int um_create_geom(eptr, tfmat, attrptr)
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
int um_create_geom(eptr, tfmat, attrptr)
struct UM_entitydatabag *eptr;
UM_transf tfmat;
struct UM_attrdata_rec *attrptr;

{
	struct UM_transf_rec transpacket;
	int status;
	int geom_class;
	int rel_num, isub;
	UU_LOGICAL store_wf;
	UM_int2 nclstatus=0;
	char label[NCL_MAX_LABEL];
	struct UM_point_rec *e1;
	struct UM_surfattr_rec sfattr;

	uu_denter(UU_MTRC,(us,"um_create_geom(rel_num=%d, tfmat:%x, attrptr:%x)", 
							eptr->rel_num, tfmat, attrptr));

/*
.....Changed all 'strcpy(label...' to 'strncpy(label,...,NCL_MAX_LABEL)'
.....Because some routines apparently do not NULL terminate
.....the label string in 'eptr'.  Midtrim for example.
.....Bobby  -  11/26/96
*/
	e1 = (struct UM_point_rec *) eptr;
	rel_num = e1->rel_num;
	strncpy (label,e1->label,NCL_MAX_LABEL);
	status = UU_SUCCESS; /* assume success */
	store_wf = UU_FALSE;

	/* initialize label field */
	geom_class = uc_super_class(eptr->rel_num);

	switch (geom_class)
	{
	case UM_POINT_CLASS:
	case UM_CURVE_CLASS:
	case UM_SURFACE_CLASS:
	case UM_SOLID_CLASS:
		if((*eptr).rel_num == UM_POLY_REL) break;
		else if (NCL_create_compcrv && NCL_copy_compcrv &&
		        (rel_num == UM_COMPCRV_REL || rel_num == NCL_NETSF_REL))
			break;
		{
			struct UM_point_rec *e;

			e = (struct UM_point_rec *) eptr;
			rel_num = e->rel_num;

			if ((NCL_create_compcrv || NCL_copy_compcrv) && 
			    (*eptr).rel_num != UM_COMPCRV_REL)
			{
				strcpy(e->label,"@UN    ");
			}
			else
			{
/*
...Don't create labels if copying from unibase to unibase
*/
				if (NCL_ubcopy == 0)
				{
					e->key = 0;
					ncl_label_wf(rel_num, e->label, &e->subscr, e->key, &nclstatus);
				}
/*
...Don't need store in ranfile if copying to secondary unibase
*/
				if (NCL_ubcopy != 2) store_wf = UU_TRUE;
			}
			strncpy (label,e->label,NCL_MAX_LABEL);
			if (NCL_multi_ent == 1)	e1 = e;
		}
		break;
	default:
		break;
	}

	/* initialize entity specific display attributes with current default
		values */
	if (attrptr == UM_CURRENT_ATTR)
	{
		switch (geom_class)
		{
		case UM_CURVE_CLASS:
			{
				struct UM_line_rec *e;
				e = (struct UM_line_rec *) eptr;
			}
			break;
		case UM_SURFACE_CLASS:
			if((*eptr).rel_num == UM_POLY_REL) break;
			break;
		default:
			break;
		}
	}

	/* create master tuple in UNIBASE */
	if (ncl_create_data(eptr) != 0)
	{
		uu_uerror1(UM_MODEL, 157, eptr->rel_num);
		/* message is: um_create_geom: entity not stored, rel_num = %d */
		status = UU_FAILURE;
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
			 * that has key = %d 
			 */
			status = UU_FAILURE;
			goto failed;
		}
	}

	/* initialize attributes if current attributes are not desired */
	if (attrptr != UM_CURRENT_ATTR)
	{
		attrptr->key = eptr->key;
		if (ur_update_attr(attrptr) != 0)
		{
			status = UU_FAILURE;
			goto failed;
		}
	}
	else
	{
/*
.....Solid attributes
*/
		if (eptr->rel_num == UM_SOLID_REL)
		{
			ncl_init_surf_attr(eptr->key,&sfattr,0);
		}
		else if (ncl_create_surfattr(eptr->rel_num))
		{
			ncl_init_surf_attr(eptr->key,&sfattr,NCLI_SURF);
		}
/*
.....Standard modelling attributes
*/
		else
		{
			/* UM_attr.key = eptr->key; */
			ur_put_attrmdl_key(eptr->key);
			/* UM_attr.rel_num = UM_ATTR_REL; */
			ur_put_attrmdl_rel_num(UM_ATTR_REL);
			if (ur_update_attr(&UM_attrmdl) != 0)
			{
				status = UU_FAILURE;
				goto failed;
			}
		}
	}

	/* set the view that this piece of geometry is in */
	ur_update_view_key(eptr->key, ur_get_dispattr_view_in());

/*
... Store in 'ranfile'
...  vp 4/25/94 added flag to store differenly for loadu
...  and get command 
*/
	if (store_wf)
	{
		if (NCL_ubcopy == 1 || NCL_multi_ent == 1)
		{
			isub = ncl_get_subscript (e1);
			status = ncl_store_wf2(eptr->key,rel_num,label,isub);
		}
		else if (NCL_ubcopy == 0)
			status = ncl_store_wf1(eptr->key);
	}

failed:
	uu_dexit;
	return(status);
	}


/**************************************************************************
**		FUNCTION: int um_current_default_attr(rel_num, attrptr)
**			This function returns the current default attributes (ATTRPTR)
**			for a modelling relation (REL_NUM).
**    PARAMETERS   
**       INPUT  : 
**         rel_num		modelling relation number for which attributes
**								are to be retrieved.
**       OUTPUT :  
**         attrptr		pointer to the filled in attribute bundle.
**    RETURNS:
**			UU_SUCCESS if no problems encountered
**			UU_FAILURE otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_current_default_attr(rel_num, attrptr)
	int rel_num;
	struct UM_attrdata_rec *attrptr;
	{
	int len1, len2;
	int status;

	uu_denter(UU_MTRC,(us,"um_current_default_attr(rel_num:%d,attrptr:%x)",
					rel_num, attrptr));

	status = UU_FAILURE;

	if (!um_isvalid_relation(rel_num))
		{
		uu_uerror2(UM_MODEL, 223, rel_num, "um_current_default_attr");
		/* Invalid modelling relation: %d, (%s) */
		goto done;
		}

	/* UM_attr.rel_num = UM_ATTR_REL; */
	ur_put_attrmdl_rel_num(UM_ATTR_REL); /*put "UM_ATTR_REL" in UM_attrmdl.rel*/
	len1 = sizeof(UM_attrmdl);
	len2 = sizeof(struct UM_attrdata_rec);
	if (len1 > len2)
		{
		uu_uerror1(UM_MODEL, 222, "um_current_default_attr");
		/* Can't copy default attributes, (%s). */
		goto done;
		}
	uu_move_byte(&UM_attrmdl, attrptr, len1);
	status = UU_SUCCESS;
	
done:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION : int um_update_geom(eptr, tfmat) 
**			Update all of the data defining an entity (EPTR) and its
**			transformation. If the transformation is UU_NULL, the
**			identity matrix (UM_idmat) will be stored; otherwise the
**			given matrix will be stored.
**		INPUT  : 
**       eptr			pointer to the updated entity record.
**			tfmat			updated transformation matrix.
**		OUTPUT :  
**         none
**		RETURNS :
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_update_geom(eptr, tfmat)
	struct UM_entitydatabag *eptr;
	UM_transf tfmat;

	{
	int status;
	static struct UM_transf_rec transfpacket;

	uu_denter(UU_MTRC,(us,"um_update_geom(key:%d,tfmat:%x)", eptr->key, tfmat));
	status = UU_SUCCESS;
	if (ur_update_data(eptr) != 0)
		{
		uu_uerror1(UM_MODEL, 159, eptr->key);
		/* message is: um_update_geom: can't update entity that has key = %d */
		status = UU_FAILURE;
		}
	else 
		{
		if ((tfmat == UM_DEFAULT_TF) && (ur_has_default_transf(eptr->key)))
			goto done;
		/* else transf has potentially changed; so store new transf */
		transfpacket.key = eptr->key;
		transfpacket.rel_num = UM_TRANSFORM_REL;
		if (tfmat != UM_DEFAULT_TF)
			um_tftotf(tfmat, transfpacket.tfmat);	
		else /* tfmat is the default transformation */
			um_tftotf(UM_idmat, transfpacket.tfmat);
		if (ur_update_transf(&transfpacket) != 0)
			{
			uu_uerror1(UM_MODEL, 160, eptr->key);
			/* message is: um_update_geom: can't update transform for entity
			 * that has key = %d 
			 */
			status = UU_FAILURE;
			}
		}
done:;
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_can_undelete(key, undeleteok)
**       This routine will set the UNDELETEOK flag to UU_TRUE regardless
**			of the key.
**    PARAMETERS   
**       INPUT  : 
**				key				key of composite curve
**       OUTPUT :  
**          undeleteok		UU_TRUE => can be undeleted
**									UU_FALSE => can not be undeleted
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_can_undelete(key, undeleteok)
	UU_KEY_ID key;
	UU_LOGICAL *undeleteok;

	{
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"um_can_undelete(key:%d)",key));

	*undeleteok = UU_TRUE;

done:;
	uu_dexitstatus("um_can_undelete", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_cannot_undelete(key, undeleteok)
**       This routine will set the UNDELETEOK flag to UU_FALSE regardless
**			of the key.
**    PARAMETERS   
**       INPUT  : 
**				key				key of composite curve
**       OUTPUT :  
**          undeleteok		UU_TRUE => can be undeleted
**									UU_FALSE => can not be undeleted
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_cannot_undelete(key, undeleteok)
	UU_KEY_ID key;
	UU_LOGICAL *undeleteok;

	{
	int status = UU_SUCCESS;

	uu_denter(UU_MTRC,(us,"um_cannot_undelete(key:%d)",key));

	*undeleteok = UU_FALSE;

done:;
	uu_dexitstatus("um_cannot_undelete", status);
	return (status);
	}
