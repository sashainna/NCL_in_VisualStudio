/*********************************************************************
**    NAME         :  n2dba.c 
**		CONTAINS: modeling interface routines to UNIBASE
**			int ncl_create_geom_drw(eptr, tfmat, attrptr)
**          int ncl_create_aggeo(eptr,tfmat, attrptr)
**    COPYRIGHT 1989 (c) MILLS DATA SYSTEMS CO Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       n2dba.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:20
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "mdcoord.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mattr.h"
#include "mcrv.h"
#include "msrf.h"
#include "msol.h"
#include "mdgenent.h"
#include "mdebug.h"
#include "uhep.h"
#include "mfort.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION :  int ncl_create_geom_drw(eptr, tfmat, attrptr)
**			Create a UNIBASE master tuple for the specified entity (EPTR).
**			If the transformation matrix (TFMAT) is UU_NULL, associate the
**			current default transformation with the entity; otherwise, 
**			use the given transformation. If the attribute data (ATTRPTR)
**			is UU_NULL, associate the current default attributes with the
**			entity; otherwise associate the given attributes. Finally,
**			set the view(s) that the entity is to be displayed in.
**
**     Implemented for drawing subsystem.  To project geometry in the
**     drawing without changing their labels.  kathy
**
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
ncl_create_geom_drw(eptr, tfmat, attrptr)
struct UM_entitydatabag *eptr;
UM_transf tfmat;
struct UM_attrdata_rec *attrptr;
	{
	struct UM_transf_rec transpacket;
	int status;
	int geom_class;
	int rel_num;
	UU_LOGICAL store_wf;
    char label[100];
    int isub;
	UM_int2 nclstatus=0;

	uu_denter(UU_MTRC,(us,"ncl_create_geom_drw(rel_num=%d, tfmat:%x, attrptr:%x)", 
							eptr->rel_num, tfmat, attrptr));

	status = UU_SUCCESS; /* assume success */

	/* initialize entity specific display attributes with current default
		values */
	store_wf = UU_FALSE;
	geom_class = uc_super_class(eptr->rel_num);
	switch (geom_class)
		{
		case UM_POINT_CLASS:
		case UM_CURVE_CLASS:
		case UM_SURFACE_CLASS:
		case UM_SOLID_CLASS:
			{
			struct UM_point_rec *e;
			e = (struct UM_point_rec *) eptr;
			rel_num = eptr->rel_num;
/*
.....Do not create new label for drawing geometry
.....Bobby  -  2/24/94
*/
/*			ncl_label_wf(rel_num, label, &isub, eptr->key, &nclstatus);*/

			store_wf = UU_TRUE;
			}
			break;
		default:
			break;
		}
	if (attrptr == UM_CURRENT_ATTR)
		{
		switch (geom_class)
			{
			case UM_POINT_CLASS:
				{
				struct UM_point_rec *e;
				e = (struct UM_point_rec *) eptr;
				e->markertype = UM_ptattr.markertype;
				e->snap_node = UM_ptattr.snap_node;
				}
				break;
			case UM_CURVE_CLASS:
				{
				struct UM_line_rec *e;
				e = (struct UM_line_rec *) eptr;
				}
				break;
			case UM_SURFACE_CLASS:
				{
				struct UM_surfattr_rec *e;
				e = (struct UM_surfattr_rec *)attrptr;
				e->numupaths = UM_srfattr.numupaths;
				e->numvpaths = UM_srfattr.numvpaths;
				e->ptsperucrv = UM_srfattr.ptsperucrv;
				e->ptspervcrv = UM_srfattr.ptspervcrv;
				e->material = UM_srfattr.material;
				}
				break;
			case UM_SOLID_CLASS:
				{
				struct UM_body_rec *e;
				e = (struct UM_body_rec *) eptr;
				e->pitch = UM_solattr.pitch;
				}
				break;
			default:
				break;
			}
		}

	/* create master tuple in UNIBASE */
	if (ur_create_data(eptr) != 0)
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
	/* set the view that this piece of geometry is in */
	ur_update_view_key(eptr->key, ur_get_dispattr_view_in());
       /* Store in 'ranfile' */
/*
.....Do not store drawing geo in ranfile
.....Bobby  -  2/24/94
*/
/*	if (store_wf) 
      status = ncl_store_wf1(eptr->key);*/

failed:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_create_aggeo(eptr,tfmat, attrptr)
**       create ag curve.
**    PARAMETERS   
**       INPUT  : 
**          eptr          pointer to entity.
**          tfmat         transformation.
**          attrptr       pointer to attributes.
**       OUTPUT :  
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_create_aggeo(eptr,tfmat, attrptr)
struct UM_agcrv_rec *eptr;
UM_transf tfmat;
struct UM_attrdata_rec *attrptr;
   {
   int status, iclass;
   UM_int2 nclstatus=0;  

   uu_denter(UU_MTRC, (us,"ncl_create_aggeo()"));
   status = ncl_label_wf(eptr->rel_num, eptr->label, &eptr->subscr, eptr->key, &nclstatus);
   if (status == 0) status = uc_create_mtuple_data(eptr, tfmat, attrptr);
   if (status == 0) status = ncl_store_wf1(eptr->key);
/*
....Set closdinu flag
*/
   iclass = uc_super_class (eptr->rel_num);
   if (iclass == UM_CURVE_CLASS) crvcls(&eptr->key);
   else if (iclass == UM_SURFACE_CLASS) srfcls (&eptr->key);

   uu_dexit;
   return(status);
   }
