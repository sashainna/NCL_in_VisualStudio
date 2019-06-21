/*********************************************************************
**    NAME         :  m3ept.c
**       CONTAINS:
**			int um_create_pt(eptr, tfmat, attrptr)
**			int um_c1_pt(point, pptr)
**			int um_drw1_pt(ptr,tfmat,attrptr)
**			int um_p1_pt(ptr)
**			int um_cp1_copypt(e1,e2, bagsize)
**			int um_tr1_tranpt(eptr,offset)
**			int um_tf1_tranfpt(eptr,tranfmat,store)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m3ept.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:54
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "ginq.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mattr.h"
#include "mattr.h"
#include "mcrv.h"
#include "mdebug.h"
#include "mfort.h"

/*********************************************************************
**    E_FUNCTION :  int um_create_pt(eptr, tfmat, attrptr)
**			Create a UNIBASE master tuple for the specified point (EPTR).
**			If the transformation matrix (TFMAT) is UU_NULL, associate the
**			current default transformation with the entity; otherwise, 
**			use the given transformation. If the attribute data (ATTRPTR)
**			is UU_NULL, associate the current default attributes with the
**			entity; otherwise associate the given attributes. Finally,
**			set the view(s) that the entity is to be displayed in.
**    PARAMETERS   
**       INPUT  : 
**          eptr				pointer to completed point entity record to be 
**									stored in UNIBASE.
**			 	tfmat				transformation matrix (if not NULL).
**				attrptr			pointer to the attribute bundle (if not NULL).
**       OUTPUT :  
**          output
**			RETURNS      : UU_SUCCESS if no problems encountered, UU_FAILURE
**								otherwise.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_create_pt1(eptr, tfmat, attrptr)
	struct UM_point_rec *eptr;
	UM_transf tfmat;
	struct UM_attrdata_rec *attrptr;
	{
	struct UM_transf_rec transpacket;
	int status = UU_SUCCESS;
	int geom_class;
	UM_int2 nclstatus=0;

	uu_denter(UU_MTRC,(us,"um_create_pt(rel_num=%d, tfmat:%x, attrptr:%x)", 
							eptr->rel_num, tfmat, attrptr));

	eptr->key = 0;
/* initialize label field */
	ncl_label_wf(eptr->rel_num, eptr->label, &eptr->subscr, eptr->key, &nclstatus);

/* initialize entity specific display attributes with current default
		values */
	if (attrptr == UM_CURRENT_ATTR)
		{
		eptr->markertype = UM_ptattr.markertype;
		eptr->snap_node = UM_ptattr.snap_node;
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

	/*  Store in ranfile   */
	status = ncl_store_wf1(eptr->key);

failed:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_c1_pt(point, pptr)
**       Create a point entity record.
**    PARAMETERS   
**       INPUT  : 
**          point						coordinate of point
**       OUTPUT :  
**          pptr						point entity
**    RETURNS      : 
**			0 iff no error;
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c1_pt(point, pptr)
	UM_coord  point;
	struct UM_point_rec *pptr;

	{
	int status;

	uu_denter(UU_MTRC,(us,"um_c1_pt(pt=(%f,%f,%f))",point[0],point[1],
		point[2]));
	status = ur_setup_data(UM_POINT_REL, pptr, sizeof(struct UM_point_rec));
	if (status != 0) goto failed;

	/* MILLS- Initialize the LABEL and SUBSCRIPT field. */
	strcpy(pptr->label, "");
	pptr->subscr =0;

	um_vctovc(point, pptr->pt);

failed:
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_drw1_pt(ptr,tfmat,attrptr)
**       Display a point in the current open segment.
**    PARAMETERS   
**       INPUT  : 
**				ptr               pointer to point record
**				tfmat					transformation matrix (UM_DEFAULT_TF => identity)
**				attrptr				pointer to attribute record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_drw1_pt(ptr,tfmat,attrptr)
	struct  UM_point_rec  *ptr;
	UM_transf tfmat;
	struct UM_attrdata_rec *attrptr;

	{
	UM_coord pt;
	Gwpoint3 gpt;					/* markers to send to GKS */
	int markertype;				/* type of DIGGS marker */

	uu_denter(UU_MTRC,(us,"um_drw1_pt(key:%d,tfmat:%x,attrptr:%x)",
					ptr->key,tfmat,attrptr));

	gsmarkcolor(attrptr->color);
	markertype = gqmarktype();	/* get current marker type */

	/* added to put points in the drawing. kathy */
	if (ptr->markertype <=0 || ptr->markertype > 11)
		ptr->markertype=2;

	gsmarktype(ptr->markertype);
	um_cctmtf(ptr->pt, tfmat, pt);
	gpt.x = pt[0];
	gpt.y = pt[1];
	gpt.z = pt[2];
	gpolymarker3(1,&gpt);
	gsmarktype(markertype);	/* reset DIGGS marker type */
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_p1_pt(ptr)
**       Print the contents of a point record.
**    PARAMETERS   
**       INPUT  : 
**				ptr 	            pointer to point record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_p1_pt(ptr)
	struct  UM_point_rec  *ptr;
	{
	UM_transf tfmat;

	uu_denter(UU_MTRC,(us,"um_p1_pt(%8x)",ptr));
	sprintf(UM_sbuf, "POINT %d", ptr->key);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "label %7.7s", ptr->label);
	um_pscroll(UM_sbuf);
	um_p_ary(UM_PFLOAT, "pt", 3, ptr -> pt);
	um_p_ary(UM_PINT,"markertype",1,&ptr->markertype);
	um_p_ary(UM_PLOGICAL,"snap_node",1,&ptr->snap_node);
	um_get_transformation(ptr->key, tfmat);
	umi_print_transformation(tfmat);
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_cp1_copypt(e1,e2, bagsize)
**      Copy a point into a new point.   
**    PARAMETERS   
**       INPUT  : 
**				e1			pointer to entity to be copied
**				bagsize	size of storage for entity.
**       OUTPUT :  
**				e2       pointer to new entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_cp1_copypt(e1,e2, bagsize)
	struct UM_point_rec *e1;        /* The entity to copy */
	struct UM_point_rec *e2;        /* New entity */
	int bagsize;

	{
	struct UM_attrdata_rec attrbag;
	uu_denter(UU_MTRC,(us,"um_cp1_copypt(?)"));
                                            
	ur_setup_data(e1->rel_num, e2, bagsize);
	e2->markertype = e1->markertype;
	e2->snap_node = e1->snap_node;

	/* MILLS- Initialize the LABEL and SUBSCRIPT field. */
	strcpy(e2->label, "");
	e2->subscr = 0;
   um_vctovc(e1->pt, e2->pt);

	um_get_disp_attr(e1->key, &attrbag);
	um_create_pt1(e2, UM_DEFAULT_TF, &attrbag); 
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_tr1_tranpt(eptr,offset)
**      Translate the specified point from the "from" point to the "to" point.
**       description
**    PARAMETERS   
**       INPUT  : 
**				eptr		pointer to the  entity to be translated
**		      offset	vector by which to translate
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tr1_tranpt(eptr,offset)
	struct UM_point_rec *eptr;
	UM_transf offset;

	{
	uu_denter( UU_MTRC,(us,"um_tr1_tranpt(?,?)"));  
    
   um_vcplvc(eptr->pt, offset, eptr->pt);

	um_update_geom(eptr, UM_DEFAULT_TF);
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_tf1_tranfpt(eptr,tranfmat,store)
**      Transform a point by the given 4X3 matrix and update
**			UNIBASE iff store == UU_TRUE.
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be transformed
**          tranfmat     the 4x3 transformation matrix
**				store			 TRUE iff UNIBASE is to be updated here.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_tf1_tranfpt(eptr,tranfmat,store)
	struct UM_point_rec *eptr;
   UM_transf    tranfmat;
	UU_LOGICAL store;

	{                                     
   UM_coord    temppt;           /* Temporary point */

	uu_denter(UU_MTRC,(us,"um_tf1_tranfpt(key:%d,tfmat:%x,store:%d)",
						eptr->key,tranfmat,store));
    
   um_cctmtf(eptr->pt, tranfmat, eptr->pt);

	if (store)
		um_update_geom(eptr, UM_DEFAULT_TF);
	uu_dexit;
	return (UU_SUCCESS);
	}
