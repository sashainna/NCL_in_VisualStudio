
/*********************************************************************
**    NAME         :  atxtdba.c
**       CONTAINS:
**				ua_create_text(eptr,tfmat,attrptr)
**				ua_cp_ctext(e1,e2,bagsize)
**				ua_cp_text(e1,e2,bagsize)
**				ua_class_copy_ctext(e1,e2,bagsize)
**				ua_copy_ctext(e1,e2)
**				ua_tr_ctext(eptr,offset)
**				ua_tr_text(eptr,offset)
**				ua_tf_ctext(eptr,tranfmat,store)
**				ua_tf_text(eptr,tranfmat,store)
**				ua_tf_text1(eptr,tranfmat,store)
**				ua_scal_ctext(eptr,scalpt,scalmat,scale)
**				ua_scal_text(eptr,scalpt,scalmat,scale)
**				ua_get_text(eptr, databagsize)
**				ua_get_text1(eptr)
**				ua_mirror_ctext(eptr,mirrmat,store)
**				ua_mirror_text(eptr,mirrmat,store)
**				ua_delete_text(key)
**          ua_txt_to_drawing()
**       names of functions in file
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       atxtdba.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:41
*********************************************************************/

#include		"usysdef.h"
#include		"mdcoord.h"
#include		"udebug.h"
#include		"ulist.h"
#include		"class.h"
#include		"atext.h"
#include		"uhep.h"
#include		"mcrv.h"
#include		"mattr.h"
#include		"mdrel.h"
#include		"mdgenent.h"
#include		"mdattr.h"
#include		"mdraw.h"
#include		"nccs.h"
#include		"nclfc.h"
#include		"nclvx.h"

/*********************************************************************
**    I_FUNCTION :  ua_create_text(eptr,tfmat,attrptr)
**       Create a Unibase master tuple for the text entity, then 
**			associates the attributes with the entity. Finally, set the
**			view(s) that the entity is to be displayed.
**    PARAMETERS   
**       INPUT  : 
**          input
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int 
ua_create_text(eptr,tfmat,attrptr)
struct UA_txt_rec	*eptr;
UM_transf				tfmat;
struct UA_txtattr_rec	*attrptr;

{
	struct UM_transf_rec		transpacket;
	int	 status;

	uu_denter(UU_STRC,(us,"ua_create_text(tfmat=%x,attrptr=%x)",tfmat,attrptr));
	status = UU_SUCCESS;		/* assume success */

		/* create  master tuple in UNIBASE */
	if (ncl_create_data(eptr) != 0)
	  {
			/* ua_create_text: text entity not stored		*/
		status = UU_FAILURE;
		goto done;
	  }

		/* initialize transformation */
	if (tfmat != UM_DEFAULT_TF)
	  {
			/* only need to update transformation if not default */
		transpacket.key = eptr->key;
		transpacket.rel_num = UM_TRANSFORM_REL;
		um_tftotf(tfmat,transpacket.tfmat);
		if (ur_update_transf(&transpacket) != 0)
		  {
				/* ua_create_data: can't store transform for text that has key = */
			status = UU_FAILURE;
			goto done;
		  }
	  }

		 /* associate the attributes with the entity */
 	attrptr->key = eptr->key;
 	attrptr->rel_num = UA_TEXTATTR_REL;
	if (ur_update_attr(attrptr) != 0)
		status = UU_FAILURE;
done:
	uu_dexit;
	return(status);
}	/* ua_create_text */

/*********************************************************************
**    E_FUNCTION     : int ua_class_copy_ctext(e1,e2,bagsize)
**      Copy an anote.
**    PARAMETERS   
**       INPUT  : 
**				e1       pointer to entity to be copied
**				bagsize  entity structure size
**       OUTPUT :  
**				e2       pointer to new entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_class_copy_ctext(e1,e2,bagsize)
struct UA_txt_rec *e1;        /* The entity to copy */
struct UA_txt_rec *e2;        /* New entity */
int bagsize;
{
	int status;
	UM_int2 nclstatus;

	uu_denter(UU_STRC,(us,"ua_class_copy_ctext(%8x,%8x)",e1,e2));
	status = UU_SUCCESS;

	status = ncl_label_wf(UA_TEXT_REL, e2->label, &e2->subscr, 0, &nclstatus);
	if (status == UU_SUCCESS) status = ua_copy_ctext(e1, e2);
	if (status == UU_SUCCESS) status = ncl_store_wf1(e2->key);

	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int ua_copy_ctext(e1,e2)
**      Copy a text primitive and create new text entity in unibase.
**    PARAMETERS   
**       INPUT  : 
**				e1			pointer to entity to be copied
**				bagsize  entity structure size
**       OUTPUT :  
**				e2       pointer to new entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_copy_ctext(e1,e2)
struct UA_txt_rec *e1;        /* The entity to copy */
struct UA_txt_rec *e2;        /* New entity */
{
	struct UA_txtattr_rec attrptr;

	UM_int2 lfl_77;
	int status, isub, isze;
	char labl[NCL_MAX_LABEL];
	struct NCL_fixed_databag c1, c2;

	uu_denter(UU_STRC,(us,"ua_copy_ctext(%8x,%8x)",e1,e2));
	
	status = UU_SUCCESS;
	strncpy(labl,e2->label,NCL_MAX_LABEL);
	isub = e2->subscr;
/*
.....Setup text record
*/
	ua_init_txtrec(e2,&attrptr,&UA_txtattr,UU_FALSE);
	ur_setup_data(UA_TEXT_REL,e2,sizeof(struct UA_txt_rec));
	ur_setup_data(UA_TEXTATTR_REL,&attrptr,sizeof(struct UA_txtattr_rec));
	*e2 = *e1;
	e2->key = 0;

	strncpy(e2->label,labl,NCL_MAX_LABEL);
	e2->subscr = isub;

/*	e2->dx = e1->dx;
	e2->dy = e1->dy;
	e2->subtype = e1->subtype;
	e2->tangle = e1->tangle;
	um_vctovc(e1->position, e2->position);*/

	e2->no_tchar = 0;
	e2->tchar = NULL;
	e2->no_displst = 0;
	e2->displst = NULL;

	e2->arckey = NULLKEY; 
	
	if (e1->arckey != NULLKEY)
	{

		isze = sizeof(struct NCL_fixed_databag);

		c1.key = e1->arckey;
		c2.key = 0;
		status = ncl_retrieve_data (&c1, isze);
		if (c1.rel_num == UM_CIRCLE_REL)
		{
			lfl_77 = 1;
			stunlb (&lfl_77);
			if (status == UU_SUCCESS)
				status = uc_copy (&c1, &c2, isze);
			if (status == UU_SUCCESS)
			{
				ur_update_displayable(c2.key, UM_NEVERDISPLAYABLE);
				e2->arckey = c2.key;
			}
			stunlb (&lfl_77);
		}
	}

	if (status == UU_SUCCESS)
	{
		attrptr.key = e1->key; /* store key to entity for which attributes wanted */
		if (ur_retrieve_attr(&attrptr) != 0)
			status = UU_FAILURE; 
		else
			ua_create_text(e2,UM_DEFAULT_TF,&attrptr);
	}

	if (status == UU_SUCCESS)
	     status = ur_update_data_varlist (e2->key, 1, e1->tchar, 1, e1->no_tchar);
	if (status == UU_SUCCESS)
	     status = ur_update_data_varlist (e2->key, 2, e1->displst, 1, e1->no_displst);
	if (status == UU_SUCCESS)
		status = ncl_retrieve_data_fixed (e2);

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int ua_cp_ctext(e1,e2,bagsize)
**      Don't do anything if text is associated with another entity.
**    PARAMETERS   
**       INPUT  : 
**				e1			pointer to entity to be copied
**				bagsize  entity structure size
**       OUTPUT :  
**				e2       pointer to new entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_cp_ctext(e1,e2,bagsize)
struct  UA_txt_rec *e1;        /* The entity to copy */
struct  UA_txt_rec *e2;        /* New entity */
int		bagsize;						/* e2 structure size */

{
	int         status;

	uu_denter(UU_STRC,(us,"ua_cp_ctext(%8x,%8x)",e1,e2));
	status = UU_SUCCESS;

	if (e1->arckey == 0)		/* don't do anything for text on arc */
		status = ua_cp_text(e1,e2,bagsize);
	uu_dexit;
	return(status);
}	/* ua_cp_text */

/*********************************************************************
**    E_FUNCTION     : int ua_cp_text(e1,e2,bagsize)
**      Copy a text primitive and create new text entity in unibase.
**    PARAMETERS   
**       INPUT  : 
**				e1			pointer to entity to be copied
**				bagsize  entity structure size
**       OUTPUT :  
**				e2       pointer to new entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_cp_text(e1,e2,bagsize)
struct  UA_txt_rec *e1;        /* The entity to copy */
struct  UA_txt_rec *e2;        /* New entity */
int		bagsize;						/* e2 structure size */

{
	struct UA_txtattr_rec attrptr;
	int         status=UU_SUCCESS;

	uu_denter(UU_STRC,(us,"ua_cp_text(%8x,%8x)",e1,e2));
	uu_dprint(UU_STRC,(us,"e1.key=%x,e1.rel=%d",e1->key,e1->rel_num));

	if (bagsize < (sizeof(struct UA_txt_rec)))	/* entity size is too small */
		goto failed;
	ur_setup_data(e1->rel_num,e2,bagsize);
	e2->dx = e1->dx;
	e2->dy = e1->dy;
	e2->subtype = e1->subtype;
	e2->tangle = e1->tangle;
	e2->arckey = e1->arckey; 

	um_vctovc(e1->position, e2->position);
 	e2->no_tchar = e1->no_tchar;
 	strcpy(e2->tchar, e1->tchar);
	attrptr.key = e1->key; /* store key to entity for which attributes wanted */
	if (ur_retrieve_attr(&attrptr) != 0)
		status = UU_FAILURE; 
	else
		ua_create_text(e2,UM_DEFAULT_TF,&attrptr);
	goto done;
failed:	status = UU_FAILURE;
done:
	uu_dexit;
	return (status);
}	/* ua_cp_text */


/*********************************************************************
**    E_FUNCTION     : int ua_tr_ctext(eptr,offset)
**      Don't do anything if text is associated with another entity.
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
ua_tr_ctext(eptr,offset)
struct  UA_txt_rec *eptr;
UU_REAL   offset[3];

{
	int	status = UU_SUCCESS;

	uu_denter( UU_STRC,(us,"ua_tr_ctext(?,?)"));  
	if (eptr->arckey == 0)	/* don't do anything for text on arc */
		status = ua_tr_text(eptr,offset);
	uu_dexit;
	return(status);
}	/* ua_tr_ctext */

/*********************************************************************
**    E_FUNCTION     : int ua_tr_text(eptr,offset)
**      Translate the specified text from the "from" point to the "to" point.
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
ua_tr_text(eptr,offset)
struct  UA_txt_rec *eptr;
UU_REAL   offset[3];

{
	int	status = UU_SUCCESS;

	uu_denter( UU_STRC,(us,"ua_tr_text(?,?)"));  
   um_vcplvc(eptr->position, offset, eptr->position);
/*	um_update_geom(eptr, UM_DEFAULT_TF);  */
	ur_update_data_fixed(eptr);
	uu_dexit;
	return (status);
}	/* ua_tr_text */


/*********************************************************************
**    E_FUNCTION     : int ua_tf_ctext(eptr,tranfmat,store)
**      Don't do anything if text is associated with another entity.
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be transformed
**          tranfmat     the 4x3 transformation matrix
**				store			store in unibase if true
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_tf_ctext(eptr,tranfmat,store)
struct     UA_txt_rec *eptr;
UU_REAL    tranfmat[4][3];
UU_LOGICAL store;

{                                     
	int	status = UU_SUCCESS;

	uu_denter( UU_MTRC,(us,"ua_tf_ctext(key=%d,tfmat=%x,store=%d)",
		eptr->key,tranfmat,store));

	if (eptr->arckey == 0) 	/* don't do anything for text on arc */
		status = ua_tf_text1 (eptr,tranfmat,store);
	uu_dexit;
	return(status);
}	/* ua_tf_ctext */

/*********************************************************************
**    E_FUNCTION     : int ua_tf_text1(eptr,tranfmat,store)
**      Transform the specified text by the given 4x3  matrix.
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be transformed
**          tranfmat     the 4x3 transformation matrix
**				store			store in unibase if true
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_tf_text1 (eptr,tranfmat,store)
struct UA_txt_rec *eptr;
UU_REAL tranfmat[4][3];
UU_LOGICAL store;
{                                     
	struct UA_txtattr_rec ea;
	UM_vector xaxis, yaxis, zaxis;
	UU_REAL sy;
	int	status = UU_SUCCESS;

	uu_denter( UU_MTRC,(us,"ua_tf_text(key=%d,tfmat=%x,store=%d)",
		eptr->key,tranfmat,store));

	ea.key = eptr->key;
	ur_retrieve_attr(&ea);

	um_unitvc(ea.up, yaxis);
	um_unitvc(ea.plane, zaxis);
	um_cross(yaxis, zaxis, xaxis);

	um_vctmtf(ea.up, tranfmat, ea.up);
	um_vctmtf(ea.plane, tranfmat, ea.plane);
	um_cctmtf(eptr->position, tranfmat, eptr->position);

	um_vctmtf(xaxis, tranfmat, xaxis);
	sy = um_mag(ea.up);
	uu_dprint( UU_MTRC,(us,"sy=%g, height=%g", sy, ea.height));
	ea.height = ea.height * sy;
	uu_dprint( UU_MTRC,(us,"new height=%g", ea.height));
	ea.expn = um_mag(xaxis) / sy;

	um_unitvc(ea.up, ea.up);
	um_unitvc(ea.plane, ea.plane);

	if (store)
	{
		status = ur_update_data_fixed(eptr); 
		ur_update_attr(&ea);
	}
	uu_dexit;
	return (status);
}	/* ua_tf_text */

/*********************************************************************
**    E_FUNCTION     : int ua_tf_text(eptr,tranfmat,store)
**      Transform the specified text by the given 4x3  matrix.
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be transformed
**          tranfmat     the 4x3 transformation matrix
**				store			store in unibase if true
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_tf_text(eptr,tranfmat,store)
struct     UA_txt_rec *eptr;
UU_REAL    tranfmat[4][3];
UU_LOGICAL store;

{                                     
	struct    UA_txtattr_rec ea;
	UM_vector xaxis, yaxis, zaxis;
	UU_REAL sy;
	int	status = UU_SUCCESS;

	uu_denter( UU_MTRC,(us,"ua_tf_text(key=%d,tfmat=%x,store=%d)",
		eptr->key,tranfmat,store));

	ea.key = eptr->key;
	ur_retrieve_attr(&ea);

	um_unitvc(ea.up, yaxis);
	um_unitvc(ea.plane, zaxis);
	um_cross(yaxis, zaxis, xaxis);

	um_vctmtf(ea.up, tranfmat, ea.up);
	um_vctmtf(ea.plane, tranfmat, ea.plane);
	um_cctmtf(eptr->position, tranfmat, eptr->position);

	um_vctmtf(xaxis, tranfmat, xaxis);
	sy = um_mag(ea.up);
	uu_dprint( UU_MTRC,(us,"sy=%g, height=%g", sy, ea.height));
	ea.height = ea.height * sy;
	uu_dprint( UU_MTRC,(us,"new height=%g", ea.height));
	ea.expn = um_mag(xaxis) / sy;

	um_unitvc(ea.up, ea.up);
	um_unitvc(ea.plane, ea.plane);

	if (store)
	  {
/*		um_update_geom(eptr,UM_DEFAULT_TF);*/
		ur_update_data_fixed(eptr);
		ur_update_attr(&ea);
	  }
	uu_dexit;
	return (status);
}	/* ua_tf_text */


/*********************************************************************
**    E_FUNCTION     : int ua_scal_ctext(eptr,scalpt,scalmat,scale)
**      Don't do anything if text is associated with another entity.
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be scaled
**          scalpt        point to be scaled about
**          scalmat       4x3 scale matrix
**          scale         scaling factor
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_scal_ctext(eptr,scalpt,scalmat,scale)
struct   UA_txt_rec *eptr;
UU_REAL    scalpt[3];
UU_REAL    scalmat[4][3];
UU_REAL    scale;

{                                     
	int	status = UU_SUCCESS;

	uu_denter( UU_MTRC,(us,"ua_scal_ctext(?,?,?)"));
    
	if (eptr->arckey == 0)	/* don't do anything for text on arc */
		status = ua_scal_text(eptr,scalpt,scalmat,scale);
	uu_dexit;
	return(status);
}	/* ua_scal_ctext */

/*********************************************************************
**    E_FUNCTION     : int ua_scal_text(eptr,scalpt,scalmat,scale)
**      Scale the text string.
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be scaled
**          scalpt        point to be scaled about
**          scalmat       4x3 scale matrix
**          scale         scaling factor
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_scal_text(eptr,scalpt,scalmat,scale)
struct   UA_txt_rec *eptr;
UU_REAL    scalpt[3];
UU_REAL    scalmat[4][3];
UU_REAL    scale;

{                                     
	struct  UA_txtattr_rec  ea;

	uu_denter( UU_MTRC,(us,"ua_scal_text(?,?,?)"));
    
		/*- transform the text position point -*/
	um_cctmtf(eptr->position,scalmat,eptr->position);
/*	um_update_geom(eptr, UM_DEFAULT_TF);*/
	ur_update_data_fixed(eptr);
		/*-note: text plane and up vector do not change -*/
	ea.key = eptr->key;
	ur_retrieve_attr( &ea);
	ea.height *= scale;    /* fix the char height */
	ur_update_attr( &ea);
	uu_dexit;
	return (UU_SUCCESS);
}	/* ua_scale_text */


/*********************************************************************
**    E_FUNCTION     : int ua_get_text(eptr, databagsize)
**			Given an entity pointed to by EPTR->KEY this function
**			will retrieve all immediate information associated with the 
**			entity (i.e. fixed length entity records and all variable lists).
**    PARAMETERS   
**       INPUT  : 
**				eptr          pointer to an entity structure for returning
**								  the requested information.  Note that the
**                        "key" of the entity must be initialized. 
**				databagsize   size of the data bag pointed to by "eptr".
**       OUTPUT :  
**          eptr				all fixed and variable lists of the entity
**									are returned
**    RETURNS      :
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_get_text(eptr, databagsize)
struct UM_entitydatabag *eptr;
int databagsize;

{
	int isize;
	int status;

	uu_denter(UU_STRC,(us,"ua_get_text(key:%d, bagsize:%d)",
							eptr->key, databagsize));
	status = UU_SUCCESS; /* assume success */
	if ((int) eptr->key < 0)
		status = UU_FAILURE;
	else
	  {
		ur_retrieve_data_relnum(eptr->key, &eptr->rel_num);
		if (eptr->rel_num == UA_TEXT_REL)
			{
			 status = ur_retrieve_data(eptr, databagsize);
			 if (status != 0)
				{
				 uu_uerror1(UM_MODEL, 139, eptr->rel_num);
				  /* message is: um_get_all_geom: not enough space for
				   * retrieval of relation.
				  */			
				  status = UU_FAILURE;
				}
			  else		 /* got data OK	*/
				 {
				  isize = ((struct UA_txt_rec *)eptr)->no_tchar;
						/* end string */
				  ((struct UA_txt_rec *)eptr)->tchar[isize] = '\0';
				 }
			}
		else					/* invalid relation number		*/
			{
			 uu_uerror1(/*um_get_all_geometry: illegal relation %d*/
				UM_MODEL,49,eptr->rel_num);
			 status = UU_FAILURE;
			}
		}
	if (status == UU_FAILURE)
		{
		uu_dprint(UU_MTRC,(us,"FAILURE: rel=%d, key=%d, databagsize=%d",
					eptr->rel_num, eptr->key, databagsize));
		}
	uu_dexit;
	return(status);
}	/* ua_get_text */

/*********************************************************************
**    E_FUNCTION     : int ua_get_text1 (eptr)
**			Given an entity pointed to by EPTR->KEY this function
**			will retrieve the fixed length information associated with the 
**			entity.
**    PARAMETERS   
**       INPUT  : 
**				eptr          pointer to an entity structure for returning
**								  the requested information.  Note that the
**                        "key" of the entity must be initialized. 
**       OUTPUT :  
**          eptr				fixed data of the entity is returned
**    RETURNS      :
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ua_get_text1 (eptr)
struct UA_txt_rec *eptr;
{
	int status;

	uu_denter(UU_STRC,(us,"ua_get_text1(key:%d)",eptr->key));

	status = UU_FAILURE;

	if (eptr->key != NULLKEY)
	{
		ur_retrieve_data_relnum(eptr->key, &eptr->rel_num);
		if (eptr->rel_num == UA_TEXT_REL)
		{
			if (ncl_retrieve_data_fixed (eptr) == 0)
				status = UU_SUCCESS;
			else
			{
				uu_uerror1(UM_MODEL, 139, eptr->rel_num); 
			/* um_get_all_geom: not enough space for retrieval of relation. */
			}
		}
		else /* invalid relation number */
		{
			uu_uerror1(UM_MODEL,49,eptr->rel_num);
		/*um_get_all_geometry: illegal relation %d*/
		}
	}

	if (status == UU_FAILURE)
	{
		uu_dprint(UU_MTRC,(us,"FAILURE: rel=%d, key=%d, databagsize=%d",
					eptr->rel_num, eptr->key, databagsize));
	}
	
	uu_dexit;
	return(status);
}

/*********************************************************************
**    E_FUNCTION     : ua_mirror_ctext(eptr, mirrpt, mirrnorm, mirrmat)
**      Don't do anything if text is associated with another entity.
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be transformed
**          mirrmat     the 4x3 transformation matrix
**				store      update unibase if TRUE
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_mirror_ctext(eptr, mirrpt, mirrnorm, mirrmat)
struct  UA_txt_rec  *eptr;
UM_coord mirrpt;
UM_vector mirrnorm;
UM_transf mirrmat;

{                        
	int	status = UU_SUCCESS;

	uu_denter( UU_STRC,(us,"ua_mirror_text(key=%d,mirrmat=%x)",
		eptr->key,mirrmat));
	if (eptr->arckey == 0)	/* don't do anything for text on arc */
		status = ua_mirror_text(eptr, mirrpt, mirrnorm, mirrmat);
	uu_dexit;
	return(status);
}	/* ua_mirror_ctext */

/*********************************************************************
**    E_FUNCTION     : ua_mirror_text(eptr, mirrpt, mirrnorm, mirrmat)
**      Mirror the specified text by the given 4x3 matrix.
**    PARAMETERS   
**       INPUT  : 
**				eptr         pointer to the entity to be transformed
**          mirrmat     the 4x3 transformation matrix
**				store      update unibase if TRUE
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_mirror_text(eptr, mirrpt, mirrnorm, mirrmat)
struct  UA_txt_rec  *eptr;
UM_coord mirrpt;
UM_vector mirrnorm;
UM_transf mirrmat;

{                        
	struct    UA_txtattr_rec ea;
	UM_vector xaxis, yaxis, zaxis;
	UU_REAL sy;

	uu_denter( UU_STRC,(us,"ua_mirror_text(key=%d,mirrmat=%x)",
		eptr->key,mirrmat));

	ea.key = eptr->key;
	ur_retrieve_attr(&ea);

	um_unitvc(ea.up, yaxis);
	um_unitvc(ea.plane, zaxis);
	um_cross(yaxis, zaxis, xaxis);

	um_vctmtf(ea.up, mirrmat, ea.up);
	um_vctmtf(ea.plane, mirrmat, ea.plane);
	um_cctmtf(eptr->position, mirrmat, eptr->position);

	um_vctmtf(xaxis, mirrmat, xaxis);
	sy = um_mag(ea.up);
	ea.height = ea.height * sy;
	ea.expn = um_mag(xaxis) / sy;

	um_unitvc(ea.up, ea.up);
	um_unitvc(ea.plane, ea.plane);
	um_vctmsc(ea.plane, (UU_REAL) -1.0, ea.plane);
/*	um_update_geom(eptr,UM_DEFAULT_TF);*/
	ur_update_data_fixed(eptr);
	ur_update_attr(&ea);
	uu_dexit;
	return (UU_SUCCESS);
}	/* ua_mirror_text */




/*********************************************************************
**    E_FUNCTION     : ua_delete_text(key)
**       Delete a text entity.
**    PARAMETERS   
**       INPUT  : 
**          key					key of text entity to delete
**       OUTPUT :  none
**    RETURNS: UU_SUCCESS if no problems; UB_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/

int ua_delete_text(key)
UU_KEY_ID key;
{
	int status = UU_SUCCESS;
	struct UA_txt_rec note;

	uu_denter(UU_STRC,(us,"ua_delete_text(key=%d)", key));

	note.key = key;
	if (ua_get_text1(&note) == UU_SUCCESS)
	{
		if (note.arckey != NULLKEY)
			ur_delete_all(note.arckey);
	}

		/* delete the text */
	if (ur_delete_all(key) != 0)
		status = UU_FAILURE;
	uu_dexit;
	return(status);
}	/* ua_delete_text */	




/*********************************************************************
**    E_FUNCTION     : int ua_txt_to_drawing(eptr, tfmat, attrptr, 
**									drwmat, vrefpt, vpnorm,
**									keylist, render_solids)
**			Create a list of keys (KEYLIST) of entities to be included
**			in a drawing for the given text entity (EPTR, TFMAT, ATTRPTR).
**			The transformation (DRWMAT) will position the viewplane
**			(VREFPT, VPNORM) on the XY plane of the drawing appropriately 
**			scaled to map the MCS to the DCS.
**    PARAMETERS   
**       INPUT  : 
**				eptr						pointer to entity data
**				tfmat						matrix to position entity in MCS
**				attrptr					pointer to attribute bundle 
**				drwmat					transformation to convert MCS to DCS
**				vrefpt					reference point of viewing plane
**				vpnorm					normal of viewing plane
**				render_solids			UU_TRUE => render solid entities
**       OUTPUT :  
**				keylist					keys of projected entities are pushed
**											onto this list of keys
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE 
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ua_txt_to_drawing(eptr, tfmat, attrptr, drwmat, vrefpt, vpnorm,
						keylist, render_solids)
struct UA_txt_rec *eptr;
UM_transf tfmat;
struct UM_attrdata_rec *attrptr;
UM_transf drwmat;
UM_coord vrefpt;
UM_vector vpnorm;
UU_LIST *keylist;
UU_LOGICAL render_solids;

{
	struct UA_txt_rec		txtrec;
	struct UM_circle_rec	arc1, arc2;
	struct UM_attrdata_rec attr;
	UM_covec *ptv;
	UM_coord *pts;
	UU_KEY_ID drw_view;
	int status,npts,i;

	uu_denter(UU_STRC,(us,"ua_txt_to_drawing(key=%x)", eptr->key));

	status = UU_SUCCESS;

/*
.....Text contains a display list
*/
	if (eptr->no_displst != 0)
	{
		txtrec.key = eptr->key;
		uc_retrieve_data(&txtrec,sizeof(struct UA_txt_rec));
		ptv = (UM_covec *)eptr->displst;
		npts = eptr->no_displst;
		pts = (UM_coord *)uu_malloc(sizeof(UM_coord)*npts);
		for (i=0;i<npts;i++) um_vctovc(ptv[i],pts[i]);
		status = ncl_displst_proj_to_drawing(eptr,pts,attrptr,drwmat,
			vrefpt,vpnorm,keylist);
		uu_free(pts);
	}
	/* copy and transform the text entity in UNIBASE. Call the text routine
		directly to make sure text along the arc is copied too.*/

	else
	{
		ua_cp_text(eptr, &txtrec, sizeof(struct UA_txt_rec));
		if (txtrec.arckey != 0)		/* Text is associated with an arc */
	  	{
			arc1.key = txtrec.arckey;		
			if (uc_retrieve_data(&arc1, sizeof(arc1)) != UU_SUCCESS)	goto failed;
			if (uc_copy(&arc1, &arc2, sizeof(arc1)) != UU_SUCCESS)	goto failed;
			if (uc_retrieve_attr(arc2.key, &attr) != UU_SUCCESS)		goto failed;
			attr.selectable = UU_FALSE;
			attr.displayable = UM_NEVERDISPLAYABLE;
			if (ur_update_attr(&attr) != 0)			goto failed;
		/* The arc has to be transformed before the text set a pointer to its key.
			otherwise the association action will transform the text while arc is
			transformed */
			if (uc_transform(&arc2, drwmat, UU_TRUE) != UU_SUCCESS)	goto failed;
			txtrec.arckey = arc2.key; 
	  	}
		ua_tf_text(&txtrec, drwmat, UU_TRUE); 

	/* update the view the new drafting entity is visible in, make
		sure it is displayable in this view, and display it */
		drw_view = ur_get_drwmdl_drwvw();
		ur_update_view_key(txtrec.key, drw_view);
		ur_update_displayable(txtrec.key, UM_DISPLAYABLE);
		uc_display(&txtrec);

	/* push the new entity onto the list of entities to include in
		the drawing */
		uu_list_push(keylist, &txtrec.key);
	}
	goto done;

failed:	status = UU_FAILURE;
done:
	uu_dexit;
	return (status);
} /* ua_txt_to_drawing */
