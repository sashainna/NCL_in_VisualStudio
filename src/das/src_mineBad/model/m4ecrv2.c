
/*********************************************************************
**    NAME         :  m4ecrv2.c
**       CONTAINS: AG curve support routines 
**			void um_transf_point(pt, tfmat)
**			int um_agcrv_translate(eptr, offset)
**			int um_agcrv_transform(eptr, tfmat, store)
**			int um_agcrv_copy(e1ptr, e2ptr, bagsize)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m4ecrv2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:02
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "mattr.h"
#include "mdebug.h"
#include "mdeval.h"
#include "mcrv.h"
#include "mdcpln.h"
#include "modef.h"
#include "misect.h"
#include "mderror.h"

#include "ag_incl.h"
#include "ag_global.h"

extern UU_LOGICAL ncl_where;
/*********************************************************************
**    E_FUNCTION     : um_transf_point(pt, tfmat)
**       Apply the transformation (TFMAT) to the given point (PT).
**    PARAMETERS   
**       INPUT  : 
**          pt							point to transform
**				tfmat						transformation matrix
**       OUTPUT :  
**          pt							transformed point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_transf_point(pt, tfmat)
	UM_coord pt;
	UM_transf tfmat;

	{
	uu_denter(UU_MTRC, (us,"um_transf_point()"));

	um_cctmtf(pt, tfmat, pt);

	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : int um_agcrv_translate(eptr, offset)
**      Translate one curve (key) by the specified offset.
**    PARAMETERS   
**       INPUT  : 
**				eptr      Pointer to the curve to be translated.
**          offset    The vector by which to offset.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_translate(eptr, offset)
    struct UM_agcrv_rec *eptr;
    UM_vector  offset;

	{                        
	int status;
	UM_transf tfmat;

	uu_denter(UU_MTRC,(us,"um_agcrv_translate(key=%d)", eptr->key));
	um_disptf(offset, tfmat);
	status = um_agcrv_transform(eptr, tfmat, UU_TRUE);
	uu_dexitstatus("um_agcrv_translate", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION: int um_agcrv_transform(eptr, tfmat, store)
**      Tranform entity (EPTR) by the specified transform (TFMAT) and
**			store the entity in UNIBASE if STORE is UU_TRUE.
**    PARAMETERS   
**       INPUT  : 
**			 eptr	      	entity pointer
**        tfmat	   	4 x 3 transformation matrix to transform entity by.
**			 store			TRUE iff the transformed entity is to be stored
**								in UNIBASE. 
**       OUTPUT :  
**          eptr			pointer to the transformed entity record.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_transform(eptr, tfmat, store)
   struct UM_agcrv_rec *eptr;
   UM_transf    tfmat;
	UU_LOGICAL store;

	{
	int status;
	int um_transf_point();

	uu_denter(UU_MTRC, (us,"um_agcrv_transform(key=%d,tfmat=%x,store=%d)",
				eptr->key, tfmat, store));

	if (!store)
		status = UU_FAILURE;
	else
		{
		status = UU_SUCCESS;
		ag_tr_crv(eptr->crvaddr, um_transf_point, tfmat, um_transf_point);
		}
	
	uu_dexitstatus("um_agcrv_transform",status);
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : int um_agcrv_copy(e1ptr, e2ptr, bagsize)
**			Make a copy (E1PTR)  of an entity (E1PTR).
**    PARAMETERS   
**       INPUT  : 
**				e1ptr    pointer to entity to be copied
**				bagsize	size of the data bags pointed to by e1 and e2.
**       OUTPUT :  
**				e2ptr    pointer to new entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_copy(e1ptr, e2ptr, bagsize)
   struct UM_agcrv_rec *e1ptr;
   struct UM_agcrv_rec *e2ptr;
	int bagsize;

	{
	int status;
	struct UM_attrdata_rec attrbag;
 
   uu_denter(UU_MTRC, (us,"um_agcrv_copy(key=%x,bagsize=%d)",e1ptr,bagsize));

	status = UU_SUCCESS;

	uc_setup_data(UM_AGCRV_REL, e2ptr, bagsize);

	status = umi_agcrv_copy(e1ptr, e2ptr, bagsize);
	if (status != UU_SUCCESS) goto done;

	/* MILLS- ncl_where=UU_TRUE indicates that this routine has been called from 
	   ncl_update_data to rename one curve to another; in which case there is
	   no need to create new entity after copying one to another. */
	if (!ncl_where)
		{
		uc_retrieve_attr(e1ptr->key, &attrbag);
		uc_create_data(e2ptr, UM_DEFAULT_TF, &attrbag); 
		}

done:;
	uu_dexitstatus("um_agcrv_copy",status);
	return (status);
	}

