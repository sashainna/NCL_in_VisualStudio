
/*********************************************************************
**    NAME         : m2egen.c 
**       CONTAINS:
**			um_evxx_genent
**			um_drwxx_genent
**			um_pxx_genent
**			um_cpxx_copygenent
**			um_tfxx_tranfgenent
**			um_trxx_trangenent
**			um_scxx_scalgenent
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2egen.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:46
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "uhep.h"
#include "udebug.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mdgenent.h"
#include "mattr.h"
#include "modef.h"

#define UM_TRACE  0

/*********************************************************************
**    E_FUNCTION: um_evxx_genent(evflag, u, eptr, tfmat, evoutptr)
**			DESCRIPTION:
**				Evaluate a curve at the parameter value u.
**			PARAMETERS   
**				INPUT: 
**		      	evflag	 flag specifying data to evaluate
**									>= UM_POINT => calculate (X,Y,Z) 
**									>= UM_FRSTDERIV => calculate 
**										(X,Y,Z) plus (DX/DU,DY/DU,DZ/DU)
**									>= UM_SECDERIV => calculate 
**										(X,Y,Z) plus (DX/DU,DY/DU,DZ/DU)
**										plus	(D2X/DU2,D2Y/DU2,D2Z/DU2)
**									>= UM_CURVATURE => calculate all the
**										above plus curvature
**					u			 parameter to evaluate at
**					eptr      pointer to the curve entity
**					tfmat		 transformation matrix for orienting the entity
**								 pointed to by "eptr".
**				OUTPUT :  
**					evoutptr  pointer to a curve evaluator record containing 
**                       the requested information.
**			RETURNS      : none
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
um_evxx_genent(evflag, u, eptr, tfmat, evoutptr)
	int evflag;  
	UM_param u;
	struct UM_entitydatabag *eptr;
	UM_transf tfmat;
	struct UM_evcrvout *evoutptr;

	{
	uu_denter(UU_MTRC,(us,"um_evxx_genent(%d,%g,%x,%x,%x)",
					evflag,u,eptr,tfmat,evoutptr));
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION: um_drwxx_genent(eptr,tfmat,attrptr)
**			DESCRIPTION:
**				Draws an entity.
**			PARAMETERS   
**				INPUT:
**					eptr			pointer to entity record
**					tfmat			transformation matrix
**					attrptr		pointer to attribute record
**				OUTPUT :       none. 
**			RETURNS      : none
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
um_drwxx_genent(eptr,tfmat,attrptr)
	struct UM_entitydatabag *eptr;
	UM_transf tfmat;
	struct UM_attrdata_rec *attrptr;

	{
	uu_denter(UU_MTRC,(us,"um_drwxx_genent(%x,%x,%x)", eptr,tfmat,attrptr));
	
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION: um_pxx_genent(ptr)
**			DESCRIPTION: Print the parameters defining an entity.
**			PARAMETERS   
**				INPUT: 
**					ptr          pointer to the entity record.
**				OUTPUT :  
**			RETURNS      : none
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
um_pxx_genent(ptr)
	struct UM_entitydatabag  *ptr;

	{
	uu_denter(UU_MTRC,(us,"um_pxx_genent(%8x)",ptr));
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION: um_trxx_trangenent(eptr, offset)
**			DESCRIPTION: Translate a entity.
**			PARAMETERS   
**				INPUT: 
**					eptr          pointer to the bspline entity record.
**					offset        direction vector for translation.
**				OUTPUT : none.  
**			RETURNS      : none
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
um_trxx_trangenent(eptr, offset)
	struct UM_entitydatabag *eptr;
	UM_vector offset[];
	{
	UM_transf tfmat;

	uu_denter(UU_MTRC, (us, "um_trxx_trangenent(%x, %x)", eptr, offset));
	um_get_transformation(eptr->key, tfmat);
	um_vcplvc(offset, tfmat[3], tfmat[3]);
	um_update_transformation(eptr->key, tfmat);
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION: um_tfxx_tranfgenent(eptr,tfmat,store)
**      Tranform entity (eptr) by the specified transform. We assume the 
**		  immediate geometry of the entity has been obtained, but not 
**		  any subentity geometry; i.e. subentities associated with "eptr". 
**		  Note, only the transformation for the entity pointed to by "eptr"
**		  is changed here, so no subentity geometry is changed.
**			PARAMETERS   
**				INPUT: 
**					eptr         pointer to a generic entity to transform.
**					tfmat     	 transformation matrix. 
**					store			 TRUE iff the transformed entity is to be stored
**									 in UNIBASE in place of the current entity record.
**				OUTPUT :  
**					eptr			 pointer to the transformed entity.
**			RETURNS      : none
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
um_tfxx_tranfgenent(eptr,tfmat,store)
	struct UM_entitydatabag *eptr;
	UM_transf tfmat;
	UU_LOGICAL store;
	{
	UU_REAL current_tfmat[4][3];

	uu_denter(UU_MTRC, 
		(us,"um_tfxx_tranfgenent(eptr->key:%d, tfmat:%x, store:%d)",
					eptr->key, tfmat, store));
	/* get current transformation */
	um_get_transformation(eptr->key, current_tfmat);

	/* concatenate current transform with new transform */
	um_tftmtf(current_tfmat, tfmat, current_tfmat);
	if (store) /* store new transform */
		um_update_transformation(eptr->key, current_tfmat);

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION: um_cpxx_copygenent(e1ptr, e2ptr, bagsize)
**			DESCRIPTION: Copies the data for one entity into 
**					another entity. The new entity also inherits the old entity's
**					transformation and attributes.
**			PARAMETERS   
**				INPUT: 
**					e1ptr         entity pointer
**					bagsize		  size of storage for entity.
**				OUTPUT :  
**					e2ptr         entity pointer for the new entity.
**			RETURNS      : none
**			SIDE EFFECTS : none
**			WARNINGS     : none
*********************************************************************/
um_cpxx_copygenent(e1ptr, e2ptr, bagsize)
	struct UM_entitydatabag *e1ptr;
	struct UM_entitydatabag *e2ptr;
	int bagsize;
	{
	UU_KEY_ID tempkey;
	UM_transf tfmat;
	struct UM_attrdata_rec attrbag;
	int i;

	uu_denter(UU_MTRC, (us, "um_cpxx_copygenent(e1ptr->key:%d, %x, bagsize:%d)",
				e1ptr->key, e2ptr, bagsize));
	tempkey = e1ptr->key;
	um_get_transformation(tempkey, tfmat);
	um_get_disp_attr(tempkey, &attrbag);
	um_create_geom(e1ptr, tfmat, &attrbag);
	e2ptr->key = e1ptr->key;
	um_get_all_geom(e2ptr, bagsize);

	e1ptr->key = tempkey;
	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : um_scxx_scalgenent(eptr,pt,scalmat)
**      Scale an entity around a point
**    PARAMETERS   
**       INPUT  : 
**				eptr        pointer to the entity to be scaled
**          scalpt      point to be scaled about
**          scalmat     the 4x3 scaling matrix
**       OUTPUT :  
**          none
**    RETURNS      : none
**   SIDE EFFECTS  : none
**    WARNINGS     : none
*********************************************************************/
um_scxx_scalgenent(eptr,pt,scalmat)
	struct  UM_entitydatabag *eptr;
	UM_coord pt;
	UM_transf scalmat;

	{
	UM_transf tfmat;

	uu_denter(UU_MTRC, (us, "um_scxx_scalgenent(eptr->key:%d, %x, %x)", 
						eptr->key, pt, scalmat));
	um_get_transformation(eptr->key, tfmat);
	um_tftmtf(tfmat, scalmat, tfmat);
	um_update_transformation(eptr->key, tfmat);
	uu_dexit;
	}
