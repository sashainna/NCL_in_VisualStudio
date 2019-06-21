/*********************************************************************
**    NAME         :  m2etran.c
**       CONTAINS: support routines to handle transforming entities.
**
**			int um_translate_using_matrix
**			int um_rotate_using_matrix
**			int um_mirror_using_matrix
**			int um_scale_using_matrix
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2etran.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:46
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "class.h"
#include "mfort.h"
#include "nclfc.h"

/*********************************************************************
**    E_FUNCTION     : int um_translate_using_matrix(eptr, offset)
**      Translate an entity (EPTR) by the calling the class routine to
**			apply the transformation matrix resulting from OFFSET.
**    PARAMETERS   
**       INPUT  : 
**			 eptr	      	entity pointer
**          offset		direction to translate
**       OUTPUT :  
**          eptr			pointer to translated entity.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_translate_using_matrix(eptr, offset)
struct UC_entitydatabag *eptr;
UM_vector    offset;
{
	int status;
	UM_transf tfmat;
	UM_int2 num;

	uu_denter(UU_MTRC, (us,"um_translate_using_matrix(key=%x)",eptr->key));
	um_disptf(offset, tfmat);
	status = uc_transform(eptr, tfmat, UU_TRUE);
/*
..... aak 14-apr-1998: update eptr and redisplay SSPLIN's on surface
*/
	if (status == UU_SUCCESS) status = ncl_retrieve_data_fixed (eptr);
	if (status == UU_SUCCESS)
	{
		ncl_gtssnum (&eptr->key, &num);
		if (num > 0) ncl_redisp_ss (eptr);
	}

	uu_dexit;
	return (status);
}

/*********************************************************************
**    E_FUNCTION     : int um_rotate_using_matrix(eptr, pt, dir, angle, rotmat)
**      Rotate an entity (EPTR) by the calling the class routine to
**			apply the transformation matrix.
**    PARAMETERS   
**       INPUT  : 
**			 eptr	      	entity pointer
**          pt		      point on the rotation axis
**          dir			direction vector of axis
**          angle   		the angle through which to rotate
**          rotmat	  	4 x 3 rotation matrix
**       OUTPUT :  
**          eptr			pointer to rotated entity.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_rotate_using_matrix(eptr, pt, dir, angle, rotmat)
struct UC_entitydatabag *eptr;
UM_coord   pt;
UM_vector  dir;
UM_angle   angle;
UM_transf  rotmat;
{
	int status;
	UM_int2 num;

	uu_denter(UU_MTRC, (us,"um_rotate_using_matrix(key=%x)",eptr->key));
	status = uc_transform(eptr, rotmat, UU_TRUE);
/*
..... aak 14-apr-1998: update eptr and redisplay SSPLIN's on surface
*/
	if (status == UU_SUCCESS) status = ncl_retrieve_data_fixed (eptr);
	if (status == UU_SUCCESS)
	{
		ncl_gtssnum (&eptr->key, &num);
		if (num > 0) ncl_redisp_ss (eptr);
	}

	uu_dexit;
	return (status);
}

/**************************************************************************
**  E_FUNCTION:   int um_mirror_using_matrix(eptr, mirrpt, normal, mirrmat)
**      Mirror an entity (EPTR) by using the supplied transformation
**			matrix.
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
int um_mirror_using_matrix(eptr, mirrpt, mirrnorm, mirrmat)
struct UC_entitydatabag *eptr;
UM_coord mirrpt;
UM_vector mirrnorm;
UM_transf mirrmat;
{                        
	int status;
	UM_int2 num;

	uu_denter(UU_MTRC,(us,"um_mirror_using_matrix(key=%x)", eptr->key));
	status = uc_transform(eptr, mirrmat, UU_TRUE);
/*
..... aak 14-apr-1998: update eptr and redisplay SSPLIN's on surface
*/
	if (status == UU_SUCCESS) status = ncl_retrieve_data_fixed (eptr);
	if (status == UU_SUCCESS)
	{
		ncl_gtssnum (&eptr->key, &num);
		if (num > 0) ncl_redisp_ss (eptr);
	}
	uu_dexit;
	return (status);
}

/**************************************************************************
**  E_FUNCTION:   int um_scale_using_matrix(eptr, scalpt, scalmat, scale)
**			Scale an entity by using the supplied matrix.
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
int um_scale_using_matrix(eptr, scalpt, scalmat, scale)
struct UC_entitydatabag *eptr;
UM_coord scalpt;
UM_transf scalmat;
UM_length scale;
{                        
	int status;
	UM_int2 num;

	uu_denter(UU_MTRC,(us,"um_scale_using_matrix(key=%x)",eptr->key));
	status = uc_transform(eptr, scalmat, UU_TRUE);
/*
..... aak 14-apr-1998: update eptr and redisplay SSPLIN's on surface
*/
	if (status == UU_SUCCESS) status = ncl_retrieve_data_fixed (eptr);
	if (status == UU_SUCCESS)
	{
		ncl_gtssnum (&eptr->key, &num);
		if (num > 0) ncl_redisp_ss (eptr);
	}

	uu_dexit;
	return (status);
}
