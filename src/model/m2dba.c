/******************************************************************************
**	NAME: m2dba.c
**		CONTAINS: modeling interface routines to UNIBASE
**
**			int um_retrieve_data_relnum
**			int um_get_all_geom
**			int um_get_transformation
**			UU_LOGICAL um_dynamic_varlist
**			UU_LOGICAL um_isvalid_relation
**			UU_LOGICAL um_islabel_relation
**       int um_init_lablocal
**
**   vp 12.8.95 - This file contains 5 routines from previous version of
**                m2dba.c file which are used in IGES.  See m2dba2.c where
**                are remaning routines (not used in IGES) from old file.
**
**	COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m2dba.c , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**       04/16/19 , 14:54:09
*******************************************************************************/

#include "nccs.h"
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
#include "nclvx.h"

extern int NCLX_internal_geom;

extern int ud_prmerr(char *);

/*********************************************************************
**    E_FUNCTION     : int um_retrieve_data_relnum(key, relnum)
**       Determine the relation number (RELNUM) for the specified
**			entity (KEY). This routine assumes that negative KEY values
**			indicate an entity which resides in ROMULUS. In this case,
**			the relation number (RELNUM) is the UNIBASE equivalent to 
**			the ROMULUS entity.
**    PARAMETERS   
**       INPUT  : 
**          key						entity key (key < 0 => ROMULUS entity)
**       OUTPUT :  
**          relnum					relation number of entity
**    RETURNS      : 
**			UU_SUCCESS if entity exists
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_retrieve_data_relnum(key, relnum)
	UU_KEY_ID key;
	int *relnum;

	{
	struct UM_crvdatabag cptr;
	int status;

	uu_denter(UU_MTRC,(us,"um_retrieve_data_relnum(key=%d)",key));

	if ((int) key < 0)
		status = um_ret_romgeom(key, relnum, &cptr);
	else
		status = ur_retrieve_data_relnum(key, relnum);

	uu_dexit;
	return (status);
	}


/*********************************************************************
**    E_FUNCTION     : int um_get_all_geom(eptr, databagsize)
**			Given an entity pointed to by EPTR->KEY this function
**			will retrieve all immediate information associated with the 
**			entity (i.e. fixed length entity records and all variable lists).
**			and curves that define surfaces are retrieved.  However,
**			information about the geometric entities on a variable list
**			is not retrieved.
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
um_get_all_geom(eptr, databagsize)
	struct UM_entitydatabag *eptr;
	int databagsize;

	{
	UU_LOGICAL	um_isvalid_relation();
	UU_LOGICAL	um_dynamic_varlist();
	int status;

	uu_denter(UU_MTRC,(us,"um_get_all_geom(key:%d, bagsize:%d)",
							eptr->key, databagsize));

	status = UU_SUCCESS; /* assume success */

	if ((int) eptr->key < 0)
		status = um_ret_romgeom(eptr->key, &eptr->rel_num, eptr);
	else
		{
		ur_retrieve_data_relnum(eptr->key, &eptr->rel_num);

		if (um_isvalid_relation(eptr->rel_num))	/* valid relation number	*/
			{
			if (um_dynamic_varlist(eptr->rel_num))
				status = ur_retrieve_app_data(eptr);
			else 
				status = ur_retrieve_data(eptr, databagsize);
			if (status != 0)
				{
				uu_uerror1(UM_MODEL, 139, eptr->rel_num);
				/* message is: um_get_all_geom: not enough space for
				 * retrieval of relation.
				 */			
				status = UU_FAILURE;
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
	}
/*********************************************************************
**    E_FUNCTION     : int um_get_transformation(key, tfmat)
**       Retrieve the transformation (TFMAT) for the entity with
**			the given KEY.
**    PARAMETERS   
**       INPUT  : 
**				key					key of entity to have its transformation
**										retrieved.
**       OUTPUT :  
**				tfmat					transformation matrix.
**    RETURNS      : 
**			UU_SUCCESS iff no errors; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_get_transformation(key, tfmat)
	int key;
	UM_transf tfmat;
	{
	int status;
	int urstatus;	/* return status from UNIBASE */
	struct UM_transf_rec transfpacket;
	char ebuf[256];

	uu_denter(UU_MTRC,(us,"um_get_transformation(key:%d, tfmat:%x)",key, tfmat));
	status = UU_SUCCESS; /* assume success */
	if ((int) key < 0 || NCLX_internal_geom)
		{
		um_tftotf(UM_idmat, tfmat);
		}
	else
		{
		transfpacket.key = key;
		transfpacket.rel_num = UM_TRANSFORM_REL;
		urstatus = ur_retrieve_transf(&transfpacket);
		if (urstatus != 0)
			{
 			uu_uerror1(UM_MODEL, 163, key, ebuf);
			ud_prmerr (ebuf);
			status = UU_FAILURE;
			}
		else
			um_tftotf(transfpacket.tfmat, tfmat);
		}
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION     : UU_LOGICAL um_dynamic_varlist(rel_num)
**       Determine if a modeling relation is to be handled by
**			dynamic storage allocation scheme.
**    PARAMETERS   
**       INPUT  : 
**          rel_num						relation number
**       OUTPUT :  
**          none
**    RETURNS      : 
**				UU_TRUE 				if relation is to be handled by dynamic 
**										storage allocation scheme
**				UU_FALSE				otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL 
um_dynamic_varlist(rel_num)
	int rel_num;

	{
	UU_LOGICAL status;

	uu_denter(UU_MTRC,(us,"um_dynamic_varlist(%d)",rel_num));
	switch (rel_num)
		{
		case UM_DRAWING_REL:
		case UM_POLYLINE_REL:
			status = UU_TRUE;
			break;
		default:
			status = UU_FALSE;
			break;
		}
	uu_dexit;
	return(status);
	}

/*********************************************************************
**    E_FUNCTION : UU_LOGICAL um_isvalid_relation(rel_num)
**       Checks for valid modeling relation number.
**    PARAMETERS   
**       INPUT  : 
**          rel_num		 		relation number
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE if in modeling's list of relation numbers,
**							UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL
um_isvalid_relation(relation)
	int	relation;

	{
	UU_LOGICAL	retval;

	uu_denter(UU_MTRC,(us,"um_isvalid_relation()"));

		switch (relation)
			{
			case UM_POINT_REL:
			case UM_LINE_REL:
			case UM_CIRCLE_REL:
			case UM_CONIC_REL:
			case UM_COMPCRV_REL:
			case UM_RBSPLCRV_REL:
			case UM_UVCVONSF_REL:
			case UM_BODY_REL:
			case UM_POLY_REL:
			case UM_POLYLINE_REL:
			case UM_COORDSYS_REL:
			case UM_LIGHT_REL:
			case UM_GROUP_REL:
			case UM_DRAWING_REL:
			case UM_LAYER_REL:
			case UV_VIEW_REL:
			case UM_AGCRV_REL:
			case UM_AGSRF_REL:
			case UM_AGSHELL_REL:
				retval = UU_TRUE;
				break;
			default:
				retval = UU_FALSE;
			}
		uu_dexit;
		return (retval);
	}

/*********************************************************************
**    E_FUNCTION : UU_LOGICAL um_islabel_relation(rel_num)
**       Checks for valid labeled geometry relation number.
**    PARAMETERS   
**       INPUT  : 
**          rel_num		 		relation number
**       OUTPUT :  
**          none
**    RETURNS      : UU_TRUE if in modeling's list of relation numbers,
**							UU_FALSE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_LOGICAL
um_islabel_relation(relation)
int	relation;
{
	UU_LOGICAL	retval;
	if (ncl_label_type(relation) == UU_SUCCESS) retval = UU_TRUE;
	else retval = UU_FALSE;
	return (retval);
}

/*********************************************************************
**    E_FUNCTION :  um_init_lablocal (eptr)
**       Initalize label location
**    PARAMETERS   
**       INPUT  : 
**          eptr    - data structure of entity.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
  um_init_lablocal (eptr)
  struct NCL_id_rec *eptr;
  {
   if (um_islabel_relation(eptr->rel_num))
     {
      eptr->labloc[0] = eptr->labloc[1] = eptr->labloc[2] = 0.;
/*
.....Added leader line location 
*/
	  eptr->ldrloc[0] = eptr->ldrloc[1] = eptr->ldrloc[2] = 0.;
     }
   return(0);
  }

/*********************************************************************
**    I_FUNCTION     : umi_print_transf(key)
**       Retrieve the transformation for the given entity  (KEY)
**       from UNIBASE and print it to psfile.
**    PARAMETERS
**       INPUT  :
**          key            UNIBASE key of entity to print transf
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : The must be valid or the attemped retrieval may
** fail in ur_get_tuple_ptr. (DEBUG is o.k. - error will be caught)
*********************************************************************/
int
umi_print_transf(key)
  UU_KEY_ID key;
  {
	return 0;
  }
/*********************************************************************
**    I_FUNCTION     : um_retrieve_data_fixed (eptr)
**       Retrieve fiexed data for the given entity  (KEY)
**       from UNIBASE.  If it is drafting entity retrieved data is
**       not is real (not pointers to unibase variable lists) but
**       it must fit into eptr reserved (min UC_entitydatabag).
**       This is temporary solution to get draftings data converted
**       from unibase (UA_draft_rec) to drafting UA_generic_draft
**       format.
**    PARAMETERS
**       INPUT  :
**          eptr.key      UNIBASE key of entity
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : The must be valid or the attemped retrieval may
** fail in ur_get_tuple_ptr. (DEBUG is o.k. - error will be caught)
*********************************************************************/
int
um_retrieve_data_fixed (eptr)
struct UC_entitydatabag *eptr;
{
	int status, rel;

	if (um_retrieve_data_relnum(eptr->key,&rel) == UU_SUCCESS)
	{
		if (rel < UA_LINEAR_DIMS_REL || rel > UA_TEXTATTR_REL || rel == UA_TEXT_REL)
			status = ncl_retrieve_data_fixed (eptr);
		else
			status = uc_retrieve_data (eptr,sizeof(struct UC_entitydatabag));

	} 
	else
		status = UU_FAILURE;

	return(status);
}
