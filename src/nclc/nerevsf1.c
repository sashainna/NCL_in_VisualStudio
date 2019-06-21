/*********************************************************************
**    NAME         :  nerevsf1.c
**       CONTAINS:
**           int ncl_transform_revsf
**
**    COPYRIGHT 2005 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nerevsf1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:46
*********************************************************************/

#include "udebug.h"
#include "nccs.h"
#include "class.h"
#include "mdrel.h"
#include "mattr.h"
#include "ncl.h"
#include "nclvx.h"
#include "mdeval.h"
#include "nclfc.h"
#include "modef.h"
#include "mdattr.h"

/*********************************************************************
**    E_FUNCTION     : int ncl_transform_revsf (eptr, tfmat, store)
**       Transform NCL surface of revolution by the given 4X3 transformation 
**       and store in UNIBASE iff store is true.
**    PARAMETERS
**       INPUT  :
**          eptr     pointer to the surface.
**          tfmat    transformation matrix.
**          store    TRUE iff transformed entity is to be put in UNIBASE.
**       OUTPUT :  none.
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_transform_revsf (eptr, tfmat, store)
struct NCL_revsurf_rec *eptr;
UM_transf tfmat;
UU_LOGICAL store;
{
	struct NCL_fixed_databag crv;
	UM_transf crv_tfmat;
	int status;
	int neg = 0;
	UU_REAL det,dist;

	if (eptr->cvkey <= 0) return (UU_FAILURE);

	status = UU_SUCCESS;

	um_determinant (tfmat,&det);
	if (det < 0.0) neg = 1;
/*
...fix surface offset if there is a nontrivial trnsformation
*/
	dist = eptr->offdist;
	if (dist != 0.0)
	{
		if (neg) det = -det;
		if (det != 1.0) det = pow (det,1./3.);
		if (neg) det = -det;
		eptr->offdist = dist * det;
	}
	
	if (eptr->primitive >= NCLSF_PLANE)
		ncl_transform_primdat (&eptr->primitive,eptr->prim_param,tfmat);

	crv.key = eptr->cvkey;
	status = ncl_retrieve_data_fixed (&crv);
	if (status == UU_SUCCESS && store != -1)
	{
		uc_retrieve_transf (crv.key, crv_tfmat);
		um_tftmtf (crv_tfmat, tfmat, crv_tfmat);
		uc_transform (&crv, crv_tfmat, store);
	}

	if (status == UU_SUCCESS)
	{
		um_cctmtf(eptr->pta, tfmat, eptr->pta);
		um_vctmtf(eptr->vca, tfmat, eptr->vca);
		if (neg)
		{
			eptr->sa = -eptr->sa; eptr->ta = -eptr->ta;
		}
	}

	if (status == UU_SUCCESS && store != -1)
	{
		if (eptr->no_tesslst!=0)
			ncl_lst_delete(TESSELLATION_LIST, &eptr->key);
		if (eptr->no_displst!=0)
			ncl_lst_delete(DISPLAY_LIST, &eptr->key);
		if (eptr->no_boxlst!=0)
			ncl_lst_delete(BOX_LIST, &eptr->key);
		if (eptr->no_xyzbylst!=0)
			ncl_lst_delete(WHOLE_BOUNDARY_LIST, &eptr->key);
	}

	if (status == UU_SUCCESS && store == 1) ur_update_data_fixed (eptr);

   return (status);
}
