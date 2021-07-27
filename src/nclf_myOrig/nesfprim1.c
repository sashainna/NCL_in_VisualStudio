/*********************************************************************
**    NAME         :  nesfprim1.c
**       CONTAINS:
**    int ncl_get_sf_primtyp (sfkey,primtyp)
**    int ncl_get_sf_primdat (sfkey,primtyp,primdata)
**    int ncl_transform_primdat(typ,param,tfmat)
**    COPYRIGHT 2001 (c) Numerical Control Computer Sciences Inc.
**                          All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nesfprim1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:48
*********************************************************************/
#include "udebug.h"
#include "ncl.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "modef.h"
#include "msrf.h"
#include "nccs.h"
#include "nclfc.h"
#include "nclxmdl.h"

extern int NCLX_internal_geom;

/*********************************************************************
**    E_FUNCTION     : ncl_get_sf_primtyp (sfkey,primtyp)
**      get prim_param field of the surface struct.
**    PARAMETERS
**       INPUT  :
**              sfkey   surface key
**       OUTPUT :
**              primtyp primitive typ 
**    RETURNS      :
**              UU_SUCCESS if successful; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_sf_primtyp(sfkey,primtyp)
UM_int4 *sfkey;
UM_int2 *primtyp;
{
	int status;
	struct NCL_fixed_databag sf;
	
	*primtyp = 0;

	sf.key = *sfkey;
	status = ncl_retrieve_data_fixed (&sf);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	if (NCLX_internal_geom)
	{
		NCLX_mdl_surf *nclxsf;
		if (sf.rel_num == NCLX_MDL_TRIMSF)
		{
			NCLX_mdl_trimsf *tmp;		
			tmp = (NCLX_mdl_trimsf *) &sf;
			nclxsf = tmp->surf;
		}
		else if (sf.rel_num == NCLX_MDL_SURF || sf.rel_num == 
							NCLX_MDL_NSURF)
			nclxsf = (NCLX_mdl_surf *) &sf;
		else
			return (UU_FAILURE);

		*primtyp = nclxsf->primitive;
		return (status);
	}
	if (ncl_itsa_trimsrf(&sf))
	{
		struct NCL_trimsf_rec *eptr;
		eptr = (struct NCL_trimsf_rec *) &sf;
		sf.key = eptr->bs_key;
		status = ncl_retrieve_data_fixed (&sf);
		if (status != UU_SUCCESS) return(UU_FAILURE);
	}

	if (sf.rel_num !=  NCL_SURF_REL && sf.rel_num != UM_RBSPLSRF_REL &&
			sf.rel_num !=  NCL_REVSURF_REL) return (UU_FAILURE);

	switch (sf.rel_num)
	{
		case NCL_SURF_REL:
		{
			struct NCL_surface_rec *eptr;
			eptr = (struct NCL_surface_rec *) &sf;
			*primtyp = eptr->primitive;
		}
		break;

		case NCL_REVSURF_REL:
		{
			struct NCL_revsurf_rec *eptr;
			eptr = (struct NCL_revsurf_rec *) &sf;
			if (eptr->primitive <= 1)
				*primtyp = NCLSF_REVOLV;
			else
				*primtyp = eptr->primitive;
		}
		break;

		case UM_RBSPLSRF_REL:
		{
			struct UM_rbsplsrf_rec *eptr;
			eptr = (struct UM_rbsplsrf_rec *) &sf;
			*primtyp = eptr->primitive;
		}
		break;

		default:
			status = UU_FAILURE;
		break;
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_get_sf_primdat (sfkey,primtyp,primdata)
**      get prim_param field and primdata of the surface struct.
**    PARAMETERS
**       INPUT  :
**              sfkey   surface key
**       OUTPUT :
**              primtyp primitive type
**              primdat primitive data
**    RETURNS      :
**              UU_SUCCESS if successful; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_sf_primdat(sfkey,primtyp,primdata)
UM_int4 *sfkey;
UM_int2 *primtyp;
UM_real8 primdata[16];
{
	int status,i,trimd = 0;
	int n = 16;
	struct NCL_fixed_databag sf;
	UM_transf tfmat;
	
	*primtyp = 0;

	sf.key = *sfkey;
	status = ncl_retrieve_data_fixed (&sf);
	if (status != UU_SUCCESS) return (UU_FAILURE);

	if (NCLX_internal_geom)
	{
		NCLX_mdl_surf *nclxsf;
		if (sf.rel_num == NCLX_MDL_TRIMSF)
		{
			NCLX_mdl_trimsf *tmp;		
			tmp = (NCLX_mdl_trimsf *) &sf;
			nclxsf = tmp->surf;
		}
		else if (sf.rel_num == NCLX_MDL_SURF || sf.rel_num == 
							NCLX_MDL_NSURF)
			nclxsf = (NCLX_mdl_surf *) &sf;
		else
			return (UU_FAILURE);

		*primtyp = nclxsf->primitive;
		if (nclxsf->primitive > NCLSF_FREEFORM)
		{
			for (i=0; i<n; i++) 
				primdata[i] = nclxsf->prim_param[i];
		}
		return (status);
	}

	if (ncl_itsa_trimsrf(&sf))
	{
		struct NCL_trimsf_rec *eptr;
		eptr = (struct NCL_trimsf_rec *) &sf;
		if (uc_retrieve_transf(sf.key, tfmat) != UU_SUCCESS) 
			return(UU_FAILURE);
		sf.key = eptr->bs_key;
		status = ncl_retrieve_data_fixed (&sf);
		if (status != UU_SUCCESS) return(UU_FAILURE);
		trimd = 1;
	}

	if (sf.rel_num !=  NCL_SURF_REL && sf.rel_num != UM_RBSPLSRF_REL &&
			sf.rel_num !=  NCL_REVSURF_REL) return (UU_FAILURE);

	switch (sf.rel_num)
	{
		case NCL_SURF_REL:
		{
			struct NCL_surface_rec *eptr;
			eptr = (struct NCL_surface_rec *) &sf;
			if (trimd) ncl_transform_primdat (&eptr->primitive,
				eptr->prim_param,tfmat);
			*primtyp = eptr->primitive;
			if (eptr->primitive > NCLSF_FREEFORM)
			{
				for (i=0; i<n; i++) 
					primdata[i] = eptr->prim_param[i];
			}
		}
		break;

		case NCL_REVSURF_REL:
		{
			struct NCL_revsurf_rec *eptr;
			eptr = (struct NCL_revsurf_rec *) &sf;
			if (eptr->primitive <= NCLSF_FREEFORM)
				*primtyp = NCLSF_REVOLV;
			else
			{
				*primtyp = eptr->primitive;

				if (trimd) 
				ncl_transform_primdat (&eptr->primitive,eptr->prim_param,tfmat);

				for (i=0; i<n; i++) 
					primdata[i] = eptr->prim_param[i];
			}
		}
		break;

		case UM_RBSPLSRF_REL:
		{
			struct UM_rbsplsrf_rec *eptr;
			eptr = (struct UM_rbsplsrf_rec *) &sf;
			if (trimd) ncl_transform_primdat (&eptr->primitive,
				eptr->prim_param,tfmat);
			*primtyp = eptr->primitive;
			if (eptr->primitive > NCLSF_FREEFORM)
			{
				for (i=0; i<n; i++) 
					primdata[i] = eptr->prim_param[i];
			}
		}
		break;

		default:
			status = UU_FAILURE;
		break;
	}

	return (status);
}

/*********************************************************************
**    E_FUNCTION     : ncl_transform_primdat(typ,param,tfmat)
**      transform primdata of the surface struct.
**    PARAMETERS
**       INPUT  :
**              typ     primitive type
**              param   primitive data
**              tfmat   transformation matrix
**       OUTPUT :
**              typ     primitive type
**              param   primitive data
**    RETURNS      :
**              UU_SUCCESS if successful; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_transform_primdat(typ,param,tfmat)
nclsf_prim_type *typ;
UU_REAL *param;
UM_transf tfmat;
{
	UU_REAL scale;

	if (*typ == NCLSF_PLANE)
	{
		um_vctmtf(param,tfmat,param); /* plane normal */
		um_cctmtf(&param[4],tfmat,&param[4]); /* plane point */
		um_unitvc (param,param);
		param[3] = um_dot(param,&param[4]); /* the D parameter in (pl/A,B,C,D) */
	}
	else if (*typ == NCLSF_SPHERE)
	{
/* 
..... tfmat is distorting if it scales differently along different axes
*/
		if (um_distorting_tf (tfmat,&scale))
			*typ = NCLSF_UNKNOWN;
		else
		{
			um_cctmtf(param,tfmat,param); /* center */
			param[3] *= scale; /* radius */
		}
	}
	else if (*typ == NCLSF_CYLINDER)
	{
		if (um_distorting_tf (tfmat,&scale))
			*typ = NCLSF_UNKNOWN;
		else
		{
			um_cctmtf(param,tfmat,param); /* cylinder base center */
			um_vctmtf(&param[3],tfmat,&param[3]); /* cylinder axis */
			um_unitvc (&param[3],&param[3]);
			param[6] *= scale; /* base radius */
			param[7] *= scale; /* height */
		}
	}
	else if (*typ == NCLSF_CONE)
	{
		if (um_distorting_tf (tfmat,&scale))
			*typ = NCLSF_UNKNOWN;
		else
		{
			um_cctmtf(param,tfmat,param); /* cone apex point */
			um_vctmtf(&param[3],tfmat,&param[3]); /* cone axis */
			um_unitvc (&param[3],&param[3]);
			param[7] *= scale; /* height */
			param[8] *= scale; /* distance from apex to top plane */
		}
	}
	else if (*typ == NCLSF_TORUS)
	{
		if (um_distorting_tf (tfmat,&scale))
			*typ = NCLSF_UNKNOWN;
		else
		{
			um_cctmtf(param,tfmat,param); /* torus center */
			um_vctmtf(&param[3],tfmat,&param[3]); /* torus axis */
			um_unitvc (&param[3],&param[3]);
			param[6] *= scale; /* major radius */
			param[7] *= scale; /* minor radius */
		}
	}
	
	return (0);
}
