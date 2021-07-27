/*********************************************************************
**    NAME         :  netrimsf2.c
**       CONTAINS: Fortran interface, display, and evaluator routines
**                 for trimmed surfaces which are:
**
**					ncl_pt_inside_trimsf
**					ncl_free_bndry
**					ncl_get_bndry
**
**    COPYRIGHT 2012 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       netrimsf2.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:55
*********************************************************************/
#include "mcrv.h"
#include "msrf.h"
#include "ncl.h"
#include "nccs.h"
#include "mgeom.h"
#include "umath.h"
#include "ulist.h"
#include "nclvx.h"
#include "ngeom.h"

void ncl_free_bndry();

/*********************************************************************
**    E_FUNCTION     : ncl_pt_inside_trimsf (nkey,u,v,ierr)
**       Fortran callable routine to check if a point inside trimmed surface.
**    PARAMETERS   
**       INPUT  : 
**          nkey          Key of curve.
**          u             U-parameter of surface.
**          v             V-parameter of surface.
**       OUTPUT : 
**          ierr          Non-zero if there is an error.
**    RETURNS      : 
**       UU_SUCESS if inside,UU_FAILURE if outside
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_pt_inside_trimsf (sfkey,u,v,told,iret)
UM_int4 *sfkey;
UM_int2 *iret;
UM_real4 *u,*v;
UM_real8 *told;
{
	int i,irot,ib,npts,status,ier,itsk,iprcnt,insf;	
	struct NCL_fixed_databag sf;
	UM_srf_boundary bndr;
	UM_coord *pts,pt,uv;
	UU_LIST cvlst,uvlst;
	UU_REAL dtol,u1,v1;
	UM_transf tfmat;

	status = UU_SUCCESS;
	dtol = *told;
	u1 = *u;
	v1 = *v;
	ncl_set_boundary_toler (dtol);
/*	ncl_get_rotfl (&irot);*/

/*
.....Initialize surface, sf boundary
*/	
	um_init_boundary (&bndr);
	uu_list_init (&cvlst, sizeof(UM_coord), 200, 200);
	uu_list_init (&uvlst, sizeof(UM_coord), 200, 200);
	bndr.uvpts = &uvlst;
	bndr.cvpts = &cvlst;

/*
.....Get surface and surface boundary
*/
	sf.key = *sfkey;
	status = ncl_retrieve_data_fixed (&sf);
	if (status == UU_SUCCESS)
		status = uc_retrieve_transf (sf.key, tfmat);

/*
.....Check if trimmed surface
*/
	if (!ncl_itsa_trimsrf(&sf))
	{
		*iret = 0;
		return;
	}

	if (status == UU_SUCCESS)
	{
		ncl_free_bndry (&bndr);
		UU_LIST_EMPTY (bndr.uvpts);
		UU_LIST_EMPTY (bndr.cvpts);
		status = ncl_get_bndry (&sf,tfmat,&bndr,dtol,UU_FALSE);
		if (bndr.nb < 1) status = UU_FAILURE;
	}
/*
.....Get point on surface
*/
	itsk = 0;
	iprcnt= 0;
	ncl_pt_on_sf(&itsk,&sf.key,&iprcnt,&u1,&v1,&pt,&ier);


	uv[0] = u1;
	uv[1] = v1;
	insf = um_inside_bndry (uv,pt,&bndr,&dtol);

	if (insf == -1)
		*iret = -1;
	else
		*iret = 0;

/*
.....Free memory allocated
*/
Done:
	ncl_free_bndry (&bndr);
	uu_list_free (&uvlst);
	uu_list_free (&cvlst);
}

/*********************************************************************
*********************************************************************/
void ncl_free_bndry (p)
UM_srf_boundary *p;
{
	p->nb = 0;
	UU_FREE (p->np);
	UU_FREE (p->ummx);
	UU_FREE (p->vmmx);
}

/*********************************************************************
**    I_FUNCTION     : int ncl_get_bndry (sf,tfmat,p,btol)
**       Get a surface boundary, standard with some memory-saving tricks,
**        and fewer uu_malloc calls.
**    PARAMETERS
**       INPUT  :
**          sf         - surface structure
**          tfmat      - surface transformation matrix
**          btol       - boundary tolerance
**          lunib      - Retrieves a boundary list from surface unibase or not
**       OUTPUT :
**          p          - standard boundary structure filled
**    RETURNS      :
**         UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_get_bndry (sf,tfmat,p,btol,lunib)
struct NCL_fixed_databag *sf;
UM_transf tfmat;
UM_srf_boundary *p;
UU_REAL btol;
UU_LOGICAL lunib;
{
	int status;
	status = UU_FAILURE;
/*
....Retrieves a boundary list from surface unibase
*/
	if (lunib)
		status = ncl_get_bndrlist96 (sf,p);

	if (status != UU_SUCCESS)
	{
		p->toler = btol;
		status = um_pre_srf_bndr (sf,tfmat,p,(struct NCL_uvconv *)UU_NULL);
		if (status != UU_SUCCESS) return (status);
	}

	if (p->np[0] < 4 || p->uvpts->cur_cnt < 4 || p->cvpts->cur_cnt < 4 ||
		p->ummx[0][1] - p->ummx[0][0] <= UM_DFUZZ ||
		p->vmmx[0][1] - p->vmmx[0][0] <= UM_DFUZZ)
		p->nb = 0;

	return (status);
}
