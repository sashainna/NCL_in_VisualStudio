/*********************************************************************
**    NAME         :  nevolve.c
**    CONTAINS: Routines converting curves to array of points with
**              given chordal tolerance in respect of the CV curvature.
**
**       ncl_evolve1_curve
**       ncl_evolve1_crv_on_srf
**       ncl_evolve1_bn_crv_on_srf
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       nevolve.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:57
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "mdrel.h"
#include "mcrv.h"
#include "msrf.h"
#include "mdebug.h"
#include "mdattr.h"
#include "mdeval.h"
#include "modef.h"
#include "ulist.h"

#include "nccs.h"
#include "ncl.h"
#include "nclfc.h"

UU_REAL ncl_chordh_cnpts();

/********************************************************************
**    E_FUNCTION: ncl_evolve1_curve (eptr,tfmat,nu,pptr)
**       Evolve curve to 'nu' points polyline with 
**       evenly spread variable parameter.
**    PARAMETERS   
**       INPUT  : 
**          eptr   - pointer to curve record
**          tfmat  - ID matrix of input curve
**          nu     - number of points to evalute
**       OUTPUT :  
**          pptr   - pointer to points list 
**    RETURNS      : 
**          Number of points stored in list.
**    SIDE EFFECTS : none
**    WARNINGS     : none
********************************************************************/
int ncl_evolve1_curve (eptr, tfmat, nu, pptr)
struct UM_crvdatabag *eptr;
UM_transf tfmat;
int nu;
UU_LIST *pptr;
{
	UU_REAL du, u;
	int i, status;
	struct UM_evcrvout evout;

	uc_init_evcrvout (eptr,&evout);
	du  = 1./(nu-1);
	for (i=0, u=0.; i<nu; i++, u+=du)
	{
		if (u>1.) u = 1.;
		status = uc_evcrv (UM_POINT, u, eptr, tfmat, &evout);
		if (status == UU_SUCCESS) uu_list_push (pptr,evout.cp);
	}

	return(nu);
}

/********************************************************************
**    E_FUNCTION: ncl_evolve1_crv_on_srf (eptr,tfmat,uv,nu,cvtyp,pptr)
**       Evolve isoparametric surface curve to 'nu' points polyline 
**       with evenly spread variable parameter.
**    PARAMETERS   
**       INPUT  : 
**          eptr   - pointer to surface record
**          tfmat  - ID matrix of input surface
**          uv     - constant parameter
**          nu     - number of points to evalute
**          cvtyp  - 1 = u_curve, 2 = v_curve.
**       OUTPUT :  
**          pptr   - pointer to points list 
**    RETURNS      : 
**          Number of points stored in list.
**    SIDE EFFECTS : none
**    WARNINGS     : none
********************************************************************/
int ncl_evolve1_crv_on_srf (eptr, tfmat, uv, cvtyp, nu, pptr)
struct UM_srfdatabag *eptr;
UU_REAL uv;
UM_transf tfmat;
int nu, cvtyp;
UU_LIST *pptr;
{
	UU_REAL du, u;
	UM_coord pt;
	int i, status;
	struct UM_evsrfout evout;

	uc_init_evsrfout (eptr,&evout);
	du  = 1./(nu-1);
	for (i=0, u=0.; i<nu; i++, u+=du)
	{
		if (u>1.) u = 1.;
		if (cvtyp == 1)
			status = uc_evsrf (UM_POINT, u, uv, eptr, tfmat, &evout);
		else
			status = uc_evsrf (UM_POINT, uv, u, eptr, tfmat, &evout);
/*
....add tranformation here because uc_evsrf does not apply tfmat
....it ignored tfmat but we can not changed that function( too many
....routin call it
....Yurong 10/2/97
*/
		um_cctmtf(evout.sp, tfmat, pt);
		if (status == UU_SUCCESS) uu_list_push (pptr,pt);
/*		if (status == UU_SUCCESS) uu_list_push (pptr,evout.sp); */
	}

	return(nu);
}

/********************************************************************
**    E_FUNCTION: ncl_evolve1_bn_crv_on_srf (bsptr,tfmat,cptr,nu,pptr)
**       Evolve any surface curve defined as: (u,v) = CVf(t) &
**       (x,y,z) = SFf(u,v) to 'nu' points polyline with given chordal
**       tolerance for specified curve.
**    PARAMETERS   
**       INPUT  : 
**          eptr   - pointer to trimmed surface base SF record
**          tfmat  - ID matrix of input surface
**          cptr   - pointer to curve on surface
**          nu     - number of points to calculate
**       OUTPUT :  
**          pptr   - pointer to points list 
**    RETURNS      : 
**          Number of points stored in list.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ncl_evolve1_bn_crv_on_srf (bsptr,tfmat,cptr,bplm,nu,pptr)
struct NCL_fixed_databag *bsptr;
UM_transf tfmat;
struct UM_crvdatabag *cptr;
UU_REAL *bplm;
int nu;
UU_LIST *pptr;
{
	int i,status;
	struct UM_evcrvout evcv,uvcv;
	UU_REAL du, u;

	status = UU_SUCCESS;

	uc_init_evcrvout (cptr, &evcv);
	uc_init_evcrvout (bsptr, &uvcv);

	du  = 1./(nu-1);
	for (i=0, u=0.; i<nu && status == UU_SUCCESS; i++, u+=du)
	{
		status = 
			um_ev7_crv_on_surf (UM_POINT,bsptr,cptr,bplm,u,tfmat,&evcv,&uvcv);
		if (status == UU_SUCCESS) uu_list_push (pptr,evcv.cp);
	}

	return (status);
}
