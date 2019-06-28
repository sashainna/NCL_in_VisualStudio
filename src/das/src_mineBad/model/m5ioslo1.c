/*********************************************************************
**    NAME         :  m5ioslo1.c
**       CONTAINS:
**			um_ev7_crv_on_surf (evflg,eptr,cvptr,bpar,t,tfmat,evout,uvcv)
**			um_ev7_uvcrv (evflg,cvptr,bpar,t,uvcv)
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m5ioslo1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:06
*********************************************************************/

#include "mdeval.h"
#include "uminmax.h"
#include "nccs.h"
#include "nclx.h"
#include "ycom.h"
#include "mdrel.h"
#include "mcrv.h"
#include "msrf.h"

/*********************************************************************
**    E_FUNCTION: um_ev7_crv_on_surf (evflg,eptr,cvptr,t,tfmat,evout,
**                                    uvcv); 
**       Evaluate a CV point on a bspline surface at a specified
**       parameter t of the (u,v)=f(t) curve which lies on surface
**       and is a part of the trimmed surface definition.  NOTE:
**       data returned in evout record is related to the input curve.
**    PARAMETERS   
**       INPUT  : 
**          evflg    flag specifying data to evaluate
**          eptr     pointer to rational bspline SF record
**          cvptr    pointer to parametric curve record
**          bpar     u,v limits: [0] = u_min value, [1] = u_max,
**                   [2] = v_min, [3] = v_max value.
**          t        curve parameter value to evaluate u,v values
**          tfmat    transformation matrix for SF.
**       OUTPUT :  
**          evout    pointer to (x,y,z) CV evaluation record
**          uvcv     pointer to (u,v) curve evaluation record
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_ev7_crv_on_surf (evflg,eptr,cvptr,bpar,t,tfmat,evout,uvcv)
int evflg;
struct UM_rbsplsrf_rec *eptr;
UM_transf tfmat;
struct NCL_fixed_databag *cvptr;
UU_REAL t, *bpar;
struct UM_evcrvout *evout, *uvcv;
{
	int status, flag;

	flag = evflg;
	if (flag > UM_SECDERIV) flag = UM_SECDERIV;
	status = um_ev7_uvcrv (flag, cvptr, bpar, t, uvcv);

	if (status == UU_SUCCESS)
		status = um_ev7_uvev_to_cvev(evflg,cvptr,eptr,tfmat,uvcv,evout);
	return (status);
}

/*********************************************************************
**    E_FUNCTION: um_ev7_uvcrv (evflg,cvptr,bpar,t,uvcv)
**       Evaluate a u,v point on a bspline curve at a specified
**       parameter t of the (u,v)=f(t) curve. 
**    PARAMETERS   
**       INPUT  : 
**          evflg    flag specifying data to evaluate
**          cvptr    pointer to parametric curve record
**          bpar     u,v limits: [0] = u_min value, [1] = u_max,
**                   [2] = v_min, [3] = v_max value.
**          t        curve parameter value to evaluate u,v values
**       OUTPUT :  
**          uvcv     pointer to (u,v) curve evaluation record
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_ev7_uvcrv (evflg,cvptr,bpar,t,uvcv)
int evflg;
struct NCL_fixed_databag *cvptr;
UU_REAL t, *bpar;
struct UM_evcrvout *uvcv;
{
	UU_REAL dup[2];
	int status, i;
/*
...get point on curve of parameters
*/
	status = uc_evcrv (evflg, t, cvptr, UM_DEFAULT_TF, uvcv);
/*
...convert point to u,v space of surface
*/
	if (status == UU_SUCCESS) 
	{ 
		dup[0] = bpar[1] - bpar[0];
		dup[1] = bpar[3] - bpar[2];
/*
		if (dup[0] != 1.0 || dup[1] != 1.0)
..... wrong if, e.g., bpar[0]=4, bpar[1]=5, bpar[2]=2, bpar[3]=3
*/
		if (dup[0] <= 0.0 || dup[1] <= 0.0)
			return (UU_FAILURE);
		for (i=0; i<2; i++)
		{
			uvcv->cp[i] = (uvcv->cp[i] - bpar[2*i]) / dup[i]; 
/*
.....added a check for UM_POINT and UM_FRSTDERIV to avoid UMR in purify
*/
			if(evflg > UM_POINT)
			{
        			uvcv->dcdu[i] = uvcv->dcdu[i] / dup[i];
        			if(evflg > UM_FRSTDERIV)
                			uvcv->d2cdu2[i] =uvcv->d2cdu2[i]/dup[i];
			}
		}
		if (evflg >= UM_CURVATURE)
			um_get_curvature (uvcv->dcdu, uvcv->d2cdu2, &uvcv->curv);
	}
	return(status);
}

/*********************************************************************
**    E_FUNCTION: um_ev7_uvev_to_cvev (evflg,cvptr,tfmat,uvcv,evout)
**       Convert a UV curve evaluation record to an XYZ evaluation
**       record.
**    PARAMETERS   
**       INPUT  : 
**          evflg    flag specifying data to evaluate
**          cvptr    pointer to parametric curve record
**          eptr     pointer to base surface record
**          tfmat    transformation matrix for SF.
**          uvcv     pointer to (u,v) curve evaluation record
**       OUTPUT :  
**          evout    pointer to (x,y,z) CV evaluation record
**    RETURNS      : 
**       UU_SUCCESS if no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int um_ev7_uvev_to_cvev(evflg,cvptr,eptr,tfmat,uvcv,evout)
int evflg;
struct UM_uvcvonsf_rec *cvptr;
struct UM_rbsplsrf_rec *eptr;
UM_transf tfmat;
struct UM_evcrvout *evout, *uvcv;
{
	int status, flag, i;
	UU_REAL uu,vv,du,dv;
	UM_vector d1c, d2c, d1c2;
	struct UM_evsrfout evsrf;
/*
.....Evaluate surface at UV
*/
	uc_init_evsrfout (eptr, &evsrf); 
	uu   = MAX2(0.,uvcv->cp[0]);
	uu   = MIN2(1.,uu); 
	vv   = MAX2(0.,uvcv->cp[1]); 
	vv   = MIN2(1.,vv); 
	flag = evflg;
	if (flag > UM_SECDERIV) flag = UM_SECDERIV;
	status = uc_evsrf(flag, uu, vv, eptr, UM_DEFAULT_TF, &evsrf);
	if (status != UU_SUCCESS) goto done;
/*
.....Convert UV evaluation to XYZ evaluation
*/
	if (evflg >= UM_POINT)
	{
		um_vctovc (evsrf.sp,evout->cp);
	} 
	if (evflg >= UM_FRSTDERIV)
	{
		um_vctovc (uvcv->dcdu,d1c);   
		for (i=0; i<3; i++)
			evout->dcdu[i] = evsrf.dsdu[i]*d1c[0] + evsrf.dsdv[i]*d1c[1];       
	} 
	if (evflg >= UM_SECDERIV)
	{
		d1c2[0] = d1c[0]*d1c[0];
		d1c2[1] = d1c[1]*d1c[1];
		um_vctovc (uvcv->d2cdu2,d2c);   
		for (i=0; i<3; i++)
		{
			du = evsrf.d2sdu2[i]*d1c2[0] + evsrf.dsdu[i]*d2c[0];
			dv = evsrf.d2sdv2[i]*d1c2[1] + evsrf.dsdv[i]*d2c[1];
			evout->d2cdu2[i] = du + dv;       
		}
	}
	if (evflg >= UM_CURVATURE)
	{
		um_get_curvature (evout->dcdu, evout->d2cdu2, &evout->curv);
	}
/*
.....Transform results
*/
	status = ncl_transform_evcrvout (evflg, tfmat, evout);
/*
.....End of routine
*/
done:;
	return(status);
}
