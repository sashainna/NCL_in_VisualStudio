/*********************************************************************
**    NAME         :  neeval.c
**       CONTAINS:
**
**           uc_evcrv
**           nclx_evcrv
**           uc_evsrf
**           nclx_evsrf
**           ncl_triansf_fix_norm
**
**    COPYRIGHT 1996 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       neeval.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:32
*********************************************************************/

#include "usysdef.h"
#include "mdcoord.h"
#include "mdeval.h"
#include "nccs.h"
#include "nclx.h"
#include "ycom.h"
#include "uminmax.h"
#include "nclfc.h"
#include "ngeom.h"

extern int NCLX_internal_geom;
/*
...jingrong 02/26/99 Added for evaluator timer.
*/
extern int etim;
extern int ncall;
static int nclx_evcrv ();
static int nclx_evsrf ();

/*******************************************************************
**  E_FUNCTION: int uc_evcrv(evflag, u, crvptr, tfmat, crvout) 
**		Given a curve entity (CRVPTR) with associated transformation
**		matrix (TFMAT which may be UM_DEFAULT_TF), calculate the
**		data requested by EVFLAG at the logical parameter value U
**		[0.0 <= U <= 1.0] and return it in CRVOUT.
**    PARAMETERS   
**       INPUT  : 
**          evflag				UM_POINT=>		point
**										UM_FRSTDERIV=> 1st deriv plus above
**										UM_SECDERIV=>	2nd deriv plus above
**										UM_CURVATURE=> curvature plus above
**				u						parameter value to evaluate curve function
**				crvptr				pointer to curve entity
**				tfmat					transformation matrix
**       OUTPUT :  
**          crvout				curve evaluator record to put results
**    RETURNS      : 
**			UM_VALID only; ultimately will return extended diagnostic
**    SIDE EFFECTS : 
**			The curve evaluator record (CRVOUT) may be updated with
**			information which makes subsequent evaluator calls faster
**    WARNINGS     : 
**			The curve evaluator record (CRVOUT) must be initialized
**			before the first call to uc_evcrv() with a new curve.
*********************************************************************/
int uc_evcrv(evflag, u, crvptr, tfmat, crvout)
int evflag;
UM_param u;
char *crvptr;
UM_transf tfmat;
struct UM_evcrvout *crvout;
{
	int status;
/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   int stims,stimm,etims,etimm;
   ncall = ncall +1;
   gtimx(&stims,&stimm);

	if (NCLX_internal_geom)
		status = nclx_evcrv (u, crvptr, crvout);
	else
		status = uc_evcrv1 (evflag, u, crvptr, tfmat, crvout);
/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   gtimx(&etims,&etimm);
   etim = etim + ((etims-stims)*1000 + (etimm - stimm));

	return (status);
}

/*******************************************************************
**  E_FUNCTION: int nclx_evcrv (evflag, u, crvptr, tfmat, crvout) 
**		Evaluate external curve.
**    PARAMETERS   
**       INPUT  : 
**          u                 parameter value to evaluate curve function
**          eptr              pointer to curve entity
**       OUTPUT :  
**          crvout            curve evaluator record to put results
**    RETURNS      : 
**         UM_VALID only; ultimately will return extended diagnostic
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int nclx_evcrv (u, eptr, evcrv)
UM_param u;
char *eptr;
struct UM_evcrvout *evcrv;
{
	int status = UU_SUCCESS;
	NCLX_mdl_curve *ncv;
	NCLX_mdl_composite *ccv;
	int (*func)();
	int NclxMdlEvalLine();
	int NclxMdlEvalCircle();
	int NclxMdlEvalPolyline();
	NCLX_mdl_curve_eval eval;

	ncv = (NCLX_mdl_curve *)eptr;
	switch (ncv->header.relnum)
	{
	case NCLX_MDL_CURVE:
	case NCLX_MDL_BSPLINE:
		func = (int(*) ())ncv->evaluator;
		break;
	case NCLX_MDL_COMPOSITE:
		ccv = (NCLX_mdl_composite *)ncv;
		func = (int(*) ())ccv->evaluator;
		break;
	case NCLX_MDL_LINE:
		func = NclxMdlEvalLine;
		break;
	case NCLX_MDL_CIRCLE:
		func = NclxMdlEvalCircle;
		break;
/*
.....Needed polyline evaluator since the addition of
.....polylines in the OML. JLS 9/21/99
*/
	case NCLX_MDL_POLYLINE:
		func = NclxMdlEvalPolyline;
		break;
	default:
		status = UU_FAILURE;
		break;
	}

	if (status == UU_SUCCESS)
	{
		u = MIN2 (1., MAX2 (0.,u));
	status = (*(func))(ncv,u,&eval);
#ifdef TEMP_FIX
	status = 0; /* TEMP-FIX */
#endif
	if (status != 0) status = 466;
		if (status == UU_SUCCESS)
		{
			um_vctovc (eval.pt,    evcrv->cp);
			um_vctovc (eval.udrv1, evcrv->dcdu);
			um_vctovc (eval.udrv2, evcrv->d2cdu2);
			evcrv->curv = eval.ucrv;
/*   ------   FIX - Put this code in if user evaluator does not calculate */
/*                  the radius of curvature                               */
			um_get_curvature (eval.udrv1, eval.udrv2, &evcrv->curv);
		}
	}

	NclxDbgPmdlEvalcv (u,ncv,&eval);
	NclxDbgEvalRtn ("Curve",status);

   return (status);
}

/*********************************************************************
*********************************************************************/
static void Swap_reals (a,b)
UU_REAL *a,*b;
{
	UU_REAL c;

	c = *a;
	*a = *b;
	*b = c;
}

/*********************************************************************
**    E_FUNCTION     : int uc_evsrf (evflag, u, v, srfptr, tfmat, srfout)
**    Given a surface entity (SRFPTR) with associated transformation
**    matrix (TFMAT which may be UM_DEFAULT_TF), calculate the
**    data requested by EVFLAG at the logical parameter value U,V
**    [0.0 <= U,V <= 1.0].
**    PARAMETERS
**       INPUT  :
**          evflag               UM_POINT    => point on surface
**                               UM_NORM     => surface normal, plus above
**                               UM_FRSTDERIV=> first cross derivatives,
**                                              plus above
**                               UM_SECDERIV => second cross derivatives,
**                                              plus above
**                               UM_CURVATURE=> curvature, plus above
**          u,v                  logical parameter at which to evaluate
**                               surface equation (i.e. 0.0 <= U,V <= 1.0)
**          srfptr               pointer to surface entity
**          tfmat                transformation matrix positioning surface
**                               in MCS (may be UM_DEFAULT_TF)
**       OUTPUT :
**          srfout               pointer to surface evaluator record
**    RETURNS      :
**       UM_VALID:         all requested fields are valid;
**       UM_BADFIELDS:     at least one requested fields is invalid;
**       UM_BADRECORDS:    at least one entire record is invalid;
**       UM_INVALID:       all output is suspect.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int uc_evsrf (evflag, u, v, srfptr, tfmat, srfout)
int evflag;
UM_param u,v;
char *srfptr;
UM_transf tfmat;
struct UM_evsrfout *srfout;
{
	int status,iswap;
	UU_LOGICAL revnorm,urev,vrev,uvswap;
	UM_param uu,vv,uvtmp;
/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   int stims,stimm,etims,etimm;
   ncall = ncall +1;
   gtimx(&stims,&stimm);

	if (NCLX_internal_geom)
		status = nclx_evsrf(evflag, u, v, srfptr, srfout);
	else
	{
		ncl_get_redef_params (srfptr,&iswap,&revnorm);
		if (iswap > 0 && iswap < 8)
		{
			urev = iswap & 4;
			vrev = iswap & 2;
			uvswap = iswap & 1;
		}
		else
			urev = vrev = uvswap = UU_FALSE;		
	
		uu = u; vv = v;

		if (urev)
		{
			uu = 1 - uu;
			revnorm = !revnorm;
		}
		if (vrev)
		{
			vv = 1 - vv;
			revnorm = !revnorm;
		}
		if (uvswap)
		{
			uvtmp = vv; vv = uu; uu = uvtmp;
			revnorm = !revnorm;
		}

		status = uc_evsrf1(evflag, uu, vv, srfptr, tfmat, srfout);

		if (status == UU_SUCCESS && evflag >= UM_NORM)
		{
			if (uvswap)
				um_swapvc (srfout->dsdu,srfout->dsdv);
			if (urev)
				um_negvc (srfout->dsdu,srfout->dsdu);
			if (vrev)
				um_negvc (srfout->dsdv,srfout->dsdv);
			if (revnorm)
				um_negvc (srfout->snorm,srfout->snorm);
			if (evflag >= UM_SECDERIV)
			{
				if (uvswap)
					um_swapvc (srfout->d2sdu2,srfout->d2sdv2);
				if (urev)
					um_negvc (srfout->d2sdu2,srfout->d2sdu2);
				if (vrev)
					um_negvc (srfout->d2sdv2,srfout->d2sdv2);

				if (uvswap && evflag >= UM_CURVATURE)
					Swap_reals (&srfout->ucurv,&srfout->vcurv);
			}
		}
	}
/*
...jingrong 02/26/99 Added for evaluator timer.
*/
   gtimx(&etims,&etimm);
   etim = etim + ((etims-stims)*1000 + (etimm - stimm));

	return(status);
}

/*********************************************************************
**    E_FUNCTION     : int nclx_evsrf(evflag, u, v, srfptr, tfmat, srfout)
**    Given a surface entity (SRFPTR) with associated transformation
**    matrix (TFMAT which may be UM_DEFAULT_TF), calculate the
**    data requested by EVFLAG at the logical parameter value U,V
**    [0.0 <= U,V <= 1.0].
**    PARAMETERS
**       INPUT  :
**          evflag               UM_POINT    => point on surface
**                               UM_NORM     => surface normal, plus above
**                               UM_FRSTDERIV=> first cross derivatives,
**                                              plus above
**                               UM_SECDERIV => second cross derivatives,
**                                              plus above
**                               UM_CURVATURE=> curvature, plus above
**          u,v                  logical parameter at which to evaluate
**                               surface equation (i.e. 0.0 <= U,V <= 1.0)
**          srfptr               pointer to surface entity
**       OUTPUT :
**          evsrf                pointer to surface evaluator record
**    RETURNS      :
**       UM_VALID:         all requested fields are valid;
**       UM_BADFIELDS:     at least one requested fields is invalid;
**       UM_BADRECORDS:    at least one entire record is invalid;
**       UM_INVALID:       all output is suspect.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
static int nclx_evsrf (evflag, u, v, srfptr, evsrf)
int evflag;
UM_param u, v;
char *srfptr;
struct UM_evsrfout *evsrf;
{
	int nclxflag,status = UU_SUCCESS;
	NCLX_mdl_surf *nsf;
	NCLX_mdl_trimsf *tsf;
	NCLX_mdl_surf_eval eval;
	int (*func)();

	tsf = (NCLX_mdl_trimsf *)srfptr;

	switch (tsf->header.relnum)
	{
	case NCLX_MDL_TRIMSF:
		nsf = tsf->surf;
		break;
	case NCLX_MDL_SURF:
	case NCLX_MDL_NSURF:
		nsf = (NCLX_mdl_surf *)srfptr;
		break;
	default:
		status = UU_FAILURE;
		break;
	}

	if (status == UU_SUCCESS)
	{
		func = (int(*) ())nsf->evaluator;
		u = MIN2 (1., MAX2 (0.,u));
		v = MIN2 (1., MAX2 (0.,v));
		nclxflag = (evflag <= UM_FRSTDERIV)? 1: 2;
		status = (*(func)) (nsf, u, v, &eval, nclxflag);
#ifdef TEMP_FIX
		status = 0;  /* TEMP-FIX */
#endif
		if (status != 0) status = 466;
		if (status == UU_SUCCESS)
		{
			um_vctovc (eval.pt,     evsrf->sp);
			um_vctovc (eval.normal, evsrf->snorm);
			um_vctovc (eval.udrv1,  evsrf->dsdu);
			um_vctovc (eval.vdrv1,  evsrf->dsdv);
			if (evflag > UM_FRSTDERIV)
			{
				um_vctovc (eval.udrv2,  evsrf->d2sdu2);
				um_vctovc (eval.vdrv2,  evsrf->d2sdv2);
				evsrf->ucurv = eval.ucrv;
				evsrf->vcurv = eval.vcrv;
			}
		}
	}

	NclxDbgEvalRtn("Surface",status);

	return (status);
}

/*********************************************************************
**  FUNCTION :  void ncl_triansf_fix_norm (u,v,srf,tfmat,snorm,tolsq)
**    Fix zero norm vector for a triangular surface.
**
**  PARAMETERS   
**     INPUT  : 
**        u,v                  surface parameters
**        srf                  pointer to surface entity
**        tfmat                surface transformation matrix
**        snorm                surface normal
**        tolsq                squared tolerance
**     OUTPUT :  
**        snorm                surface normal maybe fixed
**  RETURNS      : none
**     UU_SUCCESS iff no error; else UU_FAILURE
**  SIDE EFFECTS : none
**  WARNINGS     : none
*********************************************************************/
void ncl_triansf_fix_norm (u,v,srf,tfmat,snorm,tolsq)
UU_REAL u,v,tolsq;
char *srf;
UM_transf tfmat;
UM_vector snorm;
{
	struct UM_evsrfout evsrf;
	int status;
	UU_REAL dd,dd1,du,dv;

	dd = UM_DOT (snorm,snorm);
	if (dd < tolsq)
	{
		du = dv = 0;
		if (NCL_triansf == 1 && u == 0)
			du = 0.0001;
		else if (NCL_triansf == 2 && u == 1)
			du = -0.0001;
		else if (NCL_triansf == 3 && v == 0)
			dv = 0.0001;
		else if (NCL_triansf == 4 && v == 1)
			dv = -0.0001;

		status = uc_evsrf(UM_NORM, u+du, v+dv, srf, tfmat, &evsrf);
		if (status == UU_SUCCESS)
		{
			dd1 = UM_DOT (evsrf.snorm,evsrf.snorm);
			if (dd1 > 4*tolsq)
				um_vctovc (evsrf.snorm,snorm);
		}
	}
}
