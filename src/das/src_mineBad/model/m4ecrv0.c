
/*********************************************************************
**    NAME         :  m4ecrv0.c
**       CONTAINS: AG curve evaluator support routines
**			int um_agcrv_utot(crvptr, u, t)
**			int um_agcrv_ttou(crvptr, t, u)
**			int um_agcrv_evaluate(evflag, u, eptr, tfmat, evcrv)
**			int um_agcrv_cctou(crvptr, tfmat, pt, u, distp)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m4ecrv0.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:02
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "mcrv.h"
#include "mdeval.h"
#include "mdebug.h"

#include "ag_incl.h"
#include "ag_global.h"

/*********************************************************************
**    E_FUNCTION     : int um_agcrv_utot(crvptr, u, t)
**       Given a curve (CRVPTR) and a logical parameter U in [0.0, 1.0],
**			determine the physical parameter T which corresponds to the
**			logical parameter.
**    PARAMETERS   
**       INPUT  : 
**          crvptr					pointer to curve entity
**				u							logical parameter
**       OUTPUT :  
**				t							physical parameter
**    RETURNS      : 
**          UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : 
**			CRVPTR->CRVADDR->BS is set to the b-spline curve corresponding
**			to T.
**    WARNINGS     : 
**			none
*********************************************************************/
int
um_agcrv_utot(crvptr, u, t)
	struct UM_agcrv_rec *crvptr;
	UM_param u;
	UU_REAL *t;

	{
	UU_REAL t0, t1;
	AG_CURVEP bcp;
	AG_SPLINEP bsp;

	uu_denter(UU_MTRC,(us,"um_agcrv_utot(key=%x, u=%g)", crvptr->key, u));

	/* recast to get local pointer to curve */
	bcp = (AG_CURVEP) crvptr->crvaddr;
	bsp = bcp->bs0;

	/* map [0,1] logical parameter to physical parameter */
	t0 = *(bsp->node0->t);
	t1 = *(bsp->prev->noden->t);
	*t = t0 + ((t1 - t0) * u);

	/* set current b-spline to one corresponding to parameter value t */
	do
		{
		bcp->bs = bsp;
		if (*t > *(bsp->noden->t)) bsp = bsp->next; else bsp = NULL;
		}
	while ((bsp != NULL) && (bsp != bcp->bs0));

	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_agcrv_ttou(crvptr, t, u)
**       Given a curve (CRVPTR) and a physical parameter T,
**			determine the logical parameter U which corresponds to the
**			physical parameter.
**    PARAMETERS   
**       INPUT  : 
**          crvptr					pointer to curve entity
**				t							physical parameter
**       OUTPUT :  
**				u							logical parameter
**    RETURNS      : none
**				UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : 
**				none
*********************************************************************/
int
um_agcrv_ttou(crvptr, t, u)
	struct UM_agcrv_rec *crvptr;
	UU_REAL t;
	UM_param *u;

	{
	UU_REAL t0, t1;
	AG_CURVEP bc;

	uu_denter(UU_MTRC,(us,"um_agcrv_ttou(key=%x, t=%g)", crvptr->key, t));

	/* recast to get local pointer to curve */
	bc = (AG_CURVEP) crvptr->crvaddr;

	/* map [0,1] logical parameter to physical parameter */
	t0 = *(bc->bs0->node0->t);
	t1 = *(bc->bs0->noden->t);
	*u = (t - t0) / (t1 - t0);

	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_agcrv_evaluate(evflag, u, eptr, tfmat, evcrv)
**       Evaluate a rational bspline curve at a specified LOGICAL
**			parameter U.
**    PARAMETERS   
**       INPUT  : 
**				evflag					flag specifying data to evaluate
**				u							logical parameter (0.0 <= u <= 1.0)
**				eptr						pointer to rational bspline curve
**				tfmat						transformation matrix
**       OUTPUT :  
**				evcrv						pointer to curve evaluator record
**    RETURNS      : 
**			UU_SUCCESS iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_agcrv_evaluate(evflag, u, eptr, tfmat, evcrv)
	int evflag;
	UM_param u;
	struct UM_agcrv_rec *eptr;
	UM_transf tfmat;
	struct UM_evcrvout *evcrv;

	{
	int status;
	UU_REAL t;

	uu_denter(UU_MTRC,(us,"um_agcrv_evaluate(evflag=%d, u=%g, key=%x)",
		evflag, u, eptr->key));

	/* map [0,1] logical user parameter range to physical range */
	um_agcrv_utot(eptr, u, &t);

	status = umi_agcrv_evaluate(evflag, t, eptr, tfmat, evcrv);

	uu_dexitstatus("um_agcrv_evaluate", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION :  um_agcrv_cctou(crvptr, tfmat, pt, u, distp)
**			Determine the logical parameter U such that the curve (EPTR)
**			evaluated at U will result in the specified point (PT).
**    PARAMETERS   
**       INPUT  : 
**          crvptr		--	pointer to curve entity
**				tfmat 		--	transformation from curve space to MCS space
**				pt				--	point in space MCS
**       OUTPUT :  
**          *u				--	INTERNAL parameter for intersection point
**				*distp		--	distance from pt to C(u)
**    RETURNS      : 0 if OK, UM_FAILURE if inversion screws up,
**							UM_UNIMPLEMENTED
**    SIDE EFFECTS : none
**    WARNINGS     : although this routine suffices to find the "nearest
**				point" on a curve in some cases, no assurance is made unless
**				the point is very close to the curve. 
*********************************************************************/
um_agcrv_cctou(crvptr, tfmat, pt, u, distp)
	struct	UM_agcrv_rec	*crvptr;
	UM_transf	tfmat;
	UM_coord		pt;
	UU_REAL		*u;
	UU_REAL		*distp;

	{
	int status = UU_SUCCESS;
	UM_transf tinv;
	UM_coord dpt;
	struct UM_evcrvout evcrv;
	AG_CURVEP cv;
	UU_REAL t;

	uu_denter(UU_MTRC,(us,"um_agcrv_cctou(eptr(r,k,t)=(%d,%x,%x), pt=<%g,%g,%g>)", 
				crvptr->rel_num, crvptr->key, tfmat, pt[0], pt[1], pt[2]));
	
	/* invert point to curve definition space	*/
	if (tfmat != UM_DEFAULT_TF)
		{
		um_inverttf(tfmat, tinv);
		um_cctmtf(pt, tinv, dpt);
		}
	else
		{
		um_vctovc(pt, dpt);
		}

	/* calculate the physical parameter T and the logical parameter U 
		for the point on the curve */
	cv = (AG_CURVEP) crvptr->crvaddr;
	ag_cls_pt_crv(cv, dpt, &t);
	um_agcrv_ttou(crvptr, t, u);

	/* test for error */
	um_agcrv_evaluate(UM_POINT, *u, crvptr, UM_DEFAULT_TF, &evcrv);
	*distp = um_dcccc(dpt, evcrv.cp);
	uu_dprint(UU_MTRC, (us, "um_agcrv_cctou: returns: u = %g, distance = %g",
		*u,  *distp));

	uu_dexitstatus("um_agcrv_cctou", status);
	return (status);
	}

