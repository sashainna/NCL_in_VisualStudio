/*********************************************************************
**    NAME         :  m4icrv1.c
**       CONTAINS: routines to support AG curves
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**			int umi_agcrv_delete(crv)
**			int umi_agcrv_copy(e1ptr, e2ptr, bagsize)
**			int umi_agcrv_evaluate(evflag, t, eptr, tfmat, evcrv)
**			int umi_agcrv_split(eptr, t, eptr1, eptr2) 
**			int umi_agcrv_reparam(crvp)
**     MODULE NAME AND RELEASE LEVEL 
**       m4icrv1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:04
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "ulist.h"
#include "go.h"
#include "dasnog.h"
#include "class.h"
#include "mdrel.h"
#include "mattr.h"
#include "mdebug.h"
#include "mdeval.h"
#include "mcrv.h"
#include "modef.h"
#include "mderror.h"

#include "ag_incl.h"
#include "ag_crv_def.h"

/*********************************************************************
**    I_FUNCTION     : int umi_agcrv_delete(crv)
**			Delete the specified curve from AG only.
**    PARAMETERS   
**       INPUT  : 
**				crv						pointer to AG curve entity
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : 
**			it is assumed that ONLY the AG curve data structure must be
**			deleted and that NO curve entity is in UNIBASE.
**
*********************************************************************/
int
umi_agcrv_delete(crv)
	struct UM_agcrv_rec *crv;

	{
	int status;
	int ag_status;
	AG_CURVEP cv;

	uu_denter(UU_MTRC,(us,"umi_agcrv_delete(key=%d)", crv->key));

	status = UU_SUCCESS;
	cv = (AG_CURVEP) crv->crvaddr;
	if (cv > (AG_CURVEP)0)
	  {
	  ag_status = ag_db_crv(&cv);
	  if (ag_status != 0) status = UU_FAILURE;
	  }

done:
	uu_dexitstatus("umi_agcrv_delete",status);
	return (status);
	}

/*********************************************************************
**    I_FUNCTION     : int umi_agcrv_copy(e1ptr, e2ptr, bagsize)
**			Make a copy of the AG curve (E1PTR->CRVADDR) and store the
**			address of the copy in E2PTR->CRVADDR.
**    PARAMETERS   
**       INPUT  : 
**				e1ptr    pointer to entity to be copied
**				bagsize	size of the data bags pointed to by e1 and e2.
**       OUTPUT :  
**				e2ptr    pointer to new entity
**    RETURNS      : 
**				UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umi_agcrv_copy(e1ptr, e2ptr, bagsize)
   struct UM_agcrv_rec *e1ptr;
   struct UM_agcrv_rec *e2ptr;
	int bagsize;

	{
	int status = UU_FAILURE;
	AG_CURVEP  cv;
	AG_CURVEP  ag_crv_copy();
 

   uu_denter(UU_MTRC, (us,"umi_agcrv_copy(key=%x,bagsize=%d)",e1ptr,bagsize));

	um_agcrv_setup_data(UM_AGCRV_REL, e2ptr, sizeof(struct UM_agcrv_rec));

	cv = NULL;
	cv = ag_crv_copy(e1ptr->crvaddr, cv);

	if (cv != NULL)
		{
		status = UU_SUCCESS;
		e2ptr->crvaddr = (int) cv;
		}

	uu_dexitstatus("um_agcrv_copy",status);
	return (status);
	}

/*********************************************************************
**    I_FUNCTION     : int umi_agcrv_evaluate(evflag, t, eptr, tfmat, evcrv)
**       Evaluate a rational bspline curve at a specified PHYSICAL
**			parameter T.
**    PARAMETERS   
**       INPUT  : 
**				evflag					flag specifying data to evaluate
**				t							physical parameter value
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
umi_agcrv_evaluate(evflag, t, eptr, tfmat, evcrv)
	int evflag;
	UU_REAL t;
	struct UM_agcrv_rec *eptr;
	UM_transf tfmat;
	struct UM_evcrvout *evcrv;

	{
	int status;
	int nd;
	AG_CPOINT cp[3];
	AG_CURVEP cv;
	AG_SPLINEP bs;
	UU_REAL t0, t1, t3;

	uu_denter(UU_MTRC,(us,"umi_agcrv_evaluate(evflag=%d, t=%f, key=%x)",
		evflag, t, eptr->key));

	status = UU_SUCCESS;

	cv = (AG_CURVEP) eptr->crvaddr;

	/* initialize curve point data structure */
	ag_set_cp2(&cp[0], &cp[1], &cp[2], evcrv->cp, evcrv->dcdu, evcrv->d2cdu2);

	switch (evflag)
		{
		case UM_FRSTDERIV:
			nd = 1; 
			break;
		case UM_SECDERIV:
			nd = 2; 
			break;
		case UM_POINT:
		default:
			nd = 0;
			break;
		}

	/* evaluate curve at the specified parameter */
	ag_eval_crv(t, nd, cv, &cp[0]);

	switch (evflag)
		{
		case UM_FRSTDERIV:
			cv = (AG_CURVEP) eptr->crvaddr;
			bs = cv->bs0;
			t0 = *(bs->node0->t);
			t1 = *(bs->prev->noden->t);
			t3 = (t1 - t0);
			um_vctmsc(evcrv->dcdu, t3, evcrv->dcdu);
			break;
		case UM_SECDERIV:
		case UM_POINT:
		default:
			break;
		}

/*
	if (evflag >= UM_POINT)
		{
		sprintf(UM_sbuf, "cp(%g) = (%g,%g,%g)", t,
			evcrv->cp[0], evcrv->cp[1], evcrv->cp[2]);
		um_pscroll(UM_sbuf);
		}

	if (evflag >= UM_FRSTDERIV)
		{
		sprintf(UM_sbuf, "dcdu(%g) = (%g,%g,%g)", t,
			evcrv->dcdu[0], evcrv->dcdu[1], evcrv->dcdu[2]);
		um_pscroll(UM_sbuf);
		}

	if (evflag >= UM_SECDERIV)
		{
		sprintf(UM_sbuf, "d2cdu2(%g) = (%g,%g,%g)", t,
			evcrv->d2cdu2[0], evcrv->d2cdu2[1], evcrv->d2cdu2[2]);
		um_pscroll(UM_sbuf);
		}
*/

	uu_dexitstatus("umi_agcrv_evaluate", status);
	return (status);
	}

/*********************************************************************
**    I_FUNCTION     : int umi_agcrv_split(eptr, t, eptr1, eptr2) 
**			Split a curve (EPTR) at a physical parameter T into
**			two pieces (EPTR1, EPTR2). The original is left intact, and 
**			the two pieces are stored in AG.
**    PARAMETERS   
**       INPUT  : 
**          eptr				curve entity
**				t					internal parameter to split curve at
**       OUTPUT :  
**          eptr1				first part  entity
**          eptr2				second part entity
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umi_agcrv_split(eptr, t, eptr1, eptr2)
	struct UM_agcrv_rec *eptr;
	UU_REAL t;
	struct UM_agcrv_rec *eptr1;
	struct UM_agcrv_rec *eptr2;

	{
	int status = UU_FAILURE;
	AG_CURVEP cv;
	AG_CURVEP newcv;
	AG_CURVEP  ag_crv_div();

	uu_denter( UU_MTRC,(us,"umi_agcrv_split(e(r,k)=(%d,=%x), t=%f)",
		eptr->rel_num, eptr->key, t));

	/* copy original curve; this is the one that will be split;
		it is the second part of the split curve */
	status = umi_agcrv_copy(eptr, eptr2, sizeof(struct UM_agcrv_rec));
	if (status != UU_SUCCESS) goto done;

	/* split copy at the intersection parameter */
	cv = (AG_CURVEP) eptr2->crvaddr;
	newcv = ag_crv_div(cv, t);

	/* if the split successfully resulted into two pieces, initialize the
		first part of the split operation; otherwise, delete the copy */
	if ((newcv != NULL) && (newcv != cv))
		{
		um_agcrv_setup_data(UM_AGCRV_REL, eptr1, sizeof(struct UM_agcrv_rec));
		eptr1->crvaddr = (int) newcv;
		status = UU_SUCCESS;
		}
	else
		{
		umi_agcrv_delete(eptr2);
		status = UU_FAILURE;
		}

done:;
	uu_dexitstatus("umi_agcrv_split", status);
	return (status);
	}

/*********************************************************************
**    I_FUNCTION     : int umi_agcrv_reparam(crvp)
**			Reparameterize the AG curve (CRVP) so that the underlying
**			parameter range of the constituent bspline curves is
**			continuous.
**    PARAMETERS   
**       INPUT  : 
**				crvp				AG curve pointer
**       OUTPUT :  none
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
umi_agcrv_reparam(crvp)
	AG_CURVEP crvp;

	{
	int status = UU_FAILURE;
	AG_SPLINEP bsp;

	uu_denter( UU_MTRC,(us,"umi_agcrv_reparam(crvp=%x)", crvp));

	bsp = crvp->bs0;
	ag_crv_bs_merge(crvp, *(crvp->bs0->node0->t));

	status = UU_SUCCESS;

done:;
	uu_dexitstatus("umi_agcrv_reparam", status);
	return (status);
	}

