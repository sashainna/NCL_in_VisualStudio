/*********************************************************************
**    NAME         :  m3ecomp1.c
**   NOTE - This file contains routines which are used by both NCL & IGES.
**       CONTAINS: composite curve support routines
**			int um_ev5_compcrv(evflag, u, crvptr, tfmat, crvout) 
**			int um_reverse_compcrv(eptr)
**       build_cmd(previw)
**       OnSelect(fieldno, val, stat)
**       OnOffset(fieldno, val, stat)
**       OnEdit(fieldno, val, stat)
**       OnAction(fieldno, val, stat)
**       umu_cv_offset_comp()
**       um_compcrv_connect
**       um_update_norm
**       um_compcrv_deloop
**       umf_offset_compcrv_comps
**       um_offset_compcrv_comps
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3ecomp1.c , 25.1 
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:52 
*********************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "class.h"
#include "mdclass.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattr.h"
#include "mcrv.h"
#include "modef.h"
#include "mdeval.h"
#include "mdebug.h"
#include "mfort.h"
#include "nccs.h"
#include "tzmdrel.h"
#include "misect.h"
#include "nconst.h"
#include "nclmplay.h"
#include "mdpick.h"
#include "mxxx.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclvx.h"
#include "nclupok.h"
#include "vsegbf.h"

#include "dasnog.h"
#include "dasg.h"
#include "dselmask.h"
#include "mgeom.h"

/*******************************************************************
**  E_FUNCTION: int um_ev5_compcrv(evflag, u, crvptr, tfmat, crvout) 
**		Given a composite curve entity (CRVPTR) with associated transformation
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
int
um_ev5_compcrv(evflag, u, eptr, tfmat, evoutptr)
	int evflag;
	UM_param u;
	struct UM_compcrv_rec *eptr;
	UM_transf tfmat;
	struct UM_evcrvout *evoutptr;

	{
	int i;							/* index */
	UU_REAL uu;						/* parameter u converted to account for
											forward/reverse direction of constituent */
	UU_REAL unew;					/* parameter on constituent curve */
	UU_REAL u0,u1;					/* start and end parameter range u0 <= u < u1 */
	UU_REAL t0,t1;					/* start and end parameter range t0 <= u < t1 */
	UU_REAL cu;                /* u parameter converted to t0->t1 range */
	UU_LOGICAL reverse;
	int status;

	t0 = eptr->t0;
	t1 = eptr->t1;

	cu = t0 + (u * (t1 - t0));

	uu_denter(UU_MTRC, (us,
		"um_ev5_compcrv(evflag:%d,u:%f,eptr->key:%d,tfmat:%x,%x)",
					evflag,u, eptr->key,tfmat,evoutptr));
	for (i=0; (i<eptr->no_cid-1) && (cu >= eptr->cid[i].endparam); i++);

	/* if the subcurve we are currently on is not the right one, or, this is the
	 * first time for the evaluation of this curve, then get a new subcurve
	 */
	if ((evoutptr->firsteval) || (eptr->cid[i].crvid != evoutptr->subcrv.key))
		{
		evoutptr->subcrv.key = eptr->cid[i].crvid;
		uc_retrieve_data(&(evoutptr->subcrv), sizeof(struct UM_crvdatabag));
		uc_retrieve_transf(evoutptr->subcrv.key, evoutptr->subcrv_tfmat);
		um_tftmtf(tfmat, evoutptr->subcrv_tfmat, evoutptr->subcrv_tfmat);
		evoutptr->firsteval = UU_FALSE;
		}

	reverse = (eptr->cid[i].reverse);
	if (i==0) u0 = 0.0; else u0 = eptr->cid[i-1].endparam;
	u1 = eptr->cid[i].endparam;
	uu = (cu - u0) / (u1 - u0);
	if (reverse) unew = 1. - uu; else unew = uu; 
	status = uc_evcrv(evflag,unew, &(evoutptr->subcrv), evoutptr->subcrv_tfmat, evoutptr);
	umi_adjust_evaluatorec(evflag, reverse, eptr, i, evoutptr); 
	uu_dexit;
	return(status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_reverse_compcrv(eptr)
**			Reverse the parameterization of the compcrv.
**    PARAMETERS   
**       INPUT  : 
**          eptr						pointer to compcrv
**       OUTPUT :  
**          eptr						pointer to compcrv
**    RETURNS      : 
**			UU_SUCCESS iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_reverse_compcrv(eptr)
	struct UM_compcrv_rec *eptr;

	{
	int i,j;
	struct UM_compcrv_rec *e;

	uu_denter(UU_MTRC,(us,"um_reverse_compcrv(key=%d)", eptr->key));

	e = (struct UM_compcrv_rec *) uu_malloc(sizeof(struct UM_compcrv_rec));
	ur_setup_data(UM_COMPCRV_REL, e, sizeof(struct UM_compcrv_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e->label, "");
	e->subscr = 0;

	for (i=0, j=(eptr->no_cid-1); i<eptr->no_cid; i++, j--)
		{
		e->cid[i].crvid = eptr->cid[j].crvid;
		e->cid[i].reverse = !eptr->cid[j].reverse;
		if (j==0)
			e->cid[i].endparam = 1.0;
		else
			e->cid[i].endparam = 1.0 - eptr->cid[j-1].endparam;
		}

	for (i=0; i<eptr->no_cid; i++) eptr->cid[i] = e->cid[i];

	uu_free(e);

	uu_dexit;
	return (UU_SUCCESS);
	}
