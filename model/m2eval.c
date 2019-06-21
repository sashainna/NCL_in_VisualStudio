/*********************************************************************
**    NAME         :  m2eval.c
**       CONTAINS: routines to initialize evaluator records
**			int um_init_evcrvout(eptr, evoutptr)
**			int um_init_evsrfout(eptr, evoutptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2eval.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:47
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mdrel.h"
#include "mdclass.h"
#include "mdeval.h"
#include "mcrv.h"
#include "msrf.h"

/*********************************************************************
**    E_FUNCTION: int um_init_evcrvout(eptr, evoutptr)
**			Initialize an evaluator record for curves. This function must
**			be called prior to the first call to a curve evaluator for the
**			specified entity.
**    PARAMETERS   
**       INPUT: 
**				eptr					the entity to have an evaluator record
**										set up for.
**       OUTPUT:  
**				evoutptr				pointer to the set up evaluator record. 
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_init_evcrvout(eptr, evoutptr)
	struct UM_crvdatabag *eptr;
	struct UM_evcrvout *evoutptr;

	{
	int status = UU_FAILURE;

	uu_denter(UU_MTRC,(us, "um_init_evcrvout(rel_num=%d)", eptr->rel_num));

	if (uc_super_class(eptr->rel_num) == UM_CURVE_CLASS)
		{
		evoutptr->firsteval = UU_TRUE;
		status = UU_SUCCESS;
		}

	uu_dexitstatus("um_init_evcrvout", status);
	return (status);
	}

/*********************************************************************
**    E_FUNCTION: int um_init_evsrfout(eptr, evoutptr)
**			Initialize an evaluator record for surfaces; Note this function
**			should be called before the surface evaluator is called.
**    PARAMETERS   
**       INPUT: 
**				eptr					the entity to have an evaluator record
**										set up for.
**				evoutptr				pointer to the evaluator record to be set up.
**       OUTPUT:  
**				evoutptr				pointer to the set up evaluator record. 
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_init_evsrfout(eptr, evsrfptr)
	struct UM_entitydatabag *eptr;
	struct UM_evsrfout *evsrfptr;

	{
	int status;

	uu_denter(UU_MTRC,(us, "um_init_evsrfout(eptr->rel_num:%d, evsrfptr:%x)", 
											eptr->rel_num, evsrfptr));
	status = UU_FAILURE;
	if (uc_super_class(eptr->rel_num) == UM_SURFACE_CLASS)
		{
		switch(eptr->rel_num)
			{
			default: 
				evsrfptr->firsteval = UU_TRUE;
				break;
			}
		status = UU_SUCCESS;
		}
	uu_dexitstatus("um_init_evsrfout", status);
	return (status);
	}

