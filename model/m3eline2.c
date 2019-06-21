/*********************************************************************
**    NAME         :  m3eline.c
**   NOTE - This file contains routines which are used by both NCL & IGES.
**       CONTAINS:
**			int um_ev2_line(evflag,u,eptr,tfmat,evout)
**			int um_reverse_line(eptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3eline2.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:54
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "mdrel.h"
#include "mdcoord.h"
#include "mattr.h"
#include "mdattr.h"
#include "mcrv.h"
#include "mdeval.h"
#include "modef.h"
#include "mplot.h"
#include "mdebug.h"

/********************************************************************* 
**  E_FUNCTION: int um_ev2_line(evflag,u,eptr,tfmat,evout)
**			evaluate a line at a parameter
**  	PARAMETERS:   
**  		INPUT: 
**				evflag        UM_POINT= calculate point on circle only;
**         	              UM_FRSTDERIV= calculate point and 1st 
**                        			derivative;
**                        UM_SECDERIV= calculate point, 1st and 
**                        			2nd derivative;
**                        UM_CURVATURE= calc point, 1st, 2nd deriv, 
**                                 and curvature;
**				u					the parameter value
**				eptr				pointer to the entity data
**				tfmat				transformation matrix.
**			OUTPUT:
**				evout				pointer to a curve evaluator
**                         record containing both the requested
**                         information, and (ultimately) a 
**                         status value indicating the validity
**                         of the requested information.
**  RETURNS : nothing currently, ultimately will return one of the 
**				following: 
**            UM_VALID: all requested fields are valid;
**            UM_BADFIELDS: at least one requested fields is invalid;
**            UM_BADRECORDS: at least one entire record is invalid;
**            UM_INVALID: all output is suspect.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int
um_ev2_line(evflag,u,eptr,tfmat, evout)
   int evflag;
   UM_param u;
   struct UM_line_rec *eptr;
	UM_transf tfmat;
	struct UM_evcrvout *evout;

	{
   int i;

	uu_denter(UU_MTRC,(us,"um_ev2_line(evflag:%d,u:%g,key:%d,tfmat:%x,evout:%x)",
							evflag, u, eptr->key, tfmat, evout));

	switch (evflag)
		{
		case UM_ALL:
		case UM_CURVATURE:
		case UM_SECDERIV:
			evout->curv = 0.0;
			for (i=0 ; i < 3 ; i++) evout->d2cdu2[i] = 0.0;
	  
		case UM_FRSTDERIV:
		case UM_POINT:
			um_vcmnvc (eptr->ept,eptr->spt,evout->dcdu);
			for (i=0; i<3; i++)
				evout->cp[i] = eptr->spt[i] + u * evout->dcdu[i];
			break;

		default:
			uu_uerror0(/*error - illegal evaluation request*/UM_MODEL,50);
			break;
		}
	/* position results in evaluator record according to the transform, tfmat */
	um_transform_evcrvout(evflag, eptr, tfmat, evout);
	uu_dexit;
	return (UU_SUCCESS);
	}

/*********************************************************************
**    E_FUNCTION     : int um_reverse_line(eptr)
**			Reverse the parameterization of the line.
**    PARAMETERS   
**       INPUT  : 
**          eptr						pointer to line
**       OUTPUT :  
**          eptr						pointer to line
**    RETURNS      : 
**			UU_SUCCESS iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_reverse_line(eptr)
	struct UM_line_rec *eptr;

	{
	int status;
	UM_coord temp;

	uu_denter(UU_MTRC,(us,"um_reverse_line(key=%d)", eptr->key));

	um_vctovc(eptr->spt, temp);
	um_vctovc(eptr->ept, eptr->spt);
	um_vctovc(temp, eptr->ept);

	uu_dexit;
	return (UU_SUCCESS);
	}
