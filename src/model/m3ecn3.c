/*********************************************************************
**    NAME         :  m3ecn3.c
**   NOTE - This file contains routines which are used by both NCL & IGES.
**       CONTAINS:
**			int um_ev4_conic(evflag,u,e,tfmat,evout)
**			int um_reverse_conic(eptr)
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m3ecn3.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:52
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mattr.h"
#include "mcrv.h"
#include "modef.h"
#include "mdeval.h"
#include "mdebug.h"

/*****************************************************************************
**  E_FUNCTION:  int um_ev4_conic(evflag,u,e,tfmat,evout)
**
**		DESCRIPTION: Evaluate a conic segment at user's parameter.
**
**		PARAMETERS:   
**			INPUT:
**				PARAMETER      TYPE       MEANING
**				evflag         int      UM_POINT= calculate point on conic only;
**                                  UM_FRSTDERIV= calculate point and 1st 
**                                   derivative;
**                                  UM_SECDERIV= calculate point, 1st and 2nd 
**                                   derivative;
**                                  UM_CURVATURE= calc point, 1st, 2nd deriv, 
**                                   and curvature;
**
**				u					UU_REAL   the parameter value in range [0,1]
**
**				*e			      struct UM_conic_rec	
**                                    pointer to the entity record
**				tfmat				transformation matrix.
**
**			OUTPUT:
**			*evout		      struct UM_evcrvout	
**                                    pointer to a curve evaluator
**                                    record containing both the requested
**                                    information, and (ultimately) a 
**                                    status value indicating the validity
**                                    of the requested information.
**
**		RETURNS : none currently, ultimately will return one of the 
**				following: 
**            UM_VALID: all requested fields are valid;
**            UM_BADFIELDS: at least one requested fields is invalid;
**            UM_BADRECORDS: at least one entire record is invalid;
**            UM_INVALID: all output is suspect.
**
**		CALLING SEQUENCE EXAMPLE:  um_ev4_conic(evflag, u, &e, &evout);
**
**		SIDE EFFECTS :  none
**
**		WARNINGS     :  none
**
**************************************************************************/
int
um_ev4_conic(evflag,u,e,tfmat,evout)

	int  evflag;  /* POINT=calculate pt on conic; UM_FRSTDERIV=1st derivative;
					   *UM_SECDERIV=2nd derivative; UM_CURVATURE=curature*/
	UM_param u;    /* parameter value for new pt on arc, to be found, 
						* u in [0,1] */
	struct UM_conic_rec *e;		/*entity ptr for arc */
	UM_transf tfmat;			/* transformtion matrix */
	struct UM_evcrvout *evout;	/* output structure */
	{
	UU_REAL	t;						/* Internal conic parameter	*/
	UU_REAL	interval;			/* total change in t	(scale)	*/
	UM_transf	net_tfmat;	/* used to hold composition of
										 * tfmat coming down with tform
										 * held in entity record.
										 */

   uu_denter(UU_MTRC,
		(us,"um_ev4_conic(evflag=%d,u=%g,key=%d,tfmat=%x,evout=%x)",
		evflag,u,e->key,tfmat,evout));

	/* get internal parameter and scale factor */
	um_conic_parameter(u, e, &t, &interval);

	/* Do evaluation in internal parameter, answers in 
	 * definition plane
	 */
	um_val4_conic(evflag, t, e, evout);

	/* scale to parameter interval == dt/du	*/
	if (evflag != UM_POINT)
		{
		um_vctmsc(evout->dcdu, interval, evout->dcdu);

		if (evflag != UM_FRSTDERIV)
			um_vctmsc(evout->d2cdu2, interval * interval, evout->d2cdu2);

		}
	/**  CONIC SPECIAL
	 ** compose tfmat with local entity transformation
	 ** This should go away when the local transformation
	 ** is integrated into the unibase transformation
	 ** (by multiplying with the local tf on the left)
	 **/
	um_tftmtf(e->tfmat, tfmat, net_tfmat);


	/* position results in evaluator record according to the transform, tfmat */
	um_transform_evcrvout(evflag, e, net_tfmat, evout);

	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_reverse_conic(eptr)
**			Reverse the parameterization of the conic.
**    PARAMETERS   
**       INPUT  : 
**          eptr						pointer to conic
**       OUTPUT :  
**          eptr						pointer to conic
**    RETURNS      : 
**			UU_SUCCESS iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_reverse_conic(eptr)
	struct UM_conic_rec *eptr;

	{
	int status;
	UM_coord spt;
	UM_coord ept;
	UM_coord temp;
	UM_transf cninv;
	UM_transf rotmat;

	uu_denter(UU_MTRC,(us,"um_reverse_conic(key=%d)", eptr->key));

	/* determine the endpoints of the original conic */
	status = um_get_endpts(eptr, UM_idmat, spt, ept);
	if (status != UU_SUCCESS) goto done;

	/* rotate the original conic 180 about the xaxis to get the reversed conic */
	um_rottf(UM_xaxis, UM_PI, rotmat);
	um_tftmtf(rotmat, eptr->tfmat, eptr->tfmat);

	/* calculate the inverse of the transformation placing the new conic 
		into model space */
	um_inverttf(eptr->tfmat, cninv);

	/* determine the start and end points of the new conic in definition space */
	um_cctmtf(spt, cninv, temp);
	um_cctmtf(ept, cninv, spt);
	um_vctovc(temp, ept);

	/* calculate the new parameter values of the new conic */
	status = um_cn4_endpoints(eptr, spt, ept, cninv);
	if (status != UU_SUCCESS) goto done;

	status = UU_SUCCESS;

done:;
	uu_dexitstatus("um_reverse_conic", status);
	return (status);
	}
