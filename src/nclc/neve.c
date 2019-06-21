/*********************************************************************
**    NAME         :  nevec.c
**       CONTAINS: routines to handle NCL vectors
**       int ncl_vector_to_nclvec(e, ncl)
**       int ncl_nclvec_to_vector(ncl, e)
**       ncl_p_nclvec(ncl)
**       ncl_p86_vector(eptr)
**       ncl_ev_vector(eptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       neve.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:56
*********************************************************************/

#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "mdeval.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mdebug.h"
#include "mdcoord.h"

#include "nccs.h"
#include "ncl.h"
#include "nclvx.h"

/*********************************************************************
**    E_FUNCTION     : int ncl_vector_to_nclvec(e, ncl)
**       Convert a UNICAD vector to an NCL  vector.
**    PARAMETERS   
**       INPUT  : 
**          e                 UNICAD vector entity
**       OUTPUT :  
**          ncl               buffer to place NCL vector
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_vector_to_nclvec(e, ncl)
   struct NCL_vector_rec *e;
   struct NCLI_vector_rec *ncl;

   {
   int status;

   uu_denter(UU_MTRC,(us,"ncl_vector_to_nclvec(key=%x, ncl=%x)",
      e->key, ncl));

   status = UU_SUCCESS;
   ncl_uureal_to_real8(3, e->vec, ncl->vec);
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_nclvec_to_vector(ncl, e)
**       Convert an NCL vector to a UNICAD vector.
**    PARAMETERS   
**       INPUT  : 
**          ncl                  buffer holding NCL vector
**       OUTPUT :  
**          e                    UNICAD vector entity
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_nclvec_to_vector(ncl, e)
   struct NCLI_vector_rec *ncl;
   struct NCL_vector_rec *e;

   {
   int status;

   uu_denter(UU_MTRC,(us,"ncl_nclvec_to_vector(ncl=%x, e=%x)",
      ncl, e));
   status = UU_SUCCESS;
   e->rel_num = NCL_VECTOR_REL;
   ncl_real8_to_uureal(3, ncl->vec, e->vec);
   uu_dexit;
   return (status);
   }

/*********************************************************************
**    E_FUNCTION     : int ncl_nclve_to_unive(nclve, unive)
**			Convert an NCL vector to a UNICAD line
**    PARAMETERS   
**       INPUT  : 
**          nclln						NCL line entity
**       OUTPUT :  
**          uniln						UNICAD line entity
**    RETURNS      : 
**			UU_SUCCESS iff no error
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
/* implimented by kathy for drawing */

ncl_nclve_to_unive(nclve, unive)
	struct NCL_vector_rec *nclve;
	struct UM_line_rec *unive;

	{
	int status;
	UM_vector vec1;
    char label[100];

	uu_denter(UU_MTRC,(us,"ncl_nclve_to_unive(key=%x)", nclve->key));

	/* added  to correct the label on drawing.  kathy*/
	ncl_get_label_with_key(nclve->key, label);
	strncpy (unive->label, label, NCL_MAX_LABEL);
	nclve->subscr = nclve->subscr;

	status = UU_SUCCESS;
	unive->rel_num = UM_LINE_REL;
	vec1[0] = 0.0;
	vec1[1] = 0.0;
	vec1[2] = 0.0;
	um_vctovc(vec1, unive->spt);
	um_vctovc(nclve->vec, unive->ept);
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : ncl_p_nclvec(ncl)
**       Print an NCL vector.
**    PARAMETERS   
**       INPUT  : 
**          ncl               buffer holding NCL vector.
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_p_nclvec(ncl)
   struct NCLI_vector_rec *ncl;

   {
   uu_denter(UU_MTRC,(us,"ncl_p_nclvec(ncl=%x)",ncl));
   sprintf(UM_sbuf,"NCLVECTOR:");
   um_pscroll(UM_sbuf);
   sprintf(UM_sbuf,"  vec=(%g,%g,%g)", ncl->vec[0], ncl->vec[1], ncl->vec[2]);
   um_pscroll(UM_sbuf);
   uu_dexit;
   }

/*********************************************************************
**    E_FUNCTION     : ncl_p86_vector(eptr)
**       Print an UNIBASE vector entity.
**    PARAMETERS   
**       INPUT  : 
**          eptr              vector entity
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_p86_vector(eptr)
   struct NCL_vector_rec *eptr;

   {
   uu_denter(UU_MTRC,(us,"ncl_p86_vector(key=%x)",eptr->key));
   sprintf(UM_sbuf,"VECTOR:");
   um_pscroll(UM_sbuf);

   sprintf(UM_sbuf, "label %s",eptr->label);
   um_pscroll(UM_sbuf);

   sprintf(UM_sbuf,"  vec=(%g,%g,%g)", eptr->vec[0], eptr->vec[1], eptr->vec[2]);
   um_pscroll(UM_sbuf);
   uu_dexit;
   }

/*********************************************************************
**  E_FUNCTION: int ncl_ev_vector(evflag,u,eptr,tfmat,evout)
**          evaluate a vector at a parameter
**      PARAMETERS:
**          INPUT:
**              evflag        UM_POINT= calculate point on circle only;
**                        UM_FRSTDERIV= calculate point and 1st
**                                  derivative;
**                        UM_SECDERIV= calculate point, 1st and
**                                  2nd derivative;
**                        UM_CURVATURE= calc point, 1st, 2nd deriv,
**                                 and curvature;
**              u                   the parameter value
**              eptr                pointer to the entity data
**              tfmat               transformation matrix.
**          OUTPUT:
**              evout               pointer to a curve evaluator
**                         record containing both the requested
**                         information, and (ultimately) a
**                         status value indicating the validity
**                         of the requested information.
**  RETURNS : nothing currently, ultimately will return one of the
**              following:
**            UM_VALID: all requested fields are valid;
**            UM_BADFIELDS: at least one requested fields is invalid;
**            UM_BADRECORDS: at least one entire record is invalid;
**            UM_INVALID: all output is suspect.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
int
ncl_ev_vector(evflag,u,eptr,tfmat, evout)
int evflag;
UM_param u;
struct NCL_vector_rec *eptr;
UM_transf tfmat;
struct UM_evcrvout *evout;
	{
	int status;
	struct UM_line_rec unientity;

	ncl_nclve_to_unive(eptr, &unientity);
	status = uc_evcrv(evflag, u, &unientity, tfmat, evout);

	return (status);
	}
