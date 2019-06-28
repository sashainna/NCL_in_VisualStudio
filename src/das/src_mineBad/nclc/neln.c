/*********************************************************************
**    NAME         :  ncline.c
**       CONTAINS: routines to manipulate NCL line representations
**       int ncl_line_to_nclline(e, ncl)
**       int ncl_uline_to_nclline(e, ncl)
**       int ncl_nclline_to_line(ncl, e)
**       ncl_p_nclline(ncl)
**       ncl_p89_line(ptr)
**       ncl_line_transl(e,offset)
**       ncl_ev_line(evflag,u,eptr,tfmat, evout)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       neln.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:08:36
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mfort.h"
#include "mdcoord.h"
#include "mdeval.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mdebug.h"

#include "ncl.h"
#include "nccs.h"
#include "nclvx.h"

/*********************************************************************
**    E_FUNCTION     : int ncl_line_to_nclline(e, ncl)
**       Convert a UNICAD line to an NCL line.
**    PARAMETERS   
**       INPUT  : 
**          e							UNICAD line
**       OUTPUT :  
**          ncl						buffer to hold NCL line
**    RETURNS      : 
**			UU_SUCCESS iff no error
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_line_to_nclline(e, ncl)
	struct NCL_nclln_rec *e;
	struct NCLI_line_rec *ncl;

	{
	int status;
	UM_vector vec;

	uu_denter(UU_MTRC,(us,"ncl_line_to_nclline(key=%x, ncl=%x)",
		e->key, ncl));
	status = UU_SUCCESS;
	um_vcmnvc(e->ept, e->spt, vec);
	ncl_uureal_to_real8(3, e->spt, ncl->spt);
	ncl_uureal_to_real8(3, vec, ncl->ijk);
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_uline_to_nclline(e, ncl)
**       Convert a UNICAD line to an NCL line.
**    PARAMETERS   
**       INPUT  : 
**          e							UNICAD line
**       OUTPUT :  
**          ncl						buffer to hold NCL line
**    RETURNS      : 
**			UU_SUCCESS iff no error
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_uline_to_nclline(e, ncl)
	struct UM_line_rec *e;
	struct NCLI_line_rec *ncl;

	{
	int status;
	UM_vector vec;

	uu_denter(UU_MTRC,(us,"ncl_uline_to_nclline(key=%x, ncl=%x)",
		e->key, ncl));
	status = UU_SUCCESS;
	um_vcmnvc(e->ept, e->spt, vec);
	ncl_uureal_to_real8(3, e->spt, ncl->spt);
	ncl_uureal_to_real8(3, vec, ncl->ijk);
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_nclline_to_line(ncl, e)
**			Convert an NCL line to a UNICAD line
**    PARAMETERS   
**       INPUT  : 
**          ncl						buffer holding NCL line
**       OUTPUT :  
**          e							UNICAD line
**    RETURNS      : 
**			UU_SUCCESS iff no error
**			UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_nclline_to_line(ncl, e)
	struct NCLI_line_rec *ncl;
	struct NCL_nclln_rec *e;

	{
	int status;
	UM_vector vec;

	uu_denter(UU_MTRC,(us,"ncl_nclline_to_line(ncl=%x, e=%x)",
		ncl, e));
	status = UU_SUCCESS;
	e->rel_num = NCL_LINE_REL;

	/* If key exists, retrieve entity */
	if (e->key != 0)
		status = ur_retrieve_data(e, sizeof(struct NCL_nclln_rec));

	if (status != UU_SUCCESS)
		goto done;

	ncl_real8_to_uureal(3, ncl->spt, e->spt);
	ncl_real8_to_uureal(3, ncl->ijk, vec);
	um_vcplvc(e->spt, vec, e->ept);
done:;
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : int ncl_nclln_to_uniln(nclln, uniln)
**			Convert an NCL line to a UNICAD line
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
ncl_nclln_to_uniln(nclln, uniln)
	struct NCL_nclln_rec *nclln;
	struct UM_line_rec *uniln;

	{
	int status;
	char label[100];

	uu_denter(UU_MTRC,(us,"ncl_nclln_to_uniln(key=%x)", nclln->key));

	/* added to correct the label on drawing. kathy */
	ncl_get_label_with_key(nclln->key, label);
	strncpy (uniln->label, label, NCL_MAX_LABEL);
/*
.....WIREFRAME ENTITIES NOW HAVE subscr FIELD
*/
	uniln->subscr = nclln->subscr;

	status = UU_SUCCESS;
	uniln->rel_num = UM_LINE_REL;
	um_vctovc(nclln->spt, uniln->spt);
	um_vctovc(nclln->ept, uniln->ept);
	uu_dexit;
	return (status);
	}

/*********************************************************************
**    E_FUNCTION     : ncl_p_nclline(ncl)
**       Print the data defining an NCL line.
**    PARAMETERS   
**       INPUT  : 
**          ncl						buffer holding NCL line
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_p_nclline(ncl)
	struct NCLI_line_rec *ncl;

	{
	uu_denter(UU_MTRC,(us,"ncl_p_nclline(ncl=%x)", ncl));
	sprintf(UM_sbuf,"NCLLINE:");
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf,"  spt=(%g,%g,%g)",ncl->spt[0],ncl->spt[1],ncl->spt[2]);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf,"  ijk=(%g,%g,%g)",ncl->ijk[0],ncl->ijk[1],ncl->ijk[2]);
	um_pscroll(UM_sbuf);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION     : ncl_p89_line(ptr)
**       Print the contents of an NCL line record.
**    PARAMETERS   
**       INPUT  : 
**				ptr			pointer to fixed data of line record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void ncl_p89_line(ptr)
	struct  NCL_nclln_rec  *ptr;

	{

	uu_denter(UU_MTRC,(us,"ncl_p89_line(key=%x)",ptr->key));

	sprintf(UM_sbuf, "LINE %d", ptr->key);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "label %s:6", ptr->label);
	um_pscroll(UM_sbuf);
	sprintf(UM_sbuf, "subscr %d", ptr->subscr);
	um_pscroll(UM_sbuf);
	um_p_ary(UM_PFLOAT, "spt", 3, ptr->spt); 
	um_p_ary(UM_PFLOAT, "ept", 3, ptr->ept); 

	uu_dexit;
	}
/*********************************************************************
**    E_FUNCTION     : int ncl_line_transl(e, offset)
**       Translate an NCL line along a vector.
**    PARAMETERS   
**       INPUT  : 
**          e             NCL line entity
**          offset        vector
**       OUTPUT : 
**          none
**    RETURNS      : 
**       UU_SUCCESS iff no error
**       UU_FAILURE otherwise
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
ncl_line_transl(e, offset)
   struct NCL_nclln_rec *e;
   UM_vector offset;

   {
   int status;

   uu_denter(UU_MTRC,(us,"ncl_line_transl(key=%x)", e->key));

   status = UU_SUCCESS;

   um_vcplvc(e->spt, offset, e->spt);
   um_vcplvc(e->ept, offset, e->ept);
   um_update_geom(e, UM_DEFAULT_TF);

   uu_dexit;
   return (status);
}
/*********************************************************************
**    E_FUNCTION     : int ncl_ev_line(evflag,u,eptr,tfmat,evout)
**       Evaluate an NCL line at a parameter
**      PARAMETERS:
**          INPUT:
**              evflag        UM_POINT= calculate point on line only;
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
ncl_ev_line(evflag,u,eptr,tfmat, evout)
int evflag;
UM_param u;
struct NCL_nclln_rec *eptr;
UM_transf tfmat;
struct UM_evcrvout *evout;
	{
	int status;
	struct UM_line_rec unientity;

	ncl_nclln_to_uniln(eptr, &unientity);

	status = uc_evcrv(evflag, u, &unientity, tfmat, evout);

	return (status);
	}
