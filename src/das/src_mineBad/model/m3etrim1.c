/*******************************************************************
**    NAME         :  m3etrim1.c
**       CONTAINS:
**         um_splitcurve
**         um_c2_splitline
**         um_c2_pp
**         um_c2_ptvec
**         um_cp2_copyline
**         um_c4_splitconic
**
**    COPYRIGHT 2013 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3etrim1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 . 25.1
*********************************************************************/
#include "zsysdep.h"
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "class.h"
#include "mcrv.h"
#include "mattr.h"
#include "modef.h"
#include "misect.h"
#include "mdeval.h"
#include "mdpick.h"
#include "mdebug.h"
#include "ncl.h"
#include "nclfc.h"
#include "nclvx.h"
#include "nclmodals.h"

/*********************************************************************
**    E_FUNCTION : int um_splitcurve(eptr, u, udel, eptr1, eptr2)
**       Dispatcher to handle the spliting of a curve (EPTR) at
**      a user parameter U (0.0 < U < 1.0) into two curves
**      (EPTR1 and EPTR2).
**    PARAMETERS   
**       INPUT  : 
**          none
**        eptr           pointer to original curve
**        u              parameter value to split at
**        udel           parameter value at pick point
**        eptr1          first half of curve
**        eptr2          second half of curve
**       OUTPUT :  
**          none
**    RETURNS      : 
**      UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_splitcurve(eptr, u, udel, eptr1, eptr2)
  struct UM_crvdatabag *eptr;
  UM_param *u, *udel;
  struct UM_crvdatabag *eptr1;
  struct UM_crvdatabag *eptr2;

  {
  int status;

  uu_denter(UU_MTRC,(us,"um_splitcurve(key=%d, u=%f, eptr1=%x, eptr2=%x)",
    eptr->key, u, eptr1, eptr2));

  switch (eptr->rel_num)
    {
    case UM_LINE_REL:
      status = um_c2_splitline(eptr, u, udel, eptr1, eptr2);
      break;
    case UM_CIRCLE_REL:
      status = um_c3_splitcircle(eptr, u, udel, eptr1, eptr2);
      break;
    case UM_CONIC_REL:
      status = um_c4_splitconic(eptr, u, udel, eptr1, eptr2);
      break;
    case UM_RBSPLCRV_REL:
      status = um_c7_trimrbsplcrv(eptr, u, udel, eptr1, eptr2);
      break;
	case NCL_CURVE_REL:
		status = ncl_c7_trimnclcrv(eptr, u, udel, eptr1, eptr2);
		break;
	case UM_COMPCRV_REL:
		status = um_c5_splitcompcrv(eptr, u, udel, eptr1, eptr2);
		break;
    default:
      status = UU_FAILURE;
      break;
    }
  uu_dexit;
  return (status);
  }
/*********************************************************************
**    E_FUNCTION     : int um_c2_splitline(eptr, u, udel, eptr1, eptr2)
**       Split a line (EPTR) into two curves at a user parameter 
**			U (0.0 < U < 1.0). 
**    PARAMETERS   
**       INPUT  : 
**          eptr						pointer to line
**				u							parameter to split line at
**				eptr1						first half of line
**				eptr2						second half of line
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c2_splitline(eptr, u, udel, eptr1, eptr2)
	struct UM_line_rec *eptr;
	UM_param *u, *udel;
	struct UM_line_rec *eptr1;
	struct UM_line_rec *eptr2;

	{
	int status;
	UM_vector lvec;

	uu_denter(UU_MTRC,(us,"um_c2_splitline(key=%d, u=%f, eptr1=%x, eptr2=%x)",
		eptr->key, u[0], eptr1, eptr2));

	um_vcmnvc(eptr->ept, eptr->spt, lvec);
	um_vctmsc(lvec, u[0], lvec);

	status = um_c2_ptvec(eptr->spt, lvec, eptr1);
	if (status == UU_SUCCESS)
		status = um_c2_pp(eptr1->ept, eptr->ept, eptr2);

	uu_dexitstatus("um_c2_splitline", status);
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_c2_pp(spt, ept, lptr)
**       Create a line between two points.
**    PARAMETERS   
**       INPUT  : 
**          spt					start coordinate of line
**				ept					end coordinate of line
**       OUTPUT :  
**          lptr					line entity
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c2_pp(spt,ept,lptr)
	UM_coord	spt;
	UM_coord	ept;
	struct UM_line_rec *lptr;

	{
	UU_REAL arclen;
	int status;

	uu_denter(UU_MTRC,(us,"um_c2_pp(%x,%x,%x)",spt,ept,lptr));
	status = UU_FAILURE;
	if (um_cceqcc(spt, ept))
		uu_uerror0(/*start and end point are identical*/UM_MODEL,55);
	else
		{
		ur_setup_data(UM_LINE_REL, lptr, sizeof(struct UM_line_rec));
		/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
		strcpy (lptr->label, "");
		lptr->subscr = 0;
		um_vctovc(spt, lptr->spt);
		um_vctovc(ept, lptr->ept);
		status = UU_SUCCESS;
		}
	uu_dexitstatus("um_c2_pp", status);
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_c2_ptvec(spt, vec, lptr)
**			Create a line (LPTR) that starts at a point (SPT) and goes in
**			the direction defined by a vector (VEC).
**    PARAMETERS   
**       INPUT  : 
**          spt					start coordinate of line
**				vec					vector defining length and direction
**       OUTPUT :  
**          lptr					line entity
**    RETURNS      : 
**			UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c2_ptvec(spt,vec,lptr)
	UM_coord	spt;
	UM_vector vec;
	struct UM_line_rec *lptr;

	{
	UU_REAL arclen;
	int status;

	uu_denter(UU_MTRC,(us,"um_c2_ptvec(%x,%x,%x)",spt,vec,lptr));
	status = UU_FAILURE;
	if (um_cceqcc(vec, UM_zerovec))
		uu_uerror0(/*start and end point are identical*/UM_MODEL,55);
	else
		{
		status = UU_SUCCESS;
		ur_setup_data(UM_LINE_REL, lptr, sizeof(struct UM_line_rec));
		/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
		strcpy (lptr->label, "");
		lptr->subscr = 0;
		um_vctovc(spt, lptr->spt);
		um_vcplvc(spt, vec, lptr->ept);
		}
	uu_dexitstatus("um_c2_ptvec", status);
	return (status);
	}
/*********************************************************************
**    E_FUNCTION     : int um_cp2_copyline(e1, e2, bagsize)
**      Copy a line into a new line.   
**    PARAMETERS   
**       INPUT  : 
**				e1			pointer to entity to be copied
**				bagsize  storage size for entity.
**       OUTPUT :  
**				e2       pointer to new entity
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_cp2_copyline(e1, e2, bagsize)
	struct UM_line_rec *e1;        /* The  entity to translate */
	struct UM_line_rec *e2;        /* New  entity */
	int bagsize;

	{
	struct UM_attrdata_rec attrbag;

	uu_denter(UU_MTRC,(us,"um_cp2_copyline(?,?,?)"));
                                            
	ur_setup_data(e1->rel_num, e2, bagsize);
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (e2->label, "");
	e2->subscr = 0;
   um_vctovc(e1->spt, e2->spt);
   um_vctovc(e1->ept, e2->ept);

	um_get_disp_attr(e1->key, &attrbag);
	um_create_geom(e2, UM_DEFAULT_TF, &attrbag);
	uu_dexit;
	return (UU_SUCCESS);
	}
/*********************************************************************
**    E_FUNCTION     : int um_c4_splitconic(eptr, u, udel, eptr1, eptr2)
**       Split a conic (EPTR) into two curves at a user parameter 
**			U (0.0 < U < 1.0). 
**    PARAMETERS   
**       INPUT  : 
**          eptr						pointer to conic
**				u							parameter to split conic at
**				eptr1						first half of conic
**				eptr2						second half of conic
**       OUTPUT :  
**          none
**    RETURNS      : 
**			UU_SUCCESS iff no error
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_c4_splitconic(eptr, u, udel, eptr1, eptr2)
	struct UM_conic_rec *eptr;
	UM_param *u, *udel;
	struct UM_conic_rec *eptr1;
	struct UM_conic_rec *eptr2;

	{
	int status;
	struct UM_evcrvout evcrv;
	UM_coord spt;
	UM_coord ept, dlt;
	UM_param d, s;

	uu_denter(UU_MTRC,(us,"um_c4_splitconic(key=%d, u=%f, eptr1=%x, eptr2=%x)",
		eptr->key, u[0], eptr1, eptr2));

	ur_setup_data(UM_CONIC_REL, eptr1, sizeof(struct UM_conic_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (eptr1->label, "");
	eptr1->subscr = 0;
	ur_setup_data(UM_CONIC_REL, eptr2, sizeof(struct UM_conic_rec));
	/* MILLS-  Initialize the LABEL and SUBSCRIPT fields. */
	strcpy (eptr2->label, "");
	eptr2->subscr = 0;

	uc_init_evcrvout(eptr, &evcrv);
	uc_evcrv(UM_POINT, (UU_REAL) 0.0, eptr, UM_DEFAULT_TF, &evcrv);
	um_vctovc(evcrv.cp, spt);
	uc_evcrv(UM_POINT, (UU_REAL) 1.0, eptr, UM_DEFAULT_TF, &evcrv);
	um_vctovc(evcrv.cp, ept);
	uc_evcrv(UM_POINT, u[0], eptr, UM_DEFAULT_TF, &evcrv);

	zbytecp(*eptr1, *eptr);
	zbytecp(*eptr2, *eptr);
   if (eptr->type == UM_ELLIPSE)
     {
	   um_vcmnvc(spt, ept, dlt);
      if (um_mag(dlt) < UM_FUZZ && u[1] > 0.0)
        {
         um_vctovc (evcrv.cp,dlt);
	      uc_evcrv(UM_POINT, u[1], eptr, UM_DEFAULT_TF, &evcrv);
         s   = .5*(u[0] + u[1]);
         d   = fabs (u[0] - s);
         if (u[0] < u[1])
           {
	         um_cn4_endpoints(eptr2, dlt, evcrv.cp, UM_DEFAULT_TF);
	         um_cn4_endpoints(eptr1, evcrv.cp, dlt, UM_DEFAULT_TF);
           }
         else
           {
	         um_cn4_endpoints(eptr2, evcrv.cp, dlt, UM_DEFAULT_TF);
	         um_cn4_endpoints(eptr1, dlt, evcrv.cp, UM_DEFAULT_TF);
           }
         if (fabs(*udel - s) > d)
            *udel = u[0] - .1;
         else
            *udel = u[0] + .1;
         goto Done;
        }
     }
   um_cn4_endpoints(eptr1, spt, evcrv.cp, UM_DEFAULT_TF);
   um_cn4_endpoints(eptr2, evcrv.cp, ept, UM_DEFAULT_TF);

Done:;
	uu_dexit;
	return (UU_SUCCESS);
	}
