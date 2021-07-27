
/*********************************************************************
**    NAME         :  m7mathev.c
**    CONTAINS: support routines for curve/surface evaluator
**			um_setstatus(entype, fldflag, recordptr, status)
**			um_evstatus(entype, fldflag, recordptr)
**			um_transform_evsrfout(evflag, eptr, tfmat, srfoutptr)
**			um_print_evcrvout(evflag, u, crvoutptr)
**			um_print_evsrfout(evflag, u, v, srfoutptr)
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m7mathev.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:09
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mdcoord.h"
#include "mdclass.h"
#include "mdeval.h"
#include "modef.h"
#include "mdebug.h"

/*********************************************************************
**    E_FUNCTION     : um_setstatus(entype, fldflag, recordptr, status)
**      		Sets status of evaluation records; currently there is only a single
**				status for the entire evaluation record.  Ultimately there is
**				to be a status for each field in an evaluation record.
**    PARAMETERS   
**       INPUT  : 
**				entype      this is either UM_CURVE_CLASS or UM_SURFACE_CLASS; 
**								it is a flag indicating whether the evalution record 
**								pointed to by "recordptr" is for a curve (UM_CURVE_CLASS)
**								 or for a surface (UM_SURFACE_CLASS).
**
**				fldflag       this is a field flag indicating which field in the 
**                        evaluation record should have its status set.
**                        Currently this parameter has no effect.
**
**				recordptr     pointer to the evaluation record to have its status
**                        set; note, this may point to either a curve
**                        evaluator record or a surface evaluator record;
**                        the type is determined by "entype".
**
**				status        status to be given a field of an evaluation record;
**                        this should be either UM_VALID or UM_INVALID.
**                        That is, the status for any field of an evaluator
**                        record will ultimately be one of these values.
**       OUTPUT :  
**          recordptr     pointer to the evaluator record with the requested
**                        status set.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_setstatus(entype, fldflag, recordptr, status)
	int entype;
	int fldflag;
	struct  UM_evcrvout *recordptr;
	int status;
	{
	struct  UM_evsrfout *recptr;

   switch (entype)
		{
		case  UM_CURVE_CLASS:
	     switch (fldflag)
		  	{
			  case  UM_POINT: 
				 recordptr->status = status;
				 break;
           case  UM_FRSTDERIV: 
				 recordptr->status = status;
				 break;
           case  UM_SECDERIV: 
				 recordptr->status = status;
				 break;
           case  UM_CURVATURE: 
				 recordptr->status = status;
				 break;
           case  UM_ALL: 
				 recordptr->status = status;
				 break;
           default: 
				 sprintf(UM_sbuf,"from um_setstatus, illegal field flag of %d",
									 fldflag);
             um_pscroll(UM_sbuf);
				 break;
       }
       break;
     case  UM_SURFACE_CLASS:
		 recptr = (struct UM_evsrfout *) recordptr;
		 switch (fldflag)
		 	{
	       case  UM_POINT: 
				recptr->status = status;
				break;
          case  UM_NORM: 
				recptr->status = status;
				break;
          case  UM_FRSTDERIV: 
				recptr->status = status;
				break;
          case  UM_SECDERIV: 
				recptr->status = status;
				break;
          case  UM_CURVATURE: 
				recptr->status = status;
				break;
          case  UM_ALL: 
				recptr->status = status;
				break;
          default: 
				sprintf(UM_sbuf,"from um_setstatus, illegal field flag of %d",
									 fldflag);
            um_pscroll(UM_sbuf);
				break;
       		}
      	break;
		default: 
			sprintf(UM_sbuf,"from um_setstatus, illegal entity type of %d",entype);
			um_pscroll(UM_sbuf);
			break;
   	}
	}
/*********************************************************************
**    E_FUNCTION     : um_evstatus(entype, fldflag, recordptr)
**      Determines whether the entries in an evaluation record are valid 
**    PARAMETERS   
**       INPUT  : 
**				entype      this is either UM_CURVE_CLASS or UM_SURFACE_CLASS; 
**								it is a flag indicating whether the evalution record 
**								pointed to by "recordptr" is for a curve (UM_CURVE_CLASS)
**								 or for a surface (UM_SURFACE_CLASS).
**
**				fldflag       this is a field flag indicating which field in the 
**                        evaluation record should have its status interrogated.
**                        Currently this parameter has no effect.
**
**				recordptr     pointer to the evaluation record to have its 
**                        interrogated; note, this may point to either a curve
**                        evaluator record or a surface evaluator record;
**                        the type is determined by "entype".
**       OUTPUT :  
**          none.
**    RETURNS      : the status of the requested evalutor field.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_evstatus(entype, fldflag, recordptr)
 int entype;
 int fldflag;
 struct  UM_evcrvout *recordptr;
	{
 int status;
 struct  UM_evsrfout *recptr;

  switch (entype)
	 	{
		case  UM_CURVE_CLASS:
		  switch (fldflag)
				{
           case  UM_POINT:
					 status =  UM_VALID;
					 break;
           case  UM_FRSTDERIV:
					 status =  UM_VALID;
					 break;
           case  UM_SECDERIV:
					 status =  UM_VALID;
					 break;
           case  UM_CURVATURE:
					 status =  UM_VALID;
					 break;
           case  UM_ALL:
					 status =  UM_VALID;
					 break;
           default: 
					 sprintf(UM_sbuf,"from um_evstatus, illegal field flag of %d",fldflag);
					 um_pscroll(UM_sbuf);
					 status =  UM_INVALID;
					 break;
         } /* end  UM_CRV */
        break;
      case  UM_SURFACE_CLASS:
		  recptr = (struct  UM_evsrfout *) recordptr;
		  switch (fldflag)
				{
			  case  UM_POINT:
		          if (recptr->status ==  UM_VALID)	 status =  UM_VALID;
                else status =  UM_INVALID;
              	 break;
           case  UM_NORM:
		          if (recptr->status ==  UM_VALID)	 status =  UM_VALID;
                else status =  UM_INVALID;
                break;
			  case  UM_FRSTDERIV:
		          if (recptr->status ==  UM_VALID)	 status =  UM_VALID;
                else status =  UM_INVALID;
                break;
			  case  UM_SECDERIV:
		          if (recptr->status ==  UM_VALID)	 status =  UM_VALID;
                else status =  UM_INVALID;
                break;
			  case  UM_CURVATURE:
		          if (recptr->status ==  UM_VALID)	 status =  UM_VALID;
                else status =  UM_INVALID;
                break;
           case  UM_ALL:
		          if (recptr->status ==  UM_VALID)	 status =  UM_VALID;
                else status =  UM_INVALID;
                break;
			  default:
					 sprintf(UM_sbuf,"from um_evstatus, illegal field flag of %d",fldflag);
					 um_pscroll(UM_sbuf);
					 status =  UM_INVALID;
					 break;
         } /* end  UM_SRF */
        break;
      default:
			  sprintf(UM_sbuf,"from um_evstatus, illegal  entity type of %d", entype);
			  um_pscroll(UM_sbuf);
			  status =  UM_INVALID;
			  break;
		}	  
	return(status);
	}
/*********************************************************************
**    E_FUNCTION : um_transform_evsrfout(evflag, eptr, tfmat, srfoutptr)
**			Apply the transformation matrix (TFMAT) to all of the defined
**			fields in the curve evaluator record (EVOUTPTR).
**    PARAMETERS   
**       INPUT  : 
**          evflag		UM_POINT=>		point
**								UM_FRSTFERIV=> point, 1st deriv
**								UM_SECDETIV=>	point, 1st deriv, 2nd deriv
**								UM_CURVATURE=>	point, 1st deriv, 2nd deriv,
**													curvature
**				u				parameter value to evaluate curve function
**				crvptr		pointer to curve entity
**				tfmat			transformation matrix
**       OUTPUT :  
**          srfoutptr	curve evaluator record to put results
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_transform_evsrfout(evflag, srfptr, tfmat, srfoutptr)
	int evflag;
	struct UM_entitydatabag *srfptr;
	UM_transf tfmat;
	struct UM_evsrfout *srfoutptr;

	{
	UM_transf temptf;
	UU_REAL numerator, denom;
	UU_LOGICAL um_is_idmat();

	uu_denter(UU_MTRC,
		(us,"um_transform_evsrfout(evflag:%d,srfptr->key:%d,tfmat:%x,srfoutptr:%x)",
		evflag, srfptr->key, tfmat, srfoutptr));
		
	if (!um_is_idmat(tfmat))
		{
		um_vctmtf(srfoutptr->sp, tfmat, srfoutptr->sp);
		if (evflag >= UM_NORM)
			{
			um_nodisptf(tfmat, temptf);
			um_vctmtf(srfoutptr->snorm, temptf, srfoutptr->snorm);
			if (evflag >= UM_FRSTDERIV)
				{
				um_vctmtf(srfoutptr->dsdu, temptf, srfoutptr->dsdu);
				um_vctmtf(srfoutptr->dsdv, temptf, srfoutptr->dsdv);
				if (evflag >= UM_SECDERIV)
					{
					um_vctmtf(srfoutptr->d2sdu2, temptf, srfoutptr->d2sdu2);
					um_vctmtf(srfoutptr->d2sdv2, temptf, srfoutptr->d2sdv2);
					if (evflag >= UM_CURVATURE)
					/* we calculate curvature directly here in terms of the 
					 * transformed components of the first and second derivative; 
					 * namely, by using the formula: 
				 	 *               |FRSTDERIV X SECONDERIV|
				 	 * curvature =  ------------------------
				 	 *  (sum of the squares of the components of FRSTDERIV)**(3/2)
				 	 */
						{
						/* u curvature */
						um_cross(srfoutptr->dsdu, srfoutptr->d2sdu2, numerator);
						denom = srfoutptr->dsdu[0]*srfoutptr->dsdu[0] +
								  srfoutptr->dsdu[1]*srfoutptr->dsdu[1] +
								  srfoutptr->dsdu[2]*srfoutptr->dsdu[2];
						denom = sqrt(denom) * denom;
						srfoutptr->ucurv = numerator / denom;
						/* v curvature */
						um_cross(srfoutptr->dsdv, srfoutptr->d2sdv2, numerator);
						denom = srfoutptr->dsdv[0]*srfoutptr->dsdv[0] +
								  srfoutptr->dsdv[1]*srfoutptr->dsdv[1] +
								  srfoutptr->dsdv[2]*srfoutptr->dsdv[2];
						denom = sqrt(denom) * denom;
						srfoutptr->vcurv = numerator / denom;
						}
					}
				}
			}
		}
	uu_dexit;
	}

/*********************************************************************
**    I_FUNCTION     : um_print_evcrvout(evflag, u, crvoutptr)
**       Print a curve evaluation record.
**    PARAMETERS   
**       INPUT  : 
**        	evflag				tells which fields of record to print
**				u						parameter value on curve corresponding
**										to evaluator record
**				crvoutptr				pointer to curve evaluator record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_print_evcrvout(evflag, u, crvoutptr)
	int evflag;
	UU_REAL u;
	struct  UM_evcrvout *crvoutptr;

	{
	sprintf(UM_sbuf,"CURVE EVALUATION AT u= %g", u);
	um_pscroll(UM_sbuf);
	switch (evflag)
		{
		case UM_CURVATURE:
			um_p_ary(UM_PFLOAT, "  curvature:",1, &(crvoutptr->curv));
		case UM_SECDERIV:
			um_p_ary(UM_PFLOAT, "  2nd deriv  :",3, crvoutptr->d2cdu2);
		case UM_FRSTDERIV:
			um_p_ary(UM_PFLOAT, "  1st deriv  :",3, crvoutptr->dcdu);
		case UM_POINT:
			um_p_ary(UM_PFLOAT, "  curve point:",3, crvoutptr->cp);
			break;
		default:
			um_p_ary(UM_PINT, "  illegal flag", 1, &evflag);
			break;
		}
	}

/*********************************************************************
**    I_FUNCTION     : um_print_evsrfout(evflag, u, v, srfoutptr)
**       Print a surface evaluation record.
**    PARAMETERS   
**       INPUT  : 
**        	evflag				tells which fields of record to print
**				u, v					parameter value on surface corresponding
**										to evaluator record
**				srfoutptr			pointer to surface evaluator record
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_print_evsrfout(evflag, u, v, srfoutptr)
	int evflag;
	UU_REAL u,v;
	struct UM_evsrfout *srfoutptr;

	{
	sprintf(UM_sbuf,"SURFACE EVALUATION AT u= %g v= %g",u, v);
	um_pscroll(UM_sbuf);
	switch (evflag)
		{
		case UM_CURVATURE:
			um_p_ary(UM_PFLOAT, " u-curv v-curv:",2, &(srfoutptr->ucurv));
		case UM_SECDERIV:
			um_p_ary(UM_PFLOAT, " u-2nd deriv:",3, srfoutptr->d2sdu2);
			um_p_ary(UM_PFLOAT, " v-2nd deriv:",3, srfoutptr->d2sdv2);
		case UM_FRSTDERIV:
			um_p_ary(UM_PFLOAT, " u-1st deriv:",3, srfoutptr->dsdu);
			um_p_ary(UM_PFLOAT, " v-1st deriv:",3, srfoutptr->dsdv);
		case UM_NORM:
			um_p_ary(UM_PFLOAT, " surface norm:",3, srfoutptr->snorm);
		case UM_POINT:
			um_p_ary(UM_PFLOAT, " surface pt:",3, srfoutptr->sp);
			break;
		default:
			um_p_ary(UM_PINT, "  illegal flag", 1, &evflag);
			break;
		}
	}
