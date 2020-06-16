
/*********************************************************************
**    NAME         :  mecirc3
**       CONTAINS:
**       um_transform_evaluator(evflag, eptr, tfmat, evoutptr)
**       um_class(rel_num)
**			um_ev3_circle  -  removed vp 12.8.95, use model/um_ev3_circle
**                         evaluator (identical).
**
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       tioeval.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:52
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
/* #include "go.h"	*/
#include "mdcoord.h"
#include "mdrel.h"
#include "mattr.h"
#include "mcrv.h"
#include "modef.h"
#include "mdeval.h"
#include "mdebug.h"
#include	"mdclass.h"

/*********************************************************************
**    E_FUNCTION : um_transform_evaluator(evflag, eptr, tfmat, evoutptr)
**       Transform an evaluator record for a curve or a surface using the
**			transformation, "tfmat".
**    PARAMETERS   
**       INPUT  : 
**     			evflag            UM_POINT=calculate point on the curve/surface;
**                               UM_NORM=calculate normal to surface,
**                               	plus, the point; 
**                               UM_FRSTDERIV=calc 1st derivative of 
**												curve/surface, plus, normal and point; 
**                               UM_SECDERIV=calc 2nd derivative of curve/
**												surface, plus, 1st deriv, normal, and point;
**                               UM_CURVATURE=calc curvature of curve/surface, 
**												plus, 1st and 2nd deriv, and normal and pt;
**
**					eptr					pointer to the entity being evaluated.
**
**					tfmat					transformation matrix associated with entity.
**		
**					evoutptr				pointer to the evaluator structure to be 
**											transformed.
**
**       OUTPUT :  
**          evoutptr					pointer to the transformed evaluator record.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_transform_evaluator(evflag, eptr, tfmat, evoutptr)
	int evflag;
	struct UM_entitydatabag *eptr;
	UM_transf tfmat;
	struct UM_evcrvout *evoutptr;
	{
	struct UM_evsrfout *srfoutptr;
	UM_transf temptf;
	UU_REAL numerator, denom;
	UM_vector vt;

	uu_denter(UU_MTRC,
		(us,"um_transform_evaluator(evflag:%d,eptr->key:%d,tfmat:%x,evoutptr:%x)",
		evflag, eptr->key, tfmat, evoutptr));
		
	srfoutptr = (struct UM_evsrfout *) evoutptr;
	switch(um_class(eptr->rel_num))
		{
		case UM_POINT_CLASS:
			um_cctmtf(evoutptr->cp, tfmat, evoutptr->cp);
			break;
		case UM_CURVE_CLASS: 
			um_cctmtf(evoutptr->cp, tfmat, evoutptr->cp); 
			if (evflag >= UM_FRSTDERIV)
				{
				/* um_nodisptf(tfmat, temptf); */
				um_vctmtf(evoutptr->dcdu, tfmat, evoutptr->dcdu);
				if (evflag >= UM_SECDERIV)
					{
					um_vctmtf(evoutptr->d2cdu2, temptf, evoutptr->d2cdu2);
					if (evflag >= UM_CURVATURE)
					/* we calculate curvature directly here in terms of the 
					 * transformed components of the first and second derivative; 
					 * namely, by using the formula: 
				 	 *               |FRSTDERIV X SECONDERIV|
				 	 *  curvature =  ------------------------
				 	 *     (sum of the squares of the components of FRSTDERIV)**(3/2)
				 	 */
						{
						um_cross(evoutptr->dcdu, evoutptr->d2cdu2, vt);
						numerator = um_mag(vt);
						denom = evoutptr->dcdu[0]*evoutptr->dcdu[0] +
								  evoutptr->dcdu[1]*evoutptr->dcdu[1] +
								  evoutptr->dcdu[2]*evoutptr->dcdu[2];
						denom = sqrt(denom) * denom;
						evoutptr->curv = numerator / denom;
						}
					}
				}
			break;
#ifdef UM_MPE
		case UM_SURFACE_CLASS:
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
			break;
#endif
		default: 
			uu_uerror2(UM_MODEL,164, um_class(eptr->rel_num), eptr->key);
			/* message is: um_transform_evaluator: undefined class of %d 
			 * for entity with key = %d
			 */
			break;
		}
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION: um_class(rel_num)
**
**			DESCRIPTION: Given the relation number of an entity, this
**				function returns the appropriate class of the entity.
**       
**			PARAMETERS   
**				INPUT: 
**          		rel_num         relation number of the entity whose
**										 class is to be returned.
**				OUTPUT :  none.
**          
**			RETURNS      : either UM_POINT_CLASS, UM_CURVE_CLASS, or 
**					UM_SURFACE_CLASS, UM_SOLID_CLASS, or UM_UNKNOWN_CLASS depending
**					whether the entity is a point, curve, surface, a solid, or
**					unknown.
**
**			SIDE EFFECTS : none
**
**			WARNINGS     : none
*********************************************************************/
int 
um_class(rel_num)
	int rel_num;
		{
	int class;

	uu_denter(UU_MTRC,(us,"um_class(%d)",rel_num));
	switch (rel_num)
			{
		case UM_POINT_REL:
			class = UM_POINT_CLASS;
			break;
		case UM_LINE_REL:
		case NCL_PLN_REL:    /*vp717*/
		case UM_CIRCLE_REL:
		case UM_CONIC_REL:
		case UM_COMPCRV_REL:
		case UM_RBSPLCRV_REL:
		case UM_AGCRV_REL:
 		case UM_POLYLINE_REL:
 		case UM_UVCVONSF_REL:
			class = UM_CURVE_CLASS;
			break;
		case NCL_MESHSURF_REL:
		case NCL_SURF_REL:
		case NCL_REVSURF_REL:
		case UM_RBSPLSRF_REL:
			class = UM_SURFACE_CLASS;
			break;
		case UA_TEXT_REL:
			class = UM_TEXT_CLASS;
			break;
#ifdef UM_MPE
		case UM_BODY_REL:
			class = UM_SOLID_CLASS;
			break;
#endif
		default:
			class = UM_UNKNOWN_CLASS;
			break;
			}
	uu_dexit;
	return (class);
		}


