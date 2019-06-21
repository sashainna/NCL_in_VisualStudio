/*********************************************************************
**    NAME         :  m3ecirc6.c
**       CONTAINS:
**			int um_ev3_circle(evflag, u, eptr, tfmat, evout)
**    COPYRIGHT 2004 (c) NCCS Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       m3ecirc6.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:51
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "go.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mdattr.h"
#include "mattr.h"
#include "mcrv.h"
#include "modef.h"
#include "mdeval.h"
#include "mplot.h"
#include "mdebug.h"

/*******************************************************************************
**  E_FUNCTION:  int um_ev3_circle(evflag,u,eptr,tfmat,evout)
**			Evaluate a circle at a parameter.
**		PARAMETERS:   
**			INPUT:
**				evflag         UM_POINT= calculate point on circle only;
**                         UM_FRSTDERIV= calculate point and 1st 
**                                   derivative;
**                         UM_SECDERIV= calculate point, 1st and 2nd 
**                                   derivative;
**                         UM_CURVATURE= calc point, 1st, 2nd deriv, 
**                                   and curvature;
**				u					the parameter value in range [0,1]
**
**				eptr			   pointer to the entity record
**				tfmat				transformation matrix.
**			OUTPUT:
**				evout		      pointer to a curve evaluator
**                         record containing both the requested
**                         information, and (ultimately) a 
**                         status value indicating the validity
**                         of the requested information.
**		RETURNS : none currently, ultimately will return one of the 
**				following: 
**            UM_VALID: all requested fields are valid;
**            UM_BADFIELDS: at least one requested fields is invalid;
**            UM_BADRECORDS: at least one entire record is invalid;
**            UM_INVALID: all output is suspect.
**		SIDE EFFECTS :  none
**		WARNINGS     :  none
**************************************************************************/
int
um_ev3_circle(evflag,u,eptr,tfmat,evout)
	int  evflag;
	UM_param u;
	struct UM_circle_rec *eptr;
	UM_transf tfmat;
	struct UM_evcrvout *evout;

	{
   UM_length rad;				/* radius of circle */
   UM_angle dang;				/* total delta angle of arc entity */
   UU_REAL *center;			/* center of circle */
   UU_REAL *svec;				/* direction unit vector from center
										to start point on circle */
   UU_REAL *nvec;				/* direction unit vector normal to plane
										containing the circle*/
   UU_REAL *circpt;			/* pointer to output location for new point */   
   UU_REAL *tangent;			/* pointer to output location for tangent vector */
   UU_REAL *accel;  	     /* pointer to output location for second deriv */
   UM_vector crosvec;		/* vector in plane of circle and at rt angle 
										to svec    */
   UM_vector radvec;			/* storage for circpt - center */
   UM_angle ang;				/* angle of rotation from starting pt */ 
   UU_REAL cosine;
   UU_REAL sine;
   int i;

   uu_denter(UU_MTRC,
		(us,"um_ev3_circle(evflag:%d,u:%g,key:%d,tfmat:%x,evout:%x)",
		evflag,u,eptr->key,tfmat,evout));

   rad = eptr->radius;
   dang = eptr->dang;
   center = eptr->center;
   svec = eptr->svec;
   nvec = eptr->nvec;

   circpt = evout->cp; /*circpt now pts to the storage location to put new pt*/

   /*"radvec" will have the correct direction and magnitude*/
	um_vctmsc(svec,rad,radvec);

   /*get angle from starting pt to the pt corresponding to u*/
   ang = dang * u;
   um_cross(nvec, radvec, crosvec);
   cosine = cos(ang);
   sine = sin(ang);
   circpt[0] = sine * crosvec[0] + cosine * radvec[0] + center[0];
   circpt[1] = sine * crosvec[1] + cosine * radvec[1] + center[1];
   circpt[2] = sine * crosvec[2] + cosine * radvec[2] + center[2];

   if (evflag != UM_POINT) /* evout->cp has new pt */ 
   	{
   	for (i=0; i<3; i++)/* get direction vector of circpt - center */
         radvec[i] = circpt[i] - center[i];                           

   	tangent = evout->dcdu;                                     
   	/* now do:  tangent = dang * (nvec X (circpt - center))  */
   	tangent[0] =  dang * (nvec[1] * radvec[2] - nvec[2] * radvec[1]);
   	tangent[1] =  dang * (nvec[2] * radvec[0] - nvec[0] * radvec[2]);
   	tangent[2] =  dang * (nvec[0] * radvec[1] - nvec[1] * radvec[0]);

   	if (evflag != UM_FRSTDERIV) /*evout->dcdu has tan vector*/
   		{
    		/* printf("    radius vector to new pt: %f %f %f ",
     		 * radvec[0],radvec[1],radvec[2]);
     		 */
   		accel = evout->d2cdu2;
   		dang = (-1.0) * dang * dang;
   		for (i=0; i<3; i++)  accel[i] = dang * radvec[i];
   		if (evflag != UM_SECDERIV)  /* evout->d2cdu2 has 2nd deriv */
   			/* (evflag == UM_CURVATURE) */
		 		evout->curv = 1.0 / rad;
			}/* end not UM_FRSTDERIV */
		}/* end not UM_POINT */

	/* position results in evaluator record according to the transform, tfmat */
	um_transform_evcrvout(evflag, eptr, tfmat, evout);
	uu_dexit;
	return (UU_SUCCESS);
	}
