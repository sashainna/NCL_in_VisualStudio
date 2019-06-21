
/*********************************************************************
**    NAME         :  m3icirc1.c
**       CONTAINS:
**			um_set3_circle
**			um_circle_nsides
**			um_cir_svec
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m3icirc1.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:07:55
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "uhep.h"
#include "mdcoord.h"
#include "mdrel.h"
#include "mcrv.h"
#include "mattr.h"
#include "mplot.h"
#include "mfort.h"
#include "nclfc.h"
#include "mdraw.h"

void um_circle_nsides();

/*********************************************************************
**    I_FUNCTION     : um_set3_circle(ptr,nside,ang,deltang)
**      Calculate number of line segments for circle display.
**    PARAMETERS   
**       INPUT  : 
**				ptr    			pointer to circle record
**       OUTPUT :  
**				nside     		number of line segments.
**          ang       		start angle.
**          deltang   		angle increment.
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_set3_circle(ptr,nside,ang,deltang)
struct  UM_circle_rec  *ptr;
int   *nside;					/* the number of sides of the approximating
										polygon */
UM_angle *ang;					/* angle */
UM_angle *deltang;			/* delta angle */
{
	int minsid;
	UM_int2 ifl;
	UU_REAL prec,cosa,rad,dang,rminsid;
/*
.....Calculate the display tolerance
*/
	if (!UM_plotting)
	{
      ifl  = 175;
      getsc (&ifl,&prec);
	}
	else
	{
		prec = UM_plotprec;
	}
/*
.....Get the circle parameters
*/
	rad = fabs(ptr->radius);
	dang = fabs(ptr->dang);
/*
.....Get then number of sides to display
*/
	um_circle_nsides(rad,dang,prec,nside);
/*
.....Make sure minimum number of sides are displayed
*/
	if (UM_plotting)
	{
		minsid = 4;
	}
	else
	{
		rminsid = 20. * (dang/6.3);
		minsid = rminsid;
		if (minsid < 8) minsid = 8;
	}
	if (*nside < minsid) *nside = minsid;
	if (*nside > 5000) *nside = 5000;
/*
.....Calculate starting and delta angles
*/
	*deltang = ptr->dang / (*nside);
	*ang = *deltang;
}

/*********************************************************************
**    I_FUNCTION     : um_circle_nsides(rad,dang,told,nside)
**      Calculate number of line segments for circle display.
**    PARAMETERS   
**       INPUT  : 
**				rad    			Radius of circle.
**				dang    			Delta angle of circle.
**				told    			Tolerance to use for calculations.
**       OUTPUT :  
**				nside     		number of line segments.
**    RETURNS      : none
**    SIDE EFFECTS :
**          The minimum number of sides returned is 4 and the maximum
**          is 5000. If the calling routine has other limits, they
**          will have to be checked by the calling routine.
**    WARNINGS     : none
*********************************************************************/
void um_circle_nsides(rad,dang,told,nside)
UU_REAL rad;
UM_angle dang;
UU_REAL told;
int *nside;
{
	UU_REAL deltang;
	UU_REAL cosa;

/*
.....Radius is too small
*/
	if (rad <= told)
	{
		*nside = 0;
	}
/*
.....Calculate number of sides within tolerance
*/
	else
	{
		cosa = (rad-told) / rad;
		if (cosa > 1.) cosa = 1.;
		deltang = fabs(acos(cosa));
		if (deltang <= 0.) *nside = 0;
		else *nside = dang / deltang;
	}
/*
.....Make sure minimum number of sides are displayed
*/
	if (*nside < 4) *nside = 4;
	if (*nside > 5000) *nside = 5000;
	return;
}

/*********************************************************************
**    I_FUNCTION     : um_cir_svec
**       Calculate the starting vector for a circle.
**    PARAMETERS   
**       INPUT  : 
**          eptr					entity pointer to circle 
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void um_cir_svec(eptr)
	struct UM_circle_rec *eptr;

	{
	uu_denter( UU_MTRC,(us,"um_cir_svec(?)"));
	if (um_vcparall(eptr->nvec, UM_xaxis))
		{
		eptr->svec[0] = UM_zaxis[0];
		eptr->svec[1] = UM_zaxis[1];
		eptr->svec[2] = UM_zaxis[2];
		}
	else
		{
		um_cross(eptr->nvec, UM_xaxis, eptr->svec);
		um_cross(eptr->svec, eptr->nvec, eptr->svec);
		um_unitvc(eptr->svec, eptr->svec);
		}
	uu_dexit;
	}
