/*********************************************************************
**    NAME         :  m4ioslo1.c
**       CONTAINS:
**       um_get_curvature
**    COPYRIGHT 1997 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       m4ioslo1.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:08:04
*********************************************************************/

#include "mdeval.h"
#include "uminmax.h"
#include "nccs.h"
#include "nclx.h"
#include "ycom.h"

/*********************************************************************
**    E_FUNCTION: um_get_curvature (drv1, drv2, curv)
**       Evaluate a rational bspline curve at a specified parameter.
**    PARAMETERS   
**       INPUT  : 
**          drv1     First derivative vector at point of CV.
**          drv2     Second derivative vector at point of CV.
**       OUTPUT :  
**          curv     Curvature at the point
**    RETURNS      : 
**       UU_SUCCESS iff no error; else UU_FAILURE
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int
um_get_curvature (drv1, drv2, curv)
UM_vector drv1, drv2;
UU_REAL *curv;
{
	UU_REAL c1, d2, s2;
	int status;

	d2 = um_dot (drv1,drv1);
	if (d2 > 1.e-20)
	{
		status = UU_SUCCESS;
		s2 = um_dot (drv2,drv2);
		c1 = um_dot (drv1,drv2);
		c1 = d2*s2 - c1*c1;
		c1 = (c1 > 0.0)? c1: 0.0;
/*
...vp 4/20/98 fix zero in denominator on VMS machines
...where 1e-38 is next to zero.
*/
		c1 = (c1/d2)/d2;
		*curv = sqrt(c1/d2);
	}
	else
	{
		status = UU_FAILURE;
		*curv = 0;
	}

	return(status);
}
