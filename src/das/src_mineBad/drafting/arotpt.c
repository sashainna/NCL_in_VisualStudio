/*********************************************************************
**    NAME         :  arotpt.c
**       CONTAINS:
**			ua_rotatept
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       arotpt.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:38
*********************************************************************/
#include "usysdef.h"
#include "umath.h"
#include "udebug.h"
#include "mdeval.h"
#include "mdcoord.h"
#include "modef.h"
/*********************************************************************
**    I_FUNCTION :  UU_REAL ua_rotatept(pt,rotvec,axispt,angle)
**			Interface to um_rotatept.
**    PARAMETERS   
**       INPUT  : 
**				pt				point to be rotated
**				rotvec		axis to rotate point about
**				axispt		rotvec axis origin
**				angle			angle to rotate point
**       OUTPUT :  
**				pt				returned rotated point
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_rotatept(pt,rotvec,axispt,angle)
    UU_REAL    *pt;            /* point entity */
    UU_REAL    rotvec[];       /* vector about which to rotate */
    UU_REAL    axispt[];       /* point on the axis of rotation*/
    UU_REAL    angle;          /* angle through which to rotate*/
{
	struct UM_rotmatrix		m;		/* for rotate point routine */
	/*----------- begin function code ----------------------------------*/
	uu_denter(UU_STRC,
		(us,"ua_rotatept(pt=<%g,%g,%g>,angle=%g,)",
		pt[0],pt[1],pt[2],angle));
	uu_denter2(UU_STRC,
		(us,"ua_rotatept(rotvec=<%g,%g,%g>,axispt=<%g,%g,%g>)",
		rotvec[0], rotvec[1], rotvec[2],
		axispt[0], axispt[1], axispt[2]));
	uu_dexit;
	um_rotatept( pt,rotvec,axispt,angle,UU_TRUE,&m);
	uu_dexit;
}
