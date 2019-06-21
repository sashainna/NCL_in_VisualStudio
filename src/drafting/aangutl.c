/*********************************************************************
**    NAME         :  aangutl.c
**       CONTAINS:
**				ua_angular_arcang
**				ua_angular_swappt
**				ua_angular_swapval
**
**    COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**       aangutl.c , 25.1
**    DATE AND TIME OF LAST MODIFICATION
**       04/29/15 , 15:05:29
*********************************************************************/
#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"
#include "adrfdefs.h"

/*********************************************************************
**    E_FUNCTION :  ua_angular_swappt(pt1,pt2)
**		Swap points 1 and 2.
**    PARAMETERS   
**       INPUT  : 
**				pt1				point 1
**				pt1				point 2
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_angular_swappt(pt1, pt2)
UU_REAL	pt1[3];
UU_REAL	pt2[3];
	{
	UU_REAL	temppt[3];

	uu_denter(UU_STRC,(us,"ua_angular_swappt(pt1=%s, pt2=%s)", "...", "..."));
	um_vctovc(pt1,temppt);
	um_vctovc(pt2,pt1);
	um_vctovc(temppt,pt2);
	uu_dexit;
	}

/*********************************************************************
**    E_FUNCTION :  ua_angular_arcang(cpt,pt,uvc) RETURNS REAL
**		Determine angle of radius line FROM cpt TO pt FROM xaxis.
**    PARAMETERS   
**       INPUT  : 
**				cpt				center point
**				pt					radius point
**       OUTPUT :  
**				uvc				unit vector FROM center TO radius
**    RETURNS      : angle (0- 2PI) FROM xaxis TO radial line.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL	ua_angular_arcang(cpt, pt, uvc)
UU_REAL	cpt[3];
UU_REAL	pt[3];
UU_REAL	uvc[3];
	{
	UU_REAL	ang;

	uu_denter(UU_STRC,(us,
		"ua_angular_arcang(cpt=<%g,%g,%g>,pt=<%g,%g,%g>, uvc=%s)",
		cpt[0],cpt[1],cpt[2], pt[0],pt[1],pt[2], "..."));
		{
		UU_REAL	us_t36[3];
		um_vcmnvc(pt,cpt,us_t36);
		um_unitvc(us_t36,uvc);
		}
		{
		UU_REAL	us_t37[3];
		UU_REAL	us_t38[3];
		us_t37[0] = 1.000000e+000;
		us_t37[1] = 0.000000e+000;
		us_t37[2] = 0.000000e+000;
		us_t38[0] = 0.000000e+000;
		us_t38[1] = 0.000000e+000;
		us_t38[2] = 1.000000e+000;
		ang = um_angle2p(us_t37,uvc,us_t38);
		}
	uu_dexit;
	return(ang);
	}

/*********************************************************************
**    E_FUNCTION :  ua_angular_swapval(val1,val2)
**		Swap REAL values 1 and 2.
**    PARAMETERS   
**       INPUT  : 
**				val1				value 1
**				val1				value 2
**       OUTPUT :  
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
ua_angular_swapval(val1, val2)
UU_REAL	(*val1);
UU_REAL	(*val2);
	{
	UU_REAL	tempval;

	uu_denter(UU_STRC,(us,
		"ua_angular_swapval(val1=%s,val2=%s)","...","..."));
	tempval = (*val1);
	(*val1) = (*val2);
	(*val2) = tempval;
	uu_dexit;
	}
