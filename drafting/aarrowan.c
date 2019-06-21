/*********************************************************************
**    NAME         : aarrowan.c
**       CONTAINS:
**					ua_arrowang
**					
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       aarrowan.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:30
*********************************************************************/
#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) aarrowan.c 3.2 3/23/88 16:52:51 single"};
#else
static char uu_sccsident[]={"@(#) aarrowan.c 3.2 3/23/88 16:52:51 double"};
#endif


#include "ustrings.h"
#include "usysdef.h"
#include "umath.h"
#include "ulist.h"
#include "udebug.h"
#include "umoveb.h"
#include "adraft.h"
#include "adrfcom.h"
#include "adrfdefs.h"
#include "mdcoord.h"

/*********************************************************************
**    E_FUNCTION     : UU_REAL	ua_arrowang(uvc, radius, arrowlen, rotpath)
**	Rotate uvc either HALFPI or -HALFPI. Return the angle with positive
**	x axis- this will be the arrow head angle.  Adjust angle lower to
**	account for the curvature of the dimension arc.
**	NOTE: normal vector to uvc assumes to be <0,0,1> (constr plane).
**
**    PARAMETERS   
**       INPUT  : 
**			uvc:						unit vectore along radius at point for
**											arrowhead.
**			radius:					radius of dimension arc.
**			arrowlen:				arrow head length.
**			rotpath:					1 = counter clockwise rotate = HALFPI
**										0 = clockwise rotate = -HALFPI
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
UU_REAL	ua_arrowang(uvc, radius, arrowlen, rotpath)
UM_coord	uvc;
UU_REAL	radius;
UU_REAL	arrowlen;
int		rotpath;
	{
	UM_coord	uvcx, v1, v2;
	UU_REAL	uvcang;
	UU_REAL	rotang;
	UU_REAL	deltaang;

	uu_denter(UU_STRC,(us,
		"ua_arrowang(uvc=<%g,%g,%g>, radius=%g, arrowlen=%g, rotpath=%d)",
		uvc[0],uvc[1],uvc[2], radius, arrowlen, rotpath));
	deltaang = asin(( ( arrowlen/2.0 )/radius ));
	switch( rotpath )
		{
		case 0:
			rotang = ( -1.570796 + deltaang );
			break;
		case 1:
			rotang = ( 1.570796 - deltaang );
			break;
		}
	um_vctovc(uvc,uvcx);
	v1[0] = 0.0;
	v1[1] = 0.0;
	v1[2] = 1.0;
	v2[0] = 0.0;
	v2[1] = 0.0;
	v2[2] = 0.0;
	ua_rotatept(uvcx,v1,v2,rotang);
	v1[0] = 1.0;
	v1[1] = 0.0;
	v1[2] = 0.0;
	v2[0] = 0.0;
	v2[1] = 0.0;
	v2[2] = 1.0;
	uvcang = um_angle2p(v1,uvcx,v2);
	uu_dexit;
	return(uvcang);
	}
