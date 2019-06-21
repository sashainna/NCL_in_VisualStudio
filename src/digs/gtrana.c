/*********************************************************************
**    NAME         :  gtrana.c
**       CONTAINS:
**		Gerror gsvref3(xform,refpt) -- Set view reference point.
**		Gerror gsvpn3(xform,vpn) -- Set view point normal.
**		Gerror gsvup3(xform,vup) -- Set view up vector 3D.
**		Gerror gsvup2(xform,vup) -- Set 2D view up vector.
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       gtrana.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:26
*********************************************************************/
#include "zsysdep.h"
#include <stdio.h>
#include <math.h>
#include "g.h"
#include "gerror.h"
#include "gdidd.h"
#include "gviw.h"
#include "gmat4.h"
#include "udebug.h"
#include "gsegop.h"
#include "gsegac.h"

#ifdef UU_SINGLE
static char uu_sccsident[]={"@(#) gtrana.c 2.1 5/29/86 23:00:27 single"};
#else
static char uu_sccsident[]={"@(#) gtrana.c 2.1 5/29/86 23:00:27 double"};
#endif

/********************************************************************* 
**  E_FUNCTION:  Gerror gsvref3(xform,refpt) -- Set view reference point.
**  PARAMETERS   
**      INPUT:  Gint xform -- transformation number.
**					 Gwpoint3 *refpt -- new view reference point.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsvref3(xform,refpt)		/* set view reference point */
/*$ INPUT */
Gwpoint3 *refpt;				/* ref point, 3D world coordinate point */
Gint xform;
{
	int prmsiz;
	struct { Gint op; Gws id; Gint xf; Gwpoint3 pt; } prms;

	 uu_denter(UU_GTRC,(us,"gsvref(%d,%3g,%3g,%3g)",xform,(*refpt).x,(*refpt).y,(*refpt).z));

#ifdef trace
	if ((xform<1)||(xform>=UG_MAXNTRAN)) {
		ug_errorhand(EBADXFRM,"gsvref3",&xform);
	}
	else 
#endif
	{
		ug_gksstli.vtran[xform].vrefpt.x=(*refpt).x; 
		ug_gksstli.vtran[xform].vrefpt.y=(*refpt).y; 
		ug_gksstli.vtran[xform].vrefpt.z=(*refpt).z;
		ug_winviw(xform);
		prms.op=UG_DVREF3;
		prms.xf=xform;
		zbytecp ( prms.pt, *refpt);
		ug_wkout ( &prms, prmsiz=sizeof(prms)/sizeof(int));
		ug_ndcntranboxdel(xform);	/* delete segment ndc bounding boxes for 
												those segs using this normtran */
	}
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsvpn3(xform,vpn) -- Set view point normal.
**  PARAMETERS   
**      INPUT:  Gint xform -- transformation number
**					 Gwpoint3 *vpn -- view point normal.
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsvpn3(xform,vpn)					/* set view plane normal */ 
/*$ INPUT */
Gint xform;
Gwpoint3 *vpn;						/* view plane normal */
{
	int prmsiz;
	struct { Gint op; Gws id; Gint xf; Gwpoint3 pl; } prms;

	 uu_denter(UU_GTRC,(us,"gsvpn3(%d,%3g,%3g,%3g)",xform,(*vpn).x,(*vpn).y,(*vpn).z));

	ug_gksstli.vtran[xform].vpnorm.x=(*vpn).x; 
	ug_gksstli.vtran[xform].vpnorm.y=(*vpn).y; 
	ug_gksstli.vtran[xform].vpnorm.z=(*vpn).z;

	ug_winviw(xform);

	prms.op=UG_DVPN3;
	prms.xf=xform;
	zbytecp ( prms.pl, *vpn );
	ug_wkout ( &prms, prmsiz=sizeof(prms)/sizeof(int));

	/* delete segment ndc bounding boxes for those segs using this normtran */
	ug_ndcntranboxdel(xform);	
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsvup3(xform, vup) -- Set view up vector 3D.
**
**  PARAMETERS   
**      INPUT: Gint xform		: tranformation to modify
**					Gwpoint3 *vup	: view up vector
**
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsvup3(xform, vup)
/*$ INPUT */
Gint xform;								/* tranformation to modify */
Gwpoint3 *vup;							/* view up vector 			*/
{
	int prmsiz;
	struct { Gint op; Gws id; Gfloat dxup,dyup,dzup; int xf;} prms;

	uu_denter(UU_GTRC,(us,"gsvup3(%d %3g %3g %3g)",xform, (*vup).x, (*vup).y,
								(*vup).z));

	ug_gksstli.vtran[xform].vup.x=(*vup).x; 
	ug_gksstli.vtran[xform].vup.y=(*vup).y; 
	ug_gksstli.vtran[xform].vup.z=(*vup).z;

	prms.op=UG_DVUP3;
	prms.dxup=(*vup).x;
	prms.dyup=(*vup).y;
	prms.dzup=(*vup).z;
	prms.xf=xform;
	ug_wkout(&prms,prmsiz=sizeof(prms)/sizeof(int));

	ug_winviw(xform);

	/* delete segment ndc bounding boxes for those segs using this normtran */
	ug_ndcntranboxdel(xform);	
	
	uu_dexit;
	return(NCL_NO_ERROR);
}

/********************************************************************* 
**  E_FUNCTION:  Gerror gsvup2(xform, vup) -- Set 2D view up vector.
**  PARAMETERS   
**      INPUT: Gint xform		: tranformation to modify
**					Gwpoint *vup	: view up vector
**  RETURNS      :  NCL_NO_ERROR if all went OK.
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
*********************************************************************/
Gerror gsvup2(xform, vup)
/*$ INPUT */
Gint xform;								/* tranformation to modify */
Gwpoint *vup;							/* view up vector 			*/
{
	Gwpoint3 temp;

	uu_denter(UU_GTRC,(us,"gsvup2(%d %3g %3g)",xform, (*vup).x, (*vup).y));

	temp.x = (*vup).x;
	temp.y = (*vup).y;
	temp.z = 0.0;

	gsvup3(xform, temp);
	uu_dexit;
}

