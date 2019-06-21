/*********************************************************************
**    NAME         :  m2vxcntl
**       CONTAINS:
**			um_repaint
**			um_reset
**    COPYRIGHT 1984 (c) UNICAD Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL 
**       m2vxcntl.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:07:50
*********************************************************************/
#include "umath.h"
#include "usysg.h"
#include "udebug.h"
#include "mromcom.h"
#include "mattr.h"
#include "view.h"
#include "unserve.h"
#include "gtblvar6.h"

/*********************************************************************
**    I_FUNCTION     : um_repaint()
**      Repaint the display.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_repaint()

{
	int i;
	UV_vport vport;

	uu_denter( UU_MTRC,(us,"um_repaint()"));
	if ( ur_get_dispattr_hidn_lines() == UU_TRUE)
	{
		for (i = 0; i < UV_act_screen[0].nvports; i++)
		{
			uv_getvpid(UV_act_screen[0].vports[i], &vport);
			/*	unblank all the bodies in this view port */
			uv_blanksolinvp(&vport, UG_VISIBLE);
		}
		ur_put_dispattr_hidn_lines(UU_FALSE);
	}
	gredrawsegs();

 /* refresh status information - on GPX, the top line is shifted up
    by the gredrawsegs call */
 uz_status();

	/* added to fix the visible select buffer. kathy */
	ug_vislistok=0;

	uu_dexit;
}
/*********************************************************************
**    E_FUNCTION     : um_reset
**       Reset viewing I guess.
**    PARAMETERS   
**       INPUT  : 
**          none
**       OUTPUT :  
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
um_reset()
	{
	um_pscroll("um_reset stub");
	}

