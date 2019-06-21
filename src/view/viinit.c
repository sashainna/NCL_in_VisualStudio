/*********************************************************************
**    NAME         :  viinit.c
**       CONTAINS: Initialization routines for viewing subsystem
**			uv_init()
**  COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**   MODULE NAME AND RELEASE LEVEL 
**       viinit.c , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:59
**************************************************************************/
#include "usysdef.h"
#include "udebug.h"
#define VPGM
#include "view.h"
#include "view0.h"
#include "view1.h"
#undef VPGM
#include "unserve.h"
#include "mromcom.h"

/**************************************************************************
**  E_FUNCTION:  uv_init()
**      Initialize the viewing sub system
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
void uv_init()
	{
	int	i;

	uu_denter(UU_MTRC,(us,"uv_init()"));

	/* initialize the number server */
	uu_nserv_init(0, 14, UU_XFORM_NM);
	uu_nserv_resv(UU_XFORM_NM, 0);

	/* clear the view to view port array */
	for (i = 0; i < UV_NVPORTS; i++)
		{
		UV_view_to_vport.vport_key[i] = 0;
		UV_view_to_vport.view_key [i] = 0;
		}

	/* initialize the screens, views, and view ports */
	uv_dd1init();
/*
.....Initialize background
*/
	UV_background.shader = 0;
	UV_background.bgcolor = 1;
	UV_background.bgrgb[0] = UV_background.bgrgb[1] =
		UV_background.bgrgb[2] = 0.;
	UV_background.grad[0] = 2;
	UV_background.grad[1] = 1;
	UV_background.grgb[0][0] = UV_background.grgb[0][1] =
		UV_background.grgb[0][2] = 1.;
	UV_background.grgb[1][0] = UV_background.grgb[1][1] =
		UV_background.grgb[1][2] = 0.;
	for (i=0;i<4;i++)
	{
		UV_background.fcolor[i] = 1;
		UV_background.frgb[i][0] = UV_background.frgb[i][1] =
			UV_background.frgb[i][2] = 0.;
	}
	UV_background.bgfile[0] = '\0';
	UV_background.rotate = 0;
	UV_background.stretch = UU_TRUE;
	uv_set_background();

	uu_dexit;
	}
