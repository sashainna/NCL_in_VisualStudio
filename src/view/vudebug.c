
/*********************************************************************
**    NAME         :  vprint.c
**       CONTAINS: User interface routines to print viewing infomation
**				uvu_dumpvp()
**				uvu_dumpv()
**  COPYRIGHT 1985 (c) UNICAD Inc.  All Rights Reserved.
**   MODULE NAME AND RELEASE LEVEL 
**       vudebug.c , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:11:59
**************************************************************************/
#include "usysdef.h"
#include "udebug.h"
#include "uhep.h"
#include "dasnog.h"
#include "view.h"

/**************************************************************************
**  E_FUNCTION:  uvu_dumpvp()
**			Prompt the user to pick a viewport and print the data
**			defining the picked viewport.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/
uvu_dumpvp()
	{
	UV_vport	vport;				/* view port structure */

	uu_denter(UU_MTRC,(us, "uvu_dumpvp()"));

	if (uvu_pickvp(&vport) == UU_SUCCESS)
		uv_print_viewport(&vport);

	uu_dexit;
	}

/**************************************************************************
**  E_FUNCTION:  uvu_dumpv()
**			User interface to debug system to dump view data to psfile.
**  PARAMETERS   
**      INPUT  :  none
**      OUTPUT :  none
**  RETURNS      :  none
**  SIDE EFFECTS :  none
**  WARNINGS     :  none
**************************************************************************/

uvu_dumpv()
	{
	UV_view	view;					/* view */
	char		viewname[15];
	int		numint, status;

	uu_denter(UU_MTRC,(us, "uvu_dumpv()"));

	ud_ldas(UD_DASSTRING,UM_MODEL,246,viewname,15,&numint,UD_NODEFAULT);
	status = uv_getvnm(viewname, &view);
	if (status == UU_SUCCESS )
		uv_print_view(&view);

	uu_dexit;
	}
