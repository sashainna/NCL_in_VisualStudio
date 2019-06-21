/*********************************************************************
**    NAME         :  gpocket.c
**       CONTAINS:
**				ug_pawaitloc
**    COPYRIGHT 2001 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       gpocket.c , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:05:23
*********************************************************************/

#include "usysdef.h"
#include "driver.h"
#include "gtbl.h"
#include "gdidd.h"
#include "gdidd2.h"
#include "mpocket.h"
#include "dinput.h"

extern char *UM_pocket_hwnd;
extern int UM_swap_ipv;
int UG_cursor_window = 0;
/*********************************************************************
**    E_FUNCTION     :  ug_pawaitloc(prompt,cursor,button,xpos,ypos)
**       Routine used to locate a position in the pocket window utilizing
**       the mouse.
**    PARAMETERS
**       INPUT  :
**          prompt   Prompt to display.
**          cursor   Cursor to display.  0 = Pick. 2 = Locate.
**          stat     Field status.
**       OUTPUT :
**          none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int ug_pawaitloc(prompt,cursor,button,xpos,ypos)
char *prompt;
int cursor;
int *button;
int *xpos,*ypos;
{
	int xy[2],done;
	char *um_get_ncl_win();

	if(UD_Rpstate[UD_Rpstate_ptr].flag == PLAYBACK)
	{
/*
......get the next input from disk
*/
		ud_rprd_ipvloc(&done, button, xpos,ypos);
		return done;
	}	
/*
.....Initialize pocket picking
*/
	UG_cursor_window = 2;
	if (UM_swap_ipv==0)
		UM_pocket_hwnd = um_get_pocket_window(UM_IPV_WINDOW);
	else
		UM_pocket_hwnd = um_get_ncl_win();
/*
.....Display prompt
*/
	ud_wrprm(prompt);
/*
.....Let the user pick a location
*/
	xy[0] = *xpos; xy[1] = *ypos;
	done = (*(ug_gksstli.wsopen[0].connid)[UG_DTRK])(0,xy,button,cursor,
		0,0,UU_NULL);
	*xpos = xy[0];
	*ypos = xy[1];
/*
.....Take down prompt
*/
	ud_wrprm(" ");
/*
.....Reset pocket picking
*/
	UM_pocket_hwnd = UU_NULL;
	if(UD_Rpstate[UD_Rpstate_ptr].flag == RECORD)
		ud_rpwr_ipvloc(done, *button, xy[0], xy[1]);
	UG_cursor_window = 0;
	return(done);
}
