#include "zsysdep.h"
#if UU_COMP == UU_WIN2K
/*********************************************************************
**  NAME:  wsntcursor.cpp
**
**      Windows NT cursor routines.
**
**       CONTAINS:
**				uw_ntdefcursor()
**				uw_ntsetcursor()
**				uw_ntgetcursor()
**
**    COPYRIGHT 2005 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**       wsntcursor.cpp , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:12:21
*********************************************************************/
#include "wsntstdafx.h"
#include "wsgl.h"
#include "wsntglfunc.h"
#include "wsntctl.h"
#include "wsntdoc.h"
#include "wsntview.h"
#include "wsntres.h"
#include "lipv.h"

#define NCURSOR 10
#define BLANK_CURSOR 0
#define MENU_CURSOR 1
#define PICK_CURSOR 2
#define LOCATE_CURSOR 3
#define STROKE_CURSOR 4
#define TEXT_CURSOR 5
#define PAN_CURSOR 6
#define ROTATE_CURSOR 7
#define ZOOM_CURSOR 8
#define MOUSE_CURSOR 9

extern CWnd *NCL_Current_View;
extern "C" int UG_cursor_window;
extern CDialog *Pocket_Win[UM_MAX_POCKET];
extern "C" int UM_swap_ipv;

static int curr_cursor = -99;
static HCURSOR Scursor[NCURSOR];
extern "C" void uw_setpkt_cursor_val (CWnd* win, HCURSOR cur);
extern "C" void uw_setpkt_cursor (CWnd* win, HCURSOR cur);
extern "C" void uw_setview_cursor(HCURSOR cur, int flag);
extern "C" int MSLite;
/*********************************************************************
**    E_FUNCTION : uw_ntdefcursor()
**
**	         Defines all of the cursors used within NCL.
**			
**    PARAMETERS   
**       INPUT  : none
**       OUTPUT :  none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
**
*********************************************************************/
extern "C" void uw_ntdefcursor()
{
	HINSTANCE hInst;
/*
.....Pick cursor
*/
	hInst = AfxFindResourceHandle(MAKEINTRESOURCE(IDC_PICKCUR), RT_CURSOR);
	Scursor[PICK_CURSOR] = LoadCursor(hInst, MAKEINTRESOURCE(IDC_PICKCUR));
/*
.....Menu cursor
*/
	Scursor[MENU_CURSOR] = LoadCursor(NULL, IDC_ARROW);
/*
.....Locator cursor
*/
	hInst = AfxFindResourceHandle(MAKEINTRESOURCE(IDC_LOCCUR), RT_CURSOR);
	Scursor[LOCATE_CURSOR] = LoadCursor(hInst, MAKEINTRESOURCE(IDC_LOCCUR));
/*
.....Stroke cursor
*/
	Scursor[STROKE_CURSOR] = LoadCursor(NULL, IDC_WAIT);
/*
.....Text cursor
*/
	Scursor[TEXT_CURSOR] = LoadCursor(NULL, IDC_IBEAM);
/*
.....PAN cursor
*/
	hInst = AfxFindResourceHandle(MAKEINTRESOURCE(IDC_PANCUR), RT_CURSOR);
	Scursor[PAN_CURSOR] = LoadCursor(hInst, MAKEINTRESOURCE(IDC_PANCUR));
/*
.....ROTATE cursor
*/
	hInst = AfxFindResourceHandle(MAKEINTRESOURCE(IDC_ROTCUR), RT_CURSOR);
	Scursor[ROTATE_CURSOR] = LoadCursor(hInst, MAKEINTRESOURCE(IDC_ROTCUR));
/*
.....ZOOM cursor
*/
	hInst = AfxFindResourceHandle(MAKEINTRESOURCE(IDC_ZOOMCUR), RT_CURSOR);
	Scursor[ZOOM_CURSOR] = LoadCursor(hInst, MAKEINTRESOURCE(IDC_ZOOMCUR));
/*
.....Mouse cursor
*/
	hInst = AfxFindResourceHandle(MAKEINTRESOURCE(IDC_MOUSECUR), RT_CURSOR);
	Scursor[MOUSE_CURSOR] = LoadCursor(hInst, MAKEINTRESOURCE(IDC_MOUSECUR));
/*
.........Blank cursor
*/
	Scursor[BLANK_CURSOR] = LoadCursor(NULL, IDC_WAIT);
}

/*********************************************************************
**    I_FUNCTION : uw_ntsetcursor(cursorno)
**
**	         Set the windows cursor as requested.
**			
**    PARAMETERS   
**       INPUT  : 
**          cursorno = 1            - Pick cursor.
**                     2            - Menu cursor.
**                     3,6,7        - Stroke cursor.
**                     4,5          - Locator cursor.
**                     21           - Blank cursor.
**			           40           - Pan cursor.
**			           41,42,44,45  - Rotate cursor.
**			           43           - Zoom cursor.
**       OUTPUT :  
**          output
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntsetcursor(int cursorno)
{
	int cursor;
/*
.....Initialize the cursors
*/
	if (curr_cursor == -99)
	{
		uw_ntdefcursor();
		curr_cursor = 99;
	}
/*
.....Define the cursor type
*/
	switch (cursorno)
	{
/*
........Pick cursor
*/
	case 0:
		cursor = PICK_CURSOR;
		break;
/*
........Menu cursor
*/
	case 1:
		cursor = MENU_CURSOR;
		break;
/*
........Locator cursor
*/
	case 2:
	case 4:
	case 5:
		cursor = LOCATE_CURSOR;
		break;
/*
........Stroke cursor
*/
	case 3:
	case 6:
	case 7:
		cursor = STROKE_CURSOR;
		break;
/*
........Text cursor
*/
	case 22:
		cursor = TEXT_CURSOR;
		break;
/*
........PAN cursor
*/
	case 40:
		cursor = PAN_CURSOR;
		break;
/*
........ROTATE cursor
*/
	case 41:
	case 42:
	case 44:
	case 45:
		cursor = ROTATE_CURSOR;
		break;
/*
........ZOOM cursor
*/
	case 43:
		cursor = ZOOM_CURSOR;
		break;
/*
........Mouse cursor
*/
	case 46:
		cursor = MOUSE_CURSOR;
		break;
/*
........Blank cursor
*/
	case 21:
	default:
		cursor = BLANK_CURSOR;
		break;
	}  /* end switch */
	if (Scursor[cursor] == NULL) return;
	if ((MSLite)||(LW_nclipv==LW_STANDALONE))
		UG_cursor_window = 3;
/*
.....set cursor for particular NCL view window
*/
	if  (UG_cursor_window==0)
	{
		if (NCL_Current_View != NULL)
		{
			uw_setview_cursor(Scursor[cursor], 0);
			if (Pocket_Win[UM_IPV_WINDOW]!=NULL)
				uw_setpkt_cursor_val (Pocket_Win[UM_IPV_WINDOW], Scursor[cursor]);
/*
.....don't use SetCursor directly, will set all cursor include menu/titlebar which we don't wait
.....just set cursor for the view
*/
//			SetCursor(Scursor[cursor]);
/*
......changed back, still set all cursor include menu/titlebar since if not, the cursor will stay pointer
......cursor even though the cursor is set to wait cursor if the cursor is on the menubar when set the cursor of the graphic area
......for example, set the wait cursor at begining of "Get/all" when cursor is on the menu, then move the cursor
......to the graphic area during the "Get/all", since the "Get/all" have not done (may take long time) and 
......not getback to window routine (it takes 10sec as we set to go back to window routine to reflush), 
......so the wait cursor not show until then. If we use general SetCursor routine, it will set all cursor, so it will stay.
*/
			SetCursor(Scursor[cursor]);
			uw_setview_cursor(Scursor[cursor], 1);
			if (Pocket_Win[UM_IPV_WINDOW]!=NULL)
				uw_setpkt_cursor (Pocket_Win[UM_IPV_WINDOW], Scursor[cursor]);
		}
	}
	else if ((UG_cursor_window==1)&&(UM_swap_ipv==0))
	{
		uw_setview_cursor(Scursor[cursor], 1);
	}
	else if ((UG_cursor_window==1)&&(UM_swap_ipv==1)
			&& (Pocket_Win[UM_IPV_WINDOW]!=NULL) )
	{
		uw_setpkt_cursor (Pocket_Win[UM_IPV_WINDOW], Scursor[cursor]);
	}
	else if ((UG_cursor_window==2)&&(Pocket_Win[UM_IPV_WINDOW]!=NULL) 
			&& (UM_swap_ipv==0))
	{
		uw_setpkt_cursor (Pocket_Win[UM_IPV_WINDOW], Scursor[cursor]);
	}
	else if ((UG_cursor_window==2)&&(UM_swap_ipv==1)&&(NCL_Current_View!=NULL))
		uw_setview_cursor(Scursor[cursor], 1);
	else if (UG_cursor_window==3)
		uw_setview_cursor(Scursor[cursor], 1);
/*
.......Save the current cursor
*/
	curr_cursor = cursorno;
}

/*********************************************************************
**    E_FUNCTION : uw_ntgetcur_cursor()
**
**    DESCRIPTION:
**        Get the current active cursor (graphic area)
**
**    PARAMETERS
**       INPUT  :
**          None
**       OUTPUT :
**          None
**
**    RETURNS: Current cursor  :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntgetcur_cursor()
{
	return curr_cursor;
}
#endif
