#include "zsysdep.h"
#if UU_COMP == UU_WIN2K
/*********************************************************************
**  NAME:  wsntevent.cpp
**
**      GKS OpenGL event handling rontine
**
**       CONTAINS:
**				uw_ntclrcom()
**				uw_ntbcomlin()
**				uw_ntecomlin()
**				uw_ntget_event
**				uw_ntevent(event,xy)
**				uw_nttext_event()
**				uw_ntkbd
**				uw_ntbeep()
**				uw_ntgetcur_cursor()
**				uw_ntuser_event()
**				uw_ntpost_textmsg
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**       wsntevent.cpp , 25.2
**  DATE AND TIME OF LAST  MODIFICATION
**       11/09/16 , 10:44:21
*********************************************************************/
#include "wsntstdafx.h"
#include "wsntctl.h"
#include <conio.h>
#include <winspool.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include "gobas.h"
#include "go2.h"
#include <GL/gl.h>
#include <GL/glu.h>

#include "dmark.h"
#include "wsntdoc.h"
#include "wsntview.h"
#include "wsntframe.h"
#include "wsgl.h"
#include "wsntglfunc.h"
#include "zkeysym.h"
#include "wsntform.h"
#include "wsntres.h"
#include "wsntsmouse.h"
#include "spmouse.h"
#include "wsntpktwin.h"
#include "nclcmd.h"

extern HMAGELLAN MagellanHandle;

extern CMainFrame *NCL_MainFrame;
extern HWND NCL_cmdwin, NCL_statwin;

extern CWnd *NCL_Current_View;
extern CNCLForm *UW_active_frm;
extern HWND UW_NTgraphicIPV;

static int expose_event = 0;
char *kbdStr;
extern "C" int NT_FuncEvent, NT_FuncEof;

static int curr_cursor = 99;

extern "C" int UD_pickmode;
extern "C" int UV_dynview_active, UV_dynwheel;
int NCL_KeyReturn = 0;
extern "C"  int UW_text_cursor, UW_text_select;
 
extern "C" int uz_user_fkey_name(int, char*);
extern "C" int uz_user_key(int num,char **index,int xflag);
extern "C" int uz_user_fkey(int num,char **index,int xflag);
extern "C" int uw_ntsctopk(POINT *pt);

extern "C" int uw_ntgetcur_cursor();
extern "C" void ud_updatews(int);

extern "C" char *UM_pocket_hwnd;
extern "C" int uz_user_dascalls(char *indx);
extern "C" int uw_ntdispmsg(char*);
extern "C" int ud_jump(int,int);
extern "C" int uz_user_smouse(int event, int data[6], char **indx, int flag);
extern "C" int uw_ntcmd_sel(int start, int end);
extern int NCL_exit;
int UW_not_paint = 0;
extern "C" int UG_cursor_window;
extern "C" int UM_swap_ipv;
extern "C" int UD_textpre_curpos;
extern "C" int UD_string_add;
extern "C" int ud_setpick_type(int);
extern "C" int ud_getpick_type();
extern "C" void uw_ntupd_cinput(char*);
extern "C" int UW_keypad;
extern "C" void uw_nt_nclfunc(UINT id, int jflag);
extern "C" int uw_cmd_extend();
extern "C" int uw_ntload_cmdstr();
extern "C" int uw_ntget_cmdstr2(char* cmdstr, int *ln);
extern "C" int uw_ntreplc_cmdstr(char *string);
extern "C" void setnln(int *);

extern "C" char UD_prompt[1224];

static int Skbd_open = 0;
static char S_lstr[200], S_pstr[200], S_estr[200];

extern "C" int uw_ntinsert_cmdstr(char *);
extern "C" int NCL_cmdmod;
extern "C" int insmode_val();
/**********************************************************************
**    I_FUNCTION :  uw_ntclrcom
**       Clear Command edit field for single-line and current line
**			for multi_window
**    PARAMETERS   
**       INPUT  :None 
**	     OUTPUT : None 
**    RETURNS      : none
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntclrcom()
{
	if ((NCL_MainFrame->m_commandBar)->m_visible==0)
		return 0;
	if (uw_cmd_extend()&&(NCL_cmdmod))
	{
		(NCL_MainFrame->m_commandBar)->ClearCurrentLine();
		return 0;
	}
	CString label;
	NCL_MainFrame->m_commandBar->setcmdtext(label, "");
	if ((UW_active_frm!=NULL)&&(UW_active_frm->m_actedit!=NULL)
			&&((UW_active_frm->m_actedit)->IsWindowEnabled()))
	{
		(UW_active_frm->m_actedit)->SetWindowText("");
	}
	return 0;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntbcomlin
**       Pit insert cirsor in the beginning of Command edit field
**    PARAMETERS   
**       INPUT  :None 
**	     OUTPUT : None 
**    RETURNS      : none
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntbcomlin()
{
	if ((NCL_MainFrame->m_commandBar)->m_visible==0)
		return 0;
	NCL_MainFrame->m_commandBar->setcmdsel(0, 0);
	if ((UW_active_frm!=NULL)&&(UW_active_frm->m_actedit!=NULL)
			&&((UW_active_frm->m_actedit)->IsWindowEnabled()))
	{
		(UW_active_frm->m_actedit)->SetSel(0, 0);
	}
	return 0;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntecomlin
**       Pit insert cursor in the end of Command edit field
**    PARAMETERS   
**       INPUT  :None 
**	     OUTPUT : None 
**    RETURNS      : none
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntecomlin()
{
	CString wintxt;
	int len;
	if ((NCL_MainFrame->m_commandBar)->m_visible==0)
		return 0;
	NCL_MainFrame->m_commandBar->Set_ecomlin();
	if ((UW_active_frm!=NULL)&&(UW_active_frm->m_actedit!=NULL)
			&&((UW_active_frm->m_actedit)->IsWindowEnabled()))
	{
		(UW_active_frm->m_actedit)->GetWindowText(wintxt); 
		len = wintxt.GetLength();
		(UW_active_frm->m_actedit)->SetSel(len, len);
	}
	return 0;
}
extern "C" void uw_ntcom_dellin()
{
	if ((NCL_MainFrame->m_commandBar)->m_visible==0)
		return;
	NCL_MainFrame->m_commandBar->DeleteLine();
}

extern "C" void uw_ntcom_inslin()
{
	if ((NCL_MainFrame->m_commandBar)->m_visible==0)
		return;
	NCL_MainFrame->m_commandBar->InsertLine();
}
extern "C" void uw_ntcom_actlin()
{
	if ((NCL_MainFrame->m_commandBar)->m_visible==0)
		return;
	NCL_MainFrame->m_commandBar->ActLine();
}
extern "C" void uw_deset_insert()
{
	if ((NCL_MainFrame->m_commandBar)->m_visible==0)
		return;
	NCL_MainFrame->m_commandBar->Deset_insert();
}
/**********************************************************************
**    I_FUNCTION :  uw_ntcomlin
**       Put insert cirsor in "curpos" of Command edit field
**    PARAMETERS   
**       INPUT  :None 
**	     OUTPUT : None 
**    RETURNS      : none
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntcomlin(int curpos)
{
	if ((NCL_MainFrame->m_commandBar)->m_visible==0)
		return 0;
	if (uw_cmd_extend()&&(NCL_cmdmod))
	{
		NCL_MainFrame->m_commandBar->setcmdlsel(curpos, curpos);
	}
	else
		NCL_MainFrame->m_commandBar->setcmdsel(curpos, curpos);
	return 0;
}
/**********************************************************************
**    I_FUNCTION :  uw_ntget_ecurpos
**       Get current cursor in Command edit field
**    PARAMETERS   
**       INPUT  :None 
**	     OUTPUT : None 
**    RETURNS      : none
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntget_ecurpos(int *start, int *end)
{
	CString wintxt;
	if ((NCL_MainFrame->m_commandBar)->m_visible==0)
	{
		*start = *end = UD_textpre_curpos;
		return;
	}
	return NCL_MainFrame->m_commandBar->get_ecurpos(start, end);
}

/**********************************************************************
**    I_FUNCTION :  uw_ntget_lcurpos
**       Get current cursor in Command edit field
**    PARAMETERS   
**       INPUT  :None 
**	     OUTPUT : None 
**    RETURNS      : none
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntget_lcurpos(int *start, int *end, int *ln)
{
	CString wintxt;
	if ((NCL_MainFrame->m_commandBar)->m_visible==0)
	{
		*start = *end = UD_textpre_curpos;
		*ln = 0;
		return;
	}
	return NCL_MainFrame->m_commandBar->get_lcurpos(start, end, ln);
}

/*********************************************************************
**    E_FUNCTION : uw_ntget_event(int *event, int pflag, int *xpos,int*ypos, int cflag)
**      
**    DESCRIPTION:
**        Get the next event in the input queue.  This routine waits
**        until an event is received before returning.
**
**    PARAMETERS   
**       INPUT  :  purpose of use pflag and cflag is to save the execute time
**					to avoid unneed cursor reset and position get
**			pflag: 1: get the XY position
**						0: do not get XY position
**          cflag: 1: reset to origial cursor
**					0: do nothing about the cursor
**       OUTPUT :  
**          event      =  Key or mouse button entered
**          xpos,ypos  =  Position of mouse when event received
**
**    RETURNS      :
**          1  =  Normal character key.
**          2  =  Keypad key.
**          3  =  Mouse button.
**				4  =  Buttons and Dials
**          5  =  Arrow key.
**				6  =  Button release.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntget_event(int *event, int pflag, int *xpos,int*ypos, int cflag)
{ 
	int stat;
	MSG msg;
	int shift, control,savcur;
	char *hwnd;
	char buf[20], *indx;
	int data[6],sav_mode;
	POINT pt;
	UINT id;
	short delta;

	shift = 0;
	control = 0;
	stat = 0;
	indx = buf;
/*
.....Update the graphics
*/
	ud_updatews(UG_SUPPRESS);
/*
.....this function may change cursor back to default
.....save cursor and reset it if it is on graphic area
*/
	if (cflag)
		savcur = uw_ntgetcur_cursor();
/*
......when get into event function, we handle the user defined function by the NCL
......not by default accelerator key,... So we usally use UD_pickmode value to see
......if we are in input mode, but somewhere like dynanmic view loop, the UD_pickmode is 0
......and we are not in a input mode, but the callback/accelerator key function could called
......and cause the function called twice, so set UD_pickmode=1 here and reset UD_pickmode value after
......the function end
*/
	sav_mode = UD_pickmode;
	UD_pickmode = 1;
/*
.....Get event on the queue
*/
	if (GetMessage( &msg, NULL, 0, 0 ) != -1) 
	{
		if (cflag)
			uw_ntsetcursor(savcur);
/*
.....Pocket window input is active
*/
		if ((((UM_pocket_hwnd != 0) && (UM_swap_ipv==0) && (msg.hwnd != (HWND)UM_pocket_hwnd))
			|| ((msg.hwnd==UW_NTgraphic) && (UM_swap_ipv) && (UM_pocket_hwnd != 0)) )
			&&(msg.message != ID_NCL_FUNCTION)&&(msg.message != ID_NCL_USEREVENT))
		{
			if (msg.hwnd == (HWND)UM_pocket_hwnd &&
				(msg.message == WM_LBUTTONDOWN || msg.message == WM_MBUTTONDOWN ||
				msg.message == WM_RBUTTONDOWN))
			{
				if (msg.message == WM_LBUTTONDOWN) *event = 1;
				else if (msg.message == WM_MBUTTONDOWN) *event = 2;
				else *event = 3;
				stat = 3;
				*xpos = msg.pt.x;
				*ypos = msg.pt.y;
				if (cflag)
					uw_ntsetcursor(savcur);
				TranslateMessage(&msg);
				DispatchMessage(&msg);
				UD_pickmode = sav_mode;
				return(stat);
			}
			if (msg.hwnd == UW_NTgraphic &&
				(msg.message == WM_MBUTTONDOWN || msg.message == WM_RBUTTONDOWN))
			{
				if (msg.message == WM_MBUTTONDOWN) *event = 2;
				else *event = 3;
				stat = 3;
				*xpos = 0;
				*ypos = 0;
				if (cflag)
					uw_ntsetcursor(savcur);
				TranslateMessage(&msg);
				DispatchMessage(&msg);
				UD_pickmode = sav_mode;
				return(stat);
			}
			else
			{
				hwnd = UM_pocket_hwnd;
				UM_pocket_hwnd = 0;
				NT_FuncEof= expose_event;
				TranslateMessage(&msg);
				DispatchMessage( &msg );
				*event = NT_FuncEvent;
				stat = NT_FuncEof;
				*xpos = 0;
				*ypos = 0;
				UM_pocket_hwnd = hwnd;
			}
			if (cflag)
				uw_ntsetcursor(savcur);
			UD_pickmode = sav_mode;
			if ((MagellanHandle!=NULL)&&((msg.message==MagellanHandle->MagellanMotionEvent)
				|| (msg.message==MagellanHandle->MagellanButtonPressEvent)
				|| (msg.message==MagellanHandle->MagellanButtonReleaseEvent)))
					goto check_spacemouse;
			return(stat);
		}

/*
.....Event from other than graphic window and IPV window
*/
check_spacemouse:;
		if (((msg.hwnd!=UW_NTgraphic) && (msg.hwnd != UW_NTgraphicIPV))
			&&(msg.message != ID_NCL_FUNCTION)&&(msg.message != ID_NCL_USEREVENT)
			&& (msg.message != ID_3DBUTTON_EVENT))
		{
			NT_FuncEof= expose_event;
/*
......added for SpaceMouse
......spacemouse msg window handler will not be UW_NTgraphic
*/
			int bevent;
			MagellanIntegerEvent MagellanData;

			if (MagellanHandle!=NULL)
			{
				bevent = MagellanTranslateEvent(MagellanHandle, &msg,
		 		  									&MagellanData, event);
				if (bevent==ButtonPressEvent)
				{
					*event = MagellanData.MagellanButton;
					data[0] = MagellanData.MagellanData[0];
					data[1] = MagellanData.MagellanData[1];
					data[2] = MagellanData.MagellanData[2];
					data[3] = MagellanData.MagellanData[3];
					data[4] = MagellanData.MagellanData[4];
					data[5] = MagellanData.MagellanData[5];
					NT_FuncEof = uz_user_smouse(*event, data, &indx,1);
					if (NT_FuncEof==1)
					{
/*
.....Das key function, so the function is not execute now, pass on
*/
						NT_FuncEvent = *event;
						NT_FuncEof = 4;
					}
				}
				else if (bevent==MotionEvent)
				{
					data[0] = MagellanData.MagellanData[0];
					data[1] = MagellanData.MagellanData[1];
					data[2] = MagellanData.MagellanData[2];
					data[3] = MagellanData.MagellanData[3];
					data[4] = MagellanData.MagellanData[4];
					data[5] = MagellanData.MagellanData[5];
					NT_FuncEof = uz_user_smouse(*event, data, &indx,1);
				}
				if (UD_pickmode==0)
				{
					if (((int)(msg.wParam)!=VK_RETURN)&&(msg.message != WM_KICKIDLE && !NCL_MainFrame->NCLPreTranslateMessage(&msg)))
					{
						TranslateMessage(&msg);
						DispatchMessage(&msg);
					}
				}
				else
				{
					if (((int)(msg.wParam)!=VK_RETURN)&&(msg.message != WM_KICKIDLE))
					{
						NCL_MainFrame->NCLPreTranslateMessage2(&msg);
						TranslateMessage(&msg);
						DispatchMessage(&msg);
					}
				}
				if (!((bevent==ButtonPressEvent)||(bevent==MotionEvent)))
					*event = NT_FuncEvent;
				stat = NT_FuncEof;
				goto done;
			}
/*
.....the "RETURN" did nothing but beep
.....from DispatchMessage(&msg) for command line edit field
*/
			if (UD_pickmode==0)
			{
				if (((int)(msg.wParam)!=VK_RETURN)&&(msg.message != WM_KICKIDLE && !NCL_MainFrame->NCLPreTranslateMessage(&msg)))
				{
					TranslateMessage(&msg);
					DispatchMessage(&msg);
				}
			}
			else
			{
				if (((int)(msg.wParam)!=VK_RETURN)&&(msg.message != WM_KICKIDLE))
				{
/*
......we need handle mouse movement for statusbar
*/
					NCL_MainFrame->NCLPreTranslateMessage2(&msg);
					TranslateMessage(&msg);
					DispatchMessage(&msg);
				}
			}
			*event = NT_FuncEvent;
			if (cflag)
				uw_ntsetcursor(savcur);
			UD_pickmode = sav_mode;
			return NT_FuncEof;
		}
		switch (msg.message)
		{
		case  WM_TIMER:
			break;
		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
			if ((int)(msg.wParam)==VK_DOWN)
			{
				stat = 5;
				*event = 2;
			}
			else if ((int)(msg.wParam)==VK_UP)
			{
				stat = 5;
				*event = 1;
			}
			else if ((int)(msg.wParam)==VK_LEFT)
			{
				stat = 5;
				*event = 4;
			}
			else if ((int)(msg.wParam)==VK_RIGHT)
			{
				stat = 5;
				*event = 3;
			}

			else if ((int)(msg.wParam)==VK_PAUSE)
			{
				stat = 2;
				*event = 1;
			}
			else if ((int)(msg.wParam)==VK_SCROLL)
			{
				stat = 2;
				*event = 2;
			}
			else if ((int)(msg.wParam)==VK_HOME)
			{
				stat = 2;
				*event = 4;
			}
			else if ((int)(msg.wParam)==VK_PRIOR)
			{
				stat = 2;
				*event = 5;
			}
			else if ((int)(msg.wParam)==VK_NEXT)
			{
				stat = 2;
				*event = 6;
			}
			else if ((int)(msg.wParam)==VK_END)
			{
				stat = 2;
				*event = 7;
			}
			else if ((int)(msg.wParam)==VK_PRINT)
			{
				stat = 2;
				*event = 9;
			}
			else if ((int)(msg.wParam)==VK_EXECUTE)
			{
				stat = 2;
				*event = 10;
			}
			else if ((int)(msg.wParam)==VK_INSERT)
			{
				stat = 2;
				*event = 11;
			}
/* *event = 12, 13 "undo", "find" */
			else if ((int)(msg.wParam)==VK_CANCEL)
			{
				stat = 2;
				*event = 14;
			}
			else if ((int)(msg.wParam)==VK_HELP)
			{
				stat = 2;
				*event = 15;
			}
/* "break" 	*event = 16; */		
			else if ((int)(msg.wParam)==VK_NUMLOCK)
			{
				stat = 2;
				*event = 17;
			}
			else if (((int)(msg.wParam)==VK_SPACE) &&
				(HIWORD(msg.lParam) & KF_EXTENDED))
			{
				stat = 2;
				*event = 18;
			}
			else if ((int)(msg.wParam)==VK_TAB)
			{
				stat = 2;
				*event = 19;
			}
			else if ((int)(msg.wParam)==VK_MULTIPLY)
			{
				stat = 2;
				*event = 26;
			}
			else if ((int)(msg.wParam)==VK_ADD)
			{
				stat = 2;
				*event = 27;
			}
			else if ((int)(msg.wParam)==VK_SUBTRACT)
			{
				stat = 2;
				*event = 29;
			}
			else if ((int)(msg.wParam)==VK_SEPARATOR)
			{
				stat = 2;
				*event = 28;
			}
			else if ((int)(msg.wParam)==VK_DECIMAL)
			{
				stat = 2;
				*event = 30;
			}
			else if ((int)(msg.wParam)==VK_DIVIDE)
			{
				stat = 2;
				*event = 31;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD0)
			{
				stat = 2;
				*event = 32;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD1)
			{
				stat = 2;
				*event = 33;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD2)
			{
				stat = 2;
				*event = 34;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD3)
			{
				stat = 2;
				*event = 35;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD4)
			{
				stat = 2;
				*event = 36;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD5)
			{
				stat = 2;
				*event = 37;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD6)
			{
				stat = 2;
				*event = 38;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD7)
			{
				stat = 2;
				*event = 39;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD8)
			{
				stat = 2;
				*event = 40;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD9)
			{
				stat = 2;
				*event = 41;
			}
			else if ((int)(msg.wParam)==VK_F1)
			{
				stat = 2;
				*event = 42;
			}
			else if ((int)(msg.wParam)==VK_F2)
			{
				stat = 2;
				*event = 43;
			}
			else if ((int)(msg.wParam)==VK_F3)
			{
				stat = 2;
				*event = 44;
			}
			else if ((int)(msg.wParam)==VK_F4)
			{
				stat = 2;
				*event = 45;
			}
			else if ((int)(msg.wParam)==VK_F5)
			{
				stat = 2;
				*event = 46;
			}
			else if ((int)(msg.wParam)==VK_F6)
			{
				stat = 2;
				*event = 47;
			}
			else if ((int)(msg.wParam)==VK_F7)
			{
				stat = 2;
				*event = 48;
			}
			else if ((int)(msg.wParam)==VK_F8)
			{
				stat = 2;
				*event = 49;
			}
			else if ((int)(msg.wParam)==VK_F9)
			{
				stat = 2;
				*event = 50;
			}
			else if ((int)(msg.wParam)==VK_F10)
			{
				stat = 2;
				*event = 51;
			}
			else if ((int)(msg.wParam)==VK_F11)
			{
				stat = 2;
				*event = 52;
			}
			else if ((int)(msg.wParam)==VK_F12)
			{
				stat = 2;
				*event = 53;
			}
			else if ((int)(msg.wParam)==VK_RETURN)
			{
				if ((HIWORD(msg.lParam) & KF_EXTENDED) && UW_keypad == 1)
/*
.....kp_enter in the right side of keypad, act as function key here
*/
				{
					stat = 2;
					*event = 20;
				}
				else
/*
.....kp_enter in the left side of keypad, act as normal key enter
*/
				{
					stat = 1;
					*event = '\015';
				}
			}
			else
			{
				*event = (int)(msg.wParam);
				if ((*event!=VK_CONTROL) && (*event!=VK_SHIFT))
	 				stat = 1;
				else
					stat = 0;
				GetKeyNameText(msg.lParam, buf, 20);
				if (*event>=NKEYSYMS)
					*event = buf[0];
			}
			if (::GetKeyState(VK_SHIFT) < 0)
				shift = 1;
			if (::GetKeyState(VK_CONTROL) < 0)
				control = 1;
/*
.......lower case Normal key (MFC accept key as upper case
*/
			if (stat == 1 && shift == 0)
			{
				if ((*event >= 65)&&(*event<=90))
					*event = *event + 32;
			}
/*
.......Controlled Normal key
*/
			if (stat == 1 && control == 1)
			{
				if ((*event >= 97)&&(*event<=122))
					*event = *event - 96;
				if (*event == 0) *event = 64;
				if (*event >= 32) *event = *event + NKEYSYMS;
			}
/*
.......Controlled Function key
*/
			if (stat== 2 && control == 1)
			{
				*event = *event + NFKEYSYMS * 2;
			}
/*
.......Shifted Function key
*/
			if (stat== 2 && shift == 1)
			{
				*event = *event + NFKEYSYMS;
			}
			break;
		case WM_LBUTTONDBLCLK:
		case WM_LBUTTONDOWN:
			*event = 1;
			stat = 3;
			break;
		case WM_LBUTTONUP:
			*event = 1;
			stat = 6;
			break;
		case WM_MBUTTONDBLCLK:
		case WM_MBUTTONDOWN:
			*event = 2;
			stat = 3;
			break;
		case WM_MBUTTONUP:
			*event = 2;
			stat = 6;
			break;
		case WM_RBUTTONDBLCLK:
		case WM_RBUTTONDOWN:
			*event = 3;
			stat = 3;
			break;
		case WM_RBUTTONUP:
			*event = 3;
			stat = 6;
			break;
		case WM_MOUSEWHEEL:
			if ((UV_dynwheel==0)&&(UV_dynview_active))
			{
				*event = 0;
				stat = 0;
			}
			else
			{
				delta = HIWORD(msg.wParam);
				if (delta>0)
					*event = 4;
				else
					*event = 5;
				stat = 3;
			}
			break;
		case WM_MOUSEMOVE:
			stat = 0;
			break;
		case ID_NCL_USEREVENT:
			NT_FuncEvent = (int)(msg.wParam);
			*event = NT_FuncEvent;
			NT_FuncEof = (int)(msg.lParam);
			TranslateMessage(&msg);
			DispatchMessage( &msg );
			UD_pickmode = sav_mode;
			return NT_FuncEof;
		case ID_NCL_FUNCTION:
			id = (UINT)msg.wParam;
			uw_nt_nclfunc(id, 1);
			stat = 0;
			break;
		case ID_3DBUTTON_EVENT:
			NT_FuncEvent = (int)(msg.wParam);
			*event = NT_FuncEvent;
			stat = NT_FuncEof = 4;
			break;
		case ID_NCL_SMEVENT:
		default:
			{
/*
......added for SpaceMouse
*/
			int bevent;
			MagellanIntegerEvent MagellanData;

			if (MagellanHandle!=NULL)
			{
				bevent = MagellanTranslateEvent(MagellanHandle, &msg,
		 		  									&MagellanData, event);
				if (bevent==ButtonPressEvent)
				{
					*event = MagellanData.MagellanButton;
					data[0] = MagellanData.MagellanData[0];
					data[1] = MagellanData.MagellanData[1];
					data[2] = MagellanData.MagellanData[2];
					data[3] = MagellanData.MagellanData[3];
					data[4] = MagellanData.MagellanData[4];
					data[5] = MagellanData.MagellanData[5];
					NT_FuncEof = uz_user_smouse(*event, data, &indx,1);
					if (NT_FuncEof==1)
					{
/*
.....Das key function, so the function is not execute now, pass on
*/
						NT_FuncEvent = *event;
						NT_FuncEof = 4;
					}
				}
				else if (bevent==MotionEvent)
				{
					data[0] = MagellanData.MagellanData[0];
					data[1] = MagellanData.MagellanData[1];
					data[2] = MagellanData.MagellanData[2];
					data[3] = MagellanData.MagellanData[3];
					data[4] = MagellanData.MagellanData[4];
					data[5] = MagellanData.MagellanData[5];
					NT_FuncEof = uz_user_smouse(*event, data, &indx,1);
				}
			}
			else
			{
				NT_FuncEof= expose_event;
				*event = NT_FuncEvent;
			}
			stat = NT_FuncEof;
			if (((int)(msg.wParam)!=VK_RETURN)&&(msg.message != WM_KICKIDLE && !NCL_MainFrame->NCLPreTranslateMessage(&msg)))
			{
				TranslateMessage(&msg);
				DispatchMessage( &msg );
			}
			goto done;
			}
/*
.....key press...
*/
		}
		if (UD_pickmode==0)
		{
			if (((int)(msg.wParam)!=VK_RETURN)&&(msg.message != WM_KICKIDLE && 
				!NCL_MainFrame->NCLPreTranslateMessage(&msg)))
			{
				TranslateMessage(&msg);
				DispatchMessage( &msg );
			}
		}
		else
		{
			if (((int)(msg.wParam)!=VK_RETURN)&&(msg.message != WM_KICKIDLE))
			{
				NCL_MainFrame->NCLPreTranslateMessage2(&msg);
				TranslateMessage(&msg);
				DispatchMessage( &msg );
			}
		}
	}
	else
		stat = expose_event;
done:;
/*
......check to see if window have already end, if yes, just to top
*/
	if (NCL_exit)
	{
		UD_pickmode = sav_mode;
		ud_jump(-1,0);
	}
	if (stat == expose_event)
	{
		*xpos = 0;
		*ypos = 0;
	}
	else
	{
/*
........Windows 7 returns the pt location
........as 0,0 when the message is
........internally generated (Verify key for example)
........So get the cursor position in this case
........Bobby - 12/4/09
*/
		if (msg.pt.x == 0 && msg.pt.y == 0)
		{
			if (pflag)
			{
				GetCursorPos(&pt);
				*xpos = pt.x;
				*ypos = pt.y;
			}
		}
		else
		{
			*xpos = msg.pt.x;
			*ypos = msg.pt.y;
		}
	}
	if (cflag)
		uw_ntsetcursor(savcur);
	UD_pickmode = sav_mode;
	return stat;
}

/***********************************************************************
c
c   SUBROUTINE:  uw_ntflush_paint()
c
c   FUNCTION:  This function flush the paint command, execute paint once and
c              remove extra paint messages.  It was originally written
c              because of excessive flashing on Windows 2000, but is no
c              longer needed.  It actually causes some required repaints
c              to be tossed and can cause NCL to lock up on Windows XP if
c              a lot of repaints are received in a row.
c
c   INPUT: none
c   OUTPUT: none
c	RETURN: none			
c
c***********************************************************************
*/
extern "C" void uw_ntflush_paint()
{
/*
	MSG msg;;
	RECT lprc;
	int once = 0;
	while (::PeekMessage(&msg, NULL, NULL, NULL, PM_NOREMOVE))
	{
		if ((msg.message != WM_PAINT) || (msg.hwnd != UW_NTgraphic))
			break;
		GetClipBox(UW_NTgraphicDC, &lprc);
		if ((lprc.left==0) && (lprc.right==0) && (lprc.top==0) && (lprc.bottom==0) )
			break;
		if (!( (lprc.left == ((CNCLView*)NCL_Current_View)->m_oldRect.left)
			&&  (lprc.right == ((CNCLView*)NCL_Current_View)->m_oldRect.right)
			&&  (lprc.top == ((CNCLView*)NCL_Current_View)->m_oldRect.top)
			&&  (lprc.bottom == ((CNCLView*)NCL_Current_View)->m_oldRect.bottom)))
			break;
		if (!GetMessage(&msg, UW_NTgraphic, WM_PAINT, WM_PAINT))
			break;
		if (once==1)
		{
			UW_not_paint = 1;
			DispatchMessage(&msg);
			UW_not_paint = 0;
		}
		else
		{
			once = 1;
			DispatchMessage(&msg);
		}
	}
*/
	UW_not_paint = 0;
}
/*********************************************************************
**    E_FUNCTION : uw_ntevent(event,pflag, xy, flag)
**      
**    DESCRIPTION:
**        Get the next event in the input queue.  This routine does not
**        wait until an event is received before returning.  It will
**        return with the current mouse position.
**
**    PARAMETERS   
**       INPUT  : purpose of use pflag and cflag is to save the execute time
**					to avoid unneed cursor reset and position get
**			pflag: 1: get the XY position
**						0: do not get XY position
**          cflag: 1: reset to origial cursor
**					0: do nothing about the cursor
**       OUTPUT :  
**          event      =  Key or mouse button entered.  Set to 0 when
**                        an event is not available.
**          xpos,ypos  =  Position of mouse
**
**    RETURNS      :
**          1  =  Normal character key.
**          2  =  Keypad key.
**          3  =  Mouse button.
**			4  =  Buttons and Dials.
**          5  =  Arrow key.
**			6  =  Button Release.
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntevent(int *event, int pflag, int xy[2], int cflag)
{
	int irtn;
	MSG msg;
	POINT pt;
	
	NT_FuncEvent = 0;
	NT_FuncEof = 0;
	expose_event = -1;

	while (::PeekMessage(&msg, NULL, WM_PAINT, WM_PAINT, PM_NOREMOVE))
	{
		if (!GetMessage(&msg, NULL, WM_PAINT, WM_PAINT))
			break;
		DispatchMessage(&msg);
	}
	if (!PeekMessage(&msg, NULL, 0, 0, PM_NOREMOVE ))
	{
/*
.....Get the current mouse position
*/
		if (pflag)
		{
			GetCursorPos(&pt);
			uw_ntsctogc(&pt);
			xy[0] = pt.x; 
			xy[1] = pt.y;
		}
		expose_event = 0;
		return 0;
	}

	irtn = uw_ntget_event(event, pflag, &xy[0], &xy[1], cflag);

	if (irtn == expose_event)
	{
/*
.....Get the current mouse position
*/
		if (pflag)
		{
			GetCursorPos(&pt);
			uw_ntsctogc(&pt);
			xy[0] = pt.x; 
 			xy[1] = pt.y;
		}
		irtn = 0;
		NT_FuncEof = 0;
	}
	else
	{
		if (pflag)
		{
			pt.x = xy[0];
			pt.y = xy[1];
			uw_ntsctogc(&pt);
			xy[0] = pt.x; 
 			xy[1] = pt.y;
		}
	}
/*
.....Reset expose event flag
*/
	expose_event = 0;
	return(irtn);
}
/*********************************************************************
**    E_FUNCTION : uw_ntuser_event(int func_event, int func_end)
**
**    DESCRIPTION:
**        Post a NCL user event
**
**    PARAMETERS
**       INPUT  :
**          func_event: NCL event
**       func_end: event status
**       OUTPUT :
**          None
**
**    RETURNS: none      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntuser_event(int func_event, int func_end)
{
	NCL_MainFrame->PostMessage(ID_NCL_USEREVENT, (WPARAM)func_event, (LPARAM)func_end);
}

/*********************************************************************
**    E_FUNCTION : uw_nttext_event()
**      
**    DESCRIPTION:
**        Get the text input event
**
**    PARAMETERS   
**       INPUT  : 
**          None
**       OUTPUT :  
**          None
**
**    RETURNS: none      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_nttext_event()
{ 
	int event;
	char buf[20],*indx, *text;
	int stat, ir;
	MSG msg;
	int shift, control, data[6];
	UINT id;
	short delta;
	int msg_ret;

	shift = 0;
	control = 0;
	stat = 0;
	indx = buf;
/*
.....Get event on the queue
*/
	if (GetMessage( &msg, NULL, 0, 0 ) != -1) 
	{
		msg_ret = 0;
		switch (msg.message)
		{
		case WM_SYSKEYDOWN:
		case WM_KEYDOWN:
			if ((int)(msg.wParam)==VK_DOWN)
			{
				if (msg.hwnd!=NCL_cmdwin)
				{
					stat = 0;
					event = 0;
				}
				else if (uw_cmd_extend()&&(NCL_cmdmod))
				{
					stat = 0;
					event = 0;
				}
				else
				{
					stat = 5;
					event = 129;
				}
			}
			else if ((int)(msg.wParam)==VK_UP)
			{
				if (msg.hwnd!=NCL_cmdwin)
				{
					stat = 0;
					event = 0;
				}
				else if (uw_cmd_extend()&&(NCL_cmdmod))
				{
					stat = 0;
					event = 0;
				}
				else
				{
					stat = 5;
					event = 128;
				}
			}
			else if ((int)(msg.wParam)==VK_LEFT)
			{
				stat = 0;
				event = 0;
			}
			else if ((int)(msg.wParam)==VK_RIGHT)
			{
				stat = 0;
				event = 0;
			}

			else if ((int)(msg.wParam)==VK_PAUSE)
			{
				stat = 2;
				event = 1;
			}
			else if ((int)(msg.wParam)==VK_SCROLL)
			{
				if (msg.hwnd!=NCL_cmdwin)
				{
					stat = 0;
					event = 0;
				}
				else if (uw_cmd_extend()&&(NCL_cmdmod))
				{
					stat = 0;
					event = 0;
				}
				else
				{
					stat = 2;
					event = 2;
				}
			}
			else if ((int)(msg.wParam)==VK_HOME)
			{
				if (msg.hwnd!=NCL_cmdwin)
				{
					stat = 0;
					event = 0;
				}
				else
				{
					stat = 2;
					event = 4;
				}
			}
			else if ((int)(msg.wParam)==VK_PRIOR)
			{
				if (msg.hwnd!=NCL_cmdwin)
				{
					stat = 0;
					event = 0;
				}
				else if (uw_cmd_extend()&&(NCL_cmdmod))
				{
					stat = 0;
					event = 0;
				}
				else
				{
					stat = 2;
					event = 5;
				}
			}
			else if ((int)(msg.wParam)==VK_NEXT)
			{
				if (msg.hwnd!=NCL_cmdwin)
				{
					stat = 0;
					event = 0;
				}
				else if (uw_cmd_extend()&&(NCL_cmdmod))
				{
					stat = 0;
					event = 0;
				}
				else
				{
					stat = 2;
					event = 6;
				}
			}
			else if ((int)(msg.wParam)==VK_END)
			{
				if (msg.hwnd!=NCL_cmdwin)
				{
					stat = 0;
					event = 0;
				}
				else if (uw_cmd_extend()&&(NCL_cmdmod))
				{
					stat = 0;
					event = 0;
				}
				else
				{
					stat = 2;
					event = 7;
				}
			}
			else if ((int)(msg.wParam)==VK_PRINT)
			{
				stat = 2;
				event = 9;
			}
			else if ((int)(msg.wParam)==VK_EXECUTE)
			{
				stat = 2;
				event = 10;
			}
			else if ((int)(msg.wParam)==VK_INSERT)
			{
				if (msg.hwnd!=NCL_cmdwin)
				{
					stat = 0;
					event = 0;
				}
				else
				{
					stat = 2;
					event = 11;
				}
			}
			else if ((int)(msg.wParam)==VK_CANCEL)
			{
				stat = 2;
				event = 14;
			}
			else if ((int)(msg.wParam)==VK_HELP)
			{
				stat = 2;
				event = 15;
			}
			
			else if ((int)(msg.wParam)==VK_NUMLOCK)
			{
				stat = 2;
				event = 17;
			}
			else if ((int)(msg.wParam)==VK_MULTIPLY)
			{
				stat = 2;
				event = 26;
			}
			else if ((int)(msg.wParam)==VK_ADD)
			{
				stat = 2;
				event = 27;
			}
			else if ((int)(msg.wParam)==VK_SUBTRACT)
			{
				stat = 2;
				event = 29;
			}
			else if ((int)(msg.wParam)==VK_SEPARATOR)
			{
				stat = 2;
				event = 28;
			}
			else if ((int)(msg.wParam)==VK_DECIMAL)
			{
				stat = 2;
				event = 30;
			}
			else if ((int)(msg.wParam)==VK_DIVIDE)
			{
				stat = 2;
				event = 31;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD0)
			{
				stat = 2;
				event = 32;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD1)
			{
				stat = 2;
				event = 33;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD2)
			{
				stat = 2;
				event = 34;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD3)
			{
				stat = 2;
				event = 35;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD4)
			{
				stat = 2;
				event = 36;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD5)
			{
				stat = 2;
				event = 37;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD6)
			{
				stat = 2;
				event = 38;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD7)
			{
				stat = 2;
				event = 39;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD8)
			{
				stat = 2;
				event = 40;
			}
			else if ((int)(msg.wParam)==VK_NUMPAD9)
			{
				stat = 2;
				event = 41;
			}
			else if ((int)(msg.wParam)==VK_F1)
			{
				stat = 2;
				event = 42;
			}
			else if ((int)(msg.wParam)==VK_F2)
			{
				stat = 2;
				event = 43;
			}
			else if ((int)(msg.wParam)==VK_F3)
			{
				stat = 2;
				event = 44;
			}
			else if ((int)(msg.wParam)==VK_F4)
			{
				stat = 2;
				event = 45;
			}
			else if ((int)(msg.wParam)==VK_F5)
			{
				stat = 2;
				event = 46;
			}
			else if ((int)(msg.wParam)==VK_F6)
			{
				stat = 2;
				event = 47;
			}
			else if ((int)(msg.wParam)==VK_F7)
			{
				stat = 2;
				event = 48;
			}
			else if ((int)(msg.wParam)==VK_F8)
			{
				stat = 2;
				event = 49;
			}
			else if ((int)(msg.wParam)==VK_F9)
			{
				stat = 2;
				event = 50;
			}
			else if ((int)(msg.wParam)==VK_F10)
			{
				stat = 2;
				event = 51;
			}
			else if ((int)(msg.wParam)==VK_F11)
			{
				stat = 2;
				event = 52;
			}
			else if ((int)(msg.wParam)==VK_F12)
			{
				stat = 2;
				event = 53;
			}
			else if ((int)(msg.wParam)==VK_RETURN)
			{
				if ((HIWORD(msg.lParam) & KF_EXTENDED) && UW_keypad == 1)
/*
.....kp_enter in the right side of keypad, act as function key here
.....if (UW_keypad==1)
*/
				{
					stat = 2;
					event = 20;
				}
				else
/*
.....kp_enter in the left side of keypad, act as normal key enter
*/
				{
/*
......extented command line handle itself
*/
					if (uw_cmd_extend()&&(NCL_cmdmod))
					{
						stat = 0;
						event = 0;
					}
					else
					{
						stat = 1;
						event = '\015';
					}
				}
			}
			else
			{
				event = (int)(msg.wParam);
				if ((event!=VK_CONTROL) && (event!=VK_SHIFT))
	 				stat = 1;
				else
					stat = 0;
				GetKeyNameText(msg.lParam, buf, 20);
				if (event>=NKEYSYMS)
					event = buf[0];
			}
			if (::GetKeyState(VK_SHIFT) < 0)
				shift = 1;
			if (::GetKeyState(VK_CONTROL) < 0)
				control = 1;	
/*
.......lower case Normal key (MFC accept key as upper case
*/
			if (stat == 1 && shift == 0)
			{
				if ((event >= 65)&&(event<=90))
					event = event + 32;
			}
/*
.......Controlled Normal key
*/
			ir = 0;
			if (stat == 1 && control == 1)
			{
				if ((event >= 97)&&(event<=122))
					event = event - 96;
				if (event == 0) event = 64;
				if (event >= 32) event = event + NKEYSYMS;
			}
			else if ((stat == 1)&& (( event != '\015') && ( event != '\013')))
				stat = 0;
/*
.....0,1..9 keypad, could be function or just numeric key
.....for keypad '.', '+', '-', '*', '/' also
*/
			else if ((UW_keypad==0) && (stat == 2)
					&& (( event >=26) && ( event <= 41)))
				stat = 0;
/*
.......Controlled Function key
*/
			if (stat== 2 && control == 1)
			{
				event = event + NFKEYSYMS * 2;
			}
/*
.......Shifted Function key
*/
			if (stat== 2 && shift == 1)
			{
				event = event + NFKEYSYMS;
			}
			if (uw_cmd_extend()&&(NCL_cmdmod))
				msg_ret = (NCL_MainFrame->m_commandBar)->HandleSpecialKey(&msg);
			if ((stat!=1)&&(stat!=2)&&(msg_ret==0))
				TranslateMessage(&msg);
/*
......if it is controled key, check if it is defined function, otherwise
......use window default function (we want CTL+c CTL+v and CTL+x works)
*/
			if (control == 1)
			{
				ir = uz_user_key(event, &indx, 0);
				if (ir==2)
					TranslateMessage(&msg);
			}
			break;
		case WM_CHAR:
			break;
		case WM_LBUTTONDBLCLK:
		case WM_LBUTTONDOWN:
			stat = 0;
			break;
		case WM_MBUTTONDBLCLK:
		case WM_MBUTTONDOWN:
			event = 2;
			stat = 3;
			if (msg.hwnd==NCL_cmdwin)
				stat = 0;
			break;
		case WM_RBUTTONDBLCLK:
		case WM_RBUTTONDOWN:
			event = 3;
			stat = 3;
			if ((msg.hwnd==NCL_cmdwin)||(msg.hwnd==NCL_statwin))
				stat = 0;
			break;
		case WM_MOUSEWHEEL:
			if (((UV_dynwheel==0)&&(UV_dynview_active)) || (uw_cmd_extend()&&(NCL_cmdmod)))
			{
				event = 0;
				stat = 0;
			}
			else
			{
				delta = HIWORD(msg.wParam);
				if (delta>0)
					event = 4;
				else
					event = 5;
				stat = 3;
			}
			break;
		case WM_MOUSEMOVE:
			stat = 0;
			break;
		case ID_NCL_USEREVENT:
			NT_FuncEvent = (int)(msg.wParam);
			NT_FuncEof = (int)(msg.lParam);
			return NT_FuncEof;
		case ID_NCL_USERTEXT:
			text = (char*)(msg.lParam);
			uw_ntupd_cinput(text);
			stat = 0;
			break;
		case ID_NCL_FUNCTION:
			
char cmdstr[NCL_MAX_COMLINE];			
int textpre_curln;
		if (uw_cmd_extend()&&(NCL_cmdmod))
		{
			cmdstr[0] = '\0';
			uw_ntget_cmdstr2(cmdstr, &textpre_curln);
			textpre_curln++;
			setnln(&textpre_curln);
//			sprintf_s(UD_prompt, 256, "edit line %d:", textpre_curln);
		}
//		else
	//		uw_ntget_cmdstr(cmdstr);
			
			
			id = (UINT)msg.wParam;
			uw_nt_nclfunc(id, 1);
			stat = 0;
			break;
		case IDC_EXECUTE:
			NCL_MainFrame->m_commandBar->HandleCommandExe(msg.lParam, msg.wParam);
			stat = 0;
			break;
		case ID_3DBUTTON_EVENT:
			NT_FuncEvent = (int)(msg.wParam);
			event = NT_FuncEvent;
			stat = NT_FuncEof = 4;
			break;
		default:
			{
/*
......added for SpaceMouse
*/
				int bevent;
				MagellanIntegerEvent MagellanData;

				if (MagellanHandle!=NULL)
				{
					bevent = MagellanTranslateEvent(MagellanHandle, &msg,
		 		  									&MagellanData, &event);
					if (bevent==ButtonPressEvent)
					{
						event = MagellanData.MagellanButton;
						data[0] = MagellanData.MagellanData[0];
						data[1] = MagellanData.MagellanData[1];
						data[2] = MagellanData.MagellanData[2];
						data[3] = MagellanData.MagellanData[3];
						data[4] = MagellanData.MagellanData[4];
						data[5] = MagellanData.MagellanData[5];
						NT_FuncEof = uz_user_smouse(event, data, &indx,1);
						if (NT_FuncEof==1)
						{
/*
.....Das key function, so the function is not execute now, pass on
*/
							NT_FuncEvent = event;
							NT_FuncEof = 4;
						}
					}
					else if (bevent==MotionEvent)
					{
						data[0] = MagellanData.MagellanData[0];
						data[1] = MagellanData.MagellanData[1];
						data[2] = MagellanData.MagellanData[2];
						data[3] = MagellanData.MagellanData[3];
						data[4] = MagellanData.MagellanData[4];
						data[5] = MagellanData.MagellanData[5];
						NT_FuncEof = uz_user_smouse(event, data, &indx,1);
					}
				}
				else
				{
					NT_FuncEof= 0;
				}
				stat = NT_FuncEof;
			}
/*
.....key press...
*/
		}
		if (msg_ret==0)
			DispatchMessage( &msg );
	}
	else
		stat = 0;
/*
.....temp set here because for SendMessage WM_KEYDOWN from ncl routine
.....is not working, so use NCL_KeyReturn for return key
*/
/*
......check to see if window have already end, if yes, just to top
*/		
	if (NCL_exit)
		ud_jump(-1,0);
	if (NCL_KeyReturn==1)
	{
		stat = 1;
		event = '\015';
		NCL_KeyReturn = 0;
	}
	NT_FuncEvent = event;
	NT_FuncEof= stat;

	return NT_FuncEof;
}

/**********************************************************************
**    I_FUNCTION :  uw_ntkbd(ws,row,col,dev,msg,msglen,k)
**       Managing routine for the "By Text" prompt area.  Gets the user's
**			input from the text input area.
**    PARAMETERS   
**       INPUT  : 
**				ws      = Ignored.
**          msg     = Text message to display in Prompt area.
**			msglen  = Length of 'msg'.
**				row     = Ignored.
**				col     = Ignored.
**				dev     = Ignored.
**       OUTPUT :  
**          k       = Key that terminated input.  Text entered is
**				      stored in 'kbdStr'.
**    RETURNS      : none
**    SIDE EFFECTS : Redirects keyboard input from all other
**		               windows into the text field.
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_ntkbd(int ws, int row, int col, int dev, char *msg,
						int msglen, int *k)
{
	int ir,markval,save_ptype, save_cmdmod;
	char savstr[NCL_MAX_COMLINE];
	int sav_mode, end;
	int ist, status,textpre_curln;
	char cmdstr[NCL_MAX_COMLINE], fname[256];
	char *indx, buf[256];


	indx = buf;
/*
.....show the command window
*/
	status = (NCL_MainFrame->m_commandBar)->ShowWindow();
/*
.....if should not get here. let ourselives know
*/
	if (status==-1)
	{
		uw_ntdispmsg("Call uw_ntkbd while command window is not created");
		ud_jump(-1, UU_FALSE); 
	}
/*
.....Update the graphics
*/
	ud_updatews(UG_SUPPRESS);
/*
.....Save currurent event in order to
.....execute event that interupt by select menu
.....after execute menu function
.....Yurong 9/9/98
*/
	ug_save_event();
	sav_mode = UD_pickmode ;
	UD_pickmode = 1;
	save_ptype = ud_getpick_type();
	save_cmdmod = NCL_cmdmod;
	if ((uw_cmd_extend()==0)||(NCL_cmdmod==0))
		uw_ntget_cmdstr(savstr);
	else
		savstr[0] = '\0';
	
	Skbd_open = 1;
	UD_MARK(markval, UU_FALSE);
	if(markval == 0)
	{
/*
.....Enable keyboard input &
*/
//	XtSetSensitive(uw_mfprompt.prompt,True);
/*
......need enable command first since enable routine will reset the initial value
......which load_cmdstr might changed.
*/
		uw_ntenable_cmd(1);
		if ((uw_cmd_extend()==0)||(NCL_cmdmod==0))
			uw_ntset_cmdstr(msg);
		else
		{
			uw_ntload_cmdstr();
/*
.....if INSERT mode, don't replace
*/
			if (insmode_val()!=2)
			{
				uw_deset_insert();
				uw_ntreplc_cmdstr(msg);
			}
		}
		uw_ntsetcom_focus(1);

		if (UD_string_add)
			uw_ntcomlin(UD_textpre_curpos);
		else if ((uw_cmd_extend()==0)||(NCL_cmdmod==0))
		{
			if (UW_text_cursor)
				uw_ntecomlin();
			else
				uw_ntbcomlin();
		}
		if ((UW_text_select)&&((uw_cmd_extend()==0)||(NCL_cmdmod==0)))
			uw_ntcmd_sel(0, -1);
/*
.....Save prompt area labels
.....just in case they are overwritten
.....by another call during text input
*/
		uw_ntgetprm(S_pstr); 
		uw_ntgetprlabel(S_lstr); 
		uw_ntgetprmerr(S_estr);
/*
.....Loop until End-of-Input
.....Which is set in the Text Field Activation Callback
*/
		NT_FuncEof = 0;
		cmdstr[0] = '\0';
//		uw_ntget_ecurpos(&UD_textpre_curpos, &end);
		uw_ntget_lcurpos(&UD_textpre_curpos, &end, &textpre_curln);
		while (!NT_FuncEof)
		{
			uw_ntget_lcurpos(&UD_textpre_curpos, &end, &textpre_curln);
//			uw_ntget_ecurpos(&UD_textpre_curpos, &end);
//			uw_ntsetcursor(22);
/*
........Get input event
*/
			uw_nttext_event();
			ir = 0;
			if (NT_FuncEof == 1)
/*
.....ctrl normal key, will treat as hot key to execute the function now
.....there stay where they are
*/
			{
				uw_ntsetcursor(21);
				ir = uz_user_key(NT_FuncEvent,&indx,1);
/*
......if not return key
......after execute the function, stay
*/
				if ((NT_FuncEvent!='\013') && (NT_FuncEvent!='\015'))
					NT_FuncEof = 0;
			}
			if (NT_FuncEof == 2) 
			{
				uw_ntsetcursor(21); 
				uz_user_fkey_name(NT_FuncEvent,fname);
				if ((strcmp(fname, "NCL_CALCULATOR")==0) ||
					(strcmp(fname, "CAM_SCALARS")==0))
				{
					ir = uz_user_fkey(NT_FuncEvent,&indx,1);
				}
				else
					ir = uz_user_fkey(NT_FuncEvent,&indx,0);
			}
		
			if (ir == -1) NT_FuncEof = 0;
/*
........We still support Clear field
*/
			else if (ir == 1 && strcmp(indx,"\\\\I") == 0)
			{
				uw_ntclrcom();
				NT_FuncEof = 0;
			}
			else if (ir == 1 && strcmp(indx,"\\blin") == 0)
			{
				uw_ntbcomlin();
				NT_FuncEof = 0;
			}
			else if (ir == 1 && strcmp(indx,"\\elin") == 0)
			{
				uw_ntecomlin();
				NT_FuncEof = 0;
			}
			else if (ir == 1 && strcmp(indx,"\\dellin") == 0)
			{
				uw_ntcom_dellin();
				NT_FuncEof = 0;
			}
			else if (ir == 1 && strcmp(indx,"\\inslin") == 0)
			{
				uw_ntcom_inslin();
				NT_FuncEof = 0;
			}
			else if (ir == 1 && strcmp(indx,"\\actln") == 0)
			{
				uw_ntcom_actlin();
				NT_FuncEof = 0;
			}
			else if (ir == 5)
			{
				if (indx[0]!='\0')
					uw_ntinsert_cmdstr(indx);
				NT_FuncEof = 0;
			}
/*
.....add flush for openGL
.....yurong
*/
			ud_updatews(UG_SUPPRESS);
/*
........In case this is reentrant code
........Set the prompt area sensitivity
*/
			uw_ntenable_cmd(1);
/*
.....commenting the SetFocus system call, since it causes freezing of NCL in 
.....WINDOWS VISTA when command mode is selected. SetFocus was called repeatedly
.....every time this loop is executed causing VISTA to freeze.
		uw_ntsetcom_focus(1);
*/
/*
......don't reset this every time we get an event for multi-window
......it handle event themselvies
*/
			if ((uw_cmd_extend()==0)||(NCL_cmdmod==0))
			{
				uw_ntwrplabel(S_lstr);
				uw_ntwrprm(S_pstr);
				uw_ntprmerr(S_estr);
			}
/*
.....Reset currurent event in order to
.....execute event that interupt by select menu
.....Yurong 9/9/98
*/
			ug_reset_event();
			UD_pickmode = sav_mode;
			ud_setpick_type(save_ptype);
			NCL_cmdmod = save_cmdmod;
		}
		if (uw_cmd_extend()&&(NCL_cmdmod))
		{
			cmdstr[0] = '\0';
			uw_ntget_cmdstr2(cmdstr, &textpre_curln);
			textpre_curln++;
			setnln(&textpre_curln);
			sprintf_s(UD_prompt, 256, "edit line %d:", textpre_curln);
		}
		else
			uw_ntget_cmdstr(cmdstr);
/*
.....delete current insert line if it's empty in INSERT mode
*/
		(NCL_MainFrame->m_commandBar)->handle_insln();
	}
	else
	{
		cmdstr[0] = '\0';
	}
	kbdStr = cmdstr;
/*
.....remove focus from command line
*/
	uw_ntsetcom_focus(0);
/*
.....Disable text input
*/
	uw_ntenable_cmd(0);
/*
.....Reset default string
.....(reentrant code)
*/
	if (uw_cmd_extend()&&(NCL_cmdmod))
		uw_ntset_cmdstr("");
	else
		uw_ntset_cmdstr(savstr);
/*
.....Return key that ended input
*/
	*k = NT_FuncEvent;
	strncpy(msg, kbdStr, NCL_MAX_COMLINE-1);
	msg[NCL_MAX_COMLINE-1]= '\0';
	
	uw_ntsetcursor(21); 
	ug_reset_event();

	ist = NT_FuncEof;
	NT_FuncEvent = 0;
	NT_FuncEof = 0;
	Skbd_open = 0;
	UD_UNMARK(markval);
	return(ist);
}

/**********************************************************************
**    I_FUNCTION :  uw_ntbeep
**       Beep
**    PARAMETERS   
**       INPUT  :None 
**	     OUTPUT : None 
**    RETURNS      : none
**    SIDE EFFECTS : None
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntbeep()
{
	MessageBeep(0);
}

/*********************************************************************
**    E_FUNCTION : uw_ntpost_textmsg(char *text)
**
**    DESCRIPTION:
**        Post a ID_NCL_USERTEXT event which will update insert a text string
**			into the command line
**
**    PARAMETERS
**       INPUT  :
**        text: text string to be input into command line window
**       OUTPUT :
**          None
**
**    RETURNS: none      :
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntpost_textmsg(char *text)
{
	NCL_MainFrame->PostMessage(ID_NCL_USERTEXT, (WPARAM)NULL, (LPARAM)text);
}
/*********************************************************************
**    E_FUNCTION     : ud_chkwin_event();
**			Check if there is a window event in the mesage queue
**    PARAMETERS
**       INPUT  :none
**
**       OUTPUT :none
**    RETURNS      : 0: no event in the queue
**					1: yes, there is a message in the queue
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" int uw_check_event()
{
	MSG msg;
	if (!PeekMessage(&msg, NULL, 0, 0, PM_NOREMOVE ))
		return 0;
	return 1;
}
extern "C" void uw_ntsav_plabel(char *text)
{
	strcpy_s(S_lstr, sizeof(S_lstr), text);
}
extern "C" void uw_ntsav_pprm(char *text)
{
	strcpy_s(S_pstr, sizeof(S_pstr), text);
}
extern "C" void uw_ntsav_perr(char *text)
{
	strcpy_s(S_estr,  sizeof(S_estr), text);
}

#endif

