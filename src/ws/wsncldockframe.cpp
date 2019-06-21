/********************************************************************* 
**  NAME:  wsncldockframe.cpp
**
**			Implementation file for CNCLDockFrameWnd
**	CONTAINS: 
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsncldockframe.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:15
*********************************************************************/

#include "toolstdafx.h"
#include "wsncldockframe.h"
#include "wsncltoolbar.h"
#include "wsntformbar.h"
#include "wsnttmenu.h"
#include "wsntcmdbar.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
#define _AfxGetDlgCtrlID(hWnd)          ((UINT)(WORD)::GetDlgCtrlID(hWnd))
/*
.....don't use value which define such as HTCLOSE, HTHELP in user.h
*/
#define DFHT_HELP 		52
extern "C" int UL_clswin_flag;
extern "C" int clswin();
extern int UW_struct_change;

/////////////////////////////////////////////////////////////////////////////
// CNCLDockFrameWnd

IMPLEMENT_DYNCREATE(CNCLDockFrameWnd, CMiniDockFrameWnd)

CNCLDockFrameWnd::CNCLDockFrameWnd()
{
	m_ButtonDown = -1;
}

CNCLDockFrameWnd::~CNCLDockFrameWnd()
{
}

BEGIN_MESSAGE_MAP(CNCLDockFrameWnd, CMiniDockFrameWnd)
	//{{AFX_MSG_MAP(CNCLDockFrameWnd)
	ON_WM_CLOSE()
	ON_WM_CREATE()
	ON_WM_NCPAINT()
	ON_WM_NCACTIVATE()
	ON_WM_NCHITTEST()
	ON_WM_NCLBUTTONDOWN()
	ON_WM_NCLBUTTONUP()
//	ON_WM_NCRBUTTONUP()
	ON_WM_NCRBUTTONDOWN()
	ON_WM_SYSCOMMAND()
	ON_WM_NCLBUTTONDBLCLK()
	ON_WM_NCMOUSEMOVE()
	ON_COMMAND_RANGE(ID_POPUP_SAVE, ID_POPUP_EDIT_MENU, OnHandlePopup)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNCLDockFrameWnd message handlers

/***********************************************************************
**
**   FUNCTION: OnCreate(LPCREATESTRUCT lpCreateStruct) 
**
**		Override this member function to perform any needed 
**		initialization of a derived class. For here modify the system menu
**   
**		INPUT:  LPCREATESTRUCT lpCreateStruct: contains copies of 
**						the parameters used to create the window.
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
int CNCLDockFrameWnd::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CMiniDockFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;
	CMenu* pSysMenu = GetSystemMenu(FALSE);
	int status;
	if (pSysMenu != NULL)
	{
		for (int i=0; i<20; i++)
		{
			status = pSysMenu->DeleteMenu(0, MF_BYPOSITION);
			if (status==0)
				break;
		}
		pSysMenu->AppendMenu(MF_STRING|MF_ENABLED, ID_POPUP_SAVE, _T("Save"));
		pSysMenu->AppendMenu(MF_STRING|MF_ENABLED, ID_POPUP_SAVE_AS, _T("Save As"));
		pSysMenu->AppendMenu(MF_STRING|MF_ENABLED, ID_POPUP_EDIT_MENU, _T("Edit"));
		pSysMenu->AppendMenu(MF_STRING|MF_ENABLED, ID_POPUP_CANCEL, _T("Cancel"));
	}
}

/***********************************************************************
c
c   FUNCTION: OnHandlePopup(UINT nID)
c
c           callbacks for Popup menu from title bar
c
c   INPUT:  nID: menu item ID
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDockFrameWnd::OnHandlePopup(UINT nID)
{
	int choice;
	if (nID==ID_POPUP_SAVE_AS)
		choice = 1;
	else if (nID==ID_POPUP_SAVE)
		choice = 2;
	else if (nID==ID_POPUP_EDIT_MENU)
		choice = 3;
	else
		choice = -1;
	if (choice==-1)
		return;
	CControlBar* pBar = (CControlBar*) (m_wndDockBar.m_arrBars[1]);
	if (pBar != NULL)
	{
		if (pBar->IsKindOf(RUNTIME_CLASS(CToolmenu)))
		{
			((CToolmenu*)pBar)->HandleRButtonUp(choice);
			return;
		}
	}
}
/***********************************************************************
c
c   FUNCTION: OnSysCommand(UINT nID, LPARAM lp)
c
c           callbacks for menu item from System menu
c
c   INPUT:  nID: menu item ID
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDockFrameWnd::OnSysCommand(UINT nID, LPARAM lp)
{
	if ((nID!=ID_POPUP_SAVE)&&(nID!=ID_POPUP_SAVE_AS)&&(nID!=ID_POPUP_EDIT_MENU))
	{
		CMiniDockFrameWnd::OnSysCommand(nID, lp);
		return;
	}
	PostMessage(WM_COMMAND, nID, 0);
}
/***********************************************************************
c
c   FUNCTION: HitTest( CPoint point )
c
c       Call this function to find out which area the user is click in
c
c   INPUT: 
c			point:  Specifies the x- and y-coordinate of point need to be tested. 
c
c   OUTPUT :   None
c   RETURN:    One of the mouse hit-test enumerated values listed below. 
c				DFHT_HELP		in the title-bar '?' help box area
c				HTBORDER   In the border of a window that does not have a sizing border.
c				HTBOTTOM   In the lower horizontal border of the window.
c				HTBOTTOMLEFT   In the lower-left corner of the window border.
c				HTBOTTOMRIGHT   In the lower-right corner of the window border.
c				HTCAPTION   In a title-bar area.
c				HTCLIENT   In a client area.
c				HTERROR   On the screen background or on a dividing line between windows (same as HTNOWHERE except that the DefWndProc Windows function produces a system beep to indicate an error).
c				HTGROWBOX   In a size box.
c				HTHSCROLL   In the horizontal scroll bar.
c				HTLEFT   In the left border of the window.
c				HTMAXBUTTON   In a Maximize button.
c				HTMENU   In a menu area.
c				HTMINBUTTON   In a Minimize button.
c				HTNOWHERE   On the screen background or on a dividing line between windows.
c				HTREDUCE   In a Minimize button.
c				HTRIGHT   In the right border of the window.
c				HTSIZE   In a size box (same as HTGROWBOX).
c				HTSYSMENU   In a Control menu or in a Close button in a child window.
c				HTTOP   In the upper horizontal border of the window.
c				HTTOPLEFT   In the upper-left corner of the window border.
c				HTTOPRIGHT   In the upper-right corner of the window border.
c				HTTRANSPARENT   In a window currently covered by another window.
c				HTVSCROLL   In the vertical scroll bar.
c				HTZOOM   In a Maximize button. 
c
**********************************************************************/
DWORD CNCLDockFrameWnd::HitTest(CPoint point)
{
	CPoint pt=point;
	ScreenToClient(&pt);
	if (IsIconic()==0)
	{
		pt.y += GetSystemMetrics(SM_CYCAPTION) - 2;
		pt.x += GetSystemMetrics(SM_CXBORDER) + GetSystemMetrics(SM_CXFRAME);
	}

	int frmtype = 0;
	CRect rect=m_rcHelp;
	int nPos = 1;
	CControlBar* pBar = NULL;
	while(pBar == NULL && nPos < m_wndDockBar.m_arrBars.GetSize())
	{
		pBar = (CControlBar*) (m_wndDockBar.m_arrBars[nPos]);
		nPos++;
	}

	if (pBar->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
	{
		if (((CNCLFormBar* )pBar)->m_helpact == 1)
			frmtype = 1;
	}
	if ((frmtype==1) && (rect.PtInRect(pt)))
	{
		return (DWORD) DFHT_HELP;
	}
	else
		return 0;
}

/***********************************************************************
c
c   FUNCTION: OnNcHitTest( CPoint point )
c
c       The framework calls this member function 
c			for the CWnd object that contains the cursor 
c			every time the mouse is moved.
c
c   INPUT: 
c			point:  Specifies the x- and y-coordinate of the cursor. 
c					These coordinates are always screen coordinates.
c
c   OUTPUT :   None
c   RETURN:    One of the mouse hit-test enumerated values listed below. 
c				DFHT_HELP		in the title-bar '?' help box area
c				HTBORDER   In the border of a window that does not have a sizing border.
c				HTBOTTOM   In the lower horizontal border of the window.
c				HTBOTTOMLEFT   In the lower-left corner of the window border.
c				HTBOTTOMRIGHT   In the lower-right corner of the window border.
c				HTCAPTION   In a title-bar area.
c				HTCLIENT   In a client area.
c				HTERROR   On the screen background or on a dividing line between windows (same as HTNOWHERE except that the DefWndProc Windows function produces a system beep to indicate an error).
c				HTGROWBOX   In a size box.
c				HTHSCROLL   In the horizontal scroll bar.
c				HTLEFT   In the left border of the window.
c				HTMAXBUTTON   In a Maximize button.
c				HTMENU   In a menu area.
c				HTMINBUTTON   In a Minimize button.
c				HTNOWHERE   On the screen background or on a dividing line between windows.
c				HTREDUCE   In a Minimize button.
c				HTRIGHT   In the right border of the window.
c				HTSIZE   In a size box (same as HTGROWBOX).
c				HTSYSMENU   In a Control menu or in a Close button in a child window.
c				HTTOP   In the upper horizontal border of the window.
c				HTTOPLEFT   In the upper-left corner of the window border.
c				HTTOPRIGHT   In the upper-right corner of the window border.
c				HTTRANSPARENT   In a window currently covered by another window.
c				HTVSCROLL   In the vertical scroll bar.
c				HTZOOM   In a Maximize button. 
c
**********************************************************************/
LRESULT CNCLDockFrameWnd::OnNcHitTest(CPoint point) 
{
	DWORD hitTest = HitTest(point);
	if (hitTest==0)
	{
		CPoint pt=point;
		ScreenToClient(&pt);
		if (IsIconic()==0)
		{
			pt.y += GetSystemMetrics(SM_CYCAPTION) - 2;
			pt.x += GetSystemMetrics(SM_CXBORDER) + GetSystemMetrics(SM_CXFRAME);
		}
		return CMiniDockFrameWnd::OnNcHitTest(pt);
	}
	else
		return hitTest;
}

/***********************************************************************
c
c   FUNCTION: OnClose()
c
c           callback for close dock frame window.
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDockFrameWnd::OnClose()
{
	CControlBar* pBar = NULL;
	for (int nPos = 0; nPos < m_wndDockBar.m_arrBars.GetSize(); nPos++)
	{
		pBar = (CControlBar* )m_wndDockBar.m_arrBars[nPos];
		if (pBar != NULL)
		{
			if (pBar->IsKindOf(RUNTIME_CLASS(CNCLToolBar)))
			{
				((CNCLToolBar* )pBar)->m_visible = 0;
				CFrameWnd* pFrameWnd = pBar->GetDockingFrame();
				pFrameWnd->ShowControlBar(pBar, FALSE, FALSE);
			}
			else if (pBar->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
			{
				if (((CNCLDialogBar* )pBar)->m_bartype==6)
					return;
				((CNCLFormBar* )pBar)->FormClose();
			}
			else
			{
				if ((((CNCLDialogBar* )pBar)->m_bartype==1)
					||(((CNCLDialogBar* )pBar)->m_bartype==5)
					||(((CNCLDialogBar* )pBar)->m_bartype==4)
					||(((CNCLDialogBar* )pBar)->m_bartype==6))
					return;
				((CNCLDialogBar* )pBar)->m_visible = 0;
				CFrameWnd* pFrameWnd = pBar->GetDockingFrame();
				pFrameWnd->ShowControlBar(pBar, FALSE, FALSE);
				if (((CNCLDialogBar* )pBar)->m_bartype==2)
				{
					UL_clswin_flag = 1;
					clswin();
					UL_clswin_flag = 0;
				}
			}
		}
	}
}
/***********************************************************************
c
c   FUNCTION: OnNcMouseMove(UINT nHitTest, CPoint point)
c
c           The framework calls this member function when the
c			cursor is moved within a nonclient area
c
c   INPUT:  nHitTest: Specifies the hit-test code. A hit test is a test that determines the location of the cursor
c			point: Specifies a CPoint object that contains the x and y screen coordinates of the cursor position
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDockFrameWnd::OnNcMouseMove( UINT nHitTest, CPoint point)
{
	CMiniFrameWnd::OnNcMouseMove(nHitTest, point);
	if (m_ButtonDown == DFHT_HELP) 
	{
		CWindowDC dc(this);
		DWORD hitTest = HitTest(point);
		if (hitTest!=DFHT_HELP)
		{
			DrawFrameControl(dc.m_hDC,
				m_rcHelp,
				DFC_CAPTION,
				DFCS_CAPTIONHELP);
			m_ButtonDown = -1;
		}
	}
}

/***********************************************************************
c
c   FUNCTION: OnNcLButtonUp(UINT nHitTest, CPoint point)
c
c           The framework calls this member function when the user 
c			releases the left mouse button while the cursor is within a nonclient area
c
c   INPUT:  nHitTest: Specifies the hit-test code. A hit test is a test that determines the location of the cursor
c			point: Specifies a CPoint object that contains the x and y screen coordinates of the cursor position
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
/*
......not used any more
*/
void CNCLDockFrameWnd::OnNcLButtonUp(UINT nHitTest, CPoint point)
{
	DWORD hitTest = HitTest(point);
	if (hitTest == DFHT_HELP) 
	{
		int nPos = 1;
		CControlBar* pBar = NULL;
		while(pBar == NULL && nPos < m_wndDockBar.m_arrBars.GetSize())
		{
			pBar = (CControlBar*) (m_wndDockBar.m_arrBars[nPos]);
			nPos++;
		}
		if (pBar == NULL)
			return;

		((CNCLFormBar*)pBar)->FormHelp();
	}
	else
		CMiniFrameWnd::OnNcLButtonUp(nHitTest, point);
	if (m_ButtonDown == DFHT_HELP) 
	{
		CWindowDC dc(this);
		DrawFrameControl(dc.m_hDC,
				m_rcHelp,
				DFC_CAPTION,
				DFCS_CAPTIONHELP);
	}
	m_ButtonDown = -1;
}
void CNCLDockFrameWnd::OnNcRButtonDown(UINT nHitTest, CPoint point)
{
/*
.....Don't call default Right button function in order to avoid to call
.....system popup menu with "MOVE" and "HIDE", we want our own Pupup menu
*/
	CControlBar* pBar = (CControlBar*) (m_wndDockBar.m_arrBars[1]);

/*
......for some reason, the system menu item "Hide" is always show even I remove all system item
......in OnCreate routine, But if I remove here, it works
*/
	CMenu* pSysMenu = GetSystemMenu(FALSE);
	int status;
	if (pSysMenu != NULL)
	{
		pSysMenu->DeleteMenu(4, MF_BYPOSITION);
	}
	CMiniFrameWnd::OnNcRButtonDown(nHitTest, point);
}
/***********************************************************************
c
c   FUNCTION: OnNcRButtonUp(UINT nHitTest, CPoint point)
c
c           The framework calls this member function when the user 
c			releases the right mouse button while the cursor is within a nonclient area
c
c   INPUT:  nHitTest: Specifies the hit-test code. A hit test is a test that determines the location of the cursor
c			point: Specifies a CPoint object that contains the x and y screen coordinates of the cursor position
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDockFrameWnd::OnNcRButtonUp(UINT nHitTest, CPoint point)
{
/*
.....first check if this is a menu
*/
	CControlBar* pBar = (CControlBar*) (m_wndDockBar.m_arrBars[1]);
	if (pBar != NULL)
	{
		if (pBar->IsKindOf(RUNTIME_CLASS(CToolmenu)))
		{
			((CToolmenu*)pBar)->HandleRButtonUp(-1);
			return;
		}
	}
	CMiniDockFrameWnd::OnNcLButtonUp(nHitTest, point);
}


/***********************************************************************
c
c   FUNCTION: OnNcLButtonDown(UINT nHitTest, CPoint point)
c
c           The framework calls this member function when the user 
c			push the left mouse button while the cursor is within a nonclient area
c
c   INPUT:  nHitTest: Specifies the hit-test code. A hit test is a test that determines the location of the cursor
c			point: Specifies a CPoint object that contains the x and y screen coordinates of the cursor position
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLDockFrameWnd::OnNcLButtonDown(UINT nHitTest, CPoint point)
{
	DWORD hitTest = HitTest(point);
	if (hitTest == DFHT_HELP) 
	{
		CWindowDC dc(this);
		int nPos = 1;
		CControlBar* pBar = NULL;
		while(pBar == NULL && nPos < m_wndDockBar.m_arrBars.GetSize())
		{
			pBar = (CControlBar*) (m_wndDockBar.m_arrBars[nPos]);
			nPos++;
		}
		if (pBar == NULL)
			return;

		if (pBar->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
		{
			if (((CNCLFormBar* )pBar)->m_helpact)
			{
				dc.DrawFrameControl(m_rcHelp,
								DFC_CAPTION,
								DFCS_CAPTIONHELP | DFCS_PUSHED);
			}
			else
			{
				dc.DrawFrameControl(m_rcHelp,
									DFC_CAPTION,
									DFCS_CAPTIONHELP | DFCS_INACTIVE);
			}
		}
		m_ButtonDown = nHitTest;
		return;
	}
	else if (nHitTest == HTCAPTION)
	{

		// initiate toolbar drag for non-CBRS_FLOAT_MULTI toolbars
		if ((m_wndDockBar.m_dwStyle & CBRS_FLOAT_MULTI) == 0)
		{
			UW_struct_change = 1;
			int nPos = 1;
			CControlBar* pBar = NULL;
			while(pBar == NULL && nPos < m_wndDockBar.m_arrBars.GetSize())
				pBar = (CControlBar* )m_wndDockBar.m_arrBars[nPos++];

			ASSERT(pBar != NULL);
			ASSERT_KINDOF(CControlBar, pBar);
			if (pBar->IsKindOf(RUNTIME_CLASS(CNCLToolBar)))
			{
				// special activation for floating toolbars
				ActivateTopParent();
				ASSERT(((CNCLToolBar*)pBar)->m_pDockContext != NULL);
				((CNCLToolBar*)pBar)->m_pDockContext->StartDrag(point);
			}
			else
			{
				ASSERT(pBar->m_pDockContext != NULL);
				if (pBar->IsKindOf(RUNTIME_CLASS(CNCLDialogBar)))
					((CNCLDialogBar*)pBar)->m_startdrag = 1;
				pBar->m_pDockContext->StartDrag(point);
				if (pBar->IsKindOf(RUNTIME_CLASS(CNCLDialogBar)))
					((CNCLDialogBar*)pBar)->m_startdrag = 0;
			}
			UW_struct_change = 0;
			return;
		}
	}
	else if (nHitTest >= HTSIZEFIRST && nHitTest <= HTSIZELAST)
	{
		UW_struct_change = 1;
		int nPos = 1;
		CControlBar* pBar = NULL;
		while(pBar == NULL && nPos < m_wndDockBar.m_arrBars.GetSize())
			pBar = (CControlBar* )m_wndDockBar.m_arrBars[nPos++];

		ASSERT(pBar != NULL);
		ASSERT_KINDOF(CControlBar, pBar);

// CBRS_SIZE_DYNAMIC toolbars cannot have the CBRS_FLOAT_MULTI style
		ASSERT((m_wndDockBar.m_dwStyle & CBRS_FLOAT_MULTI) == 0);
		if (pBar->IsKindOf(RUNTIME_CLASS(CNCLToolBar)))
		{
			ActivateTopParent();
			ASSERT(((CNCLToolBar*)pBar)->m_pDockContext != NULL);
			((CNCLToolBar*)pBar)->m_pDockContext->StartResize(nHitTest, point);
			SIZE bsize;
			bsize.cx = -1;
			bsize.cy = -1;
			((CNCLToolBar*)pBar)->SetButSizes(bsize);
		}
		else
		{
			ASSERT(pBar->m_pDockContext != NULL);
/*
......do not resize the CNCLCmdBar when it is single line window
*/
/*.....temp try resize
			if (pBar->IsKindOf(RUNTIME_CLASS(CNCLCmdBar)))
			{
				if (((CNCLCmdBar*)pBar)->get_extend()==0)
				{
					m_ButtonDown = nHitTest;
					UW_struct_change = 0;
					CMiniFrameWnd::OnNcLButtonDown(nHitTest, point);
					return;
				}
			}
*/
			pBar->m_pDockContext->StartResize(nHitTest, point);
		}
		m_ButtonDown = nHitTest;
		UW_struct_change = 0;
		return;
	}
	CMiniFrameWnd::OnNcLButtonDown(nHitTest, point);
}

/***********************************************************************
**
**   FUNCTION: OnNcLButtonDblClk(UINT nHitTest, CPoint point)
**
**       Left mouse button double click callback 
**		This function is overwrite CToolbar::CNCLDockFrameWnd
**				with little changes
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLDockFrameWnd::OnNcLButtonDblClk(UINT nHitTest, CPoint point)
{
	DWORD hitTest = HitTest(point);
	if (hitTest == DFHT_HELP) 
	{
		CWindowDC dc(this);
		int nPos = 1;
		CControlBar* pBar = NULL;
		while(pBar == NULL && nPos < m_wndDockBar.m_arrBars.GetSize())
		{
			pBar = (CControlBar*) (m_wndDockBar.m_arrBars[nPos]);
			nPos++;
		}
		if (pBar == NULL)
			return;

		if (pBar->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))
		{
			if (((CNCLFormBar* )pBar)->m_helpact)
			{
				dc.DrawFrameControl(m_rcHelp,
								DFC_CAPTION,
								DFCS_CAPTIONHELP | DFCS_PUSHED);
			}
			else
			{
				dc.DrawFrameControl(m_rcHelp,
									DFC_CAPTION,
									DFCS_CAPTIONHELP | DFCS_INACTIVE);
			}
		}
		return;
	}
	else if (nHitTest == HTCAPTION)
	{
		ActivateTopParent();

		// initiate toolbar toggle for non-CBRS_FLOAT_MULTI toolbars
		if ((m_wndDockBar.m_dwStyle & CBRS_FLOAT_MULTI) == 0)
		{
			int nPos = 1;
			CControlBar* pBar = NULL;
			while(pBar == NULL && nPos < m_wndDockBar.m_arrBars.GetSize())
				pBar = (CControlBar* )m_wndDockBar.m_arrBars[nPos++];

			ASSERT(pBar != NULL);
			ASSERT_KINDOF(CControlBar, pBar);
			if (pBar->IsKindOf(RUNTIME_CLASS(CNCLToolBar)))
			{
				ASSERT(((CNCLToolBar*)pBar)->m_pDockContext != NULL);
				((CNCLToolBar*)pBar)->m_pDockContext->ToggleDocking();
			}
			else
			{
				ASSERT(pBar->m_pDockContext != NULL);
				pBar->m_pDockContext->ToggleDocking();
			}
			return;
		}
	}
	CMiniFrameWnd::OnNcLButtonDblClk(nHitTest, point);
}
/***********************************************************************
c
c   FUNCTION: OnNcActivate(BOOL bActive)
c			called when active no-client area of the dialog
c
c   INPUT:  bActive: Specifies when a caption bar or icon needs to be 
c					changed to indicate an active or inactive state. 
c					The bActive parameter is TRUE if an active caption or 
c					icon is to be drawn. It is FALSE for an inactive caption or icon.
c
c   OUTPUT :   None.
c   RETURN:    True
c
**********************************************************************/
BOOL CNCLDockFrameWnd::OnNcActivate(BOOL bActive)
{
	Default();
	if ((GetStyle() & MFS_SYNCACTIVE) == 0)
	{
		m_bActive = bActive;
		SendMessage(WM_NCPAINT);
	}
	else if(m_nFlags & WF_KEEPMINIACTIVE)
	{
		return FALSE;
	}
	return TRUE;
}

/***********************************************************************
c
c   FUNCTION: OnNcPaint()
c			paint the no-client area
c
c   INPUT:  None.
c
c   OUTPUT :   None.
c   RETURN:    None.
c
**********************************************************************/
void CNCLDockFrameWnd::OnNcPaint() 
{
	int nPos = 1;
	CControlBar* pBar = NULL;
	while(pBar == NULL && nPos < m_wndDockBar.m_arrBars.GetSize())
	{
		pBar = (CControlBar*) (m_wndDockBar.m_arrBars[nPos]);
		nPos++;
	}
	if (pBar != NULL)
	{
		if (pBar->IsKindOf(RUNTIME_CLASS(CToolmenu)))
		{
			if (((CToolmenu* )pBar)->m_mtype==4)
			{
				ModifyStyle( WS_SYSMENU, 0, 0);
			}
		}
	}
	CMiniFrameWnd::OnNcPaint();

	if (pBar == NULL)
		return;

	if ((pBar->IsKindOf(RUNTIME_CLASS(CNCLFormBar)))==0)
		return;
	
	if (((CNCLFormBar* )pBar)->m_bartype != 3)
		return;

	CWindowDC dc(this);

	CRect rc;
	GetWindowRect(rc);
	rc.bottom = GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFRAME);

	CRect helpRect;
	helpRect.top = GetSystemMetrics(SM_CYBORDER) + GetSystemMetrics(SM_CYFRAME) + 1;
	helpRect.bottom = GetSystemMetrics(SM_CYCAPTION) - GetSystemMetrics(SM_CYBORDER) 
						- GetSystemMetrics(SM_CYFRAME) - 2;
	helpRect.left = rc.right - 2*(helpRect.bottom-helpRect.top) - 
				2 * (GetSystemMetrics(SM_CXBORDER) + GetSystemMetrics(SM_CXFRAME))
				- rc.left;
	helpRect.right = helpRect.left - helpRect.top + helpRect.bottom;

	if (((CNCLFormBar* )pBar)->m_helpact)
	{
		dc.DrawFrameControl(helpRect,
							DFC_CAPTION,
							DFCS_CAPTIONHELP);
	}
	else
	{
		dc.DrawFrameControl(helpRect,
							DFC_CAPTION,
							DFCS_CAPTIONHELP | DFCS_INACTIVE);
	}
	m_rcHelp = helpRect;
}


/***********************************************************************
**
**   FUNCTION: uw_ntadd_menuborder(CRect *rect)
**
**       Added border to rect
**
**   INPUT:  rect: rect to be changed
**
**   OUTPUT :   rect: rect which added border
**   RETURN:    None
**
**********************************************************************/
void uw_ntadd_menuborder(CRect *rect)
{
	CMiniFrameWnd::CalcBorders(rect);
	rect->InflateRect(-2, -2);
}
