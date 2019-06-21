/********************************************************************* 
**  NAME:  wsncltoolbar.cpp
**
**			Native WinNT CNCLToolBar class
**			implementation. This class is a subclass of
**			CToolBar but have some specified character for NCL
**
**	CONTAINS: CNCLToolBar class functions
**			all functions declared in wsncltoolbar.h
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsncltoolbar.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:15
*********************************************************************/

#include "toolstdafx.h"
#include "wsncltoolbar.h"
#include <afxole.h>         // MFC OLE classes

#include "afxpriv.h"
#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

extern int UW_drag_obj;
extern "C" void uw_nthide_menu(CToolBar *bar);

int _afxDropDownWidth = -1;
CNCLToolBar *UW_current_menubar = NULL;

#define ARROW_X _afxDropDownWidth


/***********************************************************************
**
**   FUNCTION: _AfxGetDropDownWidth()
**
**       Return Menu CY size
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
int _AfxGetDropDownWidth()
{
	// return cached version if already determined...
	if (_afxDropDownWidth != -1)
		return _afxDropDownWidth;

	// otherwise calculate it...
	HDC hDC = GetDC(NULL);
	ASSERT(hDC != NULL);
	HFONT hFont;
	if ((hFont = CreateFont(GetSystemMetrics(SM_CYMENUCHECK), 0, 0, 0,
		FW_NORMAL, 0, 0, 0, SYMBOL_CHARSET, 0, 0, 0, 0, _T("Marlett"))) != NULL)
		hFont = (HFONT)SelectObject(hDC, hFont);
	VERIFY(GetCharWidth(hDC, '6', '6', &_afxDropDownWidth));
	if (hFont != NULL)
	{
		SelectObject(hDC, hFont);
		DeleteObject(hFont);
	}
	ReleaseDC(NULL, hDC);
	ASSERT(_afxDropDownWidth != -1);
	return _afxDropDownWidth;
}

IMPLEMENT_DYNAMIC(CNCLToolBar,CToolBar)

BEGIN_MESSAGE_MAP(CNCLToolBar,CToolBar)
	//{{AFX_MSG_MAP(CNCLToolBar)
//	ON_WM_ERASEBKGND()
	ON_WM_CONTEXTMENU()
	ON_WM_DESTROY()
	ON_WM_PAINT()
	ON_WM_NCCALCSIZE()
	ON_WM_NCPAINT()
	ON_WM_NCHITTEST()
	ON_WM_WINDOWPOSCHANGING() 
	ON_WM_LBUTTONDOWN()
	ON_WM_LBUTTONDBLCLK()
	ON_WM_LBUTTONUP()
	ON_WM_NCACTIVATE()
	ON_WM_MOUSEMOVE()
	//}}AFX_MSG_MAP
 END_MESSAGE_MAP()

BOOL CNCLToolBar::m_bButtons=FALSE;
BOOL CNCLToolBar::m_bText=FALSE;

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

/***********************************************************************
**
**   FUNCTION: CNCLToolBar
**
**		Constructor of class CNCLToolBar
**
**   INPUT:  none
**
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLToolBar::CNCLToolBar()
{
	m_bHasBitmaps=TRUE;
/*
.....see if we force to display text, now set to false
*/
	m_bForceText=FALSE;
	m_dwMode = 0x0000;
	m_mbuttype = 1;
	m_visible = -1;
	m_floating = -1;
	m_pDockContext = NULL;
	m_changed = 0;
	m_row = 0;
	m_StartDrag = 0;
	m_cyTopBorder = 3;
	m_cyBottomBorder = 3;
	m_StartPoint = CPoint(-1, -1);
	m_TargetDrop = new CNCLMnDropTarget();
	if(m_TargetDrop)
		m_TargetDrop->Register(this, CF_TEXT);
	m_TimerID = 0;
	m_hititem = -1;
	m_barnum = -1;
/*
.....when stardard type = 1
.....it always execute the default MFC function
.....since all toolbar of NCL use CNCLTOOLBAR, we allow
.....set the type to execute the standard MFC CToolbar
*/
	m_standard_type = 0;
}

/***********************************************************************
**
**   FUNCTION: ~CNCLToolBar
**
**              Destructor of class CNCLToolBar, free space.
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLToolBar::~CNCLToolBar()
{
	CNCLDockContext* pDockContext = m_pDockContext;
	if (pDockContext!=NULL)
		delete pDockContext;
	m_pDockContext = NULL;
}

/***********************************************************************
**
**   FUNCTION: OnRecalcSize()
**
**       Changed the setting and Recalculate ToolBar size
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::OnRecalcSize()
{
	SettingChange();
	RecalcSize();
	GetParentFrame()->DelayRecalcLayout();
}

/***********************************************************************
**
**   FUNCTION: SettingChange()
**
**       Changed ToolBar Setting
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::SettingChange()
{
	TBBUTTON tb;
	for(int n=0; n<GetCount(); n++)
	{
		SendMessage(TB_GETBUTTON,n,(LPARAM)&tb);
		tb.iString=m_bText?m_TextIds[n]:-1;
		_SetButton(n,&tb);
	}
	SendMessage(TB_SETMAXTEXTROWS,m_bText||m_bForceText?4:0);
	SendMessage(TB_SETBITMAPSIZE,0,m_bButtons&&m_bHasBitmaps?MAKELONG(16,15):0);
	SendMessage(TB_AUTOSIZE);
}

/***********************************************************************
**
**   FUNCTION: RecalcSize()
**
**       Recalculate ToolBar size
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLToolBar::RecalcSize()
{
	UINT nID, nStyle;
	int iImage;
	CRect rect;
	int cx=0,cy=0;
	SendMessage(TB_AUTOSIZE);
	for(int n=0; n<GetToolBarCtrl().GetButtonCount(); n++)
	{
		GetItemRect(n,&rect);
		GetButtonInfo(n, nID, nStyle, iImage);
		if (nStyle&TBSTYLE_DROPDOWN)
		{
			rect.right -= ARROW_X;
		}
		if(rect.Width()>cx) cx=rect.Width();
	}
	m_sizeButton.cx=cx;
	m_bDelayedButtonLayout=TRUE;
	return TRUE;
}

/***********************************************************************
**
**   FUNCTION: CreateEx(CWnd* pParentWnd, DWORD dwExStyle, 
**			DWORD dwStyle, UINT nID, LPCTSTR szTitle)
**
**       Create a NCL Toolbar with extended style
**
**   INPUT:  pParentWnd: parent window
**			dwExStyle: Toolbar extended style
**			dwStyle: Toolbar style
**			nID: Toolbar ID
**			szTitle: title of toolbar
**
**   OUTPUT :   None
**   RETURN:    FALSE: not created
**
**********************************************************************/
BOOL CNCLToolBar::CreateEx(CWnd* pParentWnd, DWORD dwExStyle, DWORD dwStyle, UINT nID, LPCTSTR szTitle)
{
	if(!CToolBar::Create(pParentWnd,dwStyle,nID)) return FALSE;
	if(szTitle) SetWindowText(szTitle);
	m_pParent=pParentWnd;
	ModifyStyle(0,dwExStyle);
	_AfxGetDropDownWidth();
	return TRUE;
}

/***********************************************************************
**
**   FUNCTION: SetButtonText(int nIndex, LPCTSTR lpszText)
**
**       Set Toolbar button text
**
**   INPUT:  nIndex: toolbar button index
**			lpszText: button text to be set
**
**   OUTPUT :   None
**   RETURN:    FALSE: not set.
**
**********************************************************************/
BOOL CNCLToolBar::SetButtonText(int nIndex, LPCTSTR lpszText)
{
	if(!CToolBar::SetButtonText(nIndex,lpszText)) return FALSE;
	if (m_standard_type)
		return TRUE;
	if (m_bartype==0)
		return RecalcSize();
	else
	{
/*
.....status bar, size will not changed by reset Text
*/
		SetSizes(m_sizeButton, m_sizeImage);
		return TRUE;
	}
}


/***********************************************************************
**
**   FUNCTION: RemoveButtonText(int nIndex)
**
**       Remove Toolbar button text label
**
**   INPUT:  nIndex: toolbar button index
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::RemoveButtonText(int nIndex)
{
	TBBUTTON button;
	_GetButton(nIndex, &button);
	if (button.iString==-1)
		return;
	button.iString = -1;
	_SetButton(nIndex, &button);

	m_bDelayedButtonLayout=TRUE;
}


/***********************************************************************
**
**   FUNCTION: OnDestroy() 
**
**       Destroy the toolbar
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::OnDestroy() 
{
	CToolBar::OnDestroy();
}


/***********************************************************************
**
**   FUNCTION: _SetButton(int nIndex, TBBUTTON* pButton)
**
**       Borrowed from MFC because this function is not public function
**
**   INPUT:  nIndex: button index to be set
**			pButton: button information to be set
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::_SetButton(int nIndex, TBBUTTON* pButton)
{
	// get original button state
	TBBUTTON button;
	VERIFY(DefWindowProc(TB_GETBUTTON, nIndex, (LPARAM)&button));

	// prepare for old/new button comparsion
	button.bReserved[0] = 0;
	button.bReserved[1] = 0;
	// TBSTATE_ENABLED == TBBS_DISABLED so invert it
	pButton->fsState ^= TBSTATE_ENABLED;
	pButton->bReserved[0] = 0;
	pButton->bReserved[1] = 0;

	// nothing to do if they are the same
	if (memcmp(pButton, &button, sizeof(TBBUTTON)) != 0)
	{
		// don't redraw everything while setting the button
		DWORD dwStyle = GetStyle();
		ModifyStyle(WS_VISIBLE, 0);
		VERIFY(DefWindowProc(TB_DELETEBUTTON, nIndex, 0));
		VERIFY(DefWindowProc(TB_INSERTBUTTON, nIndex, (LPARAM)pButton));
		ModifyStyle(0, dwStyle & WS_VISIBLE);

		// invalidate appropriate parts
		if (((pButton->fsStyle ^ button.fsStyle) & TBSTYLE_SEP) ||
			((pButton->fsStyle & TBSTYLE_SEP) && pButton->iBitmap != button.iBitmap))
		{
			// changing a separator
			Invalidate(FALSE);
		}
		else
		{
			// invalidate just the button
			CRect rect;
			SendMessage(TB_AUTOSIZE);
			if (DefWindowProc(TB_GETITEMRECT, nIndex, (LPARAM)&rect))
				InvalidateRect(rect, FALSE);    // don't erase background
		}
	}
}

/***********************************************************************
**
**   FUNCTION: OnNcCalcSize(BOOL bCalcValidRects, NCCALCSIZE_PARAMS* lpncsp)
**
**       Calculate the non-client area - adjusting for grippers
**
**   INPUT:  bCalcValidRects: Specifies whether the application should specify 
**							which part of the client area contains valid information. 
**							Windows will copy the valid information to the specified 
**							area within the new client area. If this parameter is TRUE, 
**							the application should specify which part of the client area is valid.
**			lpncsp: Points to a NCCALCSIZE_PARAMS data structure that contains information 
**						an application can use to calculate the new size and position of the CWnd rectangle 
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::OnNcCalcSize(BOOL bCalcValidRects, NCCALCSIZE_PARAMS* lpncsp)
{
    CToolBar::OnNcCalcSize(bCalcValidRects,lpncsp);
	if (m_standard_type)
		return;

     if (m_dwStyle & CBRS_FLOATING)
	 {          
/*
......no grippers
*/
     }
	 else if (m_dwStyle & CBRS_ORIENT_HORZ)
	 {
/*
......move 2 pixels right to make room
*/
          lpncsp->rgrc[0].left += BAR_GRIPPER - 4;
          lpncsp->rgrc[0].right += BAR_GRIPPER - 4;
          lpncsp->rgrc[0].top += 2;
          lpncsp->rgrc[0].bottom += 2;
	}
	else
	{          
/*
......move 4 pixels downto make room
*/
		lpncsp->rgrc[0].top += BAR_GRIPPER - 4;
		lpncsp->rgrc[0].bottom += BAR_GRIPPER - 4;     
	}
}

/***********************************************************************
**
**   FUNCTION: DrawGripper(CWindowDC *pDC, CRect& rectWindow)
**
**       Draw the gripper at left or top
**
**   INPUT:  pDC: toolbar device context 
**			rectWindow: toolbar rectangle
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::DrawGripper(CWindowDC *pDC, CRect& rectWindow)
{
	int x,y;
/*
......get the gripper rect (1 pixel smaller than toolbar)
*/
	CRect gripper = rectWindow;
	if (m_dwStyle & CBRS_FLOATING)
	{          
/*
......no grippers
*/
	}
	else if (m_dwStyle & CBRS_ORIENT_HORZ)
	{          
		x = BAR_GRIPPER - 8;
		gripper.DeflateRect(x,1);
/*
......gripper at left
*/
		gripper.right = gripper.left+3;
		gripper.bottom = gripper.bottom-1-BAR_GRIPPER;
		pDC->Draw3dRect(gripper,::GetSysColor(COLOR_3DHIGHLIGHT),::GetSysColor(COLOR_3DSHADOW));
		gripper.OffsetRect(+4,0);
		pDC->Draw3dRect(gripper,::GetSysColor(COLOR_3DHIGHLIGHT),::GetSysColor(COLOR_3DSHADOW));

		CRect closeRect;
		closeRect.top = rectWindow.bottom-1-BAR_GRIPPER;
		closeRect.bottom = BAR_GRIPPER + closeRect.top;
		closeRect.left = rectWindow.left + 1;
		closeRect.right = closeRect.left + BAR_GRIPPER;

		pDC->DrawFrameControl(closeRect,
			DFC_CAPTION,
			DFCS_CAPTIONCLOSE);
		m_rcClose = closeRect;
//		rectWindow.left += BAR_GRIPPER; 
	} 
	else
	{          
/*
......gripper at top
*/
		y = BAR_GRIPPER - 8;
		gripper.DeflateRect(1,y);

		gripper.bottom = gripper.top+3;
		gripper.right = rectWindow.right- 2 - BAR_GRIPPER;
		pDC->Draw3dRect(gripper,::GetSysColor(COLOR_3DHIGHLIGHT),::GetSysColor(COLOR_3DSHADOW));
		gripper.OffsetRect(0,+4);
		pDC->Draw3dRect(gripper,::GetSysColor(COLOR_3DHIGHLIGHT),::GetSysColor(COLOR_3DSHADOW));
		
		CRect closeRect;
		closeRect.top = rectWindow.top + 1;
		closeRect.bottom = BAR_GRIPPER + closeRect.top;
		closeRect.right = rectWindow.right - 1;
		closeRect.left = closeRect.right - BAR_GRIPPER;
		pDC->DrawFrameControl(closeRect,
			DFC_CAPTION,
			DFCS_CAPTIONCLOSE );
		m_rcClose = closeRect;
	}
}

/***********************************************************************
**
**   FUNCTION: EraseNonClient(BOOL bRaised)
**
**       Erase the non-client area (borders) - copied from MFC implementation
**
**   INPUT:   bRaised: not used here
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/

void CNCLToolBar::EraseNonClient(BOOL bRaised)
{
//get window DC that is clipped to the non-client area
	CWindowDC dc(this);
	CRect rectClient;
	GetClientRect(rectClient);
	CRect rectWindow;
	GetWindowRect(rectWindow);
	ScreenToClient(rectWindow);
	rectClient.OffsetRect(-rectWindow.left, -rectWindow.top);
	dc.ExcludeClipRect(rectClient);     
	rectWindow.OffsetRect(-rectWindow.left, -rectWindow.top);
	DrawBorders(&dc, rectWindow);     
	dc.IntersectClipRect(rectWindow);
	SendMessage(WM_ERASEBKGND, (WPARAM)dc.m_hDC);
	DrawGripper(&dc, rectWindow); 
}

/***********************************************************************
**
**   FUNCTION: DrawBorders(CDC* pDC, CRect& rect)
**
**       Drawing border (we don't want draw border for our menubar, so simply return)
**
**   INPUT:   pDC: toolbar device context 
**				rect: toolbar rectangle
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::DrawBorders(CDC* pDC, CRect& rect)
{
	if (m_standard_type)
		CToolBar::DrawBorders(pDC, rect);
	ASSERT_VALID(this);
	ASSERT_VALID(pDC);

	DWORD dwStyle = m_dwStyle;
	if (!(dwStyle & CBRS_BORDER_ANY))
		return;

	// prepare for dark lines
	ASSERT(rect.top == 0 && rect.left == 0);

	COLORREF clr=GetSysColor(COLOR_3DSHADOW);
	if(dwStyle&CBRS_BORDER_RIGHT)
		pDC->FillSolidRect(rect.right-1,0,rect.right,rect.bottom,clr); 
	if(dwStyle&CBRS_BORDER_BOTTOM)
		pDC->FillSolidRect(0,rect.bottom-1,rect.right,rect.bottom,clr); 

	clr=GetSysColor(COLOR_3DHIGHLIGHT);
	if(dwStyle&CBRS_BORDER_TOP)
		pDC->FillSolidRect(0,0,rect.right,1,clr); //top
	if(dwStyle&CBRS_BORDER_LEFT)
		pDC->FillSolidRect(0,0,1,rect.bottom,clr); //left

	if(dwStyle&CBRS_BORDER_TOP)
		rect.top++;
	if(dwStyle&CBRS_BORDER_RIGHT)
		rect.right--;
	if(dwStyle&CBRS_BORDER_BOTTOM)
		rect.bottom--;
	if(dwStyle&CBRS_BORDER_LEFT)
		rect.left++;
}

/***********************************************************************
**
**   FUNCTION: RepaintBackground()
**
**       Repaint Background
**
**   INPUT:   None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::RepaintBackground()
{
	CWnd* pParent = GetParent();
	if (pParent)
	{
/*
......get rect for this toolbar
*/
		CRect rw; GetWindowRect(&rw);
		CRect rc = rw; pParent->ScreenToClient(&rc);
		pParent->InvalidateRect(&rc);          
/*
......now do all the other toolbars (etc) that belong 
......to the parent
*/
		for (CWnd* pSibling = pParent->GetWindow(GW_CHILD); pSibling; pSibling = pSibling->GetNextWindow(GW_HWNDNEXT))
		{
/*
......but do not draw ourselves
*/
			if (pSibling == this) continue;
			CRect rc = rw; pSibling->ScreenToClient(&rc);
			pSibling->InvalidateRect(&rc);
		}
	}
}


/***********************************************************************
**
**   FUNCTION: OnUpdateCmdUI(CFrameWnd* pTarget, BOOL bDisableIfNoHndler)
**		This member function is called by the framework to update the status of the toolbar.
**       Because buttons are transparent, we need to repaint background if style changes
**
**   INPUT:   pTarget: Points to the main frame window of the application. This pointer is used for routing update messages.
**			bDisableIfNoHndler: Flag that indicates whether a control that has no update 
**							handler should be automatically displayed as disabled.
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::OnUpdateCmdUI(CFrameWnd* pTarget, BOOL bDisableIfNoHndler)
{
	static CUIntArray styles;
	int nIndexMax = (int)DefWindowProc(TB_BUTTONCOUNT, 0, 0);
	int nIndex;
	for (nIndex = 0; nIndex < nIndexMax; nIndex++)
	{
		UINT dwStyle = GetButtonStyle(nIndex);
		styles.SetAtGrow(nIndex,dwStyle);
	}
	CToolBar::OnUpdateCmdUI(pTarget,bDisableIfNoHndler);
	for (nIndex = 0; nIndex < nIndexMax; nIndex++)
	{
		UINT dwStyle = GetButtonStyle(nIndex);
		if (dwStyle & TBBS_DISABLED)
		{
		} 
		else if (dwStyle & TBBS_CHECKBOX)
		{
			UINT dwStyleWas = dwStyle;

			if (dwStyle & TBBS_CHECKED)
			{
				dwStyle |= TBBS_PRESSED;
			} 
			else if (!(styles[nIndex]&TBBS_CHECKED) && (styles[nIndex]&TBBS_PRESSED))
			{
				dwStyle |= TBBS_PRESSED;
			} 
			else
			{
				dwStyle &= ~TBBS_PRESSED;
			}

			if (dwStyleWas != dwStyle) SetButtonStyle(nIndex,dwStyle);
		}
	}
	for (nIndex = 0; nIndex < nIndexMax; nIndex++)
	{
		UINT dwStyle = GetButtonStyle(nIndex);
		if (styles[nIndex] != dwStyle)
		{
			Invalidate();
			break;
		}
	}
}

/***********************************************************************
**
**   FUNCTION: OnWindowPosChanging(LPWINDOWPOS lpwp)
**		This member function is called When toolbar position changed, 
**		Because buttons are transparent, we need to repaint background on size or move
**
**   INPUT:   lpwp: WINDOWPOS strcture
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::OnWindowPosChanging(LPWINDOWPOS lpwp)
{
	CToolBar::OnWindowPosChanging(lpwp);
	if (m_standard_type)
		return;
	RepaintBackground();
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
void CNCLToolBar::OnNcPaint()
{
	if (m_standard_type)
		return CToolBar::OnNcPaint();
   EraseNonClient(FALSE); 
}

/***********************************************************************
**
**   FUNCTION: OnNcHitTest(CPoint point) 
**
**       The framework calls this member function for the CWnd object that contains the cursor 
**			every time the mouse is moved.
**
**   INPUT:  
**			point: Contains the x- and y-coordinates of the cursor. These coordinates are always screen coordinates
**			
**   OUTPUT :   None
**   RETURN:    One of the mouse hit-test enumerated values listed below:
**				HTBORDER
**				HTBOTTOM
**				HTBOTTOMLEFT
**				HTBOTTOMRIGHT
**				HTCAPTION
**				HTCLIENT
**				HTERROR
**				HTGROWBOX
**				HTHSCROLL
**				HTLEFT
**				HTMAXBUTTON
**				HTMENU
**				HTMINBUTTON
**				HTNOWHERE
**				HTREDUCE
**				HTRIGHT
**				HTSIZE 
**				HTSYSMENU
**				HTTOP   
**				HTTOPLEFT
**				HTTOPRIGHT 
**				HTTRANSPARENT
**				HTVSCROLL
**				HTZOOM
**
**********************************************************************/
LRESULT CNCLToolBar::OnNcHitTest(CPoint point) 
{
	CPoint pt(point);
	CRect wRect;

	GetWindowRect(wRect);
	pt.x-=wRect.left;
	pt.y-=wRect.top;

	return CToolBar::OnNcHitTest(point);
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
c				DHT_CLOSE	in the title-bar 'X' close box area
c				DHT_HELP		in the title-bar '?' help box area
c				DHT_NCBAR   In a title-bar area.
c
**********************************************************************/
DWORD CNCLToolBar::HitTest(CPoint pt)
{
	CRect rect=m_rcClose;

	if(rect.PtInRect(pt))
		return (DWORD) DHT_CLOSE;
	else
		return (DWORD) DHT_NCBAR;
}


/***********************************************************************
c
c   FUNCTION: OnLButtonUp(UINT nFlags, CPoint point)
c			Callback function for mouse button up
c
c   INPUT:  nFlags: Indicates whether various virtual keys are down.
c					MK_CONTROL   Set if the CTRL key is down.
c					MK_MBUTTON   Set if the middle mouse button is down.
c					MK_RBUTTON   Set if the right mouse button is down.
c					MK_SHIFT   Set if the SHIFT key is down.
c			point:  Specifies the x- and y-coordinate of the cursor.
c
c   OUTPUT :   None.
c   RETURN:    None.
c
**********************************************************************/
void CNCLToolBar::OnLButtonUp(UINT nFlags, CPoint point) 
{
	if (m_standard_type)
		return CToolBar::OnLButtonUp(nFlags, point) ;

	m_StartPoint.x = -100;
	m_StartPoint.y = -100;	
	if(m_TimerID)
	{
		KillTimer(m_TimerID);
		m_TimerID = 0;
	}
	UW_current_menubar = NULL;
	if(this != GetCapture())
		goto done;
	if (m_pDockBar != NULL && OnToolHitTest(point, NULL) == -1)
	{	
		CRect wRect;
		GetWindowRect(wRect);
		ScreenToClient(&wRect);
		point.x-=wRect.left;
		point.y-=wRect.top;

		DWORD hitTest = HitTest(point);

		CWindowDC dc(this);

		DrawFrameControl(dc.m_hDC,
				m_rcClose,
				DFC_CAPTION,
				DFCS_CAPTIONCLOSE);

		switch(hitTest)	
		{
			case DHT_CLOSE:
				m_visible = 0;
				uw_nthide_menu(this);
				break;
			default:
				break;
		}
		ReleaseCapture();
	}
	else
	{
		CToolBar::OnLButtonUp(nFlags, point) ;
	}
done:;
}

/***********************************************************************
c
c   FUNCTION: OnNcActivate(BOOL bActive)
c			called when active no-client area of the dialog
c
c   INPUT:  bActive: not used here.
c
c   OUTPUT :   None.
c   RETURN:    True
c
**********************************************************************/
BOOL CNCLToolBar::OnNcActivate(BOOL bActive) 
{
	if (m_standard_type)
		return CToolBar::OnNcActivate(bActive) ;
   OnNcPaint(); 
   return TRUE; 
}

/***********************************************************************
**
**   FUNCTION: CalcFixedLayout(BOOL bStretch, BOOL bHorz)
**
**       Calculate toolbar's fixed layout
**
**   INPUT:  bStretch: Indicates whether the bar should be stretched to the size of the frame
**			bHorz: Indicates that the bar is horizontally or vertically oriented
**
**   OUTPUT :   None
**   RETURN:    layout size
**
**********************************************************************/
CSize CNCLToolBar::CalcFixedLayout(BOOL bStretch, BOOL bHorz)
{
	if (m_standard_type)
		return CToolBar::CalcFixedLayout(bStretch, bHorz);

	DWORD dwMode = bStretch ? LM_STRETCH : 0;
	dwMode |= bHorz ? LM_HORZ : 0;
	CSize s = CalcLayout(dwMode);

	if (m_dwStyle&CBRS_ORIENT_HORZ)
		s.cx += BAR_GRIPPER - 6;
	else
		s.cy += BAR_GRIPPER - 6;
	return s;
}

/***********************************************************************
**
**   FUNCTION: _GetButton(int nIndex, TBBUTTON* pButton) const
**
**       Borrow from MFC source. Make a little change.
**
**   INPUT:  
**			nIndex: index number of button in this toolbar
**			
**   OUTPUT :   pButton: Toolbar button information struct
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::_GetButton(int nIndex, TBBUTTON* pButton) const
{
	CNCLToolBar* pBar = (CNCLToolBar*)this;
	VERIFY(pBar->DefWindowProc(TB_GETBUTTON, nIndex, (LPARAM)pButton));
	pButton->fsState ^= TBSTATE_ENABLED;
}

#ifdef _MAC
	#define CX_OVERLAP  1
#else
	#define CX_OVERLAP  0
#endif

struct _AFX_CONTROLPOS
{
	int nIndex, nID;
	CRect rectOldPos;
};

/***********************************************************************
**
**   FUNCTION: WrapToolBar(TBBUTTON* pData, int nCount, int nWidth)
**
**       Borrow from MFC source with little change. Recalculate each button's m_sizeButton value.
**
**   INPUT:  pData: Toolbar button information struct
**			nCount: number of button in this toolbar
**			nWidth: Width of toolbar
**			
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
int CNCLToolBar::WrapToolBar(TBBUTTON* pData, int nCount, int nWidth)
{
	ASSERT(pData != NULL && nCount > 0);

	int nResult = 0;
	int x = 0;

	CSize sizeButton = CToolBar::m_sizeButton;				
	if ((m_dwStyle & CBRS_FLOATING) && (m_dwStyle & CBRS_SIZE_DYNAMIC))
		sizeButton.cy += 1;

	CPoint cur(0,0);
	CSize sizeResult(0,0);

	if ((m_dwMode & LM_MRUWIDTH)&&(nWidth==32767))
	{
		int rcount = m_nColumns;
		for (int i = 0; i < rcount; i++)
		{
			if (pData[i].fsState & TBSTATE_HIDDEN)
				continue;

			if (pData[i].fsStyle & TBSTYLE_SEP)
			{
/*
.....A separator represents either a height or width
*/
				if (pData[i].fsState & TBSTATE_WRAP)
/*
.....Add 3 points for separator in flat toolbar.
*/
					sizeResult.cy = max(cur.y + sizeButton.cy + pData[i].iBitmap * 2 / 3, sizeResult.cy) + 3;
				else
					sizeResult.cx = max(cur.x + pData[i].iBitmap, sizeResult.cx);
			}

			else if (pData[i].fsStyle & TBSTYLE_DROPDOWN)
			{
				sizeResult.cx = max(cur.x + m_sizeButton.cx + ARROW_X, sizeResult.cx);
				sizeResult.cy = max(cur.y + sizeButton.cy, sizeResult.cy);
			}
			else
			{
				sizeResult.cx = max(cur.x + m_sizeButton.cx, sizeResult.cx);
				sizeResult.cy = max(cur.y + sizeButton.cy, sizeResult.cy);
			}

			if (pData[i].fsStyle & TBSTYLE_SEP)
				cur.x += pData[i].iBitmap;
			else if (pData[i].fsStyle & TBSTYLE_DROPDOWN)
				cur.x += m_sizeButton.cx + ARROW_X - CX_OVERLAP;
			else
				cur.x += m_sizeButton.cx - CX_OVERLAP;
		}
		nWidth = sizeResult.cx + 1;
	}
	for (int i = 0; i < nCount; i++)
	{
		pData[i].fsState &= ~TBSTATE_WRAP;

		if (pData[i].fsState & TBSTATE_HIDDEN)
			continue;

		int dx, dxNext;
		if (pData[i].fsStyle & TBSTYLE_SEP)
		{
			if ((i>0)&&(pData[i-1].fsStyle & TBSTYLE_SEP))
				dx = 0;
			else
				dx = pData[i].iBitmap;
			dxNext = dx;
		}
		else if (pData[i].fsStyle & TBSTYLE_DROPDOWN)
		{
			dx = m_sizeButton.cx + ARROW_X;
			dxNext = dx - CX_OVERLAP;
		}
		else
		{
			dx = m_sizeButton.cx;
			dxNext = dx - CX_OVERLAP;
		}

		if (x + dx > nWidth)
		{
			BOOL bFound = FALSE;
			for (int j = i; j >= 0  &&  !(pData[j].fsState & TBSTATE_WRAP); j--)
			{
/*
......Find last separator that isn't hidden
......a separator that has a command ID is not
......a separator, but a custom control.
*/
				if ((pData[j].fsStyle & TBSTYLE_SEP) &&
					(pData[j].idCommand == 0) &&
					!(pData[j].fsState & TBSTATE_HIDDEN))
				{
					bFound = TRUE; i = j; x = 0;
					pData[j].fsState |= TBSTATE_WRAP;
					nResult++;
					break;
				}
			}
			if (!bFound)
			{
				for (int j = i - 1; j >= 0 && !(pData[j].fsState & TBSTATE_WRAP); j--)
				{
/*
......Never wrap anything that is hidden,
......or any custom controls
*/
					if ((pData[j].fsState & TBSTATE_HIDDEN) ||
						((pData[j].fsStyle & TBSTYLE_SEP) &&
						(pData[j].idCommand != 0)))
						continue;

					bFound = TRUE; i = j; x = 0;
					pData[j].fsState |= TBSTATE_WRAP;
					nResult++;
					break;
				}
				if (!bFound)
					x += dxNext;
			}
		}
		else
			x += dxNext;
	}
	return nResult + 1;
}

/***********************************************************************
**
**   FUNCTION: SizeToolBar(TBBUTTON* pData, int nCount, int nLength, BOOL bVert)
**
**       Must borrow from MFC source, because this function is not virtual.
**		If not borrow, the new CalcSize function won't be called.
**
**   INPUT:  pData: Toolbar button information struct
**			nCount: number of button in this toolbar
**			nLength: The requested dimension of the control bar, either horizontal or vertical
**			bVert: horizontal or vertical
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::SizeToolBar(TBBUTTON* pData, int nCount, int nLength, BOOL bVert)
{
	ASSERT(pData != NULL && nCount > 0);

	if (!bVert)
	{
		int nMin, nMax, nTarget, nCurrent, nMid;

		// Wrap ToolBar as specified
		nMax = nLength;
		nTarget = WrapToolBar(pData, nCount, nMax);

		// Wrap ToolBar vertically

		nMin = 0;
		nCurrent = WrapToolBar(pData, nCount, nMin);

		if (nCurrent != nTarget)
		{
			while (nMin < nMax)
			{
				nMid = (nMin + nMax) / 2;
				nCurrent = WrapToolBar(pData, nCount, nMid);

				if (nCurrent == nTarget)
					nMax = nMid;
				else
				{
					if (nMin == nMid)
					{
						WrapToolBar(pData, nCount, nMax);
						break;
					}
					nMin = nMid;
				}
			}
		}
		CSize size = CalcSize(pData, nCount);
		m_row = WrapToolBar(pData, nCount, size.cx);
	}
	else
	{
		CSize sizeMax, sizeMin, sizeMid, old_sizeMid;
		int old_row;
/*
......Wrap ToolBar vertically
*/
		m_row = WrapToolBar(pData, nCount, 0);
		sizeMin = CalcSize(pData, nCount);
/*
......Wrap ToolBar horizontally
*/
		m_row = WrapToolBar(pData, nCount, 32767);
		sizeMax = CalcSize(pData, nCount);
		while (sizeMin.cx < sizeMax.cx)
		{
			old_sizeMid = sizeMid;
			old_row = m_row;
			sizeMid.cx = (sizeMin.cx + sizeMax.cx) / 2;
			m_row =WrapToolBar(pData, nCount, sizeMid.cx);
			sizeMid = CalcSize(pData, nCount);

			if (nLength < sizeMid.cy)
			{
				if (sizeMin == sizeMid)
				{
					m_row =WrapToolBar(pData, nCount, sizeMax.cx);
					return;
				}
				sizeMin = sizeMid;
				if ((old_sizeMid.cx == sizeMid.cx)&&(old_row==m_row))
				{
					m_row =WrapToolBar(pData, nCount, sizeMax.cx);
					return;
				}
			}
			else if (nLength > sizeMid.cy)
			{
				if (sizeMax == sizeMid)
				{
					m_row =WrapToolBar(pData, nCount, sizeMin.cx);
					return;
				}
				sizeMax = sizeMid;
				if ((old_sizeMid.cx == sizeMid.cx)&&(old_row==m_row))
				{
					m_row =WrapToolBar(pData, nCount, sizeMin.cx);
					return;
				}
			}
			else
				return;
		}
	}
}

/***********************************************************************
**
**   FUNCTION: CalcSize(TBBUTTON* pData, int nCount)
**
**       Calculate toolbar's size 
**
**   INPUT:  pData: Toolbar button information struct
**			nCount: number of button in this toolbar
**
**   OUTPUT :   None
**   RETURN:    toolbar size
**
**********************************************************************/
CSize CNCLToolBar::CalcSize(TBBUTTON* pData, int nCount)
{
	ASSERT(pData != NULL && nCount > 0);

	CPoint cur(0,0);
	CSize sizeResult(0,0);

	CSize sizeButton = CToolBar::m_sizeButton;
	if ((m_dwStyle & CBRS_FLOATING) && (m_dwStyle & CBRS_SIZE_DYNAMIC))
		sizeButton.cy += 1;

	for (int i = 0; i < nCount; i++)
	{
		if (pData[i].fsState & TBSTATE_HIDDEN)
			continue;

		if (pData[i].fsStyle & TBSTYLE_SEP)
		{
/*
.....A separator represents either a height or width
*/
			if (pData[i].fsState & TBSTATE_WRAP)
			{
/*
.....Add 3 points for separator in flat toolbar.
*/
				sizeResult.cy = max(cur.y + sizeButton.cy + pData[i].iBitmap * 2 / 3, sizeResult.cy) + 3;
			}
			else
			{
				sizeResult.cx = max(cur.x + pData[i].iBitmap, sizeResult.cx);
				sizeResult.cy = sizeButton.cy;
			}
		}
		else if (pData[i].fsStyle & TBSTYLE_DROPDOWN)
		{
			sizeResult.cx = max(cur.x + m_sizeButton.cx + ARROW_X, sizeResult.cx);
			sizeResult.cy = max(cur.y + sizeButton.cy, sizeResult.cy);
		}
		else
		{
			sizeResult.cx = max(cur.x + m_sizeButton.cx, sizeResult.cx);
			sizeResult.cy = max(cur.y + sizeButton.cy, sizeResult.cy);
		}

		if (pData[i].fsStyle & TBSTYLE_SEP)
			cur.x += pData[i].iBitmap;
		else if (pData[i].fsStyle & TBSTYLE_DROPDOWN)
			cur.x += m_sizeButton.cx + ARROW_X - CX_OVERLAP;
		else
			cur.x += m_sizeButton.cx - CX_OVERLAP;

		if (pData[i].fsState & TBSTATE_WRAP)
		{
			cur.x = 0;
			cur.y += sizeButton.cy;
			if (pData[i].fsStyle & TBSTYLE_SEP)
				cur.y += pData[i].iBitmap * 2 / 3 + 3;
		}
	}
	return sizeResult;
}
/***********************************************************************
**
**   FUNCTION: CalcLayout(DWORD dwMode, int nLength)
**
**       Calculate toolbar's dynamic layout
**
**   INPUT:  nLength: The requested dimension of the control bar, either horizontal or vertical, depending on dwMode
**			dwMode: Layout mode flags
**
**   OUTPUT :   None
**   RETURN:    layout size
**
**********************************************************************/
CSize CNCLToolBar::CalcLayout(DWORD dwMode, int nLength)
{
	ASSERT_VALID(this);
	ASSERT(::IsWindow(m_hWnd));
	if (dwMode & LM_HORZDOCK)
		ASSERT(dwMode & LM_HORZ);

	m_dwMode = dwMode;
	int nCount;
	TBBUTTON* pData;
	CSize sizeResult(0,0);

/*
......Load Buttons
*/
	{
		nCount = DefWindowProc(TB_BUTTONCOUNT, 0, 0);
		if (nCount != 0)
		{
			int i;
			pData = new TBBUTTON[nCount];
			for (i = 0; i < nCount; i++)
				_GetButton(i, &pData[i]);
		}
	}

	if (nCount > 0)
	{
		if (!(m_dwStyle & CBRS_SIZE_FIXED))
		{
			BOOL bDynamic = m_dwStyle & CBRS_SIZE_DYNAMIC;

			if (bDynamic && (dwMode & LM_MRUWIDTH))
				SizeToolBar(pData, nCount, m_nMRUWidth);
			else if (bDynamic && (dwMode & LM_HORZDOCK))
				SizeToolBar(pData, nCount, 32767);
			else if (bDynamic && (dwMode & LM_VERTDOCK))
				SizeToolBar(pData, nCount, 0);
			else if (bDynamic && (nLength != -1))
			{
				CRect rect; rect.SetRectEmpty();
				CalcInsideRect(rect, (dwMode & LM_HORZ));
				BOOL bVert = (dwMode & LM_LENGTHY);
				int nLen = nLength + (bVert ? rect.Height() : rect.Width());

				SizeToolBar(pData, nCount, nLen, bVert);
			}
			else if (bDynamic && (m_dwStyle & CBRS_FLOATING))
				SizeToolBar(pData, nCount, m_nMRUWidth);
			else
				SizeToolBar(pData, nCount, (dwMode & LM_HORZ) ? 32767 : 0);
		}
/*
.....MFC will not call following for 
.....CBRS_SIZE_FIXED because it has fixed size
.....but for status bar (which use m_bartype==1)
.....we want change size while dragging from floating->docking or
.....docking -> floating but only for dragging
*/
		else if ((m_bartype==1)
				&& (m_StartDrag == 1))
		{
			if (dwMode & LM_MRUWIDTH)
				SizeToolBar(pData, nCount, m_nMRUWidth);
			else if (dwMode & LM_HORZDOCK)
				SizeToolBar(pData, nCount, 32767);
			else if (dwMode & LM_VERTDOCK)
				SizeToolBar(pData, nCount, 0);
			else if (nLength != -1)
			{
				CRect rect; rect.SetRectEmpty();
				CalcInsideRect(rect, (dwMode & LM_HORZ));
				BOOL bVert = (dwMode & LM_LENGTHY);
				int nLen = nLength + (bVert ? rect.Height() : rect.Width());

				SizeToolBar(pData, nCount, nLen, bVert);
			}
			else if (m_dwStyle & CBRS_FLOATING)
				SizeToolBar(pData, nCount, m_nMRUWidth);
			else
				SizeToolBar(pData, nCount, (dwMode & LM_HORZ) ? 32767 : 0);
		}

		sizeResult = CalcSize(pData, nCount);

		if (dwMode & LM_COMMIT)
		{
			_AFX_CONTROLPOS* pControl = NULL;
			int nControlCount = 0;
			BOOL bIsDelayed = m_bDelayedButtonLayout;
			m_bDelayedButtonLayout = FALSE;

			for(int i = 0; i < nCount; i++)
				if ((pData[i].fsStyle & TBSTYLE_SEP) && (pData[i].idCommand != 0))
					nControlCount++;

			if (nControlCount > 0)
			{
				pControl = new _AFX_CONTROLPOS[nControlCount];
				nControlCount = 0;

				for(int i = 0; i < nCount; i++)
				{
					if ((pData[i].fsStyle & TBSTYLE_SEP) && (pData[i].idCommand != 0))
					{
						pControl[nControlCount].nIndex = i;
						pControl[nControlCount].nID = pData[i].idCommand;

						CRect rect;
						GetItemRect(i, &rect);
						ClientToScreen(&rect);
						pControl[nControlCount].rectOldPos = rect;

						nControlCount++;
					}
				}
			}

			if ((m_dwStyle & CBRS_FLOATING) && (m_dwStyle & CBRS_SIZE_DYNAMIC))
				m_nMRUWidth = sizeResult.cx;
/*
.....we don't want change button
.....for status bar 
*/
			if (m_bartype!=1)
			{
			CRect rect;
			SendMessage(TB_AUTOSIZE);
			DefWindowProc(TB_GETITEMRECT, 0, (LPARAM)&rect);

				for (int i = 0; i < nCount; i++)
					_SetButton(i, &pData[i]);
			}
			if (nControlCount > 0)
			{
				for (int i = 0; i < nControlCount; i++)
				{
					CWnd* pWnd = GetDlgItem(pControl[i].nID);
					if (pWnd != NULL)
					{
						CRect rect;
						pWnd->GetWindowRect(&rect);
						CPoint pt = rect.TopLeft() - pControl[i].rectOldPos.TopLeft();
						GetItemRect(pControl[i].nIndex, &rect);
						pt = rect.TopLeft() + pt;
						pWnd->SetWindowPos(NULL, pt.x, pt.y, 0, 0, SWP_NOACTIVATE | SWP_NOSIZE | SWP_NOZORDER);
					}
				}
				delete[] pControl;
			}
			m_bDelayedButtonLayout = bIsDelayed;
		}
		delete[] pData;
	}

/*
......Adjust Margins
*/
	{
		CRect rect; rect.SetRectEmpty();
		CalcInsideRect(rect, (dwMode & LM_HORZ));
		sizeResult.cy -= rect.Height();
		sizeResult.cx -= rect.Width();
		
		if ((m_dwStyle & CBRS_FLOATING)	&& (!(m_dwStyle & CBRS_SIZE_FIXED)))
		{
			sizeResult.cy -= 1*m_row;
		}

		CSize size = CControlBar::CalcFixedLayout((dwMode & LM_STRETCH), (dwMode & LM_HORZ));
		sizeResult.cx = max(sizeResult.cx, size.cx);
		sizeResult.cy = max(sizeResult.cy, size.cy);
	}
	return sizeResult;
}
/***********************************************************************
**
**   FUNCTION: CalcDynamicLayout(int nLength, DWORD dwMode)
**
**       Calculate toolbar's dynamic layout
**
**   INPUT:  nLength: The requested dimension of the control bar, either horizontal or vertical, depending on dwMode
**			dwMode: Layout mode flags
**
**   OUTPUT :   None
**   RETURN:    layout size
**
**********************************************************************/
CSize CNCLToolBar::CalcDynamicLayout(int nLength, DWORD dwMode)
{
	if (m_standard_type)
		return CToolBar::CalcDynamicLayout(nLength, dwMode);
	CSize s; 
	m_dwMode = dwMode;
	if ((nLength == -1) && !(dwMode & LM_MRUWIDTH) && !(dwMode & LM_COMMIT) &&
		((dwMode & LM_HORZDOCK) || (dwMode & LM_VERTDOCK)))
		s = CalcFixedLayout(dwMode & LM_STRETCH, dwMode & LM_HORZDOCK);
	else
		s = CalcLayout(dwMode, nLength);

	if(m_TargetDrop)
		m_TargetDrop->Register(this, CF_TEXT);

	return s;
}

/***********************************************************************
**
**   FUNCTION: SetSizes(SIZE sizeButton, SIZE sizeImage)
**
**       Set Toolbar button and image size - Mostly copy from MFC
**				but we want to allow Image size = 0
**
**   INPUT:  sizeButton: toolbar button size to set
**			sizeImage: toobar Image size to be set
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::SetSizes(SIZE sizeButton, SIZE sizeImage)
{
	ASSERT_VALID(this);

	// sizes must be non-zero and positive
	ASSERT(sizeButton.cx > 0 && sizeButton.cy > 0);

	// button must be big enough to hold image
	//   + 7 pixels on x
	//   + 6 pixels on y
	ASSERT(sizeButton.cx >= sizeImage.cx + 7);
	ASSERT(sizeButton.cy >= sizeImage.cy + 6);

	if (::IsWindow(m_hWnd))
	{
		// set the sizes via TB_SETBITMAPSIZE and TB_SETBUTTONSIZE
		VERIFY(SendMessage(TB_SETBITMAPSIZE, 0, MAKELONG(sizeImage.cx, sizeImage.cy)));
		VERIFY(SendMessage(TB_SETBUTTONSIZE, 0, MAKELONG(sizeButton.cx, sizeButton.cy)));

		Invalidate();   
	}
	else
	{
		m_sizeButton = sizeButton;
		m_sizeImage = sizeImage;
	}
}


/***********************************************************************
**
**   FUNCTION: EnableDocking(DWORD dwDockStyle)
**
**       Enable toolbar docking
**		This function is overwrite CToolbar::EnableDocking with little change
**
**   INPUT:  dwDockStyle: toolbar docking style allowed
**			
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::EnableDocking(DWORD dwDockStyle)
{
	if (m_standard_type)
		return CToolBar::EnableDocking(dwDockStyle);
	// must be CBRS_ALIGN_XXX or CBRS_FLOAT_MULTI only
	ASSERT((dwDockStyle & ~(CBRS_ALIGN_ANY|CBRS_FLOAT_MULTI)) == 0);
	// CBRS_SIZE_DYNAMIC toolbar cannot have the CBRS_FLOAT_MULTI style
	ASSERT(((dwDockStyle & CBRS_FLOAT_MULTI) == 0) || ((m_dwStyle & CBRS_SIZE_DYNAMIC) == 0));

	m_dwDockStyle = dwDockStyle;
	if (m_pDockContext == NULL)
		m_pDockContext = new CNCLDockContext(this);

	// permanently wire the bar's owner to its current parent
	if (m_hWndOwner == NULL)
		m_hWndOwner = ::GetParent(m_hWnd);
}


/***********************************************************************
**
**   FUNCTION: OnLButtonDown(UINT nFlags, CPoint pt)
**
**       Left mouse button down callback
**		This function is same as CToolbar::OnLButtonDown
**		we overwrite just for call our own 
**		m_pDockContext->StartDrag function
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::OnLButtonDown(UINT nFlags, CPoint pt)
{
	if (m_standard_type)
		return CToolBar::OnLButtonDown(nFlags, pt);
	int i;
	TOOLINFO pTI;
	pTI.cbSize = sizeof(TOOLINFO);
	// only start dragging if clicked in "void" space
	if (m_pDockBar != NULL && OnToolHitTest(pt, NULL) == -1)
	{
/*
.....if on 'Close' button
*/
		CRect wRect;
		CPoint point = pt;
		GetWindowRect(wRect);
		ScreenToClient(&wRect);
		point.x-=wRect.left;
		point.y-=wRect.top;
		DWORD hitTest = HitTest(point);
		if (hitTest==DHT_CLOSE)
		{
		    CWindowDC dc(this);
			DrawFrameControl(dc.m_hDC,
			m_rcClose,
			DFC_CAPTION,
			DFCS_CAPTIONCLOSE | DFCS_PUSHED);
			SetCapture();
			return;
		}
		else
		{
			ASSERT(m_pDockContext != NULL);
			ClientToScreen(&pt);
			m_pDockContext->StartDrag(pt);
		}
	}
	else if (m_pDockBar != NULL && OnToolHitTest(pt, &pTI) != -1)
//	else if (m_pDockBar != NULL && OnToolHitTest(pt, NULL) != -1)
	{
/*
.....on the button
*/
		m_changed = 1;
/*
......menubar only, no statusbar
*/
//statusbar also now yurong
//		if (m_bartype==0)
		{
			m_StartPoint = 	pt;
			m_hitRect = pTI.rect;
			for (i=0; i<GetCount();i++)
			{
				if (pTI.uId==GetItemID(i))
				{
					m_hititem = i;
					break;
				}
			}
			m_TimerID = SetTimer(1, 200, NULL);
			UW_current_menubar = this;

/*			m_StartPoint = 	pt;
			m_hititem = -1;
			status = GetItemIndx_hit2(pt, &m_hititem);
			if (status==-1)
			{
				m_hititem = -1;
				CWnd::OnLButtonDown(nFlags, pt);
				return;
			}
			m_TimerID = SetTimer(1, 200, NULL);
*/
		}
		CWnd::OnLButtonDown(nFlags, pt);
	}
	else
	{
		CWnd::OnLButtonDown(nFlags, pt);
	}
}

/***********************************************************************
**
**   FUNCTION: OnLButtonDblClk(UINT nFlags, CPoint pt)
**
**       Left mouse button double click callback
**		This function is same as CToolbar::OnLButtonDown
**		we overwrite just for call our own 
**		m_pDockContext->ToggleDocking function
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::OnLButtonDblClk(UINT nFlags, CPoint pt)
{
	if (m_standard_type)
		return CToolBar::OnLButtonDblClk(nFlags, pt);

	// only toggle docking if clicked in "void" space
	if (m_pDockBar != NULL && OnToolHitTest(pt, NULL) == -1)
	{
/*
.....if on 'Close' button
*/
		CRect wRect;
		CPoint point = pt;
		GetWindowRect(wRect);
		ScreenToClient(&wRect);
		point.x-=wRect.left;
		point.y-=wRect.top;
		DWORD hitTest = HitTest(point);
		if (hitTest==DHT_CLOSE)
		{
		    CWindowDC dc(this);
			DrawFrameControl(dc.m_hDC,
			m_rcClose,
			DFC_CAPTION,
			DFCS_CAPTIONCLOSE | DFCS_PUSHED);
			SetCapture();
			return;
		}
		else
		{
			// start the drag
			ASSERT(m_pDockContext != NULL);
			m_pDockContext->ToggleDocking();
		}
	}
	else
	{
		CWnd::OnLButtonDblClk(nFlags, pt);
	}
}


/***********************************************************************
**
**   FUNCTION: OnPaint()
**
**       Toolbar paint function
**
**   INPUT:  None
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::OnPaint() 
{
	if (m_standard_type)
		return CToolBar::OnPaint();
	if (m_bDelayedButtonLayout)
	{
		Layout();
/*
....force to set size for toolbar
....Yurong
*/
		SIZE bsize;
		bsize.cx = -1;
		bsize.cy = -1;
		SetButSizes(bsize);
	}
	CPaintDC dc(this); // device context for paintin
	DefWindowProc(WM_PAINT,WPARAM(dc.m_hDC),0);

	// Separators hiding
	HideSeparators(&dc);
}

/***********************************************************************
**
**   FUNCTION: GetBackgroundBrush()
**
**       Returns a pointer to a CBrush object of a window background brush 
**
**   INPUT:  None
**   OUTPUT :   None
**   RETURN:    Background Brush
**
**********************************************************************/
CBrush* CNCLToolBar::GetBackgroundBrush()
{
	if(!::IsWindow(GetSafeHwnd()))
		return NULL;

	TCHAR szClassName[1024] = {NULL};

	WNDCLASSEX WndClassEx;
	ZeroMemory(&WndClassEx,sizeof(WndClassEx));
	WndClassEx.cbSize = sizeof(WndClassEx);

	if(::GetClassName(GetSafeHwnd(), 
					  szClassName, 
					  sizeof(szClassName)/sizeof(TCHAR)) &&
	   ::GetClassInfoEx(::AfxGetApp()->m_hInstance,
						szClassName,
						&WndClassEx))
		return CBrush::FromHandle(WndClassEx.hbrBackground);

	return NULL;
}
/***********************************************************************
**
**   FUNCTION: OnEraseBkgnd(CDC* pDC) 
**			The framework calls this member function 
**			when the CNCLView object background needs 
**			erasing (for example, when resized). 
**		    It is called to prepare an invalidated 
**			region for painting.
**		    Because we don't don't window erase the 
**			graphic for us, we will erase it in openGL
**			function, so simply return 0;
**   
**	 INPUT:  CDC* pDC: device context
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLToolBar::OnEraseBkgnd(CDC* pDC) 
{
	if (m_standard_type)
		return CToolBar::OnEraseBkgnd(pDC);
	CRect rect;
	GetClientRect(&rect);

	CBrush* pFillBrush = GetBackgroundBrush();
	if(!pFillBrush)
		return true;

	CBrush *old = pDC->SelectObject(pFillBrush);
	BOOL ret = pDC->PatBlt(0,0,rect.Width(), rect.Height(), PATCOPY);
	pDC->SelectObject(old);
	return ret;
}
/***********************************************************************
**
**   FUNCTION: HideSeparators(CDC *pDC)
**
**       Hide the seperator behide a control or aside of the toolbar button
**		when it should display down of the button
**
**   INPUT:  None
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::HideSeparators(CDC *pDC)
{
	if(!::IsWindow(GetSafeHwnd()))
		return;

	CBrush* pFillBrush = GetBackgroundBrush();
	if(!pFillBrush)
		return;

	CRect ClipBox(0,0,0,0);
	pDC->GetClipBox(ClipBox);
	if(ClipBox.IsRectEmpty())
		return;

	CRect FillRect;
	int nToolBarItemCount = GetCount();

	CRect rectWindow;
	GetWindowRect(rectWindow);
	ScreenToClient(rectWindow);
	rectWindow.OffsetRect(-rectWindow.left, -rectWindow.top);

	for(int nI=1;nI<nToolBarItemCount;nI++)
	{
		UINT nID = 0;
		UINT nStyle = 0;
		int savid, iImage = 0;

		GetButtonInfo(nI,nID,nStyle,iImage);

		savid = nID;
		if (nID!=ID_SEPARATOR)
			continue;
		if ((nStyle&TBBS_WRAPPED)==0)
			return;
		CRect ItemRect;
		GetItemRect(nI,ItemRect);
		if (m_sizeButton.cx>ItemRect.right - ItemRect.left)
		{
			if (savid==ID_SEPARATOR)
				continue;
			GetItemRect(nI-1,ItemRect);
			FillRect.top = ItemRect.top;
			FillRect.bottom = ItemRect.bottom;
			FillRect.left = ItemRect.right;
			FillRect.right = rectWindow.right;
			pDC->FillRect(FillRect,pFillBrush);
		}
	}
	return;
}
/***********************************************************************
**
**   FUNCTION: SetWraped(int wrap)
**
**        Wrap/Unwrap toolbar 
**
**   INPUT:  wrap: 1: wraped by using m_nColumns
**					2: wraped every button (one col)
**					0: not wraped
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolBar::SetWraped(int wrap)
{
	int i, nCount;
	UINT nStyle;
	BOOL bWrap;
	if (wrap==1)
	{
		nCount = GetToolBarCtrl().GetButtonCount();
		for (i = 0; i < nCount; i++)
		{
			nStyle = GetButtonStyle(i);
			bWrap = (((i + 1) % m_nColumns) == 0);
			if (bWrap)
				nStyle |= TBBS_WRAPPED;
			else
				nStyle &= ~TBBS_WRAPPED;
			SetButtonStyle(i, nStyle);
		}
		Invalidate();
	}
	else if (wrap==2)
	{
		nCount = GetToolBarCtrl().GetButtonCount();
		for (i = 0; i < nCount; i++)
		{
			nStyle = GetButtonStyle(i);
			nStyle |= TBBS_WRAPPED;
			SetButtonStyle(i, nStyle);
		}
		Invalidate();
	}
	else
	{
		nCount = GetToolBarCtrl().GetButtonCount();
		for (i = 0; i < nCount; i++)
		{
			nStyle = GetButtonStyle(i);
			nStyle &= ~TBBS_WRAPPED;
			SetButtonStyle(i, nStyle);
		}
		Invalidate();
	}
	SetSizes(m_sizeButton, m_sizeImage);
}

/***********************************************************************
**
**   FUNCTION: SetDefFloatWidth()
**
**        Set default floating toolbar width
**
**   INPUT:  None
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/

void CNCLToolBar::SetDefFloatWidth()
{
	int nToolBarItemCount = GetCount();
	for(int nI=0;nI<nToolBarItemCount;nI++)
	{
		UINT nID = 0;
		UINT nStyle = 0;
		int iImage = 0;

		GetButtonInfo(nI,nID,nStyle,iImage);

		if (nStyle&TBBS_WRAPPED)
		{
			m_nMRUWidth = m_sizeButton.cx * (nI+1);
			return;
		}
	}
}

/***********************************************************************
**
**   FUNCTION: SetDefFloatWidth(int cols)
**
**        Set default floating toolbar width as cols
**
**   INPUT:  cols: cols to default floating toolbar
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/

void CNCLToolBar::SetDefFloatWidth(int cols)
{
	m_nMRUWidth = m_sizeButton.cx * cols;
	m_bDelayedButtonLayout=TRUE;
}

/***********************************************************************
**
**   FUNCTION: GetCols()
**
**        return the toolbar cols per row
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    toolbar cols per row
**
**********************************************************************/

int CNCLToolBar::GetCols()
{
	int nToolBarItemCount = GetCount();
	for(int nI=0;nI<nToolBarItemCount;nI++)
	{
		UINT nID = 0;
		UINT nStyle = 0;
		int iImage = 0;

		GetButtonInfo(nI,nID,nStyle,iImage);

		if (nStyle&TBBS_WRAPPED)
		{
			return (nI+1);
		}
	}
	return nToolBarItemCount;
}

/***********************************************************************
**
**   FUNCTION: SetButSizes(SIZE bsize)
**
**        Set the Buttton Sizes
**
**   INPUT:  size: size to be set
**
**   OUTPUT :   None
**   RETURN:    None.
**
**********************************************************************/
void CNCLToolBar::SetButSizes(SIZE bsize)
{
	if (bsize.cx<=0)
	{
		bsize.cx = m_sizeButton.cx;
	}
	if (bsize.cy<=0)
	{
		bsize.cy = m_sizeButton.cy;
	}
	SetSizes(bsize, m_sizeImage);
}

/***********************************************************************
c
c   FUNCTION: OnMouseMove(UINT nFlags, CPoint point) 
c
c       The framework calls this member function 
c			when the mouse cursor moves
c
c   INPUT:  nFlags: Indicates whether various virtual keys are Down
c			point:  Specifies the x- and y-coordinate of the cursor. 
c					These coordinates are always relative to the 
c					upper-left corner of the window.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLToolBar::OnMouseMove(UINT nFlags, CPoint point) 
{
	if (m_standard_type)
		return CToolBar::OnMouseMove(nFlags, point);
	char menudata[100];
	if ((m_TimerID > 0)&&(m_barnum>=0))
	{
		//	check if we really moved enough
		int iX = m_StartPoint.x - point.x;
		int iY = m_StartPoint.y - point.y;
		if((iX*iX + iY*iY) > 100)
		{
			m_StartPoint.x = -100;
			m_StartPoint.y = -100;
			if(m_TimerID)
			{
				KillTimer(m_TimerID);
				m_TimerID = 0;
				UW_current_menubar = NULL;
			}
			if (m_hititem==-1)
				return;
			COleDataSource*	pSource = new COleDataSource();
			if(pSource)
			{
				CSharedFile	sf(GMEM_MOVEABLE|GMEM_DDESHARE|GMEM_ZEROINIT);
				CString iText;
				if (m_bartype==0)
					sprintf_s(menudata, 100, "CToolmenu %d, %d", m_barnum, m_hititem);
				else
					sprintf_s(menudata, 100, "CToolmenu2 %d, %d", m_barnum, m_hititem);

				iText = menudata;

				//	write name to clipboard
				sf.Write(iText, iText.GetLength());

				HGLOBAL hMem = sf.Detach();
				if (!hMem) 
					return;
				pSource->CacheGlobalData(CF_TEXT, hMem);

				if (m_bartype==1)
					UW_drag_obj = 1;
				else
					UW_drag_obj = 0;
				//	Do drag and drop!
				pSource->DoDragDrop();

				//	free source
				delete pSource;
				UW_drag_obj = -1;
			}
		}
	}
	CToolBar::OnMouseMove(nFlags, point);
}
/***********************************************************************
c
c   FUNCTION: CheckClosestBut(CPoint point, int &itemnum, int &pos_flag)
c
c       check the 'point' is at the button
c
c   INPUT:  point: mouse position
c
c   OUTPUT :   itemnum: the closest item number the point at
c				pos_flag: 1 position is at the right-bottom of the closest button
c   RETURN:    None
c
**********************************************************************/
void CNCLToolBar::CheckClosestBut(CPoint point, int &itemnum, int &pos_flag)
{
	CPoint pt=point;
	ScreenToClient(&pt);

	CNCLToolBar* pBar = (CNCLToolBar*)this;
	int nButtons = (int)pBar->DefWindowProc(TB_BUTTONCOUNT, 0, 0);
	itemnum = 0;
	pos_flag = 0;
	CRect rect, rect1;
	for (int i = 0; i < nButtons; i++)
	{
		if (pBar->DefWindowProc(TB_GETITEMRECT, i, (LPARAM)&rect))
		{
			++rect.bottom;
			++rect.right;
			if (rect.PtInRect(pt))
			{
				itemnum = i;
/*
......we need check out if the pt is at the right-bottom position of the button
*/
				rect1.top = rect.top + (long)(rect.Height()/2.0);
				rect1.bottom = rect.bottom;
				rect1.left = rect.left + (long)(rect.Width()/2.0);
				rect1.right = rect.right;
				if (rect1.PtInRect(pt))
					pos_flag = 1;
				break;
			}
			else
			{
				if (pt.y <= rect.top)
				{
					if (pt.x<rect.right)
					{
						itemnum = i - 1;
						if (itemnum==-1) itemnum = 0;
						break;
					}
				}
				else
					itemnum = i;
			}
		}
	}
}

void CNCLToolBar::reset_timer()
{
	m_StartPoint.x = -100;
	m_StartPoint.y = -100;
	if(m_TimerID)
	{
		KillTimer(m_TimerID);
		m_TimerID = 0;
		UW_current_menubar = NULL;
	}
}
