/************************************************************************
**
**   FILE NAME: wsntpalettewnd.cpp
**
**	 Description - Functions implementation for
**		CNCLPaletteWnd class 
**	 CONTAINS: 
**		all functions declared in wsntpalettewnd.h
**
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntpalettewnd.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:29
**
************************************************************************
*/
#include "stdafx.h"
#include "wsntpalettewnd.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CNCLPaletteWnd


CNCLPaletteWnd::CNCLPaletteWnd()
{
	m_pColorControl = NULL;
	m_pNotifyWnd = NULL;
}

CNCLPaletteWnd::~CNCLPaletteWnd()
{
	if(m_pColorControl != NULL)
		delete m_pColorControl;
}


BEGIN_MESSAGE_MAP(CNCLPaletteWnd, CWnd)
//{{AFX_MSG_MAP(CNCLPaletteWnd)
ON_WM_ERASEBKGND()
ON_WM_LBUTTONDOWN()
ON_WM_LBUTTONUP()
ON_WM_DESTROY()
ON_WM_PAINT()
ON_WM_RBUTTONDOWN()
ON_WM_RBUTTONUP()
ON_WM_CANCELMODE()
ON_WM_KEYDOWN()
ON_WM_SIZE()
ON_WM_SYSCOLORCHANGE()
ON_WM_GETDLGCODE()	
ON_WM_KILLFOCUS()
ON_WM_SETFOCUS()

	//}}AFX_MSG_MAP
ON_MESSAGE(WM_NCL_SELECTCOLOROK, OnSelectColorOK)
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CNCLPaletteWnd message handlers
/***********************************************************************
c
c   SUBROUTINE:  Create(DWORD dwStyle,CRect rcPos, CWnd* pParent,UINT nID,COLORREF crColor,int flag)
c
c   FUNCTION:  This function Create a CNCLPaletteControl window
c
c   INPUT:  dwStyle: window style
c			pParent: parent window
c			rcPos: window position
c			crColor: default color
c			flag: window type flag
c			nID: window ID
c   OUTPUT: none
c
c***********************************************************************
*/
BOOL CNCLPaletteWnd::Create(DWORD dwStyle,CRect rcPos, CWnd* pParent,UINT nID,COLORREF crColor,int flag)
{
	LPVOID lp = (LPVOID)NULL;
	m_focus = 0;
	if(!CreateEx(0,
		AfxRegisterWndClass(CS_DBLCLKS, ::LoadCursor(NULL, IDC_ARROW)), 
		_T("CNCLPaletteWnd"),
		dwStyle,
		rcPos.left,
		rcPos.top,
		rcPos.Width(),
		rcPos.Height(),
		pParent->GetSafeHwnd(),
		(HMENU) nID,
		lp))
		
	{
		TRACE0("Failed to create CNCLPaletteWnd\n");
		return FALSE;
	}
	m_pNotifyWnd = pParent;
	m_pColorControl = new CNCLPaletteControl;
	CRect rc;
	GetClientRect(&rc);
	m_pColorControl->Create(this,crColor,rc,flag);
    UpdateWindow();
    return TRUE;
}

BOOL CNCLPaletteWnd::OnEraseBkgnd(CDC* pDC) 
{
	return TRUE;
}

/***********************************************************************
**
**   FUNCTION: OnLButtonDown(UINT nFlags, CPoint pt)
**
**       Left mouse button down callback
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLPaletteWnd::OnLButtonDown(UINT nFlags, CPoint point) 
{
	SetCapture ();
	SetFocus();
	m_pColorControl->OnLButtonDown(nFlags,point);
}

/***********************************************************************
**
**   FUNCTION: OnLButtonUp(UINT nFlags, CPoint pt)
**
**       Left mouse button up callback
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLPaletteWnd::OnLButtonUp(UINT nFlags, CPoint point) 
{
	if (GetCapture() == this)
		::ReleaseCapture();
	m_pColorControl->OnLButtonUp(nFlags,point);
	CWnd::OnLButtonUp(nFlags, point);
}
void CNCLPaletteWnd::OnDestroy() 
{
	CWnd::OnDestroy();
	
	// TODO: Add your message handler code here
	
}

/***********************************************************************
c
c   FUNCTION: OnPaint()
c
c         The framework calls this member function when Windows 
c			or an application makes a request to repaint a 
c			portion of an application's window. The WM_PAINT 
c			message is sent when the UpdateWindow or RedrawWindow 
c			member function is called.
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLPaletteWnd::OnPaint() 
{
	CPaintDC dcPaint(this); // device context for painting
	CRect rectClient;
	GetClientRect(rectClient);
	CDC dc;
	dc.CreateCompatibleDC(&dcPaint);
	CBitmap bmpMem;
	bmpMem.CreateCompatibleBitmap(&dcPaint, rectClient.Width(), rectClient.Height());
	CBitmap* pBmpOld = dc.SelectObject(&bmpMem);
	
	m_pColorControl->OnDraw(&dc);
	dcPaint.BitBlt(rectClient.left, rectClient.top,	rectClient.Width(), rectClient.Height(), &dc, 0, 0, SRCCOPY);
	
	dc.SelectObject(pBmpOld);
}
/***********************************************************************
**
**   FUNCTION: OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags) 
**
**       key down callback
**
**   INPUT:  nFlags:
**				nChar:
**				nRepCnt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLPaletteWnd::OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags) 
{
	m_pColorControl->OnKeyDown(nChar,nRepCnt,nFlags);
	CWnd::OnKeyDown(nChar, nRepCnt, nFlags);
}

BOOL CNCLPaletteWnd::PreCreateWindow(CREATESTRUCT& cs) 
{	
	return CWnd::PreCreateWindow(cs);
}
/***********************************************************************
**
**   FUNCTION: OnSize(UINT nType, int cx, int cy)
**
**       size change callback
**
**   INPUT:  nType:
**				cx, cy:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLPaletteWnd::OnSize(UINT nType, int cx, int cy) 
{
	CWnd::OnSize(nType, cx, cy);
	CRect rc;
	GetClientRect(&rc);
	if(m_pColorControl != NULL)
	{
		m_pColorControl->SetRect(&rc);
		m_pColorControl->UpdateAll();
	}
}
/***********************************************************************
**
**   SUBROUTINE: OnSelectColorOK
**
**   FUNCTION:  callback for WM_NCL_SELECTCOLOROK message
**
**   INPUT:  
**			none
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
LRESULT CNCLPaletteWnd::OnSelectColorOK(WPARAM wParam, LPARAM lParam)
{
	m_pNotifyWnd->PostMessage( WM_NCL_SELECTCOLOROK, wParam, lParam );
	return 0L;
}
/***********************************************************************
**
**   SUBROUTINE: OnSysColorChange
**
**   FUNCTION:  called when system color changed
**
**   INPUT:  
**			none
**   OUTPUT: none
**	 RETURN: none
**
***********************************************************************/
void CNCLPaletteWnd::OnSysColorChange()
{
	gfxData.OnSysColorChange();
	Invalidate ();
}

UINT CNCLPaletteWnd::OnGetDlgCode() 
{
	ASSERT_VALID(this);

	UINT nDlgCode = DLGC_WANTARROWS;
	nDlgCode |= DLGC_WANTTAB;

	return nDlgCode;
}

/***********************************************************************
**
**   FUNCTION: OnKillFocus(CWnd* pNewWnd) 
**
**       lose focus callback
**
**   INPUT:  pNewWnd: new focus window
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLPaletteWnd::OnKillFocus(CWnd* pNewWnd) 
{
	CWnd::OnKillFocus(pNewWnd);
	m_focus = 0;
	Invalidate ();
}
/***********************************************************************
**
**   FUNCTION: OnSetFocus(CWnd* pNewWnd) 
**
**       set focus callback
**
**   INPUT:  pNewWnd: new focus window
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLPaletteWnd::OnSetFocus(CWnd* pNewWnd) 
{
	CWnd::OnSetFocus(pNewWnd);
	m_focus = 1;
	Invalidate ();
}
