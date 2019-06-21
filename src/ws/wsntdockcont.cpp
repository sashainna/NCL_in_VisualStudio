/********************************************************************* 
**  NAME:  wsntdockcont.cpp
**
**			Implementation file for CNCLDockContext
**	CONTAINS: 
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntdockcont.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:22      
*********************************************************************/
#include "toolstdafx.h"
#include "wsntframe.h"
#include "wsncldockbar.h"
#include "wsntdockcont.h"
#include "wsnttmenu.h"
#include "wsntcfunc.h"
#include "wsnclframe.h"
#include "wsncltoolbar.h"

#include "afxpriv.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define HORZF(dw) (dw & CBRS_ORIENT_HORZ)
#define VERTF(dw) (dw & CBRS_ORIENT_VERT)
#define _AfxSetDlgCtrlID(hWnd, nID)     SetWindowLong(hWnd, GWL_ID, nID)
#define _AfxGetDlgCtrlID(hWnd)          ((UINT)(WORD)::GetDlgCtrlID(hWnd))

struct AUX_DATA
{
	// system metrics
	int cxVScroll, cyHScroll;
	int cxIcon, cyIcon;

	int cxBorder2, cyBorder2;

	// device metrics for screen
	int cxPixelsPerInch, cyPixelsPerInch;

	// convenient system color
	HBRUSH hbrWindowFrame;
	HBRUSH hbrBtnFace;

	// color values of system colors used for CToolBar
	COLORREF clrBtnFace, clrBtnShadow, clrBtnHilite;
	COLORREF clrBtnText, clrWindowFrame;

	// standard cursors
	HCURSOR hcurWait;
	HCURSOR hcurArrow;
	HCURSOR hcurHelp;       // cursor used in Shift+F1 help

	// special GDI objects allocated on demand
	HFONT   hStatusFont;
	HFONT   hToolTipsFont;
	HBITMAP hbmMenuDot;

	// other system information
	UINT    nWinVer;        // Major.Minor version numbers
	BOOL    bWin95;         // TRUE if Windows 95 (not NT)
	BOOL    bWin4;          // TRUE if Windows 4.0
	BOOL    bNotWin4;       // TRUE if not Windows 4.0
	BOOL    bSmCaption;     // TRUE if WS_EX_SMCAPTION is supported
	BOOL    bMarked4;       // TRUE if marked as 4.0

// Implementation
	AUX_DATA();
	~AUX_DATA();
	void UpdateSysColors();
	void UpdateSysMetrics();
};

AFX_STATIC void AFXAPI _AfxAdjustRectangle(CRect& rect, CPoint pt)
{
	int nXOffset = (pt.x < rect.left) ? (pt.x - rect.left) :
					(pt.x > rect.right) ? (pt.x - rect.right) : 0;
	int nYOffset = (pt.y < rect.top) ? (pt.y - rect.top) :
					(pt.y > rect.bottom) ? (pt.y - rect.bottom) : 0;
	rect.OffsetRect(nXOffset, nYOffset);
}

extern CMainFrame *NCL_MainFrame;

/////////////////////////////////////////////////////////////////////////////
// CNCLDockContext

CNCLDockContext::CNCLDockContext(CControlBar* pBar):CDockContext(pBar)
{
	ASSERT(pBar != NULL);
	ASSERT(pBar->m_pDockSite != NULL);

	m_pBar = pBar;
	m_pDockSite = pBar->m_pDockSite;

	m_uMRUDockID = 0;
	m_rectMRUDockPos.left = 0;
	m_rectMRUDockPos.top = 0;
	if (pBar->m_dwStyle & CBRS_SIZE_DYNAMIC)
		m_dwMRUFloatStyle = pBar->m_dwStyle & (CBRS_ALIGN_TOP | CBRS_SIZE_DYNAMIC);
	else if (pBar->m_dwStyle & CBRS_ORIENT_HORZ)
		m_dwMRUFloatStyle = CBRS_ALIGN_TOP | (pBar->m_dwStyle & CBRS_FLOAT_MULTI);
	else
		m_dwMRUFloatStyle = CBRS_ALIGN_LEFT | (pBar->m_dwStyle & CBRS_FLOAT_MULTI);
	m_ptMRUFloatPos.x = CW_USEDEFAULT;

	ASSERT(m_pDockSite->IsFrameWnd());
	m_pDC = NULL;
}

CNCLDockContext::~CNCLDockContext()
{
}

/////////////////////////////////////////////////////////////////////////////
// CNCLDockContext Drag Operations

/**********************************************************************
**    I_FUNCTION :  StartDrag(CPoint pt)
**       function execute when start dragging
**
**    PARAMETERS   
**       INPUT  : pt: control bar position when dragging
**       OUTPUT :  None
**				
**    RETURNS      :None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLDockContext::StartDrag(CPoint pt)
{
	ASSERT_VALID(m_pBar);
	m_bDragging = TRUE;

	InitLoop();

	// GetWindowRect returns screen coordinates(not mirrored),
	// so if the desktop is mirrored then turn off mirroring
	// for the desktop dc so that we get correct focus rect drawn.
	// This layout change should be remembered, just in case ...

//	if (m_pDC->GetLayout() & LAYOUT_RTL)
//		m_pDC->SetLayout(LAYOUT_LTR);

/*
.....MFC will not call following for 
.....CBRS_SIZE_FIXED because it has fixed size
.....but for status bar (which use m_bartype==1)
.....we want change size while dragging from floating->docking or
.....docking -> floating but only for dragging
*/
//	if (m_pBar->m_dwStyle & CBRS_SIZE_DYNAMIC)
	((CNCLToolBar*)m_pBar)->m_StartDrag = 1;
	if ((((CNCLToolBar*)m_pBar)->m_dwStyle & CBRS_SIZE_DYNAMIC)
		||(((CNCLToolBar*)m_pBar)->m_bartype==1))
	{
		// get true bar size (including borders)
		CRect rect;
		m_pBar->GetWindowRect(rect);
		m_ptLast = pt;
		CSize sizeHorz = m_pBar->CalcDynamicLayout(0, LM_HORZ | LM_HORZDOCK);
		CSize sizeVert = m_pBar->CalcDynamicLayout(0, LM_VERTDOCK);
		CSize sizeFloat = m_pBar->CalcDynamicLayout(0, LM_HORZ | LM_MRUWIDTH);

		sizeHorz.cx += BAR_GRIPPER - 6;
		sizeVert.cy += BAR_GRIPPER - 6;
		m_rectDragHorz = CRect(rect.TopLeft(), sizeHorz);
		m_rectDragVert = CRect(rect.TopLeft(), sizeVert);

		// calculate frame dragging rectangle
		m_rectFrameDragHorz = CRect(rect.TopLeft(), sizeFloat);
		m_rectFrameDragVert = CRect(rect.TopLeft(), sizeFloat);

		CMiniFrameWnd::CalcBorders(&m_rectFrameDragHorz);
		CMiniFrameWnd::CalcBorders(&m_rectFrameDragVert);

		m_rectFrameDragHorz.InflateRect(-2, -2);
		m_rectFrameDragVert.InflateRect(-2, -2);
	}
	else if (m_pBar->m_dwStyle & CBRS_SIZE_FIXED)
	{
		// get true bar size (including borders)
		CRect rect;
		m_pBar->GetWindowRect(rect);
		m_ptLast = pt;
		CSize sizeHorz = m_pBar->CalcDynamicLayout(-1, LM_HORZ | LM_HORZDOCK);
		CSize sizeVert = m_pBar->CalcDynamicLayout(-1, LM_VERTDOCK);

		// calculate frame dragging rectangle
		m_rectFrameDragHorz = m_rectDragHorz = CRect(rect.TopLeft(), sizeHorz);
		m_rectFrameDragVert = m_rectDragVert = CRect(rect.TopLeft(), sizeVert);

		CMiniFrameWnd::CalcBorders(&m_rectFrameDragHorz);
		CMiniFrameWnd::CalcBorders(&m_rectFrameDragVert);
		m_rectFrameDragHorz.InflateRect(-2, -2);
		m_rectFrameDragVert.InflateRect(-2, -2);
	}
	else
	{
		// get true bar size (including borders)
		CRect rect;
		m_pBar->GetWindowRect(rect);
		m_ptLast = pt;
		BOOL bHorz = HORZF(m_dwStyle);
		DWORD dwMode = !bHorz ? (LM_HORZ | LM_HORZDOCK) : LM_VERTDOCK;
		CSize size = m_pBar->CalcDynamicLayout(-1, dwMode);

		// calculate inverted dragging rect
		if (bHorz)
		{
			m_rectDragHorz = rect;
			m_rectDragVert = CRect(CPoint(pt.x - rect.Height()/2, rect.top), size);
		}
		else // vertical orientation
		{
			m_rectDragVert = rect;
			m_rectDragHorz = CRect(CPoint(rect.left, pt.y - rect.Width()/2), size);
		}

		// calculate frame dragging rectangle
		m_rectFrameDragHorz = m_rectDragHorz;
		m_rectFrameDragVert = m_rectDragVert;

		CMiniFrameWnd::CalcBorders(&m_rectFrameDragHorz);
		CMiniFrameWnd::CalcBorders(&m_rectFrameDragVert);
		m_rectFrameDragHorz.InflateRect(-2, -2);
		m_rectFrameDragVert.InflateRect(-2, -2);
	}
	// adjust rectangles so that point is inside
	_AfxAdjustRectangle(m_rectDragHorz, pt);
	_AfxAdjustRectangle(m_rectDragVert, pt);
	_AfxAdjustRectangle(m_rectFrameDragHorz, pt);
	_AfxAdjustRectangle(m_rectFrameDragVert, pt);

	((CNCLToolBar*)m_pBar)->m_StartDrag = 0;

	// initialize tracking state and enter tracking loop
	m_dwOverDockStyle = CanDock();
	Move(pt);   // call it here to handle special keys
	Track();
}


/**********************************************************************
**    I_FUNCTION :  Move(CPoint pt)
**       function execute when mouse move
**
**    PARAMETERS   
**       INPUT  : None
**       OUTPUT :  None
**				
**    RETURNS      :None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLDockContext::Move(CPoint pt)
{
	CPoint ptOffset = pt - m_ptLast;

	// offset all drag rects to new position
	m_rectDragHorz.OffsetRect(ptOffset);
	m_rectFrameDragHorz.OffsetRect(ptOffset);
	m_rectDragVert.OffsetRect(ptOffset);
	m_rectFrameDragVert.OffsetRect(ptOffset);
	m_ptLast = pt;

	// if control key is down don't dock
	m_dwOverDockStyle = m_bForceFrame ? 0 : CanDock();

	DrawFocusRect();
}

/**********************************************************************
**    I_FUNCTION :  EndDrag()
**       function execute when end drag
**
**    PARAMETERS   
**       INPUT  : None
**       OUTPUT :  None
**				
**    RETURNS      :None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLDockContext::EndDrag()
{
	CancelLoop();
/*
......need do something for NCL
......Yurong
*/
	if (m_dwOverDockStyle != 0)
	{
		CNCLDockBar* pDockBar = (CNCLDockBar*)GetDockBar(m_dwOverDockStyle);
		ASSERT(pDockBar != NULL);

		CRect rect = (m_dwOverDockStyle & CBRS_ORIENT_VERT) ?
			m_rectDragVert : m_rectDragHorz;

		UINT uID = _AfxGetDlgCtrlID(pDockBar->m_hWnd);
		if (uID >= AFX_IDW_DOCKBAR_TOP &&
			uID <= AFX_IDW_DOCKBAR_BOTTOM)
		{
			m_uMRUDockID = uID;
			m_rectMRUDockPos = rect;
			pDockBar->ScreenToClient(&m_rectMRUDockPos);
		}
/*
......m_bartype = 1 is for status bar we can set the button size as we want
......added for NCL (changed MFC default), statusbar show one row or one cols when docked
*/
		if (((CNCLToolBar*)m_pBar)->m_bartype==1)
		{
			if ((uID == AFX_IDW_DOCKBAR_TOP)||
				(uID == AFX_IDW_DOCKBAR_BOTTOM))
			{
				((CNCLToolBar*)m_pBar)->SetWraped(0);
			}
			else
				((CNCLToolBar*)m_pBar)->SetWraped(2);
		}
		// dock it at the specified position, RecalcLayout will snap
		((CNCLFrameWnd*)NCL_MainFrame)->DockControlBar(m_pBar, pDockBar, &rect);
		((CNCLFrameWnd*)NCL_MainFrame)->RecalcLayout();
	}
	else if ((m_dwStyle & CBRS_SIZE_DYNAMIC) || (HORZF(m_dwStyle) && !m_bFlip) ||
			(VERTF(m_dwStyle) && m_bFlip))
	{
		m_dwMRUFloatStyle = CBRS_ALIGN_TOP | (m_dwDockStyle & CBRS_FLOAT_MULTI);
		m_ptMRUFloatPos = m_rectFrameDragHorz.TopLeft();
		if(m_pBar->IsKindOf(RUNTIME_CLASS(CNCLToolBar)))
		{		
			if (m_pBar->IsFloating())
				((CNCLToolBar*)m_pBar)->Set_Changed (1);
			else
			{
				((CNCLToolBar*)m_pBar)->Set_Changed (0);
/*
......m_bartype = 1 is for status bar we can set the button size as we want
......added for NCL (changed MFC default), statusbar show one row or one cols when docked
*/
				if (((CNCLToolBar*)m_pBar)->m_bartype==1)
					((CNCLToolBar*)m_pBar)->SetWraped(1);
			}
		}
		((CNCLFrameWnd*)NCL_MainFrame)->FloatControlBar(m_pBar, m_ptMRUFloatPos, m_dwMRUFloatStyle);
	}
	else // vertical float
	{
		m_dwMRUFloatStyle = CBRS_ALIGN_LEFT | (m_dwDockStyle & CBRS_FLOAT_MULTI);
		m_ptMRUFloatPos = m_rectFrameDragVert.TopLeft();
		if(m_pBar->IsKindOf(RUNTIME_CLASS(CNCLToolBar)))
		{		
			if (m_pBar->IsFloating())
				((CNCLToolBar*)m_pBar)->Set_Changed (1);
			else
			{
				((CNCLToolBar*)m_pBar)->Set_Changed (0);
/*
......m_bartype = 1 is for status bar we can set the button size as we want
......added for NCL (changed MFC default), statusbar show one row or one cols when docked
*/
				if (((CNCLToolBar*)m_pBar)->m_bartype==1)
					((CNCLToolBar*)m_pBar)->SetWraped(1);
			}
		}
		((CNCLFrameWnd*)NCL_MainFrame)->FloatControlBar(m_pBar, m_ptMRUFloatPos, m_dwMRUFloatStyle);
	}
}

/**********************************************************************
**    I_FUNCTION :  Track()
**       Track the mouse event over control bar
**
**    PARAMETERS   
**       INPUT  : None
**       OUTPUT :  None
**				
**    RETURNS      :None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
BOOL CNCLDockContext::Track()
{
	// don't handle if capture already set
	if (::GetCapture() != NULL)
		return FALSE;

	// set capture to the window which received this message
	m_pBar->SetCapture();
	ASSERT(m_pBar == CWnd::GetCapture());

	// get messages until capture lost or cancelled/accepted
	while (CWnd::GetCapture() == m_pBar)
	{
		MSG msg;
		if (!::GetMessage(&msg, NULL, 0, 0))
		{
			AfxPostQuitMessage(msg.wParam);
			break;
		}

		switch (msg.message)
		{
		case WM_LBUTTONUP:
			if (m_bDragging)
				EndDrag();
			else
				EndResize();
			return TRUE;
		case WM_MOUSEMOVE:
			if (m_bDragging)
				Move(msg.pt);
			else
				Stretch(msg.pt);
			break;
		case WM_KEYUP:
			if (m_bDragging)
				OnKey((int)msg.wParam, FALSE);
			break;
		case WM_KEYDOWN:
			if (m_bDragging)
				OnKey((int)msg.wParam, TRUE);
			if (msg.wParam == VK_ESCAPE)
			{
				CancelLoop();
				return FALSE;
			}
			break;
		case WM_RBUTTONDOWN:
			CancelLoop();
			return FALSE;

		// just dispatch rest of the messages
		default:
			DispatchMessage(&msg);
			break;
		}
	}

	CancelLoop();

	return FALSE;
}

/**********************************************************************
**    I_FUNCTION :  ToggleDocking()
**       Docking back to original position
**
**    PARAMETERS   
**       INPUT  : None
**       OUTPUT :  None
**				
**    RETURNS      :None
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CNCLDockContext::ToggleDocking()
{
	if (m_pBar->IsFloating())
	{
		// Dock it only if is allowed to be docked
		if (m_pBar->m_dwDockStyle & CBRS_ALIGN_ANY)
		{
			ASSERT((m_uMRUDockID >= AFX_IDW_DOCKBAR_TOP &&
				m_uMRUDockID <= AFX_IDW_DOCKBAR_BOTTOM) ||
				m_uMRUDockID == 0);

			CRect rect = m_rectMRUDockPos;
			CDockBar* pDockBar = NULL;
			if (m_uMRUDockID != 0)
			{
				pDockBar = (CDockBar*)m_pDockSite->GetControlBar(m_uMRUDockID);
				pDockBar->ClientToScreen(&rect);
			}
// dock it at the specified position, RecalcLayout will snap
			((CNCLToolBar*)m_pBar)->m_StartDrag = 1;
/*
......m_bartype = 1 is for status bar we can set the button size as we want
......added for NCL (changed MFC default), statusbar show one row or one cols when docked
*/
			if (((CNCLToolBar*)m_pBar)->m_bartype==1)
			{
				if ((m_uMRUDockID == AFX_IDW_DOCKBAR_TOP)||
					(m_uMRUDockID == AFX_IDW_DOCKBAR_BOTTOM))
				{
					((CNCLToolBar*)m_pBar)->SetWraped(0);
				}
				else
					((CNCLToolBar*)m_pBar)->SetWraped(2);
			}
			((CNCLFrameWnd*)NCL_MainFrame)->ReDockControlBar(m_pBar, pDockBar, &rect);
			((CNCLFrameWnd*)NCL_MainFrame)->RecalcLayout();
			((CNCLToolBar*)m_pBar)->m_StartDrag = 0;
		}
	}
	else
	{
		CPoint ptFloat = m_ptMRUFloatPos;
		if (ptFloat.x < 0 || ptFloat.y < 0)
		{
			ptFloat = m_rectMRUDockPos.TopLeft();
			m_pBar->GetParent()->ClientToScreen(&ptFloat);
		}
		((CNCLFrameWnd*)NCL_MainFrame)->FloatControlBar(m_pBar, ptFloat, m_dwMRUFloatStyle);
	}
}



/////////////////////////////////////////////////////////////////////////////
// CNCLDockContext Resize Operations

#define m_rectRequestedSize     m_rectDragHorz
#define m_rectActualSize        m_rectDragVert
#define m_rectActualFrameSize   m_rectFrameDragHorz
#define m_rectFrameBorders      m_rectFrameDragVert


//IMPLEMENT_DYNAMIC(CNCLDockContext,CDockContext)

/////////////////////////////////////////////////////////////////////////////
