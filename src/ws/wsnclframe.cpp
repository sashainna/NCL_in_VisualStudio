/********************************************************************* 
**  NAME:  wsnclframe.cpp
**
**			Implementation file for NCL frame class
**	CONTAINS: 
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsnclframe.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:15
*********************************************************************/

#include "toolstdafx.h"
#include "wsnclframe.h"
#include "wsncltoolbar.h"
#include "wsncldockbar.h"
#include "wsncldockframe.h"
#include "wsntdlgbar.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define _AfxGetDlgCtrlID(hWnd)          ((UINT)(WORD)::GetDlgCtrlID(hWnd))

const DWORD CFrameWnd::dwDockBarMap[4][2] =
{
	{ AFX_IDW_DOCKBAR_TOP,      CBRS_TOP    },
	{ AFX_IDW_DOCKBAR_BOTTOM,   CBRS_BOTTOM },
	{ AFX_IDW_DOCKBAR_LEFT,     CBRS_LEFT   },
	{ AFX_IDW_DOCKBAR_RIGHT,    CBRS_RIGHT  },
};

/////////////////////////////////////////////////////////////////////////////

CNCLFrameWnd::CNCLFrameWnd()
{
	m_delay = 0;
}

CNCLFrameWnd::~CNCLFrameWnd()
{
}

IMPLEMENT_DYNAMIC(CNCLFrameWnd, CFrameWnd)

BEGIN_MESSAGE_MAP(CNCLFrameWnd, CFrameWnd)
	//{{AFX_MSG_MAP(CNCLFrameWnd)
//	ON_WM_CREATE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CNCLFrameWnd message handlers
/*
int CNCLFrameWnd::OnCreate(LPCREATESTRUCT lpCreateStruct) 
{
	if (CFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;
}
*/
/***********************************************************************
**
**   FUNCTION: EnableDocking(DWORD dwDockStyle)
**
**       Enable Frame window docking
**		This function is overwrite CFrameWnd::EnableDocking with little change
**
**   INPUT:  dwDockStyle: Frame window docking style allowed
**			
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLFrameWnd::EnableDocking(DWORD dwDockStyle)
{
	// must be CBRS_ALIGN_XXX or CBRS_FLOAT_MULTI only
	ASSERT((dwDockStyle & ~(CBRS_ALIGN_ANY|CBRS_FLOAT_MULTI)) == 0);

	m_pFloatingFrameClass = RUNTIME_CLASS(CNCLDockFrameWnd);
	for (int i = 0; i < 4; i++)
	{
		if (dwDockBarMap[i][1] & dwDockStyle & CBRS_ALIGN_ANY)
		{
			CDockBar* pDock = (CDockBar*)GetControlBar(dwDockBarMap[i][0]);
			if (pDock == NULL)
			{
				pDock = new CNCLDockBar;
				if (!pDock->Create(this,
					WS_CLIPSIBLINGS|WS_CLIPCHILDREN|WS_CHILD|WS_VISIBLE |
						dwDockBarMap[i][1], dwDockBarMap[i][0]))
				{
					AfxThrowResourceException();
				}
			}
		}
	} 
}


/***********************************************************************
**
**   FUNCTION: DockControlBar(CControlBar* pBar, UINT nDockBarID, LPCRECT lpRect)
**
**       Dock the control bar
**		This function is the same as CFrameWnd::DockControlBar
**		we have to write here because we want call our own
**		DockControlBar(CControlBar* pBar, CDockBar* pDockBar, LPCRECT lpRect)
**		which is changed for our purpose
**		
**
**   INPUT:  pBar: control bar to be docked
**			nDockBarID: dockbar ID
**			lpRect: dock rectangle where bar to be docked
**
**   OUTPUT :   None
**   RETURN:    
**
**********************************************************************/
void CNCLFrameWnd::DockControlBar(CControlBar* pBar, UINT nDockBarID, LPCRECT lpRect)
{
	CDockBar* pDockBar = (nDockBarID == 0) ? NULL :
		(CDockBar*)GetControlBar(nDockBarID);
	DockControlBar(pBar, pDockBar, lpRect);
}

/***********************************************************************
**
**   FUNCTION: DockControlBar(CControlBar* pBar, CDockBar* pDockBar, LPCRECT lpRect)
**
**       Dock the control bar
**		This function is the overwrite CFrameWnd::DockControlBar
**		with little changes for our purpose
**		
**
**   INPUT:  pBar: control bar to be docked
**			pDockBar: dockbar to dock
**			lpRect: dock rectangle where bar to be docked
**
**   OUTPUT :   None
**   RETURN:    
**
**********************************************************************/
void CNCLFrameWnd::DockControlBar(CControlBar* pBar, CDockBar* pDockBar, LPCRECT lpRect)
{
	ASSERT(pBar != NULL);
	// make sure CControlBar::EnableDocking has been called
	if (pBar->IsKindOf(RUNTIME_CLASS(CNCLToolBar)))
		ASSERT(((CNCLToolBar*)pBar)->m_pDockContext != NULL);
	else if (pBar->IsKindOf(RUNTIME_CLASS(CNCLDialogBar)))
		ASSERT(((CNCLDialogBar*)pBar)->m_pDockContext != NULL);
	else
		ASSERT(pBar->m_pDockContext != NULL);

	if (pDockBar == NULL)
	{
		for (int i = 0; i < 4; i++)
		{
			if ((dwDockBarMap[i][1] & CBRS_ALIGN_ANY) ==
				(pBar->m_dwStyle & CBRS_ALIGN_ANY))
			{
				pDockBar = (CDockBar*)GetControlBar(dwDockBarMap[i][0]);
				ASSERT(pDockBar != NULL);
				// assert fails when initial CBRS_ of bar does not
				// match available docking sites, as set by EnableDocking()
				break;
			}
		}
	}
	ASSERT(pDockBar != NULL);
	ASSERT(m_listControlBars.Find(pBar) != NULL);
	ASSERT(pBar->m_pDockSite == this);
	// if this assertion occurred it is because the parent of pBar was not initially
	// this CFrameWnd when pBar's OnCreate was called
	// i.e. this control bar should have been created with a different parent initially

	((CNCLDockBar *) pDockBar)->DockControlBar(pBar, lpRect);
}

/***********************************************************************
**
**   FUNCTION: ReDockControlBar(CControlBar* pBar, CDockBar* pDockBar, LPCRECT lpRect)
**
**      Redock the control bar
**		This function is the overwrite CFrameWnd::ReDockControlBar
**		with little changes for our purpose
**		
**
**   INPUT:  pBar: control bar to be docked
**			pDockBar: dockbar to dock
**			lpRect: dock rectangle where bar to be docked
**
**   OUTPUT :   None
**   RETURN:    
**
**********************************************************************/
void CNCLFrameWnd::ReDockControlBar(CControlBar* pBar, CDockBar* pDockBar, LPCRECT lpRect)
{
	ASSERT(pBar != NULL);
	// make sure CControlBar::EnableDocking has been called
	if (pBar->IsKindOf(RUNTIME_CLASS(CNCLToolBar)))
		ASSERT(((CNCLToolBar*)pBar)->m_pDockContext != NULL);
	else if (pBar->IsKindOf(RUNTIME_CLASS(CNCLDialogBar)))
		ASSERT(((CNCLDialogBar*)pBar)->m_pDockContext != NULL);
	else
		ASSERT(pBar->m_pDockContext != NULL);

	if (pDockBar == NULL)
	{
		// Search for the place holder.

		// In case we don't find a place holder, find a bar with the correct alignment
		// and keep it in pPossibleBar.
		CDockBar* pPossibleBar = NULL;
		for (int i = 0; i < 4; i++)
		{
			CDockBar* pTempBar = (CDockBar*)GetControlBar(dwDockBarMap[i][0]);
			if (pTempBar != NULL)
			{
				// Is this the same bar we docked with before?
				if (pTempBar->FindBar((CControlBar*)_AfxGetDlgCtrlID(pBar->m_hWnd)) > 0)
				{
					pDockBar = pTempBar;
					break;
				}
			}

			if ((dwDockBarMap[i][1] & CBRS_ALIGN_ANY) ==
				(pBar->m_dwStyle & CBRS_ALIGN_ANY))
			{
				pPossibleBar = (CDockBar*)GetControlBar(dwDockBarMap[i][0]);
				ASSERT(pPossibleBar != NULL);
				// assert fails when initial CBRS_ of bar does not
				// match available docking sites, as set by EnableDocking()
			}
		}

		// Did we find the place holder?
		if (pDockBar == NULL)
			pDockBar = pPossibleBar;
	}
	ASSERT(pDockBar != NULL);
	ASSERT(m_listControlBars.Find(pBar) != NULL);
	ASSERT(pBar->m_pDockSite == this);
	// if this assertion occurred it is because the parent of pBar was not initially
	// this CFrameWnd when pBar's OnCreate was called
	// i.e. this control bar should have been created with a different parent initially
	((CNCLDockBar *) pDockBar)->ReDockControlBar(pBar, lpRect);
}

/***********************************************************************
**
**   FUNCTION: FloatControlBar(CControlBar* pBar, CPoint point, DWORD dwStyle)
**
**      Floating the control bar
**		This function is the overwrite CFrameWnd::ReDockControlBar
**		with little changes for our purpose
**		
**
**   INPUT:  pBar: control bar to be Floating
**			point: control bar position to be floating
**			dwStyle: floating bar style
**
**   OUTPUT :   None
**   RETURN:    
**
**********************************************************************/
void CNCLFrameWnd::FloatControlBar(CControlBar* pBar, CPoint point, DWORD dwStyle)
{
	ASSERT(pBar != NULL);

	// if the bar is already floating and the dock bar only contains this
	// bar and same orientation then move the window rather than recreating
	// the frame
	if (pBar->m_pDockSite != NULL && pBar->m_pDockBar != NULL)
	{
		CDockBar* pDockBar = pBar->m_pDockBar;
		ASSERT_KINDOF(CDockBar, pDockBar);
		if (pDockBar->m_bFloating && pDockBar->GetDockedCount() == 1 &&
			(dwStyle & pDockBar->m_dwStyle & CBRS_ALIGN_ANY) != 0)
		{
			CNCLDockFrameWnd* pDockFrame =
				(CNCLDockFrameWnd*)pDockBar->GetParent();
			ASSERT(pDockFrame != NULL);
			ASSERT_KINDOF(CMiniDockFrameWnd, pDockFrame);
			pDockFrame->SetWindowPos(NULL, point.x, point.y, 0, 0,
				SWP_NOSIZE|SWP_NOZORDER|SWP_NOACTIVATE);
			pDockFrame->RecalcLayout(TRUE);
			pDockFrame->UpdateWindow();
			return;
		}
	}

	if (pBar->m_dwStyle & CBRS_SIZE_DYNAMIC)
	{
		dwStyle |= CBRS_SIZE_DYNAMIC;
		if (dwStyle & CBRS_ORIENT_VERT)
		{
			dwStyle &= ~CBRS_ALIGN_ANY;
			dwStyle |= CBRS_ALIGN_TOP;
		}
	}

	CNCLDockFrameWnd* pDockFrame = (CNCLDockFrameWnd* )CreateFloatingFrame(dwStyle);
	ASSERT(pDockFrame != NULL);
	pDockFrame->SetWindowPos(NULL, point.x, point.y, 0, 0,
		SWP_NOSIZE|SWP_NOZORDER|SWP_NOACTIVATE);
	if (pDockFrame->m_hWndOwner == NULL)
		pDockFrame->m_hWndOwner = pBar->m_hWnd;

	CNCLDockBar* pDockBar = (CNCLDockBar*)pDockFrame->GetDlgItem(AFX_IDW_DOCKBAR_FLOAT);
	ASSERT(pDockBar != NULL);
	ASSERT_KINDOF(CDockBar, pDockBar);

	ASSERT(pBar->m_pDockSite == this);
	// if this assertion occurred it is because the parent of pBar was not
	//  initially this CFrameWnd when pBar's OnCreate was called
	// (this control bar should have been created with a different
	//  parent initially)

	((CNCLDockBar *) pDockBar)->DockControlBar(pBar);
	pDockFrame->RecalcLayout(TRUE);
	if ((m_delay==0)&&(GetWindowLong(pBar->m_hWnd, GWL_STYLE) & WS_VISIBLE))
	{
		pDockFrame->ShowWindow(SW_SHOWNA);
		pDockFrame->UpdateWindow();
	}
}

/***********************************************************************
**
**   FUNCTION: CanDock(CRect rect, DWORD dwDockStyle, CDockBar** ppDockBar)
**
**      Check if rect can be dock
**		
**
**   INPUT:  rect: rect to be check
**			ppDockBar: dockbar can be docked
**			dwDockStyle: allowable styles of bar
**
**   OUTPUT :   None
**   RETURN:    
**
**********************************************************************/
DWORD CNCLFrameWnd::CanDock(CRect rect, DWORD dwDockStyle, CDockBar** ppDockBar)
{
	// dwDockStyle -- allowable styles of bar
	// don't allow to dock to floating unless multi is specified
	dwDockStyle &= CBRS_ALIGN_ANY|CBRS_FLOAT_MULTI;

	if (ppDockBar != NULL)
		*ppDockBar = NULL;
	POSITION pos = m_listControlBars.GetHeadPosition();
	while (pos != NULL)
	{
		CNCLDockBar* pDockBar = (CNCLDockBar*)m_listControlBars.GetNext(pos);
		if (pDockBar->IsDockBar() && pDockBar->IsWindowVisible() &&
			(pDockBar->m_dwStyle & dwDockStyle & CBRS_ALIGN_ANY) &&
			(!pDockBar->m_bFloating ||
				(dwDockStyle & pDockBar->m_dwStyle & CBRS_FLOAT_MULTI)))
		{
			CRect rectBar;
			pDockBar->GetWindowRect(&rectBar);
			if (rectBar.Width() == 0)
				rectBar.right++;
			if (rectBar.Height() == 0)
				rectBar.bottom++;
			if (rectBar.IntersectRect(rectBar, rect))
			{
				if (ppDockBar != NULL)
					*ppDockBar = pDockBar;
				return pDockBar->m_dwStyle & dwDockStyle;
			}
		}
	}
	return 0;
}
/***********************************************************************
**
**   FUNCTION: FloatControlBar(CControlBar* pBar, CPoint point, DWORD dwStyle)
**
**      Floating the control bar
**		This function is the overwrite CFrameWnd::ReDockControlBar
**		with little changes for our purpose
**		
**
**   INPUT:  pBar: control bar to be Floating
**			point: control bar position to be floating
**			dwStyle: floating bar style
**
**   OUTPUT :   None
**   RETURN:    
**
**********************************************************************/
void CNCLFrameWnd::Delay_FloatControlBar(CControlBar* pBar, CPoint point, DWORD dwStyle)
{
	m_delay = 1;
	FloatControlBar(pBar, point, dwStyle);
	m_delay = 0;
}
