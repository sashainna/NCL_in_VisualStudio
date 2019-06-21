/********************************************************************* 
**  NAME:  wsncldockbar.cpp
**
**			Implementation file for CNCLDockBar
**	CONTAINS: 
**			uw_ntget_ctlbar_type
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsncldockbar.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:15
*********************************************************************/
//////////////////////////////////////////////////////////////////////

#include "toolstdafx.h"
#include "wsncldockbar.h"
#include "wsncltoolbar.h"
#include "wsntframe.h"


#include "afxpriv.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

#define _AfxGetDlgCtrlID(hWnd)          ((UINT)(WORD)::GetDlgCtrlID(hWnd))
#ifndef _countof
#define _countof(array) (sizeof(array)/sizeof(array[0]))
#endif
extern CMainFrame *NCL_MainFrame;
extern int UW_drag_pos[5];

IMPLEMENT_DYNCREATE(CNCLDockBar,CDockBar)

BEGIN_MESSAGE_MAP(CNCLDockBar,CDockBar)
	//{{AFX_MSG_MAP(CNCLDockBar)
	ON_COMMAND(UW_ADDNEW_MENUBAR, OnAddMenuBar)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

CNCLDockBar::CNCLDockBar()
{
//	m_TargetDrop = new CNCLMnDropTarget();
//	if(m_TargetDrop)
//		m_TargetDrop->Register(this, CF_TEXT);
}

CNCLDockBar::~CNCLDockBar()
{

}

/***********************************************************************
**
**   FUNCTION: CalcFixedLayout( BOOL bStretch, BOOL bHorz )
**
**       Calculate dockbar's fixed layout
**		This function is overwrite CDockBar::CalcFixedLayout with little change
**
**   INPUT:  bStretch: Indicates whether the bar should be stretched to the size of the frame
**			bHorz: Indicates that the bar is horizontally or vertically oriented
**
**   OUTPUT :   None
**   RETURN:    layout size
**
**********************************************************************/

CSize CNCLDockBar::CalcFixedLayout( BOOL bStretch, BOOL bHorz )
{
	ASSERT_VALID(this);

	CSize sizeFixed = CControlBar::CalcFixedLayout(bStretch, bHorz);

	// get max size
	CSize sizeMax;
	if (!m_rectLayout.IsRectEmpty())
		sizeMax = m_rectLayout.Size();
	else
	{
		CRect rectFrame;
		CFrameWnd* pFrame = GetParentFrame();
		pFrame->GetClientRect(&rectFrame);
		sizeMax = rectFrame.Size();
	}

	const int NormalcxBorder2=GetSystemMetrics(SM_CYBORDER)*2;
	const int NormalcyBorder2=2;
	const int AltcxBorder2=0;
	const int AltcyBorder2=0;
	int cxBorder2=0;
	int cyBorder2=0;

	AFX_SIZEPARENTPARAMS layout;
	layout.hDWP = m_bLayoutQuery ?
		NULL : ::BeginDeferWindowPos(m_arrBars.GetSize());
	CPoint pt(-cxBorder2, -NormalcyBorder2);
	int nWidth = 0;

	BOOL bWrapped = FALSE;

	for (int nPos = 0; nPos < m_arrBars.GetSize(); nPos++)
	{
		CControlBar* pBar = GetDockedControlBar(nPos);
		void* pVoid = m_arrBars[nPos];

		if (pBar != NULL)
		{
			if (pBar->IsVisible())
			{
				if(pBar->IsKindOf(RUNTIME_CLASS(CNCLToolBar)))
				{
					cxBorder2=AltcxBorder2;
					cyBorder2=AltcyBorder2;
				}
				else
				{
					cxBorder2=NormalcxBorder2;
					cyBorder2=NormalcyBorder2;
				}
/* 
......get ideal rect for bar
*/
				DWORD dwMode = 0;
				if ((pBar->m_dwStyle & CBRS_SIZE_DYNAMIC) &&
					(pBar->m_dwStyle & CBRS_FLOATING))
					dwMode |= LM_HORZ | LM_MRUWIDTH;
				else if (pBar->m_dwStyle & CBRS_ORIENT_HORZ)
					dwMode |= LM_HORZ | LM_HORZDOCK;
				else
					dwMode |=  LM_VERTDOCK;

				CSize sizeBar = pBar->CalcDynamicLayout(-1, dwMode);
				CRect rect(pt, sizeBar);

				// get current rect for bar
				CRect rectBar;
				pBar->GetWindowRect(&rectBar);
				ScreenToClient(&rectBar);

				if (bHorz)
				{
/*
......Offset Calculated Rect out to Actual
*/
					if (rectBar.left > rect.left && !m_bFloating)
						rect.OffsetRect(rectBar.left - rect.left, 0);

/*
......If ControlBar has been wrapped, then left justify
*/
					if (bWrapped)
					{
						bWrapped = FALSE;
						rect.OffsetRect(-(rect.left + cxBorder2), 0);
					}
/*
......For NCL, if ControlBar can't completely shpwing, then wrap it
*/
					if ((rect.right > sizeMax.cx && !m_bFloating) &&
						(nPos > 0) && (m_arrBars[nPos - 1] != NULL))
					{
						m_arrBars.InsertAt(nPos, (CObject*)NULL);
						pBar = NULL; pVoid = NULL;
						bWrapped = TRUE;
					}
					if (!bWrapped)
					{
						if (rect != rectBar)
						{
							if (!m_bLayoutQuery &&
								!(pBar->m_dwStyle & CBRS_FLOATING))
							{
								if (pBar->IsKindOf(RUNTIME_CLASS(CNCLToolBar)))
									((CNCLToolBar*)pBar)->m_pDockContext->m_rectMRUDockPos = rect;
								else
									pBar->m_pDockContext->m_rectMRUDockPos = rect;
							}
							AfxRepositionWindow(&layout, pBar->m_hWnd, &rect);
						}
						pt.x = rect.left + sizeBar.cx - cxBorder2;
						nWidth = max(nWidth, sizeBar.cy);
					}
				}
				else
				{
/*
......Offset Calculated Rect out to Actual
*/
					if (rectBar.top > rect.top && !m_bFloating)
						rect.OffsetRect(0, rectBar.top - rect.top);

/*
...... If ControlBar has been wrapped, then top justify
*/
					if (bWrapped)
					{
						bWrapped = FALSE;
						rect.OffsetRect(0, -(rect.top + cyBorder2));
					}
/*
......For NCL, if ControlBar can't completely showing, then wrap it
*/
					if ((rect.bottom > sizeMax.cy && !m_bFloating) &&
						(nPos > 0) && (m_arrBars[nPos - 1] != NULL))
					{
						m_arrBars.InsertAt(nPos, (CObject*)NULL);
						pBar = NULL; pVoid = NULL;
						bWrapped = TRUE;
					}
					if (!bWrapped)
					{
						if (rect != rectBar)
						{
							if (!m_bLayoutQuery &&
								!(pBar->m_dwStyle & CBRS_FLOATING))
							{
								if (pBar->IsKindOf(RUNTIME_CLASS(CNCLToolBar)))
									((CNCLToolBar*)pBar)->m_pDockContext->m_rectMRUDockPos = rect;
								else
									pBar->m_pDockContext->m_rectMRUDockPos = rect;
							}
							AfxRepositionWindow(&layout, pBar->m_hWnd, &rect);
						}
						pt.y = rect.top + sizeBar.cy - cyBorder2;
						nWidth = max(nWidth, sizeBar.cx);
					}
				}
			}
			if (!bWrapped)
			{
				// handle any delay/show hide for the bar
				pBar->RecalcDelayShow(&layout);
			}
		}
		if (pBar == NULL && pVoid == NULL && nWidth != 0)
		{
			// end of row because pBar == NULL
			if (bHorz)
			{
				pt.y += nWidth - cyBorder2;
				sizeFixed.cx = max(sizeFixed.cx, pt.x);
				sizeFixed.cy = max(sizeFixed.cy, pt.y);
				pt.x = -cxBorder2;
			}
			else
			{
				pt.x += nWidth - cxBorder2;
				sizeFixed.cx = max(sizeFixed.cx, pt.x);
				sizeFixed.cy = max(sizeFixed.cy, pt.y);
				pt.y = -cyBorder2;
			}
			nWidth = 0;
		}
	}
	if (!m_bLayoutQuery)
	{
		// move and resize all the windows at once!
		if (layout.hDWP == NULL || !::EndDeferWindowPos(layout.hDWP))
			TRACE0("Warning: DeferWindowPos failed - low system resources.\n");
	}

	// adjust size for borders on the dock bar itself
	CRect rect;
	rect.SetRectEmpty();
	CalcInsideRect(rect, bHorz);

	if ((!bStretch || !bHorz) && sizeFixed.cx != 0)
		sizeFixed.cx += -rect.right + rect.left;
	if ((!bStretch || bHorz) && sizeFixed.cy != 0)
		sizeFixed.cy += -rect.bottom + rect.top;

//	if(m_TargetDrop)
//		m_TargetDrop->Register(this, CF_TEXT);

	return sizeFixed;
}

/***********************************************************************
**
**   FUNCTION: RemoveControlBar(CControlBar* pBar, int nPosExclude, int nAddPlaceHolder)
**
**       Remove the control bar
**		This function is overwrite CDockBar::RemoveControlBar with little change
**
**   INPUT:  pBar: control bar to remove
**			nPosExclude: 
**			nAddPlaceHolder: 
**
**   OUTPUT :   None
**   RETURN:    
**
**********************************************************************/

BOOL CNCLDockBar::RemoveControlBar(CControlBar* pBar, int nPosExclude, int nAddPlaceHolder)
{
	ASSERT(nAddPlaceHolder == 1 || nAddPlaceHolder == 0 || nAddPlaceHolder == -1);
	ASSERT_VALID(this);
	ASSERT(pBar != NULL);
	int nPos = FindBar(pBar, nPosExclude);
	ASSERT(nPos > 0);

	if (nAddPlaceHolder == 1)
	{
		m_arrBars[nPos] = (void*)_AfxGetDlgCtrlID(pBar->m_hWnd);

		// check for already existing place holder
		int nPosOld = FindBar((CControlBar*)m_arrBars[nPos], nPos);
		if (nPosOld > 0)
		{
			m_arrBars.RemoveAt(nPos);

			// remove section indicator (NULL) if nothing else in section
			if (m_arrBars[nPos-1] == NULL && m_arrBars[nPos] == NULL)
				m_arrBars.RemoveAt(nPos);
		}
	}
	else
	{
		m_arrBars.RemoveAt(nPos);
		if (m_arrBars[nPos-1] == NULL && m_arrBars[nPos] == NULL)
			m_arrBars.RemoveAt(nPos);

		// Remove any pre-existing place holders.
		if (nAddPlaceHolder != -1)
			RemovePlaceHolder(pBar);
	}

	// don't do anything more in the shutdown case!
	if (pBar->IsKindOf(RUNTIME_CLASS(CNCLToolBar)))
	{
		if (((CNCLToolBar*)pBar)->m_pDockContext == NULL)
			return FALSE;
	}
	else
	{
		if (pBar->m_pDockContext == NULL)
			return FALSE;
	}
	// get parent frame for recalc layout/frame destroy
	CFrameWnd* pFrameWnd = GetDockingFrame();
	if (m_bFloating && GetDockedVisibleCount() == 0)
	{
		if (GetDockedCount() == 0)
		{
			pFrameWnd->DestroyWindow();
			return TRUE; // Self-Destruct
		}
		else
			pFrameWnd->ShowWindow(SW_HIDE);
	}
	else
		pFrameWnd->DelayRecalcLayout();

	return FALSE;
}

/***********************************************************************
**
**   FUNCTION: CControlBar* CNCLDockBar::GetDockedControlBar(int nPos) 
**
**       Giving the bar position and Get Docked control bar
**		This function is overwrite CDockBar::GetDockedControlBar with little change
**
**   INPUT:  nPos: control bar position
**			
**
**   OUTPUT :   None
**   RETURN:    Control bar to be find
**
**********************************************************************/
CControlBar* CNCLDockBar::GetDockedControlBar(int nPos) const
{
	CControlBar* pResult = (CControlBar*)m_arrBars[nPos];
	if (HIWORD(pResult) == 0)
		return NULL;
	return pResult;
}

/***********************************************************************
**
**   FUNCTION: DockControlBar(CControlBar* pBar, LPCRECT lpRect)
**
**       Dock the comtrol bar the control bar
**		This function is overwrite CDockBar::DockControlBar with little change
**		
**
**   INPUT:  pBar: control bar to be docked
**			lpRect: dock rectangle where bar to be docked
**
**   OUTPUT :   None
**   RETURN:    
**
**********************************************************************/
void CNCLDockBar::DockControlBar(CControlBar* pBar, LPCRECT lpRect)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pBar);
	ASSERT_KINDOF(CControlBar, pBar);

	CRect rectBar;
	pBar->GetWindowRect(&rectBar);
	if (pBar->m_pDockBar == this && (lpRect == NULL || rectBar == *lpRect))
	{
		// already docked and no change in position
		return;
	}

	// set CBRS_FLOAT_MULTI style if docking bar has it
	if (m_bFloating && (pBar->m_dwDockStyle & CBRS_FLOAT_MULTI))
		m_dwStyle |= CBRS_FLOAT_MULTI;

	m_dwStyle &= ~(CBRS_SIZE_FIXED | CBRS_SIZE_DYNAMIC);
	m_dwStyle |= pBar->m_dwStyle & (CBRS_SIZE_FIXED | CBRS_SIZE_DYNAMIC);

	if (!(m_dwStyle & CBRS_FLOAT_MULTI))
	{
		TCHAR szTitle[_MAX_PATH];
		pBar->GetWindowText(szTitle, _countof(szTitle));
		AfxSetWindowText(m_hWnd, szTitle);
	}

	// align correctly and turn on all borders
	DWORD dwStyle = pBar->GetBarStyle();
	dwStyle &= ~(CBRS_ALIGN_ANY);
	dwStyle |=  (m_dwStyle & CBRS_ALIGN_ANY) | CBRS_BORDER_ANY;

	if (m_bFloating)
		dwStyle |= CBRS_FLOATING;
	else
		dwStyle &= ~CBRS_FLOATING;

	pBar->SetBarStyle(dwStyle);

	// hide first if changing to a new docking site to avoid flashing
	BOOL bShow = FALSE;
	if (pBar->m_pDockBar != this && pBar->IsWindowVisible())
	{
		pBar->SetWindowPos(NULL, 0, 0, 0, 0,
			SWP_NOSIZE|SWP_NOMOVE|SWP_NOZORDER|SWP_NOACTIVATE|SWP_HIDEWINDOW);
		bShow = TRUE;
	}

	int nPos = -1;
	if (lpRect != NULL)
	{
		// insert into appropriate row
		CRect rect(lpRect);
		ScreenToClient(&rect);
		CPoint ptMid(rect.left + rect.Width()/2, rect.top + rect.Height()/2);
		nPos = Insert(pBar, rect, ptMid);

		// position at requested position
		pBar->SetWindowPos(NULL, rect.left, rect.top, rect.Width(),
			rect.Height(), SWP_NOZORDER|SWP_NOACTIVATE|SWP_NOCOPYBITS);
	}
	else
	{
		// always add on current row, then create new one
		m_arrBars.Add(pBar);
		m_arrBars.Add(NULL);

		// align off the edge initially
		pBar->SetWindowPos(NULL, -2, -2, 0, 0,
			SWP_NOSIZE|SWP_NOZORDER|SWP_NOACTIVATE|SWP_NOCOPYBITS);
	}

	// attach it to the docking site
	if (pBar->GetParent() != this)
		pBar->SetParent(this);
	if (pBar->m_pDockBar == this)
		((CNCLDockBar*)(pBar->m_pDockBar))->RemoveControlBar(pBar, nPos);
	else if (pBar->m_pDockBar != NULL)
		((CNCLDockBar*)(pBar->m_pDockBar))->RemoveControlBar(pBar, -1, m_bFloating && !pBar->m_pDockBar->m_bFloating);
	pBar->m_pDockBar = this;

	if (bShow)
	{
		ASSERT(!pBar->IsWindowVisible());
		pBar->SetWindowPos(NULL, 0, 0, 0, 0,
			SWP_NOSIZE|SWP_NOMOVE|SWP_NOZORDER|SWP_NOACTIVATE|SWP_SHOWWINDOW);
	}

	// remove any place holder for pBar in this dockbar
	RemovePlaceHolder(pBar);

	// get parent frame for recalc layout
	CFrameWnd* pFrameWnd = GetDockingFrame();
	pFrameWnd->DelayRecalcLayout();
}

/***********************************************************************
**
**   FUNCTION: ReDockControlBar(CControlBar* pBar, LPCRECT lpRect)
**
**       Redock the control bar
**		This function is overwrite CDockBar::ReDockControlBar with little change
**		
**
**   INPUT:  pBar: control bar to be docked
**			lpRect: dock rectangle where bar to be docked
**
**   OUTPUT :   None
**   RETURN:    
**
**********************************************************************/
void CNCLDockBar::ReDockControlBar(CControlBar* pBar, LPCRECT lpRect)
{
	ASSERT_VALID(this);
	ASSERT_VALID(pBar);
	ASSERT_KINDOF(CControlBar, pBar);
	ASSERT(pBar->m_pDockBar != this); // can't redock here if already docked here

	CRect rectBar;
	pBar->GetWindowRect(&rectBar);
	if (pBar->m_pDockBar == this && (lpRect == NULL || rectBar == *lpRect))
	{
		// already docked and no change in position
		return;
	}

	// set CBRS_FLOAT_MULTI style if docking bar has it
	if (m_bFloating && (pBar->m_dwDockStyle & CBRS_FLOAT_MULTI))
		m_dwStyle |= CBRS_FLOAT_MULTI;

	m_dwStyle &= ~(CBRS_SIZE_FIXED | CBRS_SIZE_DYNAMIC);
	m_dwStyle |= pBar->m_dwStyle & (CBRS_SIZE_FIXED | CBRS_SIZE_DYNAMIC);

	if (!(m_dwStyle & CBRS_FLOAT_MULTI))
	{
		TCHAR szTitle[_MAX_PATH];
		pBar->GetWindowText(szTitle, _countof(szTitle));
		AfxSetWindowText(m_hWnd, szTitle);
	}

	// align correctly and turn on all borders
	DWORD dwStyle = pBar->GetBarStyle();
	dwStyle &= ~(CBRS_ALIGN_ANY);
	dwStyle |=  (m_dwStyle & CBRS_ALIGN_ANY) | CBRS_BORDER_ANY;

	if (m_bFloating)
		dwStyle |= CBRS_FLOATING;
	else
		dwStyle &= ~CBRS_FLOATING;

	pBar->SetBarStyle(dwStyle);

	int nPos = FindBar((CControlBar*)_AfxGetDlgCtrlID(pBar->m_hWnd));
	if (nPos > 0)
		m_arrBars[nPos] = pBar;

	if (lpRect != NULL)
	{
		CRect rect(lpRect);
		ScreenToClient(&rect);

		if (nPos < 1)
		{
			CPoint ptMid(rect.left + rect.Width()/2, rect.top + rect.Height()/2);
			nPos = Insert(pBar, rect, ptMid);
		}

		// position at requested position
		pBar->SetWindowPos(NULL, rect.left, rect.top, rect.Width(),
			rect.Height(), SWP_NOZORDER|SWP_NOACTIVATE|SWP_NOCOPYBITS);
	}
	else
	{
		if (nPos < 1)
		{
			// always add on current row, then create new one
			m_arrBars.Add(pBar);
			m_arrBars.Add(NULL);
		}

		// align off the edge initially
		pBar->SetWindowPos(NULL, -2, -2, 0, 0,
			SWP_NOSIZE|SWP_NOZORDER|SWP_NOACTIVATE|SWP_NOCOPYBITS);
	}

	// attach it to the docking site
	if (pBar->GetParent() != this)
		pBar->SetParent(this);
	if (pBar->m_pDockBar != NULL)
		((CNCLDockBar*)(pBar->m_pDockBar))->RemoveControlBar(pBar);
	pBar->m_pDockBar = this;

	// get parent frame for recalc layout
	CFrameWnd* pFrameWnd = GetDockingFrame();
	pFrameWnd->DelayRecalcLayout();
}
/***********************************************************************
**
**   FUNCTION: Get_BarRect(CRect *rect, int *newline, int flag)
**
**       Get the first/last control bar's Rect 
**		
**
**   INPUT: flag: 1: first bar rect
**					2: last bar rect
**
**   OUTPUT :
**			newline: 1: the last bar is start at new row
**						in this case, return the whole dockbar rect
**					0: return first/last control bar rect 
**						inside the dockbar
**			rect: position and size of controlbar
**   RETURN:    
**
**********************************************************************/
void CNCLDockBar::Get_BarRect(CRect *rect, int *newline, int flag)
{
	int nPos = 0;
	int nPosInsAfter = 0;
	int nWidth = 0;
	int nTotalWidth = 0;
	BOOL bHorz = m_dwStyle & CBRS_ORIENT_HORZ;

	*newline = 1;
	for (nPos = 0; nPos < m_arrBars.GetSize(); nPos++)
	{	
		CControlBar* pBar = GetDockedControlBar(nPos);
		if (pBar != NULL && pBar->IsVisible())
		{
			pBar->GetWindowRect(rect);
			*newline = 0;
			if (flag==1)
				return;
		}
	}
	if (*newline==1)
		GetWindowRect(rect);
}


/***********************************************************************
**
**   FUNCTION: uw_getdock_barrect(CRect *rect, int *newline, int flag, int bflag)
**       Get the first/last control bar's Rect of the particular dockbar
**		
**
**   INPUT: flag: direction of the dockbar
**					1: Top		2: Bottom
**					3: Left		4: Right
**			bflag: 1: first bar rect
**					2: last bar rect
**
**   OUTPUT :
**			newline: 1: the last bar is start at new row
**						in this case, return the whole dockbar rect
**					0: return first/last control bar rect 
**						inside the dockbar
**			rect: position and size of controlbar
**   RETURN:    
**
**********************************************************************/
void uw_getdock_barrect(CRect *rect, int *newline, int flag, int bflag)
{
	CNCLDockBar* pDockBar = NULL;
	if (flag==1)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_TOP);
	}
	else if (flag==2)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_BOTTOM);
	}
	else if (flag==3)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_LEFT);
	}
	else if (flag==4)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_RIGHT);
	}
	pDockBar->Get_BarRect(rect, newline, bflag);
}	

/***********************************************************************
**
**   FUNCTION: uw_nthide_bars(int bar_pos)
**       Get the first/last control bar's Rect of the particular dockbar
**		
**
**   INPUT: bar_pos: direction of the dockbar
**					1: Top		2: Bottom
**					3: Left		4: Right
**
**   OUTPUT : None
**   RETURN:   None 
**
**********************************************************************/
void uw_nthide_bars(int bar_pos)
{
	CNCLDockBar* pDockBar = NULL;
	if (bar_pos==1)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_TOP);
	}
	else if (bar_pos==2)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_BOTTOM);
	}
	else if (bar_pos==3)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_LEFT);
	}
	else if (bar_pos==4)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_RIGHT);
	}

	CControlBar* pBar = NULL;
	for (int nPos = 0; nPos < pDockBar->m_arrBars.GetSize(); nPos++)
	{
		pBar = (CControlBar* )pDockBar->GetDockedControlBar(nPos);

		if ((pBar != NULL)&&(pBar->IsVisible()))
		{
			NCL_MainFrame->ShowControlBar(pBar, FALSE, TRUE);
			if (pBar->IsKindOf(RUNTIME_CLASS(CNCLToolBar)))
			{
				((CNCLToolBar*)pBar)->m_visible = 0;
			}
			else if (pBar->IsKindOf(RUNTIME_CLASS(CNCLDialogBar)))
			{
				((CNCLDialogBar*)pBar)->m_visible = 0;
			}
		}
	}
}

/***********************************************************************
**
**   FUNCTION: uw_ntget_bars(int bar_pos, int indx)
**       Get the indx'th control bar of the particular dockbar
**		
**
**   INPUT: bar_pos: direction of the dockbar
**					1: Top		2: Bottom
**					3: Left		4: Right
**			indx: control bar position inx
**
**   OUTPUT :None
**   RETURN: None   
**
**********************************************************************/
CControlBar* uw_ntget_bars(int bar_pos, int indx)
{
	CNCLDockBar* pDockBar = NULL;
	if (bar_pos==1)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_TOP);
	}
	else if (bar_pos==2)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_BOTTOM);
	}
	else if (bar_pos==3)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_LEFT);
	}
	else if (bar_pos==4)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_RIGHT);
	}

	CControlBar* pBar = NULL;
	int bar_indx = 0;
	for (int nPos = 0; nPos < pDockBar->m_arrBars.GetSize(); nPos++)
	{
		pBar = pDockBar->GetDockedControlBar(nPos);

		if ((pBar != NULL)&&(pBar->IsVisible()))
		{
			if(pBar->IsKindOf(RUNTIME_CLASS(CControlBar)))
			{
				if (bar_indx==indx)
				{
					return pBar;
				}
				bar_indx++;
			}
		}
	}
	return NULL;
}
/***********************************************************************
**
**   FUNCTION: AddMenuBar(int bar_pos, int indx)
**       Add a MenuBar (it is not used for now since the function called for drag and drop
**				into a dockbar and have some problems). We still keep it for now
**				but should never got to here
**		
**
**   INPUT: none
**
**   OUTPUT :None
**   RETURN: None   
**
**********************************************************************/
void CNCLDockBar::AddMenuBar(CPoint pt, char* input_text)
{
	m_iPoint = pt;
	strcpy_s(m_iText, 100, input_text);
	CPoint pt1;
	PostMessage(WM_COMMAND, UW_ADDNEW_MENUBAR);
	_sleep(1);
}

/***********************************************************************
**
**   FUNCTION: OnAddMenuBar
**       Add a MenuBar (it is not used for now since the function called for drag and drop
**				into a dockbar and have some problems). We still keep it for now
**				but should never got to here
**
**   INPUT: none
**
**   OUTPUT :None
**   RETURN: None   
**
**********************************************************************/
void CNCLDockBar::OnAddMenuBar()
{
	int menunum1, itemnum1;
	char menudata[100];
	int menu_desgn = 0;
/*
	strcpy_s(menudata, 100, m_iText);
	if (strncmp(menudata, "CToolmenu", 9)==0)
		sscanf_s (menudata, "CToolmenu %d, %d", &menunum1, &itemnum1);
	else if (strncmp(menudata, "CNCLMenu", 8)==0)
		sscanf_s (menudata, "CNCLMenu %d, %d", &menunum1, &itemnum1);
	else if (strcmp(menudata, "MenuDesign")==0)
	{
		menu_desgn = 1;
	}
	else
/*
.....should not come here
*/
//		return;
menunum1 = itemnum1 = 1;

//	CNCLDockBar* pDockBar = NULL;
	int bar_pos = 0;
//	CControlBar* pBar = NULL;
	CRect rectBar;
	int bar_indx = 0;
/*
start:;
	bar_pos++;
	if (bar_pos==1)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_TOP);
	}
	else if (bar_pos==2)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_BOTTOM);
	}
	else if (bar_pos==3)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_LEFT);
	}
	else if (bar_pos==4)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_RIGHT);
	}
	if (pDockBar==this)
	{
		for (int nPos = 0; nPos < pDockBar->m_arrBars.GetSize(); nPos++)
		{
			pBar = pDockBar->GetDockedControlBar(nPos);

			if ((pBar != NULL)&&(pBar->IsVisible()))
			{
				if(pBar->IsKindOf(RUNTIME_CLASS(CControlBar)))
				{
/*
......get the rect of the control bar
*/
/*					pBar->GetWindowRect(&rectBar);
					if (rectBar.PtInRect(m_iPoint))
					{
						break;
					}
					bar_indx++;
				}
			}
		}
		goto done;
	}
	if (bar_pos<4) 
		goto start;
	return;
*/
done:;
	NCL_MainFrame->m_addmenu = -1;
	NCL_MainFrame->m_additem = -1;
	NCL_MainFrame->m_remove_menu = menunum1;
	NCL_MainFrame->m_remove_item = itemnum1;
	NCL_MainFrame->m_menu_desgn = menu_desgn;

	char tempstr[100];
	bar_indx = 1; bar_pos = 2;
	NCL_MainFrame->m_add_menupos[0] = bar_indx;
	if (NCL_MainFrame->m_add_menupos[0]!=bar_indx)
		return;
		
	sprintf_s(tempstr, 100, "%d", bar_indx);
	sprintf(tempstr, "%d", NCL_MainFrame->m_add_menupos[0]);

	NCL_MainFrame->m_add_menupos[1] = -1;
	if (NCL_MainFrame->m_add_menupos[1]!=-1)
		return;
	NCL_MainFrame->m_add_menupos[2] = bar_pos;
	NCL_MainFrame->m_add_menupos[3] = m_iPoint.x;
	NCL_MainFrame->m_add_menupos[4] = m_iPoint.y;
	if (NCL_MainFrame->m_add_menupos[2]!=bar_pos)
		return;
	if (NCL_MainFrame->m_add_menupos[2]!=2)
		return;
/*	UW_drag_pos[0] = bar_indx;
	UW_drag_pos[1] = -1;
	UW_drag_pos[2] = bar_pos;
	UW_drag_pos[3] = m_iPoint.x;
	UW_drag_pos[4] = m_iPoint.y;
*/
	CFrameWnd* pFrame = GetParentFrame();
	NCL_MainFrame->PostMessage(WM_COMMAND, UW_ADDNEW_MENUBAR);
	_sleep(1);
	return;
}

/***********************************************************************
**
**   FUNCTION: uw_ntget_ctlbar_type(CControlBar* chkbar)
**       Get the control bar position (TOP, BOTTOM, LEFT,RIGHT)
**
**   INPUT: none
**
**   OUTPUT :None
**   RETURN: None   
**
**********************************************************************/
int uw_ntget_ctlbar_type(CControlBar* chkbar)
{
	CNCLDockBar* pDockBar = NULL;
	int bar_pos = 0;

start:;
	bar_pos++;
	if (bar_pos==1)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_TOP);
	}
	else if (bar_pos==2)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_BOTTOM);
	}
	else if (bar_pos==3)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_LEFT);
	}
	else if (bar_pos==4)
	{
		pDockBar = (CNCLDockBar*)NCL_MainFrame->GetControlBar(AFX_IDW_DOCKBAR_RIGHT);
	}
	CControlBar* pBar = NULL;
	int bar_indx = 0;
	for (int nPos = 0; nPos < pDockBar->m_arrBars.GetSize(); nPos++)
	{
		pBar = pDockBar->GetDockedControlBar(nPos);
		if ((pBar != NULL)&&(pBar->IsVisible()))
		{
			if(pBar->IsKindOf(RUNTIME_CLASS(CControlBar)))
			{
				if (pBar==chkbar)
				{
					return bar_pos;
				}
				bar_indx++;
			}
		}
	}
	if (bar_pos<4) 
		goto start;
	return -1;
}
