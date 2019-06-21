/*********************************************************************
**  NAME:  wsntSliderCtrl.cpp
**
**       Control routine for form Slider Bar Controls.
**
** CONTAINS:
**
**    COPYRIGHT 2015 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       wsntSliderCtrl.cpp , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       08/31/15 , 09:26:15
*********************************************************************/
// wsntSliderCtrl.cpp : implementation file
//

#include "stdafx.h"
#include "wsntsliderctrl.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CNCLSliderCtrl

CNCLSliderCtrl::CNCLSliderCtrl()
{
	m_pBuddyWnd = NULL;
}

CNCLSliderCtrl::~CNCLSliderCtrl()
{
}


BEGIN_MESSAGE_MAP(CNCLSliderCtrl, CSliderCtrl)
	//{{AFX_MSG_MAP(CNCLSliderCtrl)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNCLSliderCtrl message handlers

HWND CNCLSliderCtrl::SetBuddy(CWnd *pBuddyWnd)
{
	ASSERT( ::IsWindow(pBuddyWnd->m_hWnd) );
	CRect rect;
	
	m_pBuddyWnd = pBuddyWnd;
	pBuddyWnd->SetOwner( this );
	pBuddyWnd->ShowWindow( SW_SHOW );

	HWND hWnd = (HWND) SendMessage( TBM_SETBUDDY, (WPARAM)FALSE, (LPARAM)pBuddyWnd->m_hWnd );
	ReflectedScrollMessage();	// in order to get the initial display of text
	return pBuddyWnd->m_hWnd;
}
/*
.....we already set the background color, but
.....in order for set color to working, we have to setfocus to it
*/
/*
void CNCLSliderCtrl::SetBkColor(COLORREF c)
{
//	CWnd *w = CWnd::GetFocus();
	SetFocus();
//	if (w!=NULL)
//		w->SetFocus();
	m_parent->SetFocus();
}
*/
/////////////////////////////////////////////////////////////////////////////
// CNCLSliderCtrl message handlers
void CNCLSliderCtrl::ReflectedScrollMessage()
{
	static int first = 1;
/*
.....somehow m_pBuddyWnd is not working when used in formbar
.....so used GetBuddy to get the window
*/
	CWnd *win = GetBuddy(FALSE);
//	if ( m_pBuddyWnd != NULL )
	if (win!= NULL )
	{
		int iPos = GetPos();
		int iMax = GetRangeMax();
		CString text;
		text.Format( "%d", iPos);
//		m_pBuddyWnd->SetWindowText( text );
		win->SetWindowText( text );
		CRect rect, rects;
		int wid;
		DWORD dwStyle = GetStyle();
		GetWindowRect(&rects);
		win->GetWindowRect(&rect);
		if ( dwStyle & TBS_VERT )
		{
			wid = rect.Width();
			rect.left = rects.left;
			rect.right = rect.left + wid;
		}
		else
		{
			wid = rect.Height();
			rect.top = rects.top;
			rect.bottom = rect.top + wid;
		}
		m_parent->ScreenToClient(rect);
		win->MoveWindow(&rect);
	}
}

void CNCLSliderCtrl::SetPos(int nPos)
{
	CSliderCtrl::SetPos( nPos );
	ReflectedScrollMessage();
}
