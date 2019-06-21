/************************************************************************
**
**   FILE NAME: wsntformfrm.cpp
**
**	 Description - Functions and implementations for
**		CNCLFormFrm class
**
**	 CONTAINS: 
**		class functions of CNCLFormFrm class
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntformfrm.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:25
***********************************************************************
*/
#include "stdafx.h"
#include "wsntformfrm.h"
#include "UxTheme.h"
#include "wsntfrmview.h"
#include "wsntfrmMview.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern CFrameWnd *UW_Dform_frame;
/////////////////////////////////////////////////////////////////////////////
// CNCLFormFrm

IMPLEMENT_DYNCREATE(CNCLFormFrm, CFrameWnd)

/***********************************************************************
**   FUNCTION: CNCLFormFrm
**		Constructor of class CNCLFormFrm
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLFormFrm::CNCLFormFrm()
{
	m_type = 0;
}

CNCLFormFrm::~CNCLFormFrm()
{
}


BEGIN_MESSAGE_MAP(CNCLFormFrm, CFrameWnd)
	//{{AFX_MSG_MAP(CNCLFormFrm)
//	ON_COMMAND(ID_FILE_NEW, OnFileNew)
	ON_WM_SIZE()
	ON_WM_MOVE()
	ON_WM_NCHITTEST()
//	ON_WM_CLOSE()
	ON_WM_EXITSIZEMOVE()
	ON_WM_ENTERSIZEMOVE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   FUNCTION: PreTranslateMessage(MSG* pMsg) 
c
c       translate window messages before they are dispatch
c
c   INPUT:  pMsg   Points to a MSG structure that contains the 
c					message to process.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CNCLFormFrm::PreTranslateMessage(MSG* msg)
{
	return CFrameWnd::PreTranslateMessage(msg);
}

/***********************************************************************
**
**   FUNCTION: OnSize(UINT nType, int cx, int cy) 
**
**		call this member function 
**		after the window's size has changed. 
**   
**	 INPUT:  nType:   Specifies the type of resizing 
**					requested. This parameter can 
**					be one of the following values:
**					SIZE_MAXIMIZED   Window has been maximized.
**					SIZE_MINIMIZED   Window has been minimized.
**					SIZE_RESTORED   Window has been resized, but neither 
**									SIZE_MINIMIZED nor SIZE_MAXIMIZED applies.
**					SIZE_MAXHIDE   Message is sent to all pop-up windows when some other window is maximized.
**					SIZE_MAXSHOW   Message is sent to all pop-up windows when some other window has been restored to its former size.
**			  cx:   Specifies the new width of the client area.
**			  cy:   Specifies the new height of the client area.
**
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLFormFrm::OnSize(UINT nType, int cx, int cy) 
{
	CFrameWnd::OnSize(nType, cx, cy);	
	CRect rect, frect, vrect;
	if (UW_Dform_frame!=NULL)
	{
		((CNCLFdsnFrame*)UW_Dform_frame)->SetSizeText();
/*
.....removed for now, these code will case the frame window shift on the scroll-view
.....leave everything handled by frame/split view itselves
*/
/*
		CNCLFormMView *mview = (CNCLFormMView *)(((CNCLFdsnFrame*)UW_Dform_frame)->m_mview);
		mview->m_frame->GetWindowRect(&frect);
		vrect.left = frect.left - 10;
		vrect.right = frect.right + 10;
		vrect.top = frect.top - 10;
		vrect.bottom = frect.bottom + 10;
		((CNCLFdsnFrame*)UW_Dform_frame)->MoveMViewWindow(vrect);
		mview->SetScrollSizes(MM_TEXT, CSize(frect.Width()+20, frect.Height()+20));
*/	
	}
}

void CNCLFormFrm::OnEnterSizeMove()
{
	CNCLFormMView *mview = (CNCLFormMView *)(((CNCLFdsnFrame*)UW_Dform_frame)->m_mview);
	m_scrpos = mview->GetScrollPosition();
	CFrameWnd::OnEnterSizeMove();
}

void CNCLFormFrm::OnExitSizeMove()
{
	CFrameWnd::OnExitSizeMove();
/*	CNCLFormMView *mview = (CNCLFormMView *)(((CNCLFdsnFrame*)UW_Dform_frame)->m_mview);
	CRect frect, mrect;
	int mode;
	SIZE sizeTotal, sizePage, sizeLine;
	mview->GetDeviceScrollSizes(mode, sizeTotal, sizePage, sizeLine);
	GetWindowRect(&frect);
	mview->ScreenToClient(&frect);
	mview->GetWindowRect(&mrect);
	CPoint scrpos;
	scrpos.x = sizeTotal.cx - mrect.Width() - 20;
	if (scrpos.x<0)
		scrpos.x = 0;
	scrpos.y = sizeTotal.cy - mrect.Height() - 20;
	if (scrpos.y<0)
		scrpos.y = 0;
	int nScrollX, nScrollY;
	if (scrpos.x>0)
	{
		nScrollX = m_scrpos.x - scrpos.x - 10;
	}
	else
	{
		nScrollX = 10 - frect.left;
	}
	if (scrpos.y>0)
	{
		nScrollY = m_scrpos.y - scrpos.y - 10;
	}
	else
	{
		nScrollY = 10 - frect.top;
	}
	mview->ScrollWindow(nScrollX, 0);
	mview->ScrollWindow(0, nScrollY);
*/
	CRect frect, vrect;
	CNCLFormMView *mview = (CNCLFormMView *)(((CNCLFdsnFrame*)UW_Dform_frame)->m_mview);
	mview->m_frame->GetWindowRect(&frect);
	vrect.left = frect.left - 10;
	vrect.right = frect.right + 10;
	vrect.top = frect.top - 10;
	vrect.bottom = frect.bottom + 10;
	((CNCLFdsnFrame*)UW_Dform_frame)->MoveMViewWindow(vrect);
	mview->SetScrollSizes(MM_TEXT, CSize(frect.Width()+20, frect.Height()+20));
	mview->ScrollToPosition(m_scrpos);
	mview->m_frame->RedrawWindow();
}
/***********************************************************************
**
**   FUNCTION: OnMove(int x, int y)
**
**		The framework calls this member function 
**		after the window's posiiton has changed. 
**   
**	 INPUT:  
**			  x:   Specifies the new x of position.
**			  y:   Specifies the new y of position.
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CNCLFormFrm::OnMove(int x, int y)
{
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
LRESULT CNCLFormFrm::OnNcHitTest(CPoint point) 
{
	CRect rect;
	DWORD hitTest = CFrameWnd::OnNcHitTest(point);
	if (m_type==0)
	{
		switch (hitTest)
		{
		case HTSYSMENU:
		case HTMINBUTTON:
		case HTMAXBUTTON:
		case HTCLOSE:
		case HTCAPTION:
		case HTBOTTOMLEFT:
		case HTLEFT:
		case HTTOPLEFT:
		case HTTOP:
			hitTest = HTNOWHERE;
		}
	}
	else
	{
		switch (hitTest)
		{
		case HTSYSMENU:
		case HTMINBUTTON:
		case HTMAXBUTTON:
		case HTCAPTION:
		case HTBOTTOMRIGHT:
		case HTRIGHT:
		case HTTOPRIGHT:
			hitTest = HTNOWHERE;
			break;
		case HTBOTTOMLEFT:
		case HTLEFT:
		case HTTOPLEFT:
//			int status = ((CNCLDSNForm*)m_parent)->IsPFormResizable(point);
//			if (status==0)
				hitTest = HTNOWHERE;
		}
	}
	return hitTest;
}

/***********************************************************************
c
c   FUNCTION: OnClose()
c
c           callback for close frame window.
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLFormFrm::OnClose()
{
}
/*
void CNCLFormFrm::OnFileNew() 
{
	CNCLFormDocTemp * pTemplate = (CNCLFormDocTemp *) GetActiveDocument()->GetDocTemplate();
	pTemplate->OpenDocumentFile(NULL);
}
*/
/***********************************************************************
**
**   FUNCTION: OnCreate(LPCREATESTRUCT lpCreateStruct) 
**
**		Override this member function to perform any needed 
**		initialization of the class.
**   
**		INPUT:  LPCREATESTRUCT lpCreateStruct: contains copies of 
**						the parameters used to create the window.
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
int CNCLFormFrm::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;
	return 0;
}
