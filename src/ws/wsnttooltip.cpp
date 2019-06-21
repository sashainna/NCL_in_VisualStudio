/********************************************************************* 
**  NAME:  wsnttooltip.cpp
**
**			NCL Toolip class
**
**	CONTAINS: NCL Toolip class functions and structure
**			implimentation
**
**    COPYRIGHT 2004 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsnttooltip.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:30
*********************************************************************/
#include "stdafx.h"
#include "wsnttooltip.h"

IMPLEMENT_DYNAMIC( CNCLToolTip, CWnd );

BEGIN_MESSAGE_MAP(CNCLToolTip, CWnd)
	//{{AFX_MSG_MAP(CNCLToolTip)
	ON_WM_PAINT()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
**
**   FUNCTION: CNCLToolTip()
**
**              Constructor of class CNCLToolTip
**
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLToolTip::CNCLToolTip()
{
	m_szText = "";
	m_pParentWnd = NULL;
	m_bShowStatus = FALSE;
}

/***********************************************************************
**
**   FUNCTION: ~CNCLToolTip()
**
**              Destructor of class CNCLToolTip
**			
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLToolTip::~CNCLToolTip()
{
}

/***********************************************************************
**
**   FUNCTION: Create(CWnd* pParentWnd)
**
**              Create a tooltip window
**			
**   INPUT:  pParentWnd: parent window
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLToolTip::Create(CWnd* pParentWnd)
{
	ASSERT(this != NULL );
	ASSERT(pParentWnd != NULL);

	m_pParentWnd = pParentWnd;
	
	m_font.CreateFont(15, 0, 0, 0, FW_REGULAR, 0, 0, 0, 0, 0, 0, 0, 0, 
		"MS Sans Serif");
	CRect rectInitialSize(0,0,0,0);
	return CreateEx(WS_EX_LEFT, AfxRegisterWndClass(NULL),"ToolTip",
		WS_POPUP/* |  WS_CHILD | WS_CLIPSIBLINGS*/,
		rectInitialSize,pParentWnd, NULL, NULL);
}

/***********************************************************************
**
**   FUNCTION: SetText(const CString& rsText)
**
**            Set a tooltip window text
**			
**   INPUT:  rsText: tooltip window text
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolTip::SetText(const CString& rsText)
{
	ASSERT(this != NULL );
	m_szText = rsText;
	m_szText.TrimRight();
}

/***********************************************************************
**
**   FUNCTION: Show(const CPoint& rCurrentPoint)
**
**            Show a tooltip window
**			
**   INPUT:  rCurrentPoint: tooltip window position
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
BOOL CNCLToolTip::Show(const CPoint& rCurrentPoint)
{
	ASSERT(this != NULL );
	ASSERT(m_hWnd != NULL );

//	if ( m_szText.IsEmpty() || m_bShowStatus)
	if ( m_szText.IsEmpty())
		return FALSE;

	m_ptCurrent = rCurrentPoint;
	m_bShowStatus = TRUE;
	DisplayToolTip(rCurrentPoint);
	return TRUE;
}

/***********************************************************************
**
**   FUNCTION: Close()
**
**            close a tooltip window
**			
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolTip::Close()
{
	ASSERT(this != NULL );
	ASSERT(m_hWnd != NULL );

	ShowWindow(SW_HIDE); 
	m_bShowStatus = FALSE;
}

/***********************************************************************
**
**   FUNCTION: OnPaint()
**
**            tooltip window paint function
**			
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolTip::OnPaint()
{
	CPaintDC dc(this);

	DisplayToolTip(m_ptCurrent);
}

/***********************************************************************
**
**   FUNCTION: DisplayToolTip(const CPoint& rCurrentPoint)
**
**            display tooltip window function
**			
**   INPUT:  rCurrentPoint: tooltip position
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CNCLToolTip::DisplayToolTip(const CPoint& rCurrentPoint)
{
	CDC* pDC = GetDC();

	CBrush		*pOldBrush;

	CFont *pOldFont;
	pOldFont = pDC->SelectObject(&m_font);
	CSize size = pDC->GetTextExtent(m_szText);
	pDC->LPtoDP(&size);
	CRect rectToolTip(rCurrentPoint.x + 8, rCurrentPoint.y, 
rCurrentPoint.x+size.cx+13, rCurrentPoint.y+size.cy+2);

	pDC->SetBkMode(TRANSPARENT);
	CBrush brushToolTip(GetSysColor(COLOR_INFOBK));
	pOldBrush = pDC->SelectObject(&brushToolTip);

	CPen penBlack(PS_SOLID, 0, COLORREF(RGB(0, 0, 0)));
	CPen* pOldPen = pDC->SelectObject(&penBlack);

	pDC->Rectangle(0,0,rectToolTip.Width(),rectToolTip.Height());

   	pDC->SetTextColor( GetSysColor(COLOR_INFOTEXT) );
	pDC->SetTextAlign(TA_LEFT);
	pDC->TextOut(3,1, m_szText);

	CRect rectWnd = rectToolTip;
	m_pParentWnd->ClientToScreen(rectWnd); 
	CPoint ptToolTipLeft = rectWnd.TopLeft();

	SetWindowPos(&wndTop,ptToolTipLeft.x+1, ptToolTipLeft.y+1, rectWnd.Width(), 
rectWnd.Height(),SWP_SHOWWINDOW|SWP_NOOWNERZORDER|SWP_NOACTIVATE);

	pDC->SelectObject(pOldBrush);
	pDC->SelectObject(pOldPen);
	pDC->SelectObject(pOldFont);

	ReleaseDC(pDC);
}


