/********************************************************************* 
**  NAME:  wsntgettxt2.cpp
**
**			popup text window functions
**			implementation of CNCLGetText2 class functions
**	CONTAINS: CNCLGetText class functions
**			all functions declared in wsnttxtwin2.h
**
**    COPYRIGHT 2015 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntgettxt2.cpp , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**			10/27/16 , 11:17:23
*********************************************************************/
#include "wsntstdafx.h"
#include "wsntgettxt2.h"

/***********************************************************************
**
**   FUNCTION: CNCLGetText(CWnd* pParentWnd, char *title)
**
**              Constructor of class CNCLGetText
**
**   INPUT:  pParentWnd: parent window
**			title: title of text window
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLGetText2::CNCLGetText2(CWnd* pParentWnd) : CDialog(CNCLGetText2::IDD, pParentWnd)
{
	m_parent = pParentWnd;
}

/***********************************************************************
**
**   FUNCTION: ~CNCLGetText()
**
**              Destructor of class CNCLGetText
**			
**   INPUT:  none
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLGetText2::~CNCLGetText2()
{
}


void CNCLGetText2::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CNCLTextWin)
	//}}AFX_DATA_MAP
}
LRESULT CNCLGetText2::KillFocusWin(WPARAM wparm, LPARAM lparm)
{
	return 0;
}
BEGIN_MESSAGE_MAP(CNCLGetText2, CDialog)
	//{{AFX_MSG_MAP(CNCLGetText2)
		ON_MESSAGE(WM_KILLFOCUS, KillFocusWin)
	ON_WM_LBUTTONUP()
	ON_WM_MBUTTONUP()
	ON_WM_RBUTTONUP()
	ON_WM_KEYUP()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

void CNCLGetText2::OnLButtonUp(UINT nFlags, CPoint point)
{
	CDialog::OnLButtonUp(nFlags, point);
/*
.....check if the point with the window
*/
	CRect rect;
	GetClientRect(&rect);
	if (rect.PtInRect(point))
		return;
	ReleaseCapture();
	PostMessage(WM_COMMAND, IDCANCEL);
}
void CNCLGetText2::OnMButtonUp(UINT nFlags, CPoint point)
{
	CDialog::OnMButtonUp(nFlags, point);
/*
.....check if the point with the window
*/
	CRect rect;
	GetClientRect(&rect);
	if (rect.PtInRect(point))
		return;
	ReleaseCapture();
	PostMessage(WM_COMMAND, IDCANCEL);
}
void CNCLGetText2::OnRButtonUp(UINT nFlags, CPoint point)
{
	CDialog::OnRButtonUp(nFlags, point);
/*
.....check if the point with the window
*/
	CRect rect;
	GetClientRect(&rect);
	if (rect.PtInRect(point))
		return;
	ReleaseCapture();
	PostMessage(WM_COMMAND, IDCANCEL);
}
void CNCLGetText2::OnKeyUp(UINT nChar, UINT nRepCnt, UINT nFlags)
{
	CDialog::OnKeyUp(nChar, nRepCnt, nFlags);
/*
.....check if the point with the window
*/
	CRect rect;
	GetWindowRect(&rect);
	CPoint point;
	GetCursorPos(&point);
	if (rect.PtInRect(point))
		return;
	ReleaseCapture();
	PostMessage(WM_COMMAND, IDCANCEL);
}

/***********************************************************************
c
c   SUBROUTINE:  OnSize( UINT nType, int cx, int cy )
c
c   FUNCTION:  This function called after resize window
c
c   INPUT:  ntype: Specifies the type of resizing 
c			cx, cy: new width and height
c   OUTPUT: none
c
c***********************************************************************
*/
void CNCLGetText2::OnSize( UINT nType, int cx, int cy )
{
	CWnd::OnSize(nType, cx, cy );
	return;
}
/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog
c
c   FUNCTION:  This function initialize window text
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
BOOL CNCLGetText2::OnInitDialog()
{
	CDialog::OnInitDialog();
	CRect rect, new_rect;
	GetWindowRect(&rect);

	int stat = m_txtfont.CreatePointFont (80, "MS Sans Serif");
	SetFont(&m_txtfont);
	
	((CEdit*)GetDlgItem(IDC_TEXTWINEDIT))->SetFont(&m_txtfont);
	((CWnd*)GetDlgItem(IDC_TEXTWINLABEL))->SetFont(&m_txtfont);

	CClientDC dc(this);
	CSize sizeText;
	CFont* savfont = dc.SelectObject(&m_txtfont);
	if (strlen(m_prompt)>0)
	{
		sizeText = dc.GetTextExtent(m_prompt);
	}
	else
	{
		sizeText = dc.GetTextExtent("l", 1);
	}
	int wid1, wid2, hgt;
	wid1 = sizeText.cx;
	hgt = sizeText.cy;

	if (hgt<m_tsize[1])
		hgt = m_tsize[1];

	CRect rect1, rect2;
	rect1.left = 1;
	rect1.top = 1;
	rect1.right = rect1.left + wid1+1;
	rect1.bottom = rect1.top + hgt;
	((CEdit*)GetDlgItem(IDC_TEXTWINLABEL))->MoveWindow(rect1);
	((CEdit*)GetDlgItem(IDC_TEXTWINLABEL))->SetWindowText(m_prompt);

	rect2.left = rect1.right + 3;
	rect2.top = rect1.top;
	rect2.right = rect2.left + m_tsize[0];
	rect2.bottom = rect2.top + m_tsize[1];
	((CWnd*)GetDlgItem(IDC_TEXTWINEDIT))->MoveWindow(rect2);
	((CWnd*)GetDlgItem(IDC_TEXTWINEDIT))->SetWindowText(m_text);

	int cx = wid1 + m_tsize[0] + 8;

	new_rect.left = m_pos.x;
	new_rect.top = m_pos.y;
	new_rect.right = new_rect.left + cx;
	new_rect.bottom = new_rect.top + hgt + 4;
	MoveWindow(new_rect);
	SetCapture();
	return false;
}	

void CNCLGetText2::OnOK()
{
	((CEdit*)GetDlgItem(IDC_TEXTWINEDIT))->GetWindowText(m_text);
	ReleaseCapture();
	CDialog::OnOK();
}
