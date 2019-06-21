/********************************************************************* 
**  NAME:  wsntgettxt.cpp
**
**			Native WinNT edit text scrolling window functions
**			implementation of CNCLGetText class functions
**	CONTAINS: CNCLGetText class functions
**			all functions declared in wsnttxtwin.h
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntgettxt.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:26
*********************************************************************/
#include "wsntstdafx.h"
#include "wsntgettxt.h"
#include "lcom.h"

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
CNCLGetText::CNCLGetText(CWnd* pParentWnd, char *title) : CDialog(CNCLGetText::IDD, pParentWnd)
{
	if (title!=NULL)
		m_title = title;
	else
		m_title = "";
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
CNCLGetText::~CNCLGetText()
{
}


void CNCLGetText::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CNCLTextWin)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CNCLGetText, CDialog)
	//{{AFX_MSG_MAP(CNCLGetText)
		ON_WM_SIZE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

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
void CNCLGetText::OnSize( UINT nType, int cx, int cy )
{
	CWnd::OnSize(nType, cx, cy );

	CRect windowRect;
	GetClientRect(windowRect);

	windowRect.bottom -= 30;
	CWnd* pChildWnd = GetDlgItem(IDC_TEXTWINEDIT);
	pChildWnd->MoveWindow(windowRect);
	
	windowRect.bottom += 26;
	windowRect.top = windowRect.bottom - 22;
	windowRect.right = cx/2-1;
	pChildWnd = GetDlgItem(IDOK);
	pChildWnd->MoveWindow(windowRect);
	
	windowRect.left = cx/2+1;
	windowRect.right = cx;
	pChildWnd = GetDlgItem(IDCANCEL);
	pChildWnd->MoveWindow(windowRect);
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
BOOL CNCLGetText::OnInitDialog()
{
	CDialog::OnInitDialog();
	SetWindowText(m_title);
	((CEdit*)GetDlgItem(IDC_TEXTWINEDIT))->LimitText(1000000);
	char *dsptext;
/*
......replace '\n" with '\r\n'
*/
	int i,j, len = m_text.GetLength();
	dsptext = new char[len+100000];

	for (i=0,j=0; i<len; i++,j++)
	{
		if (m_text.GetAt(i)=='\n')
		{
			dsptext[j] = m_text.GetAt(i);
			dsptext[j++] = '\r';
		}
		dsptext[j] = m_text.GetAt(i);
	}
	dsptext[j] = '\0';
	int p1, p2;
	((CEdit*)GetDlgItem(IDC_TEXTWINEDIT))->SetWindowText(dsptext);
	((CEdit*)GetDlgItem(IDC_TEXTWINEDIT))->SetFocus();
	((CEdit*)GetDlgItem(IDC_TEXTWINEDIT))->GetSel(p1, p2);
	return FALSE;
}	

void CNCLGetText::OnOK()
{
	((CEdit*)GetDlgItem(IDC_TEXTWINEDIT))->GetWindowText(m_text);
	CDialog::OnOK();
}
