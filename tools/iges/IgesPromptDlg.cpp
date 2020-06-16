/************************************************************************
c
c   FILE NAME: IgesPromptDlg.cpp
c
c	 CONTAINS: 
c		Functions for the class IgesPromptDlg
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       IgesPromptDlg.cpp , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:12:56               
c
c**********************************************************************
*/
// IgesPromptDlg.cpp : implementation file
//

#include "stdafx.h"
#include "iges.h"
#include "IgesPromptDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CIgesPromptDlg dialog


CIgesPromptDlg::CIgesPromptDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CIgesPromptDlg::IDD, pParent)
{
	m_cols = 80;
	m_lns = 1;
	m_title = "Prompt Dialog";
	m_msg = "Input Text: ";
	m_input = "";
	//{{AFX_DATA_INIT(CIgesPromptDlg)
	//}}AFX_DATA_INIT
}


void CIgesPromptDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CIgesPromptDlg)
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CIgesPromptDlg, CDialog)
	//{{AFX_MSG_MAP(CIgesPromptDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CIgesPromptDlg message handlers

/***********************************************************************
c
c   FUNCTION: OnInitDialog() 
c
c		This function Set the initialized parameter into dialog
c       This member function is called in response to 
c		the WM_INITDIALOG message. This message is sent to 
c		the dialog box during the Create, CreateIndirect, 
c		or DoModal calls, which occur immediately before 
c		the dialog box is displayed. 
c   INPUT:  None.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CIgesPromptDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	GetDlgItem(IDC_PROMPT_MSG)->SetWindowText(m_msg);
	GetDlgItem(IDC_INPUT)->SetWindowText(m_input);
	SetWindowText(m_title);
	return TRUE;  
}

/*********************************************************************
**    I_FUNCTION     : SetDlgValue(int cols, int lns, char *title, char *msg, char*input)
**			Set Dialog values
**    PARAMETERS
**       INPUT  :
**			title: list box title
**			msg: prompt message for list box
**			cols: columns of text field
**			rows: rows of text field
**			input: initial text value
**       OUTPUT :
**			none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CIgesPromptDlg::SetDlgValue(int cols, int lns, char *title, char *msg, char*input)
{
	m_cols = cols;
	m_lns = lns;
	m_msg = msg;
	m_input = input;
	m_title = title;
}

/*********************************************************************
**    I_FUNCTION     : GetInput(char *input, int maxnum)
**
**			Get Current input text value
**    PARAMETERS
**       INPUT  :
**          num: maxinum number of charaters for input
**       OUTPUT :
**			input: text input
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CIgesPromptDlg::GetInput(char *input, int maxnum)
{
	char *temp = m_input.GetBuffer(maxnum);
	strcpy (input, temp);
}

void CIgesPromptDlg::OnOK() 
{
	GetDlgItem(IDC_INPUT)->GetWindowText(m_input);
	CDialog::OnOK();
}
