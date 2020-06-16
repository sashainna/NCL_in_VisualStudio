/************************************************************************
c
c   FILE NAME: ToolPromptDlg.cpp
c
c	 CONTAINS: 
c		Functions for the class ToolPromptDlg
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       ToolPromptDlg.cpp , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       10/12/15 , 17:33:54
c
c**********************************************************************
*/
// ToolPromptDlg.cpp : implementation file
//

#include "toolibstdafx.h"
#include "toolib.h"
#include "ToolPromptDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CToolPromptDlg dialog

/***********************************************************************
c
c   FUNCTION: CToolPromptDlg(CWnd* pParent)
c
c              Constructor of class CToolPromptDlg
c
c   INPUT:  pParent: parent window
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
CToolPromptDlg::CToolPromptDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CToolPromptDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CToolPromptDlg)
	m_title = "Prompt Dialog";
	m_msg = "Input Text: ";
	m_input = "";
	//}}AFX_DATA_INIT
}

/***********************************************************************
c
c   FUNCTION: DoDataExchange(CDataExchange* pDX)
c
c         Called by the framework to exchange and validate dialog data.
c
c   INPUT:  pDX   A pointer to a CDataExchange object.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CToolPromptDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CToolPromptDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CToolPromptDlg, CDialog)
	//{{AFX_MSG_MAP(CToolPromptDlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CToolPromptDlg message handlers
/***********************************************************************
c
c   FUNCTION: OnInitDialog() 
c
c         This member function is called in response to 
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
BOOL CToolPromptDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	GetDlgItem(IDC_PROMPT_MSG)->SetWindowText(m_msg);
	GetDlgItem(IDC_INPUT)->SetWindowText(m_input);
	SetWindowText(m_title);
	GetDlgItem(IDC_INPUT)->SetFocus();
	return FALSE;  
}

/***********************************************************************
c
c   FUNCTION: SetDlgValue(char *title, char *msg, char*input)
c
c         This function set the Dialog values
c
c   INPUT:  title: dialog Title
c			msg: prompt label
c			input: Default text in the prompt edit field
c
c   OUTPUT : None
c   RETURN:  None
c
**********************************************************************/
void CToolPromptDlg::SetDlgValue(char *title, char *msg, char*input)
{
	m_msg = msg;
	m_input = input;
	m_title = title;
}

/***********************************************************************
c
c   FUNCTION: GetInput(char *input, int maxnum)
c
c         This function Get the user input text string
c
c   INPUT:  maxnum: number of character we want to get from input
c
c   OUTPUT  input: user input text in the prompt edit field
c   RETURN:  None
c
**********************************************************************/
void CToolPromptDlg::GetInput(char *input, int maxnum)
{
#ifdef _UNICODE	
	int nc = m_input.GetLength();
	if (nc>0)
	{
		WCHAR *winput = m_input.GetBuffer(nc);
		wcstombs(input, winput, nc);
		input[nc] = '\0';
	}
	else
		input[0] = '\0';
#else
	char *temp = m_input.GetBuffer(maxnum);
	strcpy (input, temp);
#endif
}

/***********************************************************************
c
c   FUNCTION: CToolPromptDlg::OnOK()
c
c         This is callback function for OK button.
c
c   INPUT:  None
c
c   OUTPUT  None
c   RETURN:  None
c
**********************************************************************/
void CToolPromptDlg::OnOK() 
{
	GetDlgItem(IDC_INPUT)->GetWindowText(m_input);
	CDialog::OnOK();
}
