/************************************************************************
c
c   FILE NAME: PasswdDlg.cpp
c		implementation file for password box
c	 CONTAINS: 
c		CPasswdDlg::CPasswdDlg()
c		CPasswdDlg::OnInitDialog() 
c
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PasswdDlg.cpp , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:13 
c
c**********************************************************************
*/

#include "stdafx.h"
#include "nccs_auth.h"
#include "PasswdDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPasswdDlg dialog


/***********************************************************************
c
c   SUBROUTINE: CPasswdDlg(CWnd* pParent)	
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CPasswdDlg::CPasswdDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CPasswdDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CPasswdDlg)
	m_passwd = _T("");
	//}}AFX_DATA_INIT
}


void CPasswdDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPasswdDlg)
	DDX_Text(pDX, IDC_EDIT1, m_passwd);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CPasswdDlg, CDialog)
	//{{AFX_MSG_MAP(CPasswdDlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog
c
c   FUNCTION:  This function initialize 
c				the dialog (set focus to text field)
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
BOOL CPasswdDlg::OnInitDialog()
{
	CDialog::OnInitDialog();
	((CWnd*)GetDlgItem(IDC_EDIT1))->SetFocus();
	return FALSE;  
}

/////////////////////////////////////////////////////////////////////////////
// CPasswdDlg message handlers
