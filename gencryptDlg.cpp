/************************************************************************
c
c   FILE NAME: gencryptDlg.cpp
c   CONTAINS: Defines the class behaviors for the application.
c
c   CONTAINS:
c
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        gencryptDlg.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:32
c
c**********************************************************************/
// gencryptDlg.cpp : implementation file
//

#include "stdafx.h"
#include "gencrypt.h"
#include "gencryptDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int gencrypt(char opmsg[80], int *nca, int *crypt_err);
/////////////////////////////////////////////////////////////////////////////
// CGencryptDlg dialog

CGencryptDlg::CGencryptDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CGencryptDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CGencryptDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void CGencryptDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CGencryptDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CGencryptDlg, CDialog)
	//{{AFX_MSG_MAP(CGencryptDlg)
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CGencryptDlg message handlers

BOOL CGencryptDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon
	
	// TODO: Add extra initialization here
	
	return TRUE;  // return TRUE  unless you set the focus to a control
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CGencryptDlg::OnPaint() 
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, (WPARAM) dc.GetSafeHdc(), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}

// The system calls this to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR CGencryptDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

void CGencryptDlg::OnOK() 
{
	char txt[80];
	int len,err;
	len = GetDlgItem(IDC_EDIT1)->GetWindowText(txt, 80);
	gencrypt(txt, &len, &err);
	if (err==0)
		CDialog::OnOK();
}
