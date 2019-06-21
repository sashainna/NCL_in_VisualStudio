/************************************************************************
c
c   FILE NAME: gcryptctl.cpp
c   CONTAINS: Defines the class behaviors for the application.
c
c   CONTAINS:
c
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        gcryptctl.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:32
c
c**********************************************************************/
// gencrypt.cpp : Defines the class behaviors for the application.
//

#include "pwstdafx.h"
#include "gencrypt.h"
#include "gencryptDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CGencryptApp

BEGIN_MESSAGE_MAP(CGencryptApp, CWinApp)
	//{{AFX_MSG_MAP(CGencryptApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG
	ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CGencryptApp construction

CGencryptApp::CGencryptApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CGencryptApp object

CGencryptApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CGencryptApp initialization

BOOL CGencryptApp::InitInstance()
{
	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

	CGencryptDlg dlg;
	m_pMainWnd = &dlg;
	int nResponse = dlg.DoModal();
	if (nResponse == IDOK)
	{
		// TODO: Place code here to handle when the dialog is
		//  dismissed with OK
	}
	else if (nResponse == IDCANCEL)
	{
		// TODO: Place code here to handle when the dialog is
		//  dismissed with Cancel
	}

	// Since the dialog has been closed, return FALSE so that we exit the
	//  application, rather than start the application's message pump.
	return FALSE;
}
