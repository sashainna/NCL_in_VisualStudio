/************************************************************************
**
**   FILE NAME: nccs_auth.cpp
**
**  Description - Main program for NCCS_AUTH on Windows NT.
**
**  CONTAINS:
**
**    COPYRIGHT 2002 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nccs_auth.cpp , 23.1
**    DATE AND TIME OF LAST  MODIFICATION
**       05/22/12 , 11:15:13
*********************************************************************/
// nccs_auth.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include "nccs_auth.h"
#include "nccs_authDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int auth_ntinit(char *msg, int *ierr);
extern char PW_msgtitle[256];

extern CWnd *Pw_maindlg;

/////////////////////////////////////////////////////////////////////////////
// CNccs_authApp

BEGIN_MESSAGE_MAP(CNccs_authApp, CWinApp)
	//{{AFX_MSG_MAP(CNccs_authApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG
	ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNccs_authApp construction

CNccs_authApp::CNccs_authApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CNccs_authApp object

CNccs_authApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CNccs_authApp initialization

BOOL CNccs_authApp::InitInstance()
{
	AfxEnableControlContainer();

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

	char msg[256];
	int ierr;

	strcpy(PW_msgtitle, "NCCS_AUTH Message");
	for (int i=0; i<256;i++)
		msg[i] = ' ';
	auth_ntinit(msg, &ierr);

	if (ierr!=0)
	{
		msg[255] = '\0';
		CString cmsg = msg;
		cmsg.TrimRight();
		MessageBox(NULL, cmsg, "ERROR",MB_OK);
		return FALSE;
	}
/*
......see if there are fillename or option
......on the command line
*/
//	CWinApp* pApp;
//	pApp = AfxGetApp();
/*
.....Let accept command line
*/
//	char * lpCmdLine, batfile[256];
//	lpCmdLine = pApp->m_lpCmdLine;
//	strcpy(batfile, lpCmdLine);
//	int num = strlen(batfile);
//	if (num>0)
//	{
//		batchlic(batfile, &num, &ierr);
//		if (ierr==0)
//			return FALSE;
//	}

	CNccs_authDlg dlg;
	m_pMainWnd = &dlg;
	Pw_maindlg = &dlg;
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
