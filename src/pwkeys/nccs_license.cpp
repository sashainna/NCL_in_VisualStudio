/************************************************************************
c
c   FILE NAME: Nccs_license.cpp
c
c        CONTAINS: 
c               CNccs_licenseApp::InitInstance()
c
c    COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL 
c       nccs_license.cpp , 23.1
c    DATE AND TIME OF LAST MODIFICATION
c       05/22/12 , 11:15:14
c
c**********************************************************************
*/

#include "pwenv.h"
#include "pwstdafx.h"
#include "nccs_license.h"
#include "nccs_licenseDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
extern "C" int license_ntinit(char *msg, int *ierr);
extern char PW_msgtitle[256];
extern CWnd *Pw_maindlg;
/////////////////////////////////////////////////////////////////////////////
// CNccs_licenseApp

BEGIN_MESSAGE_MAP(CNccs_licenseApp, CWinApp)
	//{{AFX_MSG_MAP(CNccs_licenseApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG
	ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNccs_licenseApp construction

CNccs_licenseApp::CNccs_licenseApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CNccs_licenseApp object

CNccs_licenseApp theApp;
extern "C" int batchlic(char *batfile, int *num, int *ierr);
/////////////////////////////////////////////////////////////////////////////
// CNccs_licenseApp initialization

/***********************************************************************
c
c   FUNCTION: InitInstance()
c
c         This function is to initialize Pted application 
c
c   INPUT:  None
c
c   OUTPUT  None
c   RETURN:  None
c
**********************************************************************/

BOOL CNccs_licenseApp::InitInstance()
{
	AfxEnableControlContainer();

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

	char msg[80];
	int ierr;

	strcpy(PW_msgtitle, "NCCS_LICENSE Message");

	license_ntinit(msg, &ierr);

	if (ierr!=0)
	{
		msg[79] = '\0';
		MessageBox(NULL, msg, "ERROR",MB_OK);
		return FALSE;
	}
/*
......see if there are fillename or option
......on the command line
*/
	CWinApp* pApp;
	pApp = AfxGetApp();
/*
.....Let accept command line
*/
	char * lpCmdLine, batfile[UX_MAX_PATH];
	lpCmdLine = pApp->m_lpCmdLine;
	strcpy(batfile, lpCmdLine);
	int num = strlen(batfile);
	if (num>0)
	{
		batchlic(batfile, &num, &ierr);
		if (ierr==0)
			return FALSE;
	}

	CNccs_licenseDlg dlg;
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
