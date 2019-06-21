/************************************************************************
c
c   FILE NAME: gwordctl.cpp
c	 CONTAINS: Defines the class behaviors for the application.
c
c	 CONTAINS: 
c		CGenWordApp::InitInstance()
c
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        GWordctl.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:32
c
c**********************************************************************
*/
#include "pwenv.h"
#include "pwstdafx.h"
#include "genword.h"

#include "PWMainFrm.h"
#include "PostworksDoc.h"
#include "genwordView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern int UU_BATCH;
extern CWnd *Pw_maindlg;
extern CWnd *Pw_mainwin;
extern "C" int genword(char *comstr);
extern "C" int gword_version();
/////////////////////////////////////////////////////////////////////////////
// CGenWordApp

BEGIN_MESSAGE_MAP(CGenWordApp, CWinApp)
	//{{AFX_MSG_MAP(CGenWordApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
	// Standard file based document commands
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CGenWordApp construction

CGenWordApp::CGenWordApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CGenWordApp object

CGenWordApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CGenWordApp initialization

/***********************************************************************
c
c   SUBROUTINE:  InitInstance()
c
c   FUNCTION:  This function initialize application and start it
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
BOOL CGenWordApp::InitInstance()
{
	AfxEnableControlContainer();

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

	// Change the registry key under which our settings are stored.
	// TODO: You should modify this string to be something appropriate
	// such as the name of your company or organization.
	SetRegistryKey(_T("Local AppWizard-Generated Applications"));

	LoadStdProfileSettings(0);  // Load standard INI file options (including MRU)

	gword_version();
/*
......see if there are fillename or option
......on the command line
*/
	CWinApp* pApp;
	pApp = AfxGetApp();
/*
.....Let accept command line
*/
	char * lpCmdLine, comstr[UX_MAX_PATH+80];
	lpCmdLine = pApp->m_lpCmdLine;
	strcpy(comstr, lpCmdLine);
	int num = strlen(comstr);
/*
......if there are filename in the command line
......use command batch mode, don't use window interface
......Dislay error message in one error window when done.
*/
	if (num > 0)
	{
		UU_BATCH = 3;
/*
.....add trailing space for Fortran
*/
		for (int i=num; i<UX_MAX_PATH;i++)
			comstr[i] = ' ';
		genword(comstr);
		if (Pw_maindlg==NULL)
			return FALSE;
		else
		{
			m_pMainWnd = Pw_maindlg;
			Pw_mainwin = Pw_maindlg;
			return TRUE;
		}
	}
	// Register the application's document templates.  Document templates
	//  serve as the connection between documents, frame windows and views.

	CSingleDocTemplate* pDocTemplate;
	pDocTemplate = new CSingleDocTemplate(
		IDR_MAINFRAME,
		RUNTIME_CLASS(CPostworksDoc),
		RUNTIME_CLASS(CPWMainFrame),       // main SDI frame window
		RUNTIME_CLASS(CGenwordView));
	AddDocTemplate(pDocTemplate);

	// Parse command line for standard shell commands, DDE, file open
	CCommandLineInfo cmdInfo;
/*
......always use command as filenew (default)
......don't parse the filename to open, Postworks
......handel itself
*/
//	ParseCommandLine(cmdInfo);

	// Dispatch commands specified on the command line
	if (!ProcessShellCommand(cmdInfo))
		return FALSE;

	// The one and only window has been initialized, so show and update it.
	m_pMainWnd->ShowWindow(SW_SHOW);
	m_pMainWnd->UpdateWindow();

	return TRUE;
}


/////////////////////////////////////////////////////////////////////////////
// CGenWordApp message handlers

