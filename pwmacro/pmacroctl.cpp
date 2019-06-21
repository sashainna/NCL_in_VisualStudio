/************************************************************************
c
c   FILE NAME: Pmacroctlp.cpp
c	 CONTAINS: Defines the class behaviors for the application.
c
c	 CONTAINS: 
c		CPmacroApp::InitInstance()
c
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        pmacroctl.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:53
c
c**********************************************************************
*/
#include "pwenv.h"
#include "stdafx.h"
#include "pmacro.h"

#include "PWMainFrm.h"
#include "PostworksDoc.h"
#include "pmacroView.h"
#include "Pmfunc.h"
#include "PWMessageBox.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern int UU_BATCH;
extern FILE *PW_logfile;
extern CWnd *Pw_mainwin;
extern CWnd *Pw_maindlg;

/////////////////////////////////////////////////////////////////////////////
// CPmacroApp

BEGIN_MESSAGE_MAP(CPmacroApp, CWinApp)
	//{{AFX_MSG_MAP(CPmacroApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
	// Standard file based document commands
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPmacroApp construction

CPmacroApp::CPmacroApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CPmacroApp object

CPmacroApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CPmacroApp initialization

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
BOOL CPmacroApp::InitInstance()
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
	int kfl, ierr,ifil;
	char msg[82];

	pmacro_ntinit();
	kfl = 0;
    cloddat (&kfl,msg,&ierr);
	if (ierr != 0)
		cerrkil (msg,&ierr);
/*
......init values for option (from init files)
*/
	getopcc (msg,&ierr);
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
......if have a command string, parse it
*/
	ifil = 0;
	if (num != 0)	
		prsopt (comstr, &num,&ifil,msg,&ierr);
     
	if (ifil == 1)
	{
		if (isquiet()==1)
		{
/*
.....put message into log file
*/
			UU_BATCH = 2;
/*
.....Open log file
*/
			PW_logfile = fopen("pmacro.log", "a+");
			if (ierr!=0)
				cerrkil (msg,&ierr);
			else
				pmacro();
			fclose(PW_logfile);
			return FALSE;
		}
		else
		{
//			UU_BATCH = 1;
/*
......only open a message box when there is a batch message to be displayed
*/
			UU_BATCH = 3;
/*
.....Open a message window
*/
/*			m_pMainWnd = &PW_msgbox;
			PW_msgbox.Create(IDD_BATCH_MESSAGE);
			PW_msgbox.ShowWindow(SW_SHOW);
*/
			if (ierr!=0)
				cerrkil (msg,&ierr);
			else
				pmacro();
			m_pMainWnd = Pw_maindlg;
			Pw_mainwin = Pw_maindlg;
			return TRUE;
		}
	}
/*
......if every is ok and no filename input from command
......line, use window interface
*/
	if (ierr != 0)
        MessageBox(NULL, msg, "ERROR", MB_OK);

	// Register the application's document templates.  Document templates
	//  serve as the connection between documents, frame windows and views.

	CSingleDocTemplate* pDocTemplate;
	pDocTemplate = new CSingleDocTemplate(
		IDR_MAINFRAME,
		RUNTIME_CLASS(CPostworksDoc),
		RUNTIME_CLASS(CPWMainFrame),       // main SDI frame window
		RUNTIME_CLASS(CPmacroView));
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
	Pw_mainwin = m_pMainWnd;

	return TRUE;
}


/////////////////////////////////////////////////////////////////////////////
// CPmacroApp message handlers

