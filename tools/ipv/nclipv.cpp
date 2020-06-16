/********************************************************************* 
**  NAME:  nclipv.cpp
**
**			Defines the class behaviors 
**				for the Machine Simulator Lite application.
**			implementation of CNclipvApp class functions
**		CONTAINS: CNclipvApp class functions
**			all functions declared in mslite.h
**
**    COPYRIGHT 2008 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       nclipv.cpp , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**       09/08/15 , 08:32:40
*********************************************************************/
// nclipv.cpp : Defines the class behaviors for the application.
//
#include "wsntstdafx.h"
//#include <iostream>
#include "nclipv.h"
#define  NCLWNT_CCMAIN
#include "wsntfuncid.h"
#undef	NCLWNT_CCMAIN

#include "NCLipvFrm.h"
#include "wsntdoc.h"
#include "nclipvView.h"
#include "lipv.h"
#include "UxTheme.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
extern "C" char NCL_init_fstr[20];
extern "C" int uw_ntsetcursor(int);
extern "C" int UW_signon;
int NCL_exit = 0;
DWORD UW_theme;
extern "C" int IPV_failed;
extern "C" int ul_verify_reset();
extern "C" int ncl_mot_stack_init();
extern "C" void ul_ipv_start();
extern "C" void uw_ntupd_comfont();
extern "C" int UL_ipv;
extern "C" void nclipv_signon();
extern "C" int nclipv_init_auth(char*);
extern "C" void nclipv_update_win_title();
extern "C" int nclipv_parse_cmd(char*, char*, int*);
extern "C" int uw_ntflush_paint();
extern "C" int NCL_event_reject;
extern "C" void NclxDbgPstr(char *);
//using namespace std;
//extern ostream cout;
/////////////////////////////////////////////////////////////////////////////
// CNclipvApp

BEGIN_MESSAGE_MAP(CNclipvApp, CWinApp)
	//{{AFX_MSG_MAP(CNclipvApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
	// Standard file based document commands
	ON_COMMAND(ID_APP_EXIT, OnIPVSafeExit)
	// Standard print setup command
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNclipvApp construction

CNclipvApp::CNclipvApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CNclipvApp object

CNclipvApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CNclipvApp initialization

BOOL CNclipvApp::InitInstance()
{
	int status, err;
	m_strMyClassName = AfxRegisterWndClass(0, 
			::LoadCursor (NULL, IDC_ARROW), 
			(HBRUSH) (COLOR_WINDOW+1),
			LoadIcon("IDR_NCLIPVTYPE"));
//the following function will not allow create dynamic dialog view
//
//	AfxEnableControlContainer();

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

	// Change the registry key under which our settings are stored.
	// TODO: You should modify this string to be something appropriate
	// such as the name of your company or organization.
	SetRegistryKey(_T("NCLIPV Standalone application"));

	LoadStdProfileSettings();  // Load standard INI file options (including MRU)

	CWinApp* pApp;
	pApp = AfxGetApp();
	LW_nclipv = LW_STANDALONE;
/*
.....Let accept command line
*/
	LPTSTR lpCmdLine;
	char cmdline[UX_MAX_PATH_LEN];
	char errmsg[256];
	lpCmdLine = pApp->m_lpCmdLine;
	strcpy(cmdline, lpCmdLine);
/*
.....do something here to accept command input
*/
	err = 0;
NclxDbgPstr("Call nclipv_parse_command");
	status = nclipv_parse_cmd(cmdline, errmsg, &err);
	if (err)
		MessageBox(NULL, errmsg, "NCLIPV error", MB_OK); 
	if (status!=0) 
	{
		return FALSE;
	}
	_putenv("ws=openGL");

	strcpy (NCL_init_fstr, "NCL_INIT_FILES");
/*
......check license
*/
NclxDbgPstr("Call nclipv_init_auth");
	status = nclipv_init_auth(errmsg);
	if (status!=0) 
	{
		MessageBox(NULL, errmsg, "NCLIPV error", MB_OK); 
		return FALSE;
	}
	// Register the application's document templates.  Document templates
	//  serve as the connection between documents, frame windows and views.
/*
......SetThemeAppProperties(0) will disable COM common dialog functions
......so save here in order for reset back
......I think we initial use SetThemeAppProperties(0) to disply toolbar/menubar nicely,
......especially on XP, there might be other reasons we can't remember, so
......I will still leave it and save the theme in order to set back
*/
	UW_theme = GetThemeAppProperties();
//	SetThemeAppProperties(0);
	SetThemeAppProperties(STAP_ALLOW_NONCLIENT);

	CSingleDocTemplate* pDocTemplate;
	pDocTemplate = new CSingleDocTemplate(
		IDR_MAINFRAME,
		RUNTIME_CLASS(CNCLDoc),
		RUNTIME_CLASS(CMainFrame),       // main SDI frame window
		RUNTIME_CLASS(CNclipvView));
	AddDocTemplate(pDocTemplate);

	// Parse command line for standard shell commands, DDE, file open
	CCommandLineInfo cmdInfo;

	// Dispatch commands specified on the command line
	if (!ProcessShellCommand(cmdInfo))
		return FALSE;
/*
.....Initialize IPV
*/
	UL_ipv = 1;
NclxDbgPstr("Call ul_verify_reset");
	ul_verify_reset();
NclxDbgPstr("Call ul_ipv_start");
	ul_ipv_start();
	uw_ntupd_comfont();
NclxDbgPstr("Call nclipv_signon");
	nclipv_signon();
/*
.....Initialize internal clfile
*/
//	clinit();
/*
.....Initialize motion stack
*/
//	ncl_mot_stack_init();
/*
.....Initialize cutter display
*/
//	ncl_cutter_reset();

	if (IPV_failed)
		return FALSE;
	UW_signon = 1;
	m_pMainWnd->ShowWindow(SW_SHOW);
	m_pMainWnd->UpdateWindow();

	nclipv_update_win_title();
	uw_ntsetcursor(-99);
	return TRUE;
}
/***********************************************************************
c
c   FUNCTION: Run()
c
c         Provides a default message loop. 
c		Run acquires and dispatches Windows messages 
c		until the application receives a WM_QUIT 
c		message. 
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
int CNclipvApp::Run()
{
	int jmpflag;
	MSG msg;

start:;
	UD_enablejmp = UU_TRUE;
	UD_MARK(jmpflag,UU_TRUE);
	UD_pickmode = 0;
	NCL_event_reject = 0;
/*
......(UD_markptr==1) jump have alredy at the top
*/
	if ((jmpflag == 0) || (UD_markptr==1))
	{
/*
.....overwrite to do some of our work
*/
		if (m_pMainWnd == NULL && AfxOleGetUserCtrl())
		{
/*
.....Not launched /Embedding or /Automation, but has no main window!
*/
			TRACE0("Warning: m_pMainWnd is NULL in CWinApp::Run - quitting application.\n");
			AfxPostQuitMessage(0);
		}
		ASSERT_VALID(this);
/*
......for tracking the idle time state
*/
		BOOL bIdle = TRUE;
		LONG lIdleCount = 0;

/*
.......acquire and dispatch messages until a WM_QUIT message is received.
*/
		for (;;)
		{
			while (bIdle &&
				!::PeekMessage(&msg, NULL, NULL, NULL, PM_NOREMOVE))
			{
				if (!OnIdle(lIdleCount++))
					bIdle = FALSE; 
			}
			do
			{
				uw_ntflush_paint();
/*
......pump message, but quit on WM_QUIT
*/
				if (!PumpMessage())
				{
					return ExitInstance();
				}
				if (IsIdleMessage(&msg))
				{
					bIdle = TRUE;
					lIdleCount = 0;
				}
			} while (::PeekMessage(&msg, NULL, NULL, NULL, PM_NOREMOVE));
		}
	}
	else
	{
		UD_UNMARK(jmpflag);
		goto start;
	}
	ASSERT(FALSE);  // not reachable
}
/***********************************************************************
c
c   FUNCTION: OnIPVSafeExit()
c
c         safely exit NCLIPV
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNclipvApp::OnIPVSafeExit()
{
	NCL_exit = 1;
	m_pMainWnd->PostMessage(WM_CLOSE);
}
extern "C" void ncl_cmdmsg(char *msg)
{
	MessageBox(NULL, msg, "NCLIPV Message", MB_OK); 
//	cout<<msg;
}

/***********************************************************************
**
**   SUBROUTINE: ncl_is_exiting()
**
**   FUNCTION:  This routine returns UU_TRUE if NCL is in the process
**              of exiting.
**
**   INPUT:  none
**
**   OUTPUT: none
**
**   RETURNS: UU_TRUE if NCL is exiting.
**
***********************************************************************/
extern "C" 	UU_LOGICAL ncl_is_exiting()
{
	if (NCL_exit == 1) return(UU_TRUE);
	return(UU_FALSE);
}

/////////////////////////////////////////////////////////////////////////////
// CNclipvApp message handlers
