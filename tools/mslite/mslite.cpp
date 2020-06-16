/********************************************************************* 
**  NAME:  mslite.cpp
**
**			Defines the class behaviors 
**				for the Machine Simulator Lite application.
**			implementation of CMsliteApp class functions
**		CONTAINS: CMsliteApp class functions
**			all functions declared in mslite.h
**
**    COPYRIGHT 2008 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       mslite.cpp , 26.3
**    DATE AND TIME OF LAST  MODIFICATION
**       09/25/18 , 10:32:43
*********************************************************************/
// mslite.cpp : Defines the class behaviors for the application.
//
#include "wsntstdafx.h"
#include "mslite.h"
#define  NCLWNT_CCMAIN
#include "wsntfuncid.h"
#undef	NCLWNT_CCMAIN

#include "mslMainFrm.h"
#include "wsntdoc.h"
#include "msliteView.h"
#include "UxTheme.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
extern "C" int MSLite;
extern "C" char NCL_init_fstr[20];
extern "C" int uw_ntsetcursor(int);
extern "C" void getver(double *ver);
extern "C" int UW_signon;
int NCL_exit = 0;
extern "C" UX_pathname MSL_init_mach;
extern "C" int MS_failed;
DWORD UW_theme;
/////////////////////////////////////////////////////////////////////////////
// CMsliteApp

BEGIN_MESSAGE_MAP(CMsliteApp, CWinApp)
	//{{AFX_MSG_MAP(CMsliteApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
	// Standard file based document commands
	ON_COMMAND(ID_APP_EXIT, OnMSLSafeExit)
	ON_COMMAND(ID_FILE_NEW, CWinApp::OnFileNew)
	ON_COMMAND(ID_FILE_OPEN, CWinApp::OnFileOpen)
	// Standard print setup command
//	ON_COMMAND(ID_FILE_PRINT_SETUP, CWinApp::OnFilePrintSetup)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CMsliteApp construction

CMsliteApp::CMsliteApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CMsliteApp object

CMsliteApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CMsliteApp initialization

BOOL CMsliteApp::InitInstance()
{
	m_strMyClassName = AfxRegisterWndClass(0, 
			::LoadCursor (NULL, IDC_ARROW), 
			(HBRUSH) (COLOR_WINDOW+1),
			LoadIcon("IDR_MSLITETYPE"));
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
	SetRegistryKey(_T("Machine Simulator Lite"));

	LoadStdProfileSettings();  // Load standard INI file options (including MRU)

	CWinApp* pApp;
	pApp = AfxGetApp();
/*
.....Let accept command line
*/
	LPTSTR lpCmdLine;
	char cmdline[UX_MAX_PATH_LEN];
	lpCmdLine = pApp->m_lpCmdLine;
	strcpy(cmdline, lpCmdLine);
	_putenv("ws=openGL");

	MSLite = 1;
/*
.....do something here to accept command input
*/
	strcpy(MSL_init_mach, lpCmdLine);
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
	SetThemeAppProperties(0);

	CSingleDocTemplate* pDocTemplate;
	pDocTemplate = new CSingleDocTemplate(
		IDR_MAINFRAME,
		RUNTIME_CLASS(CNCLDoc),
		RUNTIME_CLASS(CMainFrame),       // main SDI frame window
		RUNTIME_CLASS(CMsliteView));
	AddDocTemplate(pDocTemplate);

	// Parse command line for standard shell commands, DDE, file open
	CCommandLineInfo cmdInfo;

	// Dispatch commands specified on the command line
	if (!ProcessShellCommand(cmdInfo))
		return FALSE;
	if (MS_failed)
		return FALSE;
	UW_signon = 1;

	m_pMainWnd->ShowWindow(SW_SHOW);
	m_pMainWnd->UpdateWindow();

	m_pMainWnd->SetWindowText("Machine Simulator Lite");

	uw_ntsetcursor(-99);
	return TRUE;
}
/***********************************************************************
c
c   FUNCTION: OnMSLSafeExit()
c
c         safely exit MSLITE
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMsliteApp::OnMSLSafeExit()
{
	m_pMainWnd->PostMessage(WM_CLOSE);
}

/////////////////////////////////////////////////////////////////////////////
// CMsliteApp message handlers

/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About
class CAboutDlg : public CDialog
{
public:
	CAboutDlg();
// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAboutDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX); 
	//}}AFX_VIRTUAL

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
	virtual BOOL OnInitDialog();
		// No message handlers
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
	//{{AFX_DATA_INIT(CAboutDlg)
	//}}AFX_DATA_INIT
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAboutDlg)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
		// No message handlers
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog
c
c   FUNCTION:  This function initialize window text
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
BOOL CAboutDlg::OnInitDialog()
{
	CDialog::OnInitDialog();
	double ver;
	char label[256];

	getver(&ver);
	sprintf(label, "MsLite Version %7.2f", ver); 
	GetDlgItem(IDC_STATIC1)->SetWindowText(label);
	GetDlgItem(IDC_STATIC2)->SetWindowText("Copyright (C) Numerical Control Computer Sciences 2007-2018");
	SetWindowText("About MsLite");
	return 0;
}

/***********************************************************************
c
c   FUNCTION: msl_about()
c
c		display an about dialog box
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
extern "C" void msl_about()
{
	CAboutDlg aboutDlg;
	aboutDlg.DoModal();
}
