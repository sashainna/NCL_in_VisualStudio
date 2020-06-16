/************************************************************************
c
c   FILE NAME: toolibapp.cpp
c
c	 CONTAINS: 
c		Functions for the class CToolibApp
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       toolibapp.cpp , 25.3
c    DATE AND TIME OF LAST  MODIFICATION
c       10/26/15 , 08:02:25
c
c**********************************************************************
*/
// toolib.cpp : Defines the class behaviors for the application.
//

#include "toolibstdafx.h"
#include "afxwinappex.h"
#include "afxdialogex.h"
#include "toolib.h"
#include "MainFrm.h"
#include "toolibDoc.h"
#include "toolibView.h"
#include "toolibcc.h"
#include "xenv1.h"
#include "toolibdata.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif
#define UU_XFORM_NM		5
#define UU_OT_NM	    10
#define MAXT 150

extern "C" int tool_init_font();
extern "C" int toolib_getabout_str(char *str1, char *str2);
extern "C" struct TL_toolhead_rec Tool_head;
extern "C" int tool_init_tool(int*, int*);
extern "C" int uu_toolmalloc_init();
extern "C" int uu_toolmalloc_term();
extern "C" int uu_nserv_init(int, int, int);

// CToolibApp

BEGIN_MESSAGE_MAP(CToolibApp, CWinAppEx)
	ON_COMMAND(ID_APP_ABOUT, &CToolibApp::OnAppAbout)
	// Standard file based document commands
	ON_COMMAND(ID_FILE_NEW, &CWinAppEx::OnFileNew)
	ON_COMMAND(ID_FILE_OPEN, &CWinAppEx::OnFileOpen)
END_MESSAGE_MAP()


// CToolibApp construction
CToolibApp::CToolibApp()
{
	// support Restart Manager
	m_dwRestartManagerSupportFlags = AFX_RESTART_MANAGER_SUPPORT_ALL_ASPECTS;
#ifdef _MANAGED
	// If the application is built using Common Language Runtime support (/clr):
	//     1) This additional setting is needed for Restart Manager support to work properly.
	//     2) In your project, you must add a reference to System.Windows.Forms in order to build.
	System::Windows::Forms::Application::SetUnhandledExceptionMode(System::Windows::Forms::UnhandledExceptionMode::ThrowException);
#endif

	// TODO: replace application ID string below with unique ID string; recommended
	// format for string is CompanyName.ProductName.SubProduct.VersionInformation
	SetAppID(_T("toolib.AppID.NoVersion"));

	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

// The one and only CToolibApp object

CToolibApp theApp;


// CToolibApp initialization
/***********************************************************************
c
c   FUNCTION: InitInstance()
c
c         This function is to initialize toolib application 
c
c   INPUT:  None
c
c   OUTPUT  None
c   RETURN:  None
c
**********************************************************************/
BOOL CToolibApp::InitInstance()
{
	// InitCommonControlsEx() is required on Windows XP if an application
	// manifest specifies use of ComCtl32.dll version 6 or later to enable
	// visual styles.  Otherwise, any window creation will fail.
	INITCOMMONCONTROLSEX InitCtrls;
	InitCtrls.dwSize = sizeof(InitCtrls);
	// Set this to include all the common control classes you want to use
	// in your application.
	InitCtrls.dwICC = ICC_WIN95_CLASSES;
	InitCommonControlsEx(&InitCtrls);

	CWinAppEx::InitInstance();


	// Initialize OLE libraries
	if (!AfxOleInit())
	{
		return FALSE;
	}

	AfxEnableControlContainer();

	EnableTaskbarInteraction(FALSE);

	// AfxInitRichEdit2() is required to use RichEdit control	
	// AfxInitRichEdit2();

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	// of your final executable, you should remove from the following
	// the specific initialization routines you do not need
	// Change the registry key under which our settings are stored
	// TODO: You should modify this string to be something appropriate
	// such as the name of your company or organization
	SetRegistryKey(_T("NCCS_TOOLIB"));
	LoadStdProfileSettings(4);  // Load standard INI file options (including MRU)

	int cont, err;
	uu_toolmalloc_init();
	uu_nserv_init(0,14,UU_XFORM_NM);
	uu_nserv_init(0,MAXT-1,UU_OT_NM);
	tool_init_tool(&cont, &err);
	if (err!=0) 
	{
		uu_toolmalloc_term();
		return FALSE;
	}
/*
.....if the toolib done with batch, exit
*/
	if (cont==0) 
	{
		uu_toolmalloc_term();
		return FALSE;
	}
	tool_init_font();

	InitContextMenuManager();
	InitKeyboardManager();

	InitTooltipManager();
	CMFCToolTipInfo ttParams;
	ttParams.m_bVislManagerTheme = TRUE;
	theApp.GetTooltipManager()->SetTooltipParams(AFX_TOOLTIP_TYPE_ALL,
		RUNTIME_CLASS(CMFCToolTipCtrl), &ttParams);

	// Register the application's document templates.  Document templates
	//  serve as the connection between documents, frame windows and views
	CSingleDocTemplate* pDocTemplate;
	pDocTemplate = new CSingleDocTemplate(
		IDR_MAINFRAME,
		RUNTIME_CLASS(CToolibDoc),
		RUNTIME_CLASS(CMainFrame),       // main SDI frame window
		RUNTIME_CLASS(CToolibView));
	if (!pDocTemplate)
		return FALSE;
	AddDocTemplate(pDocTemplate);


	// Parse command line for standard shell commands, DDE, file open
	CCommandLineInfo cmdInfo;
	ParseCommandLine(cmdInfo);

	// Dispatch commands specified on the command line.  Will return FALSE if
	// app was launched with /RegServer, /Register, /Unregserver or /Unregister.
	if (!ProcessShellCommand(cmdInfo))
		return FALSE;

	m_pMainWnd->ShowWindow(SW_SHOW);
	UX_pathname tlib;
	char disp[40], slib[22],buf[80];
	int nc1, nc2, nc3;
	strcpy (disp, Tool_head.description);
	strcpy (tlib, Tool_head.name);
	strcpy (slib, Tool_head.symlib);
	nc1 = strlen (tlib);
	if (nc1>0)
	{
		char title[256];
		strcpy(title, "NCL TOOL LIBRARY:");
		tool_short_filename(tlib,buf,60);
		strcat(title, buf);
#ifdef _UNICODE	
		WCHAR *wtitle;
		int len = strlen (title) + 1;
		wtitle = new WCHAR[len];
		int wlen = MultiByteToWideChar(CP_ACP, 0, title, -1, 
							wtitle, len);
		m_pMainWnd->SetWindowText(wtitle);
#else
		m_pMainWnd->SetWindowText(title);
#endif
	}
	else
		m_pMainWnd->SetWindowText(_T("NCL TOOL LIBRARY"));
	// The one and only window has been initialized, so show and update it
	m_pMainWnd->ShowWindow(SW_SHOW);
	m_pMainWnd->UpdateWindow();
	// call DragAcceptFiles only if there's a suffix
	//  In an SDI app, this should occur after ProcessShellCommand
	return TRUE;
}


// CAboutDlg dialog used for App About

class CAboutDlg : public CDialogEx
{
public:
	CAboutDlg();

// Dialog Data
	enum { IDD = IDD_ABOUTBOX };

protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
	virtual BOOL OnInitDialog();
		// No message handlers
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialogEx(CAboutDlg::IDD)
{
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialogEx::DoDataExchange(pDX);
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialogEx)
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
	char label1[256], label2[256];

	toolib_getabout_str(label1, label2);

	WCHAR wlabel[256];
	int len = strlen (label1) + 1;
	int wlen = MultiByteToWideChar(CP_ACP, 0, label1, -1, 
							wlabel, len);
	GetDlgItem(IDC_ABOUTLABEL1)->SetWindowText(wlabel);

	len = strlen (label2) + 1;
	wlen = MultiByteToWideChar(CP_ACP, 0, label2, -1, 
							wlabel, len);
	GetDlgItem(IDC_ABOUTLABEL2)->SetWindowText(wlabel);

	SetWindowText(_T("About Toolib"));
	return 0;
}

// App command to run the dialog
void CToolibApp::OnAppAbout()
{
	CAboutDlg aboutDlg;
	aboutDlg.DoModal();
}

// CToolibApp message handlers



