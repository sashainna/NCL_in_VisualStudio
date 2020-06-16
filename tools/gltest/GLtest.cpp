/************************************************************************
c
c   FILE NAME: testOG.cpp
c
c	 CONTAINS: 
c		BOOL COGApp::InitInstance()
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        GLtest.cpp , 21.1
c     DATE AND TIME OF LAST  MODIFICATION
c        12/10/09 , 18:01:42
c
c**********************************************************************
*/
#include "glStdafx.h"
#include "GLtest.h"

#include "testMainFrm.h"
#include "GLtestDoc.h"
#include "GLtestView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
extern "C" FILE *GLjou_fd, *GLcode_file;
extern "C" int open_input_output();

BEGIN_MESSAGE_MAP(COGApp, CWinApp)
	//{{AFX_MSG_MAP(COGApp)
	ON_COMMAND(ID_APP_ABOUT, OnAppAbout)
	//}}AFX_MSG_MAP
	ON_COMMAND(ID_FILE_NEW, CWinApp::OnFileNew)
	ON_COMMAND(ID_FILE_PRINT_SETUP, CWinApp::OnFilePrintSetup)
END_MESSAGE_MAP()

COGApp::COGApp(){}
COGApp theApp;

BOOL COGApp::InitInstance()
{
	Enable3dControls();
	SetRegistryKey(_T("Local AppWizard-Generated Applications"));

	LoadStdProfileSettings();

	CSingleDocTemplate* pDocTemplate;
	pDocTemplate = new CSingleDocTemplate(
		IDR_MAINFRAME,
		RUNTIME_CLASS(COGDoc),
		RUNTIME_CLASS(CMainFrame),
		RUNTIME_CLASS(COGView));
	AddDocTemplate(pDocTemplate);

	CCommandLineInfo cmdInfo;
	ParseCommandLine(cmdInfo);

	int stat = open_input_output();

	if (stat==-1)
		return FALSE;
	if (!ProcessShellCommand(cmdInfo))
		return FALSE;
	m_pMainWnd->ShowWindow(SW_SHOW);
	m_pMainWnd->UpdateWindow();
	if ((GLjou_fd == NULL) || (GLcode_file==NULL))
		return FALSE;
	return TRUE;
}

void COGApp::OnAppAbout()
{
	CDialog(IDD_ABOUTBOX).DoModal();
}

