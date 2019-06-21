/************************************************************************
c
c   FILE NAME: Makepost.cpp
c
c	 CONTAINS: Defines the class behaviors for the application.
c	 all CMPostApp class override functions and 
c			Implementation functions
c
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        makepost.cpp , 25.3
c     DATE AND TIME OF LAST  MODIFICATION
c        03/11/16 , 11:32:02
c**********************************************************************
*/

#include "pwenv.h"
#include "PwStdafx.h"
#include <stdio.h>
#include <direct.h>
#include "Mpost.h"
#include "DialogTemplate.h"
#include "NpwHeaders.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

int Main_first = 1 ;
int MpChanged = 0;
char copyRight[82];
extern char Pw_formdlg_font[80];
extern char Pw_promptdlg_font[80];
extern char Pw_helpdlg_font[80];
extern char Pw_butdlg_font[80];

/*********************************************************************
**  E_FUNCTION: pw_remove_quotes (buf)
**          This function removes quotes from a string.
**    PARAMETERS
**       INPUT  :   buf = input/output string.
**		 OUTPUT :	buf = output string.
**
**    RETURNS      :    None.
**
**    SIDE EFFECTS : none
**
**    WARNINGS     : none.
*********************************************************************/
void pw_remove_quotes(char *buf)
{
	char *fq,*index(),*rindex();
/*
.....Remove bounding quote marks
.....from string
*/
    fq = strchr(buf,'\"');
	if (fq != NULL)
	{
		strcpy(buf,(fq+1));
		fq = strrchr(buf,'\"');
		if (fq != NULL) *fq = '\0';
	}
	return;
}

/***********************************************************************
c
c   FUNCTION: readinenv(char* filename)
c
c         Read a initial file and set enviroment
c			for this application
c
c   INPUT:  filename: initialize file
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
static int readinenv(char* filename)
{
	FILE *fp;
	char envlin[500];
	char *str;
	if ((!strlen(filename)) || ((fp = fopen(filename, "r") )== NULL))
	{
		return -1;
	}
	while (fgets(envlin, 500, fp))
	{
		str = strtok(envlin, "\n\r");
		if (str!=NULL)
		{
			strcpy(envlin, str);
			_putenv(envlin);
		}
	}
	return 0;
}

/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
BEGIN_MESSAGE_MAP(CMPostApp, CWinApp)
	//{{AFX_MSG_MAP(CMPostApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG
	ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CMPostApp construction

CMPostApp::CMPostApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

CMPostApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CMPostApp initialization

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
BOOL CMPostApp::InitInstance()
{
//	AfxEnableControlContainer();

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.


	int argc;
	char **argv;
	int istat;
	char winLabel[82],cmsg[82], cmline[132], lerr[20];


	m_newpage = 0;
	m_pageParent = NULL;

	LoadStdProfileSettings();  // Load standard INI file options (including MRU)
	char cdir[UX_MAX_PATH];
	_getcwd(cdir,UX_MAX_PATH);
	strcat(cdir,"\\mpost.init");
	readinenv(cdir);
/*
	argc = 1;
*/
/*
.....Let accept command line
.....Yurong 8/25/99
*/
	CWinApp* pApp = AfxGetApp();
	LPTSTR lpCmdLine;
	lpCmdLine = pApp->m_lpCmdLine;
	strcpy(cmline, lpCmdLine);

	istat = NpwNTInitApp(copyRight,winLabel,cmline,cmsg);
	if (istat != 0)
	{
		if (istat == -1)
			MessageBox(NULL, cmsg, "Warning", MB_OK);
		else
		{
			MessageBox(NULL, cmsg, "Error", MB_OK);
			exit(0);
		}
	}

/*
......Get the number of buttons to get from the application
*/
	npwDynWinStruct winStruct;
	npwDynFormStruct formStruct;
	char message[80];
	int level[10];
	int curStage;
	level[0] = 0;
	curStage = 1;
	
	int result = NpwGetWinLayout(level,&curStage,&winStruct, 
		&formStruct, message);
	if(result)
	{
/*
.....we have an error here, abort application
*/
		if (MessageBox(NULL, "NpwGetWinLayout error", "Error!", MB_OK)==IDOK)
		exit(1);
	}
/*
.....get font
*/
	char *p;
	strcpy(Pw_formdlg_font, "Courier");
	strcpy(Pw_helpdlg_font, "Courier");
	strcpy(Pw_butdlg_font, "Arial");
	strcpy(Pw_promptdlg_font, "Courier");

	p = getenv ("PW_formdlg_font");
	if (p!=NULL)
	{
		strcpy(Pw_formdlg_font, p);
		pw_remove_quotes(Pw_formdlg_font);
	}

	p = getenv ("PW_helpdlg_font");
	if (p!=NULL)
	{
		strcpy(Pw_helpdlg_font, p);
		pw_remove_quotes(Pw_helpdlg_font);
	}

	p = getenv ("PW_butdlg_font");
	if (p!=NULL)
	{
		strcpy(Pw_butdlg_font, p);
		pw_remove_quotes(Pw_butdlg_font);
	}

	p = getenv ("PW_promptdlg_font");
	if (p!=NULL)
	{
		strcpy(Pw_promptdlg_font, p);
		pw_remove_quotes(Pw_promptdlg_font);
	}
/*
......Create our main window and display it
*/
	Main_first = 1;
	CDialogButton* dlg = new CDialogButton(NULL, 1, level, curStage, -1);
	dlg->inittemp(&winStruct);
	dlg->Create();
	m_pMainWnd = dlg;
	CoInitialize(NULL);
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
int CMPostApp::Run()
{
	MSG msg;
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
/*
......pump message, but quit on WM_QUIT
*/
			if (!PumpMessage())
			{
				CoUninitialize();
				return ExitInstance();
			}

			if (IsIdleMessage(&msg))
			{
				bIdle = TRUE;
				lIdleCount = 0;
			}

		} while (::PeekMessage(&msg, NULL, NULL, NULL, PM_NOREMOVE));
	}

	ASSERT(FALSE);  // not reachable
}
