/************************************************************************
**
**   FILE NAME: ncq.cpp
**	 CONTAINS: Defines the class behaviors for the NCQ application.
**				also class for about dialog
**
**	 CONTAINS: 
**		CNcqApp::InitInstance()
**		CNcqApp::ExitInstance()
**		extern "C" 	ncq_open_common(char *common_file, int ver)
**		extern "C" 	ncq_close_common()
**		extern "C" 	ncq_read_comblk(NCLInfo *info)
**		extern "C" 	ncq_read_comblk96(NCLInfo96 *info)
**		extern "C" 	ncq_read_commsg
**
**     COPYRIGHT 2003 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**    MODULE NAME AND RELEASE LEVEL
**       ncq.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:13:03
**
************************************************************************
*/
// ncq.cpp : Defines the class behaviors for the application.
//

#include "stdafx.h"
#include <stdio.h>
#include "ncq.h"
#include "ncqres.h"

#include "NcqMainFrm.h"
#include "ncqDoc.h"
#include "ncqView.h"
#include "ncqcom.h"
#include "wsntmmap.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

char NCL_common_name[256] = "";
CMemoryMappedFile NCL_common_file;
int NCQ_comm_pos = 0;

extern "C" int ncq_init();
extern "C" int ncqbatch(char *comstr, int num, char *cmsg);
extern "C" char ncq_localdir[MAX_PATH];
extern "C" char ncq_initfile[MAX_PATH];
extern "C" char ncq_file[MAX_PATH];
extern "C" void ncq_getabout_str(char *str1, char *str2);
extern "C" void ncq_dispmsg(char *cmsg);
extern CNcqView* NCQ_mainview;
int NCQ_common_open = 0;
int NCQ_filenum = 0;
char **NCQ_ppfile = NULL;
int *NCQ_ppf_post = NULL;
extern "C" int NCQ_runver;

/***********************************************************************
**
**   SUBROUTINE: ncq_close_common()
**
**   FUNCTION:  This routine close the common memory area
**
**   INPUT:  none
**
**   OUTPUT: none
**
***********************************************************************/
extern "C" void ncq_close_common()
{
/*
.....Added check to see if common is open - ASF 1/8/14.
*/
	if (NCQ_common_open) NCL_common_file.Close();
	NCQ_common_open = 0;
}
/***********************************************************************
**
**   SUBROUTINE: ncq_open_common(char *common_file)
**
**   FUNCTION:  This routine creates the common memory area
**
**   INPUT:  common_file:			common area file name
**
**   OUTPUT: none
**
***********************************************************************/
extern "C" 	int ncq_open_common(char *common_file, int ver)
{
	NCLInfo info;
	NCLInfo96 info96;
	char ldat[12], ltim[9];
	int size, stat;
	if (ver>=97)
	{
		size = sizeof (NCLInfo) + 2 * NCQ_filenum * 
					(3*sizeof (int) + (12+9)*sizeof (char));
	}
	else
	{
		size = sizeof (NCLInfo96) + 2 * NCQ_filenum * 
					(3*sizeof (int) + (12+9)*sizeof (char));
	}
/*
.....Make sure common is closed before opening - ASF 1/8/14.
*/
	ncq_close_common();
	stat = NCL_common_file.Open(common_file, INVALID_HANDLE_VALUE,
			PAGE_READWRITE, 0, size);
	if (stat==-1)
/*
.....already open, it shouldn't be, anyway, close it and open again
*/
	{
		NCL_common_file.Close();
		stat = NCL_common_file.Open(common_file, INVALID_HANDLE_VALUE,
			PAGE_READWRITE, 0, size);
	}
	if (stat!=1)
	{
		::MessageBox(NULL, "Could not connect to NCL.", "Error!", MB_OK);
		return 0;
	}
	NCL_common_file.SeekToBegin();
	info.flag = 0;
	info96.flag = 0;
	if (ver>=97)
		NCL_common_file.Write(&info, sizeof (NCLInfo));
	else
		NCL_common_file.Write(&info96, sizeof (NCLInfo96));
	int num = 0;
	ldat[0] = '\0';
	ltim[0] = '\0';
	for (int i=0; i<2*NCQ_filenum; i++)
	{
		NCL_common_file.Write(&num, sizeof (int));
		NCL_common_file.Write(&num, sizeof (int));
		NCL_common_file.Write(&num, sizeof (int));
		NCL_common_file.Write(&ldat, 12*sizeof (char));
		NCL_common_file.Write(&ltim, 9*sizeof (char));
	}
	NCQ_common_open = 1;
	NCQ_comm_pos = 0;
	return stat;
}

/***********************************************************************
**
**   SUBROUTINE: ncq_read_comblk(NCLInfo *info)
**
**   FUNCTION:  This routine read NCL information from common memory area
**
**   INPUT:  none
**
**   OUTPUT: info:         NCLInfo structure contain NCL information
**
***********************************************************************/
extern "C" int ncq_read_comblk(NCLInfo *info)
{
	int count;
	if (NCQ_common_open)
	{
		NCL_common_file.SeekToBegin();
		count = NCL_common_file.Read(info, sizeof (NCLInfo));
		return count;
	}
	else
		return -1;
}
/***********************************************************************
**
**   SUBROUTINE: ncq_read_comblk96(NCLInfo96 *info)
**
**   FUNCTION:  This routine read NCL information from common memory area
**				this function is for running NCL96 or earlier
**
**   INPUT:  none
**
**   OUTPUT: info:         NCLInfo structure contain NCL information
**
***********************************************************************/
extern "C" int ncq_read_comblk96(NCLInfo96 *info)
{
	int count;
	if (NCQ_common_open)
	{
		NCL_common_file.SeekToBegin();
		count = NCL_common_file.Read(info, sizeof (NCLInfo96));
		return count;
	}
	else
		return -1;
}

/***********************************************************************
**
**   SUBROUTINE: ncq_read_commsg(int *jobno, int *err, int *warn, char *ldate, char *ltim)
**
**   FUNCTION:  This routine read NCL pp file execution
**						information from common memory area
**
**   INPUT:  none
**				 
**   OUTPUT:  jobno: job number executable now
**				 err: error executable the job
**					warn: warnings executable the job
**			ldate, ltim: date and time	
**
***********************************************************************/
extern "C" int ncq_read_commsg(int *err, int *warn, int *jobno, char *ldate, char *ltim)
{
	int count;
	if (NCQ_common_open)
	{
		if (NCQ_runver>=97)
		{
			if (NCQ_comm_pos==0)
				NCQ_comm_pos = sizeof (NCLInfo);
		}
		else
		{
			if (NCQ_comm_pos==0)
				NCQ_comm_pos = sizeof (NCLInfo96);
		}
		NCL_common_file.Seek(NCQ_comm_pos, CFile::begin);
		count = NCL_common_file.Read(jobno, sizeof (int));
/*
.....if the data is not write by NCL yet (value still 0)
.....we just return
*/
		if ((count==0) || (*jobno<=0))
		{
			*jobno = 0;
			return 0;
		}
		if (*jobno>NCQ_filenum)
		{
			*jobno = 0;
			return 0;
		}		
		count = NCL_common_file.Read(err, sizeof (int));
		count = NCL_common_file.Read(warn, sizeof (int));
		count = NCL_common_file.Read(ldate, 12*sizeof(char));
		count = NCL_common_file.Read(ltim, 9*sizeof(char));
		NCQ_comm_pos = NCQ_comm_pos + 3*sizeof (int) + 9*sizeof(char) 
						+ 12 * sizeof (char);
		ldate[11] = '\0';
		ltim[8] = '\0';
	}
	else
		return -1;
	return 0;
}


/////////////////////////////////////////////////////////////////////////////
// CNcqApp

BEGIN_MESSAGE_MAP(CNcqApp, CWinApp)
	//{{AFX_MSG_MAP(CNcqApp)
	ON_COMMAND(ID_APP_ABOUT, OnAppAbout)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
	// Standard file based document commands
	ON_COMMAND(ID_FILE_NEW, CWinApp::OnFileNew)
	ON_COMMAND(ID_FILE_OPEN, CWinApp::OnFileOpen)
	// Standard print setup command
	ON_COMMAND(ID_FILE_PRINT_SETUP, CWinApp::OnFilePrintSetup)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNcqApp construction

CNcqApp::CNcqApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CNcqApp object

CNcqApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CNcqApp initialization

/***********************************************************************
c
c   FUNCTION: InitInstance()
c
c         initialize new instance of the
c			application running under Windows. 
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CNcqApp::InitInstance()
{
	char cmsg[MAX_PATH+40];
	AfxEnableControlContainer();

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

	// Change the registry key under which our settings are stored.
	// TODO: You should modify this string to be something appropriate
	// such as the name of your company or organization.
	SetRegistryKey(_T("Local AppWizard-Generated Applications"));

	LoadStdProfileSettings();  // Load standard INI file options (including MRU)
//	SetThreadPriority(THREAD_PRIORITY_HIGHEST); 
	HANDLE hd = GetCurrentThread();
	::SetThreadPriority(hd, THREAD_PRIORITY_HIGHEST);
/*
.....we only use one ncq.que file, so we need save this file when first
.....time open with all path because if not we may open a different ncq.que in another
.....directory because of file browser change
*/
	GetCurrentDirectory(MAX_PATH, ncq_localdir);
	GetCurrentDirectory(MAX_PATH, ncq_file);
	
	strcat (ncq_file, "\\ncl.que");
	ncq_init();
/*
......see if there are fillename or option
......on the command line
*/
	CWinApp* pApp;
	pApp = AfxGetApp();
/*
.....Let accept command line
*/
	char * lpCmdLine, comstr[MAX_PATH+40];
	lpCmdLine = pApp->m_lpCmdLine;
	strcpy(comstr, lpCmdLine);
	int num = strlen(comstr);
	int istop;
/*
......if have a command string, parse it
*/
	if (num != 0)		
	{
		UU_BATCH = 1;
		istop = ncqbatch (comstr, num, cmsg);
		if (istop == 1) 
		{
			if (cmsg[0]!='\0')
				ncq_dispmsg(cmsg);
			return FALSE;
		}
	}
	UU_BATCH = 0;
	// Register the application's document templates.  Document templates
	//  serve as the connection between documents, frame windows and views.

	CSingleDocTemplate* pDocTemplate;
	pDocTemplate = new CSingleDocTemplate(
		IDR_MAINFRAME,
		RUNTIME_CLASS(CNcqDoc),
		RUNTIME_CLASS(CMainFrame),       // main SDI frame window
		RUNTIME_CLASS(CNcqView));
	AddDocTemplate(pDocTemplate);

	// Parse command line for standard shell commands, DDE, file open
	CCommandLineInfo cmdInfo;
	ParseCommandLine(cmdInfo);

	// Dispatch commands specified on the command line
	if (!ProcessShellCommand(cmdInfo))
		return FALSE;

	// The one and only window has been initialized, so show and update it.
	m_pMainWnd->ShowWindow(SW_SHOW);
	m_pMainWnd->SetWindowText("Ncl Batch Scheduler (Ncq)");
	m_pMainWnd->UpdateWindow();

	return TRUE;
}
/***********************************************************************
c
c   FUNCTION: ExitInstance()
c
c         clean up when your application terminate
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
int CNcqApp::ExitInstance()
{
	ncq_close_common();
/*
.....free the job list too
*/
	if (NCQ_ppfile!=NULL)
	{
		for (int i=0; i<NCQ_filenum; i++)
			free(NCQ_ppfile[i]);
		free (NCQ_ppfile);
		NCQ_ppfile = NULL;
	}
	return CWinApp::ExitInstance();
}

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
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
	virtual BOOL OnInitDialog();
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
	char label1[256], label2[256];
	
	ncq_getabout_str(label1, label2);
	GetDlgItem(IDC_ABOUTLABEL1)->SetWindowText(label1);
	GetDlgItem(IDC_ABOUTLABEL2)->SetWindowText(label2);

	SetWindowText("About Ncq");
	return 0;
}

// App command to run the dialog
void CNcqApp::OnAppAbout()
{
	CAboutDlg aboutDlg;
	aboutDlg.DoModal();
}

/////////////////////////////////////////////////////////////////////////////
// CNcqApp message handlers


