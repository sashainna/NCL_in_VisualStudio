/********************************************************************* 
**  NAME:  wsntctl.cpp
**
**			Defines the class behaviors 
**				for the NCL application.
**			implementation of CNCLApp class functions
**		CONTAINS: CNCLApp class functions
**			all functions declared in wsntctl.h
**			define and inplement of CAboutDlg dialog
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       wsntctl.cpp , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**       08/18/15 , 08:45:25
*********************************************************************/

#include "wsntstdafx.h"
#include <signal.h>
#include "wsntctl.h"
#include "wsntframe.h"
#include "wsntdoc.h"
#include "wsntview.h"
#include "dmark.h"
#include "dmotif.h"
#include "spmouse.h"
#include "UxTheme.h"

#define  NCLWNT_CCMAIN
#include "wsntfuncid.h"
#undef	NCLWNT_CCMAIN
#include "wsgl.h"
#include "wsntcfunc.h"
#include "wsntglfunc.h"
#include "wsglfun.h"
#include "wsntmmap.h"
#include "dselect.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

UINT NCL_EXITMSG = RegisterWindowMessage("Ncl_running_exit");
UINT NCL_UPDATEMSG = RegisterWindowMessage("Ncl_running_update");
UINT NCQ_REQUEST_MSG = RegisterWindowMessage("Ncq_request_ncl_info");
extern int frm_done;
int NCL_exit = 0;
int NCL_safeexit = 0;
DWORD UW_theme;
extern "C" int UD_firsttime;
extern "C" int nclrun(int argc, char**argv);
extern "C" int UD_pickmode;
extern "C" int ud_jump(int,int);
extern "C" int uw_ntsignoff(int flag);
extern "C" int isquit ();
extern "C" int UW_signon_failed;
extern "C" int ud_playinit();
extern "C" int uw_ntflush_paint();
extern "C" int ud_updatews(int);

extern "C" int NCL_event_reject;
int UW_ncq_id;
extern "C" int NCQ_running;

extern "C" int Ncq_ncl_id;
int NCL_common_open = 0;
extern "C" int NCL_com_size;
CMemoryMappedFile NCL_common_file, NCL_keycommon_file;
extern "C" int NCQ_filenum;
extern "C" unsigned int NCL_subprocess;
typedef struct
{
	int flag;
	char ppfile[256];
	int current, highest, lines;
	char macro[64];
	int warn, error;
} NCLInfo;
extern "C" char NCL_keyinfo[9];
extern "C" int ud_getbat_info(NCLInfo *info);
extern "C" int ud_setpick_type(int);
extern "C" int uw_ntsignon();
extern "C" int uz_status();
extern "C" void ud_update_win_title();
extern "C" void uw_nt_nclfunc(UINT id, int jflag);
extern "C" void uw_set_update_stat(int);

//extern "C" void SetThemeAppProperties(DWORD);
//temp
extern "C" void getdir (char *dir, int *nc)
{
	GetCurrentDirectory(UX_MAX_PATH_LEN, dir);
	*nc = strlen (dir);
}

/***********************************************************************
**
**   SUBROUTINE: uw_chk_common()
**
**   FUNCTION:  check if we have the common memory area (Ncq_ncl_id as key)
**				if yes, attached to global buffer
**
**   INPUT:  none
**
**   OUTPUT: none
**	 RETURN: 0: common memory area not opened.
**				1: common memory area opened.
**
***********************************************************************/
extern "C" int uw_chk_common()
{
	char common_file[256];
	if (Ncq_ncl_id==-1)
		return 0;
	sprintf_s(common_file, "NCL_NCQ_%d", Ncq_ncl_id);
	if (common_file[0] == '\0')
	{
		NCL_common_open = 0;
		return 0;
	}
	NCL_com_size = sizeof (NCLInfo) + 2 * NCQ_filenum * 
					(3*sizeof (int) + (12+9)*sizeof (char));
	int stat = NCL_common_file.Open(common_file, INVALID_HANDLE_VALUE,
			PAGE_READWRITE, 0, NCL_com_size);
	if (stat!=-1)
	{
/*
.....the memory block not open yet, it maybe NCQ open MEMORY failed, or NCQ
.....have already close, so we will have write to this memory later
*/
		::MessageBox(NULL, "The common NCQ memory not exist", "Error!", MB_OK);
/*
.....if we opened memory block in above NCL_common_file.Open, we need close it
.....The NCQ open/close memory block, not the NCL
*/
		NCL_common_file.Close();
		NCL_common_open = 0;
		return 0;
	}
	NCL_common_open = 1;
	return 1;
}
/***********************************************************************
**
**   SUBROUTINE: uw_read_comblk(NCLInfo *info)
**
**   FUNCTION:  This routine write NCL information into common memory area
**
**   INPUT:  info:         NCLInfo structure contain NCL information
**
**   OUTPUT: none
**
***********************************************************************/
extern "C" 	void uw_write_comblk(NCLInfo *info)
{
	if (NCL_common_open)
	{
		NCL_common_file.SeekToBegin();
		NCL_common_file.Write(info, sizeof (NCLInfo));
	}
}

/*********************************************************************
**    E_FUNCTION     : uw_nthandle_exit()
**       Send a NCL EXIT message to NCQ
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_nthandle_exit()
{
/*
.....send thread message before exit
*/	
	NCLInfo info;
/*
.....flag=1 NCL update flag
*/
	info.flag = 1;
	strcpy_s(info.ppfile, "NCL EXIT");
	uw_write_comblk(&info);
}

/*********************************************************************
**    E_FUNCTION     : uw_nthandle_exit2()
**       handle exit for sub-NCL process
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_nthandle_exit2()
{
}


/*********************************************************************
**    E_FUNCTION     : uw_ntupdate_ncq()
**       Send a NCL UPDATE message to NCQ
**    PARAMETERS
**       INPUT  :
**          none
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntupdate_ncq()
{
	NCLInfo info;
/*
.....flag=1 NCL update flag
*/
	info.flag = 1;
	strcpy_s(info.ppfile, "NCL UPDATE");
	uw_write_comblk(&info);
}

/*********************************************************
************
**    E_FUNCTION     : uw_ntsend_NCLinfo()
**       Get and Send a NCL information message to NCQ
**    PARAMETERS
**       INPUT  :
**			none
**       OUTPUT :
**          none
**    RETURNS      :
**          none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
extern "C" void uw_ntsend_NCLinfo()
{
	NCLInfo info;
	if (Ncq_ncl_id==-1)
		return;
	if (NCQ_running)
	{
		ud_getbat_info(&info);
		info.flag = 3;
		uw_write_comblk(&info);
	}
}


/////////////////////////////////////////////////////////////////////////////
// CNCLApp

BEGIN_MESSAGE_MAP(CNCLApp, CWinApp)
	//{{AFX_MSG_MAP(CNCLApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
	// Standard file based document commands
	ON_COMMAND(ID_APP_EXIT, OnNCLSafeExit)
	ON_COMMAND(ID_FILE_NEW, CWinApp::OnFileNew)
	ON_COMMAND(ID_FILE_OPEN, CWinApp::OnFileOpen)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNCLApp construction

CNCLApp::CNCLApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}


/////////////////////////////////////////////////////////////////////////////
// The one and only CNCLApp object

CNCLApp theApp;

/////////////////////////////////////////////////////////////////////////////
// CNCLApp initialization

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
BOOL CNCLApp::InitInstance()
{
	m_strMyClassName = AfxRegisterWndClass(0, 
			::LoadCursor (NULL, IDC_ARROW), 
			(HBRUSH) (COLOR_WINDOW+1),
			LoadIcon(IDI_NCLICON));
//the following function will not allow create dynamic dialog view
//
//	AfxEnableControlContainer();
	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

	SetRegistryKey(_T("NCL"));
	LoadStdProfileSettings();  // Load standard INI file options (including MRU)

	// Register the application's document templates.  Document templates
	//  serve as the connection between documents, frame windows and views.
	CWinApp* pApp;
	pApp = AfxGetApp();
/*
.....Let accept command line
*/
	LPTSTR lpCmdLine;
	char cmdline[UX_MAX_PATH_LEN];
	lpCmdLine = pApp->m_lpCmdLine;
	strcpy_s(cmdline, _countof(cmdline), lpCmdLine);
	char *str, **argv, tmpstr[100], *next_str;
	int i, status;

	//afxAmbientActCtx = FALSE;
/*
.....we will using parse routine like parse a C command
.....line, so the command itself is first arg
.....Yurong 6/7/00
*/
	argv = (char**)malloc(15*sizeof(char*));
	for (i=0; i<15; i++)
		argv[i] = (char*)malloc(UX_MAX_PATH_LEN*sizeof(char));

	strcpy_s(argv[0], UX_MAX_PATH_LEN, "NCL");
	i = 1;
	int defined = 0;
	char *indx;
	if (cmdline[0]!='\0')
	{
		indx = strchr(cmdline, '\"');
		if (indx!=0)
		{
			strcpy_s(cmdline, UX_MAX_PATH_LEN, &(cmdline[1]));
			indx = strchr(cmdline, '\"');
			if (indx!=0)
			{
				*indx = '\0';
				strcpy_s(argv[i], UX_MAX_PATH_LEN, cmdline);
				i++;
				strcpy_s(cmdline, UX_MAX_PATH_LEN, indx+1);
			}
			else
				MessageBox(NULL, "Double quotes do not match!","Error!", MB_ICONINFORMATION|MB_OK);
		}
		str = strtok_s(cmdline, " \t\n", &next_str);
		while(str)
		{
			if ((strcmp(str,"-b")==0)|| (strcmp(str,"-B")==0)
				|| (strcmp(str,"BATCH")==0))
			{
				_putenv("runtype=BATCH");
				defined = 1;
			}
			else if ((strncmp(str,"-q",2)==0)|| (strncmp(str,"-Q",2)==0))
			{
				_putenv("runtype=BATCH");
				strcpy_s(tmpstr, 100, &(str[3]));
				UW_ncq_id = atoi(tmpstr);
				defined = 1;
				atexit(uw_nthandle_exit);
			}
			else if ((strcmp(str,"-v")==0)|| (strcmp(str,"-V")==0)
				|| (strcmp(str,"NCLVT")==0))
			{
				_putenv("runtype=NCLVT");
				defined = 1;
			} 
			else if ((strncmp(str,"-n=",3)==0)|| (strncmp(str,"-N=",3)==0))
			{
				atexit(uw_nthandle_exit2);
			}
			strcpy_s(argv[i], UX_MAX_PATH_LEN, str);
			i++;
			str = strtok_s(NULL, " \t\n", &next_str);
		}
		argv[i] = NULL;
	}
	if (defined!=1)
		_putenv("runtype=INTER");
	_putenv("ws=openGL");
	status = nclrun(i, argv);
/*
.....free argv
*/
	for (i=0; i<15; i++)
		free (argv[i]);
	free ((char*)argv);
	if (status!=0)
	{
		return FALSE;
	}
/*
......SetThemeAppProperties(0) will disable COM common dialog functions
......so save here in order for reset back
......I think we initial use SetThemeAppProperties(0) to disply toolbar/menubar nicely,
......especially on XP, there might be other reasons we can't remember, so
......I will still leave it and save the theme in order to set back
*/
	UW_theme = GetThemeAppProperties();
// this call needed for XP as I remember is for the looks toolbar menu,
// changed for use for STAP_ALLOW_NONCLIENT in order for form change
// to looks model
//	SetThemeAppProperties(0);
	SetThemeAppProperties(STAP_ALLOW_NONCLIENT);

	CSingleDocTemplate* pDocTemplate;
	pDocTemplate = new CSingleDocTemplate(
		IDR_MAINFRAME,
		RUNTIME_CLASS(CNCLDoc),
		RUNTIME_CLASS(CMainFrame),       // main SDI frame window
		RUNTIME_CLASS(CNCLView));
	AddDocTemplate(pDocTemplate);

	// Parse command line for standard shell commands, DDE, file open
	CCommandLineInfo cmdInfo;
	if (!ProcessShellCommand(cmdInfo))
		return FALSE;
/*
.....signon here
*/
	uw_ntsignon();
	uw_set_update_stat(1);
	uz_status();
	uw_set_update_stat(0);

	if (isquit ())
		return FALSE;
	if (UW_signon_failed)
		return FALSE;

	if (UDM_layout.maximize!=1)
		m_pMainWnd->ShowWindow(SW_SHOW);
	else
		m_pMainWnd->ShowWindow(SW_SHOWMAXIMIZED);
	m_pMainWnd->UpdateWindow();
	ud_update_win_title();
	uw_ntsetcursor(1);
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
int CNCLApp::Run()
{
	int jmpflag, playback, playback_bIdle;
	MSG msg;

start:;
	UD_enablejmp = UU_TRUE;
	UD_MARK(jmpflag,UU_TRUE);
	ud_updatews(UG_SUPPRESS);
	UD_pickmode = 0;
	NCL_event_reject = 0;
	UV_Cur_Dyn = 0;
	ud_setpick_type(UD_PICK_NORMAL);
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
		if(UD_firsttime==UU_TRUE)
		{
			ud_playinit();
			UD_firsttime = UU_FALSE;
		}	
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
			playback_bIdle = 0;
			if (frm_done == 1)
				playback = ud_chk_rprd();
			else
				playback = 0;
			while (bIdle &&
				!::PeekMessage(&msg, NULL, NULL, NULL, PM_NOREMOVE))
			{
				if (playback!=1)
				{
					if (!OnIdle(lIdleCount++))
					bIdle = FALSE; 
				}
				else
				{
					playback_bIdle = 1;
					break;
				}
			}
			if (playback_bIdle==1)
				continue;
			do
			{
////////////
again:;
				ud_updatews(UG_SUPPRESS);
				uw_ntflush_paint();
/////////////
				playback_bIdle = 0;
				if (frm_done == 1)
					playback = ud_chk_rprd();
				else
					playback = 0;
				if (playback!=1)
				{
/*
......pump message, but quit on WM_QUIT
*/
					if (!PumpMessage())
					{
						return ExitInstance();
					}
				}
				else
				{
/*
......if the message is UW_QUIT
......accept and exit, otherwise, ignore the input now and finished playback
*/
					if (msg.message==WM_QUIT)
						return ExitInstance();
					goto again;
				}
				if (IsIdleMessage(&msg))
				{
					bIdle = TRUE;
					lIdleCount = 0;
				}
				if (msg.message==ID_NCL_FUNCTION)
				{
					UINT id = (UINT)msg.wParam;
					uw_nt_nclfunc(id, 0);
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
c   FUNCTION: OnNCLSafeExit()
c
c         safely exit NCL
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLApp::OnNCLSafeExit()
{
/*
.....if it is still in pick mode,
.....get out of the event loop, (but we need
.....post the ID_APP_EXIT message first
.....in order to handel real exit again
*/
	NCL_safeexit = 1;

	ASSERT(m_pMainWnd != NULL);
	if (UD_pickmode==1)
	{
		m_pMainWnd->PostMessage(WM_CLOSE);
		if (UD_pickmode==1)
			ud_jump(-1,0);
	}
	else
	{
		int jmpflag;
		int stat;
		UD_MARK(jmpflag,UU_TRUE);
		stat = -1;
		if (jmpflag == 0)
		{
			stat = uw_ntsignoff(0);
		}
		if (stat == -1)
			NCL_safeexit = 0;
		UD_UNMARK(jmpflag);
	}
}

/***********************************************************************
c
c   FUNCTION: OnNCLAppExit()
c
c         Exit to NCL application
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CNCLApp::OnNCLAppExit()
{
	NCL_exit = 1;
/*
.....if it is still in pick mode,
.....get out of the event loop, (but we need
.....post the WM_CLOSE message first
.....then give something else to MFC window handled
*/
	ASSERT(m_pMainWnd != NULL);
	m_pMainWnd->PostMessage(WM_CLOSE);
/*
....UD_pickmode should handled in OnNCLSafeExit but for safe, still 
....handel here
*/
	if (UD_pickmode==1)
		ud_jump(-1,0);
}
/***********************************************************************
**
**   SUBROUTINE: uw_chkkey_common()
**
**   FUNCTION:  check if we have the common memory area (for NCL keys)
**				if yes, attached to global buffer
**
**   INPUT:  none
**
**   OUTPUT: none
**	 RETURN: 0: common memory area not opened.
**				1: common memory area opened.
**
***********************************************************************/
extern "C" int uw_chkkey_common(char *keys)
{
	char common_file[256];
	if (NCL_subprocess==0)
		return 0;

	sprintf_s(common_file, 256, "NCLkeys_%u", NCL_subprocess);
	if (common_file[0] == '\0')
	{
		strcpy_s(keys, 9, "00000000");
		strcpy_s(NCL_keyinfo, 9, "00000000");
		return 0;
	}
	NCL_keycommon_file.Close();
	int size = sizeof (NCL_keyinfo);
	int stat = NCL_keycommon_file.Open(common_file, INVALID_HANDLE_VALUE,
			PAGE_READWRITE, 0, size);
	if (stat!=-1)
	{
		NCL_keycommon_file.Close();
		strcpy_s(keys, 9, "00000000");
		strcpy_s(NCL_keyinfo, 9, "00000000");
		return 0;
	}
	NCL_keycommon_file.SeekToBegin();
	int count = NCL_keycommon_file.Read(NCL_keyinfo, sizeof(NCL_keyinfo));
	if (count==0)
	{
		NCL_keycommon_file.Close();
		strcpy_s(keys, 9, "00000000");
		strcpy_s(NCL_keyinfo, 9, "00000000");
		return 0;
	}
	strcpy_s(keys, 9, NCL_keyinfo);
	return 1;
}

/***********************************************************************
**
**   SUBROUTINE: ncl_open_keycom(char *common_file)
**
**   FUNCTION:  This routine creates the common memory area for keys
**				and write the NCL_keyinfo into this area
**
**   INPUT:  common_file:			common area file name
**
**   OUTPUT: none
**
***********************************************************************/
extern "C" 	int uw_open_keycom(char *common_file)
{
	int size = sizeof (NCL_keyinfo);
	int stat = NCL_keycommon_file.Open(common_file, INVALID_HANDLE_VALUE,
			PAGE_READWRITE, 0, size);
	if (stat==-1)
/*
.....already open, it shouldn't be, anyway, close it and open again
*/
	{
		NCL_keycommon_file.Close();
		stat = NCL_keycommon_file.Open(common_file);
	}
	if (stat!=1)
	{
		::MessageBox(NULL, "Can't open common memory for keys", "Error!", MB_OK);
		return 0;
	}
	NCL_keycommon_file.SeekToBegin();
	NCL_keycommon_file.Write(&NCL_keyinfo, sizeof (NCL_keyinfo));
	return stat;
}
/***********************************************************************
**
**   SUBROUTINE: uw_close_keycom()
**
**   FUNCTION:  This routine close the common memory area
**
**   INPUT:  none
**
**   OUTPUT: none
**
***********************************************************************/
extern "C" void uw_close_keycom()
{
	NCL_keycommon_file.Close();
}

/////////////////////////////////////////////////////////////////////////////
// CNCLApp commands

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
	if (NCL_exit == 1 || NCL_safeexit == 1) return(UU_TRUE);
	return(UU_FALSE);
}
