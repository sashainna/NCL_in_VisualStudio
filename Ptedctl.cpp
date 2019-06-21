/************************************************************************
c
c   FILE NAME: Pted.cpp
c
c	 CONTAINS: 
c		Defines the class behaviors for the application.
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			Ptedctl.cpp , 24.3
c     DATE AND TIME OF LAST  MODIFICATION
c			11/04/13 , 09:05:29
c
c**********************************************************************
*/

#include "pwstdafx.h"
#include <afxtempl.h>
#include <iostream>
#include <direct.h>
#include <conio.h>
#include "Pted.h"
#include "PtedBatch.h"
#include "PtedMainWindow.h"

#include "PtdFunc.h"
#include "PtdGlobal.h"
#include "PWMessageBox.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern int UU_BATCH;
extern FILE *PW_logfile;
extern PWMessageBox PW_msgbox;
extern CWnd *Pw_mainwin;
int FontSize=12;

extern "C" int Pw_dispmsg(char *msgline, int flag);
extern "C" int Ptd_checkkey(char *msg);
extern "C" void Ptd_releasekey();
char file_list[5][UX_MAX_PATH];
char PWINIT_FILE[UX_MAX_PATH];
extern "C" void Pted_disply_ProcessDlg(char *title);
extern "C" void Pted_Display_Dlg_percent(int num);
extern "C" void Pted_Close_ProcessDlg();

extern "C" char Pted_localdir[UX_MAX_PATH];
static char colorname[256][20];
static COLORREF colorvalue[102];
static char editname[102][20] = {"MSG", "MSGTEXT", "NUMBER", "SYMBOLS", "MAJOR", "MINOR",
				"EOB", "OPSKIP", "TAB", "REWIND", "A1", "A2", "A3", "B1", "B2", 
				"B3", "C1", "C2", "C3", "C4", "C5", "C6", "D", "E", "F1", "F2",
				"F3", "G0", "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9",
				"GA", "H", "I1", "I2", "J1", "J2","K1", "K2", "L", "M0", "M1", "M2",
				"M3", "M4", "M5", "M6", "M7", "M8", "M9", "MA", "N", "O", "P", "Q",
				"R", "S", "T","U1", "U2", "V1", "V2", "W1", "W2", "X1", "X2",
				"Y1", "Y2", "Z1", "Z2", "AA", "AB", "AC", "AD", "AE", "AF", "AG",
				"AH", "AI", "AJ", "AK", "AL", "AM", "AN", "AO", "AP", "AQ", "AR",
				"AS", "AT", "AU", "AV", "AW", "AX", "AY", "AZ"};
COLORREF Ptd_editcolor[102] = { RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), RGB(0,0,0), 
				RGB(0,0,0), RGB(0,0,0) };

PtedBatch *m_pBatch = new PtedBatch("Batch");

COLORREF get_color_value(char *string)
{
	char *tok;
	int red=0, green = 0, blue = 0;
	if ((string==NULL) || (string[0]=='\0'))
	{
		return RGB(0,0,0);
	}
	tok = strtok(string, " ,\r\n");
	if (tok!=NULL)
	{
		red = atoi (tok);
		if ((red<0)||(red>255))
			red = 0;
		tok = strtok(NULL, " ,\r\n");
		if (tok!=NULL)
		{
			green = atoi (tok);
			if ((green<0)||(green>255))
				green = 0;
			tok = strtok(NULL, " ,\r\n");
			if (tok!=NULL)
			{
				blue = atoi (tok);
				if ((blue<0)||(blue>255))
					blue = 0;
			}
		}
	}
	return RGB(red, green, blue);
}

void Ptd_loadedit_color()
{
	FILE *fp;
	int i, j, k;
	char efile[UX_MAX_PATH], *fdir, *tok, linestr[80];

	strcpy(efile,Pted_localdir);
	strcat(efile,"\\postedit.txt");
	if ((  (fp = fopen(efile, "r") ) == NULL))
	{
		fdir = getenv("PWORKS_DATA");
		if (fdir==NULL)
			return;
		strcpy(efile, fdir);
		strcat(efile,"\\postedit.txt");
		if ((  (fp = fopen(efile, "r") ) == NULL))
			return;
	}
	i=0;
	j=-1;
	while (fgets(linestr, 80, fp))
	{
		if (linestr[0]=='#')
			continue;
/*
.....check the string is a colorname defination or "symbols color" definamton
*/
		tok = strtok(linestr, " =\r\n#");
		if (tok==NULL) continue;
		for (i=0; i<102; i++)
		{
			if (_stricmp(editname[i], tok)==0)
			{			
				tok = strtok(NULL, " =\r\n#");
				if ((tok!=NULL)&&(j>=0))
				{
					for (k=0; k<=j; k++)
					{
						if (_stricmp(colorname[k], tok)==0)
						{
							Ptd_editcolor[i] = colorvalue[k];
							break;
						}
					}
				}
				break;
			}
		}
		if (i>=102)
		{
/*
.....not a symbol defination, so it must be a color variable defined by user
*/
			j++;
			strcpy(colorname[j], tok);
			tok = strtok(NULL, " =\r\n#");
			if (tok!=NULL)
			{
				colorvalue[j] = get_color_value(tok);	
			}
			else
				j--;
		}
	}
}

/////////////////////////////////////////////////////////////////////////////
// CPtedApp

BEGIN_MESSAGE_MAP(CPtedApp, CWinApp)
	//{{AFX_MSG_MAP(CPtedApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG
//	ON_COMMAND(ID_HELP, CWinApp::OnHelp)
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPtedApp construction

CPtedApp::CPtedApp()
{
	for (int i=0; i<50; i++)
		m_argv[i] = NULL;
}

/////////////////////////////////////////////////////////////////////////////
// The one and only CPtedApp object

CPtedApp theApp;

/***********************************************************************
**
**   FUNCTION: InitInstance()
**
**         This function is to initialize Pted application 
**
**   INPUT:  None
**
**   OUTPUT  None
**   RETURN:  None
**
**********************************************************************/
BOOL CPtedApp::InitInstance()
{
	int stat,narg;
	int len;
	char lmsg[80];
	int i, status;
	FILE *fp;
	char initfile[UX_MAX_PATH];
	char *fdir;
	CWinApp* pApp;
	char * lpCmdLine, comstr[UX_MAX_PATH+80];
	char tempdir[UX_MAX_PATH];
	CString tmp;

	AfxEnableControlContainer();

	// Standard initialization
	// If you are not using these features and wish to reduce the size
	//  of your final executable, you should remove from the following
	//  the specific initialization routines you do not need.

	GetCurrentDirectory(_MAX_PATH, Pted_localdir);

	status = Ptd_checkkey(lmsg);
	if (status==-1)
	{
		MessageBox(NULL, lmsg, "Pted Message", MB_OK|MB_ICONERROR);
		return FALSE;
	}
	GetTempPath(UX_MAX_PATH, tempdir);
	Ptd_Appinit();

/*
.....Open the initialization file
*/
	_getcwd(initfile,UX_MAX_PATH);
	strcat(initfile,"\\pted.ini");
	narg = 0;
	if ((  (fp = fopen(initfile, "r") ) == NULL))
	{
		fdir = getenv("PWORKS_DATA");
		if (fdir==NULL)
			goto command_line;

		strcpy(initfile, fdir);
		strcat(initfile,"\\pted.ini");
		if ((  (fp = fopen(initfile, "r") ) == NULL))
			goto command_line;
	}
/*
.....Parse the initialization file
*/
	while (fgets(comstr, 80, fp))
	{
		i = strlen(comstr) - 1;
		while (comstr[i] == '\n' || comstr[i] == '\r')
		{
			comstr[i] = '\0';
			i--;
		}
		ParseCmdLine(comstr,&narg);
	}
/*
.....Get the command line
*/	
command_line:;
	pApp = AfxGetApp();
	lpCmdLine = pApp->m_lpCmdLine;
	strcpy(comstr, lpCmdLine);
/*
.....Parse the command line
*/
	ParseCmdLine(comstr,&narg);
/*
.....See if we are running in quiet mode
*/
	UU_BATCH = 0;
	for (i=1;i<narg;i++)
	{
		tmp = m_argv[i];
		tmp.MakeUpper();
		if (tmp.Find("-QUIET")!=-1)
		{
			UU_BATCH = 2;
			break;
		}
	}
/*
.....put message into log file when run batch
*/
	if (UU_BATCH == 2)
	{
/*
.....Open log file
*/
		PW_logfile = fopen("pted.log", "a+");
		m_pBatch->Run(narg, m_argv);
		if (m_pBatch->m_batch_cnt==0)
		{
			i = 0;
			while (m_argv[i])
			{
				free(m_argv[i]);
				i++;
			}
			delete m_pBatch;
			Pw_dispmsg("Pted Batch completed.", 1);
			Pted_Close_ProcessDlg();
			goto done;
		}
		fclose(PW_logfile);
		UU_BATCH = 0;
	}
	else
	{
/*
......only open a message box when there is a batch message to be displayed
*/
//		UU_BATCH = 3;
		UU_BATCH = 0;
		Pted_disply_ProcessDlg("Pted Batch Start...");
		Pted_Display_Dlg_percent(-50);
		m_pBatch->Run(narg, m_argv);
		Pted_Close_ProcessDlg();
		if (m_pBatch->m_batch_cnt==0)
		{
			Ptd_releasekey();
			goto done;
		}
		UU_BATCH = 0;
	}
window:;
/*
......initial window edit color
*/
	Ptd_loadedit_color();

	fdir = getenv("PWORKS_DATA");
/*
.....initial privous file list
*/
	char user_name[80];
	char errMsg[300];
	DWORD fsize;
	fsize = 80;
	stat = GetUserName(user_name, &fsize);
	m_flist_name[0] = '\0';
	if (fdir!=NULL)
	{
		strcat(m_flist_name, fdir);
		strcat(m_flist_name, "\\");
	}
	strcat(m_flist_name, "PTED_FILES_");
	strcat(m_flist_name, user_name);
	strcat(m_flist_name, ".PFI");
	char flist[5][UX_MAX_PATH];
	stat = Ptd_open_flist(m_flist_name, flist, errMsg);
/*
.....set temp init file name for running PWORKS
*/
/*
......don't use tmpnam since this just generate a unique tmpnam, but it could
......be in top directory (such as c:) but have problems when create it and open it.
......since a lot of machine don't allowed to create a new file by default, 
......so I changed to use _tempnam (which will generate a unique name too)
......to have a tempname in local Pted directory
*/
	char *tmpinit, localdir[UX_MAX_PATH];
//	tmpnam(PWINIT_FILE);
	GetCurrentDirectory(_MAX_PATH, localdir);
	tmpinit = (char*)malloc(UX_MAX_PATH*sizeof(char));
	tmpinit = _tempnam(localdir, "INIT");
	strcpy(PWINIT_FILE, tmpinit);
	free(tmpinit);

	CPtedMainWindow* dlg;
	if (m_argv[0]!=NULL)
	{
		dlg = new CPtedMainWindow(m_argv[0], flist);
	}
	else
		dlg = new CPtedMainWindow(NULL, flist);
	dlg->Create(IDD_WINDOWDIALOG);;
	dlg->ShowWindow(TRUE);
	dlg->SetViewFocus();

	m_pMainWnd = dlg;
	Pw_mainwin = dlg;
/*
.....remove temp file
*/
done:
	i = 0;
	while (m_argv[i])
	{
		free(m_argv[i]);
		i++;
	}
	delete m_pBatch;
	return TRUE;
}

/***********************************************************************
**
**   FUNCTION: ParseCmdLine(cmdstr,narg)
**
**         This function parses a command line and places the furnished
**         arguments into the 'm_argv' array.
**
**   INPUT:
**      cmdstr   = Command line string to parse.
**      narg     = Number of arguments currently stored.
**
**   OUTPUT:
**      narg     = Updated number of arguments.
**   RETURN:  None
**
**********************************************************************/
void CPtedApp::ParseCmdLine(char *cmdstr, int *narg)
{
	int inc,nc,i,iqot,ist,font;
	char str[1024],*indx;
/*
.....Initialize routine
*/
	inc = 0;
	ist = *narg;
	iqot = 0;
	nc = strlen(cmdstr);
/*
.....Break out parameters
*/
	for (i=0;i<=nc;i++)
	{
/*
.....Position to next non-space
*/
		if (i != nc)
		{
			while (cmdstr[i] != ' ' && i > nc) i++;
/*
........Allow quoted strings
*/
			if (cmdstr[i] == '\"')
			{
				if (iqot == 1) iqot = 0;
				else iqot = 1;
				continue;
			}
		}
/*
........Store argument
*/
		if (i == nc || ((cmdstr[i] == ' ' || cmdstr[i] == '\t') && iqot == 0))
		{
			if (ist == 0) ist = 1;
			str[inc] = '\0';
/*
...........Unmatched quotes
*/
			if (i == nc && iqot == 1)
			{
				MessageBox(NULL,"Beginning quote without ending quote","Error!",
					MB_ICONINFORMATION|MB_OK);
				return;
			}
/*
...........Filename
*/
			if (str[0] != '-' && str[0] != '/' && m_argv[0] == NULL)
			{
				m_argv[0] = (char*)malloc((inc+2)*sizeof(char));
				strcpy(m_argv[0],str);
			}
/*
...........Runtime option
*/
			else
			{
/*
..............Breakout FONT setting
*/
				if (strncmp(&str[1],"FON",3) == 0 || strncmp(&str[1],"fon",3) == 0)
				{
					indx = strchr(str, ':');
					if (indx != NULL)
					{
						indx++;
						font = 0; font = atoi(indx);
						if (font >= 8 && font <= 72)
						{
							FontSize = font;
							continue;
						}
					}
				}
				m_argv[ist] = (char*)malloc((inc+2)*sizeof(char));
				strcpy(m_argv[ist],str);
				ist++;
			}
			inc = 0;
		}
/*
........Store character in argument
*/
		else
		{
			str[inc] = cmdstr[i];
			inc++;
		}
	}
/*
.....Define number of arguments provided
*/
	*narg = ist;
}

PROCESS_LOCAL(_AFX_WIN_STATE, _afxWinState)
