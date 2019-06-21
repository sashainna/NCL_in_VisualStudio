/************************************************************************
c
c   FILE NAME: DialogButton.cpp
c
c	 CONTAINS: 
c	 all CDialogButton class override functions and 
c			Implementation functions
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        DialogButton.cpp , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        05/05/14 , 14:30:44
c
c**********************************************************************
*/

#include "pwenv.h"
#include "Pwstdafx.h"
#include "PWBrowser.h"
#include "Mpost.h"
#include "DialogButton.h"
#include "DialogForm.h"
#include "DialogPrompt.h"
#include "DialogHelp.h"
#include "Dlgs.h"
#include "mpostres.h"
#include "NpwHeaders.h"
#include "PwNTAboutDlg.h"

static int TOTALITEMS;
static int Slocal_ans,Slocal_saveas;
extern int Main_first;
extern int MpChanged;
extern char copyRight[82];
extern int pagenum_act;
char Pw_butdlg_font[80];
static PROCESS_INFORMATION ms_info;

extern "C" void pwddea(char *);
extern "C" void getmach(char *);
extern "C" void pwg_mdfdsc(char *,char *);
extern void pw_getstr_size(CWnd *win, char *string, int pt, char *fntname, int *wid, int* hgt, int flag);

/***********************************************************************
c
c   MESSAGE_MAP: callback descriptions
c
c***********************************************************************
*/

BEGIN_MESSAGE_MAP(CDialogButton, CDialog)
	//{{AFX_MSG_MAP(CDialogButton)
	ON_COMMAND(ID_VDOC, OnViewDocumentFile)
	ON_COMMAND(ID_VMACH, OnViewMachineSim)
	ON_COMMAND(ID_CDOC, OnCreateDocument)
	ON_COMMAND(ID_CMACH, OnCreateMachineSim)
	ON_COMMAND(ID_LOAD, OnLoadMachine)
	ON_COMMAND(ID_FSAVE, OnSaveMachine)
	ON_COMMAND(ID_FSAVE_AS, OnSaveMachineAs)
	ON_COMMAND(ID_HELP_CONTENTS, OnHelpContents)
	ON_COMMAND(ID_HELP_ABOUT, OnHelpAbout)
	ON_COMMAND_RANGE(ID_PUSH0,ID_PUSH19, OnPushedBut) 
	ON_WM_DROPFILES()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
/***********************************************************************
c
c   SUBROUTINE:  ~CDialogButton()
c   FUNCTION:  Deconstructor
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/

CDialogButton::~CDialogButton()
{
/*	for (int i=0;i<20; i++)
		if (m_dlgTemplate[i]!=NULL)
			delete m_dlgTemplate[i];
*/
}

/***********************************************************************
c
c   SUBROUTINE: CDialogButton(CWnd* pParent, int style, int *level, 
c			int cur_stage, int id)	
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c			int *level	
c			int cur_stage:	
c			int id:		window id.
c			
c   OUTPUT: none
c
c***********************************************************************
*/

CDialogButton::CDialogButton(CWnd* pParent, int style, int *level, 
int cur_stage, int id)	
{
	int i;

	m_style = style;
	m_pParent = pParent;
	m_ChildId = id;
	m_curStage = cur_stage;

	if (level==NULL) cur_stage = 0;
	for(i=0;i<cur_stage;i++)
		m_pLevel[i] = level[i];
	if (m_style==0)
		CDialog::CDialog();
	else
		CDialog::CDialog();
	for ( i=0; i<20; i++)
	{
		m_childtype[i] = 0;
		m_pModeless[i] = NULL;
//temp		m_dlgTemplate[i] = NULL;
		sheet_disp[i] = 0;
	}
	m_pTextDialog = NULL;
	m_close_ans = 0;
	m_hIcon = AfxGetApp()->LoadIcon(IDI_DLGICON);
}

/***********************************************************************
c
c   SUBROUTINE:  Inittemp(NpwDynWinStruct* winStruct)
c
c   FUNCTION:  This function initialize class data
c
c   INPUT:  CWnd* pParent : parent window
c			NpwDynWinStruct* winStruct: window structure of Dynamic template
c			
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogButton::inittemp(NpwDynWinStruct* winStruct)
{
	char tmp[120];
	int cy;
	sprintf(tmp,"%s",winStruct->winTitle);
	m_winTitle = tmp; 
	int i, len1, len2, sizex, sizey;
	len1 = 0;
	for( i=0;i<winStruct->numButtons;i++)
	{
		pw_getstr_size(m_pParent, winStruct->Buttonname[i], 80, Pw_butdlg_font, &sizex, &sizey,1);
		if (sizex>len1) len1 = sizex;
	}

	for( i=0;i<winStruct->numButtons;i++)
	{
		m_rgDlgItem[i].setId(winStruct->ButtonId[i]);
		if (i==(winStruct->numButtons-1))
			m_rgDlgItem[i].Inittemp(winStruct->Buttonname[i], 
				winStruct->Buttype[i], IDCANCEL, 10, sizey+i*(sizey+2), len1, (sizey+2));
		else
			m_rgDlgItem[i].Inittemp(winStruct->Buttonname[i],
			 winStruct->Buttype[i], ID_PUSH0+i, 10, sizey+i*(sizey+2), len1, (sizey+2));
	}
	if (Main_first==1)
	{
		m_rgDlgItem[i].setctl(1);
		m_rgDlgItem[i].setId(i);
		DWORD style = 
	WS_CHILD | WS_VISIBLE | ES_MULTILINE |ES_WANTRETURN | ES_LEFT | ES_READONLY |WS_VSCROLL |WS_HSCROLL | WS_BORDER;
		m_rgDlgItem[i].settemp2(10, (i+1)*(sizey+2), 3*(sizey+2), len1, 0, style);
		m_rgDlgItem[i].setcaption(copyRight);
		i++;
		m_rgDlgItem[i].InitBox(5, 5, (len1+15), (i+3)*(sizey+2)+sizey/2);
		i++;
		cy = (i+2)*(sizey+2)+sizey/2 + 5;
	}
	else
	{
		m_rgDlgItem[i].InitBox(5, 5, (len1+15), (i+1)*(sizey+2));
		i++;
		cy = i*(sizey+2) + 5;
	}
	TOTALITEMS = i;
	m_dlgTempl.cx = (len1+15) + 5;
	m_dlgTempl.cy = cy;
	m_dlgTempl.style = 
	WS_CAPTION | WS_VISIBLE | WS_POPUP |WS_SYSMENU | WS_MINIMIZEBOX | DS_SETFONT;
	m_dlgTempl.dwExtendedStyle = 0;
	m_dlgTempl.x = 100;
	m_dlgTempl.y = 100;
	m_dlgTempl.cdit = i;
}

/***********************************************************************
c
c   SUBROUTINE:  Create()
c
c   FUNCTION:  This function Create Dynamic Template Dialog
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

int CDialogButton::Create()
{
	char msg[80];
	
	WCHAR* szFontName; 
	WCHAR MainMenu[] = L"MainFrame_Menu";
	WCHAR*	szBoxCaption;
	int		nChars, nActualChars, nfChars, stat;

	nChars = strlen(Pw_butdlg_font) + 1;
	szFontName = new WCHAR[nChars];
	nfChars = MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, Pw_butdlg_font, nChars, 
							szFontName, nChars);

	nChars = m_winTitle.GetLength() + 1;
	szBoxCaption = new WCHAR[nChars];
	nActualChars = MultiByteToWideChar(CP_ACP, 0, m_winTitle, -1, szBoxCaption, nChars);
					

	ASSERT(nActualChars > 0);

/*
......We will first convert the control captions to UNICODE
*/
	int		nTotalLength = 0;  
	int		i;
/*
...... catch memory exceptions and don't worry about allocation failures
*/
	TRY  
	{
		int nBufferSize =  sizeof(DLGTEMPLATE) +
					 (sizeof(WORD))/*class*/ + nActualChars * sizeof(WCHAR);
/* 
......font information
*/ 
		nBufferSize += sizeof(WORD) + nfChars * sizeof(WCHAR); 
/*
......menu information, only for main frame
*/ 
		if (Main_first==1)
			nBufferSize += sizeof(MainMenu); 
		else
			nBufferSize += sizeof(WORD);
/*
......adjust size to make first control DWORD aligned
*/
		nBufferSize = (nBufferSize + 3) & ~3;  

		for (i = 0; i < TOTALITEMS; i++)
		{
			int nItemLength = sizeof(DLGITEMTEMPLATE) + 3 * sizeof(WORD);
			nItemLength += (m_rgDlgItem[i].m_strCaption.GetLength() + 1) * sizeof(WCHAR);

			if (i != TOTALITEMS -1 )
/*
...... take into account gap so next control is DWORD aligned
...... but the last control does not need extra bytes
*/
				nItemLength = (nItemLength + 3) & ~3;  

			nBufferSize += nItemLength;
		}


		HLOCAL hLocal = LocalAlloc(LHND, nBufferSize);
		if (hLocal == NULL)
			AfxThrowMemoryException();

		BYTE*	pBuffer = (BYTE*)LocalLock(hLocal);
		if (pBuffer == NULL)
		{
			LocalFree(hLocal);
			AfxThrowMemoryException();
		}

		BYTE*	pdest = pBuffer;
/*
......transfer DLGTEMPLATE structure to the buffer
*/
		memcpy(pdest, &m_dlgTempl, sizeof(DLGTEMPLATE));
		pdest += sizeof(DLGTEMPLATE);
		if (Main_first==1)
		{
			memcpy(pdest, MainMenu, sizeof(MainMenu));
			pdest += sizeof(MainMenu);
			Main_first = 0;
		}
		else
		{
			*(WORD*)pdest = 0; 
			pdest += sizeof(WORD);
		}
/*
.....use default window class
*/
		*(WORD*)pdest = 0;  
		pdest += sizeof(WORD);
		memcpy(pdest, szBoxCaption, nActualChars * sizeof(WCHAR));
		pdest += nActualChars * sizeof(WCHAR);
		delete szBoxCaption ;
		*(WORD*)pdest = 8;  // font size
		pdest += sizeof(WORD);
		memcpy(pdest, szFontName, nfChars * sizeof(WCHAR));
		pdest += nfChars * sizeof(WCHAR);
		delete szFontName ;
/* 
......We will now transfer the information for each one of the item templates
*/
		for (i = 0; i < TOTALITEMS; i++)
		{
			pdest = (BYTE*)(((DWORD)pdest + 3) & ~3);
			memcpy(pdest, (void *)&m_rgDlgItem[i].m_dlgItemTemplate, 
							sizeof(DLGITEMTEMPLATE));
			pdest += sizeof(DLGITEMTEMPLATE);
			*(WORD*)pdest = 0xFFFF;  // indicating atom value
			pdest += sizeof(WORD);
			*(WORD*)pdest = m_rgDlgItem[i].m_controltype;	 
			pdest += sizeof(WORD);

/*
......transfer the caption even when it is an empty string
*/
			WCHAR*	pchCaption;
			int		nChars, nActualChars;

			nChars = m_rgDlgItem[i].m_strCaption.GetLength() + 1;
			pchCaption = new WCHAR[nChars];
			nActualChars = MultiByteToWideChar(CP_ACP, 0,
					 m_rgDlgItem[i].m_strCaption, -1, pchCaption, nChars);
			ASSERT(nActualChars > 0);
			memcpy(pdest, pchCaption, nActualChars * sizeof(WCHAR));
			pdest += nActualChars * sizeof(WCHAR);
			delete pchCaption;

			*(WORD*)pdest = 0;  // How many bytes in data for control
			pdest += sizeof(WORD);
		}
		ASSERT(pdest - pBuffer == nBufferSize); 
/*Temptemp 1/10/01		if (m_style==1)
		{
			if (InitModalIndirect((DLGTEMPLATE*)pBuffer, m_pParent))
			{
				stat = DoModal();
				if (stat==-1)
				{
					if (MessageBox("DoModal Failed", "Error!", MB_OK)==IDOK)
					   exit(1);
				}
				else if (stat==IDABORT)
				{
					if (MessageBox("DoModal abort", "Error!", MB_OK)==IDOK)
					exit(1);
				}
			}
			else
				if (MessageBox("error init", "Error!", MB_OK)==IDOK)					
					exit(1);
		}
		else
*/			int stat = CDialog::CreateIndirect((DLGTEMPLATE*)pBuffer, m_pParent);

		LocalUnlock(hLocal);
		LocalFree(hLocal);
	}
	CATCH(CMemoryException, e)
	{
		MessageBox("Memory allocation for dialog template failed.",
			"Allocation Failure", MB_OK);
	}
	END_CATCH

	return TRUE;
}

/***********************************************************************
c
c   SUBROUTINE:  DlgCancel
c
c   FUNCTION:  This function simply call OnCancel because OnCancel
c				is protected function
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogButton::DlgCancel()
{
	OnCancel();
}

/***********************************************************************
c
c   SUBROUTINE:  OnCancel
c   FUNCTION:  This function called when "Cancel" button pushed
c				it destroy and remove child window and itself
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogButton::OnCancel() 
{
	int ans=0;
	if (m_pParent!=NULL)
		ans = ((CDialogButton*)m_pParent)->m_close_ans;
	CString msg = 
	"Some of the sub-menus are still active,\nDo you wish to save any changes?";
	for (int i=0; i<20; i++)
	{
//temp		if ((m_pModeless[i]!=NULL)||(m_dlgTemplate[i]!=NULL))
		if ((m_pModeless[i]!=NULL)||(sheet_disp[i]==1))		{
			if (ans==0)
				ans = MessageBox(msg, "Question?", MB_YESNOCANCEL);
			m_close_ans = ans;
	
			if (ans == IDYES)
			{
				switch(m_childtype[i])
				{
				case 1:
					((CDialogButton*)m_pModeless[i])->DlgCancel();
					m_pModeless[i] = NULL;
					break;
				case 2:
//					m_dlgTemplate[i].sheet->DlgOK();
					((CDialogPrompt*)m_pModeless[i])->DlgOK();
					m_pModeless[i] = NULL;
					break;
				case 3:
				case 4:
				case 5:
					((CDialogForm*)m_pModeless[i])->DlgOK();
					m_pModeless[i] = NULL;
					break;
				}
			}
			else if (ans == IDNO)
			{
				switch(m_childtype[i])
				{
				case 1:
					((CDialogButton*)m_pModeless[i])->DlgCancel();
					m_pModeless[i] = NULL;
					break;
				case 2:
					((CDialogPrompt*)m_pModeless[i])->DlgCancel();
					m_pModeless[i] = NULL;
					break;
				case 3:
				case 4:
				case 5:
					((CDialogForm*)m_pModeless[i])->DlgCancel();
					m_pModeless[i] = NULL;
					break;
				}
				
			}
		}
	}
	if (ans!=IDCANCEL)
	{
		if (m_pParent!=NULL)
			((CDialogButton*)m_pParent)->Remove_Child(m_ChildId);
		if (m_pTextDialog!=NULL)
		{
			((CDialogHelp*)m_pTextDialog)->DlgCancel();
			m_pTextDialog = NULL;
		}
/*
.....exit main template and file changed
*/
		if ((MpChanged)&&(m_style==1))
		{
			char buf[150];
			sprintf(buf,"%s\n%s\n","Changes have been made.",
				"Do you wish to save the MDF file?");
			ans = MessageBox(buf, "Question?", MB_YESNOCANCEL);
			if (ans == IDYES)
			{
				OnSaveMachine();
				ans = Slocal_ans;
			}
		}	
	}
	if (ans!=IDCANCEL)
	{
		pwddea("MAKEPOST");
		pwddea("");
		CDialog::OnCancel();
/*
.....modeless dialog must call DestroyWindow()
.....to destroy the window, otherwise, onlty make it invisible
.....but not destroyed.
*/
//Temptemp 1/10/01		if (m_style==0)
			DestroyWindow();
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnPushedBut(UINT id) 
c
c   FUNCTION:  This function called when button been pushed.
c
c   INPUT:  i:  button num
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogButton::OnPushedBut(UINT id)
{
	int buttonNum = id - ID_PUSH0;
	int i = id - ID_PUSH0;
	m_pLevel[m_curStage-1] = (this->m_rgDlgItem[i]).id;
	
/*
......First check whether the dialog already exists or whether
......we need to create a new one. We query the id of the dialog
......and compare it to the button activated
*/
	if (m_pModeless[i] != NULL)
	{
		m_pModeless[i]->SetActiveWindow();
		return;
	}
/*
......Query the application for the window properties
*/
	NpwDynWinStruct winStruct;
	NpwDynFormStruct formStruct;
	char message[80];

	int result = NpwGetWinLayout(m_pLevel,&m_curStage,
				&winStruct, &formStruct, message);
	if(result)
	{
		MessageBox(message, "Error!",MB_OK);
		return;
	}
	else if(winStruct.numButtons==0)
	{
		MessageBox("This sub-menu is currently not active\n", "Info!", MB_OK);
		return;
	}
		 
/*
......Create a new dialog. For the correct window type,query the
......window style and call the appropriate constructor
*/
	m_childtype[i] = winStruct.winType;
	switch(winStruct.winType)
	{
		case 1:
			m_pModeless[i] = new CDialogButton(this, 0, m_pLevel, m_curStage, i);
			((CDialogButton*)m_pModeless[i])->inittemp(&winStruct);
			((CDialogButton*)m_pModeless[i])->m_pLevel[m_curStage]=0;
			((CDialogButton*)m_pModeless[i])->m_curStage = m_curStage+1;
			((CDialogButton*)m_pModeless[i])->Create();
			break;
		case 2:
			m_pModeless[i] = new CDialogPrompt(this, m_pLevel, m_curStage, i);
			((CDialogPrompt*)m_pModeless[i])->init(&winStruct);
			((CDialogPrompt*)m_pModeless[i])->m_pLevel[m_curStage]=0;
			((CDialogPrompt*)m_pModeless[i])->m_curStage = m_curStage+1;
//			WS_VISIBLE
			((CDialogPrompt*)m_pModeless[i])->Create(IDD_PROMPTDIALOG, this);;
			((CDialogPrompt*)m_pModeless[i])->ShowWindow(TRUE);
			break;
		case 3:
		case 4:
		case 5:
			m_pModeless[i] = new CDialogForm(this, m_pLevel, m_curStage, i);
			((CDialogForm*)m_pModeless[i])->initform(&winStruct, &formStruct);
			((CDialogForm*)m_pModeless[i])->m_pLevel[m_curStage]=0;
			((CDialogForm*)m_pModeless[i])->m_curStage = m_curStage+1;
			((CDialogForm*)m_pModeless[i])->Create();
			break;
		default:
			MessageBox("Error!",
	"Error: Unknown window type requested.\nDialog generation aborted.", MB_OK);
	}		
}

/***********************************************************************
c
c   SUBROUTINE:  PostNcDestroy() 
c
c   FUNCTION:  This function called when Destroy window
c				it delete object pointer.
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogButton::PostNcDestroy() 
{
	CDialog::PostNcDestroy();
	if (m_style==0)
		delete this;
}

/***********************************************************************
c
c   SUBROUTINE:  Remove_Child(int id) 
c
c   FUNCTION:  This function remove the child dialog
c
c   INPUT:  id: child id
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogButton::Remove_Child(int id)
{
	m_pModeless[id] = NULL;
}

/***********************************************************************
c
c   SUBROUTINE:  Remove_Child(int id) 
c
c   FUNCTION:  This function remove the child PropertySheet
c
c   INPUT:  id: child id
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogButton::Remove_Temp(int id)
{
}

/***********************************************************************
c
c   SUBROUTINE:  Add_Child(CDialog* dlg, int id)
c
c   FUNCTION:  This function add the dialog in child dialog list.
c
c   INPUT:  id: child id
c			CDialog* dlg: Dialog pointer.
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogButton::Add_Child(CDialog* dlg, int id)
{
	m_pModeless[id] = dlg;
}

/***********************************************************************
c
c   SUBROUTINE:  OnViewDocumentFile
c
c   FUNCTION:  This function called when user select "View Document"
c				from Main Menu
c				it Open a window to display documenu file..
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogButton::OnViewDocumentFile()
{
	int nc;
	char tmpName[UX_MAX_PATH], buf[160],msg1[80],msg2[80];
	tmpnam(tmpName);
	nc = strlen(tmpName);
	if(strlen(tmpName)==0)
	{
		MessageBox("System Error: Unable to generate temporary file name", "Error!", MB_OK);
		return;		
	}
	if (tmpName[nc-1] != '.') strcat(tmpName,".");
	if (tmpName[0] == '\\') strcpy(tmpName,&tmpName[1]);
	int retVal = NpwCreateDocument(tmpName, msg1, msg2);
	if(retVal)
	{
		sprintf(buf,"%s\n%s",  msg1, msg2);
		MessageBox(buf, "Error!" , MB_OK);
		return;
	}
	if(m_pTextDialog != NULL)
	{
		m_pTextDialog->SetActiveWindow();
	}
	else
	{
		m_pTextDialog = new CDialogHelp(this, 3);
		((CDialogHelp*)m_pTextDialog)->settext(tmpName);
		((CDialogHelp*)m_pTextDialog)->inittemp();
		((CDialogHelp*)m_pTextDialog)->Create();
	}		
	unlink(tmpName);
}


/***********************************************************************
c
c   SUBROUTINE:  OnViewMachineSim
c
c   FUNCTION:  This function called when user select "View Machine"
c				from Main Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogButton::OnViewMachineSim()
{
	char buf[MAX_PATH+20], cmdparm[80];
	STARTUPINFO ms_stinfo;
	char machstr[256],lstr[82];
	int retVal;
	DWORD ret;
/*
......don't using execlp for WNT because it will replace current mpost process
......and never return
......create a new process t run MSLITE
			stat = execlp("MSLITE.exe","mslite.exe", "0",(char *)0);
*/
/*	int retVal = NpwViewMachine(msg1, msg2); */
	ms_stinfo.cb = NULL;
	ms_stinfo.cbReserved2 = NULL;
	ms_stinfo.dwFillAttribute = NULL;
	ms_stinfo.dwFlags = NULL;
	ms_stinfo.dwX = NULL;
	ms_stinfo.dwXCountChars = NULL;
	ms_stinfo.dwXSize = NULL;
	ms_stinfo.dwY = NULL;
	ms_stinfo.dwYCountChars = NULL;
	ms_stinfo.dwYSize = NULL;
	ms_stinfo.hStdError = NULL;
	ms_stinfo.hStdInput = NULL;
	ms_stinfo.hStdOutput = NULL;
	ms_stinfo.lpDesktop = NULL;
	ms_stinfo.lpReserved = NULL;
	ms_stinfo.lpReserved2 = 0;
	ms_stinfo.lpTitle = NULL;
	ms_stinfo.wShowWindow = NULL;
	DWORD mid = ::GetCurrentThreadId();

	char * com = getenv("MSEXE");
	char *path = getenv("PWORKS_MACH");

	getmach (lstr);
	if (path==NULL)
		sprintf(machstr, "%s", lstr);
	else
		sprintf(machstr, "%s\\%s", path, lstr);
	sprintf(cmdparm, "\"%s\" %s", com, machstr);
		
	retVal = CreateProcess(NULL, cmdparm, NULL, NULL, FALSE, 
		DETACHED_PROCESS | NORMAL_PRIORITY_CLASS, 
		NULL, NULL, &ms_stinfo, &ms_info);
	if (retVal==0)
	{
		sprintf(buf, "Error trying to run MS Lite");
		MessageBox(buf, "Error!" , MB_OK);
	}
	ret = WaitForInputIdle(ms_info.hProcess, INFINITE);
	if (ret!=0)
	{
		TerminateProcess(ms_info.hProcess, 0);
		CloseHandle(ms_info.hProcess);
		CloseHandle(ms_info.hThread);
	}
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  OnCreateDocument
c
c   FUNCTION:  This function called when user select "Create Document"
c				from Main Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogButton::OnCreateDocument()
{
	int stat;
	char FileName[UX_MAX_PATH];
	char msg1[80],msg2[80],buf[162];
	FileName[0] = '\0';
	stat = browsefile("All Files (*.*)\0*.*\0\0", FileName,FALSE,0,FALSE);
	if (stat==0)
		return;
	stat = NpwCreateDocument(FileName,msg1,msg2);
	if (stat!=0)
	{
		sprintf(buf,"%s\n%s",msg1,msg2);
		MessageBox(buf, "Error!" , MB_OK);
	}
}
/***********************************************************************
c
c   SUBROUTINE:  OnCreateMachineSim
c
c   FUNCTION:  This function called when user select "Create Machine"
c				from Main Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogButton::OnCreateMachineSim()
{
	int irtn,kask, ans, stat;
	char msg1[82],msg2[82],buf[162];

	kask = 0;
	irtn = NpwCreateMachine(&kask,msg1,msg2);
/*
.....File exists
.....Let's ask to overwrite it
*/
	if (irtn == 1)
	{
		sprintf(msg2,"File '%s' exists.\nDo you want to overwrite it.",msg1);
		ans = MessageBox(msg2, "Question?", MB_YESNO);
		if (ans == IDYES)
		{
			int kask = 1;
			stat = NpwCreateMachine(&kask,msg1,msg2);
/*
.....Error Creating File
*/
			if (stat != 0)
			{
				sprintf(buf,"%s\n%s",msg1,msg2);
				MessageBox(buf, "Error!" , MB_OK);
			}
		}
		irtn = 0;
	}
/*
.....Error Creating File
*/
	else if (irtn != 0)
	{
		sprintf(buf,"%s\n%s",msg1,msg2);
		MessageBox(buf, "Error!" , MB_OK);
	}
	return;
}
/***********************************************************************
c
c   SUBROUTINE:  OnLoadMachine()
c
c   FUNCTION:  This function called when user select "Load Machine"
c				from Main Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogButton::OnLoadMachine()
{
	int stat, ans = 0;
	char FileName[UX_MAX_PATH];
	char msg1[80],msg2[80],buf[162];

	if ((MpChanged)&&(m_style==1))
	{
		sprintf(buf,"%s\n%s\n","Changes have been made.",
				"Do you wish to save the MDF file?");
		ans = MessageBox(buf, "Question?", MB_YESNOCANCEL);
		if (ans == IDYES)
		{
			OnSaveMachine();
			ans = Slocal_ans;
		}
	}
	if (ans==IDCANCEL)
		return;

	FileName[0] = '\0';
	stat = browsefile("MDF Files (*.MDF)\0*.MDF\0\0", FileName,TRUE,0,TRUE);
	if (stat==0) return;	
	stat = NpwLoadMachine(FileName,msg1,msg2);
/*
.....Error Creating File
*/
	if (stat != 0)
	{
		sprintf(buf,"%s\n%s",msg1,msg2);
		MessageBox(buf, "Error!", MB_OK);
	}
	else
	{
		MpChanged = 0;
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnSaveMachineAs
c
c   FUNCTION:  This function called when user select "Save Machine As..."
c				from Main Menu
c
c   INPUT:  none
c   OUTPUT: none
c   RETURNS: none
c
c***********************************************************************
*/
void CDialogButton::OnSaveMachineAs()
{
	Slocal_saveas = TRUE;
	OnSaveMachine();
	Slocal_saveas = FALSE;
}

/***********************************************************************
c
c   SUBROUTINE:  OnSaveMachine
c
c   FUNCTION:  This function called when user select "Save Machine"
c				from Main Menu
c
c   INPUT:  none
c   OUTPUT: none
c   RETURNS: IDOK if file was saved successfully, IDCANCEL if the user
c            cancelled the save. (Value returned in Slocal_ans);
c
c***********************************************************************
*/
void CDialogButton::OnSaveMachine()
{
	int kask,irtn, stat, ans, loaded;
	char msg1[80],msg2[80];
	char name1[UX_MAX_PATH], name2[UX_MAX_PATH];
	char FileName[UX_MAX_PATH],sfile[UX_MAX_PATH],buf[UX_MAX_PATH];
	char *p;
/*
.....Get the directory to save the machine in
.....If a Machine was not originally loaded
*/
	Slocal_ans = IDOK;
	loaded = NpwWasLoaded(FileName);
	if (loaded == 0 || Slocal_saveas)
	{
browse:
		strcpy(sfile,FileName);
		stat = browsefile("MDF Files (*.MDF)\0*.MDF\0\0", FileName,FALSE,0,FALSE);
		if (stat==0)
		{
			Slocal_ans = IDCANCEL;
			return;
		}
/*
........Make sure user didn't change filename
*/
		p = strrchr(sfile,'\\');
		if (p != 0) 
			strcpy(name1,p+1); 
		else 
			strcpy(name1,sfile);
		p = strrchr(FileName,'\\');
		if (p != 0) 
			strcpy(name2,p+1); 
		else 
			strcpy(name2,sfile);
		if (strcmp(name1,name2) != 0)
		{
			sprintf(buf,"%s\n%s\n",
				"You cannot change the filename from the default value.",
				"Only the folder can be changed.  Hit OK to redisplay browser.");
			ans = MessageBox(buf,"Error!",MB_OKCANCEL);
			strcpy(FileName,sfile);
			if (ans == IDOK) goto browse;
			else return;
		}
		NpwSetDefault(FileName);
	}
/*
.....Save the MDF file
*/
	kask = 0;
	irtn = NpwSaveMachine(&kask,msg1,msg2);
	if (irtn == 0)
	{
		MpChanged = 0;
	}
/*
.....File exists
.....Let's ask to overwrite it
*/
	else if (irtn == 1)
	{
		if (loaded == 0)
			ans = IDYES;
		else
		{
			sprintf(buf,"File '%s' exists.\nDo you want to overwrite it.",msg1);
			ans = MessageBox(buf, "Question?", MB_YESNO);
		}
		if (ans == IDYES)
		{
			int kask = 1;
			stat = NpwSaveMachine(&kask,msg1,msg2);
/*
.....Error Creating File
*/
			if (stat != 0)
			{
				Slocal_ans = IDCANCEL;
				sprintf(buf,"%s\n%s",msg1,msg2);
				MessageBox(buf, "Error!", MB_OK);
			}
			else
			{
				MpChanged = 0;
			}
		}
/*
.....User chose not to override external file
*/
		else
			Slocal_ans = IDCANCEL;
		irtn = 0;
	}
/*
.....Error Creating File
*/
	else if (irtn != 0)
	{
		sprintf(buf,"%s\n%s",msg1,msg2);
		MessageBox(buf, "Error!", MB_OK);
	}
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  Remove_Text
c
c   FUNCTION:  This function remove the text window
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

void CDialogButton::Remove_Text()
{
	m_pTextDialog = NULL;
}

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
BOOL CDialogButton::OnInitDialog()
{
	CDialog::OnInitDialog();
 	SetIcon(m_hIcon, TRUE);
	DragAcceptFiles(TRUE);	
	return 0;
}

/***********************************************************************
c
c   SUBROUTINE:  OnHelpAbout()
c
c   FUNCTION:  This function called when user select "Help->About"
c				from Main Menu, it will display an About dialog
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogButton::OnHelpAbout()
{
	CAboutDlg aboutDlg;
	aboutDlg.DoModal();
}

/***********************************************************************
c
c   SUBROUTINE:  OnHelpContents()
c
c   FUNCTION:  This function called when user select "Help->Contents"
c				from Main Menu, it will spawn a command
c				"acroread %NCL_DOC%\postworks.pdf"
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CDialogButton::OnHelpContents()
{
	char com[256], buf[256];
	int stat;
	strcpy(com, "%acroread% %NCL_DOC%\\postworks.pdf");
//	sprintf(buf,"start \"Postworks\" /B cmd.exe /C %s", com); 
	sprintf(buf,"start \"Postworks\" /B %s", com); 
	stat = system(buf);
	if (stat > 0) 
	{
		sprintf(buf,"Can't execute \"%s\"", com); 
		MessageBox(buf, "Command Spawn Error",MB_OK);
	}
}
/***********************************************************************
**
**   FUNCTION: OnDropFiles(HDROP hDropInfo)
**
**		The framework calls this member function when the user releases 
**		the left mouse button over a Postworks window as the recipient of dropped files.
**   
**	 INPUT:  hDropInfo: A pointer to an internal data structure that describes 
**			the dropped files. 
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
void CDialogButton::OnDropFiles(HDROP hDropInfo)
{
//	CString FileName = "";
//	char buf[MAX_PATH+20];
    UINT i;
    UINT nFiles = ::DragQueryFile(hDropInfo, (UINT) -1, NULL, 0);
/*
.....only can accept one file
*/
/*
    for (i = 0; i < nFiles; i++)
    {
        char szFileName[_MAX_PATH];
        ::DragQueryFile(hDropInfo, i, szFileName, _MAX_PATH);
		FileName = FileName + "\"" + szFileName + "\" ";
    }
*/
	char FileName[_MAX_PATH];
	char msg1[80],msg2[80],buf[162];

	i = nFiles - 1;
    ::DragQueryFile(hDropInfo, i, FileName, _MAX_PATH);
    ::DragFinish(hDropInfo);

	int stat, ans = 0;
	if ((MpChanged)&&(m_style==1))
	{
		sprintf(buf,"%s\n%s\n","Changes have been made.",
				"Do you wish to save the MDF file?");
		ans = MessageBox(buf, "Question?", MB_YESNOCANCEL);
		if (ans == IDYES)
		{
			OnSaveMachine();
			ans = Slocal_ans;
		}
	}
	if (ans==IDCANCEL)
		return;
	stat = NpwLoadMachine(FileName,msg1,msg2);
/*
.....Error Creating File
*/
	if (stat != 0)
	{
		sprintf(buf,"%s\n%s",msg1,msg2);
		MessageBox(buf, "Error!", MB_OK);
	}
	else
	{
		MpChanged = 0;
	}
}

