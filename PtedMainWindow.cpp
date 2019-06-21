/************************************************************************
c
c   FILE NAME: PtedMainWindow.cpp
c
c   CONTAINS:
c		CPtedMainWindow::CPtedMainWindow()
c		CPtedMainWindow::DlgQuit()
c		CPtedMainWindow::PostNcDestroy() 
c		CPtedMainWindow::OnFileOpen()
c		CPtedMainWindow::OnInitDialog()
c		CPtedMainWindow::OnCancel() 
c		CPtedMainWindow::Remove_Child(CDialog *dialog)
c		CPtedMainWindow::Remove_Child(int id)
c		CPtedMainWindow::Add_Child(CDialog* dlg)
c		CPtedMainWindow::SetCutterIndex(CDialog *dialog)
c		CPtedMainWindow::ResetCutterIndex(CDialog *dialog)
c		CPtedMainWindow::LoadCutterData()
c		CPtedMainWindow::OnFileNew()
c		CPtedMainWindow::Decre_untitle()
c		CPtedMainWindow::On_Window_Backup()
c		CPtedMainWindow::OnAppAbout()
c		CPtedMainWindow::OnEditEditinput() 
c		CPtedMainWindow::OnEditEditoutput() 	
c		CPtedMainWindow::OnFileLoadinputmdf() 
c		CPtedMainWindow::OnFileLoadoutputmdf() 
c		CPtedMainWindow::OnFileLoadmdf() 
c		CPtedMainWindow::OnFileClose() 
c		CPtedMainWindow::OnFileFileList(UINT id)
c		CPtedMainWindow::SetPreFile()
c		CPtedMainWindow::OnOptionPworksoption() 
c		CPtedMainWindow::OnConvertRunpworks() 	
c		CPtedMainWindow::OnFileRunmpost() 
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PtedMainWindow.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:26
c        
c**********************************************************************
*/
#include "pwenv.h"
#include "pwstdafx.h"
#include "PWBrowser.h"
#include "Pted.h"
#include "PtedMainWindow.h"
#include "PtedChildWindow.h"
#include "PtdGlobal.h"
#include "Ptedres.h"
#include "PtdFunc.h"
#include "PtedIncFileDialog.h"
#include "PtedFindReplaceDialog.h"
#include "PtedReseqDialog.h"
#include "PtedSetDialog.h"
#include "PtedPWOptDialog.h"
#include <sys/stat.h>
#include <sys/types.h>
#include <process.h> 
#include "PwNTAboutDlg.h"
#include "PtedStatusDlg.h"
#include "PtedTextBuffer.h"

static int TOTALITEMS;

#define IDC_OLDFILE1  3001
#define IDC_OLDFILE2  3002
#define IDC_OLDFILE3  3003
#define IDC_OLDFILE4  3004
#define IDC_OLDFILE5  3005

extern "C" void Ptd_releasekey();

extern char PWINIT_FILE[UX_MAX_PATH];

static UINT WM_FILEHELP = ::RegisterWindowMessage(HELPMSGSTRING);
static UINT WM_FINDREPLACE = ::RegisterWindowMessage(FINDMSGSTRING);

/***********************************************************************
c
c   MESSAGE_MAP: callback descriptions
c
c***********************************************************************
*/
int CPtedMainWindow::m_num = 0;

CPtedMainWindow *Pted_MainDlg = NULL;

BEGIN_MESSAGE_MAP(CPtedMainWindow, CDialog)
	//{{AFX_MSG_MAP(CPtedMainWindow)
	ON_WM_CLOSE()
	ON_REGISTERED_MESSAGE( WM_FILEHELP, OnIncludeRange)
	ON_REGISTERED_MESSAGE( WM_FINDREPLACE, OnFindReplaceCmd)
	ON_EN_CHANGE (ID_ECOMMAND, OnCommandChanged)
	ON_COMMAND(ID_VIEW_STATUS, OnViewStatus)
	ON_COMMAND(ID_FSAVE, OnSaveFile)
	ON_COMMAND(ID_FSAVEAS, OnSaveAsFile)
	ON_COMMAND(IDC_UNDO, On_Eundo)
	ON_COMMAND(IDC_REDO, On_Eredo)
	ON_COMMAND(ID_ECUT, On_Ecut)
	ON_COMMAND(ID_ECOPY, On_Ecopy)
	ON_COMMAND(ID_EPASTE, On_Epaste)
	ON_COMMAND(ID_EDELETE, On_Edelete)
	ON_COMMAND(ID_EDELETEL, On_EdeleteL)
	ON_COMMAND(ID_EINSERTL, On_EinsertL)
	ON_COMMAND(ID_FFIND, On_Ffind)
	ON_COMMAND(ID_FFINDNEXT, On_Ffindnext)
	ON_COMMAND(ID_FFINDPREV, On_Ffindprev)
	ON_COMMAND(ID_FFINDALL, On_Ffindall)
	ON_COMMAND(ID_VTOP, On_View_Top)
	ON_COMMAND(ID_VBOTTOM, On_View_Botm)
	ON_COMMAND(ID_FILE_INCLUDE, OnFileInclude)
	ON_COMMAND(ID_FILE_OPENCUTTERFILE, OnFileCutter)
	ON_COMMAND(ID_SEARCH_REPLACE, OnSearchReplace)
	ON_COMMAND(ID_CONVERT_CONVERT, OnConvertConvert)
	ON_COMMAND(ID_CONVERT_BADBLOCKS, OnConvertBadblocks)
	ON_COMMAND(ID_CONVERT_FORMAT, OnConvertFormat)
	ON_COMMAND(ID_CONVERT_UNFORMAT, OnConvertUnformat)
	ON_COMMAND(ID_CONVERT_CONVERT2, OnConvertConvert2)
	ON_COMMAND(ID_CONVERT_BADBLOCKS2, OnConvertBadblocks2)
	ON_COMMAND(ID_CONVERT_FORMAT2, OnConvertFormat2)
	ON_COMMAND(ID_CONVERT_UNFORMAT2, OnConvertUnformat2)
	ON_COMMAND(ID_CONVERT_LENGTH2, OnConvertLength2)
	ON_COMMAND(ID_EDIT_REVERSE, OnEditReverse)
	ON_COMMAND(ID_CONVERT_LENGTH, OnConvertLength)
	ON_COMMAND(ID_CONVERT_RESEQUENCE, OnConvertResequence)
	ON_COMMAND(ID_CONVERT_SETREGISTER, OnConvertSetregister)
	ON_COMMAND(ID_CUNDO_ENABLED, OnEditEnableCUndo)
	ON_COMMAND(ID_WINDOW_FILETYPE_APTSOURCE, OnWastyp)
	ON_COMMAND(ID_WINDOW_FILETYPE_CONTROLDATA, OnWputyp)
	ON_COMMAND(ID_WINDOW_FILETYPE_TEXTFILE, OnWclatyp)
	ON_COMMAND(ID_WCLTYP, OnWcltyp)
	ON_COMMAND(ID_WINDOW_FILETYPE_SIMULATE, OnWindowFiletypeSimulate)
	ON_COMMAND(ID_WINDOW_FILETYPE_CUTTERFILE, OnWindowFiletypeCutterfile)
	ON_COMMAND(ID_WINDOW_LOADCUTTERDATA, OnWindowLoadCutterData)
	ON_COMMAND(ID_WINDOW_MARK, OnWindowBookMark)
	ON_COMMAND(ID_WINDOW_SYNTAXCLR, OnWindowSyntaxClr)
	ON_COMMAND(ID_CONVERT_NCTOAPT, OnConvertNctoapt)
	ON_COMMAND(ID_CONVERT_APTTONC, OnConvertApttonc)
	ON_COMMAND(ID_CONVERT_NCTOSIM, OnConvertTosim)
	ON_COMMAND(ID_WINDOW_COMMANDLINEON, OnWindowCommandlineon)

	ON_WM_SIZE()
	ON_WM_CTLCOLOR()
	ON_COMMAND(ID_FNEW, OnFileNew)
	ON_COMMAND(ID_FOPEN, OnFileOpen)
	ON_COMMAND(ID_FCANCEL, DlgQuit)
	ON_COMMAND(ID_WBACKUP, On_Window_Backup)
	ON_COMMAND(ID_HELP_CONTENTS, OnHelpContents)
	ON_COMMAND(ID_HELP_ABOUT, OnHelpAbout)
	ON_COMMAND(ID_FILE_RUNMPOST, OnFileRunmpost)
	ON_COMMAND(ID_EDIT_EDITINPUT, OnEditEditinput)
	ON_COMMAND(ID_EDIT_EDITOUTPUT, OnEditEditoutput)
	ON_COMMAND(ID_FILE_LOADINPUTMDF, OnFileLoadinputmdf)
	ON_COMMAND(ID_FILE_LOADOUTPUTMDF, OnFileLoadoutputmdf)
	ON_COMMAND(ID_FILE_LOADMDF, OnFileLoadmdf)
	ON_COMMAND(IDP_FILE_CLOSE, OnFileClose)
	ON_COMMAND(ID_OPTION_PWORKSOPTION, OnOptionPworksoption)
	ON_COMMAND(ID_CONVERT_RUNPWORKS, OnConvertRunpworks)
	ON_COMMAND_RANGE(IDC_OLDFILE1, IDC_OLDFILE5, OnFileFileList)

	ON_COMMAND_RANGE(ID_EDIT_TOGGLE_BOOKMARK0, ID_EDIT_TOGGLE_BOOKMARK9, OnToggleBookmark)
	ON_COMMAND_RANGE(ID_EDIT_GO_BOOKMARK0, ID_EDIT_GO_BOOKMARK9, OnGoBookmark)
	ON_COMMAND(ID_EDIT_CLEAR_BOOKMARKS, OnClearBookmarks)
	ON_COMMAND(ID_EDIT_TOGGLE_BOOKMARK,     OnToggleBookmark)
	ON_COMMAND(ID_EDIT_GOTO_NEXT_BOOKMARK,  OnNextBookmark)
	ON_COMMAND(ID_EDIT_GOTO_PREV_BOOKMARK,  OnPrevBookmark)
	ON_COMMAND(ID_EDIT_CLEAR_ALL_BOOKMARKS, OnClearAllBookmarks)
	ON_COMMAND(ID_FILE_FPRINT, OnFilePrint)
	ON_COMMAND(ID_FILE_FPAGE_SETUP, OnFilePageSetup)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

typedef int (WINAPI* AFX_COMPARE_PROC)(LPCTSTR str1, LPCTSTR str2);

static int m_rlen[5];

void ptd_sets_opt(char *inibuf, PtedPWOptDialog *dialog)
{
	char *passstr, *tok, tmpstr[5000], tmpstr2[5000];
	
	strcpy(tmpstr, inibuf);
pass:;
	passstr = strchr (tmpstr, '-');
	if (passstr==NULL)
		return;
	strcpy(tmpstr, passstr);
	if (strncmp(tmpstr, "-ADJUST", 7)==0)
	{
		strcpy(tmpstr2, &(tmpstr[8]));
		strcpy(tmpstr, &(tmpstr[9]));
		tok = strtok(tmpstr2, " \n\r");
		if (tok==NULL)
			return;
		dialog->m_adjfile = tok;
		goto pass;
	} 
	else if (strncmp(tmpstr, "-WARNING", 8)==0)
	{
		strcpy(tmpstr2, &(tmpstr[9]));
		strcpy(tmpstr, &(tmpstr[10]));
		tok = strtok(tmpstr2, " \n\r");
		if (tok==NULL)
			return;
		dialog->m_cwarn = atoi(tok);
		goto pass;
	} 
	else if (strncmp(tmpstr, "-ERROR", 6)==0)
	{
		strcpy(tmpstr2, &(tmpstr[7]));
		strcpy(tmpstr, &(tmpstr[8]));
		tok = strtok(tmpstr2, " \n\r");
		if (tok==NULL)
			return;
		dialog->m_cerror = atoi(tok);
		goto pass;
	} 
	else if (strncmp(tmpstr, "-FATAL", 6)==0)
	{
		strcpy(tmpstr2, &(tmpstr[7]));
		strcpy(tmpstr, &(tmpstr[8]));
		tok = strtok(tmpstr2, " \n\r");
		if (tok==NULL)
			return;
		dialog->m_cfatal = atoi(tok);
		goto pass;
	} 
	else if (strncmp(tmpstr, "-PRINT", 6)==0)
	{
		strcpy(tmpstr2, &(tmpstr[7]));
		strcpy(tmpstr, &(tmpstr[8]));
		tok = strtok(tmpstr2, " \n\r");
		if (tok==NULL)
			return;
		dialog->m_cprint = tok;
		dialog->m_print = 1;
		goto pass;
	} 
	else if (strncmp(tmpstr, "-NOPRINT", 8)==0)
	{
		strcpy(tmpstr, &(tmpstr[8]));
		dialog->m_print = 0;
		goto pass;
	} 
	else if (strncmp(tmpstr, "-MACHINE", 8)==0)
	{
		strcpy(tmpstr2, &(tmpstr[9]));
		strcpy(tmpstr, &(tmpstr[10]));
		tok = strtok(tmpstr2, " \n\r");
		if (tok==NULL)
			return;
		dialog->m_machine = tok;
		goto pass;
	} 
	else if (strncmp(tmpstr, "-PAGE_LEN", 9)==0)
	{
		strcpy(tmpstr2, &(tmpstr[10]));
		strcpy(tmpstr, &(tmpstr[11]));
		tok = strtok(tmpstr2, " \n\r");
		if (tok==NULL)
			return;
		dialog->m_plen = atoi(tok);
		goto pass;
	} 
	else if (strncmp(tmpstr, "-OPTIONS", 8)==0)
	{
		strcpy(tmpstr2, &(tmpstr[9]));
		strcpy(tmpstr, &(tmpstr[10]));
		tok = strtok(tmpstr2, " \n\r");
		if (tok==NULL)
			return;
		dialog->m_option = tok;
		goto pass;
	} 
}
/***********************************************************************
c
c   SUBROUTINE: CPtedMainWindow()
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

CPtedMainWindow::CPtedMainWindow(char *filename, char flist[5][UX_MAX_PATH], 
								 int actcom): CPtedWindow(NULL, "MAINFRAME_MENU", filename, 0, 0)	
{
	m_CutterDialogIndex = -1;
	m_pModeless = NULL;
	if (filename==NULL)
	{
		strcpy(m_file, "Untitled0.pu1");
		m_untitled++;
		m_openflag = 0;
	}
	else
	{
		strcpy(m_file, filename);
		m_openflag = 1;
	}
	m_flist = 0;
	for (int i=0; i<5; i++)
	{
		if (flist[i][0]!='\0')
			strcpy(m_filelist[i], flist[i]);
		else
			m_filelist[i][0] = '\0';
	}
}
CPtedMainWindow::~CPtedMainWindow()
{
}
/***********************************************************************
c
c   SUBROUTINE:  DlgQuit
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
void CPtedMainWindow::DlgQuit()
{
	int stat = 0;
	int num = m_num;
	for (int i=num-1; i>=0; i--)
	{
		if (m_pModeless[i]!=NULL)
		{
			stat = ((CPtedChildWindow*)m_pModeless[i])->DlgClose();
			if (stat!=0) break;
		}
	}
	if (stat!=0)
	{
		return;
	}
/*
.....save the file list
*/
	CPtedApp* pApp = (CPtedApp*)AfxGetApp();
	char errMsg[300];
	stat = Ptd_save_flist(pApp->m_flist_name, m_filelist, errMsg);
	if (stat!=0)
		MessageBox(errMsg,"Error", MB_OK);
	OnCancel();
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

void CPtedMainWindow::PostNcDestroy() 
{
	delete m_pEditBkBrush;
	remove(PWINIT_FILE);
	delete this;
}
/***********************************************************************
c
c   SUBROUTINE:  OnFileOpen
c
c   FUNCTION:  This function called when user select "Open"
c              from Main Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

void CPtedMainWindow::OnFileOpen()
{
	CPtedWindow::OnFileOpen();
	char title[300];
	sprintf(title, "Main: %s", m_file);
	SetWindowText(title);
/*
.....set menu for previous 5 files
*/
	SetPreFile();
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

void CPtedMainWindow::OnCancel() 
{
	int stat = 0;
	int ans = 0;

	char msg[400];
	sprintf(msg, "Do you want to save changes made to %s?", m_file);
	if (m_TextView->IsModified())
	{
		ans = MessageBox(msg, "Question?", MB_YESNOCANCEL);
		if (ans == IDYES)
		{
			if (m_openflag==1)
				stat = ProgramSaveAs(m_file);
			else
			{
				char FileName[UX_MAX_PATH];
				FileName[0] = '\0';
				stat = browsefile("CL Text Files (*.cla)\0*.cla\0NC Data Files (*.pu*)\0*.pu*\0CL Binary Files (*.cl)\0*.cl\0APT Source Files (*.as)\0*.as\0Simulation Files (*.sim)\0*.sim\0Cutter Files (*.dat)\0*.dat\0All Files (*.*)\0*.*\0\0", FileName,0, m_ftype,FALSE);
				if (stat==0)
					return;
				stat = ProgramSaveAs(FileName);
			}
		}
		else if (ans == IDCANCEL)
			stat = -1;
	}
	if (stat==0)
	{
		Ptd_releasekey();
		DestroyWindow();
		PostQuitMessage (0);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  Remove_Child(CDialog *dialog) 
c
c   FUNCTION:  This function remove the child dialog
c
c   INPUT:  id: child id
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedMainWindow::Remove_Child(CDialog *dialog)
{
	int i;
	for (i = 0; i<m_num; i++)
	{
		if (m_pModeless[i]==dialog)
			break;
	}
	if (i<m_num)
	{
		Remove_Child(i);
		if (i+1 == m_CutterDialogIndex) m_CutterDialogIndex = -1;
	}
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

void CPtedMainWindow::Remove_Child(int id)
{
	CDialog **new_list = NULL;
	int i, j;
	m_pModeless[id] = NULL;

	if (m_num>1)
	{
		new_list = new CDialog*[m_num-1];
		for (i=0; i<id; i++)
			new_list[i] = m_pModeless[i];
		for (j=id, i=id+1; i<m_num; i++, j++)
		{
			new_list[j] = m_pModeless[i];
			((CPtedChildWindow *)new_list[j])->SetId(j);
		}
	}
	delete m_pModeless;
	m_pModeless = new_list;
	m_num--;
}
		
/***********************************************************************
c
c   SUBROUTINE:  Add_Child(CDialog* dlg)
c
c   FUNCTION:  This function add the dialog in child dialog list.
c
c   INPUT:  id: child id
c			CDialog* dlg: Dialog pointer.
c   OUTPUT: none
c
c***********************************************************************
*/

void CPtedMainWindow::Add_Child(CDialog* dlg)
{
	int i;
	static int offx = 20;
	static int offy = 20;

	CDialog **new_list = new CDialog*[m_num+1];
	for (i=0; i<m_num; i++)
	{
		new_list[i] = m_pModeless[i];
	}
	if (m_pModeless!=NULL)
		delete m_pModeless;

	new_list[m_num] = dlg;
	m_pModeless = new_list;

	m_num++;
/*
.....offset the child window position
*/
	CRect lpRect;
	GetWindowRect(lpRect);
	lpRect.OffsetRect(offx, offy);
	dlg->MoveWindow(lpRect);
	if (offx >= 200)
	{
		offx = 20;
		offy = 20;
	}
	else
	{
		offx += 20;
		offy += 20;
	}
}
/***********************************************************************
c
c   SUBROUTINE:  SetCutterIndex(CDialog *dialog) 
c
c   FUNCTION:  Set the cutter dialog index.
c
c   INPUT:  dialog  -  dialog to cutter dialog index.
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedMainWindow::SetCutterIndex(CDialog *dialog)
{
	int i;
	for (i = 0; i<m_num; i++)
	{
		if (m_pModeless[i]==dialog) break;
	}

	if (i<m_num) m_CutterDialogIndex = i+1;
}

/***********************************************************************
c
c   SUBROUTINE:  ResetCutterIndex(CDialog *dialog) 
c
c   FUNCTION:  Reset the index to the cutter window if it is
c              the window passed in
c
c   INPUT:  dialog  -  window to test against cutter window index.
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedMainWindow::ResetCutterIndex(CDialog *dialog)
{
	int i;
	if (m_CutterDialogIndex > 0)
	{
		for (i = 0; i<m_num; i++)
		{
			if (m_pModeless[i]==dialog) break;
		}
		if (i<m_num && i+1 == m_CutterDialogIndex) m_CutterDialogIndex = -1;
	}
}

/***********************************************************************
c
c   SUBROUTINE:  LoadCutterData() 
c
c   FUNCTION:  Load the cutter data from the current cutter window,
c              if any into the fortran cutter common.
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedMainWindow::LoadCutterData()
{
	CString sText;
	char errMsg[160];
	int kerr;

	if (m_CutterDialogIndex < 0) return;

	if (m_CutterDialogIndex == 0)
	{
		(m_TextView->Get_TextBuffer())->LoadCutterData(&kerr, errMsg);
	}
	else
	{
		CPtedChildWindow* dialog = (CPtedChildWindow*)m_pModeless[m_CutterDialogIndex-1];
		(dialog->m_TextView->Get_TextBuffer())->LoadCutterData(&kerr,errMsg);
	}
	if (kerr) Disp_Msg(errMsg, 1);
}
/***********************************************************************
c
c   SUBROUTINE:  OnFileNew
c
c   FUNCTION:  This function called when user select "New"
c              from Main Menu to open a new window.
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

void CPtedMainWindow::OnFileNew()
{
	char filename[UX_MAX_PATH];
	sprintf(filename, "Untitled%d.pu1", m_untitled);
	CPtedChildWindow* child = new CPtedChildWindow(this, filename);
	child->Create(IDD_WINDOWDIALOG);;
	child->ShowWindow(TRUE);
	Add_Child(child);
	m_untitled++;
}

/***********************************************************************
c
c   SUBROUTINE:  Decre_untitle()
c
c   FUNCTION:  Decrese the untitle document number 
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedMainWindow::Decre_untitle()
{
	m_untitled--;
}

/***********************************************************************
c
c   SUBROUTINE: On_Window_Backup 
c
c   FUNCTION:  display a back up window
c
c   INPUT:  None
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedMainWindow::On_Window_Backup()
{
	CPtedChildWindow* child = new CPtedChildWindow(this, m_file, 1, 2);
	child->Create(IDD_WINDOWDIALOG);;
	child->ShowWindow(TRUE);
	Add_Child(child);
}

/***********************************************************************
c
c   SUBROUTINE:  OnHelpAbout()
c
c   FUNCTION:  This function display about dialog
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedMainWindow::OnHelpAbout()
{
/*
.....use model dialog
*/
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
void CPtedMainWindow::OnHelpContents()
{
	char com[256], buf[256];
	int stat;
	strcpy(com, "%acroread% %NCL_DOC%\\postworks.pdf");
	sprintf(buf,"start \"Postworks\" /B %s", com); 
	stat = system(buf);
	if (stat > 0) 
	{
		sprintf(buf,"Can't execute \"%s\"", com); 
		MessageBox(buf, "Command Spawn Error",MB_OK);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnEditEditinput
c
c   FUNCTION:  This function called when user select "Edit Input"
c              from Edit Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedMainWindow::OnEditEditinput() 
{

	int result;
	char buf[256];
	result = Ptd_Edit_input();
	if (result==-1)
	{
		sprintf(buf, "Error trying to run Mpost");
		Disp_Msg(buf, 1);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnEditEditoutput
c
c   FUNCTION:  This function called when user select "Edit Output"
c              from Edit Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedMainWindow::OnEditEditoutput() 
{

	int result;
	char buf[256];
	result = Ptd_Edit_output();
	if (result==-1)
	{
		sprintf(buf, "Error trying to run Mpost");
		Disp_Msg(buf, 1);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnFileLoadinputmdf
c
c   FUNCTION:  This function called when user select "Load Input MDF"
c              from File Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedMainWindow::OnFileLoadinputmdf() 
{

	int stat;
	char FileName[UX_MAX_PATH];
	char msg1[80],msg2[80],buf[162];

	FileName[0] = '\0';
	stat = browsefile("MDF Files (*.MDF)\0*.MDF\0\0", FileName,TRUE,0,TRUE);
	if (stat==0) return;
	stat = Ptd_LoadInput(FileName,msg1,msg2);
/*
.....Error Loading File
*/
	if (stat != 0)
	{
		sprintf(buf,"%s\n%s",msg1,msg2);
		Disp_Msg(buf, 1);
		return;
	}
	strcpy(m_input_mdf, FileName);
}

/***********************************************************************
c
c   SUBROUTINE:  OnFileLoadoutputmdf
c
c   FUNCTION:  This function called when user select "Load Output MDF"
c              from File Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedMainWindow::OnFileLoadoutputmdf() 
{

	int stat;
	char FileName[UX_MAX_PATH];
	char msg1[80],msg2[80],buf[162];
	FileName[0] = '\0';
	stat = browsefile("MDF Files (*.MDF)\0*.MDF\0\0", FileName, TRUE,0,TRUE);
	if (stat==0) return;
	stat = Ptd_LoadOutput(FileName,msg1,msg2);
/*
.....Error Loading File
*/
	if (stat != 0)
	{
		sprintf(buf,"%s\n%s",msg1,msg2);
		Disp_Msg(buf, 1);
		return;
	}
	strcpy(m_output_mdf, FileName);
}

/***********************************************************************
c
c   SUBROUTINE:  OnFileLoadmdf
c
c   FUNCTION:  This function called when user select "Load MDF"
c              from File Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedMainWindow::OnFileLoadmdf() 
{

	int stat;
	char FileName[UX_MAX_PATH];
	char msg1[80],msg2[80],buf[162];
	FileName[0] = '\0';
	stat = browsefile("MDF Files (*.MDF)\0*.MDF\0\0", FileName, TRUE,0,TRUE);
	if (stat==0) return;
	stat = Ptd_LoadMDF(FileName,msg1,msg2);
/*
.....Error Loading File
*/
	if (stat != 0)
	{
		sprintf(buf,"%s\n%s",msg1,msg2);
		Disp_Msg(buf, 1);
		return;
	}
	strcpy(m_output_mdf, FileName);
	strcpy(m_input_mdf, FileName);
}

/***********************************************************************
c
c   SUBROUTINE:  OnFileClose
c
c   FUNCTION:  This function called when user select "Close"
c				from File Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedMainWindow::OnFileClose() 
{
	int stat, ans;

	char msg[400];
	stat = 0;
	sprintf(msg, "Do you want to save changes made to %s?", m_file);
	if (m_TextView->IsModified())
	{
		ans = MessageBox(msg, "Question?", MB_YESNOCANCEL);
		if (ans == IDYES)
		{
			if (m_openflag==1)
				stat = ProgramSaveAs(m_file);
			else
			{
				char FileName[UX_MAX_PATH];
				FileName[0] = '\0';
				stat = browsefile("CL Text Files (*.cla)\0*.cla\0NC Data Files (*.pu*)\0*.pu*\0CL Binary Files (*.cl)\0*.cl\0APT Source Files (*.as)\0*.as\0Simulation Files (*.sim)\0*.sim\0Cutter Files (*.dat)\0*.dat\0All Files (*.*)\0*.*\0\0", FileName,0, m_ftype, FALSE);
				if (stat==0)
					return;
				stat = ProgramSaveAs(FileName);
			}
		}
		else if (ans == IDCANCEL)
			stat = -1;
	}
	if (stat!=0)
		return;
	strcpy (m_file, "Untitle0.pu1");
	char title[300];
	sprintf(title, "Main: %s", m_file);
	SetWindowText(title);
	m_TextView->OnCloseReset();
	m_TextView->InitNew();
	m_TextView->AttachToBuffer(NULL);
	m_TextView->RedrawWindow();
	Reset_menu();
}
/***********************************************************************
c
c   SUBROUTINE:  OnFileFileList(UINT id)
c
c   FUNCTION:  This function load a file in the file list
c
c   INPUT:  list_num: index number in the filelist
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedMainWindow::OnFileFileList(UINT id)
{
	int list_num;
	int stat, ans;

	char msg[400];

	list_num = id - IDC_OLDFILE1;
	stat = 0;
	sprintf(msg, "Do you want to save changes made to %s?", m_file);
	if (m_TextView->IsModified())
	{
		ans = MessageBox(msg, "Question?", MB_YESNOCANCEL);
		if (ans == IDYES)
		{
			if (m_openflag==1)
				stat = ProgramSaveAs(m_file);
			else
			{
				char FileName[UX_MAX_PATH];
				FileName[0] = '\0';
				stat = browsefile("CL Text Files (*.cla)\0*.cla\0NC Data Files (*.pu*)\0*.pu*\0CL Binary Files (*.cl)\0*.cl\0APT Source Files (*.as)\0*.as\0Simulation Files (*.sim)\0*.sim\0Cutter Files (*.dat)\0*.dat\0All Files (*.*)\0*.*\0\0", FileName,0, m_ftype, FALSE);
				if (stat==0)
					return;
				stat = ProgramSaveAs(FileName);
			}
		}
		else if (ans == IDCANCEL)
			stat = -1;
	}
	if (stat!=0)
		return;
	LoadProgram(m_filelist[list_num]);
	strcpy (m_file, m_filelist[list_num]);
	char title[300];
	sprintf(title, "Main: %s", m_file);
	SetWindowText(title);
	m_openflag = 1;
/*
.....set menu for previous 5 files
*/
	SetPreFile();
}

/***********************************************************************
c
c   SUBROUTINE:  SetPreFile()
c
c   FUNCTION:  This function set previous file list menu
c
c   INPUT:  None
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedMainWindow::SetPreFile()
{
	int i;
	if (m_openflag)
	{
		for (i=0; i<5; i++)
		{
			if (strcmp(m_filelist[i], m_file)==0)
			{
				i++;
				break;
			}
		}
		for (int j=i-1; j>0; j--)
		{
			if (m_filelist[j-1][0]!='\0')
				strcpy(m_filelist[j], m_filelist[j-1]);
		}
		strcpy(m_filelist[0], m_file);
	}
	if (m_filelist[0][0]!='\0')
	{
		CMenu* pmenu = GetMenu();
		CMenu* fmenu = pmenu->GetSubMenu(0);
		while (m_flist!=0)
		{
			fmenu->DeleteMenu(12+m_flist, MF_BYPOSITION);
			m_flist--;
		}
		if (m_flist == 0)
		{
			fmenu->InsertMenu(13, MF_SEPARATOR|MF_BYPOSITION,
					0, " ");
			m_flist++;
			i = 0;
			char menulabel[300];
			while ((m_filelist[i][0]!='\0')&&(i<5))
			{
				sprintf(menulabel, "%d. %s", i+1, m_filelist[i]);
				fmenu->InsertMenu(14+i, MF_STRING|MF_BYPOSITION, 
						IDC_OLDFILE1+i, menulabel);
				m_flist++;
				i++;
			}
		}
		SetMenu(pmenu );
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnOptionPworksoption() 
c
c   FUNCTION:  This function is callback of menu item "Postworks Options"
c				it display an option dialog and accept options
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedMainWindow::OnOptionPworksoption() 
{
	PtedPWOptDialog *dialog = new PtedPWOptDialog(this, m_input_mdf);
/*
.....read option in the PWorks initial file for PTED
.....and fill in the option dialog
*/
	FILE *pfile;
	char inibuf[5000];
/*
.....open PWorks initial file for PTED (temp file store in
.....global variable PWINIT_FILE. Open for read
*/
	pfile = fopen(PWINIT_FILE , "r" );
	if (pfile!=NULL)
	{
		fgets(inibuf, 5000, pfile);
		ptd_sets_opt(inibuf, dialog);
		fclose(pfile);
	}
	if (dialog->DoModal()==IDCANCEL)
	{
		delete dialog;
		return;
	}
/*
.....save option in the PWorks initial file for PTED
*/
/*
.....open PWorks initial file for PTED (temp file store in 
.....global variable PWINIT_FILE. Open for write and destroy
.....old record if file exist
*/
	pfile = fopen(PWINIT_FILE , "w" );

	char *tempstr;
	int len;
	len = (dialog->m_adjfile).GetLength();
	if (len>0)
	{
		tempstr = (dialog->m_adjfile).GetBuffer(len);
		fprintf(pfile, "-ADJUST:%s ", tempstr);
	}

	if ((dialog->m_warn)&&(dialog->m_cwarn>0)&&(dialog->m_cwarn!=9999))
		fprintf(pfile, "-WARNING:%d ", dialog->m_cwarn);
	else if (dialog->m_warn==0)
		fprintf(pfile, "-WARNING:0 ");

	if ((dialog->m_error)&&(dialog->m_cerror>0)&&(dialog->m_cerror!=9999))
		fprintf(pfile, "-ERROR:%d ", dialog->m_cerror);
	else if (dialog->m_error==0)
		fprintf(pfile, "-ERROR:0 ");
	if ((dialog->m_fatal)&&(dialog->m_cfatal>0)&&(dialog->m_cfatal!=1))
		fprintf(pfile, "-FATAL:%d ", dialog->m_cfatal);
	else if (dialog->m_fatal==0)
		fprintf(pfile, "-FATAL:0 ");

	if (dialog->m_print)
	{
		len = (dialog->m_cprint).GetLength();
		if (len>0)
		{
			tempstr = (dialog->m_cprint).GetBuffer(len);
//			if (strcmp(tempstr,".pr1"))
				fprintf(pfile, "-PRINT:%s ", tempstr);
		}
	}
	else
	{
		fprintf(pfile, "-NOPRINT ");
	}

	if (dialog->m_machine!="")
	{
		char *tmpstr = (dialog->m_machine).GetBuffer(40);
			fprintf(pfile, "-MACHINE:%s ", tmpstr);
	}

	if (dialog->m_plen>=0)
		fprintf(pfile, "-PAGE_LEN:%d ", dialog->m_plen);

	len = (dialog->m_option).GetLength();
	if (len>0)
	{
		tempstr = (dialog->m_option).GetBuffer(len);
		fprintf(pfile, "-OPTIONS:%s ", tempstr);
	}
	fclose(pfile);
	delete dialog;
}

/***********************************************************************
c
c   SUBROUTINE:  OnConvertRunpworks()
c
c   FUNCTION:  This function is callback of menu item "Run Postworks"
c				removed
c
c   INPUT:  None
c
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedMainWindow::OnConvertRunpworks() 
{
/*
.....run pworks
.....first open temp init file to get default command options
*/
	FILE *pfile;	
	char *indx, tmpstr[1000], inibuf[1000], comlin[1000];
	int num;
	pfile = fopen(PWINIT_FILE , "r");
	if (pfile!=NULL)
	{
		fgets(inibuf, 1000, pfile);
	}
	strcpy(tmpstr, inibuf);
	indx = strstr(tmpstr, "-MACHINE:");
	if (indx!=NULL)
		sprintf(comlin, "Pworks %s", inibuf);
	else
	{
		num = 0;
		if (m_input_mdf[0]!='\0')
		{
			strcpy(tmpstr, m_input_mdf);
			indx = strstr(tmpstr, "PWORKS_");
			if (indx == NULL) indx = strstr(tmpstr, "pworks_");
			if (indx!=NULL)
			{
				indx += 6;
				strcpy(tmpstr, &(indx[1]));
				indx = strchr(tmpstr, '.');
				if (indx!=NULL)
					*indx = '\0';
			}
		}		
		sprintf(comlin, "Pworks -MACHINE:%s %s", tmpstr, inibuf);
	}
	int result, len = strlen(comlin);
	csystem(comlin, &len, &result);
	fclose(pfile);
	if (result==-1)
	{
		char buf[256];
		sprintf(buf, "Error trying to run PWorks");
		Disp_Msg(buf, 1);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnFileRunmpost
c
c   FUNCTION:  This function called when user select "Run Mpost"
c				from File Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedMainWindow::OnFileRunmpost() 
{

	int result;
	char buf[256];
//	result = _spawnlp( _P_WAIT, "Mpost", "Mpost",  NULL );
	int nc = 5;
	csystem("Mpost", &nc, &result);
	if (result==-1)
	{
		sprintf(buf, "Error trying to run Mpost");
		Disp_Msg(buf, 1);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  ViewStatus
c
c   FUNCTION:  This function dispaly a status window
c				
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedMainWindow::ViewStatus(CWnd *parent)
{
	Ptd_File_status *files;
	int filenum, i;
	CPtedChildWindow* dialog;
	char  input[256], output[256];

	filenum = m_num + 1;
	files = (Ptd_File_status*)malloc(filenum*sizeof(Ptd_File_status));

	files[0].wtype = 0;
	files[0].type = m_ftype;
	strcpy(files[0].filename, m_file);

	for (i=0; i<m_num; i++)
	{
		dialog = (CPtedChildWindow*)m_pModeless[i];
		files[i+1].wtype = 1;
		files[i+1].type = dialog->m_ftype;
		strcpy(files[i+1].filename, dialog->m_file);
	}
	strcpy(input, m_input_mdf);
	strcpy(output, m_output_mdf);	

	CPtedStatusDlg *dlg = new CPtedStatusDlg(parent, files, filenum, input, output);
	dlg->DoModal();
}
void CPtedMainWindow::OnClose() 
{
	DlgQuit();
}

int CPtedMainWindow::Get_subwindow_name(char *ofile, char *infile, char *fname, char * fext)
{
	int stat;

	char ftmp[UX_MAX_PATH], tmp[UX_MAX_PATH];
	int num = m_num;
	if ((num==0) || (m_pModeless==NULL))
	{
		strcpy(fname, infile);
		return 0;
	}
	if (strcmp(infile, m_file)==0)
	{
		sprintf(fname, "%s_%d%s", ofile, 1, fext);
		stat = Get_subwindow_name(ofile, fname, fname, fext);
		return stat;
	}

	for (int i=0; i<num; i++)
	{
		if (m_pModeless[i]!=NULL)
		{
			strcpy(tmp, ((CPtedChildWindow*)m_pModeless[i])->m_file);
			if (strcmp(tmp, infile)!=0)
				continue;
			else
			{
				for (int j=1; j<2000; j++)
				{
					sprintf(ftmp, "%s_%d%s", ofile, j, fext);
					if (strcmp(infile, ftmp)!=0)
						continue;

					sprintf(fname, "%s_%d%s", ofile, j+1, fext);
					stat = Get_subwindow_name(ofile, fname, fname, fext);
					return stat;
				}
			}
		}
	}
	strcpy(fname, infile);
	return -1;
}

int pted_get_subwindow_name(char *ofile, char *infile, char *fname, char *fext)
{
	int stat;
	CPtedApp *app = (CPtedApp*)AfxGetApp();
	CPtedMainWindow *MainDlg = (CPtedMainWindow*)(app->GetMainWnd());
	
	stat = MainDlg->Get_subwindow_name(ofile, infile, fname, fext);

	return stat;
}
