/************************************************************************
c
c   FILE NAME: PtedChildWindow.cpp
c
c   CONTAINS:
c		CPtedChildWindow::CPtedChildWindow()
c		CPtedChildWindow::DlgQuit()
c		CPtedChildWindow::PostNcDestroy() 
c		CPtedChildWindow::OnFileOpen()
c		CPtedChildWindow::SetFtype(int type)
c		CPtedChildWindow::DlgClose()
c		
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PtedChildWindow.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:59:26
c
c**********************************************************************
*/
#include "pwenv.h"
#include "pwstdafx.h"
#include "PWBrowser.h"
#include "Pted.h"
#include "PtedChildWindow.h"
#include "PtedMainWindow.h"
#include "PtdGlobal.h"
#include "Ptedres.h"
#include "PtdFunc.h"
#include "PtedIncFileDialog.h"
#include "PtedFindReplaceDialog.h"
#include "PtedReseqDialog.h"
#include "PtedSetDialog.h"
#include <sys/stat.h>
#include <sys/types.h>

static int TOTALITEMS;
#define IDC_UNDO	3100
#define IDC_REPEAT	3101
#define IDC_REDO	3102

extern char PWINIT_FILE[UX_MAX_PATH];

static UINT WM_FILEHELP = ::RegisterWindowMessage(HELPMSGSTRING);
static UINT WM_FINDREPLACE = ::RegisterWindowMessage(FINDMSGSTRING);

/***********************************************************************
c
c   MESSAGE_MAP: callback descriptions
c
c***********************************************************************
*/

BEGIN_MESSAGE_MAP(CPtedChildWindow, CDialog)
	//{{AFX_MSG_MAP(PtedChildWindow)	
	ON_WM_CLOSE()
	ON_WM_SIZE()
	ON_WM_CTLCOLOR()
	ON_COMMAND(ID_FOPEN, OnFileOpen)
	ON_COMMAND(ID_FSAVE, OnSaveFile)
	ON_COMMAND(ID_FCANCEL, DlgQuit)
	
	ON_REGISTERED_MESSAGE( WM_FILEHELP, OnIncludeRange)
	ON_REGISTERED_MESSAGE( WM_FINDREPLACE, OnFindReplaceCmd)
	ON_EN_CHANGE (ID_ECOMMAND, OnCommandChanged)
	ON_COMMAND(ID_VIEW_STATUS, OnViewStatus)
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
	ON_COMMAND(ID_FILE_FPRINT, OnFilePrint)
	ON_COMMAND(ID_FILE_FPAGE_SETUP, OnFilePageSetup)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE: CPtedChildWindow(CWnd* pParent, int type)	
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c			int type:		window type.
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CPtedChildWindow::CPtedChildWindow(CWnd* pParent, char *file, 
								   int fopen, int type) : CPtedWindow(pParent, "CHILDFRAME_MENU", file, fopen, type)	
{
	m_ChildType = type;
	if (m_ChildType==1)
		strcpy(m_menuname, "CHILDFRAME_MENU"); 
	else if (m_ChildType==2)
		strcpy(m_menuname, "BACKUPFRAME_MENU"); 
	else
		strcpy(m_menuname, "TEXTFRAME_MENU"); 
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
void CPtedChildWindow::DlgQuit()
{
	DlgClose();
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
void CPtedChildWindow::PostNcDestroy() 
{
	delete m_pEditBkBrush;
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

void CPtedChildWindow::OnFileOpen()
{
	CPtedWindow::OnFileOpen();
	((CPtedMainWindow *)m_pParent)->Decre_untitle();
	char title[300];
	sprintf(title, "Child: %s", m_file);
	SetWindowText(title);
}



/***********************************************************************
c
c   SUBROUTINE:  SetFtype(int type)
c
c   FUNCTION:  This function set the file type 
c
c   INPUT:  type: file type to set
c
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedChildWindow::SetFtype(int type)
{ 
	if ((m_ChildType==2)||(m_ChildType==3))
		return;
	CPtedWindow::SetFtype(type);
	return;
}
/***********************************************************************
c
c   SUBROUTINE:  DlgClose
c
c   FUNCTION:  This function simply call OnCancel because OnCancel
c              is protected function
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
int CPtedChildWindow::DlgClose()
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
				char FileName[200];
				FileName[0] = '\0';
				stat = browsefile(
					"CL Text Files (*.cla)\0*.cla\0NC Data Files (*.pu*)\0*.pu*\0CL Binary Files (*.cl)\0*.cl\0APT Source Files (*.as)\0*.as\0Simulation Files (*.sim)\0*.sim\0Cutter Files (*.dat)\0*.dat\0All Files (*.*)\0*.*\0\0",
					FileName,0, m_ftype,FALSE);
				if (stat==0)
					return -1;
				stat = ProgramSaveAs(FileName);
			}
		}
		else if (ans == IDCANCEL)
			stat = -1;			
	}
	if (stat==0)
	{
		((CPtedMainWindow *)m_pParent)->Remove_Child(this);
		if (m_openflag==0)
			((CPtedMainWindow *)m_pParent)->Decre_untitle();
		DestroyWindow();
	}
	return stat;
}
void CPtedChildWindow::OnClose() 
{
	DlgClose();
}

	
