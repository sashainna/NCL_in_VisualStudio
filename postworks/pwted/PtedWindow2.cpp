/************************************************************************
c
c   FILE NAME: PtedWindow2.cpp
c
c   CONTAINS:
c		CPtedWindow::OnFileOpen()
c		CPtedWindow::OnSaveFile()
c		CPtedWindow::ProgramSaveAs( char *fileName)
c		CPtedWindow::OnSaveAsFile()
c		CPtedWindow::browsefile(char*Filter, char*FileName, int flag)
c		CPtedWindow::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor)
c		CPtedWindow::DlgQuit()
c		CPtedWindow::GetSelected_FindText(CString& strResult)
c		CPtedWindow::On_Ecut()
c		CPtedWindow::On_Eredo()
c		CPtedWindow::On_Eundo()
c		CPtedWindow::On_Ecopy()
c		CPtedWindow::On_Epaste()
c		CPtedWindow::On_Edelete()
c		CPtedWindow::On_EdeleteL()
c		CPtedWindow::On_EinsertL()
c		CPtedWindow::On_View_Top()
c		CPtedWindow::On_View_Botm()
c		CPtedWindow::ReverseRange(PtedRangeStruct cRange)
c		CPtedWindow::OnEditReverse()
c		CPtedWindow::Reset_menu() 
c		CPtedWindow::Update_undo_redomenu(int undo_stat, int redo_stat)
c		CPtedWindow::OnWindowLoadCutterData()
c		CPtedWindow::OnWclatyp() 
c		CPtedWindow::OnWcltyp() 
c		CPtedWindow::OnWastyp()
c		CPtedWindow::OnWputyp() 
c		CPtedWindow::OnWindowFiletypeSimulate() 
c		CPtedWindow::OnWindowFiletypeCutterfile()
c		CPtedWindow::SetFtype(int type)
c		CPtedWindow::OnWindowCommandlineon()
c		CPtedWindow::ConvertMathRange(char **adds, int num, PtedRangeStruct cRange, int mflag)
c		CPtedWindow::OnCommandChanged()
c		CPtedWindow::OnCommandPreKeys()
c
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c	MODULE NAME AND RELEASE LEVEL
c       %M% , %I%
c	DATE AND TIME OF LAST  MODIFICATION
c       %G% , %U%
c
c**********************************************************************
*/
#include "pwenv.h"
#include "pwstdafx.h"
#include "PWBrowser.h"
#include "Pted.h"
#include "PtedWindow.h"
#include "PtedChildWindow.h"
#include "PtedMainWindow.h"
#include "PtdGlobal.h"
#include "Ptedres.h"
#include "PtdFunc.h"
#include "PtedFileDialog.h"
#include <sys/stat.h>
#include <sys/types.h>
#include <direct.h>

static int TOTALITEMS;
extern "C" void Pted_disply_ProcessWindow(char *title, CPtedWindow* parent);
extern "C" void Pted_Display_as_percent(int num, CPtedWindow* parent);
extern "C" void Pted_Close_ProcessWindow(CPtedWindow *parent);
extern "C" char Pted_localdir[UX_MAX_PATH];

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

void CPtedWindow::OnFileOpen()
{
	int stat, ans;
	char filename[UX_MAX_PATH];

	char msg[400];
	sprintf(msg, "Do you want to save changes made to %s?", m_file);

	stat = 0;
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
				stat = browsefile("CL Text Files (*.cla)\0*.cla\0NC Data Files (*.pu*)\0*.pu*\0CL Binary Files (*.cl)\0*.cl\0APT Source Files (*.as)\0*.as\0Simulation Files (*.sim)\0*.sim\0Cutter Files (*.dat)\0*.dat\0All Files (*.*)\0*.*\0\0", FileName,0, m_ftype-1,FALSE);
				if (stat==0)
					return;
				stat = ProgramSaveAs(FileName);
			}
		}
		else if (ans == IDCANCEL)
			stat = -1;
	}
	if (stat!=0) return;

	DWORD dwFlags;
	CString CFileName;
	dwFlags = OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT;
	dwFlags |= OFN_ENABLETEMPLATE ;
	LPCTSTR filter = "CL Text Files (*.cla)|*.cla|NC Data Files (*.pu*)|*.pu*|CL Binary Files (*.cl)|*.cl|APT Source Files (*.as, *.cls)|*.as;*.cls|Simulation Files (*.sim)|*.sim|Cutter Files (*.dat)|*.dat|All Files (*.*)|*.*||";
	PtedFileDialog *fdlg = new PtedFileDialog(TRUE, "pu1", NULL, dwFlags,
			filter, this);
	(fdlg->m_ofn).lpTemplateName = MAKEINTRESOURCE (IDD_PTD_FILEDLG);
	(fdlg->m_ofn).nFilterIndex = m_ftype;

	fdlg->m_verify = 0;
	if (fdlg->DoModal()==IDCANCEL)
	{
		delete fdlg;
		goto done;
	}
	
	CFileName = fdlg->GetPathName();
	int i;
	for (i=0; i<CFileName.GetLength(); i++)
		filename[i] = CFileName.GetAt(i);
	filename[i] = '\0';
	LoadProgram(filename, fdlg->m_verify);
	delete fdlg;
	strcpy (m_file, filename);
	m_openflag = 1;
done:;
	_chdir(Pted_localdir);
}
 
/***********************************************************************
c
c   SUBROUTINE:  OnSaveFile
c
c   FUNCTION:  This function called when user select "Save"
c              from Main Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

void CPtedWindow::OnSaveFile()
{
	if (m_openflag==1)
	{
		ProgramSaveAs(m_file);
	}
	else
		OnSaveAsFile();
}

/***********************************************************************
c
c   SUBROUTINE:  ProgramSaveAs( char *fileName)
c
c   FUNCTION:  This function save the current edit text 
c              into a file fileName
c
c   INPUT:  fileName: save file
c   OUTPUT: none
c
c***********************************************************************
*/
int 
CPtedWindow::ProgramSaveAs( char *fileName)
{
	CString tmpstr;
	char *indx, tmp[UX_MAX_PATH];
	if (fileName[0]=='\0')
	{
		if (m_file[0]=='\0')
			return 0;
		else
		{
			strcpy(fileName, m_file);
			indx = strchr(fileName, '.');
			if (indx!=NULL)
				*indx = '\0';
			if (m_ftype==1)
				strcat(fileName, ".cla");
			else if (m_ftype==2)
				strcat(fileName, ".pu");
			else if (m_ftype==3)
				strcat(fileName, ".cl");
			else if (m_ftype==4)
				strcat(fileName, ".as");
			else if (m_ftype==5)
				strcat(fileName, ".sim");
			else if (m_ftype==6)
				strcat(fileName, ".dat");
		}
	}
	indx = strchr(fileName, '.');
	if (indx==NULL)
	{
		if (m_ftype==1)
			strcat(fileName, ".cla");
		else if (m_ftype==2)
			strcat(fileName, ".pu");
		else if (m_ftype==3)
			strcat(fileName, ".cl");
		else if (m_ftype==4)
			strcat(fileName, ".as");
		else if (m_ftype==5)
			strcat(fileName, ".sim");
		else if (m_ftype==6)
			strcat(fileName, ".dat");
	}
	else if (fileName[0]=='.')
	{
		strcpy(tmp, m_file);
		indx = strchr(tmp, '.');
		if (indx!=NULL)
			*indx = '\0';
		if (m_ftype==1)
			strcat(tmp, ".cla");
		else if (m_ftype==2)
			strcat(tmp, ".pu");
		else if (m_ftype==3)
			strcat(tmp, ".cl");
		else if (m_ftype==4)
			strcat(tmp, ".as");
		else if (m_ftype==5)
			strcat(tmp, ".sim");
		else if (m_ftype==6)
			strcat(fileName, ".dat");
		strcpy(fileName, tmp);
	}
	m_TextView->SaveToFile(fileName, m_ftype, (int *)this, 1);
	return 0;
}

/***********************************************************************
c
c   SUBROUTINE:  OnSaveAsFile
c
c   FUNCTION:  This function called when user select "Save As"
c              from Main Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/

void CPtedWindow::OnSaveAsFile()
{
	int stat;
	char FileName[UX_MAX_PATH];
	if (m_file[0]!='\0')
		strcpy(FileName, m_file);
	else
		FileName[0] = '\0';
	stat = browsefile("CL Text Files (*.cla)\0*.cla\0NC Data Files (*.pu*)\0*.pu*\0CL Binary Files (*.cl)\0*.cl\0APT Source Files (*.as, *.cls)\0*.as;*.cls\0Simulation Files (*.sim)\0*.sim\0Cutter Files (*.dat)\0*.dat\0All Files (*.*)\0*.*\0\0", FileName,0, m_ftype-1,FALSE);
	if (stat==0)
		return;
	ProgramSaveAs(FileName);
	strcpy (m_file, FileName);
	SetWindowText(m_file);
/*
.....after save as function, the file as current file,
.....so should reset m_openflag = 1
*/
	m_openflag = 1;
	m_convert = 0;
}

/***********************************************************************
c
c   SUBROUTINE:  OnCtlColor() 
c
c   FUNCTION:  This function called when a child control 
c				is about to be drawn. We use override this
c				method to change background color oof a control
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
HBRUSH CPtedWindow::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor)
{
	CWnd* pComWnd = (CWnd*)GetDlgItem(ID_ECOMMAND);
	CWnd* pMsgWnd = (CWnd*)GetDlgItem(ID_EMESSAGE);
	HWND cmdhnd = pComWnd->GetSafeHwnd();
	HWND msghnd = pMsgWnd->GetSafeHwnd();
	HWND whnd = pWnd->GetSafeHwnd();
	switch (nCtlColor)
	{
		case CTLCOLOR_MSGBOX:
		case CTLCOLOR_EDIT:
			if (cmdhnd==whnd)
			{
				pDC->SetTextColor(RGB(255, 20, 20));
				pDC->SetBkColor(RGB(255, 255, 255));
			}
			else
			{
				pDC->SetTextColor(RGB(20, 20, 20));
				pDC->SetBkColor(RGB(255, 255, 255));
			}
			return (HBRUSH)(m_pEditBkBrush->GetSafeHandle());
/*
......we set Message edit control readonly, so window treat it
......as static label.
*/
		case CTLCOLOR_STATIC:
			if (msghnd==whnd)
			{
				pDC->SetTextColor(RGB(20, 20, 255));
				pDC->SetBkColor(RGB(255, 255, 255));
				return (HBRUSH)(m_pEditBkBrush->GetSafeHandle());
			}
			else
				return CDialog::OnCtlColor(pDC, pWnd, nCtlColor);
		default:
			return CDialog::OnCtlColor(pDC, pWnd, nCtlColor);
	}
}



/***********************************************************************
c
c   SUBROUTINE:  DlgQuit
c
c   FUNCTION:  This function exit the window
c				implement by child class window
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::DlgQuit()
{
	DestroyWindow();
}
/***********************************************************************
c
c   SUBROUTINE:  GetSelected_FindText(CString& strResult)
c
c   FUNCTION:  This function get the select text from the edit window
c
c   INPUT:  strResult: 
c   OUTPUT: strResult: Selected text
c   RETURN: None
c
c***********************************************************************
*/
void CPtedWindow::GetSelected_FindText(CString& strResult)
{
	m_TextView->GetSelected_FindText(strResult);
}
/***********************************************************************
c
c   SUBROUTINE: On_Ecut() 
c
c   FUNCTION:  This function called when pick "Cut" from menu 
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::On_Ecut()
{
	m_TextView->Cut();

	Update_undo_redomenu(1,0);
	m_TextView->RedrawWindow();
/*
.....active paste menu
*/
	CMenu* fmenu = m_pmenu->GetSubMenu(1);
	if (m_undo_menu)
		fmenu->EnableMenuItem(5, MF_BYPOSITION|MF_ENABLED);
	else
		fmenu->EnableMenuItem(2, MF_BYPOSITION|MF_ENABLED);
	SetMenu(m_pmenu );
}

/***********************************************************************
c
c   SUBROUTINE:  On_Eredo()
c
c   FUNCTION:  This function redo the last redoable command 
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::On_Eredo()
{
	m_TextView->OnEditRedo();
}

/***********************************************************************
c
c   SUBROUTINE:  On_Eundo()
c
c   FUNCTION:  This function undo the last undoable command 
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::On_Eundo()
{
	m_TextView->OnEditUndo();
}


/***********************************************************************
c
c   SUBROUTINE:  On_Ecopy()
c
c   FUNCTION:  This function called when user pick "Copy" from menu 
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::On_Ecopy()
{
	m_TextView->Copy();
/*
.....active paste menu
*/
	CMenu* fmenu = m_pmenu->GetSubMenu(1);
	if (m_undo_menu)
		fmenu->EnableMenuItem(5, MF_BYPOSITION|MF_ENABLED);
	else
		fmenu->EnableMenuItem(2, MF_BYPOSITION|MF_ENABLED);
	SetMenu(m_pmenu );
	m_TextView->RedrawWindow();
}


/***********************************************************************
c
c   SUBROUTINE:  On_Epaste
c
c   FUNCTION:  This function called when user pick "Paste" from menu 
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::On_Epaste()
{
	m_TextView->Paste();
	Update_undo_redomenu(1,0);
	m_TextView->RedrawWindow();
}

/***********************************************************************
c
c   SUBROUTINE: On_Edelete  
c
c   FUNCTION:  This function called when user pick "Delete" from menu  
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::On_Edelete()
{
	m_TextView->OnEditDeleteBack();
}


/***********************************************************************
c
c   SUBROUTINE:  On_EdeleteL
c
c   FUNCTION:  This function called when user pick "Delete Line" from menu 
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::On_EdeleteL()
{
	m_TextView->OnEditDeleteLine();
}


/***********************************************************************
c
c   SUBROUTINE: On_EinsertL  
c
c   FUNCTION:  This function called when user pick "Insert Line" from menu 
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::On_EinsertL()
{
	m_TextView->OnEditInsertLine() ;
}

/***********************************************************************
c
c   SUBROUTINE:  On_View_Top 
c
c   FUNCTION:  This function called when user pick "Top of file" from menu 
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::On_View_Top()
{
	m_TextView->ScrollToLine(0);
	m_TextView->RedrawWindow();
	m_TextView->SetCursorPos(CPoint(0,0));
	m_TextView->EnsureVisible(CPoint(0,0));
}


/***********************************************************************
c
c   SUBROUTINE: On_View_Botm  
c
c   FUNCTION:  This function called when user pick "Bottom of file" from menu  
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::On_View_Botm()
{
	CRect rect;
	m_TextView->GetClientRect(&rect);
	int lines = rect.Height() / m_TextView->GetLineHeight();

	lines = m_TextView->GetLineCount() - lines;
	m_TextView->ScrollToLine(lines);
	lines = m_TextView->GetLineCount()-1;
	m_TextView->SetCursorPos(CPoint(0,lines));
	m_TextView->EnsureVisible(CPoint(0,lines));
	m_TextView->RedrawWindow();
}
/***********************************************************************
c
c   SUBROUTINE:  OnEditReverse
c
c   FUNCTION:  This function called when user select "Reverse"
c				from Edit Menu
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnEditReverse() 
{
	m_TextView->OnEditReverse();
	Update_undo_redomenu(1,0);
}
/***********************************************************************
c
c   SUBROUTINE:  ReverseRange(PtedRangeStruct cRange)
c
c   FUNCTION:  This function Reverse all text in current window
c				in specified range
c
c   INPUT:  cRange: range structure
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::ReverseRange(PtedRangeStruct cRange)
{
	CPoint ptStart, ptEnd;
	(m_TextView->Get_TextBuffer())->Get_Range_Pos(&cRange, ptStart, ptEnd);
	if (ptStart!=ptEnd)
	{
		if (m_processor)
		{
			return;
		}
		else
		{
			Pted_disply_ProcessWindow("Pted Reverse...", this);
			Pted_Display_as_percent(1, this);
		}
		(m_TextView->Get_TextBuffer())->ReverseRange(ptStart, ptEnd);
		Pted_Close_ProcessWindow(this);
		m_TextView->RedrawWindow();
		Update_undo_redomenu(1,0);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  Reset_menu()
c
c   FUNCTION:  This function Reset the original menu
c
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::Reset_menu()
{
	if ((m_wtype==0) || (m_wtype==1) || (m_wtype==2))
	{
		CMenu* emenu = m_pmenu->GetSubMenu(1);
		if (m_undo_menu)
		{
			emenu->DeleteMenu(0, MF_BYPOSITION);
			emenu->DeleteMenu(0, MF_BYPOSITION);
			emenu->DeleteMenu(0, MF_BYPOSITION);
			m_undo_menu = 0;
		}
		emenu->EnableMenuItem(2, MF_BYPOSITION|MF_GRAYED);
		SetMenu(m_pmenu );
	}
}

/***********************************************************************
c
c   SUBROUTINE:  Update_undo_redomenu(int undo_stat, int redo_stat)
c
c   FUNCTION:  This function Update the undo/redo menu label
c
c
c   INPUT:  undo_stat: 1: enable undo menu item
c			redo_stat: 1: enable redo menu item
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::Update_undo_redomenu(int undo_stat, int redo_stat)
{
	CString undo_desc, redo_desc, desc2;
	int UndoPos, RedoPos;

	UndoPos = (int) m_TextView->GetUndoDescription(desc2);
	if (UndoPos==-1)
		undo_desc = "Can't undo";
	else
		undo_desc = "Undo " + desc2;

	RedoPos = (int)m_TextView->GetRedoDescription(desc2);
	if (RedoPos==-1)
		redo_desc = "Can't redo";
	else
	redo_desc = "Redo " + desc2;

	CMenu* fmenu = m_pmenu->GetSubMenu(1);
	if (m_undo_menu)
	{
		fmenu->DeleteMenu(0, MF_BYPOSITION);
		fmenu->DeleteMenu(0, MF_BYPOSITION);
		fmenu->DeleteMenu(0, MF_BYPOSITION);
	}
	m_undo_menu = 1;		
	fmenu->InsertMenu(0, MF_STRING|MF_BYPOSITION,
					IDC_UNDO, undo_desc);
	fmenu->InsertMenu(1, MF_STRING|MF_BYPOSITION,
					IDC_REDO, redo_desc);
	fmenu->InsertMenu(2, MF_SEPARATOR|MF_BYPOSITION,
					0, " ");
	if ((undo_stat) && (UndoPos!=-1))
		fmenu->EnableMenuItem(0, MF_BYPOSITION|MF_ENABLED);
	else
		fmenu->EnableMenuItem(0, MF_BYPOSITION|MF_GRAYED);
	if ((redo_stat) && (RedoPos!=-1))
		fmenu->EnableMenuItem(1, MF_BYPOSITION|MF_ENABLED);
	else
		fmenu->EnableMenuItem(1, MF_BYPOSITION|MF_GRAYED);

	SetMenu(m_pmenu );
}

/***********************************************************************
c
c   SUBROUTINE:  OnWindowLoadCutterData
c
c   FUNCTION:  This function is called when the user selects
c              Load Cutter Data from the Window menu.
c
c   INPUT:  none
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnWindowLoadCutterData()
{
	char errMsg[160];
	int kerr;
	CString sText;

	CWaitCursor wait;
	CPoint ptSelStart, ptSelEnd;
	ptSelStart.x = ptSelStart.y = 0;
	ptSelEnd.x = ptSelEnd.y = -1;
	m_TextView->GetText(ptSelStart, ptSelEnd, sText);
	Ptd_docutter(sText.GetBuffer(1), &kerr, errMsg);
	sText.ReleaseBuffer();
	if(kerr)
	{
		Disp_Msg(errMsg, 1);
	}
	else
	{
		if (m_pParent==NULL)
			m_CutterDialogIndex = 0;
		else
			((CPtedMainWindow *)m_pParent)->SetCutterIndex(this);
	}
	return;
}
/***********************************************************************
c
c   SUBROUTINE:  OnWclatyp
c
c   FUNCTION:  This function is callback of menu item "Control Data"
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedWindow::OnWclatyp() 
{
	m_ftype = 1;
	
	CMenu* pmenu = GetMenu();
	CMenu* wmenu = pmenu->GetSubMenu(4);
	CMenu* cmenu = pmenu->GetSubMenu(5);
	wmenu->CheckMenuItem(ID_WCLTYP, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_CONTROLDATA, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_APTSOURCE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_TEXTFILE, MF_CHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_SIMULATE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_CUTTERFILE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_APTTONC, MF_ENABLED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_NCTOAPT, MF_GRAYED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_NCTOSIM, MF_GRAYED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_WINDOW_LOADCUTTERDATA, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_RESEQUENCE, MF_ENABLED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_FORMAT, MF_ENABLED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_FORMAT2, MF_ENABLED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_UNFORMAT, MF_ENABLED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_UNFORMAT2, MF_ENABLED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_BADBLOCKS, MF_ENABLED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_BADBLOCKS2, MF_ENABLED | MF_BYCOMMAND );
	if (m_pParent==NULL)
	{
		if (m_CutterDialogIndex == 0) m_CutterDialogIndex = -1;
	}
	else
		((CPtedMainWindow *)m_pParent)->ResetCutterIndex(this);
	m_TextView->RedrawWindow();
}
/***********************************************************************
c
c   SUBROUTINE:  OnWcltyp
c
c   FUNCTION:  This function is callback of menu item "CL File"
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedWindow::OnWcltyp() 
{
	m_ftype = 3;
	CMenu* pmenu = GetMenu();
	CMenu* wmenu = pmenu->GetSubMenu(4);
	CMenu* cmenu = pmenu->GetSubMenu(5);
	wmenu->CheckMenuItem(ID_WCLTYP, MF_CHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_CONTROLDATA, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_APTSOURCE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_TEXTFILE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_SIMULATE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_CUTTERFILE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_APTTONC, MF_ENABLED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_NCTOAPT, MF_GRAYED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_NCTOSIM, MF_ENABLED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_WINDOW_LOADCUTTERDATA, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_RESEQUENCE, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_FORMAT, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_FORMAT2, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_UNFORMAT, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_UNFORMAT2, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_BADBLOCKS, MF_ENABLED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_BADBLOCKS2, MF_ENABLED | MF_BYCOMMAND );
	if (m_pParent==NULL)
	{
		if (m_CutterDialogIndex == 0) m_CutterDialogIndex = -1;
	}
	else
		((CPtedMainWindow *)m_pParent)->ResetCutterIndex(this);
	m_TextView->RedrawWindow();
}
/***********************************************************************
c
c   SUBROUTINE:  OnWastyp
c
c   FUNCTION:  This function is callback of menu item "APT Source File"
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedWindow::OnWastyp() 
{
	m_ftype = 4;
	
	CMenu* pmenu = GetMenu();
	CMenu* wmenu = pmenu->GetSubMenu(4);
	CMenu* cmenu = pmenu->GetSubMenu(5);
	wmenu->CheckMenuItem(ID_WCLTYP, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_CONTROLDATA, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_APTSOURCE, MF_CHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_TEXTFILE, MF_UNCHECKED | MF_BYCOMMAND );	
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_SIMULATE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_CUTTERFILE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_APTTONC, MF_ENABLED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_NCTOAPT, MF_GRAYED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_NCTOSIM, MF_ENABLED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_WINDOW_LOADCUTTERDATA, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_RESEQUENCE, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_FORMAT, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_FORMAT2, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_UNFORMAT, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_UNFORMAT2, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_BADBLOCKS, MF_ENABLED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_BADBLOCKS2, MF_ENABLED | MF_BYCOMMAND );
	if (m_pParent==NULL)
	{
		if (m_CutterDialogIndex == 0) m_CutterDialogIndex = -1;
	}
	else
		((CPtedMainWindow *)m_pParent)->ResetCutterIndex(this);
	m_TextView->RedrawWindow();
}
/***********************************************************************
c
c   SUBROUTINE:  OnWputyp
c
c   FUNCTION:  This function is callback of menu item "Control Data File"
c
c   INPUT:  None
c
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedWindow::OnWputyp() 
{
	m_ftype = 2;
	
	CMenu* pmenu = GetMenu();
	CMenu* wmenu = pmenu->GetSubMenu(4);
	CMenu* cmenu = pmenu->GetSubMenu(5);
	wmenu->CheckMenuItem(ID_WCLTYP, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_CONTROLDATA, MF_CHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_APTSOURCE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_TEXTFILE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_SIMULATE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_CUTTERFILE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_APTTONC, MF_GRAYED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_NCTOAPT, MF_ENABLED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_NCTOSIM, MF_ENABLED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_WINDOW_LOADCUTTERDATA, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_RESEQUENCE, MF_ENABLED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_FORMAT, MF_ENABLED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_FORMAT2, MF_ENABLED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_UNFORMAT, MF_ENABLED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_UNFORMAT2, MF_ENABLED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_BADBLOCKS, MF_ENABLED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_BADBLOCKS2, MF_ENABLED | MF_BYCOMMAND );
	if (m_pParent==NULL)
	{
		if (m_CutterDialogIndex == 0) m_CutterDialogIndex = -1;
	}
	else
		((CPtedMainWindow *)m_pParent)->ResetCutterIndex(this);
	m_TextView->RedrawWindow();
}

/***********************************************************************
c
c   SUBROUTINE:  OnWindowFiletypeSimulate() 
c
c   FUNCTION:  This function is callback of menu item "Simulate File"
c
c   INPUT:  None
c
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedWindow::OnWindowFiletypeSimulate() 
{
	m_ftype = 5;
	
	CMenu* pmenu = GetMenu();
	CMenu* wmenu = pmenu->GetSubMenu(4);
	CMenu* cmenu = pmenu->GetSubMenu(5);
	wmenu->CheckMenuItem(ID_WCLTYP, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_CONTROLDATA, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_APTSOURCE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_TEXTFILE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_SIMULATE, MF_CHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_CUTTERFILE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_APTTONC, MF_GRAYED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_NCTOAPT, MF_GRAYED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_NCTOSIM, MF_GRAYED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_WINDOW_LOADCUTTERDATA, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_RESEQUENCE, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_FORMAT, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_FORMAT2, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_UNFORMAT, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_UNFORMAT2, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_BADBLOCKS, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_BADBLOCKS2, MF_GRAYED | MF_BYCOMMAND );
	if (m_pParent==NULL)
		m_CutterDialogIndex = -1;
	else
		((CPtedMainWindow *)m_pParent)->ResetCutterIndex(this);
	m_TextView->RedrawWindow();
}
/***********************************************************************
c
c   SUBROUTINE:  OnWindowFiletypeCutterfile() 
c
c   FUNCTION:  This function is callback of menu item "Cutter File"
c
c   INPUT:  None
c
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedWindow::OnWindowFiletypeCutterfile() 
{
	m_ftype = 6;

	CMenu* pmenu = GetMenu();
	CMenu* wmenu = pmenu->GetSubMenu(4);
	CMenu* cmenu = pmenu->GetSubMenu(5);
	wmenu->CheckMenuItem(ID_WCLTYP, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_CONTROLDATA, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_APTSOURCE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_TEXTFILE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_SIMULATE, MF_UNCHECKED | MF_BYCOMMAND );
	wmenu->CheckMenuItem(ID_WINDOW_FILETYPE_CUTTERFILE, MF_CHECKED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_APTTONC, MF_GRAYED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_NCTOAPT, MF_GRAYED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_CONVERT_NCTOSIM, MF_GRAYED | MF_BYCOMMAND );
	wmenu->EnableMenuItem(ID_WINDOW_LOADCUTTERDATA, MF_ENABLED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_RESEQUENCE, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_FORMAT, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_FORMAT2, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_UNFORMAT, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_UNFORMAT2, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_BADBLOCKS, MF_GRAYED | MF_BYCOMMAND );
	cmenu->EnableMenuItem(ID_CONVERT_BADBLOCKS2, MF_GRAYED | MF_BYCOMMAND );
	if (m_pParent==NULL)
		m_CutterDialogIndex = -1;
	else
		((CPtedMainWindow *)m_pParent)->SetCutterIndex(this);
	m_TextView->RedrawWindow();
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
void CPtedWindow::SetFtype(int type)
{ 
	switch (type)
	{
	case 1:
		OnWclatyp();
		break;
	case 2:
		OnWputyp();
		break;
	case 3:
		OnWcltyp();
		break;
	case 4:
		OnWastyp();
		break;
	case 5:
		OnWindowFiletypeSimulate();
		break;
	case 6:
		OnWindowFiletypeCutterfile();
		break;
	default:
		OnWclatyp();
		break;
	}
	return;
}

/***********************************************************************
c
c   SUBROUTINE:  OnWindowCommandlineon()
c
c   FUNCTION:  This function is callback of menu item "Command on"
c
c   INPUT:  None
c
c   OUTPUT: None
c
c***********************************************************************
*/
void CPtedWindow::OnWindowCommandlineon() 
{
	CMenu* pmenu = GetMenu();
	CMenu* wmenu = pmenu->GetSubMenu(4);
	if (m_actcom==1)
	{
		wmenu->CheckMenuItem(ID_WINDOW_COMMANDLINEON, MF_UNCHECKED | MF_BYCOMMAND );
		m_actcom = 0;
	}
	else
	{
		wmenu->CheckMenuItem(ID_WINDOW_COMMANDLINEON, MF_CHECKED | MF_BYCOMMAND );
		m_actcom = 1;
	}

	CRect windowRect, windowRect1;
	GetClientRect(windowRect);
	int hgt, wid;
	hgt = windowRect.Height();
	wid = windowRect.Width();

	OnSize(SIZE_RESTORED, wid, hgt );
/*
.....put the cursor on the command window
*/
	((CEdit*)GetDlgItem(ID_ECOMMAND))->SetFocus();
	((CEdit*)GetDlgItem(ID_ECOMMAND))->SetSel(m_comcur, m_comcur, TRUE);
}
/***********************************************************************
c
c   SUBROUTINE:  ConvertMathRange(char **adds, int num, PtedRangeStruct cRange, 
c					int mflag)
c				
c   FUNCTION:  Do math convert function in specified range
c
c   INPUT:  adds: array of input
c			range:  range struction
c			num: number of input
c			mflag: which math funtion.
c				1:	ADD
c				2:	Mirror
c				3:	Multiply
c				4:	Rotate
c				5:	Scale
c				6:	Translate
c
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
void CPtedWindow::ConvertMathRange(char **adds, int num, PtedRangeStruct cRange, int mflag)
{
	CPoint ptStart, ptEnd;
	m_TextView->Get_Range_Pos(&cRange, ptStart, ptEnd);
	if (ptStart!=ptEnd)
	{
		if (m_processor)
		{
			return;
		}
		else
		{
			Pted_disply_ProcessWindow("Pted ConvertMathRange", this);
			Pted_Display_as_percent(1, this);
		}
		m_TextView->ConvertMathRange(adds, num, ptStart, ptEnd, mflag);
		Pted_Close_ProcessWindow(this);
		Update_undo_redomenu(1,0);
	}
}
/***********************************************************************
c
c   SUBROUTINE:  OnCommandChanged
c
c   FUNCTION:  This function called when user changed text
c				
c
c   INPUT:  None
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnCommandChanged()
{
	int pos, line, len, len2;
	CEdit *cwin = (CEdit*)GetDlgItem(ID_ECOMMAND);

/*
.....if it is undo call this function, simple return
.....otherwise, it will goes to a loop
*/
	static int ud = 0;
	if (ud)
		return;
	cwin->GetSel(pos, pos);
	if (pos<m_comcur)
	{
		ud = 1;
		cwin->Undo();
		ud = 0;
		cwin->SetSel(m_comcur,m_comcur );
		cwin->EmptyUndoBuffer();
		return;
	}

	if (pos==0)
	{
		cwin->EmptyUndoBuffer();
		return;
	}
	if (pos < m_comcursor)
	{
		cwin->EmptyUndoBuffer();
		return;
	}
	CString sText;
	cwin->GetWindowText(sText);
	m_comlen = sText.GetLength();
	if (sText[pos-1]=='\n')
	{
		line = cwin->LineFromChar(pos-1);
		char comd[1000], comd2[1000];
/*
.....if user hit return at the middle of the line
.....we need send the whole line to command
*/
		len = cwin->GetLine(line, comd, 1000);
		comd[len] = '\0';
		len2 = cwin->GetLine(line+1, comd2, 1000);
		comd2[len2] = '\0';
		if (len2!=0)
			strcat(comd, comd2);
		Run_command(&(comd[0]), (int*)this, 1);
		if (len2==0)
		{
			m_comcur = pos;
		}
		else
		{
			ud = 1;
			cwin->Undo();
			ud = 0;
			int inspos = m_comcur + len + len2;
			cwin->SetSel(inspos, inspos);
			cwin->ReplaceSel("\r\n");		
			m_comcur = m_comcur + len + len2 + 2;
		}
	}
/*
.....only undo the last typing action
.....so clean buffer
*/
	cwin->EmptyUndoBuffer();
}
/***********************************************************************
c
c   SUBROUTINE:  OnCommandPreKeys()
c
c   FUNCTION:  function called when pretranslate keys in command window
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CPtedWindow::OnCommandPreKeys()
{
	int pos;
	CEdit *cwin = (CEdit*)GetDlgItem(ID_ECOMMAND);
	cwin->GetSel(pos, pos);
	m_comcursor = pos;
	m_comline = cwin->LineFromChar(m_comcursor);
	m_comcur = cwin->LineIndex(m_comline);
}

void CPtedWindow::SetCutCopyDel_menu(int flag)
{
	if ((m_wtype==0) || (m_wtype==1) || (m_wtype==2))
	{
/*
.....deactive "Cut", "Copy" and "Delete" menu item
*/
		CMenu* fmenu = m_pmenu->GetSubMenu(1);
		if (m_undo_menu)
		{
			if (flag==0)
			{
				fmenu->EnableMenuItem(3, MF_BYPOSITION|MF_GRAYED);
				fmenu->EnableMenuItem(4, MF_BYPOSITION|MF_GRAYED);
				fmenu->EnableMenuItem(6, MF_BYPOSITION|MF_GRAYED);
			}
			else
			{
				fmenu->EnableMenuItem(3, MF_BYPOSITION|MF_ENABLED);
				fmenu->EnableMenuItem(4, MF_BYPOSITION|MF_ENABLED);
				fmenu->EnableMenuItem(6, MF_BYPOSITION|MF_ENABLED);
			}
		}
		else
		{
			if (flag==0)
			{
				fmenu->EnableMenuItem(0, MF_BYPOSITION|MF_GRAYED);
				fmenu->EnableMenuItem(1, MF_BYPOSITION|MF_GRAYED);
				fmenu->EnableMenuItem(3, MF_BYPOSITION|MF_GRAYED);
			}
			else
			{
				fmenu->EnableMenuItem(0, MF_BYPOSITION|MF_ENABLED);
				fmenu->EnableMenuItem(1, MF_BYPOSITION|MF_ENABLED);
				fmenu->EnableMenuItem(3, MF_BYPOSITION|MF_ENABLED);
			}
		}
		SetMenu(m_pmenu );
	}
}
