/********************************************************************* 
**  NAME:  wsntdir.cpp
**
**			Native WinNT file browser functions (we changed default browser
**				to  allow accept directory, default browser will accept only
**				if there are filename)
**			implementation of CNCLDirDialog class functions
**	CONTAINS: CNCLDirDialog class functions
**			all functions declared in wsntdir.h
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntdir.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:21
*********************************************************************/

#include "wsntstdafx.h"
#include <direct.h>
#include "wsntdir.h"
#include "wsntres.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

char ppath[256];
/////////////////////////////////////////////////////////////////////////////
// CNCLDirDialog

IMPLEMENT_DYNAMIC(CNCLDirDialog, CFileDialog)

/***********************************************************************
**
**   FUNCTION: CNCLDirDialog(BOOL bOpenFileDialog, LPCTSTR lpszDefExt, LPCTSTR lpszFileName,
**					DWORD dwFlags, LPCTSTR lpszFilter, CWnd* pParentWnd)
**
**		Constructor of class CNCLtFileDialog
**
**   INPUT:  bOpenFileDialog: Set to TRUE to construct a File Open dialog box or FALSE to construct a File Save As dialog box.
**			lpszDefExt: The default filename extension. If the user does not include an extension in the Filename edit box, 
**						the extension specified by lpszDefExt is automatically appended to the filename. If this parameter is NULL, no file extension is appended.
**			lpszFileName: The initial filename that appears in the filename edit box. If NULL, no filename initially appears.
**			dwFlags:   A combination of one or more flags that allow you to customize the dialog box. For a description of these flags, see the OPENFILENAME structure in the Win32 SDK documentation. If you modify the m_ofn.Flags structure member, use a bitwise-OR operator in your changes to keep the default behavior intact.
**			lpszFilter:   A series of string pairs that specify filters you can apply to the file. If you specify file filters, only selected files will appear in the Files list box. See the Remarks section for more information on how to work with file filters.
**			pParentWnd:   A pointer to the file dialog-box object's parent or owner window.
**
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CNCLDirDialog::CNCLDirDialog(BOOL bOpenFileDialog, LPCTSTR lpszDefExt, LPCTSTR lpszFileName,
		DWORD dwFlags, LPCTSTR lpszFilter, CWnd* pParentWnd) :
//if VS2005 or earlier, there is 2 less pararmeter for CFileDialog contructor
//		CFileDialog(bOpenFileDialog, lpszDefExt, lpszFileName, dwFlags, lpszFilter, pParentWnd)
		CFileDialog(bOpenFileDialog, lpszDefExt, lpszFileName, dwFlags, lpszFilter, pParentWnd, 0,0)
{
	GetCurrentDirectory(255, m_current);
	if ((lpszFileName==NULL)||(lpszFileName[0]=='\0'))
	{
		m_filename = m_current;
	}
	else
		m_filename = lpszFileName;
	m_initdlg = 0;
	m_ofn.lpstrInitialDir = m_filename;
}


BEGIN_MESSAGE_MAP(CNCLDirDialog, CFileDialog)
	//{{AFX_MSG_MAP(CNCLDirDialog)
	ON_BN_CLICKED(IDOK, OnDirAccept)
	ON_LBN_DBLCLK(ID_DIREC_LIST, OnDirSelected)
	ON_CBN_SELCHANGE(ID_DISK_LIST, OnDiskChange)
	ON_COMMAND(IDC_FDLG_DEFAULT, OnDirDefault)
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
BOOL CNCLDirDialog::OnInitDialog()
{
	CFileDialog::OnInitDialog();
	PostMessage(WM_COMMAND, IDC_FDLG_DEFAULT);
	OnDirChange();
	return TRUE;
}

/***********************************************************************
c
c   SUBROUTINE:  OnDirDefault
c
c   FUNCTION:  This function initialize default directory
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNCLDirDialog::OnDirDefault()
{
	GetDlgItem(IDC_FDLG_FNAME)->SetWindowText(m_filename);
	m_initdlg = 1;
	SendMessage(WM_COMMAND, IDOK);
	m_initdlg = 0;
/*
.....this filename field just for set directory, we
.....nedd hide it after we use it
*/
	GetDlgItem(IDC_FDLG_FNAME)->ShowWindow(SW_HIDE);
	GetDlgItem(IDC_FOLDER_TEXT)->SetWindowText(m_filename);
}


/***********************************************************************
c
c   SUBROUTINE:  OnDirSelected
c
c   FUNCTION:  Callback function for select a directory
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNCLDirDialog::OnDirSelected()
{
	Default();
	OnDirChange();
}

/***********************************************************************
c
c   SUBROUTINE:  OnDirChange
c
c   FUNCTION: function to do when directory changed
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNCLDirDialog::OnDirChange()
{
	CString dir;
	int cursel = ((CListBox*)GetDlgItem(ID_DIREC_LIST))->GetCurSel();
	CString rString ;
	for (int i=0; i<=cursel; i++)
	{
		((CListBox*)GetDlgItem(ID_DIREC_LIST))->GetText(i, rString );
		if (i==0)
			dir = rString;
		else if (i==cursel)
			dir = dir + rString;
		else
			dir = dir + rString + "\\";
	}
	GetDlgItem(IDC_FOLDER_TEXT)->SetWindowText(dir);
}	

/***********************************************************************
c
c   SUBROUTINE:  OnDiskChange
c
c   FUNCTION:  Callback function for select a disk
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNCLDirDialog::OnDiskChange()
{
	CString dir;
	int cursel = ((CComboBox*)GetDlgItem(ID_DISK_LIST))->GetCurSel();
	CString rString ;
	((CComboBox*)GetDlgItem(ID_DISK_LIST))->GetLBText(cursel, rString);
	rString.TrimRight();
	dir = rString + "\\";
	GetDlgItem(IDC_FOLDER_TEXT)->SetWindowText(dir);
}	
/***********************************************************************
c
c   SUBROUTINE:  OnDirAccept
c
c   FUNCTION:  Callback function for "OK" button
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNCLDirDialog::OnDirAccept()
{
	if (m_initdlg == 1)
	{	
		Default();
		return;
	}
	GetDlgItem(IDC_FOLDER_TEXT)->GetWindowText(m_filename);
/*
.....set the file field a junk name just for OnOK() to close
.....the dialog, if we don't set a junk name here, then it will
.....not call OnFileNameOK() to check to see if close the window
.....and the dialog box will not close. If we use CDialog::OnOK to close
.....the window, the CFileDialog does not know dialog is closed and will
.....get into trouble.
.....Yurong 6/15/05
*/  
	GetDlgItem(IDC_FDLG_FNAME)->SetWindowText("TT");
	OnOK();
}
/***********************************************************************
c
c   SUBROUTINE:  OnFileNameOK
c
c   FUNCTION:  We vverride this function because we need alway OK
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
BOOL CNCLDirDialog::OnFileNameOK()
{
	return 0;
}
/***********************************************************************
c
c   SUBROUTINE:  OnCommand( WPARAM wParam, LPARAM lParam)
c
c   FUNCTION:  OnCommand processes the message map for control 
c				notification and ON_COMMAND entries, and calls the 
c				appropriate member function. 
c				We overwrite this function because we use CFileDialog 
c				as the basic class for directory dialog, so the close
c				function may difference and cause error after we close
c				dialog window (it still tried to dispatch the message
c				even though the window is gone (and cause a error)
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
BOOL CNCLDirDialog::OnCommand( WPARAM wParam, LPARAM lParam)
{
	HWND hWndCtrl = (HWND)lParam;
	if (::IsWindow(hWndCtrl))
		return CWnd::OnCommand(wParam, lParam);
	return 1;
}

/***********************************************************************
c
c   SUBROUTINE:  GetPathName
c
c   FUNCTION:  Get the directory name
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
CString CNCLDirDialog::GetPathName() const
{
	return m_filename;
}

