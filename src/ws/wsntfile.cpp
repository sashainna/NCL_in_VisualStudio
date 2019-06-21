/********************************************************************* 
**  NAME:  wsntfile.cpp
**
**			Native WinNT file browser functions (we changed default browser
**				to  allow accept directory, default browser will accept only
**				if there are filename)
**			implementation of CNCLFileDialog class functions
**	CONTAINS: CNCLFileDialog class functions
**			all functions declared in wsntfile.h
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntfile.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:23      
*********************************************************************/

#include "wsntstdafx.h"
#include "wsntfile.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// COptFileDialog

IMPLEMENT_DYNAMIC(CNCLFileDialog, CFileDialog)

/***********************************************************************
**
**   FUNCTION: CNCLFileDialog(BOOL bOpenFileDialog, LPCTSTR lpszDefExt, LPCTSTR lpszFileName,
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
CNCLFileDialog::CNCLFileDialog(BOOL bOpenFileDialog, LPCTSTR lpszDefExt, LPCTSTR lpszFileName,
		DWORD dwFlags, LPCTSTR lpszFilter, CWnd* pParentWnd) :
		CFileDialog(bOpenFileDialog, lpszDefExt, lpszFileName, dwFlags, lpszFilter, pParentWnd)
{
}


BEGIN_MESSAGE_MAP(CNCLFileDialog, CFileDialog)
	//{{AFX_MSG_MAP(CNCLFileDialog)
	ON_BN_CLICKED(IDOK, OnFileAccept)
	ON_COMMAND(IDOK, OnFileAccept)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

void CNCLFileDialog::OnFileAccept()
{
	Default();
	CString name = GetFileName();
	if (name=="")
/*
.....if filename is "", it mean the selection is a directory
.....but the default will not close filebrowser, but since 
.....we want the browser to close but need to remember the path
....and assaign to the filename
*/
	{
		CString dir;
		int cursel = ((CListBox*)GetDlgItem(1121))->GetCurSel();
		CString rString ;
		for (int i=0; i<=cursel; i++)
		{
			((CListBox*)GetDlgItem(1121))->GetText(i, rString );
			if (i==0)
				dir = rString;
			else
				dir = dir + rString + "\\";
		}
		m_filename = dir;
		CDialog::OnOK();
	}
	else
		m_filename = CFileDialog::GetPathName();	
}

CString CNCLFileDialog::GetPathName() const
{
	return m_filename;
}

