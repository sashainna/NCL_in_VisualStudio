/********************************************************************* 
**  NAME:  wsntopt.h
**
**			interface of CNCLFileDialog class functions
**
**	CONTAINS: CNCLFileDialog class functions and structure
**				declarations
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntfile.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:17      
*********************************************************************/

#ifndef NCLFILEDIALOG_H
#define NCLFILEDIALOG_H

/////////////////////////////////////////////////////////////////////////////
// CNCLFileDialog dialog

class CNCLFileDialog : public CFileDialog
{
	DECLARE_DYNAMIC(CNCLFileDialog)

public:
	CNCLFileDialog(BOOL bOpenFileDialog, // TRUE for FileOpen, FALSE for FileSaveAs
		LPCTSTR lpszDefExt = NULL,
		LPCTSTR lpszFileName = NULL,
		DWORD dwFlags = OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		LPCTSTR lpszFilter = NULL,
		CWnd* pParentWnd = NULL);

	CString GetPathName() const;  // return full path and filename

protected:
	CString m_filename;
	//{{AFX_MSG(CNCLFileDialog)
	afx_msg void OnFileAccept();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#endif 
