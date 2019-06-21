/********************************************************************* 
**  NAME:  wsntopt.h
**
**			interface of CNCLDirDialog class functions
**
**	CONTAINS: CNCLDirDialog class functions and structure
**				declarations
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntdir.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:16
*********************************************************************/

#ifndef NCLDIRDIALOG_H
#define NCLDIRDIALOG_H

/////////////////////////////////////////////////////////////////////////////
// CNCLDirDialog dialog

class CNCLDirDialog : public CFileDialog
{
	DECLARE_DYNAMIC(CNCLDirDialog)

public:
	CNCLDirDialog(BOOL bOpenFileDialog, // TRUE for FileOpen, FALSE for FileSaveAs
		LPCTSTR lpszDefExt = NULL,
		LPCTSTR lpszFileName = NULL,
		DWORD dwFlags = OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		LPCTSTR lpszFilter = NULL,
		CWnd* pParentWnd = NULL);

	CString GetPathName() const;  // return full path and filename
	void OnDirChange();

protected:
	CString m_filename;
	char m_current[256];
	int m_initdlg;
	virtual BOOL OnFileNameOK( );
	virtual BOOL OnCommand( WPARAM wParam, LPARAM lParam );


	//{{AFX_MSG(CNCLDirDialog)
	afx_msg void OnDirAccept();
	afx_msg void OnDirSelected();
	afx_msg void OnDiskChange();
	virtual BOOL OnInitDialog();
	afx_msg void OnDirDefault();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#endif 
