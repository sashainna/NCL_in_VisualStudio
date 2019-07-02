/************************************************************************
c
c   FILE NAME: OptFileDialog.h
c
c	 CONTAINS: 
c		Header file for OptFileDialog class 
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       OptFileDialog.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:06:06
c
c**********************************************************************
*/
#if !defined(AFX_OPTFILEDIALOG_H__332768F4_93D6_11D4_81FA_00C04F336F5E__INCLUDED_)
#define AFX_OPTFILEDIALOG_H__332768F4_93D6_11D4_81FA_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// OptFileDialog.h : header file
//

#include "xenv1.h"

/////////////////////////////////////////////////////////////////////////////
// COptFileDialog dialog

class COptFileDialog : public CFileDialog
{
	DECLARE_DYNAMIC(COptFileDialog)

public:
	COptFileDialog(BOOL bOpenFileDialog, // TRUE for FileOpen, FALSE for FileSaveAs
		LPCTSTR lpszDefExt = NULL,
		LPCTSTR lpszFileName = NULL,
		DWORD dwFlags = OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		LPCTSTR lpszFilter = NULL,
		CWnd* pParentWnd = NULL, char *dlocal = NULL, char *dsys = NULL);

protected:
	char m_defdirec[UX_MAX_PATH_LEN];
	char m_dlocal[UX_MAX_PATH_LEN], m_dsys[UX_MAX_PATH_LEN];
	//{{AFX_MSG(COptFileDialog)
	afx_msg void OnSysdir();
	afx_msg void OnLocDir();
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_OPTFILEDIALOG_H__332768F4_93D6_11D4_81FA_00C04F336F5E__INCLUDED_)
