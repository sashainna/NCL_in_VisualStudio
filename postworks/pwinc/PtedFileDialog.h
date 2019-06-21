/************************************************************************
c
c   FILE NAME: PtedFileDialog.h
c
c	 CONTAINS: 
c	 all PtedFileDialog class: CFileDialog with "Verify" toggle
c			variables and function definition
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			PtedFileDialog.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c			09/11/13 , 12:58:34
c
c**********************************************************************
*/
#if !defined(AFX_PtedFileDialog_H__2C7AFD94_9C62_11D3_80E9_00C04F336F5E__INCLUDED_)
#define AFX_PtedFileDialog_H__2C7AFD94_9C62_11D3_80E9_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// PtedFileDialog.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// PtedFileDialog dialog

class PtedFileDialog : public CFileDialog
{
	DECLARE_DYNAMIC(PtedFileDialog)

public:
	PtedFileDialog(BOOL bOpenFileDialog, // TRUE for FileOpen, FALSE for FileSaveAs
		LPCTSTR lpszDefExt = NULL,
		LPCTSTR lpszFileName = NULL,
		DWORD dwFlags = OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		LPCTSTR lpszFilter = NULL,
		CWnd* pParentWnd = NULL);
		int m_verify;
		virtual INT_PTR DoModal();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(PtedPWOptDialog)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL


protected:
	virtual BOOL OnNotify( WPARAM wParam, LPARAM lParam, LRESULT* pResult );


	//{{AFX_MSG(PtedFileDialog)

	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PtedFileDialog_H__2C7AFD94_9C62_11D3_80E9_00C04F336F5E__INCLUDED_)
