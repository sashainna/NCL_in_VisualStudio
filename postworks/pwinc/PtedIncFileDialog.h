/************************************************************************
c
c   FILE NAME: PtedIncFileDialog.h
c
c	 CONTAINS: 
c	 all PtedIncFileDialog class: file browser with a button label changed
c			variables and function definition
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        PtedIncFileDialog.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:34
c
c**********************************************************************
*/
#ifndef PTEDINCFILEDIALOG_H
#define PTEDINCFILEDIALOG_H


#include <DLGS.h>
#include "ptedres.h"
#include "PtedRangeBox.h"

// PtedIncFileDialog.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// PtedIncFileDialog dialog

class PtedIncFileDialog : public CFileDialog
{
	DECLARE_DYNAMIC(PtedIncFileDialog)

public:

	PtedRangeStruct m_fRange;
	PtedIncFileDialog(BOOL bOpenFileDialog, // TRUE for FileOpen, FALSE for FileSaveAs
		LPCTSTR lpszDefExt = NULL,
		LPCTSTR lpszFileName = NULL,
		DWORD dwFlags = OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT,
		LPCTSTR lpszFilter = NULL,
		CWnd* pParentWnd = NULL);
		virtual INT_PTR DoModal( );
// Dialog Data
	//{{AFX_DATA(PtedIncFileDialog)
	
	//}}AFX_DATA

protected:
	//{{AFX_MSG(PtedIncFileDialog)
		// NOTE - the ClassWizard will add and remove member functions here.
		virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
#endif
