/************************************************************************
c
c   FILE NAME: PtedIncFileDialog.cpp
c
c	 CONTAINS: 
c	 all PtedFileDialog class: file browser with a button label changed
c			for include file. 
c			Implementation functions
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			PtedIncFileDialog.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c			09/11/13 , 12:59:26
c
c**********************************************************************
*/
// PtedIncFileDialog.cpp : implementation file
//

#include "pwstdafx.h"
#include "Pted.h"
#include "PtedIncFileDialog.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// PtedIncFileDialog


IMPLEMENT_DYNAMIC(PtedIncFileDialog, CFileDialog)

/***********************************************************************
c
c   SUBROUTINE: PtedIncFileDialog	
c
c   FUNCTION:  constructor
c
c   INPUT:  
c			
c   OUTPUT: none
c
c***********************************************************************
*/
PtedIncFileDialog::PtedIncFileDialog(BOOL bOpenFileDialog, LPCTSTR lpszDefExt, LPCTSTR lpszFileName,
		DWORD dwFlags, LPCTSTR lpszFilter, CWnd* pParentWnd) :
		CFileDialog(bOpenFileDialog, lpszDefExt, lpszFileName, dwFlags, lpszFilter, pParentWnd)
{
}
/***********************************************************************
c
c   MESSAGE_MAP: callback descriptions
c
c***********************************************************************
*/
BEGIN_MESSAGE_MAP(PtedIncFileDialog, CFileDialog)
	//{{AFX_MSG_MAP(PtedIncFileDialog)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog() 
c
c   FUNCTION:  This function is called when the dialog box is initialized
c				This is a virtual function. 
c
c   INPUT:  None
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
BOOL PtedIncFileDialog::OnInitDialog() 
{
	CFileDialog::OnInitDialog();
	SetControlText(IDOK, _T("&Accept"));
	SetControlText(pshHelp, _T("&Range..."));
	return 1;
		
}

/***********************************************************************
c
c   SUBROUTINE:  DoModal
c
c   FUNCTION:  This function to display the Windows common file dialog 
c				box and allow the user to browse files and directories and 
c				enter a filename.
c				We overwrite this CFileDialog virtual function because we don't
c				want the 'EnableWindow' and 'SetFocus' function called which
c				will cause all edit text selected after the browser close.
c
c   INPUT:  none
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
INT_PTR PtedIncFileDialog::DoModal()
{
	ASSERT_VALID(this);
	ASSERT(m_ofn.Flags & OFN_ENABLEHOOK);
	ASSERT(m_ofn.lpfnHook != NULL);

	DWORD nOffset = lstrlen(m_ofn.lpstrFile)+1;
	ASSERT(nOffset <= m_ofn.nMaxFile);
	memset(m_ofn.lpstrFile+nOffset, 0, (m_ofn.nMaxFile-nOffset)*sizeof(TCHAR));

	BOOL bEnableParent = FALSE;
	m_ofn.hwndOwner = PreModal();
	AfxUnhookWindowCreate();

	_AFX_THREAD_STATE* pThreadState = AfxGetThreadState();
	ASSERT(pThreadState->m_pAlternateWndInit == NULL);

	if (m_ofn.Flags & OFN_EXPLORER)
		pThreadState->m_pAlternateWndInit = this;
	else
		AfxHookWindowCreate(this);

	int nResult;
	if (m_bOpenFileDialog)
		nResult = ::GetOpenFileName(&m_ofn);
	else
		nResult = ::GetSaveFileName(&m_ofn);

	if (nResult)
		ASSERT(pThreadState->m_pAlternateWndInit == NULL);
	pThreadState->m_pAlternateWndInit = NULL;

	PostModal();
	return nResult ? nResult : IDCANCEL;
}
