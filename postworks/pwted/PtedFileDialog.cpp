/************************************************************************
c
c   FILE NAME: PtedFileDialog.cpp
c
c	 CONTAINS: 
c	 all PtedFileDialog class: file browser with a verify toggle 
c			Implementation functions
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			PtedFileDialog.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c			09/11/13 , 12:59:26
c
c**********************************************************************
*/ 
// PtedFileDialog.cpp : implementation file
//

#include "pwstdafx.h"
#include "Pted.h"
#include "PtedFileDialog.h"
#include "ptedres.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// PtedFileDialog

IMPLEMENT_DYNAMIC(PtedFileDialog, CFileDialog)
/***********************************************************************
c
c   SUBROUTINE: PtedFileDialog	
c
c   FUNCTION:  constructor
c
c   INPUT:  
c			
c   OUTPUT: none
c
c***********************************************************************
*/
PtedFileDialog::PtedFileDialog(BOOL bOpenFileDialog, LPCTSTR lpszDefExt, LPCTSTR lpszFileName,
		DWORD dwFlags, LPCTSTR lpszFilter, CWnd* pParentWnd) :
		CFileDialog(bOpenFileDialog, lpszDefExt, lpszFileName, dwFlags, lpszFilter, pParentWnd)
{
	m_verify = 1;
}

/***********************************************************************
c
c   MESSAGE_MAP: callback descriptions
c
c***********************************************************************
*/
BEGIN_MESSAGE_MAP(PtedFileDialog, CFileDialog)
	//{{AFX_MSG_MAP(PtedFileDialog)
	ON_WM_CLOSE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE:  DoDataExchange(CDataExchange* pDX)
c
c   FUNCTION:  This function called by the "UpdateData" member function
c				This is a virtual function. 
c
c   INPUT:  pDX: A pointer to a CDataExchange object
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
void PtedFileDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(PtedFindReplaceDialog)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	DDX_Check(pDX, IDD_VERIFY, m_verify);
	//}}AFX_DATA_MAP
}

/***********************************************************************
c
c   SUBROUTINE:  OnNotify( WPARAM wParam, LPARAM lParam, LRESULT* pResult )
c
c   FUNCTION:  This function processes the message map for control notification
c				This is a virtual function. 
c
c   INPUT:  lParam:   Pointer to a notification message (NMHDR) structure that contains the 
c						notification code and additional information. 
c			wParam:   Identifies the control that sends the message if the message is from 
c						a control. Otherwise, wParam is 0.
c   OUTPUT: None
c	RETURN: None
c
c***********************************************************************
*/
BOOL PtedFileDialog::OnNotify( WPARAM wParam, LPARAM lParam, LRESULT* pResult )
{
	if (((LPOFNOTIFY)lParam)->hdr.code==CDN_FILEOK)
		UpdateData();
	return CWnd::OnNotify( wParam,lParam, pResult );
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
INT_PTR PtedFileDialog::DoModal()
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
