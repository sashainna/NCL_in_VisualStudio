/************************************************************************
c
c   FILE NAME: PtedSetDialog.cpp
c
c	 Description - Functions for PtedSetDialog
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			PtedSetDialog.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c			09/11/13 , 12:59:27
c
c**********************************************************************
*/
// PtedSetDialog.cpp : implementation file
//

#include "pwstdafx.h"
#include "Pted.h"
#include "PtedSetDialog.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// PtedSetDialog dialog

/***********************************************************************
c
c   SUBROUTINE: PtedSetDialog
c
c   FUNCTION:  constructor
c
c   INPUT:  
c			
c   OUTPUT: none
c
c***********************************************************************
*/
PtedSetDialog::PtedSetDialog(CWnd* pParent /*=NULL*/)
	: CDialog(PtedSetDialog::IDD, pParent)
{
	//{{AFX_DATA_INIT(PtedSetDialog)
	m_settext = _T("");
	//}}AFX_DATA_INIT
}

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
void PtedSetDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(PtedSetDialog)
	DDX_Text(pDX, IDC_SETTEXT, m_settext);
	DDV_MaxChars(pDX, m_settext, 500);
	//}}AFX_DATA_MAP
}

/***********************************************************************
c
c   MESSAGE_MAP: callback descriptions
c
c***********************************************************************
*/
BEGIN_MESSAGE_MAP(PtedSetDialog, CDialog)
	//{{AFX_MSG_MAP(PtedSetDialog)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
