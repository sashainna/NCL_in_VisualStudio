/************************************************************************
c
c   FILE NAME: PtedSetupPageDialog.cpp
c
c	 CONTAINS: 
c	 all PtedSetupPageDialog class: print setup dialog
c			Implementation functions
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			 PtedSetupPageDialog.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c			 09/11/13 , 12:59:27
c
c**********************************************************************
*/
// PtedSetupPageDialog.cpp : implementation file
//

#include "pwstdafx.h"
#include "Pted.h"
#include "PtedSetupPageDialog.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int Ptd_getMDF_unit();
/////////////////////////////////////////////////////////////////////////////
// PtedSetupPageDialog dialog


/***********************************************************************
c
c   SUBROUTINE: PtedSetupPageDialog
c
c   FUNCTION:  constructor
c
c   INPUT:  header, footer: default file header/footer flag
c			htext, ftext:    default file header/footer text
c			left, right, top, bottom: default margins
c			pParent: parent window
c			
c   OUTPUT: none
c
c***********************************************************************
*/
PtedSetupPageDialog::PtedSetupPageDialog(int header, int footer, int left, int right,
		int top, int bottom, char *htext, char *ftext,
		CWnd* pParent)	: CDialog(PtedSetupPageDialog::IDD, pParent)
{
	m_header = header;
	m_footer = footer;
	m_unit = Ptd_getMDF_unit();
	if (m_unit==1)
	{
		m_left = ((double)left)/25.4;
		m_right = ((double)right)/25.4;
		m_top = ((double)top)/25.4;
		m_bottom = ((double)bottom)/25.4;
	}
	else
	{
		m_left = left;
		m_right = right;
		m_top = top;
		m_bottom = bottom;
	}
	if ((htext==NULL) || (htext[0]=='\0'))
		strcpy(m_htext, "&f");
	else
		strcpy(m_htext, htext);
	if ((ftext==NULL) || (ftext[0]=='\0'))
		strcpy(m_ftext, "&p");
	else
		strcpy(m_ftext, ftext);
}
void PtedSetupPageDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(PtedSetupPageDialog)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	DDX_Text(pDX, IDC_MARGIN_LEFT, m_left);
	DDX_Text(pDX, IDC_MARGIN_RIGHT, m_right);
	DDX_Text(pDX, IDC_MARGIN_TOP, m_top);
	DDX_Text(pDX, IDC_MARGIN_BOTTOM, m_bottom);
	DDX_Check(pDX, IDC_HEADER, m_header);
	DDX_Check(pDX, IDC_FOOTER, m_footer);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(PtedSetupPageDialog, CDialog)
	//{{AFX_MSG_MAP(PtedSetupPageDialog)
	ON_BN_CLICKED(IDC_HEADER, OnHeader)
	ON_BN_CLICKED(IDC_FOOTER, Onfooter)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// PtedSetupPageDialog message handlers

/***********************************************************************
c
c   SUBROUTINE:  OnHeader
c
c   FUNCTION:  This function called when user click "Header" check button
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void PtedSetupPageDialog::OnHeader() 
{
	if (m_header)
		m_header = 0;
	else
		m_header = 1;
	GetDlgItem(IDC_HEADERTEXT)->EnableWindow(m_header);
}
/***********************************************************************
c
c   SUBROUTINE:  Onfooter
c
c   FUNCTION:  This function called when user click "Footer" check button
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void PtedSetupPageDialog::Onfooter() 
{
	if (m_footer)
		m_footer = 0;
	else
		m_footer = 1;
	GetDlgItem(IDC_FOOTERTEXT)->EnableWindow(m_footer);
}

/***********************************************************************
c
c   SUBROUTINE:  OnOK
c
c   FUNCTION:  This function called when user click "OK" button
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void PtedSetupPageDialog::OnOK() 
{	
	CDialog::OnOK();
	if (m_header)
	{
		GetDlgItem(IDC_HEADERTEXT)->GetWindowText(m_htext, 256);
	}
	if (m_footer)
	{
		GetDlgItem(IDC_FOOTERTEXT)->GetWindowText(m_ftext, 256);
	}	
	if (m_unit==1)
	{
/*
.....convert unit back to mm
*/
		m_left = m_left*25.4;
		m_right = m_right*25.4;
		m_top = m_top*25.4;
		m_bottom = m_bottom*25.4;
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog
c
c   FUNCTION:  This function initialize the setup dialog
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
BOOL PtedSetupPageDialog::OnInitDialog()
{
	CDialog::OnInitDialog();
	GetDlgItem(IDC_HEADERTEXT)->EnableWindow(m_header);
	GetDlgItem(IDC_FOOTERTEXT)->EnableWindow(m_footer);
	if (m_header)
	{
		GetDlgItem(IDC_HEADERTEXT)->SetWindowText(m_htext);
	}
	if (m_footer)
	{
		GetDlgItem(IDC_FOOTERTEXT)->SetWindowText(m_ftext);
	}	
	GetDlgItem(IDC_HEADERTEXT)->SetWindowText(m_htext);
	GetDlgItem(IDC_FOOTERTEXT)->SetWindowText(m_ftext);
	if (m_unit==1)
		GetDlgItem(IDC_PAGEUNIT)->SetWindowText("Margins (inch)");
	else
		GetDlgItem(IDC_PAGEUNIT)->SetWindowText("Margins (mm)");

	return true;
}
