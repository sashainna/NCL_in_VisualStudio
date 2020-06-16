/************************************************************************
c
c   FILE NAME: StepOutFiltDlg.cpp
c
c	 CONTAINS: 
c		Functions for the class CStepOutFiltDlg, which is
c			class for "Filter Entities" dialog
c
c     COPYRIGHT 2014 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       StepOutFiltDlg.cpp , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:13:25
c
c**********************************************************************
*/
// StepOutFiltDlg.cpp : implementation file
//

#include "stdafx.h"
#include "step.h"
#include "StepOutFiltDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int entity_out[37];
extern "C" int UIG_splitccrv;

#define NCHECKS 7

/////////////////////////////////////////////////////////////////////////////
// CStepOutFiltDlg dialog


CStepOutFiltDlg::CStepOutFiltDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CStepOutFiltDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CStepOutFiltDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CStepOutFiltDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CStepOutFiltDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CStepOutFiltDlg, CDialog)
	//{{AFX_MSG_MAP(CStepOutFiltDlg)
	ON_BN_CLICKED(IDC_ALLOFF, OnAlloff)
	ON_BN_CLICKED(IDC_ALLON, OnAllon)
	ON_BN_CLICKED(IDOK, OnAccept)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CStepOutFiltDlg message handlers

void CStepOutFiltDlg::OnAlloff() 
{
	int i;
	for (i=0; i<NCHECKS;i++)
		((CButton*)GetDlgItem(IDC_CHECK1+i))->SetCheck(0);	
}

void CStepOutFiltDlg::OnAllon() 
{
	int i;
	for (i=0; i<NCHECKS;i++)
		((CButton*)GetDlgItem(IDC_CHECK1+i))->SetCheck(1);	
}

void CStepOutFiltDlg::OnCancel() 
{	
	CDialog::OnCancel();
}

/*********************************************************************
**    I_FUNCTION     :  OnAccept() 
**			Callback function for the ACCEPT button 
**    PARAMETERS
**       INPUT  :
**				none
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CStepOutFiltDlg::OnAccept() 
{
	int i;
	for (i=0;i<37;i++)
	{
		if (i < NCHECKS)
			entity_out[i] = ((CButton*)GetDlgItem(IDC_CHECK1+i))->GetCheck();
		else
			entity_out[i] = 0;
	}

	CDialog::OnOK();
}

void CStepOutFiltDlg::OnSetEntity(UINT id)
{
/*
.....I think it shouldn't set here, cancel may cancel them
.....only when accept it, we accept these value
*/
/*
	int i = id - IDC_CHECK1;
	entity_out[i] = ((CButton*)GetDlgItem(id))->GetCheck();
*/
}

/***********************************************************************
c
c   FUNCTION: OnInitDialog() 
c
c		This function Set the initialized parameter into dialog
c       This member function is called in response to 
c		the WM_INITDIALOG message. This message is sent to 
c		the dialog box during the Create, CreateIndirect, 
c		or DoModal calls, which occur immediately before 
c		the dialog box is displayed. 
c   INPUT:  None.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
BOOL CStepOutFiltDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	int i, state;
	for (i=0; i<NCHECKS;i++)
		((CButton*)GetDlgItem(IDC_CHECK1+i))->SetCheck(entity_out[i]);
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}
