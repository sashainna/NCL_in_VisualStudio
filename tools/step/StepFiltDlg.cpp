/************************************************************************
c
c   FILE NAME: StepFiltDlg.cpp
c
c	 CONTAINS: 
c		Functions for the class CStepFiltDlg, which is
c			class for "Filter Entities" dialog
c
c     COPYRIGHT 2013 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       StepFiltDlg.cpp , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:13:25
c
c**********************************************************************
*/
// StepFiltDlg.cpp : implementation file
//

#include "stdafx.h"
#include "step.h"
#include "StepFiltDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int entity_mask[37];
extern "C" int UIG_splitccrv;

#define NCHECKS 5

/////////////////////////////////////////////////////////////////////////////
// CStepFiltDlg dialog


CStepFiltDlg::CStepFiltDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CStepFiltDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CStepFiltDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CStepFiltDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CStepFiltDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CStepFiltDlg, CDialog)
	//{{AFX_MSG_MAP(CStepFiltDlg)
	ON_BN_CLICKED(IDC_ALLOFF, OnAlloff)
	ON_BN_CLICKED(IDC_ALLON, OnAllon)
	ON_BN_CLICKED(IDOK, OnAccept)
	ON_BN_CLICKED(IDC_CHECK1, OnCheckSfCv)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CStepFiltDlg message handlers

void CStepFiltDlg::OnAlloff() 
{
	int i;
	for (i=0; i<NCHECKS;i++)
		((CButton*)GetDlgItem(IDC_CHECK1+i))->SetCheck(0);	

	((CWnd*)GetDlgItem(IDC_RADIO_CURVEONLY))->EnableWindow(0);
	((CWnd*)GetDlgItem(IDC_RADIO_COMPSONLY))->EnableWindow(0);
	((CWnd*)GetDlgItem(IDC_RADIO_CURVEANDCOMPS))->EnableWindow(0);
}

void CStepFiltDlg::OnAllon() 
{
	int i;
	for (i=0; i<NCHECKS;i++)
		((CButton*)GetDlgItem(IDC_CHECK1+i))->SetCheck(1);	

	((CWnd*)GetDlgItem(IDC_RADIO_CURVEONLY))->EnableWindow(1);
	((CWnd*)GetDlgItem(IDC_RADIO_COMPSONLY))->EnableWindow(1);
	((CWnd*)GetDlgItem(IDC_RADIO_CURVEANDCOMPS))->EnableWindow(1);
}

void CStepFiltDlg::OnCancel() 
{	
	CDialog::OnCancel();
}

void CStepFiltDlg::OnCheckSfCv()
{
	int state = ((CButton*)GetDlgItem(IDC_CHECK1))->GetCheck();

	((CWnd*)GetDlgItem(IDC_RADIO_CURVEONLY))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_RADIO_COMPSONLY))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_RADIO_CURVEANDCOMPS))->EnableWindow(state);
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
void CStepFiltDlg::OnAccept() 
{
	int i;
	for (i=0;i<37;i++)
	{
		if (i < NCHECKS)
			entity_mask[i] = ((CButton*)GetDlgItem(IDC_CHECK1+i))->GetCheck();
		else
			entity_mask[i] = 0;
	}

	UIG_splitccrv = 0;
	if (((CButton*)GetDlgItem(IDC_RADIO_COMPSONLY))->GetCheck() == 1)
		UIG_splitccrv = 1;
	else if (((CButton*)GetDlgItem(IDC_RADIO_CURVEANDCOMPS))->GetCheck() == 1)
		UIG_splitccrv = 2;

	CDialog::OnOK();
}

void CStepFiltDlg::OnSetEntity(UINT id)
{
/*
.....I think it shouldn't set here, cancel may cancel them
.....only when accept it, we accept these value
*/
/*
	int i = id - IDC_CHECK1;
	entity_mask[i] = ((CButton*)GetDlgItem(id))->GetCheck();
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
BOOL CStepFiltDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	int i, state;
	for (i=0; i<NCHECKS;i++)
		((CButton*)GetDlgItem(IDC_CHECK1+i))->SetCheck(entity_mask[i]);
	
	state = entity_mask[0];
	((CWnd*)GetDlgItem(IDC_RADIO_CURVEONLY))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_RADIO_COMPSONLY))->EnableWindow(state);
	((CWnd*)GetDlgItem(IDC_RADIO_CURVEANDCOMPS))->EnableWindow(state);
/*
.....Initialize these fields according to the modfile entry.
*/
	if (UIG_splitccrv == 0)
		i = IDC_RADIO_CURVEONLY;
	if (UIG_splitccrv == 1)
		i = IDC_RADIO_COMPSONLY;
	else if (UIG_splitccrv == 2)
		i = IDC_RADIO_CURVEANDCOMPS;
	((CButton*)GetDlgItem(i))->SetCheck(1);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}
