/************************************************************************
c
c   FILE NAME: IgesFiltDlg.cpp
c
c	 CONTAINS: 
c		Functions for the class CIgesFiltDlg, which is
c			class for "Filter Entities" dialog
c
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       IgesFiltDlg.cpp , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:12:55               
c
c**********************************************************************
*/
// IgesFiltDlg.cpp : implementation file
//

#include "stdafx.h"
#include "iges.h"
#include "IgesFiltDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int entity_mask[37];
extern "C" int UIG_splitccrv;

/////////////////////////////////////////////////////////////////////////////
// CIgesFiltDlg dialog


CIgesFiltDlg::CIgesFiltDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CIgesFiltDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CIgesFiltDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CIgesFiltDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CIgesFiltDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CIgesFiltDlg, CDialog)
	//{{AFX_MSG_MAP(CIgesFiltDlg)
	ON_BN_CLICKED(IDC_ALLOFF, OnAlloff)
	ON_BN_CLICKED(IDC_ALLON, OnAllon)
	ON_BN_CLICKED(IDOK, OnAccept)
	ON_BN_CLICKED(IDC_CHECK19, OnCheckSfCv)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CIgesFiltDlg message handlers

void CIgesFiltDlg::OnAlloff() 
{
	int i;
	for (i=0; i<37;i++)
		((CButton*)GetDlgItem(IDC_CHECK1+i))->SetCheck(0);	

	((CWnd*)GetDlgItem(IDC_RADIO_CURVEONLY))->EnableWindow(0);
	((CWnd*)GetDlgItem(IDC_RADIO_COMPSONLY))->EnableWindow(0);
	((CWnd*)GetDlgItem(IDC_RADIO_CURVEANDCOMPS))->EnableWindow(0);
}

void CIgesFiltDlg::OnAllon() 
{
	int i;
	for (i=0; i<37;i++)
		((CButton*)GetDlgItem(IDC_CHECK1+i))->SetCheck(1);	

	((CWnd*)GetDlgItem(IDC_RADIO_CURVEONLY))->EnableWindow(1);
	((CWnd*)GetDlgItem(IDC_RADIO_COMPSONLY))->EnableWindow(1);
	((CWnd*)GetDlgItem(IDC_RADIO_CURVEANDCOMPS))->EnableWindow(1);
}

void CIgesFiltDlg::OnCancel() 
{	
	CDialog::OnCancel();
}

void CIgesFiltDlg::OnCheckSfCv()
{
	int state = ((CButton*)GetDlgItem(IDC_CHECK19))->GetCheck();

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
void CIgesFiltDlg::OnAccept() 
{
	int i;
	for (i=0; i<37;i++)
		entity_mask[i] = ((CButton*)GetDlgItem(IDC_CHECK1+i))->GetCheck();

	UIG_splitccrv = 0;
	if (((CButton*)GetDlgItem(IDC_RADIO_COMPSONLY))->GetCheck() == 1)
		UIG_splitccrv = 1;
	else if (((CButton*)GetDlgItem(IDC_RADIO_CURVEANDCOMPS))->GetCheck() == 1)
		UIG_splitccrv = 2;

	CDialog::OnOK();
}

void CIgesFiltDlg::OnSetEntity(UINT id)
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
BOOL CIgesFiltDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	int i, state;
	for (i=0; i<37;i++)
		((CButton*)GetDlgItem(IDC_CHECK1+i))->SetCheck(entity_mask[i]);
	
	state = entity_mask[18];
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
