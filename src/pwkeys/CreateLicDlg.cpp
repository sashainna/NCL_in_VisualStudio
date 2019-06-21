/************************************************************************
**
**   FILE NAME: CreateLicDlg.cpp
**
**  Description - Functions and implementations for
**    CCreateLicDlg class.
**
**  CONTAINS:
**
**    COPYRIGHT 2002 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       CreateLicDlg.cpp , 23.1
**    DATE AND TIME OF LAST  MODIFICATION
**       05/22/12 , 11:15:13
*********************************************************************/
// CreateLicDlg.cpp : implementation file
//

#include "stdafx.h"
#include "nccs_auth.h"
#include "CreateLicDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CCreateLicDlg dialog


CCreateLicDlg::CCreateLicDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CCreateLicDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CCreateLicDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CCreateLicDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CCreateLicDlg)
	DDX_Control(pDX, IDC_MATCHLISTS, m_lbLicense);
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CCreateLicDlg, CDialog)
	//{{AFX_MSG_MAP(CCreateLicDlg)
	ON_CONTROL(LBN_SELCHANGE, IDC_MATCHLISTS, SelectCallbacks)
	ON_BN_CLICKED(IDC_SELECTALL, OnSelectall)
	ON_BN_CLICKED(IDOK, OnCreate)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CCreateLicDlg message handlers

BOOL CCreateLicDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	int iItem = 
	m_lbLicense.AddString("           Test1                 TTT1         *");
	m_lbLicense.AddString("           Test2                 ttt2         *    ");
	m_lbLicense.AddString("           Test3                 TTT3         *  ");
	m_lbLicense.AddString("           Test4                 ttt4         *  ");
//	m_lbLicense.SetItemData( iItem, iCategory );
	
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CCreateLicDlg::OnSelectall() 
{
	int i, count;
	count = m_lbLicense.GetCount();
	for (i=0;i<count;i++)
		m_lbLicense.SetCheck( i, 1);
}

void CCreateLicDlg::OnCreate() 
{
	// TODO: Add your control notification handler code here
	
}

void CCreateLicDlg::OnCancel() 
{
	// TODO: Add extra cleanup here
	
	CDialog::OnCancel();
}

void CCreateLicDlg::SelectCallbacks() 
{
	int indx;
	indx = m_lbLicense.GetCurSel();
	m_lbLicense.SetCheck( indx, 1);
}
