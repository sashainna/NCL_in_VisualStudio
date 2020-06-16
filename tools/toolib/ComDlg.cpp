/************************************************************************
c
c   FILE NAME: ComDlg.cpp
c
c	 CONTAINS: 
c		Functions for CComDlg class 
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       ComDlg.cpp , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       10/12/15 , 17:21:36
c
c**********************************************************************
*/
// ComDlg.cpp : implementation file
//

#include "toolibstdafx.h"
#include "toolib.h"
#include "ComDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CComDlg dialog

/***********************************************************************
c
c   FUNCTION: CComDlg(CWnd* pParent)
c
c              Constructor of class CComDlg
c
c   INPUT:  pParent: parent window
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

CComDlg::CComDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CComDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CComDlg)
	m_com1 = _T("");
	m_com2 = _T("");
	m_com3 = _T("");
	m_com4 = _T("");
	m_com5 = _T("");
	m_com6 = _T("");
	m_com7 = _T("");
	m_com8 = _T("");
	m_com9 = _T("");
	m_com10 = _T("");
	m_com11 = _T("");
	m_com12 = _T("");
	m_com13 = _T("");
	m_com14 = _T("");
	m_com15 = _T("");
	m_com16 = _T("");
	m_com17 = _T("");
	m_com18 = _T("");
	m_com19 = _T("");
	m_com20 = _T("");
	m_com21 = _T("");
	m_com22 = _T("");
	m_com23 = _T("");
	m_com24 = _T("");
	m_com25 = _T("");
	m_com26 = _T("");
	m_com27 = _T("");
	m_com28 = _T("");
	m_com29 = _T("");
	m_com30 = _T("");
	m_com31 = _T("");
	m_com32 = _T("");
	m_com33 = _T("");
	m_com34 = _T("");
	m_com35 = _T("");
	m_com36 = _T("");
	m_com37 = _T("");
	m_com38 = _T("");
	m_com39 = _T("");
	m_com40 = _T("");
	m_com41 = _T("");

		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}

/***********************************************************************
c
c   FUNCTION: DoDataExchange(CDataExchange* pDX)
c
c         Called by the framework to exchange and validate dialog data.
c
c   INPUT:  pDX   A pointer to a CDataExchange object.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CComDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CComDlg)
	DDX_Text(pDX, IDC_EDIT1, m_com1);
	DDX_Text(pDX, IDC_EDIT2, m_com2);
	DDX_Text(pDX, IDC_EDIT3, m_com3);
	DDX_Text(pDX, IDC_EDIT4, m_com4);
	DDX_Text(pDX, IDC_EDIT5, m_com5);
	DDX_Text(pDX, IDC_EDIT6, m_com6);
	DDX_Text(pDX, IDC_EDIT7, m_com7);
	DDX_Text(pDX, IDC_EDIT8, m_com8);
	DDX_Text(pDX, IDC_EDIT9, m_com9);
	DDX_Text(pDX, IDC_EDIT10, m_com10);
	DDX_Text(pDX, IDC_EDIT11, m_com11);
	DDX_Text(pDX, IDC_EDIT12, m_com12);
	DDX_Text(pDX, IDC_EDIT13, m_com13);
	DDX_Text(pDX, IDC_EDIT14, m_com14);
	DDX_Text(pDX, IDC_EDIT15, m_com15);
	DDX_Text(pDX, IDC_EDIT16, m_com16);
	DDX_Text(pDX, IDC_EDIT17, m_com17);
	DDX_Text(pDX, IDC_EDIT18, m_com18);
	DDX_Text(pDX, IDC_EDIT19, m_com19);
	DDX_Text(pDX, IDC_EDIT20, m_com20);
	DDX_Text(pDX, IDC_EDIT21, m_com21);
	DDX_Text(pDX, IDC_EDIT22, m_com22);
	DDX_Text(pDX, IDC_EDIT23, m_com23);
	DDX_Text(pDX, IDC_EDIT24, m_com24);
	DDX_Text(pDX, IDC_EDIT25, m_com25);
	DDX_Text(pDX, IDC_EDIT26, m_com26);
	DDX_Text(pDX, IDC_EDIT27, m_com27);
	DDX_Text(pDX, IDC_EDIT28, m_com28);
	DDX_Text(pDX, IDC_EDIT29, m_com29);
	DDX_Text(pDX, IDC_EDIT30, m_com30);
	DDX_Text(pDX, IDC_EDIT31, m_com31);
	DDX_Text(pDX, IDC_EDIT32, m_com32);
	DDX_Text(pDX, IDC_EDIT33, m_com33);
	DDX_Text(pDX, IDC_EDIT34, m_com34);
	DDX_Text(pDX, IDC_EDIT35, m_com35);
	DDX_Text(pDX, IDC_EDIT36, m_com36);
	DDX_Text(pDX, IDC_EDIT37, m_com37);
	DDX_Text(pDX, IDC_EDIT38, m_com38);
	DDX_Text(pDX, IDC_EDIT39, m_com39);
	DDX_Text(pDX, IDC_EDIT40, m_com40);
	DDX_Text(pDX, IDC_EDIT41, m_com41);

		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CComDlg, CDialog)
	//{{AFX_MSG_MAP(CComDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CComDlg message handlers

void CComDlg::OnOK() 
{	
	CDialog::OnOK();
}
/***********************************************************************
c
c   FUNCTION: OnInitDialog() 
c
c		This function get the initialized parameter command
c		and fill the text field
c         This member function is called in response to 
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

BOOL CComDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
	
	((CEdit*)GetDlgItem(IDC_EDIT1))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT2))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT3))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT4))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT5))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT6))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT7))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT8))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT9))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT10))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT11))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT12))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT13))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT14))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT15))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT16))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT17))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT18))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT19))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT20))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT21))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT22))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT23))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT24))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT25))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT26))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT27))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT28))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT29))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT30))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT31))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT32))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT33))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT34))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT35))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT36))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT37))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT38))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT39))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT40))->LimitText(20);
	((CEdit*)GetDlgItem(IDC_EDIT41))->LimitText(20);
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}
