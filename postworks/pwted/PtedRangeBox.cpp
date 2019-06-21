/************************************************************************
c
c   FILE NAME: PtedRangeBox.cpp.cpp
c
c	 CONTAINS: 
c	 all PtedRangeBox.cpp class: a dialog with all the Range input
c			Implementation functions
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			PtedRangeBox.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c			09/11/13 , 12:59:27
c
c**********************************************************************
*/
// PtedRangeBox.cpp : implementation file
//

#include "pwstdafx.h"
#include "Pted.h"
#include "PtedRangeBox.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// PtedRangeBox dialog

/***********************************************************************
c
c   SUBROUTINE: PtedRangeBox
c
c   FUNCTION:  constructor
c
c   INPUT:  
c			
c   OUTPUT: none
c
c***********************************************************************
*/
PtedRangeBox::PtedRangeBox(CWnd* pParent /*=NULL*/, DWORD flag, PtedRangeStruct *range)
	: CDialog(PtedRangeBox::IDD, pParent)
{
	if (flag==0)
		m_flags = TED_RANGE_BEGIN1 | TED_RANGE_BEGIN2 | TED_RANGE_BEGIN3
				| TED_RANGE_BEGIN4 | TED_RANGE_BEGIN5 | TED_RANGE_BEGIN7
				| TED_RANGE_BEGIN6 | TED_RANGE_END1 | TED_RANGE_END5
				| TED_RANGE_END2 | TED_RANGE_END3 | TED_RANGE_END4;
	else
		m_flags = flag;

	m_begin1 = 1;
	m_begin2 = 0;
	m_begin3 = 0;
	m_begin4 = 0;
	m_begin5 = 0;
	m_begin6 = 0;
	m_begin7 = 0;
	m_end1 = 1;
	m_end2 = 0;
	m_end3 = 0;
	m_end4 = 0;
	m_end5 = 0;
	m_estring = "";
	m_bstring = "";
	m_eaddress = "";
	m_baddress = "";
	m_enumber = "";
	m_bline = 0;
	m_eline = 0;
	if (range!=NULL)
	{
		if (range->begin==1)
			m_begin1 = 1;
		else
			m_begin1 = 0;
		if (range->begin==2)
			m_begin2 = 1;
		if (range->begin==3)
			m_begin3 = 1;
		if (range->begin==4)
			m_begin4 = 1;
		if (range->begin==5)
			m_begin5 = 1;
		if (range->begin==6)
			m_begin6 = 1;
		if (range->begin==7)
			m_begin7 = 1;
		if (range->end==1)
			m_end1 = 1;
		else
			m_end1 = 0;
		if (range->end==2)
			m_end2 = 1;
		if (range->end==3)
			m_end3 = 1;
		if (range->end==4)
			m_end4 = 1;
		if (range->end==5)
			m_end4 = 5;

		if (range->estring[0]!='\0')
			m_estring = range->estring;
		if (range->bstring[0]!='\0')
			m_bstring = range->bstring;
		if (range->enumber[0]!='\0')
			m_enumber = range->enumber;
		if (range->eaddress[0]!='\0')
			m_eaddress = range->eaddress;
		if (range->baddress[0]!='\0')
			m_baddress = range->baddress;
		m_bline = range->bline;
		m_eline = range->eline;
	}
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
void PtedRangeBox::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(PtedRangeBox)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	DDX_Text(pDX, IDC_EDIT1, m_baddress);
	DDX_Text(pDX, IDC_EDIT2, m_bstring);
	DDX_Text(pDX, IDC_EDIT3, m_eaddress);
	DDX_Text(pDX, IDC_EDIT4, m_estring);
	DDX_Text(pDX, IDC_EDIT5, m_enumber);
	DDX_Text(pDX, IDC_EDIT7, m_bline);
	DDX_Text(pDX, IDC_EDIT9, m_eline);
	DDX_Check(pDX, IDC_RADIO1, m_begin1);
	DDX_Check(pDX, IDC_RADIO2, m_begin2);
	DDX_Check(pDX, IDC_RADIO3, m_begin3);
	DDX_Check(pDX, IDC_RADIO4, m_begin4);
	DDX_Check(pDX, IDC_RADIO5, m_begin5);
	DDX_Check(pDX, IDC_RADIO6, m_begin6);
	DDX_Check(pDX, IDC_RADIO7, m_begin7);
	DDX_Check(pDX, IDC_RADIO8, m_end1);
	DDX_Check(pDX, IDC_RADIO9, m_end2);
	DDX_Check(pDX, IDC_RADIO10, m_end3);
	DDX_Check(pDX, IDC_RADIO11, m_end4);
	DDX_Check(pDX, IDC_RADIO12, m_end5);
	//}}AFX_DATA_MAP
}

/***********************************************************************
c
c   MESSAGE_MAP: callback descriptions
c
c***********************************************************************
*/
BEGIN_MESSAGE_MAP(PtedRangeBox, CDialog)
	//{{AFX_MSG_MAP(PtedRangeBox)
	ON_BN_CLICKED(IDC_RADIO1, OnRadio1)
	ON_BN_CLICKED(IDC_RADIO2, OnRadio1)
	ON_BN_CLICKED(IDC_RADIO3, OnRadio3)
	ON_BN_CLICKED(IDC_RADIO4, OnRadio3)
	ON_BN_CLICKED(IDC_RADIO5, OnRadio3)
	ON_BN_CLICKED(IDC_RADIO6, OnRadio3)
	ON_BN_CLICKED(IDC_RADIO7, OnRadio3)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog
c
c   FUNCTION:  This function initialize window text
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
BOOL PtedRangeBox::OnInitDialog()
{
	CDialog::OnInitDialog();
	if ((m_flags&TED_RANGE_BEGIN1)==0)
		GetDlgItem(IDC_RADIO1)-> EnableWindow(FALSE);
	if ((m_flags&TED_RANGE_BEGIN2)==0)
		GetDlgItem(IDC_RADIO2)-> EnableWindow(FALSE);
	if ((m_flags&TED_RANGE_BEGIN3)==0)
		GetDlgItem(IDC_RADIO3)-> EnableWindow(FALSE);
	if ((m_flags&TED_RANGE_BEGIN4)==0)
		GetDlgItem(IDC_RADIO4)-> EnableWindow(FALSE);
	if ((m_flags&TED_RANGE_BEGIN5)==0)
	{
		GetDlgItem(IDC_RADIO5)-> EnableWindow(FALSE);
		GetDlgItem(IDC_EDIT1)-> EnableWindow(FALSE);
	}
	if ((m_flags&TED_RANGE_BEGIN6)==0)
	{
		GetDlgItem(IDC_RADIO6)-> EnableWindow(FALSE);
		GetDlgItem(IDC_EDIT2)-> EnableWindow(FALSE);
	}
	if ((m_flags&TED_RANGE_BEGIN7)==0)
	{
		GetDlgItem(IDC_RADIO7)-> EnableWindow(FALSE);
		GetDlgItem(IDC_EDIT7)-> EnableWindow(FALSE);
	}
	if ((m_flags&TED_RANGE_END1)==0)
	{
		GetDlgItem(IDC_RADIO8)-> EnableWindow(FALSE);
		GetDlgItem(IDC_EDIT5)-> EnableWindow(FALSE);
	}
	if ((m_flags&TED_RANGE_END2)==0)
		GetDlgItem(IDC_RADIO9)-> EnableWindow(FALSE);
	if ((m_flags&TED_RANGE_END3)==0)
	{
		GetDlgItem(IDC_RADIO10)-> EnableWindow(FALSE);
		GetDlgItem(IDC_EDIT3)-> EnableWindow(FALSE);
	}
	if ((m_flags&TED_RANGE_END4)==0)
	{
		GetDlgItem(IDC_RADIO11)-> EnableWindow(FALSE);
		GetDlgItem(IDC_EDIT4)-> EnableWindow(FALSE);
	}
	if ((m_flags&TED_RANGE_END5)==0)
	{
		GetDlgItem(IDC_RADIO12)-> EnableWindow(FALSE);
		GetDlgItem(IDC_EDIT9)-> EnableWindow(FALSE);
	}
	((CEdit*)GetDlgItem(IDC_EDIT7))->LimitText(1);
	((CEdit*)GetDlgItem(IDC_EDIT7))->SetWindowText("0");
	((CEdit*)GetDlgItem(IDC_EDIT9))->LimitText(1);
	((CEdit*)GetDlgItem(IDC_EDIT9))->SetWindowText("0");

	if ((m_begin1)||(m_begin2))
		Disable_End();
	return 0;
}  
/////////////////////////////////////////////////////////////////////////////
// PtedRangeBox message handlers

void PtedRangeBox::OnOK() 
{
	
	CDialog::OnOK();
}

void PtedRangeBox::OnCancel() 
{
	
	CDialog::OnCancel();
}
/***********************************************************************
c
c   SUBROUTINE:  OnRadio1() 
c
c   FUNCTION:  This function is callback of first 2 radio button 
c				of beginning line
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void PtedRangeBox::OnRadio1() 
{
	Disable_End();
}
/***********************************************************************
c
c   SUBROUTINE:  Disable_End() 
c
c   FUNCTION:  This function disable all fields of end line
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void PtedRangeBox::Disable_End()
{

	GetDlgItem(IDC_RADIO9)->EnableWindow(FALSE);

	GetDlgItem(IDC_RADIO8)->EnableWindow(FALSE);
	GetDlgItem(IDC_EDIT5)->EnableWindow(FALSE);	

	GetDlgItem(IDC_RADIO10)->EnableWindow(FALSE);
	GetDlgItem(IDC_EDIT3)->EnableWindow(FALSE);	

	GetDlgItem(IDC_RADIO11)->EnableWindow(FALSE);
	GetDlgItem(IDC_EDIT4)->EnableWindow(FALSE);	

	GetDlgItem(IDC_RADIO12)->EnableWindow(FALSE);
	GetDlgItem(IDC_EDIT9)->EnableWindow(FALSE);	
}
/***********************************************************************
c
c   SUBROUTINE:  OnRadio3() 
c
c   FUNCTION:  This function is callback of 3-6 radio button
c					of beginning line
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void PtedRangeBox::OnRadio3() 
{
	Enable_End();
}

/***********************************************************************
c
c   SUBROUTINE:  Enable_End() 
c
c   FUNCTION:  This function Enable all fields of end line
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void PtedRangeBox::Enable_End()
{
	GetDlgItem(IDC_RADIO9)->EnableWindow(TRUE);

	GetDlgItem(IDC_RADIO8)->EnableWindow(TRUE);
	GetDlgItem(IDC_EDIT5)->EnableWindow(TRUE);	

	GetDlgItem(IDC_RADIO10)->EnableWindow(TRUE);
	GetDlgItem(IDC_EDIT3)->EnableWindow(TRUE);	

	GetDlgItem(IDC_RADIO11)->EnableWindow(TRUE);
	GetDlgItem(IDC_EDIT4)->EnableWindow(TRUE);	

	GetDlgItem(IDC_RADIO12)->EnableWindow(TRUE);
	GetDlgItem(IDC_EDIT9)->EnableWindow(TRUE);	
}
