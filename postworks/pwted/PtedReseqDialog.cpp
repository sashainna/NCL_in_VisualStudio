/************************************************************************
c
c   FILE NAME: PtedReseqDialog.cpp
c
c	 CONTAINS: 
c	 all PtedReseqDialog class: a dialog with all the Resequence input
c			Implementation functions
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			PtedReseqDialog.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c			09/11/13 , 12:59:27
c
c**********************************************************************
*/
// PtedReseqDialog.cpp : implementation file
//

#include "pwstdafx.h"
#include "Pted.h"
#include "PtedReseqDialog.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// PtedReseqDialog dialog


PtedReseqDialog::PtedReseqDialog(CWnd* pParent /*=NULL*/)
	: CDialog(PtedReseqDialog::IDD, pParent)
{
	m_seqinc = 1;
	m_bseq = 1;
	m_seqn = 1;
	m_nonly = 1;
	m_sRange.begin = 1;
	m_sRange.end = 1;
	m_sRange.baddress[0] = '\0';
	m_sRange.bstring[0] = '\0';
	m_sRange.enumber[0] = '\0';
	m_sRange.eaddress[0] = '\0';
	m_sRange.estring[0] = '\0';
}


void PtedReseqDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(PtedReseqDialog)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	DDX_Text(pDX, IDC_BSEQ, m_bseq);
	DDX_Text(pDX, IDC_SEQINC, m_seqinc);
	DDX_Text(pDX, IDC_SEQN, m_seqn);
	DDX_Check(pDX, IDC_NONLY, m_nonly);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(PtedReseqDialog, CDialog)
	//{{AFX_MSG_MAP(PtedReseqDialog)
	ON_BN_CLICKED(IDC_REQ_RANGE, OnReqRange)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// PtedReseqDialog message handlers

void PtedReseqDialog::OnReqRange() 
{
	int i;
	DWORD flags = TED_RANGE_BEGIN1 | TED_RANGE_BEGIN6 | TED_RANGE_BEGIN7
				| TED_RANGE_BEGIN2 | TED_RANGE_BEGIN3 | TED_RANGE_END5
				| TED_RANGE_BEGIN4 | TED_RANGE_BEGIN5 | TED_RANGE_END1
				| TED_RANGE_END2 | TED_RANGE_END3 | TED_RANGE_END4;
	PtedRangeBox *RangeBox = new PtedRangeBox(this, flags, &m_sRange);
	if (RangeBox->DoModal()==IDCANCEL)
	{
		delete RangeBox;
		return;
	}

	if (RangeBox->m_begin1==1)
		m_sRange.begin = 1;
	if (RangeBox->m_begin2==1)
		m_sRange.begin = 2;
	if (RangeBox->m_begin3==1)
		m_sRange.begin = 3;
	if (RangeBox->m_begin4==1)
		m_sRange.begin = 4;
	if (RangeBox->m_begin5==1)
		m_sRange.begin = 5;
	if (RangeBox->m_begin6==1)
		m_sRange.begin = 6;
	if (RangeBox->m_end1==1)
		m_sRange.end = 1;
	if (RangeBox->m_end2==1)
		m_sRange.end = 2;
	if (RangeBox->m_end3==1)
		m_sRange.end = 3;
	if (RangeBox->m_end4==1)
		m_sRange.end = 4;
	
	if (RangeBox->m_estring!="")
	{
		for (i=0; i<(RangeBox->m_estring).GetLength(); i++)
			m_sRange.estring[i] = RangeBox->m_estring[i];
		m_sRange.estring[i] = '\0';
	}
	if (RangeBox->m_bstring!="")
	{
		for (i=0; i<(RangeBox->m_bstring).GetLength(); i++)
			m_sRange.bstring[i] = RangeBox->m_bstring[i];
		m_sRange.bstring[i] = '\0';
	}
	if (RangeBox->m_enumber!="")
	{
		for (i=0; i<(RangeBox->m_enumber).GetLength(); i++)
			m_sRange.enumber[i] = RangeBox->m_enumber[i];
		m_sRange.enumber[i]= '\0';	
	}
	if (RangeBox->m_eaddress!="")
	{
		for (i=0; i<(RangeBox->m_eaddress).GetLength(); i++)
			m_sRange.eaddress[i] = RangeBox->m_eaddress[i];
		m_sRange.eaddress[i] = '\0';
	}
	if (RangeBox->m_baddress!="")
	{
		for (i=0; i<(RangeBox->m_baddress).GetLength(); i++)
			m_sRange.baddress[i] = RangeBox->m_baddress[i];
		m_sRange.baddress[i] = '\0';
	}	
}

void PtedReseqDialog::OnOK() 
{	
	CDialog::OnOK();
}

void PtedReseqDialog::OnCancel() 
{	
	CDialog::OnCancel();
}

BOOL PtedReseqDialog::OnInitDialog()
{
	return CDialog::OnInitDialog();
}
