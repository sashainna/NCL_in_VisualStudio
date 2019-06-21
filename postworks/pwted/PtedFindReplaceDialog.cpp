/************************************************************************
c
c   FILE NAME: PtedFindReplaceDialog.cpp
c
c	 CONTAINS: 
c	 all PtedFindReplaceDialog class: CFindReplaceDialog with "Range" and
c			some extra fields 
c			Implementation functions
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			PtedFindReplaceDialog.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c			09/11/13 , 12:59:26
c
c**********************************************************************
*/
// PtedFindReplaceDialog.cpp : implementation file
//

#include "pwstdafx.h"
#include "Pted.h"
#include "PtedFindReplaceDialog.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// PtedFindReplaceDialog


IMPLEMENT_DYNAMIC(PtedFindReplaceDialog, CFindReplaceDialog)
/***********************************************************************
c
c   SUBROUTINE: PtedFindReplaceDialog	
c
c   FUNCTION:  constructor
c
c   INPUT:  
c			
c   OUTPUT: none
c
c***********************************************************************
*/
PtedFindReplaceDialog::PtedFindReplaceDialog() :
		CFindReplaceDialog()
{
	m_fRangeBox = NULL;
	m_letter = 0;
	m_fRange.begin = 1;
	m_fRange.end = 1;
	m_fRange.baddress[0] = '\0';
	m_fRange.bstring[0] = '\0';
	m_fRange.enumber[0] = '\0';
	m_fRange.eaddress[0] = '\0';
	m_fRange.estring[0] = '\0';
	m_up =0;
	m_down = 1;
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
void PtedFindReplaceDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(PtedFindReplaceDialog)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	DDX_Check(pDX, ID_LETTER, m_letter);
	DDX_Check(pDX, 1056, m_up);
	DDX_Check(pDX, 1057, m_down);
	//}}AFX_DATA_MAP
}
/***********************************************************************
c
c   MESSAGE_MAP: callback descriptions
c
c***********************************************************************
*/
BEGIN_MESSAGE_MAP(PtedFindReplaceDialog, CFindReplaceDialog)
	//{{AFX_MSG_MAP(PtedFindReplaceDialog)
	ON_COMMAND(ID_RANGE, OnRange)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE:  OnRange() 
c
c   FUNCTION:  This function is callback of "Range" button
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void  PtedFindReplaceDialog::OnRange() 
{	
	DWORD flags = TED_RANGE_BEGIN1 | TED_RANGE_BEGIN6 | TED_RANGE_BEGIN7
				| TED_RANGE_BEGIN2 | TED_RANGE_BEGIN3	| TED_RANGE_END5
				| TED_RANGE_BEGIN4 | TED_RANGE_BEGIN5 | TED_RANGE_END1
				| TED_RANGE_END2 | TED_RANGE_END3 | TED_RANGE_END4;

	if (m_fRangeBox!=0)
	{
		m_fRangeBox->SetActiveWindow();
		m_fRangeBox->ShowWindow(SW_SHOW);
		return;
	}
	m_fRangeBox = new PtedRangeBox(this, flags, &m_fRange);
	if (m_fRangeBox->DoModal()==IDCANCEL)
	{
		delete m_fRangeBox;
		m_fRangeBox = NULL;
		return;
	}
	if (m_fRangeBox->m_begin1==1)
		m_fRange.begin = 1;
	if (m_fRangeBox->m_begin2==1)
		m_fRange.begin = 2;
	if (m_fRangeBox->m_begin3==1)
		m_fRange.begin = 3;
	if (m_fRangeBox->m_begin4==1)
		m_fRange.begin = 4;
	if (m_fRangeBox->m_begin5==1)
		m_fRange.begin = 5;
	if (m_fRangeBox->m_begin6==1)
		m_fRange.begin = 6;
	if (m_fRangeBox->m_begin7==1)
		m_fRange.begin = 7;
	if (m_fRangeBox->m_end1==1)
		m_fRange.end = 1;
	if (m_fRangeBox->m_end2==1)
		m_fRange.end = 2;
	if (m_fRangeBox->m_end3==1)
		m_fRange.end = 3;
	if (m_fRangeBox->m_end4==1)
		m_fRange.end = 4;
	if (m_fRangeBox->m_end5==1)
		m_fRange.end = 5;
	int i;
	if (m_fRangeBox->m_estring!="")
	{
		for (i=0; i<(m_fRangeBox->m_estring).GetLength(); i++)
			m_fRange.estring[i] = m_fRangeBox->m_estring[i];
		m_fRange.estring[i] = '\0';
	}
	if (m_fRangeBox->m_bstring!="")
	{
		for (i=0; i<(m_fRangeBox->m_bstring).GetLength(); i++)
			m_fRange.bstring[i] = m_fRangeBox->m_bstring[i];
		m_fRange.bstring[i] = '\0';
	}
	if (m_fRangeBox->m_enumber!="")
	{
		for (i=0; i<(m_fRangeBox->m_enumber).GetLength(); i++)
			m_fRange.enumber[i] = m_fRangeBox->m_enumber[i];
		m_fRange.enumber[i]= '\0';	
	}
	if (m_fRangeBox->m_eaddress!="")
	{
		for (i=0; i<(m_fRangeBox->m_eaddress).GetLength(); i++)
			m_fRange.eaddress[i] = m_fRangeBox->m_eaddress[i];
		m_fRange.eaddress[i] = '\0';
	}
	if (m_fRangeBox->m_baddress!="")
	{
		for (i=0; i<(m_fRangeBox->m_baddress).GetLength(); i++)
			m_fRange.baddress[i] = m_fRangeBox->m_baddress[i];
		m_fRange.baddress[i] = '\0';
	}
	m_fRange.bline = m_fRangeBox->m_bline;
	m_fRange.eline = m_fRangeBox->m_eline;
	delete m_fRangeBox;
	m_fRangeBox = NULL;		
}
