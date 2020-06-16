/************************************************************************
c
c   FILE NAME: ModListDlg.cpp
c
c	 CONTAINS: 
c		Functions for CModListDlg class 
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       ModListDlg.cpp , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       10/12/15 , 17:23:55
c
c**********************************************************************
*/
// ModListDlg.cpp : implementation file
//

#include "toolibstdafx.h"
#include "toolib.h"
#include "ModListDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CModListDlg dialog
/***********************************************************************
c
c   FUNCTION: CModListDlg(CWnd* pParent)
c
c              Constructor of class CModListDlg
c
c   INPUT:  pParent: parent window
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
CModListDlg::CModListDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CModListDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CModListDlg)
	m_cutparm = TRUE;
	m_dispparm = TRUE;
	m_heading = TRUE;
	m_ltool = TRUE;
	m_pcut = TRUE;
	m_tnumdes = TRUE;
	m_optcom = TRUE;
	m_paramslb = TRUE;
	m_psymbol = TRUE;
	m_pshank = TRUE;
	m_pholder = TRUE;
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
void CModListDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CModListDlg)
	DDX_Check(pDX, IDC_CUTPARM, m_cutparm);
	DDX_Check(pDX, IDC_DISP, m_dispparm);
	DDX_Check(pDX, IDC_HEADING, m_heading);
	DDX_Check(pDX, IDC_LTOOL, m_ltool);
	DDX_Check(pDX, IDC_PCUT, m_pcut);
	DDX_Check(pDX, IDC_TOOLNUM, m_tnumdes);
	DDX_Check(pDX, IDC_OPT_COMMAND, m_optcom);
	DDX_Check(pDX, IDC_PARAM_LABEL, m_paramslb);
	DDX_Check(pDX, IDC_PSYMBOL, m_psymbol);
	DDX_Check(pDX, IDC_PSHANK, m_pshank);
	DDX_Check(pDX, IDC_PHOLDER, m_pholder);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CModListDlg, CDialog)
	//{{AFX_MSG_MAP(CModListDlg)
		// NOTE: the ClassWizard will add message map macros here
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CModListDlg message handlers
