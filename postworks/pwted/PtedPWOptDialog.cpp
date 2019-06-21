/************************************************************************
c
c   FILE NAME: PtedPWOptDialog.cpp
c
c	 CONTAINS: 
c	 all PtedPWOptDialog class: a dialog with all the postworks options
c			Implementation functions
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			PtedPWOptDialog.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c			09/11/13 , 12:59:27
c
c**********************************************************************
*/
// PtedPWOptDialog.cpp : implementation file
//

#include "pwenv.h"
#include "pwstdafx.h"
#include "Pted.h"
#include "PtedPWOptDialog.h"
#include <direct.h>

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
extern "C" char Pted_localdir[UX_MAX_PATH];

/////////////////////////////////////////////////////////////////////////////
// PtedPWOptDialog dialog

/***********************************************************************
c
c   SUBROUTINE: PtedPWOptDialog	
c
c   FUNCTION:  constructor
c
c   INPUT:  
c			
c   OUTPUT: none
c
c***********************************************************************
*/
PtedPWOptDialog::PtedPWOptDialog(CWnd* pParent /*=NULL*/, char *input)
	: CDialog(PtedPWOptDialog::IDD, pParent)
{
	char *indx, tmp[UX_MAX_PATH];
	//{{AFX_DATA_INIT(PtedPWOptDialog)
		// NOTE: the ClassWizard will add member initialization here
		m_machine = "";
		if (input!=NULL)
		{
			strcpy(tmp, input);
			indx = strrchr(tmp, '_');
			if (indx!=NULL)
			{
				strcpy(tmp, &(indx[1]));
				indx = strchr(tmp, '.');
				if (indx!=NULL)
					*indx = '\0';
				m_machine = tmp;
			}
		}
		m_adjfile = "";
		m_llen = 72;
		m_warn = 1;
		m_error = 1;
		m_fatal = 1;
		m_cwarn = 9999;
		m_cerror = 9999;
		m_cfatal = 1;
		m_option = "";
		m_plen = 60;
		m_cprint = ".pr1";
		m_print = 1;
	//}}AFX_DATA_INIT
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
void PtedPWOptDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(PtedPWOptDialog)
		DDX_Text(pDX, IDC_EDIT1, m_adjfile);
		DDX_Text(pDX, IDC_EDIT3, m_cwarn);
		DDX_Text(pDX, IDC_EDIT4, m_cerror);
		DDX_Text(pDX, IDC_EDIT5, m_cfatal);
		DDX_Text(pDX, IDC_EDIT7, m_machine);
		DDX_Text(pDX, IDC_EDIT8, m_option);
		DDX_Text(pDX, IDC_EDIT9, m_plen);
		DDX_Text(pDX, IDC_EDIT10, m_cprint);
		DDX_Check(pDX, IDC_CHECK1, m_warn);
		DDX_Check(pDX, IDC_CHECK2, m_error);
		DDX_Check(pDX, IDC_CHECK3, m_fatal);
		DDX_Check(pDX, IDC_CHECK6, m_print);
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}

/***********************************************************************
c
c   MESSAGE_MAP: callback descriptions
c
c***********************************************************************
*/
BEGIN_MESSAGE_MAP(PtedPWOptDialog, CDialog)
	//{{AFX_MSG_MAP(PtedPWOptDialog)
	ON_BN_CLICKED(IDC_BROWSE, OnBrowse)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// PtedPWOptDialog message handlers
BOOL PtedPWOptDialog::OnInitDialog()
{
	CDialog::OnInitDialog();
	if (m_cwarn<=0)
	{
		CheckDlgButton(IDC_CHECK1,0);
		SetDlgItemText(IDC_EDIT3,"");
	}
	else
		CheckDlgButton(IDC_CHECK1,1);

	if (m_cerror<=0)
	{
		CheckDlgButton(IDC_CHECK2,0);
		SetDlgItemText(IDC_EDIT4,"");
	}
	else
		CheckDlgButton(IDC_CHECK2,1);

	if (m_cfatal<=0)
	{
		CheckDlgButton(IDC_CHECK3,0);
		SetDlgItemText(IDC_EDIT5,"");
	}
	else
		CheckDlgButton(IDC_CHECK3,1);

	return 0;
}
/***********************************************************************
c
c   SUBROUTINE:  OnBrowse() 
c
c   FUNCTION:  This function is callback of "Browse..." button
c
c   INPUT:  None
c			
c   OUTPUT: None
c
c***********************************************************************
*/
void PtedPWOptDialog::OnBrowse() 
{
	DWORD dwFlags;
	CString FileName;
	dwFlags = OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT;

	LPCTSTR filter = "All Files (*.*)|*.*||";
	CFileDialog *filedialog = new CFileDialog(TRUE, NULL, m_adjfile, dwFlags,
			filter, this);
	if (filedialog->DoModal()==IDCANCEL)
	{
		delete filedialog;
		goto done;;
	}

	FileName = filedialog->GetPathName();
	GetDlgItem(IDC_EDIT1)->SetWindowText(FileName);
	delete filedialog;
done:;
	_chdir(Pted_localdir);
}


void PtedPWOptDialog::OnOK() 
{	
	CDialog::OnOK();
}
