//
//     MODULE NAME AND RELEASE LEVEL
//       PWMessageBox.cpp , 24.1
//    DATE AND TIME OF LAST  MODIFICATION
//       09/11/13 , 12:58:20
//
// PWMessageBox.cpp : implementation file
//

#include "pwstdafx.h"
#include "Pwwindef.h"
#include "PWMessageBox.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
extern CWnd *Pw_maindlg;
extern int UU_BATCH;
CWnd *Pw_mainwin = NULL;

/////////////////////////////////////////////////////////////////////////////
// PWMessageBox dialog


PWMessageBox::PWMessageBox(CWnd* pParent /*=NULL*/)
	: CDialog(PWMessageBox::IDD, pParent)
{
	//{{AFX_DATA_INIT(PWMessageBox)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	m_pEditBkBrush = new CBrush(RGB(255, 255, 255));
}

PWMessageBox::~PWMessageBox()
{
	delete m_pEditBkBrush;
}

void PWMessageBox::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(PWMessageBox)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(PWMessageBox, CDialog)
	//{{AFX_MSG_MAP(PWMessageBox)
	ON_WM_CTLCOLOR()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// PWMessageBox message handlers

BOOL PWMessageBox::OnInitDialog() 
{
	CDialog::OnInitDialog();
	Pw_maindlg = this;

	CEdit* edt;
	if (UU_BATCH==0)
		edt = (CEdit *) (Pw_maindlg->GetDlgItem(IDC_POSTWORKS_MESSAGE));
	else
		edt = (CEdit *) (Pw_maindlg->GetDlgItem(IDC_BATCH_MESSAGE));

	edt->LimitText(10000000);

	return FALSE; 
}

HBRUSH PWMessageBox::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor) 
{
	CWnd* pMessageWnd = (CWnd*)GetDlgItem(IDC_BATCH_MESSAGE);
	HWND cmdhnd = pMessageWnd->GetSafeHwnd();
	HWND whnd = pWnd->GetSafeHwnd();
	switch (nCtlColor)
	{
		case CTLCOLOR_EDIT:
/*
......we set Message edit control readonly, so window treat it
......as static label.
*/
		case CTLCOLOR_STATIC:
			if (cmdhnd==whnd)
			{
				pDC->SetTextColor(RGB(20, 20, 255));
				pDC->SetBkColor(RGB(255, 255, 255));
				return (HBRUSH)(m_pEditBkBrush->GetSafeHandle());
			}
			else
				return CDialog::OnCtlColor(pDC, pWnd, nCtlColor);
		default:
			return CDialog::OnCtlColor(pDC, pWnd, nCtlColor);
	}
}

void PWMessageBox::OnCancel() 
{	
	if (this == Pw_mainwin)
		PostQuitMessage (0);
	else
		CDialog::OnCancel() ;
}

void PWMessageBox::OnOK() 
{	
	if (this == Pw_mainwin)
		PostQuitMessage (0);
	else
		CDialog::OnOK() ;
}
