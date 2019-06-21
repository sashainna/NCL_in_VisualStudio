/************************************************************************
c
c   FILE NAME: LicenseLstDlg.cpp
c		implementation file for license list box
c	 CONTAINS: 
c		CLicenseLstDlg::CLicenseLstDlg()
c		CLicenseLstDlg::~CLicenseLstDlg()
c		CLicenseLstDlg::OnCreate() 
c		CLicenseLstDlg::OnSelectall() 
c		CLicenseLstDlg::OnCancel() 
c		CLicenseLstDlg::OnInitDialog() 
c
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        LicenseLstDlg.cpp , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:13 
c
c**********************************************************************
*/
#include "pwenv.h"
#include "stdafx.h"
#include "nccs_auth.h"
#include "LicenseLstDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int sav_clr_spos();
extern "C" int getsearch(char list[10][132], int *err);
extern "C" int reset_spos();
extern "C" int openlicfile(char *fname);
extern "C" int addlicence(char rst[10][132]);
extern "C" int closelicfile();
/////////////////////////////////////////////////////////////////////////////
// CLicenseLstDlg dialog


/***********************************************************************
c
c   SUBROUTINE: CLicenseLstDlg(CWnd* pParent)	
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CLicenseLstDlg::CLicenseLstDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CLicenseLstDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CLicenseLstDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	for (int i=0; i<1000; i++)
	{
		for (int j=0; j<10; j++)
			m_liclist[i][j] = NULL;
	}
}

/***********************************************************************
c
c   SUBROUTINE: ~CLicenseLstDlg()	
c
c   FUNCTION:  Destructor
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CLicenseLstDlg::~CLicenseLstDlg()
{
	for (int i=0; i<1000; i++)
	{
		for (int j=0; j<10; j++)
			if (m_liclist[i][j] != NULL)
				free (m_liclist[i][j]);
	}
}

void CLicenseLstDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CLicenseLstDlg)
	DDX_Control(pDX, IDD_MATCH_LISTS, m_LicenseList);
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CLicenseLstDlg, CDialog)
	//{{AFX_MSG_MAP(CLicenseLstDlg)
	ON_BN_CLICKED(IDOK, OnCreate)
	ON_BN_CLICKED(IDC_SELECTALL, OnSelectall)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLicenseLstDlg message handlers

/***********************************************************************
c
c   SUBROUTINE: OnCreate() 
c
c   FUNCTION:  called when "Create" button is pushed 
c		it will a license file
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CLicenseLstDlg::OnCreate() 
{
	int len,nItem,j;
	char rst[10][132];
	char fname[UX_MAX_PATH];
	char* temp;
	static int once = 1;
	if (once==0)
		return;
	once = 0;
	POSITION pos = m_LicenseList.GetFirstSelectedItemPosition();
	if (pos != NULL)
	{
		LPCTSTR filter = "Text files (*.lic)|*.lic|All Files (*.*)|*.*||";		
		DWORD dwFlags = OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT;
		CFileDialog *filedlg = new CFileDialog(TRUE, "License", NULL, dwFlags,
									filter, this);
		if (filedlg->DoModal()==IDCANCEL)
			return;
		CString FileName = filedlg->GetPathName();
		len = FileName.GetLength();
		temp = FileName.GetBuffer(len);
		strcpy(fname, temp);
		delete filedlg;	
/*
.....create and open license file
*/
		openlicfile(fname);
		while (pos)
		{
			nItem = m_LicenseList.GetNextSelectedItem(pos);
			for (j=0; j<10;j++)
			{
				strcpy(rst[j], m_liclist[nItem][j]);
			}
/*
.....add a record into license file
*/
			addlicence(rst);
		}
/*
.....close license file
*/
		closelicfile();
	}
	once = 1;
}

/***********************************************************************
c
c   SUBROUTINE: OnCancel() 
c
c   FUNCTION:  called when "cancel" button is pushed 
c		it will exit this dialog box
c
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CLicenseLstDlg::OnCancel() 
{
	CDialog::OnCancel();
}

/***********************************************************************
c
c   SUBROUTINE: OnSelectall() 
c
c   FUNCTION:  called when "select all" button is pushed 
c		it will select all list items
c   INPUT:  none
c			
c   OUTPUT: none
c
c***********************************************************************
*/
void CLicenseLstDlg::OnSelectall() 
{
	int i, count, indx;
	count = m_LicenseList.GetItemCount();
	for (i=-1; i<count-1; i++)
	{
		indx = m_LicenseList.GetNextItem(i, LVNI_ALL) ;
		m_LicenseList.SetItemState(indx, LVIS_SELECTED | LVIS_FOCUSED, LVIS_SELECTED | LVIS_FOCUSED);	
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog
c
c   FUNCTION:  This function initialize 
c				the dialog
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
BOOL CLicenseLstDlg::OnInitDialog() 
{
	char rst[10][132];
	int i, j, stat,len;
	CDialog::OnInitDialog();
	
	CRect rectTemplate;
	GetDlgItem(IDD_MATCH_LISTS)->GetWindowRect(rectTemplate);
	int wid = (rectTemplate.Width()-GetSystemMetrics(SM_CXVSCROLL))/4 - 1;
	m_LicenseList.InsertColumn( 0, "Company",	LVCFMT_LEFT, wid);
	m_LicenseList.InsertColumn( 1, "Hardware",	LVCFMT_LEFT, wid);
	m_LicenseList.InsertColumn( 2, "Software",	LVCFMT_LEFT, wid);
	m_LicenseList.InsertColumn( 3, "System",	LVCFMT_LEFT, wid);
	stat = 0;
	i = 0;
	sav_clr_spos();
	getsearch(rst,&stat);
/*
.....save the data in the local member
*/
	for (j=0; j<10;j++)
	{
		len = strlen (rst[j]);
		m_liclist[i][j] = (char *)malloc((len+1)*sizeof(char));
		strcpy(m_liclist[i][j], rst[j]);
	}
	if (stat!=0)
	{
		MessageBox("No data matched in database!", "Create License Info", MB_OK);
	}
	while (stat==0)
	{
		m_LicenseList.InsertItem(i, rst[0]);
		m_LicenseList.SetItemText(i,1,rst[1]);
		m_LicenseList.SetItemText(i,2,rst[2]);
		m_LicenseList.SetItemText(i,3,rst[7]);
		getsearch(rst, &stat);
		i++;
		for (j=0; j<10;j++)
		{
			len = strlen (rst[j]);
			m_liclist[i][j] = (char *)malloc((len+1)*sizeof(char));
			strcpy(m_liclist[i][j], rst[j]);
		}
	}
	reset_spos();

	DWORD dwStyle = m_LicenseList.SendMessage(LVM_GETEXTENDEDLISTVIEWSTYLE,0,0);
	dwStyle |= LVS_EX_FULLROWSELECT;
	m_LicenseList.SendMessage(LVM_SETEXTENDEDLISTVIEWSTYLE,0,dwStyle);
	return TRUE;  // return TRUE unless you set the focus to a control
}
