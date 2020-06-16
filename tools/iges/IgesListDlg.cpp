/************************************************************************
c
c   FILE NAME: IgesListDlg.cpp
c
c	 CONTAINS: 
c		Functions for the class IgesListDlg 
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       IgesListDlg.cpp , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:12:55               
c
c**********************************************************************
*/
// IgesListDlg.cpp : implementation file
//

#include "stdafx.h"
#include "iges.h"
#include "IgesListDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CIgesListDlg dialog


CIgesListDlg::CIgesListDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CIgesListDlg::IDD, pParent)
{
	m_single = 0;
	m_snum = 0;
	m_list = NULL;
	//{{AFX_DATA_INIT(CIgesListDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}

CIgesListDlg::~CIgesListDlg()
{
	if (m_list!=NULL)
	{
		for (int i=0; i<m_lnum; i++)
		{
			free (m_list[i]);
		}
		free (m_list);
	}

}


void CIgesListDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CIgesListDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CIgesListDlg, CDialog)
	//{{AFX_MSG_MAP(CIgesListDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CIgesListDlg message handlers

void CIgesListDlg::OnOK() 
{
	CListBox* lstbox = (CListBox*)GetDlgItem(IDC_LIST);
	m_snum = lstbox->GetSelCount();
	lstbox->GetSelItems(100, m_select); 
	
	CDialog::OnOK();
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
BOOL CIgesListDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();

	int i;
	GetDlgItem(IDC_LIST_MSG)->SetWindowText(m_msg);
	SetWindowText(m_title);
/*
.....added list
*/
	CListBox* lstbox = (CListBox*)GetDlgItem(IDC_LIST);
	for (i=0; i<m_lnum; i++)
	{
		lstbox->AddString(m_list[i]);
	}
	lstbox->SetCurSel(0);

	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

/*********************************************************************
**    I_FUNCTION     : SetDlgValue(int flag, char *title, char *msg, 
**							   char **list, int num)
**			Set Dialog values
**    PARAMETERS
**       INPUT  :
**          flag: not used now
**			title: list box title
**			msg: prompt message for list box
**			list: initial list
**			num: total number of list item
**       OUTPUT :
**			none
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
int CIgesListDlg::SetDlgValue(int flag, char *title, char *msg, 
							   char **list, int num)
{
	int i,len;
	m_flag = flag;
	m_lnum = num;
	m_title = title;
	m_msg = msg;
	if (num<=0)
		return -1;
	m_list = (char**)malloc(num*sizeof(char*));
	if (m_list==NULL)
		return -1;
	for (i=0; i<num; i++)
	{
		len = strlen(list[i]);
		m_list[i] = (char*)malloc((len+1)*sizeof(char));
		strcpy(m_list[i], list[i]);
	}
	return 1;
}


/*********************************************************************
**    I_FUNCTION     : GetSelect(int *select, int *num)
**
**			Get Current select item
**    PARAMETERS
**       INPUT  :
**          None
**       OUTPUT :
**			*select: array of select item (number)
**			*num:	totakl select number
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CIgesListDlg::GetSelect(int *select, int *num)
{
	int i;
	*num = m_snum;
	for (i=0; i<m_snum; i++)
/*
.....1 base instead of 0 base
*/
//		select[i] = m_select[i];
		select[i] = m_select[i] + 1;
}

