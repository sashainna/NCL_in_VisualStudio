/********************************************************************* 
**  NAME:  wsntopt.cpp
**
**			Native WinNT edit menu area functions
**			implementation of CAreaEditDialog class functions
**	CONTAINS: CAreaEditDialog class functions
**			all functions declared in wsntopt.h
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntarea.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:19      
*********************************************************************/

#include "wsntstdafx.h"
#include "wsntarea.h"
#include "wsntcfunc.h"

/////////////////////////////////////////////////////////////////////////////
// CAreaEditDialog

/***********************************************************************
**
**   FUNCTION: CAreaEditDialog
**
**		Constructor of class CAreaEditDialog
**
**   INPUT:  none
**
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
CAreaEditDialog::CAreaEditDialog() : CDialog(CAreaEditDialog::IDD)
{
	m_cols = 1;
	m_rows = 1;
	m_dir = 1;
	m_width = 50;
	m_height = 50;
	m_areaname = "";
	//{{AFX_DATA_INIT(CAreaEditDialog)
	//}}AFX_DATA_INIT
}


BEGIN_MESSAGE_MAP(CAreaEditDialog, CDialog)
	//{{AFX_MSG_MAP(CAreaEditDialog)
	ON_COMMAND(IDC_ADD, OnAdd)
	ON_COMMAND(IDC_DELETE, OnDelete)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

void CAreaEditDialog::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAreaEditDialog)
	DDX_Text(pDX, IDC_EDIT1, m_rows);
	DDX_Text(pDX, IDC_EDIT2, m_cols);
	DDX_Text(pDX, IDC_EDIT3, m_width);
	DDX_Text(pDX, IDC_EDIT4, m_height);
	DDX_Text(pDX, IDC_AREA_NAME, m_areaname);
	DDX_CBIndex(pDX, IDC_DIRECTION, m_dir);
	//}}AFX_DATA_MAP
}

/***********************************************************************
**
**   FUNCTION: OnAdd() 
**
**       called when "Add" button is clicked 
**
**   INPUT:  None
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CAreaEditDialog::OnAdd() 
{
	char *temp =  m_areaname.GetBuffer( 80 );
	uw_ntadd_menuarea(temp, m_dir+1, m_rows, m_cols, m_width, m_height);
}

/***********************************************************************
**
**   FUNCTION: OnDelete() 
**
**       called when "Delete" button is clicked 
**
**   INPUT:  None
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CAreaEditDialog::OnDelete() 
{
	char *temp =  m_areaname.GetBuffer( 80 );
	uw_ntdelete_menuarea(temp);
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

BOOL CAreaEditDialog::OnInitDialog() 
{
	CDialog::OnInitDialog();
	CComboBox * comb = (CComboBox*)GetDlgItem(IDC_AREA_NAME);

	char area_lst[100][80];
	int num_area, i;
	uw_getmarea_list(area_lst, &num_area);
	for (i=0; i<num_area; i++)	
		comb->AddString(area_lst[i]);
	return TRUE;
}

