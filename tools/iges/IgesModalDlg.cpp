/************************************************************************
c
c   FILE NAME: IgesModalDlg.cpp
c
c	 CONTAINS: 
c		Functions for the class CIgesModalDlg, which is
c			class for "Name Modals" dialog
c
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       IgesModalDlg.cpp , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:12:55               
c
c**********************************************************************/
// IgesModalDlg.cpp : implementation file
//

#include "stdafx.h"
#include "iges.h"
#include "IgesModalDlg.h"
#include "mdrel.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int iges_config(int );
extern "C" int ul_to_upper(char *);
extern "C" int um_update_rel_label(int, char[3], int);
extern "C" void ncl_check_label_prefix(char *,int,int);

extern "C" char geo_lab[11][7];
extern "C" int lab_flag[11];

/////////////////////////////////////////////////////////////////////////////
// CIgesModalDlg dialog


CIgesModalDlg::CIgesModalDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CIgesModalDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CIgesModalDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void CIgesModalDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CIgesModalDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CIgesModalDlg, CDialog)
	//{{AFX_MSG_MAP(CIgesModalDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CIgesModalDlg message handlers

void CIgesModalDlg::OnCancel() 
{	
	CDialog::OnCancel();
}
/*********************************************************************
**    I_FUNCTION     :  CIgesModalDlg::OnOK() 
**			Callback function for the ACCEPT button on the iges_name_modal
**			Motif form.  Read in info from global array of textbox widgets
**			that are displayed in the form.  This is to find out what
**			abbreviations the user wants for the geometry (ex. pt for points).
**			The abbreviations must be between 2 and 6 characters.  Pass 
**			the global array lab_flag[] and the geometry labels to a
**			function to update the INIT file containing the defaults.
**			Next, modify the global data structure UM_labelmdl.
**			Finally, destroy the form. 
**    PARAMETERS
**       INPUT  :
**				none
**       OUTPUT :
**				none
**    RETURNS      : none
**    SIDE EFFECTS : 
**			Changes the global default naming modal convention by 
**			changing UM_labelmdl 
**    WARNINGS     : none
*********************************************************************/
void CIgesModalDlg::OnOK() 
{
	CString tmpstr;
	char *text;
	int i, status, len;
	static int relnum[] = {UM_POINT_REL, NCL_POINTVEC_REL, UM_LINE_REL,
      NCL_VECTOR_REL, NCL_PLN_REL, UM_CIRCLE_REL, UM_RBSPLCRV_REL, 
		UM_RBSPLSRF_REL, NCL_SHAPE_REL, NCL_MATRIX_REL, NCL_PATERN_REL};
/*
..... Get the abbreviations that the user wants to use by getting the 
..... text from the global array of textbox widgets that are used in the
..... iges_name_modal form.  Check to make sure that they are between 2
..... and 6 characters long.
*/
	for (i = 0; i < 11; i++)
	{
		lab_flag[i] = ((CComboBox*)GetDlgItem(IDC_COMBO1+i))->GetCurSel();
		((CWnd*)GetDlgItem(IDC_EDIT1+i))->GetWindowText(tmpstr);
		len = tmpstr.GetLength();
		if ((lab_flag[i] == 0) && (len != 2))
		{
			MessageBox("Prefixes must be 2 characters!", "Error!", MB_OK);
			return;
		}
		if ((lab_flag[i] == 1) && (len > 6))
		{
			MessageBox("Labels for subscripting must be <= 6 characters!", "Error!", MB_OK);
			return;
		}
		if (len!=0)
		{
			text = tmpstr.GetBuffer(256);
			strcpy(geo_lab[i], text);
			ul_to_upper(geo_lab[i]);
		}
		else
		{
			geo_lab[i][0] = '\0';
		}
		ncl_check_label_prefix(geo_lab[i],lab_flag[i],TRUE);
		um_update_rel_label (relnum[i], geo_lab[i], lab_flag[i]);
	}
/*
.....following MOTIF version, do nothing here
	status = iges_config(1);
	if (status != 0) return;
*/
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
BOOL CIgesModalDlg::OnInitDialog() 
{
	int i;
	CDialog::OnInitDialog();
	for (i = 0; i < 11; i++)
	{
		((CWnd*)GetDlgItem(IDC_EDIT1+i))->SetWindowText(geo_lab[i]);
	}
	for (i = 0; i < 11; i++)
	{
		((CComboBox*)GetDlgItem(IDC_COMBO1+i))->SetCurSel(lab_flag[i]);
	}
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}
