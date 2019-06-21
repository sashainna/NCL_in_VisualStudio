/************************************************************************
c
c   FILE NAME: PWconvOptDlg.cpp
c
c	 CONTAINS: 
c		PWconvOptDlg::PMacroOptDlg()
c		PWconvOptDlg::OnInitDialog()
c		PWconvOptDlg::OnCancel()
c		PWconvOptDlg::OnOk() 
c
c     COPYRIGHT 2011 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PWconvOptDlg.cpp , 25.2
c     DATE AND TIME OF LAST  MODIFICATION
c        09/26/17 , 11:48:40
c
c**********************************************************************
*/

#include "pwstdafx.h"
#include "pwconv.h"
#include "PWconvOptDlg.h"
#include "Pvfunc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// PWconvOptDlg dialog


/***********************************************************************
c
c   SUBROUTINE: PWconvOptDlg(CWnd* pParent)	
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c			
c   OUTPUT: none
c
c***********************************************************************
*/
PWconvOptDlg::PWconvOptDlg(CWnd* pParent /*=NULL*/)
	: CDialog(PWconvOptDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(PWconvOptDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void PWconvOptDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(PWconvOptDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(PWconvOptDlg, CDialog)
	//{{AFX_MSG_MAP(PWconvOptDlg)
	ON_BN_CLICKED(ID_OK, OnOk)
	ON_BN_CLICKED(ID_CANCEL, OnCancel)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// PWconvOptDlg message handlers

/***********************************************************************
c
c   SUBROUTINE:  OnOK
c
c   FUNCTION:  This function called when pushed 'Ok' button 
c				the dialog
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void PWconvOptDlg::OnOk() 
{
	int number, len;
	BOOL trans;
/*
.....get options and save
*/
	int iopt[20];
	char copt[20][256];
/*
.....default to value now
*/
	pv_getoptions(iopt, copt);
/*
......option pulldown list
......CLFILE
*/
	iopt[7] = ((CComboBox*)GetDlgItem(ID_CLFILE))->GetCurSel();
/*
.....Apt source line length
*/
	iopt[8] = GetDlgItemInt(ID_OPTION_EDIT1,&trans);
/*
......APT Source error
*/
	if (((CButton*)GetDlgItem(ID_APTERR))->GetCheck())
		iopt[10] = 1;
	else
		iopt[10] = 0;
/*
......PAGE_LEN
*/
	number = GetDlgItemInt(ID_OPTION_EDIT5, &trans);
	if (trans)
	{
		if (number>0)
			iopt[2] = number;
	}
/*
......LISTING
*/
	if (((CButton*)GetDlgItem(ID_LIST))->GetCheck())
	{
		iopt[1] = 1;
		GetDlgItemText(ID_OPTION_EDIT2,copt[1],80);
	}
	else
		iopt[1] = 0;
/*
......OUTPUT
*/
	if (((CButton*)GetDlgItem(ID_OBJ))->GetCheck())
	{
		iopt[3] = 1;
		GetDlgItemText(ID_OPTION_EDIT4,copt[3],80);
	}
	else
		iopt[3] = 0;
/*
......FILL
*/
	if (((CButton*)GetDlgItem(ID_FILL))->GetCheck())
	{
		GetDlgItemText(ID_OPTION_EDIT6,copt[11],80);
		iopt[11] = 1;
	}
	else
		iopt[11] = 0;
	pv_saveoptions(iopt, copt);	
	CDialog::OnOK();
}

/***********************************************************************
c
c   SUBROUTINE:  OnCancel
c
c   FUNCTION:  This function called when pushed 'Cancel' button 
c				the dialog
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void PWconvOptDlg::OnCancel() 
{	
	CDialog::OnCancel();
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
BOOL PWconvOptDlg::OnInitDialog()
{
	CDialog::OnInitDialog();
/*
.....get options and initialize
*/
	int iopt[20];
	char copt[20][256];

	pv_getoptions(iopt, copt);
/*
......option pulldown list
 ......CLFILE
*/
	CComboBox* cmbbx = (CComboBox*)GetDlgItem(ID_CLFILE);
	cmbbx->AddString("Automatic");
	cmbbx->AddString("NCL Binary Clfile");
	cmbbx->AddString("APT Source Textual File");
	cmbbx->AddString("Catia V4 Binary Clfile");
	cmbbx->AddString("APT Source Textual, UG circles");
	cmbbx->AddString("Catia V5 Binary Clfile");
	cmbbx->AddString("MasterCam NCI File");
	cmbbx->SetCurSel(iopt[7]);
	SetDlgItemInt(ID_OPTION_EDIT1, iopt[8]);
/*
......APT Source error
*/
	if (iopt[10] == 1)
		CheckDlgButton(ID_APTERR, 1);
/*
......PAGE_LEN
*/
	SetDlgItemInt(ID_OPTION_EDIT5,iopt[2]);
/*
......LISTING
*/
	if (iopt[1]==1)
	{
		CheckDlgButton(ID_LIST, 1);
	}
//	if (strlen(copt[0])==0)
	if (strlen(copt[1])==0)
		SetDlgItemText(ID_OPTION_EDIT2, ".lis");
	else
//		SetDlgItemText(ID_OPTION_EDIT2,copt[0]);
		SetDlgItemText(ID_OPTION_EDIT2,copt[1]);
/*
......OUTPUT
*/
	if (iopt[3]==1)
	{
		CheckDlgButton(ID_OBJ, 1);
	}
	if (strlen(copt[0])==0)
		SetDlgItemText(ID_OPTION_EDIT4, ".cl");
	else
		SetDlgItemText(ID_OPTION_EDIT4,copt[3]);
/*
......FILL
*/
	if (iopt[11]==1)
	{
		CheckDlgButton(ID_FILL, 1);
		SetDlgItemText(ID_OPTION_EDIT6,copt[11]);
	}
	return TRUE;
}
