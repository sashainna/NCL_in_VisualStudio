/************************************************************************
c
c   FILE NAME: PMacroOptDlg.cpp
c
c	 CONTAINS: 
c		PMacroOptDlg::PMacroOptDlg()
c		PMacroOptDlg::OnInitDialog()
c		PMacroOptDlg::OnCancel()
c		PMacroOptDlg::OnOk() 
c
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PMacroOptDlg.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:53
c
c**********************************************************************
*/

#include "pwstdafx.h"
#include "pmacro.h"
#include "PMacroOptDlg.h"
#include "pmfunc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// PMacroOptDlg dialog


/***********************************************************************
c
c   SUBROUTINE: PMacroOptDlg(CWnd* pParent)	
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c			
c   OUTPUT: none
c
c***********************************************************************
*/
PMacroOptDlg::PMacroOptDlg(CWnd* pParent /*=NULL*/)
	: CDialog(PMacroOptDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(PMacroOptDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void PMacroOptDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(PMacroOptDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(PMacroOptDlg, CDialog)
	//{{AFX_MSG_MAP(PMacroOptDlg)
	ON_BN_CLICKED(ID_OK, OnOk)
	ON_BN_CLICKED(ID_CANCEL, OnCancel)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// PMacroOptDlg message handlers

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
void PMacroOptDlg::OnOk() 
{
	int number, len;
	BOOL trans;
/*
.....get options and save
*/
	int iopt[21];
	char copt[20][256];
/*
.....default to value now
*/
	pm_getoptions(iopt, copt);
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
......IDENT
*/
	if (((CButton*)GetDlgItem(ID_IDENT))->GetCheck())
		iopt[5] = 1;
	else
		iopt[5] = 0;
/*
......LISTING
*/
	if (((CButton*)GetDlgItem(ID_LIST))->GetCheck())
	{
		iopt[1] = 1;
		GetDlgItemText(ID_OPTION_EDIT2,copt[1],256);
	}
	else
		iopt[1] = 0;
/*
......MACHINE
*/
	number = GetDlgItemInt(ID_OPTION_EDIT3, &trans);
	if (trans)
	{
		iopt[19] = number;
		iopt[4] = 1;
		iopt[20] = 1;
	}
/*
......OBJECT
*/
	if (((CButton*)GetDlgItem(ID_OBJ))->GetCheck())
	{
		iopt[3] = 1;
		GetDlgItemText(ID_OPTION_EDIT4,copt[3],256);
	}
	else
		iopt[3] = 0;
/*
......POST
*/
	if (((CButton*)GetDlgItem(ID_POST))->GetCheck())
		iopt[6] = 1;
	else
		iopt[6] = 0;
/*
......FILL
*/
	if (((CButton*)GetDlgItem(ID_FILL))->GetCheck())
	{
		GetDlgItemText(ID_OPTION_EDIT6,copt[11],256);
		iopt[11] = 1;
	}
	else
		iopt[11] = 0;
	pm_saveoptions(iopt, copt);	
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
void PMacroOptDlg::OnCancel() 
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
BOOL PMacroOptDlg::OnInitDialog()
{
	CDialog::OnInitDialog();
/*
.....get options and initialize
*/
	int iopt[21];
	char copt[20][256];

	pm_getoptions(iopt, copt);
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
......IDENT
*/
	if (iopt[5] == 1)
		CheckDlgButton(ID_IDENT, 1);
/*
......LISTING
*/
	if (iopt[1]==1)
	{
		CheckDlgButton(ID_LIST, 1);
	}
	if (strlen(copt[1])==0)
		SetDlgItemText(ID_OPTION_EDIT2, ".lis");
	else
		SetDlgItemText(ID_OPTION_EDIT2,copt[1]);
/*
......MACHINE
*/
	if (iopt[4]==1)
		SetDlgItemInt(ID_OPTION_EDIT3,iopt[10]);
/*
......OBJECT
*/
	if (iopt[3]==1)
	{
		CheckDlgButton(ID_OBJ, 1);
	}
	if (strlen(copt[3])==0)
		SetDlgItemText(ID_OPTION_EDIT4, ".obj");
	else
		SetDlgItemText(ID_OPTION_EDIT4,copt[3]);
/*
......POST
*/
	if (iopt[6]==1)
	{
		CheckDlgButton(ID_POST, 1);
	}
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
