/************************************************************************
c
c   FILE NAME: PworksOptDlg.cpp
c
c	 CONTAINS: 
c		PCompOptDlg::PCompOptDlg()
c		PCompOptDlg::OnInitDialog()
c		PCompOptDlg::OnCancel()
c		PCompOptDlg::OnOk() 
c
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PCompOptDlg.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:14 
c
c**********************************************************************
*/
#include "pwstdafx.h"
#include "postcomp.h"
#include "PCompOptDlg.h"
#include "Pcfunc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// PCompOptDlg dialog


/***********************************************************************
c
c   SUBROUTINE: PCompOptDlg(CWnd* pParent)	
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c			
c   OUTPUT: none
c
c***********************************************************************
*/
PCompOptDlg::PCompOptDlg(CWnd* pParent /*=NULL*/)
	: CDialog(PCompOptDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(PCompOptDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
}


void PCompOptDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(PCompOptDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(PCompOptDlg, CDialog)
	//{{AFX_MSG_MAP(PCompOptDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// PCompOptDlg message handlers

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
BOOL PCompOptDlg::OnInitDialog()
{
	CDialog::OnInitDialog();
/*
.....get options and initialize
*/
	int iopt[5];
	char copt[5][256];
	pc_getoptions(iopt, copt);
/*
......Document
*/
	if (iopt[4]==1)
	{
		CheckDlgButton(ID_OBJ, 1);
	}
	if (strlen(copt[4])==0)
		SetDlgItemText(ID_OPTION_EDIT1, ".obj");
	else
		SetDlgItemText(ID_OPTION_EDIT1,copt[4]);
/*
......PAGE_LEN
*/
	SetDlgItemInt(ID_OPTION_EDIT4,iopt[2]);
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
......OBJECT
*/
	if (iopt[3]==1)
	{
		CheckDlgButton(ID_OBJ, 1);
	}
	if (strlen(copt[3])==0)
		SetDlgItemText(ID_OPTION_EDIT3, ".obj");
	else
		SetDlgItemText(ID_OPTION_EDIT3,copt[3]);

	return TRUE;
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
void PCompOptDlg::OnCancel() 
{
	CDialog::OnCancel();
}

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
void PCompOptDlg::OnOK() 
{
	int number, trans;
/*
.....get options and save
*/
	int iopt[5];
	char copt[5][256];
/*
.....default to value now
*/
	pc_getoptions(iopt, copt);
/*
......Document
*/
	if (((CButton*)GetDlgItem(ID_DOC))->GetCheck())
	{
		iopt[4] = 1;
		GetDlgItemText(ID_OPTION_EDIT1,copt[4],256);
	}
	else
		iopt[4] = 0;
/*
......PAGE_LEN
*/
	number = GetDlgItemInt(ID_OPTION_EDIT4, &trans);
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
		GetDlgItemText(ID_OPTION_EDIT2,copt[1],256);
	}
	else
		iopt[1] = 0;
/*
......OBJECT
*/
	if (((CButton*)GetDlgItem(ID_OBJ))->GetCheck())
	{
		iopt[3] = 1;
		GetDlgItemText(ID_OPTION_EDIT3,copt[3],256);
	}
	else
		iopt[3] = 0;
	pc_saveoptions(iopt, copt);	
	
	CDialog::OnOK();
}
