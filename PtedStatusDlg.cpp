/************************************************************************
c
c   FILE NAME: PtedStatusDlg.cpp
c
c	 CONTAINS: 
c	 all CPtedStatusDlg class: a dialog display Pted status
c			Implementation functions
c
c     COPYRIGHT 2003 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			PtedStatusDlg.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c			09/11/13 , 12:59:27
c
c**********************************************************************
*/
// PtedStatusDlg.cpp : implementation file
//

#include "pwenv.h"
#include "pwstdafx.h"
#include "Pted.h"
#include "PtedStatusDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/***********************************************************************
c
c   SUBROUTINE: Get_Type_str(int type, char *typestr)
c
c   FUNCTION:  Get type extension
c
c   INPUT:  type: file type
c			
c   OUTPUT: typestr: file extension
c
c***********************************************************************
*/
static void Get_Type_str(int type, char *typestr)
{
	switch (type)
	{
	case 1:
		strcpy(typestr, "CL Text File");
		break;
	case 2:
		strcpy(typestr, "Control Data File");
		break;
	case 3:
		strcpy(typestr, "CL Binary File");
		break;
	case 4:
		strcpy(typestr, "APT Source File");
		break;
	case 5:
		strcpy(typestr, "Simulation File");
		break;
	case 6:
		strcpy(typestr, "Cutter File");
		break;
	default:
		strcpy(typestr, "Unknown File Type");
		break;
	}
	return;
}

/////////////////////////////////////////////////////////////////////////////
// CPtedStatusDlg dialog

/***********************************************************************
c
c   SUBROUTINE: CPtedStatusDlg
c
c   FUNCTION:  constructor
c
c   INPUT:  
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CPtedStatusDlg::CPtedStatusDlg(CWnd* pParent, Ptd_File_status *files, int file_num, char *input, char* output)
	: CDialog(CPtedStatusDlg::IDD, pParent)
{
	m_hIcon = AfxGetApp()->LoadIcon(IDI_PTED_ICON);

	int i;
	if (input==NULL)
		strcpy(m_input_mdf, "PWORKS_0.MDF");
	else
		strcpy(m_input_mdf, input);
	if (output==NULL)
		strcpy(m_output_mdf, "PWORKS_0.MDF");
	else
		strcpy(m_output_mdf, output);

	m_filenum = file_num;

	if (m_filenum==0)
	{
		m_files = NULL;
		return;
	}
	m_files = (Ptd_File_status*)malloc(file_num*sizeof(Ptd_File_status));

	for (i=0; i<file_num; i++)
	{
		m_files[i].wtype = files[i].wtype;
		m_files[i].type = files[i].type;
		strcpy(m_files[i].filename,files[i].filename);
	}
}

CPtedStatusDlg::~CPtedStatusDlg()
{
	if ((m_filenum!=0)&&(m_files!=NULL))
		free(m_files);
}

/***********************************************************************
c
c   MESSAGE_MAP: callback descriptions
c
c***********************************************************************
*/
BEGIN_MESSAGE_MAP(CPtedStatusDlg, CDialog)
	//{{AFX_MSG_MAP(CPtedStatusDlg)
	ON_WM_CREATE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog
c
c   FUNCTION:  This function initialize window text
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
BOOL CPtedStatusDlg::OnInitDialog()
{
	char typestr[40], tmp[UX_MAX_PATH+20];
	CDialog::OnInitDialog();
	SetIcon(m_hIcon, TRUE);			// Set big icon
/*
.....resize window
*/
	int i, x1, y1, wid, hgt, row_hgt;
	CWnd *w1, *w2, *w3;
	CRect windowRect, windowRectb;
	x1 = 13;
	y1 = 18;
	hgt = y1 + 30*m_filenum + 20*2 + 15 + 30;
	wid = 260 + 10;
	CRect temp (0,0, wid, hgt);
	MapDialogRect(&temp);
	MoveWindow(&temp);
/*
.....adjust windows in the dialog and set label
*/
	GetDlgItem(IDC_MAIN_LABEL1)->SetWindowText("Main:");
	GetDlgItem(IDC_MAIN_LABEL2)->SetWindowText(m_files[0].filename);
	Get_Type_str(m_files[0].type, typestr);
	GetDlgItem(IDC_MAIN_LABEL3)->SetWindowText(typestr);
	
	w1 = GetDlgItem(IDC_MAIN_LABEL1);
	w2 = GetDlgItem(IDC_MAIN_LABEL2);
	w3 = GetDlgItem(IDC_MAIN_LABEL3);

	w1->GetWindowRect(&windowRect);
	row_hgt = windowRect.Height();
	CRect temp0(0,0, x1, y1);
	MapDialogRect(&temp0);
	windowRect.top = temp0.Height();
	windowRect.bottom = windowRect.top + row_hgt;
	w1->MoveWindow(&windowRect);

	w2->GetWindowRect(&windowRect);
	windowRect.top = temp0.Height();
	windowRect.bottom = windowRect.top + row_hgt;
	w2->MoveWindow(&windowRect);
	
	CRect temp00(0,0, x1, y1 + 10);
	MapDialogRect(&temp00);
	w3->GetWindowRect(&windowRect);
	windowRect.top = temp00.Height();
	windowRect.bottom = windowRect.top + row_hgt;
	w3->MoveWindow(&windowRect);

	for (i=1; i<m_filenum; i++)
	{
		w1 = GetDlgItem(IDC_CHILD1_LABEL1 + 3*(i - 1));
		w2 = GetDlgItem(IDC_CHILD1_LABEL2 + 3*(i - 1));
		w3 = GetDlgItem(IDC_CHILD1_LABEL3 + 3*(i - 1));

		w1->GetWindowRect(&windowRect);
		row_hgt = windowRect.Height();
		CRect temp2(0,0, x1, y1 + 30*i);
		MapDialogRect(&temp2);

		windowRect.top = temp2.Height();
		windowRect.bottom = windowRect.top + row_hgt;
		w1->MoveWindow(&windowRect);

		w2->GetWindowRect(&windowRect);
		windowRect.top = temp2.Height();
		windowRect.bottom = windowRect.top + row_hgt;
		w2->MoveWindow(&windowRect);
	
		CRect temp3(0,0, x1, y1 + 30*i+10);
		MapDialogRect(&temp3);
		w3->GetWindowRect(&windowRect);
		windowRect.top = temp3.Height();
		windowRect.bottom = windowRect.top + row_hgt;
		w3->MoveWindow(&windowRect);

		sprintf(tmp, "Child%d:", i);
		w1->SetWindowText(tmp);
		w2->SetWindowText(m_files[i].filename);
		Get_Type_str(m_files[i].type, typestr);
		w3->SetWindowText(typestr);
	}
	CRect temp4(0,0, x1, y1 + 30*m_filenum + 7);
	MapDialogRect(&temp4);

	w1 = GetDlgItem(IDC_FRAME2);
	w1->GetWindowRect(&windowRect);
	row_hgt = windowRect.Height();
	windowRect.top = temp4.Height();
	windowRect.bottom = windowRect.top + row_hgt;
	w1->MoveWindow(&windowRect);

	CRect temp5(0,0, x1, y1 + 30*m_filenum + 20);
	MapDialogRect(&temp5);
	w1 = GetDlgItem(IDC_INPUTMDF_LABEL);
	w1->GetWindowRect(&windowRect);
	row_hgt = windowRect.Height();
	windowRect.top = temp5.Height();
	windowRect.bottom = windowRect.top + row_hgt;
	w1->MoveWindow(&windowRect);
	sprintf(tmp, "Input MDF: %s", m_input_mdf);
	w1->SetWindowText(tmp);

	CRect temp6(0,0, x1, y1 + 30*m_filenum + 30);
	MapDialogRect(&temp6);
	w2 = GetDlgItem(IDC_OUTPUTMDF_LABEL);
	w2->GetWindowRect(&windowRect);
	windowRect.top = temp6.Height();
	windowRect.bottom = windowRect.top + row_hgt;
	w2->MoveWindow(&windowRect);
	sprintf(tmp, "Output MDF: %s", m_output_mdf);
	w2->SetWindowText(tmp);

	CRect temp8(0,0, x1, y1 + 30*m_filenum + 30 + 20);
	MapDialogRect(&temp8);
	w2 = GetDlgItem(IDOK);
	w2->GetWindowRect(&windowRectb);

	hgt = windowRectb.Height();
	windowRectb.top = temp8.Height();
	windowRectb.bottom = windowRectb.top + hgt;
	w2->MoveWindow(&windowRectb);

	CRect temp7(7,5, x1, 12 + 30*m_filenum);
	MapDialogRect(&temp7);

	w1 = GetDlgItem(IDC_FRAME1);
	w1->GetWindowRect(&windowRect);
	windowRect.top = temp7.top;
	windowRect.bottom = windowRect.top + temp7.Height();
	w1->MoveWindow(&windowRect);

	for (i=m_filenum-1; i<18; i++)
	{
		w1 = GetDlgItem(IDC_CHILD1_LABEL1 + 3*i);
		w2 = GetDlgItem(IDC_CHILD1_LABEL2 + 3*i);
		w3 = GetDlgItem(IDC_CHILD1_LABEL3 + 3*i);
		w1->ShowWindow(SW_HIDE);
		w2->ShowWindow(SW_HIDE);
		w3->ShowWindow(SW_HIDE);
	}
	CenterWindow();
	return 0;
}  
