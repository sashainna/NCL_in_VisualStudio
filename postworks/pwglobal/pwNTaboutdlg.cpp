
/********************************************************************* 
**  NAME:  PwAboutDlg.cpp
**
**			Defines the class behaviors 
**				for the About Dialog
**		CONTAINS: 
**			define and inplement of CAboutDlg dialog
**
**    COPYRIGHT 2002 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			pwNTaboutdlg.cpp , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			03/10/16 , 12:02:53
*********************************************************************/

#include "pwstdafx.h"
#include "PwNTAboutDlg.h"

extern "C" int pw_getver_info(char *,char *, char *, int *, int *);

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
	//{{AFX_DATA_INIT(CAboutDlg)
	//}}AFX_DATA_INIT
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CAboutDlg)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
	//{{AFX_MSG_MAP(CAboutDlg)
		// No message handlers
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
BOOL CAboutDlg::OnInitDialog()
{
	CDialog::OnInitDialog();
	int i,maj,min;
	char program[11], revdate[9], rightdate[10];
	char label[256];
/*
.....set about text according to 'program' 'version' and copy right date
*/
	pw_getver_info(program, revdate, rightdate, &maj, &min);
	program[10] = '\0';
	revdate[8]  = '\0';
	rightdate[9] = '\0';
	sprintf(label, "%s Version %d.%d", program, maj, min); 
	if (sizeof(char *) == 4) strcat(label,"   Windows 32-bit");
	else strcat(label,"   Windows 64-bit");
	GetDlgItem(IDC_ABOUTLABLE1)->SetWindowText(label);
	sprintf(label, "Copyright %s Numerical Control Computer Sciences", rightdate);
	GetDlgItem(IDC_ABOUTLABLE2)->SetWindowText(label);

	sprintf(label, "About %s", program); 
	SetWindowText(label);
	return 0;
}
