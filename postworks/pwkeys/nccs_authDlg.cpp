/************************************************************************
c
c   FILE NAME: Nccs_authDlg.cpp
c
c	 CONTAINS: 
c		CNccs_authDlg::CNccs_authDlg()
c		CNccs_authDlg::OnInitDialog()
c		CNccs_authDlg::OnSysCommand()
c		CNccs_authDlg::OnPaint() 
c		CNccs_authDlg::OnQueryDragIcon()
c		CNccs_authDlg::OnFileExit() 
c		CNccs_authDlg::OnAdd()
c		CNccs_authDlg::OnDelete() 
c		CNccs_authDlg::OnSearch() 
c		CNccs_authDlg::OnClear() 
c		CNccs_authDlg::OnFileAdministrator
c		CNccs_authDlg::OnShowSearch
c		CNccs_authDlg::OnFileCreatebatch
c		CNccs_authDlg::OnLicenseCreatelicensefile
c		CNccs_authDlg::OnLicenseCalculatepassword
c		CNccs_authDlg::showsuper()
c		CNccs_authDlg::OnSize( UINT nType, int cx, int cy )
c		CNccs_authDlg::SizeDialogItem(int cx, int cy )
c		CNccs_authDlg::OnTsearch() 
c		CNccs_authDlg::OnCheck1()
c		CNccs_authDlg::OnCheck2()
c		CNccs_authDlg::OnCheck3()
c		CNccs_authDlg::OnCheck4()
c		CNccs_authDlg::OnCheck5()
c		CNccs_authDlg::OnCheck6()
c		CNccs_authDlg::OnCheck7()
c		CNccs_authDlg::OnCheck8()
c		CNccs_authDlg::OnCheck9()
c		extern "C" showsuper()
c
c
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        nccs_authDlg.cpp , 23.1
c     DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:14 
c
c**********************************************************************
*/

#include "pwenv.h"
#include "stdafx.h"
#include <direct.h>
#include "nccs_auth.h"
#include "nccs_authDlg.h"
#include "LicenseLstDlg.h"
#include "PasswdDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int addrecc (char msg[256], int *nc, int *ierr);
extern "C" int delrecc (char msg[256], int *nc, int *ierr);
extern "C" int searchc (char msg[256], int *nc, int *ierr);
extern "C" int purgc (char msg[256], int *nc, int *ierr);
extern "C" int pwdpasc (int *ierr);
extern "C" int savinput(char buf[10][132], int parm[10], int *flag);
extern "C" int clrrec();
extern "C" int onfexit();
extern "C" int onfsuper(char msg[256], int *nc, int *ierr);
extern "C" int create_batfile(char *fname);
extern "C" int reset_batch();
extern "C" int batchlic(char *batfile, int *num, int *ierr);
extern "C" int getsparm(char buf[10][132], int parm[10]);
extern "C" int savsparm(int *parm, char buf[132], int *item);
extern "C" int checkpass(int *ierr);
/////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////
// CNccs_authDlg dialog

/***********************************************************************
c
c   SUBROUTINE: CNccs_authDlg(CWnd* pParent)	
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CNccs_authDlg::CNccs_authDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CNccs_authDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CNccs_authDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
	m_sizechg = 0;
	HINSTANCE hInst = AfxFindResourceHandle(MAKEINTRESOURCE(IDR_ACCEL), RT_ACCELERATOR);
	m_accel = ::LoadAccelerators(hInst, MAKEINTRESOURCE(IDR_ACCEL));
	m_issuper = 0;
}

void CNccs_authDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CNccs_authDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CNccs_authDlg, CDialog)
	//{{AFX_MSG_MAP(CNccs_authDlg)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_BN_CLICKED(ID_ADD, OnAdd)
	ON_BN_CLICKED(ID_CLEAR, OnClear)
	ON_BN_CLICKED(ID_DELETE, OnDelete)
	ON_COMMAND(ID_EXIT, OnExit)
	ON_COMMAND(ID_FILE_LOADLICENSE, OnFileLoadlicense)
	ON_COMMAND(ID_LICENSE_CALCULATEPASSWORD, OnLicenseCalculatepassword)
	ON_COMMAND(ID_LICENSE_CREATELICENSEFILE, OnLicenseCreatelicensefile)
	ON_BN_CLICKED(ID_SEARCH, OnSearch)
	ON_COMMAND(ID_FILE_ADMINISTRATOR, OnFileAdministrator)
	ON_COMMAND(ID_FILE_CREATEBATCH, OnFileCreatebatch)
	ON_COMMAND(ID_SHOW_SEARCH, OnShowSearch)
	ON_WM_SIZE()
	ON_COMMAND(ID_EDIT_PURGE, OnEditPurge)
	ON_BN_CLICKED(IDC_CHECK1, OnCheck1)
	ON_BN_CLICKED(IDC_CHECK2, OnCheck2)
	ON_BN_CLICKED(IDC_CHECK3, OnCheck3)
	ON_BN_CLICKED(IDC_CHECK4, OnCheck4)
	ON_BN_CLICKED(IDC_CHECK5, OnCheck5)
	ON_BN_CLICKED(IDC_CHECK6, OnCheck6)
	ON_BN_CLICKED(IDC_CHECK7, OnCheck7)
	ON_BN_CLICKED(IDC_CHECK8, OnCheck8)
	ON_BN_CLICKED(IDC_CHECK9, OnCheck9)
	ON_BN_CLICKED(IDC_CHECK10, OnCheck10)
	ON_COMMAND(ID_TSEARCH, OnTsearch)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNccs_authDlg message handlers

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
BOOL CNccs_authDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon
/*
.....Hide 'Password' and 'License' Field and resize window	
*/
	CRect rect, rect1;
	GetDlgItem(ID_PASSWD_LBL)->GetWindowRect(&rect1);
	GetWindowRect(&rect);
	m_origrect = rect;
/*
......leave 22 unit for active bar area height
*/
	CRect temp (0,0, 22, 22);
	MapDialogRect(&temp);
	
	rect1.bottom += temp.Height() - 8;

	rect.bottom = rect1.bottom;
	m_sizechg = 1;

	GetDlgItem(IDC_CHECK9)->ShowWindow(SW_HIDE);
	GetDlgItem(ID_PASSWD_LBL)->ShowWindow(SW_HIDE);
	GetDlgItem(ID_EDIT9)->ShowWindow(SW_HIDE);
	GetDlgItem(IDC_CHECK10)->ShowWindow(SW_HIDE);
	GetDlgItem(ID_LICENSE_LBL)->ShowWindow(SW_HIDE);
	GetDlgItem(ID_EDIT10)->ShowWindow(SW_HIDE);
	MoveWindow(rect);
/*
......default button to "ADD"
*/
	SetDefID(ID_ADD);
	return TRUE;  // return TRUE  unless you set the focus to a control
}
/***********************************************************************
c
c   SUBROUTINE:  OnSize( UINT nType, int cx, int cy )
c
c   FUNCTION:  Called when user sizing the dialog box
c
c   INPUT:  nType: size flag
c		cx, cy: dialog box size
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_authDlg::OnSize( UINT nType, int cx, int cy )
{
	CWnd::OnSize(nType, cx, cy );
	if (m_sizechg)
		SizeDialogItem(cx,cy);
}

/***********************************************************************
c
c   SUBROUTINE:  SizeDialogItem(int cx, int cy )
c
c   FUNCTION:  size dialog item according to dialog size
c
c   INPUT:  cx, cy: dialog box size
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_authDlg::SizeDialogItem(int cx, int cy )
{
	CRect windowRect, windowRect2, boxrect;
	GetClientRect(windowRect);
	GetClientRect(windowRect2);
/*
......leave 22 unit for active bar area height
*/
	CRect temp (0,0, 22, 22);
	MapDialogRect(&temp);
	
	windowRect.bottom -= temp.Height();
	boxrect = windowRect;
	boxrect.top += 10;
	boxrect.left += 10;
	boxrect.right -= 10;
	boxrect.bottom -= 5;

	CWnd* pChildWnd = (CWnd*)GetDlgItem(ID_BOX);
	if (pChildWnd!=NULL)
		pChildWnd->MoveWindow(boxrect);
	else
		return;

	int butlen = (cx - 25)/4;
	windowRect2.top = windowRect.bottom + 5;
	windowRect2.bottom -= 5;
	windowRect2.right -= 5;
	windowRect2.left = windowRect2.right - butlen;

	pChildWnd = (CWnd*)GetDlgItem(ID_SEARCH);
	if (pChildWnd!=NULL)
		pChildWnd->MoveWindow(windowRect2);

	windowRect2.right = windowRect2.left - 5;
	windowRect2.left = windowRect2.right - butlen;
	pChildWnd = (CWnd*)GetDlgItem(ID_CLEAR);
	if (pChildWnd!=NULL)
		pChildWnd->MoveWindow(windowRect2);

	windowRect2.right = windowRect2.left - 5;
	windowRect2.left = windowRect2.right - butlen;
	pChildWnd = (CWnd*)GetDlgItem(ID_DELETE);
	if (pChildWnd!=NULL)
		pChildWnd->MoveWindow(windowRect2);

	windowRect2.right = windowRect2.left - 5;
	windowRect2.left = windowRect2.right - butlen;
	pChildWnd = (CWnd*)GetDlgItem(ID_ADD);
	if (pChildWnd!=NULL)
		pChildWnd->MoveWindow(windowRect2);
}

/***********************************************************************
c
c   SUBROUTINE:  OnSysCommand
c
c   FUNCTION:  Callback for system menu of
c				the dialog
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_authDlg::OnSysCommand(UINT nID, LPARAM lParam)
{
	CDialog::OnSysCommand(nID, lParam);
}

/***********************************************************************
c
c   SUBROUTINE:  OnPaint
c
c   FUNCTION:  Called when dialog  need repaint
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_authDlg::OnPaint() 
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, (WPARAM) dc.GetSafeHdc(), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}

// The system calls this to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR CNccs_authDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

/***********************************************************************
c
c   SUBROUTINE:  Get_user_input
c
c   FUNCTION:  Get user input from window
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
int CNccs_authDlg::Get_user_input (char buf[10][132], int parm[10])
{
	int empty,i;
	CString input;
	char *tmp;
	empty = 1;

	for (i=0; i<10;i++)
		buf[i][0] = '\0';
	GetDlgItemText(ID_EDIT1, input);
	input.TrimLeft();
	input.TrimRight();
	if (input.GetLength()>0)
	{
		input.MakeUpper();
		empty = 0;
		tmp = input.GetBuffer(132);
		strcpy(buf[0], tmp);
	}
	GetDlgItemText(ID_EDIT2, input);
	input.TrimLeft();
	input.TrimRight();
	if (input.GetLength()>0)
	{
		empty = 0;
		input.MakeUpper();
		tmp = input.GetBuffer(132);
		strcpy(buf[1], tmp);
	}
	GetDlgItemText(ID_EDIT3, input);
	input.TrimLeft();
	input.TrimRight();
	if (input.GetLength()>0)
	{
		empty = 0;
		input.MakeUpper();
		tmp = input.GetBuffer(132);
		strcpy(buf[2], tmp);
	}
	GetDlgItemText(ID_EDIT4, input);
	input.TrimLeft();
	input.TrimRight();
	if (input.GetLength()>0)
	{
		empty = 0;
		input.MakeUpper();
		tmp = input.GetBuffer(132);
		strcpy(buf[3], tmp);
	}
	GetDlgItemText(ID_EDIT5, input);
	input.TrimLeft();
	input.TrimRight();
	if (input.GetLength()>0)
	{
		empty = 0;
		input.MakeUpper();
		tmp = input.GetBuffer(132);
		strcpy(buf[4], tmp);
	}
	GetDlgItemText(ID_EDIT6, input);
	input.TrimLeft();
	input.TrimRight();
	if (input.GetLength()>0)
	{
		empty = 0;
		input.MakeUpper();
		tmp = input.GetBuffer(132);
		strcpy(buf[5], tmp);
	}
	GetDlgItemText(ID_EDIT7, input);
	input.TrimLeft();
	input.TrimRight();
	if (input.GetLength()>0)
	{
		empty = 0;
		input.MakeUpper();
		tmp = input.GetBuffer(132);
		strcpy(buf[6], tmp);
	}
	GetDlgItemText(ID_EDIT8, input);
	input.TrimLeft();
	input.TrimRight();
	if (input.GetLength()>0)
	{
		empty = 0;
		input.MakeUpper();
		tmp = input.GetBuffer(132);
		strcpy(buf[7], tmp);
	}
	GetDlgItemText(ID_EDIT9, input);
	input.TrimLeft();
	input.TrimRight();
	if (input.GetLength()>0)
	{
		empty = 0;
		input.MakeUpper();
		tmp = input.GetBuffer(132);
		strcpy(buf[8], tmp);
	}
	GetDlgItemText(ID_EDIT10, input);
	input.TrimLeft();
	input.TrimRight();
	if (input.GetLength()>0)
	{
		empty = 0;
		input.MakeUpper();
		tmp = input.GetBuffer(132);
		strcpy(buf[9], tmp);
	}
	parm[0] = ((CButton*)GetDlgItem(IDC_CHECK1))->GetCheck();
	parm[1] = ((CButton*)GetDlgItem(IDC_CHECK2))->GetCheck();
	parm[2] = ((CButton*)GetDlgItem(IDC_CHECK3))->GetCheck();
	parm[3] = ((CButton*)GetDlgItem(IDC_CHECK4))->GetCheck();
	parm[4] = ((CButton*)GetDlgItem(IDC_CHECK5))->GetCheck();
	parm[5] = ((CButton*)GetDlgItem(IDC_CHECK6))->GetCheck();
	parm[6] = ((CButton*)GetDlgItem(IDC_CHECK7))->GetCheck();
	parm[7] = ((CButton*)GetDlgItem(IDC_CHECK8))->GetCheck();
	parm[8] = ((CButton*)GetDlgItem(IDC_CHECK9))->GetCheck();
	parm[9] = ((CButton*)GetDlgItem(IDC_CHECK10))->GetCheck();
	if (empty)
	{
		return -1;
	}
	return 0;
}

/***********************************************************************
c
c   SUBROUTINE:  OnAdd
c
c   FUNCTION:  Callback for 'Add' button
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_authDlg::OnAdd() 
{
	int flag, nc, ierr;
	char msg[256];
	char buf[10][132]; /* same as FRMBUF defined on menu.inc */
	int parm[10];
/*
.....must be administrator
*/
	OnFileAdministrator();

	if (m_issuper!=1)
		return;

	int status = Get_user_input (buf, parm);
	if (status!=0)
	{
		MessageBox("Empty Input!", "NCCS_LICENSE Warning Message", MB_OK);
		return;
	}
	flag = 1;
	savinput(buf, parm, &flag);
/*
......Make sure password matches
*/
	checkpass(&ierr);
	if (ierr!=0)
		return;
	addrecc (msg, &nc, &ierr);
	if (ierr != 0)
	{
		msg[nc] = '\0';
		MessageBox(msg, "NCCS_LICENSE Error Message",MB_OK);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnClear
c
c   FUNCTION:  Callback for 'Clear' button
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_authDlg::OnClear() 
{
	SetDlgItemText(ID_EDIT1,"");
	SetDlgItemText(ID_EDIT2,"");	
	SetDlgItemText(ID_EDIT3,"");	
	SetDlgItemText(ID_EDIT4,"");	
	SetDlgItemText(ID_EDIT5,"");	
	SetDlgItemText(ID_EDIT6,"");	
	SetDlgItemText(ID_EDIT7,"");	
	SetDlgItemText(ID_EDIT8,"");	
	SetDlgItemText(ID_EDIT9,"");
	SetDlgItemText(ID_EDIT10,"");
	clrrec();
}

/***********************************************************************
c
c   SUBROUTINE:  OnDelete
c
c   FUNCTION:  Callback for 'Delete' button
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_authDlg::OnDelete() 
{
	int flag, nc, ierr;
	char msg[256];
	char buf[10][132]; /* same as FRMBUF defined on menu.inc */
	int parm[10];
/*
.....must be administrator
*/
	OnFileAdministrator();

	if (m_issuper!=1)
		return;
	int status = Get_user_input (buf, parm);
	if (status!=0)
	{
		MessageBox("Empty Input!", "NCCS_LICENSE Warning Message", MB_OK);
		return;
	}
	flag = 1;
	savinput(buf, parm, &flag);
	delrecc (msg, &nc, &ierr);
	if (ierr != 0)
	{
		msg[nc] = '\0';
		MessageBox(msg, "NCCS_LICENSE Error Message",MB_OK);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnExit
c
c   FUNCTION:  Called when "exit" menu is picked
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_authDlg::OnExit() 
{
	CDialog::OnCancel();	
	onfexit();
}

/***********************************************************************
c
c   SUBROUTINE:  OnFileLoadlicense
c
c   FUNCTION:  Called when "Load License file" menu is picked
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_authDlg::OnFileLoadlicense() 
{
/*
.....must be administrator to load batch file because the load 
.....batch file will overwrite the current database
*/
	char msg[256];
	int nc, ierr;

	OnFileAdministrator();

	if (m_issuper!=1)
		return;
/*
.....save current directory in case filebrowser change it
*/
	char save_dir[UX_MAX_PATH];
	GetCurrentDirectory(UX_MAX_PATH, save_dir);

	int i;
	LPCTSTR filter = "Batch files (*.bat)|*.bat|All Files (*.*)|*.*||";		
	DWORD dwFlags = OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT;

	CFileDialog *filedlg = new CFileDialog(TRUE, "Nccs_license", NULL, dwFlags,
			filter, this);
	if (filedlg->DoModal()==IDCANCEL)
		return;
	_chdir(save_dir);

	CString FileName = filedlg->GetPathName();
	char batfile[UX_MAX_PATH];
	ierr = 0;

	int len = FileName.GetLength();
	delete filedlg;		

	for (i=0;i<len;i++)
		batfile[i] = FileName[i];
	for (i=len;i<UX_MAX_PATH;i++)
		batfile[i] = ' ';
	reset_batch();	
	batchlic(batfile, &len, &ierr);
}

/***********************************************************************
c
c   SUBROUTINE:  OnLicenseCalculatepassword
c
c   FUNCTION:  Called when "Calculate Password" menu is picked
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_authDlg::OnLicenseCalculatepassword() 
{
	int flag, ierr;
	char msg[256];
	char buf[10][132]; /* same as FRMBUF defined on menu.inc */
	int parm[10];
/*
.....must be administrator
*/
	OnFileAdministrator();

	if (m_issuper!=1)
		return;

	int status = Get_user_input (buf, parm);
	if (status!=0)
	{
		MessageBox("Empty Input!", "NCCS_LICENSE Warning Message", MB_OK);
		return;
	}
	flag = 1;
	savinput(buf, parm, &flag);
	pwdpasc (&ierr);
	if (ierr==0) return;
	if (ierr == 1)
		strcpy(msg, "Calculate password failed with 'Company name' field!");
	else if (ierr == 2)
		strcpy(msg, "Calculate password failed with 'Hardware' field!");
	else if (ierr == 3)
		strcpy(msg, "Calculate password failed with 'Software' field!");
	else if (ierr == 4)
		strcpy(msg, "Calculate password failed with 'Options' field!");
	else if (ierr == 5)
		strcpy(msg, "Calculate password failed with 'users' field!");
	else if (ierr == 6)
		strcpy(msg, "Calculate password failed with 'Termination' field!");
	else if (ierr == 7)
		strcpy(msg, "Calculate password failed with 'Version' field!");
	else if (ierr == 8)
		strcpy(msg, "Calculate password failed with 'System' field!");

	MessageBox(msg, "NCCS_LICENSE Error Message",MB_OK);
}

/***********************************************************************
c
c   SUBROUTINE:  OnLicenseCreatelicensefile
c
c   FUNCTION:  Called when "Create license file" menu is picked
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_authDlg::OnLicenseCreatelicensefile() 
{
/*
.....must be administrator
*/
	OnFileAdministrator();

	if (m_issuper!=1)
		return;
	CLicenseLstDlg *ldlg = new CLicenseLstDlg(this);
	ldlg->DoModal();
}

/***********************************************************************
c
c   SUBROUTINE:  OnSearch
c
c   FUNCTION:  Callback for 'Search' button
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_authDlg::OnSearch() 
{
	int nc, ierr;
	char msg[256];
	char buf[10][132]; /* same as FRMBUF defined on menu.inc */
	int parm[10];
	int status = Get_user_input (buf, parm);
	searchc (msg, &nc, &ierr);
	if (ierr != 0)
	{
		msg[nc] = '\0';
		MessageBox(msg, "NCCS_LICENSE Error Message",MB_OK);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  OnFileAdministrator
c
c   FUNCTION:  Called when "Administrator" menu is picked
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_authDlg::OnFileAdministrator() 
{
	char msg[256];
	int nc, ierr;
	CPasswdDlg *dlg;
	
	if (m_issuper)
		return;

	dlg = new CPasswdDlg(this);
showdialog:
	if (dlg->DoModal()==IDCANCEL)
	{
		delete dlg;
		return;
	}
	if (dlg->m_passwd!="0123456789")
	{
		MessageBox("Password incorrect!", "NCCS_LICENSE Error Message",MB_OK);
		goto showdialog;
	}
	delete dlg;
adm:;
	onfsuper(msg, &nc, &ierr);
	if (ierr != 0)
	{
		msg[nc] = '\0';
		MessageBox(msg, "NCCS_LICENSE Error Message",MB_OK);
	}	
	m_issuper = 1;

	CNccs_authApp *app = (CNccs_authApp*)AfxGetApp();
	CNccs_authDlg *MainDlg = (CNccs_authDlg*)(app->GetMainWnd());	
	((CEdit*)(MainDlg->GetDlgItem(ID_EDIT1)))->SetFocus();
}

/***********************************************************************
c
c   SUBROUTINE:  OnFileCreatebatch
c
c   FUNCTION:  Called when "Create Batch" menu is picked
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_authDlg::OnFileCreatebatch() 
{
/*
.....must be administrator to create batch file
*/
	int nc, ierr;
	OnFileAdministrator();

	if (m_issuper!=1)
		return;
	int len;
	char fname[UX_MAX_PATH];
	char* temp;
	LPCTSTR filter = "Batch files (*.bat)|*.bat|All Files (*.*)|*.*||";		
	DWORD dwFlags = OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT;
	CFileDialog *filedlg = new CFileDialog(TRUE, "License", "*.bat", dwFlags,
									filter, this);
	if (filedlg->DoModal()==IDCANCEL)
		return;
	CString FileName = filedlg->GetPathName();
	len = FileName.GetLength();
	temp = FileName.GetBuffer(len);
	strcpy(fname, temp);
	delete filedlg;	
	create_batfile(fname);
}

/***********************************************************************
c
c   SUBROUTINE:  OnShowSearch
c
c   FUNCTION:  Called when "Show Search" menu is picked
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_authDlg::OnShowSearch() 
{
	char buf[10][132];
	int parm[10];
	char label[10000], disp[10000];
	label[0] = '\0';
	getsparm(buf, parm);

	if (parm[0]==1)
	{
		SetDlgItemText(ID_EDIT1, buf[0]);
	}
	else
		SetDlgItemText(ID_EDIT1, "");
	if (parm[1]==1)
	{
		SetDlgItemText(ID_EDIT2, buf[1]);
	}
	else
		SetDlgItemText(ID_EDIT2, "");
	if (parm[2]==1)
	{
		SetDlgItemText(ID_EDIT3, buf[2]);
	}
	else
		SetDlgItemText(ID_EDIT3, "");
	if (parm[3]==1)
	{
		SetDlgItemText(ID_EDIT4, buf[3]);
	}
	else
		SetDlgItemText(ID_EDIT4, "");
	if (parm[4]==1)
	{
		SetDlgItemText(ID_EDIT5, buf[4]);
	}
	else
		SetDlgItemText(ID_EDIT5, "");
	if (parm[5]==1)
	{
		SetDlgItemText(ID_EDIT6, buf[5]);
	}
	else
		SetDlgItemText(ID_EDIT6, "");
	if (parm[6]==1)
	{
		SetDlgItemText(ID_EDIT7, buf[6]);
	}
	else
		SetDlgItemText(ID_EDIT7, "");
	if (parm[7]==1)
	{
		SetDlgItemText(ID_EDIT8, buf[7]);
	}
	else
		SetDlgItemText(ID_EDIT8, "");
//	if ((parm[8]==1)&&(m_issuper))
	if (parm[8]==1)
	{
		SetDlgItemText(ID_EDIT9, buf[8]);
	}
	else
		SetDlgItemText(ID_EDIT9, "");
//	if ((parm[9]==1)&&(m_issuper))
	if (parm[9]==1)
	{
		SetDlgItemText(ID_EDIT10, buf[9]);
	}
	else
		SetDlgItemText(ID_EDIT10, "");
}
/***********************************************************************
c
c   SUBROUTINE:  showsuper
c
c   FUNCTION:  This function shows 'Password' and 'License' field
c				for super user
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_authDlg::showsuper()
{
	CRect rect;
	GetWindowRect(&rect);
	rect.bottom = rect.top + (m_origrect.bottom - m_origrect.top);
	rect.right = rect.left + (m_origrect.right - m_origrect.left);
	MoveWindow(rect);
	GetDlgItem(IDC_CHECK9)->ShowWindow(SW_SHOW);
	GetDlgItem(ID_PASSWD_LBL)->ShowWindow(SW_SHOW);
	GetDlgItem(ID_EDIT9)->ShowWindow(SW_SHOW);
	GetDlgItem(IDC_CHECK10)->ShowWindow(SW_SHOW);
	GetDlgItem(ID_LICENSE_LBL)->ShowWindow(SW_SHOW);
	GetDlgItem(ID_EDIT10)->ShowWindow(SW_SHOW);
}
/***********************************************************************
c
c   SUBROUTINE:  disfldc(char buf[10][132], int *ist, int *ien)
c
c   FUNCTION:  display fields
c
c   INPUT:  ist: start field to display
c			ien: end field to display
c
c   OUTPUT: nine
c
c***********************************************************************
*/
extern "C" int disfldc(char buf[10][132], int *ist, int *ien)
{
	CNccs_authApp *app = (CNccs_authApp*)AfxGetApp();
	CNccs_authDlg *MainDlg = (CNccs_authDlg*)(app->GetMainWnd());	
/*
.....remove trailling space
*/
	CString tmpstr;
	char *tmp;
	for (int i=0; i<10;i++)
	{
		buf[i][131] = '\0';
		tmpstr = buf[i];
		tmpstr.TrimRight();
		tmp = tmpstr.GetBuffer(132);
		strcpy(buf[i], tmp);
	}
	if ((*ien==10)&&(*ist<=10))
	{
		MainDlg->SetDlgItemText(ID_EDIT10,buf[9]);	
	}
	if ((*ien>=9)&&(*ist<=9))
	{
		MainDlg->SetDlgItemText(ID_EDIT9,buf[8]);	
	}
	if ((*ien>=8)&&(*ist<=8))
	{
		MainDlg->SetDlgItemText(ID_EDIT8,buf[7]);	
	}
	if ((*ien>=7)&&(*ist<=7))
	{
		MainDlg->SetDlgItemText(ID_EDIT7,buf[6]);	
	}
	if ((*ien>=6)&&(*ist<=6))
	{
		MainDlg->SetDlgItemText(ID_EDIT6,buf[5]);	
	}
	if ((*ien>=5)&&(*ist<=5))
	{
		MainDlg->SetDlgItemText(ID_EDIT5,buf[4]);	
	}
	if ((*ien>=4)&&(*ist<=4))
	{
		MainDlg->SetDlgItemText(ID_EDIT4,buf[3]);	
	}
	if ((*ien>=3)&&(*ist<=3))
	{
		MainDlg->SetDlgItemText(ID_EDIT3,buf[2]);	
	}
	if ((*ien>=2)&&(*ist<=2))
	{
		MainDlg->SetDlgItemText(ID_EDIT2,buf[1]);	
	}
	if ((*ien>=1)&&(*ist<=1))
	{
		MainDlg->SetDlgItemText(ID_EDIT1,buf[0]);
	}
	if (*ist==1)
	{
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT1)))->SetFocus();
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT1)))->SetSel(0, -1);
	}
	if (*ist==2)
	{
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT2)))->SetFocus();
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT2)))->SetSel(0, -1);
	}
	if (*ist==3)
	{
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT3)))->SetFocus();
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT3)))->SetSel(0, -1);
	}
	if (*ist==4)
	{
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT4)))->SetFocus();
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT4)))->SetSel(0, -1);
	}
	if (*ist==5)
	{
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT5)))->SetFocus();
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT5)))->SetSel(0, -1);
	}
	if (*ist==6)
	{
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT6)))->SetFocus();
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT6)))->SetSel(0, -1);
	}
	if (*ist==7)
	{
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT7)))->SetFocus();
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT7)))->SetSel(0, -1);
	}
	if (*ist==8)
	{
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT8)))->SetFocus();
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT8)))->SetSel(0, -1);
	}
	if (*ist==9)
	{
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT9)))->SetFocus();
		((CEdit*)(MainDlg->GetDlgItem(ID_EDIT9)))->SetSel(0, -1);
	}
	return 0;
}
/***********************************************************************
c
c   SUBROUTINE:  showsuper()
c
c   FUNCTION:  Shows 'Password' and 'License' field for superuser
c
c   INPUT:  None
c
c   OUTPUT: None
c
c***********************************************************************
*/
extern "C" void showsuper()
{
	CNccs_authApp *app = (CNccs_authApp*)AfxGetApp();
	CNccs_authDlg *MainDlg = (CNccs_authDlg*)(app->GetMainWnd());	

	MainDlg->showsuper();	
}

/***********************************************************************
c
c   SUBROUTINE:  OnEditPurge
c
c   FUNCTION:  Called when "Purge" menu is picked
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_authDlg::OnEditPurge() 
{
	int nc, ierr;
	char msg[256];
/*
.....must be administrator
*/
	OnFileAdministrator();

	if (m_issuper!=1)
		return;
	purgc (msg, &nc, &ierr);
	if (ierr != 0)
	{
		msg[nc] = '\0';
		MessageBox(msg, "NCCS_LICENSE Error Message",MB_OK);
	}
}

/***********************************************************************
c
c   SUBROUTINE:  PreTranslateMessage
c
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
BOOL CNccs_authDlg::PreTranslateMessage(MSG* msg)
{
	HWND hWnd = (HWND)*this; 
	if (TranslateAccelerator(hWnd, m_accel, msg))
		return TRUE;
	else if (IsDialogMessage( msg ) ) 
		return TRUE;
	else
		return CWnd::PreTranslateMessage( msg );
}

/***********************************************************************
c
c   SUBROUTINE:  OnCheck1()
c				Callback for check box #1. This function save the search
c					parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_authDlg::OnCheck1()
{
	int parm, item;
	char buf[132];
	buf[0] = '\0';
	item = 1;
	parm = ((CButton*)GetDlgItem(IDC_CHECK1))->GetCheck();
	GetDlgItemText(ID_EDIT1, buf, 131);
	savsparm(&parm, buf, &item);
}

/***********************************************************************
c
c   SUBROUTINE:  OnCheck2()
c				Callback for check box #1. This function save the search
c					parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_authDlg::OnCheck2()
{
	int parm, item;
	char buf[132];
	buf[0] = '\0';
	item = 2;
	parm = ((CButton*)GetDlgItem(IDC_CHECK2))->GetCheck();
	GetDlgItemText(ID_EDIT2, buf, 131);
	savsparm(&parm, buf, &item);
}

/***********************************************************************
c
c   SUBROUTINE:  OnCheck3()
c				Callback for check box #1. This function save the search
c					parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_authDlg::OnCheck3()
{
	int parm, item;
	char buf[132];
	buf[0] = '\0';
	item = 3;
	parm = ((CButton*)GetDlgItem(IDC_CHECK3))->GetCheck();
	GetDlgItemText(ID_EDIT3, buf, 131);
	savsparm(&parm, buf, &item);
}
/***********************************************************************
c
c   SUBROUTINE:  OnCheck4()
c				Callback for check box #1. This function save the search
c					parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_authDlg::OnCheck4()
{
	int parm, item;
	char buf[132];
	buf[0] = '\0';
	item = 4;
	parm = ((CButton*)GetDlgItem(IDC_CHECK4))->GetCheck();
	GetDlgItemText(ID_EDIT4, buf, 131);
	savsparm(&parm, buf, &item);
}

/***********************************************************************
c
c   SUBROUTINE:  OnCheck5()
c				Callback for check box #1. This function save the search
c					parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_authDlg::OnCheck5()
{
	int parm, item;
	char buf[132];
	buf[0] = '\0';
	item = 5;
	parm = ((CButton*)GetDlgItem(IDC_CHECK5))->GetCheck();
	GetDlgItemText(ID_EDIT5, buf, 131);
	savsparm(&parm, buf, &item);
}

/***********************************************************************
c
c   SUBROUTINE:  OnCheck6()
c				Callback for check box #1. This function save the search
c					parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_authDlg::OnCheck6()
{
	int parm, item;
	char buf[132];
	buf[0] = '\0';
	item = 6;
	parm = ((CButton*)GetDlgItem(IDC_CHECK6))->GetCheck();
	GetDlgItemText(ID_EDIT6, buf, 131);
	savsparm(&parm, buf, &item);
}

/***********************************************************************
c
c   SUBROUTINE:  OnCheck7()
c				Callback for check box #1. This function save the search
c					parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_authDlg::OnCheck7()
{
	int parm, item;
	char buf[132];
	buf[0] = '\0';
	item = 7;
	parm = ((CButton*)GetDlgItem(IDC_CHECK7))->GetCheck();
	GetDlgItemText(ID_EDIT7, buf, 131);
	savsparm(&parm, buf, &item);
}

/***********************************************************************
c
c   SUBROUTINE:  OnCheck8()
c				Callback for check box #1. This function save the search
c					parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_authDlg::OnCheck8()
{
	int parm, item;
	char buf[132];
	buf[0] = '\0';
	item = 8;
	parm = ((CButton*)GetDlgItem(IDC_CHECK8))->GetCheck();
	GetDlgItemText(ID_EDIT8, buf, 131);
	savsparm(&parm, buf, &item);
}

/***********************************************************************
c
c   SUBROUTINE:  OnCheck9()
c				Callback for check box #1. This function save the search
c					parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_authDlg::OnCheck9()
{
	int parm, item;
	char buf[132];
	buf[0] = '\0';
	item = 9;
	parm = ((CButton*)GetDlgItem(IDC_CHECK9))->GetCheck();
	GetDlgItemText(ID_EDIT9, buf, 131);
	savsparm(&parm, buf, &item);
}

/***********************************************************************
c
c   SUBROUTINE:  OnCheck10()
c				Callback for check box #1. This function save the search
c					parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_authDlg::OnCheck10()
{
	int parm, item;
	char buf[132];
	buf[0] = '\0';
	item = 10;
	parm = ((CButton*)GetDlgItem(IDC_CHECK10))->GetCheck();
	GetDlgItemText(ID_EDIT10, buf, 131);
	savsparm(&parm, buf, &item);
}

/***********************************************************************
c
c   SUBROUTINE:  OnTsearch()
c        Callback for "Toggle Check". This function will toggle the check box
c         next to the current active field
c   FUNCTION:  
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_authDlg::OnTsearch() 
{
	CWnd *wnd = GetFocus();
	CWnd *tmp;
	int parm;

	tmp = GetDlgItem(ID_EDIT1);
	if (wnd==tmp)
	{
		parm = ((CButton*)GetDlgItem(IDC_CHECK1))->GetCheck();
		if (parm)
			((CButton*)GetDlgItem(IDC_CHECK1))->SetCheck(0);
		else
			((CButton*)GetDlgItem(IDC_CHECK1))->SetCheck(1);
		OnCheck1();
		return;
	}

	tmp = GetDlgItem(ID_EDIT2);
	if (wnd==tmp)
	{
		parm = ((CButton*)GetDlgItem(IDC_CHECK2))->GetCheck();
		if (parm)
			((CButton*)GetDlgItem(IDC_CHECK2))->SetCheck(0);
		else
			((CButton*)GetDlgItem(IDC_CHECK2))->SetCheck(1);
		OnCheck2();
		return;
	}

	tmp = GetDlgItem(ID_EDIT3);
	if (wnd==tmp)
	{
		parm = ((CButton*)GetDlgItem(IDC_CHECK3))->GetCheck();
		if (parm)
			((CButton*)GetDlgItem(IDC_CHECK3))->SetCheck(0);
		else
			((CButton*)GetDlgItem(IDC_CHECK3))->SetCheck(1);
		OnCheck3();
		return;
	}

	tmp = GetDlgItem(ID_EDIT4);
	if (wnd==tmp)
	{
		parm = ((CButton*)GetDlgItem(IDC_CHECK4))->GetCheck();
		if (parm)
			((CButton*)GetDlgItem(IDC_CHECK4))->SetCheck(0);
		else
			((CButton*)GetDlgItem(IDC_CHECK4))->SetCheck(1);
		OnCheck4();
		return;
	}

	tmp = GetDlgItem(ID_EDIT5);
	if (wnd==tmp)
	{
		parm = ((CButton*)GetDlgItem(IDC_CHECK5))->GetCheck();
		if (parm)
			((CButton*)GetDlgItem(IDC_CHECK5))->SetCheck(0);
		else
			((CButton*)GetDlgItem(IDC_CHECK5))->SetCheck(1);
		OnCheck5();
		return;
	}

	tmp = GetDlgItem(ID_EDIT6);
	if (wnd==tmp)
	{
		parm = ((CButton*)GetDlgItem(IDC_CHECK6))->GetCheck();
		if (parm)
			((CButton*)GetDlgItem(IDC_CHECK6))->SetCheck(0);
		else
			((CButton*)GetDlgItem(IDC_CHECK6))->SetCheck(1);
		OnCheck6();
		return;
	}

	tmp = GetDlgItem(ID_EDIT7);
	if (wnd==tmp)
	{
		parm = ((CButton*)GetDlgItem(IDC_CHECK7))->GetCheck();
		if (parm)
			((CButton*)GetDlgItem(IDC_CHECK7))->SetCheck(0);
		else
			((CButton*)GetDlgItem(IDC_CHECK7))->SetCheck(1);
		OnCheck7();
		return;
	}

	tmp = GetDlgItem(ID_EDIT8);
	if (wnd==tmp)
	{
		parm = ((CButton*)GetDlgItem(IDC_CHECK8))->GetCheck();
		if (parm)
			((CButton*)GetDlgItem(IDC_CHECK8))->SetCheck(0);
		else
			((CButton*)GetDlgItem(IDC_CHECK8))->SetCheck(1);
		OnCheck8();
		return;
	}

	tmp = GetDlgItem(ID_EDIT9);
	if (wnd==tmp)
	{
		parm = ((CButton*)GetDlgItem(IDC_CHECK9))->GetCheck();
		if (parm)
			((CButton*)GetDlgItem(IDC_CHECK9))->SetCheck(0);
		else
			((CButton*)GetDlgItem(IDC_CHECK9))->SetCheck(1);
		OnCheck9();
		return;
	}

	tmp = GetDlgItem(ID_EDIT10);
	if (wnd==tmp)
	{
		parm = ((CButton*)GetDlgItem(IDC_CHECK10))->GetCheck();
		if (parm)
			((CButton*)GetDlgItem(IDC_CHECK10))->SetCheck(0);
		else
			((CButton*)GetDlgItem(IDC_CHECK10))->SetCheck(1);
		OnCheck10();
		return;
	}
}
