/************************************************************************
c
c   FILE NAME: stepDlg.cpp
c
c	 CONTAINS: 
c		Functions for the class CStepDlg 
c
c     COPYRIGHT 2013 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       stepDlg.cpp , 26.2
c    DATE AND TIME OF LAST  MODIFICATION
c       09/25/18 , 10:36:09
c
c**********************************************************************
*/
// stepDlg.cpp : implementation file
//

#include "stdafx.h"
#include <io.h>
#include "step.h"
#include "wsntres.h"
#include "stepDlg.h"
#include "StepOptDlg.h"
#include "StepFiltDlg.h"
#include "StepOutFiltDlg.h"
#include "tiges.h"
#include "xenv1.h"
#include "usignal.h"
#include "ustdio.h"
#include "ulist.h"
#include "nclver.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


extern "C" int output_units,output_normals;
extern "C" char iges_fname[UX_MAX_PATH_LEN];
extern "C" char iges_tmpfile[UX_MAX_PATH_LEN];
extern "C" UU_LOGICAL shade_set;
extern "C" int UTP_step_214;

extern "C" int no_of_views;
extern "C" int current_dir_number;
extern "C" int number_of_masters;
extern "C" int sequence_no;
extern "C" int utp_in_convert();
extern "C" int utp_out_main();
extern "C" char *ux_getenv(char *);
extern "C" UU_LIST *UIO_surf_list;
extern "C" int MAX_PARA_REC=300000;
extern "C" int UG_def_line_wt = 0;
extern "C" int  label_type;
extern "C" int tig_num_range, tig_range[1000][2];
extern "C" int iges_wntstr_out(char *msg, int display);
extern "C" int uig_summary();
extern "C" int uig_unb_opnfil(char*);
extern "C" int ux_delete0(char*);
extern "C" int ul_break_fname(char*, char*, char*);
extern "C" int ul_get_full_dir(char* dir, char* fullname);
extern "C" void ul_short_filename(char* fin, char* fout, int maxc);
int Iges_limit_txt;
extern "C" void getver(double *ver);
extern "C" void utp_getabout_str(char *str1,char *str2);
extern "C" int utp_read_step_file(char *);
extern "C" int ncl_init_color();
extern "C" int utp_load_modals();
extern "C" void utp_summary();
extern "C" void utp_free_lists(int);


/////////////////////////////////////////////////////////////////////////////
// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	//{{AFX_DATA(CAboutDlg)
	enum { IDD = IDD_ABOUTBOX };
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CAboutDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	//{{AFX_MSG(CAboutDlg)
	virtual BOOL OnInitDialog();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

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

BOOL CAboutDlg::OnInitDialog()
{
   char label1[256], label2[256];
   CDialog::OnInitDialog();

   utp_getabout_str(label1, label2);
   GetDlgItem(IDC_ABOUTLABEL1)->SetWindowText(label1);
   GetDlgItem(IDC_ABOUTLABEL2)->SetWindowText(label2);


   SetWindowText("About NCL/STEP");
   return 0;
}


/////////////////////////////////////////////////////////////////////////////
// CStepDlg dialog

CStepDlg::CStepDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CStepDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CStepDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
	Iges_limit_txt = 10000*80;
	m_processor = 0;
	m_curpos = 0;
}

void CStepDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CStepDlg)
	DDX_Control(pDX, IDC_PROGRESS1, m_pctl);
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CStepDlg, CDialog)
	//{{AFX_MSG_MAP(CStepDlg)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_BN_CLICKED(IDC_BROWSE1, OnBrowse1)
	ON_BN_CLICKED(IDC_BROWSE2, OnBrowse2)
	ON_BN_CLICKED(IDC_BROWSE3, OnBrowse3)
	ON_BN_CLICKED(IDC_BROWSE4, OnBrowse4)
	ON_BN_CLICKED(IDC_CHECK_IN, OnCheckIn)
	ON_BN_CLICKED(IDC_CHECK_OUT, OnCheckOut)
	ON_BN_CLICKED(IDC_FSUMMARY, OnFsummary)
	ON_BN_CLICKED(IDC_IN_OPTIONS, OnInOptions)
	ON_BN_CLICKED(IDC_FILTERE, OnFilter)
	ON_BN_CLICKED(IDC_FILTER_OUT, OnOutFilter)
	ON_CBN_SELCHANGE(IDC_OUTPUT_UNIT, OnSelchangeOutputUnit)
	ON_CBN_SELCHANGE(IDC_OUTPUT_NORMALS, OnSelchangeOutputNormals)
	ON_COMMAND(ID_HELP_HELP,OnHelpHelp)
	ON_COMMAND(ID_APP_ABOUT,OnAppAbout)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CStepDlg message handlers
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
BOOL CStepDlg::OnInitDialog()
{
	CDialog::OnInitDialog();
	static UU_LOGICAL Smodals_init=UU_FALSE;
	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon	
/*
.....set max text limit for status text window, otherwise
.....it will use default and too little.
.....Yurong 3/20/03
*/
	((CEdit*)GetDlgItem(IDC_STATUS))->LimitText(Iges_limit_txt);

	((CComboBox*)GetDlgItem(IDC_OUTPUT_UNIT))->SetCurSel(1);
	((CWnd*)GetDlgItem(IDC_INPUT_STEP))->SetFocus();

	int ifl;
	char verstr[256];
	double ver;
	getver(&ver);
	sprintf(verstr,"NCL/STEP V%7.2f",ver);
	SetWindowText(verstr);
	CloseProcessWindow();
	OnCheckIn();
	ncl_init_color();
	if (!Smodals_init)
	{
		utp_load_modals();
		Smodals_init = UU_TRUE;
		if (UTP_step_214 == 1)
		{
			((CButton*)GetDlgItem(IDC_RADIO_203))->SetCheck(0);
			((CButton*)GetDlgItem(IDC_RADIO_214))->SetCheck(1);
		}
		else
		{
			((CButton*)GetDlgItem(IDC_RADIO_203))->SetCheck(1);
			((CButton*)GetDlgItem(IDC_RADIO_214))->SetCheck(0);
		}
		ifl = 0;
		if (output_units == 1) ifl = 1;
		else if (output_units == 0) ifl = 2;
		((CComboBox*)GetDlgItem(IDC_OUTPUT_UNIT))->SetCurSel(ifl);
		ifl = output_normals;
		((CComboBox*)GetDlgItem(IDC_OUTPUT_NORMALS))->SetCurSel(ifl);
	}
	return FALSE;
}

/***********************************************************************
c
c   FUNCTION: OnSysCommand(UINT nID, LPARAM lParam)
c
c		This function Called when system menu is selected
c. 
c   INPUT:  None.
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepDlg::OnSysCommand(UINT nID, LPARAM lParam)
{
	if ((nID & 0xFFF0) == IDM_ABOUTBOX)
	{
		CAboutDlg dlgAbout;
		dlgAbout.DoModal();
	}
	else
	{
		CDialog::OnSysCommand(nID, lParam);
	}
}

/***********************************************************************
c
c   FUNCTION: OnPaint()
c
c		If you add a minimize button to your dialog, you will need the code below
c		to draw the icon.  For MFC applications using the document/view model,
c		this is automatically done for you by the framework.
c         The framework calls this member function when Windows 
c			or an application makes a request to repaint a 
c			portion of an application's window. The WM_PAINT 
c			message is sent when the UpdateWindow or RedrawWindow 
c			member function is called.
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepDlg::OnPaint() 
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

/***********************************************************************
c
c   FUNCTION: OnQueryDragIcon()
c
c		The system calls this to obtain the cursor to display while the user drags
c		the minimized window.
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
HCURSOR CStepDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

/***********************************************************************
c
c   FUNCTION: OnBrowse1()
c
c		This function called when user push the first "Browse" button (from Top to bottom)
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepDlg::OnBrowse1() 
{
	DWORD dwFlags;
	dwFlags = OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT | OFN_ENABLESIZING;
	LPCTSTR filter = "Input STEP Data File (*.step,*.stp)|*.step;*.stp|All Files (*.*)|*.*||";		
	CFileDialog *filedlg = new CFileDialog(TRUE, "step", NULL, dwFlags,
			filter, this);
	if (filedlg->DoModal()==IDCANCEL)
		return;
	CString FileName = filedlg->GetPathName();
	GetDlgItem(IDC_INPUT_STEP)->SetWindowText(FileName);
	delete filedlg;
}

/***********************************************************************
c
c   FUNCTION: OnBrowse1()
c
c		This function called when user push the second "Browse" button (from Top to bottom)
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepDlg::OnBrowse2() 
{
	DWORD dwFlags;
	dwFlags = OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT |
		OFN_ENABLESIZING;
	LPCTSTR filter = "Output Unibase File (*.u)|*.u|All Files (*.*)|*.*||";		
	CFileDialog *filedlg = new CFileDialog(FALSE, "u", NULL, dwFlags,
			filter, this);
	if (filedlg->DoModal()==IDCANCEL)
		return;
	CString FileName = filedlg->GetPathName();
	GetDlgItem(IDC_OUTPUT_UNI)->SetWindowText(FileName);
	delete filedlg;
}

/***********************************************************************
c
c   FUNCTION: OnBrowse1()
c
c		This function called when user push the third "Browse" button (from Top to bottom)
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepDlg::OnBrowse3() 
{
	DWORD dwFlags;
	dwFlags = OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT | OFN_ENABLESIZING;
	LPCTSTR filter = "Input Part File (*.u)|*.u|All Files (*.*)|*.*||";		
	CFileDialog *filedlg = new CFileDialog(TRUE, "u", NULL, dwFlags,
			filter, this);
	if (filedlg->DoModal()==IDCANCEL)
		return;
	CString FileName = filedlg->GetPathName();
	GetDlgItem(IDC_INPUT_PART)->SetWindowText(FileName);
	delete filedlg;
}

/***********************************************************************
c
c   FUNCTION: OnBrowse1()
c
c		This function called when user push the forth "Browse" button (from Top to bottom)
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepDlg::OnBrowse4() 
{
	DWORD dwFlags;
	dwFlags = OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT |
		OFN_ENABLESIZING;
	LPCTSTR filter = "Output STEP Data File (*.step,*.stp)|*.step;*.stp|All Files (*.*)|*.*||";		
	CFileDialog *filedlg = new CFileDialog(FALSE, "igs", NULL, dwFlags,
			filter, this);
	if (filedlg->DoModal()==IDCANCEL)
		return;
	CString FileName = filedlg->GetPathName();
	GetDlgItem(IDC_OUTPUT_DATA)->SetWindowText(FileName);
	delete filedlg;
}

/***********************************************************************
c
c   FUNCTION: OnCheckIn() 
c
c		This function called when user push the "STEP IN" check button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepDlg::OnCheckIn() 
{
	((CButton*)GetDlgItem(IDC_CHECK_IN))->SetCheck(1);
	((CButton*)GetDlgItem(IDC_CHECK_OUT))->SetCheck(0);
	((CWnd*)GetDlgItem(IDC_FSUMMARY))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_IN_OPTIONS))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_STATIC2))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_INPUT_STEP))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_OUTPUT_UNI))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_BROWSE1))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_BROWSE2))->EnableWindow(TRUE);	
	((CWnd*)GetDlgItem(IDC_FILTERE))->EnableWindow(TRUE);	

	((CWnd*)GetDlgItem(IDC_OUTPUT_UNIT))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_OUTPUT_NORMALS))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_STATIC3))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_STATIC4))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_STATIC6))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_STATIC7))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_INPUT_PART))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_OUTPUT_DATA))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_BROWSE3))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_BROWSE4))->EnableWindow(FALSE);	
	((CWnd*)GetDlgItem(IDC_FILTER_OUT))->EnableWindow(FALSE);	
	((CWnd*)GetDlgItem(IDC_RADIO_203))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_RADIO_214))->EnableWindow(FALSE);
}

/***********************************************************************
c
c   FUNCTION: OnCheckOut() 
c
c		This function called when user push the "STEP OUT" check button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepDlg::OnCheckOut() 
{
	((CButton*)GetDlgItem(IDC_CHECK_IN))->SetCheck(0);
	((CButton*)GetDlgItem(IDC_CHECK_OUT))->SetCheck(1);
	((CWnd*)GetDlgItem(IDC_FSUMMARY))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_IN_OPTIONS))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_STATIC2))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_INPUT_STEP))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_OUTPUT_UNI))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_BROWSE1))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_BROWSE2))->EnableWindow(FALSE);	
	((CWnd*)GetDlgItem(IDC_FILTERE))->EnableWindow(FALSE);	

	((CWnd*)GetDlgItem(IDC_OUTPUT_UNIT))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_OUTPUT_NORMALS))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_STATIC3))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_STATIC4))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_STATIC6))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_STATIC7))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_INPUT_PART))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_OUTPUT_DATA))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_BROWSE3))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_BROWSE4))->EnableWindow(TRUE);		
	((CWnd*)GetDlgItem(IDC_FILTER_OUT))->EnableWindow(TRUE);	
	((CWnd*)GetDlgItem(IDC_RADIO_203))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_RADIO_214))->EnableWindow(TRUE);
}

/*********************************************************************
**    I_FUNCTION     :  OnFsummary()
**       Callback for "File Summary" button on the main window.
**    PARAMETERS
**       INPUT  :
**          None
**       OUTPUT :
**          None
**    RETURNS      : none
**    SIDE EFFECTS : none
**    WARNINGS     : none
*********************************************************************/
void CStepDlg::OnFsummary() 
{
	UX_pathname filename,fname,dir,tmp;
	char *text;
	char msg[200];
	int status;
	UX_pathname tdir, tnam;
	CString tmpstr;
	((CWnd*)GetDlgItem(IDC_INPUT_STEP))->GetWindowText(tmpstr);
	if(tmpstr.GetLength()!=0)
	{
		text = tmpstr.GetBuffer(UX_MAX_PATH_LEN);
		strcpy(filename, text);
	}
	else
		filename[0] = '\0';
/*
.....check if user entered filename
*/
	ul_break_fname(filename, dir, fname);	
//	strcpy(fname,filename);
	if (strlen(fname)==0)
	{
		MessageBox("No input STEP data file specified!", "ERROR", MB_OK);
		return;
	}

	status = 0;
	if (dir[0]=='\0')
	{
/*
.....get local directory
*/
		ul_get_full_dir(".", dir);
	}
	strcat(dir, "\\");
	strcat(dir, fname);
	strcpy(filename, dir);
/*
.....Process the Step file
*/
	status = utp_read_step_file(filename);
	if (status == 0)
		strcpy(iges_fname, tmp);
	else
		iges_fname[0] = '\0'; 
/*
.....Create window to display file summary
*/
	if (status == 0) utp_summary(); 	
}

/***********************************************************************
c
c   FUNCTION: OnInOptions() 
c
c		This function called when user push the "Options" button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepDlg::OnInOptions() 
{
	UX_pathname fname;
	char *text;
	CString tmpstr;
	((CWnd*)GetDlgItem(IDC_INPUT_STEP))->GetWindowText(tmpstr);
	if(tmpstr.GetLength()!=0)
	{
		text = tmpstr.GetBuffer(UX_MAX_PATH_LEN);
		strcpy(fname, text);
	}
	else
		fname[0] = '\0';
	CStepOptDlg* optdlg = new CStepOptDlg(this, fname);
	optdlg->DoModal();
	delete optdlg;
}

/***********************************************************************
c
c   FUNCTION: OnFilter()
c
c     This function called when user pushed "Filter Entities" button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepDlg::OnFilter()
{
	CStepFiltDlg* filtdlg = new CStepFiltDlg(this);
	filtdlg->DoModal();
	delete filtdlg;
}

/***********************************************************************
c
c   FUNCTION: OnOutFilter()
c
c     This function called when user pushed "Output Filter Entities" button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepDlg::OnOutFilter()
{
	CStepOutFiltDlg* filtdlg = new CStepOutFiltDlg(this);
	filtdlg->DoModal();
	delete filtdlg;
}

/***********************************************************************
c
c   FUNCTION: OnSelchangeOutputUnit() 
c
c		This function called when user changed the "Output Unit" choices
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepDlg::OnSelchangeOutputUnit() 
{
	int choice;
	choice = ((CComboBox*)GetDlgItem(IDC_OUTPUT_UNIT))->GetCurSel();
	if (choice == 0)
		output_units = -1;
	else if (choice == 1)
		output_units = 1;
	else
		output_units = 0;
}

/***********************************************************************
c
c   FUNCTION: OnSelchangeOutputNormals() 
c
c		This function called when user changed the "Use Surface Normals" choices
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepDlg::OnSelchangeOutputNormals() 
{
	int choice;
	choice = ((CComboBox*)GetDlgItem(IDC_OUTPUT_UNIT))->GetCurSel();
	output_normals = choice;
}

/***********************************************************************
c
c   FUNCTION: OnCancel() 
c
c		This function called when user pushed "Cancel" button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepDlg::OnCancel() 
{
	// TODO: Add extra cleanup here
	utp_free_lists(1);
	CDialog::OnCancel();
}

/***********************************************************************
c
c   FUNCTION: OnOK() 
c
c		This function called when user pushed "Run" button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepDlg::OnOK() 
{
	int status, step_in, step_out;
	char *text;
	UX_pathname filename,fname,dir,tmp,tdir,tnam;
	char msg[256];
	step_in = ((CButton*)GetDlgItem(IDC_CHECK_IN))->GetCheck();
	step_out = ((CButton*)GetDlgItem(IDC_CHECK_OUT))->GetCheck();

	dbyte = pbyte = 0;
	if (step_in==1)
	{
		CString tmpstr;
	
		((CWnd*)GetDlgItem(IDC_INPUT_STEP))->GetWindowText(tmpstr);
		if(tmpstr.GetLength()!=0)
		{
			text = tmpstr.GetBuffer(UX_MAX_PATH_LEN);
			strcpy(filename, text);
		}
		else
			filename[0] = '\0';
/*
.....check if user entered filename
*/
		ul_break_fname(filename, dir, fname);
//		strcpy(fname , filename);
		if (strlen(fname)==0)
		{
			MessageBox("No input STEP data file specified!", "ERROR", MB_OK);
			return;
		}
		status = 0;
		if (dir[0]=='\0')
		{
/*
.....get local directory
*/
			ul_get_full_dir(".", dir);
		}
		strcat(dir, "\\");
		strcat(dir, fname);
		strcpy(filename, dir);

		if (strcmp(filename, iges_fname)!=0)
		{
			if (iges_fname[0]!='\0')
			{
				if (iges_fd != 0) _close(iges_fd);
				iges_fd = 0;
				iges_fname[0] = '\0';
			}
			strcpy(tmp, filename);
			status = utp_read_step_file(filename);
			if ( status == 0)
				strcpy(iges_fname, tmp);
			else
				iges_fname[0] = '\0';
		}
		if (status!=0) return;
		no_of_views = 0;
		current_dir_number = 0;
		number_of_masters = 0;
		sequence_no = 0;
		utp_in_convert();  
		t_num = 0;
	}
	else if (step_out==1)
	{

		UTP_step_214 = ((CButton*)GetDlgItem(IDC_RADIO_214))->GetCheck();
		utp_out_main();
	}
	else
	{
		MessageBox("You need to either pick STEP IN or STEP OUT", "Choose one",
		 	MB_OK);
	}
}

/***********************************************************************
c
c   FUNCTION: OnHelpHelp() 
c
c		This function called when user clicks on Help from the Menubar.
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CStepDlg::OnHelpHelp() 
{
	char *p, buf[256];
	int len;

	p = ux_getenv("NCL_STEP_HELP");
	if (p != 0)
	{
		len = strlen(p);
		if (len==0)
		{
			sprintf(buf,"%s", p);
			system(buf);
			return;
		}
		system(p);
	}

}
/***********************************************************************
c
c   SUBROUTINE:  Display_as_percent
c
c   FUNCTION:  This function display process as percent num
c
c   INPUT:  num: percentage to display
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CStepDlg::Display_as_percent(int num)
{
	MSG msg;
	int stat;
	char label[256];
	if (m_processor==0)
		return;
	int pos = abs(num);
	m_pctl.SetPos(pos);

	if (num<0)
		strcpy(label, "Processing ...");
	else
		sprintf(label, "%d%% Completed ...", num);
	if (pos-m_curpos>=2)
	{
		SetDlgItemText(IDC_LPROCESSOR2, label);
		m_curpos = pos;
		m_pctl.UpdateWindow();
		while (::PeekMessage(&msg, NULL, NULL, NULL, PM_NOREMOVE))
		{
			if ((msg.message==WM_PAINT) || (msg.message==WM_SIZING)
				|| (msg.message==WM_SIZE) || (msg.message==WM_MOVE)
				|| (msg.message==WM_MOVING) || (msg.message==WM_WINDOWPOSCHANGED))
			{
				if (!GetMessage(&msg, NULL, msg.message, msg.message))
					return;
				DispatchMessage(&msg);
			}
			else
			{
				stat = ::PeekMessage(&msg, msg.hwnd, msg.message, msg.message, PM_REMOVE);
			}
		}
	}
}

/***********************************************************************
c
c   SUBROUTINE:  ShowProcessWindow(char *title)
c
c   FUNCTION:  This function show a process window
c
c   INPUT:  char *title: label before processor
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CStepDlg::ShowProcessWindow(char *title)
{
	if (m_processor)
		return;
	CRect temp (0,0, 20, 20);
	MapDialogRect(&temp);
	int dy = temp.Height() ;

	CRect rect;
/*
	((CWnd*)GetDlgItem(IDOK))->GetWindowRect(&rect);
	rect.top = rect.top - dy;
	rect.bottom = rect.bottom - dy;
	((CWnd*)GetDlgItem(IDOK))->MoveWindow(&rect);

	((CWnd*)GetDlgItem(IDCANCEL))->GetWindowRect(&rect);
	rect.top = rect.top - dy;
	rect.bottom = rect.bottom - dy;
	((CWnd*)GetDlgItem(IDCANCEL))->MoveWindow(&rect);
	GetWindowRect(&rect);
	rect.bottom = rect.bottom + dy;
	MoveWindow(&rect);
*/
	m_processor = 1;
	((CWnd*)GetDlgItem(IDOK))->ShowWindow(SW_HIDE);
	((CWnd*)GetDlgItem(IDCANCEL))->ShowWindow(SW_HIDE);

	SetDlgItemText(IDC_LPROCESSOR1, title);
	SetDlgItemText(IDC_LPROCESSOR2, "1% Completed ...");
	m_curpos = 0;
	((CWnd*)GetDlgItem(IDC_LPROCESSOR1))->ShowWindow(SW_SHOW);
	((CWnd*)GetDlgItem(IDC_PROGRESS1))->ShowWindow(SW_SHOW);
	((CWnd*)GetDlgItem(IDC_LPROCESSOR2))->ShowWindow(SW_SHOW);
	UpdateWindow();
}
/***********************************************************************
c
c   SUBROUTINE:  CloseProcessWindow()
c
c   FUNCTION:  This function close a process window
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CStepDlg::CloseProcessWindow()
{
	CRect temp (0, 0, 20, 20);
	MapDialogRect(&temp);
	int dy = temp.Height() ;
/*
	CRect rect;
	((CWnd*)GetDlgItem(IDOK))->GetWindowRect(&rect);
	rect.top = rect.top - dy;
	rect.bottom = rect.bottom - dy;
	((CWnd*)GetDlgItem(IDOK))->MoveWindow(&rect);

	((CWnd*)GetDlgItem(IDCANCEL))->GetWindowRect(&rect);
	rect.top = rect.top - dy;
	rect.bottom = rect.bottom - dy;
	((CWnd*)GetDlgItem(IDCANCEL))->MoveWindow(&rect);

	GetWindowRect(&rect);
	rect.bottom = rect.bottom - dy;
	MoveWindow(&rect);
*/
	m_processor = 0;
	((CWnd*)GetDlgItem(IDOK))->ShowWindow(SW_SHOW);
	((CWnd*)GetDlgItem(IDCANCEL))->ShowWindow(SW_SHOW);

	((CWnd*)GetDlgItem(IDC_LPROCESSOR1))->ShowWindow(SW_HIDE);
	((CWnd*)GetDlgItem(IDC_LPROCESSOR2))->ShowWindow(SW_HIDE);
	((CWnd*)GetDlgItem(IDC_PROGRESS1))->ShowWindow(SW_HIDE);
	UpdateWindow();
	m_curpos = 0;
}
/***********************************************************************
c
c   SUBROUTINE:  SetProcessLabel()
c
c   FUNCTION:  This function set labels in a process window
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CStepDlg::SetProcessLabel(char *label1, char *label2)
{
	SetDlgItemText(IDC_LPROCESSOR1, label1);
	SetDlgItemText(IDC_LPROCESSOR2, label2);
}
/***********************************************************************
c
c   FUNCTION: OnAppAbout() 
c
c		This function called when user clicks on About from the Menubar.
c
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/

void CStepDlg::OnAppAbout()
{
   CAboutDlg aboutDlg;
   aboutDlg.DoModal();
}

