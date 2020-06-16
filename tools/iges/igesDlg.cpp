/************************************************************************
c
c   FILE NAME: igesDlg.cpp
c
c	 CONTAINS: 
c		Functions for the class CIgesDlg 
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       igesDlg.cpp , 26.2
c    DATE AND TIME OF LAST  MODIFICATION
c       09/25/18 , 10:28:37
c
c**********************************************************************
*/
// igesDlg.cpp : implementation file
//

#include "stdafx.h"
#include <io.h>
#include "iges.h"
#include "wsntres.h"
#include "igesDlg.h"
#include "IgesOptDlg.h"
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


extern "C" char iges_fname[UX_MAX_PATH_LEN];
extern "C" char iges_tmpfile[UX_MAX_PATH_LEN];

extern "C" int uig_in_convert();
extern "C" int uio_main();
extern "C" char *ux_getenv(char *);
extern "C" int MAX_PARA_REC=300000;
extern "C" int UG_def_line_wt = 0;
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
extern "C" void iges_getabout_str(char *str1,char *str2);


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

   iges_getabout_str(label1, label2);
   GetDlgItem(IDC_ABOUTLABEL1)->SetWindowText(label1);
   GetDlgItem(IDC_ABOUTLABEL2)->SetWindowText(label2);


   SetWindowText("About NCL/IGES");
   return 0;
}


/////////////////////////////////////////////////////////////////////////////
// CIgesDlg dialog

CIgesDlg::CIgesDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CIgesDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CIgesDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
	Iges_limit_txt = 10000*80;
	m_processor = 0;
	m_curpos = 0;
}

void CIgesDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CIgesDlg)
	DDX_Control(pDX, IDC_PROGRESS1, m_pctl);
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CIgesDlg, CDialog)
	//{{AFX_MSG_MAP(CIgesDlg)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_BN_CLICKED(IDC_BROWSE1, OnBrowse1)
	ON_BN_CLICKED(IDC_BROWSE2, OnBrowse2)
	ON_BN_CLICKED(IDC_BROWSE3, OnBrowse3)
	ON_BN_CLICKED(IDC_BROWSE4, OnBrowse4)
	ON_BN_CLICKED(IDC_CHECK_IN, OnCheckIn)
	ON_BN_CLICKED(IDC_CHECK_OUT, OnCheckOut)
	ON_CBN_SELCHANGE(IDC_DRAW_ONLY, OnSelchangeDrawOnly)
	ON_BN_CLICKED(IDC_FSUMMARY, OnFsummary)
	ON_BN_CLICKED(IDC_IN_OPTIONS, OnInOptions)
	ON_CBN_SELCHANGE(IDC_OUTPUT_UNIT, OnSelchangeOutputUnit)
	ON_COMMAND(ID_HELP_HELP,OnHelpHelp)
	ON_COMMAND(ID_APP_ABOUT,OnAppAbout)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CIgesDlg message handlers
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
BOOL CIgesDlg::OnInitDialog()
{
	CDialog::OnInitDialog();
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

//	OnCheckIn();
	((CComboBox*)GetDlgItem(IDC_OUTPUT_UNIT))->SetCurSel(1);
	((CComboBox*)GetDlgItem(IDC_DRAW_ONLY))->SetCurSel(1);
	((CWnd*)GetDlgItem(IDC_INPUT_IGES))->SetFocus();

	char verstr[256];
	double ver;
	getver(&ver);
	sprintf(verstr,"NCL/IGES V%7.2f",ver);
	SetWindowText(verstr);
	CloseProcessWindow();
	OnCheckIn();
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
void CIgesDlg::OnSysCommand(UINT nID, LPARAM lParam)
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
void CIgesDlg::OnPaint() 
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
HCURSOR CIgesDlg::OnQueryDragIcon()
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
void CIgesDlg::OnBrowse1() 
{
	DWORD dwFlags;
	dwFlags = OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT | OFN_ENABLESIZING;
	LPCTSTR filter = "Input IGES Data File (*.igs, *.iges)|*.igs;*.iges|All Files (*.*)|*.*||";		
	CFileDialog *filedlg = new CFileDialog(TRUE, "igs", NULL, dwFlags,
			filter, this);
	if (filedlg->DoModal()==IDCANCEL)
		return;
	CString FileName = filedlg->GetPathName();
	GetDlgItem(IDC_INPUT_IGES)->SetWindowText(FileName);
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
void CIgesDlg::OnBrowse2() 
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
void CIgesDlg::OnBrowse3() 
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
void CIgesDlg::OnBrowse4() 
{
	DWORD dwFlags;
	dwFlags = OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT |
		OFN_ENABLESIZING;
	LPCTSTR filter = "Output IGES Data File (*.igs, *.iges)|*.igs;*.iges|All Files (*.*)|*.*||";		
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
c		This function called when user push the "IGES IN" check button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesDlg::OnCheckIn() 
{
	((CButton*)GetDlgItem(IDC_CHECK_IN))->SetCheck(1);
	((CButton*)GetDlgItem(IDC_CHECK_OUT))->SetCheck(0);
	((CWnd*)GetDlgItem(IDC_FSUMMARY))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_IN_OPTIONS))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_STATIC2))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_INPUT_IGES))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_OUTPUT_UNI))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_BROWSE1))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_BROWSE2))->EnableWindow(TRUE);	

	((CWnd*)GetDlgItem(IDC_OUTPUT_UNIT))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_DRAW_ONLY))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_STATIC3))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_STATIC4))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_STATIC5))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_STATIC6))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_INPUT_PART))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_OUTPUT_DATA))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_BROWSE3))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_BROWSE4))->EnableWindow(FALSE);	
}

/***********************************************************************
c
c   FUNCTION: OnCheckIn() 
c
c		This function called when user push the "IGES OUT" check button
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesDlg::OnCheckOut() 
{
	((CButton*)GetDlgItem(IDC_CHECK_IN))->SetCheck(0);
	((CButton*)GetDlgItem(IDC_CHECK_OUT))->SetCheck(1);
	((CWnd*)GetDlgItem(IDC_FSUMMARY))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_IN_OPTIONS))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_STATIC2))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_INPUT_IGES))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_OUTPUT_UNI))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_BROWSE1))->EnableWindow(FALSE);
	((CWnd*)GetDlgItem(IDC_BROWSE2))->EnableWindow(FALSE);	

	((CWnd*)GetDlgItem(IDC_OUTPUT_UNIT))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_DRAW_ONLY))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_STATIC3))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_STATIC4))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_STATIC5))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_STATIC6))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_INPUT_PART))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_OUTPUT_DATA))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_BROWSE3))->EnableWindow(TRUE);
	((CWnd*)GetDlgItem(IDC_BROWSE4))->EnableWindow(TRUE);		
}

void CIgesDlg::OnSelchangeDrawOnly() 
{
	int choice;
	choice = ((CComboBox*)GetDlgItem(IDC_DRAW_ONLY))->GetCurSel();
	if (choice == 0)
		UIG_drawing_only = 1;
	else
		UIG_drawing_only = 0;	
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
void CIgesDlg::OnFsummary() 
{
	UX_pathname filename,fname,dir,tmp;
	char *text;
	char msg[200];
	int status;
	UX_pathname tdir, tnam;
	CString tmpstr;
	((CWnd*)GetDlgItem(IDC_INPUT_IGES))->GetWindowText(tmpstr);
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
		MessageBox("No input IGES data file specified!", "ERROR", MB_OK);
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
			_close(iges_fd);
			iges_fd = 0;
			if (ux_delete0(iges_tmpfile))
			{
				ul_short_filename(iges_fname,tmp,50);
				sprintf (msg, "can't delete %s corresponding %s", iges_tmpfile, tmp);
				MessageBox(msg, "Error!", MB_OK);
			}
			iges_fname[0] = '\0';
		}
		strcpy(tmp, filename);
		dbyte = pbyte = 0;
		status = uig_unb_opnfil(filename);
		if ( status == 0)
			strcpy(iges_fname, tmp);
		else
			iges_fname[0] = '\0'; 
	}
/*
.....Create window to display file summary
*/
	if ( status == 0)
		uig_summary(); 	
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
void CIgesDlg::OnInOptions() 
{
	UX_pathname fname;
	char *text;
	CString tmpstr;
	((CWnd*)GetDlgItem(IDC_INPUT_IGES))->GetWindowText(tmpstr);
	if(tmpstr.GetLength()!=0)
	{
		text = tmpstr.GetBuffer(UX_MAX_PATH_LEN);
		strcpy(fname, text);
	}
	else
		fname[0] = '\0';
	CIgesOptDlg* optdlg = new CIgesOptDlg(this, fname);
	optdlg->DoModal();
	delete optdlg;
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
void CIgesDlg::OnSelchangeOutputUnit() 
{
	int choice;
	choice = ((CComboBox*)GetDlgItem(IDC_OUTPUT_UNIT))->GetCurSel();
	if (choice == 0)
		output_units = 1;
	else
		output_units = 0;
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
void CIgesDlg::OnCancel() 
{
	// TODO: Add extra cleanup here
	
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
void CIgesDlg::OnOK() 
{
	int status, iges_in, iges_out;
	char *text;
	UX_pathname filename,fname,dir,tmp,tdir,tnam;
	char msg[256];
	iges_in = ((CButton*)GetDlgItem(IDC_CHECK_IN))->GetCheck();
	iges_out = ((CButton*)GetDlgItem(IDC_CHECK_OUT))->GetCheck();

	dbyte = pbyte = 0;
	if (iges_in==1)
	{
		CString tmpstr;
	
		((CWnd*)GetDlgItem(IDC_INPUT_IGES))->GetWindowText(tmpstr);
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
			MessageBox("No input IGES data file specified!", "ERROR", MB_OK);
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
				_close(iges_fd);
				iges_fd = 0;
				if (ux_delete0(iges_tmpfile))
				{
					ul_short_filename(iges_fname,tmp,50);
					sprintf (msg, "can't delete %s corresponding %s", iges_tmpfile, tmp);
					MessageBox(msg, "Error!", MB_OK);
				}
				iges_fname[0] = '\0';
			}
			strcpy(tmp, filename);
			status = uig_unb_opnfil(filename);
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
		uig_in_convert();  
		t_num = 0;
	}
	else if (iges_out==1)
	{
		uio_main();
	}
	else
	{
		MessageBox("You need either pick IGES IN or IGES OUT", "No Input", MB_OK);
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
void CIgesDlg::OnHelpHelp() 
{
	char *p, buf[256];
	int len;

	p = ux_getenv("NCL_IGES_HELP");
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
void CIgesDlg::Display_as_percent(int num)
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
void CIgesDlg::ShowProcessWindow(char *title)
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
void CIgesDlg::CloseProcessWindow()
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
void CIgesDlg::SetProcessLabel(char *label1, char *label2)
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

void CIgesDlg::OnAppAbout()
{
   CAboutDlg aboutDlg;
   aboutDlg.DoModal();
}

