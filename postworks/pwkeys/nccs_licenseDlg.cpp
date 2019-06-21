/************************************************************************
c
c   FILE NAME: Nccs_licenseDlg.cpp
c
c        CONTAINS: 
c               CAboutDlg::CAboutDlg()
c               CNccs_licenseDlg::CNccs_licenseDlg()
c               CNccs_licenseDlg::OnInitDialog()
c               CNccs_licenseDlg::OnSysCommand()
c               CNccs_licenseDlg::OnPaint() 
c               CNccs_licenseDlg::OnQueryDragIcon()
c               CNccs_licenseDlg::OnFileExit() 
c               CNccs_licenseDlg::OnAdd()
c               CNccs_licenseDlg::OnDelete() 
c               CNccs_licenseDlg::OnSearch() 
c               CNccs_licenseDlg::OnClear() 
c               CNccs_licenseDlg::OnFileLoadlicense() 
c               CNccs_licenseDlg::OnShowSearch()
c               disfldc(char buf[10][132], int *ist, int *ien)
c				CNccs_licenseDlg::OnTsearch() 
c				void CNccs_licenseDlg::OnCheck1()
c				void CNccs_licenseDlg::OnCheck2()
c				void CNccs_licenseDlg::OnCheck3()
c				void CNccs_licenseDlg::OnCheck4()
c				void CNccs_licenseDlg::OnCheck5()
c				void CNccs_licenseDlg::OnCheck6()
c				void CNccs_licenseDlg::OnCheck7()
c				void CNccs_licenseDlg::OnCheck8()
c				void CNccs_licenseDlg::OnCheck9()
c
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        nccs_licenseDlg.cpp , 23.1
c      DATE AND TIME OF LAST  MODIFICATION
c        05/22/12 , 11:15:14 
c
c**********************************************************************
*/

#include "pwenv.h"
#include "pwstdafx.h"
#include "nccs_license.h"
#include "nccs_licenseDlg.h"
#include "PwNTAboutDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int addrec (char *msg, int *ierr);
extern "C" int delrec (char *msg, int *ierr);
extern "C" int search ();
extern "C" int savinput(char buf[10][132], int parm[10], int *flag);
extern "C" int reset_batch();
extern "C" int batchlic(char *batfile, int *num, int *ierr);
extern "C" int clrrec();
extern "C" int onfexit();
extern "C" int getsparm(char buf[10][132], int parm[10]);
extern "C" int savsparm(int *parm, char buf[132], int *item);
/////////////////////////////////////////////////////////////////////////////
// CNccs_licenseDlg dialog

/***********************************************************************
c
c   SUBROUTINE: CNccs_licenseDlg(CWnd* pParent) 
c
c   FUNCTION:  constructor
c
c   INPUT:  CWnd* pParent : parent window
c                       
c   OUTPUT: none
c
c***********************************************************************
*/
CNccs_licenseDlg::CNccs_licenseDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CNccs_licenseDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CNccs_licenseDlg)
	//}}AFX_DATA_INIT
	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
	HINSTANCE hInst = AfxFindResourceHandle(MAKEINTRESOURCE(IDR_ACCEL), RT_ACCELERATOR);
	m_accel = ::LoadAccelerators(hInst, MAKEINTRESOURCE(IDR_ACCEL));
}

void CNccs_licenseDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CNccs_licenseDlg)
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CNccs_licenseDlg, CDialog)
	//{{AFX_MSG_MAP(CNccs_licenseDlg)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_BN_CLICKED(ID_ADD, OnAdd)
	ON_BN_CLICKED(ID_CLEAR, OnClear)
	ON_BN_CLICKED(ID_DELETE, OnDelete)
	ON_BN_CLICKED(ID_SEARCH, OnSearch)
	ON_COMMAND(ID_EXIT, OnExit)
	ON_COMMAND(ID_FILE_LOADLICENSE, OnFileLoadlicense)
	ON_COMMAND(ID_SHOW_SEARCH, OnShowSearch)
	ON_BN_CLICKED(IDC_CHECK1, OnCheck1)
	ON_BN_CLICKED(IDC_CHECK2, OnCheck2)
	ON_BN_CLICKED(IDC_CHECK3, OnCheck3)
	ON_BN_CLICKED(IDC_CHECK4, OnCheck4)
	ON_BN_CLICKED(IDC_CHECK5, OnCheck5)
	ON_BN_CLICKED(IDC_CHECK6, OnCheck6)
	ON_BN_CLICKED(IDC_CHECK7, OnCheck7)
	ON_BN_CLICKED(IDC_CHECK8, OnCheck8)
	ON_BN_CLICKED(IDC_CHECK9, OnCheck9)
	ON_COMMAND(ID_TSEARCH, OnTsearch)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNccs_licenseDlg message handlers

/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog
c
c   FUNCTION:  This function initialize 
c                               the dialog
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
BOOL CNccs_licenseDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Add "About..." menu item to system menu.

	// IDM_ABOUTBOX must be in the system command range.
	ASSERT((IDM_ABOUTBOX & 0xFFF0) == IDM_ABOUTBOX);
	ASSERT(IDM_ABOUTBOX < 0xF000);

	CMenu* pSysMenu = GetSystemMenu(FALSE);
	if (pSysMenu != NULL)
	{
		CString strAboutMenu;
		strAboutMenu.LoadString(IDS_ABOUTBOX);
		if (!strAboutMenu.IsEmpty())
		{
			pSysMenu->AppendMenu(MF_SEPARATOR);
			pSysMenu->AppendMenu(MF_STRING, IDM_ABOUTBOX, strAboutMenu);
		}
	}

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);                 // Set big icon
	SetIcon(m_hIcon, FALSE);                // Set small icon
	
	// TODO: Add extra initialization here

	CRect windowRect, windowRect2, boxrect;
	GetClientRect(windowRect);
	GetClientRect(windowRect2);

	int butlen = (windowRect2.Width() - 25)/4;
	windowRect2.top = windowRect.bottom -30;
	windowRect2.bottom -= 5;
	windowRect2.right -= 5;
	windowRect2.left = windowRect2.right - butlen;

	CWnd* pChildWnd;
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


	return TRUE;  // return TRUE  unless you set the focus to a control
}

/***********************************************************************
c
c   SUBROUTINE:  OnSysCommand
c
c   FUNCTION:  Callback for system menu of
c                               the dialog
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_licenseDlg::OnSysCommand(UINT nID, LPARAM lParam)
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

void CNccs_licenseDlg::OnPaint() 
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
HCURSOR CNccs_licenseDlg::OnQueryDragIcon()
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
int CNccs_licenseDlg::Get_user_input (char buf[10][132])
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
		empty = 0;
		input.MakeUpper();
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
void CNccs_licenseDlg::OnAdd() 
{
	int flag, ierr;
	char msg[80];
	char buf[10][132]; /* same as FRMBUF defined on menu.inc */
	int parm[10];
	int status = Get_user_input (buf);
	if (status!=0)
	{
		MessageBox("Empty Input!", "NCCS_LICENSE Warning", MB_OK);
		return;
	}
	flag = 1;
	savinput(buf, parm, &flag);
	addrec (msg, &ierr);
	if (ierr != 0)
		MessageBox(msg, "NCCS_LICENSE Error",MB_OK);
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
void CNccs_licenseDlg::OnClear() 
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
void CNccs_licenseDlg::OnDelete() 
{
	int flag, ierr;
	char msg[80];
	char buf[10][132]; /* same as FRMBUF defined on menu.inc */
	int parm[10];
	int status = Get_user_input (buf);
	if (status!=0)
	{
		MessageBox("Empty Input!", "NCCS_LICENSE Warning", MB_OK);
		return;
	}
	flag = 1;
	savinput(buf, parm, &flag);
	delrec (msg, &ierr);
	if (ierr != 0)
		MessageBox(msg, "NCCS_LICENSE Error",MB_OK); 
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
void CNccs_licenseDlg::OnSearch() 
{
	search ();
}

/***********************************************************************
c
c   SUBROUTINE:  OnFileExit
c
c   FUNCTION:  Called when "exit" menu is picked
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CNccs_licenseDlg::OnExit() 
{
	CDialog::OnCancel();
	onfexit();
}

/***********************************************************************
c
c   SUBROUTINE:  disfldc(char buf[10][132], int *ist, int *ien)
c
c   FUNCTION:  display fields
c
c   INPUT:  ist: start field to display
c                       ien: end field to display
c
c   OUTPUT: nine
c
c***********************************************************************
*/
extern "C" void disfldc(char buf[10][132], int *ist, int *ien)
{
	CNccs_licenseApp *app = (CNccs_licenseApp*)AfxGetApp();
	CNccs_licenseDlg *MainDlg = (CNccs_licenseDlg*)(app->GetMainWnd());     
/*
.....added end marker and remove trailling space
*/
	CString tmpstr;
	char *tmp;
	for (int i=0; i<9;i++)
	{
		buf[i][131] = '\0';
		tmpstr = buf[i];
		tmpstr.TrimRight();
		tmp = tmpstr.GetBuffer(132);
		strcpy(buf[i], tmp);
	}
	if ((*ien==9)&&(*ist<=9))
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
void CNccs_licenseDlg::OnFileLoadlicense() 
{
	int i;
	LPCTSTR filter = "License Files(*.lic)|*.lic|All Files (*.*)|*.*||";            
	DWORD dwFlags = OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT;

	CFileDialog *filedlg = new CFileDialog(TRUE, "Nccs_license", NULL, dwFlags,
			filter, this);
	if (filedlg->DoModal()==IDCANCEL)
		return;
	CString FileName = filedlg->GetPathName();
	char batfile[UX_MAX_PATH];
	int ierr = 0;
	int len = FileName.GetLength();
	for (i=0;i<len;i++)
		batfile[i] = FileName[i];
	for (i=len;i<UX_MAX_PATH;i++)
		batfile[i] = ' ';
	reset_batch();  
	batchlic(batfile, &len, &ierr);
	delete filedlg;         
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
void CNccs_licenseDlg::OnShowSearch() 
{
	char buf[10][132];
	int parm[10];
	char label[10000];
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
	if (parm[8]==1)
	{
		SetDlgItemText(ID_EDIT9, buf[8]);
	}
	else
		SetDlgItemText(ID_EDIT9, "");
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
BOOL CNccs_licenseDlg::PreTranslateMessage(MSG* msg)
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
c                               Callback for check box #1. This function save the search
c                                       parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_licenseDlg::OnCheck1()
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
c                               Callback for check box #1. This function save the search
c                                       parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_licenseDlg::OnCheck2()
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
c                               Callback for check box #1. This function save the search
c                                       parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_licenseDlg::OnCheck3()
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
c                               Callback for check box #1. This function save the search
c                                       parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_licenseDlg::OnCheck4()
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
c                               Callback for check box #1. This function save the search
c                                       parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_licenseDlg::OnCheck5()
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
c                               Callback for check box #1. This function save the search
c                                       parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_licenseDlg::OnCheck6()
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
c                               Callback for check box #1. This function save the search
c                                       parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_licenseDlg::OnCheck7()
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
c                               Callback for check box #1. This function save the search
c                                       parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_licenseDlg::OnCheck8()
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
c                               Callback for check box #1. This function save the search
c                                       parameters into global value
c   FUNCTION:  
c
c   INPUT:  msg
c
c   OUTPUT: none
c
c***********************************************************************
*/
void CNccs_licenseDlg::OnCheck9()
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
void CNccs_licenseDlg::OnTsearch() 
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
}

/* no super to show, junk routine */
extern "C" void showsuper()
{
}
