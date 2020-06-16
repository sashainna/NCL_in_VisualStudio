/************************************************************************
c
c   FILE NAME: plotDlg.cpp
c
c	 CONTAINS: 
c		Functions for the class plotDlg 
c
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       plotDlg.cpp , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c       04/29/15 , 15:13:20
c
c**********************************************************************
*/
// plotDlg.cpp : implementation file
//

#include "stdafx.h"
#include "xenv1.h"
#include "plot.h"
#include "plotDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
#include "gobas.h"
#include "tplot.h"
int Plot_Port=0;
extern "C" int utp_interact();
extern "C" UX_pathname pfnm;
extern "C" int utp_if_psprinter();
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

/////////////////////////////////////////////////////////////////////////////
// CPlotDlg dialog

CPlotDlg::CPlotDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CPlotDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CPlotDlg)
		// NOTE: the ClassWizard will add member initialization here
	//}}AFX_DATA_INIT
	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void CPlotDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPlotDlg)
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CPlotDlg, CDialog)
	//{{AFX_MSG_MAP(CPlotDlg)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_BN_CLICKED(IDC_BROWSE1, OnBrowse1)
	ON_BN_CLICKED(IDC_BROWSE2, OnBrowse2)
	ON_CBN_SELCHANGE(IDC_BYPASS, OnSelchangeBypass)
	ON_CBN_SELCHANGE(IDC_OUTPUT, OnSelchangeOutput)
	ON_CBN_SELCHANGE(IDC_PLOT_SIZE, OnSelchangePlotSize)
	ON_CBN_SELCHANGE(IDC_PLOT_TYPE, OnSelchangePlotType)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPlotDlg message handlers

/***********************************************************************
c
c   SUBROUTINE:  OnInitDialog
c
c   FUNCTION:  This function initialize plot applicaton window
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
BOOL CPlotDlg::OnInitDialog()
{
	char tempstr[256];
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
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon
	if (strcmp(plotopts.type, "7475")==0)
		((CComboBox*)GetDlgItem(IDC_PLOT_TYPE))->SetCurSel(0);
	else if (strcmp(plotopts.type, "7580")==0)
		((CComboBox*)GetDlgItem(IDC_PLOT_TYPE))->SetCurSel(1);
	else if (strcmp(plotopts.type, "ps")==0)
		((CComboBox*)GetDlgItem(IDC_PLOT_TYPE))->SetCurSel(2);
	else if (strcmp(plotopts.type, "1043")==0)
		((CComboBox*)GetDlgItem(IDC_PLOT_TYPE))->SetCurSel(3);

	if (plotopts.size<0) plotopts.size = 0;
	((CComboBox*)GetDlgItem(IDC_PLOT_SIZE))->SetCurSel(plotopts.size);

	((CComboBox*)GetDlgItem(IDC_BYPASS))->SetCurSel(plotopts.bypass);

	((CComboBox*)GetDlgItem(IDC_OUTPUT))->SetCurSel(0);
	if (plotopts.print == 1)
	{
		((CComboBox*)GetDlgItem(IDC_OUTPUT))->SetCurSel(1);
		Plot_Port = 1;
	}
	else 
	{
		((CComboBox*)GetDlgItem(IDC_OUTPUT))->SetCurSel(0);
		Plot_Port = 0;
	}
	if (Plot_Port==0)
	{
		((CWnd*)GetDlgItem(IDC_STATIC2))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_OUTPUT_DISK))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_BROWSE2))->EnableWindow(TRUE);
	}
	else
	{
		((CWnd*)GetDlgItem(IDC_STATIC2))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_OUTPUT_DISK))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_BROWSE2))->EnableWindow(FALSE);
	}
	sprintf(tempstr, "%f",plotopts.linewt);
	((CWnd*)GetDlgItem(IDC_LINE_WID))->SetWindowText(tempstr);
	((CWnd*)GetDlgItem(IDC_INPUT_FILE))->SetWindowText(pfnm);
	((CWnd*)GetDlgItem(IDC_OUTPUT_DISK))->SetWindowText(plotopts.diskfnm);
	return TRUE;  // return TRUE  unless you set the focus to a control
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
void CPlotDlg::OnSysCommand(UINT nID, LPARAM lParam)
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
void CPlotDlg::OnPaint() 
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
HCURSOR CPlotDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

/***********************************************************************
c
c   FUNCTION: OnBrowse1()
c
c		The function open a filebrowser to get the plot input file
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPlotDlg::OnBrowse1() 
{
	DWORD dwFlags;
	dwFlags = OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT | OFN_ENABLESIZING;
	LPCTSTR filter = "Plot Files (*.pl1)|*.pl1|All Files (*.*)|*.*||";		
	CFileDialog *filedlg = new CFileDialog(TRUE, "pl1", NULL, dwFlags,
			filter, this);
	filedlg->m_ofn.lpstrTitle = "Input Plot File";
	if (filedlg->DoModal()==IDCANCEL)
		return;
	CString FileName = filedlg->GetPathName();
	GetDlgItem(IDC_INPUT_FILE)->SetWindowText(FileName);
	delete filedlg;	
}

/***********************************************************************
c
c   FUNCTION: OnBrowse1()
c
c		The function open a filebrowser to get the plot output file
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPlotDlg::OnBrowse2() 
{
	DWORD dwFlags;
	int typeindx;
	dwFlags = OFN_PATHMUSTEXIST | OFN_HIDEREADONLY | OFN_OVERWRITEPROMPT |
		OFN_ENABLESIZING;
	LPCTSTR filter = "HP 7475 Files (*.7475)|*.7475|HP 7580 (DesignJet) Files (*.758)|*.758|PostScript Files (*.ps)|*.ps|Calcomp 1043 Files (*.1043)|*.1043|All Files (*.*)|*.*||";		
	CFileDialog *filedlg = new CFileDialog(TRUE, NULL, NULL, dwFlags,
			filter, this);
	filedlg->m_ofn.lpstrTitle = "Output Disk File";
	if (filedlg->DoModal()==IDCANCEL)
		return;
	CString FileName = filedlg->GetPathName();
	CString Fileext = filedlg->GetFileExt(); 
	if (Fileext=="")
	{
		typeindx = filedlg->m_ofn.nFilterIndex;
		switch(typeindx)
		{
		case 1:
			Fileext = "7475";
			break;
		case 2:
			Fileext = "758";
			break;
		case 3:
			Fileext = "ps";
			break;
		case 4:
			Fileext = "1043";
			break;
		default:
			Fileext = "7475";
		}
		FileName = FileName + '.' + Fileext;		
	}
	GetDlgItem(IDC_OUTPUT_DISK)->SetWindowText(FileName);
	delete filedlg;		
}

/***********************************************************************
c
c   FUNCTION: OnSelchangeBypass()
c
c		The function called when choice select for by pass is changed
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPlotDlg::OnSelchangeBypass() 
{
	plotopts.bypass = ((CComboBox*)GetDlgItem(IDC_BYPASS))->GetCurSel();
	
}

/***********************************************************************
c
c   FUNCTION: OnSelchangeOutput()
c
c		The function called when choice select for "Output" is changed
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPlotDlg::OnSelchangeOutput() 
{
	CComboBox* cmbbox = (CComboBox*)GetDlgItem(IDC_OUTPUT);
	Plot_Port = cmbbox->GetCurSel();
	if (Plot_Port==0)
	{
		((CWnd*)GetDlgItem(IDC_STATIC2))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_OUTPUT_DISK))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_BROWSE2))->EnableWindow(TRUE);
	}
	else
	{
		((CWnd*)GetDlgItem(IDC_STATIC2))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_OUTPUT_DISK))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_BROWSE2))->EnableWindow(FALSE);
	}	
}

/***********************************************************************
c
c   FUNCTION: OnSelchangePlotSize()
c
c		The function called when choice select for "Plot Size" is changed
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPlotDlg::OnSelchangePlotSize() 
{
	plotopts.size = ((CComboBox*)GetDlgItem(IDC_PLOT_SIZE))->GetCurSel();	
}

/***********************************************************************
c
c   FUNCTION: OnSelchangePlotType()
c
c		The function called when choice select for "Plot Type" is changed
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPlotDlg::OnSelchangePlotType() 
{
	CComboBox* cmbbox = (CComboBox*)GetDlgItem(IDC_PLOT_TYPE);
	if (cmbbox->GetCurSel()==0)
	{
		strcpy(plotopts.type, "7475");
		((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_BYPASS))->EnableWindow(TRUE);
	}
	else if (cmbbox->GetCurSel()==1)
	{
		strcpy(plotopts.type, "7580");
		((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(TRUE);
		((CWnd*)GetDlgItem(IDC_BYPASS))->EnableWindow(TRUE);
	}
	else if (cmbbox->GetCurSel()==2)
	{
		strcpy(plotopts.type, "ps");
		((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_BYPASS))->EnableWindow(FALSE);
	}
	else if (cmbbox->GetCurSel()==3)
	{
		strcpy(plotopts.type, "1043");
		((CWnd*)GetDlgItem(IDC_STATIC1))->EnableWindow(FALSE);
		((CWnd*)GetDlgItem(IDC_BYPASS))->EnableWindow(FALSE);
	}
}

/***********************************************************************
c
c   FUNCTION: OnOK()
c
c		The function called when "PLOT" button is pushed
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPlotDlg::OnOK() 
{
	char *text;
	char *indx;

	CString tmpstr;
	((CWnd*)GetDlgItem(IDC_INPUT_FILE))->GetWindowText(tmpstr);
	if(tmpstr.GetLength()!=0)
	{
		text = tmpstr.GetBuffer(UX_MAX_PATH_LEN);
		strcpy(pfnm, text);
		indx = strstr(pfnm, ".pl");
		if (indx!=NULL)
			*indx = '\0';
		tmpstr.ReleaseBuffer();  
	}
	else
	{
		MessageBox("No input plot file specified.", "ERROR", MB_OK);
		return;
	}
	((CWnd*)GetDlgItem(IDC_LINE_WID))->GetWindowText(tmpstr);
	if(tmpstr.GetLength()!=0)
	{
		text = tmpstr.GetBuffer(256);
		plotopts.linewt = atof (text);
		tmpstr.ReleaseBuffer();
	}
	if (plotopts.linewt<0)
	{
		MessageBox("Invalid line width.","ERROR", MB_OK);
		return;
	}
	if (Plot_Port==0)
	{
		plotopts.print = 0;
		((CWnd*)GetDlgItem(IDC_OUTPUT_DISK))->GetWindowText(tmpstr);
		if(tmpstr.GetLength()!=0)
		{
			text = tmpstr.GetBuffer(UX_MAX_PATH_LEN);
			strcpy(plotopts.diskfnm, text);
			tmpstr.ReleaseBuffer();  
		}
		else
		{
			MessageBox("No output disk file specified.", "ERROR", MB_OK);
			return;
		}
	}
	else
	{
/*
......we only support postscript in PC
*/
		if (strcmp(plotopts.type, "ps")!=0)
		{
			MessageBox("We only support Postscript Printer on PC", "Info", MB_OK);
			return;
		}
		plotopts.print = 1;
		CPlotApp *app = (CPlotApp*)AfxGetApp();
		app->OnPrintPS();
		return;
	}
	utp_interact();
}
