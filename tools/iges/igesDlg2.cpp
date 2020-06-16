/************************************************************************
c
c   FILE NAME: igesDlg.cpp
c
c	 CONTAINS: 
c		Functions for the class CIgesDlg2w 
c
c     COPYRIGHT 2011 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       igesDlg2.cpp , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       01/20/17 , 09:37:08
c
c**********************************************************************
*/
// igesDlg2.cpp : implementation file
//

#include "stdafx.h"
#include <io.h>
#include "wsntres.h"
#include "igesDlg2.h"


#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern "C" int Iges_batch;
extern "C" void iges_batchrun ();
extern "C" void iges_open_process_win(char*);
extern "C" void iges_disply_as_percent(int);
extern "C" void getver(double *ver);
/////////////////////////////////////////////////////////////////////////////
// CIgesDlg dialog

CIgesDlg2::CIgesDlg2(CWnd* pParent /*=NULL*/)
	: CDialog(CIgesDlg2::IDD, pParent)
{
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
	m_processor = 0;
	m_curpos = 0;
}

void CIgesDlg2::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CIgesDlg2)
	DDX_Control(pDX, IDC_PROGRESS1, m_pctl);
		// NOTE: the ClassWizard will add DDX and DDV calls here
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CIgesDlg2, CDialog)
	//{{AFX_MSG_MAP(CIgesDlg2)
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CIgesDlg2 message handlers
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
BOOL CIgesDlg2::OnInitDialog()
{
	CDialog::OnInitDialog();

	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon	

	char verstr[256];
	double ver;
	getver(&ver);
	sprintf(verstr,"NCL/IGES V%5.3f",ver);
	SetWindowText(verstr);
	iges_open_process_win("Converting IGES to Unibase file");
	iges_disply_as_percent(1);
	PostMessage(WM_COMMAND, IDOK);
	return FALSE;
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
void CIgesDlg2::OnPaint() 
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
HCURSOR CIgesDlg2::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
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
void CIgesDlg2::OnCancel() 
{
	// TODO: Add extra cleanup here
	
	CDialog::OnCancel();
}


/***********************************************************************
c
c   FUNCTION: OnOk() 
c
c		This function called when user pushed "OK" button
c		but here we called at beginning when starting run but
c		not close the dialog unti canceled
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CIgesDlg2::OnOK() 
{
	((CWnd*)GetDlgItem(IDOK))->EnableWindow(FALSE);
	iges_batchrun ();
	CDialog::OnOK();
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
void CIgesDlg2::Display_as_percent(int num)
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
void CIgesDlg2::ShowProcessWindow(char *title)
{
	if (m_processor)
		return;
	CRect temp (0,0, 20, 20);
	MapDialogRect(&temp);
	int dy = temp.Height() ;

	CRect rect;
	m_processor = 1;

	SetDlgItemText(IDC_LPROCESSOR1, title);
	SetDlgItemText(IDC_LPROCESSOR2, "1% Completed ...");
	m_curpos = 0;
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
void CIgesDlg2::CloseProcessWindow()
{
	m_processor = 0;
	m_curpos = 0;
	SendMessage(WM_COMMAND, IDCANCEL);
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
void CIgesDlg2::SetProcessLabel(char *label1, char *label2)
{
	SetDlgItemText(IDC_LPROCESSOR1, label1);
	SetDlgItemText(IDC_LPROCESSOR2, label2);
}

