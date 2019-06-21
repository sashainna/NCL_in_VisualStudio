/************************************************************************
c
c   FILE NAME: PtedProcessDlg.cpp
c
c	 CONTAINS: 
c	 all CPtedProcessDlg class: a dialog display Pted process dialog
c			Implementation functions
cand 
c			extern "C" void Pted_disply_ProcessDlg(title)
c			extern "C" void Pted_Display_Dlg_percent(int num)
c			extern "C" void Pted_Close_ProcessDlg()
c
c     COPYRIGHT 2003 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c			PtedProcessDlg.cpp , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c			09/11/13 , 12:59:27
c
c**********************************************************************
*/
#include "pwstdafx.h"
#include "Pted.h"
#include "PtedProcessDlg.h"

#ifdef _DEBUG
#undef THIS_FILE
static char BASED_CODE THIS_FILE[] = __FILE__;
#endif

extern int UU_BATCH;
CPtedProcessDlg *Ptd_pdlg =  NULL;
/////////////////////////////////////////////////////////////////////////////
// CPtedProcessDlg dialog

/***********************************************************************
c
c   SUBROUTINE: CPtedProcessDlg
c
c   FUNCTION:  constructor
c
c   INPUT:  
c			
c   OUTPUT: none
c
c***********************************************************************
*/
CPtedProcessDlg::CPtedProcessDlg(CWnd* pParent, /*=NULL*/ char *title)
	: CDialog(CPtedProcessDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CPtedProcessDlg)
	m_label = "1% Completed";
	if (title!=NULL)
		m_title = title;
	else
		m_title = "Pted Conversion";
	//}}AFX_DATA_INIT
	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32
	m_hIcon = AfxGetApp()->LoadIcon(IDI_PTED_ICON);
	m_curpos = 0;
}

void CPtedProcessDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CPtedProcessDlg)
	DDX_Control(pDX, IDC_PROGRESS1, m_pctl);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CPtedProcessDlg, CDialog)
	//{{AFX_MSG_MAP(CPtedProcessDlg)
	ON_WM_CLOSE()
	ON_WM_PAINT()
	ON_WM_DESTROY()
	ON_WM_TIMER()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPtedProcessDlg message handlers
/*
void CALLBACK EXPORT TimerProc_test(
   HWND hWnd,      // handle of CWnd that called SetTimer
   UINT nMsg,      // WM_TIMER
   UINT nIDEvent,   // timer identification
   DWORD dwTime    // system time
)
{
}
*/
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
BOOL CPtedProcessDlg::OnInitDialog()
{
	CDialog::OnInitDialog();
	CenterWindow();

	m_pctl.SetRange( 0, 100);
	m_pctl.SetPos(0);

	SetWindowText(m_title);
	UINT timer = SetTimer(1, 2000, NULL);
	SetDlgItemText(IDC_PROCESS_LABEL, "Pted is processing...");
	return TRUE;  // return TRUE  unless you set the focus to a control
}

/***********************************************************************
c
c   SUBROUTINE:  CloseWindow
c
c   FUNCTION:  This function close the process window: public function
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CPtedProcessDlg::CloseWindow()
{
	KillTimer(1);
	OnCancel();
}

/***********************************************************************
c
c   SUBROUTINE:  OnClose
c
c   FUNCTION:  Called when 'close' button on window pushed
c
c   INPUT:  none
c
c   OUTPUT: nine
c
c***********************************************************************
*/
void CPtedProcessDlg::OnClose() 
{
	OnCancel();
}

void CPtedProcessDlg::OnCancel() 
{	
	CDialog::OnCancel() ;
	CDialog::DestroyWindow();
}

void CPtedProcessDlg::OnDestroy() 
{
	CDialog::OnDestroy();
	Ptd_pdlg =  NULL;
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
void CPtedProcessDlg::Display_as_percent(int num)
{
/*
....this is for batch only, so just DispatchMessage the timer message
*/
/*
.....if num<0, display percentage
*/
	if (num<0)
	{
		char label[256];
		int pos = abs(num);
		m_pctl.SetPos(pos);
		strcpy(label, "Processing ...");
		SetDlgItemText(IDC_PROCESS_LABEL, label);
		return;
	}
	MSG msg;
	if (::PeekMessage(&msg, NULL, WM_TIMER, WM_TIMER, PM_NOREMOVE))
	{
		if (!GetMessage(&msg, NULL, WM_TIMER, WM_TIMER))
			return;
		DispatchMessage(&msg);
	}
/*
	char label[256];
	int pos = abs(num);
	m_pctl.SetPos(pos);

	if (num<0)
		strcpy(label, "Processing ...");
	else
		sprintf(label, "%d%% Completed ...", num);
	if (pos-m_curpos>=5)
	{
		SetDlgItemText(IDC_PROCESS_LABEL, label);
		m_curpos = pos;
	}
*/
}
void CPtedProcessDlg::OnTimer(UINT_PTR nIDEvent) 
{
	CDialog::OnTimer(nIDEvent);

	SetDlgItemText(IDC_PROCESS_LABEL, "Pted is processing...");

	m_curpos = m_curpos + 5;
	if (m_curpos>100)
	{
		m_curpos = 5;
	}
	m_pctl.SetPos(m_curpos);
}

/***********************************************************************
c
c   SUBROUTINE:  Pted_disply_ProcessDlg(title)
c
c   FUNCTION:  This function display a process awindow
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" void Pted_disply_ProcessDlg(char *title)
{
	if (UU_BATCH==2)
		return;
	if (Ptd_pdlg!=NULL)
	{
//		Ptd_pdlg->ShowWindow(SW_SHOW);
		return;
	}
	CPtedApp *app = (CPtedApp*)AfxGetApp();
	if (app==NULL)
		return;

	CWnd *MainDlg = (CWnd*)(app->GetMainWnd());
	if (MainDlg!=NULL)
		MainDlg = MainDlg->GetActiveWindow();

	Ptd_pdlg = new CPtedProcessDlg(MainDlg, title);
	Ptd_pdlg->Create(CPtedProcessDlg::IDD,  MainDlg);
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
extern "C" void Pted_Display_Dlg_percent(int num)
{
	if (Ptd_pdlg!=NULL)
	{
		Ptd_pdlg->Display_as_percent(num);
		return;
	}
}
/***********************************************************************
c
c   SUBROUTINE:  Pted_Close_ProcessDlg
c
c   FUNCTION:  This function close the process window
c
c   INPUT:  none
c
c   OUTPUT: none
c
c***********************************************************************
*/
extern "C" void Pted_Close_ProcessDlg()
{
	if (Ptd_pdlg!=NULL)
	{
		Ptd_pdlg->CloseWindow();
		return;
	}
}
