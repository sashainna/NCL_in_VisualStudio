/************************************************************************
**
**   FILE NAME: NcqMainFrm.cpp
**	  
**   CONTAINS:
**		CPWMainFrame::CPWMainFrame()
**		CPWMainFrame::~CPWMainFrame()
**		 CPWMainFrame::PreCreateWindow()
**		CPWMainFrame::OnCreate()
**		CPWMainFrame::OnNclExit(WPARAM wParam,LPARAM lParam)
**		CMainFrame::OnNclUpdate(WPARAM wParam,LPARAM lParam)
**		CMainFrame::OnCopyData(CWnd* pWnd, COPYDATASTRUCT* pData) 
**		CMainFrame::OnViewMonitor()
**		CMainFrame::OnFileSaveQueue() 
**		CMainFrame::OnFileLoadQueue() 
**
**     COPYRIGHT 2003 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**    MODULE NAME AND RELEASE LEVEL
**       ncqmainfrm.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:13:03
**
************************************************************************
*/
// MainFrm.cpp : implementation of the CMainFrame class
//

#include "stdafx.h"
#include "ncq.h"

#include "NcqMainFrm.h"
#include "ncqDoc.h"
#include "ncqView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

extern CNcqView* NCQ_mainview;
extern "C" int NCQ_time_limit;
/////////////////////////////////////////////////////////////////////////////
// CMainFrame

IMPLEMENT_DYNCREATE(CMainFrame, CFrameWnd)
/*
.....Added option to cancel current batch file - ASF 1/8/14.
*/
BEGIN_MESSAGE_MAP(CMainFrame, CFrameWnd)
	//{{AFX_MSG_MAP(CMainFrame)
	ON_WM_CREATE()
	ON_WM_DROPFILES()
	ON_COMMAND(ID_OPTION_PRIORITY_HIGH, OnOptionPriorityHigh)
	ON_COMMAND(ID_OPTION_PRIORITY_LOW, OnOptionPriorityLow)
	ON_COMMAND(ID_OPTION_PRIORITY_NORMAL, OnOptionPriorityNormal)
	ON_COMMAND(ID_OPTION_TIMELIMIT_1HOUR, OnOptionTimelimit1hour)
	ON_COMMAND(ID_OPTION_TIMELIMIT_15MINUTES, OnOptionTimelimit15minutes)
	ON_COMMAND(ID_OPTION_TIMELIMIT_2HOURS, OnOptionTimelimit2hours)
	ON_COMMAND(ID_OPTION_TIMELIMIT_30MINUTES, OnOptionTimelimit30minutes)
	ON_COMMAND(ID_OPTION_TIMELIMIT_DISABLED, OnOptionTimelimitDisabled)
	ON_COMMAND(ID_OPTION_TIMELIMIT_CURRENT, OnOptionCancelCurrent)
	ON_COMMAND(ID_OPTION_TIMELIMIT_ALL, OnOptionCancelAll)
	ON_COMMAND(ID_FILE_LOADQUEUE, OnFileLoadQueue)
	ON_COMMAND(ID_FILE_SAVEQUEUE, OnFileSaveQueue)
	ON_COMMAND(ID_VIEW_MONITOR, OnViewMonitor)
	ON_COMMAND(ID_VIEW_CLEARSTATUS, OnClearStatus)
	ON_COMMAND(ID_VIEW_UPDATERATE_1SECOND, OnViewUpd_1second)
	ON_COMMAND(ID_VIEW_UPDATERATE_2SECONDS, OnViewUpd_2seconds)
	ON_COMMAND(ID_VIEW_UPDATERATE_3SECONDS, OnViewUpd_3seconds)
	ON_COMMAND(ID_VIEW_UPDATERATE_5SECONDS, OnViewUpd_5seconds)
	ON_COMMAND(ID_VIEW_UPDATERATE_10SECONDS, OnViewUpd_10seconds)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CMainFrame construction/destruction

CMainFrame::CMainFrame()
{
	// TODO: add member initialization code here
	
}

CMainFrame::~CMainFrame()
{
}
/***********************************************************************
**
**   FUNCTION: OnCreate(LPCREATESTRUCT lpCreateStruct) 
**
**		Override this member function to perform any needed 
**		initialization of a derived class. 
**   
**		INPUT:  LPCREATESTRUCT lpCreateStruct: contains copies of 
**						the parameters used to create the window.
**
**   OUTPUT :   none
**   RETURN:    None
**
**********************************************************************/
int CMainFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;
	DragAcceptFiles(TRUE);	
	CRect rect;
	GetWindowRect(&rect);
	if (!m_wndToolBar.CreateEx(this, TBSTYLE_FLAT, WS_CHILD | WS_VISIBLE | CBRS_TOP
		| CBRS_TOOLTIPS | CBRS_FLYBY | CBRS_SIZE_FIXED, CRect(0,0,rect.Width(),0)) ||
		!m_wndToolBar.LoadToolBar(IDR_MAINFRAME))
	{
		TRACE0("Failed to create toolbar\n");
		return -1;      // fail to create
	}

	// TODO: Delete these three lines if you don't want the toolbar to
	//  be dockable
	m_wndToolBar.EnableDocking(CBRS_ALIGN_ANY);
	EnableDocking(CBRS_ALIGN_ANY);
	DockControlBar(&m_wndToolBar);
	if (NCQ_time_limit==0)
		OnOptionTimelimitDisabled();
	else if (NCQ_time_limit==1)
		OnOptionTimelimit15minutes();
	else if (NCQ_time_limit==2)
		OnOptionTimelimit30minutes();
	else if (NCQ_time_limit==3)
		OnOptionTimelimit1hour();
	else if (NCQ_time_limit==4)
		OnOptionTimelimit2hours();
	return 0;
}

/***********************************************************************
**
**   FUNCTION: PreCreateWindow(CREATESTRUCT& cs)
**
**       Called by the framework before the creation 
**		of the Windows window attached to this CWnd object.
**		Override this member function to modify the 
**		CREATESTRUCT structure before the window is created. 
**
**		Never call this function directly.
**
**
**   INPUT:  CREATESTRUCT& cs
**
**   OUTPUT :   CREATESTRUCT& cs
**   RETURN:    None
**
**********************************************************************/
BOOL CMainFrame::PreCreateWindow(CREATESTRUCT& cs)
{
	if( !CFrameWnd::PreCreateWindow(cs) )
		return FALSE;
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs

	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////
// CMainFrame diagnostics

#ifdef _DEBUG
void CMainFrame::AssertValid() const
{
	CFrameWnd::AssertValid();
}

void CMainFrame::Dump(CDumpContext& dc) const
{
	CFrameWnd::Dump(dc);
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CMainFrame message handlers


/***********************************************************************
c
c   FUNCTION: OnOptionPriorityHigh() 
c         Set thr NCL runnung priority to high
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnOptionPriorityHigh() 
{
	(GetMenu()->GetSubMenu(2))->GetSubMenu(1)->CheckMenuItem(2, MF_CHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(1)->CheckMenuItem(1, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(1)->CheckMenuItem(0, MF_UNCHECKED | MF_BYPOSITION);
	((CNcqView*)NCQ_mainview)->SetPriority(HIGH_PRIORITY_CLASS);	
}

/***********************************************************************
c
c   FUNCTION: OnOptionPriorityLow() 
c         Set thr NCL runnung priority to low
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnOptionPriorityLow() 
{
	(GetMenu()->GetSubMenu(2))->GetSubMenu(1)->CheckMenuItem(0, MF_CHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(1)->CheckMenuItem(1, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(1)->CheckMenuItem(2, MF_UNCHECKED | MF_BYPOSITION);
	((CNcqView*)NCQ_mainview)->SetPriority(IDLE_PRIORITY_CLASS);	
}

/***********************************************************************
c
c   FUNCTION: OnOptionPriorityNormal() 
c         Set thr NCL runnung priority to normal
c
c   INPUT: none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnOptionPriorityNormal() 
{
	(GetMenu()->GetSubMenu(2))->GetSubMenu(1)->CheckMenuItem(1, MF_CHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(1)->CheckMenuItem(2, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(1)->CheckMenuItem(0, MF_UNCHECKED | MF_BYPOSITION);
	((CNcqView*)NCQ_mainview)->SetPriority(NORMAL_PRIORITY_CLASS);	
}

/***********************************************************************
c
c   FUNCTION: OnOptionTimelimit1hour() 
c        Called when menu "1 Hour" under "limit time" called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnOptionTimelimit1hour() 
{
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(0, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(1, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(2, MF_CHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(3, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(4, MF_UNCHECKED | MF_BYPOSITION);
	if (NCQ_mainview!=NULL)
		((CNcqView*)NCQ_mainview)->SetTimeLimit(60);
	NCQ_time_limit = 3;
}

/***********************************************************************
c
c   FUNCTION: OnOptionTimelimit15minutes() 
c         Called when menu "15 Minutes" under "limit time" called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnOptionTimelimit15minutes() 
{
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(0, MF_CHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(1, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(2, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(3, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(4, MF_UNCHECKED | MF_BYPOSITION);
	if (NCQ_mainview!=NULL)
		((CNcqView*)NCQ_mainview)->SetTimeLimit(15);	
	NCQ_time_limit = 1;
}

/***********************************************************************
c
c   FUNCTION: OnOptionTimelimit2hours() 
c         Called when menu "2 Hour" under "limit time" called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnOptionTimelimit2hours() 
{
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(0, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(1, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(2, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(3, MF_CHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(4, MF_UNCHECKED | MF_BYPOSITION);
	if (NCQ_mainview!=NULL)
		((CNcqView*)NCQ_mainview)->SetTimeLimit(120);	
	NCQ_time_limit = 4;
}

/***********************************************************************
c
c   FUNCTION: OnOptionTimelimit30minutes() 
c         Called when menu "30 Minutes" under "limit time" called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnOptionTimelimit30minutes() 
{
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(0, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(1, MF_CHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(2, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(3, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(4, MF_UNCHECKED | MF_BYPOSITION);
	if (NCQ_mainview!=NULL)
		((CNcqView*)NCQ_mainview)->SetTimeLimit(30);
	NCQ_time_limit = 2;
}

/***********************************************************************
c
c   FUNCTION: OnOptionTimelimitDisabled
c         Called when menu "Disabled" under "limit time" called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnOptionTimelimitDisabled() 
{
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(0, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(1, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(2, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(3, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(2))->GetSubMenu(2)->CheckMenuItem(4, MF_CHECKED | MF_BYPOSITION);
	if (NCQ_mainview!=NULL)
		((CNcqView*)NCQ_mainview)->SetTimeLimit(-1);	
	NCQ_time_limit = 0;
}

/***********************************************************************
c
c   FUNCTION: OnOptionCancelAll
c         Called when menu "Cancel All" is called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnOptionCancelCurrent() 
{
	if (NCQ_time_limit==0)
		OnOptionTimelimitDisabled();
	else if (NCQ_time_limit==1)
		OnOptionTimelimit15minutes();
	else if (NCQ_time_limit==2)
		OnOptionTimelimit30minutes();
	else if (NCQ_time_limit==3)
		OnOptionTimelimit1hour();
	else if (NCQ_time_limit==4)
		OnOptionTimelimit2hours();
	((CNcqView*)NCQ_mainview)->OnNcqStop(0);
}

/***********************************************************************
c
c   FUNCTION: OnOptionCancelAll
c         Called when menu "Cancel All" is called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnOptionCancelAll() 
{
	if (NCQ_time_limit==0)
		OnOptionTimelimitDisabled();
	else if (NCQ_time_limit==1)
		OnOptionTimelimit15minutes();
	else if (NCQ_time_limit==2)
		OnOptionTimelimit30minutes();
	else if (NCQ_time_limit==3)
		OnOptionTimelimit1hour();
	else if (NCQ_time_limit==4)
		OnOptionTimelimit2hours();
	((CNcqView*)NCQ_mainview)->OnNcqStop(1);
}

/***********************************************************************
c
c   FUNCTION: OnFileLoadQueue
c         Called when menu "Load Queue" under "File" called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnFileLoadQueue() 
{
	((CNcqView*)NCQ_mainview)->OnFileLoadQueue();	
}
/***********************************************************************
c
c   FUNCTION: OnFileSaveQueue
c         Called when menu "Save Queue" under "File" called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnFileSaveQueue() 
{
	((CNcqView*)NCQ_mainview)->OnFileSaveQueue();	
}
/***********************************************************************
c
c   FUNCTION: OnViewMonitor
c         Called when menu "NCL Monitor" under "View" called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnViewMonitor()
{
	((CNcqView*)NCQ_mainview)->OnViewMonitor();	
}
/***********************************************************************
c
c   FUNCTION: OnClearStatus
c         Called when menu "Clear Status" under "View" called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnClearStatus()
{
	((CNcqView*)NCQ_mainview)->OnClearStatus();	
}
/***********************************************************************
c
c   FUNCTION: OnViewUpd_1second() 
c         Called when menu "1 second" under "Update rate" called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnViewUpd_1second() 
{
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(0, MF_CHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(1, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(2, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(3, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(4, MF_UNCHECKED | MF_BYPOSITION);
	((CNcqView*)NCQ_mainview)->OnViewUpd_rate(1);	
}
/***********************************************************************
c
c   FUNCTION: OnViewUpd_2seconds() 
c         Called when menu "2 seconds" under "Update rate" called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnViewUpd_2seconds() 
{
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(0, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(1, MF_CHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(2, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(3, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(4, MF_UNCHECKED | MF_BYPOSITION);
	((CNcqView*)NCQ_mainview)->OnViewUpd_rate(2);	
}
/***********************************************************************
c
c   FUNCTION: OnViewUpd_3seconds() 
c         Called when menu "3 seconds" under "Update rate" called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnViewUpd_3seconds() 
{
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(0, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(1, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(2, MF_CHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(3, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(4, MF_UNCHECKED | MF_BYPOSITION);
	((CNcqView*)NCQ_mainview)->OnViewUpd_rate(3);	
}
/***********************************************************************
c
c   FUNCTION: OnViewUpd_5seconds() 
c         Called when menu "5 seconds" under "Update rate" called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnViewUpd_5seconds() 
{
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(0, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(1, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(2, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(3, MF_CHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(4, MF_UNCHECKED | MF_BYPOSITION);
	((CNcqView*)NCQ_mainview)->OnViewUpd_rate(5);	
}
/***********************************************************************
c
c   FUNCTION: OnViewUpd_10seconds() 
c         Called when menu "10 seconds" under "Update rate" called
c
c   INPUT:  none
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CMainFrame::OnViewUpd_10seconds() 
{
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(0, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(1, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(2, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(3, MF_UNCHECKED | MF_BYPOSITION);
	(GetMenu()->GetSubMenu(1))->GetSubMenu(3)->CheckMenuItem(4, MF_CHECKED | MF_BYPOSITION);
	((CNcqView*)NCQ_mainview)->OnViewUpd_rate(10);	
}
