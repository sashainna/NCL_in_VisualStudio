/***********************************************************************
**
**   FILE NAME: PWMainFrame.cpp
**
**   CONTAINS:
**		CPWMainFrame::CPWMainFrame()
**		CPWMainFrame::~CPWMainFrame()
**		 CPWMainFrame::PreCreateWindow()
**		CPWMainFrame::OnCreate()
**
**    COPYRIGHT 2002(c) Numerical Control Computer Sciences.
**          All Rights Reserved
**     MODULE NAME AND RELEASE LEVEL
**			PWMainFrm.cpp , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**			09/11/13 , 12:58:20      
**
**********************************************************************/
#include "pwstdafx.h"
#include "PWwindef.h"

#include "PWMainFrm.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif
#define _countof(array) (sizeof(array)/sizeof(array[0]))
//int AFXAPI AfxLoadString(UINT nIDS, LPTSTR lpszBuf, UINT nMaxBuf = 256);

IMPLEMENT_DYNAMIC(CPWToolBar,CToolBar)

BEGIN_MESSAGE_MAP(CPWToolBar,CToolBar)
	//{{AFX_MSG_MAP(CPWToolBar)
	ON_WM_LBUTTONDBLCLK()
	ON_WM_LBUTTONDOWN()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

CPWToolBar::CPWToolBar()
{
}

void CPWToolBar::OnLButtonDown(UINT nFlags, CPoint pt)
{
	CWnd::OnLButtonDown(nFlags, pt);
}
/***********************************************************************
**
**   FUNCTION: OnLButtonDblClk(UINT nFlags, CPoint pt)
**		overwrite virtual function just for doing when double click on postworks icon bar
**
**   INPUT:  nFlags:
**				pt:
**
**   OUTPUT :   None
**   RETURN:    None
**
**********************************************************************/
void CPWToolBar::OnLButtonDblClk(UINT nFlags, CPoint pt)
{
	CWnd::OnLButtonDblClk(nFlags, pt);
}

/////////////////////////////////////////////////////////////////////////////
// CPWMainFrame

IMPLEMENT_DYNCREATE(CPWMainFrame, CFrameWnd)

BEGIN_MESSAGE_MAP(CPWMainFrame, CFrameWnd)
	//{{AFX_MSG_MAP(CPWMainFrame)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code !
		ON_WM_CLOSE()
		ON_WM_CREATE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPWMainFrame construction/destruction

CPWMainFrame::CPWMainFrame()
{
	// TODO: add member initialization code here
	
}

CPWMainFrame::~CPWMainFrame()
{
}

/***********************************************************************
c
c   FUNCTION: OnClose()
c
c           callback for close frame window
c
c   INPUT:  None
c
c   OUTPUT :   None
c   RETURN:    None
c
**********************************************************************/
void CPWMainFrame::OnClose()
{
	PostMessage(WM_QUIT);
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
int CPWMainFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;
	CRect rect;
	GetWindowRect(&rect);
	if (!m_wndToolBar.CreateEx(this, TBSTYLE_FLAT, WS_CHILD | WS_VISIBLE | CBRS_TOP | CBRS_SIZE_FIXED | CBRS_TOOLTIPS, CRect(0,0,rect.Width(),0 ))||
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
BOOL CPWMainFrame::PreCreateWindow(CREATESTRUCT& cs)
{
	if( !CFrameWnd::PreCreateWindow(cs) )
		return FALSE;	
	cs.style = WS_MINIMIZEBOX | WS_OVERLAPPED | WS_SYSMENU | WS_BORDER;

	return TRUE;
}


/////////////////////////////////////////////////////////////////////////////
// CPWMainFrame diagnostics

#ifdef _DEBUG
void CPWMainFrame::AssertValid() const
{
	CFrameWnd::AssertValid();
}

void CPWMainFrame::Dump(CDumpContext& dc) const
{
	CFrameWnd::Dump(dc);
}

#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CPWMainFrame message handlers

