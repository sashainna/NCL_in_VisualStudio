/************************************************************************
c
c   FILE NAME: testMainFrame.c
c
c	 CONTAINS: 
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        testMainFrm.cpp , 21.1
c     DATE AND TIME OF LAST  MODIFICATION
c        12/10/09 , 18:01:43
c
c**********************************************************************
*/
#include "glStdAfx.h"
#include "GLtest.h"

#include "testMainFrm.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

IMPLEMENT_DYNCREATE(CMainFrame, CFrameWnd)

BEGIN_MESSAGE_MAP(CMainFrame, CFrameWnd)
	//{{AFX_MSG_MAP(CMainFrame)
	ON_WM_CREATE()
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

CMainFrame::CMainFrame(){}
CMainFrame::~CMainFrame(){}

int CMainFrame::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
	if (CFrameWnd::OnCreate(lpCreateStruct) == -1)
		return -1;
	return 0;
}

BOOL CMainFrame::PreCreateWindow(CREATESTRUCT& cs)
{
	if( !CFrameWnd::PreCreateWindow(cs) )
		return FALSE;
	cs.x = 20;
	cs.y = 20;
	cs.cx = 950;
	cs.cy = 700;
	return TRUE;
}
