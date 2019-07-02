/************************************************************************
c
c   FILE NAME: testMainFrame.h
c
c	 CONTAINS: 
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        testMainFrm.h , 25.1
c     DATE AND TIME OF LAST  MODIFICATION
c        04/29/15 , 15:06:53
c
c**********************************************************************
*/
#if !defined TESTOGMAINFRAME_H
#define TESTOGMAINFRAME_H
#pragma once

class CMainFrame : public CFrameWnd
{
protected:
	CStatusBar  m_wndStatusBar;
	CToolBar    m_wndToolBar;

	CMainFrame();
	//{{AFX_MSG(CMainFrame)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	//}}AFX_MSG
	DECLARE_DYNCREATE(CMainFrame)
public:
	virtual ~CMainFrame();
	//{{AFX_VIRTUAL(CMainFrame)
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_VIRTUAL

	DECLARE_MESSAGE_MAP()
};
//{{AFX_INSERT_LOCATION}}
#endif
