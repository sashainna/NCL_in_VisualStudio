/************************************************************************
**
**   FILE NAME: NcqMainFrm.h
**	  
**   CONTAINS:
**		interface of the CMainFrame class
**
**     COPYRIGHT 2003 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**    MODULE NAME AND RELEASE LEVEL
**       ncqmainfrm.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:39
**
************************************************************************
*/// MainFrm.h : interface of the CMainFrame class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_MAINFRM_H__8777012B_16D5_11D7_9C47_00C04F336F5E__INCLUDED_)
#define AFX_MAINFRM_H__8777012B_16D5_11D7_9C47_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CMainFrame : public CFrameWnd
{
	
protected: // create from serialization only
	CMainFrame();
	DECLARE_DYNCREATE(CMainFrame)

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMainFrame)
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CMainFrame();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:  // control bar embedded members
	CToolBar    m_wndToolBar;

// Generated message map functions
protected:
	//{{AFX_MSG(CMainFrame)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnNclExit(WPARAM wParam,LPARAM lParam);
	afx_msg void OnNclUpdate(WPARAM wParam,LPARAM lParam);
	afx_msg void OnOptionPriorityHigh();
	afx_msg void OnOptionPriorityLow();
	afx_msg void OnOptionPriorityNormal();
	afx_msg void OnOptionTimelimit1hour();
	afx_msg void OnOptionTimelimit15minutes();
	afx_msg void OnOptionTimelimit2hours();
	afx_msg void OnOptionTimelimit30minutes();
	afx_msg void OnOptionTimelimitDisabled();
	afx_msg void OnOptionCancelCurrent();
	afx_msg void OnOptionCancelAll();
	afx_msg void OnFileLoadQueue();
	afx_msg void OnFileSaveQueue();
	afx_msg void OnViewMonitor();	
	afx_msg void OnClearStatus();	
	afx_msg void OnViewUpd_1second();	
	afx_msg void OnViewUpd_2seconds();	
	afx_msg void OnViewUpd_3seconds();	
	afx_msg void OnViewUpd_5seconds();	
	afx_msg void OnViewUpd_10seconds();	
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MAINFRM_H__8777012B_16D5_11D7_9C47_00C04F336F5E__INCLUDED_)
