/************************************************************************
**
**   FILE NAME: NcqView.h
**	  
**   CONTAINS:
**		interface of the CMainFrame class
**
**     COPYRIGHT 2003 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**    MODULE NAME AND RELEASE LEVEL
**       ncqview.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:39
**
************************************************************************
*/// ncqView.h : interface of the CNcqView class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_NCQVIEW_H__8777012F_16D5_11D7_9C47_00C04F336F5E__INCLUDED_)
#define AFX_NCQVIEW_H__8777012F_16D5_11D7_9C47_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "NcqMonitor.h"

class CNcqView : public CFormView
{
protected: // create from serialization only
	CNcqView();
	DECLARE_DYNCREATE(CNcqView)

public:
	int m_selchg;
	NcqMonitor* m_monitor;
	//{{AFX_DATA(CNcqView)
	enum{ IDD = IDD_NCQ_FORM };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

// Attributes
public:
	CNcqDoc* GetDocument();

// Operations
public:

	void ListSelchange();
	void Display_status_message(char *msg);
	BOOL OnGetNCLInfo();
	BOOL OnGetNCLmsg();
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNcqView)
	public:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	protected:
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void OnInitialUpdate(); // called first time after construct
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CNcqView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	void OnNclExit();
	void On_ncq_add();
	void SetTimeLimit(int minutes) { m_runtime = 60*minutes*1000; }
	void SetPriority (DWORD priority) { m_priority = priority; }
	void OnFileLoadQueue();
	void OnFileSaveQueue();
	void OnViewMonitor();
	void OnClearStatus();
	void OnViewUpd_rate(int rate);
	void resize_fields();
	void OnNcqStop(int type);

protected:
	CBrush* m_pEditBkBrush, *m_pEditBkBrush2;
	BOOL m_run;
	DWORD m_priority;
	int m_runtime, m_montime;
	char m_current_pp[256];
	POINT m_size;
	int m_init;
	CRect m_wndrc, m_rect[7];

// Generated message map functions
protected:
	//{{AFX_MSG(CNcqView)
	afx_msg void OnTimer(UINT_PTR nIDEvent);
	afx_msg void OnAppAbout();
	afx_msg void OnAppExit();
	afx_msg void OnEditDelete();
	afx_msg void OnFileOpen();
	afx_msg void OnOptionOptions();
	afx_msg void OnViewUpdate();
	afx_msg void OnNcqRun();
	afx_msg void OnSelchangeNcqList();
	afx_msg void OnChangeFilename();
	afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
	afx_msg void OnDropFiles(HDROP hDropInfo);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in ncqView.cpp
inline CNcqDoc* CNcqView::GetDocument()
   { return (CNcqDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NCQVIEW_H__8777012F_16D5_11D7_9C47_00C04F336F5E__INCLUDED_)
