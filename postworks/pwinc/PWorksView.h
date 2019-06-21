/************************************************************************
c
c   FILE NAME: PworksView.h
c
c	 CONTAINS: 
c		definitions of CPworksView
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PWorksView.h , 24.3
c     DATE AND TIME OF LAST  MODIFICATION
c        05/05/14 , 14:40:04
c
c**********************************************************************
*/
#if !defined(AFX_PWORKSVIEW_H__AECAB582_3053_11D6_90AB_00C04F336F5E__INCLUDED_)
#define AFX_PWORKSVIEW_H__AECAB582_3053_11D6_90AB_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


class CPWorksView : public CFormView
{
protected: // create from serialization only
	char m_DirName[512];
	CPWorksView();
	DECLARE_DYNCREATE(CPWorksView)

public:
	//{{AFX_DATA(CPWorksView)
	enum{ IDD = IDD_PWORKS_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

// Attributes
public:
	CPostworksDoc* GetDocument();

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPWorksView)
	public:
	afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void OnInitialUpdate(); // called first time after construct
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CPWorksView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	CBrush* m_pEditBkBrush;

// Generated message map functions
protected:
	//{{AFX_MSG(CPWorksView)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
		afx_msg void OnFileExit();
		afx_msg void OnFileOpen();
		afx_msg void OnOptionOptions();
		afx_msg void OnPworksRun();
		afx_msg void OnHelpContents();
		afx_msg void OnHelpAbout();
		afx_msg void OnDropFiles(HDROP hDropInfo);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in PWorksView.cpp
inline CPostworksDoc* CPWorksView::GetDocument()
   { return (CPostworksDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PWORKSVIEW_H__AECAB582_3053_11D6_90AB_00C04F336F5E__INCLUDED_)
