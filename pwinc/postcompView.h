/************************************************************************
c
c   FILE NAME: PostcompView.h
c
c	 CONTAINS: 
c		definitions of CPostcompView
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        postcompView.h , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        05/05/14 , 14:52:58
c
c**********************************************************************
*/
#if !defined(AFX_POSTCOMPVIEW_H__08672A2F_32AF_11D6_90AB_00C04F336F5E__INCLUDED_)
#define AFX_POSTCOMPVIEW_H__08672A2F_32AF_11D6_90AB_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


class CPostcompView : public CFormView
{
protected: // create from serialization only
	CPostcompView();
	DECLARE_DYNCREATE(CPostcompView)

public:
	//{{AFX_DATA(CPostcompView)
	enum{ IDD = IDD_POSTCOMP_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

// Attributes
public:
	CPostworksDoc* GetDocument();

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPostcompView)
	public:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void OnInitialUpdate(); // called first time after construct
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CPostcompView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	CBrush* m_pEditBkBrush;

// Generated message map functions
protected:
	//{{AFX_MSG(CPostcompView)
		// NOTE - the ClassWizard will add and remove member functions here.
		afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
		afx_msg void OnPostcompRun();
		afx_msg void OnFileOpen();
		afx_msg void OnFileExit();
		afx_msg void OnOptionOptions();
		afx_msg void OnHelpContents();
		afx_msg void OnHelpAbout();
		afx_msg void OnDropFiles(HDROP hDropInfo);
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in postcompView.cpp
inline CPostworksDoc* CPostcompView::GetDocument()
   { return (CPostworksDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_POSTCOMPVIEW_H__08672A2F_32AF_11D6_90AB_00C04F336F5E__INCLUDED_)
