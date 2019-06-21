/************************************************************************
c
c   FILE NAME: GenpromptView.h
c
c	 CONTAINS: 
c		definitions of CGenpromptView
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        genpromptView.h , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        05/05/14 , 14:46:36
c
c**********************************************************************
*/
#if !defined(AFX_GENPROMPTVIEW_H__E4E0B13F_31D5_11D6_90AB_00C04F336F5E__INCLUDED_)
#define AFX_GENPROMPTVIEW_H__E4E0B13F_31D5_11D6_90AB_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


class CGenpromptView : public CFormView
{
protected: // create from serialization only
	CGenpromptView();
	DECLARE_DYNCREATE(CGenpromptView)

public:
	//{{AFX_DATA(CGenpromptView)
	enum{ IDD = IDD_GENPROMPT_DIALOG };
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

// Attributes
public:
	CPostworksDoc* GetDocument();

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CGenpromptView)
	public:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	virtual void OnInitialUpdate(); // called first time after construct
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CGenpromptView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	CBrush* m_pEditBkBrush;

// Generated message map functions
protected:
	//{{AFX_MSG(CGenpromptView)
		// NOTE - the ClassWizard will add and remove member functions here.
		afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
		afx_msg void OnFileExit();
		afx_msg void OnFileOpen();
		afx_msg void OnGenpromptRun();
		afx_msg void OnHelpContents();
		afx_msg void OnHelpAbout();
		afx_msg void OnDropFiles(HDROP hDropInfo);
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in GenpromptView.cpp
inline CPostworksDoc* CGenpromptView::GetDocument()
   { return (CPostworksDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_GenpromptView_H__E4E0B13F_31D5_11D6_90AB_00C04F336F5E__INCLUDED_)
