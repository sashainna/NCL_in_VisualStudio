/************************************************************************
c
c   FILE NAME: PWMainFrm.h
c
c	 CONTAINS: 
c		definitions of CPWMainFrm
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PWMainFrm.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:27
c
c**********************************************************************
*/
#if !defined(AFX_MAINFRM_H__AECAB57E_3053_11D6_90AB_00C04F336F5E__INCLUDED_)
#define AFX_MAINFRM_H__AECAB57E_3053_11D6_90AB_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CPWToolBar : public CToolBar
{
	DECLARE_DYNAMIC(CPWToolBar)
public:
	CPWToolBar();
protected:
	//{{AFX_MSG(CPWToolBar)
	afx_msg void OnLButtonDblClk(UINT nFlags, CPoint pt);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint pt);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

class CPWMainFrame : public CFrameWnd
{
	
protected: // create from serialization only
	CPWMainFrame();
	DECLARE_DYNCREATE(CPWMainFrame)

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPWMainFrame)
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CPWMainFrame();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:  // control bar embedded members
	CPWToolBar    m_wndToolBar;

// Generated message map functions
protected:
	//{{AFX_MSG(CPWMainFrame)
		virtual void OnClose();
		afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MAINFRM_H__AECAB57E_3053_11D6_90AB_00C04F336F5E__INCLUDED_)
