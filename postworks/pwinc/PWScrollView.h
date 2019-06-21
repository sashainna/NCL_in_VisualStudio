/************************************************************************
c
c   FILE NAME: PWScrollView.h
c
c	 Description - Functions and struct declarations for
c		PWScrollView class (ScrollView)
c     COPYRIGHT 2001 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PWScrollView.h , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        05/05/14 , 14:37:09
c
c**********************************************************************
*/
#ifndef PWSCROLLVIEW_H
#define PWSCROLLVIEW_H

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
//
#include "NpwHeaders.h"
#include "dialogitem.h"

/////////////////////////////////////////////////////////////////////////////

class CPWScrollView : public CScrollView
{
protected:
	CPWScrollView();           // protected constructor used by dynamic creation
	DECLARE_DYNCREATE(CPWScrollView)

	DLGTEMPLATE m_dlgTempl;
	CDialogItem	m_rgDlgItem[200]; 
	int m_create;
	CWnd* m_parent;
// Attributes
public:
	virtual ~CPWScrollView();
	int SetDlgTemp(DLGTEMPLATE *dlgTempl,  CDialogItem dlgItem[200]);
	BOOL CreateIndirect(CWnd* pParentWnd, DLGTEMPLATE *dlgTempl, CDialogItem rgDlgItem[20], int itemnum);
	BOOL CreateDlgView(CWnd *parent);
	BOOL initview(NpwDynWinStruct DynWinStruct);
// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPWScrollView)
	protected:
	virtual void OnDraw(CDC* pDC);      // overridden to draw this view
	virtual void OnInitialUpdate();     // first time after construct
	virtual void OnUpdate( CView* pSender, LPARAM lHint, CObject* pHint );
	afx_msg void OnPaint();
	afx_msg void OnChoicePicked(UINT id);
	afx_msg void FormUserCallbacks1(UINT id);
	afx_msg void FormUserCallbacks2(UINT id);
	afx_msg void FormUserCallbacks3(UINT id);
	afx_msg void FormUserCallbacks4(UINT id);
	afx_msg void OnFormTabbed();
	afx_msg void OnFormSTabbed();
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual BOOL Create(LPCTSTR, LPCTSTR, DWORD,
		const RECT&, CWnd*, UINT, CCreateContext*);
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	// Generated message map functions
	//{{AFX_MSG(CPWScrollView)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif 
