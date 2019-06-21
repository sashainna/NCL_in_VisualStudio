/************************************************************************
c
c   FILE NAME: msliteView.h
c
c	 CONTAINS: 
c		Header file for CMsliteView class for MSLITE
c
c    COPYRIGHT 2008 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c         msliteView.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c         04/29/15 , 15:06:33
c**********************************************************************/
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_MSLITEVIEW_H__E6726C16_D96E_4D44_A775_E1132F028917__INCLUDED_)
#define AFX_MSLITEVIEW_H__E6726C16_D96E_4D44_A775_E1132F028917__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include "wsntbitmap.h"


class CMsliteView : public CView
{
protected: // create from serialization only
	CMsliteView();
	DECLARE_DYNCREATE(CMsliteView)

// Attributes
public:
	CNCLDoc* GetDocument();

	void PrintDIB(CDC* pDC);
	void SetContext();

	HCURSOR m_current_cursor;
    CPalette*    m_cPalette;
    CPalette    *m_pOldPalette;
	CClientDC	*m_pDC;
	HGLRC	m_hrc;
	CRect		m_oldRect;

	CPalette*	m_palDIB;
	HDIB		m_hDIB;

// Operations
public:

	void Init();
    BOOL bSetupPixelFormat(void);
	int  inicolormap();
	void SetCursor(HCURSOR cursor);
	char *GetWin();
	void AddFloatMenu(CPoint pt, char* input_text);
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMsliteView)
	public:
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	protected:
	virtual BOOL OnPreparePrinting(CPrintInfo* pInfo);
	virtual void OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnEndPrinting(CDC* pDC, CPrintInfo* pInfo);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CMsliteView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	void GetDIB();
	void FreeDIB();

// Generated message map functions
protected:
	//{{AFX_MSG(CMsliteView)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnDestroy();
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnFilePrint();
	afx_msg void OnFilePrintPreview();
	afx_msg LRESULT OnNcHitTest( CPoint point );
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in msliteView.cpp
inline CNCLDoc* CMsliteView::GetDocument()
   { return (CNCLDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MSLITEVIEW_H__E6726C16_D96E_4D44_A775_E1132F028917__INCLUDED_)
