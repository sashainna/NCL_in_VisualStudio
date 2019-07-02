/************************************************************************
c
c   FILE NAME: NclipvView.h
c
c	 CONTAINS: 
c		Header file for CNclipvView class for Nclipv
c
c    COPYRIGHT 2010 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c         nclipvView.h , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c         02/08/16 , 09:15:33
c**********************************************************************/
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_NCLIPVVIEW_H__E6726C16_D96E_4D44_A775_E1132F028917__INCLUDED_)
#define AFX_NCLIPVVIEW_H__E6726C16_D96E_4D44_A775_E1132F028917__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include "wsntbitmap.h"


class CNclipvView : public CView
{
protected: // create from serialization only
	CNclipvView();
	DECLARE_DYNCREATE(CNclipvView)

// Attributes
public:
	CNCLDoc* GetDocument();

	void PrintDIB(CDC* pDC);
	void SaveToBMP(char *filename);
	void SetContext();

	HCURSOR m_current_cursor;
    CPalette*    m_cPalette;
    CPalette    *m_pOldPalette;
	CClientDC	*m_pDC;
	HGLRC	m_hrc;
	CRect		m_oldRect;
	int m_papersize;
	int m_fit, m_bcolor, m_pcenter;

	CPalette*	m_palDIB;
	HDIB		m_hDIB;

// Operations
public:

	void Init();
    BOOL bSetupPixelFormat(void);
	int  inicolormap();
	void SetCursor(HCURSOR cursor);
	char *GetWin();
	void GetDIB();
	void FreeDIB();
	void AddFloatMenu(CPoint pt, char* input_text);
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNclipvView)
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
	virtual ~CNclipvView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

// Generated message map functions
protected:
	//{{AFX_MSG(CNclipvView)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnDestroy();
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnFilePrint();
	afx_msg void OnFilePrintPreview();
	afx_msg LRESULT OnNcHitTest( CPoint point );

	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnMButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnMButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	afx_msg BOOL OnMouseWheel(UINT nFlags, short zDelta, CPoint pt);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in msliteView.cpp
inline CNCLDoc* CNclipvView::GetDocument()
   { return (CNCLDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MSLITEVIEW_H__E6726C16_D96E_4D44_A775_E1132F028917__INCLUDED_)
