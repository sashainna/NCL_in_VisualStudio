/************************************************************************
c
c   FILE NAME: GLtestView.h
c
c	 CONTAINS: 
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        GLtestView.h , 25.1
c     DATE AND TIME OF LAST  MODIFICATION
c        04/29/15 , 15:06:05
c
c**********************************************************************
*/
#if !defined TESTOGVIEW_H
#define TESTOGVIEW_H
#pragma once

class COGView : public CView
{
protected:
	COGView();
	DECLARE_DYNCREATE(COGView)
public:
	HGLRC	m_hRC;	
	HDC	m_hDC;		

	COGDoc* GetDocument() { return DYNAMIC_DOWNCAST(COGDoc,m_pDocument); }
	virtual ~COGView();

	void DrawScene();
	
	//{{AFX_VIRTUAL(COGView)
	public:
	virtual void OnDraw(CDC* pDC);
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_VIRTUAL
protected:
	//{{AFX_MSG(COGView)
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnDestroy();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
#endif
