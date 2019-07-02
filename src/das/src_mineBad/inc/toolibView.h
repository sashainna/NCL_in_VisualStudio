
/************************************************************************
c
c   FILE NAME: toolibView.h
c
c	 CONTAINS: 
c		Header file all class CToolibView
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       toolibView.h , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       10/12/15 , 17:19:05
c
c**********************************************************************
*/
// toolibView.h : interface of the CToolibView class
//
/////////////////////////////////////////////////////////////////////////////
#pragma once

class CToolibView : public CView
{
protected: // create from serialization only
	CToolibView();
	DECLARE_DYNCREATE(CToolibView)

// Attributes
public:
	CToolibDoc* GetDocument() const;

// Operations
public:

// Overrides
public:
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
protected:

// Implementation
public:
	virtual ~CToolibView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

// Generated message map functions
protected:
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in toolibView.cpp
inline CToolibDoc* CToolibView::GetDocument() const
   { return reinterpret_cast<CToolibDoc*>(m_pDocument); }
#endif

