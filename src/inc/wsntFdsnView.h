/************************************************************************
c
c   FILE NAME: wsntFdsnView.h
c
c	 CONTAINS: 
c		Header file all class CNCLFdsnView
c
c     COPYRIGHT 2014 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c         wsntFdsnView.h , 25.1
c      DATE AND TIME OF LAST  MODIFICATION
c         04/29/15 , 15:07:14
c
c**********************************************************************
*/
/////////////////////////////////////////////////////////////////////////////

#if !defined(WSNTFDSNVIEW_INCLUDED_)
#define WSNTFDSNVIEW_INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "wsntformdoc.h"

class CNCLFdsnView : public CView
{
protected: // create from serialization only
	CNCLFdsnView();
	DECLARE_DYNCREATE(CNCLFdsnView)

// Attributes
public:
	CNCLFormDoc* GetDocument();

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLFdsnView)
	public:
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	protected:
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CNCLFdsnView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

// Generated message map functions
protected:
	//{{AFX_MSG(CNCLFdsnView)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  
inline CNCLFormDoc* CNCLFdsnView::GetDocument()
   { return (CNCLFormDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif
