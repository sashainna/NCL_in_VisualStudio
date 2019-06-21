/************************************************************************
c
c   FILE NAME: wsntFdsnView.cpp
c
c	 CONTAINS: 
c		Functions for the class CNCLFdsnView
c
c     COPYRIGHT 2014 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c         wsntFdsnView.cpp , 25.1
c      DATE AND TIME OF LAST  MODIFICATION
c         04/29/15 , 15:12:18
c
c**********************************************************************
*/
#include "stdafx.h"
#include "wsntctl.h"
#include "wsntformDoc.h"
#include "wsntFdsnView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CNCLFdsnView

IMPLEMENT_DYNCREATE(CNCLFdsnView, CView)

BEGIN_MESSAGE_MAP(CNCLFdsnView, CView)
	//{{AFX_MSG_MAP(CNCLFdsnView)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNCLFdsnView construction/destruction

CNCLFdsnView::CNCLFdsnView()
{
	// TODO: add construction code here

}

CNCLFdsnView::~CNCLFdsnView()
{
}

BOOL CNCLFdsnView::PreCreateWindow(CREATESTRUCT& cs)
{
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs

	return CView::PreCreateWindow(cs);
}

/////////////////////////////////////////////////////////////////////////////
// CNCLFdsnView drawing

void CNCLFdsnView::OnDraw(CDC* pDC)
{
//	CNCLFormDoc* pDoc = GetDocument();
//	ASSERT_VALID(pDoc);

	// TODO: add draw code for native data here
}

/////////////////////////////////////////////////////////////////////////////
// CNCLFdsnView diagnostics

#ifdef _DEBUG
void CNCLFdsnView::AssertValid() const
{
	CView::AssertValid();
}

void CNCLFdsnView::Dump(CDumpContext& dc) const
{
	CView::Dump(dc);
}

CNCLFormDoc* CNCLFdsnView::GetDocument() // non-debug version is inline
{
//	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CNCLFormDoc)));
	return (CNCLFormDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CNCLFdsnView message handlers
