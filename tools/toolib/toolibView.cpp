/************************************************************************
c
c   FILE NAME: toolibView.cpp
c
c   CONTAINS:
c     Functions for the class toolibView
c
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c       toolibView.cpp , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       10/12/15 , 17:31:17
c
c**********************************************************************
*/
// toolibView.cpp : implementation of the CToolibView class
//

#include "toolibstdafx.h"
// SHARED_HANDLERS can be defined in an ATL project implementing preview, thumbnail
// and search filter handlers and allows sharing of document code with that project.
#ifndef SHARED_HANDLERS
#include "toolib.h"
#endif

#include "toolibDoc.h"
#include "toolibView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif


// CToolibView

IMPLEMENT_DYNCREATE(CToolibView, CView)

BEGIN_MESSAGE_MAP(CToolibView, CView)
END_MESSAGE_MAP()

// CToolibView construction/destruction

CToolibView::CToolibView()
{
	// TODO: add construction code here

}

CToolibView::~CToolibView()
{
}

BOOL CToolibView::PreCreateWindow(CREATESTRUCT& cs)
{
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs

	return CView::PreCreateWindow(cs);
}

// CToolibView drawing

void CToolibView::OnDraw(CDC* /*pDC*/)
{
	CToolibDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);
	if (!pDoc)
		return;

	// TODO: add draw code for native data here
}
// CToolibView diagnostics

#ifdef _DEBUG
void CToolibView::AssertValid() const
{
	CView::AssertValid();
}

void CToolibView::Dump(CDumpContext& dc) const
{
	CView::Dump(dc);
}

CToolibDoc* CToolibView::GetDocument() const // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CToolibDoc)));
	return (CToolibDoc*)m_pDocument;
}
#endif //_DEBUG


// CToolibView message handlers
