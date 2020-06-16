/************************************************************************
c
c   FILE NAME: GLtestDoc.cpp
c
c	 CONTAINS: 
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        GLtestDoc.cpp , 21.1
c     DATE AND TIME OF LAST  MODIFICATION
c        12/10/09 , 18:01:42
c
c**********************************************************************
*/
#include "glStdAfx.h"
#include "GLtest.h"

#include "GLtestDoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// COGDoc

IMPLEMENT_DYNCREATE(COGDoc, CDocument)

BEGIN_MESSAGE_MAP(COGDoc, CDocument)
	//{{AFX_MSG_MAP(COGDoc)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// COGDoc construction/destruction

COGDoc::COGDoc()
{
}

COGDoc::~COGDoc()
{
}

BOOL COGDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;

	return TRUE;
}



/////////////////////////////////////////////////////////////////////////////
// COGDoc serialization

void COGDoc::Serialize(CArchive& ar)
{
	if (ar.IsStoring())
	{
	}
	else
	{
	}
}

/////////////////////////////////////////////////////////////////////////////
// COGDoc diagnostics

#ifdef _DEBUG
void COGDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void COGDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// COGDoc commands
