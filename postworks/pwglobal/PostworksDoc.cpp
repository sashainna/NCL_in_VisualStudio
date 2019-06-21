/***********************************************************************
**
**   FILE NAME: PostworksDoc.cpp
**
**   CONTAINS:
**		CPostworksDoc::CPostworksDoc()
**		CPostworksDoc::~CPostworksDoc()
**		 CPostworksDoc::OnNewDocument()
**		CPostworksDoc::Serialize()
**
**		Those function are not directly called, they are called by MFC
**
**    COPYRIGHT 2002(c) Numerical Control Computer Sciences.
**          All Rights Reserved
**     MODULE NAME AND RELEASE LEVEL
**			PostworksDoc.cpp , 24.1
**    DATE AND TIME OF LAST  MODIFICATION
**			09/11/13 , 12:58:20      
**
**********************************************************************/
#include "pwstdafx.h"
#include "PWorks.h"

#include "PostworksDoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CPostworksDoc

IMPLEMENT_DYNCREATE(CPostworksDoc, CDocument)

BEGIN_MESSAGE_MAP(CPostworksDoc, CDocument)
	//{{AFX_MSG_MAP(CPostworksDoc)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CPostworksDoc construction/destruction

CPostworksDoc::CPostworksDoc()
{
	// TODO: add one-time construction code here

}

CPostworksDoc::~CPostworksDoc()
{
}

BOOL CPostworksDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;

	// TODO: add reinitialization code here
	// (SDI documents will reuse this document)

	return TRUE;
}



/////////////////////////////////////////////////////////////////////////////
// CPostworksDoc serialization

void CPostworksDoc::Serialize(CArchive& ar)
{
	if (ar.IsStoring())
	{
		// TODO: add storing code here
	}
	else
	{
		// TODO: add loading code here
	}
}

/////////////////////////////////////////////////////////////////////////////
// CPostworksDoc diagnostics

#ifdef _DEBUG
void CPostworksDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void CPostworksDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CPostworksDoc commands
