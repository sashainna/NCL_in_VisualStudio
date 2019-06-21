/********************************************************************* 
**  NAME:  wsntdoc.cpp
**
**			implementation of CNCLDoc class functions
**			This class is necessary for MFC Frame work
**			and is create by MFC wizard, but for NCL
**			we don't need do anything about it now
**
**		CONTAINS: CNCLDoc class functions
**			all functions declared in wsntdoc.h
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntdoc.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:22
*********************************************************************/

#include "wsntstdafx.h"
#include "wsntdoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CNCLDoc

IMPLEMENT_DYNCREATE(CNCLDoc, CDocument)

BEGIN_MESSAGE_MAP(CNCLDoc, CDocument)
	//{{AFX_MSG_MAP(CNCLDoc)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNCLDoc construction/destruction

CNCLDoc::CNCLDoc()
{
//	m_bAutoDelete = FALSE;
	m_bAutoDelete = TRUE;
}

CNCLDoc::~CNCLDoc()
{
}

BOOL CNCLDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;

	// TODO: add reinitialization code here
	// (SDI documents will reuse this document)

	return TRUE;
}



/////////////////////////////////////////////////////////////////////////////
// CNCLDoc serialization

void CNCLDoc::Serialize(CArchive& ar)
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
// CNCLDoc diagnostics

#ifdef _DEBUG
void CNCLDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void CNCLDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CNCLDoc commands
