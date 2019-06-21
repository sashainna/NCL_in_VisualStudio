/************************************************************************
**
**   FILE NAME: wsntfrmdoc.cpp
**
**	 Description - Functions and implementations for
**		CNCLFormDoc class
**
**	 CONTAINS: 
**		class functions of CNCLFormDoc class
**
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntfrmdoc.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:26
***********************************************************************
*/
#include "stdafx.h"
#include "wsntctl.h"
#include "wsntformdoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CNCLFormDoc

IMPLEMENT_DYNCREATE(CNCLFormDoc, CDocument)

BEGIN_MESSAGE_MAP(CNCLFormDoc, CDocument)
	//{{AFX_MSG_MAP(CNCLFormDoc)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNCLFormDoc construction/destruction

CNCLFormDoc::CNCLFormDoc()
{
}

CNCLFormDoc::~CNCLFormDoc()
{
}

BOOL CNCLFormDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;
	return TRUE;
}



/////////////////////////////////////////////////////////////////////////////
// CNCLFormDoc serialization

void CNCLFormDoc::Serialize(CArchive& ar)
{
}

/////////////////////////////////////////////////////////////////////////////
// CNCLFormDoc diagnostics

#ifdef _DEBUG
void CNCLFormDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void CNCLFormDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CNCLFormDoc commands
