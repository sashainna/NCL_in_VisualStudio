/************************************************************************
**
**   FILE NAME: ncqDoc.cpp
**			implementation of CNcqDoc class functions
**			This class is necessary for MFC Frame work
**			and is create by MFC wizard, but for NCQ
**			we don't need do anything about it now
**
**		CONTAINS: CNcqDoc class functions
**			all functions declared in ncqdoc.h
**
**     COPYRIGHT 2003 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**    MODULE NAME AND RELEASE LEVEL
**       ncqdoc.cpp , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:13:03
**
************************************************************************
*/
// ncqDoc.cpp : implementation of the CNcqDoc class
//

#include "stdafx.h"
#include "ncq.h"

#include "ncqDoc.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CNcqDoc

IMPLEMENT_DYNCREATE(CNcqDoc, CDocument)

BEGIN_MESSAGE_MAP(CNcqDoc, CDocument)
	//{{AFX_MSG_MAP(CNcqDoc)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CNcqDoc construction/destruction

CNcqDoc::CNcqDoc()
{
	// TODO: add one-time construction code here

}

CNcqDoc::~CNcqDoc()
{
}

BOOL CNcqDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;

	// TODO: add reinitialization code here
	// (SDI documents will reuse this document)

	return TRUE;
}



/////////////////////////////////////////////////////////////////////////////
// CNcqDoc serialization

void CNcqDoc::Serialize(CArchive& ar)
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
// CNcqDoc diagnostics

#ifdef _DEBUG
void CNcqDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void CNcqDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CNcqDoc commands
