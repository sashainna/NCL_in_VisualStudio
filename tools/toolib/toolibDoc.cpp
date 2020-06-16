/************************************************************************
c
c   FILE NAME: toolibDoc.cpp
c
c	 CONTAINS: 
c		Functions for the class CToolibDoc
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       toolibDoc.cpp , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       10/12/15 , 17:29:20
c
c**********************************************************************
*/
// toolibDoc.cpp : implementation of the CToolibDoc class
//

#include "toolibstdafx.h"
// SHARED_HANDLERS can be defined in an ATL project implementing preview, thumbnail
// and search filter handlers and allows sharing of document code with that project.
#ifndef SHARED_HANDLERS
#include "toolib.h"
#endif
#include "toolibDoc.h"
#include <propkey.h>
#ifdef _DEBUG
#define new DEBUG_NEW
#endif

// CToolibDoc

IMPLEMENT_DYNCREATE(CToolibDoc, CDocument)

BEGIN_MESSAGE_MAP(CToolibDoc, CDocument)
END_MESSAGE_MAP()


// CToolibDoc construction/destruction

CToolibDoc::CToolibDoc()
{
	// TODO: add one-time construction code here

}

CToolibDoc::~CToolibDoc()
{
}

BOOL CToolibDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;

	// TODO: add reinitialization code here
	// (SDI documents will reuse this document)

	m_cuttype = 1;
	m_tool = "";
	m_desp = "";
	m_def1 = "";
	m_def2 = "";
	m_def3 = "";
	m_def4 = "";
	m_def5 = "";
	m_def6 = "";
	m_parm1 = "";
	m_parm2 = "";
	m_parm3 = "";
	m_parm4 = "";
	m_parm5 = "";
	m_parm6 = "";
	m_cdef1 = "";
	m_cdef2 = "";
	m_cdef3 = "";
	m_cdef4 = "";
	m_cdef5 = "";
	m_cdef6 = "";
	m_cparm1 = "";
	m_cparm2 = "";
	m_cparm3 = "";
	m_cparm4 = "";
	m_cparm5 = "";
	m_cparm6 = "";
	m_def_cut = 0;

	m_segment = 0;
	m_moving = 0;
	m_shade = 0;
	m_loadcom = 0;
	m_symbol_ck = 0;
	m_shank_ck = 0;
	m_holder_ck = 0;
	m_symbol = "";
	m_holder = "";
	m_shank = "";
	m_tooldraw = "";
	for (int i=0; i<41; i++)
		m_com[i] = "";
	symbol_value.m_class = "All";
	symbol_value.m_value1 = 0.0;
	symbol_value.m_value2 = 0.0;
	symbol_value.m_value3 = 0.0;
	symbol_value.m_value4 = 0.0;
	symbol_value.m_parm1 = "";
	symbol_value.m_parm2 = "";
	symbol_value.m_parm3 = "";
	symbol_value.m_parm4 = "";
	symbol_value.m_symbol = "";
	symbol_value.m_shade = "Off";

	shank_value.m_class = "All";
	shank_value.m_value1 = 0.0;
	shank_value.m_value2 = 0.0;
	shank_value.m_value3 = 0.0;
	shank_value.m_value4 = 0.0;
	shank_value.m_parm1 = "";
	shank_value.m_parm2 = "";
	shank_value.m_parm3 = "";
	shank_value.m_parm4 = "";
	shank_value.m_symbol = "";
	shank_value.m_shade = "Off";

	holder_value.m_class = "All";
	holder_value.m_value1 = 0.0;
	holder_value.m_value2 = 0.0;
	holder_value.m_value3 = 0.0;
	holder_value.m_value4 = 0.0;
	holder_value.m_parm1 = "";
	holder_value.m_parm2 = "";
	holder_value.m_parm3 = "";
	holder_value.m_parm4 = "";
	holder_value.m_symbol = "";
	holder_value.m_shade = "Off";
	return TRUE;
}




// CToolibDoc serialization

void CToolibDoc::Serialize(CArchive& ar)
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
// CToolibDoc diagnostics

#ifdef _DEBUG
void CToolibDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void CToolibDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG


// CToolibDoc commands
