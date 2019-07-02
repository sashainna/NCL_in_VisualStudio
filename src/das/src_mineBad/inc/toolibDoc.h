/************************************************************************
c
c   FILE NAME: toolibDoc.h
c
c	 CONTAINS: 
c		Header file all class CtoolibDoc
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c       toolibDoc.h , 25.2
c    DATE AND TIME OF LAST  MODIFICATION
c       10/12/15 , 17:16:52
c
c**********************************************************************
*/

#pragma once

struct TL_tooldisp_rec
{
	CString	m_class;
	double	m_value1;
	double	m_value2;
	double	m_value3;
	double	m_value4;
	CString		m_parm1;
	CString		m_parm2;
	CString		m_parm3;
	CString		m_parm4;
	CString	m_shade;
	CString	m_symbol;
};

class CToolibDoc : public CDocument
{
protected: // create from serialization only
	CToolibDoc();
	DECLARE_DYNCREATE(CToolibDoc)

// Attributes
public:
	int m_cuttype, m_def_cut;
	CString m_tool, m_desp;
	CString m_def1, m_def2, m_def3, m_def4, m_def5, m_def6, m_def7;
	CString m_parm1, m_parm2, m_parm3, m_parm4, m_parm5, m_parm6, m_parm7;
	CString m_cdef1, m_cdef2, m_cdef3, m_cdef4, m_cdef5, m_cdef6, m_cdef7;
	CString m_cparm1, m_cparm2, m_cparm3, m_cparm4, m_cparm5, m_cparm6, m_cparm7;

	int m_segment, m_moving, m_shade, m_loadcom, 
		m_symbol_ck, m_shank_ck, m_holder_ck;
	CString m_symbol, m_holder, m_shank, m_tooldraw;
	CString m_com[42];
	struct TL_tooldisp_rec symbol_value, shank_value, holder_value;

// Operations
public:

// Overrides
public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);
#ifdef SHARED_HANDLERS
	virtual void InitializeSearchContent();
#endif // SHARED_HANDLERS

// Implementation
public:
	virtual ~CToolibDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

// Generated message map functions
protected:
	DECLARE_MESSAGE_MAP()
};
