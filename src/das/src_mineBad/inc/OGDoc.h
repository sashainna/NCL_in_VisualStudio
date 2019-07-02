/*********************************************************************
**    NAME         :  OGDoc.h
**       CONTAINS:
**    COPYRIGHT 2003 (c) NCCS Inc.  All Rights Reserved.
**     MODULE NAME AND RELEASE LEVEL
**       OGDoc.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:06
*********************************************************************/

// OGDoc.h : interface of the COGDoc class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_OGDOC_H__4925556D_97E1_4908_B34A_C543F2B6F499__INCLUDED_)
#define AFX_OGDOC_H__4925556D_97E1_4908_B34A_C543F2B6F499__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


class COGDoc : public CDocument
{
protected: // create from serialization only
	COGDoc();
	DECLARE_DYNCREATE(COGDoc)

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(COGDoc)
	public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~COGDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

// Generated message map functions
protected:
	//{{AFX_MSG(COGDoc)
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_OGDOC_H__4925556D_97E1_4908_B34A_C543F2B6F499__INCLUDED_)
