/************************************************************************
c
c   FILE NAME: GLtestDoc.h
c
c	 CONTAINS: 
c
c     COPYRIGHT 2004 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c     MODULE NAME AND RELEASE LEVEL
c        GLtestDoc.h , 25.1
c     DATE AND TIME OF LAST  MODIFICATION
c        04/29/15 , 15:06:04
c
c**********************************************************************
*/
#if !defined TESTOGDOC_H
#define TESTOGDOC_H

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

#endif // !defined TESTOGDOC_H
