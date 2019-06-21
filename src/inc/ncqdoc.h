/************************************************************************
**
**   FILE NAME: NcqDoc.h
**	  
**   CONTAINS:
**		interface of the CNcqDoc class
**
**     COPYRIGHT 2003 (c) Numerical Control Computer Sciences.
**           All Rights Reserved
**    MODULE NAME AND RELEASE LEVEL
**       ncqdoc.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**       04/29/15 , 15:06:38
**
************************************************************************
*/// ncqDoc.h : interface of the CNcqDoc class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_NCQDOC_H__8777012D_16D5_11D7_9C47_00C04F336F5E__INCLUDED_)
#define AFX_NCQDOC_H__8777012D_16D5_11D7_9C47_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


class CNcqDoc : public CDocument
{
protected: // create from serialization only
	CNcqDoc();
	DECLARE_DYNCREATE(CNcqDoc)

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNcqDoc)
	public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CNcqDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

// Generated message map functions
protected:
	//{{AFX_MSG(CNcqDoc)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NCQDOC_H__8777012D_16D5_11D7_9C47_00C04F336F5E__INCLUDED_)
