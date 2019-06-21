/************************************************************************
**
**   FILE NAME: wsntDDformdoc.h
**
**       Description - Functions and struct declarations for
**              CNCLFormDoc class
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntformdoc.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:17
**********************************************************************
*/
#ifndef WSNTFORMDOC_H_
#define WSNTFORMDOC_H_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

class CNCLFormDoc : public CDocument
{
protected: // create from serialization only
	CNCLFormDoc();
	DECLARE_DYNCREATE(CNCLFormDoc)

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLFormDoc)
	public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CNCLFormDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

// Generated message map functions
protected:
	//{{AFX_MSG(CNCLFormDoc)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
#endif
