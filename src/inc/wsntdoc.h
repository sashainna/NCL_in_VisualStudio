/********************************************************************* 
**  NAME:  wsntdoc.h
**
**			functions and datas declaration for CNCLDoc class
**
**		CONTAINS: 
**			functions and datas declaration for CNCLDoc class
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntdoc.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:16      
*********************************************************************/

#if !defined(AFX_NCLDOC_H__3D6A248B_4608_11D4_81A6_00C04F336F5E__INCLUDED_)
#define AFX_NCLDOC_H__3D6A248B_4608_11D4_81A6_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000


class CNCLDoc : public CDocument
{
protected: // create from serialization only
	CNCLDoc();
	DECLARE_DYNCREATE(CNCLDoc)

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLDoc)
	public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CNCLDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

// Generated message map functions
protected:
	//{{AFX_MSG(CNCLDoc)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NCLDOC_H__3D6A248B_4608_11D4_81A6_00C04F336F5E__INCLUDED_)
