/************************************************************************
c
c   FILE NAME: PostworksDoc.h
c
c	 CONTAINS: 
c		definitions of CPostworksDoc
c     COPYRIGHT 2002 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        PostworksDoc.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:28      
c
c**********************************************************************
*/
#if !defined(AFX_PWORKSDOC_H__AECAB580_3053_11D6_90AB_00C04F336F5E__INCLUDED_)
#define AFX_PWORKSDOC_H__AECAB580_3053_11D6_90AB_00C04F336F5E__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000


class CPostworksDoc : public CDocument
{
protected: // create from serialization only
	CPostworksDoc();
	DECLARE_DYNCREATE(CPostworksDoc)

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CPostworksDoc)
	public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CPostworksDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

// Generated message map functions
protected:
	//{{AFX_MSG(CPostworksDoc)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_PWORKSDOC_H__AECAB580_3053_11D6_90AB_00C04F336F5E__INCLUDED_)
