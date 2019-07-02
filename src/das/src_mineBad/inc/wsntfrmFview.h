/************************************************************************
**
**   FILE NAME: wsntfrmFview.h
**
**       Description - Functions and struct declarations for
**              CNCLFormFView class - form view used for form item property disply 
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntfrmFview.h , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/16/18 , 15:23:06
**********************************************************************
*/
#if !defined(WSNTFRMFVIEW_H)
#define WSNTFRMFVIEW_H

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "wsntformdoc.h"
#include "wsntFdsnFrm.h"
#include "wsntIconListBox.h"

class CNCLFormFView : public CFormView
{
protected: // create from serialization only
	CNCLFormFView();
	DECLARE_DYNCREATE(CNCLFormFView)
	CIconListBox m_ListBox;
	int m_macro_flag;
// Attributes
public:
	CNCLFormDoc* GetDocument();
	virtual void OnInitialUpdate();
	//{{AFX_DATA(CNCLFormFView)
	enum { IDD = IDD_FORMITEM_PRO };
	//}}AFX_DATA

// Operations
public:
	void SetParentFrm(CFrameWnd *parent)
	{
		m_parent = parent;
	}
	void SetMacroFlag(int flag)
	{
		m_macro_flag = flag;
	};
	void SetMacroActive(int mwin_no, int active);
	int GetCheckedMacro(int itemno);
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLFormFView)
	public:
//	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
//	afx_msg void OnSize(UINT nType, int cx, int cy);
	protected:
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CNCLFormFView();
#ifdef _DEBUG
	virtual void AssertValid() const;
#endif

protected:
	CFrameWnd *m_parent;
// Generated message map functions
protected:
	//{{AFX_MSG(CNCLFormFView)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
		afx_msg void OnCheckMacro1();
		afx_msg void OnCheckMacro2();
		afx_msg void OnCheckMacro3();
		afx_msg void OnCheckMacro4();
		afx_msg void OnCheckMacro5();
		afx_msg	void OnCheckMacro6();
		afx_msg void OnFormType();
		afx_msg void OnFormSection();
		afx_msg void OnTitleChange();
		afx_msg void OnFormSizeChange();
		//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
	friend class CNCLFdsnFrame;
};

#ifndef _DEBUG
inline CNCLFormDoc* CNCLFormFView::GetDocument()
   { return (CNCLFormDoc*)m_pDocument; }
#endif
#endif
