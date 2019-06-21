/************************************************************************
**
**   FILE NAME: wsntDDformMview.h
**
**       Description - Functions and struct declarations for
**              CNCLFormMView class - form view used for form item property disply 
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntfrmMview.h , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			05/04/15 , 11:56:57
**********************************************************************
*/
#if !defined(WSNTFRMMVIEW_H)
#define WSNTFRMMVIEW_H

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "wsntformdoc.h"
#include "wsntFdsnFrm.h"

class CNCLFormMView : public CFormView
{
protected: // create from serialization only
	CNCLFormMView();
	DECLARE_DYNCREATE(CNCLFormMView)

// Attributes
public:
	CNCLFormDoc* GetDocument();
	virtual void OnInitialUpdate();
	//{{AFX_DATA(CNCLFormMView)
	enum { IDD = IDD_FORMMAIN_VIEW};
	//}}AFX_DATA

// Operations
public:
	void CreateFrame();
	CFrameWnd *m_frame;
	void SetParentFrm(CFrameWnd *parent)
	{
		m_parent = parent;
	};
	void SetMacroFlag(int flag)
	{
		m_macro_flag = flag;
	};
	CView* GetPView();
	void EnableAlignBtn(int flag);
	void UpdatePropertySize(int cx, int cy);
	void UpdatePropertyPos(int x, int y);
	void OpenPropertyPage(CNCLFormProp *prop_dlg, int flag = 0);
	void OnUndo();
	void SetSizeText();
	void SetMacroActive(int mwin_no, int active);
	int GetCheckedMacro(int no);
	void DeleteFrame();
	void UpdatePropertyHSPTSize(CRect sizerec, float prect[4]);
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLFormMView)
	public:
//	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
//	afx_msg void OnSize(UINT nType, int cx, int cy);
	protected:
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CNCLFormMView();
#ifdef _DEBUG
	virtual void AssertValid() const;
#endif

protected:
	int m_toggle, m_macro_flag;
	CFrameWnd *m_parent;
	CRect m_rect, m_ctlrect, m_org_frect, m_frmrect, m_viewrect;
	int m_delx, m_dely, m_view_change, m_fvdx, m_fvdy;
	int FormSave(char *infile, char *helptxt);
	void FormLoad(char *infile, char **helptxt, char *title);
// Generated message map functions
protected:
	//{{AFX_MSG(CNCLFormMView)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	afx_msg void OnDestroy();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
	friend class CNCLDDform;
	friend class CNCLFormFrm;
	friend class CNCLFormPView;
	friend class CNCLFormView;
	friend class CNCLFdsnFrame;
};

#ifndef _DEBUG
inline CNCLFormDoc* CNCLFormMView::GetDocument()
   { return (CNCLFormDoc*)m_pDocument; }
#endif
#endif
