/************************************************************************
**
**   FILE NAME: wsntDDformview.h
**
**       Description - Functions and struct declarations for
**              CNCLFormView class - form view used for form design
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntfrmview.h , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**			05/04/15 , 11:58:12
**********************************************************************
*/
#if !defined(WSNTFRMVIEW_H)
#define WSNTFRMVIEW_H

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "wsntformdoc.h"
#include "wsntDDform.h"
#include "wsntFormProp.h"

class CNCLFormView : public CFormView
{
protected: // create from serialization only
	CNCLFormView();
	DECLARE_DYNCREATE(CNCLFormView)

// Attributes
public:
	CNCLFormDoc* GetDocument();
	void CreateForm();
	void SizeForm(int force=0);
	void SetPrentDlg(CFormView* dlg)
	{
		m_parent_dlg = dlg;
	};
	void SetType(int type);
	void OpenPropertyPage(CNCLFormProp *prop_dlg, int flag = 0);
	CNCLFormProp *GetPropertyPage()
	{
		return m_prop_dlg;
	}
	void OnUndo();
	void OnRedo();
	virtual void OnInitialUpdate();
	void UpdatePropertySize(int cx, int cy);
	void UpdatePropertyPos(int x, int y);
	int GetToggle();
	void CreateSectionButton(CRect rect);
	void LoadFormItem(UD_FSTRUCT *fstruct);
	void GetSecInfo(int indx, char *name, char *color)
	{
		strcpy(name, m_sec_name[indx]);
		strcpy(color, m_sec_color[indx]);
	};
	void SetSecInfo(int indx, char *name, char *color);
	void OnItemMouseMove(int itemnum);
	void OnDeleteSec(int itemnum);
	void OnInsertSec(int itemnum);
//	void OnChangeSecLabel(int itemnum);
	void OnChangeSecProperty(int itemnum, int flag=0);
	void OnChangeSecColor(int itemnum);
	void OnSectionSelItem(int itemnum);
	int get_valid_id();
	void SetSection(int type);
	void OnDragDropSection(int secnum, char *drop_text);
	void SetSecFlag(int flag)
		{
			m_sec_flag = flag;
	};
	CString GetSectionLabel(int indx)
	{
		CString label = m_sec_name[indx];
		return label;
	};
	CString GetSectionColor(int indx)
	{
		CString color = m_sec_color[indx];
		return color;
	};
	void SetSectionAttr(int indx, CString label, CString color);
	void UpdatePropertyHSPTSize(CRect sizerec, float prect[4]);
	//{{AFX_DATA(CNCLFormView)
	enum { IDD = IDD_DIALOG1 };
	//}}AFX_DATA

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLFormView)
	public:
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	protected:
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CNCLFormView();
#ifdef _DEBUG
	virtual void AssertValid() const;
//	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	int m_init, m_type, m_view_reset, m_formchanged;
	int m_selsec;
	int m_sec_id[50], m_del_id;
	CFormView *m_parent_dlg;
	CNCLFormProp* m_prop_dlg;
	CRect m_rect, m_viewrect;
	CNCLDDform *m_right_form;
	CWnd *m_sec_win[51], *m_delete_secbut;
	char m_sec_name[50][40], m_sec_color[50][40];
	CButton *m_box1, *m_box2;
	int m_secno, m_sec_flag;

// Generated message map functions
protected:
	//{{AFX_MSG(CNCLFormView)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	afx_msg void OnDestroy();
	afx_msg void OnSectionSel(UINT id);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	LRESULT OnDeleteSecBut(WPARAM wParam, LPARAM lParam);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

	friend class CNCLDDform;
	friend class CNCLFormFrm;
	friend class CNCLFdsnFrame;
	friend class CNCLFormMView;
};

#ifndef _DEBUG
inline CNCLFormDoc* CNCLFormView::GetDocument()
   { return (CNCLFormDoc*)m_pDocument; }
#endif
#endif
