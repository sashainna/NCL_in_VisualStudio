/************************************************************************
**
**   FILE NAME: wsntDDformPview.h
**
**       Description - Functions and struct declarations for
**              CNCLFormPView class - form view used for form item property disply 
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntfrmPview.h , 25.4
**    DATE AND TIME OF LAST  MODIFICATION
**			08/18/15 , 08:51:55
**********************************************************************
*/
#if !defined(WSNTFRMPVIEW_H)
#define WSNTFRMPVIEW_H

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "wsntformdoc.h"
#include "wsntFormProp.h"
#include "wsntclrbtn.h"
#include "wsntFdsnFrm.h"

class CNCLFormPView : public CFormView
{
protected: // create from serialization only
	CNCLFormPView();
	DECLARE_DYNCREATE(CNCLFormPView)

// Attributes
public:
	CNCLFormDoc* GetDocument();
	void CopyPropertyPage(CNCLFormProp *prop_dlg_from);
	CNCLFormProp* GetPropertyPage();
	void SetParentFrm(CFrameWnd* dlg)
	{
		m_parent = dlg;
	};
	void SetMacroFlag(int flag)
	{
		m_macro_flag = flag;
	};
	int GetIColor(int color);
	void filldata();
	void SaveAll();
	virtual void OnInitialUpdate();
	void UpdatePropertySize(int cx, int cy);
	void UpdatePropertyPos(int x, int y);
	void OpenPropertyPage(CNCLFormProp *prop_dlg, int flag = 0);
	void SetDtype(int type) { m_dtype = type;};
	void SetFormInputNo(int formnum)
	{
		m_inputno = formnum;
	};
	void SetFormSecNo(int secno)
	{
		m_secno = secno;
	};
	void CreateColorBut();
	void UpdatePropertyHSPTSize(CRect sizerec, float prect[4]);
	void Convert_rect(float in_rect[4], float out_rect[4], int flag);
	void UpdatePropertyPicture(CString pic_label, CString pic_tooltip,
				float pic_rect[4], CString pic_params, int indx); 
	void Add_picarea(char *name);
	//{{AFX_DATA(CNCLFormPView)
	enum { IDD = IDD_FORMITEM_PRO2 };
	//}}AFX_DATA

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLFormPView)
	public:
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	protected:
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CNCLFormPView();
#ifdef _DEBUG
	virtual void AssertValid() const;
//	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	CNCLColorButton m_button[4];
	int m_init, m_view_reset, m_formchanged, m_inputno;
	int m_ffg, m_fbg, m_pfg, m_pbg;
	CFrameWnd *m_parent;
	CRect m_rect, m_viewrect;
	CNCLFormProp* m_prop_dlg;
	int m_macro_flag;
	int m_dtype, m_itype;
	CString m_label;
	int m_pos[2], m_size[2];
	double m_font;
	UD_DASIN m_range[2]; 
	int m_range_flag;
	int m_type, m_input, m_len, m_prec, m_active, m_input_itemno;
	CString m_limit, m_choices, m_color, m_pcolor;
	int m_page, m_secno, m_justified;
	int m_reloading;
/*
......added for picture field
*/
	CString m_pic_label;
	CString m_pic_tooltip;
	float m_pic_rect[4]; //percentage, not pixel, minx, miny, maxx, maxy
	CString m_pic_params;
	int m_pic_act_area;
	UD_PICAREA *m_picarea;
	int m_picarea_no;
// Generated message map functions
protected:
	void SaveJustified();
	//{{AFX_MSG(CNCLFormPView)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
//	virtual BOOL OnInitDialog();
	afx_msg void OnFileBrowse();
	afx_msg void OnColorBut1();
	afx_msg void OnColorBut2();
	afx_msg void OnColorBut3();
	afx_msg void OnColorBut4();
	afx_msg void OnSaveLabel();
	afx_msg void OnSaveSize();
	afx_msg void OnSaveLength();
	afx_msg void OnSaveFieldColor();
	afx_msg void OnSavePColor();
	afx_msg void OnSaveFontSize();
	afx_msg void OnSaveInputNo();
	afx_msg void OnSavePageNo();
	afx_msg void OnSaveActive();
	afx_msg void OnSaveChoices();
	afx_msg void OnSaveLimit();
	afx_msg void OnSavePrec();
	afx_msg void OnSaveRange();
	afx_msg void OnSaveInputType();
	afx_msg void OnSaveStrType();
	afx_msg void OnSavePosition();
	afx_msg void OnSavePicName();
	afx_msg void OnSaveTooltip();
	afx_msg void OnSavePicPos();
	afx_msg void OnSavePicParms();
	afx_msg void OnLoadActivePic();

	virtual BOOL PreTranslateMessage(MSG* pMsg);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
	friend class CNCLDDform;
	friend class CNCLFormFrm;
	friend class CNCLFdsnFrame;
};

#ifndef _DEBUG
inline CNCLFormDoc* CNCLFormPView::GetDocument()
   { return (CNCLFormDoc*)m_pDocument; }
#endif
#endif
