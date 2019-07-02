/************************************************************************
c
c   FILE NAME: wsntFdsnFrm.h
c
c	 CONTAINS: 
c		Header file for CNCLFdsnFrame class 
c
c    COPYRIGHT 2014 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c         wsntFdsnFrm.h , 26.2
c      DATE AND TIME OF LAST  MODIFICATION
c         04/16/18 , 15:22:07
c**********************************************************************
*/
/////////////////////////////////////////////////////////////////////////////

#if !defined(WSNTFDSNFRM_INCLUDED_)
#define WSNTFDSNFRM_INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
#include "wsntfrmPview.h"
#include "wsntfrmFview.h"
#include "wsntfrmMview.h"
#include "wsntfrmview.h"

class CNCLFdsnFrame : public CFrameWnd
{
public: // create from serialization only
	CNCLFdsnFrame();
	DECLARE_DYNCREATE(CNCLFdsnFrame)
	int m_rep;

// Attributes
protected:
	CSplitterWnd m_wndSplitter;
	CToolBar    m_wndToolBar;
	int m_macro_flag;
	int m_macro_parmno;
	char **m_macro_parms;
	char* m_helptext;
	char m_filename[1024];
	int m_delx, m_dely;
public:
	void SetFileflag(char *filename, int flag);
	void MoveMViewWindow(CRect &vrect);
// Operations
public:
	void SaveProperty();
	void ChangePromptType(int type);
	void UpdatePropertySize(int cx, int cy);
	void UpdatePropertyPos(int x, int y);
	void SetSizeText();
	void OpenPropertyPage(CNCLFormProp *prop_dlg, int flag = 0);
	CView* GetPView();
	void EnableAlignBtn(int flag);
	void EnableUndoBtn(int flag);
	void EnableRedoBtn(int flag);
	void SetMacroActive(int mwin_no, int active);
	int GetCheckedMacro(int no);
	void DeleteFrame();
	int GetToggle() { return m_toggle; };
	int CheckPos(int &x, int &y);
	void reset_selvalue();
	void MoveFormFrameRect(int left, int right, int top, int bottom);
	void SetSecFlagVal(int flag);
	int get_current_type()
	{
		return m_type;
	}
	int get_current_secflag()
	{
		return m_sec_flag;
	};
	void Resize_form_frame(int dx, int dy);
	void SaveUndoItem(int action);
	void UpdatePropertyHSPTSize(CRect sizerec, float prect[4]);
	void UpdatePropertyPicture(CString pic_label, CString pic_tooltip,
				float pic_rect[4], CString pic_params, int indx); 
	void LoadActiveHotSpot(int dtype, int itype, int inputno, int hpnum);
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLFdsnFrame)
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CNCLFdsnFrame();
	virtual BOOL OnCreateClient(LPCREATESTRUCT lpcs, CCreateContext* pContext);

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	CFormView * m_fview;
	CFormView *m_mview;
	CFormView *m_pview;
	int m_cx1, m_cx2, m_cx3, m_total_dx;
	int m_formchanged;
	int m_type, m_sec_flag;
	CRect m_rect, m_org_frect, m_frmrect, m_viewrect;
	int m_toggle;
	HACCEL m_accel;
	ACCEL *m_dlg_accel;
	int m_accelnum;
	int m_align;
	int m_upd_undo, m_upd_redo;

	int FormSave();
	void FormLoad(char *filename);
	void AdjuctMoveWindows(int flag);
	void OnFormSizeChange(int cx, int cy);
	void OnTitleChange(CString sText);
	void OnCheckMacro1();
	void OnCheckMacro2();
	void OnCheckMacro3();
	void OnCheckMacro4();
	void OnCheckMacro5();
	void OnCheckMacro6();
	void OnFormType(int type);
	void OnFormTypeBox(int type);
	void OnFormSection(int flag);
//	void Convert_pic_rect(CRect in_rect, CRect *out_rect, int flag);
	void Convert_pic_rect(float in_rect[4], float out_rect[4], int flag);

// Generated message map functions
protected:
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	//{{AFX_MSG(CNCLFdsnFrame)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnClose();
	afx_msg void OnEnterSizeMove();
	afx_msg void OnDestroy();
	afx_msg void OnFormSave();
	afx_msg void OnFormLoad();
	afx_msg void OnFormLMargin();
	afx_msg void OnFormRMargin();
	afx_msg void OnFormTMargin();
	afx_msg void OnFormBMargin();
	afx_msg void OnFormSameW();
	afx_msg void OnFormSameH();
	afx_msg void OnFormSameSize();
	afx_msg void OnFormToggleGrid();
	afx_msg void OnFormToggleGuides();
	afx_msg void OnUndo();
	afx_msg void OnRedo();
	afx_msg void OnFormHelp();
	BOOL OnToolTipText(UINT, NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnUpdateAlign(CCmdUI* pCmdUI);
	afx_msg void OnUpdateUndo(CCmdUI* pCmdUI);
	afx_msg void OnUpdateRedo(CCmdUI* pCmdUI);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
	friend class CNCLDDform;
	friend class CNCLFormFrm;
	friend class CNCLFormPView;
	friend class CNCLFormFView;
	friend class CNCLFormMView;
	friend class CNCLFormView;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif 
