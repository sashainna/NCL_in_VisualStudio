/************************************************************************
**
**   FILE NAME: wsntDDlstctl.h
**
**       Description - Functions and struct declarations for
**              CNCLDDListCtrl class (Class for Drag&Drop List table)
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntDDlstctl.h , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			05/04/15 , 11:43:26
**********************************************************************
*/
#if !defined(WSNTDDLSTCTL_INCLUDE)
#define WSNTDDLSTCTL_INCLUDE

#if _MSC_VER > 1000
#pragma once
#endif 
#include "udforms.h"
#include "wsntmDropTarget2.h"
#include "wsntFormProp.h"

/////////////////////////////////////////////////////////////////////////////
//

class CNCLDDListCtrl : public CListCtrl
{
	DECLARE_DYNAMIC(CNCLDDListCtrl)
// Construction
public:
	CNCLDDListCtrl();

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLDDListCtrl)
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CNCLDDListCtrl();
	void OnDragDropCallback(CPoint pt, char *input_text);
	void	InitDrag();
	void SetDragImageFlag(int flag)
	{
		m_flag=flag;
	}
	void SetType(int type)
	{
		m_type=type;
	}
	void SetParent(CWnd *parent) { m_parent = parent; };
	void SetProperty(CNCLFormProp *prop_dlg);
	CNCLFormProp *GetPropertyPage()
	{
		return m_prop_dlg;
	}
	void SetItemNo(int itemno);
	void DrawGuides(CPoint org_pt, POINT ptCursor);
	void set_prop_values(double font_scale, int active, char *color, char *pcolor);
	void SetActive(int active);
	void SetPColorValue(CString pcolor)
	{
		m_prop_dlg->m_pcolor = pcolor;
	};
	void set_prop_HSPvalues(int n_picarea, UD_PICAREA *picarea);
	// Generated message map functions
protected:
	CNCLFormProp* m_prop_dlg;
	//{{AFX_MSG(CNCLDDListCtrl)
	afx_msg void OnNcCalcSize(BOOL bCalcValidRects, NCCALCSIZE_PARAMS FAR* lpncsp);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnTimer(UINT_PTR nIDEvent);
	afx_msg void OnDestroy();
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnMove(int x, int y);
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()

private:
	int m_flag;
	int m_selitem, m_itemno;
	int m_change;
	CFont m_fieldFont;
	int m_type;
	CWnd *m_parent;
	CNCLMnDropTarget2		*m_TargetDrop;
	CPoint					m_StartPoint;
	UINT					m_TimerID;
	CImageList *m_DragImage;
	CImageList *m_DragAllImage;
	int m_dragimg_create;
	friend class CNCLMnDropTarget2;
	friend class CNCLDDform;
};
#endif
