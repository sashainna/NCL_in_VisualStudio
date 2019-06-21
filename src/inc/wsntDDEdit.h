/************************************************************************
**
**   FILE NAME: wsntDDEdit.h
**
**       Description - Functions and struct declarations for
**              CNCLDDEdit class (Class for Drag&Drop Edit box)
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntDDEdit.h , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			05/04/15 , 11:40:25
**********************************************************************
*/
#ifndef WSNTDDEDIT_H
#define WSNTDDEDIT_H

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "udforms.h"
#include "wsntmDropTarget2.h"
#include "wsntFormProp.h"

// *****************************************************************
// CNCLDDEdit window
// *****************************************************************
class CNCLDDEdit : public CEdit
{
	DECLARE_DYNAMIC(CNCLDDEdit)

public:
	CNCLDDEdit(int type);
	virtual ~CNCLDDEdit();
	void	InitDrag();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLDDEdit)
	//}}AFX_VIRTUAL
	void OnDragDropCallback(CPoint pt, char *input_text);
	void SetParent(CWnd *parent) { m_parent = parent; };
	void SetProperty(CNCLFormProp *prop_dlg);
	CNCLFormProp *GetPropertyPage()
	{
		return m_prop_dlg;
	}
	void SetItemNo(int itemno);
	void DrawGuides(CPoint org_pt, POINT ptCursor);
	void set_prop_values(int rflag, UD_DASIN range[2],  double font_scale, int type, int input, int len,
		int prec, int active, char *limit, char *color, char *pcolor);
	void SetActive(int active);
	void SetPColorValue(CString pcolor)
	{
		m_prop_dlg->m_pcolor = pcolor;
	};
	void set_prop_HSPvalues(int n_picarea, UD_PICAREA *picarea);

	LRESULT OnCtrlColorEdit(WPARAM wParam, LPARAM lParam);

	// Generated message map functions
protected:
	CNCLFormProp* m_prop_dlg;
	//{{AFX_MSG(CNCLDDEdit)
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
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
	int m_itemno;
	CFont m_fieldFont;
	CWnd *m_group;
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
