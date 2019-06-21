/************************************************************************
**
**   FILE NAME: wsntDDgroup.h
**
**       Description - Functions and struct declarations for
**              CNCLDDGroup class (Class for Drag&Drop group control)
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntDDgroup.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:13
**********************************************************************
*/
#ifndef WSNTDDGROUP_H
#define WSNTDDGROUP_H

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "wsntmDropTarget2.h"
#include "wsntFormProp.h"

// *****************************************************************
// CNCLDDGroup window
// *****************************************************************
class CNCLDDGroup : public CButton
{
	DECLARE_DYNAMIC(CNCLDDGroup)

public:
	CNCLDDGroup(int m_type = -1);
	virtual ~CNCLDDGroup();
	void	InitDrag();
	void OnDragDropCallback(CPoint pt, char *input_text);
	void SetParent(CWnd *parent) { m_parent = parent; };
	void SetLabelText(CString text);
	void SetProperty(CNCLFormProp *prop_dlg);
	CNCLFormProp *GetPropertyPage()
	{
		return m_prop_dlg;
	}
	int Handle_frame_event(int evt, UINT nFlags, CPoint point);
	void SetItemNo(int itemno);
	void set_prop_values(double font_scale, int active, char *color);
	void DrawGuides(CPoint org_pt, POINT ptCursor);
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLDDGroup)
	//}}AFX_VIRTUAL
	LRESULT OnCtrlColorStatic(WPARAM wParam, LPARAM lParam);

	// Generated message map functions
protected:
	//{{AFX_MSG(CNCLDDGroup)
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
public:
	int m_itemno, m_prompt;
	CFont m_fieldFont;
	CNCLFormProp* m_prop_dlg;

private:
	int m_type, m_form_event;
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
