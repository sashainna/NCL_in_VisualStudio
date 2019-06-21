/************************************************************************
**
**   FILE NAME: wsntDDstatic.h
**
**       Description - Functions and struct declarations for
**              CNCLDDStatic class (Class for Drag&Drop static control)
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntDDstatic.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:14
**********************************************************************
*/
#ifndef WSNTDDSTATIC_H
#define WSNTDDSTATIC_H

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "wsntmDropTarget2.h"
#include "wsntFormProp.h"

// *****************************************************************
// CNCLDDStatic window
// *****************************************************************
class CNCLDDStatic : public CWnd
{
	DECLARE_DYNAMIC(CNCLDDStatic)

public:
	CNCLDDStatic(int m_type = -1);
	virtual ~CNCLDDStatic();
	void	InitDrag();
	void SetLabelText(CString text);
	CStatic *m_label;
	void OnDragDropCallback(CPoint pt, char *input_text);
	void SetParent(CWnd *parent) { m_parent = parent; };
	void GetLabelText(CString &text);
	int GetLabelText(LPTSTR buf, int maxcount);
	void SetProperty(CNCLFormProp *prop_dlg, int flag = 0);
	CNCLFormProp *GetPropertyPage()
	{
		return m_prop_dlg;
	}
	void SetItemNo(int itemno);
	void DrawGuides(CPoint org_pt, POINT ptCursor);
	void set_prop_values(double font_scale, int active, char *pcolor);

	LRESULT OnCtrlColorStatic(WPARAM wParam, LPARAM lParam);
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLDDStatic)
	//}}AFX_VIRTUAL

	// Generated message map functions
protected:
	CNCLFormProp* m_prop_dlg;
	//{{AFX_MSG(CNCLDDStatic)
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
	int m_type;
	int m_itemno, m_prompt;
	CFont m_fieldFont;

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
