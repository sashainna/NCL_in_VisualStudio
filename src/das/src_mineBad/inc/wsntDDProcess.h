/************************************************************************
**
**   FILE NAME: wsntDDprocess.h
**
**       Description - Functions and struct declarations for
**              CNCLDDProcess class (Class for Drag&Drop process control)
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntDDProcess.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:13
**********************************************************************
*/
#ifndef WSNTDDPROCESS_H
#define WSNTDDPROCESS_H

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "wsntmDropTarget2.h"
#include "wsntFormProp.h"

// *****************************************************************
// CNCLDDProcess window
// *****************************************************************
class CNCLDDProcess : public CProgressCtrl
{
	DECLARE_DYNAMIC(CNCLDDProcess)

// Construction
public:
	CNCLDDProcess(int type=-1);
	virtual ~CNCLDDProcess();
	void	InitDrag();
	void OnDragDropCallback(CPoint pt, char *input_text);
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
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CProgressCtrl)
	//}}AFX_VIRTUAL

	// Generated message map functions
protected:
	CNCLFormProp* m_prop_dlg;
	//{{AFX_MSG(CProgressCtrl)
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnTimer(UINT_PTR nIDEvent);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnDestroy();
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnMove(int x, int y);
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
private:
	int m_itemno;
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
