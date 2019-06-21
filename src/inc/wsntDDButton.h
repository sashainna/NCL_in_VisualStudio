/************************************************************************
**
**   FILE NAME: wsntDDbutton.h
**
**       Description - Functions and struct declarations for
**              CNCLDDButton class (Class for Drag&Drop button)
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntDDButton.h , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/16/18 , 15:19:24
**********************************************************************
*/
#ifndef WSNTDDBUTTON_H
#define WSNTDDBUTTON_H

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "udforms.h"
#include "wsntmDropTarget2.h"
#include "wsntFormProp.h"
#include "wsntNCLbutton.h"

// *****************************************************************
// CNCLDDButton window
// *****************************************************************
class CNCLDDButton : public CNCLButton
{
	DECLARE_DYNAMIC(CNCLDDButton)

// Construction
public:
	CNCLDDButton(int m_type=1);
	virtual ~CNCLDDButton();
	void	InitDrag();
	void OnDragDropCallback(CPoint pt, char *input_text);
	void SetParent(CWnd *parent) { m_parent = parent; };
	void SetProperty(CNCLFormProp *prop_dlg, int flag = 0);
	CNCLFormProp *GetPropertyPage()
	{
		return m_prop_dlg;
	}
	void set_prop_values(double font_scale, int input, int active, char *color, char *pcolor);
	void set_prop_VideoValue(char *file, char *bmp)
	{
		m_prop_dlg->m_choices = file;
		m_prop_dlg->m_label = bmp;
	};
	int get_prompt()
	{
		return m_prompt;
	};
	void set_prompt(int flag)
	{
		m_prompt = flag;
	};
	void SetMacroFlag(int flag)
	{
		m_macro_flag = flag;
	};
	void SetItemNo(int itemno);
	void DrawGuides(CPoint org_pt, POINT ptCursor);
	void SetActive(int active);
	void set_prop_HSPvalues(int n_picarea, UD_PICAREA *picarea);
protected:
	int m_macro_flag;
	CNCLFormProp* m_prop_dlg;

	// Generated message map functions
protected:
	//{{AFX_MSG(CNCLDDButton)
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnTimer(UINT_PTR nIDEvent);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnDestroy();
	afx_msg int  OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnMove(int x, int y);
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
private:
	int m_itemno;
	int m_prompt;
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
