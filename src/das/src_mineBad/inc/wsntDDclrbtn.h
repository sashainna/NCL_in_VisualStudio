/************************************************************************
**
**   FILE NAME: wsntDDclrbtn.h
**
**       Description - Functions and struct declarations for
**              CNCLDDColorButton class (color button for drag&drop)
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntDDclrbtn.h , 25.3
**    DATE AND TIME OF LAST  MODIFICATION
**			06/17/15 , 14:15:17
**********************************************************************
*/
#ifndef WSNTDDCOLORBTN_H
#define WSNTDDCOLORBTN_H

#include "udforms.h"
#include "wsntmDropTarget2.h"
#include "wsntFormProp.h"

class CNCLDDColorButton : public CButton
{
DECLARE_DYNAMIC(CNCLDDColorButton)
public:
	CNCLDDColorButton(int type=-1); 
	virtual ~CNCLDDColorButton(); 
	void	InitDrag();
	void OnDragDropCallback(CPoint pt, char *input_text);
	void SetProperty(CNCLFormProp *prop_dlg);
	CNCLFormProp *GetPropertyPage()
	{
		return m_prop_dlg;
	}
	void set_color(
		const COLORREF BGColor = RGB(192, 192, 192),		// gray button
		const COLORREF FGColor = RGB(1, 1, 1)				// black text 
	);
	void SetParent(CWnd *parent) { m_parent = parent; };
	void SetItemNo(int itemno);
	void DrawGuides(CPoint org_pt, POINT ptCursor);
	void set_prop_values(double font_scale, int active, char *color, char *pcolor, char *limit = NULL, int input = 1);
	void SetActive(int active);
	void SetPColorValue(CString pcolor)
	{
		m_prop_dlg->m_pcolor = pcolor;
	};
	void set_prop_HSPvalues(int n_picarea, UD_PICAREA *picarea);
protected:
	CNCLFormProp* m_prop_dlg;
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDIS);
	void DrawFrame(CDC *DC, CRect R, int Inset);
	void DrawFilledRect(CDC *DC, CRect R, COLORREF color);
	void DrawLine(CDC *DC, CRect EndPoints, COLORREF color);
	void DrawLine(CDC *DC, long left, long top, long right, long bottom, COLORREF color);
	void DrawButtonText(CDC *DC, CRect R, const char *Buf, COLORREF TextColor);
	COLORREF GetFGColor() { return m_fg; }	
	COLORREF GetBGColor() { return m_bg; }

private:
	int m_itemno;
	CFont m_fieldFont;
	COLORREF m_fg, m_bg;
	// Generated message map functions
protected:
	//{{AFX_MSG(CNCLDDButton)
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
