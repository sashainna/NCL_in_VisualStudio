/************************************************************************
**
**   FILE NAME: wsntDDSlider.h
**
**       Description - Functions and struct declarations for
**              CNCLDDSlider class (slider for drag&drop)
**    COPYRIGHT 2015 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntDDSlider.h , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			08/31/15 , 09:21:07
**********************************************************************
*/
#ifndef WSNTDDSLIDER_H
#define WSNTDDSLIDER_H

#include "udforms.h"
#include "wsntmDropTarget2.h"
#include "wsntFormProp.h"
#include "wsntSliderctrl.h"

class CNCLDDSlider : public CNCLSliderCtrl
{
DECLARE_DYNAMIC(CNCLDDSlider)
public:
	CNCLDDSlider(int type=-1); 
	virtual ~CNCLDDSlider(); 
	void	InitDrag();
	void OnDragDropCallback(CPoint pt, char *input_text);
	void SetProperty(CNCLFormProp *prop_dlg);
	CNCLFormProp *GetPropertyPage()
	{
		return m_prop_dlg;
	}
	void SetParent(CWnd *parent) { m_parent = parent; };
	void SetItemNo(int itemno);
	void DrawGuides(CPoint org_pt, POINT ptCursor);
	void set_prop_values(int active, char *color, char *pcolor, 
			int rflag, UD_DASIN range[2], int justified);
	void SetActive(int active);
	void set_color(const COLORREF bcolor, const COLORREF fcolor);
	void SetPColorValue(CString pcolor)
	{
		m_prop_dlg->m_pcolor = pcolor;
	};
	void set_prop_HSPvalues(int n_picarea, UD_PICAREA *picarea);

protected:
	CNCLFormProp* m_prop_dlg;

private:
	int m_itemno;
	// Generated message map functions
protected:
	//{{AFX_MSG(CNCLDDSlider)
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
	int m_type;
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
