/************************************************************************
**
**   FILE NAME: wsntDDlstctl2.h
**
**       Description - Functions and struct declarations for
**              CNCLDDListCtrl2 class (Class for Drag&Drop List table-Record type)
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntDDlstctl2.h , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			05/04/15 , 11:44:31
**********************************************************************
*/
#if !defined(WSNTDDLSTCTL2_INCLUDE)
#define WSNTDDLSTCTL2_INCLUDE

#if _MSC_VER > 1000
#pragma once
#endif 

#include "udforms.h"
#include "wsntmDropTarget2.h"
#include "wsntFormProp.h"
/////////////////////////////////////////////////////////////////////////////
//

class CNCLDDListCtrl2 : public CListCtrl
{
	DECLARE_DYNAMIC(CNCLDDListCtrl2)
// Construction
public:
	CNCLDDListCtrl2();
// Operations
public:
	void SetParent_id(CWnd *parent, UINT id)
	{
		m_parent = parent;
		m_id = id;
	};
	void SetParent(CWnd *parent) { m_parent = parent; };
	void fill_item_data(int row, UD_ITEMDATA *data);
	void GetSelectedItem(int *row, int *col);
	void SetItemSel(int row, int col);
	void DeSelAll();
	void DrawGuides(CPoint org_pt, POINT ptCursor);
	void SetActive(int active);
	void SetPColorValue(CString pcolor)
	{
		m_prop_dlg->m_pcolor = pcolor;
	};
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLDDListCtrl2)
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CNCLDDListCtrl2();
	void OnDragDropCallback(CPoint pt, char *input_text);
	void	InitDrag();
	void SetProperty(CNCLFormProp *prop_dlg);
	void SetItemNo(int itemno);
	void set_prop_values(double font_scale, int active, char *color, char *pcolor);
	void SetDragImageFlag(int flag)
	{
		m_flag=flag;
	}
	void SetType(int type)
	{
		m_type=type;
	}
	CNCLFormProp *GetPropertyPage()
	{
		return m_prop_dlg;
	}
	void set_prop_HSPvalues(int n_picarea, UD_PICAREA *picarea);
	// Generated message map functions
protected:
	CNCLFormProp* m_prop_dlg;
	//{{AFX_MSG(CNCLDDListCtrl2)
	afx_msg void OnClick(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnKeydown(NMHDR* pNMHDR, LRESULT* pResult);
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
	void invalidate_grid(int row,int col);
	afx_msg void OnCustomDraw(NMHDR *pNMHDR, LRESULT *pResult);

	int m_flag;
	int m_sel_row;
	int m_sel_col;
	int m_selitem, m_selSitem, m_itemno;
	int m_change;
	CFont m_fieldFont;
	CWnd *m_parent;
	UINT m_id;
	int m_nNumberOfRows;
	int m_nNumberOfCols;
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
