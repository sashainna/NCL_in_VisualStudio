/************************************************************************
**
**   FILE NAME: wsnttblst.h
**
**       Description - Functions and struct declarations for
**              CNCLTableList class
**    COPYRIGHT 2012 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsnttblst.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , %u%
**********************************************************************
*/
#if !defined(WSNTTABLELSTCTL_INCLUDE)
#define WSNTTABLELSTCTL_INCLUDE

#if _MSC_VER > 1000
#pragma once
#endif 
#include "wsntmdroptarget.h"
#include "wsntheaderctrl.h"

/////////////////////////////////////////////////////////////////////////////
//

class CNCLTableList : public CListCtrl
{
// Construction
public:
	CNCLTableList();
	virtual ~CNCLTableList();
	void InitDrag();
	void OnCtlClicked();
	void SetParent(CWnd *parent)
	{
		m_parent = parent;
	};
// Attributes
public:
	int m_selitem;
	int m_change;
	CNCLHeaderCtrl m_HeaderCtrl;
// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLTableList)
	//}}AFX_VIRTUAL
	virtual void PreSubclassWindow();

// Implementation
public:
	void OnHandleMouseclick(UINT nMsg, UINT nFlags, CPoint point, BOOL bTriggerEdit, int bandclick=-1);	
	void SetSelectPoint(CPoint point);
	void SetBitmap(HBITMAP hbmap, int itemnum);
	void ReSetBitmap();
	void ResetRowSize();
	void OnHeadItemEndTrack (NMHDR* pNMHDR, LRESULT* pResult);

	// Generated message map functions
protected:
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDIS);
	//{{AFX_MSG(CNCLTableList)
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnTimer(UINT_PTR nIDEvent);
	afx_msg void OnDestroy();
	afx_msg void OnNcCalcSize(BOOL bCalcValidRects, NCCALCSIZE_PARAMS FAR* lpncsp);
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnCustomDraw(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	afx_msg void OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	afx_msg void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void MeasureItem(LPMEASUREITEMSTRUCT lpMeasureItemStruct );
	afx_msg void OnPaint();
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnAdjustStatic();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
private:
	HBITMAP m_hBitmap[5000];  
	int m_iconsize;
	int m_row_height;
	CWnd *m_parent;

	CNCLMnDropTarget		*m_TargetDrop;
	CPoint					m_StartPoint;
	UINT					m_TimerID;
	friend class CNCLMnDropTarget;
};

/////////////////////////////////////////////////////////////////////////////

#endif // !defined(WSNTTABLELSTCTL_INCLUDE)
