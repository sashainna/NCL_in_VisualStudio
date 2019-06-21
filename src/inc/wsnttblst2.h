/************************************************************************
**
**   FILE NAME: wsnttblst2.h
**
**       Description - Functions and struct declarations for
**              CNCLTableList2 class
**    COPYRIGHT 2012 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsnttblst2.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:21
**********************************************************************
*/
#if !defined(WSNTTABLELSTCTL2_INCLUDE)
#define WSNTTABLELSTCTL2_INCLUDE

#if _MSC_VER > 1000
#pragma once
#endif 

/////////////////////////////////////////////////////////////////////////////
//

class CNCLTableList2 : public CListCtrl
{
// Construction
public:
	CNCLTableList2();
	virtual ~CNCLTableList2();
	void SetParent(CWnd *parent)
	{
		m_parent = parent;
	};
// Attributes
public:
	int m_selitem;
	int m_change;
// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLTableList2)
	//}}AFX_VIRTUAL

// Implementation
public:
	void OnHandleMouseclick(UINT nMsg, UINT nFlags, CPoint point, BOOL bTriggerEdit, int bandclick=-1);	
	void SetSelectPoint(CPoint point);
	void SetBitmap(HBITMAP hbmap, int itemnum);
	void ReSetBitmap();
	void ResetRowSize();

	// Generated message map functions
protected:
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDIS);
	//{{AFX_MSG(CNCLTableList2)
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnCustomDraw(NMHDR* pNMHDR, LRESULT* pResult);
	afx_msg void OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	afx_msg void OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	afx_msg void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void MeasureItem(LPMEASUREITEMSTRUCT lpMeasureItemStruct );
	afx_msg void OnPaint();
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
private:
	HBITMAP m_hBitmap[5000];  
	int m_iconsize;
	int m_row_height;
	CWnd *m_parent;
};

/////////////////////////////////////////////////////////////////////////////

#endif // !defined(WSNTTABLELSTCTL2_INCLUDE)
