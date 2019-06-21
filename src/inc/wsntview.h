/********************************************************************* 
**  NAME:  wsntview.h 
**
**			interface of the CNCLView class
**
**	CONTAINS: CNCLView  class functions and structure
**				declaration
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**       wsntview.h , 25.2
**  DATE AND TIME OF LAST  MODIFICATION
**       04/05/18 , 14:37:00
*********************************************************************/

// wsntview.h : interface of the CNCLView class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_NCLVIEW_H__3CA341DD_FAA0_11D3_8155_00C04F336F5E__INCLUDED_)
#define AFX_NCLVIEW_H__3CA341DD_FAA0_11D3_8155_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "usysdef.h"
#include "wsntbitmap.h"
#include "mpocket.h"
#include "wsnttooltip.h"

class CNCLView : public CView
{
protected: // create from serialization only
	CNCLView();
	DECLARE_DYNCREATE(CNCLView)

// Attributes
public:
	CNCLDoc* GetDocument();

	void PrintDIB(CDC* pDC);
	void SaveToBMP(char *filename);
	void reset_redraw();

	HCURSOR m_current_cursor;
    CPalette*    m_cPalette;
    CPalette    *m_pOldPalette;
	CRect		m_oldRect;
	CClientDC	*m_pDC;
	HGLRC	m_hrc;
	CNCLToolTip m_RectTips;
	char *m_tiptext;
	int m_papersize;
	int m_fit, m_bcolor, m_pcenter;
	UM_pkwin_type m_Wtype;
	int m_x, m_y, m_timer;
	CPoint m_mpoint;

	CPalette*	m_palDIB;
	HDIB		m_hDIB;

// Operations
public:
	void Init();
    BOOL bSetupPixelFormat(void);
	int  inicolormap();
	void SetContext();
	char *GetWin();
	void Set_win_type(UM_pkwin_type type);
	void SetCursor(HCURSOR cursor);
	void AddFloatMenu(CPoint pt, char* input_text);
//	BOOL OnTipText(UINT id, NMHDR* pNMHDR, LRESULT* pResult);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLView)
	public:
	virtual void OnDraw(CDC* pDC);  // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	virtual void OnInitialUpdate();
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	protected:
	virtual BOOL OnPreparePrinting(CPrintInfo* pInfo);
	virtual void OnBeginPrinting(CDC* pDC, CPrintInfo* pInfo);
	virtual void OnEndPrinting(CDC* pDC, CPrintInfo* pInfo);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CNCLView();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:
	int m_redraw;
	void GetDIB();
	void FreeDIB();
// Generated message map functions
protected:
	//{{AFX_MSG(CNCLView)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnDestroy();
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnMButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonDblClk(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnMButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnFilePrint();
	afx_msg void OnFilePrintPreview();
	afx_msg void OnTimer(UINT_PTR nIDEvent);
	afx_msg LRESULT OnNcHitTest( CPoint point );
	afx_msg BOOL OnMouseWheel(UINT nFlags, short zDelta, CPoint pt);


	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#ifndef _DEBUG  // debug version in NCLView.cpp
inline CNCLDoc* CNCLView::GetDocument()
   { return (CNCLDoc*)m_pDocument; }
#endif

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NCLVIEW_H__3CA341DD_FAA0_11D3_8155_00C04F336F5E__INCLUDED_)

