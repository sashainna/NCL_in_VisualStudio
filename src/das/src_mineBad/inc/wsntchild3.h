/********************************************************************* 
**  NAME:  wsntchild3.h
**
**			Defines the class function and structure 
**				for the NCL child view number 3.
**			It is not used now but it may used 
**			later when we use splitter window for
**			different graphic view (it have some test case
**			in wsntframe.cpp so, don't remove this
**			class now.
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntchild3.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:15      
*********************************************************************/
#if !defined(AFX_NCLCHILDVIEW3_H__79A26C16_46AB_11D4_81A7_00C04F336F5E__INCLUDED_)
#define AFX_NCLCHILDVIEW3_H__79A26C16_46AB_11D4_81A7_00C04F336F5E__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

/////////////////////////////////////////////////////////////////////////////
// CChildView3 view

class CChildView3 : public CView
{
protected:
	CChildView3();           // protected constructor used by dynamic creation
	DECLARE_DYNCREATE(CChildView3)

// Attributes
public:
	CClientDC	*m_pDC;
	CNCLDoc* GetDocument();
    CPalette    m_cPalette;
    CPalette    *m_pOldPalette;
	CRect		m_oldRect;
	CToolTipCtrl m_RectTips;
	int m_tipcreated;

// Operations
public:
	void Init();
	void CreateINDEXPalette(void);
    BOOL bSetupPixelFormat(void);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CChildView3)
	public:
	virtual void OnInitialUpdate();
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	protected:
	virtual void OnDraw(CDC* pDC);      // overridden to draw this view
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CChildView3();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	// Generated message map functions
protected:
	//{{AFX_MSG(CChildView3)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnDestroy();
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_NCLCHILDVIEW3_H__79A26C16_46AB_11D4_81A7_00C04F336F5E__INCLUDED_)