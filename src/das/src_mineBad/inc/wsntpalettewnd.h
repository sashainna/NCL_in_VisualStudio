/************************************************************************
**
**   FILE NAME: wsntpalettewnd.h
**
**       Description - Functions and struct declarations for
**              CNCLPaletteWnd class
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntpalettewnd.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:20
**********************************************************************
*/
#if !defined(NCL_PALLETTEWND_H)
#define NCL_PALLETTEWND_H

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CNCLPaletteWnd window

#include "wsntpalettectl.h"

class CNCLPaletteWnd : public CWnd
{
// Construction
public:
	CNCLPaletteWnd();

// Attributes
public:
	int m_focus;
	//Create
	BOOL Create(DWORD dwStyle,
		CRect rcPos, 
		CWnd* pParent,
		UINT nID,
		COLORREF crColor,
		BOOL bPopup = FALSE);

// Operations
	//Get ColorControl
	CNCLPaletteControl *GetColorControl()	{ return m_pColorControl; }

protected:

	// The pointer to ColorControl
	CNCLPaletteControl *m_pColorControl;

	// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLPaletteWnd)
	protected:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	//}}AFX_VIRTUAL

// Implementation
public:
	// Destructor
	virtual ~CNCLPaletteWnd();

	// Generated message map functions
protected:
	//{{AFX_MSG(CNCLPaletteWnd)
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnDestroy();
	afx_msg void OnPaint();
	afx_msg void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnSysColorChange();
	afx_msg UINT OnGetDlgCode();
	afx_msg void OnSetFocus(CWnd* pOldWnd);	
	afx_msg void OnKillFocus(CWnd* pNewWnd);
	//}}AFX_MSG
	afx_msg LRESULT OnSelectColorOK(WPARAM wParam, LPARAM lParam);
	DECLARE_MESSAGE_MAP()
protected:
	CWnd *m_pNotifyWnd;
};

/////////////////////////////////////////////////////////////////////////////

#endif // !defined(NCL_PALLETTEWND_H)
