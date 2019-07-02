/********************************************************************* 
**  NAME:  wsncldockframe.h
**
**			Header file for CNCLDockFrameWnd
**	CONTAINS: 
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsncldockframe.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:12
*********************************************************************/
#ifndef WSNCLDOCKFRAME__H
#define WSNCLDOCKFRAME__H

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include <afxpriv.h>

/////////////////////////////////////////////////////////////////////////////

class CNCLDockFrameWnd : public CMiniDockFrameWnd
{
	DECLARE_DYNCREATE(CNCLDockFrameWnd)
protected:
	CNCLDockFrameWnd();           // protected constructor used by dynamic creation

// Attributes
public:
	int m_changed;
	CRect m_rcHelp;
	DWORD m_LastHit;
	DWORD m_ButtonDown;
// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLDockFrameWnd)
	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual ~CNCLDockFrameWnd();
	DWORD HitTest(CPoint pt);

	// Generated message map functions
	//{{AFX_MSG(CNCLDockFrameWnd)
		afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
		afx_msg void OnSysCommand(UINT nID, LPARAM lp);
		afx_msg void OnNcPaint();
		afx_msg void OnClose();
		afx_msg void OnNcLButtonDown(UINT nHitTest, CPoint point);
		afx_msg void OnNcLButtonDblClk(UINT nHitTest, CPoint point);
		afx_msg void OnNcLButtonUp( UINT nHitTest, CPoint point);
		afx_msg void OnNcRButtonUp( UINT nHitTest, CPoint point);
		afx_msg void OnNcRButtonDown( UINT nHitTest, CPoint point);
		afx_msg LRESULT OnNcHitTest( CPoint point);
		afx_msg BOOL OnNcActivate(BOOL bActive);
		afx_msg void OnNcMouseMove( UINT nHitTest, CPoint point);	
		afx_msg void OnHandlePopup(UINT nID);
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif 
