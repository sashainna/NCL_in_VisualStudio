/********************************************************************* 
**  NAME:  wsnclframe.h
**
**			Header file for NCL frame class
**	CONTAINS: 
**
**    COPYRIGHT 2001 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsnclframe.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:12
*********************************************************************/
#ifndef WSNCLFRAME__H
#define WSNCLFRAME__H

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000


/////////////////////////////////////////////////////////////////////////////

class CNCLFrameWnd : public CFrameWnd
{
	DECLARE_DYNAMIC(CNCLFrameWnd)
public:

	CNCLFrameWnd();
	virtual ~CNCLFrameWnd();
	void EnableDocking(DWORD dwDockStyle);
	void DockControlBar(CControlBar* pBar, CDockBar* pDockBar,
		LPCRECT lpRect = NULL);
	void ReDockControlBar(CControlBar* pBar, CDockBar* pDockBar,
		LPCRECT lpRect = NULL);
	void DockControlBar(CControlBar* pBar, UINT nDockBarID = 0,
		LPCRECT lpRect = NULL);
	void FloatControlBar(CControlBar* pBar, CPoint point,
		DWORD dwStyle = CBRS_ALIGN_TOP);
	DWORD CanDock(CRect rect, DWORD dwDockStyle,
		CDockBar** ppDockBar = NULL); // called by CDockContext

	int m_delay;
	void Delay_FloatControlBar(CControlBar* pBar, CPoint point,
		DWORD dwStyle = CBRS_ALIGN_TOP);
protected:
	//{{AFX_VIRTUAL(CNCLFrameWnd)
	//}}AFX_VIRTUAL

	//{{AFX_MSG(CNCLFrameWnd)
//	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#undef AFX_DATA
#define AFX_DATA

#endif // !defined(AFX_MDIMENUFRAMEWND_H__2F686113_A464_11D1_B0D4_00A0C94457BF__INCLUDED_)
