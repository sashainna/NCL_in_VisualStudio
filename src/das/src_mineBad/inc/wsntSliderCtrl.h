/*********************************************************************
**  NAME:  wsntSliderCtrl.h
**
**       Control routine for form Slider Bar Controls.
**
** CONTAINS:
**
**    COPYRIGHT 2015 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL
**       wsntSliderCtrl.h , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**       08/31/15 , 09:27:04
*********************************************************************/
#if !defined(WSNT_SLIDERCTRL_H_INCLUDED_)
#define WSNT_SLIDERCTRL_H_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
//
// wsntSliderCtrl.h : header file
//

/////////////////////////////////////////////////////////////////////////////
// CNCLSliderCtrl window

class CNCLSliderCtrl : public CSliderCtrl
{
// Construction
public:
	CNCLSliderCtrl();

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLSliderCtrl)
	//}}AFX_VIRTUAL

// Implementation
public:
	CWnd* m_pBuddyWnd;
	HWND SetBuddy( CWnd* pBuddyWnd );
	void SetPos( int nPos );
	void SetParent(CWnd *parent) { m_parent = parent; };
	void ReflectedScrollMessage();
	virtual ~CNCLSliderCtrl();

	// Generated message map functions
protected:
	CWnd *m_parent;
	COLORREF m_fg, m_bg;
	//{{AFX_MSG(CNCLSliderCtrl)
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(WSNT_SLIDERCTRL_H_INCLUDED_)
