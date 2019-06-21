/************************************************************************
**
**   FILE NAME: wsntNCLbutton.h
**
**       Description - Functions and struct declarations for
**              CNCLButton class 
**    COPYRIGHT 2015 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntNCLButton.h , 26.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/10/18 , 14:42:18
**********************************************************************
*/
#ifndef WSNTNCLBUTTON_H
#define WSNTNCLBUTTON_H

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#include "afxbutton.h"
#include "udforms.h"

// *****************************************************************
// CNCLButton window
// *****************************************************************
class CNCLButton : public CMFCButton
{
	DECLARE_DYNAMIC(CNCLButton)

// Construction
public:
	CNCLButton(int m_type=-1);
	virtual ~CNCLButton();
	void SetParent(CWnd *parent) { m_parent = parent; };
	void set_color(
		const COLORREF BGColor = RGB(192, 192, 192),		// gray button
		const COLORREF FGColor = RGB(1, 1, 1)				// black text 
	);
	void SetActive(int active)
	{
		m_active = active;
	};
	void SetType(int type)
	{
		m_type = type;
	}
	void SetBitMapFile(char *filename);

protected:
	CFont m_fieldFont;
	COLORREF m_fg, m_bg;
	int m_type, m_active;
	CWnd *m_parent;
	char m_bmpfile[256];

	virtual void DrawItem(LPDRAWITEMSTRUCT lpDIS);
	void DrawItemOldWay(LPDRAWITEMSTRUCT lpDIS);
	void DrawFrame(CDC *DC, CRect R, int Inset);
	void DrawFilledRect(CDC *DC, CRect R, COLORREF color);
	void DrawLine(CDC *DC, CRect EndPoints, COLORREF color);
	void DrawLine(CDC *DC, long left, long top, long right, long bottom, COLORREF color);
	void DrawButtonText(CDC *DC, CRect R, const char *Buf, COLORREF TextColor);
	void DrawButtonIcon(CDC *DC, CRect R, HBITMAP hBmap);
	COLORREF GetFGColor() { return m_fg; }	
	COLORREF GetBGColor() { return m_bg; }

	// Generated message map functions
protected:
	//{{AFX_MSG(CNCLButton)
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg int  OnCreate(LPCREATESTRUCT lpCreateStruct);
	virtual afx_msg void OnFillBackground(CDC* pDC, const CRect & rect); 
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
private:
};
#endif
