/************************************************************************
**
**   FILE NAME: wsntdrawbutton.h
**
**       Description - Functions and struct declarations for
**              CNCLDrawButton class
**    COPYRIGHT 2010 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntdrawbutton.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:17
**********************************************************************
*/
#if !defined(NCLDRAWBUTTON_H)
#define NCLDRAWBUTTON_H

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "wsntclrcell.h"

class CNCLDrawButton : public CButton
{
	DECLARE_DYNAMIC(CNCLDrawButton)
public:
	CNCLDrawButton(); 
	virtual ~CNCLDrawButton(); 
	BOOL Attach(const UINT nID, 
		CWnd* pParent, 
		const COLORREF crColor =		RGB(192, 192, 192),
		const COLORREF crOldColor =		RGB(1, 1, 1)
		);
	void SetButtonColor(COLORREF crColor);
	void SetButtondef(int flag);
	COLORREF SetOldColor(COLORREF crOld)		{ return m_crOldColor = crOld; }	
	void SetOldColordef(int flag)
	{
		m_colordef_old = flag;
	}
protected:
	virtual void DrawItem(LPDRAWITEMSTRUCT lpDIS);
	virtual void DrawFrame(CDC *DC, CRect rect);
	virtual void DrawFilledRect(CDC *DC, CRect R, COLORREF color);
	virtual void DrawFilledDef(CDC *DC, CRect rect);

protected:
	afx_msg UINT OnGetDlgCode();
	afx_msg void OnSysColorChange();
	//}}AFX_MSG

	DECLARE_MESSAGE_MAP()
		
private:
	COLORREF	m_crOldColor;
	COLORREF	m_crColor;
	int m_colordef, m_colordef_old;
};

#endif // !defined(NCLDRAWBUTTON_H)
