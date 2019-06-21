/********************************************************************* 
**  NAME:  wstooltip.h
**
**			NCL Toolip class
**
**	CONTAINS: CNCLToolTip class functions and structure
**			declarations
**
**    COPYRIGHT 2004 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsnttooltip.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:22
*********************************************************************/
#ifndef WSTOOLTIP_H
#define WSTOOLTIP_H
#include "stdafx.h"

class CNCLToolTip : public CWnd
{
DECLARE_DYNAMIC( CNCLToolTip );
private: 
	CString m_szText;
	CWnd* m_pParentWnd;
	BOOL m_bShowStatus;
	CPoint m_ptCurrent;
	CFont m_font;
private:
	void DisplayToolTip(const CPoint& rCurrentPoint);
	CNCLToolTip(const CNCLToolTip&);
	CNCLToolTip& operator=(const CNCLToolTip&);
public: 
	CNCLToolTip();
	~CNCLToolTip();
	BOOL Create(CWnd* pParentWnd);
	void SetText(const CString&);
	BOOL Show(const CPoint&);
	void Close();
protected:
	// Generated message map functions
	//{{AFX_MSG(CNCLToolTip)
	afx_msg void OnPaint();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};
#endif
