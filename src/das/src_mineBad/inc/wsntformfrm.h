/************************************************************************
**
**   FILE NAME: wsntformfrm.h
**
**       Description - Functions and struct declarations for
**              CNCLFormFrm class
**    COPYRIGHT 2013 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntformfrm.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:07:17
**********************************************************************
*/
#if !defined(WSNTFORMFORMFRM_H)
#define WSNTFORMFORMFRM_H

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

/////////////////////////////////////////////////////////////////////////////
// CNCLFormFrm frame

class CNCLFormFrm : public CFrameWnd
{
	DECLARE_DYNCREATE(CNCLFormFrm)
protected:
	CNCLFormFrm();           // protected constructor used by dynamic creation

// Attributes
public:
	void SetType (int type)
	{
		m_type = type;
	};
// Operations
public:


// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLFormFrm)
	//}}AFX_VIRTUAL
// Implementation
protected:
	int m_type;
	CPoint m_scrpos;
	virtual ~CNCLFormFrm();
	virtual BOOL PreTranslateMessage(MSG* pMsg);

	// Generated message map functions
	//{{AFX_MSG(CNCLFormFrm)
	virtual void OnClose();
	afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnMove(int x, int y);
	afx_msg void OnExitSizeMove();
	afx_msg void OnEnterSizeMove();
	afx_msg LRESULT OnNcHitTest( CPoint point );
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
	friend class CNCLFormMView;
};
#endif
