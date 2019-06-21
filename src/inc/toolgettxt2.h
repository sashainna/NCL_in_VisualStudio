/********************************************************************* 
**  NAME:  toolgettxt2.h
**
**			popup text window functions
**			interface of CNCLGetText2 class
**
**	CONTAINS: CNCLGetText2 class functions and structure
**			declarations
**
**    COPYRIGHT 2015 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			toolgettxt2.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			10/12/15 , 17:14:49
*********************************************************************/
#ifndef TOOLGETTEXT2_H
#define TOOLGETTEXT2_H
#include "toolibres.h"

class CNCLGetText2 : public CDialog
{
public:
	CNCLGetText2(CWnd* pParentWnd);
	~CNCLGetText2();

// Dialog Data
	//{{AFX_DATA(CNCLTextWin2)
	enum { IDD = IDD_TEXTWIN2 };
	//}}AFX_DATA

	void SetPos(CPoint pt, int size[2])
	{
		m_pos = pt;
		m_tsize[0] = size[0];
		m_tsize[1] = size[1];
	}
	void SetTextString(char* text)
	{
		m_text = text;
	}
	void SetPromptString(char* text)
	{
		m_prompt = text;
	}
	void GetTextString(CString &text)
	{
		text = m_text;
	}
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLTextWin2)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	CString m_text, m_prompt;
	CPoint m_pos;
	int m_tsize[2];
	CWnd *m_parent;
	CFont m_txtfont;
	//{{AFX_MSG(CNCLTextWin2)
	virtual void OnSize( UINT nType, int cx, int cy );
	virtual BOOL OnInitDialog();
	LRESULT CNCLGetText2::KillFocusWin(WPARAM wparm, LPARAM lparm);
	void OnOK();
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnMButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnKeyUp(UINT nChar, UINT nRepCnt, UINT nFlags);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#endif
