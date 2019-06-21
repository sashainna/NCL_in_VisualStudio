/********************************************************************* 
**  NAME:  wsnttxtwin.h
**
**			Native WinNT read only text scrolling window 
**			interface of CNCLTextWin class
**
**	CONTAINS: CNCLTextWin class functions and structure
**			declarations
**
**    COPYRIGHT 2000 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsnttxtwin.h , 25.2
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 18:04:34
*********************************************************************/
#ifndef NCLTEXTWIN_H
#define NCLTEXTWIN_H
#include "wsntres.h"

// HitTest Constants
#define DHT_CLOSE		0x01
#define DHT_CAPTION		0x04
#define DHT_MIN		0x03

class CNCLTextWin : public CDialog
{
public:
	CNCLTextWin(CWnd* pParentWnd, int type, char *title=NULL);
	~CNCLTextWin();

// Dialog Data
	//{{AFX_DATA(CNCLTextWin)
	enum { IDD = IDD_TEXTWIN };
	//}}AFX_DATA

	int m_nrows, m_ncols;
	int m_x,m_y, m_w, m_h;
	int m_vis;
	int m_insertpos;
	CString m_title;
	void SetText(char *text);
	void InsertText(char *text);
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLTextWin)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	int m_type;
	CFont m_txtfont;

	CRect m_rcClose, m_rcMin;
	DWORD m_LastHit;
	DWORD m_ButtonDown;
	DWORD HitTest(CPoint pt);

	//{{AFX_MSG(CNCLTextWin)
	virtual void OnSize( UINT nType, int cx, int cy );
	virtual BOOL OnInitDialog();

	afx_msg void OnNcPaint();
    afx_msg void OnNcLButtonDown(UINT nHitTest, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);	
	afx_msg LRESULT OnNcHitTest(CPoint point);
	afx_msg BOOL OnNcActivate(BOOL bActive);
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	LRESULT OnCtrlColorEdit(WPARAM wParam, LPARAM lParam);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#endif
