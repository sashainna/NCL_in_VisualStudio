/********************************************************************* 
**  NAME:  wsnteditctl.h 
**
**			interface of the CNCLeditctl class
**
**	CONTAINS: CNCLeditctl  class functions and structure
**				declaration
**
**    COPYRIGHT 2012 (c) NCCS.  All Rights Reserved.
**  MODULE NAME AND RELEASE LEVEL
**     wsnteditctl.h , 25.1
**  DATE AND TIME OF LAST  MODIFICATION
**     04/29/15 , 15:07:17
*********************************************************************/

#if !defined NCLEDITCTL_H
#define NCLEDITCTL_H

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "Afxtempl.h"
class CNCLCmdBar;

/////////////////////////////////////////////////////////////////////////////
// CNCLeditctl window

class CNCLeditctl : public CEdit
{
// Construction
public:
	CNCLeditctl();

// Attributes
public:
// Operations
public:
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLeditctl)
	public:
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CNCLeditctl();
protected:
	int m_act_line, m_start_line, m_end_line, m_exe_start,/* actually pp line */ 
		m_start_char, m_end_char; /* select charater in editor, not pp */
	int m_total_line, m_load_done;
	CString m_whole_text; /* text displayed in editor, not whole pp text */
	int m_load_start, m_done_upload, m_done_dnload;
	CFont *m_txtfont;
	int m_ncl_insert_ln, m_ins_save;
	// Generated message map functions
protected:
	void SetSelection(int start, int end, BOOL bNoscroll = FALSE, int end_flag = 0);
	int GetCurLine() { return m_end_line; };
	void MoveToCenter(int line);
	void SetShowLine();
	void SelectCurrentLine();
	void SelectLines(int line);
	void SelectCurrentCur(int flag);
	void InsertLine();
	void DeleteLine();
	void ActLine();
	void ClearCurrentLine();
	void SetCursorAtLineEnd();
	void GetCurPos();
	void SaveInsertLine();
	void RedrawColorText();
	int HandleSpecialKey(MSG* pMsg);

	void HandleCommandExe(WPARAM wParam, LPARAM lParam);
	int HandleCommandLoad(int flag, int &add_lns);
	int HandleCommandLoad2(int &add_lns);
	void InvalidateColorText();
	int is_curent_line(int charinx, int *iLineBegins,int *iLineEndPos, int line, int *end_line);
	int is_active_line(int charinx, int *iLineBegins, int *iLineEndPos, int line, int *end_line);
	void DrawColorText(int start_line, int end_line, CClientDC& dc, int flag);
	void Redraw_text();
	void Redraw_current_line(int line);
	void Redraw_current_lntext(int line);
	void SaveEditedLine(int force_save=0);
	//{{AFX_MSG(CNCLeditctl)
	afx_msg void OnPaint();
	afx_msg void OnChange();
	afx_msg void OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	afx_msg void OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnDestroy();
	afx_msg void OnKeyUp(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnKeyDown(UINT nChar, UINT nRepCnt, UINT nFlags);
	afx_msg void OnRButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnRButtonDown(UINT nFlags, CPoint point); 
	afx_msg void OnMButtonUp(UINT nFlags, CPoint point);
	afx_msg void OnMButtonDown(UINT nFlags, CPoint point); 
	afx_msg void OnUpdate();
	afx_msg BOOL OnEraseBkgnd(CDC* pDC);
	afx_msg void SetFocusCallback();
	afx_msg void LossFocusCallback();
	afx_msg void OnMaxText();
	afx_msg void OnTimer(UINT_PTR nIDEvent);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
	friend class CNCLCmdBar;

private:
	int m_show_lines; /* number of lines showing in the window */
	int m_executed, m_error, m_edit_line, m_changed, m_linenum_changed, m_continue_line, m_end;
	int m_cursor_pos; /* cols position of the cursor */
	char m_exestr[1024];
	int m_current_line; /* the line current cursor is on */
	int m_text_hgt;
	int m_x, m_y,m_button_down;
private:
	void Init();
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined NCLEDITCTL_H
