/************************************************************************
**
**   FILE NAME: wsntcmdbar.h 
**
**       Description - Functions and struct declarations for
**              CNCLCmdBar class
**    COPYRIGHT 2012 (c) NCCS.  All Rights Reserved.
**    MODULE NAME AND RELEASE LEVEL 
**			wsntcmdbar.h , 25.1
**    DATE AND TIME OF LAST  MODIFICATION
**			04/29/15 , 15:12:03
**********************************************************************
*/

#ifndef NCLCMDBAR_H
#define NCLCMDBAR_H

#include "wsntdlgbar.h"
#include "wsnteditctl.h"
#include "wsntbmpbtn.h"

// HitTest Constants
#define DHT_CLOSE		0x01
#define DHT_NCBAR		0x02
#define DHT_HELP		0x03
#define DHT_CAPTION		0x04

class CNCLCmdBar : public CNCLDialogBar
{
	DECLARE_DYNAMIC(CNCLCmdBar)
public:
	CNCLCmdBar(CWnd* pParent = NULL, int dispflag = 0);
	~CNCLCmdBar();
	BOOL CreateCmdBar(); 
	void initcmd();
	int m_disptype, m_scrollx, m_scrolly;
	int uw_ntcmd_showmore();
	void set_butlabel(int fieldno, char* label);
	void Recreate_button(int i, LPCTSTR lpszCaption, DWORD dwStyle, RECT& rect, UINT nID,
		COLORREF bcolor, COLORREF fcolor);
	void enable_win(int flag);
	int insert_cmd(int sub);
	void getcmdtext(CString &label, CString &text, int &line);
	void setcmdtext(CString label, CString text, int line = 0);
	HWND getcmdwin();
	void insert_cmdstr(CString string);
	void setcmdsel(int start, int end);
	void setcmdfocus(int focus);
	void get_ecurpos(int *start, int *end);
	int get_lineno(){ return m_line_no;};
	int get_extend() { return m_extend; };
	void HandleCommandExe(WPARAM wParam, LPARAM lParam);
	void UpdateScrollBar();
	int Load_cmdstr();
	void GetWindowMultiSize(int &cx, int &cy, int &rows, int ffloat);
	int ShowWindow();
	void reset_active_ln();
	void Set_ecomlin();
	void InsertLine();
	void DeleteLine();
	void ClearCurrentLine();
	void set_cmd_lnend();
	// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLCmdBar)
	protected:
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

protected:

	CWnd* m_pParent;
	CSize m_sizeDefault_save;
	DLGTEMPLATE m_dlgTempl;
	CDialogItem  m_rgDlgItem[200];
	char m_title[256];
	int m_itemnum;
	int m_init;
	int m_total_line;
	int m_type;
	CNCLeditctl m_edit;
	CEdit m_edit_single;
	CNCLBmpButton m_arrow;
	CBrush* m_pEditBkBrush;
	int m_extend, m_line_no;
	void set_text(char *text);
	void set_label(char* label);
	void initcmdwin(int bar_size[2]);
	void SetScrollBarPos(int total, int pos);
	void reshow_edit();
	// Generated message map functions
	//{{AFX_MSG(CNCLCmdBar)
	afx_msg LRESULT HandleInitDialog(WPARAM, LPARAM);
	afx_msg void OnPaint();
//for arrow button callbacks
	afx_msg void FormUserCallbacks1();
//for edit box edit change callbacks
	afx_msg void OnCommandLine();
    afx_msg void OnNcLButtonDown(UINT nHitTest, CPoint point);
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);	
	afx_msg BOOL OnNcActivate(BOOL bActive);
    afx_msg void OnSize(UINT nType, int cx, int cy);
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#endif

