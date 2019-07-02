/************************************************************************
c
c   FILE NAME: wsntfsview.h
c
c	 Description - Functions and struct declarations for
c		NCLFormScrollView class (CNCLFormScrollView)
c     COPYRIGHT 2005 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        wsntfsview.h , 26.2
c     DATE AND TIME OF LAST  MODIFICATION
c        04/10/18 , 14:40:02
c
c**********************************************************************
*/
#ifndef WSNTFSVIEW_H
#define WSNTFSVIEW_H

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
//
#include "wsntdlgitem.h"
#include "wsntclrbtn.h"
#include "wsntlstctl.h"
#include "wsntlstctl2.h"
#include "wsntpicsel.h"
#include "wsntsecbtn.h"
#include "wsntsliderctrl.h"
#include "wsntnclbutton.h"

/////////////////////////////////////////////////////////////////////////////

class CNCLFormScrollView : public CScrollView
{
protected:
	CNCLFormScrollView(); 
	DECLARE_DYNCREATE(CNCLFormScrollView)

	CNCLColorButton m_button[MAX_FORMITEM], m_button2[MAX_FORMITEM];
	CNCLButton m_button3[MAX_FORMITEM];
	CNCLButton m_button4[MAX_FORMITEM];
	CNCLSliderCtrl m_slider[MAX_FORMITEM];
	CNCLListCtrl m_listctl[MAX_FORM_LIST];
	CNCLListCtrl2 m_listctl2[MAX_FORM_LIST];
	CNCLSecButton m_secbut[MAX_SECTION_BUT];
	DLGTEMPLATE m_dlgTempl;
	CDialogItem     m_rgDlgItem[MAX_FORMITEM];
	CNCLPicSelWin *m_picture[MAX_FORM_PIC];
	int m_create;
	CWnd* m_parent;
	int m_dlgtyp, m_secno, m_selsec;
	CFont m_txtfont;

// Attributes
public:
	void setdlgtyp(int typ) 
	{
		m_dlgtyp = typ;
	};
	virtual ~CNCLFormScrollView();
	int SetDlgTemp(DLGTEMPLATE *dlgTempl,  CDialogItem dlgItem[MAX_FORMITEM]);
	BOOL CreateIndirect(CWnd* pParentWnd, DLGTEMPLATE *dlgTempl, CDialogItem rgDlgItem[MAX_FORMITEM], int itemnum);
	BOOL CreateDlgView(CWnd *parent);
	void Recreate_button(int i, LPCTSTR lpszCaption, DWORD dwStyle, RECT& rect, UINT nID,
		COLORREF bcolor, COLORREF fcolor);
	void Recreate_button2(int i, LPCTSTR lpszCaption, DWORD dwStyle, RECT& rect, UINT nID,
		COLORREF bcolor, COLORREF fcolor);
	void Recreate_button3(int i, LPCTSTR lpszCaption, DWORD dwStyle, RECT& rect, UINT nID,
		COLORREF bcolor, COLORREF fcolor, int type);
	void Recreate_listctl(int i, char *listptr, DWORD dwStyle,
									 RECT& rect, UINT nID);
	void Recreate_listctl2(int i, char *listptr, DWORD dwStyle,
									 RECT& rect, UINT nID);
	void Create_Slider(int i, UINT fid, DWORD dwStyle, RECT& rbtn, 
				int range1, int range2, int init_value, UINT budid, int vert);
	void set_button_color(int i, COLORREF bcolor, COLORREF fcolor);
	void set_button_color2(int i, COLORREF bcolor, COLORREF fcolor);
	void set_button_color3(int i, COLORREF bcolor, COLORREF fcolor);
	void Create_PicArea(int indx, char *name, char* filename, RECT& rect, UINT nID);
	void Reset_picture(int indx, char* filename);
	void set_picarea(int picno, int n_picarea, UD_PICAREA *picarea, UINT pID);
	void close_tooltip();
	void create_secbutton(char *name, int indx, int page, UINT fid, COLORREF &color, int type);
	void setsecbut(int num) 
	{
		m_secno = num;
	};
	void OnSecButton(int indx);
	void EnableSection(int indx, int flag);
	void SetSecColor(int indx, int color[3], int bold);
	int Getindx(int page);
	int GetInputText(CString &input_text, CString prompt_str, int size[2]);
	void Recreate_imgbutton(int i, char* imgfile, DWORD dwStyle, RECT& rect, UINT nID,
		COLORREF bcolor);
public:
	LRESULT OnItemClick(WPARAM wParam, LPARAM lParam);
	LRESULT OnPicAreaClick(WPARAM wParam, LPARAM lParam);
	LRESULT OnFilterTlist(WPARAM wParam, LPARAM lParam);
	void OnItemMouseMove(int itemnum);
	void GetCurrentTlist(int itemnum, UD_TLIST *list);
	int ifFormViewValidId(UINT id);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CNCLFormScrollView)
	protected:
	virtual void OnDraw(CDC* pDC); 
	virtual void OnInitialUpdate(); 
	virtual void OnUpdate( CView* pSender, LPARAM lHint, CObject* pHint );
	afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
	afx_msg void OnPaint();
	afx_msg void OnMouseMove(UINT nFlags, CPoint point);
//for normal button callbacks
	afx_msg void FormUserCallbacks1(UINT id);
	afx_msg void FormUserCallbacks2(UINT id);
	afx_msg void FormUserCallbacks3(UINT id);
	afx_msg void FormUserCallbacks4(UINT id);
	afx_msg void FormUserCallbacks5(UINT id);
	afx_msg void FormUserCallbacks6(UINT id);
	afx_msg void FormUserCallbacks7(UINT id);
	afx_msg void FormUserCallbacks8(UINT id);
	afx_msg void FormUserCallbacks9(UINT id);
	afx_msg void FormUserCallbacks14(UINT id);
	afx_msg void FormUserCallbacks10(UINT id, NMHDR *pNMHDR, LRESULT *pResult);
	afx_msg void FormUserCallbacks11(UINT id, NMHDR *pNMHDR, LRESULT *pResult);
	afx_msg void FormUserCallbacks13(UINT id, NMHDR *pNMHDR, LRESULT *pResult);
	afx_msg void FormUserCallbacks15(UINT id, NMHDR *pNMHDR, LRESULT *pResult);
	afx_msg void FormUserCallbacks16(UINT id, NMHDR *pNMHDR, LRESULT *pResult);
	int OnMouseActivate(CWnd* pDesktopWnd, UINT nHitTest, UINT message);
	afx_msg void OnFormTabbed();
	afx_msg void OnFormSTabbed();
	afx_msg void OnAccelFunctions(UINT nID);
	afx_msg void FormUserCallbacks0();
	afx_msg void FormUserCallbacks12(UINT id, NMHDR *pNMHDR, LRESULT *pResult);
	LRESULT OnUpdateUIState(WPARAM wParam, LPARAM lParam);
	afx_msg void FormUserCBLDblclk(UINT id);
	afx_msg void OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);
	afx_msg void OnHScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar);

	//}}AFX_VIRTUAL

// Implementation
protected:
	virtual BOOL Create(LPCTSTR, LPCTSTR, DWORD,
		const RECT&, CWnd*, UINT, CCreateContext*);
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

	// Generated message map functions
	//{{AFX_MSG(CNCLFormScrollView)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
	friend class CNCLForm;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif 
