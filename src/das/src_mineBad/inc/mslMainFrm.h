/************************************************************************
c
c   FILE NAME: mslMainFrm.h
c
c	 CONTAINS: 
c		Header file for CMainFrame class for MSLITE
c
c    COPYRIGHT 2008 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c         mslMainFrm.h , 25.1
c    DATE AND TIME OF LAST  MODIFICATION
c         04/29/15 , 15:06:32
c
c**********************************************************************/
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_MAINFRM_H__85482DB4_4A89_4266_9974_E62D17E815F7__INCLUDED_)
#define AFX_MAINFRM_H__85482DB4_4A89_4266_9974_E62D17E815F7__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000
#include "wsnttmenu.h"
#include "wsntdlgbar.h"
#include "wsntformbar.h"
#include "wsntcmdbar.h"
#include "wsntmenudsndlg.h"
#include "wsntmdroptarget.h"

class CMainFrame : public CFrameWnd
{
	
protected: // create from serialization only
	CMainFrame();
	DECLARE_DYNCREATE(CMainFrame)

// Attributes
public:

// Operations
public:

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CMainFrame)
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	protected:
	virtual LRESULT WindowProc(UINT message, WPARAM wParam, LPARAM lParam);
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CMainFrame();
	BOOL OnDynamicTipText(UINT id, NMHDR* pNMHDR, LRESULT* pResult);
	virtual BOOL OnNotify( WPARAM wParam, LPARAM lParam, LRESULT* pResult);

#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	CToolmenu *NCL_menubar[200];
	CNCLDialogBar *m_promptBar,
				*m_errorBar, *m_statusBar;
	CNCLCmdBar *m_commandBar;
	CNCLFormBar *m_formbar[60];
	void WriteLabel(char *label);
	void WritePLabel(char *label);
	void WriteError(char *label);
	void SetCommand_focus(int focus);
	void SetCommand_Str(CString msg, int line_num = 0, int upload = 0);
	void InsertCommand_Str(CString msg);
	void GetCommand_Str(CString &msg, int &line_num, int &upload);
	void SetCommand_Insertpos(int cmdcur1, int cmdcur2);
	void Enable_cmdbar(int flag);
	void WriteStat(int field, char *msg);
	int Get_CmdEnable();
	void SetCommand_Select(int start=0, int end=-1);
	BOOL NCLPreTranslateMessage(MSG* pMsg);
	BOOL NCLPreTranslateMessage2(MSG* pMsg);
	void initial_msl();
protected: 
	
	HACCEL m_accel;
	PROCESS_INFORMATION m_ncl_info[50];

public:
	int m_sub_ncl;
	int m_addmenu, m_additem, m_remove_menu, m_remove_item, m_add_menupos[5];
	
	void Recreate_AcceleratorTable() ;
	void StartNewNCL(char *parm=NULL);
	void remove_subproc(int sub);
	int GetSubNCL();
	void Ontimer2();
	void redisp_popmenu(int menu);
	void redisp_menubar(int menu);
	void SaveMenu(int menunum);
	CNCLMenuDsnDlg *m_menudsgn_dlg;
	int m_menu_desgn;

// Generated message map functions
protected:
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	//{{AFX_MSG(CMainFrame)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG
	afx_msg void OnDestroy();
	virtual void OnClose();
	afx_msg void OnClose2();
	afx_msg void OnDisplayMenu(UINT id);
	afx_msg void OnMSLFunctions(UINT id);
	afx_msg void OnAccelFunctions(UINT id);
	afx_msg void OnPupupChoices(UINT id);
	afx_msg void OnKeyNoop();
	afx_msg void Ondummy1();
	afx_msg void OnDragMenu(UINT id);
	afx_msg void OnMeasureItem( int nIDCtl, LPMEASUREITEMSTRUCT lpMeasureItemStruct );
	afx_msg void OnTimer(UINT_PTR nIDEvent);
	afx_msg void Ondummy2();
	afx_msg void OnDrawItem(int nIDCtl, LPDRAWITEMSTRUCT lpDrawItemStruct);
	LRESULT OnMenuDrag(WPARAM, LPARAM);
	LRESULT OnMenuGetObject(WPARAM, LPARAM);
	LRESULT OnMenuRbuttonup(WPARAM, LPARAM);
	afx_msg void OnUpdateMenubar();
	afx_msg void OnAddNewMenubar();
	afx_msg void OnLButtonUp(UINT nFlags, CPoint point);	
	afx_msg void OnLButtonDown(UINT nFlags, CPoint point);	
	DECLARE_MESSAGE_MAP()

	CNCLMnDropTarget		*m_TargetDrop;
	void UpdatePOPMenu(CPoint pt, char* input_text);
	friend class CNCLMnDropTarget;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_MAINFRM_H__85482DB4_4A89_4266_9974_E62D17E815F7__INCLUDED_)
