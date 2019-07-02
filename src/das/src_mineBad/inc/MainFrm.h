/************************************************************************
c
c   FILE NAME: MainFrm.h
c
c	 CONTAINS: 
c		Header file for CMainFrame class 
c
c    COPYRIGHT 2000 (c) NCCS Inc.  All Rights Reserved.
c      MODULE NAME AND RELEASE LEVEL
c         MainFrm.h , 25.3
c    DATE AND TIME OF LAST  MODIFICATION
c         12/01/15 , 08:03:01
c      
c**********************************************************************
*/
// MainFrm.h : interface of the CMainFrame class
//
/////////////////////////////////////////////////////////////////////////////

#pragma once

#include "toolibdata.h"
#include "xenv1.h"

class CPaneChildDlg : public CDialogEx
{
	DECLARE_DYNAMIC(CPaneChildDlg)
	DECLARE_MESSAGE_MAP()
public:
	CPaneChildDlg(CWnd* pParent = NULL);   // standard constructor
	virtual ~CPaneChildDlg();

	enum {IDD = IDD_CHILD4};
protected:
	virtual BOOL OnInitDialog();
	afx_msg void OnTAdd();
	afx_msg void OnTDelete();
	afx_msg void OnTSearch();
};

class CDialogPane : public CDockablePane
{
	DECLARE_DYNAMIC(CDialogPane)
	DECLARE_MESSAGE_MAP()
public:
	CDialogPane();
	virtual ~CDialogPane();
protected:
	afx_msg int OnCreate(LPCREATESTRUCT lp);
	afx_msg void OnSize(UINT nType,int cx,int cy);
private :
	CPaneChildDlg m_wndDlg ;
};

class CViewExSplitWnd : public CSplitterWnd
{
	DECLARE_DYNAMIC(CViewExSplitWnd)

// Implementation
public:
	CViewExSplitWnd();
	~CViewExSplitWnd();
	void SaveData();
	CWnd* GetActivePane(int* pRow = NULL, int* pCol = NULL);
};

class CMainFrame : public CFrameWndEx
{
	
protected: // create from serialization only
	CMainFrame();
	DECLARE_DYNCREATE(CMainFrame)
protected:
	CViewExSplitWnd m_wndSplitter;

// Attributes
public:
	int m_update;
// Operations
public:
	void save_tool_data();
	void AdjectWindowSize();

// Overrides
public:
	virtual BOOL PreCreateWindow(CREATESTRUCT& cs);
	virtual BOOL OnCreateClient(LPCREATESTRUCT lpcs, CCreateContext* pContext);

// Implementation
public:
	virtual ~CMainFrame();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif
	int get_current_toolpos() { return 	m_current_tpos;};
	void set_current_toolpos(int pos) { m_current_tpos = pos;};
	int SetFindPos(int pos);
	void  SaveFindStr(CString find_str) { m_findstr = find_str; };
	void tool_deleteone(double toolno);

protected:  // control bar embedded members
	UX_pathname m_filename; 
	CMFCStatusBar     m_wndStatusBar;
	CDialogPane  m_wndDlgPan;
	CString m_findstr;
	CMenu *m_popmenu;
	int m_current_tpos;
	void tool_list(int flag);
	void tool_updList(double toolnum=-1, int pos=-1);
	void tool_updTool(TL_tooldata_rec tool_data);

// Generated message map functions
protected:
	//{{AFX_MSG(CMainFrame)
	afx_msg int OnCreate(LPCREATESTRUCT lpCreateStruct);
	afx_msg void OnFileLoad();
	afx_msg void OnFileDefine();
	afx_msg void OnStatusStatus();
	afx_msg void OnListBrief();
	afx_msg void OnListFull();
	afx_msg void OnTAdd();
	afx_msg void OnTDelete();
	afx_msg void OnTSearch();
	afx_msg void OnPopup();
	afx_msg void OnCuttype0();
	afx_msg void OnCuttype1();
	afx_msg void OnCuttype2();
	afx_msg void OnCuttype3();
	afx_msg void OnCuttype4();
	afx_msg void OnCuttype5();
	afx_msg void OnCuttype6();
	afx_msg void OnCuttype7();
	afx_msg void OnCuttype8();
	afx_msg void OnCuttype9();
	afx_msg void OnCuttype10();
	afx_msg void OnCuttype11();
	afx_msg void OnCuttype12();
	afx_msg void OnCuttype13();
	afx_msg void OnCuttype14();
	afx_msg void OnCuttype15();
	afx_msg void OnAppExit();
	afx_msg void OnListModify();
	afx_msg void OnDescriptionsOperatormessage();
	afx_msg void OnDescriptionsParameters();
	afx_msg void OnFileSave();
	afx_msg void OnHelpHelp();
	afx_msg void OnFileInit();
	afx_msg void OnClose();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};


