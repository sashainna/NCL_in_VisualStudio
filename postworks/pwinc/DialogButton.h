/************************************************************************
c
c   FILE NAME: DialogButton.h
c
c	 Description - Functions and struct declarations for
c		CDialogButton class (Dynamic Dialog)
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        DialogButton.h , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        05/05/14 , 14:31:29
c
c**********************************************************************
*/


#ifndef DIALOGTEMP_H
#define DIALOGTEMP_H

#include "NpwHeaders.h"
#include "DialogItem.h"

class CDialogButton : public CDialog
{
	friend class  CDialogPrompt;
public:
	CDialogButton(CWnd* pParent = NULL, int style = 1, int *level = NULL, 
			int cur_stage = 1, int id = -1);
	~CDialogButton();
	int Create();  
	void inittemp(npwDynWinStruct *winStruct);
	void DlgCancel();
	void Remove_Text();
	void Remove_Child(int id);
	void Add_Child(CDialog* dlg, int id);
	void Remove_Temp(int id);
	int sheet_disp[20];
	int m_close_ans;
	// Dialog Data
	//{{AFX_DATA(CDialogButton)
		// NOTE: the ClassWizard will add data members here
	//}}AFX_DATA

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDialogButton)
	protected:
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL
//	CScrollDialog	m_dlgTemplate[20];
protected:

	CWnd* m_pParent;
	HICON m_hIcon;
/*
.....Modaless or not
.....m_style = 0 modaless
*/
	int m_style; 
	DLGTEMPLATE m_dlgTempl;
	CDialogItem	m_rgDlgItem[20]; 
	CDialog *m_pModeless[20];
	CDialog *m_pTextDialog;
	int m_pLevel[10];
	int m_curStage;
	int m_ChildId;
	int m_childtype[20];
	int m_active_child;
	CString m_winTitle;
//	int browsefile(char*Filter, char*FileName);
//	static LPOFNHOOKPROC BrowseCallback(HWND,WORD,WPARAM,LPARAM);
	// Generated message map functions
	//{{AFX_MSG(CDialogButton)
	virtual void OnCancel();
	afx_msg void OnViewDocumentFile();
	afx_msg void OnViewMachineSim();
	afx_msg void OnCreateDocument();
	afx_msg void OnCreateMachineSim();
	afx_msg void OnLoadMachine();
	afx_msg void OnSaveMachineAs();
	afx_msg void OnSaveMachine();
	afx_msg void OnHelpContents();
	afx_msg void OnHelpAbout();
	afx_msg void OnPushedBut(UINT id) ;
	virtual BOOL OnInitDialog();
	afx_msg void OnDropFiles(HDROP hDropInfo);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

#endif

