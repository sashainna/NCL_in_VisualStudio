/************************************************************************
c
c   FILE NAME: DialogPrompt.h
c
c	 Description - Functions and struct declarations for
c		CDialogPrompt class (PropertyPage)
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        DialogPrompt.h , 24.2
c     DATE AND TIME OF LAST  MODIFICATION
c        05/05/14 , 14:33:51
c
c**********************************************************************
*/

#ifndef DIALOGPRMP_H
#define DIALOGPRMP_H

#include "NpwHeaders.h"
#include "PwScrollView.h"

class CDialogPrompt : public CDialog
{
public:
	CDialogPrompt(CWnd* pParent = NULL, int *level = NULL, int cur_stage = 1, int id = -1);
	CPWScrollView *m_pScrollView;
	void init(NpwDynWinStruct* winStruct=NULL);
	int SaveData();
	void Remove_Help();
	int m_pLevel[10];
	int m_curStage;
	NpwReturnStruct m_RetValStruct;
	CWnd* m_pParent;
	int m_modify;
	void DlgOK();
	void DlgCancel();
	void OnChoicePicked(UINT id);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDialogPrompt)
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

protected:
	UINT m_def_id;
	HICON m_hIcon;
	int m_ChildId;
	DLGTEMPLATE m_dlgTempl;
	CDialogItem	m_rgDlgItem[200]; 
	CDialog *m_pHelpDialog;
	NpwDynWinStruct m_DynWinStruct;
	NpwDynFormStruct m_DynFormStruct;
	void SizeDialogItem(int cx, int cy );
	
	// Generated message map functions
	//{{AFX_MSG(CDialogPrompt)
	virtual void OnCancel();
	virtual void OnOK();
	afx_msg void OnApply();
	afx_msg void OnAHelp();
	virtual BOOL OnInitDialog();
	afx_msg void OnSize( UINT nType, int cx, int cy );
	afx_msg LRESULT OnViewChoicePicked(WPARAM wparm, LPARAM lparm);
	LRESULT FormUserCallbacks1(WPARAM wparm, LPARAM lparm);
	LRESULT FormUserCallbacks2(WPARAM wparm, LPARAM lparm);
	LRESULT FormUserCallbacks3(WPARAM wparm, LPARAM lparm);
	afx_msg void OnFormTabbed();
	afx_msg void OnFormSTabbed();
	virtual BOOL PreTranslateMessage(MSG* pMsg);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

#endif

