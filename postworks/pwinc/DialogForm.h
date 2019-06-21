 /************************************************************************
c
c   FILE NAME: DialogForm.h
c
c	 Description - Functions and struct declarations for
c		CDialogForm class (Dynamic Dialog)
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        DialogForm.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:25
c
c**********************************************************************
*/


#ifndef DIALOGFORM_H
#define DIALOGFORM_H

#include "NpwHeaders.h"
#include "DialogItem.h"

class CDialogForm : public CDialog
{
public:
	CDialogForm(CWnd* pParent = NULL, int *level = NULL, int cur_stage = 1, int id = -1);
	BOOL Create(); 
	void initform(npwDynWinStruct *winStruct, npwDynFormStruct* formStruct);
	void initTable(int &wid, int &ht, int &ctl_num );
	void DlgOK();
	void DlgCancel();
	void Remove_Help();
	int m_pLevel[10];
	int m_curStage;

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDialogForm)
	protected:
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

protected:

	HICON m_hIcon;
	CWnd* m_pParent;
	DLGTEMPLATE m_dlgTempl;
	CDialogItem	m_rgDlgItem[200]; 
	CDialog *m_pHelpDialog;
	int m_ChildId,m_init,m_cx, m_cy;
	CRect m_listrect;
	CFont m_listfont;
	NpwDynWinStruct m_DynWinStruct;
	NpwDynFormStruct m_DynFormStruct;
	NpwReturnStruct m_RetValStruct;
	int currentListPos;
	int currentCursorPos;
	int SaveData();
	void SaveSelection();
	void SizeListBox(int cx, int cy );
	// Generated message map functions
	//{{AFX_MSG(CDialogForm)
	virtual void OnCancel();
	virtual void OnOK();
	virtual BOOL OnInitDialog();
	afx_msg void OnApply();
	afx_msg void OnAHelp();
	afx_msg void OnInsert();
	afx_msg void OnDelete();
	afx_msg void OnPicked();
	afx_msg void OnChoicePicked(UINT id);
	afx_msg void OnInputFocus(UINT id);
	afx_msg void OnSize( UINT nType, int cx, int cy );
//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

#endif

