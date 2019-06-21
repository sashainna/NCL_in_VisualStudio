 /************************************************************************
c
c   FILE NAME: DialogHelp.h
c
c	 Description - Functions and struct declarations for
c		CDialogHelp class (Windows for display text)
c     COPYRIGHT 1997 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        DialogHelp.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:25
c
c**********************************************************************
*/

#ifndef DIALOGHELP_H
#define DIALOGHELP_H

#include "NpwHeaders.h"
#include "DialogItem.h"

class CDialogHelp : public CDialog
{
public:
	CDialogHelp(CWnd* pParent = NULL, int type=0);
	BOOL Create();  
	void sethelp(npwDynWinStruct *winStruct);
	void settext(char* filename);
	void inittemp();
	void DlgCancel();
// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDialogHelp)
	protected:
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL

protected:

	HICON m_hIcon;
	CWnd* m_pParent;
	DLGTEMPLATE m_dlgTempl;
	CDialogItem	m_rgDlgItem[20]; 
	char *m_pFileData, m_winTitle[80];
	int m_ParentType;
	CBrush* m_pEditBkBrush;

	// Generated message map functions
	//{{AFX_MSG(CDialogHelp)
	virtual void OnCancel();
	virtual BOOL OnInitDialog();
	virtual void OnSize( UINT nType, int cx, int cy );
	afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

};

#endif

