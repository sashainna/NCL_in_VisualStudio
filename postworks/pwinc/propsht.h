/************************************************************************
c
c   FILE NAME: propsht.h
c
c	 Description - Functions and struct declarations for
c		CDlgSheet class (PropertySheet)
c     COPYRIGHT 2000 (c) Numerical Control Computer Sciences.
c           All Rights Reserved
c      MODULE NAME AND RELEASE LEVEL
c        propsht.h , 24.1
c     DATE AND TIME OF LAST  MODIFICATION
c        09/11/13 , 12:58:40 
c
c**********************************************************************
*/

#ifndef DIALOGSHEET_H
#define DIALOGSHEET_H

#include "DialogPrompt.h"
class CDlgSheet : public CPropertySheet
{
	friend class  CDialogPrompt;

	DECLARE_DYNAMIC(CDlgSheet)
//construct
public:
	CDlgSheet();
// Attributes
public:
	int m_ChildId;
	CWnd* m_pParent;
	CDialogPrompt	page[10];
//operation
public:
	void AddControlPages(void);
	void SetPage(int num);
	void setact_page(int pagenum);
	void DlgOK();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDlgSheet)
protected:
	virtual void PostNcDestroy();
	//}}AFX_VIRTUAL
// Implementation

	// Generated message map functions
protected:
	int m_pagenum;
	NpwReturnStruct m_RetValStruct;
	int act_page;
    RECT m_PageRect;
	int SaveData();
	CFont m_fntPage;
	virtual void BuildPropPageArray();
	//{{AFX_MSG(CDlgSheet)
	virtual BOOL OnNotify(WPARAM wParam, LPARAM lParam, LRESULT* pResult); 
    virtual BOOL OnInitDialog();
	virtual void OnCancel();
	virtual void OnOK();
	afx_msg void OnAHelp();
    afx_msg void OnApplyNow();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

#endif
